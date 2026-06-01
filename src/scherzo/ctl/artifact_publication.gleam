import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/github as github_repository
import scherzo/ctl/schedule_state
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/workflow_checkpoint

pub fn list(
  root: String,
  json_output: Bool,
  run_id: String,
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(schedule_state.load_projection(root, pair_error))
  use _ <- result.try(require_publication_run(projected, run_id))
  let summaries = publication_summaries(projected, root, run_id)
  case json_output {
    True ->
      output_line(
        json.object([
          #("run_id", json.string(run_id)),
          #("workspace_root", json.string(root)),
          #("publications", json.array(summaries, publication_summary_to_json)),
        ])
        |> json.to_string,
      )
    False -> print_list(run_id, summaries, output_line)
  }
  Ok(Nil)
}

pub fn show(
  root: String,
  json_output: Bool,
  run_id: String,
  publication_id: String,
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(schedule_state.load_projection(root, pair_error))
  use _ <- result.try(require_publication_run(projected, run_id))
  let attempts =
    projection.publication_attempts_for_run(projected, run_id, publication_id)
  use latest <- result.try(publication_or_not_found(attempts, publication_id))
  case json_output {
    True ->
      output_line(
        json.object([
          #("run_id", json.string(run_id)),
          #("publication_id", json.string(publication_id)),
          #("workspace_root", json.string(root)),
          #("latest", publication_attempt_to_json(root, latest)),
          #("attempt_count", json.int(list.length(attempts))),
          #(
            "attempts",
            json.array(attempts, fn(attempt) {
              publication_attempt_to_json(root, attempt)
            }),
          ),
        ])
        |> json.to_string,
      )
    False ->
      print_show(root, run_id, publication_id, latest, attempts, output_line)
  }
  Ok(Nil)
}

pub fn retry(
  root: String,
  json_output: Bool,
  run_id: String,
  publication_id: String,
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  retry_with_runner(
    root,
    json_output,
    run_id,
    publication_id,
    command_runner.production(),
    output_line,
  )
}

pub fn retry_with_runner(
  root: String,
  json_output: Bool,
  run_id: String,
  publication_id: String,
  runner: command_runner.Runner,
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(schedule_state.load_projection(root, pair_error))
  use _ <- result.try(require_publication_run(projected, run_id))
  let attempts =
    projection.publication_attempts_for_run(projected, run_id, publication_id)
  use latest <- result.try(publication_or_not_found(attempts, publication_id))
  use manifest_ref <- result.try(require_manifest_ref(latest))
  use retained <- result.try(load_publication_manifest(root, manifest_ref))
  use planned <- result.try(require_retry_manifest(retained))
  let route =
    artifact_publication_config.PublicationRoute(
      id: retained.publication_id,
      repository: planned.repository_kind <> "." <> planned.repository_id,
      required: retained.required,
      pull_request: None,
      files: [],
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, monotonic_ms)
  use prepared <- result.try(
    github_repository.prepare_publication_input(
      planned,
      artifact_store.new(root),
    )
    |> result.map_error(fn(error) {
      #(github_repository.code(error), github_repository.message(error))
    }),
  )
  let manifest =
    github_repository.publish(prepared, root, runner, checkpoint.now_ms())
  use attempt <- result.try(
    artifact_publication_recording.record_manifest_attempt(
      route,
      retained.workflow_id,
      retained.run_id,
      manifest,
      checkpoint,
    )
    |> result.map_error(fn(message) {
      #("publication_retry_record_failed", message)
    }),
  )
  let projected_attempt =
    projection_attempt_from_summary(
      retained.run_id,
      retained.workflow_id,
      attempt,
    )
  case json_output {
    True ->
      output_line(
        json.object([
          #("run_id", json.string(run_id)),
          #("publication_id", json.string(publication_id)),
          #("workspace_root", json.string(root)),
          #("attempt", publication_attempt_to_json(root, projected_attempt)),
        ])
        |> json.to_string,
      )
    False ->
      print_retry(root, run_id, publication_id, projected_attempt, output_line)
  }
  Ok(Nil)
}

type PublicationSummary {
  PublicationSummary(
    publication_id: String,
    series_id: String,
    latest_status: String,
    latest_attempt_id: String,
    attempt_count: Int,
    version_id: Option(String),
    manifest_ref: Option(String),
    manifest_sha256: Option(String),
    manifest_bytes: Option(Int),
    branch: Option(String),
    commit_sha: Option(String),
    pr_url: Option(String),
    retryable: Bool,
    retry_execution_available: Bool,
    error_code: Option(String),
    error_message: Option(String),
  )
}

type PublicationManifestDetails {
  PublicationManifestDetails(
    branch: Option(String),
    commit_sha: Option(String),
    pr_url: Option(String),
  )
}

fn pair_error(code: String, message: String) -> #(String, String) {
  #(code, message)
}

fn require_publication_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(Nil, #(String, String)) {
  case projection.has_workflow_run(projected, run_id) {
    True -> Ok(Nil)
    False ->
      Error(#(
        "publication_run_not_found",
        "publication run not found: " <> run_id,
      ))
  }
}

fn publication_or_not_found(
  attempts: List(projection.PublicationAttempt),
  publication_id: String,
) -> Result(projection.PublicationAttempt, #(String, String)) {
  case list.reverse(attempts) {
    [latest, ..] -> Ok(latest)
    [] ->
      Error(#(
        "publication_not_found",
        "publication not found: " <> publication_id,
      ))
  }
}

fn publication_summaries(
  projected: projection.Projection,
  root: String,
  run_id: String,
) -> List(PublicationSummary) {
  publication_summaries_loop(
    projection.publication_ids_for_run(projected, run_id),
    projected,
    root,
    run_id,
    [],
  )
}

fn publication_summaries_loop(
  publication_ids: List(String),
  projected: projection.Projection,
  root: String,
  run_id: String,
  acc: List(PublicationSummary),
) -> List(PublicationSummary) {
  case publication_ids {
    [] -> list.reverse(acc)
    [publication_id, ..rest] -> {
      let attempts =
        projection.publication_attempts_for_run(
          projected,
          run_id,
          publication_id,
        )
      case publication_or_not_found(attempts, publication_id) {
        Ok(latest) -> {
          let details = manifest_details_for_attempt(root, latest)
          publication_summaries_loop(rest, projected, root, run_id, [
            PublicationSummary(
              publication_id: publication_id,
              series_id: latest.series_id,
              latest_status: latest.status,
              latest_attempt_id: latest.attempt_id,
              attempt_count: list.length(attempts),
              version_id: latest.version_id,
              manifest_ref: latest.manifest_ref,
              manifest_sha256: latest.manifest_sha256,
              manifest_bytes: latest.manifest_bytes,
              branch: manifest_detail_option(details, fn(value) { value.branch }),
              commit_sha: manifest_detail_option(details, fn(value) {
                value.commit_sha
              }),
              pr_url: manifest_detail_option(details, fn(value) { value.pr_url }),
              retryable: latest.retryable,
              retry_execution_available: latest.retry_execution_available,
              error_code: latest.error_code,
              error_message: latest.error_message,
            ),
            ..acc
          ])
        }
        Error(_) ->
          publication_summaries_loop(rest, projected, root, run_id, acc)
      }
    }
  }
}

fn print_list(
  run_id: String,
  summaries: List(PublicationSummary),
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("run_id: " <> run_id)
  case summaries {
    [] -> output_line("publications: -")
    _ ->
      list.each(summaries, fn(summary) {
        output_line(
          summary.publication_id
          <> ": status="
          <> summary.latest_status
          <> " attempts="
          <> int.to_string(summary.attempt_count)
          <> " latest_attempt="
          <> summary.latest_attempt_id,
        )
        output_line("  series_id: " <> summary.series_id)
        output_line("  version_id: " <> optional_string(summary.version_id))
        output_line("  branch: " <> optional_string(summary.branch))
        output_line("  commit_sha: " <> optional_string(summary.commit_sha))
        output_line("  pr_url: " <> optional_string(summary.pr_url))
        output_line("  manifest_ref: " <> optional_string(summary.manifest_ref))
        output_line(
          "  manifest_sha256: " <> optional_string(summary.manifest_sha256),
        )
        output_line(
          "  manifest_bytes: " <> optional_int(summary.manifest_bytes),
        )
        output_line("  retryable: " <> bool_string(summary.retryable))
        output_line(
          "  retry_execution_available: "
          <> bool_string(summary.retry_execution_available),
        )
        output_line("  error_code: " <> optional_string(summary.error_code))
        output_line(
          "  error_message: " <> optional_string(summary.error_message),
        )
      })
  }
}

fn print_show(
  root: String,
  run_id: String,
  publication_id: String,
  latest: projection.PublicationAttempt,
  attempts: List(projection.PublicationAttempt),
  output_line: fn(String) -> Nil,
) -> Nil {
  let latest_details = manifest_details_for_attempt(root, latest)
  output_line("run_id: " <> run_id)
  output_line("publication_id: " <> publication_id)
  output_line("series_id: " <> latest.series_id)
  output_line("latest_status: " <> latest.status)
  output_line("latest_attempt_id: " <> latest.attempt_id)
  output_line("attempt_count: " <> int.to_string(list.length(attempts)))
  output_line("version_id: " <> optional_string(latest.version_id))
  output_line(
    "branch: "
    <> manifest_detail_string(latest_details, fn(value) { value.branch }),
  )
  output_line(
    "commit_sha: "
    <> manifest_detail_string(latest_details, fn(value) { value.commit_sha }),
  )
  output_line(
    "pr_url: "
    <> manifest_detail_string(latest_details, fn(value) { value.pr_url }),
  )
  output_line("manifest_ref: " <> optional_string(latest.manifest_ref))
  output_line("manifest_sha256: " <> optional_string(latest.manifest_sha256))
  output_line("manifest_bytes: " <> optional_int(latest.manifest_bytes))
  output_line("retryable: " <> bool_string(latest.retryable))
  output_line(
    "retry_execution_available: "
    <> bool_string(latest.retry_execution_available),
  )
  output_line("error_code: " <> optional_string(latest.error_code))
  output_line("error_message: " <> optional_string(latest.error_message))
  output_line("attempts:")
  list.each(attempts, fn(attempt) {
    let details = manifest_details_for_attempt(root, attempt)
    output_line(
      "- "
      <> int.to_string(attempt.recorded_at_ms)
      <> " "
      <> attempt.status
      <> " "
      <> attempt.attempt_id,
    )
    output_line("  version_id: " <> optional_string(attempt.version_id))
    output_line(
      "  branch: "
      <> manifest_detail_string(details, fn(value) { value.branch }),
    )
    output_line(
      "  commit_sha: "
      <> manifest_detail_string(details, fn(value) { value.commit_sha }),
    )
    output_line(
      "  pr_url: "
      <> manifest_detail_string(details, fn(value) { value.pr_url }),
    )
    output_line("  manifest_ref: " <> optional_string(attempt.manifest_ref))
    output_line(
      "  manifest_sha256: " <> optional_string(attempt.manifest_sha256),
    )
    output_line("  manifest_bytes: " <> optional_int(attempt.manifest_bytes))
    output_line("  retryable: " <> bool_string(attempt.retryable))
    output_line(
      "  retry_execution_available: "
      <> bool_string(attempt.retry_execution_available),
    )
    output_line("  error_code: " <> optional_string(attempt.error_code))
    output_line("  error_message: " <> optional_string(attempt.error_message))
  })
}

fn print_retry(
  root: String,
  run_id: String,
  publication_id: String,
  attempt: projection.PublicationAttempt,
  output_line: fn(String) -> Nil,
) -> Nil {
  let details = manifest_details_for_attempt(root, attempt)
  output_line("run_id: " <> run_id)
  output_line("publication_id: " <> publication_id)
  output_line("status: " <> attempt.status)
  output_line("attempt_id: " <> attempt.attempt_id)
  output_line("manifest_ref: " <> optional_string(attempt.manifest_ref))
  output_line("version_id: " <> optional_string(attempt.version_id))
  output_line(
    "branch: " <> manifest_detail_string(details, fn(value) { value.branch }),
  )
  output_line(
    "commit_sha: "
    <> manifest_detail_string(details, fn(value) { value.commit_sha }),
  )
  output_line(
    "pr_url: " <> manifest_detail_string(details, fn(value) { value.pr_url }),
  )
  output_line("retryable: " <> bool_string(attempt.retryable))
  output_line(
    "retry_execution_available: "
    <> bool_string(attempt.retry_execution_available),
  )
  output_line("error_code: " <> optional_string(attempt.error_code))
  output_line("error_message: " <> optional_string(attempt.error_message))
}

fn publication_summary_to_json(summary: PublicationSummary) -> json.Json {
  json.object([
    #("publication_id", json.string(summary.publication_id)),
    #("series_id", json.string(summary.series_id)),
    #("latest_status", json.string(summary.latest_status)),
    #("latest_attempt_id", json.string(summary.latest_attempt_id)),
    #("attempt_count", json.int(summary.attempt_count)),
    #("version_id", optional_string_json(summary.version_id)),
    #("branch", optional_string_json(summary.branch)),
    #("commit_sha", optional_string_json(summary.commit_sha)),
    #("pr_url", optional_string_json(summary.pr_url)),
    #("manifest_ref", optional_string_json(summary.manifest_ref)),
    #("manifest_sha256", optional_string_json(summary.manifest_sha256)),
    #("manifest_bytes", optional_int_json(summary.manifest_bytes)),
    #("retryable", json.bool(summary.retryable)),
    #("retry_execution_available", json.bool(summary.retry_execution_available)),
    #("error_code", optional_string_json(summary.error_code)),
    #("error_message", optional_string_json(summary.error_message)),
  ])
}

fn publication_attempt_to_json(
  root: String,
  attempt: projection.PublicationAttempt,
) -> json.Json {
  let details = manifest_details_for_attempt(root, attempt)
  json.object([
    #("run_id", json.string(attempt.run_id)),
    #("workflow_id", json.string(attempt.workflow_id)),
    #("publication_id", json.string(attempt.publication_id)),
    #("series_id", json.string(attempt.series_id)),
    #("attempt_id", json.string(attempt.attempt_id)),
    #("status", json.string(attempt.status)),
    #("required", json.bool(attempt.required)),
    #("retryable", json.bool(attempt.retryable)),
    #("retry_execution_available", json.bool(attempt.retry_execution_available)),
    #("version_id", optional_string_json(attempt.version_id)),
    #(
      "branch",
      optional_string_json(
        manifest_detail_option(details, fn(value) { value.branch }),
      ),
    ),
    #(
      "commit_sha",
      optional_string_json(
        manifest_detail_option(details, fn(value) { value.commit_sha }),
      ),
    ),
    #(
      "pr_url",
      optional_string_json(
        manifest_detail_option(details, fn(value) { value.pr_url }),
      ),
    ),
    #("manifest_ref", optional_string_json(attempt.manifest_ref)),
    #("manifest_sha256", optional_string_json(attempt.manifest_sha256)),
    #("manifest_bytes", optional_int_json(attempt.manifest_bytes)),
    #("error_code", optional_string_json(attempt.error_code)),
    #("error_message", optional_string_json(attempt.error_message)),
    #("recorded_at_ms", json.int(attempt.recorded_at_ms)),
  ])
}

fn manifest_details_for_attempt(
  root: String,
  attempt: projection.PublicationAttempt,
) -> Option(PublicationManifestDetails) {
  case attempt.manifest_ref {
    Some(ref) ->
      case load_publication_manifest(root, ref) {
        Ok(manifest) ->
          Some(PublicationManifestDetails(
            branch: manifest.branch,
            commit_sha: manifest.commit_sha,
            pr_url: manifest.pr_url,
          ))
        Error(_) -> None
      }
    None -> None
  }
}

fn load_publication_manifest(
  root: String,
  manifest_ref: String,
) -> Result(
  artifact_publication_manifest.PublicationManifest,
  #(String, String),
) {
  use contents <- result.try(
    artifact_store.read_artifact_unverified(
      artifact_store.new(root),
      manifest_ref,
    )
    |> result.map_error(fn(error) {
      #("publication_manifest_read_failed", artifact_store_error_message(error))
    }),
  )
  artifact_publication_manifest.decode_manifest_json(contents)
  |> result.map_error(fn(message) {
    #("publication_manifest_decode_failed", message)
  })
}

fn require_manifest_ref(
  attempt: projection.PublicationAttempt,
) -> Result(String, #(String, String)) {
  case attempt.manifest_ref {
    Some(ref) -> Ok(ref)
    None ->
      Error(#(
        "publication_manifest_missing",
        "publication attempt is missing a retained manifest ref",
      ))
  }
}

fn require_retry_manifest(
  manifest: artifact_publication_manifest.PublicationManifest,
) -> Result(
  artifact_publication_planner.DryRunPublicationManifest,
  #(String, String),
) {
  case manifest.dry_run_manifest {
    Some(planned) -> Ok(planned)
    None ->
      Error(#(
        "publication_retry_unavailable",
        "retained publication manifest does not include retry execution inputs",
      ))
  }
}

fn projection_attempt_from_summary(
  run_id: String,
  workflow_id: String,
  attempt: artifact_publication_recording.PublicationAttemptSummary,
) -> projection.PublicationAttempt {
  projection.PublicationAttempt(
    run_id: run_id,
    workflow_id: workflow_id,
    publication_id: attempt.publication_id,
    series_id: attempt.series_id,
    attempt_id: attempt.attempt_id,
    status: attempt.status,
    required: attempt.required,
    retryable: attempt.retryable,
    retry_execution_available: attempt.retry_execution_available,
    version_id: attempt.version_id,
    manifest_ref: Some(attempt.manifest_ref),
    manifest_sha256: Some(attempt.manifest_sha256),
    manifest_bytes: Some(attempt.manifest_bytes),
    error_code: attempt.error_code,
    error_message: attempt.error_message,
    recorded_at_ms: 0,
  )
}

fn manifest_detail_string(
  details: Option(PublicationManifestDetails),
  select: fn(PublicationManifestDetails) -> Option(String),
) -> String {
  details |> manifest_detail_option(select) |> optional_string
}

fn manifest_detail_option(
  details: Option(PublicationManifestDetails),
  select: fn(PublicationManifestDetails) -> Option(String),
) -> Option(String) {
  case details {
    Some(value) -> select(value)
    None -> None
  }
}

fn artifact_store_error_message(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.ArtifactIo(message)
    | artifact_store.CorruptStepArtifact(message)
    | artifact_store.InvalidArtifactRef(message)
    | artifact_store.DecodeArtifactFailed(message)
    | artifact_store.DirectorySyncUnsupported(message) -> message
    artifact_store.MissingStepArtifact(ref) -> "missing artifact: " <> ref
    artifact_store.ArtifactWriteFailed(write_error) ->
      artifact_store.artifact_write_error_to_string(write_error)
  }
}

fn optional_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "-"
  }
}

fn optional_int(value: Option(Int)) -> String {
  case value {
    Some(value) -> int.to_string(value)
    None -> "-"
  }
}

fn optional_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn optional_int_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
