import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_manifest
import scherzo/ctl/schedule_state
import scherzo/state/artifact_store
import scherzo/state/projection

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
          #(
            "publications",
            json.array(summaries, fn(summary) {
              publication_summary_to_json(run_id, summary)
            }),
          ),
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

type PublicationSummary {
  PublicationSummary(
    publication_id: String,
    series_id: String,
    publication_status: String,
    latest_status: String,
    latest_attempt_id: String,
    attempt_count: Int,
    required: Bool,
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
    cleanup_diagnostics: Option(
      artifact_publication_manifest.CleanupDiagnostics,
    ),
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
              publication_status: publication_contract_status(latest.status),
              latest_status: latest.status,
              latest_attempt_id: latest.attempt_id,
              attempt_count: list.length(attempts),
              required: latest.required,
              version_id: latest.version_id,
              manifest_ref: latest.manifest_ref,
              manifest_sha256: latest.manifest_sha256,
              manifest_bytes: latest.manifest_bytes,
              branch: manifest_detail_option(details, branch_option),
              commit_sha: manifest_detail_option(details, commit_sha_option),
              pr_url: manifest_detail_option(details, pr_url_option),
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

fn publication_contract_status(latest_status: String) -> String {
  case latest_status {
    "published" | "unchanged" -> "published"
    "failed" -> "failed"
    _ -> "pending"
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
          <> summary.publication_status
          <> " latest_status="
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
  output_line("status: " <> publication_contract_status(latest.status))
  output_line("latest_status: " <> latest.status)
  output_line("latest_attempt_id: " <> latest.attempt_id)
  output_line("attempt_count: " <> int.to_string(list.length(attempts)))
  output_line("version_id: " <> optional_string(latest.version_id))
  output_line(
    "branch: " <> manifest_detail_string(latest_details, branch_option),
  )
  output_line(
    "commit_sha: " <> manifest_detail_string(latest_details, commit_sha_option),
  )
  output_line(
    "pr_url: " <> manifest_detail_string(latest_details, pr_url_option),
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
    output_line("  branch: " <> manifest_detail_string(details, branch_option))
    output_line(
      "  commit_sha: " <> manifest_detail_string(details, commit_sha_option),
    )
    output_line("  pr_url: " <> manifest_detail_string(details, pr_url_option))
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

fn publication_summary_to_json(
  run_id: String,
  summary: PublicationSummary,
) -> json.Json {
  json.object([
    #("publication_id", json.string(summary.publication_id)),
    #("series_id", json.string(summary.series_id)),
    #("status", json.string(summary.publication_status)),
    #("latest_status", json.string(summary.latest_status)),
    #("latest_attempt_id", json.string(summary.latest_attempt_id)),
    #("attempt_count", json.int(summary.attempt_count)),
    #("required", json.bool(summary.required)),
    #("version_id", optional_string_json(summary.version_id)),
    #("branch", optional_string_json(summary.branch)),
    #("commit_sha", optional_string_json(summary.commit_sha)),
    #("pr_url", optional_string_json(summary.pr_url)),
    #(
      "retry_instruction",
      optional_string_json(retry_instruction(
        run_id,
        summary.publication_id,
        summary.latest_status,
        summary.retryable,
        summary.retry_execution_available,
        summary.version_id,
      )),
    ),
    #(
      "abandon_instruction",
      optional_string_json(abandon_instruction(
        run_id,
        summary.publication_id,
        summary.latest_status,
        summary.required,
      )),
    ),
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
      optional_string_json(manifest_detail_option(details, branch_option)),
    ),
    #(
      "commit_sha",
      optional_string_json(manifest_detail_option(details, commit_sha_option)),
    ),
    #(
      "pr_url",
      optional_string_json(manifest_detail_option(details, pr_url_option)),
    ),
    #("manifest_ref", optional_string_json(attempt.manifest_ref)),
    #("manifest_sha256", optional_string_json(attempt.manifest_sha256)),
    #("manifest_bytes", optional_int_json(attempt.manifest_bytes)),
    #(
      "retry_instruction",
      optional_string_json(retry_instruction(
        attempt.run_id,
        attempt.publication_id,
        attempt.status,
        attempt.retryable,
        attempt.retry_execution_available,
        attempt.version_id,
      )),
    ),
    #(
      "abandon_instruction",
      optional_string_json(abandon_instruction(
        attempt.run_id,
        attempt.publication_id,
        attempt.status,
        attempt.required,
      )),
    ),
    #("error_code", optional_string_json(attempt.error_code)),
    #("error_message", optional_string_json(attempt.error_message)),
    #(
      "cleanup_diagnostics",
      cleanup_diagnostics_json(manifest_detail_cleanup(details)),
    ),
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
            cleanup_diagnostics: manifest.cleanup_diagnostics,
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

fn branch_option(details: PublicationManifestDetails) -> Option(String) {
  details.branch
}

fn commit_sha_option(details: PublicationManifestDetails) -> Option(String) {
  details.commit_sha
}

fn pr_url_option(details: PublicationManifestDetails) -> Option(String) {
  details.pr_url
}

fn manifest_detail_cleanup(
  details: Option(PublicationManifestDetails),
) -> Option(artifact_publication_manifest.CleanupDiagnostics) {
  case details {
    Some(details) -> details.cleanup_diagnostics
    None -> None
  }
}

fn cleanup_diagnostics_json(
  value: Option(artifact_publication_manifest.CleanupDiagnostics),
) -> json.Json {
  case value {
    Some(artifact_publication_manifest.CleanupDiagnostics(
      checkout_path,
      pre_cleanup_status,
      reset_summary,
      clean_summary,
      post_cleanup_status,
      cleanup_succeeded,
    )) ->
      json.object([
        #("checkout_path", optional_string_json(Some(checkout_path))),
        #("pre_cleanup_status", optional_string_json(pre_cleanup_status)),
        #("reset_summary", optional_string_json(reset_summary)),
        #("clean_summary", optional_string_json(clean_summary)),
        #("post_cleanup_status", optional_string_json(post_cleanup_status)),
        #("cleanup_succeeded", json.bool(cleanup_succeeded)),
      ])
    None -> json.null()
  }
}

fn retry_instruction(
  run_id: String,
  publication_id: String,
  status: String,
  retryable: Bool,
  retry_execution_available: Bool,
  version_id: Option(String),
) -> Option(String) {
  case
    artifact_publication_manifest.retry_eligibility_for_attempt(
      status,
      retryable: retryable,
      retry_execution_available: retry_execution_available,
      version_id: version_id,
    )
  {
    artifact_publication_manifest.RetryAllowed ->
      Some(
        "scherzoctl artifact publication retry --run "
        <> run_id
        <> " --publication "
        <> publication_id,
      )
    artifact_publication_manifest.RetryCannotReplan(_)
    | artifact_publication_manifest.RetryNotRetryable -> None
  }
}

fn abandon_instruction(
  run_id: String,
  publication_id: String,
  status: String,
  required: Bool,
) -> Option(String) {
  case required, status {
    True, "planned" | True, "failed" ->
      Some(
        "scherzoctl artifact publication abandon --run "
        <> run_id
        <> " --publication "
        <> publication_id
        <> " --reason <reason> --yes",
      )
    _, _ -> None
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
