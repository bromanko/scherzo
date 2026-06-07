import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_runtime
import scherzo/ctl/schedule_state
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_checkpoint

pub fn abandon(
  root: String,
  json_output: Bool,
  run_id: String,
  publication_id: String,
  reason: String,
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(schedule_state.load_projection(root, pair_error))
  use _ <- result.try(require_publication_run(projected, run_id))
  let attempts =
    projection.publication_attempts_for_run(projected, run_id, publication_id)
  use latest <- result.try(publication_or_not_found(attempts, publication_id))
  use manifest <- result.try(require_abandonable_commit_stack_attempt(
    root,
    latest,
  ))
  let checkpoint = workflow_checkpoint.ledger_writer(root, monotonic_ms)
  use abandoned <- result.try(record_abandoned_attempt(
    checkpoint,
    latest,
    manifest,
    reason,
  ))
  case json_output {
    True ->
      output_line(
        json.object([
          #("run_id", json.string(run_id)),
          #("publication_id", json.string(publication_id)),
          #("workspace_root", json.string(root)),
          #("attempt", publication_attempt_to_json(root, abandoned)),
          #(
            "retained_workspace_path",
            optional_string_json(retained_workspace_path(root, run_id)),
          ),
          #(
            "cleanup_release",
            json.string(
              "publication protection released; generic cleanup may delete the retained workspace when no other retention applies",
            ),
          ),
        ])
        |> json.to_string,
      )
    False ->
      print_abandon_result(root, run_id, publication_id, abandoned, output_line)
  }
  Ok(Nil)
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

fn require_abandonable_commit_stack_attempt(
  root: String,
  latest: projection.PublicationAttempt,
) -> Result(
  artifact_publication_manifest.PublicationManifest,
  #(String, String),
) {
  case latest.status {
    "planned" | "failed" ->
      case load_manifest_for_attempt(root, latest) {
        Ok(manifest) ->
          case
            artifact_publication_runtime.publication_manifest_is_commit_stack(
              manifest,
            ),
            latest.required
          {
            True, True -> Ok(manifest)
            False, _ ->
              Error(#(
                "publication_abandon_not_commit_stack",
                "publication abandonment only releases required same-repo commit_stack publication protection: "
                  <> latest.publication_id,
              ))
            _, False ->
              Error(#(
                "publication_abandon_not_required",
                "publication abandonment only releases required publication protection: "
                  <> latest.publication_id,
              ))
          }
        Error(error) -> Error(error)
      }
    "abandoned" ->
      Error(#(
        "publication_already_abandoned",
        "publication is already abandoned: " <> latest.publication_id,
      ))
    _ ->
      Error(#(
        "publication_abandon_not_allowed",
        "only pending or failed required same-repo commit_stack publications can be abandoned: "
          <> latest.publication_id
          <> " status="
          <> latest.status,
      ))
  }
}

fn load_manifest_for_attempt(
  root: String,
  attempt: projection.PublicationAttempt,
) -> Result(
  artifact_publication_manifest.PublicationManifest,
  #(String, String),
) {
  case attempt.manifest_ref {
    Some(ref) -> load_publication_manifest(root, ref)
    None ->
      Error(#(
        "publication_manifest_missing",
        "publication attempt has no retained manifest: "
          <> attempt.publication_id,
      ))
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

fn record_abandoned_attempt(
  checkpoint: workflow_checkpoint.Writer,
  latest: projection.PublicationAttempt,
  manifest: artifact_publication_manifest.PublicationManifest,
  reason: String,
) -> Result(projection.PublicationAttempt, #(String, String)) {
  let now_ms = checkpoint.now_ms()
  let attempt_id =
    artifact_publication_manifest.attempt_key_for_abandon(
      latest.publication_id,
      reason,
      now_ms,
    )
  let manifest =
    artifact_publication_manifest.abandoned_from_manifest(
      manifest,
      attempt_id,
      now_ms,
      reason,
    )
  let payload_json = artifact_publication_manifest.to_string(manifest)
  use written <- result.try(
    checkpoint.write_publication_manifest(
      workflow_checkpoint.WorkflowPublicationManifestWrite(
        run_id: latest.run_id,
        publication_id: latest.publication_id,
        attempt_key: attempt_id,
        payload_json: payload_json,
      ),
    )
    |> result.map_error(fn(error) {
      #(
        "publication_abandon_manifest_write_failed",
        workflow_checkpoint.describe_error(error),
      )
    }),
  )
  let ledger_record =
    record.with_id(
      publication_record_id(latest.run_id, latest.publication_id, attempt_id),
      now_ms,
      record.PublicationAttemptRecorded(
        run_id: latest.run_id,
        workflow_id: latest.workflow_id,
        publication_id: latest.publication_id,
        series_id: latest.series_id,
        attempt_id: attempt_id,
        status: "abandoned",
        required: latest.required,
        retryable: False,
        retry_execution_available: False,
        version_id: latest.version_id,
        manifest_ref: Some(written.ref),
        manifest_sha256: Some(written.sha256),
        manifest_bytes: Some(written.bytes),
        error_code: Some("publication_abandoned"),
        error_message: Some(reason),
      ),
    )
  use _ <- result.try(
    checkpoint.publication_attempt_recorded(ledger_record)
    |> result.map_error(fn(error) {
      #(
        "publication_abandon_record_failed",
        workflow_checkpoint.describe_error(error),
      )
    }),
  )
  Ok(projection.PublicationAttempt(
    run_id: latest.run_id,
    workflow_id: latest.workflow_id,
    publication_id: latest.publication_id,
    series_id: latest.series_id,
    attempt_id: attempt_id,
    status: "abandoned",
    required: latest.required,
    retryable: False,
    retry_execution_available: False,
    version_id: latest.version_id,
    manifest_ref: Some(written.ref),
    manifest_sha256: Some(written.sha256),
    manifest_bytes: Some(written.bytes),
    error_code: Some("publication_abandoned"),
    error_message: Some(reason),
    recorded_at_ms: now_ms,
  ))
}

fn publication_record_id(
  run_id: String,
  publication_id: String,
  attempt_key: String,
) -> String {
  "publication_attempt:"
  <> run_id
  <> ":"
  <> publication_id
  <> ":"
  <> attempt_key
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

fn print_abandon_result(
  root: String,
  run_id: String,
  publication_id: String,
  attempt: projection.PublicationAttempt,
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("run_id: " <> run_id)
  output_line("publication_id: " <> publication_id)
  output_line("status: " <> attempt.status)
  output_line("attempt_id: " <> attempt.attempt_id)
  output_line(
    "retained_workspace_path: "
    <> optional_string(retained_workspace_path(root, run_id)),
  )
  output_line(
    "cleanup_release: publication protection released; generic cleanup may delete the retained workspace when no other retention applies",
  )
}

fn publication_attempt_to_json(
  root: String,
  attempt: projection.PublicationAttempt,
) -> json.Json {
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
    #("manifest_ref", optional_string_json(attempt.manifest_ref)),
    #("manifest_sha256", optional_string_json(attempt.manifest_sha256)),
    #("manifest_bytes", optional_int_json(attempt.manifest_bytes)),
    #(
      "retained_workspace_path",
      optional_string_json(retained_workspace_path(root, attempt.run_id)),
    ),
    #("error_code", optional_string_json(attempt.error_code)),
    #("error_message", optional_string_json(attempt.error_message)),
    #("recorded_at_ms", json.int(attempt.recorded_at_ms)),
  ])
}

fn retained_workspace_path(root: String, run_id: String) -> Option(String) {
  case schedule_state.load_projection(root, pair_error) {
    Ok(projected) -> retained_workspace_path_from_projection(projected, run_id)
    Error(_) -> None
  }
}

fn retained_workspace_path_from_projection(
  projected: projection.Projection,
  run_id: String,
) -> Option(String) {
  case projection.workflow_run(projected, run_id) {
    Ok(status) ->
      artifact_publication_runtime.retained_workspace_path_from_run_root(
        workflow_status_run_root(status),
      )
    Error(Nil) -> None
  }
}

fn workflow_status_run_root(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(run_root: run_root, ..)
    | projection.WorkflowRunFinished(run_root: run_root, ..)
    | projection.WorkflowRunInterrupted(run_root: run_root, ..)
    | projection.WorkflowRunSuperseded(run_root: run_root, ..) -> run_root
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

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
