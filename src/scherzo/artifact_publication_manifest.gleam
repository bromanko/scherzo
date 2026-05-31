import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/artifact_publication_planner
import scherzo/hash
import scherzo/path

pub const schema_version = 1

pub const artifact_type = "scherzo.artifact_publication_manifest.v1"

pub type PublicationStatus {
  Planned
  Failed
}

pub type PublicationErrorInfo {
  PublicationErrorInfo(code: String, message: String)
}

pub type PublicationManifest {
  PublicationManifest(
    run_id: String,
    workflow_id: String,
    publication_id: String,
    series_id: String,
    version_id: Option(String),
    attempt_id: String,
    status: PublicationStatus,
    required: Bool,
    retryable: Bool,
    retry_execution_available: Bool,
    generated_at_ms: Int,
    dry_run_manifest: Option(
      artifact_publication_planner.DryRunPublicationManifest,
    ),
    error: Option(PublicationErrorInfo),
  )
}

pub fn planned_manifest(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  generated_at_ms: Int,
) -> PublicationManifest {
  PublicationManifest(
    run_id: planned.run_id,
    workflow_id: planned.workflow_id,
    publication_id: planned.publication_id,
    series_id: planned.series_id,
    version_id: Some(planned.version_id),
    attempt_id: attempt_id,
    status: Planned,
    required: planned.required,
    retryable: False,
    retry_execution_available: False,
    generated_at_ms: generated_at_ms,
    dry_run_manifest: Some(planned),
    error: None,
  )
}

pub fn failed_manifest(
  run_id: String,
  workflow_id: String,
  publication_id: String,
  series_id: String,
  required: Bool,
  attempt_id: String,
  generated_at_ms: Int,
  error: PublicationErrorInfo,
) -> PublicationManifest {
  PublicationManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    publication_id: publication_id,
    series_id: series_id,
    version_id: None,
    attempt_id: attempt_id,
    status: Failed,
    required: required,
    retryable: True,
    retry_execution_available: False,
    generated_at_ms: generated_at_ms,
    dry_run_manifest: None,
    error: Some(error),
  )
}

pub fn attempt_key_for_success(version_id: String) -> String {
  version_id
}

pub fn attempt_key_for_failure(
  publication_id: String,
  error_code: String,
  error_message: String,
) -> String {
  "failed-"
  <> hash.sha256_hex(
    publication_id <> "|" <> error_code <> "|" <> error_message,
  )
}

pub fn manifest_ref(
  run_id: String,
  publication_id: String,
  attempt_key: String,
) -> String {
  path.join(
    path.join(path.join("runs", run_id), "publications/" <> publication_id),
    attempt_key <> ".json",
  )
}

pub fn status_to_string(status: PublicationStatus) -> String {
  case status {
    Planned -> "planned"
    Failed -> "failed"
  }
}

pub fn to_json(manifest: PublicationManifest) -> json.Json {
  json.object([
    #("schema_version", json.int(schema_version)),
    #("artifact_type", json.string(artifact_type)),
    #("run_id", json.string(manifest.run_id)),
    #("workflow_id", json.string(manifest.workflow_id)),
    #("publication_id", json.string(manifest.publication_id)),
    #("series_id", json.string(manifest.series_id)),
    #("version_id", option_string_to_json(manifest.version_id)),
    #("attempt_id", json.string(manifest.attempt_id)),
    #("status", json.string(status_to_string(manifest.status))),
    #("required", json.bool(manifest.required)),
    #("retryable", json.bool(manifest.retryable)),
    #(
      "retry_execution_available",
      json.bool(manifest.retry_execution_available),
    ),
    #("generated_at_ms", json.int(manifest.generated_at_ms)),
    #("dry_run_manifest", option_dry_run_to_json(manifest.dry_run_manifest)),
    #("error", option_error_to_json(manifest.error)),
  ])
}

pub fn to_string(manifest: PublicationManifest) -> String {
  manifest |> to_json |> json.to_string
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_dry_run_to_json(
  value: Option(artifact_publication_planner.DryRunPublicationManifest),
) -> json.Json {
  case value {
    Some(value) -> artifact_publication_planner.manifest_to_json(value)
    None -> json.null()
  }
}

fn option_error_to_json(value: Option(PublicationErrorInfo)) -> json.Json {
  case value {
    Some(PublicationErrorInfo(code, message)) ->
      json.object([
        #("code", json.string(code)),
        #("message", json.string(message)),
      ])
    None -> json.null()
  }
}
