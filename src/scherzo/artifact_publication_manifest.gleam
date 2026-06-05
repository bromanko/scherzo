import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_planner_decode
import scherzo/hash
import scherzo/path

pub const schema_version = 1

pub const artifact_type = "scherzo.artifact_publication_manifest.v1"

pub type PublicationStatus {
  Planned
  Published
  Unchanged
  Failed
}

pub type PublicationErrorInfo {
  PublicationErrorInfo(code: String, message: String)
}

pub type RetryEligibility {
  RetryAllowed
  RetryNotRetryable
  RetryCannotReplan(reason: String)
}

pub fn retry_eligibility_for_attempt(
  status: String,
  retryable retryable: Bool,
  retry_execution_available retry_execution_available: Bool,
  version_id version_id: Option(String),
) -> RetryEligibility {
  case status, retryable, retry_execution_available, version_id {
    "failed", True, True, _ -> RetryAllowed
    "failed", True, False, None -> RetryAllowed
    "failed", True, False, Some(_) ->
      RetryCannotReplan(
        "failed attempt has a version_id but no retry execution metadata",
      )
    "unchanged", _, True, _ -> RetryAllowed
    _, _, _, _ -> RetryNotRetryable
  }
}

pub fn retry_replan_unavailable_error(
  publication_id: String,
  reason: String,
) -> #(String, String) {
  #(
    "publication_retry_replan_unavailable",
    "latest publication attempt is marked retryable but cannot be safely re-planned from retained outputs: "
      <> publication_id
      <> " ("
      <> reason
      <> ")",
  )
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
    branch: Option(String),
    commit_sha: Option(String),
    pr_url: Option(String),
    selected_paths: List(String),
    changed_paths: List(String),
    removed_paths: List(String),
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
    branch: Some(planned.branch),
    commit_sha: None,
    pr_url: None,
    selected_paths: destination_paths(planned.files),
    changed_paths: [],
    removed_paths: [],
    dry_run_manifest: Some(planned),
    error: None,
  )
}

pub fn published_manifest(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  generated_at_ms: Int,
  commit_sha: String,
  pr_url: Option(String),
  changed_paths: List(String),
  removed_paths: List(String),
) -> PublicationManifest {
  PublicationManifest(
    run_id: planned.run_id,
    workflow_id: planned.workflow_id,
    publication_id: planned.publication_id,
    series_id: planned.series_id,
    version_id: Some(planned.version_id),
    attempt_id: attempt_id,
    status: Published,
    required: planned.required,
    retryable: False,
    retry_execution_available: True,
    generated_at_ms: generated_at_ms,
    branch: Some(planned.branch),
    commit_sha: Some(commit_sha),
    pr_url: pr_url,
    selected_paths: destination_paths(planned.files),
    changed_paths: changed_paths,
    removed_paths: removed_paths,
    dry_run_manifest: Some(planned),
    error: None,
  )
}

pub fn unchanged_manifest(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  generated_at_ms: Int,
  commit_sha: Option(String),
  pr_url: Option(String),
  removed_paths: List(String),
) -> PublicationManifest {
  PublicationManifest(
    run_id: planned.run_id,
    workflow_id: planned.workflow_id,
    publication_id: planned.publication_id,
    series_id: planned.series_id,
    version_id: Some(planned.version_id),
    attempt_id: attempt_id,
    status: Unchanged,
    required: planned.required,
    retryable: False,
    retry_execution_available: True,
    generated_at_ms: generated_at_ms,
    branch: Some(planned.branch),
    commit_sha: commit_sha,
    pr_url: pr_url,
    selected_paths: destination_paths(planned.files),
    changed_paths: [],
    removed_paths: removed_paths,
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
    branch: None,
    commit_sha: None,
    pr_url: None,
    selected_paths: [],
    changed_paths: [],
    removed_paths: [],
    dry_run_manifest: None,
    error: Some(error),
  )
}

pub fn failed_from_planned_manifest(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  generated_at_ms: Int,
  retryable: Bool,
  branch: Option(String),
  commit_sha: Option(String),
  pr_url: Option(String),
  changed_paths: List(String),
  removed_paths: List(String),
  error: PublicationErrorInfo,
) -> PublicationManifest {
  PublicationManifest(
    run_id: planned.run_id,
    workflow_id: planned.workflow_id,
    publication_id: planned.publication_id,
    series_id: planned.series_id,
    version_id: Some(planned.version_id),
    attempt_id: attempt_id,
    status: Failed,
    required: planned.required,
    retryable: retryable,
    retry_execution_available: True,
    generated_at_ms: generated_at_ms,
    branch: branch,
    commit_sha: commit_sha,
    pr_url: pr_url,
    selected_paths: destination_paths(planned.files),
    changed_paths: changed_paths,
    removed_paths: removed_paths,
    dry_run_manifest: Some(planned),
    error: Some(error),
  )
}

pub fn attempt_key_for_success(version_id: String) -> String {
  version_id
}

pub fn attempt_key_for_success_recovery(
  publication_id: String,
  version_id: String,
  generated_at_ms: Int,
) -> String {
  "recovered-"
  <> hash.sha256_hex(
    publication_id <> "|" <> version_id <> "|" <> int.to_string(generated_at_ms),
  )
}

pub fn attempt_key_for_failure(
  publication_id: String,
  error_code: String,
  error_message: String,
  generated_at_ms: Int,
) -> String {
  "failed-"
  <> hash.sha256_hex(
    publication_id
    <> "|"
    <> error_code
    <> "|"
    <> error_message
    <> "|"
    <> int.to_string(generated_at_ms),
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
    Published -> "published"
    Unchanged -> "unchanged"
    Failed -> "failed"
  }
}

pub fn status_from_string(value: String) -> Result(PublicationStatus, Nil) {
  case value {
    "planned" -> Ok(Planned)
    "published" -> Ok(Published)
    "unchanged" -> Ok(Unchanged)
    "failed" -> Ok(Failed)
    _ -> Error(Nil)
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
    #("branch", option_string_to_json(manifest.branch)),
    #("commit_sha", option_string_to_json(manifest.commit_sha)),
    #("pr_url", option_string_to_json(manifest.pr_url)),
    #("selected_paths", json.array(manifest.selected_paths, json.string)),
    #("changed_paths", json.array(manifest.changed_paths, json.string)),
    #("removed_paths", json.array(manifest.removed_paths, json.string)),
    #("dry_run_manifest", option_dry_run_to_json(manifest.dry_run_manifest)),
    #("error", option_error_to_json(manifest.error)),
  ])
}

pub fn to_string(manifest: PublicationManifest) -> String {
  manifest |> to_json |> json.to_string
}

fn destination_paths(
  files: List(artifact_publication_planner.PlannedPublicationFile),
) -> List(String) {
  files
  |> list.map(fn(file) {
    let artifact_publication_planner.PlannedPublicationFile(_, destination_path) =
      file
    destination_path
  })
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

pub fn decode_manifest_json(
  payload_json: String,
) -> Result(PublicationManifest, String) {
  json.parse(payload_json, manifest_decoder())
  |> result.map_error(fn(_) { "invalid_publication_attempt_manifest_json" })
}

fn manifest_decoder() -> decode.Decoder(PublicationManifest) {
  use schema_version <- decode.field("schema_version", decode.int)
  use artifact <- decode.field("artifact_type", decode.string)
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use publication_id <- decode.field("publication_id", decode.string)
  use series_id <- decode.field("series_id", decode.string)
  use version_id <- decode.field("version_id", decode.optional(decode.string))
  use attempt_id <- decode.field("attempt_id", decode.string)
  use status_text <- decode.field("status", decode.string)
  use required <- decode.field("required", decode.bool)
  use retryable <- decode.field("retryable", decode.bool)
  use retry_execution_available <- decode.field(
    "retry_execution_available",
    decode.bool,
  )
  use generated_at_ms <- decode.field("generated_at_ms", decode.int)
  use branch <- decode.field("branch", decode.optional(decode.string))
  use commit_sha <- decode.field("commit_sha", decode.optional(decode.string))
  use pr_url <- decode.field("pr_url", decode.optional(decode.string))
  use selected_paths <- decode.field(
    "selected_paths",
    decode.list(decode.string),
  )
  use changed_paths <- decode.field("changed_paths", decode.list(decode.string))
  use removed_paths <- decode.field("removed_paths", decode.list(decode.string))
  use dry_run_manifest <- decode.optional_field(
    "dry_run_manifest",
    None,
    decode.optional(
      artifact_publication_planner_decode.dry_run_manifest_decoder(),
    ),
  )
  use error <- decode.optional_field(
    "error",
    None,
    decode.optional(error_decoder()),
  )
  let _ = schema_version
  let _ = artifact
  let status = case status_from_string(status_text) {
    Ok(status) -> status
    Error(_) -> Failed
  }
  decode.success(PublicationManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    publication_id: publication_id,
    series_id: series_id,
    version_id: version_id,
    attempt_id: attempt_id,
    status: status,
    required: required,
    retryable: retryable,
    retry_execution_available: retry_execution_available,
    generated_at_ms: generated_at_ms,
    branch: branch,
    commit_sha: commit_sha,
    pr_url: pr_url,
    selected_paths: selected_paths,
    changed_paths: changed_paths,
    removed_paths: removed_paths,
    dry_run_manifest: dry_run_manifest,
    error: error,
  ))
}

fn error_decoder() -> decode.Decoder(PublicationErrorInfo) {
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  decode.success(PublicationErrorInfo(code: code, message: message))
}
