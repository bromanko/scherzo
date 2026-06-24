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
  Abandoned
}

pub type PublicationErrorInfo {
  PublicationErrorInfo(code: String, message: String)
}

pub type CleanupDiagnostics {
  CleanupDiagnostics(
    checkout_path: String,
    pre_cleanup_status: Option(String),
    reset_summary: Option(String),
    clean_summary: Option(String),
    post_cleanup_status: Option(String),
    cleanup_succeeded: Bool,
  )
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
    publication_mode: Option(String),
    required: Bool,
    retryable: Bool,
    retry_execution_available: Bool,
    generated_at_ms: Int,
    branch: Option(String),
    commit_sha: Option(String),
    pr_url: Option(String),
    pr_number: Option(Int),
    base_ref: Option(String),
    base_revision: Option(String),
    head_revision: Option(String),
    change_id: Option(String),
    selected_paths: List(String),
    changed_paths: List(String),
    removed_paths: List(String),
    dry_run_manifest: Option(
      artifact_publication_planner.DryRunPublicationManifest,
    ),
    error: Option(PublicationErrorInfo),
    cleanup_diagnostics: Option(CleanupDiagnostics),
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
    publication_mode: publication_mode_for_planned(planned),
    required: planned.required,
    retryable: False,
    retry_execution_available: False,
    generated_at_ms: generated_at_ms,
    branch: Some(planned.branch),
    commit_sha: None,
    pr_url: None,
    pr_number: planned_pr_number(planned),
    base_ref: planned.github_base,
    base_revision: None,
    head_revision: None,
    change_id: None,
    selected_paths: destination_paths(
      artifact_publication_planner.planned_files(planned),
    ),
    changed_paths: [],
    removed_paths: [],
    dry_run_manifest: Some(planned),
    error: None,
    cleanup_diagnostics: None,
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
    publication_mode: publication_mode_for_planned(planned),
    required: planned.required,
    retryable: False,
    retry_execution_available: True,
    generated_at_ms: generated_at_ms,
    branch: Some(planned.branch),
    commit_sha: Some(commit_sha),
    pr_url: pr_url,
    pr_number: planned_pr_number(planned),
    base_ref: planned.github_base,
    base_revision: None,
    head_revision: Some(commit_sha),
    change_id: None,
    selected_paths: destination_paths(
      artifact_publication_planner.planned_files(planned),
    ),
    changed_paths: changed_paths,
    removed_paths: removed_paths,
    dry_run_manifest: Some(planned),
    error: None,
    cleanup_diagnostics: None,
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
    publication_mode: publication_mode_for_planned(planned),
    required: planned.required,
    retryable: False,
    retry_execution_available: True,
    generated_at_ms: generated_at_ms,
    branch: Some(planned.branch),
    commit_sha: commit_sha,
    pr_url: pr_url,
    pr_number: planned_pr_number(planned),
    base_ref: planned.github_base,
    base_revision: None,
    head_revision: commit_sha,
    change_id: None,
    selected_paths: destination_paths(
      artifact_publication_planner.planned_files(planned),
    ),
    changed_paths: [],
    removed_paths: removed_paths,
    dry_run_manifest: Some(planned),
    error: None,
    cleanup_diagnostics: None,
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
    publication_mode: None,
    required: required,
    retryable: True,
    retry_execution_available: False,
    generated_at_ms: generated_at_ms,
    branch: None,
    commit_sha: None,
    pr_url: None,
    pr_number: None,
    base_ref: None,
    base_revision: None,
    head_revision: None,
    change_id: None,
    selected_paths: [],
    changed_paths: [],
    removed_paths: [],
    dry_run_manifest: None,
    error: Some(error),
    cleanup_diagnostics: None,
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
    publication_mode: publication_mode_for_planned(planned),
    required: planned.required,
    retryable: retryable,
    retry_execution_available: True,
    generated_at_ms: generated_at_ms,
    branch: branch,
    commit_sha: commit_sha,
    pr_url: pr_url,
    pr_number: planned_pr_number(planned),
    base_ref: planned.github_base,
    base_revision: None,
    head_revision: commit_sha,
    change_id: None,
    selected_paths: destination_paths(
      artifact_publication_planner.planned_files(planned),
    ),
    changed_paths: changed_paths,
    removed_paths: removed_paths,
    dry_run_manifest: Some(planned),
    error: Some(error),
    cleanup_diagnostics: None,
  )
}

pub fn abandoned_from_manifest(
  manifest: PublicationManifest,
  attempt_id: String,
  generated_at_ms: Int,
  reason: String,
) -> PublicationManifest {
  PublicationManifest(
    ..manifest,
    attempt_id: attempt_id,
    status: Abandoned,
    retryable: False,
    retry_execution_available: False,
    generated_at_ms: generated_at_ms,
    error: Some(PublicationErrorInfo(
      code: "publication_abandoned",
      message: reason,
    )),
    cleanup_diagnostics: None,
  )
}

pub fn with_cleanup_diagnostics(
  manifest: PublicationManifest,
  cleanup_diagnostics: CleanupDiagnostics,
) -> PublicationManifest {
  PublicationManifest(
    ..manifest,
    cleanup_diagnostics: Some(cleanup_diagnostics),
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

pub fn attempt_key_for_abandon(
  publication_id: String,
  reason: String,
  generated_at_ms: Int,
) -> String {
  "abandoned-"
  <> hash.sha256_hex(
    publication_id <> "|" <> reason <> "|" <> int.to_string(generated_at_ms),
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
    Abandoned -> "abandoned"
  }
}

pub fn status_from_string(value: String) -> Result(PublicationStatus, Nil) {
  case value {
    "planned" -> Ok(Planned)
    "published" -> Ok(Published)
    "unchanged" -> Ok(Unchanged)
    "failed" -> Ok(Failed)
    "abandoned" -> Ok(Abandoned)
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
    #("publication_mode", option_string_to_json(manifest.publication_mode)),
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
    #("pr_number", option_int_to_json(manifest.pr_number)),
    #("base_ref", option_string_to_json(manifest.base_ref)),
    #("base_revision", option_string_to_json(manifest.base_revision)),
    #("head_revision", option_string_to_json(manifest.head_revision)),
    #("change_id", option_string_to_json(manifest.change_id)),
    #("selected_paths", json.array(manifest.selected_paths, json.string)),
    #("changed_paths", json.array(manifest.changed_paths, json.string)),
    #("removed_paths", json.array(manifest.removed_paths, json.string)),
    #("dry_run_manifest", option_dry_run_to_json(manifest.dry_run_manifest)),
    #("error", option_error_to_json(manifest.error)),
    #(
      "cleanup_diagnostics",
      option_cleanup_diagnostics_to_json(manifest.cleanup_diagnostics),
    ),
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

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn publication_mode_for_planned(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Option(String) {
  Some(artifact_publication_planner.planned_publication_mode(planned))
}

fn planned_pr_number(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Option(Int) {
  case planned.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(target) ->
      case target.pr_number > 0 {
        True -> Some(target.pr_number)
        False -> None
      }
    artifact_publication_planner.StableBranchTargetPlan -> None
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

fn option_cleanup_diagnostics_to_json(
  value: Option(CleanupDiagnostics),
) -> json.Json {
  case value {
    Some(CleanupDiagnostics(
      checkout_path,
      pre_cleanup_status,
      reset_summary,
      clean_summary,
      post_cleanup_status,
      cleanup_succeeded,
    )) ->
      json.object([
        #("checkout_path", json.string(checkout_path)),
        #("pre_cleanup_status", option_string_to_json(pre_cleanup_status)),
        #("reset_summary", option_string_to_json(reset_summary)),
        #("clean_summary", option_string_to_json(clean_summary)),
        #("post_cleanup_status", option_string_to_json(post_cleanup_status)),
        #("cleanup_succeeded", json.bool(cleanup_succeeded)),
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
  use publication_mode <- decode.optional_field(
    "publication_mode",
    None,
    decode.optional(decode.string),
  )
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
  use pr_number <- decode.optional_field(
    "pr_number",
    None,
    decode.optional(decode.int),
  )
  use base_ref <- decode.optional_field(
    "base_ref",
    None,
    decode.optional(decode.string),
  )
  use base_revision <- decode.optional_field(
    "base_revision",
    None,
    decode.optional(decode.string),
  )
  use head_revision <- decode.optional_field(
    "head_revision",
    None,
    decode.optional(decode.string),
  )
  use change_id <- decode.optional_field(
    "change_id",
    None,
    decode.optional(decode.string),
  )
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
  use cleanup_diagnostics <- decode.optional_field(
    "cleanup_diagnostics",
    None,
    decode.optional(cleanup_diagnostics_decoder()),
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
    publication_mode: publication_mode,
    required: required,
    retryable: retryable,
    retry_execution_available: retry_execution_available,
    generated_at_ms: generated_at_ms,
    branch: branch,
    commit_sha: commit_sha,
    pr_url: pr_url,
    pr_number: pr_number,
    base_ref: base_ref,
    base_revision: base_revision,
    head_revision: head_revision,
    change_id: change_id,
    selected_paths: selected_paths,
    changed_paths: changed_paths,
    removed_paths: removed_paths,
    dry_run_manifest: dry_run_manifest,
    error: error,
    cleanup_diagnostics: cleanup_diagnostics,
  ))
}

fn cleanup_diagnostics_decoder() -> decode.Decoder(CleanupDiagnostics) {
  use checkout_path <- decode.field("checkout_path", decode.string)
  use pre_cleanup_status <- decode.optional_field(
    "pre_cleanup_status",
    None,
    decode.optional(decode.string),
  )
  use reset_summary <- decode.optional_field(
    "reset_summary",
    None,
    decode.optional(decode.string),
  )
  use clean_summary <- decode.optional_field(
    "clean_summary",
    None,
    decode.optional(decode.string),
  )
  use post_cleanup_status <- decode.optional_field(
    "post_cleanup_status",
    None,
    decode.optional(decode.string),
  )
  use cleanup_succeeded <- decode.field("cleanup_succeeded", decode.bool)
  decode.success(CleanupDiagnostics(
    checkout_path: checkout_path,
    pre_cleanup_status: pre_cleanup_status,
    reset_summary: reset_summary,
    clean_summary: clean_summary,
    post_cleanup_status: post_cleanup_status,
    cleanup_succeeded: cleanup_succeeded,
  ))
}

fn error_decoder() -> decode.Decoder(PublicationErrorInfo) {
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  decode.success(PublicationErrorInfo(code: code, message: message))
}
