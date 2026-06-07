import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection

pub fn existing_terminal_attempt(
  workspace_root: String,
  run_id: String,
  publication_id: String,
  version_id: String,
  recovered_execution: Bool,
  requires_pr: Bool,
  requires_branch_metadata: Bool,
) -> Option(artifact_publication_recording.PublicationAttemptSummary) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(error) -> none_from_error(error)
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(error) -> none_from_error(error)
        Ok(projected) -> {
          let attempt =
            projection.publication_attempts_for_run(
              projected,
              run_id,
              publication_id,
            )
            |> latest_matching_terminal(version_id, recovered_execution, None)
          case attempt {
            Some(attempt) ->
              case
                terminal_attempt_is_complete(
                  workspace_root,
                  attempt,
                  requires_pr,
                  requires_branch_metadata,
                )
              {
                True -> Some(attempt)
                False -> None
              }
            None -> None
          }
        }
      }
  }
}

pub fn planned_requires_pr(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Bool {
  case planned.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(_) -> True
    artifact_publication_planner.StableBranchTargetPlan ->
      planned.pull_request.enabled
  }
}

pub fn terminal_attempt_reuse_allowed(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Bool {
  case planned.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(_) -> False
    artifact_publication_planner.StableBranchTargetPlan -> True
  }
}

pub fn success_attempt_id(
  workspace_root: String,
  run_id: String,
  publication_id: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  now_ms: Int,
) -> String {
  case
    prior_terminal_success_attempt_exists(
      workspace_root,
      run_id,
      publication_id,
      planned.version_id,
    )
  {
    True ->
      artifact_publication_manifest.attempt_key_for_success_recovery(
        planned.publication_id,
        planned.version_id,
        now_ms,
      )
    False ->
      artifact_publication_manifest.attempt_key_for_success(planned.version_id)
  }
}

pub fn failure_from_attempt(
  attempt: artifact_publication_recording.PublicationAttemptSummary,
) -> Option(artifact_publication_recording.PublicationFailure) {
  case attempt.status, attempt.error_code, attempt.error_message {
    "failed", Some(code), Some(message) ->
      Some(artifact_publication_recording.PublicationFailure(
        publication_id: attempt.publication_id,
        code: code,
        message: message,
        required: attempt.required,
      ))
    _, _, _ -> None
  }
}

fn terminal_attempt_is_complete(
  workspace_root: String,
  attempt: artifact_publication_recording.PublicationAttemptSummary,
  requires_pr: Bool,
  requires_branch_metadata: Bool,
) -> Bool {
  case attempt.status {
    "failed" -> True
    _ ->
      terminal_success_is_complete(
        workspace_root,
        attempt,
        requires_pr,
        requires_branch_metadata,
      )
  }
}

fn terminal_success_is_complete(
  workspace_root: String,
  attempt: artifact_publication_recording.PublicationAttemptSummary,
  requires_pr: Bool,
  requires_branch_metadata: Bool,
) -> Bool {
  case load_attempt_details(workspace_root, attempt.manifest_ref) {
    Some(#(attempt_requires_pr, branch, commit_sha, pr_url)) ->
      terminal_pr_complete(requires_pr || attempt_requires_pr, pr_url)
      && terminal_branch_complete(requires_branch_metadata, branch, commit_sha)
    None -> !requires_pr && !requires_branch_metadata
  }
}

fn terminal_pr_complete(requires_pr: Bool, pr_url: Option(String)) -> Bool {
  case requires_pr, pr_url {
    False, _ -> True
    True, Some(_) -> True
    True, None -> False
  }
}

fn terminal_branch_complete(
  requires_branch_metadata: Bool,
  branch: Option(String),
  commit_sha: Option(String),
) -> Bool {
  case requires_branch_metadata, branch, commit_sha {
    False, _, _ -> True
    True, Some(_), Some(_) -> True
    True, _, _ -> False
  }
}

fn load_attempt_details(
  workspace_root: String,
  ref: String,
) -> Option(#(Bool, Option(String), Option(String), Option(String))) {
  let store = artifact_store.new(workspace_root)
  case artifact_store.read_artifact_unverified(store, ref) {
    Ok(contents) ->
      case artifact_publication_manifest.decode_manifest_json(contents) {
        Ok(manifest) ->
          Some(#(
            manifest_requires_pr(manifest),
            manifest.branch,
            manifest.commit_sha,
            manifest.pr_url,
          ))
        Error(decode_error) -> none_from_error(decode_error)
      }
    Error(read_error) -> none_from_error(read_error)
  }
}

fn prior_terminal_success_attempt_exists(
  workspace_root: String,
  run_id: String,
  publication_id: String,
  version_id: String,
) -> Bool {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(error) -> false_from_error(error)
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(error) -> false_from_error(error)
        Ok(projected) ->
          projection.publication_attempts_for_run(
            projected,
            run_id,
            publication_id,
          )
          |> list.any(fn(attempt) {
            attempt.version_id == Some(version_id)
            && {
              attempt.status == "published" || attempt.status == "unchanged"
            }
          })
      }
  }
}

fn manifest_requires_pr(
  manifest: artifact_publication_manifest.PublicationManifest,
) -> Bool {
  case manifest.dry_run_manifest {
    Some(planned) -> planned_requires_pr(planned)
    None -> False
  }
}

fn latest_matching_terminal(
  attempts: List(projection.PublicationAttempt),
  version_id: String,
  recovered_execution: Bool,
  best: Option(projection.PublicationAttempt),
) -> Option(artifact_publication_recording.PublicationAttemptSummary) {
  case attempts {
    [] -> option.map(best, projection_attempt_to_summary)
    [attempt, ..rest] -> {
      let terminal =
        attempt.status == "published"
        || attempt.status == "unchanged"
        || { recovered_execution && attempt.status == "failed" }
      let next_best = case
        attempt.version_id == Some(version_id),
        terminal,
        best
      {
        True, True, None -> Some(attempt)
        True, True, Some(existing)
          if attempt.recorded_at_ms >= existing.recorded_at_ms
        -> Some(attempt)
        _, _, _ -> best
      }
      latest_matching_terminal(rest, version_id, recovered_execution, next_best)
    }
  }
}

fn projection_attempt_to_summary(
  attempt: projection.PublicationAttempt,
) -> artifact_publication_recording.PublicationAttemptSummary {
  artifact_publication_recording.PublicationAttemptSummary(
    publication_id: attempt.publication_id,
    series_id: attempt.series_id,
    attempt_id: attempt.attempt_id,
    status: attempt.status,
    required: attempt.required,
    retryable: attempt.retryable,
    retry_execution_available: attempt.retry_execution_available,
    version_id: attempt.version_id,
    manifest_ref: option_string_or(attempt.manifest_ref, ""),
    manifest_sha256: option_string_or(attempt.manifest_sha256, ""),
    manifest_bytes: option_int_or(attempt.manifest_bytes, 0),
    error_code: attempt.error_code,
    error_message: attempt.error_message,
    recorded_at_ms: attempt.recorded_at_ms,
  )
}

fn option_string_or(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn option_int_or(value: Option(Int), default: Int) -> Int {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn none_from_error(_error: a) -> Option(b) {
  None
}

fn false_from_error(_error: a) -> Bool {
  False
}
