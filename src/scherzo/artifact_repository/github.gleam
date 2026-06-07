import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_repository/checkout_lock
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/github_cli
import scherzo/artifact_repository/github_paths
import scherzo/artifact_repository/types
import scherzo/commit_stack_artifact
import scherzo/hash
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import simplifile

pub type PublishError {
  PublishError(code: String, message: String)
}

type CheckoutFailure {
  CheckoutFailure(
    retryable: Bool,
    error: artifact_publication_manifest.PublicationErrorInfo,
    cleanup_diagnostics: Option(
      artifact_publication_manifest.CleanupDiagnostics,
    ),
  )
}

type CleanupCommandResult {
  CleanupCommandResult(summary: String, succeeded: Bool)
}

pub fn code(error: PublishError) -> String {
  let PublishError(code: code, ..) = error
  code
}

pub fn message(error: PublishError) -> String {
  let PublishError(message: message, ..) = error
  message
}

pub fn prepare_publication_input(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  store: artifact_store.Store,
) -> Result(types.PublicationExecutionInput, PublishError) {
  case manifest.commit_stack {
    Some(stack) -> {
      use selected_stack <- result.try(load_commit_stack(stack, store))
      Ok(types.PublicationExecutionInput(manifest, [], Some(selected_stack)))
    }
    None -> {
      use selected_files <- result.try(
        load_selected_files(manifest.files, store, []),
      )
      Ok(types.PublicationExecutionInput(
        manifest,
        list.reverse(selected_files),
        None,
      ))
    }
  }
}

pub fn publish(
  input: types.PublicationExecutionInput,
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
) -> artifact_publication_manifest.PublicationManifest {
  let types.PublicationExecutionInput(manifest, selected_files, selected_stack) =
    input
  let base_success_attempt_id =
    artifact_publication_manifest.attempt_key_for_success(manifest.version_id)
  let latest = latest_success_details(workspace_root, manifest.series_id)
  case selected_stack {
    Some(stack) ->
      publish_commit_stack(
        manifest,
        stack,
        latest,
        workspace_root,
        runner,
        now_ms,
        base_success_attempt_id,
      )
    None ->
      case validate_selected_files(selected_files, []) {
        Error(error) ->
          failed_manifest(
            manifest,
            now_ms,
            False,
            Some(manifest.branch),
            None,
            latest_pr_url(latest),
            [],
            [],
            error,
          )
        Ok(Nil) -> {
          let removed_paths = stale_paths(latest, manifest.files)
          case
            latest_version_id(latest) == Some(manifest.version_id)
            && latest_success_is_complete(latest, manifest)
          {
            True ->
              artifact_publication_manifest.unchanged_manifest(
                manifest,
                base_success_attempt_id,
                now_ms,
                latest_commit_sha(latest),
                latest_pr_url(latest),
                removed_paths,
              )
            False ->
              publish_with_checkout(
                manifest,
                selected_files,
                removed_paths,
                workspace_root,
                runner,
                now_ms,
                success_attempt_id_for_execution(latest, manifest, now_ms),
              )
          }
        }
      }
  }
}

fn latest_success_is_complete(
  latest: Option(types.LatestPublicationDetails),
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> Bool {
  case latest {
    Some(details) ->
      option_present(details.branch)
      && option_present(details.commit_sha)
      && case manifest_requires_pr(manifest) {
        True -> option_present(details.pr_url)
        False -> True
      }
    None -> False
  }
}

fn option_present(value: Option(a)) -> Bool {
  case value {
    Some(_) -> True
    None -> False
  }
}

fn manifest_requires_pr(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> Bool {
  case manifest.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(_) -> True
    artifact_publication_planner.StableBranchTargetPlan ->
      manifest.pull_request.enabled
  }
}

fn success_attempt_id_for_execution(
  latest: Option(types.LatestPublicationDetails),
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  now_ms: Int,
) -> String {
  case
    latest_version_id(latest) == Some(manifest.version_id),
    latest_success_is_complete(latest, manifest)
  {
    True, False ->
      artifact_publication_manifest.attempt_key_for_success_recovery(
        manifest.publication_id,
        manifest.version_id,
        now_ms,
      )
    _, _ ->
      artifact_publication_manifest.attempt_key_for_success(manifest.version_id)
  }
}

fn current_head_or_error(
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  case github_cli.current_head(checkout_dir, runner) {
    Some(commit_sha) -> Ok(commit_sha)
    None ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "rev_parse_failed",
        message: "git rev-parse HEAD produced no commit sha",
      ))
  }
}

fn failed_manifest(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  now_ms: Int,
  retryable: Bool,
  branch: Option(String),
  commit_sha: Option(String),
  pr_url: Option(String),
  changed_paths: List(String),
  removed_paths: List(String),
  error: artifact_publication_manifest.PublicationErrorInfo,
) -> artifact_publication_manifest.PublicationManifest {
  failed_manifest_with_cleanup(
    manifest,
    now_ms,
    retryable,
    branch,
    commit_sha,
    pr_url,
    changed_paths,
    removed_paths,
    error,
    None,
  )
}

fn failed_manifest_with_cleanup(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  now_ms: Int,
  retryable: Bool,
  branch: Option(String),
  commit_sha: Option(String),
  pr_url: Option(String),
  changed_paths: List(String),
  removed_paths: List(String),
  error: artifact_publication_manifest.PublicationErrorInfo,
  cleanup_diagnostics: Option(artifact_publication_manifest.CleanupDiagnostics),
) -> artifact_publication_manifest.PublicationManifest {
  let attempt_id =
    artifact_publication_manifest.attempt_key_for_failure(
      manifest.publication_id,
      error.code,
      error.message,
      now_ms,
    )
  let manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      manifest,
      attempt_id,
      now_ms,
      retryable,
      branch,
      commit_sha,
      pr_url,
      changed_paths,
      removed_paths,
      error,
    )
  case cleanup_diagnostics {
    Some(cleanup_diagnostics) ->
      artifact_publication_manifest.with_cleanup_diagnostics(
        manifest,
        cleanup_diagnostics,
      )
    None -> manifest
  }
}

fn with_checkout_lock(
  checkout_dir: String,
  now_ms: Int,
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  lock_failure_pr_url: Option(String),
  removed_paths: List(String),
  run: fn() -> artifact_publication_manifest.PublicationManifest,
) -> artifact_publication_manifest.PublicationManifest {
  case checkout_lock.acquire(checkout_dir) {
    Ok(lock) -> {
      let result = run()
      let _ = checkout_lock.release(lock)
      result
    }
    Error(error) ->
      failed_manifest(
        manifest,
        now_ms,
        True,
        Some(manifest.branch),
        None,
        lock_failure_pr_url,
        [],
        removed_paths,
        artifact_publication_manifest.PublicationErrorInfo(
          code: "publication_lock_failed",
          message: checkout_lock.error_message(error),
        ),
      )
  }
}

fn cleanup_checkout(
  checkout_dir: String,
  runner: command_runner.Runner,
  pre_cleanup_status: Option(String),
) -> artifact_publication_manifest.CleanupDiagnostics {
  let reset =
    run_cleanup_command(
      runner,
      command_runner.sh("git", ["reset", "--hard", "HEAD"], checkout_dir),
    )
  let clean =
    run_cleanup_command(
      runner,
      command_runner.sh("git", ["clean", "-fd"], checkout_dir),
    )
  let post_status_result = status_snapshot(checkout_dir, runner)
  let post_status = case post_status_result {
    Ok(status) -> Some(status)
    Error(error) -> Some("status_failed:" <> error.message)
  }
  let cleanup_succeeded =
    reset.succeeded
    && clean.succeeded
    && status_snapshot_result_is_clean(post_status_result)
  artifact_publication_manifest.CleanupDiagnostics(
    checkout_path: checkout_dir,
    pre_cleanup_status: pre_cleanup_status,
    reset_summary: Some(reset.summary),
    clean_summary: Some(clean.summary),
    post_cleanup_status: post_status,
    cleanup_succeeded: cleanup_succeeded,
  )
}

fn status_snapshot(
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(String, artifact_publication_manifest.PublicationErrorInfo) {
  github_cli.run_stdout(
    runner,
    command_runner.sh("git", ["status", "--porcelain"], checkout_dir),
    True,
  )
}

fn status_snapshot_result_is_clean(
  snapshot: Result(String, artifact_publication_manifest.PublicationErrorInfo),
) -> Bool {
  case snapshot {
    Ok(snapshot) -> snapshot == ""
    Error(_) -> False
  }
}

fn run_cleanup_command(
  runner: command_runner.Runner,
  spec: command_runner.CommandSpec,
) -> CleanupCommandResult {
  case github_cli.run(runner, spec, True) {
    Ok(output) ->
      CleanupCommandResult(
        summary: command_runner.summarize(output),
        succeeded: output.exit_code == 0,
      )
    Error(error) ->
      CleanupCommandResult(
        summary: "spawn_failed:" <> error.message,
        succeeded: False,
      )
  }
}

fn publish_with_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  selected_files: List(types.SelectedArtifactBytes),
  removed_paths: List(String),
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
  attempt_id: String,
) -> artifact_publication_manifest.PublicationManifest {
  let checkout_dir = github_paths.checkout_dir(workspace_root, manifest)
  with_checkout_lock(checkout_dir, now_ms, manifest, None, removed_paths, fn() {
    case ensure_checkout(manifest, checkout_dir, runner) {
      Error(CheckoutFailure(retryable, error, cleanup_diagnostics)) ->
        failed_manifest_with_cleanup(
          manifest,
          now_ms,
          retryable,
          Some(manifest.branch),
          None,
          None,
          [],
          removed_paths,
          error,
          cleanup_diagnostics,
        )
      Ok(Nil) -> {
        let phase_result = case
          materialize_selected_files(
            checkout_dir,
            selected_files,
            removed_paths,
          )
        {
          Error(error) ->
            failed_manifest(
              manifest,
              now_ms,
              False,
              Some(manifest.branch),
              None,
              None,
              [],
              removed_paths,
              error,
            )
          Ok(changed_paths) ->
            case
              stage_and_diff(
                checkout_dir,
                manifest,
                changed_paths,
                removed_paths,
                runner,
              )
            {
              Error(#(retryable, error)) ->
                failed_manifest(
                  manifest,
                  now_ms,
                  retryable,
                  Some(manifest.branch),
                  None,
                  None,
                  changed_paths,
                  removed_paths,
                  error,
                )
              Ok(False) ->
                unchanged_after_no_diff(
                  manifest,
                  checkout_dir,
                  runner,
                  now_ms,
                  attempt_id,
                  removed_paths,
                )
              Ok(True) ->
                commit_push_and_pr(
                  manifest,
                  checkout_dir,
                  runner,
                  now_ms,
                  attempt_id,
                  changed_paths,
                  removed_paths,
                )
            }
        }
        case phase_result.status {
          artifact_publication_manifest.Failed -> {
            let cleanup_diagnostics =
              cleanup_checkout(checkout_dir, runner, None)
            artifact_publication_manifest.with_cleanup_diagnostics(
              phase_result,
              cleanup_diagnostics,
            )
          }
          _ -> phase_result
        }
      }
    }
  })
}

fn unchanged_after_no_diff(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
  now_ms: Int,
  attempt_id: String,
  removed_paths: List(String),
) -> artifact_publication_manifest.PublicationManifest {
  case current_head_or_error(checkout_dir, runner) {
    Error(error) ->
      failed_manifest(
        manifest,
        now_ms,
        True,
        Some(manifest.branch),
        None,
        None,
        [],
        removed_paths,
        error,
      )
    Ok(commit_sha) ->
      case manifest.pull_request.enabled {
        False ->
          artifact_publication_manifest.unchanged_manifest(
            manifest,
            attempt_id,
            now_ms,
            Some(commit_sha),
            None,
            removed_paths,
          )
        True ->
          case github_cli.ensure_pull_request(manifest, checkout_dir, runner) {
            Ok(pr_url) ->
              artifact_publication_manifest.unchanged_manifest(
                manifest,
                attempt_id,
                now_ms,
                Some(commit_sha),
                pr_url,
                removed_paths,
              )
            Error(error) ->
              failed_manifest(
                manifest,
                now_ms,
                True,
                Some(manifest.branch),
                Some(commit_sha),
                github_cli.existing_pr_url(manifest, checkout_dir, runner),
                [],
                removed_paths,
                error,
              )
          }
      }
  }
}

fn commit_push_and_pr(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
  now_ms: Int,
  attempt_id: String,
  changed_paths: List(String),
  removed_paths: List(String),
) -> artifact_publication_manifest.PublicationManifest {
  let message = commit_message(manifest)
  case
    github_cli.run_ok(
      runner,
      command_runner.sh("git", ["commit", "-m", message], checkout_dir),
      True,
    )
  {
    Error(error) ->
      failed_manifest(
        manifest,
        now_ms,
        True,
        Some(manifest.branch),
        None,
        None,
        changed_paths,
        removed_paths,
        error,
      )
    Ok(_) ->
      case current_head_or_error(checkout_dir, runner) {
        Error(error) ->
          failed_manifest(
            manifest,
            now_ms,
            True,
            Some(manifest.branch),
            None,
            None,
            changed_paths,
            removed_paths,
            error,
          )
        Ok(commit_sha) ->
          case
            github_cli.run_ok(
              runner,
              command_runner.sh(
                "git",
                ["push", "origin", manifest.branch],
                checkout_dir,
              ),
              True,
            )
          {
            Error(error) ->
              failed_manifest(
                manifest,
                now_ms,
                True,
                Some(manifest.branch),
                Some(commit_sha),
                None,
                changed_paths,
                removed_paths,
                error,
              )
            Ok(_) -> {
              let pr_result =
                github_cli.ensure_pull_request(manifest, checkout_dir, runner)
              case pr_result {
                Error(error) ->
                  failed_manifest(
                    manifest,
                    now_ms,
                    True,
                    Some(manifest.branch),
                    Some(commit_sha),
                    github_cli.existing_pr_url(manifest, checkout_dir, runner),
                    changed_paths,
                    removed_paths,
                    error,
                  )
                Ok(pr_url) ->
                  artifact_publication_manifest.published_manifest(
                    manifest,
                    attempt_id,
                    now_ms,
                    commit_sha,
                    pr_url,
                    changed_paths,
                    removed_paths,
                  )
              }
            }
          }
      }
  }
}

fn publish_commit_stack(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  selected_stack: types.SelectedCommitStackBytes,
  latest: Option(types.LatestPublicationDetails),
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
  _base_success_attempt_id: String,
) -> artifact_publication_manifest.PublicationManifest {
  case manifest.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(target) ->
      publish_existing_branch_commit_stack(
        manifest,
        selected_stack,
        target,
        workspace_root,
        runner,
        now_ms,
        commit_stack_attempt_id_for_execution(latest, manifest, now_ms),
      )
    artifact_publication_planner.StableBranchTargetPlan ->
      failed_manifest(
        manifest,
        now_ms,
        False,
        Some(manifest.branch),
        None,
        None,
        [],
        [],
        artifact_publication_manifest.PublicationErrorInfo(
          code: "unsupported_commit_stack_target",
          message: "commit_stack publication currently requires target existing_pr_branch",
        ),
      )
  }
}

fn commit_stack_attempt_id_for_execution(
  latest: Option(types.LatestPublicationDetails),
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  now_ms: Int,
) -> String {
  case latest_version_id(latest) == Some(manifest.version_id) {
    True ->
      artifact_publication_manifest.attempt_key_for_success_recovery(
        manifest.publication_id,
        manifest.version_id,
        now_ms,
      )
    False ->
      artifact_publication_manifest.attempt_key_for_success(manifest.version_id)
  }
}

fn publish_existing_branch_commit_stack(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  selected_stack: types.SelectedCommitStackBytes,
  target: commit_stack_artifact.ExistingPrBranchTarget,
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
  attempt_id: String,
) -> artifact_publication_manifest.PublicationManifest {
  let types.SelectedCommitStackBytes(stack, carrier_bytes) = selected_stack
  let checkout_dir = github_paths.checkout_dir(workspace_root, manifest)
  with_checkout_lock(
    checkout_dir,
    now_ms,
    manifest,
    Some(target.pr_url),
    [],
    fn() {
      case
        ensure_existing_branch_checkout(manifest, target, checkout_dir, runner)
      {
        Error(#(retryable, error)) ->
          failed_manifest(
            manifest,
            now_ms,
            retryable,
            Some(manifest.branch),
            None,
            Some(target.pr_url),
            [],
            [],
            error,
          )
        Ok(Nil) ->
          case
            github_cli.verify_existing_pr_branch(
              manifest,
              target,
              stack.stack.head_sha,
              checkout_dir,
              runner,
            )
          {
            Error(error) ->
              failed_manifest(
                manifest,
                now_ms,
                False,
                Some(manifest.branch),
                None,
                Some(target.pr_url),
                [],
                [],
                error,
              )
            Ok(verified_pr_url) ->
              case
                import_and_verify_commit_stack(
                  checkout_dir,
                  stack,
                  carrier_bytes,
                  target,
                  runner,
                )
              {
                Error(#(retryable, error)) ->
                  failed_manifest(
                    manifest,
                    now_ms,
                    retryable,
                    Some(manifest.branch),
                    None,
                    Some(verified_pr_url),
                    [],
                    [],
                    error,
                  )
                Ok(RemoteAlreadyAtHead) ->
                  artifact_publication_manifest.unchanged_manifest(
                    manifest,
                    attempt_id,
                    now_ms,
                    Some(stack.stack.head_sha),
                    Some(verified_pr_url),
                    [],
                  )
                Ok(RemoteReadyToPush) ->
                  case
                    github_cli.run_ok(
                      runner,
                      command_runner.sh(
                        "git",
                        [
                          "push",
                          "origin",
                          stack.stack.head_sha
                            <> ":refs/heads/"
                            <> target.head_branch,
                        ],
                        checkout_dir,
                      ),
                      True,
                    )
                  {
                    Error(error) ->
                      failed_manifest(
                        manifest,
                        now_ms,
                        True,
                        Some(manifest.branch),
                        Some(stack.stack.head_sha),
                        Some(verified_pr_url),
                        [],
                        [],
                        error,
                      )
                    Ok(_) ->
                      artifact_publication_manifest.published_manifest(
                        manifest,
                        attempt_id,
                        now_ms,
                        stack.stack.head_sha,
                        Some(verified_pr_url),
                        [],
                        [],
                      )
                  }
              }
          }
      }
    },
  )
}

type CommitStackRemoteState {
  RemoteAlreadyAtHead
  RemoteReadyToPush
}

fn import_and_verify_commit_stack(
  checkout_dir: String,
  stack: artifact_publication_planner.PlannedCommitStack,
  carrier_bytes: BitArray,
  target: commit_stack_artifact.ExistingPrBranchTarget,
  runner: command_runner.Runner,
) -> Result(
  CommitStackRemoteState,
  #(Bool, artifact_publication_manifest.PublicationErrorInfo),
) {
  use bundle_path <- result.try(write_bundle_file(
    checkout_dir,
    stack,
    carrier_bytes,
  ))
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh("git", ["bundle", "verify", bundle_path], checkout_dir),
      True,
    )
    |> result.map_error(fn(error) { #(False, error) }),
  )
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh(
        "git",
        ["fetch", bundle_path, stack.stack.head_sha],
        checkout_dir,
      ),
      True,
    )
    |> result.map_error(fn(error) { #(True, error) }),
  )
  use _ <- result.try(verify_head_tree(checkout_dir, stack, runner))
  use remote_head <- result.try(current_remote_branch_head(
    checkout_dir,
    target.head_branch,
    runner,
  ))
  case remote_head == stack.stack.head_sha {
    True -> Ok(RemoteAlreadyAtHead)
    False ->
      case remote_head == target.expected_head_sha {
        False ->
          Error(#(
            False,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "stale_existing_branch",
              message: "remote branch "
                <> target.head_branch
                <> " is no longer at expected head "
                <> target.expected_head_sha,
            ),
          ))
        True -> {
          use _ <- result.try(require_ancestor(
            checkout_dir,
            target.expected_head_sha,
            stack.stack.head_sha,
            "commit_stack_not_fast_forward",
            "commit stack head is not a fast-forward of expected branch head",
            runner,
          ))
          use _ <- result.try(require_ancestor(
            checkout_dir,
            target.base_sha,
            "origin/" <> target.base_branch,
            "stale_existing_branch_base",
            "remote base branch no longer contains expected base sha",
            runner,
          ))
          Ok(RemoteReadyToPush)
        }
      }
  }
}

fn write_bundle_file(
  checkout_dir: String,
  stack: artifact_publication_planner.PlannedCommitStack,
  carrier_bytes: BitArray,
) -> Result(String, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  let bundle_dir =
    path.join(
      github_paths.workspace_root_for(checkout_dir),
      ".scherzo-state/artifact-repositories/github/bundles",
    )
  let bundle_path =
    path.join(bundle_dir, stack.stack.carrier.sha256 <> ".bundle")
  case simplifile.create_directory_all(bundle_dir) {
    Error(_) ->
      Error(#(
        False,
        artifact_publication_manifest.PublicationErrorInfo(
          code: "commit_stack_bundle_write_failed",
          message: "could not create commit stack bundle directory",
        ),
      ))
    Ok(_) ->
      case simplifile.write_bits(to: bundle_path, bits: carrier_bytes) {
        Ok(_) -> Ok(bundle_path)
        Error(_) ->
          Error(#(
            False,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "commit_stack_bundle_write_failed",
              message: "could not write commit stack bundle bytes",
            ),
          ))
      }
  }
}

fn verify_head_tree(
  checkout_dir: String,
  stack: artifact_publication_planner.PlannedCommitStack,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case
    github_cli.run_stdout(
      runner,
      command_runner.sh(
        "git",
        ["rev-parse", stack.stack.head_sha <> "^{tree}"],
        checkout_dir,
      ),
      True,
    )
  {
    Error(error) -> Error(#(True, error))
    Ok(tree) ->
      case tree == stack.stack.head_tree {
        True -> Ok(Nil)
        False ->
          Error(#(
            False,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "commit_stack_head_tree_mismatch",
              message: "imported commit stack head tree does not match retained manifest",
            ),
          ))
      }
  }
}

fn current_remote_branch_head(
  checkout_dir: String,
  branch: String,
  runner: command_runner.Runner,
) -> Result(String, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case
    github_cli.run_stdout(
      runner,
      command_runner.sh("git", ["rev-parse", "origin/" <> branch], checkout_dir),
      True,
    )
  {
    Ok(value) -> Ok(value)
    Error(error) -> Error(#(True, error))
  }
}

fn require_ancestor(
  checkout_dir: String,
  ancestor: String,
  descendant: String,
  code: String,
  message: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case
    github_cli.run(
      runner,
      command_runner.sh(
        "git",
        ["merge-base", "--is-ancestor", ancestor, descendant],
        checkout_dir,
      ),
      True,
    )
  {
    Error(error) -> Error(#(True, error))
    Ok(output) ->
      case output.exit_code {
        0 -> Ok(Nil)
        1 ->
          Error(#(
            False,
            artifact_publication_manifest.PublicationErrorInfo(
              code: code,
              message: message,
            ),
          ))
        _ ->
          Error(#(
            True,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "git_merge_base_failed",
              message: command_runner.summarize(output),
            ),
          ))
      }
  }
}

fn ensure_existing_branch_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  target: commit_stack_artifact.ExistingPrBranchTarget,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  use repo <- result.try(
    require_option(
      manifest.github_repo,
      artifact_publication_manifest.PublicationErrorInfo(
        code: "missing_github_repo",
        message: "planned github publication is missing github_repo",
      ),
    )
    |> result.map_error(fn(error) { #(False, error) }),
  )
  let remote_url = github_paths.remote_url(repo)
  let exists = simplifile.is_directory(checkout_dir) |> result.unwrap(False)
  case exists {
    False -> {
      use _ <- result.try(create_directory_all_result(
        path.join(
          github_paths.workspace_root_for(checkout_dir),
          ".scherzo-state/artifact-repositories/github",
        ),
        "managed_checkout_parent_create_failed",
        "could not create managed checkout parent directory",
      ))
      case
        github_cli.run_ok(
          runner,
          command_runner.sh(
            "git",
            ["clone", remote_url, checkout_dir],
            github_paths.workspace_root_for(checkout_dir),
          ),
          True,
        )
      {
        Error(error) -> Error(#(True, error))
        Ok(_) -> {
          use _ <- result.try(create_directory_all_result(
            checkout_dir,
            "managed_checkout_create_failed",
            "could not create managed checkout directory",
          ))
          sync_existing_branch_checkout(manifest, target, checkout_dir, runner)
        }
      }
    }
    True ->
      case
        github_cli.run_stdout(
          runner,
          command_runner.sh(
            "git",
            ["remote", "get-url", "origin"],
            checkout_dir,
          ),
          True,
        )
      {
        Error(error) -> Error(#(True, error))
        Ok(origin) ->
          case github_paths.same_remote(origin, remote_url, repo) {
            True ->
              sync_existing_branch_checkout(
                manifest,
                target,
                checkout_dir,
                runner,
              )
            False ->
              Error(#(
                False,
                artifact_publication_manifest.PublicationErrorInfo(
                  code: "remote_mismatch",
                  message: "managed checkout origin does not match configured repository",
                ),
              ))
          }
      }
  }
}

fn create_directory_all_result(
  directory: String,
  code: String,
  message: String,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case simplifile.create_directory_all(directory) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      Error(#(
        True,
        artifact_publication_manifest.PublicationErrorInfo(
          code: code,
          message: message,
        ),
      ))
  }
}

fn sync_existing_branch_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  target: commit_stack_artifact.ExistingPrBranchTarget,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh(
        "git",
        ["fetch", "origin", target.base_branch],
        checkout_dir,
      ),
      True,
    )
    |> result.map_error(fn(error) { #(True, error) }),
  )
  use branch_exists <- result.try(remote_branch_exists(
    manifest,
    checkout_dir,
    runner,
  ))
  case branch_exists {
    False ->
      Error(#(
        False,
        artifact_publication_manifest.PublicationErrorInfo(
          code: "existing_branch_missing",
          message: "existing PR branch target does not exist on remote",
        ),
      ))
    True -> {
      use _ <- result.try(fetch_publication_branch(
        True,
        manifest,
        checkout_dir,
        runner,
      ))
      use _ <- result.try(
        github_cli.run_ok(
          runner,
          command_runner.sh(
            "git",
            ["checkout", "-B", manifest.branch, "origin/" <> manifest.branch],
            checkout_dir,
          ),
          True,
        )
        |> result.map_error(fn(error) { #(True, error) }),
      )
      ensure_clean_checkout(checkout_dir, runner)
    }
  }
}

fn ensure_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, CheckoutFailure) {
  use repo <- result.try(
    require_option(
      manifest.github_repo,
      artifact_publication_manifest.PublicationErrorInfo(
        code: "missing_github_repo",
        message: "planned github publication is missing github_repo",
      ),
    )
    |> result.map_error(fn(error) { CheckoutFailure(False, error, None) }),
  )
  let remote_url = github_paths.remote_url(repo)
  let exists = simplifile.is_directory(checkout_dir) |> result.unwrap(False)
  case exists {
    False -> {
      let _ =
        simplifile.create_directory_all(path.join(
          github_paths.workspace_root_for(checkout_dir),
          ".scherzo-state/artifact-repositories/github",
        ))
      case
        github_cli.run_ok(
          runner,
          command_runner.sh(
            "git",
            ["clone", remote_url, checkout_dir],
            github_paths.workspace_root_for(checkout_dir),
          ),
          True,
        )
      {
        Error(error) -> Error(CheckoutFailure(True, error, None))
        Ok(_) -> {
          let _ = simplifile.create_directory_all(checkout_dir)
          sync_checkout(manifest, checkout_dir, runner)
        }
      }
    }
    True -> {
      case
        github_cli.run_stdout(
          runner,
          command_runner.sh(
            "git",
            ["remote", "get-url", "origin"],
            checkout_dir,
          ),
          True,
        )
      {
        Error(error) -> Error(CheckoutFailure(True, error, None))
        Ok(origin) ->
          case github_paths.same_remote(origin, remote_url, repo) {
            True -> sync_checkout(manifest, checkout_dir, runner)
            False ->
              Error(CheckoutFailure(
                False,
                artifact_publication_manifest.PublicationErrorInfo(
                  code: "remote_mismatch",
                  message: "managed checkout origin does not match configured repository",
                ),
                None,
              ))
          }
      }
    }
  }
}

fn remote_branch_exists(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Bool, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case
    github_cli.run(
      runner,
      command_runner.sh(
        "git",
        ["ls-remote", "--exit-code", "--heads", "origin", manifest.branch],
        checkout_dir,
      ),
      True,
    )
  {
    Error(error) -> Error(#(True, error))
    Ok(output) ->
      case output.exit_code {
        0 -> Ok(True)
        2 -> Ok(False)
        _ ->
          Error(#(
            True,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "git_ls_remote_failed",
              message: command_runner.summarize(output),
            ),
          ))
      }
  }
}

fn fetch_publication_branch(
  branch_exists: Bool,
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case branch_exists {
    False -> Ok(Nil)
    True ->
      github_cli.run_ok(
        runner,
        command_runner.sh(
          "git",
          ["fetch", "origin", manifest.branch],
          checkout_dir,
        ),
        True,
      )
      |> result.map(fn(_) { Nil })
      |> result.map_error(fn(error) { #(True, error) })
  }
}

fn sync_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, CheckoutFailure) {
  use base <- result.try(
    require_option(
      manifest.github_base,
      artifact_publication_manifest.PublicationErrorInfo(
        code: "missing_github_base",
        message: "planned github publication is missing github_base",
      ),
    )
    |> result.map_error(fn(error) { CheckoutFailure(False, error, None) }),
  )
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh("git", ["fetch", "origin", base], checkout_dir),
      True,
    )
    |> result.map_error(fn(error) { CheckoutFailure(True, error, None) }),
  )
  use branch_exists <- result.try(
    remote_branch_exists(manifest, checkout_dir, runner)
    |> result.map_error(fn(pair) {
      let #(retryable, error) = pair
      CheckoutFailure(retryable, error, None)
    }),
  )
  use _ <- result.try(
    fetch_publication_branch(branch_exists, manifest, checkout_dir, runner)
    |> result.map_error(fn(pair) {
      let #(retryable, error) = pair
      CheckoutFailure(retryable, error, None)
    }),
  )
  let checkout_args = case branch_exists {
    True -> ["checkout", "-B", manifest.branch, "origin/" <> manifest.branch]
    False -> ["checkout", "-B", manifest.branch, "origin/" <> base]
  }
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh("git", checkout_args, checkout_dir),
      True,
    )
    |> result.map_error(fn(error) { CheckoutFailure(True, error, None) }),
  )
  case status_snapshot(checkout_dir, runner) {
    Ok(output) if output == "" -> Ok(Nil)
    Ok(output) -> {
      let cleanup_diagnostics =
        cleanup_checkout(checkout_dir, runner, Some(output))
      case cleanup_diagnostics.cleanup_succeeded {
        True -> Ok(Nil)
        False ->
          Error(CheckoutFailure(
            True,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "dirty_checkout",
              message: "managed checkout is dirty before materialization",
            ),
            Some(cleanup_diagnostics),
          ))
      }
    }
    Error(error) -> Error(CheckoutFailure(True, error, None))
  }
}

fn ensure_clean_checkout(
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case
    github_cli.run_stdout(
      runner,
      command_runner.sh("git", ["status", "--porcelain"], checkout_dir),
      True,
    )
  {
    Error(error) -> Error(#(True, error))
    Ok(output) ->
      case string.trim(output) == "" {
        True -> Ok(Nil)
        False ->
          Error(#(
            False,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "dirty_checkout",
              message: "managed checkout is dirty before materialization",
            ),
          ))
      }
  }
}

fn materialize_selected_files(
  checkout_dir: String,
  selected_files: List(types.SelectedArtifactBytes),
  removed_paths: List(String),
) -> Result(List(String), artifact_publication_manifest.PublicationErrorInfo) {
  let changed_paths = selected_destination_paths(selected_files)
  use _ <- result.try(validate_materialization_paths(changed_paths, []))
  list.each(removed_paths, fn(relative) {
    let target = path.join(checkout_dir, relative)
    let _ = simplifile.delete(target)
  })
  write_selected_files(checkout_dir, selected_files, [])
}

fn write_selected_files(
  checkout_dir: String,
  selected_files: List(types.SelectedArtifactBytes),
  acc: List(String),
) -> Result(List(String), artifact_publication_manifest.PublicationErrorInfo) {
  case selected_files {
    [] -> Ok(list.reverse(acc))
    [types.SelectedArtifactBytes(file, bytes), ..rest] -> {
      let artifact_publication_planner.PlannedPublicationFile(
        _,
        destination_path,
      ) = file
      let absolute = path.join(checkout_dir, destination_path)
      use dir <- result.try(
        path.dirname(absolute)
        |> result.map_error(fn(_) {
          artifact_publication_manifest.PublicationErrorInfo(
            code: "destination_dir_failed",
            message: "could not determine destination directory",
          )
        }),
      )
      use _ <- result.try(
        simplifile.create_directory_all(dir)
        |> result.map_error(fn(_) {
          artifact_publication_manifest.PublicationErrorInfo(
            code: "destination_dir_failed",
            message: "could not create destination directory",
          )
        }),
      )
      use _ <- result.try(
        simplifile.write_bits(to: absolute, bits: bytes)
        |> result.map_error(fn(_) {
          artifact_publication_manifest.PublicationErrorInfo(
            code: "destination_write_failed",
            message: "could not write publication file " <> destination_path,
          )
        }),
      )
      write_selected_files(checkout_dir, rest, [destination_path, ..acc])
    }
  }
}

fn stage_and_diff(
  checkout_dir: String,
  _manifest: artifact_publication_planner.DryRunPublicationManifest,
  changed_paths: List(String),
  removed_paths: List(String),
  runner: command_runner.Runner,
) -> Result(Bool, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  let all_paths = list.append(changed_paths, removed_paths)
  let add_args = list.append(["add", "--"], all_paths)
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh("git", add_args, checkout_dir),
      True,
    )
    |> result.map_error(fn(error) { #(True, error) }),
  )
  case
    github_cli.run(
      runner,
      command_runner.sh(
        "git",
        list.append(["diff", "--cached", "--quiet", "--"], all_paths),
        checkout_dir,
      ),
      True,
    )
  {
    Error(error) -> Error(#(True, error))
    Ok(output) ->
      case output.exit_code {
        0 -> Ok(False)
        1 -> Ok(True)
        _ ->
          Error(#(
            True,
            artifact_publication_manifest.PublicationErrorInfo(
              code: "git_diff_failed",
              message: command_runner.summarize(output),
            ),
          ))
      }
  }
}

fn commit_message(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
) -> String {
  "scherzo publication "
  <> manifest.workflow_id
  <> "/"
  <> manifest.publication_id
  <> " "
  <> manifest.version_id
}

fn require_option(
  value: Option(a),
  error: artifact_publication_manifest.PublicationErrorInfo,
) -> Result(a, artifact_publication_manifest.PublicationErrorInfo) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(error)
  }
}

fn validate_selected_files(
  selected_files: List(types.SelectedArtifactBytes),
  seen: List(String),
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  case selected_files {
    [] -> Ok(Nil)
    [types.SelectedArtifactBytes(file, _), ..rest] -> {
      let artifact_publication_planner.PlannedPublicationFile(
        _,
        destination_path,
      ) = file
      use _ <- result.try(validate_path(destination_path))
      case list.contains(seen, destination_path) {
        True ->
          Error(artifact_publication_manifest.PublicationErrorInfo(
            code: "duplicate_destination_path",
            message: "duplicate destination path: " <> destination_path,
          ))
        False -> validate_selected_files(rest, [destination_path, ..seen])
      }
    }
  }
}

fn validate_materialization_paths(
  paths: List(String),
  seen: List(String),
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  case paths {
    [] -> Ok(Nil)
    [relative, ..rest] -> {
      use _ <- result.try(validate_path(relative))
      case list.contains(seen, relative) {
        True ->
          Error(artifact_publication_manifest.PublicationErrorInfo(
            code: "duplicate_destination_path",
            message: "duplicate destination path: " <> relative,
          ))
        False -> validate_materialization_paths(rest, [relative, ..seen])
      }
    }
  }
}

fn validate_path(
  relative: String,
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  case
    relative == ""
    || path.is_absolute(relative)
    || path.has_parent_segment(relative)
    || path.contains_control_character(relative)
  {
    True ->
      Error(artifact_publication_manifest.PublicationErrorInfo(
        code: "unsafe_destination_path",
        message: "unsafe destination path: " <> relative,
      ))
    False -> Ok(Nil)
  }
}

fn selected_destination_paths(
  selected_files: List(types.SelectedArtifactBytes),
) -> List(String) {
  selected_files
  |> list.map(fn(selected) {
    let types.SelectedArtifactBytes(file, _) = selected
    let artifact_publication_planner.PlannedPublicationFile(_, destination_path) =
      file
    destination_path
  })
}

fn stale_paths(
  latest: Option(types.LatestPublicationDetails),
  current_files: List(artifact_publication_planner.PlannedPublicationFile),
) -> List(String) {
  let current_paths =
    current_files
    |> list.map(fn(file) {
      let artifact_publication_planner.PlannedPublicationFile(
        _,
        destination_path,
      ) = file
      destination_path
    })
  case latest {
    Some(details) ->
      details.selected_paths
      |> list.filter(fn(path_value) {
        !list.contains(current_paths, path_value)
      })
    None -> []
  }
}

fn latest_success_details(
  workspace_root: String,
  series_id: String,
) -> Option(types.LatestPublicationDetails) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(_) -> None
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(_) -> None
        Ok(projected) ->
          case
            latest_terminal_success_publication_for_series(projected, series_id)
          {
            Error(_) -> None
            Ok(attempt) ->
              case attempt.manifest_ref {
                Some(ref) ->
                  load_latest_manifest(workspace_root, ref)
                  |> result.unwrap(None)
                None -> None
              }
          }
      }
  }
}

fn latest_terminal_success_publication_for_series(
  projected: projection.Projection,
  series_id: String,
) -> Result(projection.PublicationAttempt, Nil) {
  projected.publication_attempts
  |> dict.values
  |> list.flatten
  |> latest_terminal_success_for_series(series_id, None)
}

fn latest_terminal_success_for_series(
  attempts: List(projection.PublicationAttempt),
  series_id: String,
  best: Option(projection.PublicationAttempt),
) -> Result(projection.PublicationAttempt, Nil) {
  case attempts {
    [] ->
      case best {
        Some(attempt) -> Ok(attempt)
        None -> Error(Nil)
      }
    [attempt, ..rest] -> {
      let next_best = case
        attempt.series_id == series_id,
        publication_status_is_terminal_success(attempt.status),
        best
      {
        True, True, None -> Some(attempt)
        True, True, Some(existing)
          if attempt.recorded_at_ms >= existing.recorded_at_ms
        -> Some(attempt)
        _, _, _ -> best
      }
      latest_terminal_success_for_series(rest, series_id, next_best)
    }
  }
}

fn publication_status_is_terminal_success(status: String) -> Bool {
  status == "published" || status == "unchanged"
}

fn load_latest_manifest(
  workspace_root: String,
  ref: String,
) -> Result(Option(types.LatestPublicationDetails), Nil) {
  let store = artifact_store.new(workspace_root)
  use contents <- result.try(
    artifact_store.read_artifact_unverified(store, ref)
    |> result.replace_error(Nil),
  )
  case artifact_publication_manifest.decode_manifest_json(contents) {
    Ok(manifest) ->
      Ok(
        Some(types.LatestPublicationDetails(
          status: artifact_publication_manifest.status_to_string(
            manifest.status,
          ),
          version_id: manifest.version_id,
          branch: manifest.branch,
          commit_sha: manifest.commit_sha,
          pr_url: manifest.pr_url,
          selected_paths: manifest.selected_paths,
        )),
      )
    Error(_) -> Ok(None)
  }
}

fn latest_version_id(
  latest: Option(types.LatestPublicationDetails),
) -> Option(String) {
  case latest {
    Some(details) -> details.version_id
    None -> None
  }
}

fn latest_commit_sha(
  latest: Option(types.LatestPublicationDetails),
) -> Option(String) {
  case latest {
    Some(details) -> details.commit_sha
    None -> None
  }
}

fn latest_pr_url(
  latest: Option(types.LatestPublicationDetails),
) -> Option(String) {
  case latest {
    Some(details) -> details.pr_url
    None -> None
  }
}

fn load_commit_stack(
  stack: artifact_publication_planner.PlannedCommitStack,
  store: artifact_store.Store,
) -> Result(types.SelectedCommitStackBytes, PublishError) {
  let carrier = stack.stack.carrier
  use _ <- result.try(validate_commit_stack_carrier_size(carrier))
  use _ <- result.try(validate_commit_stack_carrier_actual_size(carrier, store))
  use bytes <- result.try(
    artifact_store.read_artifact_bytes_unverified(store, carrier.ref)
    |> result.map_error(fn(error) {
      PublishError(
        code: "artifact_read_failed",
        message: describe_artifact_error(error),
      )
    }),
  )
  let actual_sha256 = hash.sha256_hex_bytes(bytes)
  case
    actual_sha256 == carrier.sha256,
    bit_array.byte_size(bytes) == carrier.bytes
  {
    True, True -> Ok(types.SelectedCommitStackBytes(stack, bytes))
    False, _ ->
      Error(PublishError(
        code: "hash_mismatch",
        message: "commit stack carrier bytes changed after planning for ref "
          <> carrier.ref,
      ))
    _, False ->
      Error(PublishError(
        code: "bytes_mismatch",
        message: "commit stack carrier byte count changed after planning for ref "
          <> carrier.ref,
      ))
  }
}

fn validate_commit_stack_carrier_size(
  carrier: commit_stack_artifact.CommitStackCarrier,
) -> Result(Nil, PublishError) {
  case carrier.bytes <= commit_stack_artifact.max_bundle_bytes {
    True -> Ok(Nil)
    False ->
      Error(PublishError(
        code: "commit_stack_carrier_too_large",
        message: "commit stack carrier exceeds maximum bundle size",
      ))
  }
}

fn validate_commit_stack_carrier_actual_size(
  carrier: commit_stack_artifact.CommitStackCarrier,
  store: artifact_store.Store,
) -> Result(Nil, PublishError) {
  case artifact_store.location(store, carrier.ref) {
    Ok(artifact_store.ArtifactLocation(local_path: Some(local_path), ..)) ->
      validate_local_commit_stack_carrier_size(carrier, local_path)
    Ok(_) -> Ok(Nil)
    Error(error) ->
      Error(PublishError(
        code: "artifact_read_failed",
        message: describe_artifact_error(error),
      ))
  }
}

fn validate_local_commit_stack_carrier_size(
  carrier: commit_stack_artifact.CommitStackCarrier,
  local_path: String,
) -> Result(Nil, PublishError) {
  case simplifile.file_info(local_path) {
    Ok(info) ->
      case simplifile.file_info_type(info) {
        simplifile.File ->
          validate_actual_commit_stack_carrier_size(carrier, info.size)
        _ ->
          Error(PublishError(
            code: "artifact_read_failed",
            message: "commit stack carrier is not a regular file for ref "
              <> carrier.ref,
          ))
      }
    Error(error) ->
      Error(PublishError(
        code: "artifact_read_failed",
        message: "could not stat commit stack carrier ref "
          <> carrier.ref
          <> ": "
          <> simplifile.describe_error(error),
      ))
  }
}

fn validate_actual_commit_stack_carrier_size(
  carrier: commit_stack_artifact.CommitStackCarrier,
  actual_bytes: Int,
) -> Result(Nil, PublishError) {
  case actual_bytes > commit_stack_artifact.max_bundle_bytes {
    True ->
      Error(PublishError(
        code: "commit_stack_carrier_too_large",
        message: "commit stack carrier exceeds maximum bundle size",
      ))
    False ->
      case actual_bytes == carrier.bytes {
        True -> Ok(Nil)
        False ->
          Error(PublishError(
            code: "bytes_mismatch",
            message: "commit stack carrier byte count changed after planning for ref "
              <> carrier.ref,
          ))
      }
  }
}

fn load_selected_files(
  files: List(artifact_publication_planner.PlannedPublicationFile),
  store: artifact_store.Store,
  acc: List(types.SelectedArtifactBytes),
) -> Result(List(types.SelectedArtifactBytes), PublishError) {
  case files {
    [] -> Ok(acc)
    [file, ..rest] -> {
      let artifact_publication_planner.PlannedPublicationFile(source, _) = file
      use bytes <- result.try(
        artifact_store.read_artifact_bytes_unverified(store, source.ref)
        |> result.map_error(fn(error) {
          PublishError(
            code: "artifact_read_failed",
            message: describe_artifact_error(error),
          )
        }),
      )
      let actual_sha256 = hash.sha256_hex_bytes(bytes)
      case
        actual_sha256 == source.sha256,
        bit_array.byte_size(bytes) == source.bytes
      {
        True, True ->
          load_selected_files(rest, store, [
            types.SelectedArtifactBytes(file, bytes),
            ..acc
          ])
        False, _ ->
          Error(PublishError(
            code: "hash_mismatch",
            message: "artifact bytes changed after planning for ref "
              <> source.ref,
          ))
        _, False ->
          Error(PublishError(
            code: "bytes_mismatch",
            message: "artifact byte count changed after planning for ref "
              <> source.ref,
          ))
      }
    }
  }
}

fn describe_artifact_error(error: artifact_store.ArtifactError) -> String {
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
