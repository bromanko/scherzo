import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/github_cli
import scherzo/artifact_repository/github_paths
import scherzo/artifact_repository/types
import scherzo/commit_stack
import scherzo/hash
import scherzo/path
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import simplifile

pub type PublishError {
  PublishError(code: String, message: String)
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
  case manifest.mode {
    artifact_publication_config.FilesPublication -> {
      use selected_files <- result.try(
        load_selected_files(manifest.files, store, []),
      )
      Ok(types.PublicationExecutionInput(
        manifest,
        list.reverse(selected_files),
        None,
      ))
    }
    artifact_publication_config.CommitStackPublication -> {
      use commit_stack <- result.try(load_selected_commit_stack(
        manifest.commit_stack,
        store,
      ))
      Ok(types.PublicationExecutionInput(manifest, [], Some(commit_stack)))
    }
  }
}

pub fn publish(
  input: types.PublicationExecutionInput,
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
) -> artifact_publication_manifest.PublicationManifest {
  let types.PublicationExecutionInput(manifest, selected_files, commit_stack) =
    input
  case manifest.mode {
    artifact_publication_config.FilesPublication ->
      publish_files(manifest, selected_files, workspace_root, runner, now_ms)
    artifact_publication_config.CommitStackPublication ->
      publish_commit_stack(
        manifest,
        commit_stack,
        workspace_root,
        runner,
        now_ms,
      )
  }
}

fn publish_files(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  selected_files: List(types.SelectedArtifactBytes),
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
) -> artifact_publication_manifest.PublicationManifest {
  let base_success_attempt_id =
    artifact_publication_manifest.attempt_key_for_success(manifest.version_id)
  let latest = latest_success_details(workspace_root, manifest.series_id)
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

fn publish_commit_stack(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  selected: Option(types.SelectedCommitStackBytes),
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
) -> artifact_publication_manifest.PublicationManifest {
  let latest = latest_success_details(workspace_root, manifest.series_id)
  let base_success_attempt_id =
    artifact_publication_manifest.attempt_key_for_success(manifest.version_id)
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
        [],
      )
    False ->
      case selected {
        None ->
          failed_manifest(
            manifest,
            now_ms,
            False,
            Some(manifest.branch),
            None,
            latest_pr_url(latest),
            [],
            [],
            artifact_publication_manifest.PublicationErrorInfo(
              code: "missing_commit_stack",
              message: "commit_stack publication is missing prepared artifact bytes",
            ),
          )
        Some(types.SelectedCommitStackBytes(_, bytes)) ->
          case bit_array.to_string(bytes) {
            Error(_) ->
              failed_manifest(
                manifest,
                now_ms,
                False,
                Some(manifest.branch),
                None,
                latest_pr_url(latest),
                [],
                [],
                artifact_publication_manifest.PublicationErrorInfo(
                  code: "commit_stack_not_utf8",
                  message: "commit_stack artifact bytes are not valid UTF-8 JSON",
                ),
              )
            Ok(contents) ->
              case commit_stack.decode_json(contents) {
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
                    artifact_publication_manifest.PublicationErrorInfo(
                      code: commit_stack.code(error),
                      message: commit_stack.message(error),
                    ),
                  )
                Ok(stack) ->
                  case stack.commits {
                    [] ->
                      failed_manifest(
                        manifest,
                        now_ms,
                        False,
                        Some(manifest.branch),
                        None,
                        latest_pr_url(latest),
                        [],
                        [],
                        artifact_publication_manifest.PublicationErrorInfo(
                          code: "empty_commit_stack",
                          message: "commit_stack publication requires at least one commit",
                        ),
                      )
                    _ ->
                      publish_commit_stack_with_checkout(
                        manifest,
                        stack,
                        workspace_root,
                        runner,
                        now_ms,
                        success_attempt_id_for_execution(
                          latest,
                          manifest,
                          now_ms,
                        ),
                      )
                  }
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
      && case manifest.pull_request.enabled {
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
  let attempt_id =
    artifact_publication_manifest.attempt_key_for_failure(
      manifest.publication_id,
      error.code,
      error.message,
      now_ms,
    )
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
  case ensure_checkout(manifest, checkout_dir, runner, False) {
    Error(#(retryable, error)) ->
      failed_manifest(
        manifest,
        now_ms,
        retryable,
        Some(manifest.branch),
        None,
        None,
        [],
        removed_paths,
        error,
      )
    Ok(Nil) ->
      case
        materialize_selected_files(checkout_dir, selected_files, removed_paths)
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
  }
}

fn publish_commit_stack_with_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  stack: commit_stack.CommitStack,
  workspace_root: String,
  runner: command_runner.Runner,
  now_ms: Int,
  attempt_id: String,
) -> artifact_publication_manifest.PublicationManifest {
  let checkout_dir = github_paths.checkout_dir(workspace_root, manifest)
  case ensure_checkout(manifest, checkout_dir, runner, True) {
    Error(#(retryable, error)) ->
      failed_manifest(
        manifest,
        now_ms,
        retryable,
        Some(manifest.branch),
        None,
        None,
        [],
        [],
        error,
      )
    Ok(Nil) ->
      case reset_commit_stack_branch(manifest, checkout_dir, runner) {
        Error(#(retryable, error)) ->
          failed_manifest(
            manifest,
            now_ms,
            retryable,
            Some(manifest.branch),
            None,
            None,
            [],
            [],
            error,
          )
        Ok(Nil) ->
          case apply_commit_stack(stack.commits, checkout_dir, runner, 1) {
            Error(error) ->
              failed_manifest(
                manifest,
                now_ms,
                True,
                Some(manifest.branch),
                None,
                None,
                [],
                [],
                error,
              )
            Ok(Nil) ->
              publish_commit_stack_head(
                manifest,
                checkout_dir,
                runner,
                now_ms,
                attempt_id,
              )
          }
      }
  }
}

fn publish_commit_stack_head(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
  now_ms: Int,
  attempt_id: String,
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
        [],
        error,
      )
    Ok(commit_sha) ->
      case
        github_cli.run_ok(
          runner,
          command_runner.sh(
            "git",
            ["push", "--force-with-lease", "origin", manifest.branch],
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
            [],
            [],
            error,
          )
        Ok(_) ->
          case github_cli.ensure_pull_request(manifest, checkout_dir, runner) {
            Error(error) ->
              failed_manifest(
                manifest,
                now_ms,
                True,
                Some(manifest.branch),
                Some(commit_sha),
                None,
                [],
                [],
                error,
              )
            Ok(pr_url) ->
              artifact_publication_manifest.published_manifest(
                manifest,
                attempt_id,
                now_ms,
                commit_sha,
                pr_url,
                [],
                [],
              )
          }
      }
  }
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
                None,
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
                    None,
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

fn reset_commit_stack_branch(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  use base <- result.try(
    require_option(
      manifest.github_base,
      artifact_publication_manifest.PublicationErrorInfo(
        code: "missing_github_base",
        message: "planned github publication is missing github_base",
      ),
    )
    |> result.map_error(fn(error) { #(False, error) }),
  )
  use _ <- result.try(clean_commit_stack_checkout(checkout_dir, runner))
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh(
        "git",
        ["checkout", "-B", manifest.branch, "origin/" <> base],
        checkout_dir,
      ),
      True,
    )
    |> result.map_error(fn(error) { #(True, error) }),
  )
  use _ <- result.try(clean_commit_stack_checkout(checkout_dir, runner))
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
              message: "managed checkout is dirty before commit_stack import",
            ),
          ))
      }
  }
}

fn clean_checkout_before_sync(
  clean: Bool,
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  case clean {
    True -> clean_commit_stack_checkout(checkout_dir, runner)
    False -> Ok(Nil)
  }
}

fn clean_commit_stack_checkout(
  checkout_dir: String,
  runner: command_runner.Runner,
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh("git", ["reset", "--hard"], checkout_dir),
      True,
    )
    |> result.map_error(fn(error) { #(True, error) }),
  )
  github_cli.run_ok(
    runner,
    command_runner.sh(
      "git",
      ["clean", "-f", "--", ".scherzo-commit-stack-*.patch"],
      checkout_dir,
    ),
    True,
  )
  |> result.map(fn(_) { Nil })
  |> result.map_error(fn(error) { #(True, error) })
}

fn apply_commit_stack(
  commits: List(commit_stack.CommitStackCommit),
  checkout_dir: String,
  runner: command_runner.Runner,
  index: Int,
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  case commits {
    [] -> Ok(Nil)
    [commit, ..rest] -> {
      use _ <- result.try(apply_commit_stack_commit(
        commit,
        checkout_dir,
        runner,
        index,
      ))
      apply_commit_stack(rest, checkout_dir, runner, index + 1)
    }
  }
}

fn apply_commit_stack_commit(
  commit: commit_stack.CommitStackCommit,
  checkout_dir: String,
  runner: command_runner.Runner,
  index: Int,
) -> Result(Nil, artifact_publication_manifest.PublicationErrorInfo) {
  let commit_stack.CommitStackCommit(message: message, patch: patch, ..) =
    commit
  let patch_dir = commit_stack_patch_dir(checkout_dir)
  let patch_path =
    path.join(patch_dir, "commit-stack-" <> int.to_string(index) <> ".patch")
  use _ <- result.try(
    simplifile.create_directory_all(patch_dir)
    |> result.map_error(fn(_) {
      artifact_publication_manifest.PublicationErrorInfo(
        code: "commit_stack_patch_write_failed",
        message: "could not prepare retained commit_stack patch directory",
      )
    }),
  )
  use _ <- result.try(
    simplifile.write(patch_path, patch)
    |> result.map_error(fn(_) {
      artifact_publication_manifest.PublicationErrorInfo(
        code: "commit_stack_patch_write_failed",
        message: "could not write retained commit_stack patch",
      )
    }),
  )
  let apply_result =
    github_cli.run_ok(
      runner,
      command_runner.sh(
        "git",
        ["apply", "--index", "--whitespace=nowarn", patch_path],
        checkout_dir,
      ),
      True,
    )
  let _ = simplifile.delete_file(at: patch_path)
  use _ <- result.try(apply_result)
  use _ <- result.try(github_cli.run_ok(
    runner,
    command_runner.sh("git", ["commit", "-m", message], checkout_dir),
    True,
  ))
  Ok(Nil)
}

fn commit_stack_patch_dir(checkout_dir: String) -> String {
  path.join(
    github_paths.workspace_root_for(checkout_dir),
    ".scherzo-state/artifact-repositories/tmp",
  )
}

fn ensure_checkout(
  manifest: artifact_publication_planner.DryRunPublicationManifest,
  checkout_dir: String,
  runner: command_runner.Runner,
  clean_before_sync: Bool,
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
        Error(error) -> Error(#(True, error))
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
        Error(error) -> Error(#(True, error))
        Ok(origin) ->
          case github_paths.same_remote(origin, remote_url, repo) {
            True -> {
              use _ <- result.try(clean_checkout_before_sync(
                clean_before_sync,
                checkout_dir,
                runner,
              ))
              sync_checkout(manifest, checkout_dir, runner)
            }
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
) -> Result(Nil, #(Bool, artifact_publication_manifest.PublicationErrorInfo)) {
  use base <- result.try(
    require_option(
      manifest.github_base,
      artifact_publication_manifest.PublicationErrorInfo(
        code: "missing_github_base",
        message: "planned github publication is missing github_base",
      ),
    )
    |> result.map_error(fn(error) { #(False, error) }),
  )
  use _ <- result.try(
    github_cli.run_ok(
      runner,
      command_runner.sh("git", ["fetch", "origin", base], checkout_dir),
      True,
    )
    |> result.map_error(fn(error) { #(True, error) }),
  )
  use branch_exists <- result.try(remote_branch_exists(
    manifest,
    checkout_dir,
    runner,
  ))
  use _ <- result.try(fetch_publication_branch(
    branch_exists,
    manifest,
    checkout_dir,
    runner,
  ))
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
    |> result.map_error(fn(error) { #(True, error) }),
  )
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

fn load_selected_commit_stack(
  selected: Option(artifact_publication_planner.SelectedArtifact),
  store: artifact_store.Store,
) -> Result(types.SelectedCommitStackBytes, PublishError) {
  case selected {
    None ->
      Error(PublishError(
        code: "missing_commit_stack",
        message: "commit_stack publication is missing planned commit_stack artifact",
      ))
    Some(source) ->
      case source.bytes > commit_stack.max_artifact_bytes {
        True ->
          Error(PublishError(
            code: "commit_stack_too_large",
            message: "commit_stack artifact exceeds maximum byte size",
          ))
        False -> {
          use bytes <- result.try(read_selected_artifact_bytes(source, store))
          Ok(types.SelectedCommitStackBytes(source, bytes))
        }
      }
  }
}

fn read_selected_artifact_bytes(
  source: artifact_publication_planner.SelectedArtifact,
  store: artifact_store.Store,
) -> Result(BitArray, PublishError) {
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
    True, True -> Ok(bytes)
    False, _ ->
      Error(PublishError(
        code: "hash_mismatch",
        message: "artifact bytes changed after planning for ref " <> source.ref,
      ))
    _, False ->
      Error(PublishError(
        code: "bytes_mismatch",
        message: "artifact byte count changed after planning for ref "
          <> source.ref,
      ))
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
      use bytes <- result.try(read_selected_artifact_bytes(source, store))
      load_selected_files(rest, store, [
        types.SelectedArtifactBytes(file, bytes),
        ..acc
      ])
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
