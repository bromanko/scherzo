import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_repository/command_runner
import scherzo/artifact_repository/github as github_repository
import scherzo/artifact_repository/types
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_contract_manifest

pub fn execute_routes(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_with_state_root(
    routes,
    repositories,
    config_dir,
    config_dir,
    config_dir,
    output_manifest,
    issue,
    run_id,
    checkpoint,
  )
}

pub fn execute_routes_with_state_root(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_with_runner_and_state_root(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    state_root,
    output_manifest,
    issue,
    run_id,
    checkpoint,
    command_runner.production(),
  )
}

pub fn execute_recovered_routes(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_recovered_routes_with_state_root(
    routes,
    repositories,
    config_dir,
    config_dir,
    config_dir,
    output_manifest,
    issue,
    run_id,
    checkpoint,
  )
}

pub fn execute_recovered_routes_with_state_root(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_recovered_routes_with_runner_and_state_root(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    state_root,
    output_manifest,
    issue,
    run_id,
    checkpoint,
    command_runner.production(),
  )
}

pub fn execute_routes_for_work(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_for_work_with_state_root(
    routes,
    repositories,
    config_dir,
    config_dir,
    config_dir,
    output_manifest,
    work,
    run_id,
    checkpoint,
    runner,
  )
}

pub fn execute_routes_for_work_with_state_root(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_for_work_with_mode(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    state_root,
    output_manifest,
    work,
    run_id,
    checkpoint,
    runner,
    False,
    True,
  )
}

pub fn retry_routes_for_work_with_state_root(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
  reuse_terminal_attempts: Bool,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_for_work_with_mode(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    state_root,
    output_manifest,
    work,
    run_id,
    checkpoint,
    runner,
    False,
    reuse_terminal_attempts,
  )
}

pub fn execute_recovered_routes_with_runner(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_recovered_routes_with_runner_and_state_root(
    routes,
    repositories,
    config_dir,
    config_dir,
    config_dir,
    output_manifest,
    issue,
    run_id,
    checkpoint,
    runner,
  )
}

pub fn execute_recovered_routes_with_runner_and_state_root(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  let work = artifact_publication_recording.publication_work(issue)
  execute_routes_for_work_with_mode(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    state_root,
    output_manifest,
    work,
    run_id,
    checkpoint,
    runner,
    True,
    True,
  )
}

fn execute_routes_for_work_with_mode(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
  recovered_execution: Bool,
  reuse_terminal_attempts: Bool,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_loop(
    routes,
    repositories,
    output_manifest,
    config_dir,
    workflow_bundle_dir,
    state_root,
    work,
    run_id,
    checkpoint,
    runner,
    recovered_execution,
    reuse_terminal_attempts,
    [],
    [],
    [],
  )
  |> result.map(fn(result) {
    let artifact_publication_recording.PublicationRecordingResult(
      required_failures,
      optional_failures,
      attempts,
    ) = result
    artifact_publication_recording.PublicationRecordingResult(
      required_failures: list.reverse(required_failures),
      optional_failures: list.reverse(optional_failures),
      attempts: list.reverse(attempts),
    )
  })
}

pub fn execute_routes_with_runner(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  execute_routes_with_runner_and_state_root(
    routes,
    repositories,
    config_dir,
    config_dir,
    config_dir,
    output_manifest,
    issue,
    run_id,
    checkpoint,
    runner,
  )
}

pub fn execute_routes_with_runner_and_state_root(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  issue: tracker_issue.Issue,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  let work = artifact_publication_recording.publication_work(issue)
  execute_routes_for_work_with_mode(
    routes,
    repositories,
    config_dir,
    workflow_bundle_dir,
    state_root,
    output_manifest,
    work,
    run_id,
    checkpoint,
    runner,
    False,
    True,
  )
}

fn execute_routes_loop(
  routes: List(artifact_publication_config.PublicationRoute),
  repositories: artifact_publication_config.ArtifactRepositories,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
  recovered_execution: Bool,
  reuse_terminal_attempts: Bool,
  required_failures: List(artifact_publication_recording.PublicationFailure),
  optional_failures: List(artifact_publication_recording.PublicationFailure),
  attempts: List(artifact_publication_recording.PublicationAttemptSummary),
) -> Result(artifact_publication_recording.PublicationRecordingResult, String) {
  case routes {
    [] ->
      Ok(artifact_publication_recording.PublicationRecordingResult(
        required_failures,
        optional_failures,
        attempts,
      ))
    [route, ..rest] -> {
      use outcome <- result.try(execute_route(
        route,
        repositories,
        output_manifest,
        config_dir,
        workflow_bundle_dir,
        state_root,
        work,
        run_id,
        checkpoint,
        runner,
        recovered_execution,
        reuse_terminal_attempts,
      ))
      let #(next_required, next_optional) = case outcome.failure {
        Some(failure) ->
          case failure.required {
            True -> #([failure, ..required_failures], optional_failures)
            False -> #(required_failures, [failure, ..optional_failures])
          }
        None -> #(required_failures, optional_failures)
      }
      execute_routes_loop(
        rest,
        repositories,
        output_manifest,
        config_dir,
        workflow_bundle_dir,
        state_root,
        work,
        run_id,
        checkpoint,
        runner,
        recovered_execution,
        reuse_terminal_attempts,
        next_required,
        next_optional,
        [outcome.attempt, ..attempts],
      )
    }
  }
}

type RouteExecutionOutcome {
  RouteExecutionOutcome(
    attempt: artifact_publication_recording.PublicationAttemptSummary,
    failure: Option(artifact_publication_recording.PublicationFailure),
  )
}

fn execute_route(
  route: artifact_publication_config.PublicationRoute,
  repositories: artifact_publication_config.ArtifactRepositories,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  config_dir: String,
  workflow_bundle_dir: String,
  state_root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
  recovered_execution: Bool,
  reuse_terminal_attempts: Bool,
) -> Result(RouteExecutionOutcome, String) {
  case
    artifact_publication_recording.load_body_templates(
      [route],
      repositories,
      config_dir,
      workflow_bundle_dir,
    )
  {
    Error(reason) ->
      record_route_failure(
        route,
        output_manifest.workflow_id,
        work,
        run_id,
        checkpoint,
        failure_code(reason),
        failure_message(reason),
      )
    Ok(body_templates) ->
      case
        artifact_publication_planner.plan_publication(
          output_manifest,
          repositories,
          route,
          checkpoint_store(checkpoint),
          work,
          run_id,
          body_templates,
        )
      {
        Ok(planned) -> {
          let existing_attempt = case reuse_terminal_attempts {
            True ->
              existing_terminal_attempt(
                state_root,
                run_id,
                route.id,
                planned.version_id,
                recovered_execution,
                planned.pull_request.enabled,
                planned.repository_kind == "github",
              )
            False -> None
          }
          case existing_attempt {
            Some(attempt) ->
              Ok(RouteExecutionOutcome(attempt, failure_from_attempt(attempt)))
            None ->
              case
                prepare_repository_execution_input(route, planned, checkpoint)
              {
                Ok(prepared) -> {
                  let manifest =
                    github_repository.publish(
                      prepared,
                      state_root,
                      runner,
                      checkpoint.now_ms(),
                    )
                  artifact_publication_recording.record_manifest_attempt(
                    route,
                    output_manifest.workflow_id,
                    run_id,
                    manifest,
                    checkpoint,
                  )
                  |> result.map(fn(attempt) {
                    case manifest.error {
                      Some(artifact_publication_manifest.PublicationErrorInfo(
                        code,
                        message,
                      )) ->
                        RouteExecutionOutcome(
                          attempt,
                          Some(
                            artifact_publication_recording.PublicationFailure(
                              route.id,
                              code,
                              message,
                              route.required,
                            ),
                          ),
                        )
                      None -> RouteExecutionOutcome(attempt, None)
                    }
                  })
                }
                Error(#(code, message)) ->
                  record_route_failure(
                    route,
                    output_manifest.workflow_id,
                    work,
                    run_id,
                    checkpoint,
                    code,
                    message,
                  )
              }
          }
        }
        Error(planner_error) -> {
          let artifact_publication_planner.PlannerError(message: message, ..) =
            planner_error
          record_route_failure(
            route,
            output_manifest.workflow_id,
            work,
            run_id,
            checkpoint,
            artifact_publication_planner.code(planner_error),
            message,
          )
        }
      }
  }
}

fn failure_code(reason: String) -> String {
  case string.split_once(reason, on: ":") {
    Ok(#(code, _)) -> code
    Error(_) -> reason
  }
}

fn failure_message(reason: String) -> String {
  case string.split_once(reason, on: ":") {
    Ok(#(_, message)) -> string.trim(message)
    Error(_) -> reason
  }
}

fn existing_terminal_attempt(
  workspace_root: String,
  run_id: String,
  publication_id: String,
  version_id: String,
  recovered_execution: Bool,
  requires_pr: Bool,
  requires_branch_metadata: Bool,
) -> Option(artifact_publication_recording.PublicationAttemptSummary) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(_) -> None
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Error(_) -> None
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
        Error(decode_error) -> {
          let _ = decode_error
          None
        }
      }
    Error(read_error) -> {
      let _ = read_error
      None
    }
  }
}

fn manifest_requires_pr(
  manifest: artifact_publication_manifest.PublicationManifest,
) -> Bool {
  case manifest.dry_run_manifest {
    Some(planned) -> planned.pull_request.enabled
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

fn failure_from_attempt(
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
    manifest_ref: option.unwrap(attempt.manifest_ref, ""),
    manifest_sha256: option.unwrap(attempt.manifest_sha256, ""),
    manifest_bytes: option.unwrap(attempt.manifest_bytes, 0),
    error_code: attempt.error_code,
    error_message: attempt.error_message,
  )
}

fn prepare_repository_execution_input(
  route: artifact_publication_config.PublicationRoute,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(types.PublicationExecutionInput, #(String, String)) {
  case
    artifact_publication_config.repository_ref_parts(
      route.repository,
      "publication.repository",
    )
  {
    Ok(#("github", _)) ->
      github_repository.prepare_publication_input(
        planned,
        checkpoint_store(checkpoint),
      )
      |> result.map(fn(input) { input })
      |> result.map_error(fn(error) {
        #(github_repository.code(error), github_repository.message(error))
      })
    Ok(#(kind, _)) ->
      Error(#("unsupported_repository", "unsupported repository kind: " <> kind))
    Error(error) ->
      Error(#(
        "invalid_repository",
        artifact_publication_config.error_message(error),
      ))
  }
}

fn record_route_failure(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  code: String,
  message: String,
) -> Result(RouteExecutionOutcome, String) {
  artifact_publication_recording.record_failed_attempt(
    route,
    workflow_id,
    work,
    run_id,
    checkpoint,
    code,
    message,
  )
  |> result.map(fn(pair) {
    let #(failure, attempt) = pair
    RouteExecutionOutcome(attempt, Some(failure))
  })
}

fn checkpoint_store(
  checkpoint: workflow_checkpoint.Writer,
) -> artifact_store.Store {
  artifact_store.custom(
    "workflow-checkpoint",
    artifact_store.StoreCallbacks(
      write: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_executor_write_unsupported",
        ))
      },
      read: fn(ref) {
        checkpoint.read_artifact(ref)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
        })
      },
      write_immutable_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_executor_write_unsupported",
        ))
      },
      read_bytes: fn(ref) {
        checkpoint.read_artifact(ref)
        |> result.map(bit_array.from_string)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
        })
      },
      locate: fn(ref) {
        checkpoint.artifact_location(ref)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
        })
      },
    ),
  )
}
