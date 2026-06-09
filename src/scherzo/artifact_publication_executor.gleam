import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_attempts
import scherzo/artifact_publication_config
import scherzo/artifact_publication_driver.{type WorkspacePublicationDriver}
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_repository/command_runner
import scherzo/state/artifact_store
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
    None,
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
  retry_routes_for_work_with_state_root_and_publication_driver(
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
    None,
    reuse_terminal_attempts,
  )
}

pub fn retry_routes_for_work_with_state_root_and_publication_driver(
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
  publication_driver: Option(WorkspacePublicationDriver),
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
    publication_driver,
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
    None,
    True,
    True,
  )
}

pub fn execute_recovered_routes_with_runner_and_state_root_and_publication_driver(
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
  publication_driver: Option(WorkspacePublicationDriver),
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
    publication_driver,
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
  publication_driver: Option(WorkspacePublicationDriver),
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
    publication_driver,
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
    None,
    False,
    True,
  )
}

pub fn execute_routes_with_runner_and_state_root_and_publication_driver(
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
  publication_driver: Option(WorkspacePublicationDriver),
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
    publication_driver,
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
  publication_driver: Option(WorkspacePublicationDriver),
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
        publication_driver,
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
        publication_driver,
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
  publication_driver: Option(WorkspacePublicationDriver),
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
          let existing_attempt = case
            reuse_terminal_attempts,
            artifact_publication_attempts.terminal_attempt_reuse_allowed(
              planned,
            )
          {
            True, True ->
              artifact_publication_attempts.existing_terminal_attempt(
                state_root,
                run_id,
                route.id,
                planned.version_id,
                recovered_execution,
                artifact_publication_attempts.planned_requires_pr(planned),
                planned.repository_kind == "github",
              )
            _, _ -> None
          }
          case existing_attempt {
            Some(attempt) ->
              Ok(RouteExecutionOutcome(
                attempt,
                artifact_publication_attempts.failure_from_attempt(attempt),
              ))
            None ->
              execute_planned_publication(
                route,
                output_manifest.workflow_id,
                planned,
                state_root,
                work,
                run_id,
                checkpoint,
                runner,
                publication_driver,
              )
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

fn execute_planned_publication(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  state_root: String,
  _work: artifact_publication_planner.PublicationWork,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
  publication_driver: Option(WorkspacePublicationDriver),
) -> Result(RouteExecutionOutcome, String) {
  case planned.commit_stack {
    Some(_) ->
      execute_commit_stack_publication(
        route,
        workflow_id,
        planned,
        state_root,
        run_id,
        checkpoint,
        runner,
        publication_driver,
      )
    None ->
      execute_unsupported_file_publication(
        route,
        workflow_id,
        planned,
        run_id,
        checkpoint,
      )
  }
}

fn execute_unsupported_file_publication(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(RouteExecutionOutcome, String) {
  let now_ms = checkpoint.now_ms()
  let error =
    artifact_publication_manifest.PublicationErrorInfo(
      code: "file_publication_unsupported",
      message: "GitHub file artifact publication no longer uses Scherzo-managed checkouts; use a workflow workspace-driver publication step or a mode: commit_stack publication route",
    )
  let attempt_id =
    artifact_publication_manifest.attempt_key_for_failure(
      planned.publication_id,
      error.code,
      error.message,
      now_ms,
    )
  let manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      attempt_id,
      now_ms,
      False,
      Some(planned.branch),
      None,
      planned_target_pr_url(planned),
      [],
      [],
      error,
    )
  record_execution_manifest(route, workflow_id, run_id, manifest, checkpoint)
}

fn execute_commit_stack_publication(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  planned: artifact_publication_planner.DryRunPublicationManifest,
  state_root: String,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
  publication_driver: Option(WorkspacePublicationDriver),
) -> Result(RouteExecutionOutcome, String) {
  let now_ms = checkpoint.now_ms()
  let attempt_id =
    artifact_publication_attempts.success_attempt_id(
      state_root,
      run_id,
      route.id,
      planned,
      now_ms,
    )
  let manifest = case
    artifact_publication_driver.publish_commit_stack(
      planned,
      publication_driver,
      runner,
    )
  {
    Ok(driver_result) ->
      driver_success_manifest(planned, attempt_id, now_ms, driver_result)
    Error(error) -> failed_driver_publication_manifest(planned, now_ms, error)
  }
  record_execution_manifest(route, workflow_id, run_id, manifest, checkpoint)
}

fn record_execution_manifest(
  route: artifact_publication_config.PublicationRoute,
  workflow_id: String,
  run_id: String,
  manifest: artifact_publication_manifest.PublicationManifest,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(RouteExecutionOutcome, String) {
  artifact_publication_recording.record_manifest_attempt(
    route,
    workflow_id,
    run_id,
    manifest,
    checkpoint,
  )
  |> result.map(fn(attempt) {
    case manifest.error {
      Some(artifact_publication_manifest.PublicationErrorInfo(code, message)) ->
        RouteExecutionOutcome(
          attempt,
          Some(artifact_publication_recording.PublicationFailure(
            route.id,
            code,
            message,
            route.required,
          )),
        )
      None -> RouteExecutionOutcome(attempt, None)
    }
  })
}

fn driver_success_manifest(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  attempt_id: String,
  now_ms: Int,
  result: artifact_publication_driver.DriverPublicationResult,
) -> artifact_publication_manifest.PublicationManifest {
  let pr_url = driver_result_pr_url(planned, result.url)
  let manifest = case result.status {
    "unchanged" ->
      artifact_publication_manifest.unchanged_manifest(
        planned,
        attempt_id,
        now_ms,
        Some(result.head_revision),
        pr_url,
        [],
      )
    _ ->
      artifact_publication_manifest.published_manifest(
        planned,
        attempt_id,
        now_ms,
        result.head_revision,
        pr_url,
        [],
        [],
      )
  }
  artifact_publication_manifest.PublicationManifest(
    ..manifest,
    branch: Some(result.branch),
    commit_sha: Some(result.head_revision),
    pr_url: pr_url,
    base_ref: Some(result.base_ref),
    base_revision: Some(result.base_revision),
    head_revision: Some(result.head_revision),
    change_id: result.change_id,
  )
}

fn driver_result_pr_url(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  url: Option(String),
) -> Option(String) {
  case url, planned.target {
    Some(url), _ -> Some(url)
    None, artifact_publication_planner.ExistingPrBranchTargetPlan(target) ->
      non_empty_string_option(target.pr_url)
    None, artifact_publication_planner.StableBranchTargetPlan -> None
  }
}

fn non_empty_string_option(value: String) -> Option(String) {
  case string.trim(value) {
    "" -> None
    trimmed -> Some(trimmed)
  }
}

fn failed_driver_publication_manifest(
  planned: artifact_publication_planner.DryRunPublicationManifest,
  now_ms: Int,
  error: artifact_publication_manifest.PublicationErrorInfo,
) -> artifact_publication_manifest.PublicationManifest {
  let attempt_id =
    artifact_publication_manifest.attempt_key_for_failure(
      planned.publication_id,
      error.code,
      error.message,
      now_ms,
    )
  artifact_publication_manifest.failed_from_planned_manifest(
    planned,
    attempt_id,
    now_ms,
    True,
    Some(planned.branch),
    None,
    planned_target_pr_url(planned),
    [],
    [],
    error,
  )
}

fn planned_target_pr_url(
  planned: artifact_publication_planner.DryRunPublicationManifest,
) -> Option(String) {
  case planned.target {
    artifact_publication_planner.ExistingPrBranchTargetPlan(target) ->
      non_empty_string_option(target.pr_url)
    artifact_publication_planner.StableBranchTargetPlan -> None
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

fn read_checkpoint_artifact_bytes(
  checkpoint: workflow_checkpoint.Writer,
  ref: String,
) -> Result(BitArray, artifact_store.ArtifactError) {
  case checkpoint.artifact_location(ref) {
    Ok(artifact_store.ArtifactLocation(local_path: Some(local_path), ..)) ->
      artifact_store.read_file_bytes(local_path)
    Ok(_) -> read_checkpoint_artifact_text_as_bytes(checkpoint, ref)
    Error(error) ->
      Error(
        artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error)),
      )
  }
}

fn read_checkpoint_artifact_text_as_bytes(
  checkpoint: workflow_checkpoint.Writer,
  ref: String,
) -> Result(BitArray, artifact_store.ArtifactError) {
  checkpoint.read_artifact(ref)
  |> result.map(bit_array.from_string)
  |> result.map_error(fn(error) {
    artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
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
      write_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_executor_write_unsupported",
        ))
      },
      write_immutable_bytes: fn(_, _) {
        Error(artifact_store.ArtifactIo(
          "publication_executor_write_unsupported",
        ))
      },
      read_bytes: fn(ref) { read_checkpoint_artifact_bytes(checkpoint, ref) },
      locate: fn(ref) {
        checkpoint.artifact_location(ref)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactIo(workflow_checkpoint.describe_error(error))
        })
      },
    ),
  )
}
