import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_config
import scherzo/artifact_publication_driver
import scherzo/artifact_publication_executor
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_publication_runtime
import scherzo/artifact_repository/command_runner
import scherzo/ctl/schedule_state
import scherzo/path
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/workflow_checkpoint
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import simplifile

pub fn retry(
  root: String,
  json_output: Bool,
  run_id: String,
  publication_id: Option(String),
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  retry_with_runner(
    root,
    json_output,
    run_id,
    publication_id,
    command_runner.production(),
    output_line,
  )
}

pub fn retry_with_runner(
  root: String,
  json_output: Bool,
  run_id: String,
  publication_id: Option(String),
  runner: command_runner.Runner,
  output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(schedule_state.load_projection(root, pair_error))
  use config_path <- result.try(require_config_path(root))
  use bundle <- result.try(
    runtime_bundle.load(Some(config_path))
    |> result.map_error(fn(error) {
      let runtime_bundle.BundleError(code: code, message: message) = error
      #("publication_retry_config_load_failed", code <> ": " <> message)
    }),
  )
  let checkpoint = workflow_checkpoint.ledger_writer(root, monotonic_ms)
  use attempts <- result.try(retry_selected_publications(
    projected,
    root,
    run_id,
    publication_id,
    bundle,
    checkpoint,
    runner,
  ))
  case json_output {
    True ->
      output_line(
        json.object([
          #("run_id", json.string(run_id)),
          #("publication_id", optional_string_json(publication_id)),
          #("workspace_root", json.string(root)),
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
      print_retry_results(run_id, publication_id, root, attempts, output_line)
  }
  Ok(Nil)
}

pub fn retry_attempts_with_bundle_runner(
  root: String,
  run_id: String,
  publication_id: Option(String),
  bundle: runtime_bundle.RuntimeBundle,
  runner: command_runner.Runner,
) -> Result(List(projection.PublicationAttempt), #(String, String)) {
  use projected <- result.try(schedule_state.load_projection(root, pair_error))
  let checkpoint = workflow_checkpoint.ledger_writer(root, monotonic_ms)
  retry_selected_publications(
    projected,
    root,
    run_id,
    publication_id,
    bundle,
    checkpoint,
    runner,
  )
}

pub type PublicationRecoveryInspection {
  RetryablePublicationAttempts(attempts: List(projection.PublicationAttempt))
  RequiredPublicationsAlreadyPublished(
    attempts: List(projection.PublicationAttempt),
  )
}

pub fn inspect_retryable_attempts(
  projected: projection.Projection,
  run_id: String,
  publication_id: Option(String),
) -> Result(List(projection.PublicationAttempt), #(String, String)) {
  use _ <- result.try(require_publication_run(projected, run_id))
  use targets <- result.try(select_retry_targets(
    projected,
    run_id,
    publication_id,
  ))
  use _ <- result.try(require_output_manifest_ref(projected, run_id))
  Ok(retry_targets_to_attempts(targets))
}

pub fn inspect_publication_recovery(
  projected: projection.Projection,
  run_id: String,
) -> Result(PublicationRecoveryInspection, #(String, String)) {
  use _ <- result.try(require_publication_run(projected, run_id))
  use _ <- result.try(require_output_manifest_ref(projected, run_id))
  case select_retry_targets(projected, run_id, None) {
    Ok(targets) ->
      Ok(RetryablePublicationAttempts(retry_targets_to_attempts(targets)))
    Error(#("publication_retry_targets_not_found", _)) -> {
      use attempts <- result.try(require_required_publications_published(
        projected,
        run_id,
      ))
      Ok(RequiredPublicationsAlreadyPublished(attempts))
    }
    Error(error) -> Error(error)
  }
}

type RetrySelection {
  RetrySelection(latest: projection.PublicationAttempt)
}

type RetryResolvedRoute {
  RetryResolvedRoute(route: artifact_publication_config.PublicationRoute)
}

type PublicationManifestDetails {
  PublicationManifestDetails(
    branch: Option(String),
    commit_sha: Option(String),
    pr_url: Option(String),
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

fn select_retry_targets(
  projected: projection.Projection,
  run_id: String,
  publication_id: Option(String),
) -> Result(List(RetrySelection), #(String, String)) {
  case publication_id {
    Some(publication_id) -> {
      let attempts =
        projection.publication_attempts_for_run(
          projected,
          run_id,
          publication_id,
        )
      use latest <- result.try(publication_or_not_found(
        attempts,
        publication_id,
      ))
      use _ <- result.try(require_retryable_latest(latest))
      Ok([RetrySelection(latest: latest)])
    }
    None -> {
      let #(targets, cannot_replan) =
        projection.publication_ids_for_run(projected, run_id)
        |> list.fold(#([], []), fn(acc, publication_id) {
          let #(targets, cannot_replan) = acc
          let attempts =
            projection.publication_attempts_for_run(
              projected,
              run_id,
              publication_id,
            )
          case publication_or_not_found(attempts, publication_id) {
            Ok(latest) ->
              case retry_eligibility(latest) {
                artifact_publication_manifest.RetryAllowed -> #(
                  [RetrySelection(latest: latest), ..targets],
                  cannot_replan,
                )
                artifact_publication_manifest.RetryCannotReplan(reason) -> #(
                  targets,
                  [#(latest, reason), ..cannot_replan],
                )
                artifact_publication_manifest.RetryNotRetryable -> acc
              }
            Error(_) -> acc
          }
        })
      case list.reverse(cannot_replan), list.reverse(targets) {
        [#(latest, reason), ..], _ ->
          Error(artifact_publication_manifest.retry_replan_unavailable_error(
            latest.publication_id,
            reason,
          ))
        [], [] -> Error(publication_retry_targets_not_found_error(run_id))
        [], targets -> Ok(targets)
      }
    }
  }
}

fn retry_targets_to_attempts(
  targets: List(RetrySelection),
) -> List(projection.PublicationAttempt) {
  list.map(targets, fn(target) { target.latest })
}

fn require_required_publications_published(
  projected: projection.Projection,
  run_id: String,
) -> Result(List(projection.PublicationAttempt), #(String, String)) {
  case projection.publication_ids_for_run(projected, run_id) {
    [] -> Error(publication_retry_targets_not_found_error(run_id))
    publication_ids -> {
      use attempts <- result.try(
        latest_publication_attempts(projected, run_id, publication_ids, []),
      )
      case list.all(attempts, required_publication_attempt_is_published) {
        True -> Ok(attempts)
        False -> Error(publication_retry_targets_not_found_error(run_id))
      }
    }
  }
}

fn latest_publication_attempts(
  projected: projection.Projection,
  run_id: String,
  publication_ids: List(String),
  acc: List(projection.PublicationAttempt),
) -> Result(List(projection.PublicationAttempt), #(String, String)) {
  case publication_ids {
    [] -> Ok(list.reverse(acc))
    [publication_id, ..rest] ->
      case
        projection.latest_publication_for_run(projected, run_id, publication_id)
      {
        Ok(latest) ->
          latest_publication_attempts(projected, run_id, rest, [latest, ..acc])
        Error(Nil) ->
          Error(#(
            "publication_not_found",
            "publication not found: " <> publication_id,
          ))
      }
  }
}

fn required_publication_attempt_is_published(
  attempt: projection.PublicationAttempt,
) -> Bool {
  !attempt.required
  || attempt.status == "published"
  || attempt.status == "unchanged"
}

fn publication_retry_targets_not_found_error(
  run_id: String,
) -> #(String, String) {
  #(
    "publication_retry_targets_not_found",
    "no failed retryable publications found for run: " <> run_id,
  )
}

fn retry_selected_publications(
  projected: projection.Projection,
  root: String,
  run_id: String,
  publication_id: Option(String),
  bundle: runtime_bundle.RuntimeBundle,
  checkpoint: workflow_checkpoint.Writer,
  runner: command_runner.Runner,
) -> Result(List(projection.PublicationAttempt), #(String, String)) {
  use _ <- result.try(require_publication_run(projected, run_id))
  use targets <- result.try(select_retry_targets(
    projected,
    run_id,
    publication_id,
  ))
  use output_manifest_ref <- result.try(require_output_manifest_ref(
    projected,
    run_id,
  ))
  use output_manifest <- result.try(
    workflow_contract_manifest.load_retained_output_manifest(
      root,
      output_manifest_ref.artifact_ref,
      output_manifest_ref.artifact_sha256,
      output_manifest_ref.artifact_bytes,
    ),
  )
  use workflow_status <- result.try(require_workflow_run(projected, run_id))
  use #(_, workflow) <- result.try(
    runtime_bundle.workflow_by_id(bundle, output_manifest.workflow_id)
    |> result.map_error(fn(error) {
      let runtime_bundle.BundleError(code: code, message: message) = error
      #(code, message)
    }),
  )
  use work <- result.try(publication_workflow_identity(
    projected,
    run_id,
    workflow_status,
  ))
  use resolved <- result.try(resolve_retry_routes(
    targets,
    workflow,
    bundle,
    output_manifest,
    root,
    work,
    run_id,
  ))
  let routes = list.map(resolved, fn(entry) { entry.route })
  let workflow_bundle_dir =
    runtime_bundle.workflow_bundle_dir(bundle, workflow.id)
  use publication_driver <- result.try(retry_publication_driver(
    routes,
    workflow,
    bundle,
    workflow_bundle_dir,
    run_id,
    workflow_status,
    work,
  ))
  let retry_result =
    artifact_publication_executor.retry_routes_for_work_with_state_root_and_publication_driver(
      routes,
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      workflow_bundle_dir,
      root,
      output_manifest,
      work,
      run_id,
      checkpoint,
      runner,
      publication_driver,
      list.all(targets, fn(target) { target.latest.status != "unchanged" }),
    )
  use
    artifact_publication_recording.PublicationRecordingResult(
      required_failures: required_failures,
      optional_failures: optional_failures,
      attempts: attempts,
    )
  <- result.try(
    retry_result
    |> result.map_error(fn(message) { #("publication_retry_failed", message) }),
  )
  case list.append(required_failures, optional_failures) {
    [failure, ..] ->
      Error(#(
        "publication_retry_attempt_failed",
        "publication retry recorded failed attempt for "
          <> failure.publication_id
          <> ": "
          <> failure.code
          <> ": "
          <> failure.message,
      ))
    [] ->
      Ok(
        list.map(attempts, fn(attempt) {
          projection_attempt_from_summary(
            run_id,
            output_manifest.workflow_id,
            attempt,
          )
        }),
      )
  }
}

fn retry_publication_driver(
  routes: List(artifact_publication_config.PublicationRoute),
  workflow: workflow_dag.WorkflowDag,
  bundle: runtime_bundle.RuntimeBundle,
  workflow_bundle_dir: String,
  run_id: String,
  workflow_status: projection.WorkflowRunStatus,
  work: artifact_publication_planner.PublicationWork,
) -> Result(
  Option(artifact_publication_driver.WorkspacePublicationDriver),
  #(String, String),
) {
  case routes_require_commit_stack(routes) {
    False -> Ok(None)
    True ->
      artifact_publication_runtime.driver_for_retained_run(
        workflow,
        bundle.orchestrator,
        workflow_bundle_dir,
        run_id,
        workflow_status_run_root(workflow_status),
        work,
      )
      |> result.map(Some)
      |> result.map_error(fn(message) {
        #("publication_retry_workspace_driver_unavailable", message)
      })
  }
}

fn routes_require_commit_stack(
  routes: List(artifact_publication_config.PublicationRoute),
) -> Bool {
  list.any(routes, fn(route) {
    route.mode == artifact_publication_config.CommitStackPublication
  })
}

fn resolve_retry_routes(
  targets: List(RetrySelection),
  workflow: workflow_dag.WorkflowDag,
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
) -> Result(List(RetryResolvedRoute), #(String, String)) {
  resolve_retry_routes_loop(
    targets,
    workflow.publication_routes,
    bundle,
    output_manifest,
    root,
    work,
    run_id,
    [],
  )
}

fn resolve_retry_routes_loop(
  targets: List(RetrySelection),
  routes: List(artifact_publication_config.PublicationRoute),
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  acc: List(RetryResolvedRoute),
) -> Result(List(RetryResolvedRoute), #(String, String)) {
  case targets {
    [] -> Ok(list.reverse(acc))
    [RetrySelection(latest: latest), ..rest] -> {
      use route <- result.try(find_retry_route(routes, latest.publication_id))
      use _ <- result.try(validate_retry_route(
        route,
        latest,
        bundle,
        output_manifest,
        root,
        work,
        run_id,
      ))
      resolve_retry_routes_loop(
        rest,
        routes,
        bundle,
        output_manifest,
        root,
        work,
        run_id,
        [RetryResolvedRoute(route: route), ..acc],
      )
    }
  }
}

fn find_retry_route(
  routes: List(artifact_publication_config.PublicationRoute),
  publication_id: String,
) -> Result(artifact_publication_config.PublicationRoute, #(String, String)) {
  case list.filter(routes, fn(route) { route.id == publication_id }) {
    [route] -> Ok(route)
    [] ->
      Error(#(
        "publication_retry_config_drift",
        "current workflow no longer defines publication route: "
          <> publication_id,
      ))
    [_, _, ..] ->
      Error(#(
        "publication_retry_config_drift",
        "current workflow defines publication route more than once: "
          <> publication_id,
      ))
  }
}

fn validate_retry_route(
  route: artifact_publication_config.PublicationRoute,
  latest: projection.PublicationAttempt,
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
) -> Result(Nil, #(String, String)) {
  use body_templates <- result.try(
    artifact_publication_recording.load_body_templates(
      [route],
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, output_manifest.workflow_id),
    )
    |> result.map_error(fn(message) {
      #("publication_retry_config_invalid", message)
    }),
  )
  use planned <- result.try(
    artifact_publication_planner.plan_publication(
      output_manifest,
      bundle.orchestrator.artifact_repositories,
      route,
      artifact_store.new(root),
      work,
      run_id,
      body_templates,
    )
    |> result.map_error(fn(error) {
      #(artifact_publication_planner.code(error), planner_error_message(error))
    }),
  )
  let legacy_series_id =
    work.id
    <> ":"
    <> output_manifest.workflow_id
    <> ":"
    <> latest.publication_id
  let identity_matches = case latest.version_id {
    Some(version_id) ->
      planned.series_id == latest.series_id && planned.version_id == version_id
    None ->
      latest.series_id == legacy_series_id
      || planned.series_id == latest.series_id
  }
  let matches = identity_matches && planned.required == latest.required
  case matches {
    True -> Ok(Nil)
    False ->
      Error(#(
        "publication_retry_config_drift",
        "current workflow publication config no longer matches retained retry target: "
          <> latest.publication_id,
      ))
  }
}

fn require_retryable_latest(
  latest: projection.PublicationAttempt,
) -> Result(Nil, #(String, String)) {
  case retry_eligibility(latest) {
    artifact_publication_manifest.RetryAllowed -> Ok(Nil)
    artifact_publication_manifest.RetryCannotReplan(reason) ->
      Error(artifact_publication_manifest.retry_replan_unavailable_error(
        latest.publication_id,
        reason,
      ))
    artifact_publication_manifest.RetryNotRetryable ->
      Error(#(
        "publication_not_retryable",
        "latest publication attempt is not retryable: "
          <> latest.publication_id
          <> " status="
          <> latest.status,
      ))
  }
}

fn retry_eligibility(
  latest: projection.PublicationAttempt,
) -> artifact_publication_manifest.RetryEligibility {
  artifact_publication_manifest.retry_eligibility_for_attempt(
    latest.status,
    retryable: latest.retryable,
    retry_execution_available: latest.retry_execution_available,
    version_id: latest.version_id,
  )
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

fn print_retry_results(
  run_id: String,
  publication_id: Option(String),
  root: String,
  attempts: List(projection.PublicationAttempt),
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("run_id: " <> run_id)
  case publication_id {
    Some(publication_id) -> output_line("publication_id: " <> publication_id)
    None -> output_line("publication_id: all failed retryable publications")
  }
  output_line("attempts:")
  list.each(attempts, fn(attempt) {
    print_retry_attempt(root, attempt, output_line)
  })
}

fn print_retry_attempt(
  root: String,
  attempt: projection.PublicationAttempt,
  output_line: fn(String) -> Nil,
) -> Nil {
  let details = manifest_details_for_attempt(root, attempt)
  output_line("- publication_id: " <> attempt.publication_id)
  output_line("  status: " <> attempt.status)
  output_line("  attempt_id: " <> attempt.attempt_id)
  output_line("  manifest_ref: " <> optional_string(attempt.manifest_ref))
  output_line("  version_id: " <> optional_string(attempt.version_id))
  output_line("  branch: " <> manifest_detail_string(details, branch_option))
  output_line(
    "  commit_sha: " <> manifest_detail_string(details, commit_sha_option),
  )
  output_line("  pr_url: " <> manifest_detail_string(details, pr_url_option))
  output_line("  retryable: " <> bool_string(attempt.retryable))
  output_line(
    "  retry_execution_available: "
    <> bool_string(attempt.retry_execution_available),
  )
  output_line("  error_code: " <> optional_string(attempt.error_code))
  output_line("  error_message: " <> optional_string(attempt.error_message))
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
    #("error_code", optional_string_json(attempt.error_code)),
    #("error_message", optional_string_json(attempt.error_message)),
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

fn require_output_manifest_ref(
  projected: projection.Projection,
  run_id: String,
) -> Result(projection.WorkflowContractManifestRef, #(String, String)) {
  case projection.workflow_output_manifest(projected, run_id) {
    Some(output_manifest) -> Ok(output_manifest)
    None ->
      Error(#(
        "publication_retry_output_manifest_missing",
        "workflow run is missing a retained output manifest: " <> run_id,
      ))
  }
}

fn require_workflow_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(projection.WorkflowRunStatus, #(String, String)) {
  projection.workflow_run(projected, run_id)
  |> result.map_error(fn(_) {
    #("publication_run_not_found", "publication run not found: " <> run_id)
  })
}

fn workflow_status_run_root(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(run_root: run_root, ..)
    | projection.WorkflowRunFinished(run_root: run_root, ..)
    | projection.WorkflowRunInterrupted(run_root: run_root, ..)
    | projection.WorkflowRunSuperseded(run_root: run_root, ..) -> run_root
  }
}

fn publication_workflow_identity(
  projected: projection.Projection,
  run_id: String,
  workflow_status: projection.WorkflowRunStatus,
) -> Result(artifact_publication_planner.PublicationWork, #(String, String)) {
  case workflow_status {
    projection.WorkflowRunActive(
      issue_id: issue_id,
      issue_identifier: issue_identifier,
      ..,
    ) -> {
      let source_url = case projection.workflow_task_ref(projected, run_id) {
        Ok(task_ref) -> task_ref.task_url
        Error(_) -> None
      }
      Ok(artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: issue_id,
        identifier: issue_identifier,
        slug: issue_identifier,
        title: None,
        url: source_url,
      ))
    }
    projection.WorkflowRunFinished(issue_id: issue_id, ..)
    | projection.WorkflowRunInterrupted(issue_id: issue_id, ..)
    | projection.WorkflowRunSuperseded(issue_id: issue_id, ..) -> {
      use task_ref <- result.try(
        projection.workflow_task_ref(projected, run_id)
        |> result.map_error(fn(_) {
          #(
            "publication_retry_task_ref_missing",
            "workflow run is missing retained task identity for retry: "
              <> run_id,
          )
        }),
      )
      let issue_identifier = case task_ref.task_key {
        Some(task_key) -> task_key
        None -> task_ref.task_remote_id
      }
      Ok(artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: issue_id,
        identifier: issue_identifier,
        slug: issue_identifier,
        title: None,
        url: task_ref.task_url,
      ))
    }
  }
}

fn projection_attempt_from_summary(
  run_id: String,
  workflow_id: String,
  attempt: artifact_publication_recording.PublicationAttemptSummary,
) -> projection.PublicationAttempt {
  projection.PublicationAttempt(
    run_id: run_id,
    workflow_id: workflow_id,
    publication_id: attempt.publication_id,
    series_id: attempt.series_id,
    attempt_id: attempt.attempt_id,
    status: attempt.status,
    required: attempt.required,
    retryable: attempt.retryable,
    retry_execution_available: attempt.retry_execution_available,
    version_id: attempt.version_id,
    manifest_ref: Some(attempt.manifest_ref),
    manifest_sha256: Some(attempt.manifest_sha256),
    manifest_bytes: Some(attempt.manifest_bytes),
    error_code: attempt.error_code,
    error_message: attempt.error_message,
    recorded_at_ms: attempt.recorded_at_ms,
  )
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

fn planner_error_message(
  error: artifact_publication_planner.PlannerError,
) -> String {
  let artifact_publication_planner.PlannerError(message: message, ..) = error
  message
}

fn require_config_path(root: String) -> Result(String, #(String, String)) {
  case config_path_for_root(root) {
    Some(config_path) -> Ok(config_path)
    None ->
      Error(#(
        "publication_retry_config_missing",
        "could not find scherzo.yaml for artifact publication retry; pass --root pointing at a workspace directory with a neighboring scherzo.yaml",
      ))
  }
}

fn config_path_for_root(root: String) -> Option(String) {
  config_candidates(root) |> first_existing_file
}

fn config_candidates(root: String) -> List(String) {
  let root = path.absolute_or_original(root)
  list.append([path.join(root, "scherzo.yaml")], parent_config_candidates(root))
}

fn parent_config_candidates(root: String) -> List(String) {
  case path.dirname(root) {
    Ok(parent) -> [path.join(parent, "scherzo.yaml")]
    Error(Nil) -> []
  }
}

fn first_existing_file(paths: List(String)) -> Option(String) {
  case paths {
    [] -> None
    [candidate, ..rest] ->
      case simplifile.is_file(candidate) {
        Ok(True) -> Some(candidate)
        Ok(False) | Error(_) -> first_existing_file(rest)
      }
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

fn optional_int_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn optional_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
