import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_config
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_publication_route_discovery
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag

pub type RetrySelection {
  RetrySelection(latest: projection.PublicationAttempt)
  DeclaredRetrySelection(route: artifact_publication_config.PublicationRoute)
}

pub type RetryContext {
  RetryContext(
    output_manifest: workflow_contract_manifest.ContractOutputManifest,
    workflow_status: projection.WorkflowRunStatus,
    workflow: workflow_dag.WorkflowDag,
    work: artifact_publication_planner.PublicationWork,
  )
}

pub fn load_context(
  projected: projection.Projection,
  root: String,
  run_id: String,
  bundle: runtime_bundle.RuntimeBundle,
) -> Result(RetryContext, #(String, String)) {
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
  Ok(RetryContext(output_manifest, workflow_status, workflow, work))
}

pub fn select_legacy_targets(
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

pub fn select_with_declared_routes(
  projected: projection.Projection,
  root: String,
  run_id: String,
  publication_id: Option(String),
  bundle: runtime_bundle.RuntimeBundle,
  context: RetryContext,
) -> Result(List(RetrySelection), #(String, String)) {
  case publication_id {
    Some(publication_id) ->
      case select_legacy_targets(projected, run_id, Some(publication_id)) {
        Ok(targets) -> Ok(targets)
        Error(#("publication_not_found", _)) ->
          select_declared_targets(
            projected,
            root,
            run_id,
            Some(publication_id),
            bundle,
            context,
          )
        Error(error) -> Error(error)
      }
    None -> {
      let historical = case select_legacy_targets(projected, run_id, None) {
        Ok(targets) -> Ok(targets)
        Error(#("publication_retry_targets_not_found", _)) -> Ok([])
        Error(error) -> Error(error)
      }
      use historical <- result.try(historical)
      use declared <- result.try(select_declared_targets(
        projected,
        root,
        run_id,
        None,
        bundle,
        context,
      ))
      case list.append(historical, declared) {
        [] -> Error(publication_retry_targets_not_found_error(run_id))
        targets -> Ok(targets)
      }
    }
  }
}

pub fn targets_to_attempts(
  projected: projection.Projection,
  run_id: String,
  targets: List(RetrySelection),
) -> List(projection.PublicationAttempt) {
  list.map(targets, fn(target) {
    case target {
      RetrySelection(latest: latest) -> latest
      DeclaredRetrySelection(route: route) ->
        declared_retry_attempt(projected, run_id, route)
    }
  })
}

pub fn resolve_routes(
  targets: List(RetrySelection),
  bundle: runtime_bundle.RuntimeBundle,
  context: RetryContext,
  root: String,
  run_id: String,
) -> Result(
  List(artifact_publication_config.PublicationRoute),
  #(String, String),
) {
  resolve_routes_loop(
    targets,
    workflow_dag.publication_routes(context.workflow),
    bundle,
    context.output_manifest,
    root,
    context.work,
    run_id,
    [],
  )
}

pub fn should_publish(target: RetrySelection) -> Bool {
  case target {
    RetrySelection(latest: latest) -> latest.status != "unchanged"
    DeclaredRetrySelection(_) -> True
  }
}

pub fn require_output_manifest_ref(
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

pub fn publication_retry_targets_not_found_error(
  run_id: String,
) -> #(String, String) {
  #(
    "publication_retry_targets_not_found",
    "no failed retryable publications found for run: " <> run_id,
  )
}

fn select_declared_targets(
  projected: projection.Projection,
  root: String,
  run_id: String,
  publication_id: Option(String),
  bundle: runtime_bundle.RuntimeBundle,
  context: RetryContext,
) -> Result(List(RetrySelection), #(String, String)) {
  let routes =
    declared_routes_without_attempts(
      projected,
      run_id,
      workflow_dag.publication_routes(context.workflow),
      publication_id,
    )
  case routes, publication_id {
    [], Some(publication_id) ->
      Error(#(
        "publication_not_found",
        "publication not found: " <> publication_id,
      ))
    [], None -> Ok([])
    [_, ..], _ -> {
      use _ <- result.try(
        artifact_publication_route_discovery.ensure_current_routes_safe(
          projected,
          bundle,
          root,
          run_id,
          context.workflow,
        ),
      )
      Ok(list.map(routes, fn(route) { DeclaredRetrySelection(route: route) }))
    }
  }
}

fn declared_routes_without_attempts(
  projected: projection.Projection,
  run_id: String,
  routes: List(artifact_publication_config.PublicationRoute),
  publication_id: Option(String),
) -> List(artifact_publication_config.PublicationRoute) {
  let attempted_ids = projection.publication_ids_for_run(projected, run_id)
  routes
  |> list.filter(fn(route) {
    !list.contains(attempted_ids, route.id)
    && case publication_id {
      Some(publication_id) -> route.id == publication_id
      None -> True
    }
  })
}

fn declared_retry_attempt(
  projected: projection.Projection,
  run_id: String,
  route: artifact_publication_config.PublicationRoute,
) -> projection.PublicationAttempt {
  let workflow_id = case projection.workflow_run(projected, run_id) {
    Ok(status) -> workflow_status_workflow_id(status)
    Error(Nil) -> ""
  }
  projection.PublicationAttempt(
    run_id: run_id,
    workflow_id: workflow_id,
    publication_id: route.id,
    series_id: "",
    attempt_id: "declared-pending",
    status: "pending",
    required: route.required,
    retryable: True,
    retry_execution_available: True,
    version_id: None,
    manifest_ref: None,
    manifest_sha256: None,
    manifest_bytes: None,
    error_code: None,
    error_message: None,
    recorded_at_ms: 0,
  )
}

fn resolve_routes_loop(
  targets: List(RetrySelection),
  routes: List(artifact_publication_config.PublicationRoute),
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  acc: List(artifact_publication_config.PublicationRoute),
) -> Result(
  List(artifact_publication_config.PublicationRoute),
  #(String, String),
) {
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
      resolve_routes_loop(
        rest,
        routes,
        bundle,
        output_manifest,
        root,
        work,
        run_id,
        [route, ..acc],
      )
    }
    [DeclaredRetrySelection(route: route), ..rest] -> {
      use _ <- result.try(validate_declared_retry_route(
        route,
        bundle,
        output_manifest,
        root,
        work,
        run_id,
      ))
      resolve_routes_loop(
        rest,
        routes,
        bundle,
        output_manifest,
        root,
        work,
        run_id,
        [route, ..acc],
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
  use body_templates <- result.try(load_body_templates(
    route,
    bundle,
    output_manifest,
  ))
  use planned <- result.try(plan_route(
    route,
    bundle,
    output_manifest,
    root,
    work,
    run_id,
    body_templates,
  ))
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

fn validate_declared_retry_route(
  route: artifact_publication_config.PublicationRoute,
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
) -> Result(Nil, #(String, String)) {
  use body_templates <- result.try(load_body_templates(
    route,
    bundle,
    output_manifest,
  ))
  use _planned <- result.try(plan_route(
    route,
    bundle,
    output_manifest,
    root,
    work,
    run_id,
    body_templates,
  ))
  Ok(Nil)
}

fn load_body_templates(
  route: artifact_publication_config.PublicationRoute,
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
) -> Result(Dict(String, String), #(String, String)) {
  artifact_publication_recording.load_body_templates(
    [route],
    bundle.orchestrator.artifact_repositories,
    bundle.orchestrator.config_dir,
    runtime_bundle.workflow_bundle_dir(bundle, output_manifest.workflow_id),
  )
  |> result.map_error(fn(message) {
    #("publication_retry_config_invalid", message)
  })
}

fn plan_route(
  route: artifact_publication_config.PublicationRoute,
  bundle: runtime_bundle.RuntimeBundle,
  output_manifest: workflow_contract_manifest.ContractOutputManifest,
  root: String,
  work: artifact_publication_planner.PublicationWork,
  run_id: String,
  body_templates: Dict(String, String),
) -> Result(
  artifact_publication_planner.DryRunPublicationManifest,
  #(String, String),
) {
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
  })
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

fn require_workflow_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(projection.WorkflowRunStatus, #(String, String)) {
  projection.workflow_run(projected, run_id)
  |> result.map_error(fn(_) {
    #("publication_run_not_found", "publication run not found: " <> run_id)
  })
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

fn workflow_status_workflow_id(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(workflow_id: workflow_id, ..)
    | projection.WorkflowRunFinished(workflow_id: workflow_id, ..)
    | projection.WorkflowRunInterrupted(workflow_id: workflow_id, ..)
    | projection.WorkflowRunSuperseded(workflow_id: workflow_id, ..) ->
      workflow_id
  }
}

fn planner_error_message(
  error: artifact_publication_planner.PlannerError,
) -> String {
  let artifact_publication_planner.PlannerError(message: message, ..) = error
  message
}
