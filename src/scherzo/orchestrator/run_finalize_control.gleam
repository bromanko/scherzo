import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/artifact_publication_route_discovery
import scherzo/control/command
import scherzo/orchestrator/recollect_outputs_control
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_output_recollection

pub type QueueDecision {
  ExistingOperation(operation_id: String)
  ConflictingOperation(
    operation_id: String,
    operation_kind: String,
    message: String,
  )
  AlreadyFinalized(message: String)
  NewOperation(operation_id: String, body: record.RecordBody)
}

pub type QueueDecisionOutcome {
  QueueImmediate(command.CommandResult)
  QueueAppend(operation_id: String, queued_body: record.RecordBody)
}

pub type FinalizePlan {
  FinalizePlan(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    issue_identifier: String,
    output_action: String,
    publication_ids: List(String),
    publication_statuses: List(PublicationRouteStatus),
    already_finalized: Bool,
    task_ref: Option(record.TaskRefFields),
  )
}

pub type PublicationRouteStatus {
  PublicationRouteStatus(
    publication_id: String,
    required: Bool,
    status: String,
    latest_attempt_status: Option(String),
  )
}

pub type AlreadyFinalizedPublicationAction {
  AlreadyFinalizedCommandResult(command.CommandResult)
  QueueAlreadyFinalizedPublicationRetry
}

pub fn dry_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(FinalizePlan, #(String, String)) {
  use status <- result.try(
    projection.workflow_run(projected, run_id)
    |> result.map_error(fn(_) { #("run_not_found", "run not found") }),
  )
  use provenance <- result.try(
    projection.workflow_run_provenance(projected, run_id)
    |> result.map_error(fn(_) { #("issue_drift", "task identity drifted") }),
  )
  let workflow_id = workflow_id_from_status(status)
  let task_ref = case projection.workflow_task_ref(projected, run_id) {
    Ok(task_ref) -> Some(task_ref)
    Error(Nil) -> None
  }
  let publication_ids = projection.publication_ids_for_run(projected, run_id)
  case status {
    projection.WorkflowRunActive(..) ->
      Error(#("run_active", "run finalize requires a non-active run"))
    projection.WorkflowRunFinished(..) ->
      Ok(FinalizePlan(
        run_id: run_id,
        workflow_id: workflow_id,
        issue_id: provenance.issue_id,
        issue_identifier: provenance.issue_identifier,
        output_action: output_action(projected, run_id),
        publication_ids: publication_ids,
        publication_statuses: attempt_publication_statuses(
          projected,
          run_id,
          publication_ids,
        ),
        already_finalized: True,
        task_ref: task_ref,
      ))
    projection.WorkflowRunInterrupted(..) ->
      Ok(FinalizePlan(
        run_id: run_id,
        workflow_id: workflow_id,
        issue_id: provenance.issue_id,
        issue_identifier: provenance.issue_identifier,
        output_action: output_action(projected, run_id),
        publication_ids: publication_ids,
        publication_statuses: attempt_publication_statuses(
          projected,
          run_id,
          publication_ids,
        ),
        already_finalized: False,
        task_ref: task_ref,
      ))
    projection.WorkflowRunSuperseded(..) ->
      Error(#("run_superseded", "run finalize cannot finalize a superseded run"))
  }
}

pub fn validated_dry_run(
  projected: projection.Projection,
  run_id: String,
  current: recovery.CurrentWorkflowObservation,
  root: String,
) -> Result(FinalizePlan, #(String, String)) {
  use plan <- result.try(dry_run(projected, run_id))
  case plan.already_finalized {
    True -> Ok(plan)
    False -> {
      let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 0 })
      use output_action <- result.try(
        workflow_output_recollection.plan_output_action(
          projected,
          run_id,
          current,
          checkpoint,
          artifact_store.new(root),
        )
        |> result.map_error(fn(error) {
          let code = workflow_output_recollection.describe_error(error)
          #(
            code,
            option.unwrap(
              workflow_output_recollection.error_message(error),
              code,
            ),
          )
        }),
      )
      Ok(FinalizePlan(..plan, output_action: output_action))
    }
  }
}

pub fn already_finalized_publication_action(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  root: String,
  operator_command: command.OperatorCommand,
  publish: Bool,
  dry_run: Bool,
  plan: FinalizePlan,
) -> Result(AlreadyFinalizedPublicationAction, #(String, String)) {
  use plan <- result.try(plan_with_bundle_publication_statuses(
    projected,
    bundle,
    root,
    plan.run_id,
    plan,
  ))
  case dry_run, publish, unpublished_publications(plan.publication_statuses) {
    True, _, _ ->
      Ok(
        AlreadyFinalizedCommandResult(command.applied(
          operator_command,
          Some(dry_run_message(plan)),
        )),
      )
    False, True, [_, ..] -> Ok(QueueAlreadyFinalizedPublicationRetry)
    False, _, _ ->
      Ok(
        AlreadyFinalizedCommandResult(command.applied(
          operator_command,
          Some(already_finalized_message(plan.run_id)),
        )),
      )
  }
}

pub fn queue_decision(
  projected: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  now_ms: Int,
  allow_unpublished allow_unpublished: Bool,
) -> Result(QueueDecision, #(String, String)) {
  use plan <- result.try(dry_run(projected, run_id))
  case plan.already_finalized {
    True -> Ok(AlreadyFinalized(already_finalized_message(run_id)))
    False -> {
      case active_operation_for_run(projected, run_id) {
        Ok(existing) ->
          case existing.operation_kind {
            "run_finalize" -> Ok(ExistingOperation(existing.operation_id))
            _ ->
              Ok(ConflictingOperation(
                existing.operation_id,
                existing.operation_kind,
                conflict_message(existing.operation_kind, existing.operation_id),
              ))
          }
        Error(Nil) -> {
          use _ <- result.try(verify_publications_for_finalize(
            run_id,
            plan.publication_statuses,
            allow_unpublished,
          ))
          let operation_id = make_operation_id(run_id, now_ms)
          Ok(NewOperation(
            operation_id,
            record.ControlOperationQueued(
              operation_id: operation_id,
              operation_kind: "run_finalize",
              command_name: run_finalize_command_name(allow_unpublished),
              target: option.unwrap(
                command.command_target(operator_command),
                "",
              ),
              run_id: Some(run_id),
              issue_id: Some(plan.issue_id),
              issue_identifier: Some(plan.issue_identifier),
              requested_step_id: None,
              publication_id: None,
            ),
          ))
        }
      }
    }
  }
}

pub fn queue_decision_outcome(
  operator_command: command.OperatorCommand,
  decision: QueueDecision,
) -> QueueDecisionOutcome {
  case decision {
    AlreadyFinalized(message) ->
      QueueImmediate(command.applied(operator_command, Some(message)))
    ExistingOperation(operation_id) ->
      QueueImmediate(command.queued_operation(
        operator_command,
        operation_id,
        Some(
          "run finalize already queued/running; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl query operation-status "
          <> operation_id
          <> " --json",
        ),
      ))
    ConflictingOperation(_, _, message) ->
      QueueImmediate(command.rejected(
        operator_command,
        "control_operation_already_running",
        Some(message),
      ))
    NewOperation(operation_id, queued_body) ->
      QueueAppend(operation_id, queued_body)
  }
}

pub fn queue_append_failed_result(
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  command.rejected(
    operator_command,
    "ledger_append_failed",
    Some("failed to append run finalize operation"),
  )
}

pub fn accepted_result(
  operator_command: command.OperatorCommand,
  operation_id: String,
) -> command.CommandResult {
  command.queued_operation(
    operator_command,
    operation_id,
    Some("run finalize accepted; poll query operation-status for completion"),
  )
}

pub fn publication_statuses_for_bundle(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  root: String,
  run_id: String,
) -> Result(List(PublicationRouteStatus), #(String, String)) {
  use status <- result.try(
    projection.workflow_run(projected, run_id)
    |> result.map_error(fn(_) { #("run_not_found", "run not found") }),
  )
  let workflow_id = workflow_id_from_status(status)
  use #(_, workflow) <- result.try(
    runtime_bundle.workflow_by_id(bundle, workflow_id)
    |> result.map_error(fn(error) {
      let runtime_bundle.BundleError(code: code, message: message) = error
      #(code, message)
    }),
  )
  let routes = workflow_dag.publication_routes(workflow)
  let attempted_ids = projection.publication_ids_for_run(projected, run_id)
  use _ <- result.try(ensure_declared_route_statuses_safe(
    projected,
    bundle,
    root,
    run_id,
    workflow,
    attempted_ids,
  ))
  let route_statuses =
    routes
    |> list.map(fn(route) {
      case projection.latest_publication_for_run(projected, run_id, route.id) {
        Ok(attempt) ->
          PublicationRouteStatus(
            publication_id: route.id,
            required: route.required,
            status: publication_contract_status(attempt.status),
            latest_attempt_status: Some(attempt.status),
          )
        Error(Nil) ->
          PublicationRouteStatus(
            publication_id: route.id,
            required: route.required,
            status: "pending",
            latest_attempt_status: None,
          )
      }
    })
  let route_ids = list.map(routes, fn(route) { route.id })
  let historical_ids =
    attempted_ids
    |> list.filter(fn(publication_id) {
      !list.contains(route_ids, publication_id)
    })
  Ok(list.append(
    route_statuses,
    attempt_publication_statuses(projected, run_id, historical_ids),
  ))
}

fn ensure_declared_route_statuses_safe(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  root: String,
  run_id: String,
  workflow: workflow_dag.WorkflowDag,
  attempted_ids: List(String),
) -> Result(Nil, #(String, String)) {
  let declared_without_attempts =
    workflow_dag.publication_routes(workflow)
    |> list.filter(fn(route) { !list.contains(attempted_ids, route.id) })
  case declared_without_attempts {
    [] -> Ok(Nil)
    [_, ..] ->
      artifact_publication_route_discovery.ensure_current_routes_safe(
        projected,
        bundle,
        root,
        run_id,
        workflow,
      )
  }
}

pub fn plan_with_bundle_publication_statuses(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  root: String,
  run_id: String,
  plan: FinalizePlan,
) -> Result(FinalizePlan, #(String, String)) {
  use publication_statuses <- result.try(publication_statuses_for_bundle(
    projected,
    bundle,
    root,
    run_id,
  ))
  Ok(
    FinalizePlan(
      ..plan,
      publication_ids: list.map(publication_statuses, fn(status) {
        status.publication_id
      }),
      publication_statuses: publication_statuses,
    ),
  )
}

pub fn verify_publications_for_finalize(
  run_id: String,
  publication_statuses: List(PublicationRouteStatus),
  allow_unpublished allow_unpublished: Bool,
) -> Result(Nil, #(String, String)) {
  let unpublished = required_unpublished(publication_statuses)
  case unpublished, allow_unpublished {
    [], _ -> Ok(Nil)
    _, True -> Ok(Nil)
    _, False ->
      Error(#(
        "publication_pending",
        publication_pending_message(run_id, unpublished),
      ))
  }
}

pub fn required_unpublished(
  publication_statuses: List(PublicationRouteStatus),
) -> List(PublicationRouteStatus) {
  publication_statuses
  |> list.filter(fn(status) { status.required && status.status != "published" })
}

pub fn unpublished_publications(
  publication_statuses: List(PublicationRouteStatus),
) -> List(PublicationRouteStatus) {
  publication_statuses
  |> list.filter(fn(status) { status.status != "published" })
}

pub fn run_finalize_command_name(allow_unpublished: Bool) -> String {
  case allow_unpublished {
    True -> "run_finalize_allow_unpublished"
    False -> "run_finalize"
  }
}

pub fn operation_allows_unpublished(command_name: String) -> Bool {
  command_name == "run_finalize_allow_unpublished"
}

pub fn publication_pending_message(
  run_id: String,
  unpublished: List(PublicationRouteStatus),
) -> String {
  let route_summary = publication_status_summary(unpublished)
  let retry_target = case unpublished {
    [first, ..] -> " --publication " <> first.publication_id
    [] -> ""
  }
  "run finalize blocked: required publication route(s) are not published for run "
  <> run_id
  <> ": "
  <> route_summary
  <> ". Next: scherzoctl publication retry "
  <> run_id
  <> retry_target
  <> " --json, then rerun run finalize. Override only with --allow-unpublished --reason <text>."
}

pub fn dry_run_message(plan: FinalizePlan) -> String {
  let output_line = case plan.output_action {
    "adopt_outputs" -> "adopt existing materialized outputs"
    _ -> "recollect materialized outputs"
  }
  let publication_line =
    publication_summary_line(plan.publication_ids, plan.publication_statuses)
  let finalized = case plan.already_finalized {
    True -> "run is already finalized"
    False -> "run is not finalized yet"
  }
  "dry run: "
  <> finalized
  <> "; would validate retained evidence, "
  <> output_line
  <> ", verify "
  <> publication_line
  <> ", update tracker, and append workflow_run_finished without starting a worker"
}

fn attempt_publication_statuses(
  projected: projection.Projection,
  run_id: String,
  publication_ids: List(String),
) -> List(PublicationRouteStatus) {
  publication_ids
  |> list.map(fn(publication_id) {
    case
      projection.latest_publication_for_run(projected, run_id, publication_id)
    {
      Ok(attempt) ->
        PublicationRouteStatus(
          publication_id: publication_id,
          required: attempt.required,
          status: publication_contract_status(attempt.status),
          latest_attempt_status: Some(attempt.status),
        )
      Error(Nil) ->
        PublicationRouteStatus(
          publication_id: publication_id,
          required: True,
          status: "pending",
          latest_attempt_status: None,
        )
    }
  })
}

fn publication_contract_status(latest_status: String) -> String {
  case latest_status {
    "published" | "unchanged" -> "published"
    "failed" -> "failed"
    _ -> "pending"
  }
}

fn publication_summary_line(
  publication_ids: List(String),
  publication_statuses: List(PublicationRouteStatus),
) -> String {
  case publication_statuses, publication_ids {
    [], [] -> "no publication targets"
    [], ids ->
      "publication status for "
      <> int.to_string(list.length(ids))
      <> " target(s) unavailable"
    statuses, _ ->
      "publication status for "
      <> int.to_string(list.length(statuses))
      <> " target(s): "
      <> publication_status_summary(statuses)
  }
}

fn publication_status_summary(
  statuses: List(PublicationRouteStatus),
) -> String {
  statuses
  |> list.map(fn(status) {
    let required = case status.required {
      True -> "required"
      False -> "optional"
    }
    status.publication_id <> "=" <> status.status <> "(" <> required <> ")"
  })
  |> string.join(with: ", ")
}

pub fn already_finalized_message(run_id: String) -> String {
  "run " <> run_id <> " is already finalized"
}

pub fn output_action(
  projected: projection.Projection,
  run_id: String,
) -> String {
  case projection.workflow_output_manifest(projected, run_id) {
    Some(_) -> "adopt_outputs"
    None -> "recollect_outputs"
  }
}

pub fn active_operation_for_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(projection.ControlOperationStatus, Nil) {
  projected.control_operations
  |> dict.values
  |> list.find(fn(existing) {
    list.contains(
      ["run_finalize", "artifact_publication_retry", "recollect_outputs"],
      existing.operation_kind,
    )
    && existing.run_id == Some(run_id)
    && is_incomplete(existing.status)
  })
}

pub fn queue_bodies(
  plan: FinalizePlan,
  queued_body: record.RecordBody,
  reason: String,
  allow_unpublished: Bool,
) -> List(record.RecordBody) {
  [
    record.WorkflowRunDiagnostic(
      plan.run_id,
      plan.workflow_id,
      plan.issue_id,
      requested_reason(reason, allow_unpublished),
    ),
    queued_body,
  ]
}

pub fn completion_bodies(
  plan: FinalizePlan,
  operation_id: String,
  publication_statuses: List(PublicationRouteStatus),
  allow_unpublished: Bool,
) -> List(record.RecordBody) {
  [
    record.WorkflowRunDiagnostic(
      plan.run_id,
      plan.workflow_id,
      plan.issue_id,
      completion_diagnostic(publication_statuses, allow_unpublished),
    ),
    finish_record(plan),
    record.ControlOperationCompleted(
      operation_id,
      Some("run finalize completed without starting a worker"),
    ),
  ]
}

pub fn collect_output_bodies(
  outcome: recollect_outputs_control.ExecutionOutcome,
) -> Result(List(record.RecordBody), #(String, String)) {
  case outcome {
    recollect_outputs_control.ExecutionCompleted(bodies) ->
      Ok(
        list.filter(bodies, fn(body) {
          case body {
            record.ControlOperationCompleted(_, _) -> False
            _ -> True
          }
        }),
      )
    recollect_outputs_control.ExecutionFailed(reason, message) ->
      Error(#(reason, option.unwrap(message, reason)))
  }
}

pub fn finish_record(plan: FinalizePlan) -> record.RecordBody {
  case plan.task_ref {
    Some(task_ref) ->
      record.WorkflowRunFinishedWithTask(
        plan.run_id,
        plan.workflow_id,
        plan.issue_id,
        task_ref,
        "success",
        0,
        0,
      )
    None ->
      record.WorkflowRunFinished(
        plan.run_id,
        plan.workflow_id,
        plan.issue_id,
        "success",
        0,
        0,
      )
  }
}

fn requested_reason(reason: String, allow_unpublished: Bool) -> String {
  case allow_unpublished {
    True -> "run_finalize_requested_allow_unpublished:" <> reason
    False -> "run_finalize_requested:" <> reason
  }
}

fn completion_diagnostic(
  publication_statuses: List(PublicationRouteStatus),
  allow_unpublished: Bool,
) -> String {
  let unpublished = required_unpublished(publication_statuses)
  case allow_unpublished, unpublished {
    True, [_, ..] -> "run_finalize_completed_with_unpublished_publications"
    _, _ -> "run_finalize_completed"
  }
}

fn workflow_id_from_status(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(workflow_id: workflow_id, ..)
    | projection.WorkflowRunFinished(workflow_id: workflow_id, ..)
    | projection.WorkflowRunInterrupted(workflow_id: workflow_id, ..)
    | projection.WorkflowRunSuperseded(workflow_id: workflow_id, ..) ->
      workflow_id
  }
}

fn is_incomplete(status: String) -> Bool {
  status == "queued" || status == "running"
}

fn conflict_message(operation_kind: String, operation_id: String) -> String {
  let next =
    "; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl query operation-status "
    <> operation_id
    <> " --json"
  case operation_kind {
    "artifact_publication_retry" ->
      "artifact publication retry already queued/running as "
      <> operation_id
      <> next
    "recollect_outputs" ->
      "recollect-outputs already queued/running as " <> operation_id <> next
    _ -> "run finalize already queued/running as " <> operation_id <> next
  }
}

fn make_operation_id(run_id: String, now_ms: Int) -> String {
  "run-finalize:" <> run_id <> ":" <> int.to_string(now_ms)
}
