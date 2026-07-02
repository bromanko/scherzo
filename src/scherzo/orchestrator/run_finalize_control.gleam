import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/control/command
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/workflow_checkpoint
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

pub type FinalizePlan {
  FinalizePlan(
    run_id: String,
    workflow_id: String,
    issue_id: String,
    issue_identifier: String,
    output_action: String,
    publication_ids: List(String),
    already_finalized: Bool,
    task_ref: Option(record.TaskRefFields),
  )
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
        already_finalized: True,
        task_ref: task_ref,
      ))
    projection.WorkflowRunInterrupted(..) ->
      case publication_ids {
        [] ->
          Error(#(
            "publication_not_found",
            "run finalize requires at least one retained publication target",
          ))
        _ ->
          Ok(FinalizePlan(
            run_id: run_id,
            workflow_id: workflow_id,
            issue_id: provenance.issue_id,
            issue_identifier: provenance.issue_identifier,
            output_action: output_action(projected, run_id),
            publication_ids: publication_ids,
            already_finalized: False,
            task_ref: task_ref,
          ))
      }
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

pub fn queue_decision(
  projected: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  now_ms: Int,
) -> Result(QueueDecision, #(String, String)) {
  use plan <- result.try(dry_run(projected, run_id))
  case plan.already_finalized {
    True -> Ok(AlreadyFinalized(already_finalized_message(run_id)))
    False ->
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
          let operation_id = make_operation_id(run_id, now_ms)
          Ok(NewOperation(
            operation_id,
            record.ControlOperationQueued(
              operation_id: operation_id,
              operation_kind: "run_finalize",
              command_name: command.command_name(operator_command),
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

pub fn dry_run_message(plan: FinalizePlan) -> String {
  let output_line = case plan.output_action {
    "adopt_outputs" -> "adopt existing outputs"
    _ -> "recollect outputs"
  }
  let publication_line = case plan.publication_ids {
    [] -> "no publication targets"
    ids ->
      "retry publication for "
      <> int.to_string(list.length(ids))
      <> " target(s)"
  }
  let finalized = case plan.already_finalized {
    True -> "run is already finalized"
    False -> "run is not finalized yet"
  }
  "dry run: "
  <> finalized
  <> "; would validate retained evidence, "
  <> output_line
  <> ", "
  <> publication_line
  <> ", update tracker, and append workflow_run_finished without starting a worker"
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
  case operation_kind {
    "artifact_publication_retry" ->
      "artifact publication retry already queued/running as " <> operation_id
    "recollect_outputs" ->
      "recollect-outputs already queued/running as " <> operation_id
    _ -> "run finalize already queued/running as " <> operation_id
  }
}

fn make_operation_id(run_id: String, now_ms: Int) -> String {
  "run-finalize:" <> run_id <> ":" <> int.to_string(now_ms)
}
