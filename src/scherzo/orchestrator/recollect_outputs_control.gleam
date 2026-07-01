import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/orchestrator/artifact_publication_retry_control
import scherzo/orchestrator/core
import scherzo/orchestrator/startup_recovery
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_checkpoint
import scherzo/workflow_output_recollection

pub type QueueDecision {
  QueueDecision(
    operation_id: String,
    issue_id: String,
    body: Option(record.RecordBody),
  )
}

pub type QueueError {
  QueueRunNotFound
  QueueIssueDrift
}

pub type ExecutionOutcome {
  ExecutionCompleted(List(record.RecordBody))
  ExecutionFailed(reason: String, message: Option(String))
}

pub type RunningConflict {
  RunningConflict(reason: String, message: Option(String))
}

pub fn queue_decision(
  projected: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  now_ms: Int,
) -> Result(QueueDecision, QueueError) {
  use _run <- result.try(
    projection.workflow_run(projected, run_id)
    |> result.replace_error(QueueRunNotFound),
  )
  use provenance <- result.try(
    projection.workflow_run_provenance(projected, run_id)
    |> result.replace_error(QueueIssueDrift),
  )
  case active_operation_for_run(projected, run_id) {
    Ok(existing) ->
      Ok(QueueDecision(
        operation_id: existing.operation_id,
        issue_id: provenance.issue_id,
        body: None,
      ))
    Error(Nil) -> {
      let operation_id = make_operation_id(run_id, now_ms)
      Ok(QueueDecision(
        operation_id: operation_id,
        issue_id: provenance.issue_id,
        body: Some(record.ControlOperationQueued(
          operation_id: operation_id,
          operation_kind: "recollect_outputs",
          command_name: command.command_name(operator_command),
          target: option.unwrap(command.command_target(operator_command), ""),
          run_id: Some(run_id),
          issue_id: Some(provenance.issue_id),
          issue_identifier: Some(provenance.issue_identifier),
          requested_step_id: None,
          publication_id: None,
        )),
      ))
    }
  }
}

pub fn control_operation_running_conflict(
  projected: projection.Projection,
  operation: projection.ControlOperationStatus,
) -> Result(RunningConflict, Nil) {
  case
    artifact_publication_retry_control.running_conflict(projected, operation)
  {
    Ok(existing) ->
      Ok(RunningConflict(
        reason: "artifact_publication_retry_already_running",
        message: Some(
          "artifact publication retry already queued/running as "
          <> existing.operation_id,
        ),
      ))
    Error(Nil) -> recollect_running_conflict(projected, operation)
  }
}

fn recollect_running_conflict(
  projected: projection.Projection,
  operation: projection.ControlOperationStatus,
) -> Result(RunningConflict, Nil) {
  case operation.operation_kind, operation.run_id {
    "recollect_outputs", Some(run_id) ->
      projected.control_operations
      |> dict.values
      |> list.find(fn(existing) {
        existing.operation_kind == "recollect_outputs"
        && existing.operation_id != operation.operation_id
        && existing.status == "running"
        && existing.run_id == Some(run_id)
      })
      |> result.map(fn(existing) {
        RunningConflict(
          reason: "recollect_outputs_already_running",
          message: Some(
            "recollect-outputs already queued/running as "
            <> existing.operation_id,
          ),
        )
      })
    _, _ -> Error(Nil)
  }
}

fn active_operation_for_run(
  projected: projection.Projection,
  run_id: String,
) -> Result(projection.ControlOperationStatus, Nil) {
  projected.control_operations
  |> dict.values
  |> list.find(fn(existing) {
    existing.operation_kind == "recollect_outputs"
    && existing.run_id == Some(run_id)
    && is_incomplete(existing.status)
  })
}

fn is_incomplete(status: String) -> Bool {
  status == "queued" || status == "running"
}

pub fn parked_preflight_for_run(
  runtime: orchestrator_state.RuntimeState,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue_id: String,
) -> Result(Nil, command.CommandResult) {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(parked) ->
      Error(command.rejected(
        operator_command,
        "issue_parked",
        Some(
          "issue is parked for "
          <> orchestrator_reason.park_to_string(parked.reason)
          <> "; unpark before recollect-outputs for run "
          <> run_id,
        ),
      ))
    Error(Nil) -> Ok(Nil)
  }
}

pub fn queue_error_result(
  operator_command: command.OperatorCommand,
  error: QueueError,
) -> command.CommandResult {
  case error {
    QueueRunNotFound ->
      command.not_found(operator_command, Some("run not found"))
    QueueIssueDrift ->
      command.rejected(
        operator_command,
        "issue_drift",
        Some("task identity drifted"),
      )
  }
}

pub fn validate_issue_state(
  effective: config_types.EffectiveConfig,
  operator_command: command.OperatorCommand,
  run_id: String,
  issue: tracker_issue.Issue,
) -> Result(tracker_issue.Issue, command.CommandResult) {
  case
    core.is_active(effective, issue.state)
    || core.is_terminal(effective, issue.state)
  {
    True -> Ok(issue)
    False ->
      command.rejected(
        operator_command,
        "issue_state_drift:non_active_state",
        Some(
          "run "
          <> run_id
          <> " for issue "
          <> issue.identifier
          <> " is currently in non-active state "
          <> issue_state.to_string(issue.state)
          <> "; move the issue to a configured active or terminal state before recollect-outputs",
        ),
      )
      |> Error
  }
}

pub fn execute_operation(
  root: String,
  operation: projection.ControlOperationStatus,
  bundle: runtime_bundle.RuntimeBundle,
  now_ms: fn() -> Int,
  issue: tracker_issue.Issue,
  projected: projection.Projection,
) -> ExecutionOutcome {
  use run_id <- require_run_id(operation)
  let observation = startup_recovery.current_workflow_observation(bundle, issue)
  case workflow_checkpoint.next_output_recollection_index(root, run_id) {
    Error(error) ->
      ExecutionFailed(
        "ledger_read_failed",
        Some(workflow_checkpoint.describe_error(error)),
      )
    Ok(recollection_index) ->
      execute_with_recollection_index(
        root,
        operation,
        now_ms,
        projected,
        run_id,
        observation,
        recollection_index,
      )
  }
}

fn execute_with_recollection_index(
  root: String,
  operation: projection.ControlOperationStatus,
  now_ms: fn() -> Int,
  projected: projection.Projection,
  run_id: String,
  observation: recovery.CurrentWorkflowObservation,
  recollection_index: Int,
) -> ExecutionOutcome {
  let checkpoint = workflow_checkpoint.ledger_writer(root, now_ms)
  let recollection_checkpoint =
    recollection_checkpoint_without_output_append(
      root,
      now_ms,
      recollection_index,
    )
  case
    workflow_output_recollection.execute(
      projected,
      run_id,
      observation,
      checkpoint,
      recollection_checkpoint,
      artifact_store.new(root),
    )
  {
    Error(error) ->
      ExecutionFailed(
        workflow_output_recollection.describe_error(error),
        workflow_output_recollection.error_message(error),
      )
    Ok(workflow_output_recollection.AlreadyValid(recorded)) ->
      ExecutionCompleted([
        record.ControlOperationCompleted(
          operation.operation_id,
          Some(already_valid_message(run_id, recorded.ref)),
        ),
      ])
    Ok(workflow_output_recollection.Recollected(recorded, manifest)) ->
      ExecutionCompleted([
        record.WorkflowRunOutputsRecorded(
          run_id,
          manifest.workflow_id,
          manifest.workflow_fingerprint,
          recorded.ref,
          recorded.sha256,
          recorded.bytes,
        ),
        record.ControlOperationCompleted(
          operation.operation_id,
          Some(recollected_message(run_id, recorded.ref)),
        ),
      ])
  }
}

fn require_run_id(
  operation: projection.ControlOperationStatus,
  continue: fn(String) -> ExecutionOutcome,
) -> ExecutionOutcome {
  case option.unwrap(operation.run_id, "") {
    "" ->
      ExecutionFailed(
        "operation_missing_run_id",
        Some("recollect-outputs operation is missing run metadata"),
      )
    run_id -> continue(run_id)
  }
}

fn recollection_checkpoint_without_output_append(
  root: String,
  now_ms: fn() -> Int,
  recollection_index: Int,
) -> workflow_checkpoint.Writer {
  let base =
    workflow_checkpoint.recollection_ledger_writer(
      root,
      now_ms,
      recollection_index,
    )
  workflow_checkpoint.Writer(..base, workflow_outputs_recorded: fn(_) {
    Ok(Nil)
  })
}

fn make_operation_id(run_id: String, now_ms: Int) -> String {
  "recollect-outputs:" <> run_id <> ":" <> int.to_string(now_ms)
}

fn already_valid_message(run_id: String, artifact_ref: String) -> String {
  "workflow outputs already valid for " <> run_id <> ": " <> artifact_ref
}

fn recollected_message(run_id: String, artifact_ref: String) -> String {
  "recollected workflow outputs for " <> run_id <> ": " <> artifact_ref
}
