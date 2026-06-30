import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_repository/command_runner
import scherzo/control/command
import scherzo/ctl/artifact_publication_retry
import scherzo/runtime_bundle
import scherzo/state/projection
import scherzo/state/record

pub type QueueDecision {
  ExistingOperation(operation_id: String)
  NewOperation(operation_id: String, body: record.RecordBody)
}

pub type ExecutionOutcome {
  ExecutionCompleted(List(record.RecordBody))
  ExecutionFailed(reason: String, message: Option(String))
}

pub fn queue_decision(
  projected: projection.Projection,
  operator_command: command.OperatorCommand,
  run_id: String,
  publication_id: Option(String),
  now_ms: Int,
) -> Result(QueueDecision, #(String, String)) {
  use _attempts <- result.try(
    artifact_publication_retry.inspect_retryable_attempts(
      projected,
      run_id,
      publication_id,
    ),
  )
  case active_operation_for_target(projected, run_id, publication_id) {
    Ok(existing) ->
      case same_target(existing.publication_id, publication_id) {
        True -> Ok(ExistingOperation(existing.operation_id))
        False ->
          Error(#(
            "artifact_publication_retry_already_running",
            "artifact publication retry already queued/running as "
              <> existing.operation_id,
          ))
      }
    Error(Nil) -> {
      let operation_id = make_operation_id(run_id, publication_id, now_ms)
      let #(issue_id, issue_identifier) = issue_metadata(projected, run_id)
      Ok(NewOperation(
        operation_id,
        record.ControlOperationQueued(
          operation_id: operation_id,
          operation_kind: "artifact_publication_retry",
          command_name: command.command_name(operator_command),
          target: option.unwrap(command.command_target(operator_command), ""),
          run_id: Some(run_id),
          issue_id: issue_id,
          issue_identifier: issue_identifier,
          requested_step_id: None,
          publication_id: publication_id,
        ),
      ))
    }
  }
}

pub fn running_conflict(
  projected: projection.Projection,
  operation: projection.ControlOperationStatus,
) -> Result(projection.ControlOperationStatus, Nil) {
  case operation.operation_kind, operation.run_id {
    "artifact_publication_retry", Some(run_id) ->
      projected.control_operations
      |> dict.values
      |> list.find(fn(existing) {
        existing.operation_kind == "artifact_publication_retry"
        && existing.operation_id != operation.operation_id
        && existing.status == "running"
        && existing.run_id == Some(run_id)
        && targets_overlap(existing.publication_id, operation.publication_id)
      })
    _, _ -> Error(Nil)
  }
}

pub fn retry_all_attempts(
  root: String,
  run_id: String,
  bundle: runtime_bundle.RuntimeBundle,
  runner: command_runner.Runner,
) -> Result(List(projection.PublicationAttempt), #(String, String)) {
  artifact_publication_retry.retry_attempts_with_bundle_runner(
    root,
    run_id,
    None,
    bundle,
    runner,
  )
}

pub fn execute_operation(
  root: String,
  operation: projection.ControlOperationStatus,
  bundle: runtime_bundle.RuntimeBundle,
  runner: command_runner.Runner,
) -> ExecutionOutcome {
  case operation.run_id {
    None ->
      ExecutionFailed(
        "operation_missing_run_id",
        Some("artifact publication retry operation is missing run metadata"),
      )
    Some(run_id) ->
      case
        artifact_publication_retry.retry_attempts_with_bundle_runner(
          root,
          run_id,
          operation.publication_id,
          bundle,
          runner,
        )
      {
        Ok(attempts) ->
          ExecutionCompleted([
            record.ControlOperationCompleted(
              operation.operation_id,
              Some(applied_message(attempts)),
            ),
          ])
        Error(#(code, message)) -> ExecutionFailed(code, Some(message))
      }
  }
}

pub fn error_result(
  operator_command: command.OperatorCommand,
  error: #(String, String),
) -> command.CommandResult {
  let #(code, message) = error
  case code {
    "publication_run_not_found" | "publication_not_found" ->
      command.not_found(operator_command, Some(message))
    _ -> command.rejected(operator_command, code, Some(message))
  }
}

fn active_operation_for_target(
  projected: projection.Projection,
  run_id: String,
  publication_id: Option(String),
) -> Result(projection.ControlOperationStatus, Nil) {
  projected.control_operations
  |> dict.values
  |> list.find(fn(existing) {
    existing.operation_kind == "artifact_publication_retry"
    && existing.run_id == Some(run_id)
    && is_incomplete(existing.status)
    && targets_overlap(existing.publication_id, publication_id)
  })
}

fn is_incomplete(status: String) -> Bool {
  status == "queued" || status == "running"
}

fn same_target(existing: Option(String), requested: Option(String)) -> Bool {
  case existing, requested {
    None, None -> True
    Some(existing), Some(requested) -> existing == requested
    _, _ -> False
  }
}

fn targets_overlap(
  existing: Option(String),
  requested: Option(String),
) -> Bool {
  case existing, requested {
    None, _ -> True
    _, None -> True
    Some(existing), Some(requested) -> existing == requested
  }
}

fn issue_metadata(
  projected: projection.Projection,
  run_id: String,
) -> #(Option(String), Option(String)) {
  case projection.workflow_run_provenance(projected, run_id) {
    Ok(provenance) -> #(
      Some(provenance.issue_id),
      Some(provenance.issue_identifier),
    )
    Error(Nil) -> #(None, None)
  }
}

fn make_operation_id(
  run_id: String,
  publication_id: Option(String),
  now_ms: Int,
) -> String {
  "artifact-publication-retry:"
  <> run_id
  <> ":"
  <> option.unwrap(publication_id, "all")
  <> ":"
  <> int.to_string(now_ms)
}

fn applied_message(attempts: List(projection.PublicationAttempt)) -> String {
  case attempts {
    [attempt] ->
      "publication retry recorded "
      <> attempt.publication_id
      <> " as "
      <> attempt.status
    _ ->
      "publication retry recorded "
      <> int.to_string(list.length(attempts))
      <> " attempt(s)"
  }
}
