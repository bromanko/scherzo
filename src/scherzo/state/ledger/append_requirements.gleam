import gleam/option.{type Option, Some}
import scherzo/state/record

pub type AppendRecordWorkflowRequirement {
  AddWorkflowRun(run_id: String)
  RequireKnownWorkflowRun(reason: String, run_id: String)
  RejectPrunedWorkflowRunOnly(run_id: String)
  NoWorkflowRunRequirement
}

pub fn workflow_requirement(
  body: record.RecordBody,
) -> AppendRecordWorkflowRequirement {
  case body {
    record.WorkflowRunStarted(run_id, _, _, _, _, _, _, _)
    | record.WorkflowRunStartedWithTask(run_id, _, _, _, _, _, _, _, _) ->
      AddWorkflowRun(run_id)
    record.WorkflowRunFinished(run_id, _, _, _, _, _)
    | record.WorkflowRunFinishedWithTask(run_id, _, _, _, _, _, _)
    | record.WorkflowRunInterrupted(run_id, _, _, _)
    | record.WorkflowRunSuperseded(run_id, _, _, _, _) ->
      RequireKnownWorkflowRun("unknown_workflow_run", run_id)
    record.WorkflowRunProvenanceRepaired(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.WorkflowRunInputsRecorded(run_id, _, _, _, _, _)
    | record.WorkflowInterfaceSnapshotRecorded(run_id, _, _, _, _, _)
    | record.WorkflowRunOutputsRecorded(run_id, _, _, _, _, _)
    | record.PublicationAttemptRecorded(
        run_id,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
      )
    | record.WorkflowRepairRequested(run_id, _, _, _, _, _, _, _, _, _) ->
      RejectPrunedWorkflowRunOnly(run_id)
    record.StepAttemptPrepared(run_id, _, _, _, _, _, _, _, _)
    | record.StepAttemptStarted(run_id, _, _, _, _, _, _)
    | record.StepAttemptContinuationStarted(run_id, _, _, _, _)
    | record.StepAttemptPiSessionRecorded(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.StepAttemptPiSessionRecordedWithTask(
        run_id,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
      )
    | record.StepAttemptFinished(run_id, _, _, _, _, _, _, _, _, _, _)
    | record.StepAttemptInterrupted(run_id, _, _, _, _)
    | record.StepAttemptSuperseded(run_id, _, _, _, _, _) ->
      RequireKnownWorkflowRun(
        "orphan_step_attempt_without_workflow_run",
        run_id,
      )
    record.WorkflowStepRecoveryStarted(run_id, _, _, _, _, _, _, _)
    | record.WorkflowStepRecoveryFinished(run_id, _, _, _, _, _, _, _, _, _)
    | record.ScheduledJobDue(_, _, _, run_id, _)
    | record.ScheduledJobSkipped(_, _, _, run_id, _, _)
    | record.ScheduledRunPending(_, _, _, run_id, _, _)
    | record.ScheduledRunPendingBlocked(_, _, _, run_id, _, _)
    | record.ScheduledRunPendingCancelled(_, _, _, run_id, _, _)
    | record.ScheduledRunStarted(_, _, _, _, run_id, _, _, _)
    | record.ScheduledRunSucceeded(_, _, _, run_id, _, _, _, _)
    | record.ScheduledRunFailed(_, _, _, run_id, _, _, _, _, _)
    | record.ScheduledRunRetryScheduled(_, _, _, run_id, _, _, _, _)
    | record.ScheduledRunRetryCancelled(_, run_id, _, _)
    | record.ScheduledFailureReported(_, _, _, run_id, _, _, _, _)
    | record.ScheduledFailureReportFailed(_, _, _, run_id, _, _, _, _, _, _) ->
      RejectPrunedWorkflowRunOnly(run_id)
    record.ControlOperationQueued(_, _, _, _, run_id, _, _, _, _)
    | record.IssueCounterUpdated(_, _, _, _, _, run_id) ->
      optional_pruned_workflow_run_rejection(run_id)
    _ -> NoWorkflowRunRequirement
  }
}

fn optional_pruned_workflow_run_rejection(
  run_id: Option(String),
) -> AppendRecordWorkflowRequirement {
  case run_id {
    Some(run_id) -> RejectPrunedWorkflowRunOnly(run_id)
    _ -> NoWorkflowRunRequirement
  }
}
