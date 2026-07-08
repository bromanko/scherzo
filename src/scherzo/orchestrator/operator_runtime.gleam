import gleam/erlang/process
import gleam/option.{type Option, None}
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/transition_types
import scherzo/tracker/issue as tracker_issue

pub opaque type Lookup {
  Lookup(
    issue_for_ref: fn(command.IssueRef) ->
      Result(tracker_issue.Issue, command.CommandStatus),
    parked_issue_id_for_ref: fn(command.IssueRef) ->
      Result(String, command.CommandStatus),
  )
}

pub fn lookup(
  issue_for_ref issue_for_ref: fn(command.IssueRef) ->
    Result(tracker_issue.Issue, command.CommandStatus),
  parked_issue_id_for_ref parked_issue_id_for_ref: fn(command.IssueRef) ->
    Result(String, command.CommandStatus),
) -> Lookup {
  Lookup(
    issue_for_ref: issue_for_ref,
    parked_issue_id_for_ref: parked_issue_id_for_ref,
  )
}

pub opaque type ShellHandlers(state) {
  ShellHandlers(
    reload_workflow_for_operator: fn(state, command.OperatorCommand) ->
      #(state, command.CommandResult, List(transition_types.Message)),
    retry_workflow_step_for_operator: fn(
      state,
      command.OperatorCommand,
      command.RetryWorkflowStepTarget,
      Option(String),
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
    recollect_workflow_outputs_for_operator: fn(
      state,
      command.OperatorCommand,
      String,
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
    run_finalize_for_operator: fn(
      state,
      command.OperatorCommand,
      String,
      Bool,
      command.RunFinalizeOutputs,
      Bool,
      Bool,
      Bool,
      String,
      Bool,
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
    retry_artifact_publication_for_operator: fn(
      state,
      command.OperatorCommand,
      String,
      Option(String),
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
    schedule_run_now_for_operator: fn(state, command.OperatorCommand, String) ->
      #(state, command.CommandResult, List(transition_types.Message)),
    reenable_schedule_for_operator: fn(state, command.OperatorCommand, String) ->
      #(state, command.CommandResult, List(transition_types.Message)),
    abort_session_for_operator_sync: fn(
      state,
      command.OperatorCommand,
      String,
      Int,
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
    route_worker_command_sync: fn(
      state,
      command.OperatorCommand,
      String,
      Int,
      fn(
        process.Subject(worker_command.Command),
        process.Subject(worker_command.Reply),
      ) -> Nil,
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
    cleanup_orphan_steps_for_operator: fn(
      state,
      command.OperatorCommand,
      String,
      Bool,
    ) -> #(state, command.CommandResult, List(transition_types.Message)),
  )
}

pub fn command_routes(
  reload_workflow_for_operator reload_workflow_for_operator: fn(
    state,
    command.OperatorCommand,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  retry_workflow_step_for_operator retry_workflow_step_for_operator: fn(
    state,
    command.OperatorCommand,
    command.RetryWorkflowStepTarget,
    Option(String),
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  recollect_workflow_outputs_for_operator recollect_workflow_outputs_for_operator: fn(
    state,
    command.OperatorCommand,
    String,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  run_finalize_for_operator run_finalize_for_operator: fn(
    state,
    command.OperatorCommand,
    String,
    Bool,
    command.RunFinalizeOutputs,
    Bool,
    Bool,
    Bool,
    String,
    Bool,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  retry_artifact_publication_for_operator retry_artifact_publication_for_operator: fn(
    state,
    command.OperatorCommand,
    String,
    Option(String),
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  schedule_run_now_for_operator schedule_run_now_for_operator: fn(
    state,
    command.OperatorCommand,
    String,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  reenable_schedule_for_operator reenable_schedule_for_operator: fn(
    state,
    command.OperatorCommand,
    String,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  abort_session_for_operator_sync abort_session_for_operator_sync: fn(
    state,
    command.OperatorCommand,
    String,
    Int,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  route_worker_command_sync route_worker_command_sync: fn(
    state,
    command.OperatorCommand,
    String,
    Int,
    fn(
      process.Subject(worker_command.Command),
      process.Subject(worker_command.Reply),
    ) -> Nil,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
  cleanup_orphan_steps_for_operator cleanup_orphan_steps_for_operator: fn(
    state,
    command.OperatorCommand,
    String,
    Bool,
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
) -> ShellHandlers(state) {
  ShellHandlers(
    reload_workflow_for_operator: reload_workflow_for_operator,
    retry_workflow_step_for_operator: retry_workflow_step_for_operator,
    recollect_workflow_outputs_for_operator: recollect_workflow_outputs_for_operator,
    run_finalize_for_operator: run_finalize_for_operator,
    retry_artifact_publication_for_operator: retry_artifact_publication_for_operator,
    schedule_run_now_for_operator: schedule_run_now_for_operator,
    reenable_schedule_for_operator: reenable_schedule_for_operator,
    abort_session_for_operator_sync: abort_session_for_operator_sync,
    route_worker_command_sync: route_worker_command_sync,
    cleanup_orphan_steps_for_operator: cleanup_orphan_steps_for_operator,
  )
}

pub fn operator_issue_resolution(
  lookup: Lookup,
  operator_command: command.OperatorCommand,
) -> transition_types.OperatorIssueResolution {
  case operator_command {
    command.RetryIssue(issue_ref)
    | command.RetryIssueStartFresh(issue_ref, _)
    | command.ParkIssue(issue_ref, _) ->
      case lookup.issue_for_ref(issue_ref) {
        Ok(issue) -> transition_types.OperatorIssueResolved(issue)
        Error(command.NotFound) -> transition_types.OperatorIssueNotFound
        Error(command.Rejected(reason)) ->
          transition_types.OperatorIssueRejected(reason)
        Error(command.NotAllowed(reason)) ->
          transition_types.OperatorIssueNotAllowed(reason)
        Error(command.Applied) | Error(command.Queued) ->
          transition_types.OperatorIssueResolutionFailed
      }
    command.PauseDispatch
    | command.ResumeDispatch
    | command.ReloadWorkflow
    | command.RetryWorkflowStep(_, _)
    | command.RetryWorkflowStepDryRun(_, _)
    | command.RetryWorkflowStepExact(_, _)
    | command.RecollectWorkflowOutputs(_)
    | command.RunFinalize(..)
    | command.RetryArtifactPublication(_, _)
    | command.UnparkIssue(_)
    | command.AbortSession(_)
    | command.StopAfterCurrentTurn(_)
    | command.CleanupOrphanSteps(_, _)
    | command.PromptSession(_, _)
    | command.RespondUi(_, _, _)
    | command.RunScheduleNow(_)
    | command.ReenableSchedule(_)
    | command.WorkItemAction(_) -> transition_types.OperatorIssueNotResolved
  }
}

pub fn parked_issue_resolution(
  lookup: Lookup,
  operator_command: command.OperatorCommand,
) -> transition_types.ParkedIssueResolution {
  case operator_command {
    command.UnparkIssue(issue_ref) ->
      case lookup.parked_issue_id_for_ref(issue_ref) {
        Ok(issue_id) -> transition_types.ParkedIssueResolved(issue_id)
        Error(command.NotFound) -> transition_types.ParkedIssueNotFound
        Error(command.Rejected(reason)) ->
          transition_types.ParkedIssueRejected(reason)
        Error(command.NotAllowed(reason)) ->
          transition_types.ParkedIssueNotAllowed(reason)
        Error(command.Applied) | Error(command.Queued) ->
          transition_types.ParkedIssueResolutionFailed
      }
    command.PauseDispatch
    | command.ResumeDispatch
    | command.ReloadWorkflow
    | command.RetryIssue(_)
    | command.RetryIssueStartFresh(_, _)
    | command.RetryWorkflowStep(_, _)
    | command.RetryWorkflowStepDryRun(_, _)
    | command.RetryWorkflowStepExact(_, _)
    | command.RecollectWorkflowOutputs(_)
    | command.RunFinalize(..)
    | command.RetryArtifactPublication(_, _)
    | command.ParkIssue(_, _)
    | command.AbortSession(_)
    | command.StopAfterCurrentTurn(_)
    | command.CleanupOrphanSteps(_, _)
    | command.PromptSession(_, _)
    | command.RespondUi(_, _, _)
    | command.RunScheduleNow(_)
    | command.ReenableSchedule(_)
    | command.WorkItemAction(_) -> transition_types.ParkedIssueNotResolved
  }
}

pub fn apply_shell_operator_command(
  state: state,
  request: transition_effects.OperatorCommandRequest,
  handlers: ShellHandlers(state),
) -> #(state, command.CommandResult, List(transition_types.Message)) {
  let operator_command = request.operator_command
  case operator_command {
    command.ReloadWorkflow ->
      handlers.reload_workflow_for_operator(state, operator_command)
    command.RetryWorkflowStep(target, step_id)
    | command.RetryWorkflowStepDryRun(target, step_id)
    | command.RetryWorkflowStepExact(target, step_id) ->
      handlers.retry_workflow_step_for_operator(
        state,
        operator_command,
        target,
        step_id,
      )
    command.RecollectWorkflowOutputs(run_id) ->
      handlers.recollect_workflow_outputs_for_operator(
        state,
        operator_command,
        run_id,
      )
    command.RunFinalize(
      run_id: run_id,
      validate: validate,
      outputs: outputs,
      publish: publish,
      update_tracker: update_tracker,
      dry_run: dry_run,
      reason: reason,
      allow_unpublished: allow_unpublished,
    ) ->
      handlers.run_finalize_for_operator(
        state,
        operator_command,
        run_id,
        validate,
        outputs,
        publish,
        update_tracker,
        dry_run,
        reason,
        allow_unpublished,
      )
    command.RetryArtifactPublication(run_id, publication_id) ->
      handlers.retry_artifact_publication_for_operator(
        state,
        operator_command,
        run_id,
        publication_id,
      )
    command.RunScheduleNow(job_id) ->
      handlers.schedule_run_now_for_operator(state, operator_command, job_id)
    command.ReenableSchedule(job_id) ->
      handlers.reenable_schedule_for_operator(state, operator_command, job_id)
    command.AbortSession(session_id) ->
      handlers.abort_session_for_operator_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
      )
    command.StopAfterCurrentTurn(session_id) ->
      handlers.route_worker_command_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
        fn(subject, reply) {
          process.send(subject, worker_command.StopAfterCurrentTurn(reply))
        },
      )
    command.CleanupOrphanSteps(run_id, dry_run) ->
      handlers.cleanup_orphan_steps_for_operator(
        state,
        operator_command,
        run_id,
        dry_run,
      )
    command.PromptSession(session_id, message) ->
      handlers.route_worker_command_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
        fn(subject, reply) {
          process.send(subject, worker_command.QueuePrompt(message, reply))
        },
      )
    command.RespondUi(session_id, request_id, response) ->
      handlers.route_worker_command_sync(
        state,
        operator_command,
        session_id,
        request.timeout_ms,
        fn(subject, reply) {
          process.send(
            subject,
            worker_command.RespondToUi(request_id, response, reply),
          )
        },
      )
    command.PauseDispatch
    | command.ResumeDispatch
    | command.RetryIssue(_)
    | command.RetryIssueStartFresh(_, _)
    | command.ParkIssue(_, _)
    | command.UnparkIssue(_)
    | command.WorkItemAction(_) -> #(
      state,
      command.rejected(
        operator_command,
        "operator_command_already_handled",
        None,
      ),
      [],
    )
  }
}
