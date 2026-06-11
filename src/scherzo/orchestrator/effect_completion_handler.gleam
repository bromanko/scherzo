import scherzo/error
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/outbox_effects
import scherzo/state/recovery
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue

pub opaque type ResultHandlers(state) {
  ResultHandlers(
    candidate_fetch_finished: fn(
      state,
      Int,
      Result(List(tracker_issue.Issue), error.TrackerError),
    ) -> state,
    running_refresh_finished: fn(
      state,
      Int,
      Result(List(tracker_issue.Issue), error.TrackerError),
    ) -> state,
    retry_refresh_finished: fn(
      state,
      String,
      Int,
      Result(List(tracker_issue.Issue), error.TrackerError),
    ) -> state,
    dispatch_claim_validation_finished: fn(
      state,
      String,
      Int,
      Result(tracker_issue.Issue, effect_runner.DispatchClaimValidationError),
    ) -> state,
    handoff_claim_finished: fn(
      state,
      outbox_effects.Intent,
      String,
      String,
      Result(Nil, error.TrackerError),
    ) -> state,
    handoff_success_finished: fn(
      state,
      outbox_effects.Intent,
      String,
      Result(Nil, error.TrackerError),
    ) -> state,
    handoff_failure_finished: fn(
      state,
      outbox_effects.Intent,
      String,
      Result(Nil, error.TrackerError),
    ) -> state,
    handoff_park_finished: fn(
      state,
      outbox_effects.Intent,
      String,
      Result(Nil, error.TrackerError),
    ) -> state,
    invalid_workflow_report_finished: fn(
      state,
      outbox_effects.Intent,
      String,
      String,
      String,
      Result(effect_runner.InvalidWorkflowReportOutcome, error.TrackerError),
    ) -> state,
    outbox_replay_finished: fn(
      state,
      recovery.OutboxReplay,
      Result(Nil, error.TrackerError),
    ) -> state,
    scheduled_failure_report_finished: fn(
      state,
      Int,
      adapter.ScheduledFailurePublication,
      Result(adapter.ScheduledFailureReceipt, error.TrackerError),
    ) -> state,
    cleanup_finished: fn(state, String, Result(Nil, error.WorkspaceError)) ->
      state,
  )
}

pub fn result_handlers(
  candidate_fetch_finished candidate_fetch_finished: fn(
    state,
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  ) -> state,
  running_refresh_finished running_refresh_finished: fn(
    state,
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  ) -> state,
  retry_refresh_finished retry_refresh_finished: fn(
    state,
    String,
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  ) -> state,
  dispatch_claim_validation_finished dispatch_claim_validation_finished: fn(
    state,
    String,
    Int,
    Result(tracker_issue.Issue, effect_runner.DispatchClaimValidationError),
  ) -> state,
  handoff_claim_finished handoff_claim_finished: fn(
    state,
    outbox_effects.Intent,
    String,
    String,
    Result(Nil, error.TrackerError),
  ) -> state,
  handoff_success_finished handoff_success_finished: fn(
    state,
    outbox_effects.Intent,
    String,
    Result(Nil, error.TrackerError),
  ) -> state,
  handoff_failure_finished handoff_failure_finished: fn(
    state,
    outbox_effects.Intent,
    String,
    Result(Nil, error.TrackerError),
  ) -> state,
  handoff_park_finished handoff_park_finished: fn(
    state,
    outbox_effects.Intent,
    String,
    Result(Nil, error.TrackerError),
  ) -> state,
  invalid_workflow_report_finished invalid_workflow_report_finished: fn(
    state,
    outbox_effects.Intent,
    String,
    String,
    String,
    Result(effect_runner.InvalidWorkflowReportOutcome, error.TrackerError),
  ) -> state,
  outbox_replay_finished outbox_replay_finished: fn(
    state,
    recovery.OutboxReplay,
    Result(Nil, error.TrackerError),
  ) -> state,
  scheduled_failure_report_finished scheduled_failure_report_finished: fn(
    state,
    Int,
    adapter.ScheduledFailurePublication,
    Result(adapter.ScheduledFailureReceipt, error.TrackerError),
  ) -> state,
  cleanup_finished cleanup_finished: fn(
    state,
    String,
    Result(Nil, error.WorkspaceError),
  ) -> state,
) -> ResultHandlers(state) {
  ResultHandlers(
    candidate_fetch_finished: candidate_fetch_finished,
    running_refresh_finished: running_refresh_finished,
    retry_refresh_finished: retry_refresh_finished,
    dispatch_claim_validation_finished: dispatch_claim_validation_finished,
    handoff_claim_finished: handoff_claim_finished,
    handoff_success_finished: handoff_success_finished,
    handoff_failure_finished: handoff_failure_finished,
    handoff_park_finished: handoff_park_finished,
    invalid_workflow_report_finished: invalid_workflow_report_finished,
    outbox_replay_finished: outbox_replay_finished,
    scheduled_failure_report_finished: scheduled_failure_report_finished,
    cleanup_finished: cleanup_finished,
  )
}

pub opaque type Context(state) {
  Context(
    state: state,
    log_side_effect_crashed: fn(state, effect_runner.Effect, String) -> state,
    result_handlers: ResultHandlers(state),
  )
}

pub fn context(
  state state: state,
  log_side_effect_crashed log_side_effect_crashed: fn(
    state,
    effect_runner.Effect,
    String,
  ) -> state,
  result_handlers result_handlers: ResultHandlers(state),
) -> Context(state) {
  Context(
    state: state,
    log_side_effect_crashed: log_side_effect_crashed,
    result_handlers: result_handlers,
  )
}

pub fn handle_completed(
  context: Context(state),
  completion: effect_runner.Completion,
) -> state {
  case completion {
    effect_runner.Finished(_, result) -> handle_result(context, result)
    effect_runner.Crashed(_, effect, reason) -> {
      let state = context.log_side_effect_crashed(context.state, effect, reason)
      handle_result(
        Context(..context, state: state),
        crash_result_for_effect(effect, reason),
      )
    }
  }
}

pub fn crash_result_for_effect(
  effect: effect_runner.Effect,
  reason: String,
) -> effect_runner.EffectResult {
  case effect {
    effect_runner.FetchCandidates(generation, _) ->
      effect_runner.CandidateFetchFinished(
        generation,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.RefreshRunning(generation, _, _) ->
      effect_runner.RunningRefreshFinished(
        generation,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.RefreshRetry(issue_id, generation, _) ->
      effect_runner.RetryRefreshFinished(
        issue_id,
        generation,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ValidateDispatchClaim(issue_id, generation, _) ->
      effect_runner.DispatchClaimValidationFinished(
        issue_id: issue_id,
        generation: generation,
        result: Error(
          effect_runner.DispatchValidationTrackerError(error.LinearApiRequest(
            reason,
          )),
        ),
      )
    effect_runner.ClaimIssue(outbox, _, issue, _, run_id, _) ->
      effect_runner.HandoffClaimFinished(
        outbox,
        issue.id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportSuccess(outbox, _, issue_id, _, _, run_id, _, _) ->
      effect_runner.HandoffSuccessFinished(
        outbox,
        issue_id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportFailure(outbox, _, issue_id, _, _, run_id, _, _) ->
      effect_runner.HandoffFailureFinished(
        outbox,
        issue_id,
        run_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportPark(outbox, report, _) ->
      effect_runner.HandoffParkFinished(
        outbox,
        report.task.remote_id,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportInvalidWorkflow(
      outbox,
      issue,
      _,
      violation_fingerprint,
      reporting_policy_fingerprint,
      _,
      _,
      _,
    ) ->
      effect_runner.InvalidWorkflowReportFinished(
        outbox,
        issue.id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReplayOutbox(outbox_replay, _, _) ->
      effect_runner.OutboxReplayFinished(
        outbox_replay,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.ReportScheduledFailure(generation, publication, _) ->
      effect_runner.ScheduledFailureReportFinished(
        generation,
        publication,
        Error(error.LinearApiRequest(reason)),
      )
    effect_runner.CleanupWorkspace(_, workspace_path, _, _) ->
      effect_runner.CleanupFinished(
        workspace_path,
        Error(error.WorkspaceIo(reason)),
      )
  }
}

fn handle_result(
  context: Context(state),
  result: effect_runner.EffectResult,
) -> state {
  let handlers = context.result_handlers
  case result {
    effect_runner.CandidateFetchFinished(generation, result) ->
      handlers.candidate_fetch_finished(context.state, generation, result)
    effect_runner.RunningRefreshFinished(generation, result) ->
      handlers.running_refresh_finished(context.state, generation, result)
    effect_runner.RetryRefreshFinished(issue_id, generation, result) ->
      handlers.retry_refresh_finished(
        context.state,
        issue_id,
        generation,
        result,
      )
    effect_runner.DispatchClaimValidationFinished(issue_id, generation, result) ->
      handlers.dispatch_claim_validation_finished(
        context.state,
        issue_id,
        generation,
        result,
      )
    effect_runner.HandoffClaimFinished(outbox, issue_id, run_id, result) ->
      handlers.handoff_claim_finished(
        context.state,
        outbox,
        issue_id,
        run_id,
        result,
      )
    effect_runner.HandoffSuccessFinished(outbox, issue_id, _run_id, result) ->
      handlers.handoff_success_finished(context.state, outbox, issue_id, result)
    effect_runner.HandoffFailureFinished(outbox, issue_id, _run_id, result) ->
      handlers.handoff_failure_finished(context.state, outbox, issue_id, result)
    effect_runner.HandoffParkFinished(outbox, issue_id, result) ->
      handlers.handoff_park_finished(context.state, outbox, issue_id, result)
    effect_runner.InvalidWorkflowReportFinished(
      outbox,
      issue_id,
      violation_fingerprint,
      reporting_policy_fingerprint,
      result,
    ) ->
      handlers.invalid_workflow_report_finished(
        context.state,
        outbox,
        issue_id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        result,
      )
    effect_runner.OutboxReplayFinished(outbox_replay, result) ->
      handlers.outbox_replay_finished(context.state, outbox_replay, result)
    effect_runner.ScheduledFailureReportFinished(generation, request, result) ->
      handlers.scheduled_failure_report_finished(
        context.state,
        generation,
        request,
        result,
      )
    effect_runner.CleanupFinished(workspace_path, result) ->
      handlers.cleanup_finished(context.state, workspace_path, result)
  }
}
