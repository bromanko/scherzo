import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/log
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/reason as session_reason
import scherzo/state/ledger
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

pub type LedgerAppender =
  fn(effects_types.LedgerAppend) -> Result(Nil, ledger.LedgerError)

pub opaque type ShellState(shell) {
  ShellState(
    data: shell,
    append_ledger: fn(shell, effects_types.LedgerAppend) ->
      #(shell, Result(Nil, ledger.LedgerError)),
    now_ms: fn(shell) -> Int,
    log_effect: fn(shell, String, String, List(log.Field)) -> shell,
    start_worker: fn(shell, effects_types.WorkerStart) ->
      #(shell, Result(Nil, String)),
    reply_snapshot: fn(shell, orchestrator_state.RuntimeState) -> shell,
    mark_poll_in_flight: fn(shell, Int) -> shell,
    schedule_next_poll: fn(shell) -> shell,
    fetch_candidates: fn(shell, Int) -> shell,
    begin_dispatch_validation: fn(shell, String, Int) -> shell,
    begin_review_lane_preflight: fn(
      shell,
      effects_types.ReviewLanePreflightRequest,
    ) -> shell,
    reserve_session_sequence: fn(shell, Int) -> shell,
    claim_issue: fn(shell, task.TaskRef, tracker_issue.Issue, String, String) ->
      shell,
    report_invalid_workflow: fn(
      shell,
      tracker_issue.Issue,
      workflow_policy.IssueWorkflowViolation,
      String,
      String,
    ) -> shell,
    replay_outbox: fn(shell, recovery.OutboxReplay) -> shell,
    remove_retry_timer: fn(shell, String) -> shell,
    finish_retry_refresh: fn(shell, String) -> shell,
    defer_retry_timer: fn(shell, String, Int, Int) -> shell,
    begin_retry_refresh: fn(shell, String, Int) -> shell,
    schedule_retry_timer: fn(shell, String, Int, Int, reason.RetryReason) ->
      shell,
    schedule_recovered_retry_timer: fn(shell, String, Int, Int) -> shell,
    cancel_retry_timer: fn(shell, String, Int, String) -> shell,
    release_claim: fn(shell, String) -> shell,
    clear_recovery: fn(shell, String) -> shell,
    worker_start_failed: fn(shell, effects_types.WorkerStart, String) -> shell,
    remove_worker: fn(shell, effects_types.WorkerIdentity, Bool) -> shell,
    publish_worker_exited: fn(shell, effects_types.WorkerExitPublication) ->
      shell,
    report_worker_success: fn(
      shell,
      effects_types.WorkerIdentity,
      agent_types.WorkerSuccess,
    ) -> shell,
    report_worker_failure: fn(
      shell,
      effects_types.WorkerIdentity,
      agent_types.WorkerFailure,
    ) -> shell,
    cleanup_workspace: fn(shell, String) -> shell,
    park_issue: fn(shell, orchestrator_state.ParkedEntry, Option(String)) ->
      shell,
    report_park: fn(shell, adapter.ParkReport) -> shell,
    stop_worker: fn(
      shell,
      effects_types.WorkerIdentity,
      session_reason.WorkerExitReason,
    ) -> shell,
    stop_worker_after_issue_refresh: fn(
      shell,
      effects_types.WorkerIdentity,
      reason.StopReason,
    ) -> shell,
    register_yaml_step_started: fn(shell, identity.SessionId, identity.RunId) ->
      shell,
    finish_yaml_step_route: fn(shell, identity.SessionId) -> shell,
    finish_yaml_step_session: fn(
      shell,
      identity.SessionId,
      session_reason.WorkerExitReason,
    ) -> shell,
    finish_yaml_step_sessions_for_run: fn(
      shell,
      identity.RunId,
      session_reason.WorkerExitReason,
    ) -> shell,
    clear_yaml_step_routes_for_run: fn(shell, identity.RunId) -> shell,
    mark_yaml_run_stopping: fn(
      shell,
      identity.RunId,
      session_reason.WorkerExitReason,
    ) -> shell,
    shutdown_runtime: fn(shell, Bool) -> shell,
    set_operator_paused: fn(shell, Bool) -> shell,
    apply_operator_command: fn(shell, effects_types.OperatorCommandRequest) ->
      #(shell, command.CommandResult),
    finish_operator_command: fn(
      shell,
      effects_types.OperatorCommandRequest,
      command.CommandResult,
    ) -> #(shell, List(transition_types.Message)),
    report_park_effect: fn(
      shell,
      String,
      String,
      String,
      String,
      Option(String),
    ) -> shell,
  )
}

pub type ApplyResult(shell) {
  ApplyResult(
    shell: ShellState(shell),
    follow_up_messages: List(transition_types.Message),
  )
}

pub fn new_shell_state(
  append_ledger append_ledger: LedgerAppender,
  now_ms now_ms: fn() -> Int,
) -> ShellState(List(effects_types.WorkerStart)) {
  ShellState(
    data: [],
    append_ledger: fn(started_workers, request) {
      #(started_workers, append_ledger(request))
    },
    now_ms: fn(_) { now_ms() },
    log_effect: fn(started_workers, _, _, _) { started_workers },
    start_worker: fn(started_workers, request) {
      #(list.append(started_workers, [request]), Ok(Nil))
    },
    reply_snapshot: fn(started_workers, _) { started_workers },
    mark_poll_in_flight: fn(started_workers, _) { started_workers },
    schedule_next_poll: fn(started_workers) { started_workers },
    fetch_candidates: fn(started_workers, _) { started_workers },
    begin_dispatch_validation: fn(started_workers, _, _) { started_workers },
    begin_review_lane_preflight: fn(started_workers, _) { started_workers },
    reserve_session_sequence: fn(started_workers, _) { started_workers },
    claim_issue: fn(started_workers, _, _, _, _) { started_workers },
    report_invalid_workflow: fn(started_workers, _, _, _, _) { started_workers },
    replay_outbox: fn(started_workers, _) { started_workers },
    remove_retry_timer: fn(started_workers, _) { started_workers },
    finish_retry_refresh: fn(started_workers, _) { started_workers },
    defer_retry_timer: fn(started_workers, _, _, _) { started_workers },
    begin_retry_refresh: fn(started_workers, _, _) { started_workers },
    schedule_retry_timer: fn(started_workers, _, _, _, _) { started_workers },
    schedule_recovered_retry_timer: fn(started_workers, _, _, _) {
      started_workers
    },
    cancel_retry_timer: fn(started_workers, _, _, _) { started_workers },
    release_claim: fn(started_workers, _) { started_workers },
    clear_recovery: fn(started_workers, _) { started_workers },
    worker_start_failed: fn(started_workers, _, _) { started_workers },
    remove_worker: fn(started_workers, _, _) { started_workers },
    publish_worker_exited: fn(started_workers, _) { started_workers },
    report_worker_success: fn(started_workers, _, _) { started_workers },
    report_worker_failure: fn(started_workers, _, _) { started_workers },
    cleanup_workspace: fn(started_workers, _) { started_workers },
    park_issue: fn(started_workers, _, _) { started_workers },
    report_park: fn(started_workers, _) { started_workers },
    stop_worker: fn(started_workers, _, _) { started_workers },
    stop_worker_after_issue_refresh: fn(started_workers, _, _) {
      started_workers
    },
    register_yaml_step_started: fn(started_workers, _, _) { started_workers },
    finish_yaml_step_route: fn(started_workers, _) { started_workers },
    finish_yaml_step_session: fn(started_workers, _, _) { started_workers },
    finish_yaml_step_sessions_for_run: fn(started_workers, _, _) {
      started_workers
    },
    clear_yaml_step_routes_for_run: fn(started_workers, _) { started_workers },
    mark_yaml_run_stopping: fn(started_workers, _, _) { started_workers },
    shutdown_runtime: fn(started_workers, _) { started_workers },
    set_operator_paused: fn(started_workers, _) { started_workers },
    apply_operator_command: fn(started_workers, request) {
      #(
        started_workers,
        command.rejected(
          request.operator_command,
          "operator_command_unhandled",
          None,
        ),
      )
    },
    finish_operator_command: fn(started_workers, _, _) {
      #(started_workers, [])
    },
    report_park_effect: fn(started_workers, _, _, _, _, _) { started_workers },
  )
}

pub fn new_production_shell_state(
  data data: shell,
  append_ledger append_ledger: fn(shell, effects_types.LedgerAppend) ->
    #(shell, Result(Nil, ledger.LedgerError)),
  now_ms now_ms: fn(shell) -> Int,
  log_effect log_effect: fn(shell, String, String, List(log.Field)) -> shell,
  start_worker start_worker: fn(shell, effects_types.WorkerStart) ->
    #(shell, Result(Nil, String)),
  reply_snapshot reply_snapshot: fn(shell, orchestrator_state.RuntimeState) ->
    shell,
  mark_poll_in_flight mark_poll_in_flight: fn(shell, Int) -> shell,
  schedule_next_poll schedule_next_poll: fn(shell) -> shell,
  fetch_candidates fetch_candidates: fn(shell, Int) -> shell,
  begin_dispatch_validation begin_dispatch_validation: fn(shell, String, Int) ->
    shell,
  begin_review_lane_preflight begin_review_lane_preflight: fn(
    shell,
    effects_types.ReviewLanePreflightRequest,
  ) -> shell,
  reserve_session_sequence reserve_session_sequence: fn(shell, Int) -> shell,
  claim_issue claim_issue: fn(
    shell,
    task.TaskRef,
    tracker_issue.Issue,
    String,
    String,
  ) -> shell,
  report_invalid_workflow report_invalid_workflow: fn(
    shell,
    tracker_issue.Issue,
    workflow_policy.IssueWorkflowViolation,
    String,
    String,
  ) -> shell,
  replay_outbox replay_outbox: fn(shell, recovery.OutboxReplay) -> shell,
  remove_retry_timer remove_retry_timer: fn(shell, String) -> shell,
  finish_retry_refresh finish_retry_refresh: fn(shell, String) -> shell,
  defer_retry_timer defer_retry_timer: fn(shell, String, Int, Int) -> shell,
  begin_retry_refresh begin_retry_refresh: fn(shell, String, Int) -> shell,
  schedule_retry_timer schedule_retry_timer: fn(
    shell,
    String,
    Int,
    Int,
    reason.RetryReason,
  ) -> shell,
  schedule_recovered_retry_timer schedule_recovered_retry_timer: fn(
    shell,
    String,
    Int,
    Int,
  ) -> shell,
  cancel_retry_timer cancel_retry_timer: fn(shell, String, Int, String) -> shell,
  release_claim release_claim: fn(shell, String) -> shell,
  clear_recovery clear_recovery: fn(shell, String) -> shell,
  worker_start_failed worker_start_failed: fn(
    shell,
    effects_types.WorkerStart,
    String,
  ) -> shell,
  remove_worker remove_worker: fn(shell, effects_types.WorkerIdentity, Bool) ->
    shell,
  publish_worker_exited publish_worker_exited: fn(
    shell,
    effects_types.WorkerExitPublication,
  ) -> shell,
  report_worker_success report_worker_success: fn(
    shell,
    effects_types.WorkerIdentity,
    agent_types.WorkerSuccess,
  ) -> shell,
  report_worker_failure report_worker_failure: fn(
    shell,
    effects_types.WorkerIdentity,
    agent_types.WorkerFailure,
  ) -> shell,
  cleanup_workspace cleanup_workspace: fn(shell, String) -> shell,
  park_issue park_issue: fn(
    shell,
    orchestrator_state.ParkedEntry,
    Option(String),
  ) -> shell,
  report_park report_park: fn(shell, adapter.ParkReport) -> shell,
  stop_worker stop_worker: fn(
    shell,
    effects_types.WorkerIdentity,
    session_reason.WorkerExitReason,
  ) -> shell,
  stop_worker_after_issue_refresh stop_worker_after_issue_refresh: fn(
    shell,
    effects_types.WorkerIdentity,
    reason.StopReason,
  ) -> shell,
  register_yaml_step_started register_yaml_step_started: fn(
    shell,
    identity.SessionId,
    identity.RunId,
  ) -> shell,
  finish_yaml_step_route finish_yaml_step_route: fn(shell, identity.SessionId) ->
    shell,
  finish_yaml_step_session finish_yaml_step_session: fn(
    shell,
    identity.SessionId,
    session_reason.WorkerExitReason,
  ) -> shell,
  finish_yaml_step_sessions_for_run finish_yaml_step_sessions_for_run: fn(
    shell,
    identity.RunId,
    session_reason.WorkerExitReason,
  ) -> shell,
  clear_yaml_step_routes_for_run clear_yaml_step_routes_for_run: fn(
    shell,
    identity.RunId,
  ) -> shell,
  mark_yaml_run_stopping mark_yaml_run_stopping: fn(
    shell,
    identity.RunId,
    session_reason.WorkerExitReason,
  ) -> shell,
  shutdown_runtime shutdown_runtime: fn(shell, Bool) -> shell,
  set_operator_paused set_operator_paused: fn(shell, Bool) -> shell,
  apply_operator_command apply_operator_command: fn(
    shell,
    effects_types.OperatorCommandRequest,
  ) -> #(shell, command.CommandResult),
  finish_operator_command finish_operator_command: fn(
    shell,
    effects_types.OperatorCommandRequest,
    command.CommandResult,
  ) -> #(shell, List(transition_types.Message)),
  report_park_effect report_park_effect: fn(
    shell,
    String,
    String,
    String,
    String,
    Option(String),
  ) -> shell,
) -> ShellState(shell) {
  ShellState(
    data: data,
    append_ledger: append_ledger,
    now_ms: now_ms,
    log_effect: log_effect,
    start_worker: start_worker,
    reply_snapshot: reply_snapshot,
    mark_poll_in_flight: mark_poll_in_flight,
    schedule_next_poll: schedule_next_poll,
    fetch_candidates: fetch_candidates,
    begin_dispatch_validation: begin_dispatch_validation,
    begin_review_lane_preflight: begin_review_lane_preflight,
    reserve_session_sequence: reserve_session_sequence,
    claim_issue: claim_issue,
    report_invalid_workflow: report_invalid_workflow,
    replay_outbox: replay_outbox,
    remove_retry_timer: remove_retry_timer,
    finish_retry_refresh: finish_retry_refresh,
    defer_retry_timer: defer_retry_timer,
    begin_retry_refresh: begin_retry_refresh,
    schedule_retry_timer: schedule_retry_timer,
    schedule_recovered_retry_timer: schedule_recovered_retry_timer,
    cancel_retry_timer: cancel_retry_timer,
    release_claim: release_claim,
    clear_recovery: clear_recovery,
    worker_start_failed: worker_start_failed,
    remove_worker: remove_worker,
    publish_worker_exited: publish_worker_exited,
    report_worker_success: report_worker_success,
    report_worker_failure: report_worker_failure,
    cleanup_workspace: cleanup_workspace,
    park_issue: park_issue,
    report_park: report_park,
    stop_worker: stop_worker,
    stop_worker_after_issue_refresh: stop_worker_after_issue_refresh,
    register_yaml_step_started: register_yaml_step_started,
    finish_yaml_step_route: finish_yaml_step_route,
    finish_yaml_step_session: finish_yaml_step_session,
    finish_yaml_step_sessions_for_run: finish_yaml_step_sessions_for_run,
    clear_yaml_step_routes_for_run: clear_yaml_step_routes_for_run,
    mark_yaml_run_stopping: mark_yaml_run_stopping,
    shutdown_runtime: shutdown_runtime,
    set_operator_paused: set_operator_paused,
    apply_operator_command: apply_operator_command,
    finish_operator_command: finish_operator_command,
    report_park_effect: report_park_effect,
  )
}

pub fn data(shell: ShellState(shell)) -> shell {
  shell.data
}

pub fn started_workers(
  shell: ShellState(List(effects_types.WorkerStart)),
) -> List(effects_types.WorkerStart) {
  shell.data
}

pub fn apply(
  shell: ShellState(shell),
  effects: List(effects_types.Effect),
) -> ApplyResult(shell) {
  let #(shell, follow_up_messages) = apply_loop(shell, effects, [])
  ApplyResult(
    shell: shell,
    follow_up_messages: list.reverse(follow_up_messages),
  )
}

fn apply_loop(
  shell: ShellState(shell),
  effects: List(effects_types.Effect),
  follow_up_messages: List(transition_types.Message),
) -> #(ShellState(shell), List(transition_types.Message)) {
  case effects {
    [] -> #(shell, follow_up_messages)
    [effect, ..rest] ->
      case effect {
        effects_types.AppendLedger(request) -> {
          let #(data, result) = shell.append_ledger(shell.data, request)
          let shell = ShellState(..shell, data: data)
          let follow_up_messages =
            append_follow_up(shell, request, result, follow_up_messages)
          case should_stop_after_append(request, result) {
            True -> #(shell, follow_up_messages)
            False -> apply_loop(shell, rest, follow_up_messages)
          }
        }
        effects_types.StartWorker(request) -> {
          let #(data, result) = shell.start_worker(shell.data, request)
          let shell = ShellState(..shell, data: data)
          let follow_up_messages = case result {
            Ok(Nil) -> [
              transition_types.WorkerStartSucceeded(
                request.issue_id,
                request.run_id,
                request.session_id,
              ),
              ..follow_up_messages
            ]
            Error(reason) -> [
              transition_types.WorkerStartFailed(
                request.issue_id,
                request.run_id,
                request.session_id,
                reason,
              ),
              ..follow_up_messages
            ]
          }
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.Log(level, event, fields) -> {
          let data = shell.log_effect(shell.data, level, event, fields)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReplySnapshot(snapshot) -> {
          let data = shell.reply_snapshot(shell.data, snapshot)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.MarkPollInFlight(generation) -> {
          let data = shell.mark_poll_in_flight(shell.data, generation)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ScheduleNextPoll -> {
          let data = shell.schedule_next_poll(shell.data)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.FetchCandidates(generation) -> {
          let data = shell.fetch_candidates(shell.data, generation)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.BeginDispatchValidation(issue_id, generation) -> {
          let data =
            shell.begin_dispatch_validation(shell.data, issue_id, generation)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.BeginReviewLanePreflight(request) -> {
          let data = shell.begin_review_lane_preflight(shell.data, request)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReserveSessionSequence(sequence) -> {
          let data = shell.reserve_session_sequence(shell.data, sequence)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ClaimIssue(task_ref, issue, workspace_path, run_id) -> {
          let data =
            shell.claim_issue(
              shell.data,
              task_ref,
              issue,
              workspace_path,
              run_id,
            )
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReportInvalidWorkflow(
          issue,
          violation,
          violation_fingerprint,
          reporting_policy_fingerprint,
        ) -> {
          let data =
            shell.report_invalid_workflow(
              shell.data,
              issue,
              violation,
              violation_fingerprint,
              reporting_policy_fingerprint,
            )
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReplayOutbox(outbox) -> {
          let data = shell.replay_outbox(shell.data, outbox)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.RemoveRetryTimer(issue_id) -> {
          let data = shell.remove_retry_timer(shell.data, issue_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.FinishRetryRefresh(issue_id) -> {
          let data = shell.finish_retry_refresh(shell.data, issue_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.DeferRetryTimer(issue_id, generation, delay_ms) -> {
          let data =
            shell.defer_retry_timer(shell.data, issue_id, generation, delay_ms)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.BeginRetryRefresh(issue_id, generation) -> {
          let data = shell.begin_retry_refresh(shell.data, issue_id, generation)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ScheduleRetryTimer(
          issue_id,
          delay_ms,
          generation,
          retry_reason,
        ) -> {
          let data =
            shell.schedule_retry_timer(
              shell.data,
              issue_id,
              delay_ms,
              generation,
              retry_reason,
            )
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ScheduleRecoveredRetryTimer(
          issue_id,
          delay_ms,
          generation,
        ) -> {
          let data =
            shell.schedule_recovered_retry_timer(
              shell.data,
              issue_id,
              delay_ms,
              generation,
            )
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.CancelRetryTimer(issue_id, generation, cancel_reason) -> {
          let data =
            shell.cancel_retry_timer(
              shell.data,
              issue_id,
              generation,
              cancel_reason,
            )
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReleaseClaim(issue_id) -> {
          let data = shell.release_claim(shell.data, issue_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ClearRecovery(issue_id) -> {
          let data = shell.clear_recovery(shell.data, issue_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.WorkerStartFailed(request, reason) -> {
          let data = shell.worker_start_failed(shell.data, request, reason)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.RemoveWorker(identity, demonitor) -> {
          let data = shell.remove_worker(shell.data, identity, demonitor)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.PublishWorkerExited(request) -> {
          let data = shell.publish_worker_exited(shell.data, request)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReportWorkerSuccess(identity, success) -> {
          let data = shell.report_worker_success(shell.data, identity, success)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReportWorkerFailure(identity, failure) -> {
          let data = shell.report_worker_failure(shell.data, identity, failure)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.CleanupWorkspace(workspace_path) -> {
          let data = shell.cleanup_workspace(shell.data, workspace_path)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ParkIssue(parked, source_run_id) -> {
          let data = shell.park_issue(shell.data, parked, source_run_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ReportPark(report) -> {
          let data = shell.report_park(shell.data, report)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.StopWorker(identity, reason) -> {
          let data = shell.stop_worker(shell.data, identity, reason)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.StopWorkerAfterIssueRefresh(identity, reason) -> {
          let data =
            shell.stop_worker_after_issue_refresh(shell.data, identity, reason)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.RegisterYamlStepStarted(session_id, run_id) -> {
          let data =
            shell.register_yaml_step_started(shell.data, session_id, run_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.FinishYamlStepRoute(session_id) -> {
          let data = shell.finish_yaml_step_route(shell.data, session_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.FinishYamlStepSession(session_id, reason) -> {
          let data =
            shell.finish_yaml_step_session(shell.data, session_id, reason)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.FinishYamlStepSessionsForRun(run_id, reason) -> {
          let data =
            shell.finish_yaml_step_sessions_for_run(shell.data, run_id, reason)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ClearYamlStepRoutesForRun(run_id) -> {
          let data = shell.clear_yaml_step_routes_for_run(shell.data, run_id)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.MarkYamlRunStopping(run_id, reason) -> {
          let data = shell.mark_yaml_run_stopping(shell.data, run_id, reason)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ShutdownRuntime(stop_effect_runner) -> {
          let data = shell.shutdown_runtime(shell.data, stop_effect_runner)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.SetOperatorPaused(paused) -> {
          let data = shell.set_operator_paused(shell.data, paused)
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
        effects_types.ApplyOperatorCommand(request) -> {
          let #(data, result) =
            shell.apply_operator_command(shell.data, request)
          let #(data, new_follow_ups) =
            shell.finish_operator_command(data, request, result)
          let shell = ShellState(..shell, data: data)
          apply_loop(
            shell,
            rest,
            list.append(list.reverse(new_follow_ups), follow_up_messages),
          )
        }
        effects_types.FinishOperatorCommand(request, result) -> {
          let #(data, new_follow_ups) =
            shell.finish_operator_command(shell.data, request, result)
          let shell = ShellState(..shell, data: data)
          apply_loop(
            shell,
            rest,
            list.append(list.reverse(new_follow_ups), follow_up_messages),
          )
        }
        effects_types.ReportParkEffect(
          issue_id,
          issue_identifier,
          reason,
          release_policy,
          source_run_id,
        ) -> {
          let data =
            shell.report_park_effect(
              shell.data,
              issue_id,
              issue_identifier,
              reason,
              release_policy,
              source_run_id,
            )
          let shell = ShellState(..shell, data: data)
          apply_loop(shell, rest, follow_up_messages)
        }
      }
  }
}

fn append_follow_up(
  shell: ShellState(shell),
  request: effects_types.LedgerAppend,
  result: Result(Nil, ledger.LedgerError),
  follow_up_messages: List(transition_types.Message),
) -> List(transition_types.Message) {
  case transition_types.ledger_append_continuation(request.policy) {
    Some(continuation) -> [
      transition_types.LedgerAppendCompleted(
        correlation_id: request.correlation_id,
        continuation: continuation,
        result: result,
        now_ms: shell.now_ms(shell.data),
      ),
      ..follow_up_messages
    ]
    None -> follow_up_messages
  }
}

fn should_stop_after_append(
  request: effects_types.LedgerAppend,
  result: Result(Nil, ledger.LedgerError),
) -> Bool {
  case request.policy {
    effects_types.StopBatchOnFailure -> result != Ok(Nil)
    effects_types.ContinueRegardless
    | effects_types.ScheduleRetryTimerAfterAppend(..)
    | effects_types.CancelRetryTimerAfterAppend(..)
    | effects_types.SpawnClaimedWorkerAfterAppend(..)
    | effects_types.ReportParkAfterAppend(..)
    | effects_types.SetOperatorPausedAfterAppend(..) -> False
  }
}
