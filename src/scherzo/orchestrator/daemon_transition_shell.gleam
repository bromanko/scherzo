import gleam/option.{type Option}
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/log
import scherzo/orchestrator/effects/interpreter as transition_interpreter
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/reason as session_reason
import scherzo/state/ledger
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

const transition_runner_message_limit = 128

pub opaque type ShellHandlers(state) {
  ShellHandlers(
    append_ledger: fn(state, transition_effects.LedgerAppend) ->
      #(state, Result(Nil, ledger.LedgerError)),
    now_ms: fn(state) -> Int,
    log_effect: fn(state, String, String, List(log.Field)) -> state,
    start_worker: fn(state, transition_effects.WorkerStart) ->
      #(state, Result(Nil, String)),
    reply_snapshot: fn(state, orchestrator_state.RuntimeState) -> state,
    mark_poll_in_flight: fn(state, Int) -> state,
    schedule_next_poll: fn(state) -> state,
    fetch_candidates: fn(state, Int) -> state,
    begin_dispatch_validation: fn(state, String, Int) -> state,
    reserve_session_sequence: fn(state, Int) -> state,
    claim_issue: fn(state, task.TaskRef, tracker_issue.Issue, String, String) ->
      state,
    report_invalid_workflow: fn(
      state,
      tracker_issue.Issue,
      workflow_policy.IssueWorkflowViolation,
      String,
      String,
    ) -> state,
    remove_retry_timer: fn(state, String) -> state,
    finish_retry_refresh: fn(state, String) -> state,
    defer_retry_timer: fn(state, String, Int, Int) -> state,
    begin_retry_refresh: fn(state, String, Int) -> state,
    schedule_retry_timer: fn(state, String, Int, Int, reason.RetryReason) ->
      state,
    schedule_recovered_retry_timer: fn(state, String, Int, Int) -> state,
    cancel_retry_timer: fn(state, String, Int, String) -> state,
    release_claim: fn(state, String) -> state,
    clear_recovery: fn(state, String) -> state,
    worker_start_failed: fn(state, transition_effects.WorkerStart, String) ->
      state,
    remove_worker: fn(state, transition_effects.WorkerIdentity, Bool) -> state,
    publish_worker_exited: fn(state, transition_effects.WorkerExitPublication) ->
      state,
    report_worker_success: fn(
      state,
      transition_effects.WorkerIdentity,
      agent_types.WorkerSuccess,
    ) -> state,
    report_worker_failure: fn(
      state,
      transition_effects.WorkerIdentity,
      agent_types.WorkerFailure,
    ) -> state,
    cleanup_workspace: fn(state, String) -> state,
    park_issue: fn(state, orchestrator_state.ParkedEntry, Option(String)) ->
      state,
    report_park: fn(state, adapter.ParkReport) -> state,
    stop_worker: fn(
      state,
      transition_effects.WorkerIdentity,
      session_reason.WorkerExitReason,
    ) -> state,
    stop_worker_after_issue_refresh: fn(
      state,
      transition_effects.WorkerIdentity,
      reason.StopReason,
    ) -> state,
    register_yaml_step_started: fn(state, identity.SessionId, identity.RunId) ->
      state,
    finish_yaml_step_route: fn(state, identity.SessionId) -> state,
    finish_yaml_step_session: fn(
      state,
      identity.SessionId,
      session_reason.WorkerExitReason,
    ) -> state,
    finish_yaml_step_sessions_for_run: fn(
      state,
      identity.RunId,
      session_reason.WorkerExitReason,
    ) -> state,
    clear_yaml_step_routes_for_run: fn(state, identity.RunId) -> state,
    mark_yaml_run_stopping: fn(
      state,
      identity.RunId,
      session_reason.WorkerExitReason,
    ) -> state,
    shutdown_runtime: fn(state, Bool) -> state,
    set_operator_paused: fn(state, Bool) -> state,
    apply_operator_command: fn(state, transition_effects.OperatorCommandRequest) ->
      #(state, command.CommandResult),
    finish_operator_command: fn(
      state,
      transition_effects.OperatorCommandRequest,
      command.CommandResult,
    ) -> #(state, List(transition_types.Message)),
    report_park_effect: fn(
      state,
      String,
      String,
      String,
      String,
      Option(String),
    ) -> state,
  )
}

pub fn shell_handlers(
  append_ledger append_ledger: fn(state, transition_effects.LedgerAppend) ->
    #(state, Result(Nil, ledger.LedgerError)),
  now_ms now_ms: fn(state) -> Int,
  log_effect log_effect: fn(state, String, String, List(log.Field)) -> state,
  start_worker start_worker: fn(state, transition_effects.WorkerStart) ->
    #(state, Result(Nil, String)),
  reply_snapshot reply_snapshot: fn(state, orchestrator_state.RuntimeState) ->
    state,
  mark_poll_in_flight mark_poll_in_flight: fn(state, Int) -> state,
  schedule_next_poll schedule_next_poll: fn(state) -> state,
  fetch_candidates fetch_candidates: fn(state, Int) -> state,
  begin_dispatch_validation begin_dispatch_validation: fn(state, String, Int) ->
    state,
  reserve_session_sequence reserve_session_sequence: fn(state, Int) -> state,
  claim_issue claim_issue: fn(
    state,
    task.TaskRef,
    tracker_issue.Issue,
    String,
    String,
  ) -> state,
  report_invalid_workflow report_invalid_workflow: fn(
    state,
    tracker_issue.Issue,
    workflow_policy.IssueWorkflowViolation,
    String,
    String,
  ) -> state,
  remove_retry_timer remove_retry_timer: fn(state, String) -> state,
  finish_retry_refresh finish_retry_refresh: fn(state, String) -> state,
  defer_retry_timer defer_retry_timer: fn(state, String, Int, Int) -> state,
  begin_retry_refresh begin_retry_refresh: fn(state, String, Int) -> state,
  schedule_retry_timer schedule_retry_timer: fn(
    state,
    String,
    Int,
    Int,
    reason.RetryReason,
  ) -> state,
  schedule_recovered_retry_timer schedule_recovered_retry_timer: fn(
    state,
    String,
    Int,
    Int,
  ) -> state,
  cancel_retry_timer cancel_retry_timer: fn(state, String, Int, String) -> state,
  release_claim release_claim: fn(state, String) -> state,
  clear_recovery clear_recovery: fn(state, String) -> state,
  worker_start_failed worker_start_failed: fn(
    state,
    transition_effects.WorkerStart,
    String,
  ) -> state,
  remove_worker remove_worker: fn(
    state,
    transition_effects.WorkerIdentity,
    Bool,
  ) -> state,
  publish_worker_exited publish_worker_exited: fn(
    state,
    transition_effects.WorkerExitPublication,
  ) -> state,
  report_worker_success report_worker_success: fn(
    state,
    transition_effects.WorkerIdentity,
    agent_types.WorkerSuccess,
  ) -> state,
  report_worker_failure report_worker_failure: fn(
    state,
    transition_effects.WorkerIdentity,
    agent_types.WorkerFailure,
  ) -> state,
  cleanup_workspace cleanup_workspace: fn(state, String) -> state,
  park_issue park_issue: fn(
    state,
    orchestrator_state.ParkedEntry,
    Option(String),
  ) -> state,
  report_park report_park: fn(state, adapter.ParkReport) -> state,
  stop_worker stop_worker: fn(
    state,
    transition_effects.WorkerIdentity,
    session_reason.WorkerExitReason,
  ) -> state,
  stop_worker_after_issue_refresh stop_worker_after_issue_refresh: fn(
    state,
    transition_effects.WorkerIdentity,
    reason.StopReason,
  ) -> state,
  register_yaml_step_started register_yaml_step_started: fn(
    state,
    identity.SessionId,
    identity.RunId,
  ) -> state,
  finish_yaml_step_route finish_yaml_step_route: fn(state, identity.SessionId) ->
    state,
  finish_yaml_step_session finish_yaml_step_session: fn(
    state,
    identity.SessionId,
    session_reason.WorkerExitReason,
  ) -> state,
  finish_yaml_step_sessions_for_run finish_yaml_step_sessions_for_run: fn(
    state,
    identity.RunId,
    session_reason.WorkerExitReason,
  ) -> state,
  clear_yaml_step_routes_for_run clear_yaml_step_routes_for_run: fn(
    state,
    identity.RunId,
  ) -> state,
  mark_yaml_run_stopping mark_yaml_run_stopping: fn(
    state,
    identity.RunId,
    session_reason.WorkerExitReason,
  ) -> state,
  shutdown_runtime shutdown_runtime: fn(state, Bool) -> state,
  set_operator_paused set_operator_paused: fn(state, Bool) -> state,
  apply_operator_command apply_operator_command: fn(
    state,
    transition_effects.OperatorCommandRequest,
  ) -> #(state, command.CommandResult),
  finish_operator_command finish_operator_command: fn(
    state,
    transition_effects.OperatorCommandRequest,
    command.CommandResult,
  ) -> #(state, List(transition_types.Message)),
  report_park_effect report_park_effect: fn(
    state,
    String,
    String,
    String,
    String,
    Option(String),
  ) -> state,
) -> ShellHandlers(state) {
  ShellHandlers(
    append_ledger: append_ledger,
    now_ms: now_ms,
    log_effect: log_effect,
    start_worker: start_worker,
    reply_snapshot: reply_snapshot,
    mark_poll_in_flight: mark_poll_in_flight,
    schedule_next_poll: schedule_next_poll,
    fetch_candidates: fetch_candidates,
    begin_dispatch_validation: begin_dispatch_validation,
    reserve_session_sequence: reserve_session_sequence,
    claim_issue: claim_issue,
    report_invalid_workflow: report_invalid_workflow,
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

pub opaque type Context(state) {
  Context(
    state: state,
    transition_state_from_state: fn(state) -> transition_types.State,
    merge_transition_state: fn(state, transition_types.State) -> state,
    log_exhausted: fn(state, Int) -> state,
    max_messages: Int,
    handlers: ShellHandlers(state),
  )
}

pub fn context(
  state state: state,
  transition_state_from_state transition_state_from_state: fn(state) ->
    transition_types.State,
  merge_transition_state merge_transition_state: fn(
    state,
    transition_types.State,
  ) -> state,
  log_exhausted log_exhausted: fn(state, Int) -> state,
  max_messages max_messages: Int,
  handlers handlers: ShellHandlers(state),
) -> Context(state) {
  Context(
    state: state,
    transition_state_from_state: transition_state_from_state,
    merge_transition_state: merge_transition_state,
    log_exhausted: log_exhausted,
    max_messages: max_messages,
    handlers: handlers,
  )
}

pub fn run(
  context: Context(state),
  messages: List(transition_types.Message),
) -> state {
  let transition_state = context.transition_state_from_state(context.state)
  let shell = transition_shell(context.state, context.handlers)
  let transition_runner.RunResult(
    state: transition_state,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: transition_state,
      shell: shell,
      messages: messages,
      max_messages: context.max_messages,
    )
  let state =
    context.merge_transition_state(
      transition_interpreter.data(shell),
      transition_state,
    )
  case exhausted {
    True -> context.log_exhausted(state, context.max_messages)
    False -> state
  }
}

pub fn default_message_limit() -> Int {
  transition_runner_message_limit
}

pub fn interpret_effects(
  state: state,
  handlers: ShellHandlers(state),
  effects: List(transition_effects.Effect),
) -> #(state, List(transition_types.Message)) {
  let shell = transition_shell(state, handlers)
  let transition_interpreter.ApplyResult(
    shell: shell,
    follow_up_messages: follow_up_messages,
  ) = transition_interpreter.apply(shell, effects)
  #(transition_interpreter.data(shell), follow_up_messages)
}

fn transition_shell(
  state: state,
  handlers: ShellHandlers(state),
) -> transition_interpreter.ShellState(state) {
  transition_interpreter.new_production_shell_state(
    data: state,
    append_ledger: handlers.append_ledger,
    now_ms: handlers.now_ms,
    log_effect: handlers.log_effect,
    start_worker: handlers.start_worker,
    reply_snapshot: handlers.reply_snapshot,
    mark_poll_in_flight: handlers.mark_poll_in_flight,
    schedule_next_poll: handlers.schedule_next_poll,
    fetch_candidates: handlers.fetch_candidates,
    begin_dispatch_validation: handlers.begin_dispatch_validation,
    reserve_session_sequence: handlers.reserve_session_sequence,
    claim_issue: handlers.claim_issue,
    report_invalid_workflow: handlers.report_invalid_workflow,
    remove_retry_timer: handlers.remove_retry_timer,
    finish_retry_refresh: handlers.finish_retry_refresh,
    defer_retry_timer: handlers.defer_retry_timer,
    begin_retry_refresh: handlers.begin_retry_refresh,
    schedule_retry_timer: handlers.schedule_retry_timer,
    schedule_recovered_retry_timer: handlers.schedule_recovered_retry_timer,
    cancel_retry_timer: handlers.cancel_retry_timer,
    release_claim: handlers.release_claim,
    clear_recovery: handlers.clear_recovery,
    worker_start_failed: handlers.worker_start_failed,
    remove_worker: handlers.remove_worker,
    publish_worker_exited: handlers.publish_worker_exited,
    report_worker_success: handlers.report_worker_success,
    report_worker_failure: handlers.report_worker_failure,
    cleanup_workspace: handlers.cleanup_workspace,
    park_issue: handlers.park_issue,
    report_park: handlers.report_park,
    stop_worker: handlers.stop_worker,
    stop_worker_after_issue_refresh: handlers.stop_worker_after_issue_refresh,
    register_yaml_step_started: handlers.register_yaml_step_started,
    finish_yaml_step_route: handlers.finish_yaml_step_route,
    finish_yaml_step_session: handlers.finish_yaml_step_session,
    finish_yaml_step_sessions_for_run: handlers.finish_yaml_step_sessions_for_run,
    clear_yaml_step_routes_for_run: handlers.clear_yaml_step_routes_for_run,
    mark_yaml_run_stopping: handlers.mark_yaml_run_stopping,
    shutdown_runtime: handlers.shutdown_runtime,
    set_operator_paused: handlers.set_operator_paused,
    apply_operator_command: handlers.apply_operator_command,
    finish_operator_command: handlers.finish_operator_command,
    report_park_effect: handlers.report_park_effect,
  )
}
