import gleam/int
import gleam/list
import gleam/option.{type Option, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/log
import scherzo/orchestrator/effects/interpreter as transition_interpreter
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/task_lifecycle_legacy
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_invariants
import scherzo/orchestrator/transition_runner
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

const transition_runner_message_limit = 128

const invariant_violation_log_limit = 32

pub type InvariantMode {
  FailOnInvariantViolation
  WarnOnInvariantViolation
}

pub type InvariantChecker =
  fn(transition_types.State) ->
    Result(Nil, List(transition_invariants.InvariantError))

pub fn default_invariant_checker(
  state: transition_types.State,
) -> Result(Nil, List(transition_invariants.InvariantError)) {
  transition_invariants.check(state)
}

pub fn invariant_mode_from_string(value: String) -> InvariantMode {
  case string.lowercase(string.trim(value)) {
    "warn" -> WarnOnInvariantViolation
    _ -> FailOnInvariantViolation
  }
}

pub fn lifecycle_projection_failed(state: transition_types.State) -> Bool {
  case task_lifecycle_legacy.from_transition_state(state) {
    Ok(_) -> False
    Error(error) -> projection_failed(error)
  }
}

fn projection_failed(error: task_lifecycle_legacy.LifecycleError) -> Bool {
  case error {
    task_lifecycle.ConflictingLifecycleSources(_, _, _)
    | task_lifecycle.MissingClaimedLifecycle(_)
    | task_lifecycle.MissingRetryWaitingForRefresh(_, _)
    | task_lifecycle.RunningWorkerMismatch(_) -> True
  }
}

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
    begin_review_lane_preflight: fn(
      state,
      transition_effects.ReviewLanePreflightRequest,
    ) -> state,
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
    replay_outbox: fn(state, recovery.OutboxReplay) -> state,
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
      #(state, command.CommandResult, List(transition_types.Message)),
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
  begin_review_lane_preflight begin_review_lane_preflight: fn(
    state,
    transition_effects.ReviewLanePreflightRequest,
  ) -> state,
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
  replay_outbox replay_outbox: fn(state, recovery.OutboxReplay) -> state,
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
  ) -> #(state, command.CommandResult, List(transition_types.Message)),
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

pub opaque type Context(state) {
  Context(
    state: state,
    transition_state_from_state: fn(state) -> transition_types.State,
    merge_transition_state: fn(
      state,
      transition_types.State,
      transition_types.State,
    ) -> state,
    log_exhausted: fn(state, Int) -> state,
    mark_invariant_failure: fn(
      state,
      List(transition_invariants.InvariantError),
    ) -> state,
    invariant_mode: InvariantMode,
    invariant_checker: InvariantChecker,
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
    transition_types.State,
  ) -> state,
  log_exhausted log_exhausted: fn(state, Int) -> state,
  mark_invariant_failure mark_invariant_failure: fn(
    state,
    List(transition_invariants.InvariantError),
  ) -> state,
  invariant_mode invariant_mode: InvariantMode,
  invariant_checker invariant_checker: InvariantChecker,
  max_messages max_messages: Int,
  handlers handlers: ShellHandlers(state),
) -> Context(state) {
  Context(
    state: state,
    transition_state_from_state: transition_state_from_state,
    merge_transition_state: merge_transition_state,
    log_exhausted: log_exhausted,
    mark_invariant_failure: mark_invariant_failure,
    invariant_mode: invariant_mode,
    invariant_checker: invariant_checker,
    max_messages: max_messages,
    handlers: handlers,
  )
}

pub fn run(
  context: Context(state),
  messages: List(transition_types.Message),
) -> state {
  let input_transition_state =
    context.transition_state_from_state(context.state)
  let shell = transition_shell(context.state, context.handlers)
  let transition_runner.RunResult(
    state: transition_state,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: input_transition_state,
      shell: shell,
      messages: messages,
      max_messages: context.max_messages,
    )
  let state =
    context.merge_transition_state(
      transition_interpreter.data(shell),
      input_transition_state,
      transition_state,
    )
  let state = check_invariants(Context(..context, state: state))
  case exhausted {
    True -> context.log_exhausted(state, context.max_messages)
    False -> state
  }
}

pub fn check_invariants(context: Context(state)) -> state {
  let transition_state = context.transition_state_from_state(context.state)
  case context.invariant_checker(transition_state) {
    Ok(Nil) -> context.state
    Error(errors) -> apply_invariant_violations(context, errors)
  }
}

fn apply_invariant_violations(
  context: Context(state),
  errors: List(transition_invariants.InvariantError),
) -> state {
  let #(warning_errors, failure_errors) =
    split_invariant_errors(context.invariant_mode, errors)
  let state = case warning_errors {
    [] -> context.state
    _ ->
      log_invariant_violations(context, context.state, "warn", warning_errors)
  }
  case failure_errors {
    [] -> state
    _ -> {
      let state =
        log_invariant_violations(context, state, "error", failure_errors)
      context.mark_invariant_failure(state, failure_errors)
    }
  }
}

fn log_invariant_violations(
  context: Context(state),
  state: state,
  level: String,
  errors: List(transition_invariants.InvariantError),
) -> state {
  context.handlers.log_effect(
    state,
    level,
    invariant_violation_event(level),
    invariant_violation_fields(errors),
  )
}

fn invariant_violation_event(level: String) -> String {
  case level {
    "error" -> "transition_invariant_violation"
    _ -> "transition_invariant_warning"
  }
}

fn invariant_violation_fields(
  errors: List(transition_invariants.InvariantError),
) -> List(log.Field) {
  let total_count = list.length(errors)
  let logged_errors = list.take(errors, invariant_violation_log_limit)
  let logged_count = list.length(logged_errors)
  let omitted_count = total_count - logged_count
  [
    #("count", int.to_string(total_count)),
    #("logged_count", int.to_string(logged_count)),
    #("omitted_count", int.to_string(omitted_count)),
    #("truncated", case omitted_count > 0 {
      True -> "true"
      False -> "false"
    }),
    #(
      "rule_ids",
      logged_errors
        |> list.map(transition_invariants.error_code)
        |> string.join(with: ","),
    ),
    #(
      "identities",
      logged_errors
        |> list.map(transition_invariants.error_identity)
        |> string.join(with: ","),
    ),
    #("violations", transition_invariants.format_errors(logged_errors)),
  ]
}

fn split_invariant_errors(
  mode: InvariantMode,
  errors: List(transition_invariants.InvariantError),
) -> #(
  List(transition_invariants.InvariantError),
  List(transition_invariants.InvariantError),
) {
  case mode {
    WarnOnInvariantViolation -> #(errors, [])
    FailOnInvariantViolation ->
      errors
      |> list.fold(#([], []), fn(acc, error) {
        let #(warnings, failures) = acc
        case transition_invariants.is_warn_only(error) {
          True -> #([error, ..warnings], failures)
          False -> #(warnings, [error, ..failures])
        }
      })
      |> reverse_invariant_error_pair
  }
}

fn reverse_invariant_error_pair(
  pair: #(
    List(transition_invariants.InvariantError),
    List(transition_invariants.InvariantError),
  ),
) -> #(
  List(transition_invariants.InvariantError),
  List(transition_invariants.InvariantError),
) {
  let #(warnings, failures) = pair
  #(list.reverse(warnings), list.reverse(failures))
}

pub fn default_message_limit() -> Int {
  transition_runner_message_limit
}

pub fn run_one_message_with_operator_reply(
  context context: Context(state),
  message message: transition_types.Message,
  operator_command operator_command: command.OperatorCommand,
  send_reply send_reply: fn(command.CommandResult) -> Nil,
) -> state {
  let input_transition_state =
    context.transition_state_from_state(context.state)
  let transition_types.Outcome(state: transition_state, effects: effects) =
    transition.handle(message, input_transition_state)
  let #(request, result, effects_after_reply) = case
    split_operator_command_finish(effects)
  {
    Ok(split) -> split
    Error(Nil) -> #(
      transition_effects.OperatorCommandRequest(
        correlation_id: "missing",
        source: transition_effects.LocalOperatorCommand,
        operator_command: operator_command,
        timeout_ms: 0,
      ),
      command.rejected(
        operator_command,
        "operator_command_result_missing",
        Some("operator command did not produce a result"),
      ),
      effects,
    )
  }
  send_reply(result)
  let #(state, finish_follow_ups) =
    context.handlers.finish_operator_command(context.state, request, result)
  let shell = transition_shell(state, context.handlers)
  let transition_interpreter.ApplyResult(
    shell: shell,
    follow_up_messages: follow_up_messages,
  ) = transition_interpreter.apply(shell, effects_after_reply)
  let transition_runner.RunResult(
    state: transition_state,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: transition_state,
      shell: shell,
      messages: list.append(follow_up_messages, finish_follow_ups),
      max_messages: context.max_messages,
    )
  let state =
    context.merge_transition_state(
      transition_interpreter.data(shell),
      input_transition_state,
      transition_state,
    )
  case exhausted {
    True -> context.log_exhausted(state, context.max_messages)
    False -> state
  }
}

fn split_operator_command_finish(
  effects: List(transition_effects.Effect),
) -> Result(
  #(
    transition_effects.OperatorCommandRequest,
    command.CommandResult,
    List(transition_effects.Effect),
  ),
  Nil,
) {
  split_operator_command_finish_loop(effects, [])
}

fn split_operator_command_finish_loop(
  effects: List(transition_effects.Effect),
  preceding: List(transition_effects.Effect),
) -> Result(
  #(
    transition_effects.OperatorCommandRequest,
    command.CommandResult,
    List(transition_effects.Effect),
  ),
  Nil,
) {
  case effects {
    [] -> Error(Nil)
    [effect, ..rest] ->
      case effect {
        transition_effects.FinishOperatorCommand(request, result) ->
          Ok(#(request, result, list.append(list.reverse(preceding), rest)))
        _ -> split_operator_command_finish_loop(rest, [effect, ..preceding])
      }
  }
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
    begin_review_lane_preflight: handlers.begin_review_lane_preflight,
    reserve_session_sequence: handlers.reserve_session_sequence,
    claim_issue: handlers.claim_issue,
    report_invalid_workflow: handlers.report_invalid_workflow,
    replay_outbox: handlers.replay_outbox,
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
