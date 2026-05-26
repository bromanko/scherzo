import gleam/option.{type Option}
import scherzo/control/command
import scherzo/control/linear_parser
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/linear_commands
import scherzo/orchestrator/transitions/operator
import scherzo/state/ledger
import scherzo/state/recovery
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue

pub type OperatorCallbacks =
  operator.Callbacks

pub fn handle_remote_submitted(
  state: transition_types.State,
  event: adapter.RemoteCommandEvent,
  parsed: linear_parser.ParsedLinearCommand,
  safe_excerpt: String,
) -> transition_types.Outcome {
  linear_commands.handle_submitted(state, event, parsed, safe_excerpt)
}

pub fn handle_remote_applied(
  state: transition_types.State,
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
  command_name: String,
  result: command.CommandResult,
  message_excerpt: String,
  ack_body: Option(String),
) -> transition_types.Outcome {
  linear_commands.handle_applied(
    state,
    backend_kind,
    event_id,
    task_remote_id,
    command_name,
    result,
    message_excerpt,
    ack_body,
  )
}

pub fn request_remote_ack(
  state: transition_types.State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_recorded: Bool,
  outbox_kind: String,
) -> transition_types.Outcome {
  linear_commands.request_ack(
    state,
    backend_kind,
    task_remote_id,
    event_id,
    body,
    outbox_recorded,
    outbox_kind,
  )
}

pub fn handle_remote_ack_finished(
  state: transition_types.State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  outbox_kind: String,
  result: Result(Nil, String),
) -> transition_types.Outcome {
  linear_commands.handle_ack_finished(
    state,
    backend_kind,
    task_remote_id,
    event_id,
    outbox_kind,
    result,
  )
}

pub fn retry_pending_remote_acks(
  state: transition_types.State,
) -> transition_types.Outcome {
  linear_commands.retry_pending_acks(state)
}

pub fn startup_outbox_replay_effects(
  outbox_to_replay: List(recovery.OutboxReplay),
) -> List(effects_types.Effect) {
  linear_commands.startup_outbox_replay_effects(outbox_to_replay)
}

pub fn handle_remote_apply_continuation(
  state: transition_types.State,
  correlation_id: String,
  request: effects_types.OperatorCommandRequest,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_apply_continuation(
    state,
    correlation_id,
    request,
    result,
  )
}

pub fn handle_remote_enqueue_continuation(
  state: transition_types.State,
  correlation_id: String,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_enqueue_continuation(
    state,
    correlation_id,
    backend_kind,
    task_remote_id,
    event_id,
    body,
    outbox_kind,
    result,
  )
}

pub fn handle_remote_publish_continuation(
  state: transition_types.State,
  correlation_id: String,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_publish_continuation(
    state,
    correlation_id,
    backend_kind,
    task_remote_id,
    event_id,
    body,
    outbox_kind,
    result,
  )
}

pub fn handle_remote_remove_continuation(
  state: transition_types.State,
  correlation_id: String,
  task_remote_id: String,
  event_id: String,
  ack_key: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_remove_continuation(
    state,
    correlation_id,
    task_remote_id,
    event_id,
    ack_key,
    result,
  )
}

pub fn operator_callbacks(
  issue_is_running_claimed_or_pending: fn(
    transition_types.State,
    transition_types.DispatchContext,
    String,
  ) -> Bool,
  can_reserve_dispatch_slot: fn(
    transition_types.State,
    transition_types.DispatchContext,
    tracker_issue.Issue,
  ) -> Bool,
  dispatch_candidates: fn(
    List(tracker_issue.Issue),
    transition_types.State,
    transition_types.DispatchContext,
  ) -> transition_types.Outcome,
) -> operator.Callbacks {
  operator.Callbacks(
    issue_is_running_claimed_or_pending: issue_is_running_claimed_or_pending,
    can_reserve_dispatch_slot: can_reserve_dispatch_slot,
    dispatch_candidates: dispatch_candidates,
  )
}

pub fn handle_operator_submitted(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue_resolution: transition_types.OperatorIssueResolution,
  parked_issue_resolution: transition_types.ParkedIssueResolution,
  callbacks: operator.Callbacks,
) -> transition_types.Outcome {
  operator.handle_submitted(
    state,
    request,
    context,
    issue_resolution,
    parked_issue_resolution,
    callbacks,
  )
}

pub fn handle_operator_report_park_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  issue_identifier: String,
  reason: String,
  release_policy: String,
  source_run_id: Option(String),
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  operator.handle_report_park_continuation(
    state,
    correlation_id,
    issue_id,
    issue_identifier,
    reason,
    release_policy,
    source_run_id,
    result,
  )
}
