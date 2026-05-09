import gleam/option.{type Option}
import scherzo/control/command
import scherzo/control/linear_parser
import scherzo/linear
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/linear_commands
import scherzo/orchestrator/transitions/operator
import scherzo/state/ledger
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue

pub type OperatorCallbacks =
  operator.Callbacks

pub fn handle_linear_submitted(
  state: transition_types.State,
  comment: linear.LinearComment,
  parsed: linear_parser.ParsedLinearCommand,
  safe_excerpt: String,
) -> transition_types.Outcome {
  linear_commands.handle_submitted(state, comment, parsed, safe_excerpt)
}

pub fn handle_linear_applied(
  state: transition_types.State,
  comment_id: String,
  issue_id: String,
  command_name: String,
  result: command.CommandResult,
  message_excerpt: String,
  ack_body: Option(String),
) -> transition_types.Outcome {
  linear_commands.handle_applied(
    state,
    comment_id,
    issue_id,
    command_name,
    result,
    message_excerpt,
    ack_body,
  )
}

pub fn request_linear_ack(
  state: transition_types.State,
  issue_id: String,
  source_comment_id: String,
  body: String,
  outbox_recorded: Bool,
) -> transition_types.Outcome {
  linear_commands.request_ack(
    state,
    issue_id,
    source_comment_id,
    body,
    outbox_recorded,
  )
}

pub fn handle_linear_ack_finished(
  state: transition_types.State,
  issue_id: String,
  source_comment_id: String,
  result: Result(Nil, String),
) -> transition_types.Outcome {
  linear_commands.handle_ack_finished(
    state,
    issue_id,
    source_comment_id,
    result,
  )
}

pub fn retry_pending_linear_acks(
  state: transition_types.State,
) -> transition_types.Outcome {
  linear_commands.retry_pending_acks(state)
}

pub fn startup_outbox_replay_effects(
  outbox_to_replay: List(recovery.OutboxReplay),
) -> List(effects_types.Effect) {
  linear_commands.startup_outbox_replay_effects(outbox_to_replay)
}

pub fn handle_linear_apply_continuation(
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

pub fn handle_linear_enqueue_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  source_comment_id: String,
  body: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_enqueue_continuation(
    state,
    correlation_id,
    issue_id,
    source_comment_id,
    body,
    result,
  )
}

pub fn handle_linear_publish_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  source_comment_id: String,
  body: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_publish_continuation(
    state,
    correlation_id,
    issue_id,
    source_comment_id,
    body,
    result,
  )
}

pub fn handle_linear_remove_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  source_comment_id: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  linear_commands.handle_remove_continuation(
    state,
    correlation_id,
    issue_id,
    source_comment_id,
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
