import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/control/command
import scherzo/control/linear_parser
import scherzo/linear
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/state/ledger
import scherzo/state/outbox
import scherzo/state/record
import scherzo/state/recovery

pub fn handle_submitted(
  state: transition_types.State,
  comment: linear.LinearComment,
  parsed: linear_parser.ParsedLinearCommand,
  safe_excerpt: String,
) -> transition_types.Outcome {
  let command_name = command.command_name(parsed.command)
  let request =
    effects_types.OperatorCommandRequest(
      source: effects_types.LinearOperatorCommand(
        comment_id: comment.id,
        issue_id: comment.issue_id,
        command_name: command_name,
        excerpt: parsed.excerpt,
      ),
      operator_command: parsed.command,
      timeout_ms: 1000,
    )
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "linear_command_start:" <> comment.id,
      bodies: [
        record.LinearCommandSeen(
          comment.id,
          comment.issue_id,
          comment.author.id,
          command_name,
          safe_excerpt,
        ),
        record.LinearCommandStarted(comment.id, comment.issue_id, command_name),
      ],
      failure_event: "linear_command_start_record_failed",
      policy: effects_types.ContinueWith(effects_types.ApplyLinearCommand(
        request,
      )),
    )),
  ])
}

pub fn handle_applied(
  state: transition_types.State,
  comment_id: String,
  issue_id: String,
  _command_name: String,
  result: command.CommandResult,
  message_excerpt: String,
  ack_body: Option(String),
) -> transition_types.Outcome {
  let status = command.status_to_string(result.status)
  let continuation = case ack_body {
    Some(body) ->
      effects_types.ContinueWith(effects_types.EnqueueLinearCommandAck(
        issue_id,
        comment_id,
        body,
      ))
    None -> effects_types.ContinueRegardless
  }
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "linear_command_completion:" <> comment_id,
      bodies: [
        record.LinearCommandCompleted(
          comment_id,
          issue_id,
          status,
          message_excerpt,
        ),
      ],
      failure_event: "linear_command_completion_record_failed",
      policy: continuation,
    )),
  ])
}

pub fn request_ack(
  state: transition_types.State,
  issue_id: String,
  source_comment_id: String,
  body: String,
  outbox_recorded: Bool,
) -> transition_types.Outcome {
  let state =
    remember_pending_ack(
      state,
      issue_id,
      source_comment_id,
      body,
      outbox_recorded,
    )
  case outbox_recorded {
    True -> publish_pending_ack(state, issue_id, source_comment_id, body)
    False ->
      transition_types.Outcome(state: state, effects: [
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "linear_command_ack_outbox:" <> source_comment_id,
          bodies: [ack_outbox_body(issue_id, source_comment_id, body)],
          failure_event: "linear_command_ack_outbox_record_failed",
          policy: effects_types.ContinueWith(
            effects_types.PublishLinearCommandAck(
              issue_id,
              source_comment_id,
              body,
            ),
          ),
        )),
      ])
  }
}

pub fn startup_outbox_replay_effects(
  outbox_to_replay: List(recovery.OutboxReplay),
) -> List(effects_types.Effect) {
  list.flat_map(outbox_to_replay, startup_outbox_replay_entry_effects)
}

fn startup_outbox_replay_entry_effects(
  entry: recovery.OutboxReplay,
) -> List(effects_types.Effect) {
  let recovery.OutboxReplay(outbox_id, issue_id, outbox_kind, _, payload_json) =
    entry
  case outbox.decode_payload(payload_json) {
    Error(error) ->
      startup_outbox_replay_failure_effects(
        outbox_id,
        issue_id,
        outbox_kind,
        error,
      )
    Ok(payload) ->
      case outbox.recovery_replay_error(outbox_kind, payload.kind) {
        Error(error) ->
          startup_outbox_replay_failure_effects(
            outbox_id,
            issue_id,
            outbox_kind,
            error,
          )
        Ok(Nil) -> {
          let source_comment_id = outbox_ack_event_id(outbox_id, payload)
          let issue_id = outbox_ack_issue_id(issue_id, payload)
          [
            effects_types.Log("info", "outbox_replay_enqueued", [
              #("outbox_id", outbox_id),
              #("issue_id", issue_id),
              #("kind", outbox_kind),
            ]),
            effects_types.ReplayLinearCommandAck(
              issue_id,
              source_comment_id,
              payload.body,
            ),
          ]
        }
      }
  }
}

fn outbox_ack_event_id(outbox_id: String, payload: outbox.Payload) -> String {
  case payload.kind, payload.event_id, payload.source_comment_id {
    "remote_command_ack", Some(event_id), _ -> event_id
    _, _, Some(source_comment_id) -> source_comment_id
    _, _, None -> outbox_id
  }
}

fn outbox_ack_issue_id(issue_id: String, payload: outbox.Payload) -> String {
  case payload.kind, payload.task_remote_id {
    "remote_command_ack", Some(task_remote_id) -> task_remote_id
    _, _ -> issue_id
  }
}

fn startup_outbox_replay_failure_effects(
  outbox_id: String,
  issue_id: String,
  outbox_kind: String,
  error: outbox.ReplayError,
) -> List(effects_types.Effect) {
  let error_code = outbox.replay_error_code(error)
  [
    effects_types.Log("warn", "outbox_replay_failed", [
      #("outbox_id", outbox_id),
      #("issue_id", issue_id),
      #("kind", outbox_kind),
      #("error", error_code),
      #("reason", outbox.describe_replay_error(error)),
    ]),
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "outbox_failed:" <> outbox_id,
      bodies: [
        record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code),
      ],
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
  ]
}

pub fn retry_pending_acks(
  state: transition_types.State,
) -> transition_types.Outcome {
  state.pending_linear_command_acks
  |> dict.to_list
  |> list.fold(
    transition_types.Outcome(state: state, effects: []),
    fn(outcome, entry) {
      let #(source_comment_id, pending) = entry
      let transition_types.PendingLinearCommandAck(
        issue_id,
        body,
        outbox_recorded,
      ) = pending
      case
        dict.has_key(
          outcome.state.in_flight_linear_command_acks,
          source_comment_id,
        )
      {
        True -> outcome
        False -> {
          let retried =
            request_ack(
              outcome.state,
              issue_id,
              source_comment_id,
              body,
              outbox_recorded,
            )
          transition_types.Outcome(
            state: retried.state,
            effects: list.append(outcome.effects, retried.effects),
          )
        }
      }
    },
  )
}

pub fn handle_ack_finished(
  state: transition_types.State,
  issue_id: String,
  source_comment_id: String,
  result: Result(Nil, String),
) -> transition_types.Outcome {
  let state =
    transition_types.State(
      ..state,
      in_flight_linear_command_acks: dict.delete(
        state.in_flight_linear_command_acks,
        source_comment_id,
      ),
    )
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "linear_command_ack_failed", [
          #("issue_id", issue_id),
          #("comment_id", source_comment_id),
          #("error", err),
        ]),
      ])
    Ok(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "linear_command_ack_complete:" <> source_comment_id,
          bodies: [
            record.OutboxCompleted(
              source_comment_id,
              issue_id,
              "linear_command_ack",
            ),
            record.LinearCommandAcked(source_comment_id, issue_id),
          ],
          failure_event: "ledger_append_failed",
          policy: effects_types.ContinueWith(
            effects_types.RemoveLinearCommandAck(issue_id, source_comment_id),
          ),
        )),
      ])
  }
}

pub fn handle_apply_continuation(
  state: transition_types.State,
  correlation_id: String,
  request: effects_types.OperatorCommandRequest,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "linear_command_start_record_failed", [
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ApplyOperatorCommand(request),
      ])
  }
}

pub fn handle_enqueue_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  source_comment_id: String,
  body: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "linear_command_completion_record_failed", [
          #("issue_id", issue_id),
          #("comment_id", source_comment_id),
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) -> request_ack(state, issue_id, source_comment_id, body, False)
  }
}

pub fn handle_publish_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  source_comment_id: String,
  body: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "linear_command_ack_outbox_record_failed", [
          #("issue_id", issue_id),
          #("comment_id", source_comment_id),
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) -> {
      let state =
        remember_pending_ack(state, issue_id, source_comment_id, body, True)
      publish_pending_ack(state, issue_id, source_comment_id, body)
    }
  }
}

pub fn handle_remove_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  source_comment_id: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "linear_command_ack_record_failed", [
          #("issue_id", issue_id),
          #("comment_id", source_comment_id),
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) ->
      transition_types.Outcome(
        state: transition_types.State(
          ..state,
          pending_linear_command_acks: dict.delete(
            state.pending_linear_command_acks,
            source_comment_id,
          ),
        ),
        effects: [],
      )
  }
}

fn ack_outbox_body(
  issue_id: String,
  source_comment_id: String,
  body: String,
) -> record.RecordBody {
  record.OutboxPendingV2(
    source_comment_id,
    issue_id,
    "linear_command_ack",
    "linear_command_ack:" <> source_comment_id,
    outbox.linear_command_ack_payload(source_comment_id, body, []),
  )
}

fn remember_pending_ack(
  state: transition_types.State,
  issue_id: String,
  source_comment_id: String,
  body: String,
  outbox_recorded: Bool,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_linear_command_acks: dict.insert(
      state.pending_linear_command_acks,
      source_comment_id,
      transition_types.PendingLinearCommandAck(issue_id, body, outbox_recorded),
    ),
  )
}

fn publish_pending_ack(
  state: transition_types.State,
  issue_id: String,
  source_comment_id: String,
  body: String,
) -> transition_types.Outcome {
  case dict.has_key(state.in_flight_linear_command_acks, source_comment_id) {
    True -> transition_types.Outcome(state: state, effects: [])
    False ->
      transition_types.Outcome(
        state: transition_types.State(
          ..state,
          in_flight_linear_command_acks: dict.insert(
            state.in_flight_linear_command_acks,
            source_comment_id,
            True,
          ),
        ),
        effects: [
          effects_types.PostLinearCommandAck(issue_id, source_comment_id, body),
        ],
      )
  }
}

fn ledger_error_code(err: ledger.LedgerError) -> String {
  case err {
    ledger.Io(_) -> "io"
    ledger.LedgerFfiFailed(_) -> "ledger_ffi_failed"
    ledger.UnsupportedVersion(_) -> "unsupported_version"
    ledger.CorruptRecord(_, _) -> "corrupt_record"
  }
}
