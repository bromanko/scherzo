import gleam/dict
import gleam/list
import gleam/option.{type Option, Some}
import scherzo/control/command
import scherzo/control/linear_parser
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/state/ledger
import scherzo/state/outbox
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/adapter

pub fn handle_submitted(
  state: transition_types.State,
  event: adapter.RemoteCommandEvent,
  parsed: linear_parser.ParsedLinearCommand,
  safe_excerpt: String,
) -> transition_types.Outcome {
  let adapter.RemoteCommandEvent(
    event_id: event_id,
    task: task_ref,
    author_id: author_id,
    ..,
  ) = event
  let command_name = command.command_name(parsed.command)
  let request =
    effects_types.OperatorCommandRequest(
      source: effects_types.RemoteOperatorCommand(
        backend_kind: task_ref.backend_kind,
        event_id: event_id,
        task_remote_id: task_ref.remote_id,
        command_name: command_name,
        excerpt: parsed.excerpt,
      ),
      operator_command: parsed.command,
      timeout_ms: 1000,
    )
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "remote_command_start:" <> event_id,
      bodies: [
        record.RemoteCommandSeen(
          task_ref.backend_kind,
          event_id,
          task_ref.remote_id,
          task_ref.key,
          author_id,
          command_name,
          safe_excerpt,
        ),
        record.RemoteCommandStarted(
          task_ref.backend_kind,
          event_id,
          task_ref.remote_id,
          command_name,
        ),
      ],
      failure_event: "remote_command_start_record_failed",
      policy: effects_types.ContinueWith(effects_types.ApplyRemoteCommand(
        request,
      )),
    )),
  ])
}

pub fn handle_applied(
  state: transition_types.State,
  backend_kind: String,
  event_id: String,
  task_remote_id: String,
  _command_name: String,
  result: command.CommandResult,
  message_excerpt: String,
  ack_body: Option(String),
) -> transition_types.Outcome {
  let status = command.status_to_string(result.status)
  let outbox_kind = "remote_command_ack"
  let continuation = case ack_body {
    Some(body) ->
      effects_types.ContinueWith(effects_types.EnqueueRemoteCommandAck(
        backend_kind,
        task_remote_id,
        event_id,
        body,
        outbox_kind,
      ))
    _ -> effects_types.ContinueRegardless
  }
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "remote_command_completion:" <> event_id,
      bodies: [
        record.RemoteCommandCompleted(
          backend_kind,
          event_id,
          task_remote_id,
          status,
          message_excerpt,
        ),
      ],
      failure_event: "remote_command_completion_record_failed",
      policy: continuation,
    )),
  ])
}

pub fn request_ack(
  state: transition_types.State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_recorded: Bool,
  outbox_kind: String,
) -> transition_types.Outcome {
  let state =
    remember_pending_ack(
      state,
      backend_kind,
      task_remote_id,
      event_id,
      body,
      outbox_recorded,
      outbox_kind,
    )
  case outbox_recorded {
    True ->
      publish_pending_ack(
        state,
        backend_kind,
        task_remote_id,
        event_id,
        body,
        outbox_kind,
      )
    False ->
      transition_types.Outcome(state: state, effects: [
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "remote_command_ack_outbox:" <> event_id,
          bodies: [
            ack_outbox_body(
              backend_kind,
              task_remote_id,
              event_id,
              body,
              outbox_kind,
            ),
          ],
          failure_event: "remote_command_ack_outbox_record_failed",
          policy: effects_types.ContinueWith(
            effects_types.PublishRemoteCommandAck(
              backend_kind,
              task_remote_id,
              event_id,
              body,
              outbox_kind,
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
          let event_id = outbox_ack_event_id(outbox_id, payload)
          let task_remote_id = outbox_ack_issue_id(issue_id, payload)
          let backend_kind = outbox_ack_backend_kind(payload)
          [
            effects_types.Log("info", "outbox_replay_enqueued", [
              #("outbox_id", outbox_id),
              #("issue_id", task_remote_id),
              #("kind", outbox_kind),
            ]),
            effects_types.ReplayRemoteCommandAck(
              backend_kind,
              task_remote_id,
              event_id,
              payload.body,
              outbox_kind,
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
    _, _, _ -> outbox_id
  }
}

fn outbox_ack_issue_id(issue_id: String, payload: outbox.Payload) -> String {
  case payload.kind, payload.task_remote_id {
    "remote_command_ack", Some(task_remote_id) -> task_remote_id
    _, _ -> issue_id
  }
}

fn outbox_ack_backend_kind(payload: outbox.Payload) -> String {
  case payload.kind, payload.backend_kind {
    "remote_command_ack", Some(backend_kind) -> backend_kind
    _, _ -> "linear"
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
      let #(event_id, pending) = entry
      let transition_types.PendingLinearCommandAck(
        backend_kind,
        task_remote_id,
        body,
        outbox_recorded,
        outbox_kind,
      ) = pending
      case dict.has_key(outcome.state.in_flight_linear_command_acks, event_id) {
        True -> outcome
        False -> {
          let retried =
            request_ack(
              outcome.state,
              backend_kind,
              task_remote_id,
              event_id,
              body,
              outbox_recorded,
              outbox_kind,
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
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  outbox_kind: String,
  result: Result(Nil, String),
) -> transition_types.Outcome {
  let state =
    transition_types.State(
      ..state,
      in_flight_linear_command_acks: dict.delete(
        state.in_flight_linear_command_acks,
        event_id,
      ),
    )
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "remote_command_ack_failed", [
          #("task_remote_id", task_remote_id),
          #("event_id", event_id),
          #("error", err),
        ]),
      ])
    Ok(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "remote_command_ack_complete:" <> event_id,
          bodies: [
            record.OutboxCompleted(event_id, task_remote_id, outbox_kind),
            record.RemoteCommandAcked(backend_kind, event_id, task_remote_id),
          ],
          failure_event: "ledger_append_failed",
          policy: effects_types.ContinueWith(
            effects_types.RemoveRemoteCommandAck(task_remote_id, event_id),
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
        effects_types.Log("warn", "remote_command_start_record_failed", [
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
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "remote_command_completion_record_failed", [
          #("task_remote_id", task_remote_id),
          #("event_id", event_id),
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) ->
      request_ack(
        state,
        backend_kind,
        task_remote_id,
        event_id,
        body,
        False,
        outbox_kind,
      )
  }
}

pub fn handle_publish_continuation(
  state: transition_types.State,
  correlation_id: String,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "remote_command_ack_outbox_record_failed", [
          #("task_remote_id", task_remote_id),
          #("event_id", event_id),
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) -> {
      let state =
        remember_pending_ack(
          state,
          backend_kind,
          task_remote_id,
          event_id,
          body,
          True,
          outbox_kind,
        )
      publish_pending_ack(
        state,
        backend_kind,
        task_remote_id,
        event_id,
        body,
        outbox_kind,
      )
    }
  }
}

pub fn handle_remove_continuation(
  state: transition_types.State,
  correlation_id: String,
  task_remote_id: String,
  event_id: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "remote_command_ack_record_failed", [
          #("task_remote_id", task_remote_id),
          #("event_id", event_id),
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
            event_id,
          ),
        ),
        effects: [],
      )
  }
}

fn ack_outbox_body(
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
) -> record.RecordBody {
  case outbox_kind {
    "linear_command_ack" ->
      record.OutboxPendingV2(
        event_id,
        task_remote_id,
        outbox_kind,
        "linear_command_ack:" <> event_id,
        outbox.linear_command_ack_payload(event_id, body, []),
      )
    _ ->
      record.OutboxPendingV2(
        event_id,
        task_remote_id,
        "remote_command_ack",
        "remote_command_ack:" <> backend_kind <> ":" <> event_id,
        outbox.remote_command_ack_payload(
          backend_kind,
          event_id,
          task_remote_id,
          body,
          [],
        ),
      )
  }
}

fn remember_pending_ack(
  state: transition_types.State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_recorded: Bool,
  outbox_kind: String,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_linear_command_acks: dict.insert(
      state.pending_linear_command_acks,
      event_id,
      transition_types.PendingLinearCommandAck(
        backend_kind,
        task_remote_id,
        body,
        outbox_recorded,
        outbox_kind,
      ),
    ),
  )
}

fn publish_pending_ack(
  state: transition_types.State,
  backend_kind: String,
  task_remote_id: String,
  event_id: String,
  body: String,
  outbox_kind: String,
) -> transition_types.Outcome {
  case dict.has_key(state.in_flight_linear_command_acks, event_id) {
    True -> transition_types.Outcome(state: state, effects: [])
    False ->
      transition_types.Outcome(
        state: transition_types.State(
          ..state,
          in_flight_linear_command_acks: dict.insert(
            state.in_flight_linear_command_acks,
            event_id,
            True,
          ),
        ),
        effects: [
          effects_types.PostRemoteCommandAck(
            backend_kind,
            task_remote_id,
            event_id,
            body,
            outbox_kind,
          ),
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
