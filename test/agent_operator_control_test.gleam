import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/operator_control
import scherzo/agent/worker_command
import scherzo/control/command

pub fn prompt_too_large_is_rejected_test() {
  let reply = process.new_subject()
  let message =
    string.repeat("x", times: worker_command.max_operator_prompt_chars + 1)
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.initial_state(),
      worker_command.QueuePrompt(message, reply),
    )
  assert state.prompt_queue == []
  let assert [
    operator_control.Reply(
      _,
      worker_command.Rejected(
        "prompt_too_large",
        Some("operator prompt is too large"),
      ),
    ),
  ] = effects
}

pub fn prompt_queue_full_is_rejected_test() {
  let reply = process.new_subject()
  let state =
    operator_control.from_parts(
      ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"],
      False,
      None,
    )
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      state,
      worker_command.QueuePrompt("extra", reply),
    )
  assert state.prompt_queue
    == ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
  let assert [
    operator_control.Reply(
      _,
      worker_command.Rejected("prompt_queue_full", Some("prompt queue is full")),
    ),
  ] = effects
}

pub fn prompt_queued_between_turns_gets_applied_reply_test() {
  let reply = process.new_subject()
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.BetweenTurns,
      operator_control.initial_state(),
      worker_command.QueuePrompt("follow up", reply),
    )
  assert state.prompt_queue == ["follow up"]
  let assert [
    operator_control.EmitPromptQueued("follow up"),
    operator_control.Reply(
      _,
      worker_command.Applied(Some("prompt accepted for next turn")),
    ),
  ] = effects
}

pub fn prompt_queued_during_active_turn_gets_queued_reply_test() {
  let reply = process.new_subject()
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.initial_state(),
      worker_command.QueuePrompt("follow up", reply),
    )
  assert state.prompt_queue == ["follow up"]
  let assert [
    operator_control.EmitPromptQueued("follow up"),
    operator_control.Reply(
      _,
      worker_command.Queued(Some("prompt queued for next turn")),
    ),
  ] = effects
}

pub fn stop_between_turns_and_active_turn_use_existing_replies_test() {
  let between_reply = process.new_subject()
  let #(between_state, between_effects) =
    operator_control.handle_command(
      operator_control.BetweenTurns,
      operator_control.initial_state(),
      worker_command.StopAfterCurrentTurn(between_reply),
    )
  assert between_state.stop_after_turn
  let assert [
    operator_control.Reply(
      _,
      worker_command.Applied(Some("stopped before next turn")),
    ),
    operator_control.StopBeforeNextTurn(_),
  ] = between_effects

  let active_reply = process.new_subject()
  let #(active_state, active_effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.initial_state(),
      worker_command.StopAfterCurrentTurn(active_reply),
    )
  assert active_state.stop_after_turn
  let assert [
    operator_control.Reply(
      _,
      worker_command.Queued(Some("stop requested after current turn")),
    ),
  ] = active_effects
}

pub fn ui_response_without_pending_request_is_not_allowed_test() {
  let reply = process.new_subject()
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.initial_state(),
      worker_command.RespondToUi("ui-1", command.UiCancel, reply),
    )
  assert state.pending_ui == None
  let assert [
    operator_control.Reply(
      _,
      worker_command.NotAllowed(
        "ui_request_not_pending",
        Some("no operator UI request is pending"),
      ),
    ),
  ] = effects
}

pub fn ui_response_wrong_request_id_is_rejected_test() {
  let reply = process.new_subject()
  let pending =
    operator_control.PendingUi(
      request_id: "ui-1",
      method: "confirm",
      message: Some("continue?"),
      created_at_ms: 10,
      deadline_ms: 20,
    )
  let state = operator_control.from_parts([], False, Some(pending))
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      state,
      worker_command.RespondToUi("ui-2", command.UiCancel, reply),
    )
  let assert Some(ui) = state.pending_ui
  assert ui.request_id == "ui-1"
  let assert [
    operator_control.Reply(
      _,
      worker_command.Rejected(
        "ui_request_not_pending",
        Some("that UI request is not pending"),
      ),
    ),
  ] = effects
}

pub fn ui_cancel_and_value_responses_return_send_effects_test() {
  let cancel_reply = process.new_subject()
  let pending =
    operator_control.PendingUi(
      request_id: "ui-1",
      method: "confirm",
      message: Some("continue?"),
      created_at_ms: 10,
      deadline_ms: 20,
    )
  let #(cancel_state, cancel_effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.from_parts([], False, Some(pending)),
      worker_command.RespondToUi("ui-1", command.UiCancel, cancel_reply),
    )
  assert cancel_state.pending_ui == None
  let assert [operator_control.SendUiCancel(_, "ui-1")] = cancel_effects

  let value_reply = process.new_subject()
  let pending =
    operator_control.PendingUi(
      request_id: "ui-2",
      method: "input",
      message: Some("value?"),
      created_at_ms: 10,
      deadline_ms: 20,
    )
  let #(value_state, value_effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.from_parts([], False, Some(pending)),
      worker_command.RespondToUi("ui-2", command.UiValue("ok"), value_reply),
    )
  assert value_state.pending_ui == None
  let assert [operator_control.SendUiValue(_, "ui-2", "ok")] = value_effects
}

pub fn ui_response_too_large_is_rejected_before_pending_lookup_test() {
  let reply = process.new_subject()
  let value =
    string.repeat("x", times: worker_command.max_operator_ui_value_chars + 1)
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.initial_state(),
      worker_command.RespondToUi("ui-1", command.UiValue(value), reply),
    )
  assert state.pending_ui == None
  let assert [
    operator_control.Reply(
      _,
      worker_command.Rejected(
        "ui_response_too_large",
        Some("operator UI response value is too large"),
      ),
    ),
  ] = effects
}

pub fn abort_returns_abort_requested_effect_test() {
  let reply = process.new_subject()
  let #(state, effects) =
    operator_control.handle_command(
      operator_control.ActiveTurn,
      operator_control.initial_state(),
      worker_command.Abort(reply),
    )
  assert state.prompt_queue == []
  let assert [operator_control.AbortRequested(_)] = effects
}
