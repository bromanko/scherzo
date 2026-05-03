import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/worker_command
import scherzo/control/command

pub const max_prompt_queue = 10

pub type Mode {
  BetweenTurns
  ActiveTurn
}

pub type PendingUi {
  PendingUi(
    request_id: String,
    method: String,
    message: Option(String),
    created_at_ms: Int,
    deadline_ms: Int,
  )
}

pub type State {
  State(
    prompt_queue: List(String),
    stop_after_turn: Bool,
    pending_ui: Option(PendingUi),
  )
}

pub type Effect {
  Reply(process.Subject(worker_command.Reply), worker_command.Reply)
  EmitPromptQueued(message: String)
  AbortRequested(reply: process.Subject(worker_command.Reply))
  StopBeforeNextTurn(reply: process.Subject(worker_command.Reply))
  SendUiCancel(reply: process.Subject(worker_command.Reply), request_id: String)
  SendUiValue(
    reply: process.Subject(worker_command.Reply),
    request_id: String,
    value: String,
  )
}

pub fn initial_state() -> State {
  State(prompt_queue: [], stop_after_turn: False, pending_ui: None)
}

pub fn from_parts(
  prompt_queue: List(String),
  stop_after_turn: Bool,
  pending_ui: Option(PendingUi),
) -> State {
  State(
    prompt_queue: prompt_queue,
    stop_after_turn: stop_after_turn,
    pending_ui: pending_ui,
  )
}

pub fn handle_command(
  mode: Mode,
  state: State,
  command: worker_command.Command,
) -> #(State, List(Effect)) {
  case command {
    worker_command.Abort(reply) -> #(state, [AbortRequested(reply)])
    worker_command.StopAfterCurrentTurn(reply) ->
      handle_stop(mode, state, reply)
    worker_command.QueuePrompt(message, reply) ->
      handle_queue_prompt(mode, state, message, reply)
    worker_command.RespondToUi(request_id, response, reply) ->
      handle_ui_response(state, request_id, response, reply)
  }
}

pub fn prompt_too_large(message: String) -> Bool {
  string.length(message) > worker_command.max_operator_prompt_chars
}

pub fn prompt_queue_full(prompt_queue: List(String)) -> Bool {
  list.length(prompt_queue) >= max_prompt_queue
}

pub fn ui_response_too_large(response: command.UiResponse) -> Bool {
  case response {
    command.UiCancel -> False
    command.UiValue(value) ->
      string.length(value) > worker_command.max_operator_ui_value_chars
  }
}

fn handle_stop(
  mode: Mode,
  state: State,
  reply: process.Subject(worker_command.Reply),
) -> #(State, List(Effect)) {
  case mode {
    BetweenTurns -> #(State(..state, stop_after_turn: True), [
      Reply(reply, worker_command.Applied(Some("stopped before next turn"))),
      StopBeforeNextTurn(reply),
    ])
    ActiveTurn -> #(State(..state, stop_after_turn: True), [
      Reply(
        reply,
        worker_command.Queued(Some("stop requested after current turn")),
      ),
    ])
  }
}

fn handle_queue_prompt(
  mode: Mode,
  state: State,
  message: String,
  reply: process.Subject(worker_command.Reply),
) -> #(State, List(Effect)) {
  case prompt_too_large(message) {
    True -> #(state, [
      Reply(
        reply,
        worker_command.Rejected(
          "prompt_too_large",
          Some("operator prompt is too large"),
        ),
      ),
    ])
    False ->
      case prompt_queue_full(state.prompt_queue) {
        True -> #(state, [
          Reply(
            reply,
            worker_command.Rejected(
              "prompt_queue_full",
              Some("prompt queue is full"),
            ),
          ),
        ])
        False -> {
          let prompt_queue = list.append(state.prompt_queue, [message])
          let reply_value = case mode {
            BetweenTurns ->
              worker_command.Applied(Some("prompt accepted for next turn"))
            ActiveTurn ->
              worker_command.Queued(Some("prompt queued for next turn"))
          }
          #(State(..state, prompt_queue: prompt_queue), [
            EmitPromptQueued(message),
            Reply(reply, reply_value),
          ])
        }
      }
  }
}

fn handle_ui_response(
  state: State,
  request_id: String,
  response: command.UiResponse,
  reply: process.Subject(worker_command.Reply),
) -> #(State, List(Effect)) {
  case ui_response_too_large(response) {
    True -> #(state, [
      Reply(
        reply,
        worker_command.Rejected(
          "ui_response_too_large",
          Some("operator UI response value is too large"),
        ),
      ),
    ])
    False ->
      case state.pending_ui {
        None -> #(state, [
          Reply(
            reply,
            worker_command.NotAllowed(
              "ui_request_not_pending",
              Some("no operator UI request is pending"),
            ),
          ),
        ])
        Some(ui) ->
          case ui.request_id == request_id {
            False -> #(state, [
              Reply(
                reply,
                worker_command.Rejected(
                  "ui_request_not_pending",
                  Some("that UI request is not pending"),
                ),
              ),
            ])
            True -> {
              let state = State(..state, pending_ui: None)
              case response {
                command.UiCancel -> #(state, [SendUiCancel(reply, request_id)])
                command.UiValue(value) -> #(state, [
                  SendUiValue(reply, request_id, value),
                ])
              }
            }
          }
      }
  }
}
