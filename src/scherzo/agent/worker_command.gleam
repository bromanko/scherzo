import gleam/erlang/process
import gleam/option.{type Option}
import scherzo/control/command

pub const max_operator_prompt_chars = 65_536

pub const max_operator_ui_value_chars = 65_536

pub type Reply {
  Applied(message: Option(String))
  Queued(message: Option(String))
  Rejected(reason: String, message: Option(String))
  NotFound(message: Option(String))
  NotAllowed(reason: String, message: Option(String))
}

pub type Command {
  Abort(reply: process.Subject(Reply))
  StopAfterCurrentTurn(reply: process.Subject(Reply))
  QueuePrompt(message: String, reply: process.Subject(Reply))
  RespondToUi(
    request_id: String,
    response: command.UiResponse,
    reply: process.Subject(Reply),
  )
}
