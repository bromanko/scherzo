import gleam/dict.{type Dict}
import gleam/option.{Some}
import scherzo/control/command

pub opaque type State {
  State(entries: Dict(String, Entry))
}

type Entry {
  InFlight(command.OperatorCommand)
  Completed(command.OperatorCommand, command.CommandResult)
}

pub type Decision {
  StartApply
  DuplicateInFlight
  ReplayCompleted(command.CommandResult)
  Reject(command.CommandResult)
}

pub fn new() -> State {
  State(entries: dict.new())
}

pub fn register(
  state: State,
  command_id: String,
  operator_command: command.OperatorCommand,
) -> #(State, Decision) {
  case supported(operator_command) {
    False -> #(
      state,
      Reject(command.rejected(
        operator_command,
        "unsupported_remote_command",
        Some("remote control currently supports only pause and resume"),
      )),
    )
    True ->
      case dict.get(state.entries, command_id) {
        Error(Nil) -> #(
          State(dict.insert(
            state.entries,
            command_id,
            InFlight(operator_command),
          )),
          StartApply,
        )
        Ok(InFlight(existing)) if existing == operator_command -> #(
          state,
          DuplicateInFlight,
        )
        Ok(Completed(existing, result)) if existing == operator_command -> #(
          state,
          ReplayCompleted(result),
        )
        Ok(InFlight(_)) | Ok(Completed(_, _)) -> #(
          state,
          Reject(command.rejected(
            operator_command,
            "remote_command_id_conflict",
            Some("command id already used for a different command"),
          )),
        )
      }
  }
}

pub fn complete(
  state: State,
  command_id: String,
  result: command.CommandResult,
) -> State {
  case dict.get(state.entries, command_id) {
    Ok(InFlight(operator_command)) ->
      State(dict.insert(
        state.entries,
        command_id,
        Completed(operator_command, result),
      ))
    Ok(Completed(_, _)) | Error(Nil) -> state
  }
}

pub fn forget_in_flight(state: State, command_id: String) -> State {
  case dict.get(state.entries, command_id) {
    Ok(InFlight(_)) -> State(dict.delete(state.entries, command_id))
    Ok(Completed(_, _)) | Error(Nil) -> state
  }
}

fn supported(operator_command: command.OperatorCommand) -> Bool {
  case operator_command {
    command.PauseDispatch | command.ResumeDispatch -> True
    _ -> False
  }
}
