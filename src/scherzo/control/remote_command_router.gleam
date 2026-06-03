import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/control/command

pub const completed_replay_limit = 256

pub opaque type State {
  State(entries: Dict(String, Entry), completed_order: List(String))
}

type CommandIdentity {
  Decoded(command.OperatorCommand)
  Invalid(command_name: String)
}

type Entry {
  InFlight(CommandIdentity)
  Completed(CommandIdentity, command.CommandResult)
}

pub type Decision {
  StartApply
  DuplicateInFlight
  ReplayCompleted(command.CommandResult)
  Reject(command.CommandResult)
}

pub fn new() -> State {
  State(entries: dict.new(), completed_order: [])
}

pub fn register(
  state: State,
  command_id: String,
  operator_command: command.OperatorCommand,
) -> #(State, Decision) {
  register_decoded(state, command_id, operator_command, None)
}

pub fn register_limited(
  state: State,
  command_id: String,
  operator_command: command.OperatorCommand,
  max_in_flight: Int,
) -> #(State, Decision) {
  register_decoded(state, command_id, operator_command, Some(max_in_flight))
}

pub fn register_rejection(
  state: State,
  command_id: String,
  result: command.CommandResult,
) -> #(State, Decision) {
  case dict.get(state.entries, command_id) {
    Error(Nil) -> #(
      remember_completed(state, command_id, Invalid(result.command), result),
      Reject(result),
    )
    Ok(InFlight(_)) -> #(state, DuplicateInFlight)
    Ok(Completed(_, replay)) -> #(state, ReplayCompleted(replay))
  }
}

pub fn complete(
  state: State,
  command_id: String,
  result: command.CommandResult,
) -> State {
  case dict.get(state.entries, command_id) {
    Ok(InFlight(identity)) ->
      remember_completed(state, command_id, identity, result)
    Ok(Completed(_, _)) | Error(Nil) -> state
  }
}

pub fn forget_in_flight(state: State, command_id: String) -> State {
  case dict.get(state.entries, command_id) {
    Ok(InFlight(_)) ->
      State(..state, entries: dict.delete(state.entries, command_id))
    Ok(Completed(_, _)) | Error(Nil) -> state
  }
}

fn register_decoded(
  state: State,
  command_id: String,
  operator_command: command.OperatorCommand,
  max_in_flight: Option(Int),
) -> #(State, Decision) {
  let identity = Decoded(operator_command)
  case dict.get(state.entries, command_id) {
    Error(Nil) ->
      case can_start_command(state, max_in_flight) {
        True -> #(
          State(
            ..state,
            entries: dict.insert(state.entries, command_id, InFlight(identity)),
          ),
          StartApply,
        )
        False -> {
          let result =
            command.rejected(
              operator_command,
              "remote_command_overloaded",
              Some("remote command service overloaded"),
            )
          #(
            remember_completed(state, command_id, identity, result),
            Reject(result),
          )
        }
      }
    Ok(InFlight(existing)) if existing == identity -> #(
      state,
      DuplicateInFlight,
    )
    Ok(Completed(existing, result)) if existing == identity -> #(
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

fn can_start_command(state: State, max_in_flight: Option(Int)) -> Bool {
  case max_in_flight {
    None -> True
    Some(limit) -> in_flight_count(state) < limit
  }
}

fn in_flight_count(state: State) -> Int {
  state.entries
  |> dict.to_list
  |> list.fold(0, fn(count, entry) {
    let #(_, value) = entry
    case value {
      InFlight(_) -> count + 1
      Completed(_, _) -> count
    }
  })
}

fn remember_completed(
  state: State,
  command_id: String,
  identity: CommandIdentity,
  result: command.CommandResult,
) -> State {
  State(
    entries: dict.insert(state.entries, command_id, Completed(identity, result)),
    completed_order: [command_id, ..state.completed_order],
  )
  |> prune_completed_entries
}

fn prune_completed_entries(state: State) -> State {
  case list.length(state.completed_order) <= completed_replay_limit {
    True -> state
    False -> {
      let retained = list.take(state.completed_order, completed_replay_limit)
      let evicted = list.drop(state.completed_order, completed_replay_limit)
      let entries =
        evicted
        |> list.fold(state.entries, fn(entries, command_id) {
          case dict.get(entries, command_id) {
            Ok(Completed(_, _)) -> dict.delete(entries, command_id)
            Ok(InFlight(_)) | Error(Nil) -> entries
          }
        })
      State(entries: entries, completed_order: retained)
    }
  }
}
