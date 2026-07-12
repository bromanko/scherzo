import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option}

pub type State {
  State(active_operations: Dict(String, Bool))
}

pub fn new() -> State {
  State(active_operations: dict.new())
}

pub fn is_active(state: State, operation_id: String) -> Bool {
  case dict.get(state.active_operations, operation_id) {
    Ok(True) -> True
    _ -> False
  }
}

pub fn begin(state: State, operation_id: String) -> State {
  State(active_operations: dict.insert(
    state.active_operations,
    operation_id,
    True,
  ))
}

pub fn finish(state: State, operation_id: String) -> State {
  State(active_operations: dict.delete(state.active_operations, operation_id))
}

pub fn retry_step_operation_id(
  run_id: String,
  step_id: Option(String),
  now_ms: Int,
) -> String {
  "retry-step:"
  <> run_id
  <> ":"
  <> option.unwrap(step_id, "auto")
  <> ":"
  <> int.to_string(now_ms)
}
