import gleam/dict.{type Dict}

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
