import gleam/erlang/process
import scherzo/orchestrator/effect_runner

pub type State {
  State(handle: effect_runner.Handle, monitor: process.Monitor)
}

pub fn new(handle: effect_runner.Handle, monitor: process.Monitor) -> State {
  State(handle: handle, monitor: monitor)
}

pub fn handle(state: State) -> effect_runner.Handle {
  state.handle
}

pub fn monitor(state: State) -> process.Monitor {
  state.monitor
}

pub fn monitor_matches(state: State, monitor: process.Monitor) -> Bool {
  state.monitor == monitor
}

pub fn enqueue(state: State, effect: effect_runner.Effect) -> State {
  effect_runner.enqueue(state.handle, effect)
  state
}
