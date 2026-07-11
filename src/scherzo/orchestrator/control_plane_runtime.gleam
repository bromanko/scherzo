import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import scherzo/control/query/service as query_service
import scherzo/control/server as control_server

pub type Handle {
  NoControlServer
  RealControlServer(control_server.Server)
}

pub type StartupControlPlane {
  StartupControlPlane(handle: Handle, control_file_path: Option(String))
}

pub type State {
  State(
    handle: Handle,
    monitor: Option(process.Monitor),
    control_file_path: Option(String),
    query_service: query_service.Handle,
  )
}

pub fn new(
  handle: Handle,
  monitor: Option(process.Monitor),
  control_file_path: Option(String),
  query_service: query_service.Handle,
) -> State {
  State(
    handle: handle,
    monitor: monitor,
    control_file_path: control_file_path,
    query_service: query_service,
  )
}

pub fn monitor_handle(handle: Handle) -> Option(process.Monitor) {
  case handle {
    NoControlServer -> None
    RealControlServer(server) -> Some(control_server.monitor(server))
  }
}

pub fn handle(state: State) -> Handle {
  state.handle
}

pub fn monitor(state: State) -> Option(process.Monitor) {
  state.monitor
}

pub fn monitor_matches(state: State, monitor: process.Monitor) -> Bool {
  state.monitor == Some(monitor)
}

pub fn control_file_path(state: State) -> Option(String) {
  state.control_file_path
}

pub fn query_service(state: State) -> query_service.Handle {
  state.query_service
}

pub fn cleared(state: State) -> State {
  State(
    ..state,
    handle: NoControlServer,
    monitor: None,
    control_file_path: None,
  )
}
