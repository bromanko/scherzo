import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/log
import scherzo/managed_launch/grant as managed_launch_grant
import scherzo/orchestrator/daemon_remote_client
import scherzo/session/hub
import scherzo/work_item_invalidation

pub type Handle =
  daemon_remote_client.Handle

pub type StartError =
  daemon_remote_client.StartError

pub type State {
  State(
    handle: Option(Handle),
    monitor: Option(process.Monitor),
    managed_launch: Option(managed_launch_grant.Grant),
  )
}

pub fn new(managed_launch: Option(managed_launch_grant.Grant)) -> State {
  State(handle: None, monitor: None, managed_launch: managed_launch)
}

pub fn handle(state: State) -> Option(Handle) {
  state.handle
}

pub fn monitor(state: State) -> Option(process.Monitor) {
  state.monitor
}

pub fn monitor_matches(state: State, monitor: process.Monitor) -> Bool {
  state.monitor == Some(monitor)
}

pub fn managed_launch(state: State) -> Option(managed_launch_grant.Grant) {
  state.managed_launch
}

pub fn connected(
  state: State,
  handle: Handle,
  monitor: process.Monitor,
) -> State {
  State(..state, handle: Some(handle), monitor: Some(monitor))
}

pub fn cleared(state: State) -> State {
  State(..state, handle: None, monitor: None)
}

pub fn start_error_fields(error: StartError) -> #(String, String) {
  let daemon_remote_client.StartError(code: code, message: message) = error
  #(code, message)
}

pub opaque type ControlDependencies(message) {
  ControlDependencies(
    apply_operator_command: fn(
      process.Subject(message),
      command.OperatorCommand,
      Int,
    ) -> Result(command.CommandResult, Nil),
    get_remote_dispatch_paused: fn(process.Subject(message), Int) ->
      Result(Bool, Nil),
    execute_query: fn(process.Subject(message), query_types.QueryRequest, Int) ->
      Result(query_types.QueryResponse, query_types.QueryError),
  )
}

pub fn control_dependencies(
  apply_operator_command apply_operator_command: fn(
    process.Subject(message),
    command.OperatorCommand,
    Int,
  ) -> Result(command.CommandResult, Nil),
  get_remote_dispatch_paused get_remote_dispatch_paused: fn(
    process.Subject(message),
    Int,
  ) -> Result(Bool, Nil),
  execute_query execute_query: fn(
    process.Subject(message),
    query_types.QueryRequest,
    Int,
  ) -> Result(query_types.QueryResponse, query_types.QueryError),
) -> ControlDependencies(message) {
  ControlDependencies(
    apply_operator_command: apply_operator_command,
    get_remote_dispatch_paused: get_remote_dispatch_paused,
    execute_query: execute_query,
  )
}

pub fn apply_remote_command(
  daemon_subject: process.Subject(message),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  dependencies: ControlDependencies(message),
) -> Result(command.CommandResult, Nil) {
  dependencies.apply_operator_command(
    daemon_subject,
    operator_command,
    timeout_ms,
  )
}

pub fn read_remote_dispatch_paused(
  daemon_subject: process.Subject(message),
  timeout_ms: Int,
  dependencies: ControlDependencies(message),
) -> Result(Bool, Nil) {
  dependencies.get_remote_dispatch_paused(daemon_subject, timeout_ms)
}

pub fn execute_remote_query(
  daemon_subject: process.Subject(message),
  query: query_types.QueryRequest,
  timeout_ms: Int,
  dependencies: ControlDependencies(message),
) -> Result(query_types.QueryResponse, query_types.QueryError) {
  dependencies.execute_query(daemon_subject, query, timeout_ms)
}

type LateReplyCallOutcome(reply) {
  LateReplyCallReturned(reply)
  LateReplyCallWorkerDown
}

pub fn call_without_late_reply(
  send_request send_request: fn(process.Subject(reply)) -> Nil,
  timeout_ms timeout_ms: Int,
  timeout_value timeout_value: reply,
) -> reply {
  call_without_late_reply_map(
    send_request: send_request,
    timeout_ms: timeout_ms,
    timeout_value: timeout_value,
    map_reply: fn(reply) { reply },
  )
}

pub fn call_result_without_late_reply(
  send_request send_request: fn(process.Subject(reply)) -> Nil,
  timeout_ms timeout_ms: Int,
  timeout_error timeout_error: error,
) -> Result(reply, error) {
  call_without_late_reply_map(
    send_request: send_request,
    timeout_ms: timeout_ms,
    timeout_value: Error(timeout_error),
    map_reply: Ok,
  )
}

fn call_without_late_reply_map(
  send_request send_request: fn(process.Subject(reply)) -> Nil,
  timeout_ms timeout_ms: Int,
  timeout_value timeout_value: mapped,
  map_reply map_reply: fn(reply) -> mapped,
) -> mapped {
  let reply = process.new_subject()
  let worker =
    process.spawn_unlinked(fn() {
      process.send(
        reply,
        run_late_reply_call(send_request, timeout_ms, timeout_value, map_reply),
      )
      Nil
    })
  let monitor = process.monitor(worker)
  let outcome =
    process.new_selector()
    |> process.select_map(reply, LateReplyCallReturned)
    |> process.select_specific_monitor(monitor, fn(_) {
      LateReplyCallWorkerDown
    })
    |> process.selector_receive_forever
  process.demonitor_process(monitor)
  case outcome {
    LateReplyCallReturned(reply) -> reply
    LateReplyCallWorkerDown ->
      case process.receive(reply, within: 0) {
        Ok(reply) -> reply
        Error(Nil) -> timeout_value
      }
  }
}

fn run_late_reply_call(
  send_request: fn(process.Subject(reply)) -> Nil,
  timeout_ms: Int,
  timeout_value: mapped,
  map_reply: fn(reply) -> mapped,
) -> mapped {
  let reply = process.new_subject()
  send_request(reply)
  case process.receive(reply, within: timeout_ms) {
    Ok(reply) -> map_reply(reply)
    Error(Nil) -> timeout_value
  }
}

pub fn start_remote_client(
  effective: config_types.EffectiveConfig,
  managed_launch: Option(managed_launch_grant.Grant),
  event_hub: process.Subject(hub.Message),
  daemon_subject: process.Subject(message),
  managed_auth_rejected: fn(String) -> Nil,
  secrets: List(String),
  logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
  dependencies: ControlDependencies(message),
) -> Result(Handle, StartError) {
  daemon_remote_client.start_with_control_and_managed_auth_rejection(
    effective,
    managed_launch,
    event_hub,
    fn(operator_command, timeout_ms) {
      apply_remote_command(
        daemon_subject,
        operator_command,
        timeout_ms,
        dependencies,
      )
    },
    fn(timeout_ms) {
      read_remote_dispatch_paused(daemon_subject, timeout_ms, dependencies)
    },
    fn(query, timeout_ms) {
      execute_remote_query(daemon_subject, query, timeout_ms, dependencies)
    },
    managed_auth_rejected,
    secrets,
    logger,
  )
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  daemon_remote_client.stop(handle, timeout_ms)
}

pub fn monitor_remote_client(handle: Handle) -> process.Monitor {
  daemon_remote_client.monitor(handle)
}

pub fn kill(handle: Handle) -> Nil {
  daemon_remote_client.kill(handle)
}

pub fn notify_work_item_invalidation(
  handle: Handle,
  event: work_item_invalidation.Event,
) -> Nil {
  daemon_remote_client.notify_work_item_invalidation(handle, event)
}
