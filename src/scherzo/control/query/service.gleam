import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import scherzo/control/query/types

pub type Settings {
  Settings(max_concurrent: Int, max_queued: Int, timeout_ms: Int)
}

pub type Backend {
  Backend(
    run: fn(types.QueryRequest) -> Result(types.QueryResponse, types.QueryError),
  )
}

pub type StartError {
  StartError(code: String, message: String)
}

pub opaque type Handle {
  Handle(
    subject: process.Subject(Message),
    pid: process.Pid,
    reply_timeout_ms: Int,
  )
}

type Message {
  Enqueue(
    types.QueryRequest,
    process.Subject(Result(types.QueryResponse, types.QueryError)),
  )
  WorkerFinished(Int, Result(types.QueryResponse, types.QueryError))
  WorkerTimedOut(Int)
  Shutdown(process.Subject(Nil))
}

type PendingRequest {
  PendingRequest(
    request: types.QueryRequest,
    reply: process.Subject(Result(types.QueryResponse, types.QueryError)),
  )
}

type RunningRequest {
  RunningRequest(
    reply: process.Subject(Result(types.QueryResponse, types.QueryError)),
    worker_pid: process.Pid,
    timer: process.Timer,
  )
}

type State {
  State(
    subject: process.Subject(Message),
    settings: Settings,
    backend: Backend,
    next_request_id: Int,
    queued: List(PendingRequest),
    running: Dict(Int, RunningRequest),
    shutting_down: Bool,
  )
}

pub fn default_settings() -> Settings {
  Settings(max_concurrent: 2, max_queued: 8, timeout_ms: 250)
}

pub fn start(
  settings: Settings,
  backend: Backend,
) -> Result(Handle, StartError) {
  case valid_settings(settings) {
    Error(err) -> Error(err)
    Ok(Nil) -> {
      let ready = process.new_subject()
      let pid =
        process.spawn_unlinked(fn() {
          let subject = process.new_subject()
          process.send(ready, subject)
          loop(State(
            subject: subject,
            settings: settings,
            backend: backend,
            next_request_id: 1,
            queued: [],
            running: dict.new(),
            shutting_down: False,
          ))
        })
      case process.receive(ready, within: 1000) {
        Ok(subject) -> Ok(Handle(subject, pid, settings.timeout_ms + 100))
        Error(Nil) -> {
          process.kill(pid)
          Error(StartError(
            "query_service_start_timeout",
            "query service failed to start",
          ))
        }
      }
    }
  }
}

pub fn query(
  handle: Handle,
  request: types.QueryRequest,
) -> Result(types.QueryResponse, types.QueryError) {
  let Handle(subject, _, reply_timeout_ms) = handle
  let reply = process.new_subject()
  process.send(subject, Enqueue(request, reply))
  case process.receive(reply, within: reply_timeout_ms) {
    Ok(result) -> result
    Error(Nil) ->
      Error(types.QueryError(types.QueryShutdown, "query service unavailable"))
  }
}

pub fn stop(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  let Handle(subject, _, _) = handle
  let reply = process.new_subject()
  process.send(subject, Shutdown(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn kill(handle: Handle) -> Nil {
  let Handle(_, pid, _) = handle
  process.kill(pid)
}

fn valid_settings(settings: Settings) -> Result(Nil, StartError) {
  case settings.max_concurrent < 1 {
    True ->
      Error(StartError(
        "invalid_query_service_settings",
        "max_concurrent must be positive",
      ))
    False ->
      case settings.max_queued < 0 {
        True ->
          Error(StartError(
            "invalid_query_service_settings",
            "max_queued must be non-negative",
          ))
        False ->
          case settings.timeout_ms < 1 {
            True ->
              Error(StartError(
                "invalid_query_service_settings",
                "timeout_ms must be positive",
              ))
            False -> Ok(Nil)
          }
      }
  }
}

fn loop(state: State) -> Nil {
  case process.receive_forever(state.subject) {
    Enqueue(request, reply) -> loop(handle_enqueue(state, request, reply))
    WorkerFinished(request_id, result) ->
      loop(handle_worker_finished(state, request_id, result))
    WorkerTimedOut(request_id) ->
      loop(handle_worker_timed_out(state, request_id))
    Shutdown(reply) -> {
      shutdown(state)
      process.send(reply, Nil)
      Nil
    }
  }
}

fn handle_enqueue(
  state: State,
  request: types.QueryRequest,
  reply: process.Subject(Result(types.QueryResponse, types.QueryError)),
) -> State {
  case state.shutting_down {
    True -> {
      process.send(reply, shutdown_error())
      state
    }
    False ->
      case dict.size(state.running) < state.settings.max_concurrent {
        True -> start_request(state, PendingRequest(request, reply))
        False ->
          case list.length(state.queued) >= state.settings.max_queued {
            True -> {
              process.send(reply, overload_error())
              state
            }
            False ->
              State(
                ..state,
                queued: list.append(state.queued, [
                  PendingRequest(request, reply),
                ]),
              )
          }
      }
  }
}

fn start_request(state: State, pending: PendingRequest) -> State {
  let PendingRequest(request, reply) = pending
  let request_id = state.next_request_id
  let worker_pid =
    process.spawn_unlinked(fn() {
      let Backend(run) = state.backend
      let _ =
        process.send(state.subject, WorkerFinished(request_id, run(request)))
      Nil
    })
  let timer =
    process.send_after(
      state.subject,
      state.settings.timeout_ms,
      WorkerTimedOut(request_id),
    )
  State(
    ..state,
    next_request_id: request_id + 1,
    running: dict.insert(
      state.running,
      request_id,
      RunningRequest(reply, worker_pid, timer),
    ),
  )
}

fn handle_worker_finished(
  state: State,
  request_id: Int,
  result: Result(types.QueryResponse, types.QueryError),
) -> State {
  case dict.get(state.running, request_id) {
    Error(Nil) -> state
    Ok(running_request) -> {
      let RunningRequest(reply, _, _) = running_request
      cancel_running_timer(running_request)
      process.send(reply, result)
      State(..state, running: dict.delete(state.running, request_id))
      |> start_next_queued_if_possible
    }
  }
}

fn handle_worker_timed_out(state: State, request_id: Int) -> State {
  case dict.get(state.running, request_id) {
    Error(Nil) -> state
    Ok(running_request) -> {
      let RunningRequest(reply, worker_pid, _) = running_request
      process.kill(worker_pid)
      process.send(reply, timeout_error())
      State(..state, running: dict.delete(state.running, request_id))
      |> start_next_queued_if_possible
    }
  }
}

fn start_next_queued_if_possible(state: State) -> State {
  case state.shutting_down {
    True -> state
    False ->
      case
        dict.size(state.running) < state.settings.max_concurrent,
        state.queued
      {
        True, [next, ..rest] ->
          start_request(State(..state, queued: rest), next)
        _, _ -> state
      }
  }
}

fn shutdown(state: State) -> Nil {
  let shutdown_result = shutdown_error()
  state.running
  |> dict.values
  |> list.each(fn(running_request) {
    cancel_running_timer(running_request)
    process.kill(running_request.worker_pid)
    process.send(running_request.reply, shutdown_result)
  })
  state.queued
  |> list.each(fn(pending) {
    let PendingRequest(_, reply) = pending
    process.send(reply, shutdown_result)
  })
}

fn cancel_running_timer(running_request: RunningRequest) -> Nil {
  let RunningRequest(_, _, timer) = running_request
  let _ = process.cancel_timer(timer)
  Nil
}

fn timeout_error() -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(types.QueryTimeout, "query timed out"))
}

fn overload_error() -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(types.QueryOverloaded, "query service overloaded"))
}

fn shutdown_error() -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(types.QueryShutdown, "query service shutting down"))
}
