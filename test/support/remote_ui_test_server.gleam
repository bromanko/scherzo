import gleam/erlang/process
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/int
import gleam/result
import gleam/string
import simplifile

type RawServer

pub opaque type Server {
  Server(raw: RawServer, status_updates: process.Subject(String))
}

pub fn start(credential: String, transcript_path: String) -> Server {
  let status_updates = process.new_subject()
  let assert Ok(server) =
    start_fake_ui_server(credential, transcript_path, status_updates)
  Server(raw: server, status_updates: status_updates)
}

pub fn stop(server: Server) -> Nil {
  let Server(raw:, ..) = server
  stop_fake_ui_server(raw)
}

pub fn server_url(server: Server) -> String {
  let Server(raw:, ..) = server
  "http://127.0.0.1:" <> int.to_string(fake_ui_server_port(raw))
}

pub fn wait_for_contains(
  path: String,
  needle: String,
  attempts: Int,
) -> String {
  let contents = transcript(path)
  case string.contains(contents, needle) || attempts <= 0 {
    True -> contents
    False -> {
      process_sleep(20)
      wait_for_contains(path, needle, attempts - 1)
    }
  }
}

pub fn wait_for_daemons_status_contains(
  server: Server,
  needle: String,
  attempts: Int,
) -> String {
  let Server(status_updates:, ..) = server
  let _ = wait_for_daemons_status_update(status_updates, needle, attempts, "")
  case daemons_status(server) {
    Ok(body) -> body
    Error(Nil) -> ""
  }
}

fn wait_for_daemons_status_update(
  updates: process.Subject(String),
  needle: String,
  attempts: Int,
  latest: String,
) -> String {
  case string.contains(latest, needle) || attempts <= 0 {
    True -> latest
    False ->
      case process.receive(updates, within: 200) {
        Ok(next) ->
          wait_for_daemons_status_update(updates, needle, attempts - 1, next)
        Error(_) ->
          wait_for_daemons_status_update(updates, needle, attempts - 1, latest)
      }
  }
}

pub fn daemons_status(server: Server) -> Result(String, Nil) {
  ensure_http_client_started()
  use request <- result.try(http_request.to(
    server_url(server) <> "/api/daemons",
  ))
  let request =
    request
    |> http_request.set_method(http.Get)
    |> http_request.set_header("accept", "application/json")
  case httpc.configure() |> httpc.timeout(100) |> httpc.dispatch(request) {
    Ok(response) ->
      case response.status {
        200 -> Ok(response.body)
        _ -> Error(Nil)
      }
    Error(_) -> Error(Nil)
  }
}

pub fn transcript(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> ""
  }
}

@external(erlang, "scherzo_remote_websocket_ffi", "start_fake_ui_server")
fn start_fake_ui_server(
  credential: String,
  transcript_path: String,
  status_updates: process.Subject(String),
) -> Result(RawServer, String)

@external(erlang, "scherzo_remote_websocket_ffi", "stop_fake_ui_server")
fn stop_fake_ui_server(server: RawServer) -> Nil

@external(erlang, "scherzo_remote_websocket_ffi", "fake_ui_server_port")
fn fake_ui_server_port(server: RawServer) -> Int

@external(erlang, "scherzo_http_client_ffi", "ensure_started")
fn ensure_http_client_started() -> Nil

@external(erlang, "timer", "sleep")
fn process_sleep(ms: Int) -> Nil
