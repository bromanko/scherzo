import gleam/int
import gleam/string
import simplifile

type RawServer

pub opaque type Server {
  Server(raw: RawServer)
}

pub fn start(credential: String, transcript_path: String) -> Server {
  let assert Ok(server) = start_fake_ui_server(credential, transcript_path)
  Server(raw: server)
}

pub fn stop(server: Server) -> Nil {
  let Server(raw:) = server
  stop_fake_ui_server(raw)
}

pub fn server_url(server: Server) -> String {
  let Server(raw:) = server
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
) -> Result(RawServer, String)

@external(erlang, "scherzo_remote_websocket_ffi", "stop_fake_ui_server")
fn stop_fake_ui_server(server: RawServer) -> Nil

@external(erlang, "scherzo_remote_websocket_ffi", "fake_ui_server_port")
fn fake_ui_server_port(server: RawServer) -> Int

@external(erlang, "timer", "sleep")
fn process_sleep(ms: Int) -> Nil
