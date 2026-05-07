import gleam/erlang/process
import gleam/string
import simplifile

pub type Listener

pub type Socket

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn control_ffi_loopback_send_receive_and_idempotent_close_test() {
  let assert Ok(listener) = listen("127.0.0.1", 0)
  let port = bound_port(listener)
  assert port > 0
  let received = process.new_subject()
  let _ =
    process.spawn(fn() {
      let assert Ok(server) = accept(listener)
      process.send(received, recv_line(server, 1000))
      close_socket(server)
      close_socket(server)
    })

  let assert Ok(client) = connect("127.0.0.1", port, 1000)
  let assert Ok(Nil) = send_line(client, "hello", 1000)
  let assert Ok(Ok("hello")) = process.receive(received, within: 1000)

  close_socket(client)
  close_socket(client)
  close_listener(listener)
  close_listener(listener)
}

pub fn control_ffi_rejects_non_loopback_listen_test() {
  let assert Error("non_loopback_host_rejected") = listen("0.0.0.0", 0)
}

pub fn control_ffi_recv_timeout_and_closed_are_finite_errors_test() {
  let assert Ok(timeout_listener) = listen("127.0.0.1", 0)
  let timeout_result = process.new_subject()
  let _ =
    process.spawn(fn() {
      let assert Ok(server) = accept(timeout_listener)
      process.send(timeout_result, recv_line(server, 20))
      close_socket(server)
    })
  let assert Ok(timeout_client) =
    connect("127.0.0.1", bound_port(timeout_listener), 1000)
  let assert Ok(Error("timeout")) =
    process.receive(timeout_result, within: 1000)
  close_socket(timeout_client)
  close_listener(timeout_listener)

  let assert Ok(closed_listener) = listen("127.0.0.1", 0)
  let closed_result = process.new_subject()
  let _ =
    process.spawn(fn() {
      let assert Ok(server) = accept(closed_listener)
      process.send(closed_result, recv_line(server, 1000))
      close_socket(server)
    })
  let assert Ok(closed_client) =
    connect("127.0.0.1", bound_port(closed_listener), 1000)
  close_socket(closed_client)
  let assert Ok(Error("closed")) = process.receive(closed_result, within: 1000)
  close_listener(closed_listener)
}

pub fn control_ffi_token_chmod_and_env_not_found_test() {
  let assert Ok(token) = generate_token(8)
  assert string.length(token) > 0

  let root = "test/tmp/control-ffi"
  reset_dir(root)
  let private_path = root <> "/control.json"
  let assert Ok(Nil) = simplifile.write(private_path, "{}")
  let assert Ok(Nil) = chmod_private(private_path)

  let assert Error("not_found") =
    getenv("SCHERZO_CONTROL_FFI_TEST_DEFINITELY_MISSING")
}

@external(erlang, "scherzo_control_ffi", "listen")
fn listen(host: String, port: Int) -> Result(Listener, String)

@external(erlang, "scherzo_control_ffi", "accept")
fn accept(listener: Listener) -> Result(Socket, String)

@external(erlang, "scherzo_control_ffi", "connect")
fn connect(host: String, port: Int, timeout_ms: Int) -> Result(Socket, String)

@external(erlang, "scherzo_control_ffi", "send_line")
fn send_line(
  socket: Socket,
  line: String,
  timeout_ms: Int,
) -> Result(Nil, String)

@external(erlang, "scherzo_control_ffi", "recv_line")
fn recv_line(socket: Socket, timeout_ms: Int) -> Result(String, String)

@external(erlang, "scherzo_control_ffi", "close_socket")
fn close_socket(socket: Socket) -> Nil

@external(erlang, "scherzo_control_ffi", "close_listener")
fn close_listener(listener: Listener) -> Nil

@external(erlang, "scherzo_control_ffi", "bound_port")
fn bound_port(listener: Listener) -> Int

@external(erlang, "scherzo_control_ffi", "generate_token")
fn generate_token(bytes: Int) -> Result(String, String)

@external(erlang, "scherzo_control_ffi", "chmod_private")
fn chmod_private(path: String) -> Result(Nil, String)

@external(erlang, "scherzo_control_ffi", "getenv")
fn getenv(name: String) -> Result(String, String)
