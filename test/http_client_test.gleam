import gleam/erlang/process
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/int
import gleam/option.{None, Some}
import gleam/string
import scherzo/http_client

type Listener

type Socket

pub fn proxy_environment_precedence_and_validation_are_safe_test() {
  let assert Ok(Nil) = reset_httpc_proxy_options()
  let env = fn(name) {
    case name {
      "http_proxy" -> Some("http://lower-proxy.test:8080")
      "HTTP_PROXY" -> Some("http://upper-proxy.test:9080")
      "HTTPS_PROXY" -> Some("secure-proxy.test:8443")
      "no_proxy" -> Some(".internal.test, localhost:9000, [::1]:9443")
      "NO_PROXY" -> Some("ignored.test")
      _ -> None
    }
  }

  let configured = http_client.configure_with_env(env)
  let snapshot = httpc_proxy_options()
  let reset = reset_httpc_proxy_options()

  assert configured == Ok(Nil)
  let assert Ok(#("lower-proxy.test", 8080, "secure-proxy.test", 8443, no_proxy)) =
    snapshot
  assert no_proxy
    == [
      "*..internal.test",
      "internal.test",
      "localhost",
      "::1",
    ]
  assert reset == Ok(Nil)

  let wildcard_configuration =
    http_client.configure_values(
      Some("http://wildcard-proxy.test"),
      None,
      Some("*"),
    )
  let wildcard_snapshot = httpc_proxy_options()
  let wildcard_reset = reset_httpc_proxy_options()
  assert wildcard_configuration == Ok(Nil)
  assert wildcard_snapshot == Ok(#("wildcard-proxy.test", 80, "", 0, ["*."]))
  assert wildcard_reset == Ok(Nil)

  let secret_user = "proxy-user-marker"
  let secret_password = "proxy-password-marker"
  let assert Error(http_client.ConfigureError(message)) =
    http_client.configure_values(
      Some(
        "http://" <> secret_user <> ":" <> secret_password <> "@proxy.test:8080",
      ),
      None,
      None,
    )
  assert string.contains(message, "proxy authentication is not supported")
  assert !string.contains(message, secret_user)
  assert !string.contains(message, secret_password)
}

pub fn configured_httpc_profile_routes_http_and_https_test() {
  let assert Ok(Nil) = reset_httpc_proxy_options()
  let #(http_listener, http_port, http_request_line) =
    start_fake_proxy(
      "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}",
    )
  let http_proxy = "http://127.0.0.1:" <> int.to_string(http_port)
  let http_configuration =
    http_client.configure_values(Some(http_proxy), None, None)
  let http_response = dispatch("http://proxy-target.invalid/graphql")
  let observed_http_line = process.receive(http_request_line, within: 2000)
  close_listener(http_listener)

  let #(https_listener, https_port, https_request_line) =
    start_fake_proxy(
      "HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\nConnection: close\r\n\r\n",
    )
  let https_proxy = "http://127.0.0.1:" <> int.to_string(https_port)
  let https_configuration =
    http_client.configure_values(None, Some(https_proxy), None)
  let https_response = httpc_post("https://secure-proxy-target.invalid/graphql")
  let observed_https_line = process.receive(https_request_line, within: 2000)
  close_listener(https_listener)

  let #(bypass_target, bypass_target_port, bypass_target_request_line) =
    start_fake_proxy(
      "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n[]",
    )
  let #(bypass_proxy, bypass_proxy_port, _) =
    start_fake_proxy(
      "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}",
    )
  let bypass_configuration =
    http_client.configure_values(
      Some("http://127.0.0.1:" <> int.to_string(bypass_proxy_port)),
      None,
      Some("127.0.0.1"),
    )
  let bypass_response =
    dispatch(
      "http://127.0.0.1:" <> int.to_string(bypass_target_port) <> "/graphql",
    )
  let observed_bypass_line =
    process.receive(bypass_target_request_line, within: 2000)
  close_listener(bypass_target)
  close_listener(bypass_proxy)

  let reset = reset_httpc_proxy_options()

  assert http_configuration == Ok(Nil)
  let assert Ok(response) = http_response
  assert response.status == 200
  assert response.body == "{}"
  let assert Ok(Ok(http_line)) = observed_http_line
  assert http_line == "POST http://proxy-target.invalid/graphql HTTP/1.1"

  assert https_configuration == Ok(Nil)
  let assert Error(_) = https_response
  let assert Ok(Ok(https_line)) = observed_https_line
  assert https_line == "CONNECT secure-proxy-target.invalid:443 HTTP/1.1"

  assert bypass_configuration == Ok(Nil)
  let assert Ok(response) = bypass_response
  assert response.status == 200
  assert response.body == "[]"
  let assert Ok(Ok(bypass_line)) = observed_bypass_line
  assert bypass_line == "POST /graphql HTTP/1.1"
  assert reset == Ok(Nil)
}

fn dispatch(url: String) {
  let assert Ok(request) = http_request.to(url)
  let request =
    request
    |> http_request.set_method(http.Post)
    |> http_request.set_body("{}")
    |> http_request.set_header("content-type", "application/json")
  httpc.configure()
  |> httpc.timeout(1000)
  |> httpc.dispatch(request)
}

fn start_fake_proxy(
  response: String,
) -> #(Listener, Int, process.Subject(Result(String, String))) {
  let assert Ok(listener) = listen("127.0.0.1", 0)
  let request_line = process.new_subject()
  let _ =
    process.spawn(fn() {
      case accept(listener) {
        Error(message) -> process.send(request_line, Error(message))
        Ok(socket) -> {
          let line = recv_line(socket, 1000)
          let _ = send_line(socket, response, 1000)
          close_socket(socket)
          process.send(request_line, line)
        }
      }
    })
  #(listener, bound_port(listener), request_line)
}

@external(erlang, "scherzo_test_ffi", "httpc_proxy_options")
fn httpc_proxy_options() -> Result(
  #(String, Int, String, Int, List(String)),
  Nil,
)

@external(erlang, "scherzo_test_ffi", "reset_httpc_proxy_options")
fn reset_httpc_proxy_options() -> Result(Nil, Nil)

@external(erlang, "scherzo_test_ffi", "httpc_post")
fn httpc_post(url: String) -> Result(Nil, Nil)

@external(erlang, "scherzo_control_ffi", "listen")
fn listen(host: String, port: Int) -> Result(Listener, String)

@external(erlang, "scherzo_control_ffi", "accept")
fn accept(listener: Listener) -> Result(Socket, String)

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
