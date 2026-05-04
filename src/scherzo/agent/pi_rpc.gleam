import gleam/option.{type Option}
import scherzo/error
import scherzo/pi/client
import scherzo/pi/protocol
import scherzo/session/tokens as session_tokens

pub type Session =
  client.Session

pub type RpcRecord =
  protocol.RpcRecord

pub type Data =
  protocol.Data

pub fn encode_set_session_name(id: String, name: String) -> String {
  protocol.encode_set_session_name(id, name)
}

pub fn encode_set_auto_retry(id: String, enabled: Bool) -> String {
  protocol.encode_set_auto_retry(id, enabled)
}

pub fn encode_get_state(id: String) -> String {
  protocol.encode_get_state(id)
}

pub fn encode_prompt(id: String, message: String) -> String {
  protocol.encode_prompt(id, message)
}

pub fn encode_abort(id: String) -> String {
  protocol.encode_abort(id)
}

pub fn encode_get_session_stats(id: String) -> String {
  protocol.encode_get_session_stats(id)
}

pub fn encode_extension_ui_response(id: String) -> String {
  protocol.encode_extension_ui_response(id)
}

pub fn encode_extension_ui_value_response(id: String, value: String) -> String {
  protocol.encode_extension_ui_value_response(id, value)
}

pub fn decode_record(
  line: String,
) -> Result(protocol.RpcRecord, error.PiRpcError) {
  protocol.decode_record(line)
}

pub fn launch(
  command: String,
  cwd: String,
  session_name: String,
  auto_retry: Bool,
  read_timeout_ms: Int,
) -> Result(client.Session, error.PiRpcError) {
  client.launch(command, cwd, session_name, auto_retry, read_timeout_ms)
}

pub fn send_prompt(
  session: client.Session,
  message: String,
  read_timeout_ms: Int,
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  client.send_prompt(session, message, read_timeout_ms)
}

pub fn read_turn_record(
  session: client.Session,
  read_timeout_ms: Int,
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
) -> Result(#(client.Session, Option(protocol.RpcRecord)), error.PiRpcError) {
  client.read_turn_record(
    session,
    read_timeout_ms,
    turn_deadline_ms,
    stall_deadline_ms,
  )
}

pub fn send_abort(
  session: client.Session,
  read_timeout_ms: Int,
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  client.send_abort(session, read_timeout_ms)
}

pub fn send_extension_ui_cancel(
  session: client.Session,
  request_id: String,
  read_timeout_ms: Int,
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  client.send_extension_ui_cancel(session, request_id, read_timeout_ms)
}

pub fn send_extension_ui_value(
  session: client.Session,
  request_id: String,
  value: String,
  read_timeout_ms: Int,
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  client.send_extension_ui_value(session, request_id, value, read_timeout_ms)
}

pub fn get_session_stats(
  session: client.Session,
  read_timeout_ms: Int,
) -> Result(#(client.Session, session_tokens.TokenTotals), error.PiRpcError) {
  client.get_session_stats(session, read_timeout_ms)
}

pub fn terminate(session: client.Session) -> Result(Nil, error.PiRpcError) {
  client.terminate(session)
}
