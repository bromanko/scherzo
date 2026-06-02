import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None}
import scherzo/session/event

pub type SessionSnapshot {
  SessionSnapshot(
    session_id: String,
    display_name: String,
    issue_identifier: String,
    status: String,
    current_turn: Int,
    last_event_at_ms: Int,
  )
}

pub type ClientMessage {
  DaemonHello(daemon_id: String, boot_id: String)
  Heartbeat(sent_at_ms: Int)
  DaemonState(
    sent_at_ms: Int,
    dispatch_paused: Bool,
    sessions: List(SessionSnapshot),
  )
}

pub type ServerMessage {
  ServerHello(heartbeat_interval_ms: Option(Int))
  CredentialRevoked(reason: String)
  DaemonIdentityRevoked(reason: String)
  UnknownServerMessage(String)
}

pub type DecodeError {
  DecodeError
}

pub fn session_from_summary(summary: event.SessionSummary) -> SessionSnapshot {
  SessionSnapshot(
    session_id: summary.session_id,
    display_name: summary.display_name,
    issue_identifier: summary.issue_identifier,
    status: event.status_to_string(summary.status),
    current_turn: summary.current_turn,
    last_event_at_ms: summary.last_event_at_ms,
  )
}

pub fn encode_client_message(message: ClientMessage) -> String {
  case message {
    DaemonHello(daemon_id, boot_id) ->
      json.object([
        #("type", json.string("daemon_hello")),
        #("daemonId", json.string(daemon_id)),
        #("bootId", json.string(boot_id)),
      ])
      |> json.to_string
    Heartbeat(sent_at_ms) ->
      json.object([
        #("type", json.string("heartbeat")),
        #("sentAtMs", json.int(sent_at_ms)),
      ])
      |> json.to_string
    DaemonState(sent_at_ms, dispatch_paused, sessions) ->
      json.object([
        #("type", json.string("daemon_state")),
        #("sentAtMs", json.int(sent_at_ms)),
        #("dispatchPaused", json.bool(dispatch_paused)),
        #("sessions", json.array(sessions, of: session_to_json)),
      ])
      |> json.to_string
  }
}

pub fn decode_server_message(
  payload: String,
) -> Result(ServerMessage, DecodeError) {
  case json.parse(payload, server_message_decoder()) {
    Ok(message) -> Ok(message)
    Error(_) -> Error(DecodeError)
  }
}

fn server_message_decoder() -> decode.Decoder(ServerMessage) {
  use kind <- decode.field("type", decode.string)
  case kind {
    "server_hello" -> {
      use heartbeat_interval_ms <- decode.optional_field(
        "heartbeatIntervalMs",
        None,
        decode.optional(decode.int),
      )
      decode.success(ServerHello(heartbeat_interval_ms))
    }
    "credential_revoked" -> {
      use reason <- decode.optional_field(
        "reason",
        "credential revoked",
        decode.string,
      )
      decode.success(CredentialRevoked(reason))
    }
    "daemon_identity_revoked" -> {
      use reason <- decode.optional_field(
        "reason",
        "daemon identity revoked",
        decode.string,
      )
      decode.success(DaemonIdentityRevoked(reason))
    }
    other -> decode.success(UnknownServerMessage(other))
  }
}

fn session_to_json(session: SessionSnapshot) -> json.Json {
  json.object([
    #("sessionId", json.string(session.session_id)),
    #("displayName", json.string(session.display_name)),
    #("issueIdentifier", json.string(session.issue_identifier)),
    #("status", json.string(session.status)),
    #("currentTurn", json.int(session.current_turn)),
    #("lastEventAtMs", json.int(session.last_event_at_ms)),
  ])
}
