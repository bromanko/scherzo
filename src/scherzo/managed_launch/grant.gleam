import birl
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/remote/url
import scherzo/json_decode_error

pub type Capability {
  State
  Query
  Command
}

pub type Grant {
  Grant(
    launch_id: String,
    endpoint: url.ValidatedUrl,
    credential: String,
    daemon_label: Option(String),
    capabilities: List(Capability),
    command_bridge_enabled: Bool,
    expires_at: birl.Time,
    expires_at_ms: Int,
  )
}

pub type DecodeError {
  InvalidGrantJson(json.DecodeError)
  UnsupportedGrantVersion(Int)
  MissingLaunchId
  EmptyLaunchId
  InvalidEndpoint(url.ValidationError)
  EmptyCredential
  UnsupportedCapability(String)
  MissingStateCapability
  InvalidExpiresAt(String)
  ExpiredGrant
  DaemonIdentityOverrideRejected
}

type RawGrant {
  RawGrant(
    version: Int,
    launch_id: String,
    endpoint: String,
    credential: String,
    daemon_label: Option(String),
    capabilities: List(String),
    command_bridge_enabled: Bool,
    expires_at: String,
    daemon_id: Option(String),
  )
}

pub fn decode_string(
  contents: String,
  now_ms: Int,
) -> Result(Grant, DecodeError) {
  case json.parse(contents, raw_grant_decoder()) {
    Ok(raw) -> validate_raw_grant(raw, now_ms)
    Error(error) -> Error(InvalidGrantJson(error))
  }
}

pub fn capability_to_string(capability: Capability) -> String {
  case capability {
    State -> "state"
    Query -> "query"
    Command -> "command"
  }
}

pub fn capabilities_to_strings(capabilities: List(Capability)) -> List(String) {
  list.map(capabilities, capability_to_string)
}

pub fn has_capability(grant: Grant, capability: Capability) -> Bool {
  list.contains(grant.capabilities, capability)
}

pub fn error_code(error: DecodeError) -> String {
  case error {
    InvalidGrantJson(_) -> "invalid_grant_json"
    UnsupportedGrantVersion(_) -> "unsupported_grant_version"
    MissingLaunchId -> "missing_launch_id"
    EmptyLaunchId -> "empty_launch_id"
    InvalidEndpoint(_) -> "invalid_grant_endpoint"
    EmptyCredential -> "empty_grant_credential"
    UnsupportedCapability(_) -> "unsupported_grant_capability"
    MissingStateCapability -> "missing_state_capability"
    InvalidExpiresAt(_) -> "invalid_grant_expiry"
    ExpiredGrant -> "grant_expired"
    DaemonIdentityOverrideRejected -> "daemon_identity_override_rejected"
  }
}

pub fn error_message(error: DecodeError) -> String {
  case error {
    InvalidGrantJson(parse_error) ->
      "grant JSON is invalid: " <> json_decode_error.to_string(parse_error)
    UnsupportedGrantVersion(version) ->
      "grant version " <> int_to_string(version) <> " is not supported"
    MissingLaunchId -> "grant launchId is required"
    EmptyLaunchId -> "grant launchId must not be empty"
    InvalidEndpoint(validation_error) -> url.error_message(validation_error)
    EmptyCredential -> "grant credential must not be empty"
    UnsupportedCapability(capability) ->
      "grant capability is not supported: " <> capability
    MissingStateCapability ->
      "grant capabilities must include state for daemon status streaming"
    InvalidExpiresAt(value) ->
      "grant expiresAt must be a valid RFC 3339 timestamp: " <> value
    ExpiredGrant -> "grant has expired"
    DaemonIdentityOverrideRejected ->
      "grant must not provide daemonId; Core owns daemon identity"
  }
}

fn validate_raw_grant(
  raw: RawGrant,
  now_ms: Int,
) -> Result(Grant, DecodeError) {
  let RawGrant(
    version: version,
    launch_id: launch_id,
    endpoint: endpoint,
    credential: credential,
    daemon_label: daemon_label,
    capabilities: capabilities,
    command_bridge_enabled: command_bridge_enabled,
    expires_at: expires_at,
    daemon_id: daemon_id,
  ) = raw

  use _ <- result.try(ensure_version(version))
  use _ <- result.try(ensure_no_daemon_identity_override(daemon_id))
  use _ <- result.try(ensure_non_empty_launch_id(launch_id))
  use _ <- result.try(ensure_non_empty_credential(credential))
  use validated_endpoint <- result.try(validate_endpoint(endpoint))
  use validated_capabilities <- result.try(
    validate_capabilities(capabilities, []),
  )
  use _ <- result.try(ensure_state_capability(validated_capabilities))
  use validated_expiry <- result.try(validate_expiry(expires_at, now_ms))

  Ok(Grant(
    launch_id: launch_id,
    endpoint: validated_endpoint,
    credential: credential,
    daemon_label: daemon_label,
    capabilities: validated_capabilities,
    command_bridge_enabled: command_bridge_enabled,
    expires_at: validated_expiry,
    expires_at_ms: birl.to_unix_milli(validated_expiry),
  ))
}

fn ensure_version(version: Int) -> Result(Nil, DecodeError) {
  case version {
    1 -> Ok(Nil)
    _ -> Error(UnsupportedGrantVersion(version))
  }
}

fn ensure_no_daemon_identity_override(
  daemon_id: Option(String),
) -> Result(Nil, DecodeError) {
  case daemon_id {
    Some(_) -> Error(DaemonIdentityOverrideRejected)
    None -> Ok(Nil)
  }
}

fn ensure_non_empty_launch_id(launch_id: String) -> Result(Nil, DecodeError) {
  case string.trim(launch_id) {
    "" -> Error(EmptyLaunchId)
    _ -> Ok(Nil)
  }
}

fn ensure_non_empty_credential(credential: String) -> Result(Nil, DecodeError) {
  case string.trim(credential) {
    "" -> Error(EmptyCredential)
    _ -> Ok(Nil)
  }
}

fn validate_endpoint(
  endpoint: String,
) -> Result(url.ValidatedUrl, DecodeError) {
  url.validate_server_url(endpoint, allow_loopback: True)
  |> result.map_error(InvalidEndpoint)
}

fn validate_capabilities(
  remaining: List(String),
  acc: List(Capability),
) -> Result(List(Capability), DecodeError) {
  case remaining {
    [] -> Ok(list.reverse(acc))
    [capability, ..rest] ->
      case capability_from_string(capability) {
        Ok(value) -> validate_capabilities(rest, [value, ..acc])
        Error(error) -> Error(error)
      }
  }
}

fn capability_from_string(value: String) -> Result(Capability, DecodeError) {
  case value {
    "state" -> Ok(State)
    "query" -> Ok(Query)
    "command" -> Ok(Command)
    _ -> Error(UnsupportedCapability(value))
  }
}

fn ensure_state_capability(
  capabilities: List(Capability),
) -> Result(Nil, DecodeError) {
  case list.contains(capabilities, State) {
    True -> Ok(Nil)
    False -> Error(MissingStateCapability)
  }
}

fn validate_expiry(
  value: String,
  now_ms: Int,
) -> Result(birl.Time, DecodeError) {
  case birl.parse(value) {
    Ok(time) ->
      case birl.to_unix_milli(time) > now_ms {
        True -> Ok(time)
        False -> Error(ExpiredGrant)
      }
    Error(_) -> Error(InvalidExpiresAt(value))
  }
}

fn raw_grant_decoder() -> decode.Decoder(RawGrant) {
  use version <- decode.field("version", decode.int)
  use launch_id <- decode.optional_field(
    "launchId",
    None,
    decode.optional(decode.string),
  )
  use endpoint <- decode.field("endpoint", decode.string)
  use credential <- decode.field("credential", decode.string)
  use daemon_label <- decode.optional_field(
    "daemonLabel",
    None,
    decode.optional(decode.string),
  )
  use capabilities <- decode.field(
    "capabilities",
    decode.list(of: decode.string),
  )
  use command_bridge_enabled <- decode.field(
    "commandBridgeEnabled",
    decode.bool,
  )
  use expires_at <- decode.field("expiresAt", decode.string)
  use daemon_id <- decode.optional_field(
    "daemonId",
    None,
    decode.optional(decode.string),
  )

  case launch_id {
    Some(launch_id) ->
      decode.success(RawGrant(
        version: version,
        launch_id: launch_id,
        endpoint: endpoint,
        credential: credential,
        daemon_label: daemon_label,
        capabilities: capabilities,
        command_bridge_enabled: command_bridge_enabled,
        expires_at: expires_at,
        daemon_id: daemon_id,
      ))
    None ->
      decode.failure(
        RawGrant(
          version,
          "",
          endpoint,
          credential,
          daemon_label,
          capabilities,
          command_bridge_enabled,
          expires_at,
          daemon_id,
        ),
        expected: "managed launch grant with launchId",
      )
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
