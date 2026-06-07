import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/remote/credential_store
import scherzo/control/remote/daemon_label
import scherzo/control/remote/pairing_client
import scherzo/daemon_identity
import scherzo/runtime_bundle

pub type Command {
  Command(
    pairing_token: String,
    server_url: String,
    credential_ref: String,
    daemon_label: Option(String),
    replace_credential: Bool,
    json: Bool,
    allow_loopback_url: Bool,
    config_path: Option(String),
  )
}

pub type Error {
  UsageError(String)
  Failed(code: String, message: String)
}

pub type Output {
  Output(line: fn(String) -> Nil)
}

pub type Dependencies {
  Dependencies(
    load_bundle: fn(Option(String)) ->
      Result(runtime_bundle.RuntimeBundle, runtime_bundle.BundleError),
    load_or_create_identity: fn(String) ->
      Result(daemon_identity.DaemonIdentity, daemon_identity.IdentityError),
    exchange_pairing_token: fn(String, String, String, Option(String), Bool) ->
      Result(pairing_client.PairingSuccess, pairing_client.PairingError),
    write_credential: fn(
      credential_store.CredentialRef,
      String,
      String,
      credential_store.DaemonCredential,
      Bool,
    ) -> Result(credential_store.WriteResult, credential_store.StoreError),
  )
}

pub fn main(args: List(String)) -> Result(Nil, Error) {
  use command <- result.try(parse(args))
  run_with_deps(command, default_dependencies(), Output(line: io.println))
}

pub fn parse(args: List(String)) -> Result(Command, Error) {
  parse_loop(
    args,
    Command(
      pairing_token: "",
      server_url: "",
      credential_ref: "default",
      daemon_label: None,
      replace_credential: False,
      json: False,
      allow_loopback_url: False,
      config_path: None,
    ),
  )
}

fn parse_loop(args: List(String), command: Command) -> Result(Command, Error) {
  case args {
    [] -> finalize(command)
    ["--pairing-token", value, ..rest] ->
      parse_loop(rest, Command(..command, pairing_token: value))
    ["--server-url", value, ..rest] ->
      parse_loop(rest, Command(..command, server_url: value))
    ["--credential-ref", value, ..rest] ->
      parse_loop(rest, Command(..command, credential_ref: value))
    ["--name", value, ..rest] ->
      case daemon_label.normalize(value) {
        Ok(label) ->
          parse_loop(rest, Command(..command, daemon_label: Some(label)))
        Error(error) ->
          Error(UsageError("--name " <> daemon_label.error_message(error)))
      }
    ["--config", value, ..rest] ->
      parse_loop(rest, Command(..command, config_path: Some(value)))
    ["--json", ..rest] -> parse_loop(rest, Command(..command, json: True))
    ["--replace-credential", ..rest] ->
      parse_loop(rest, Command(..command, replace_credential: True))
    ["--allow-loopback-url", ..rest] ->
      parse_loop(rest, Command(..command, allow_loopback_url: True))
    ["--help", ..] | ["-h", ..] -> Error(UsageError(usage()))
    [flag, ..rest] ->
      case string.starts_with(flag, "--") {
        True -> Error(UsageError("unknown connect option: " <> flag))
        False ->
          case rest {
            [] -> finalize(Command(..command, config_path: Some(flag)))
            _ -> Error(UsageError(usage()))
          }
      }
  }
}

fn finalize(command: Command) -> Result(Command, Error) {
  case string.trim(command.pairing_token), string.trim(command.server_url) {
    "", _ -> Error(UsageError("connect requires --pairing-token <token>"))
    _, "" -> Error(UsageError("connect requires --server-url <url>"))
    _, _ -> Ok(command)
  }
}

pub fn usage() -> String {
  "Usage: scherzo connect --pairing-token <pair_...> --server-url <url> [--name <friendly-name>] [--credential-ref <name>] [--replace-credential] [--allow-loopback-url] [--json] [--config <path-to-scherzo.yaml>]\n\nOptions:\n  --name <friendly-name>  Non-secret UI daemon label. Overrides ui_server.daemon_label from config. Shape: "
  <> daemon_label.shape_description()
  <> "."
}

pub fn run_with_deps(
  command: Command,
  deps: Dependencies,
  output: Output,
) -> Result(Nil, Error) {
  use bundle <- result.try(
    deps.load_bundle(command.config_path) |> result.map_error(bundle_error),
  )
  use credential_ref <- result.try(
    credential_store.normalize_credential_ref(command.credential_ref)
    |> result.map_error(fn(message) {
      Failed("invalid_credential_ref", message)
    }),
  )
  use identity <- result.try(
    deps.load_or_create_identity(bundle.effective.workspace.root)
    |> result.map_error(identity_error),
  )
  let resolved_daemon_label = case command.daemon_label {
    Some(_) -> command.daemon_label
    None -> bundle.effective.ui_server.daemon_label
  }
  use paired <- result.try(
    deps.exchange_pairing_token(
      command.server_url,
      command.pairing_token,
      identity.daemon_id,
      resolved_daemon_label,
      command.allow_loopback_url,
    )
    |> result.map_error(pairing_error),
  )
  use write_result <- result.try(
    deps.write_credential(
      credential_ref,
      paired.server_url,
      paired.daemon_id,
      paired.credential,
      command.replace_credential,
    )
    |> result.map_error(store_error),
  )
  let store_path = case write_result {
    credential_store.CredentialWritten(path) -> path
    credential_store.CredentialAlreadyStored(path) -> path
  }
  case command.json {
    True ->
      output.line(
        json.to_string(
          json.object(json_output_fields(
            paired.server_url,
            paired.daemon_id,
            command.credential_ref,
            store_path,
            resolved_daemon_label,
          )),
        ),
      )
    False -> {
      let label_clause = case resolved_daemon_label {
        Some(label) -> " with UI label " <> label
        None -> ""
      }
      output.line(
        "Connected daemon "
        <> paired.daemon_id
        <> label_clause
        <> " to "
        <> paired.server_url
        <> " using credential_ref "
        <> command.credential_ref
        <> ". Stored credential at "
        <> store_path,
      )
    }
  }
  Ok(Nil)
}

fn json_output_fields(
  server_url: String,
  daemon_id: String,
  credential_ref: String,
  store_path: String,
  daemon_label_value: Option(String),
) -> List(#(String, json.Json)) {
  let base_fields = [
    #("status", json.string("ok")),
    #("server_url", json.string(server_url)),
    #("daemon_id", json.string(daemon_id)),
    #("credential_ref", json.string(credential_ref)),
    #("store_path", json.string(store_path)),
  ]
  case daemon_label_value {
    Some(label) -> [#("daemon_label", json.string(label)), ..base_fields]
    None -> base_fields
  }
}

fn default_dependencies() -> Dependencies {
  Dependencies(
    load_bundle: runtime_bundle.load,
    load_or_create_identity: daemon_identity.load_or_create,
    exchange_pairing_token: fn(
      server_url,
      pairing_token,
      daemon_id,
      daemon_label,
      allow_loopback_url,
    ) {
      pairing_client.exchange_pairing_token_with_label(
        server_url,
        pairing_token,
        daemon_id,
        daemon_label,
        allow_loopback_url,
        pairing_client.default_dependencies(),
      )
    },
    write_credential: credential_store.write_credential,
  )
}

fn bundle_error(error: runtime_bundle.BundleError) -> Error {
  let runtime_bundle.BundleError(code: code, message: message) = error
  Failed(code, message)
}

fn identity_error(error: daemon_identity.IdentityError) -> Error {
  Failed("daemon_identity_failed", daemon_identity.error_message(error))
}

fn pairing_error(error: pairing_client.PairingError) -> Error {
  Failed(pairing_client.error_code(error), pairing_client.error_message(error))
}

fn store_error(error: credential_store.StoreError) -> Error {
  case error {
    credential_store.ReplaceRequired(_) ->
      Failed("replace_required", credential_store.error_message(error))
    _ ->
      Failed("credential_store_failed", credential_store.error_message(error))
  }
}

pub fn error_code(error: Error) -> String {
  case error {
    UsageError(_) -> "usage_error"
    Failed(code, _) -> code
  }
}

pub fn error_message(error: Error) -> String {
  case error {
    UsageError(message) -> message
    Failed(_, message) -> message
  }
}
