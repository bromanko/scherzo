import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/client
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/remote/credential_store
import scherzo/control/remote/daemon_label
import scherzo/control/remote/pairing_client
import scherzo/control/remote/url
import scherzo/daemon_identity
import scherzo/runtime_bundle
import simplifile

pub type Command {
  Command(
    pairing_token: String,
    server_url: String,
    credential_ref: String,
    daemon_label: Option(String),
    replace_credential: Bool,
    json: Bool,
    allow_loopback_url: Bool,
    activate: Bool,
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

pub type ActivationStatus {
  ReloadNotified
  ManualReloadRequired
}

type ConfigActivationStatus {
  ActivationNotRequested
  ConfigActivated(path: String)
  ConfigAlreadyActive(path: String)
}

type ConfigActivationError {
  UiServerActivationConflict(message: String)
  UiServerActivationUnsupported(message: String)
  UiServerActivationWriteFailed(path: String, message: String)
}

type DesiredUiServerConfig {
  DesiredUiServerConfig(
    endpoint: String,
    credential_ref: String,
    daemon_label: Option(String),
  )
}

type ConfigActivationPlan {
  ActivationNotPlanned
  ActivationAlreadyPlanned(path: String)
  ActivationWritePlanned(path: String, contents: String)
}

const activation_reload_timeout_ms = 1000

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
    notify_reload: fn(String) -> ActivationStatus,
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
      activate: False,
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
    ["--activate", ..rest] ->
      parse_loop(rest, Command(..command, activate: True))
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
  "Usage: scherzo connect --pairing-token <pair_...> --server-url <url> [--activate] [--name <friendly-name>] [--credential-ref <name>] [--replace-credential] [--allow-loopback-url] [--json] [--config <path-to-scherzo.yaml>]\n\nOptions:\n  --activate              Write non-secret ui_server settings to the selected project config, then reload the daemon.\n  --name <friendly-name>  Non-secret UI daemon label. Overrides ui_server.daemon_label from config. Shape: "
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
  use activation_desired <- result.try(activation_plan(command, credential_ref))
  use config_activation_plan <- result.try(
    plan_project_config_activation(bundle, activation_desired)
    |> result.map_error(config_activation_error),
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
  let credential_store.CredentialRef(profile: credential_ref_name) =
    credential_ref
  let store_path = case write_result {
    credential_store.CredentialWritten(path) -> path
    credential_store.CredentialAlreadyStored(path) -> path
  }
  use config_activation <- result.try(
    apply_project_config_activation(config_activation_plan)
    |> result.map_error(fn(error) {
      post_credential_config_activation_error(error, store_path)
    }),
  )
  let reload_status = deps.notify_reload(bundle.effective.workspace.root)
  case command.json {
    True ->
      output.line(
        json.to_string(
          json.object(json_output_fields(
            paired.server_url,
            paired.daemon_id,
            credential_ref_name,
            store_path,
            resolved_daemon_label,
            config_activation,
            reload_status,
          )),
        ),
      )
    False -> {
      let label_clause = case resolved_daemon_label {
        Some(label) -> " with UI label " <> label
        None -> ""
      }
      output.line(
        "Stored credential for daemon "
        <> paired.daemon_id
        <> label_clause
        <> " to "
        <> paired.server_url
        <> " using credential_ref "
        <> credential_ref_name
        <> ". Stored credential at "
        <> store_path
        <> ". "
        <> config_activation_message(config_activation)
        <> " "
        <> reload_message(config_activation, reload_status),
      )
    }
  }
  Ok(Nil)
}

fn activation_plan(
  command: Command,
  credential_ref: credential_store.CredentialRef,
) -> Result(Option(DesiredUiServerConfig), Error) {
  case command.activate {
    False -> Ok(None)
    True -> {
      use validated <- result.try(
        url.validate_server_url(
          command.server_url,
          allow_loopback: command.allow_loopback_url,
        )
        |> result.map_error(fn(validation_error) {
          pairing_error(pairing_client.InvalidUrl(validation_error))
        }),
      )
      let credential_store.CredentialRef(profile: credential_ref_name) =
        credential_ref
      Ok(
        Some(DesiredUiServerConfig(
          endpoint: validated.base_url,
          credential_ref: credential_ref_name,
          daemon_label: command.daemon_label,
        )),
      )
    }
  }
}

fn check_activation_conflicts(
  ui_server: config_types.UiServerConfig,
  desired: DesiredUiServerConfig,
) -> Result(Nil, ConfigActivationError) {
  use _ <- result.try(check_optional_conflict(
    "ui_server.endpoint",
    ui_server.endpoint,
    desired.endpoint,
  ))
  use _ <- result.try(check_optional_conflict(
    "ui_server.credential_ref",
    ui_server.credential_ref,
    desired.credential_ref,
  ))
  case desired.daemon_label, ui_server.daemon_label {
    Some(desired_label), Some(existing_label)
      if desired_label != existing_label
    ->
      Error(conflict_error(
        "ui_server.daemon_label",
        existing_label,
        desired_label,
      ))
    _, _ -> Ok(Nil)
  }
}

fn check_optional_conflict(
  field: String,
  existing: Option(String),
  desired: String,
) -> Result(Nil, ConfigActivationError) {
  case existing {
    Some(existing) if existing != desired ->
      Error(conflict_error(field, existing, desired))
    _ -> Ok(Nil)
  }
}

fn conflict_error(
  field: String,
  existing: String,
  desired: String,
) -> ConfigActivationError {
  UiServerActivationConflict(
    field
    <> " is already set to "
    <> existing
    <> "; refusing to replace it with "
    <> desired
    <> " during --activate",
  )
}

fn plan_project_config_activation(
  bundle: runtime_bundle.RuntimeBundle,
  desired: Option(DesiredUiServerConfig),
) -> Result(ConfigActivationPlan, ConfigActivationError) {
  case desired {
    None -> Ok(ActivationNotPlanned)
    Some(desired) -> {
      use _ <- result.try(check_activation_conflicts(
        bundle.effective.ui_server,
        desired,
      ))
      case
        ui_server_already_active(
          bundle.effective.ui_server,
          desired,
          ui_server_command_bridge_setting_lines(string.split(
            bundle.config_contents,
            on: "\n",
          )),
        )
      {
        True -> Ok(ActivationAlreadyPlanned(bundle.config_path))
        False -> {
          use updated <- result.try(update_config_contents(
            bundle.config_contents,
            desired,
          ))
          case updated == bundle.config_contents {
            True -> Ok(ActivationAlreadyPlanned(bundle.config_path))
            False -> Ok(ActivationWritePlanned(bundle.config_path, updated))
          }
        }
      }
    }
  }
}

fn apply_project_config_activation(
  plan: ConfigActivationPlan,
) -> Result(ConfigActivationStatus, ConfigActivationError) {
  case plan {
    ActivationNotPlanned -> Ok(ActivationNotRequested)
    ActivationAlreadyPlanned(path) -> Ok(ConfigAlreadyActive(path))
    ActivationWritePlanned(path, contents) -> {
      use _ <- result.try(
        simplifile.write(path, contents)
        |> result.map_error(fn(error) {
          UiServerActivationWriteFailed(path, simplifile.describe_error(error))
        }),
      )
      Ok(ConfigActivated(path))
    }
  }
}

fn ui_server_already_active(
  ui_server: config_types.UiServerConfig,
  desired: DesiredUiServerConfig,
  command_bridge_setting: Option(Bool),
) -> Bool {
  let bridge_active = case command_bridge_setting {
    Some(False) -> True
    Some(True) | None -> ui_server.command_bridge_enabled
  }
  ui_server.enabled
  && ui_server.endpoint == Some(desired.endpoint)
  && ui_server.credential_ref == Some(desired.credential_ref)
  && bridge_active
  && daemon_label_already_active(ui_server.daemon_label, desired.daemon_label)
}

fn daemon_label_already_active(
  existing: Option(String),
  desired: Option(String),
) -> Bool {
  case desired {
    None -> True
    Some(label) -> existing == Some(label)
  }
}

fn update_config_contents(
  contents: String,
  desired: DesiredUiServerConfig,
) -> Result(String, ConfigActivationError) {
  case
    rewrite_existing_ui_server(string.split(contents, on: "\n"), desired, [])
  {
    Ok(Some(lines)) -> Ok(join_config_lines(lines))
    Ok(None) -> Ok(append_ui_server_block(contents, desired))
    Error(error) -> Error(error)
  }
}

fn rewrite_existing_ui_server(
  lines: List(String),
  desired: DesiredUiServerConfig,
  before: List(String),
) -> Result(Option(List(String)), ConfigActivationError) {
  case lines {
    [] -> Ok(None)
    [line, ..rest] ->
      case is_ui_server_header(line) {
        False -> rewrite_existing_ui_server(rest, desired, [line, ..before])
        True ->
          case ui_server_header_is_block_style(line) {
            False ->
              Error(UiServerActivationUnsupported(
                "scherzo connect --activate can only update block-style ui_server: maps; expand ui_server to block style before activating",
              ))
            True -> {
              let #(body, after) = take_ui_server_body(rest, [])
              let bridge_enabled = case command_bridge_setting_from_body(body) {
                Some(False) -> False
                Some(True) | None -> True
              }
              let new_body =
                rewrite_ui_server_body(
                  body,
                  desired_ui_server_field_lines_with_bridge(
                    desired,
                    bridge_enabled,
                  ),
                )
              let rewritten_block = [line, ..list.append(new_body, after)]
              Ok(Some(list.append(list.reverse(before), rewritten_block)))
            }
          }
      }
  }
}

fn take_ui_server_body(
  lines: List(String),
  body: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #(list.reverse(body), [])
    [line, ..rest] ->
      case is_top_level_mapping_entry(line) {
        True -> #(list.reverse(body), [line, ..rest])
        False -> take_ui_server_body(rest, [line, ..body])
      }
  }
}

fn rewrite_ui_server_body(
  body: List(String),
  desired_fields: List(#(String, String)),
) -> List(String) {
  let #(rewritten_reversed, emitted) =
    rewrite_ui_server_body_loop(body, desired_fields, [], [])
  list.append(
    missing_field_lines(desired_fields, emitted, []),
    list.reverse(rewritten_reversed),
  )
}

fn rewrite_ui_server_body_loop(
  body: List(String),
  desired_fields: List(#(String, String)),
  rewritten: List(String),
  emitted: List(String),
) -> #(List(String), List(String)) {
  case body {
    [] -> #(rewritten, emitted)
    [line, ..rest] ->
      case ui_server_body_field(line) {
        Some(key) ->
          case
            desired_field_line(desired_fields, key),
            list.contains(emitted, key)
          {
            Some(rendered), False ->
              rewrite_ui_server_body_loop(
                rest,
                desired_fields,
                [rendered, ..rewritten],
                [key, ..emitted],
              )
            Some(_), True ->
              rewrite_ui_server_body_loop(
                rest,
                desired_fields,
                rewritten,
                emitted,
              )
            None, _ ->
              rewrite_ui_server_body_loop(
                rest,
                desired_fields,
                [line, ..rewritten],
                emitted,
              )
          }
        None ->
          rewrite_ui_server_body_loop(
            rest,
            desired_fields,
            [line, ..rewritten],
            emitted,
          )
      }
  }
}

fn missing_field_lines(
  desired_fields: List(#(String, String)),
  emitted: List(String),
  missing: List(String),
) -> List(String) {
  case desired_fields {
    [] -> list.reverse(missing)
    [#(key, line), ..rest] ->
      case list.contains(emitted, key) {
        True -> missing_field_lines(rest, emitted, missing)
        False -> missing_field_lines(rest, emitted, [line, ..missing])
      }
  }
}

fn desired_field_line(
  desired_fields: List(#(String, String)),
  key: String,
) -> Option(String) {
  case desired_fields {
    [] -> None
    [#(field, line), ..rest] ->
      case field == key {
        True -> Some(line)
        False -> desired_field_line(rest, key)
      }
  }
}

fn desired_ui_server_field_lines(
  desired: DesiredUiServerConfig,
) -> List(#(String, String)) {
  desired_ui_server_field_lines_with_bridge(desired, True)
}

fn desired_ui_server_field_lines_with_bridge(
  desired: DesiredUiServerConfig,
  command_bridge_enabled: Bool,
) -> List(#(String, String)) {
  let bridge_value = case command_bridge_enabled {
    True -> "true"
    False -> "false"
  }
  let base = [
    #("enabled", "  enabled: true"),
    #("endpoint", "  endpoint: " <> yaml_string(desired.endpoint)),
    #(
      "credential_ref",
      "  credential_ref: " <> yaml_string(desired.credential_ref),
    ),
    #("command_bridge_enabled", "  command_bridge_enabled: " <> bridge_value),
  ]
  case desired.daemon_label {
    Some(label) ->
      list.append(base, [
        #("daemon_label", "  daemon_label: " <> yaml_string(label)),
      ])
    None -> base
  }
}

fn append_ui_server_block(
  contents: String,
  desired: DesiredUiServerConfig,
) -> String {
  let prefix = case contents == "" || string.ends_with(contents, "\n") {
    True -> contents
    False -> contents <> "\n"
  }
  prefix <> ui_server_block_text(desired)
}

fn ui_server_block_text(desired: DesiredUiServerConfig) -> String {
  let body =
    desired_ui_server_field_lines(desired)
    |> list.map(fn(field) {
      let #(_, line) = field
      line
    })
    |> string.join(with: "\n")
  "ui_server:\n" <> body <> "\n"
}

fn join_config_lines(lines: List(String)) -> String {
  let joined = string.join(lines, with: "\n")
  case string.ends_with(joined, "\n") {
    True -> joined
    False -> joined <> "\n"
  }
}

fn is_ui_server_header(line: String) -> Bool {
  let trimmed = string.trim(line)
  !line_is_indented(line) && string.starts_with(trimmed, "ui_server:")
}

fn ui_server_header_is_block_style(line: String) -> Bool {
  let trimmed = string.trim(line)
  case string.starts_with(trimmed, "ui_server:") {
    False -> False
    True -> {
      let suffix =
        trimmed
        |> string.drop_start(string.length("ui_server:"))
        |> string.trim
      suffix == "" || string.starts_with(suffix, "#")
    }
  }
}

fn is_top_level_mapping_entry(line: String) -> Bool {
  let trimmed = string.trim(line)
  !line_is_indented(line)
  && trimmed != ""
  && !string.starts_with(trimmed, "#")
  && string.contains(trimmed, ":")
}

fn line_is_indented(line: String) -> Bool {
  string.starts_with(line, " ") || string.starts_with(line, "\t")
}

fn ui_server_body_field(line: String) -> Option(String) {
  let trimmed = string.trim(line)
  case line_is_indented(line) && !string.starts_with(trimmed, "#") {
    False -> None
    True ->
      case string.split_once(trimmed, on: ":") {
        Ok(#(key, _)) ->
          case string.trim(key) {
            "enabled" as key
            | "endpoint" as key
            | "credential_ref" as key
            | "daemon_label" as key
            | "command_bridge_enabled" as key -> Some(key)
            _ -> None
          }
        Error(Nil) -> None
      }
  }
}

fn ui_server_command_bridge_setting_lines(lines: List(String)) -> Option(Bool) {
  case lines {
    [] -> None
    [line, ..rest] ->
      case is_ui_server_header(line), ui_server_header_is_block_style(line) {
        True, True -> {
          let #(body, _) = take_ui_server_body(rest, [])
          command_bridge_setting_from_body(body)
        }
        True, False -> None
        False, _ -> ui_server_command_bridge_setting_lines(rest)
      }
  }
}

fn command_bridge_setting_from_body(body: List(String)) -> Option(Bool) {
  case body {
    [] -> None
    [line, ..rest] ->
      case ui_server_body_field(line) {
        Some("command_bridge_enabled") ->
          case yaml_bool_value(line) {
            Some(value) -> Some(value)
            None -> command_bridge_setting_from_body(rest)
          }
        _ -> command_bridge_setting_from_body(rest)
      }
  }
}

fn yaml_bool_value(line: String) -> Option(Bool) {
  case string.split_once(string.trim(line), on: ":") {
    Ok(#(_, value)) -> {
      let value = case string.split_once(value, on: "#") {
        Ok(#(before_comment, _)) -> string.trim(before_comment)
        Error(Nil) -> string.trim(value)
      }
      case value {
        "true" -> Some(True)
        "false" -> Some(False)
        _ -> None
      }
    }
    Error(Nil) -> None
  }
}

fn yaml_string(value: String) -> String {
  "\"" <> yaml_escape(value) <> "\""
}

fn yaml_escape(value: String) -> String {
  value
  |> string.replace(each: "\\", with: "\\\\")
  |> string.replace(each: "\"", with: "\\\"")
}

fn json_output_fields(
  server_url: String,
  daemon_id: String,
  credential_ref: String,
  store_path: String,
  daemon_label_value: Option(String),
  config_activation: ConfigActivationStatus,
  reload_status: ActivationStatus,
) -> List(#(String, json.Json)) {
  let base_fields = [
    #("status", json.string("ok")),
    #("server_url", json.string(server_url)),
    #("daemon_id", json.string(daemon_id)),
    #("credential_ref", json.string(credential_ref)),
    #("store_path", json.string(store_path)),
    #(
      "config_activation_status",
      json.string(config_activation_status_name(config_activation)),
    ),
    #(
      "config_activation_message",
      json.string(config_activation_message(config_activation)),
    ),
    #("reload_status", json.string(reload_status_name(reload_status))),
    #(
      "reload_message",
      json.string(reload_message(config_activation, reload_status)),
    ),
    #("activation_status", json.string(reload_status_name(reload_status))),
    #(
      "activation_message",
      json.string(reload_message(config_activation, reload_status)),
    ),
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
    notify_reload: notify_local_reload_for_workspace,
  )
}

pub fn notify_local_reload_for_workspace(
  workspace_root: String,
) -> ActivationStatus {
  let path = control_file.path_for_workspace(workspace_root)
  case control_file.read(path) {
    Ok(control) ->
      case
        client.apply_command_with_response_timeout(
          control,
          command.ReloadWorkflow,
          activation_reload_timeout_ms,
        )
      {
        Ok(command.CommandResult(status: status, ..)) ->
          reload_status_from_command_status(status)
        Error(error) -> reload_status_from_control_error(error)
      }
    Error(error) -> reload_status_from_control_file_error(error)
  }
}

fn reload_status_from_command_status(
  status: command.CommandStatus,
) -> ActivationStatus {
  case status {
    command.Applied -> ReloadNotified
    command.Queued
    | command.Rejected(_reason)
    | command.NotFound
    | command.NotAllowed(_reason) -> ManualReloadRequired
  }
}

fn reload_status_from_control_error(
  error: client.ControlError,
) -> ActivationStatus {
  case error {
    client.ConnectionFailed(_transport_error)
    | client.RequestFailed(_code, _message)
    | client.ProtocolFailed(_message) -> ManualReloadRequired
  }
}

fn reload_status_from_control_file_error(
  error: control_file.ControlFileError,
) -> ActivationStatus {
  case error {
    control_file.ControlFileNotFound(_path)
    | control_file.ControlFileReadFailed(_path, _message)
    | control_file.ControlFileWriteFailed(_path, _message)
    | control_file.ControlFileInvalid(_path, _message)
    | control_file.ControlFilePermissionFailed(_path, _message)
    | control_file.TokenGenerationFailed(_message) -> ManualReloadRequired
  }
}

fn config_activation_status_name(status: ConfigActivationStatus) -> String {
  case status {
    ActivationNotRequested -> "not_requested"
    ConfigActivated(_) -> "config_updated"
    ConfigAlreadyActive(_) -> "already_active"
  }
}

fn config_activation_message(status: ConfigActivationStatus) -> String {
  case status {
    ActivationNotRequested ->
      "Project config was not changed; use --activate to write ui_server settings when you want the daemon to connect automatically."
    ConfigActivated(path) ->
      "Activated ui_server in "
      <> path
      <> "; daemon should now connect to the UI server after reload."
    ConfigAlreadyActive(path) ->
      "ui_server is already active in "
      <> path
      <> "; daemon should connect to the UI server after reload."
  }
}

fn reload_status_name(activation: ActivationStatus) -> String {
  case activation {
    ReloadNotified -> "reload_notified"
    ManualReloadRequired -> "manual_reload_required"
  }
}

fn reload_message(
  config_activation: ConfigActivationStatus,
  reload_status: ActivationStatus,
) -> String {
  case reload_status {
    ReloadNotified ->
      case config_activation {
        ActivationNotRequested ->
          "Notified the running daemon to reload stored UI pairing."
        _ ->
          "Hot-reloaded the running daemon; it should now connect to the UI server."
      }
    ManualReloadRequired ->
      case config_activation {
        ActivationNotRequested ->
          "Run scherzoctl reload or restart the daemon if ui_server is already enabled."
        _ ->
          "Run scherzoctl reload or restart the daemon to start the UI connection."
      }
  }
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

fn config_activation_error(error: ConfigActivationError) -> Error {
  case error {
    UiServerActivationConflict(message) ->
      Failed("ui_server_activation_conflict", message)
    UiServerActivationUnsupported(message) ->
      Failed("ui_server_activation_unsupported", message)
    UiServerActivationWriteFailed(path, message) ->
      Failed(
        "ui_server_activation_failed",
        "failed to update " <> path <> ": " <> message,
      )
  }
}

fn post_credential_config_activation_error(
  error: ConfigActivationError,
  store_path: String,
) -> Error {
  case error {
    UiServerActivationWriteFailed(path, message) ->
      Failed(
        "ui_server_activation_failed",
        "Stored daemon credential at "
          <> store_path
          <> ", but failed to activate ui_server in "
          <> path
          <> ": "
          <> message
          <> ". No credential secret was written to YAML. Fix the project config path or permissions, then rerun scherzo connect --activate with a fresh pairing token; add --replace-credential if the stored credential differs.",
      )
    _ -> config_activation_error(error)
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
