import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/config/duration_config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/workspace_driver_env
import yay

const simplified_schema_doc = "docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md"

pub fn resolve(
  workspace: yay.Node,
) -> Result(config_types.WorkspaceHookProfiles, error.ConfigError) {
  use _ <- result.try(reject_removed_public_keys(workspace))
  use configured_drivers <- result.try(read_configured_workspace_drivers(
    workspace,
  ))
  use default_driver <- result.try(resolve_workspace_driver_name(
    workspace,
    configured_drivers,
  ))
  use profiles <- result.try(ensure_selected_builtin_driver(
    configured_drivers,
    default_driver,
  ))
  Ok(config_types.WorkspaceHookProfiles(
    default_profile: default_driver,
    profiles: profiles,
  ))
}

pub fn reject_removed_public_keys(
  workspace: yay.Node,
) -> Result(Nil, error.ConfigError) {
  use _ <- result.try(reject_removed_workspace_key(
    workspace,
    "default_profile",
    "workspace.default_profile",
    "workspace.driver",
    "Rename the selected workspace driver to workspace.driver.",
  ))
  use _ <- result.try(reject_removed_workspace_key(
    workspace,
    "profiles",
    "workspace.profiles",
    "workspace.drivers",
    "Move named workspace driver definitions to workspace.drivers.",
  ))
  use _ <- result.try(reject_removed_workspace_key(
    workspace,
    "hooks",
    "workspace.hooks",
    "workspace.drivers.<name>.type: custom",
    "Top-level workspace lifecycle hooks were removed; use a custom workspace driver command instead.",
  ))
  Ok(Nil)
}

fn reject_removed_workspace_key(
  node: yay.Node,
  key: String,
  old_path: String,
  replacement: String,
  hint: String,
) -> Result(Nil, error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(Nil)
    Some(_) -> Error(removed_workspace_key_error(old_path, replacement, hint))
  }
}

fn removed_workspace_key_error(
  old_path: String,
  replacement: String,
  hint: String,
) -> error.ConfigError {
  error.InvalidConfig(
    old_path
    <> " was removed. Use "
    <> replacement
    <> ". "
    <> hint
    <> " See "
    <> simplified_schema_doc
    <> ".",
  )
}

fn read_configured_workspace_drivers(
  workspace: yay.Node,
) -> Result(
  dict.Dict(String, config_types.WorkspaceHookProfile),
  error.ConfigError,
) {
  case get_node(workspace, "drivers") {
    None -> Ok(dict.new())
    Some(yay.NodeMap(entries)) -> read_workspace_driver_entries(entries, [])
    Some(_) -> Error(error.InvalidConfig("workspace.drivers must be a map"))
  }
}

fn read_workspace_driver_entries(
  entries: List(#(yay.Node, yay.Node)),
  acc: List(#(String, config_types.WorkspaceHookProfile)),
) -> Result(
  dict.Dict(String, config_types.WorkspaceHookProfile),
  error.ConfigError,
) {
  case entries {
    [] -> Ok(dict.from_list(list.reverse(acc)))
    [#(yay.NodeStr(name), node), ..rest] -> {
      let path = "workspace.drivers." <> name
      use _ <- result.try(validate_driver_name(name, path))
      use profile <- result.try(read_workspace_driver_entry(name, node, path))
      read_workspace_driver_entries(rest, [#(name, profile), ..acc])
    }
    [#(_, _), ..] ->
      Error(error.InvalidConfig("workspace.drivers keys must be strings"))
  }
}

fn read_workspace_driver_entry(
  name: String,
  node: yay.Node,
  path: String,
) -> Result(config_types.WorkspaceHookProfile, error.ConfigError) {
  case node {
    yay.NodeMap(_) -> {
      use driver <- result.try(read_typed_workspace_driver(node, path))
      Ok(config_types.WorkspaceHookProfile(
        name: name,
        driver: Some(driver),
        source: config_types.ConfiguredWorkspaceDriver,
      ))
    }
    _ -> Error(error.InvalidConfig(path <> " must be a map"))
  }
}

fn read_typed_workspace_driver(
  node: yay.Node,
  path: String,
) -> Result(config_types.WorkspaceDriverConfig, error.ConfigError) {
  use _ <- result.try(reject_removed_workspace_driver_config_keys(node, path))
  use driver_type <- result.try(read_workspace_driver_type(node, path))
  case driver_type {
    "noop" ->
      read_builtin_workspace_driver(
        node,
        path,
        builtin_noop_driver_command(),
        noop_capabilities(),
      )
    "jj" -> read_jj_workspace_driver(node, path)
    "custom" -> read_custom_workspace_driver(node, path)
    other ->
      Error(error.InvalidConfig(
        path <> ".type must be one of noop, jj, or custom: " <> other,
      ))
  }
}

fn read_workspace_driver_type(
  node: yay.Node,
  path: String,
) -> Result(String, error.ConfigError) {
  case get_node(node, "type") {
    Some(yay.NodeStr(value)) -> Ok(value |> string.trim |> string.lowercase)
    Some(_) -> Error(error.InvalidConfig(path <> ".type must be a string"))
    None -> Error(error.InvalidConfig(path <> ".type is required"))
  }
}

fn read_builtin_workspace_driver(
  node: yay.Node,
  path: String,
  command: String,
  capabilities: List(config_types.WorkspaceCapability),
) -> Result(config_types.WorkspaceDriverConfig, error.ConfigError) {
  use _ <- result.try(reject_workspace_driver_command_key(node, path))
  use _ <- result.try(reject_jj_friendly_fields(node, path))
  use env <- result.try(read_workspace_driver_env(node, path <> ".env"))
  use timeout <- result.try(read_workspace_driver_timeout_ms(node, path))
  Ok(workspace_driver_config(command, env, timeout, capabilities))
}

fn read_jj_workspace_driver(
  node: yay.Node,
  path: String,
) -> Result(config_types.WorkspaceDriverConfig, error.ConfigError) {
  use _ <- result.try(reject_workspace_driver_command_key(node, path))
  use friendly_env <- result.try(read_jj_workspace_driver_env(node, path))
  use literal_env <- result.try(read_workspace_driver_env(node, path <> ".env"))
  use _ <- result.try(reject_workspace_driver_env_conflicts(
    friendly_env,
    literal_env,
    path,
  ))
  use timeout <- result.try(read_workspace_driver_timeout_ms(node, path))
  Ok(workspace_driver_config(
    builtin_jj_driver_command(),
    workspace_driver_env.canonicalize(list.append(friendly_env, literal_env)),
    timeout,
    jj_capabilities(),
  ))
}

fn read_custom_workspace_driver(
  node: yay.Node,
  path: String,
) -> Result(config_types.WorkspaceDriverConfig, error.ConfigError) {
  use _ <- result.try(reject_jj_friendly_fields(node, path))
  use command <- result.try(read_driver_command(node, path <> ".command"))
  use env <- result.try(read_workspace_driver_env(node, path <> ".env"))
  use timeout <- result.try(read_workspace_driver_timeout_ms(node, path))
  Ok(workspace_driver_config(command, env, timeout, []))
}

fn reject_removed_workspace_driver_config_keys(
  node: yay.Node,
  path: String,
) -> Result(Nil, error.ConfigError) {
  use _ <- result.try(reject_removed_workspace_key(
    node,
    "driver",
    path <> ".driver",
    path,
    "Move type, command, timeout, and env directly under " <> path <> ".",
  ))
  use _ <- result.try(reject_removed_workspace_key(
    node,
    "hooks",
    path <> ".hooks",
    path <> ".type: custom",
    "Workspace lifecycle hook config was removed; use a custom workspace driver command instead.",
  ))
  use _ <- result.try(reject_removed_workspace_key(
    node,
    "lifecycle",
    path <> ".lifecycle",
    path <> ".type: custom",
    "Lifecycle selection was removed from public config; workspace drivers should implement the lifecycle protocol.",
  ))
  use _ <- result.try(reject_removed_workspace_key(
    node,
    "capabilities",
    path <> ".capabilities",
    "driver describe --json",
    "Capabilities are discovered from the workspace driver at runtime.",
  ))
  Ok(Nil)
}

fn reject_workspace_driver_command_key(
  node: yay.Node,
  path: String,
) -> Result(Nil, error.ConfigError) {
  case get_node(node, "command") {
    None -> Ok(Nil)
    Some(_) ->
      Error(error.InvalidConfig(
        path
        <> ".command is only valid for type: custom. Use type: custom for command-based drivers. See "
        <> simplified_schema_doc
        <> ".",
      ))
  }
}

fn reject_jj_friendly_fields(
  node: yay.Node,
  path: String,
) -> Result(Nil, error.ConfigError) {
  case first_present_key(node, jj_friendly_driver_fields()) {
    None -> Ok(Nil)
    Some(key) ->
      Error(error.InvalidConfig(
        path
        <> "."
        <> key
        <> " is only valid for type: jj. See "
        <> simplified_schema_doc
        <> ".",
      ))
  }
}

fn read_workspace_driver_timeout_ms(
  node: yay.Node,
  path: String,
) -> Result(Int, error.ConfigError) {
  case get_node(node, "timeout_ms") {
    Some(_) ->
      Error(removed_workspace_key_error(
        path <> ".timeout_ms",
        path <> ".timeout",
        "Use a duration string such as 60s instead of bare milliseconds.",
      ))
    None -> duration_config.workspace_driver_timeout_ms(node, path)
  }
}

fn read_driver_command(
  node: yay.Node,
  path: String,
) -> Result(String, error.ConfigError) {
  case get_node(node, "command") {
    Some(yay.NodeStr(value)) ->
      config_types.validate_workspace_driver_command(value)
      |> result.map_error(fn(reason) {
        error.InvalidConfig(path <> " " <> reason)
      })
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string"))
    None -> Error(error.InvalidConfig(path <> " is required"))
  }
}

fn read_jj_workspace_driver_env(
  node: yay.Node,
  path: String,
) -> Result(List(#(String, String)), error.ConfigError) {
  use env <- result.try(
    read_optional_string_env_field(
      node,
      "remote",
      "SCHERZO_JJ_WORKSPACE_REMOTE",
      path,
      [],
    ),
  )
  use env <- result.try(read_optional_string_env_field(
    node,
    "base_branch",
    "SCHERZO_JJ_WORKSPACE_BASE_BRANCH",
    path,
    env,
  ))
  use env <- result.try(read_optional_string_env_field(
    node,
    "base",
    "SCHERZO_JJ_WORKSPACE_BASE",
    path,
    env,
  ))
  use env <- result.try(read_optional_bool_env_field(
    node,
    "fetch_base",
    "SCHERZO_JJ_WORKSPACE_FETCH_BASE",
    path,
    env,
  ))
  use env <- result.try(read_optional_string_env_field(
    node,
    "publish_remote",
    "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE",
    path,
    env,
  ))
  use env <- result.try(read_optional_string_env_field(
    node,
    "github_repo",
    "SCHERZO_GITHUB_REPO",
    path,
    env,
  ))
  Ok(workspace_driver_env.canonicalize(env))
}

fn read_optional_string_env_field(
  node: yay.Node,
  key: String,
  env_key: String,
  path: String,
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(acc)
    Some(yay.NodeStr(value)) -> Ok([#(env_key, value), ..acc])
    Some(_) ->
      Error(error.InvalidConfig(path <> "." <> key <> " must be a string"))
  }
}

fn read_optional_bool_env_field(
  node: yay.Node,
  key: String,
  env_key: String,
  path: String,
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(acc)
    Some(yay.NodeBool(value)) -> Ok([#(env_key, bool_to_string(value)), ..acc])
    Some(_) ->
      Error(error.InvalidConfig(path <> "." <> key <> " must be a boolean"))
  }
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn read_workspace_driver_env(
  node: yay.Node,
  path: String,
) -> Result(List(#(String, String)), error.ConfigError) {
  case get_node(node, "env") {
    None -> Ok([])
    Some(yay.NodeMap(entries)) ->
      read_workspace_driver_env_entries(entries, path, [], [])
    Some(_) -> Error(error.InvalidConfig(path <> " must be a map"))
  }
}

fn read_workspace_driver_env_entries(
  entries: List(#(yay.Node, yay.Node)),
  path: String,
  seen: List(String),
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), error.ConfigError) {
  case entries {
    [] -> Ok(workspace_driver_env.canonicalize(acc))
    [#(yay.NodeStr(key), value), ..rest] ->
      case list.contains(seen, key) {
        True ->
          Error(error.InvalidConfig(path <> " has duplicate key: " <> key))
        False -> {
          use pair <- result.try(read_workspace_driver_env_entry(
            key,
            value,
            path,
          ))
          read_workspace_driver_env_entries(rest, path, [key, ..seen], [
            pair,
            ..acc
          ])
        }
      }
    [#(_, _), ..] -> Error(error.InvalidConfig(path <> " keys must be strings"))
  }
}

fn read_workspace_driver_env_entry(
  key: String,
  value: yay.Node,
  path: String,
) -> Result(#(String, String), error.ConfigError) {
  case workspace_driver_env.valid_key(key) {
    False ->
      Error(error.InvalidConfig(
        path
        <> "."
        <> key
        <> " has invalid environment variable name; expected [A-Za-z_][A-Za-z0-9_]*",
      ))
    True ->
      case workspace_driver_env.reserved_generated_key(key) {
        True ->
          Error(error.InvalidConfig(
            path
            <> "."
            <> key
            <> " is reserved by Scherzo and cannot be configured in driver.env",
          ))
        False ->
          case value {
            yay.NodeStr(value) -> Ok(#(key, value))
            _ ->
              Error(error.InvalidConfig(
                path <> "." <> key <> " must be a string",
              ))
          }
      }
  }
}

fn reject_workspace_driver_env_conflicts(
  friendly_env: List(#(String, String)),
  literal_env: List(#(String, String)),
  path: String,
) -> Result(Nil, error.ConfigError) {
  case first_duplicate_env_key(friendly_env, literal_env) {
    None -> Ok(Nil)
    Some(key) ->
      Error(error.InvalidConfig(
        path
        <> ".env."
        <> key
        <> " duplicates a type: jj friendly field; configure either the friendly field or env, not both",
      ))
  }
}

fn first_duplicate_env_key(
  left: List(#(String, String)),
  right: List(#(String, String)),
) -> Option(String) {
  case left {
    [] -> None
    [#(key, _), ..rest] ->
      case env_has_key(right, key) {
        True -> Some(key)
        False -> first_duplicate_env_key(rest, right)
      }
  }
}

fn env_has_key(env: List(#(String, String)), key: String) -> Bool {
  list.any(env, fn(entry) {
    let #(candidate, _) = entry
    candidate == key
  })
}

fn resolve_workspace_driver_name(
  workspace: yay.Node,
  profiles: dict.Dict(String, config_types.WorkspaceHookProfile),
) -> Result(String, error.ConfigError) {
  case get_node(workspace, "driver") {
    None -> Ok(builtin_noop_driver_name())
    Some(yay.NodeStr(raw_driver)) -> {
      let driver = string.trim(raw_driver)
      use _ <- result.try(validate_driver_name(driver, "workspace.driver"))
      case dict.has_key(profiles, driver) || is_builtin_driver_name(driver) {
        True -> Ok(driver)
        False ->
          Error(error.InvalidConfig(
            "workspace.driver references unknown driver: " <> driver,
          ))
      }
    }
    Some(_) -> Error(error.InvalidConfig("workspace.driver must be a string"))
  }
}

fn ensure_selected_builtin_driver(
  profiles: dict.Dict(String, config_types.WorkspaceHookProfile),
  driver: String,
) -> Result(
  dict.Dict(String, config_types.WorkspaceHookProfile),
  error.ConfigError,
) {
  case dict.has_key(profiles, driver) {
    True -> Ok(profiles)
    False ->
      case builtin_workspace_profile(driver) {
        Some(profile) -> Ok(dict.insert(profiles, driver, profile))
        None ->
          Error(error.InvalidConfig(
            "workspace.driver references unknown driver: " <> driver,
          ))
      }
  }
}

fn builtin_workspace_profile(
  name: String,
) -> Option(config_types.WorkspaceHookProfile) {
  case name {
    "noop" ->
      Some(builtin_workspace_profile_with(
        name,
        builtin_noop_driver_command(),
        noop_capabilities(),
      ))
    "jj" ->
      Some(builtin_workspace_profile_with(
        name,
        builtin_jj_driver_command(),
        jj_capabilities(),
      ))
    _ -> None
  }
}

fn builtin_workspace_profile_with(
  name: String,
  command: String,
  capabilities: List(config_types.WorkspaceCapability),
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: name,
    driver: Some(workspace_driver_config(
      command,
      [],
      default_workspace_driver_timeout_ms(),
      capabilities,
    )),
    source: config_types.ConfiguredWorkspaceDriver,
  )
}

fn workspace_driver_config(
  command: String,
  env: List(#(String, String)),
  timeout_ms: Int,
  capabilities: List(config_types.WorkspaceCapability),
) -> config_types.WorkspaceDriverConfig {
  config_types.WorkspaceDriverConfig(
    command: command,
    lifecycle: all_workspace_lifecycle_operations(),
    capabilities: capabilities,
    timeout_ms: timeout_ms,
    env: env,
  )
}

fn all_workspace_lifecycle_operations() -> List(
  config_types.WorkspaceLifecycleOperation,
) {
  [
    config_types.LifecycleCreate,
    config_types.LifecycleBeforeStep,
    config_types.LifecycleAfterStep,
    config_types.LifecycleRemove,
  ]
}

fn noop_capabilities() -> List(config_types.WorkspaceCapability) {
  [
    config_types.WorkspaceStatus,
    config_types.WorkspaceChangedFiles,
    config_types.WorkspaceAssertOnly,
  ]
}

fn jj_capabilities() -> List(config_types.WorkspaceCapability) {
  [
    config_types.WorkspaceStatus,
    config_types.WorkspaceDiff,
    config_types.WorkspaceChangedFiles,
    config_types.WorkspaceAssertOnly,
    config_types.WorkspaceBaseline,
    config_types.WorkspaceRefreshBase,
    config_types.WorkspacePublishChange,
    config_types.WorkspaceExportCommitStack,
  ]
}

fn default_workspace_driver_timeout_ms() -> Int {
  60_000
}

fn builtin_noop_driver_name() -> String {
  "noop"
}

fn builtin_jj_driver_name() -> String {
  "jj"
}

fn is_builtin_driver_name(name: String) -> Bool {
  name == builtin_noop_driver_name() || name == builtin_jj_driver_name()
}

fn builtin_noop_driver_command() -> String {
  "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop"
}

fn builtin_jj_driver_command() -> String {
  "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
}

fn jj_friendly_driver_fields() -> List(String) {
  [
    "remote",
    "base_branch",
    "base",
    "fetch_base",
    "publish_remote",
    "github_repo",
  ]
}

fn first_present_key(node: yay.Node, keys: List(String)) -> Option(String) {
  case keys {
    [] -> None
    [key, ..rest] ->
      case get_node(node, key) {
        Some(_) -> Some(key)
        None -> first_present_key(node, rest)
      }
  }
}

fn validate_driver_name(
  value: String,
  path: String,
) -> Result(Nil, error.ConfigError) {
  case valid_driver_name(value) {
    True -> Ok(Nil)
    False ->
      Error(error.InvalidConfig(path <> " has invalid driver name: " <> value))
  }
}

fn valid_driver_name(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] ->
      is_lower_or_digit(first) && all(rest, is_driver_name_char)
  }
}

fn is_driver_name_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_" || ch == "-"
}

fn is_lower_or_digit(ch: String) -> Bool {
  is_lower(ch) || is_digit(ch)
}

fn is_lower(ch: String) -> Bool {
  string.compare(ch, "a") != Lt && string.compare(ch, "z") != Gt
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != Lt && string.compare(ch, "9") != Gt
}

fn all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && all(rest, predicate)
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}
