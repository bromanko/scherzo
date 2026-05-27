import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/json_value
import scherzo/log
import scherzo/path
import scherzo/port
import scherzo/workspace_driver_command
import scherzo/workspace_driver_env

pub type DiscoveryError {
  DiscoveryError(profile_name: String, command: String, reason: String)
}

pub fn enrich_orchestrator(
  orchestrator: config_types.OrchestratorConfig,
) -> Result(config_types.OrchestratorConfig, DiscoveryError) {
  use profiles <- result.try(
    enrich_profile_entries(
      dict.to_list(orchestrator.workspace_profiles.profiles),
      orchestrator,
      [],
    ),
  )
  let workspace_profiles =
    config_types.WorkspaceHookProfiles(
      ..orchestrator.workspace_profiles,
      profiles: dict.from_list(profiles),
    )
  Ok(
    config_types.OrchestratorConfig(
      ..orchestrator,
      workspace_profiles: workspace_profiles,
    ),
  )
}

pub fn enrich_profile(
  profile: config_types.WorkspaceHookProfile,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(config_types.WorkspaceHookProfile, DiscoveryError) {
  case profile.driver {
    None -> Ok(profile)
    Some(driver) ->
      case driver.capabilities {
        [_, ..] -> Ok(profile)
        [] -> {
          use capabilities <- result.try(discover_capabilities(
            profile.name,
            driver,
            orchestrator,
          ))
          let driver =
            config_types.WorkspaceDriverConfig(
              ..driver,
              capabilities: capabilities,
            )
          Ok(config_types.WorkspaceHookProfile(..profile, driver: Some(driver)))
        }
      }
  }
}

pub fn error_code(error: DiscoveryError) -> String {
  case error {
    DiscoveryError(..) -> "workspace_driver_discovery_failed"
  }
}

pub fn error_message(error: DiscoveryError) -> String {
  case error {
    DiscoveryError(profile_name, command, reason) ->
      "workspace driver discovery failed for profile "
      <> profile_name
      <> " command "
      <> command
      <> ": "
      <> reason
  }
}

fn enrich_profile_entries(
  entries: List(#(String, config_types.WorkspaceHookProfile)),
  orchestrator: config_types.OrchestratorConfig,
  acc: List(#(String, config_types.WorkspaceHookProfile)),
) -> Result(List(#(String, config_types.WorkspaceHookProfile)), DiscoveryError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(name, profile), ..rest] -> {
      use profile <- result.try(enrich_profile(profile, orchestrator))
      enrich_profile_entries(rest, orchestrator, [#(name, profile), ..acc])
    }
  }
}

fn discover_capabilities(
  profile_name: String,
  driver: config_types.WorkspaceDriverConfig,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(List(config_types.WorkspaceCapability), DiscoveryError) {
  let command = resolve_command(driver.command, orchestrator)
  case
    port.start_argv(
      "env",
      [
        "-i",
        ..list.append(discovery_env_args(command, driver.env, orchestrator), [
          command,
          "describe",
          "--json",
        ])
      ],
      discovery_repo_root(orchestrator),
      [],
    )
  {
    Error(error) ->
      discovery_error(
        profile_name,
        driver.command,
        "could not start describe --json: " <> port.port_error_to_string(error),
        workspace_driver_env.values_for_redaction(driver.env),
      )
    Ok(process) ->
      read_description(
        profile_name,
        driver.command,
        process,
        driver.timeout_ms,
        workspace_driver_env.values_for_redaction(driver.env),
      )
  }
}

fn discovery_env_args(
  resolved_command: String,
  profile_env: List(#(String, String)),
  orchestrator: config_types.OrchestratorConfig,
) -> List(String) {
  let generated = [
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #(
      "SCHERZO_REPO_ROOT",
      workspace_driver_command.default_repo_root(orchestrator),
    ),
    #("SCHERZO_WORKSPACE_DRIVER", resolved_command),
  ]
  let base = case path.env("PATH") {
    Some(path_value) ->
      case path_value != "" && !env_has_key(profile_env, "PATH") {
        True -> [#("PATH", path_value)]
        False -> []
      }
    None -> []
  }
  list.append(base, workspace_driver_env.merge(profile_env, generated))
  |> list.map(fn(entry) {
    let #(key, value) = entry
    key <> "=" <> value
  })
}

fn resolve_command(
  command: String,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  workspace_driver_command.resolve(command, orchestrator)
}

fn discovery_repo_root(
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  workspace_driver_command.inferred_repo_root(orchestrator.config_dir)
}

fn read_description(
  profile_name: String,
  command: String,
  process: port.Process,
  timeout_ms: Int,
  redaction_values: List(String),
) -> Result(List(config_types.WorkspaceCapability), DiscoveryError) {
  case port.read_stdout_line(process, timeout_ms) {
    Ok(line) ->
      wait_for_description(
        profile_name,
        command,
        process,
        timeout_ms,
        line,
        redaction_values,
      )
    Error(port.ReadTimeout) -> {
      let _cleanup_result = port.terminate(process)
      discovery_error(
        profile_name,
        command,
        "describe --json timed out",
        redaction_values,
      )
    }
    Error(port.ProcessExited(0)) -> {
      let _cleanup_result = port.terminate(process)
      discovery_error(
        profile_name,
        command,
        "describe --json produced no stdout",
        redaction_values,
      )
    }
    Error(port.ProcessExited(status)) -> {
      let diagnostics = read_diagnostics(process)
      let _cleanup_result = port.terminate(process)
      discovery_error(
        profile_name,
        command,
        "describe --json exited "
          <> int.to_string(status)
          <> diagnostic_suffix(diagnostics),
        redaction_values,
      )
    }
    Error(error) -> {
      let _cleanup_result = port.terminate(process)
      discovery_error(
        profile_name,
        command,
        "could not read describe --json stdout: "
          <> port.port_error_to_string(error),
        redaction_values,
      )
    }
  }
}

fn wait_for_description(
  profile_name: String,
  command: String,
  process: port.Process,
  timeout_ms: Int,
  stdout_line: String,
  redaction_values: List(String),
) -> Result(List(config_types.WorkspaceCapability), DiscoveryError) {
  case port.await_exit(process, timeout_ms) {
    Ok(0) ->
      parse_description(stdout_line)
      |> result.map_error(fn(error) {
        DiscoveryError(
          profile_name,
          command,
          log.redact(
            "workspace_driver_discovery",
            description_error_message(error),
            redaction_values,
          ),
        )
      })
    Ok(status) -> {
      let diagnostics = read_diagnostics(process)
      discovery_error(
        profile_name,
        command,
        "describe --json exited "
          <> int.to_string(status)
          <> diagnostic_suffix(diagnostics),
        redaction_values,
      )
    }
    Error(port.ReadTimeout) -> {
      let _cleanup_result = port.terminate(process)
      discovery_error(
        profile_name,
        command,
        "describe --json timed out",
        redaction_values,
      )
    }
    Error(error) ->
      discovery_error(
        profile_name,
        command,
        "could not wait for describe --json: "
          <> port.port_error_to_string(error),
        redaction_values,
      )
  }
}

type DescriptionError {
  DescriptionError(message: String)
}

fn description_error_message(error: DescriptionError) -> String {
  case error {
    DescriptionError(message) -> message
  }
}

fn description_error(message: String) -> Result(a, DescriptionError) {
  Error(DescriptionError(message))
}

fn parse_description(
  stdout_line: String,
) -> Result(List(config_types.WorkspaceCapability), DescriptionError) {
  let stdout_line = string.trim(stdout_line)
  case stdout_line == "" {
    True -> description_error("describe --json produced empty stdout")
    False ->
      case json_value.parse(stdout_line) {
        Error(Nil) ->
          description_error("describe --json stdout must be valid JSON")
        Ok(json_value.JObject(entries)) -> {
          use version <- result.try(object_field(entries, "version"))
          use _ <- result.try(validate_version(version))
          use capabilities <- result.try(object_field(entries, "capabilities"))
          validate_capabilities(capabilities)
        }
        Ok(_) ->
          description_error("describe --json stdout must be one JSON object")
      }
  }
}

fn object_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Result(json_value.JsonValue, DescriptionError) {
  case entries {
    [] -> description_error("missing " <> key <> " field")
    [#(current, value), ..rest] ->
      case current == key {
        True -> Ok(value)
        False -> object_field(rest, key)
      }
  }
}

fn validate_version(
  value: json_value.JsonValue,
) -> Result(Nil, DescriptionError) {
  case value {
    json_value.JInt(1) -> Ok(Nil)
    json_value.JInt(version) ->
      description_error(
        "unsupported describe version: " <> int.to_string(version),
      )
    _ -> description_error("version must be integer 1")
  }
}

fn validate_capabilities(
  value: json_value.JsonValue,
) -> Result(List(config_types.WorkspaceCapability), DescriptionError) {
  case value {
    json_value.JArray(values) -> capability_values(values, [], [])
    _ -> description_error("capabilities must be a list of strings")
  }
}

fn capability_values(
  values: List(json_value.JsonValue),
  seen: List(config_types.WorkspaceCapability),
  acc: List(config_types.WorkspaceCapability),
) -> Result(List(config_types.WorkspaceCapability), DescriptionError) {
  case values {
    [] -> Ok(config_types.canonical_workspace_capabilities(acc))
    [json_value.JString(name), ..rest] ->
      case config_types.workspace_capability_from_string(name) {
        Error(Nil) -> description_error("unknown capability: " <> name)
        Ok(capability) ->
          case list.contains(seen, capability) {
            True ->
              description_error(
                "duplicate capability: "
                <> config_types.workspace_capability_to_string(capability),
              )
            False ->
              capability_values(rest, [capability, ..seen], [capability, ..acc])
          }
      }
    [_, ..] -> description_error("capabilities entries must be strings")
  }
}

fn read_diagnostics(process: port.Process) -> String {
  case port.read_diagnostics(process) {
    Ok(diagnostics) -> diagnostics |> string.trim |> log.truncate(4000)
    Error(error) ->
      "could not read diagnostics: " <> port.port_error_to_string(error)
  }
}

fn diagnostic_suffix(diagnostics: String) -> String {
  case string.trim(diagnostics) {
    "" -> ""
    diagnostics -> ": " <> diagnostics
  }
}

fn discovery_error(
  profile_name: String,
  command: String,
  reason: String,
  redaction_values: List(String),
) -> Result(a, DiscoveryError) {
  Error(DiscoveryError(
    profile_name,
    command,
    log.redact("workspace_driver_discovery", reason, redaction_values),
  ))
}

fn env_has_key(env: List(#(String, String)), key: String) -> Bool {
  case env {
    [] -> False
    [#(current, _), ..rest] -> current == key || env_has_key(rest, key)
  }
}
