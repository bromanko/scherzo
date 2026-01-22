/// Agent custom configuration loader
///
/// Loads custom agent configurations from `.scherzo/agents/<name>/` directory.
/// - CLAUDE.md: Custom instructions (replaces defaults)
/// - settings.json: Settings overrides (merged with auto-generated)
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import simplifile

/// Custom configuration for an agent loaded from .scherzo/agents/<name>/
pub type AgentCustomConfig {
  AgentCustomConfig(
    /// Agent name (e.g., "task", "code-review")
    name: String,
    /// Custom instructions from CLAUDE.md (replaces defaults)
    instructions: Option(String),
    /// Settings overrides from settings.json (raw JSON, merged with auto-generated)
    settings_overrides: Option(String),
  )
}

/// Returns an empty config (no customizations)
pub fn empty() -> AgentCustomConfig {
  AgentCustomConfig(name: "", instructions: None, settings_overrides: None)
}

/// Load custom config for any agent by name.
/// Returns empty config (not error) if directory doesn't exist.
/// Returns error only for I/O failures or malformed JSON.
/// The returned config's name is always set to agent_name.
pub fn load(
  repo_dir: String,
  agent_name: String,
) -> Result(AgentCustomConfig, String) {
  let agent_dir = repo_dir <> "/.scherzo/agents/" <> agent_name

  // Check if directory exists
  case simplifile.is_directory(agent_dir) {
    Ok(True) -> load_from_directory(agent_dir, agent_name)
    _ -> Ok(AgentCustomConfig(..empty(), name: agent_name))
  }
}

/// Convenience function for loading task agent config
pub fn load_task_config(repo_dir: String) -> Result(AgentCustomConfig, String) {
  load(repo_dir, "task")
}

/// Load config files from an existing directory
fn load_from_directory(
  agent_dir: String,
  agent_name: String,
) -> Result(AgentCustomConfig, String) {
  // Load CLAUDE.md (optional)
  let instructions = case simplifile.read(agent_dir <> "/CLAUDE.md") {
    Ok(content) -> Some(content)
    Error(_) -> None
  }

  // Load and validate settings.json (optional, but must be valid JSON if present)
  case load_settings(agent_dir) {
    Ok(settings_overrides) ->
      Ok(AgentCustomConfig(
        name: agent_name,
        instructions: instructions,
        settings_overrides: settings_overrides,
      ))
    Error(err) -> Error(err)
  }
}

/// Load settings.json and validate it's valid JSON
fn load_settings(agent_dir: String) -> Result(Option(String), String) {
  let settings_path = agent_dir <> "/settings.json"

  case simplifile.is_file(settings_path) {
    Ok(True) -> {
      use content <- result.try(
        simplifile.read(settings_path)
        |> result.map_error(fn(err) {
          "Failed to read settings.json: " <> simplifile_error_to_string(err)
        }),
      )
      use _ <- result.try(
        validate_json(content)
        |> result.map_error(fn(err) { "Invalid settings.json: " <> err }),
      )
      Ok(Some(content))
    }
    _ -> Ok(None)
  }
}

/// Validate that a string is valid JSON
fn validate_json(content: String) -> Result(Nil, String) {
  case json.parse(content, decode.dynamic) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("malformed JSON")
  }
}

/// Convert simplifile error to a human-readable string
fn simplifile_error_to_string(err: simplifile.FileError) -> String {
  case err {
    simplifile.Enoent -> "file not found"
    simplifile.Eacces -> "permission denied"
    simplifile.Enotdir -> "not a directory"
    simplifile.Eisdir -> "is a directory"
    simplifile.Eexist -> "already exists"
    simplifile.Eio -> "I/O error"
    // Many other variants exist; catch-all is intentional
    _ -> "unknown error"
  }
}
