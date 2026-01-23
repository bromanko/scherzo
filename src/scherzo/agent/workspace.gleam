/// Agent workspace management - creates isolated jj workspaces for each agent
///
/// Each agent runs in its own jj workspace, which provides:
/// - Isolated working directory with full project copy
/// - Per-agent .claude/settings.json for hook injection
/// - Clean separation of concurrent agent work
import gleam/dynamic/decode
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/claude_settings
import scherzo/config/agent_config.{type AgentCustomConfig}
import scherzo/config/settings_merger
import scherzo/core/task.{type Task}
import scherzo/core/types.{type Id}
import scherzo/vcs/jj
import simplifile

/// An isolated workspace for an agent
pub type Workspace {
  Workspace(
    /// The task ID this workspace is for
    task_id: Id,
    /// Full path to the workspace directory
    path: String,
    /// jj workspace name
    workspace_name: String,
    /// Original repository directory
    repo_dir: String,
  )
}

/// Configuration for workspace creation
pub type WorkspaceConfig {
  WorkspaceConfig(
    /// Base directory for all agent workspaces
    /// Default: <repo_dir>/.scherzo/workspaces
    /// Can be overridden to use /tmp or other location
    workspaces_base: String,
    /// Original repository directory
    repo_dir: String,
    /// Path/command to invoke scherzo (for hooks)
    /// In development: "cd /path/to/scherzo && gleam run --"
    /// In production: "/usr/local/bin/scherzo" or just "scherzo"
    scherzo_bin: String,
  )
}

/// Create a workspace config with default settings (workspaces in project dir)
pub fn default_config(repo_dir: String) -> WorkspaceConfig {
  default_config_with_scherzo_bin(repo_dir, "scherzo")
}

/// Create a workspace config with a custom scherzo binary path
pub fn default_config_with_scherzo_bin(
  repo_dir: String,
  scherzo_bin: String,
) -> WorkspaceConfig {
  WorkspaceConfig(
    workspaces_base: repo_dir <> "/.scherzo/workspaces",
    repo_dir: repo_dir,
    scherzo_bin: scherzo_bin,
  )
}

/// Create a workspace config with workspaces in /tmp
pub fn temp_config(repo_dir: String) -> WorkspaceConfig {
  WorkspaceConfig(
    workspaces_base: "/tmp/scherzo-workspaces",
    repo_dir: repo_dir,
    scherzo_bin: "scherzo",
  )
}

/// Create a new workspace for an agent task
pub fn create(
  config: WorkspaceConfig,
  task: Task,
  agent_id: String,
  custom_config: Option(AgentCustomConfig),
) -> Result(Workspace, String) {
  let workspace_path = config.workspaces_base <> "/" <> sanitize_id(task.id)

  // Create base directory
  use _ <- result.try(
    simplifile.create_directory_all(config.workspaces_base)
    |> result.map_error(fn(err) {
      "Failed to create workspaces directory: "
      <> simplifile.describe_error(err)
    }),
  )

  // Create jj workspace
  use workspace_name <- result.try(
    jj.workspace_add(config.repo_dir, workspace_path)
    |> result.map_error(fn(err) { "Failed to create jj workspace: " <> err }),
  )

  let workspace =
    Workspace(
      task_id: task.id,
      path: workspace_path,
      workspace_name: workspace_name,
      repo_dir: config.repo_dir,
    )

  // Configure workspace files (with cleanup on failure)
  configure_workspace(
    workspace,
    task,
    agent_id,
    custom_config,
    config.scherzo_bin,
  )
}

/// Configure workspace files, cleaning up on any failure
fn configure_workspace(
  workspace: Workspace,
  task: Task,
  agent_id: String,
  custom_config: Option(AgentCustomConfig),
  scherzo_bin: String,
) -> Result(Workspace, String) {
  let custom_settings =
    custom_config
    |> option.then(fn(cfg) { cfg.settings_overrides })

  // Write .scherzo/task.json
  use _ <- try_with_cleanup(
    workspace,
    "write_task_info",
    write_task_info(workspace, task),
  )

  // Write .claude/settings.json (merged with custom if present)
  use _ <- try_with_cleanup(
    workspace,
    "write_claude_settings",
    write_claude_settings(
      workspace,
      task.id,
      agent_id,
      custom_settings,
      scherzo_bin,
    ),
  )

  // Write CLAUDE.md if custom instructions present
  use _ <- try_with_cleanup(
    workspace,
    "write_claude_instructions",
    write_claude_instructions(workspace, custom_config),
  )

  Ok(workspace)
}

/// Helper to run an operation and cleanup workspace on failure
fn try_with_cleanup(
  workspace: Workspace,
  context: String,
  result: Result(a, String),
  next: fn(a) -> Result(b, String),
) -> Result(b, String) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> {
      cleanup_on_error(workspace, context)
      Error(err)
    }
  }
}

/// Clean up workspace on error, logging any cleanup failures
fn cleanup_on_error(workspace: Workspace, context: String) -> Nil {
  case destroy(workspace) {
    Ok(_) -> Nil
    Error(cleanup_err) ->
      io.println(
        "Warning: Failed to cleanup workspace after "
        <> context
        <> " error: "
        <> cleanup_err,
      )
  }
}

/// Destroy a workspace (forget from jj and delete files)
pub fn destroy(workspace: Workspace) -> Result(Nil, String) {
  // Restore files to their original state to prevent polluting commit history
  // Note: These may fail if the files didn't exist originally, which is fine
  let _ = jj.restore_file(workspace.path, ".claude/settings.json")
  let _ = jj.restore_file(workspace.path, "CLAUDE.md")

  // Forget the workspace from jj
  case jj.workspace_forget(workspace.repo_dir, workspace.workspace_name) {
    Error(err) -> Error("Failed to forget workspace: " <> err)
    Ok(_) -> {
      // Then delete the workspace directory
      case simplifile.delete(workspace.path) {
        Error(err) ->
          Error(
            "Failed to delete workspace directory: "
            <> simplifile.describe_error(err),
          )
        Ok(_) -> Ok(Nil)
      }
    }
  }
}

/// Write .claude/settings.json with agent-specific hooks
/// If custom_settings is provided, merges it with auto-generated settings
fn write_claude_settings(
  workspace: Workspace,
  task_id: Id,
  agent_id: String,
  custom_settings: Option(String),
  scherzo_bin: String,
) -> Result(Nil, String) {
  let claude_dir = workspace.path <> "/.claude"

  // Create .claude directory
  use _ <- result.try(
    simplifile.create_directory_all(claude_dir)
    |> result.map_error(fn(err) {
      "Failed to create .claude directory: " <> simplifile.describe_error(err)
    }),
  )

  // Generate base settings JSON with scherzo_bin path for hooks
  let base_settings =
    claude_settings.generate_autonomous_settings(task_id, agent_id, scherzo_bin)

  // Merge with custom settings if provided
  use settings_json <- result.try(case custom_settings {
    None -> Ok(base_settings)
    Some(custom) ->
      settings_merger.merge(base_settings, custom)
      |> result.map_error(fn(err) { "Failed to merge settings: " <> err })
  })

  // Write settings file
  simplifile.write(claude_dir <> "/settings.json", settings_json)
  |> result.map_error(fn(err) {
    "Failed to write settings.json: " <> simplifile.describe_error(err)
  })
}

/// Write CLAUDE.md to workspace root with custom instructions
/// Only writes if custom config has instructions
fn write_claude_instructions(
  workspace: Workspace,
  custom_config: Option(AgentCustomConfig),
) -> Result(Nil, String) {
  case custom_config {
    None -> Ok(Nil)
    Some(cfg) ->
      case cfg.instructions {
        None -> Ok(Nil)
        Some(instructions) -> {
          let claude_md_path = workspace.path <> "/CLAUDE.md"
          case simplifile.write(claude_md_path, instructions) {
            Error(err) ->
              Error(
                "Failed to write CLAUDE.md: " <> simplifile.describe_error(err),
              )
            Ok(_) -> Ok(Nil)
          }
        }
      }
  }
}

/// Write .scherzo/task.json with task information for the prime command
fn write_task_info(workspace: Workspace, task: Task) -> Result(Nil, String) {
  let scherzo_dir = workspace.path <> "/.scherzo"

  // Create .scherzo directory
  use _ <- result.try(
    simplifile.create_directory_all(scherzo_dir)
    |> result.map_error(fn(err) {
      "Failed to create .scherzo directory: " <> simplifile.describe_error(err)
    }),
  )

  // Generate task JSON
  let task_json =
    json.object([
      #("id", json.string(task.id)),
      #("title", json.string(task.title)),
      #("description", json.string(task.description)),
      #("repo_dir", json.string(workspace.repo_dir)),
    ])
    |> json.to_string

  // Write task file
  simplifile.write(scherzo_dir <> "/task.json", task_json)
  |> result.map_error(fn(err) {
    "Failed to write task.json: " <> simplifile.describe_error(err)
  })
}

/// Task info stored in workspace
pub type TaskInfo {
  TaskInfo(id: Id, title: String, description: String, repo_dir: String)
}

/// Read task info from a workspace directory (used by scherzo prime)
pub fn read_task_info(workspace_path: String) -> Result(TaskInfo, String) {
  let task_path = workspace_path <> "/.scherzo/task.json"

  case simplifile.read(task_path) {
    Error(err) ->
      Error("Failed to read task.json: " <> simplifile.describe_error(err))
    Ok(content) -> parse_task_info(content)
  }
}

/// Parse task info from JSON
fn parse_task_info(content: String) -> Result(TaskInfo, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use title <- decode.field("title", decode.string)
    use description <- decode.field("description", decode.string)
    use repo_dir <- decode.field("repo_dir", decode.string)
    decode.success(TaskInfo(
      id: id,
      title: title,
      description: description,
      repo_dir: repo_dir,
    ))
  }

  case json.parse(content, decoder) {
    Error(_) -> Error("Failed to parse task.json")
    Ok(info) -> Ok(info)
  }
}

/// Sanitize a task ID for use in filesystem paths
/// Uses whitelist approach: only alphanumeric, dash, and underscore allowed
/// Rejects path traversal attempts and absolute paths
/// Trims leading/trailing dashes that may result from unicode replacement
pub fn sanitize_id(id: Id) -> String {
  // First check for dangerous patterns
  case is_dangerous_path(id) {
    True -> "invalid-task-id"
    False -> {
      let sanitized =
        id
        |> string.to_graphemes
        |> list.map(fn(c) {
          case is_safe_char(c) {
            True -> c
            False -> "-"
          }
        })
        |> string.join("")
        |> collapse_dashes
        |> trim_dashes
      // If the result is empty after sanitization, return a placeholder
      case sanitized {
        "" -> "invalid-task-id"
        _ -> sanitized
      }
    }
  }
}

/// Check for dangerous path patterns
fn is_dangerous_path(path: String) -> Bool {
  // Reject absolute paths (starts with /)
  string.starts_with(path, "/")
  // Reject .. path components
  || string.contains(path, "..")
  // Reject . path components that could be used for traversal
  || path == "."
  // Reject empty
  || string.is_empty(path)
}

/// Check if a character is safe for filesystem paths
fn is_safe_char(c: String) -> Bool {
  case c {
    // Lowercase letters
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    // Uppercase letters
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    // Numbers
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    // Safe punctuation
    "-" | "_" -> True
    // Everything else is unsafe
    _ -> False
  }
}

/// Collapse multiple consecutive dashes into a single dash
fn collapse_dashes(s: String) -> String {
  case string.contains(s, "--") {
    True -> collapse_dashes(string.replace(s, "--", "-"))
    False -> s
  }
}

/// Trim leading and trailing dashes from a string
fn trim_dashes(s: String) -> String {
  s
  |> trim_start_char("-")
  |> trim_end_char("-")
}

/// Recursively trim a character from the start of a string
fn trim_start_char(s: String, char: String) -> String {
  case string.starts_with(s, char) {
    True -> trim_start_char(string.drop_start(s, 1), char)
    False -> s
  }
}

/// Recursively trim a character from the end of a string
fn trim_end_char(s: String, char: String) -> String {
  case string.ends_with(s, char) {
    True -> trim_end_char(string.drop_end(s, 1), char)
    False -> s
  }
}
