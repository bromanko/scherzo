/// Agent workspace management - creates isolated jj workspaces for each agent
///
/// Each agent runs in its own jj workspace, which provides:
/// - Isolated working directory with full project copy
/// - Per-agent .claude/settings.json for hook injection
/// - Clean separation of concurrent agent work
import gleam/dynamic/decode
import gleam/json
import gleam/string
import scherzo/agent/claude_settings
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
  )
}

/// Create a workspace config with default settings (workspaces in project dir)
pub fn default_config(repo_dir: String) -> WorkspaceConfig {
  WorkspaceConfig(
    workspaces_base: repo_dir <> "/.scherzo/workspaces",
    repo_dir: repo_dir,
  )
}

/// Create a workspace config with workspaces in /tmp
pub fn temp_config(repo_dir: String) -> WorkspaceConfig {
  WorkspaceConfig(
    workspaces_base: "/tmp/scherzo-workspaces",
    repo_dir: repo_dir,
  )
}

/// Create a new workspace for an agent task
pub fn create(config: WorkspaceConfig, task: Task) -> Result(Workspace, String) {
  // Create workspace path: <base>/<task_id>
  let workspace_path = config.workspaces_base <> "/" <> sanitize_id(task.id)

  // Ensure the base directory exists
  case simplifile.create_directory_all(config.workspaces_base) {
    Error(err) ->
      Error(
        "Failed to create workspaces directory: "
        <> simplifile.describe_error(err),
      )
    Ok(_) -> {
      // Create jj workspace
      case jj.workspace_add(config.repo_dir, workspace_path) {
        Error(err) -> Error("Failed to create jj workspace: " <> err)
        Ok(workspace_name) -> {
          let workspace =
            Workspace(
              task_id: task.id,
              path: workspace_path,
              workspace_name: workspace_name,
              repo_dir: config.repo_dir,
            )

          // Write the .scherzo/task.json with task info
          case write_task_info(workspace, task) {
            Error(err) -> {
              let _ = destroy(workspace)
              Error(err)
            }
            Ok(_) -> {
              // Write the .claude/settings.json with hooks
              case write_claude_settings(workspace, task.id) {
                Error(err) -> {
                  let _ = destroy(workspace)
                  Error(err)
                }
                Ok(_) -> Ok(workspace)
              }
            }
          }
        }
      }
    }
  }
}

/// Destroy a workspace (forget from jj and delete files)
pub fn destroy(workspace: Workspace) -> Result(Nil, String) {
  // First, forget the workspace from jj
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
fn write_claude_settings(
  workspace: Workspace,
  task_id: Id,
) -> Result(Nil, String) {
  let claude_dir = workspace.path <> "/.claude"

  // Create .claude directory
  case simplifile.create_directory_all(claude_dir) {
    Error(err) ->
      Error(
        "Failed to create .claude directory: " <> simplifile.describe_error(err),
      )
    Ok(_) -> {
      // Generate settings JSON
      let settings_json = claude_settings.generate_autonomous_settings(task_id)
      let settings_path = claude_dir <> "/settings.json"

      // Write settings file
      case simplifile.write(settings_path, settings_json) {
        Error(err) ->
          Error(
            "Failed to write settings.json: " <> simplifile.describe_error(err),
          )
        Ok(_) -> Ok(Nil)
      }
    }
  }
}

/// Write .scherzo/task.json with task information for the prime command
fn write_task_info(workspace: Workspace, task: Task) -> Result(Nil, String) {
  let scherzo_dir = workspace.path <> "/.scherzo"

  // Create .scherzo directory
  case simplifile.create_directory_all(scherzo_dir) {
    Error(err) ->
      Error(
        "Failed to create .scherzo directory: "
        <> simplifile.describe_error(err),
      )
    Ok(_) -> {
      // Generate task JSON
      let task_json =
        json.object([
          #("id", json.string(task.id)),
          #("title", json.string(task.title)),
          #("description", json.string(task.description)),
          #("repo_dir", json.string(workspace.repo_dir)),
        ])
        |> json.to_string

      let task_path = scherzo_dir <> "/task.json"

      // Write task file
      case simplifile.write(task_path, task_json) {
        Error(err) ->
          Error("Failed to write task.json: " <> simplifile.describe_error(err))
        Ok(_) -> Ok(Nil)
      }
    }
  }
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
fn sanitize_id(id: Id) -> String {
  id
  |> string.replace("/", "-")
  |> string.replace(" ", "-")
  |> string.replace(":", "-")
}
