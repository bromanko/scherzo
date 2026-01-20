/// Jujutsu (jj) version control integration
import gleam/list
import gleam/result
import gleam/string
import scherzo/agent/driver.{type AgentResult}
import scherzo/core/shell
import scherzo/core/task.{type Task}

/// Maximum length for task titles in jj descriptions
const max_title_length = 200

/// A jj change identifier
pub type ChangeId =
  String

/// Create a new jj change for a task
pub fn new_change(
  working_dir: String,
  message: String,
) -> Result(ChangeId, String) {
  run_jj(working_dir, ["new", "-m", message])
  |> result.try(fn(_) { get_current_change(working_dir) })
}

/// Get the current change ID
pub fn get_current_change(working_dir: String) -> Result(ChangeId, String) {
  run_jj(working_dir, [
    "log",
    "-r",
    "@",
    "-T",
    "change_id",
    "--no-graph",
    "-n",
    "1",
  ])
  |> result.map(string.trim)
}

/// Describe the current change with a new message
pub fn describe(working_dir: String, message: String) -> Result(Nil, String) {
  run_jj(working_dir, ["describe", "-m", message])
  |> result.map(fn(_) { Nil })
}

/// Get the diff for a specific change
pub fn diff(working_dir: String, change_id: ChangeId) -> Result(String, String) {
  run_jj(working_dir, ["diff", "-r", change_id])
}

/// Get the current status
pub fn status(working_dir: String) -> Result(String, String) {
  run_jj(working_dir, ["status"])
}

/// Check if the working directory is a jj repository
pub fn is_repo(working_dir: String) -> Bool {
  case run_jj(working_dir, ["status"]) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Edit a specific change (make it the working copy)
pub fn edit(working_dir: String, change_id: ChangeId) -> Result(Nil, String) {
  run_jj(working_dir, ["edit", change_id])
  |> result.map(fn(_) { Nil })
}

/// Create a new jj workspace at the specified path
/// Returns the workspace name (derived from path's last component)
pub fn workspace_add(
  repo_dir: String,
  workspace_path: String,
) -> Result(String, String) {
  use _ <- result.try(run_jj(repo_dir, ["workspace", "add", workspace_path]))
  extract_workspace_name(workspace_path)
}

/// Extract workspace name from a path (last non-empty component)
pub fn extract_workspace_name(path: String) -> Result(String, String) {
  path
  |> string.split("/")
  |> list.filter(fn(s) { s != "" })
  |> list.last
  |> result.replace_error(
    "Invalid workspace path: no name component in '" <> path <> "'",
  )
}

/// Remove a jj workspace (forgets it without deleting files)
pub fn workspace_forget(
  repo_dir: String,
  workspace_name: String,
) -> Result(Nil, String) {
  run_jj(repo_dir, ["workspace", "forget", workspace_name])
  |> result.map(fn(_) { Nil })
}

/// List all workspaces in a repository
pub fn workspace_list(repo_dir: String) -> Result(String, String) {
  run_jj(repo_dir, ["workspace", "list"])
}

/// Restore a file to its state from the parent commit
/// This undoes any modifications to the file in the working copy
pub fn restore_file(
  working_dir: String,
  file_path: String,
) -> Result(Nil, String) {
  run_jj(working_dir, ["restore", "--from", "@-", file_path])
  |> result.map(fn(_) { Nil })
}

/// Run a jj command and return the output (with 60s timeout)
fn run_jj(working_dir: String, args: List(String)) -> Result(String, String) {
  shell.run_with_timeout("jj", args, working_dir, shell.jj_timeout_ms)
  |> shell.to_result("jj")
}

// ---------------------------------------------------------------------------
// Task Completion Descriptions
// ---------------------------------------------------------------------------

/// Update the jj change description based on task completion result
/// This is the shared implementation used by both process.gleam and orchestrator.gleam
pub fn describe_task_completion(
  working_dir: String,
  task: Task,
  result: AgentResult,
) -> Result(Nil, String) {
  let safe_title = sanitize_title(task.title)
  let description = case result {
    driver.Success(_output) ->
      safe_title <> "\n\nCompleted successfully.\n\nTask: " <> task.id

    driver.Failure(reason, _) ->
      "FAILED: "
      <> safe_title
      <> "\n\nError: "
      <> reason
      <> "\n\nTask: "
      <> task.id

    driver.ContextExhausted(_) ->
      "WIP: "
      <> safe_title
      <> " (context exhausted, needs continuation)\n\nTask: "
      <> task.id

    driver.Interrupted ->
      "INTERRUPTED: " <> safe_title <> "\n\nTask: " <> task.id
  }

  describe(working_dir, description)
}

/// Sanitize a task title for safe use in jj descriptions
/// - Replaces newlines, carriage returns, and tabs with spaces
/// - Truncates to max_title_length characters
/// - Trims leading/trailing whitespace
pub fn sanitize_title(title: String) -> String {
  title
  |> string.replace("\n", " ")
  |> string.replace("\r", " ")
  |> string.replace("\t", " ")
  |> string.trim
  |> string.slice(0, max_title_length)
}
