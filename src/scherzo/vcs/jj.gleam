/// Jujutsu (jj) version control integration
import gleam/list
import gleam/result
import gleam/string
import shellout

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
/// Returns the workspace name (derived from path)
pub fn workspace_add(
  repo_dir: String,
  workspace_path: String,
) -> Result(String, String) {
  run_jj(repo_dir, ["workspace", "add", workspace_path])
  |> result.map(fn(_) {
    // Extract workspace name from path (last component)
    // Safe: split("/") always returns at least [""] for empty string,
    // so list.last never fails. Fallback to full path is reasonable.
    workspace_path
    |> string.split("/")
    |> list.last
    |> result.unwrap(workspace_path)
  })
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

/// Run a jj command and return the output
fn run_jj(working_dir: String, args: List(String)) -> Result(String, String) {
  case shellout.command(run: "jj", with: args, in: working_dir, opt: []) {
    Ok(output) -> Ok(output)
    Error(#(_, err_output)) -> Error(err_output)
  }
}
