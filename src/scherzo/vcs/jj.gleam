/// Jujutsu (jj) version control integration

import gleam/result
import gleam/string
import shellout

/// A jj change identifier
pub type ChangeId =
  String

/// Create a new jj change for a task
pub fn new_change(working_dir: String, message: String) -> Result(ChangeId, String) {
  run_jj(working_dir, ["new", "-m", message])
  |> result.try(fn(_) { get_current_change(working_dir) })
}

/// Get the current change ID
pub fn get_current_change(working_dir: String) -> Result(ChangeId, String) {
  run_jj(working_dir, ["log", "-r", "@", "-T", "change_id", "--no-graph", "-n", "1"])
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

/// Run a jj command and return the output
fn run_jj(working_dir: String, args: List(String)) -> Result(String, String) {
  case
    shellout.command(
      run: "jj",
      with: args,
      in: working_dir,
      opt: [],
    )
  {
    Ok(output) -> Ok(output)
    Error(#(_, err_output)) -> Error(err_output)
  }
}
