import gleam/string
import gleeunit/should
import scherzo/vcs/jj

// Note: These tests run against the actual scherzo repository
// They only use read-only operations to avoid modifying state

pub fn is_repo_returns_true_for_jj_repo_test() {
  // Current directory should be a jj repo
  let result = jj.is_repo(".")

  result |> should.be_true
}

pub fn is_repo_returns_false_for_non_repo_test() {
  // /tmp is unlikely to be a jj repo
  let result = jj.is_repo("/tmp")

  result |> should.be_false
}

pub fn get_current_change_returns_change_id_test() {
  let result = jj.get_current_change(".")

  result |> should.be_ok
  let assert Ok(change_id) = result
  // Change IDs are alphanumeric strings
  change_id |> string.length |> fn(len) { len > 0 } |> should.be_true
}

pub fn status_returns_output_test() {
  let result = jj.status(".")

  result |> should.be_ok
}

pub fn diff_returns_output_for_current_change_test() {
  // First get the current change ID
  let assert Ok(change_id) = jj.get_current_change(".")

  // Then get the diff (may be empty, but shouldn't error)
  let result = jj.diff(".", change_id)

  result |> should.be_ok
}

pub fn workspace_list_returns_output_test() {
  let result = jj.workspace_list(".")

  result |> should.be_ok
  let assert Ok(output) = result
  // Should contain "default" workspace at minimum
  output |> string.contains("default") |> should.be_true
}

pub fn get_current_change_fails_for_non_repo_test() {
  let result = jj.get_current_change("/tmp")

  result |> should.be_error
}

pub fn status_fails_for_non_repo_test() {
  let result = jj.status("/tmp")

  result |> should.be_error
}

// extract_workspace_name tests

pub fn extract_workspace_name_normal_path_test() {
  jj.extract_workspace_name("/foo/bar/task-123")
  |> should.be_ok
  |> should.equal("task-123")
}

pub fn extract_workspace_name_no_slashes_test() {
  jj.extract_workspace_name("task-123")
  |> should.be_ok
  |> should.equal("task-123")
}

pub fn extract_workspace_name_trailing_slash_test() {
  jj.extract_workspace_name("/foo/bar/")
  |> should.be_ok
  |> should.equal("bar")
}

pub fn extract_workspace_name_multiple_trailing_slashes_test() {
  jj.extract_workspace_name("/foo/bar///")
  |> should.be_ok
  |> should.equal("bar")
}

pub fn extract_workspace_name_empty_path_fails_test() {
  jj.extract_workspace_name("")
  |> should.be_error
}

pub fn extract_workspace_name_root_only_fails_test() {
  jj.extract_workspace_name("/")
  |> should.be_error
}

pub fn extract_workspace_name_multiple_slashes_only_fails_test() {
  jj.extract_workspace_name("///")
  |> should.be_error
}
