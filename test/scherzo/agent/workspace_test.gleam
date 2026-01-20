import gleam/string
import gleeunit/should
import scherzo/agent/workspace
import simplifile

pub fn default_config_sets_workspaces_in_repo_test() {
  let config = workspace.default_config("/home/user/project")

  config.workspaces_base
  |> should.equal("/home/user/project/.scherzo/workspaces")
  config.repo_dir |> should.equal("/home/user/project")
}

pub fn temp_config_sets_workspaces_in_tmp_test() {
  let config = workspace.temp_config("/home/user/project")

  config.workspaces_base |> should.equal("/tmp/scherzo-workspaces")
  config.repo_dir |> should.equal("/home/user/project")
}

pub fn read_task_info_parses_valid_json_test() {
  // Create a temp directory with task.json
  let test_dir = "/tmp/claude/workspace_test_" <> random_suffix()
  let scherzo_dir = test_dir <> "/.scherzo"
  let task_json =
    "{\"id\":\"task-123\",\"title\":\"Test Task\",\"description\":\"A test\",\"repo_dir\":\"/repo\"}"

  let assert Ok(_) = simplifile.create_directory_all(scherzo_dir)
  let assert Ok(_) = simplifile.write(scherzo_dir <> "/task.json", task_json)

  let result = workspace.read_task_info(test_dir)

  result |> should.be_ok
  let assert Ok(info) = result
  info.id |> should.equal("task-123")
  info.title |> should.equal("Test Task")
  info.description |> should.equal("A test")
  info.repo_dir |> should.equal("/repo")

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn read_task_info_fails_for_missing_file_test() {
  let result = workspace.read_task_info("/tmp/nonexistent-workspace-dir")

  result |> should.be_error
}

pub fn read_task_info_fails_for_invalid_json_test() {
  let test_dir = "/tmp/claude/workspace_test_invalid_" <> random_suffix()
  let scherzo_dir = test_dir <> "/.scherzo"

  let assert Ok(_) = simplifile.create_directory_all(scherzo_dir)
  let assert Ok(_) =
    simplifile.write(scherzo_dir <> "/task.json", "not valid json")

  let result = workspace.read_task_info(test_dir)

  result |> should.be_error

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn read_task_info_fails_for_missing_fields_test() {
  let test_dir = "/tmp/claude/workspace_test_missing_" <> random_suffix()
  let scherzo_dir = test_dir <> "/.scherzo"
  // Missing required fields
  let task_json = "{\"id\":\"task-123\"}"

  let assert Ok(_) = simplifile.create_directory_all(scherzo_dir)
  let assert Ok(_) = simplifile.write(scherzo_dir <> "/task.json", task_json)

  let result = workspace.read_task_info(test_dir)

  result |> should.be_error

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn workspace_type_fields_test() {
  let ws =
    workspace.Workspace(
      task_id: "t-1",
      path: "/path/to/workspace",
      workspace_name: "ws-1",
      repo_dir: "/repo",
    )

  ws.task_id |> should.equal("t-1")
  ws.path |> should.equal("/path/to/workspace")
  ws.workspace_name |> should.equal("ws-1")
  ws.repo_dir |> should.equal("/repo")
}

fn random_suffix() -> String {
  // Use current time as a simple unique suffix
  let time = erlang_system_time()
  string.inspect(time)
}

@external(erlang, "os", "system_time")
fn erlang_system_time() -> Int

// Tests for sanitize_id security

pub fn sanitize_id_allows_valid_task_id_test() {
  workspace.sanitize_id("task-123") |> should.equal("task-123")
}

pub fn sanitize_id_allows_underscores_test() {
  workspace.sanitize_id("task_with_underscores") |> should.equal("task_with_underscores")
}

pub fn sanitize_id_allows_mixed_case_test() {
  workspace.sanitize_id("Task-ABC-123") |> should.equal("Task-ABC-123")
}

pub fn sanitize_id_rejects_path_traversal_test() {
  workspace.sanitize_id("task-../../../tmp") |> should.equal("invalid-task-id")
}

pub fn sanitize_id_rejects_double_dots_test() {
  workspace.sanitize_id("..") |> should.equal("invalid-task-id")
}

pub fn sanitize_id_rejects_absolute_path_test() {
  workspace.sanitize_id("/etc/passwd") |> should.equal("invalid-task-id")
}

pub fn sanitize_id_rejects_dot_only_test() {
  workspace.sanitize_id(".") |> should.equal("invalid-task-id")
}

pub fn sanitize_id_rejects_empty_string_test() {
  workspace.sanitize_id("") |> should.equal("invalid-task-id")
}

pub fn sanitize_id_replaces_spaces_test() {
  workspace.sanitize_id("task with spaces") |> should.equal("task-with-spaces")
}

pub fn sanitize_id_replaces_special_chars_test() {
  workspace.sanitize_id("task:with;special!chars")
  |> should.equal("task-with-special-chars")
}

pub fn sanitize_id_collapses_multiple_dashes_test() {
  workspace.sanitize_id("task//with///slashes")
  |> should.equal("task-with-slashes")
}

pub fn sanitize_id_handles_unicode_test() {
  // Unicode characters should be replaced with dashes, trailing dashes trimmed
  workspace.sanitize_id("task-日本語") |> should.equal("task")
}

pub fn sanitize_id_trims_trailing_dashes_test() {
  // Trailing dashes should be trimmed
  workspace.sanitize_id("task---") |> should.equal("task")
}

pub fn sanitize_id_trims_leading_dashes_test() {
  // Leading dashes should be trimmed
  workspace.sanitize_id("---task") |> should.equal("task")
}

pub fn sanitize_id_returns_invalid_for_all_unicode_test() {
  // If input is entirely unicode, result would be empty, return placeholder
  workspace.sanitize_id("日本語") |> should.equal("invalid-task-id")
}
