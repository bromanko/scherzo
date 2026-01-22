import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import scherzo/config/agent_config
import simplifile

// Test temporary directory helper
fn with_temp_dir(test_fn: fn(String) -> Nil) -> Nil {
  let temp_dir = "/tmp/scherzo-agent-config-test-" <> random_string()
  let assert Ok(_) = simplifile.create_directory_all(temp_dir)
  test_fn(temp_dir)
  let _ = simplifile.delete(temp_dir)
  Nil
}

// Generate a simple random string for temp directories
fn random_string() -> String {
  erlang_unique_integer()
  |> int_to_string()
  |> string.replace("-", "")
}

@external(erlang, "erlang", "unique_integer")
fn erlang_unique_integer() -> Int

@external(erlang, "erlang", "integer_to_list")
fn int_to_string(i: Int) -> String

// -----------------------------------------------------------------------------
// empty() tests
// -----------------------------------------------------------------------------

pub fn empty_returns_empty_config_test() {
  let config = agent_config.empty()
  config.name |> should.equal("")
  config.instructions |> should.equal(None)
  config.settings_overrides |> should.equal(None)
}

// -----------------------------------------------------------------------------
// load() tests - directory handling
// -----------------------------------------------------------------------------

pub fn load_returns_empty_when_directory_missing_test() {
  with_temp_dir(fn(dir) {
    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    // Name is always set to agent_name, even when directory missing
    config.name |> should.equal("task")
    config.instructions |> should.equal(None)
    config.settings_overrides |> should.equal(None)
    Nil
  })
}

pub fn load_returns_empty_when_scherzo_dir_missing_test() {
  with_temp_dir(fn(dir) {
    // No .scherzo directory at all
    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(None)
    config.settings_overrides |> should.equal(None)
    Nil
  })
}

// -----------------------------------------------------------------------------
// load() tests - file loading
// -----------------------------------------------------------------------------

pub fn load_reads_both_files_when_present_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with both files
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ =
      simplifile.write(agent_dir <> "/CLAUDE.md", "# Custom Instructions\n")
    let _ =
      simplifile.write(agent_dir <> "/settings.json", "{\"model\": \"opus\"}")

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(Some("# Custom Instructions\n"))
    config.settings_overrides |> should.equal(Some("{\"model\": \"opus\"}"))
    Nil
  })
}

pub fn load_reads_only_claude_md_when_settings_missing_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with only CLAUDE.md
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ =
      simplifile.write(agent_dir <> "/CLAUDE.md", "# Custom Instructions\n")

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(Some("# Custom Instructions\n"))
    config.settings_overrides |> should.equal(None)
    Nil
  })
}

pub fn load_reads_only_settings_when_claude_md_missing_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with only settings.json
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ =
      simplifile.write(agent_dir <> "/settings.json", "{\"model\": \"opus\"}")

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(None)
    config.settings_overrides |> should.equal(Some("{\"model\": \"opus\"}"))
    Nil
  })
}

pub fn load_returns_empty_when_directory_exists_but_empty_test() {
  with_temp_dir(fn(dir) {
    // Create empty agent config directory
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(None)
    config.settings_overrides |> should.equal(None)
    Nil
  })
}

// -----------------------------------------------------------------------------
// load() tests - JSON validation
// -----------------------------------------------------------------------------

pub fn load_returns_error_for_malformed_json_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with invalid JSON
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ = simplifile.write(agent_dir <> "/settings.json", "{bad json")

    let result = agent_config.load(dir, "task")
    result |> should.be_error

    let assert Error(err) = result
    err |> string.contains("Invalid settings.json") |> should.be_true
    Nil
  })
}

pub fn load_accepts_valid_json_with_hooks_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with valid JSON containing hooks
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ =
      simplifile.write(
        agent_dir <> "/settings.json",
        "{\"hooks\": {\"PostToolUse\": [{\"matcher\": \"Edit\", \"hooks\": []}]}}",
      )

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.settings_overrides |> should.not_equal(None)
    Nil
  })
}

pub fn load_accepts_empty_json_object_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with empty JSON object
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ = simplifile.write(agent_dir <> "/settings.json", "{}")

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.settings_overrides |> should.equal(Some("{}"))
    Nil
  })
}

// -----------------------------------------------------------------------------
// load() tests - different agent names
// -----------------------------------------------------------------------------

pub fn load_works_with_different_agent_names_test() {
  with_temp_dir(fn(dir) {
    // Create config for code-review agent
    let agent_dir = dir <> "/.scherzo/agents/code-review"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ =
      simplifile.write(agent_dir <> "/CLAUDE.md", "# Code Review Instructions")

    let result = agent_config.load(dir, "code-review")
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("code-review")
    config.instructions |> should.equal(Some("# Code Review Instructions"))
    Nil
  })
}

// -----------------------------------------------------------------------------
// load_task_config() tests
// -----------------------------------------------------------------------------

pub fn load_task_config_loads_task_agent_test() {
  with_temp_dir(fn(dir) {
    // Create task agent config
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ =
      simplifile.write(agent_dir <> "/CLAUDE.md", "# Task Agent Instructions")

    let result = agent_config.load_task_config(dir)
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(Some("# Task Agent Instructions"))
    Nil
  })
}

pub fn load_task_config_returns_empty_when_missing_test() {
  with_temp_dir(fn(dir) {
    let result = agent_config.load_task_config(dir)
    result |> should.be_ok

    let assert Ok(config) = result
    config.name |> should.equal("task")
    config.instructions |> should.equal(None)
    config.settings_overrides |> should.equal(None)
    Nil
  })
}

// -----------------------------------------------------------------------------
// Edge case tests
// -----------------------------------------------------------------------------

pub fn load_handles_empty_claude_md_file_test() {
  with_temp_dir(fn(dir) {
    // Create agent config directory with empty CLAUDE.md
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ = simplifile.write(agent_dir <> "/CLAUDE.md", "")

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    // Empty file is still Some(""), not None
    config.instructions |> should.equal(Some(""))
    Nil
  })
}

pub fn load_accepts_json_array_test() {
  with_temp_dir(fn(dir) {
    // JSON array is technically valid JSON
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ = simplifile.write(agent_dir <> "/settings.json", "[1, 2, 3]")

    let result = agent_config.load(dir, "task")
    result |> should.be_ok

    let assert Ok(config) = result
    config.settings_overrides |> should.equal(Some("[1, 2, 3]"))
    Nil
  })
}

pub fn load_rejects_empty_settings_file_test() {
  with_temp_dir(fn(dir) {
    // Empty file is not valid JSON
    let agent_dir = dir <> "/.scherzo/agents/task"
    let _ = simplifile.create_directory_all(agent_dir)
    let _ = simplifile.write(agent_dir <> "/settings.json", "")

    let result = agent_config.load(dir, "task")
    result |> should.be_error

    let assert Error(err) = result
    err |> string.contains("Invalid settings.json") |> should.be_true
    Nil
  })
}
