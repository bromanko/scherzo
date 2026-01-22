import gleam/string
import gleeunit/should
import scherzo/cli/init
import simplifile

// Test temporary directory helper
fn with_temp_dir(test_fn: fn(String) -> Nil) -> Nil {
  let temp_dir = "/tmp/scherzo-init-test-" <> random_string()
  let _ = simplifile.create_directory_all(temp_dir)
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
// is_initialized tests
// -----------------------------------------------------------------------------

pub fn is_initialized_false_for_empty_dir_test() {
  with_temp_dir(fn(dir) { init.is_initialized(dir) |> should.be_false })
}

pub fn is_initialized_true_when_config_exists_test() {
  with_temp_dir(fn(dir) {
    // Create .scherzo/config.toml
    let config_dir = dir <> "/.scherzo"
    let _ = simplifile.create_directory_all(config_dir)
    let _ = simplifile.write(config_dir <> "/config.toml", "# test")

    init.is_initialized(dir) |> should.be_true
  })
}

// -----------------------------------------------------------------------------
// initialize tests
// -----------------------------------------------------------------------------

pub fn initialize_creates_config_file_test() {
  with_temp_dir(fn(dir) {
    let result = init.initialize(dir, [], False, False)
    result |> should.be_ok

    // Config file should exist
    let config_path = dir <> "/.scherzo/config.toml"
    simplifile.is_file(config_path) |> should.equal(Ok(True))

    // .tickets should not exist
    let tickets_path = dir <> "/.tickets"
    simplifile.is_directory(tickets_path) |> should.equal(Ok(False))
  })
}

pub fn initialize_creates_tickets_dir_when_requested_test() {
  with_temp_dir(fn(dir) {
    let result = init.initialize(dir, [], True, False)
    result |> should.be_ok

    // Both should exist
    simplifile.is_file(dir <> "/.scherzo/config.toml") |> should.equal(Ok(True))
    simplifile.is_directory(dir <> "/.tickets") |> should.equal(Ok(True))
  })
}

pub fn initialize_fails_when_already_initialized_test() {
  with_temp_dir(fn(dir) {
    // First initialization
    let _ = init.initialize(dir, [], False, False)

    // Second should fail
    let result = init.initialize(dir, ["code-review"], False, False)
    result |> should.be_error

    let assert Error(init.AlreadyInitialized(_)) = result
    Nil
  })
}

pub fn initialize_succeeds_with_force_flag_test() {
  with_temp_dir(fn(dir) {
    // First initialization with none
    let _ = init.initialize(dir, [], False, False)

    // Second with force should succeed
    let result = init.initialize(dir, ["code-review"], False, True)
    result |> should.be_ok

    // Config should contain code-review formula
    let config_path = dir <> "/.scherzo/config.toml"
    let assert Ok(content) = simplifile.read(config_path)
    content |> string.contains("code-review") |> should.be_true
  })
}

pub fn initialize_fails_for_invalid_template_test() {
  with_temp_dir(fn(dir) {
    let result = init.initialize(dir, ["invalid-template"], False, False)
    result |> should.be_error

    let assert Error(init.InvalidTemplate(name)) = result
    name |> should.equal("invalid-template")
  })
}

pub fn initialize_with_code_review_template_test() {
  with_temp_dir(fn(dir) {
    let result = init.initialize(dir, ["code-review"], False, False)
    result |> should.be_ok

    let config_path = dir <> "/.scherzo/config.toml"
    let assert Ok(content) = simplifile.read(config_path)

    // Should contain explicit gate definitions
    content |> string.contains("type = \"command\"") |> should.be_true
    content |> string.contains("name = \"tests\"") |> should.be_true
    content |> string.contains("command = \"gleam test\"") |> should.be_true
    // Should contain parallel review gate
    content |> string.contains("type = \"parallel-review\"") |> should.be_true
    content |> string.contains("name = \"code-review\"") |> should.be_true
  })
}

pub fn initialize_with_security_audit_template_test() {
  with_temp_dir(fn(dir) {
    let result = init.initialize(dir, ["security-audit"], False, False)
    result |> should.be_ok

    let config_path = dir <> "/.scherzo/config.toml"
    let assert Ok(content) = simplifile.read(config_path)

    // Should contain explicit gate definitions
    content |> string.contains("type = \"command\"") |> should.be_true
    content |> string.contains("name = \"tests\"") |> should.be_true
    // Should contain multi-pass review gate
    content |> string.contains("type = \"multi-pass-review\"") |> should.be_true
    content |> string.contains("name = \"security-audit\"") |> should.be_true
  })
}

pub fn initialize_with_quick_review_template_test() {
  with_temp_dir(fn(dir) {
    let result = init.initialize(dir, ["quick-review"], False, False)
    result |> should.be_ok

    let config_path = dir <> "/.scherzo/config.toml"
    let assert Ok(content) = simplifile.read(config_path)

    // Should contain explicit gate definitions
    content |> string.contains("type = \"command\"") |> should.be_true
    content |> string.contains("name = \"tests\"") |> should.be_true
    content |> string.contains("name = \"check\"") |> should.be_true
    content |> string.contains("fail_fast = true") |> should.be_true
  })
}

pub fn initialize_with_multiple_templates_test() {
  with_temp_dir(fn(dir) {
    let result =
      init.initialize(dir, ["code-review", "security-audit"], False, False)
    result |> should.be_ok

    let config_path = dir <> "/.scherzo/config.toml"
    let assert Ok(content) = simplifile.read(config_path)

    // Should contain gates from both templates
    content |> string.contains("type = \"parallel-review\"") |> should.be_true
    content |> string.contains("name = \"code-review\"") |> should.be_true
    content |> string.contains("type = \"multi-pass-review\"") |> should.be_true
    content |> string.contains("name = \"security-audit\"") |> should.be_true
  })
}

// -----------------------------------------------------------------------------
// format_error tests
// -----------------------------------------------------------------------------

pub fn format_error_already_initialized_test() {
  let error = init.AlreadyInitialized("/path/.scherzo/config.toml")
  let msg = init.format_error(error)

  msg |> string.contains("Already initialized") |> should.be_true
  msg |> string.contains("--force") |> should.be_true
}

pub fn format_error_invalid_template_test() {
  let error = init.InvalidTemplate("bad-template")
  let msg = init.format_error(error)

  msg |> string.contains("Invalid template 'bad-template'") |> should.be_true
  msg |> string.contains("code-review") |> should.be_true
}

pub fn format_error_filesystem_error_test() {
  let error = init.FilesystemError("Permission denied")
  let msg = init.format_error(error)

  msg |> string.contains("Filesystem error") |> should.be_true
  msg |> string.contains("Permission denied") |> should.be_true
}
