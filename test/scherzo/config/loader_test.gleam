import gleam/string
import gleeunit/should
import scherzo/config/loader
import scherzo/config/parser
import scherzo/config/types.{Auto, CommandGate}
import simplifile

// Test temporary directory helper
fn with_temp_dir(test_fn: fn(String) -> Nil) -> Nil {
  let temp_dir = "/tmp/scherzo-loader-test-" <> random_string()
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
// load tests - config file is source of truth
// -----------------------------------------------------------------------------

pub fn load_returns_default_when_no_config_test() {
  with_temp_dir(fn(dir) {
    let result = loader.load(dir)
    result |> should.be_ok

    let assert Ok(cfg) = result
    // Should return default config (no gates)
    cfg.gates.gates |> should.equal([])
    cfg.retry.strategy |> should.equal(Auto)
    cfg.retry.max_iterations |> should.equal(3)
    Nil
  })
}

pub fn load_parses_config_file_test() {
  with_temp_dir(fn(dir) {
    // Create config file with explicit gate
    let config_dir = dir <> "/.scherzo"
    let _ = simplifile.create_directory_all(config_dir)
    let _ =
      simplifile.write(
        config_dir <> "/config.toml",
        "
[gates]
[[gates.gates]]
type = \"command\"
name = \"tests\"
command = \"gleam test\"
timeout_ms = 300000
serial = true
fail_fast = false

[retry]
strategy = \"auto\"
max_iterations = 5
",
      )

    let result = loader.load(dir)
    result |> should.be_ok

    let assert Ok(cfg) = result
    // Should have the gate from config file
    let assert [gate] = cfg.gates.gates
    let assert CommandGate(name, command, timeout_ms, serial, fail_fast) = gate
    name |> should.equal("tests")
    command |> should.equal("gleam test")
    timeout_ms |> should.equal(300_000)
    serial |> should.equal(True)
    fail_fast |> should.equal(False)

    // Should have retry config from file
    cfg.retry.max_iterations |> should.equal(5)
    Nil
  })
}

pub fn load_returns_error_for_invalid_toml_test() {
  with_temp_dir(fn(dir) {
    // Create config file with invalid TOML
    let config_dir = dir <> "/.scherzo"
    let _ = simplifile.create_directory_all(config_dir)
    let _ =
      simplifile.write(config_dir <> "/config.toml", "this is not valid toml [")

    let result = loader.load(dir)
    result |> should.be_error

    let assert Error(loader.ParseError(_)) = result
    Nil
  })
}

pub fn load_returns_error_for_missing_required_field_test() {
  with_temp_dir(fn(dir) {
    // Create config file with missing required field
    let config_dir = dir <> "/.scherzo"
    let _ = simplifile.create_directory_all(config_dir)
    let _ =
      simplifile.write(
        config_dir <> "/config.toml",
        "
[gates]
[[gates.gates]]
type = \"command\"
# missing name and command
",
      )

    let result = loader.load(dir)
    result |> should.be_error
    Nil
  })
}

// -----------------------------------------------------------------------------
// format_error tests
// -----------------------------------------------------------------------------

pub fn format_error_parse_error_test() {
  let parse_err = loader.ParseError(parser.TomlError("bad syntax"))
  let msg = loader.format_error(parse_err)

  msg |> should.equal("TOML syntax error: bad syntax")
}
