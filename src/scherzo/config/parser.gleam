/// Config parser for .scherzo/config.toml
///
/// Parses TOML configuration files into ScherzoConfig structures.
/// Handles gate definitions, retry settings, and review dimensions.
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import scherzo/config/types.{
  type GateConfig, type GatesConfig, type RetryConfig, type RetryStrategy,
  type ReviewDimension, type ReviewPass, type ScherzoConfig, Auto,
  CommandGate, FreshAgent, GatesConfig, HumanGate, MultiPassReviewGate,
  ParallelReviewGate, RetryConfig, ReviewDimension, ReviewPass, SameAgent,
  ScherzoConfig,
}
import simplifile
import tom.{type Toml}

/// Error types for config parsing
pub type ParseError {
  /// File not found or couldn't be read
  FileError(path: String, reason: String)
  /// TOML syntax error
  TomlError(message: String)
  /// Missing required field
  MissingField(path: String, field: String)
  /// Wrong type for field
  WrongType(path: String, expected: String, got: String)
  /// Unknown gate type
  UnknownGateType(name: String, gate_type: String)
  /// Unknown retry strategy
  UnknownStrategy(strategy: String)
}

/// Format a parse error as a human-readable message
pub fn format_error(error: ParseError) -> String {
  case error {
    FileError(path, reason) ->
      "Failed to read config file '" <> path <> "': " <> reason
    TomlError(message) -> "TOML syntax error: " <> message
    MissingField(path, field) ->
      "Missing required field '" <> field <> "' at " <> path
    WrongType(path, expected, got) ->
      "Wrong type at "
      <> path
      <> ": expected "
      <> expected
      <> ", got "
      <> got
    UnknownGateType(name, gate_type) ->
      "Unknown gate type '"
      <> gate_type
      <> "' for gate '"
      <> name
      <> "'. Valid types: command, parallel_review, multipass_review, human"
    UnknownStrategy(strategy) ->
      "Unknown retry strategy '"
      <> strategy
      <> "'. Valid strategies: same, fresh, auto"
  }
}

/// Parse a config file from the given path
pub fn parse_file(path: String) -> Result(ScherzoConfig, ParseError) {
  case simplifile.read(path) {
    Error(err) -> Error(FileError(path, simplifile.describe_error(err)))
    Ok(content) -> parse_string(content)
  }
}

/// Parse config from a TOML string
pub fn parse_string(content: String) -> Result(ScherzoConfig, ParseError) {
  case tom.parse(content) {
    Error(err) -> Error(TomlError(format_tom_error(err)))
    Ok(toml) -> parse_toml(toml)
  }
}

/// Format a tom parse error
fn format_tom_error(err: tom.ParseError) -> String {
  case err {
    tom.Unexpected(got, expected) ->
      "Unexpected '" <> got <> "', expected " <> expected
    tom.KeyAlreadyInUse(key) ->
      "Duplicate key: " <> string.join(key, ".")
  }
}

/// Parse the root TOML document into ScherzoConfig
fn parse_toml(toml: Dict(String, Toml)) -> Result(ScherzoConfig, ParseError) {
  // Parse gates config (optional)
  use gates_config <- result.try(parse_gates_config(toml))

  // Parse retry config (optional)
  use retry_config <- result.try(parse_retry_config(toml))

  Ok(ScherzoConfig(gates: gates_config, retry: retry_config))
}

/// Parse the [gates] section
fn parse_gates_config(
  toml: Dict(String, Toml),
) -> Result(GatesConfig, ParseError) {
  case tom.get_table(toml, ["gates"]) {
    Error(tom.NotFound(_)) -> Ok(types.default_gates_config())
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType("gates", expected, got))
    Ok(gates_table) -> {
      // Get formula (optional)
      let formula = case tom.get_string(gates_table, ["formula"]) {
        Ok(f) -> Some(f)
        Error(_) -> None
      }

      // Get gates array (optional)
      use gates <- result.try(case tom.get_array(gates_table, ["gates"]) {
        Error(tom.NotFound(_)) -> Ok([])
        Error(tom.WrongType(_, expected, got)) ->
          Error(WrongType("gates.gates", expected, got))
        Ok(gates_array) -> parse_gates_array(gates_array, [], 0)
      })

      Ok(GatesConfig(formula: formula, gates: gates))
    }
  }
}

/// Parse an array of gate definitions
fn parse_gates_array(
  gates: List(Toml),
  acc: List(GateConfig),
  index: Int,
) -> Result(List(GateConfig), ParseError) {
  case gates {
    [] -> Ok(list.reverse(acc))
    [gate_toml, ..rest] -> {
      use gate <- result.try(parse_gate(gate_toml, index))
      parse_gates_array(rest, [gate, ..acc], index + 1)
    }
  }
}

/// Parse a single gate definition
fn parse_gate(toml: Toml, index: Int) -> Result(GateConfig, ParseError) {
  let path = "gates.gates[" <> int.to_string(index) <> "]"

  case toml {
    tom.Table(table) | tom.InlineTable(table) -> parse_gate_table(table, path)
    _ -> Error(WrongType(path, "Table", "other"))
  }
}

/// Parse a gate from a table
fn parse_gate_table(
  table: Dict(String, Toml),
  path: String,
) -> Result(GateConfig, ParseError) {
  // Get required 'type' field
  use gate_type <- result.try(case tom.get_string(table, ["type"]) {
    Ok(t) -> Ok(t)
    Error(tom.NotFound(_)) -> Error(MissingField(path, "type"))
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".type", expected, got))
  })

  // Get required 'name' field
  use name <- result.try(case tom.get_string(table, ["name"]) {
    Ok(n) -> Ok(n)
    Error(tom.NotFound(_)) -> Error(MissingField(path, "name"))
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".name", expected, got))
  })

  // Parse based on type
  case gate_type {
    "command" -> parse_command_gate(table, name, path)
    "parallel_review" -> parse_parallel_review_gate(table, name, path)
    "multipass_review" -> parse_multipass_review_gate(table, name, path)
    "human" -> parse_human_gate(table, name, path)
    _ -> Error(UnknownGateType(name, gate_type))
  }
}

/// Parse a command gate
fn parse_command_gate(
  table: Dict(String, Toml),
  name: String,
  path: String,
) -> Result(GateConfig, ParseError) {
  // Get required 'command' field
  use command <- result.try(case tom.get_string(table, ["command"]) {
    Ok(c) -> Ok(c)
    Error(tom.NotFound(_)) -> Error(MissingField(path, "command"))
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".command", expected, got))
  })

  // Get optional fields with defaults
  let timeout_ms = get_int_or_default(table, ["timeout_ms"], 0)
  let serial = get_bool_or_default(table, ["serial"], True)
  let fail_fast = get_bool_or_default(table, ["fail_fast"], False)

  Ok(CommandGate(
    name: name,
    command: command,
    timeout_ms: timeout_ms,
    serial: serial,
    fail_fast: fail_fast,
  ))
}

/// Parse a parallel review gate
fn parse_parallel_review_gate(
  table: Dict(String, Toml),
  name: String,
  path: String,
) -> Result(GateConfig, ParseError) {
  // Get required 'synthesis_prompt' field
  use synthesis_prompt <- result.try(case
    tom.get_string(table, ["synthesis_prompt"])
  {
    Ok(p) -> Ok(p)
    Error(tom.NotFound(_)) -> Error(MissingField(path, "synthesis_prompt"))
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".synthesis_prompt", expected, got))
  })

  // Get dimensions array
  use dimensions <- result.try(case tom.get_array(table, ["dimensions"]) {
    Error(tom.NotFound(_)) -> Ok([])
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".dimensions", expected, got))
    Ok(dims_array) -> parse_dimensions_array(dims_array, [], path, 0)
  })

  Ok(ParallelReviewGate(
    name: name,
    dimensions: dimensions,
    synthesis_prompt: synthesis_prompt,
  ))
}

/// Parse an array of review dimensions
fn parse_dimensions_array(
  dims: List(Toml),
  acc: List(ReviewDimension),
  path: String,
  index: Int,
) -> Result(List(ReviewDimension), ParseError) {
  case dims {
    [] -> Ok(list.reverse(acc))
    [dim_toml, ..rest] -> {
      use dim <- result.try(parse_dimension(dim_toml, path, index))
      parse_dimensions_array(rest, [dim, ..acc], path, index + 1)
    }
  }
}

/// Parse a single review dimension
fn parse_dimension(
  toml: Toml,
  parent_path: String,
  index: Int,
) -> Result(ReviewDimension, ParseError) {
  let path = parent_path <> ".dimensions[" <> int.to_string(index) <> "]"

  case toml {
    tom.Table(table) | tom.InlineTable(table) -> {
      use id <- result.try(get_required_string(table, "id", path))
      use focus <- result.try(get_required_string(table, "focus", path))
      use prompt <- result.try(get_required_string(table, "prompt", path))
      Ok(ReviewDimension(id: id, focus: focus, prompt: prompt))
    }
    _ -> Error(WrongType(path, "Table", "other"))
  }
}

/// Parse a multi-pass review gate
fn parse_multipass_review_gate(
  table: Dict(String, Toml),
  name: String,
  path: String,
) -> Result(GateConfig, ParseError) {
  // Get optional fields with defaults
  let require_convergence =
    get_bool_or_default(table, ["require_convergence"], False)
  let max_passes = get_int_or_default(table, ["max_passes"], 5)

  // Get passes array
  use passes <- result.try(case tom.get_array(table, ["passes"]) {
    Error(tom.NotFound(_)) -> Ok([])
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".passes", expected, got))
    Ok(passes_array) -> parse_passes_array(passes_array, [], path, 0)
  })

  Ok(MultiPassReviewGate(
    name: name,
    passes: passes,
    require_convergence: require_convergence,
    max_passes: max_passes,
  ))
}

/// Parse an array of review passes
fn parse_passes_array(
  passes: List(Toml),
  acc: List(ReviewPass),
  path: String,
  index: Int,
) -> Result(List(ReviewPass), ParseError) {
  case passes {
    [] -> Ok(list.reverse(acc))
    [pass_toml, ..rest] -> {
      use pass <- result.try(parse_pass(pass_toml, path, index))
      parse_passes_array(rest, [pass, ..acc], path, index + 1)
    }
  }
}

/// Parse a single review pass
fn parse_pass(
  toml: Toml,
  parent_path: String,
  index: Int,
) -> Result(ReviewPass, ParseError) {
  let path = parent_path <> ".passes[" <> int.to_string(index) <> "]"

  case toml {
    tom.Table(table) | tom.InlineTable(table) -> {
      use focus <- result.try(get_required_string(table, "focus", path))
      use prompt <- result.try(get_required_string(table, "prompt", path))
      Ok(ReviewPass(focus: focus, prompt: prompt))
    }
    _ -> Error(WrongType(path, "Table", "other"))
  }
}

/// Parse a human gate
fn parse_human_gate(
  table: Dict(String, Toml),
  name: String,
  path: String,
) -> Result(GateConfig, ParseError) {
  use prompt <- result.try(case tom.get_string(table, ["prompt"]) {
    Ok(p) -> Ok(p)
    Error(tom.NotFound(_)) -> Error(MissingField(path, "prompt"))
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> ".prompt", expected, got))
  })

  Ok(HumanGate(name: name, prompt: prompt))
}

/// Parse the [retry] section
fn parse_retry_config(
  toml: Dict(String, Toml),
) -> Result(RetryConfig, ParseError) {
  case tom.get_table(toml, ["retry"]) {
    Error(tom.NotFound(_)) -> Ok(types.default_retry_config())
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType("retry", expected, got))
    Ok(retry_table) -> {
      // Get strategy (optional, defaults to Auto)
      use strategy <- result.try(case tom.get_string(retry_table, ["strategy"])
      {
        Error(_) -> Ok(Auto)
        Ok(s) -> parse_strategy(s)
      })

      // Get other optional fields with defaults
      let fresh_after_failures =
        get_int_or_default(retry_table, ["fresh_after_failures"], 2)
      let max_iterations =
        get_int_or_default(retry_table, ["max_iterations"], 3)

      Ok(RetryConfig(
        strategy: strategy,
        fresh_after_failures: fresh_after_failures,
        max_iterations: max_iterations,
      ))
    }
  }
}

/// Parse a retry strategy string
fn parse_strategy(s: String) -> Result(RetryStrategy, ParseError) {
  case string.lowercase(s) {
    "same" -> Ok(SameAgent)
    "fresh" -> Ok(FreshAgent)
    "auto" -> Ok(Auto)
    _ -> Error(UnknownStrategy(s))
  }
}

// Helper functions

/// Get a required string field
fn get_required_string(
  table: Dict(String, Toml),
  field: String,
  path: String,
) -> Result(String, ParseError) {
  case tom.get_string(table, [field]) {
    Ok(v) -> Ok(v)
    Error(tom.NotFound(_)) -> Error(MissingField(path, field))
    Error(tom.WrongType(_, expected, got)) ->
      Error(WrongType(path <> "." <> field, expected, got))
  }
}

/// Get an optional int with default
fn get_int_or_default(
  table: Dict(String, Toml),
  key: List(String),
  default: Int,
) -> Int {
  case tom.get_int(table, key) {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Get an optional bool with default
fn get_bool_or_default(
  table: Dict(String, Toml),
  key: List(String),
  default: Bool,
) -> Bool {
  case tom.get_bool(table, key) {
    Ok(v) -> v
    Error(_) -> default
  }
}
