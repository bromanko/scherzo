import gleam/option.{None, Some}
import gleeunit/should
import scherzo/config/parser
import scherzo/config/types.{
  Auto, CommandGate, FreshAgent, HumanGate, MultiPassReviewGate,
  ParallelReviewGate, SameAgent,
}

// Basic parsing tests

pub fn parse_empty_config_test() {
  let config = parser.parse_string("")
  config |> should.be_ok

  let assert Ok(cfg) = config
  cfg.gates.formula |> should.equal(None)
  cfg.gates.gates |> should.equal([])
  cfg.retry.strategy |> should.equal(Auto)
  cfg.retry.max_iterations |> should.equal(3)
}

pub fn parse_minimal_gates_section_test() {
  let toml =
    "
[gates]
formula = \"code-review\"
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  cfg.gates.formula |> should.equal(Some("code-review"))
  cfg.gates.gates |> should.equal([])
}

// Command gate tests

pub fn parse_command_gate_test() {
  let toml =
    "
[gates]
gates = [
  { type = \"command\", name = \"tests\", command = \"gleam test\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  let assert [gate] = cfg.gates.gates
  let assert CommandGate(name, command, timeout_ms, serial, fail_fast) = gate

  name |> should.equal("tests")
  command |> should.equal("gleam test")
  timeout_ms |> should.equal(0)
  serial |> should.equal(True)
  fail_fast |> should.equal(False)
}

pub fn parse_command_gate_with_options_test() {
  let toml =
    "
[gates]
[[gates.gates]]
type = \"command\"
name = \"lint\"
command = \"gleam check\"
timeout_ms = 60000
serial = false
fail_fast = true
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  let assert [gate] = cfg.gates.gates
  let assert CommandGate(name, command, timeout_ms, serial, fail_fast) = gate

  name |> should.equal("lint")
  command |> should.equal("gleam check")
  timeout_ms |> should.equal(60_000)
  serial |> should.equal(False)
  fail_fast |> should.equal(True)
}

// Human gate tests

pub fn parse_human_gate_test() {
  let toml =
    "
[gates]
gates = [
  { type = \"human\", name = \"approval\", prompt = \"Please review and approve\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  let assert [gate] = cfg.gates.gates
  let assert HumanGate(name, prompt) = gate

  name |> should.equal("approval")
  prompt |> should.equal("Please review and approve")
}

// Parallel review gate tests

pub fn parse_parallel_review_gate_test() {
  let toml =
    "
[gates]
[[gates.gates]]
type = \"parallel_review\"
name = \"code-review\"
synthesis_prompt = \"Synthesize the reviews\"
dimensions = [
  { id = \"correctness\", focus = \"Check bugs\", prompt = \"Review for correctness\" },
  { id = \"security\", focus = \"Check vulnerabilities\", prompt = \"Review for security\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  let assert [gate] = cfg.gates.gates
  let assert ParallelReviewGate(name, dimensions, synthesis_prompt) = gate

  name |> should.equal("code-review")
  synthesis_prompt |> should.equal("Synthesize the reviews")

  let assert [dim1, dim2] = dimensions
  dim1.id |> should.equal("correctness")
  dim1.focus |> should.equal("Check bugs")
  dim2.id |> should.equal("security")
}

// Multi-pass review gate tests

pub fn parse_multipass_review_gate_test() {
  let toml =
    "
[gates]
[[gates.gates]]
type = \"multipass_review\"
name = \"deep-review\"
require_convergence = true
max_passes = 3
passes = [
  { focus = \"Structure\", prompt = \"Review structure\" },
  { focus = \"Logic\", prompt = \"Review logic\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  let assert [gate] = cfg.gates.gates
  let assert MultiPassReviewGate(name, passes, require_convergence, max_passes) =
    gate

  name |> should.equal("deep-review")
  require_convergence |> should.equal(True)
  max_passes |> should.equal(3)

  let assert [pass1, pass2] = passes
  pass1.focus |> should.equal("Structure")
  pass2.focus |> should.equal("Logic")
}

// Retry config tests

pub fn parse_retry_config_test() {
  let toml =
    "
[retry]
strategy = \"fresh\"
fresh_after_failures = 1
max_iterations = 5
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  cfg.retry.strategy |> should.equal(FreshAgent)
  cfg.retry.fresh_after_failures |> should.equal(1)
  cfg.retry.max_iterations |> should.equal(5)
}

pub fn parse_retry_strategy_same_test() {
  let toml =
    "
[retry]
strategy = \"same\"
"
  let assert Ok(cfg) = parser.parse_string(toml)
  cfg.retry.strategy |> should.equal(SameAgent)
}

pub fn parse_retry_strategy_auto_test() {
  let toml =
    "
[retry]
strategy = \"auto\"
"
  let assert Ok(cfg) = parser.parse_string(toml)
  cfg.retry.strategy |> should.equal(Auto)
}

// Multiple gates test

pub fn parse_multiple_gates_test() {
  let toml =
    "
[gates]
formula = \"custom\"
gates = [
  { type = \"command\", name = \"tests\", command = \"gleam test\" },
  { type = \"command\", name = \"check\", command = \"gleam check\" },
  { type = \"human\", name = \"approval\", prompt = \"Approve?\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  cfg.gates.gates |> list.length |> should.equal(3)

  let assert [g1, g2, g3] = cfg.gates.gates
  let assert CommandGate(n1, ..) = g1
  let assert CommandGate(n2, ..) = g2
  let assert HumanGate(n3, ..) = g3

  n1 |> should.equal("tests")
  n2 |> should.equal("check")
  n3 |> should.equal("approval")
}

// Error handling tests

pub fn parse_missing_gate_type_test() {
  let toml =
    "
[gates]
gates = [
  { name = \"tests\", command = \"gleam test\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_error

  let assert Error(parser.MissingField(_, field)) = config
  field |> should.equal("type")
}

pub fn parse_missing_gate_name_test() {
  let toml =
    "
[gates]
gates = [
  { type = \"command\", command = \"gleam test\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_error

  let assert Error(parser.MissingField(_, field)) = config
  field |> should.equal("name")
}

pub fn parse_missing_command_test() {
  let toml =
    "
[gates]
gates = [
  { type = \"command\", name = \"tests\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_error

  let assert Error(parser.MissingField(_, field)) = config
  field |> should.equal("command")
}

pub fn parse_unknown_gate_type_test() {
  let toml =
    "
[gates]
gates = [
  { type = \"invalid\", name = \"foo\" }
]
"
  let config = parser.parse_string(toml)
  config |> should.be_error

  let assert Error(parser.UnknownGateType(name, gate_type)) = config
  name |> should.equal("foo")
  gate_type |> should.equal("invalid")
}

pub fn parse_unknown_strategy_test() {
  let toml =
    "
[retry]
strategy = \"invalid\"
"
  let config = parser.parse_string(toml)
  config |> should.be_error

  let assert Error(parser.UnknownStrategy(s)) = config
  s |> should.equal("invalid")
}

pub fn parse_toml_syntax_error_test() {
  let toml = "this is not valid toml ["
  let config = parser.parse_string(toml)
  config |> should.be_error

  let assert Error(parser.TomlError(_)) = config
}

// Full config test

pub fn parse_full_config_test() {
  let toml =
    "
[gates]
formula = \"code-review\"
gates = [
  { type = \"command\", name = \"tests\", command = \"gleam test\", timeout_ms = 300000 },
  { type = \"command\", name = \"check\", command = \"gleam check\" }
]

[retry]
strategy = \"auto\"
fresh_after_failures = 2
max_iterations = 3
"
  let config = parser.parse_string(toml)
  config |> should.be_ok

  let assert Ok(cfg) = config
  cfg.gates.formula |> should.equal(Some("code-review"))
  cfg.gates.gates |> list.length |> should.equal(2)
  cfg.retry.strategy |> should.equal(Auto)
  cfg.retry.fresh_after_failures |> should.equal(2)
  cfg.retry.max_iterations |> should.equal(3)
}

// Import for list.length
import gleam/list
