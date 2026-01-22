import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import scherzo/config/loader
import scherzo/config/types.{
  Auto, CommandGate, GatesConfig, ParallelReviewGate, RetryConfig, SameAgent,
  ScherzoConfig,
}

// Formula loading tests

pub fn load_none_formula_test() {
  let result = loader.load_formula("none")
  result |> should.be_ok

  let assert Ok(cfg) = result
  cfg.gates.gates |> should.equal([])
}

pub fn load_empty_string_formula_test() {
  let result = loader.load_formula("")
  result |> should.be_ok

  let assert Ok(cfg) = result
  cfg.gates.gates |> should.equal([])
}

pub fn load_code_review_formula_test() {
  let result = loader.load_formula("code-review")
  result |> should.be_ok

  let assert Ok(cfg) = result
  cfg.gates.formula |> should.equal(Some("code-review"))

  // Should have tests, check, and parallel review gates
  cfg.gates.gates |> list.length |> should.equal(3)

  let assert [g1, g2, g3] = cfg.gates.gates
  let assert CommandGate(n1, ..) = g1
  let assert CommandGate(n2, ..) = g2
  let assert ParallelReviewGate(n3, dims, ..) = g3

  n1 |> should.equal("tests")
  n2 |> should.equal("check")
  n3 |> should.equal("code-review")

  // Should have 7 review dimensions
  dims |> list.length |> should.equal(7)
}

pub fn load_security_audit_formula_test() {
  let result = loader.load_formula("security-audit")
  result |> should.be_ok

  let assert Ok(cfg) = result
  cfg.gates.formula |> should.equal(Some("security-audit"))

  // Should have tests and multipass review
  cfg.gates.gates |> list.length |> should.equal(2)
}

pub fn load_quick_review_formula_test() {
  let result = loader.load_formula("quick-review")
  result |> should.be_ok

  let assert Ok(cfg) = result
  cfg.gates.formula |> should.equal(Some("quick-review"))

  // Should have just tests and check
  cfg.gates.gates |> list.length |> should.equal(2)

  // Should have different retry config
  cfg.retry.strategy |> should.equal(SameAgent)
  cfg.retry.max_iterations |> should.equal(2)
}

pub fn load_unknown_formula_test() {
  let result = loader.load_formula("unknown")
  result |> should.be_error

  let assert Error(loader.UnknownFormula(name)) = result
  name |> should.equal("unknown")
}

// Merge tests

pub fn merge_configs_none_override_test() {
  let base = types.default_config()
  let result = loader.merge_configs(base, None)

  result |> should.equal(base)
}

pub fn merge_configs_user_gates_override_test() {
  let base =
    ScherzoConfig(
      gates: GatesConfig(formula: Some("code-review"), gates: [
        CommandGate(
          name: "tests",
          command: "gleam test",
          timeout_ms: 0,
          serial: True,
          fail_fast: False,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let user =
    ScherzoConfig(
      gates: GatesConfig(formula: Some("custom"), gates: [
        CommandGate(
          name: "custom-test",
          command: "make test",
          timeout_ms: 60_000,
          serial: False,
          fail_fast: True,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let result = loader.merge_configs(base, Some(user))

  // User gates should completely replace base gates
  result.gates.gates |> list.length |> should.equal(1)
  let assert [gate] = result.gates.gates
  let assert CommandGate(name, command, ..) = gate
  name |> should.equal("custom-test")
  command |> should.equal("make test")
}

pub fn merge_configs_empty_user_gates_keeps_base_test() {
  let base =
    ScherzoConfig(
      gates: GatesConfig(formula: Some("code-review"), gates: [
        CommandGate(
          name: "tests",
          command: "gleam test",
          timeout_ms: 0,
          serial: True,
          fail_fast: False,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let user =
    ScherzoConfig(
      gates: GatesConfig(formula: Some("custom"), gates: []),
      retry: types.default_retry_config(),
    )

  let result = loader.merge_configs(base, Some(user))

  // Base gates should be kept when user has none
  result.gates.gates |> list.length |> should.equal(1)
  let assert [gate] = result.gates.gates
  let assert CommandGate(name, ..) = gate
  name |> should.equal("tests")

  // But formula is from user
  result.gates.formula |> should.equal(Some("custom"))
}

pub fn merge_configs_retry_override_test() {
  let base =
    ScherzoConfig(
      gates: types.default_gates_config(),
      retry: RetryConfig(
        strategy: Auto,
        fresh_after_failures: 2,
        max_iterations: 3,
      ),
    )

  let user =
    ScherzoConfig(
      gates: types.default_gates_config(),
      retry: RetryConfig(
        strategy: SameAgent,
        fresh_after_failures: 5,
        max_iterations: 10,
      ),
    )

  let result = loader.merge_configs(base, Some(user))

  // User retry config should override
  result.retry.strategy |> should.equal(SameAgent)
  result.retry.fresh_after_failures |> should.equal(5)
  result.retry.max_iterations |> should.equal(10)
}

// Available formulas test

pub fn available_formulas_test() {
  let formulas = loader.available_formulas()

  formulas |> list.contains("none") |> should.be_true
  formulas |> list.contains("code-review") |> should.be_true
  formulas |> list.contains("security-audit") |> should.be_true
  formulas |> list.contains("quick-review") |> should.be_true
}

pub fn formula_description_test() {
  loader.formula_description("code-review")
  |> should.equal("Comprehensive 7-dimension parallel code review with tests")

  loader.formula_description("unknown")
  |> should.equal("Unknown formula")
}

// Error formatting test

pub fn format_unknown_formula_error_test() {
  let error = loader.UnknownFormula("bad-formula")
  let msg = loader.format_error(error)

  msg
  |> should.equal(
    "Unknown formula 'bad-formula'. Available formulas: code-review, security-audit, quick-review, none",
  )
}
