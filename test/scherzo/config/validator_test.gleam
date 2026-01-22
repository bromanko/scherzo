import gleam/list
import gleeunit/should
import scherzo/config/types.{
  Auto, CommandGate, GatesConfig, HumanGate, MultiPassReviewGate,
  ParallelReviewGate, RetryConfig, ReviewDimension, ReviewPass, ScherzoConfig,
}
import scherzo/config/validator

// Valid config tests

pub fn valid_empty_config_test() {
  let config = types.default_config()
  validator.is_valid(config) |> should.be_true
  validator.validate(config) |> should.equal([])
}

pub fn valid_command_gate_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
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

  validator.is_valid(config) |> should.be_true
}

pub fn valid_human_gate_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        HumanGate(name: "approval", prompt: "Please approve"),
      ]),
      retry: types.default_retry_config(),
    )

  validator.is_valid(config) |> should.be_true
}

pub fn valid_parallel_review_gate_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        ParallelReviewGate(
          name: "review",
          dimensions: [
            ReviewDimension(
              id: "correctness",
              focus: "Check bugs",
              prompt: "Review for bugs",
            ),
          ],
          synthesis_prompt: "Synthesize reviews",
        ),
      ]),
      retry: types.default_retry_config(),
    )

  validator.is_valid(config) |> should.be_true
}

pub fn valid_multipass_review_gate_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        MultiPassReviewGate(
          name: "review",
          passes: [ReviewPass(focus: "Structure", prompt: "Check structure")],
          require_convergence: False,
          max_passes: 3,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  validator.is_valid(config) |> should.be_true
}

// Command gate validation errors

pub fn empty_command_gate_name_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        CommandGate(
          name: "",
          command: "gleam test",
          timeout_ms: 0,
          serial: True,
          fail_fast: False,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].name")
}

pub fn empty_command_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        CommandGate(
          name: "tests",
          command: "   ",
          timeout_ms: 0,
          serial: True,
          fail_fast: False,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].command")
}

pub fn negative_timeout_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        CommandGate(
          name: "tests",
          command: "gleam test",
          timeout_ms: -1,
          serial: True,
          fail_fast: False,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].timeout_ms")
}

// Human gate validation errors

pub fn empty_human_gate_prompt_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        HumanGate(name: "approval", prompt: ""),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].prompt")
}

// Parallel review gate validation errors

pub fn empty_parallel_review_dimensions_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        ParallelReviewGate(
          name: "review",
          dimensions: [],
          synthesis_prompt: "Synthesize",
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].dimensions")
}

pub fn empty_synthesis_prompt_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        ParallelReviewGate(
          name: "review",
          dimensions: [
            ReviewDimension(id: "test", focus: "test", prompt: "test"),
          ],
          synthesis_prompt: "",
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].synthesis_prompt")
}

pub fn empty_dimension_fields_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        ParallelReviewGate(
          name: "review",
          dimensions: [ReviewDimension(id: "", focus: "", prompt: "")],
          synthesis_prompt: "Synthesize",
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  // Should have 3 errors: id, focus, prompt
  errors |> list.length |> should.equal(3)
}

// Multi-pass review gate validation errors

pub fn empty_multipass_passes_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        MultiPassReviewGate(
          name: "review",
          passes: [],
          require_convergence: False,
          max_passes: 3,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].passes")
}

pub fn zero_max_passes_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        MultiPassReviewGate(
          name: "review",
          passes: [ReviewPass(focus: "test", prompt: "test")],
          require_convergence: False,
          max_passes: 0,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("gates.gates[0].max_passes")
}

pub fn empty_pass_fields_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        MultiPassReviewGate(
          name: "review",
          passes: [ReviewPass(focus: "", prompt: "")],
          require_convergence: False,
          max_passes: 3,
        ),
      ]),
      retry: types.default_retry_config(),
    )

  let errors = validator.validate(config)
  // Should have 2 errors: focus, prompt
  errors |> list.length |> should.equal(2)
}

// Retry config validation errors

pub fn zero_max_iterations_test() {
  let config =
    ScherzoConfig(
      gates: types.default_gates_config(),
      retry: RetryConfig(
        strategy: Auto,
        fresh_after_failures: 2,
        max_iterations: 0,
      ),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("retry.max_iterations")
}

pub fn negative_fresh_after_failures_test() {
  let config =
    ScherzoConfig(
      gates: types.default_gates_config(),
      retry: RetryConfig(
        strategy: Auto,
        fresh_after_failures: -1,
        max_iterations: 3,
      ),
    )

  let errors = validator.validate(config)
  errors |> list.length |> should.equal(1)
  let assert [err] = errors
  err.path |> should.equal("retry.fresh_after_failures")
}

// Multiple errors test

pub fn multiple_errors_test() {
  let config =
    ScherzoConfig(
      gates: GatesConfig(formula: None, gates: [
        CommandGate(
          name: "",
          command: "",
          timeout_ms: -1,
          serial: True,
          fail_fast: False,
        ),
      ]),
      retry: RetryConfig(
        strategy: Auto,
        fresh_after_failures: -1,
        max_iterations: 0,
      ),
    )

  let errors = validator.validate(config)
  // Should have 5 errors: name, command, timeout, max_iterations, fresh_after_failures
  errors |> list.length |> should.equal(5)
}

// Format errors test

pub fn format_errors_empty_test() {
  let msg = validator.format_errors([])
  msg |> should.equal("Configuration is valid")
}

pub fn format_errors_with_errors_test() {
  let errors = [
    validator.ValidationError("path.to.field", "Something is wrong"),
    validator.ValidationError("another.path", "Another issue"),
  ]

  let msg = validator.format_errors(errors)
  msg
  |> should.equal(
    "Configuration validation failed:\n  - path.to.field: Something is wrong\n  - another.path: Another issue",
  )
}

// validate_strict test

pub fn validate_strict_valid_test() {
  let config = types.default_config()
  let result = validator.validate_strict(config)
  result |> should.be_ok
}

pub fn validate_strict_invalid_test() {
  let config =
    ScherzoConfig(
      gates: types.default_gates_config(),
      retry: RetryConfig(
        strategy: Auto,
        fresh_after_failures: 2,
        max_iterations: 0,
      ),
    )

  let result = validator.validate_strict(config)
  result |> should.be_error
}

import gleam/option.{None}
