import gleam/option.{None}
import gleam/string
import scherzo/structured_output_command_validator
import scherzo/structured_output_validator
import scherzo/workflow_dag

fn context(
  validator: workflow_dag.StructuredOutputValidator,
) -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    ".scherzo",
    ".",
    "test/tmp/run-root",
    "test_workflow",
    ".scherzo/workflows",
    "run-1",
    "review",
    1,
    ".",
    "review_lane_draft",
    "json",
    "final_response",
    None,
  )
  |> structured_output_validator.for_validator(validator, 0)
}

fn command_validator(
  name: String,
  script: String,
  timeout_ms: Int,
  env: List(#(String, String)),
) -> workflow_dag.StructuredOutputValidator {
  workflow_dag.CommandValidator(
    name: name,
    argv: ["python3", script],
    timeout_ms: timeout_ms,
    working_directory: workflow_dag.ValidatorInRepository,
    env: env,
  )
}

fn run(
  validator: workflow_dag.StructuredOutputValidator,
  secrets: List(String),
) {
  structured_output_command_validator.run_command_validator(
    validator,
    "{\"summary\":\"[REDACTED]\"}",
    context(validator),
    secrets,
  )
}

pub fn command_validator_exit_0_accepts_test() {
  let validator =
    command_validator(
      "accept",
      "test/fixtures/structured_output/command_validator_accept.py",
      30_000,
      [],
    )

  assert run(validator, []) == Ok(structured_output_validator.ValidatorPass)
}

pub fn command_validator_exit_1_is_retryable_test() {
  let validator =
    command_validator(
      "reject",
      "test/fixtures/structured_output/command_validator_reject.py",
      30_000,
      [],
    )
  let assert Error(error) = run(validator, ["TOPSECRET"])

  assert error.code == "structured_output_command_rejected"
  assert error.retryable
  assert !string.contains(error.diagnostic_summary, "TOPSECRET")
  assert string.contains(error.diagnostic_summary, "[REDACTED]")
}

pub fn command_validator_exit_2_is_non_retryable_test() {
  let validator =
    command_validator(
      "exit_2",
      "test/fixtures/structured_output/command_validator_exit_2.py",
      30_000,
      [],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_config_error"
  assert !error.retryable
}

pub fn command_validator_other_nonzero_is_non_retryable_test() {
  let validator =
    command_validator(
      "exit_3",
      "test/fixtures/structured_output/command_validator_exit_3.py",
      30_000,
      [],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_config_error"
  assert !error.retryable
}

pub fn command_validator_uses_clean_environment_and_context_test() {
  let validator =
    command_validator(
      "env_probe",
      "test/fixtures/structured_output/command_validator_env_probe.py",
      30_000,
      [#("CUSTOM_VALUE", "declared")],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_rejected"
  assert string.contains(error.diagnostic_summary, "\"SECRET_TOKEN\": null")
  assert string.contains(error.diagnostic_summary, "\"HOME\": null")
  assert string.contains(
    error.diagnostic_summary,
    "\"CUSTOM_VALUE\": \"declared\"",
  )
  assert string.contains(
    error.diagnostic_summary,
    "\"SCHERZO_REPO_ROOT\": \".\"",
  )
  assert string.contains(
    error.diagnostic_summary,
    "\"SCHERZO_WORKFLOW_BUNDLE_DIR\": \".scherzo/workflows\"",
  )
  assert string.contains(
    error.diagnostic_summary,
    "\"SCHERZO_VALIDATOR_TYPE\": \"command\"",
  )
  assert string.contains(
    error.diagnostic_summary,
    "\"SCHERZO_STRUCTURED_OUTPUT_SOURCE_TYPE\": \"final_response\"",
  )
}

pub fn command_validator_rejects_reserved_env_overrides_test() {
  let validator =
    command_validator(
      "reserved_env",
      "test/fixtures/structured_output/command_validator_accept.py",
      30_000,
      [#("PATH", "bad")],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_config_error"
  assert !error.retryable
}

pub fn command_validator_truncates_stdout_and_stderr_without_deadlock_test() {
  let validator =
    command_validator(
      "flood",
      "test/fixtures/structured_output/command_validator_stream_flood.py",
      30_000,
      [],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_rejected"
  assert error.stdout_truncated
  assert error.stderr_truncated
}

pub fn command_validator_timeout_cleans_up_process_test() {
  let validator =
    command_validator(
      "sleep",
      "test/fixtures/structured_output/command_validator_sleep.py",
      100,
      [],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_timeout"
  assert !error.retryable
}

pub fn command_validator_rejects_absolute_or_traversal_argv_path_test() {
  let validator =
    workflow_dag.CommandValidator(
      name: "bad_path",
      argv: ["../validator.py"],
      timeout_ms: 30_000,
      working_directory: workflow_dag.ValidatorInRepository,
      env: [],
    )
  let assert Error(error) = run(validator, [])

  assert error.code == "structured_output_command_config_error"
  assert !error.retryable
}
