import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile
import support/test_helpers

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 100,
    template_field_max_chars: 100,
    workflow_summary_max_chars: 100,
  )
}

fn diagnostic_limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 100,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 100,
  )
}

fn timeout_limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 1000,
  )
}

pub fn command_step_captures_stdout_and_exit_zero_test() {
  let dir = "test/tmp/command-step-success"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "test_step",
      "printf 'hello\\nworld\\n'",
      dir,
      1000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert step_artifact.status_to_string(artifact.status) == "success"
  assert artifact.exit_code == Some(0)
  assert artifact.stdout == "hello\nworld\n"
}

pub fn command_step_captures_stderr_and_nonzero_exit_test() {
  let dir = "test/tmp/command-step-failure"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "test_step",
      "echo bad >&2; exit 7",
      dir,
      1000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepFailed
  assert step_artifact.status_to_string(artifact.status) == "failure"
  assert artifact.exit_code == Some(7)
  assert artifact.stderr == "bad\n"
}

pub fn command_step_promotes_stable_failure_code_test() {
  let dir = "test/tmp/command-step-failure-code"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "publish_pr",
      "echo SCHERZO_FAILURE_CODE=publish_rebase_conflict >&2; exit 1",
      dir,
      1000,
      [],
      limits(),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some("publish_rebase_conflict")
  let assert Some(summary) = step_artifact.command_failure_summary(artifact)
  assert string.contains(summary, "failure_code=publish_rebase_conflict")
  let assert Some(diagnostic_path) = artifact.diagnostic_path
  let assert Ok(body) = simplifile.read(diagnostic_path)
  assert string.contains(body, "failure_code: publish_rebase_conflict")
}

pub fn command_step_captures_final_stdout_line_without_newline_test() {
  let dir = "test/tmp/command-step-no-final-newline"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "test_step",
      "printf 'no final newline'",
      dir,
      1000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert artifact.stdout == "no final newline"
}

pub fn command_step_timeout_returns_failed_artifact_test() {
  let dir = "test/tmp/command-step-timeout"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run("slow", "sleep 1", dir, 10, [], timeout_limits())
  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(124)
  assert artifact.timed_out == True
  assert artifact.failure_code == Some(command_step.timeout_failure_code)
  assert artifact.command == Some("sleep 1")
  assert string.contains(artifact.stderr, "prepared_monotonic_ms:")
  assert string.contains(artifact.stderr, "started_monotonic_ms:")
  assert string.contains(artifact.stderr, "deadline_monotonic_ms:")
  assert string.contains(artifact.stderr, "timeout_monotonic_ms:")
  let assert Some(diagnostic_path) = artifact.diagnostic_path
  let assert Ok(body) = simplifile.read(diagnostic_path)
  assert string.contains(body, "command: sleep 1")
  assert string.contains(body, "prepared_monotonic_ms:")
  assert string.contains(body, "timeout_monotonic_ms:")
}

pub fn command_step_timeout_redacts_command_identity_test() {
  let dir = "test/tmp/command-step-timeout-secret"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "secret_timeout",
      "printf start; sleep 1 # super-secret",
      dir,
      10,
      ["super-secret"],
      diagnostic_limits(),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code == Some(command_step.timeout_failure_code)
  assert artifact.command == Some("printf start; sleep 1 # [REDACTED]")
  assert !string.contains(step_artifact.to_string(artifact), "super-secret")
  let assert Some(diagnostic_path) = artifact.diagnostic_path
  let assert Ok(body) = simplifile.read(diagnostic_path)
  assert string.contains(body, "command: printf start; sleep 1 # [REDACTED]")
  assert !string.contains(body, "super-secret")
}

pub fn command_step_timeout_overrides_child_failure_code_test() {
  let dir = "test/tmp/command-step-timeout-failure-code"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "slow",
      "printf 'SCHERZO_FAILURE_CODE=child_error\\n' >&2; sleep 5",
      dir,
      1000,
      [],
      timeout_limits(),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(124)
  assert artifact.timed_out == True
  assert artifact.failure_code == Some(command_step.timeout_failure_code)
  assert string.contains(artifact.stderr, "SCHERZO_FAILURE_CODE=child_error")
  let assert Some(diagnostic_path) = artifact.diagnostic_path
  let assert Ok(body) = simplifile.read(diagnostic_path)
  assert string.contains(body, "failure_code: command_step_timeout")
}

pub fn command_step_wall_clock_timeout_stops_chatty_command_test() {
  let dir = "test/tmp/command-step-chatty-timeout"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "chatty",
      "while true; do printf 'tick\\n'; sleep 0.01; done",
      dir,
      80,
      [],
      diagnostic_limits(),
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(124)
  assert artifact.timed_out == True
  assert artifact.failure_code == Some(command_step.timeout_failure_code)
  let assert Some(duration_ms) = artifact.duration_ms
  assert duration_ms < 2000
  let assert Some(diagnostic_path) = artifact.diagnostic_path
  let assert Ok(body) = simplifile.read(diagnostic_path)
  assert string.contains(body, "failure_code: command_step_timeout")
}

pub fn command_step_env_is_available_to_helpers_test() {
  let dir = "test/tmp/command-step-env"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run_with_env(
      "env_step",
      "printf '%s|%s\n' \"$SCHERZO_RUN_ROOT\" \"$SCHERZO_STEP_ID\"",
      dir,
      1000,
      [#("SCHERZO_RUN_ROOT", "run-root"), #("SCHERZO_STEP_ID", "env_step")],
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert artifact.stdout == "run-root|env_step\n"
}

pub fn command_step_redacts_fake_secret_test() {
  let dir = "test/tmp/command-step-secret"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "secret",
      "printf 'test-key\\n'; echo 'test-key' >&2",
      dir,
      1000,
      ["test-key"],
      limits(),
    )
  assert artifact.stdout == "[REDACTED]\n"
  assert artifact.stderr == "[REDACTED]\n"
}

pub fn command_step_caps_stdout_while_collecting_test() {
  let dir = "test/tmp/command-step-long-stdout"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run(
      "long_stdout",
      "node -e 'console.log(\"x\".repeat(150))'",
      dir,
      1000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.stdout_truncated == True
  assert artifact.stdout
    == "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx..."
}

pub fn command_step_stdout_capture_preserves_pipeline_status_test() {
  let dir = "test/tmp/command-step-pipeline-status"
  test_helpers.reset_dir(dir)
  let artifact =
    command_step.run("pipeline", "false | true", dir, 1000, [], limits())

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

pub fn failed_command_step_retains_full_diagnostic_artifact_test() {
  let dir = "test/tmp/command-step-retained-diagnostics"
  test_helpers.reset_dir(dir)
  let stdout = string.repeat("o", times: 150)
  let stderr = string.repeat("e", times: 150)
  let command =
    "printf '" <> stdout <> "\\n'; printf '" <> stderr <> "\\n' >&2; exit 7"
  let artifact =
    command_step.run("final_test", command, dir, 1000, [], diagnostic_limits())

  assert artifact.status == step_artifact.StepFailed
  assert artifact.command == Some(command)
  assert artifact.exit_code == Some(7)
  let assert Some(duration_ms) = artifact.duration_ms
  assert duration_ms >= 0
  assert artifact.stdout_truncated == True
  assert artifact.stderr_truncated == True
  let assert Some(diagnostic_path) = artifact.diagnostic_path
  let assert Ok(body) = simplifile.read(diagnostic_path)
  assert string.contains(body, "step_id: final_test")
  assert string.contains(body, "exit_code: 7")
  assert string.contains(body, "stdout_truncated_in_report: true")
  assert string.contains(body, "stderr_truncated_in_report: true")
  assert string.contains(body, stdout)
  assert string.contains(body, stderr)
  assert simplifile.is_file(
      dir <> "/.scherzo/command-step-diagnostics/final_test.stdout.raw",
    )
    != Ok(True)
}
