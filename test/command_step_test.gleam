import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/domain
import scherzo/step_artifact
import simplifile

fn limits() -> domain.ArtifactLimits {
  domain.ArtifactLimits(
    command_stream_max_chars: 100,
    template_field_max_chars: 100,
    workflow_summary_max_chars: 100,
  )
}

fn diagnostic_limits() -> domain.ArtifactLimits {
  domain.ArtifactLimits(
    command_stream_max_chars: 100,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 100,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn command_step_captures_stdout_and_exit_zero_test() {
  let dir = "test/tmp/command-step-success"
  reset_dir(dir)
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
  reset_dir(dir)
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

pub fn command_step_captures_final_stdout_line_without_newline_test() {
  let dir = "test/tmp/command-step-no-final-newline"
  reset_dir(dir)
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
  reset_dir(dir)
  let artifact = command_step.run("slow", "sleep 1", dir, 10, [], limits())
  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(124)
  assert artifact.timed_out == True
}

pub fn command_step_redacts_fake_secret_test() {
  let dir = "test/tmp/command-step-secret"
  reset_dir(dir)
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
  reset_dir(dir)
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
  reset_dir(dir)
  let artifact =
    command_step.run("pipeline", "false | true", dir, 1000, [], limits())

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

pub fn failed_command_step_retains_full_diagnostic_artifact_test() {
  let dir = "test/tmp/command-step-retained-diagnostics"
  reset_dir(dir)
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
