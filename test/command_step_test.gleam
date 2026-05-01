import gleam/option.{Some}
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
