import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/step_artifact
import simplifile
import support/test_helpers

fn write_fake_linear(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let path = dir <> "/fake-linear"
  let body =
    "#!/usr/bin/env bash\n"
    <> "set -eu\n"
    <> "printf 'LINEAR_API_KEY=%s\\n' \"${LINEAR_API_KEY:-}\"\n"
    <> "for arg in \"$@\"; do\n"
    <> "  printf 'ARG:%s\\n' \"$arg\"\n"
    <> "done\n"
  let assert Ok(Nil) = simplifile.write(path, body)
  test_helpers.chmod_executable(path)
  path
}

fn run_wrapper(
  command: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  command_step.run_with_env(
    "linear-cli-wrapper",
    "bash scripts/scherzo-linear-cli-wrapper " <> command,
    ".",
    5000,
    env,
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn assert_success(artifact: step_artifact.StepArtifact) -> Nil {
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  Nil
}

pub fn linear_wrapper_adds_default_project_to_issue_create_test() {
  let fake = write_fake_linear("test/tmp/linear-cli-wrapper/default-project")
  let artifact =
    run_wrapper("issue create --team LIV --title Example --no-interactive", [
      #("SCHERZO_LINEAR_CLI", fake),
      #("LINEAR_DEFAULT_PROJECT", "Scherzo Core"),
      #("LINEAR_API_KEY", ""),
      #("SCHERZO_AGENT_LINEAR_API_KEY", "agent-key"),
    ])

  assert_success(artifact)
  assert string.contains(artifact.stdout, "LINEAR_API_KEY=agent-key\n")
  assert string.contains(artifact.stdout, "ARG:--project\nARG:Scherzo Core\n")
}

pub fn linear_wrapper_respects_explicit_project_test() {
  let fake = write_fake_linear("test/tmp/linear-cli-wrapper/explicit-project")
  let artifact =
    run_wrapper(
      "issue create --team LIV --project 'Other Project' --title Example --no-interactive",
      [
        #("SCHERZO_LINEAR_CLI", fake),
        #("LINEAR_DEFAULT_PROJECT", "Scherzo Core"),
        #("LINEAR_API_KEY", "operator-key"),
        #("SCHERZO_AGENT_LINEAR_API_KEY", "agent-key"),
      ],
    )

  assert_success(artifact)
  assert string.contains(artifact.stdout, "LINEAR_API_KEY=operator-key\n")
  assert string.contains(artifact.stdout, "ARG:--project\nARG:Other Project\n")
  assert !string.contains(artifact.stdout, "ARG:Scherzo Core\n")
}

pub fn linear_wrapper_does_not_add_project_to_other_commands_test() {
  let fake = write_fake_linear("test/tmp/linear-cli-wrapper/other-command")
  let artifact =
    run_wrapper("issue view LIV-123 --json", [
      #("SCHERZO_LINEAR_CLI", fake),
      #("LINEAR_DEFAULT_PROJECT", "Scherzo Core"),
      #("LINEAR_API_KEY", "operator-key"),
    ])

  assert_success(artifact)
  assert !string.contains(artifact.stdout, "ARG:--project\n")
  assert !string.contains(artifact.stdout, "ARG:Scherzo Core\n")
}
