import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/path
import scherzo/step_artifact
import simplifile
import support/test_helpers

fn write_fake_executable(dir: String, name: String, mode: String) -> String {
  test_helpers.reset_dir(dir)
  let script = dir <> "/" <> name
  let body =
    "#!/usr/bin/env bash\n"
    <> "set -eu\n"
    <> "if [[ \"${SCHERZO_LAUNCHER_ROUTE_ONLY:-}\" = \"1\" ]]; then\n"
    <> "  printf '%s\\n' \"${SCHERZO_FAKE_LAUNCHER_ROUTE:?}\"\n"
    <> "  exit 0\n"
    <> "fi\n"
    <> "printf 'mode=%s\\n' "
    <> test_helpers.shell_quote(mode)
    <> "\n"
    <> "for arg in \"$@\"; do\n"
    <> "  printf 'arg=%s\\n' \"$arg\"\n"
    <> "done\n"
  let assert Ok(Nil) = simplifile.write(script, body)
  test_helpers.chmod_executable(script)
  script
}

fn run_launcher(args: String, route: String) -> step_artifact.StepArtifact {
  let root = "test/tmp/scherzo-launcher"
  let direct = write_fake_executable(root <> "/direct", "direct", "direct")
  let runner = write_fake_executable(root <> "/runner", "runner", "runner")
  let assert Ok(direct_abs) = path.absolute(direct)
  let assert Ok(runner_abs) = path.absolute(runner)

  command_step.run_with_env(
    "scherzo_launcher",
    "scripts/scherzo-launcher " <> args,
    ".",
    5000,
    [
      #("SCHERZO_DIRECT_BIN", direct_abs),
      #("SCHERZO_START_RUNNER", runner_abs),
      #("SCHERZO_LAUNCHER_NAME", "scherzo"),
      #("SCHERZO_FAKE_LAUNCHER_ROUTE", route),
    ],
    [],
    test_helpers.default_artifact_limits(),
  )
}

fn assert_success(artifact: step_artifact.StepArtifact) -> Nil {
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  Nil
}

pub fn launcher_wraps_daemon_mode_through_start_runner_test() {
  let artifact = run_launcher(".scherzo/scherzo.yaml", "daemon")

  assert_success(artifact)
  assert string.contains(artifact.stdout, "mode=runner\n")
  assert string.contains(artifact.stdout, "arg=--\n")
  assert string.contains(artifact.stdout, "arg=/")
  assert string.contains(artifact.stdout, "arg=.scherzo/scherzo.yaml\n")
}

pub fn launcher_wraps_zero_arg_daemon_mode_through_start_runner_test() {
  let artifact = run_launcher("", "daemon")

  assert_success(artifact)
  assert string.contains(artifact.stdout, "mode=runner\n")
  assert string.contains(artifact.stdout, "arg=--\n")
  assert string.contains(artifact.stdout, "arg=/")
  assert !string.contains(artifact.stdout, "mode=direct\n")
}

pub fn launcher_preserves_direct_non_daemon_commands_test() {
  let help = run_launcher("--help", "direct")
  let doctor = run_launcher("doctor --list-checks", "direct")
  let once = run_launcher("--once .scherzo/scherzo.yaml", "direct")
  let ctl = run_launcher("ctl --help", "direct")
  let retired_smoke = run_launcher("--linear-smoke", "direct")
  let retired_contract =
    run_launcher("--linear-contract-check scherzo.yaml", "direct")

  assert_success(help)
  assert_success(doctor)
  assert_success(once)
  assert_success(ctl)
  assert_success(retired_smoke)
  assert_success(retired_contract)
  assert string.contains(help.stdout, "mode=direct\n")
  assert string.contains(doctor.stdout, "mode=direct\n")
  assert string.contains(once.stdout, "mode=direct\n")
  assert string.contains(ctl.stdout, "mode=direct\n")
  assert string.contains(retired_smoke.stdout, "mode=direct\n")
  assert string.contains(retired_contract.stdout, "mode=direct\n")
  assert !string.contains(help.stdout, "mode=runner\n")
  assert !string.contains(doctor.stdout, "mode=runner\n")
  assert !string.contains(once.stdout, "mode=runner\n")
  assert !string.contains(ctl.stdout, "mode=runner\n")
  assert !string.contains(retired_smoke.stdout, "mode=runner\n")
  assert !string.contains(retired_contract.stdout, "mode=runner\n")
}
