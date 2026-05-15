import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path
import scherzo/step_artifact
import scherzo_test
import simplifile

pub fn explicit_test_suites_are_documented_test() {
  let assert Ok(readme) = simplifile.read("README.md")
  let assert Ok(test_readme) = simplifile.read("test/README.md")
  let assert Ok(architecture) = simplifile.read("docs/ARCHITECTURE.md")

  assert string.contains(readme, "## Test suites")
  assert string.contains(readme, "direnv exec . gleam test")
  assert string.contains(readme, "scherzo-test-unit")
  assert string.contains(readme, "scherzo-test-contract")
  assert string.contains(readme, "scherzo-test-local-integration")
  assert string.contains(readme, "scherzo-test-real-pi-validation")
  assert string.contains(readme, "Every PR")
  assert string.contains(readme, "required dependencies")
  assert string.contains(readme, "pi` on `PATH")
  assert string.contains(readme, "SelfCI runs the unit and contract suites")

  assert string.contains(test_readme, "gleam test -- --suite contract")
  assert string.contains(architecture, "direnv exec . scherzo-test-contract")
  assert string.contains(architecture, "SelfCI runs this suite")
}

pub fn explicit_suites_have_no_env_gate_contract_test() {
  let assert Ok(local_integration) =
    simplifile.read(
      "test/local_integration/workflow_jj_workspace_smoke_test.gleam",
    )
  let assert Ok(real_pi_validation) =
    simplifile.read(
      "test/real_pi_validation/real_pi_session_validation_test.gleam",
    )
  let assert Ok(test_runner) = simplifile.read("test/scherzo_test.gleam")
  let assert Ok(contract_script) =
    simplifile.read("scripts/scherzo-test-contract")
  let assert Ok(local_integration_script) =
    simplifile.read("scripts/scherzo-test-local-integration")
  let assert Ok(real_pi_validation_script) =
    simplifile.read("scripts/scherzo-test-real-pi-validation")

  assert_no_env_gate(local_integration)
  assert_no_env_gate(real_pi_validation)
  assert_no_env_gate(test_runner)
  assert_no_env_gate(contract_script)
  assert_no_env_gate(local_integration_script)
  assert_no_env_gate(real_pi_validation_script)
  assert string.contains(test_runner, "local_integration/")
  assert string.contains(test_runner, "real_pi_validation/")
  assert string.contains(test_runner, "contract")
  assert string.contains(test_runner, "local-integration")
  assert string.contains(test_runner, "real-pi-validation")
  assert string.contains(test_runner, "No test files matched suite")
}

pub fn suite_selection_partitions_unit_and_contract_files_test() {
  assert scherzo_test.contract_test_files() == expected_contract_test_files()

  list.each(expected_contract_test_files(), assert_contract_file)

  assert_contract_file("contract/example_contract_test.gleam")
  assert !scherzo_test.is_contract_file("workflow_run_test.gleam")
  assert scherzo_test.is_unit_file("workflow_run_test.gleam")
  assert !scherzo_test.is_contract_file(
    "local_integration/workflow_jj_workspace_smoke_test.gleam",
  )
  assert !scherzo_test.is_unit_file(
    "local_integration/workflow_jj_workspace_smoke_test.gleam",
  )
  assert !scherzo_test.is_unit_file(
    "real_pi_validation/real_pi_session_validation_test.gleam",
  )
}

pub fn contract_wrapper_invokes_contract_suite_test() {
  let dir = "test/tmp/contract-wrapper-dispatch"
  reset_dir(dir)
  let bin = dir <> "/bin"
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let assert Ok(log_path) = path.absolute(dir <> "/argv.log")
  let fake_gleam = bin <> "/gleam"
  let assert Ok(Nil) =
    simplifile.write(
      fake_gleam,
      "#!/bin/sh\n"
        <> "printf 'ARG=%s\\n' \"$@\" > "
        <> shell_quote(log_path)
        <> "\n",
    )
  chmod_executable(fake_gleam)

  let artifact =
    command_step.run_with_env(
      "contract_wrapper_dispatch",
      "scripts/scherzo-test-contract",
      ".",
      5000,
      [#("PATH", env_path(bin))],
      [],
      limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(argv) = simplifile.read(log_path)
  assert argv == "ARG=test\nARG=--\nARG=--suite\nARG=contract\n"
}

fn expected_contract_test_files() -> List(String) {
  [
    "execplan_implementation_helper_test.gleam",
    "execplan_html_renderer_test.gleam",
    "jj_workspace_driver_test.gleam",
    "merge_conflict_helper_test.gleam",
    "review_artifacts_test.gleam",
    "workspace_driver_contract_test.gleam",
    "workspace_driver_discovery_test.gleam",
    "workspace_driver_lifecycle_test.gleam",
  ]
}

fn assert_contract_file(file: String) -> Nil {
  assert scherzo_test.is_contract_file(file)
  assert !scherzo_test.is_unit_file(file)
}

fn assert_no_env_gate(contents: String) -> Nil {
  assert !string.contains(contents, "path.env(")
  assert !string.contains(contents, "getenv(")
  assert !string.contains(contents, "SCHERZO_TEST_")
  assert !string.contains(contents, "GLEAM_TEST_")
}

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn chmod_executable(file: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod_contract_wrapper_fake_gleam",
      "chmod +x " <> shell_quote(file),
      ".",
      5000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn env_path(bin: String) -> String {
  case path.env("PATH") {
    Some(value) -> bin <> ":" <> value
    _ -> bin
  }
}
