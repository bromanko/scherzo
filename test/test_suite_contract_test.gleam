import gleam/list
import gleam/option.{Some}
import scherzo/command_step
import scherzo/path
import scherzo/step_artifact
import scherzo_test
import simplifile
import support/test_helpers

pub fn suite_selection_partitions_unit_and_contract_files_test() {
  let contract_files = scherzo_test.contract_test_files()
  assert contract_files != []

  list.each(contract_files, assert_contract_file)

  assert_contract_file("contract/example_contract_test.gleam")
  assert_contract_file("workflow_run_test.gleam")
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
  test_helpers.reset_dir(dir)
  let bin = dir <> "/bin"
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let assert Ok(log_path) = path.absolute(dir <> "/argv.log")
  let fake_gleam = bin <> "/gleam"
  let assert Ok(Nil) =
    simplifile.write(
      fake_gleam,
      "#!/bin/sh\n"
        <> "printf 'ARG=%s\\n' \"$@\" > "
        <> test_helpers.shell_quote(log_path)
        <> "\n",
    )
  test_helpers.chmod_executable(fake_gleam)

  let artifact =
    command_step.run_with_env(
      "contract_wrapper_dispatch",
      "scripts/scherzo-test-contract",
      ".",
      5000,
      [#("PATH", env_path(bin))],
      [],
      test_helpers.default_artifact_limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(argv) = simplifile.read(log_path)
  assert argv == "ARG=test\nARG=--\nARG=--suite\nARG=contract\n"
}

fn assert_contract_file(file: String) -> Nil {
  assert scherzo_test.is_contract_file(file)
  assert !scherzo_test.is_unit_file(file)
}

fn env_path(bin: String) -> String {
  case path.env("PATH") {
    Some(value) -> bin <> ":" <> value
    _ -> bin
  }
}
