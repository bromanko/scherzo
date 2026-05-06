import gleam/string
import simplifile

pub fn explicit_test_suites_are_documented_test() {
  let assert Ok(readme) = simplifile.read("README.md")
  assert string.contains(readme, "## Test suites")
  assert string.contains(readme, "direnv exec . gleam test")
  assert string.contains(readme, "scherzo-test-local-integration")
  assert string.contains(readme, "scherzo-test-real-pi-validation")
  assert string.contains(readme, "Every PR")
  assert string.contains(readme, "Required dependencies")
  assert string.contains(readme, "pi` on `PATH")
}

pub fn explicit_integration_suites_have_no_env_gate_contract_test() {
  let assert Ok(local_integration) =
    simplifile.read(
      "test/local_integration/workflow_jj_workspace_smoke_test.gleam",
    )
  let assert Ok(real_pi_validation) =
    simplifile.read(
      "test/real_pi_validation/real_pi_session_validation_test.gleam",
    )
  let assert Ok(test_runner) = simplifile.read("test/scherzo_test.gleam")

  assert !string.contains(local_integration, "path.env(")
  assert !string.contains(real_pi_validation, "getenv(")
  assert string.contains(test_runner, "local_integration/")
  assert string.contains(test_runner, "real_pi_validation/")
  assert string.contains(test_runner, "local-integration")
  assert string.contains(test_runner, "real-pi-validation")
  assert string.contains(test_runner, "No test files matched suite")
}
