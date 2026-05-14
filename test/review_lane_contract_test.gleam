import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile
import workflow_context_test_support

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 8000,
    template_field_max_chars: 8000,
    workflow_summary_max_chars: 8000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn run_contract(command: String) -> step_artifact.StepArtifact {
  run_shell("scripts/scherzo-review-lane-contract " <> command)
}

fn run_shell(command: String) -> step_artifact.StepArtifact {
  command_step.run(
    "review_lane_contract",
    workflow_context_test_support.without_workflow_context(command),
    ".",
    30_000,
    [],
    limits(),
  )
}

pub fn review_lane_contract_offline_accepts_migrated_review_workflow_test() {
  let dir = "test/tmp/review-lane-contract-offline"
  reset_dir(dir)

  let artifact =
    run_contract(
      "offline --workflow .scherzo/workflows/review-native.yml --fixtures test/fixtures/review-lane-contract --output-dir "
      <> dir,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "REVIEW_LANE_CONTRACT_OFFLINE=passed")
  let assert Ok(report) = simplifile.read(dir <> "/contract-report.v1.json")
  assert string.contains(report, "\"status\": \"passed\"")
  assert string.contains(report, "\"remote_mutations\": \"none\"")
  assert string.contains(report, "valid-minimal.arguments.json")
  assert string.contains(report, "unexpected-runner-metadata.arguments.json")
}

pub fn review_lane_contract_live_skips_without_credentials_test() {
  let dir = "test/tmp/review-lane-contract-live"
  reset_dir(dir)

  let artifact =
    run_shell(
      "env -u ANTHROPIC_API_KEY -u OPENAI_API_KEY -u GEMINI_API_KEY -u GOOGLE_API_KEY scripts/scherzo-review-lane-contract live --workflow .scherzo/workflows/review-native.yml --output-dir "
      <> dir
      <> " --skip-if-missing-credentials",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(report) = simplifile.read(dir <> "/live-probe-report.v1.json")
  assert string.contains(report, "skipped_missing_credentials")
  assert string.contains(report, "\"remote_mutations\": \"none\"")
}
