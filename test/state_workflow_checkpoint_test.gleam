import gleam/string
import scherzo/config/types as config_types
import scherzo/state/workflow_checkpoint
import scherzo/step_artifact
import simplifile
import support/test_helpers

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

pub fn workflow_checkpoint_roundtrips_full_step_artifact_test() {
  let root = "test/tmp/state-workflow-checkpoint/roundtrip"
  test_helpers.reset_dir(root)
  let artifact =
    step_artifact.from_command_result(
      "build",
      7,
      "stdout text",
      "stderr text",
      True,
      [],
      limits(),
    )

  let assert Ok(ref) =
    workflow_checkpoint.write_step_artifact(root, "run-1", "build", 3, artifact)
  assert !string.starts_with(ref, "/")
  let assert Ok(decoded) = workflow_checkpoint.read_step_artifact(root, ref)
  assert decoded == artifact
}

pub fn workflow_checkpoint_reports_missing_and_corrupt_artifacts_test() {
  let root = "test/tmp/state-workflow-checkpoint/fail-closed"
  test_helpers.reset_dir(root)
  let artifact =
    step_artifact.from_command_result(
      "build",
      0,
      "stdout",
      "stderr",
      False,
      [],
      limits(),
    )
  let assert Ok(ref) =
    workflow_checkpoint.write_step_artifact(root, "run-1", "build", 1, artifact)
  let full_path = root <> "/.scherzo-state/artifacts/" <> ref
  let assert Ok(Nil) = simplifile.write(full_path, "corrupt")
  let assert Error(workflow_checkpoint.ArtifactCorrupt(_, _)) =
    workflow_checkpoint.read_step_artifact(root, ref)
  let assert Ok(Nil) = simplifile.delete_file(at: full_path)
  let assert Error(workflow_checkpoint.ArtifactMissing(_)) =
    workflow_checkpoint.read_step_artifact(root, ref)
}
