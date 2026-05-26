import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/step_artifact
import simplifile

pub fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn default_artifact_limits() -> config_types.ArtifactLimits {
  artifact_limits(4000)
}

pub fn artifact_limits(max_chars: Int) -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: max_chars,
    template_field_max_chars: max_chars,
    workflow_summary_max_chars: max_chars,
  )
}

pub fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

pub fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod_test_file",
      "chmod +x " <> shell_quote(path),
      ".",
      5000,
      [],
      default_artifact_limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  Nil
}
