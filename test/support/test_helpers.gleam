import gleam/string
import scherzo/config/types as config_types
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
  // Set the executable bits directly via the filesystem (0o755) instead of
  // spawning a `chmod` subprocess. Test fixtures are freshly written scripts,
  // so a fixed rwxr-xr-x mode matches the prior `chmod +x` intent without the
  // per-call fork/exec cost.
  let assert Ok(Nil) = simplifile.set_permissions_octal(path, 0o755)
  Nil
}
