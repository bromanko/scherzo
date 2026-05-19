import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path
import scherzo/step_artifact
import simplifile

pub fn wrapper_resolves_symlinked_script_dir_and_runs_from_repo_root_test() {
  let dir = "test/tmp/scherzoctl-wrapper-symlink"
  reset_dir(dir)
  let bin = dir <> "/bin"
  let consumer = dir <> "/consumer"
  let work = dir <> "/work"
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let assert Ok(Nil) = simplifile.create_directory_all(consumer)
  let assert Ok(Nil) = simplifile.create_directory_all(work)

  let assert Ok(log_path) = path.absolute(dir <> "/direnv.log")
  let fake_direnv = bin <> "/direnv"
  let assert Ok(Nil) =
    simplifile.write(
      fake_direnv,
      "#!/bin/sh\n"
        <> "printf 'cwd=%s\\n' \"$PWD\" > "
        <> shell_quote(log_path)
        <> "\n"
        <> "printf 'arg=%s\\n' \"$@\" >> "
        <> shell_quote(log_path)
        <> "\n",
    )
  chmod_executable(fake_direnv)

  let assert Ok(scripts_dir) = path.absolute("scripts")
  let assert Ok(consumer_dir) = path.absolute(consumer)
  let link_artifact =
    command_step.run(
      "link_scherzoctl_wrapper_scripts",
      "ln -s "
        <> shell_quote(scripts_dir)
        <> " "
        <> shell_quote(consumer_dir <> "/scripts"),
      ".",
      5000,
      [],
      limits(),
    )
  assert link_artifact.status == step_artifact.StepSucceeded
  assert link_artifact.exit_code == Some(0)

  let wrapper = consumer_dir <> "/scripts/scherzoctl"
  let artifact =
    command_step.run_with_env(
      "scherzoctl_wrapper_symlink_root",
      shell_quote(wrapper) <> " ping",
      work,
      5000,
      [#("PATH", env_path(bin))],
      [],
      limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(log) = simplifile.read(log_path)
  let repo_root = string.drop_end(scripts_dir, 8)
  assert string.contains(log, "cwd=" <> repo_root <> "\n")
  assert string.contains(log, "arg=exec\n")
  assert string.contains(log, "arg=" <> repo_root <> "\n")
  assert string.contains(log, "arg=gleam\n")
  assert string.contains(log, "arg=run\n")
  assert string.contains(log, "arg=--\n")
  assert string.contains(log, "arg=ctl\n")
  assert string.contains(log, "arg=ping\n")
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
      "chmod_scherzoctl_wrapper_fake_direnv",
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
