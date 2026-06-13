import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/control/file
import scherzo/path
import scherzo/step_artifact
import simplifile
import support/test_helpers

pub fn wrapper_resolves_symlinked_script_dir_and_preserves_caller_cwd_test() {
  let dir = "test/tmp/scherzoctl-wrapper-symlink"
  test_helpers.reset_dir(dir)
  let bin = dir <> "/bin"
  let core = dir <> "/core"
  let core_scripts = core <> "/scripts"
  let consumer = dir <> "/consumer"
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let assert Ok(Nil) = simplifile.create_directory_all(core_scripts)
  let assert Ok(Nil) = simplifile.create_directory_all(consumer)

  let assert Ok(log_path) = path.absolute(dir <> "/direnv.log")
  let assert Ok(bin_dir) = path.absolute(bin)
  let fake_direnv = bin_dir <> "/direnv"
  let assert Ok(Nil) =
    simplifile.write(
      fake_direnv,
      "#!/bin/sh\n"
        <> "printf 'cwd=%s\\n' \"$PWD\" > "
        <> test_helpers.shell_quote(log_path)
        <> "\n"
        <> "printf 'caller=%s\\n' \"$SCHERZO_CALLER_CWD\" >> "
        <> test_helpers.shell_quote(log_path)
        <> "\n"
        <> "printf 'arg=%s\\n' \"$@\" >> "
        <> test_helpers.shell_quote(log_path)
        <> "\n",
    )
  test_helpers.chmod_executable(fake_direnv)

  let assert Ok(wrapper_source) = simplifile.read("scripts/scherzoctl")
  let fake_wrapper = core_scripts <> "/scherzoctl"
  let assert Ok(Nil) = simplifile.write(fake_wrapper, wrapper_source)
  test_helpers.chmod_executable(fake_wrapper)

  let assert Ok(core_dir) = path.absolute(core)
  let assert Ok(consumer_dir) = path.absolute(consumer)
  let control_rel = file.default_discovery_path
  let assert Ok(Nil) =
    file.write(
      core_dir <> "/" <> control_rel,
      file.ControlFile("127.0.0.1", 10_001, "core-token", core_dir, 1, 60_000),
    )
  let assert Ok(Nil) =
    file.write(
      consumer_dir <> "/" <> control_rel,
      file.ControlFile(
        "127.0.0.1",
        10_002,
        "consumer-token",
        consumer_dir,
        1,
        60_000,
      ),
    )
  let link_artifact =
    command_step.run(
      "link_scherzoctl_wrapper_scripts",
      "ln -s "
        <> test_helpers.shell_quote(core_dir <> "/scripts")
        <> " "
        <> test_helpers.shell_quote(consumer_dir <> "/scripts"),
      ".",
      5000,
      [],
      test_helpers.default_artifact_limits(),
    )
  assert link_artifact.status == step_artifact.StepSucceeded
  assert link_artifact.exit_code == Some(0)

  let wrapper = consumer_dir <> "/scripts/scherzoctl"
  let artifact =
    command_step.run_with_env(
      "scherzoctl_wrapper_symlink_root",
      test_helpers.shell_quote(wrapper)
        <> " ps --json --control-file "
        <> test_helpers.shell_quote(control_rel),
      consumer_dir,
      5000,
      [#("PATH", env_path(bin_dir))],
      [],
      test_helpers.default_artifact_limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(log) = simplifile.read(log_path)
  assert string.contains(log, "cwd=" <> core_dir <> "\n")
  assert string.contains(log, "caller=" <> consumer_dir <> "\n")
  assert string.contains(log, "arg=exec\n")
  assert string.contains(log, "arg=" <> core_dir <> "\n")
  assert string.contains(log, "arg=gleam\n")
  assert string.contains(log, "arg=run\n")
  assert string.contains(log, "arg=--\n")
  assert string.contains(log, "arg=ctl\n")
  assert string.contains(log, "arg=ps\n")
  assert string.contains(log, "arg=--json\n")
  assert string.contains(log, "arg=--control-file\n")
  assert string.contains(log, "arg=" <> control_rel <> "\n")
}

fn env_path(bin: String) -> String {
  case path.env("PATH") {
    Some(value) -> bin <> ":" <> value
    _ -> bin
  }
}
