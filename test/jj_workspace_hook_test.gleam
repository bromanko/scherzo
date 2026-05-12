import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path
import scherzo/step_artifact
import simplifile

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run("chmod", "chmod +x " <> path, ".", 5000, [], limits())
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn absolute(value: String) -> String {
  path.absolute(value) |> result_unwrap(value)
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

fn write_fake_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> \"$SCHERZO_FAKE_JJ_LOG\"\n"
        <> "if [ \"$1\" = --repository ]; then shift 2; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_FETCH_FAIL:-}\" = 1 ]; then echo 'simulated fetch failure' >&2; exit 7; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  rev=\n"
        <> "  prev=\n"
        <> "  for arg in \"$@\"; do\n"
        <> "    if [ \"$prev\" = -r ]; then rev=$arg; fi\n"
        <> "    prev=$arg\n"
        <> "  done\n"
        <> "  case \"$rev\" in\n"
        <> "    develop@upstream|main@origin|custom-base|@) echo commit; exit 0;;\n"
        <> "    *) exit 1;;\n"
        <> "  esac\n"
        <> "fi\n"
        <> "if [ \"$1\" = workspace ] && [ \"$2\" = add ]; then\n"
        <> "  target=\n"
        <> "  for arg in \"$@\"; do target=$arg; done\n"
        <> "  mkdir -p \"$target/.jj\" || exit 1\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = root ]; then pwd -P; exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  Nil
}

fn setup_driver_fixture(dir: String) -> #(String, String, String, String) {
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/repo/.jj")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workspaces")
  write_fake_jj(dir <> "/bin/jj")
  chmod_executable(dir <> "/bin/jj")

  #(
    absolute(dir <> "/repo"),
    absolute(dir <> "/workspaces/main"),
    absolute(dir <> "/bin"),
    absolute(dir <> "/jj.log"),
  )
}

fn run_driver_create(
  repo: String,
  workspace_path: String,
  bin: String,
  log: String,
  extra_env: String,
) -> step_artifact.StepArtifact {
  let script = absolute("scripts/scherzo-workspace-jj")
  command_step.run(
    "workspace_driver_create",
    extra_env
      <> "SCHERZO_FAKE_JJ_LOG=\""
      <> log
      <> "\" PATH=\""
      <> bin
      <> ":$PATH\" SCHERZO_REPO_ROOT=\""
      <> repo
      <> "\" SCHERZO_WORKSPACE_PATH=\""
      <> workspace_path
      <> "\" SCHERZO_WORKFLOW_ID=implementation sh \""
      <> script
      <> "\" lifecycle create",
    ".",
    5000,
    [],
    limits(),
  )
}

pub fn root_workspace_fetches_configured_publish_base_before_add_test() {
  let dir = "test/tmp/jj-workspace-driver-fetches-base"
  let #(repo, workspace_path, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_driver_create(
      repo,
      workspace_path,
      bin,
      log,
      "SCHERZO_PR_REMOTE=upstream SCHERZO_PR_BASE=develop ",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(contents) = simplifile.read(log)
  let lines = string.split(string.trim(contents), on: "\n")
  let assert [
    fetch_line,
    resolve_line,
    add_line,
    "root",
    "root",
    "status --color=never",
  ] = lines
  assert fetch_line
    == "--repository "
    <> repo
    <> " git fetch --remote upstream --branch develop"
  assert resolve_line
    == "--repository "
    <> repo
    <> " log -r develop@upstream --no-graph -T commit_id --color=never"
  assert string.contains(add_line, "--repository " <> repo <> " workspace add")
  assert string.contains(add_line, "--revision develop@upstream")
  let assert Ok(True) = simplifile.is_directory(workspace_path <> "/.jj")
}

pub fn explicit_workspace_base_skips_fetch_test() {
  let dir = "test/tmp/jj-workspace-driver-explicit-base"
  let #(repo, workspace_path, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_driver_create(
      repo,
      workspace_path,
      bin,
      log,
      "SCHERZO_JJ_WORKSPACE_BASE=custom-base ",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(contents) = simplifile.read(log)
  assert !string.contains(contents, "git fetch")
  assert string.contains(
    contents,
    "--repository "
      <> repo
      <> " log -r custom-base --no-graph -T commit_id --color=never",
  )
  assert string.contains(contents, "--revision custom-base")
}

pub fn derived_workspace_skips_fetch_and_uses_source_at_test() {
  let dir = "test/tmp/jj-workspace-driver-derived"
  let #(repo, workspace_path, bin, log) = setup_driver_fixture(dir)
  let source_path = absolute(dir <> "/source")
  let assert Ok(Nil) = simplifile.create_directory_all(source_path <> "/.jj")

  let artifact =
    run_driver_create(
      repo,
      workspace_path,
      bin,
      log,
      "SCHERZO_SOURCE_WORKSPACE_PATH=\"" <> source_path <> "\" ",
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  let assert Ok(contents) = simplifile.read(log)
  assert !string.contains(contents, "git fetch")
  assert string.contains(
    contents,
    "--repository " <> source_path <> " workspace add",
  )
  assert string.contains(contents, "--revision @")
}

pub fn fetch_failure_fails_root_workspace_creation_with_override_hint_test() {
  let dir = "test/tmp/jj-workspace-driver-fetch-failure"
  let #(repo, workspace_path, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_driver_create(
      repo,
      workspace_path,
      bin,
      log,
      "SCHERZO_PR_REMOTE=upstream SCHERZO_PR_BASE=develop SCHERZO_FAKE_JJ_FETCH_FAIL=1 ",
    )

  assert artifact.status == step_artifact.StepFailed
  assert artifact.exit_code == Some(1)
  assert string.contains(artifact.stderr, "simulated fetch failure")
  assert string.contains(
    artifact.stderr,
    "could not fetch publish base 'develop' from remote 'upstream'",
  )
  assert string.contains(artifact.stderr, "SCHERZO_JJ_WORKSPACE_BASE")
  let assert Ok(False) = simplifile.is_directory(workspace_path <> "/.jj")
}
