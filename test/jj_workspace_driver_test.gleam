import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
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

fn absolute(value: String) -> String {
  path.absolute(value) |> result_unwrap(value)
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod_fake_jj",
      "chmod +x " <> shell_quote(path),
      ".",
      5000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn write_fake_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> \"$SCHERZO_FAKE_JJ_LOG\"\n"
        <> "if [ \"$1\" = --repository ]; then shift 2; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then echo commit; exit 0; fi\n"
        <> "if [ \"$1\" = workspace ] && [ \"$2\" = add ]; then\n"
        <> "  target=\n"
        <> "  for arg in \"$@\"; do target=$arg; done\n"
        <> "  mkdir -p \"$target/.jj\" || exit 1\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = root ]; then pwd -P; exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then printf '%s' \"${SCHERZO_FAKE_JJ_STATUS:-}\"; exit 0; fi\n"
        <> "if [ \"$1\" = diff ]; then\n"
        <> "  name_only=0\n"
        <> "  for arg in \"$@\"; do if [ \"$arg\" = --name-only ]; then name_only=1; fi; done\n"
        <> "  if [ \"$name_only\" = 1 ]; then\n"
        <> "    printf '%s' \"${SCHERZO_FAKE_JJ_CHANGED_FILES:-}\"\n"
        <> "  else\n"
        <> "    printf '%s' \"${SCHERZO_FAKE_JJ_DIFF:-}\"\n"
        <> "  fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = workspace ] && [ \"$2\" = forget ]; then exit 0; fi\n"
        <> "exit 1\n",
    )
  chmod_executable(path)
}

fn setup_driver_fixture(dir: String) -> #(String, String, String, String) {
  reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/bin")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/repo/.jj")
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workspaces")
  let log = absolute(dir <> "/jj.log")
  write_fake_jj(dir <> "/bin/jj")
  let assert Ok(Nil) = simplifile.write(log, "")

  #(
    absolute(dir <> "/repo"),
    absolute(dir <> "/workspaces/main"),
    absolute(dir <> "/bin"),
    log,
  )
}

fn env_path(bin: String) -> String {
  case path.env("PATH") {
    Some(value) -> bin <> ":" <> value
    None -> bin
  }
}

fn fake_env(
  workspace: String,
  bin: String,
  log: String,
  extra: List(#(String, String)),
) -> List(#(String, String)) {
  [
    #("SCHERZO_WORKSPACE_PATH", workspace),
    #("SCHERZO_FAKE_JJ_LOG", log),
    #("PATH", env_path(bin)),
  ]
  |> list.append(extra)
}

fn fake_env_without_workspace(
  bin: String,
  log: String,
  extra: List(#(String, String)),
) -> List(#(String, String)) {
  [#("SCHERZO_FAKE_JJ_LOG", log), #("PATH", env_path(bin))]
  |> list.append(extra)
}

fn run_jj(
  step_id: String,
  args: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  run_jj_command(step_id, "sh ", args, env)
}

fn run_jj_without_inherited_workspace(
  step_id: String,
  args: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  run_jj_command(step_id, "env -u SCHERZO_WORKSPACE_PATH sh ", args, env)
}

fn run_jj_command(
  step_id: String,
  prefix: String,
  args: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  let script = absolute("scripts/scherzo-workspace-jj")
  command_step.run_with_env(
    step_id,
    prefix <> shell_quote(script) <> " " <> args,
    ".",
    5000,
    env,
    [],
    limits(),
  )
}

fn assert_exit(artifact: step_artifact.StepArtifact, code: Int) -> Nil {
  assert artifact.exit_code == Some(code)
  Nil
}

fn log_lines(log: String) -> List(String) {
  let assert Ok(contents) = simplifile.read(log)
  case string.trim(contents) {
    "" -> []
    trimmed -> string.split(trimmed, on: "\n")
  }
}

fn changed_path_decoder() -> decode.Decoder(String) {
  use path <- decode.field("path", decode.string)
  decode.success(path)
}

fn changed_paths_decoder() -> decode.Decoder(List(String)) {
  use files <- decode.field("files", decode.list(of: changed_path_decoder()))
  decode.success(files)
}

fn driver_description_decoder() -> decode.Decoder(#(Int, List(String))) {
  use version <- decode.field("version", decode.int)
  use capabilities <- decode.field(
    "capabilities",
    decode.list(of: decode.string),
  )
  decode.success(#(version, capabilities))
}

fn decode_paths(value: String) -> List(String) {
  let assert Ok(paths) = json.parse(value, changed_paths_decoder())
  paths
}

fn decode_driver_description(value: String) -> #(Int, List(String)) {
  let assert Ok(description) = json.parse(value, driver_description_decoder())
  description
}

pub fn jj_driver_describe_json_is_static_and_workspace_free_test() {
  let dir = "test/tmp/jj-workspace-driver-describe"
  let #(_, _, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_describe_json",
      "describe --json",
      fake_env_without_workspace(bin, log, []),
    )
  assert_exit(artifact, 0)
  assert artifact.stderr == ""
  assert decode_driver_description(artifact.stdout)
    == #(1, [
      "status",
      "diff",
      "changed-files",
      "assert-only",
      "baseline",
      "refresh-base",
      "publish-change",
    ])
  assert log_lines(log) == []

  let unsupported =
    run_jj(
      "jj_driver_describe_unsupported",
      "describe --yaml",
      fake_env_without_workspace(bin, log, []),
    )
  assert_exit(unsupported, 2)
  assert string.contains(unsupported.stderr, "describe requires --json")
  assert log_lines(log) == []
}

pub fn jj_driver_lifecycle_create_delegates_to_existing_helper_test() {
  let dir = "test/tmp/jj-workspace-driver-lifecycle-create"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_lifecycle_create",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_WORKFLOW_ID", "implementation"),
        #("SCHERZO_PR_REMOTE", "upstream"),
        #("SCHERZO_PR_BASE", "develop"),
      ]),
    )

  assert_exit(artifact, 0)
  let assert [
    fetch_line,
    resolve_line,
    add_line,
    "root",
    "root",
    "status --color=never",
  ] = log_lines(log)
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
  assert simplifile.is_directory(workspace <> "/.jj") == Ok(True)
}

pub fn jj_driver_changed_files_json_is_sorted_and_deduplicated_test() {
  let dir = "test/tmp/jj-workspace-driver-changed-files"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let artifact =
    run_jj(
      "jj_driver_changed_sorted",
      "changed-files --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "zeta.md\nalpha.md\nzeta.md\n\n"),
      ]),
    )

  assert_exit(artifact, 0)
  assert string.contains(artifact.stdout, "\"version\":1")
  assert string.contains(artifact.stdout, "\"path\":\"alpha.md\"")
  assert string.contains(artifact.stdout, "\"status\":\"modified\"")
  assert string.contains(artifact.stdout, "\"path\":\"zeta.md\"")
  assert log_lines(log)
    == [
      "diff --from @- --to @ --name-only --color=never",
      "diff --summary --from @- --to @ --color=never",
    ]
}

pub fn jj_driver_changed_files_json_escapes_special_path_names_test() {
  let dir = "test/tmp/jj-workspace-driver-special-paths"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let artifact =
    run_jj(
      "jj_driver_changed_special",
      "changed-files --json",
      fake_env(workspace, bin, log, [
        #(
          "SCHERZO_FAKE_JJ_CHANGED_FILES",
          "space name.md\nquote\"name.md\nbackslash\\name.md\n",
        ),
      ]),
    )

  assert_exit(artifact, 0)
  assert decode_paths(artifact.stdout)
    == [
      "backslash\\name.md",
      "quote\"name.md",
      "space name.md",
    ]
}

pub fn jj_driver_status_and_diff_use_human_jj_commands_test() {
  let dir = "test/tmp/jj-workspace-driver-human-commands"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let status =
    run_jj(
      "jj_driver_status_human",
      "status --human",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_STATUS", "working copy clean\n"),
      ]),
    )
  assert_exit(status, 0)
  assert status.stdout == "working copy clean\n"

  let diff =
    run_jj(
      "jj_driver_diff_human",
      "diff --human",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_DIFF", "diff text\n"),
      ]),
    )
  assert_exit(diff, 0)
  assert diff.stdout == "diff text\n"

  let diff_json =
    run_jj(
      "jj_driver_diff_json",
      "diff --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_DIFF", "diff --git a/a b/a\n"),
      ]),
    )
  assert_exit(diff_json, 0)
  assert string.contains(diff_json.stdout, "\"version\":1")
  assert string.contains(diff_json.stdout, "diff --git a/a b/a")
  assert string.contains(diff_json.stdout, "\"truncated\":false")

  assert log_lines(log)
    == [
      "status --color=never",
      "diff --from @- --to @ --color=never",
      "diff --from @- --to @ --git --color=never",
    ]
}

pub fn jj_driver_assert_only_accepts_exact_file_and_rejects_extra_file_test() {
  let dir = "test/tmp/jj-workspace-driver-assert-only"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let success =
    run_jj(
      "jj_driver_assert_success",
      "assert-only --path docs/plans/example.md",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "docs/plans/example.md\n"),
      ]),
    )
  assert_exit(success, 0)
  assert success.stdout == ""

  let failure =
    run_jj(
      "jj_driver_assert_extra",
      "assert-only --path docs/plans/example.md",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "docs/plans/example.md\nnotes.md\n"),
      ]),
    )
  assert_exit(failure, 1)
  assert string.contains(failure.stderr, "docs/plans/example.md")
  assert string.contains(failure.stderr, "notes.md")
  assert !string.contains(failure.stderr, workspace)
}

pub fn jj_driver_assert_only_rejects_unsafe_paths_without_invoking_jj_test() {
  let dir = "test/tmp/jj-workspace-driver-unsafe-paths"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let absolute_path = absolute(dir <> "/outside.md")

  let unsafe_paths = ["", "../outside.md", ".", absolute_path]
  list_each(unsafe_paths, fn(value) {
    let artifact =
      run_jj(
        "jj_driver_assert_unsafe",
        "assert-only --path " <> shell_quote(value),
        fake_env(workspace, bin, log, []),
      )
    assert_exit(artifact, 2)
  })
  assert log_lines(log) == []
}

pub fn jj_driver_lifecycle_remove_requires_explicit_workspace_path_test() {
  let dir = "test/tmp/jj-workspace-driver-remove-requires-workspace"
  let #(_, _, bin, log) = setup_driver_fixture(dir)

  let unset =
    run_jj_without_inherited_workspace(
      "jj_driver_remove_unset_workspace",
      "lifecycle remove",
      fake_env_without_workspace(bin, log, []),
    )
  assert_exit(unset, 2)
  assert string.contains(unset.stderr, "SCHERZO_WORKSPACE_PATH is required")

  let empty =
    run_jj(
      "jj_driver_remove_empty_workspace",
      "lifecycle remove",
      fake_env_without_workspace(bin, log, [#("SCHERZO_WORKSPACE_PATH", "")]),
    )
  assert_exit(empty, 2)
  assert string.contains(empty.stderr, "SCHERZO_WORKSPACE_PATH is required")
  assert log_lines(log) == []
}

pub fn jj_driver_lifecycle_after_step_is_successful_noop_test() {
  let dir = "test/tmp/jj-workspace-driver-after-step"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_after_step",
      "lifecycle after-step",
      fake_env(workspace, bin, log, []),
    )
  assert_exit(artifact, 0)
  assert artifact.stdout == ""
  assert artifact.stderr == ""
  assert log_lines(log) == []
}

fn list_each(values: List(a), effect: fn(a) -> Nil) -> Nil {
  case values {
    [] -> Nil
    [value, ..rest] -> {
      effect(value)
      list_each(rest, effect)
    }
  }
}
