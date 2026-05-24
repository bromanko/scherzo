import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
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

fn tmp_fixture_dir(name: String) -> String {
  let base = case path.env("TMPDIR") {
    Some(value) -> value
    None -> "/tmp"
  }
  base <> "/scherzo-" <> name
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

fn assert_symlink_target(link: String, target: String) -> Nil {
  let artifact =
    command_step.run(
      "readlink_symlink",
      "test -L " <> shell_quote(link) <> " && readlink " <> shell_quote(link),
      ".",
      5000,
      [],
      limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.trim(artifact.stdout) == target
}

fn write_fake_jj(path: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> \"$SCHERZO_FAKE_JJ_LOG\"\n"
        <> "if [ \"$1\" = --repository ]; then shift 2; fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = fetch ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_FETCH_FAIL:-}\" = 1 ]; then echo 'simulated fetch failure' >&2; exit 1; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = remote ] && [ \"$3\" = list ]; then\n"
        <> "  if [ -n \"${SCHERZO_FAKE_JJ_REMOTES+x}\" ]; then\n"
        <> "    printf '%s\\n' \"$SCHERZO_FAKE_JJ_REMOTES\"\n"
        <> "  else\n"
        <> "    printf '%s\\n' 'origin https://github.com/example/repo.git' 'upstream https://github.com/example/upstream.git' 'fork https://github.com/example/fork.git'\n"
        <> "  fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = git ] && [ \"$2\" = push ]; then exit 0; fi\n"
        <> "if [ \"$1\" = log ]; then\n"
        <> "  revision=\n"
        <> "  while [ $# -gt 0 ]; do\n"
        <> "    case \"$1\" in\n"
        <> "      -r) revision=$2; shift 2 ;;\n"
        <> "      *) shift ;;\n"
        <> "    esac\n"
        <> "  done\n"
        <> "  case \"$revision\" in conflicts*) exit 0 ;; esac\n"
        <> "  for missing in ${SCHERZO_FAKE_JJ_MISSING_REVISIONS:-}; do\n"
        <> "    if [ \"$revision\" = \"$missing\" ]; then exit 1; fi\n"
        <> "  done\n"
        <> "  printf '%s\\n' \"${SCHERZO_FAKE_JJ_LOG_OUTPUT:-commit}\"\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = workspace ] && [ \"$2\" = add ]; then\n"
        <> "  target=\n"
        <> "  for arg in \"$@\"; do target=$arg; done\n"
        <> "  mkdir -p \"$target/.jj\" || exit 1\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_PORTABLE_LINKS:-}\" = 1 ]; then\n"
        <> "    mkdir -p \"$target/.scherzo\" || exit 1\n"
        <> "    ln -s ../scherzo/scripts \"$target/scripts\" || exit 1\n"
        <> "    ln -s ../../scherzo/.scherzo/workflows \"$target/.scherzo/workflows\" || exit 1\n"
        <> "  fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = root ]; then pwd -P; exit 0; fi\n"
        <> "if [ \"$1\" = status ]; then\n"
        <> "  count=1\n"
        <> "  if [ -n \"${SCHERZO_FAKE_JJ_STATUS_COUNT_FILE:-}\" ]; then\n"
        <> "    if [ -f \"$SCHERZO_FAKE_JJ_STATUS_COUNT_FILE\" ]; then count=$(cat \"$SCHERZO_FAKE_JJ_STATUS_COUNT_FILE\"); count=$((count + 1)); fi\n"
        <> "    printf '%s' \"$count\" > \"$SCHERZO_FAKE_JJ_STATUS_COUNT_FILE\"\n"
        <> "  fi\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_STATUS_FAIL_ONCE:-}\" = 1 ] && [ \"$count\" = 1 ]; then printf '%s' \"${SCHERZO_FAKE_JJ_STATUS_FAIL_OUTPUT:-}\" >&2; exit 1; fi\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_STATUS_FAIL_ON_RETRY:-}\" = 1 ] && [ \"$count\" -gt 1 ]; then printf '%s' \"${SCHERZO_FAKE_JJ_STATUS_RETRY_FAIL_OUTPUT:-${SCHERZO_FAKE_JJ_STATUS_FAIL_OUTPUT:-}}\" >&2; exit 1; fi\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_STATUS_EXIT_CODE:-0}\" != 0 ]; then printf '%s' \"${SCHERZO_FAKE_JJ_STATUS:-}\"; exit \"$SCHERZO_FAKE_JJ_STATUS_EXIT_CODE\"; fi\n"
        <> "  printf '%s' \"${SCHERZO_FAKE_JJ_STATUS:-}\"; exit 0; fi\n"
        <> "if [ \"$1\" = workspace ] && [ \"$2\" = update-stale ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_UPDATE_STALE_FAIL:-}\" = 1 ]; then printf '%s' \"${SCHERZO_FAKE_JJ_UPDATE_STALE_OUTPUT:-update stale failed\\n}\" >&2; exit 1; fi\n"
        <> "  printf '%s' \"${SCHERZO_FAKE_JJ_UPDATE_STALE_OUTPUT:-}\"\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = diff ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_FAIL_ON_AT_MINUS:-}\" = 1 ]; then\n"
        <> "    for arg in \"$@\"; do if [ \"$arg\" = @- ]; then echo 'ambiguous @-' >&2; exit 1; fi; done\n"
        <> "  fi\n"
        <> "  name_only=0\n"
        <> "  for arg in \"$@\"; do if [ \"$arg\" = --name-only ]; then name_only=1; fi; done\n"
        <> "  if [ \"$name_only\" = 1 ]; then\n"
        <> "    printf '%s' \"${SCHERZO_FAKE_JJ_CHANGED_FILES:-}\"\n"
        <> "  else\n"
        <> "    printf '%s' \"${SCHERZO_FAKE_JJ_DIFF:-}\"\n"
        <> "  fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "if [ \"$1\" = describe ]; then exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ] && [ \"$2\" = track ]; then exit 0; fi\n"
        <> "if [ \"$1\" = bookmark ] && [ \"$2\" = set ]; then exit 0; fi\n"
        <> "if [ \"$1\" = rebase ]; then exit 0; fi\n"
        <> "if [ \"$1\" = resolve ] && [ \"$2\" = --list ]; then exit 0; fi\n"
        <> "if [ \"$1\" = workspace ] && [ \"$2\" = forget ]; then\n"
        <> "  if [ \"${SCHERZO_FAKE_JJ_FORGET_FAIL:-}\" = 1 ]; then echo 'simulated forget failure' >&2; exit 1; fi\n"
        <> "  exit 0\n"
        <> "fi\n"
        <> "exit 1\n",
    )
  chmod_executable(path)
}

fn write_fake_gh(path: String, log: String) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\n"
        <> "printf 'gh: %s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\n"
        <> "if [ \"$1\" = pr ] && [ \"$2\" = view ]; then if [ -n \"${SCHERZO_FAKE_GH_VIEW_URL:-}\" ]; then echo \"$SCHERZO_FAKE_GH_VIEW_URL\"; exit 0; fi; exit 1; fi\n"
        <> "if [ \"$1\" = pr ] && [ \"$2\" = create ]; then echo https://github.com/example/repo/pull/1; exit 0; fi\n"
        <> "exit 1\n",
    )
  chmod_executable(path)
}

fn find_python3_path(dirs: List(String)) -> String {
  case dirs {
    [] -> "python3"
    [dir, ..rest] -> {
      let candidate = dir <> "/python3"
      case simplifile.is_file(candidate) {
        Ok(True) ->
          case string.starts_with(candidate, "/usr/bin/") {
            True -> find_python3_path(rest)
            False -> candidate
          }
        _ -> find_python3_path(rest)
      }
    }
  }
}

fn install_python3_wrapper(bin: String) -> Nil {
  let search_path = case path.env("PATH") {
    Some(value) -> value
    None -> "/usr/bin:/bin"
  }
  let real_python = find_python3_path(string.split(search_path, on: ":"))
  let assert Ok(Nil) =
    simplifile.write(
      bin <> "/python3",
      "#!/bin/sh\nexec " <> shell_quote(real_python) <> " \"$@\"\n",
    )
  chmod_executable(bin <> "/python3")
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

fn fake_env_with_exact_path(
  workspace: String,
  bin: String,
  log: String,
  extra: List(#(String, String)),
) -> List(#(String, String)) {
  [
    #("SCHERZO_WORKSPACE_PATH", workspace),
    #("SCHERZO_FAKE_JJ_LOG", log),
    #("PATH", bin),
  ]
  |> list.append(extra)
}

fn fake_env_without_workspace_exact_path(
  bin: String,
  log: String,
  extra: List(#(String, String)),
) -> List(#(String, String)) {
  [#("SCHERZO_FAKE_JJ_LOG", log), #("PATH", bin)]
  |> list.append(extra)
}

fn run_jj(
  step_id: String,
  args: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  run_jj_command(step_id, "sh ", args, env)
}

fn run_jj_with_exact_path(
  step_id: String,
  args: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  run_jj_command(step_id, "/bin/sh ", args, env)
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

fn log_text(log: String) -> String {
  let assert Ok(contents) = simplifile.read(log)
  contents
}

fn log_lines(log: String) -> List(String) {
  let contents = log_text(log)
  case string.trim(contents) {
    "" -> []
    trimmed -> string.split(trimmed, on: "\n")
  }
}

fn run_publish_change_with_pr_draft(
  dir: String,
  draft: Option(String),
) -> #(step_artifact.StepArtifact, String) {
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.write(workspace <> "/title.txt", "Title\n")
  let assert Ok(Nil) = simplifile.write(workspace <> "/body.txt", "Body\n")
  write_fake_gh(bin <> "/gh", log)
  let base_env = [
    #("SCHERZO_FAKE_JJ_CHANGED_FILES", "changed.txt\n"),
    #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "origin"),
    #("SCHERZO_PR_REPO", "example/repo"),
  ]
  let env = case draft {
    Some(value) -> list.append(base_env, [#("SCHERZO_PR_DRAFT", value)])
    None -> base_env
  }
  let artifact =
    run_jj(
      "jj_driver_publish_pr_draft",
      "publish-change --kind implementation --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base main@origin --json",
      fake_env(workspace, bin, log, env),
    )
  #(artifact, log_text(log))
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

pub fn jj_driver_lifecycle_create_implements_root_workspace_creation_test() {
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

pub fn jj_driver_explicit_workspace_base_skips_fetch_test() {
  let dir = "test/tmp/jj-workspace-driver-explicit-base"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_explicit_base",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_JJ_WORKSPACE_BASE", "@"),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert !string.contains(logged, " git fetch ")
  assert string.contains(logged, "--revision @")
}

pub fn jj_driver_jj_specific_aliases_override_legacy_pr_names_test() {
  let dir = "test/tmp/jj-workspace-driver-jj-aliases"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_jj_aliases",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
        #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
        #("SCHERZO_PR_REMOTE", "origin"),
        #("SCHERZO_PR_BASE", "main"),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert string.contains(logged, "git fetch --remote upstream --branch trunk")
  assert string.contains(logged, "log -r trunk@upstream")
  assert string.contains(logged, "--revision trunk@upstream")
  assert !string.contains(logged, "--revision main@origin")
}

pub fn jj_driver_legacy_pr_base_names_still_work_test() {
  let dir = "test/tmp/jj-workspace-driver-legacy-base"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_legacy_base",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_PR_REMOTE", "upstream"),
        #("SCHERZO_PR_BASE", "develop"),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert string.contains(logged, "git fetch --remote upstream --branch develop")
  assert string.contains(logged, "--revision develop@upstream")
}

pub fn jj_driver_fetch_base_false_skips_network_test() {
  let dir = "test/tmp/jj-workspace-driver-fetch-false"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_fetch_false",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_JJ_WORKSPACE_FETCH_BASE", "false"),
        #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
        #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert !string.contains(logged, " git fetch ")
  assert string.contains(logged, "--revision trunk@upstream")
}

pub fn jj_driver_invalid_fetch_policy_exits_usage_error_test() {
  let dir = "test/tmp/jj-workspace-driver-invalid-fetch"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_invalid_fetch",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_JJ_WORKSPACE_FETCH_BASE", "maybe"),
      ]),
    )

  assert_exit(artifact, 2)
  assert string.contains(
    artifact.stderr,
    "SCHERZO_JJ_WORKSPACE_FETCH_BASE must be true or false",
  )
  assert !string.contains(log_text(log), "workspace add")
}

pub fn jj_driver_stale_explicit_legacy_base_fails_without_fallback_test() {
  let dir = "test/tmp/jj-workspace-driver-stale-legacy-base"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_stale_legacy_base",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_PR_REMOTE", "upstream"),
        #("SCHERZO_PR_BASE", "develop"),
        #("SCHERZO_FAKE_JJ_MISSING_REVISIONS", "develop@upstream develop"),
      ]),
    )

  assert_exit(artifact, 1)
  assert string.contains(artifact.stderr, "develop")
  assert string.contains(artifact.stderr, "upstream")
  assert string.contains(artifact.stderr, "SCHERZO_JJ_WORKSPACE_BASE_BRANCH")
  assert string.contains(artifact.stderr, "SCHERZO_JJ_WORKSPACE_REMOTE")
  let logged = log_text(log)
  assert !string.contains(logged, "workspace add")
  assert !string.contains(logged, "log -r main@origin")
  assert !string.contains(logged, "log -r main ")
  assert !string.contains(logged, "log -r @ ")
}

pub fn jj_driver_failed_default_fetch_explains_offline_configuration_test() {
  let dir = "test/tmp/jj-workspace-driver-default-fetch-fails"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)

  let artifact =
    run_jj(
      "jj_driver_default_fetch_fails",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_FAKE_JJ_FETCH_FAIL", "1"),
      ]),
    )

  assert_exit(artifact, 1)
  assert string.contains(artifact.stderr, "SCHERZO_JJ_WORKSPACE_BASE=@")
  assert string.contains(
    artifact.stderr,
    "SCHERZO_JJ_WORKSPACE_FETCH_BASE=false",
  )
  assert string.contains(artifact.stderr, "SCHERZO_JJ_WORKSPACE_BASE_BRANCH")
  assert string.contains(artifact.stderr, "SCHERZO_JJ_WORKSPACE_REMOTE")
  assert !string.contains(log_text(log), "workspace add")
}

pub fn jj_driver_derived_workspace_uses_source_at_and_skips_base_fetch_test() {
  let dir = "test/tmp/jj-workspace-driver-derived-workspace"
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)
  let source = absolute(dir <> "/source")
  let assert Ok(Nil) = simplifile.create_directory_all(source <> "/.jj")

  let artifact =
    run_jj(
      "jj_driver_derived_workspace",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_SOURCE_WORKSPACE_PATH", source),
        #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
        #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
        #("SCHERZO_FAKE_JJ_FETCH_FAIL", "1"),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert !string.contains(logged, " git fetch ")
  assert string.contains(logged, "--repository " <> source <> " workspace add")
  assert string.contains(logged, "--revision @")
}

pub fn jj_driver_lifecycle_create_bridges_portable_scherzo_symlinks_test() {
  let dir = tmp_fixture_dir("jj-workspace-driver-portable-symlinks")
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)
  let bridge = absolute(dir <> "/workspaces/scherzo")
  let assert Ok(core_checkout) = path.realpath(".")

  let artifact =
    run_jj(
      "jj_driver_portable_symlink_bridge",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_JJ_WORKSPACE_BASE", "@"),
        #("SCHERZO_FAKE_JJ_PORTABLE_LINKS", "1"),
      ]),
    )

  assert_exit(artifact, 0)
  assert_symlink_target(bridge, core_checkout)
  assert simplifile.is_directory(bridge) == Ok(True)
  assert simplifile.is_directory(workspace <> "/scripts") == Ok(True)
  assert simplifile.is_file(
      workspace
      <> "/.scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    )
    == Ok(True)
  let assert [_, _, "root", "root", "status --color=never"] = log_lines(log)

  let lane = absolute(dir <> "/workspaces/correctness")
  let lane_artifact =
    run_jj(
      "jj_driver_portable_symlink_bridge_lane",
      "lifecycle create",
      fake_env(lane, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_SOURCE_WORKSPACE_PATH", workspace),
        #("SCHERZO_FAKE_JJ_PORTABLE_LINKS", "1"),
      ]),
    )

  assert_exit(lane_artifact, 0)
  assert simplifile.is_file(
      lane
      <> "/.scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    )
    == Ok(True)
}

pub fn jj_driver_lifecycle_create_fails_when_portable_bridge_collides_test() {
  let dir = tmp_fixture_dir("jj-workspace-driver-portable-symlink-collision")
  let #(repo, workspace, bin, log) = setup_driver_fixture(dir)
  let bridge = absolute(dir <> "/workspaces/scherzo")
  let assert Ok(Nil) = simplifile.create_directory_all(bridge)

  let artifact =
    run_jj(
      "jj_driver_portable_symlink_collision",
      "lifecycle create",
      fake_env(workspace, bin, log, [
        #("SCHERZO_REPO_ROOT", repo),
        #("SCHERZO_JJ_WORKSPACE_BASE", "@"),
        #("SCHERZO_FAKE_JJ_PORTABLE_LINKS", "1"),
      ]),
    )

  assert_exit(artifact, 1)
  assert string.contains(artifact.stderr, "portable symlink bridge collision")
  assert string.contains(artifact.stderr, bridge)
  assert !string.contains(log_text(log), "status --color=never")
}

pub fn jj_driver_refresh_base_uses_jj_specific_aliases_test() {
  let dir = "test/tmp/jj-workspace-driver-refresh-aliases"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let artifact =
    run_jj(
      "jj_driver_refresh_aliases",
      "refresh-base --stage pre-validation --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
        #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
      ]),
    )

  assert_exit(artifact, 0)
  assert string.contains(artifact.stdout, "\"stage\":\"pre-validation\"")
  assert string.contains(artifact.stdout, "\"base_ref\":\"trunk\"")
  assert string.contains(
    artifact.stdout,
    "\"base_revision\":\"trunk@upstream\"",
  )
  assert string.contains(artifact.stdout, "\"conflict_files\":[]")
  assert string.contains(
    log_text(log),
    "git fetch --remote upstream --branch trunk",
  )
}

pub fn jj_driver_publish_remote_is_separate_from_base_remote_test() {
  let dir = "test/tmp/jj-workspace-driver-publish-remote"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.write(workspace <> "/title.txt", "Title\n")
  let assert Ok(Nil) = simplifile.write(workspace <> "/body.txt", "Body\n")
  write_fake_gh(bin <> "/gh", log)

  let artifact =
    run_jj(
      "jj_driver_publish_remote",
      "publish-change --kind implementation --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base trunk@upstream --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "changed.txt\n"),
        #("SCHERZO_JJ_WORKSPACE_REMOTE", "upstream"),
        #("SCHERZO_JJ_WORKSPACE_BASE_BRANCH", "trunk"),
        #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "origin"),
        #("SCHERZO_PR_REPO", ""),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert string.contains(logged, "git remote list")
  assert string.contains(logged, "git push --remote origin")
  assert !string.contains(logged, "git push --remote upstream")
}

pub fn jj_driver_publish_remote_legacy_fallback_test() {
  let dir = "test/tmp/jj-workspace-driver-publish-legacy-remote"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.write(workspace <> "/title.txt", "Title\n")
  let assert Ok(Nil) = simplifile.write(workspace <> "/body.txt", "Body\n")
  write_fake_gh(bin <> "/gh", log)

  let artifact =
    run_jj(
      "jj_driver_publish_legacy_remote",
      "publish-change --kind implementation --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base develop@fork --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "changed.txt\n"),
        #("SCHERZO_PR_REMOTE", "fork"),
        #("SCHERZO_PR_REPO", ""),
      ]),
    )

  assert_exit(artifact, 0)
  assert string.contains(log_text(log), "git push --remote fork")
}

pub fn jj_driver_publish_target_branch_allows_stale_local_bookmark_test() {
  let dir = "test/tmp/jj-workspace-driver-publish-target-branch"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.write(workspace <> "/title.txt", "Title\n")
  let assert Ok(Nil) = simplifile.write(workspace <> "/body.txt", "Body\n")
  write_fake_gh(bin <> "/gh", log)

  let artifact =
    run_jj(
      "jj_driver_publish_target_branch",
      "publish-change --kind merge-conflict --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base main@origin --target-branch feature/pr --target-pr 198 --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "changed.txt\n"),
        #(
          "SCHERZO_FAKE_GH_VIEW_URL",
          "https://github.com/example/repo/pull/198",
        ),
        #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "origin"),
        #("SCHERZO_PR_REPO", "example/repo"),
      ]),
    )

  assert_exit(artifact, 0)
  let logged = log_text(log)
  assert string.contains(logged, "bookmark track feature/pr --remote=origin")
  assert string.contains(
    logged,
    "bookmark set --allow-backwards feature/pr --revision @",
  )
  assert string.contains(
    logged,
    "git push --remote origin --bookmark feature/pr",
  )
}

pub fn jj_driver_publish_accepts_workflow_kind_tokens_test() {
  let dir = "test/tmp/jj-workspace-driver-publish-kind-token"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.write(workspace <> "/title.txt", "Title\n")
  let assert Ok(Nil) = simplifile.write(workspace <> "/body.txt", "Body\n")
  write_fake_gh(bin <> "/gh", log)

  let artifact =
    run_jj(
      "jj_driver_publish_kind_token",
      "publish-change --kind execplan --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base main@origin --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "changed.txt\n"),
        #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "origin"),
        #("SCHERZO_PR_REPO", "example/repo"),
      ]),
    )

  assert_exit(artifact, 0)
  assert string.contains(artifact.stdout, "\"status\":\"published\"")
  assert string.contains(log_text(log), "git push --remote origin")
}

pub fn jj_driver_pr_draft_env_controls_create_flag_test() {
  let #(draft_artifact, draft_log) =
    run_publish_change_with_pr_draft(
      "test/tmp/jj-workspace-driver-pr-draft-true",
      Some("true"),
    )
  assert_exit(draft_artifact, 0)
  assert string.contains(draft_log, "gh: pr create")
  assert string.contains(draft_log, "--draft")

  let #(ready_artifact, ready_log) =
    run_publish_change_with_pr_draft(
      "test/tmp/jj-workspace-driver-pr-draft-false",
      Some("false"),
    )
  assert_exit(ready_artifact, 0)
  assert string.contains(ready_log, "gh: pr create")
  assert !string.contains(ready_log, "--draft")

  let #(unset_artifact, unset_log) =
    run_publish_change_with_pr_draft(
      "test/tmp/jj-workspace-driver-pr-draft-unset",
      None,
    )
  assert_exit(unset_artifact, 0)
  assert string.contains(unset_log, "gh: pr create")
  assert !string.contains(unset_log, "--draft")
}

pub fn jj_driver_invalid_pr_draft_env_fails_before_publication_test() {
  let #(artifact, logged) =
    run_publish_change_with_pr_draft(
      "test/tmp/jj-workspace-driver-pr-draft-invalid",
      Some("maybe"),
    )

  assert_exit(artifact, 1)
  assert string.contains(
    artifact.stdout,
    "\"failure_code\":\"invalid_configuration\"",
  )
  assert string.contains(
    artifact.stdout,
    "SCHERZO_PR_DRAFT must be true or false",
  )
  assert logged == ""
}

pub fn jj_driver_publish_rejects_invalid_kind_tokens_test() {
  let dir = "test/tmp/jj-workspace-driver-publish-invalid-kind"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let artifact =
    run_jj(
      "jj_driver_publish_invalid_kind",
      "publish-change --kind ../bad --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base main@origin --json",
      fake_env(workspace, bin, log, []),
    )

  assert_exit(artifact, 2)
  assert string.contains(artifact.stderr, "publish-change --kind")
  assert log_lines(log) == []
}

pub fn jj_driver_missing_gh_fails_only_publish_change_test() {
  let dir = "test/tmp/jj-workspace-driver-missing-gh"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  install_python3_wrapper(bin)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.write(workspace <> "/title.txt", "Title\n")
  let assert Ok(Nil) = simplifile.write(workspace <> "/body.txt", "Body\n")

  let describe =
    run_jj_with_exact_path(
      "jj_driver_missing_gh_describe",
      "describe --json",
      fake_env_without_workspace_exact_path(bin, log, []),
    )
  assert_exit(describe, 0)
  assert log_lines(log) == []

  let publish =
    run_jj_with_exact_path(
      "jj_driver_missing_gh_publish",
      "publish-change --kind implementation --title-file title.txt --body-file body.txt --branch-prefix scherzo/test --base main@origin --json",
      fake_env_with_exact_path(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "changed.txt\n"),
        #("SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE", "origin"),
        #("SCHERZO_PR_REPO", ""),
      ]),
    )

  assert_exit(publish, 1)
  assert string.contains(
    publish.stdout,
    "\"failure_code\":\"command_not_found\"",
  )
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
    == ["diff --name-only --color=never", "diff --summary --color=never"]
}

pub fn jj_driver_changed_files_normalizes_jj_brace_rename_summaries_test() {
  let dir = "test/tmp/jj-workspace-driver-brace-renames"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let artifact =
    run_jj(
      "jj_driver_changed_brace_renames",
      "changed-files --json",
      fake_env(workspace, bin, log, [
        #(
          "SCHERZO_FAKE_JJ_CHANGED_FILES",
          "workflows/dogfood/execplan.yaml\n"
            <> "workflows/dogfood/prompts/new.md\n"
            <> "workflows/dogfood/scripts/scherzo-review\n",
        ),
        #(
          "SCHERZO_FAKE_JJ_DIFF",
          "R {.scherzo/workflows => workflows/dogfood}/execplan.yaml\n"
            <> "R {.scherzo/workflows/prompts/old.md => workflows/dogfood/prompts/new.md}\n"
            <> "R {scripts => workflows/dogfood/scripts}/scherzo-review\n",
        ),
      ]),
    )

  assert_exit(artifact, 0)
  assert decode_paths(artifact.stdout)
    == [
      "workflows/dogfood/execplan.yaml",
      "workflows/dogfood/prompts/new.md",
      "workflows/dogfood/scripts/scherzo-review",
    ]
  assert !string.contains(artifact.stdout, "dogfood}")
  assert !string.contains(artifact.stdout, "scripts}")
  assert log_lines(log)
    == ["diff --name-only --color=never", "diff --summary --color=never"]
}

pub fn jj_driver_changed_files_uses_default_current_change_diff_for_merge_revisions_test() {
  let dir = "test/tmp/jj-workspace-driver-changed-files-merge"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let artifact =
    run_jj(
      "jj_driver_changed_merge",
      "changed-files --json",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_CHANGED_FILES", "src/merged.gleam\n"),
        #("SCHERZO_FAKE_JJ_FAIL_ON_AT_MINUS", "1"),
      ]),
    )

  assert_exit(artifact, 0)
  assert decode_paths(artifact.stdout) == ["src/merged.gleam"]
  assert artifact.stderr == ""
  assert log_lines(log)
    == ["diff --name-only --color=never", "diff --summary --color=never"]
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
      "diff --color=never",
      "diff --git --color=never",
    ]
}

pub fn jj_driver_status_human_updates_stale_workspace_once_test() {
  let dir = "test/tmp/jj-workspace-driver-status-stale"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let count_file = absolute(dir <> "/status-count")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let status =
    run_jj(
      "jj_driver_status_stale_once",
      "status --human",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_STATUS_COUNT_FILE", count_file),
        #("SCHERZO_FAKE_JJ_STATUS_FAIL_ONCE", "1"),
        #(
          "SCHERZO_FAKE_JJ_STATUS_FAIL_OUTPUT",
          "working copy is stale; run `jj workspace update-stale`\n",
        ),
        #("SCHERZO_FAKE_JJ_STATUS", "working copy clean\n"),
      ]),
    )

  assert_exit(status, 0)
  assert status.stdout == "working copy clean\n"
  assert string.contains(status.stderr, "jj workspace update-stale")
  assert log_lines(log)
    == [
      "status --color=never",
      "workspace update-stale",
      "status --color=never",
    ]
}

pub fn jj_driver_status_human_does_not_retry_non_stale_failure_test() {
  let dir = "test/tmp/jj-workspace-driver-status-non-stale-failure"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let count_file = absolute(dir <> "/status-count")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let status =
    run_jj(
      "jj_driver_status_non_stale_failure",
      "status --human",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_STATUS_COUNT_FILE", count_file),
        #("SCHERZO_FAKE_JJ_STATUS_FAIL_ONCE", "1"),
        #("SCHERZO_FAKE_JJ_STATUS_FAIL_OUTPUT", "plain status failure\n"),
      ]),
    )

  assert_exit(status, 1)
  assert string.contains(status.stderr, "plain status failure")
  assert log_lines(log) == ["status --color=never"]
}

pub fn jj_driver_status_human_fails_when_update_stale_fails_test() {
  let dir = "test/tmp/jj-workspace-driver-status-update-fails"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let count_file = absolute(dir <> "/status-count")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let status =
    run_jj(
      "jj_driver_status_update_failure",
      "status --human",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_STATUS_COUNT_FILE", count_file),
        #("SCHERZO_FAKE_JJ_STATUS_FAIL_ONCE", "1"),
        #(
          "SCHERZO_FAKE_JJ_STATUS_FAIL_OUTPUT",
          "working copy is stale; run `jj workspace update-stale`\n",
        ),
        #("SCHERZO_FAKE_JJ_UPDATE_STALE_FAIL", "1"),
        #("SCHERZO_FAKE_JJ_UPDATE_STALE_OUTPUT", "update stale failed\n"),
      ]),
    )

  assert_exit(status, 1)
  assert string.contains(status.stderr, "update stale failed")
  assert log_lines(log) == ["status --color=never", "workspace update-stale"]
}

pub fn jj_driver_status_human_fails_when_retried_status_fails_test() {
  let dir = "test/tmp/jj-workspace-driver-status-retry-fails"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let count_file = absolute(dir <> "/status-count")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)

  let status =
    run_jj(
      "jj_driver_status_retry_failure",
      "status --human",
      fake_env(workspace, bin, log, [
        #("SCHERZO_FAKE_JJ_STATUS_COUNT_FILE", count_file),
        #("SCHERZO_FAKE_JJ_STATUS_FAIL_ONCE", "1"),
        #(
          "SCHERZO_FAKE_JJ_STATUS_FAIL_OUTPUT",
          "working copy is stale; run `jj workspace update-stale`\n",
        ),
        #("SCHERZO_FAKE_JJ_STATUS_FAIL_ON_RETRY", "1"),
        #("SCHERZO_FAKE_JJ_STATUS_RETRY_FAIL_OUTPUT", "still stale\n"),
      ]),
    )

  assert_exit(status, 1)
  assert string.contains(status.stderr, "still stale")
  assert log_lines(log)
    == [
      "status --color=never",
      "workspace update-stale",
      "status --color=never",
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

pub fn jj_driver_lifecycle_before_step_verifies_workspace_test() {
  let dir = "test/tmp/jj-workspace-driver-before-step"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/.jj")

  let artifact =
    run_jj(
      "jj_driver_before_step",
      "lifecycle before-step",
      fake_env(workspace, bin, log, []),
    )

  assert_exit(artifact, 0)
  assert log_lines(log) == ["root", "root", "status --color=never"]
}

pub fn jj_driver_lifecycle_before_step_reports_broken_portable_symlink_test() {
  let dir = tmp_fixture_dir("jj-workspace-driver-before-step-broken-symlink")
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/.jj")
  let assert Ok(Nil) =
    path.symlink("../not-scherzo/scripts", workspace <> "/scripts")

  let artifact =
    run_jj(
      "jj_driver_before_step_broken_symlink",
      "lifecycle before-step",
      fake_env(workspace, bin, log, []),
    )

  assert_exit(artifact, 1)
  assert string.contains(artifact.stderr, "portable symlink preflight failed")
  assert string.contains(artifact.stderr, "scripts")
  assert string.contains(artifact.stderr, "before review lanes")
  assert log_lines(log) == ["root", "root"]
}

pub fn jj_driver_lifecycle_remove_skips_portable_scherzo_bridge_test() {
  let dir = tmp_fixture_dir("jj-workspace-driver-remove-portable-bridge")
  let #(_, _, bin, log) = setup_driver_fixture(dir)
  let run_root = absolute(dir <> "/run")
  let workspace = run_root <> "/workspaces/main"
  let core_checkout = absolute(dir <> "/core")
  let bridge = run_root <> "/workspaces/scherzo"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/.jj")
  let assert Ok(Nil) = simplifile.create_directory_all(core_checkout <> "/.jj")
  let assert Ok(Nil) = path.symlink(core_checkout, bridge)

  let artifact =
    run_jj(
      "jj_driver_remove_skips_portable_bridge",
      "lifecycle remove",
      fake_env(workspace, bin, log, [#("SCHERZO_RUN_ROOT", run_root)]),
    )

  assert_exit(artifact, 0)
  assert log_lines(log) == ["root", "workspace forget"]
}

pub fn jj_driver_lifecycle_remove_forgets_run_workspaces_test() {
  let dir = "test/tmp/jj-workspace-driver-remove-run"
  let #(_, _, bin, log) = setup_driver_fixture(dir)
  let run_root = absolute(dir <> "/run")
  let workspace = run_root <> "/workspaces/main"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/.jj")

  let artifact =
    run_jj(
      "jj_driver_remove_forget",
      "lifecycle remove",
      fake_env(workspace, bin, log, [#("SCHERZO_RUN_ROOT", run_root)]),
    )

  assert_exit(artifact, 0)
  assert log_lines(log) == ["root", "workspace forget"]
}

pub fn jj_driver_lifecycle_remove_surfaces_forget_failure_test() {
  let dir = "test/tmp/jj-workspace-driver-remove-forget-failure"
  let #(_, _, bin, log) = setup_driver_fixture(dir)
  let run_root = absolute(dir <> "/run")
  let workspace = run_root <> "/workspaces/main"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/.jj")

  let artifact =
    run_jj(
      "jj_driver_remove_forget_failure",
      "lifecycle remove",
      fake_env(workspace, bin, log, [
        #("SCHERZO_RUN_ROOT", run_root),
        #("SCHERZO_FAKE_JJ_FORGET_FAIL", "1"),
      ]),
    )

  assert_exit(artifact, 1)
  assert string.contains(artifact.stderr, "simulated forget failure")
  assert string.contains(artifact.stderr, "could not forget jj workspace")
  assert simplifile.is_directory(run_root) == Ok(True)
  assert log_lines(log) == ["root", "workspace forget"]
}

pub fn jj_driver_lifecycle_remove_rejects_unsafe_targets_test() {
  let dir = "test/tmp/jj-workspace-driver-remove-unsafe-targets"
  let #(_, workspace, bin, log) = setup_driver_fixture(dir)
  let run_root = absolute(dir <> "/run")

  let root =
    run_jj(
      "jj_driver_remove_root",
      "lifecycle remove",
      fake_env("/", bin, log, []),
    )
  assert_exit(root, 2)
  assert string.contains(root.stderr, "refusing filesystem root")

  let outside_run_root =
    run_jj(
      "jj_driver_remove_outside_run_root",
      "lifecycle remove",
      fake_env(workspace, bin, log, [#("SCHERZO_RUN_ROOT", run_root)]),
    )
  assert_exit(outside_run_root, 2)
  assert string.contains(outside_run_root.stderr, "outside SCHERZO_RUN_ROOT")

  let assert Ok(Nil) = simplifile.create_directory_all(run_root)
  let run_root_itself =
    run_jj(
      "jj_driver_remove_run_root_itself",
      "lifecycle remove",
      fake_env(run_root, bin, log, [#("SCHERZO_RUN_ROOT", run_root)]),
    )
  assert_exit(run_root_itself, 2)
  assert string.contains(run_root_itself.stderr, "outside SCHERZO_RUN_ROOT")
  assert simplifile.is_directory(run_root) == Ok(True)
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
