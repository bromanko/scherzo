import gleam/dynamic/decode
import gleam/json
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path
import scherzo/step_artifact
import simplifile

const marker = ".scherzo-workspace-driver-noop"

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

fn run_noop(
  step_id: String,
  args: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  let script = absolute("scripts/scherzo-workspace-noop")
  command_step.run_with_env(
    step_id,
    "sh " <> shell_quote(script) <> " " <> args,
    ".",
    5000,
    env,
    [],
    limits(),
  )
}

fn workspace_env(workspace: String) -> List(#(String, String)) {
  [#("SCHERZO_WORKSPACE_PATH", workspace)]
}

fn workspace_run_env(
  workspace: String,
  run_root: String,
) -> List(#(String, String)) {
  [#("SCHERZO_WORKSPACE_PATH", workspace), #("SCHERZO_RUN_ROOT", run_root)]
}

fn assert_exit(artifact: step_artifact.StepArtifact, code: Int) -> Nil {
  assert artifact.exit_code == Some(code)
  Nil
}

fn changed_file_record_decoder() -> decode.Decoder(#(String, String)) {
  use path <- decode.field("path", decode.string)
  use status <- decode.field("status", decode.string)
  decode.success(#(path, status))
}

fn changed_files_decoder() -> decode.Decoder(#(Int, List(#(String, String)))) {
  use version <- decode.field("version", decode.int)
  use files <- decode.field(
    "files",
    decode.list(of: changed_file_record_decoder()),
  )
  decode.success(#(version, files))
}

fn driver_description_decoder() -> decode.Decoder(#(Int, List(String))) {
  use version <- decode.field("version", decode.int)
  use capabilities <- decode.field(
    "capabilities",
    decode.list(of: decode.string),
  )
  decode.success(#(version, capabilities))
}

fn decode_changed_files(value: String) -> #(Int, List(#(String, String))) {
  let assert Ok(records) = json.parse(value, changed_files_decoder())
  records
}

fn decode_driver_description(value: String) -> #(Int, List(String)) {
  let assert Ok(description) = json.parse(value, driver_description_decoder())
  description
}

fn write_file(path: String, contents: String) -> Nil {
  let assert Ok(Nil) = simplifile.write(path, contents)
  Nil
}

pub fn noop_driver_describe_json_is_static_and_workspace_free_test() {
  let artifact = run_noop("noop_describe_json", "describe --json", [])
  assert_exit(artifact, 0)
  assert artifact.stderr == ""
  assert decode_driver_description(artifact.stdout)
    == #(1, ["status", "changed-files", "assert-only"])

  let unsupported = run_noop("noop_describe_unsupported", "describe --yaml", [])
  assert_exit(unsupported, 2)
  assert string.contains(unsupported.stderr, "describe requires --json")
}

pub fn noop_driver_lifecycle_create_before_after_and_remove_test() {
  let root = "test/tmp/workspace-driver-noop-lifecycle"
  reset_dir(root)
  let run_root = absolute(root <> "/run")
  let workspace = absolute(root <> "/run/workspaces/workspace")

  let create =
    run_noop(
      "noop_lifecycle_create",
      "lifecycle create",
      workspace_run_env(workspace, run_root),
    )
  assert_exit(create, 0)
  assert simplifile.is_directory(workspace) == Ok(True)
  assert simplifile.is_file(workspace <> "/" <> marker) == Ok(True)

  let before =
    run_noop(
      "noop_lifecycle_before",
      "lifecycle before-step",
      workspace_run_env(workspace, run_root),
    )
  assert_exit(before, 0)

  let after =
    run_noop(
      "noop_lifecycle_after",
      "lifecycle after-step",
      workspace_run_env(workspace, run_root),
    )
  assert_exit(after, 0)

  let changed =
    run_noop(
      "noop_lifecycle_changed",
      "changed-files --json",
      workspace_env(workspace),
    )
  assert_exit(changed, 0)
  assert changed.stdout == "{\"version\":1,\"files\":[]}\n"

  let remove =
    run_noop(
      "noop_lifecycle_remove",
      "lifecycle remove",
      workspace_run_env(workspace, run_root),
    )
  assert_exit(remove, 0)
  assert simplifile.is_directory(workspace) == Ok(False)
}

pub fn noop_driver_lifecycle_remove_rejects_unset_empty_unmarked_and_outside_run_root_test() {
  let root = "test/tmp/workspace-driver-noop-remove-rejects"
  reset_dir(root)
  let run_root = absolute(root <> "/run")
  let unmarked = absolute(root <> "/run/workspaces/unmarked")
  let outside = absolute(root <> "/outside/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(unmarked)
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  write_file(outside <> "/" <> marker, "created by test\n")

  let unset = run_noop("noop_remove_unset", "lifecycle remove", [])
  assert_exit(unset, 2)

  let empty =
    run_noop("noop_remove_empty", "lifecycle remove", [
      #("SCHERZO_WORKSPACE_PATH", ""),
    ])
  assert_exit(empty, 2)

  let unmarked_result =
    run_noop(
      "noop_remove_unmarked",
      "lifecycle remove",
      workspace_run_env(unmarked, run_root),
    )
  assert_exit(unmarked_result, 2)
  assert simplifile.is_directory(unmarked) == Ok(True)

  let outside_result =
    run_noop(
      "noop_remove_outside",
      "lifecycle remove",
      workspace_run_env(outside, run_root),
    )
  assert_exit(outside_result, 2)
  assert simplifile.is_directory(outside) == Ok(True)
}

pub fn noop_driver_changed_files_json_is_sorted_relative_and_empty_safe_test() {
  let root = "test/tmp/workspace-driver-noop-changed-files"
  reset_dir(root)
  let workspace = absolute(root <> "/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/nested")

  let empty =
    run_noop(
      "noop_changed_empty",
      "changed-files --json",
      workspace_env(workspace),
    )
  assert_exit(empty, 0)
  assert empty.stdout == "{\"version\":1,\"files\":[]}\n"

  write_file(workspace <> "/zeta.md", "z\n")
  write_file(workspace <> "/nested/alpha.md", "a\n")

  let changed =
    run_noop(
      "noop_changed_sorted",
      "changed-files --json",
      workspace_env(workspace),
    )
  assert_exit(changed, 0)
  assert decode_changed_files(changed.stdout)
    == #(1, [
      #("nested/alpha.md", "modified"),
      #("zeta.md", "modified"),
    ])
}

pub fn noop_driver_status_human_is_deterministic_and_relative_test() {
  let root = "test/tmp/workspace-driver-noop-status"
  reset_dir(root)
  let workspace = absolute(root <> "/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace <> "/nested")

  let empty =
    run_noop("noop_status_empty", "status --human", workspace_env(workspace))
  assert_exit(empty, 0)
  assert empty.stdout == "No files\n"

  write_file(workspace <> "/zeta.md", "z\n")
  write_file(workspace <> "/nested/alpha.md", "a\n")

  let populated =
    run_noop(
      "noop_status_populated",
      "status --human",
      workspace_env(workspace),
    )
  assert_exit(populated, 0)
  assert populated.stdout == "Files:\nnested/alpha.md\nzeta.md\n"
  assert !string.contains(populated.stdout, workspace)
}

pub fn noop_driver_changed_files_json_escapes_special_path_names_test() {
  let root = "test/tmp/workspace-driver-noop-special-paths"
  reset_dir(root)
  let workspace = absolute(root <> "/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_file(workspace <> "/space name.md", "space\n")
  write_file(workspace <> "/quote\"name.md", "quote\n")
  write_file(workspace <> "/backslash\\name.md", "backslash\n")

  let changed =
    run_noop(
      "noop_changed_special",
      "changed-files --json",
      workspace_env(workspace),
    )
  assert_exit(changed, 0)
  assert decode_changed_files(changed.stdout)
    == #(1, [
      #("backslash\\name.md", "modified"),
      #("quote\"name.md", "modified"),
      #("space name.md", "modified"),
    ])
}

pub fn noop_driver_assert_only_accepts_exact_single_file_test() {
  let root = "test/tmp/workspace-driver-noop-assert-only-success"
  reset_dir(root)
  let workspace = absolute(root <> "/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_file(workspace <> "/research-findings.md", "findings\n")

  let artifact =
    run_noop(
      "noop_assert_success",
      "assert-only --path research-findings.md",
      workspace_env(workspace),
    )
  assert_exit(artifact, 0)
  assert artifact.stdout == ""
  assert artifact.stderr == ""
}

pub fn noop_driver_assert_only_rejects_extra_file_test() {
  let root = "test/tmp/workspace-driver-noop-assert-only-extra"
  reset_dir(root)
  let workspace = absolute(root <> "/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  write_file(workspace <> "/research-findings.md", "findings\n")
  write_file(workspace <> "/notes.md", "notes\n")

  let artifact =
    run_noop(
      "noop_assert_extra",
      "assert-only --path research-findings.md",
      workspace_env(workspace),
    )
  assert_exit(artifact, 1)
  assert string.contains(artifact.stderr, "research-findings.md")
  assert string.contains(artifact.stderr, "notes.md")
  assert !string.contains(artifact.stderr, workspace)
}

pub fn driver_assert_only_rejects_unsafe_paths_test() {
  let root = "test/tmp/workspace-driver-noop-unsafe-paths"
  reset_dir(root)
  let workspace = absolute(root <> "/workspace")
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let absolute_path = absolute(root <> "/outside.md")

  let unsafe_paths = ["", "../outside.md", ".", absolute_path]
  list_each(unsafe_paths, fn(value) {
    let artifact =
      run_noop(
        "noop_assert_unsafe",
        "assert-only --path " <> shell_quote(value),
        workspace_env(workspace),
      )
    assert_exit(artifact, 2)
  })
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
