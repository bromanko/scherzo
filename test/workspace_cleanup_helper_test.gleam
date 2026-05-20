import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path
import scherzo/step_artifact
import simplifile

pub fn cleanup_removes_empty_noop_run_and_keeps_active_or_file_work_test() {
  let dir = "test/tmp/workspace-cleanup-helper"
  reset_dir(dir)
  let workspace_root = dir <> "/workspaces"
  let empty_run = workspace_root <> "/implementation/LIV-1/run-empty"
  let active_run = workspace_root <> "/implementation/LIV-2/run-active"
  let file_work_run = workspace_root <> "/implementation/LIV-3/run-file-work"

  create_noop_workspace(empty_run, "main")
  create_noop_workspace(active_run, "main")
  create_noop_workspace(file_work_run, "main")
  let assert Ok(Nil) =
    simplifile.write(
      file_work_run <> "/workspaces/main/notes.txt",
      "operator notes\n",
    )
  write_active_ledger(workspace_root, active_run, "run-active")

  let artifact =
    command_step.run(
      "workspace_cleanup_apply",
      "scripts/scherzo-workspace-cleanup --workspace-root "
        <> shell_quote(workspace_root)
        <> " --min-age-seconds 0 --apply --json",
      ".",
      10_000,
      [],
      limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert_is_not_directory(empty_run)
  assert_is_directory(active_run)
  assert_is_directory(file_work_run)
  assert string.contains(artifact.stdout, "\"removed_count\":1")
  assert string.contains(artifact.stdout, "\"kept_count\":2")
}

fn create_noop_workspace(run_root: String, workspace_name: String) -> Nil {
  let workspace = run_root <> "/workspaces/" <> workspace_name
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) =
    simplifile.write(
      workspace <> "/.scherzo-workspace-driver-noop",
      "created by test\n",
    )
  Nil
}

fn write_active_ledger(
  workspace_root: String,
  run_root: String,
  run_id: String,
) -> Nil {
  let ledger_dir = workspace_root <> "/.scherzo-state/ledger"
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_dir)
  let assert Ok(run_root_abs) = path.absolute(run_root)
  let record =
    "{\"kind\":\"scheduled_run_started\",\"run_id\":\""
    <> run_id
    <> "\",\"run_root\":\""
    <> json_escape(run_root_abs)
    <> "\"}\n"
  let assert Ok(Nil) = simplifile.write(ledger_dir <> "/current.jsonl", record)
  Nil
}

fn assert_is_directory(path: String) -> Nil {
  let assert Ok(True) = simplifile.is_directory(path)
  Nil
}

fn assert_is_not_directory(path: String) -> Nil {
  case simplifile.is_directory(path) {
    Ok(False) -> Nil
    Ok(True) -> panic as "expected path not to be a directory"
    Error(_) -> Nil
  }
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

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn json_escape(value: String) -> String {
  value
  |> string.replace(each: "\\", with: "\\\\")
  |> string.replace(each: "\"", with: "\\\"")
}
