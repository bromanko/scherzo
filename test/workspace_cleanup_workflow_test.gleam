import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_repository/command_runner
import simplifile
import support/test_helpers

pub fn scheduled_workspace_cleanup_persists_and_reuses_cursor_test() {
  let repo = "test/tmp/workspace-cleanup-workflow/persist"
  test_helpers.reset_dir(repo)
  let workflow_script = write_workflow_script(repo)
  let fake_ctl = write_fake_scherzoctl(repo)
  let cursor_path =
    repo
    <> "/workspaces/.scherzo-state/cleanup/scheduled/workspace-cleanup.cursor"
  let output_path = repo <> "/cleanup-output.json"
  let log_path = repo <> "/cleanup-invocations.log"

  let assert Ok(Nil) =
    simplifile.write(
      output_path,
      "{\"truncated\":true,\"next_cursor\":\"cursor-1\"}\n",
    )
  let first =
    run_workflow_script(
      repo,
      workflow_script,
      fake_ctl,
      output_path,
      log_path,
      None,
      None,
      None,
    )
  assert first.exit_code == 0
  let assert Ok(saved_cursor) = simplifile.read(cursor_path)
  assert string.trim(saved_cursor) == "cursor-1"
  let assert Ok(first_log) = simplifile.read(log_path)
  assert string.contains(first_log, "cleanup --root")
  assert !string.contains(first_log, "--cursor cursor-1")

  let assert Ok(Nil) =
    simplifile.write(
      output_path,
      "{\"truncated\":false,\"next_cursor\":null}\n",
    )
  let second =
    run_workflow_script(
      repo,
      workflow_script,
      fake_ctl,
      output_path,
      log_path,
      None,
      None,
      None,
    )
  assert second.exit_code == 0
  let assert Ok(False) = simplifile.is_file(cursor_path)
  let assert Ok(second_log) = simplifile.read(log_path)
  assert string.contains(second_log, "--cursor cursor-1")
}

pub fn scheduled_workspace_cleanup_clears_invalid_cursor_test() {
  let repo = "test/tmp/workspace-cleanup-workflow/invalid-cursor"
  test_helpers.reset_dir(repo)
  let workflow_script = write_workflow_script(repo)
  let fake_ctl = write_fake_scherzoctl(repo)
  let cursor_path =
    repo
    <> "/workspaces/.scherzo-state/cleanup/scheduled/workspace-cleanup.cursor"
  let output_path = repo <> "/cleanup-output.json"
  let log_path = repo <> "/cleanup-invocations.log"
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      repo <> "/workspaces/.scherzo-state/cleanup/scheduled",
    )
  let assert Ok(Nil) = simplifile.write(cursor_path, "bad-cursor\n")
  let assert Ok(Nil) =
    simplifile.write(
      output_path,
      "{\"truncated\":true,\"next_cursor\":\"ignored\"}\n",
    )

  let result =
    run_workflow_script(
      repo,
      workflow_script,
      fake_ctl,
      output_path,
      log_path,
      Some("cleanup_wrong_root: bad-cursor"),
      Some("17"),
      None,
    )

  assert result.exit_code == 0
  let assert Ok(False) = simplifile.is_file(cursor_path)
  let assert Ok(log) = simplifile.read(log_path)
  assert string.contains(log, "--cursor bad-cursor")
}

fn run_workflow_script(
  repo: String,
  workflow_script: String,
  fake_ctl: String,
  output_path: String,
  log_path: String,
  error_text: Option(String),
  error_code: Option(String),
  workspace_root: Option(String),
) -> command_runner.CommandOutput {
  let runner = command_runner.production_with_env(fn(_) { None })
  let command_runner.Runner(run: run) = runner
  let spec =
    command_runner.sh("bash", [workflow_script], ".")
    |> command_runner.with_env([
      #("SCHERZO_CONFIG_DIR", repo <> "/.scherzo"),
      #("SCHERZO_CTL", fake_ctl),
      #("SCHERZO_REPO_ROOT", repo),
      #("FAKE_OUTPUT_PATH", output_path),
      #("FAKE_INVOCATIONS_LOG", log_path),
      #("FAKE_ERROR_TEXT", case error_text {
        Some(value) -> value
        None -> ""
      }),
      #("FAKE_ERROR_CODE", case error_code {
        Some(value) -> value
        None -> "1"
      }),
      #("SCHERZO_CLEANUP_WORKSPACE_ROOT", case workspace_root {
        Some(value) -> value
        None -> repo <> "/workspaces"
      }),
    ])
    |> command_runner.with_timeout_ms(5000)
  let assert Ok(output) = run(spec)
  output
}

fn write_workflow_script(repo: String) -> String {
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/.scherzo")
  let assert Ok(contents) =
    simplifile.read("workflows/dogfood/workspace-cleanup.yaml")
  let script_path = repo <> "/workspace-cleanup.sh"
  let assert Ok(Nil) =
    simplifile.write(
      script_path,
      "#!/bin/sh\n" <> extract_run_block(contents) <> "\n",
    )
  test_helpers.chmod_executable(script_path)
  script_path
}

fn extract_run_block(contents: String) -> String {
  let assert [_, after] = string.split(contents, on: "    run: |\n")
  let assert [block, _] = string.split(after, on: "\n    timeout:")
  block
  |> string.split(on: "\n")
  |> list.map(fn(line) {
    case string.starts_with(line, "      ") {
      True -> string.drop_start(line, 6)
      False -> line
    }
  })
  |> string.join(with: "\n")
}

fn write_fake_scherzoctl(repo: String) -> String {
  let path = repo <> "/fake-scherzoctl.sh"
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\nset -eu\nprintf '%s\\n' \"$*\" >> \"$FAKE_INVOCATIONS_LOG\"\nif [ -n \"${FAKE_ERROR_TEXT:-}\" ]; then\n  printf '%s\\n' \"$FAKE_ERROR_TEXT\" >&2\n  exit \"${FAKE_ERROR_CODE:-1}\"\nfi\ncat \"$FAKE_OUTPUT_PATH\"\n",
    )
  test_helpers.chmod_executable(path)
  path
}
