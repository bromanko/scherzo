import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/cleanup
import scherzo/path
import scherzo/state/ledger
import scherzo/state/record
import scherzo/workspace_manifest
import simplifile
import support/test_helpers

pub fn workspace_cleanup_inventory_protects_active_unmanaged_retained_and_unsafe_runs_test() {
  let repo = "test/tmp/cleanup-workspaces/inventory"
  let workspace_root = setup_repo(repo)
  let eligible =
    create_manifest_run(repo, workspace_root, "run-eligible", "main")
  let active = create_manifest_run(repo, workspace_root, "run-active", "main")
  let retained =
    create_manifest_run(repo, workspace_root, "run-retained", "main")
  let unsafe = create_manifest_run(repo, workspace_root, "run-unsafe", "main")
  let unmanaged = workspace_root <> "/implementation/LIV-4/run-unmanaged"
  let assert Ok(Nil) =
    simplifile.create_directory_all(unmanaged <> "/workspaces/main")
  let assert Ok(Nil) =
    simplifile.write(
      workspace_root <> "/.scherzo-state/ledger/current.jsonl",
      record.to_string(record.with_id(
        "active-run",
        1,
        record.WorkflowRunStarted(
          "run-active",
          "implementation",
          "fingerprint",
          "issue-id",
          "LIV-2",
          "issue-fingerprint",
          1,
          active,
        ),
      ))
        <> "\n",
    )
  let assert Ok(Nil) =
    simplifile.write(retained <> "/.scherzo-keep-workspace", "keep\n")
  let outside = repo <> "/outside"
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let assert Ok(Nil) = simplifile.write(outside <> "/sentinel", "keep")
  let assert Ok(Nil) = simplifile.delete(unsafe <> "/workspaces/main")
  let assert Ok(outside_abs) = path.absolute(outside)
  let assert Ok(Nil) = path.symlink(outside_abs, unsafe <> "/workspaces/main")

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, eligible) == Some("would_delete")
  assert item_status(items, active) == Some("retained")
  assert item_status(items, retained) == Some("retained")
  assert item_status(items, unmanaged) == Some("retained")
  assert item_status(items, unsafe) == Some("retained")
  assert item_reason_contains(items, active, "active")
  assert item_reason_contains(items, retained, "retention marker")
  assert item_reason_contains(items, unmanaged, "manifest")
  assert item_reason_contains(items, unsafe, "realpath escapes run root")
}

pub fn workspace_cleanup_apply_delegates_remove_and_reports_failures_test() {
  let repo = "test/tmp/cleanup-workspaces/apply"
  let workspace_root = setup_repo(repo)
  let eligible =
    create_manifest_run(repo, workspace_root, "run-eligible", "main")
  let failing =
    create_manifest_run(repo, workspace_root, "run-failing", "review")
  let outside = repo <> "/outside"
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let assert Ok(Nil) = simplifile.write(outside <> "/sentinel", "keep")
  let assert Ok(Nil) =
    path.symlink(outside, eligible <> "/workspaces/outside-sentinel")
  let assert Ok(Nil) =
    simplifile.write(repo <> "/remove-fail-workspace", "review\n")

  let report =
    with_env("SCHERZO_TEST_LINEAR_API_KEY", "linearkey", fn() {
      cleanup.apply(workspace_root, 0)
    })
  let items = workspace_items(report)

  assert item_status(items, eligible) == Some("deleted")
  assert item_status(items, failing) == Some("failed")
  assert item_reason_contains(items, failing, "driver lifecycle remove failed")
  let assert Ok(False) = simplifile.is_directory(eligible)
  let assert Ok(True) = simplifile.is_directory(failing)
  let assert Ok(True) = simplifile.is_file(outside <> "/sentinel")

  let second =
    with_env("SCHERZO_TEST_LINEAR_API_KEY", "linearkey", fn() {
      cleanup.apply(workspace_root, 0)
    })
  let second_items = workspace_items(second)
  assert item_status(second_items, failing) == Some("failed")
  assert item_status(second_items, eligible) == None

  let encoded = cleanup.cleanup_report_to_json(report) |> json.to_string
  assert string.contains(encoded, "\"provider_id\":\"workspaces\"")
  assert string.contains(encoded, "\"status\":\"deleted\"")
  assert string.contains(encoded, "\"status\":\"failed\"")
}

pub fn workspace_cleanup_apply_retains_all_when_active_ledger_unreadable_test() {
  let repo = "test/tmp/cleanup-workspaces/ledger-unreadable"
  let workspace_root = setup_repo(repo)
  let run_root = create_manifest_run(repo, workspace_root, "run-active", "main")
  let assert Ok(Nil) =
    simplifile.write(
      workspace_root <> "/.scherzo-state/ledger/current.jsonl",
      "not-json\n",
    )

  let report =
    with_env("SCHERZO_TEST_LINEAR_API_KEY", "linearkey", fn() {
      cleanup.apply(workspace_root, 0)
    })
  let provider = workspace_provider(report)
  let items = provider.items

  assert provider.available == False
  assert item_status(items, run_root) == Some("unavailable")
  assert item_reason_contains(items, run_root, "active-run ledger unavailable")
  let assert Ok(True) = simplifile.is_directory(run_root)
}

pub fn workspace_cleanup_inventory_skips_directory_symlink_roots_test() {
  let repo = "test/tmp/cleanup-workspaces/symlink-discovery"
  let workspace_root = setup_repo(repo)
  let outside = repo <> "/outside-run"
  let link_parent = workspace_root <> "/implementation/LIV-9"
  let link_root = link_parent <> "/linked-run"
  let assert Ok(Nil) =
    simplifile.create_directory_all(outside <> "/workspaces/main")
  let assert Ok(Nil) =
    simplifile.write(outside <> "/workspaces/main/sentinel", "keep")
  let assert Ok(Nil) = simplifile.create_directory_all(link_parent)
  let assert Ok(outside_abs) = path.absolute(outside)
  let assert Ok(Nil) = path.symlink(outside_abs, link_root)

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, link_root) == None
  let assert Ok(True) =
    simplifile.is_file(outside <> "/workspaces/main/sentinel")
}

pub fn workspace_cleanup_inventory_rejects_oversized_manifest_before_decode_test() {
  let repo = "test/tmp/cleanup-workspaces/oversized-manifest"
  let workspace_root = setup_repo(repo)
  let run_root = workspace_root <> "/implementation/LIV-5/run-large"
  let assert Ok(Nil) = simplifile.create_directory_all(run_root <> "/.scherzo")
  let assert Ok(Nil) =
    simplifile.create_directory_all(run_root <> "/workspaces/main")
  let chunk = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  let oversized = string.concat(list.repeat(chunk, times: 4097))
  let assert Ok(Nil) =
    simplifile.write(workspace_manifest.manifest_path(run_root), oversized)

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, run_root) == Some("retained")
  assert item_reason_contains(items, run_root, "too large")
}

fn setup_repo(repo: String) -> String {
  test_helpers.reset_dir(repo)
  let workspace_root = repo <> "/.scherzo/workspaces"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace_root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(repo <> "/.scherzo/workflows")
  let assert Ok(repo_abs) = path.absolute(repo)
  let assert Ok(Nil) =
    simplifile.write(
      repo <> "/driver.sh",
      "#!/bin/sh\nset -eu\nif [ \"$1 $2\" = 'describe --json' ]; then\n  printf '%s\\n' '{\"version\":1,\"capabilities\":[\"status\",\"assert-only\"]}'\n  exit 0\nfi\nprintf '%s|workspace=%s|run=%s\\n' \"$1 $2\" \"$SCHERZO_WORKSPACE_PATH\" \"$SCHERZO_RUN_ROOT\" >> \""
        <> repo_abs
        <> "/driver.log\"\ncase \"$1 $2\" in\n  'lifecycle remove')\n    if [ -f \""
        <> repo_abs
        <> "/remove-fail-workspace\" ] && [ \"$(cat \""
        <> repo_abs
        <> "/remove-fail-workspace\")\" = \"$SCHERZO_WORKSPACE_NAME\" ]; then\n      exit 23\n    fi\n    rm -rf \"$SCHERZO_WORKSPACE_PATH\"\n    ;;\n  'lifecycle create') mkdir -p \"$SCHERZO_WORKSPACE_PATH\" ;;
  'lifecycle before-step'|'lifecycle after-step') : ;;
  *) : ;;
esac\n",
    )
  test_helpers.chmod_executable(repo <> "/driver.sh")
  let assert Ok(Nil) =
    simplifile.write(
      repo <> "/.scherzo/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: SCHERZO_TEST_LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: dogfood-jj\n  drivers:\n    dogfood-jj:\n      type: custom\n      command: "
        <> repo_abs
        <> "/driver.sh\n      timeout: 5s\nworkflows:\n  workspace-cleanup: workflows/workspace-cleanup.yaml\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      repo <> "/.scherzo/workflows/workspace-cleanup.yaml",
      "version: 1\nid: workspace-cleanup\nsteps: []\n",
    )
  workspace_root
}

fn create_manifest_run(
  repo: String,
  workspace_root: String,
  run_id: String,
  workspace_name: String,
) -> String {
  let run_root = repo <> "/.scherzo/workspaces/implementation/LIV-1/" <> run_id
  let workspace_path = run_root <> "/workspaces/" <> workspace_name
  let assert Ok(Nil) = simplifile.create_directory_all(workspace_path)
  let assert Ok(Nil) = simplifile.write(workspace_path <> "/note", run_id)
  let relative_path = "workspaces/" <> workspace_name
  let assert Ok(Nil) = simplifile.create_directory_all(run_root <> "/.scherzo")
  let assert Ok(repo_abs) = path.absolute(repo)
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(run_root),
      workspace_manifest.encode_manifest(
        [
          workspace_manifest.Entry(
            run_id: run_id,
            workflow_id: "implementation",
            step_id: "implement",
            attempt_index: 1,
            workspace_name: workspace_name,
            relative_path: relative_path,
            workspace_profile: "dogfood-jj",
            driver_command: repo_abs <> "/driver.sh",
            driver_capabilities: ["status", "assert-only"],
            source_workspace_name: None,
            source_workspace_relative_path: None,
            state: workspace_manifest.Ready,
          ),
        ],
        run_id,
        "implementation",
      ),
    )
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let _ =
    ledger.append(
      paths,
      record.with_id(
        "finished-" <> run_id,
        2,
        record.WorkflowRunFinished(
          run_id,
          "implementation",
          "issue-id",
          "success",
          0,
          0,
        ),
      ),
      False,
    )
  run_root
}

fn workspace_provider(
  report: cleanup.CleanupReport,
) -> cleanup.CleanupProviderReport {
  let assert Ok(provider) =
    list.find(report.providers, fn(provider) {
      provider.provider_id == "workspaces"
    })
  provider
}

fn workspace_items(
  report: cleanup.CleanupReport,
) -> List(cleanup.CleanupItemReport) {
  workspace_provider(report).items
}

fn item_status(
  items: List(cleanup.CleanupItemReport),
  run_root: String,
) -> Option(String) {
  let run_root = path.absolute_or_original(run_root)
  case list.find(items, fn(item) { item.display_path == run_root }) {
    Ok(item) -> Some(item.status)
    Error(Nil) -> None
  }
}

fn item_reason_contains(
  items: List(cleanup.CleanupItemReport),
  run_root: String,
  expected: String,
) -> Bool {
  let run_root = path.absolute_or_original(run_root)
  case list.find(items, fn(item) { item.display_path == run_root }) {
    Ok(item) -> string.contains(item.reason, expected)
    Error(Nil) -> False
  }
}

fn with_env(name: String, value: String, action: fn() -> a) -> a {
  let previous = path.env(name)
  let _ = path.set_env(name, value)
  let result = action()
  case previous {
    Some(existing) -> {
      let _ = path.set_env(name, existing)
      Nil
    }
    None -> {
      let _ = path.unset_env(name)
      Nil
    }
  }
  result
}
