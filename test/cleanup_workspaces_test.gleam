import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/cleanup
import scherzo/commit_stack_artifact
import scherzo/ctl/artifact_publication_abandon as ctl_artifact_publication_abandon
import scherzo/hash
import scherzo/path
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/workspace
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

pub fn workspace_cleanup_inventory_protects_active_scheduled_run_test() {
  let repo = "test/tmp/cleanup-workspaces/scheduled-active"
  let workspace_root = setup_repo(repo)
  let active =
    create_manifest_run(repo, workspace_root, "run-scheduled-active", "main")
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append(
      paths,
      record.with_id(
        "scheduled-started",
        12,
        record.ScheduledRunStarted(
          "job-1",
          "implementation",
          10,
          11,
          "run-scheduled-active",
          1,
          "session-1",
          active,
        ),
      ),
      False,
    )

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, active) == Some("retained")
  assert item_reason_contains(items, active, "active")
}

pub fn workspace_cleanup_retains_pending_failed_and_releases_successful_commit_stack_publications_test() {
  let repo = "test/tmp/cleanup-workspaces/commit-stack-retention"
  let workspace_root = setup_repo(repo)
  let pending = create_manifest_run(repo, workspace_root, "run-pending", "main")
  let failed = create_manifest_run(repo, workspace_root, "run-failed", "main")
  let failed_preplan =
    create_manifest_run(repo, workspace_root, "run-failed-preplan", "main")
  let published =
    create_manifest_run(repo, workspace_root, "run-published", "main")
  seed_commit_stack_publication(workspace_root, "run-pending", "planned")
  seed_commit_stack_publication(workspace_root, "run-failed", "failed")
  seed_preplan_commit_stack_publication(workspace_root, "run-failed-preplan")
  seed_commit_stack_publication(workspace_root, "run-published", "published")

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, pending) == Some("retained")
  assert item_status(items, failed) == Some("retained")
  assert item_status(items, failed_preplan) == Some("retained")
  assert item_status(items, published) == Some("would_delete")
  assert item_reason_contains(
    items,
    pending,
    "commit_stack publication is pending",
  )
  assert item_reason_contains(
    items,
    failed,
    "commit_stack publication is failed",
  )
  assert item_reason_contains(
    items,
    failed_preplan,
    "commit_stack publication is failed",
  )
  assert item_reason_contains(items, failed, "artifact publication retry")
  assert item_reason_contains(items, failed, "artifact publication abandon")
}

pub fn workspace_cleanup_releases_abandoned_commit_stack_publication_test() {
  let repo = "test/tmp/cleanup-workspaces/commit-stack-abandoned"
  let workspace_root = setup_repo(repo)
  let abandoned =
    create_manifest_run(repo, workspace_root, "run-abandoned", "main")
  seed_commit_stack_publication(workspace_root, "run-abandoned", "abandoned")

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, abandoned) == Some("would_delete")
}

pub fn workspace_cleanup_abandon_command_records_abandoned_and_releases_publication_test() {
  let repo = "test/tmp/cleanup-workspaces/commit-stack-abandon-command"
  let workspace_root = setup_repo(repo)
  let run_root =
    create_manifest_run(repo, workspace_root, "run-abandon-command", "main")
  seed_commit_stack_publication(workspace_root, "run-abandon-command", "failed")
  let subject = process.new_subject()

  assert ctl_artifact_publication_abandon.abandon(
      workspace_root,
      True,
      "run-abandon-command",
      "implementation_commit_stack",
      "operator abandoned from test",
      fn(line) { process.send(subject, line) },
    )
    == Ok(Nil)
  let assert Ok(transcript) = process.receive(subject, within: 1000)
  assert string.contains(transcript, "\"status\":\"abandoned\"")
  assert string.contains(
    transcript,
    "\"error_message\":\"operator abandoned from test\"",
  )

  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(replayed) = ledger.replay(paths)
  let assert Ok(latest) =
    projection.latest_publication_for_run(
      replayed.projection,
      "run-abandon-command",
      "implementation_commit_stack",
    )
  assert latest.status == "abandoned"
  assert latest.retryable == False
  assert latest.retry_execution_available == False
  assert latest.error_message == Some("operator abandoned from test")

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)
  assert item_status(items, run_root) == Some("would_delete")
}

pub fn workspace_cleanup_abandon_command_rejects_non_abandonable_latest_statuses_test() {
  let repo = "test/tmp/cleanup-workspaces/commit-stack-abandon-rejects-status"
  let workspace_root = setup_repo(repo)
  let _published =
    create_manifest_run(repo, workspace_root, "run-published", "main")
  let _abandoned =
    create_manifest_run(repo, workspace_root, "run-already-abandoned", "main")
  seed_commit_stack_publication(workspace_root, "run-published", "published")
  seed_commit_stack_publication(
    workspace_root,
    "run-already-abandoned",
    "abandoned",
  )

  let assert Error(#(published_code, published_message)) =
    ctl_artifact_publication_abandon.abandon(
      workspace_root,
      False,
      "run-published",
      "implementation_commit_stack",
      "should not abandon published",
      fn(_) { Nil },
    )
  assert published_code == "publication_abandon_not_allowed"
  assert string.contains(published_message, "status=published")

  let assert Error(#(abandoned_code, abandoned_message)) =
    ctl_artifact_publication_abandon.abandon(
      workspace_root,
      False,
      "run-already-abandoned",
      "implementation_commit_stack",
      "should not abandon twice",
      fn(_) { Nil },
    )
  assert abandoned_code == "publication_already_abandoned"
  assert string.contains(abandoned_message, "already abandoned")
}

pub type WorkspaceSequenceClockMessage {
  NextWorkspaceTick(process.Subject(Int))
  StopWorkspaceClock
}

pub fn workspace_cleanup_runtime_budget_truncates_and_resumes_test() {
  let repo = "test/tmp/cleanup-workspaces/runtime-budget"
  let workspace_root = setup_repo(repo)
  let first = create_manifest_run(repo, workspace_root, "run-runtime-1", "main")
  let second =
    create_manifest_run(repo, workspace_root, "run-runtime-2", "review")
  let clock = start_workspace_sequence_clock([0, 0, 1])

  let assert Ok(first_page) =
    cleanup.run_with_clock(
      cleanup.CleanupRequest(
        cleanup.Apply,
        workspace_root,
        0,
        None,
        None,
        Some(1),
      ),
      fn() { next_workspace_tick(clock) },
    )
  stop_workspace_clock(clock)
  let items = workspace_items(first_page)

  assert first_page.truncated == True
  assert first_page.truncated_reason == Some("runtime_budget")
  assert first_page.applied == Some(1)
  assert item_status(items, first) == Some("deleted")
  let assert Some(next_cursor) = first_page.next_cursor
  let assert Ok(False) = simplifile.is_directory(first)
  let assert Ok(True) = simplifile.is_directory(second)

  let assert Ok(second_page) =
    cleanup.run_with_clock(
      cleanup.CleanupRequest(
        cleanup.Apply,
        workspace_root,
        0,
        None,
        Some(next_cursor),
        Some(10),
      ),
      fn() { 0 },
    )
  let second_items = workspace_items(second_page)
  assert second_page.truncated == False
  assert item_status(second_items, second) == Some("deleted")
}

pub fn workspace_cleanup_bounded_apply_delegates_remove_test() {
  let repo = "test/tmp/cleanup-workspaces/bounded-apply"
  let workspace_root = setup_repo(repo)
  let eligible =
    create_manifest_run(repo, workspace_root, "run-bounded-eligible", "main")

  let assert Ok(report) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.Apply,
      workspace_root,
      0,
      Some(1),
      None,
      None,
    ))
  let items = workspace_items(report)

  assert item_status(items, eligible) == Some("deleted")
  assert report.applied == Some(1)
  let assert Ok(False) = simplifile.is_directory(eligible)
  let assert Ok(driver_log) = simplifile.read(repo <> "/driver.log")
  assert string.contains(driver_log, "lifecycle remove")

  let second = cleanup.apply(workspace_root, 0)
  let second_items = workspace_items(second)
  assert item_status(second_items, eligible) == None
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
  let assert Ok(outside_abs) = path.absolute(outside)
  let assert Ok(Nil) =
    path.symlink(outside_abs, eligible <> "/workspaces/outside-sentinel")
  let assert Ok(Nil) =
    simplifile.write(repo <> "/remove-fail-workspace", "review\n")

  let report = cleanup.apply(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, eligible) == Some("deleted")
  assert item_status(items, failing) == Some("failed")
  assert item_reason_contains(items, failing, "driver lifecycle remove failed")
  let assert Ok(False) = simplifile.is_directory(eligible)
  let assert Ok(True) = simplifile.is_directory(failing)
  let assert Ok(True) = simplifile.is_file(outside <> "/sentinel")

  let second = cleanup.apply(workspace_root, 0)
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

  let report = cleanup.apply(workspace_root, 0)
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

pub fn workspace_cleanup_unmarked_interrupted_parked_runs_remain_retained_test() {
  let repo = "test/tmp/cleanup-workspaces/unmarked-hard-hold"
  let workspace_root = setup_repo(repo)
  let run_root =
    create_manifest_run(
      repo,
      workspace_root,
      "run-unmarked-interrupted",
      "main",
    )
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append_many(
      paths,
      [
        record.with_id(
          "issue-parked",
          3,
          record.IssueParked("issue-id", "LIV-1", "waiting for operator", 3),
        ),
        record.with_id(
          "workflow-interrupted",
          4,
          record.WorkflowRunInterrupted(
            "run-unmarked-interrupted",
            "implementation",
            "issue-id",
            "operator review required",
          ),
        ),
      ],
      False,
    )

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, run_root) == Some("retained")
  assert item_reason_contains(items, run_root, "parked")
}

pub fn workspace_cleanup_schema_marker_publication_guard_ages_out_test() {
  let repo = "test/tmp/cleanup-workspaces/schema-marker-age"
  let workspace_root = setup_repo(repo)
  let fresh =
    create_manifest_run(repo, workspace_root, "run-fresh-marker", "main")
  let stale =
    create_manifest_run(repo, workspace_root, "run-stale-marker", "main")
  write_schema_marker(fresh, "publication_guard", 2000)
  write_schema_marker(stale, "publication_guard", 0)

  let report = cleanup.inventory(workspace_root, 2_592_000_001)
  let items = workspace_items(report)

  assert item_status(items, fresh) == Some("retained")
  assert item_status(items, stale) == Some("would_delete")
}

pub fn workspace_cleanup_schema_marker_safe_to_delete_still_retains_interrupted_run_test() {
  let repo = "test/tmp/cleanup-workspaces/schema-marker-safe-to-delete"
  let workspace_root = setup_repo(repo)
  let run_root =
    create_manifest_run(repo, workspace_root, "run-interrupted", "main")
  write_schema_marker(run_root, "safe_to_delete", 0)
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append(
      paths,
      record.with_id(
        "workflow-started",
        1,
        record.WorkflowRunStarted(
          "run-interrupted",
          "implementation",
          "fingerprint",
          "issue-safe-to-delete",
          "LIV-55",
          "issue-fingerprint",
          1,
          run_root,
        ),
      ),
      False,
    )
  let assert Ok(Nil) =
    ledger.append(
      paths,
      record.with_id(
        "workflow-interrupted",
        2,
        record.WorkflowRunInterrupted(
          "run-interrupted",
          "implementation",
          "issue-safe-to-delete",
          "operator review required",
        ),
      ),
      False,
    )

  let report = cleanup.inventory(workspace_root, 2_592_000_001)
  let items = workspace_items(report)
  assert item_status(items, run_root) == Some("retained")
  assert item_reason_contains(items, run_root, "operator review")
}

pub fn workspace_cleanup_inventory_skips_generated_fixture_subtrees_test() {
  let repo = "test/tmp/cleanup-workspaces/generated-fixtures"
  let workspace_root = setup_repo(repo)
  let legitimate =
    create_manifest_run(repo, workspace_root, "run-legitimate", "main")
  let build_workflow =
    create_manifest_run_with_keys(
      repo,
      workspace_root,
      "build",
      "LIV-build",
      "run-build-workflow",
      "main",
    )
  let test_tmp_issue =
    create_manifest_run_with_keys(
      repo,
      workspace_root,
      "test",
      "tmp",
      "run-test-tmp-issue",
      "main",
    )
  let retained =
    create_manifest_run(repo, workspace_root, "run-retained", "main")
  let assert Ok(Nil) =
    simplifile.write(retained <> "/.scherzo-keep-workspace", "keep\n")

  let retained_fixture_scherzo =
    retained <> "/workspaces/main/test/tmp/.scherzo"
  let retained_fixture_run =
    create_generated_workspace_fixture(
      retained_fixture_scherzo,
      "run-retained-fixture",
    )
  let nested_workspace_scherzo = workspace_root <> "/generated/source/.scherzo"
  let nested_workspace_run =
    create_generated_workspace_fixture(
      nested_workspace_scherzo,
      "run-nested-workspace",
    )
  let test_tmp_fixture_scherzo =
    workspace_root <> "/generated/source/test/tmp/.scherzo"
  let test_tmp_fixture_run =
    create_generated_workspace_fixture(test_tmp_fixture_scherzo, "run-test-tmp")
  let build_fixture_scherzo = workspace_root <> "/build/generated/.scherzo"
  let build_fixture_run =
    create_generated_workspace_fixture(build_fixture_scherzo, "run-build")
  let generated_build_marker =
    workspace_root <> "/generated/source/assets/build"
  let assert Ok(Nil) = simplifile.create_directory_all(generated_build_marker)
  let assert Ok(Nil) =
    simplifile.write(
      generated_build_marker <> "/.scherzo-keep-workspace",
      "keep\n",
    )

  let report = cleanup.inventory(workspace_root, 0)
  let items = workspace_items(report)

  assert item_status(items, legitimate) == Some("would_delete")
  assert item_status(items, build_workflow) == Some("would_delete")
  assert item_status(items, test_tmp_issue) == Some("would_delete")
  assert item_status(items, retained) == Some("retained")
  assert item_status(items, retained_fixture_scherzo) == None
  assert item_status(items, retained_fixture_run) == None
  assert item_status(items, nested_workspace_scherzo) == None
  assert item_status(items, nested_workspace_run) == None
  assert item_status(items, test_tmp_fixture_scherzo) == None
  assert item_status(items, test_tmp_fixture_run) == None
  assert item_status(items, build_fixture_scherzo) == None
  assert item_status(items, build_fixture_run) == None
  assert item_status(items, generated_build_marker) == None
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

fn start_workspace_sequence_clock(
  values: List(Int),
) -> process.Subject(WorkspaceSequenceClockMessage) {
  let ready = process.new_subject()
  let _ =
    process.spawn(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      workspace_sequence_clock_loop(subject, values)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn workspace_sequence_clock_loop(
  subject: process.Subject(WorkspaceSequenceClockMessage),
  values: List(Int),
) -> Nil {
  case process.receive(subject, within: 1000) {
    Ok(NextWorkspaceTick(reply)) ->
      case values {
        [value, ..rest] -> {
          process.send(reply, value)
          workspace_sequence_clock_loop(subject, case rest {
            [] -> [value]
            _ -> rest
          })
        }
        [] -> {
          process.send(reply, 0)
          workspace_sequence_clock_loop(subject, [])
        }
      }
    Ok(StopWorkspaceClock) -> Nil
    Error(_) -> Nil
  }
}

fn next_workspace_tick(
  clock: process.Subject(WorkspaceSequenceClockMessage),
) -> Int {
  let reply = process.new_subject()
  process.send(clock, NextWorkspaceTick(reply))
  let assert Ok(now_ms) = process.receive(reply, within: 1000)
  now_ms
}

fn stop_workspace_clock(
  clock: process.Subject(WorkspaceSequenceClockMessage),
) -> Nil {
  process.send(clock, StopWorkspaceClock)
}

fn write_schema_marker(
  run_root: String,
  review_state: String,
  created_at_ms: Int,
) -> Nil {
  let assert Ok(Nil) =
    simplifile.write(
      run_root <> "/.scherzo-keep-workspace",
      "Schema: scherzo.retained-workspace.v1\nReview state: "
        <> review_state
        <> "\nCreated at ms: "
        <> int.to_string(created_at_ms)
        <> "\nSource kind: execplan\nSource: LIV-1266\n",
    )
  Nil
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
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\n  driver: dogfood-jj\n  drivers:\n    dogfood-jj:\n      type: custom\n      command: "
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

fn create_generated_workspace_fixture(
  scherzo_dir: String,
  run_id: String,
) -> String {
  let run_root =
    scherzo_dir <> "/workspaces/implementation/LIV-fixture/" <> run_id
  let assert Ok(Nil) = simplifile.create_directory_all(run_root)
  let assert Ok(Nil) =
    simplifile.write(run_root <> "/.scherzo-keep-workspace", "keep\n")
  run_root
}

fn create_manifest_run(
  repo: String,
  workspace_root: String,
  run_id: String,
  workspace_name: String,
) -> String {
  create_manifest_run_with_keys(
    repo,
    workspace_root,
    "implementation",
    "LIV-1",
    run_id,
    workspace_name,
  )
}

fn create_manifest_run_with_keys(
  repo: String,
  workspace_root: String,
  workflow_id: String,
  issue_identifier: String,
  run_id: String,
  workspace_name: String,
) -> String {
  let run_root =
    workspace_root
    <> "/"
    <> workflow_id
    <> "/"
    <> issue_identifier
    <> "/"
    <> run_id
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
            workflow_id: workflow_id,
            step_id: "implement",
            attempt_index: 1,
            workspace_name: workspace_name,
            relative_path: relative_path,
            workspace_profile: "dogfood-jj",
            driver_command: repo_abs <> "/driver.sh",
            driver_capabilities: ["status", "assert-only"],
            source: workspace.FreshWorkspace,
            state: workspace_manifest.Ready,
          ),
        ],
        run_id,
        workflow_id,
      ),
    )
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let _ =
    ledger.append_many(
      paths,
      [
        record.with_id(
          "started-" <> run_id,
          1,
          record.WorkflowRunStarted(
            run_id,
            workflow_id,
            "fingerprint",
            "issue-id",
            issue_identifier,
            "issue-fingerprint",
            1,
            run_root,
          ),
        ),
        record.with_id(
          "finished-" <> run_id,
          2,
          record.WorkflowRunFinished(
            run_id,
            workflow_id,
            "issue-id",
            "success",
            0,
            0,
          ),
        ),
      ],
      False,
    )
  run_root
}

fn seed_commit_stack_publication(
  workspace_root: String,
  run_id: String,
  status: String,
) -> Nil {
  let planned = commit_stack_plan(run_id)
  let attempt_id = status <> "-attempt"
  let manifest = case status {
    "planned" ->
      artifact_publication_manifest.planned_manifest(planned, attempt_id, 10)
    "published" ->
      artifact_publication_manifest.published_manifest(
        planned,
        attempt_id,
        10,
        commit_stack_head_sha(),
        Some(existing_pr_url()),
        [],
        [],
      )
    "abandoned" ->
      artifact_publication_manifest.planned_manifest(planned, "planned", 9)
      |> artifact_publication_manifest.abandoned_from_manifest(
        attempt_id,
        10,
        "operator abandoned publication",
      )
    _ ->
      artifact_publication_manifest.failed_from_planned_manifest(
        planned,
        attempt_id,
        10,
        True,
        Some(planned.branch),
        None,
        Some(existing_pr_url()),
        [],
        [],
        artifact_publication_manifest.PublicationErrorInfo(
          code: "publish_failed",
          message: "remote rejected publication",
        ),
      )
  }
  let ref =
    "runs/"
    <> run_id
    <> "/publications/implementation_commit_stack/"
    <> attempt_id
    <> ".json"
  let #(sha, bytes) = write_publication_manifest(workspace_root, ref, manifest)
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append(
      paths,
      record.with_id(
        "publication-" <> run_id <> "-" <> status,
        11,
        record.PublicationAttemptRecorded(
          run_id: run_id,
          workflow_id: "implementation",
          publication_id: "implementation_commit_stack",
          series_id: planned.series_id,
          attempt_id: attempt_id,
          status: status,
          required: True,
          retryable: status == "failed",
          retry_execution_available: status == "failed",
          version_id: Some(planned.version_id),
          manifest_ref: Some(ref),
          manifest_sha256: Some(sha),
          manifest_bytes: Some(bytes),
          error_code: publication_error_code(status),
          error_message: publication_error_message(status),
        ),
      ),
      False,
    )
  Nil
}

fn seed_preplan_commit_stack_publication(
  workspace_root: String,
  run_id: String,
) -> Nil {
  let publication_id = "implementation_commit_stack"
  let attempt_id = "failed-preplan-attempt"
  let series_id = "issue-id:implementation:" <> publication_id
  let error =
    artifact_publication_manifest.PublicationErrorInfo(
      code: "invalid_commit_stack_output",
      message: "commit stack output could not be read",
    )
  let manifest =
    artifact_publication_manifest.PublicationManifest(
      ..artifact_publication_manifest.failed_manifest(
        run_id,
        "implementation",
        publication_id,
        series_id,
        True,
        attempt_id,
        10,
        error,
      ),
      publication_mode: Some("commit_stack"),
    )
  let ref =
    "runs/"
    <> run_id
    <> "/publications/implementation_commit_stack/"
    <> attempt_id
    <> ".json"
  let #(sha, bytes) = write_publication_manifest(workspace_root, ref, manifest)
  let assert Ok(paths) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append(
      paths,
      record.with_id(
        "publication-" <> run_id <> "-failed-preplan",
        11,
        record.PublicationAttemptRecorded(
          run_id: run_id,
          workflow_id: "implementation",
          publication_id: publication_id,
          series_id: series_id,
          attempt_id: attempt_id,
          status: "failed",
          required: True,
          retryable: True,
          retry_execution_available: False,
          version_id: None,
          manifest_ref: Some(ref),
          manifest_sha256: Some(sha),
          manifest_bytes: Some(bytes),
          error_code: Some(error.code),
          error_message: Some(error.message),
        ),
      ),
      False,
    )
  Nil
}

fn commit_stack_plan(
  run_id: String,
) -> artifact_publication_planner.DryRunPublicationManifest {
  artifact_publication_planner.DryRunPublicationManifest(
    run_id: run_id,
    workflow_id: "implementation",
    publication_id: "implementation_commit_stack",
    series_id: "issue-id:implementation:implementation_commit_stack",
    version_id: "version-" <> run_id,
    required: True,
    dry_run: False,
    repository_kind: "github",
    repository_id: "code",
    github_repo: Some("scherzo-systems/scherzo"),
    github_base: Some("main"),
    branch: "scherzo/implementation/LIV-917",
    target: artifact_publication_planner.ExistingPrBranchTargetPlan(
      commit_stack_artifact.ExistingPrBranchTarget(
        repository: "scherzo-systems/scherzo",
        head_repo: "scherzo-systems/scherzo",
        head_branch: "scherzo/implementation/LIV-917",
        expected_head_sha: commit_stack_base_sha(),
        base_branch: "main",
        base_sha: commit_stack_base_sha(),
        pr_number: 42,
        pr_url: existing_pr_url(),
      ),
    ),
    pull_request: artifact_publication_planner.PlannedPullRequest(
      enabled: True,
      draft: True,
      title: Some("Implementation publication"),
      body: Some("Published by Scherzo"),
    ),
    publication: artifact_publication_planner.PlannedCommitStackPublication(
      commit_stack: artifact_publication_planner.PlannedCommitStack(
        output: "commit_stack",
        manifest_ref: "runs/" <> run_id <> "/outputs/commit-stack.json",
        manifest_sha256: hash.sha256_hex("{}"),
        manifest_bytes: 2,
        stack: commit_stack_artifact.CommitStackArtifact(
          repository: "scherzo-systems/scherzo",
          base_ref: "main",
          base_sha: commit_stack_base_sha(),
          head_sha: commit_stack_head_sha(),
          head_tree: commit_stack_tree_sha(),
          carrier: commit_stack_artifact.CommitStackCarrier(
            ref: "runs/" <> run_id <> "/outputs/commit-stack.bundle",
            sha256: hash.sha256_hex("bundle"),
            bytes: 6,
            media_type: commit_stack_artifact.bundle_media_type,
          ),
        ),
      ),
    ),
    work: artifact_publication_planner.PublicationWork(
      kind: artifact_publication_planner.TaskWork,
      id: "issue-id",
      identifier: "LIV-917",
      slug: "LIV-917",
      title: Some("Cleanup publication"),
      url: Some("https://linear.example/LIV-917"),
    ),
  )
}

fn write_publication_manifest(
  workspace_root: String,
  ref: String,
  manifest: artifact_publication_manifest.PublicationManifest,
) -> #(String, Int) {
  let payload = artifact_publication_manifest.to_string(manifest)
  let absolute = workspace_root <> "/.scherzo-state/artifacts/" <> ref
  let assert Ok(dir) = path.dirname(absolute)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let assert Ok(Nil) = simplifile.write(absolute, payload)
  #(
    hash.sha256_hex(payload),
    bit_array.byte_size(bit_array.from_string(payload)),
  )
}

fn publication_error_code(status: String) -> Option(String) {
  case status {
    "failed" -> Some("publish_failed")
    "abandoned" -> Some("publication_abandoned")
    _ -> None
  }
}

fn publication_error_message(status: String) -> Option(String) {
  case status {
    "failed" -> Some("remote rejected publication")
    "abandoned" -> Some("operator abandoned publication")
    _ -> None
  }
}

fn commit_stack_base_sha() -> String {
  "1111111111111111111111111111111111111111"
}

fn commit_stack_head_sha() -> String {
  "2222222222222222222222222222222222222222"
}

fn commit_stack_tree_sha() -> String {
  "3333333333333333333333333333333333333333"
}

fn existing_pr_url() -> String {
  "https://example.test/pr/42"
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
