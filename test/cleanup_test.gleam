import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/cleanup
import scherzo/cleanup/cursor as cleanup_cursor
import scherzo/cleanup/local_state
import scherzo/state/local_artifacts
import simplifile

pub fn cleanup_inventory_reports_provider_backed_json_test() {
  let root = "test/tmp/cleanup/provider-json"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let eligible = archive_dir <> "/segment-1.jsonl"
  let retained = root <> "/.scherzo-state/ledger/current.jsonl"
  let assert Ok(Nil) = simplifile.write(eligible, "old")
  let assert Ok(Nil) = simplifile.write(retained, "current")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let report = cleanup.inventory(root, now)

  assert report.mode == cleanup.DryRun
  assert provider_ids(report)
    == [
      "local_state",
      "workspaces",
      "artifact_store",
      "task_store",
      "provider_live",
      "remote_provider_cache",
      "browser",
    ]
  let assert Ok(local_state) = find_provider(report, "local_state")
  assert local_state.available == True
  assert list.any(local_state.items, fn(item) {
    item.provider_id == "local_state"
    && item.status == "would_delete"
    && item.idempotency_key == "local_state:" <> item.item_id
  })
  let assert Ok(artifact_store) = find_provider(report, "artifact_store")
  assert artifact_store.available == False
  let assert [artifact_boundary] = artifact_store.items
  assert artifact_boundary.status == "unavailable"

  let encoded = cleanup.cleanup_report_to_json(report) |> json.to_string
  assert string.contains(encoded, "\"mode\":\"dry_run\"")
  assert string.contains(encoded, "\"provider_id\":\"workspaces\"")
  assert string.contains(encoded, "\"provider_id\":\"artifact_store\"")
  assert string.contains(encoded, "\"ownership_evidence\"")
  assert string.contains(encoded, "\"safety_checks\"")
  assert string.contains(encoded, "\"idempotency_key\"")
  assert !string.contains(encoded, "\"truncated\"")
}

pub fn cleanup_apply_preserves_local_state_cleanup_behavior_test() {
  let root = "test/tmp/cleanup/provider-apply"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let eligible = archive_dir <> "/segment-1.jsonl"
  let retained = root <> "/.scherzo-state/ledger/current.jsonl"
  let assert Ok(Nil) = simplifile.write(eligible, "old")
  let assert Ok(Nil) = simplifile.write(retained, "current")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let report = cleanup.apply(root, now)

  assert report.mode == cleanup.Apply
  let assert Ok(provider) = find_provider(report, "local_state")
  assert list.any(provider.items, fn(item) {
    item.status == "deleted" && item.display_path == eligible
  })
  let assert Ok(False) = simplifile.is_file(eligible)
  let assert Ok(True) = simplifile.is_file(retained)

  let second = cleanup.apply(root, now)
  let assert Ok(second_provider) = find_provider(second, "local_state")
  assert !list.any(second_provider.items, fn(item) {
    item.display_path == eligible
  })

  let summary = cleanup.cleanup_summary(report)
  assert string.contains(summary, "deleted=1")
  assert string.contains(summary, "unavailable=5")

  let encoded = cleanup.cleanup_report_to_json(report) |> json.to_string
  assert string.contains(encoded, "\"status\":\"deleted\"")
  assert string.contains(encoded, "\"status\":\"unavailable\"")
  assert string.contains(encoded, "\"dry_run\":false")
  assert !string.contains(encoded, "\"truncated\"")
}

pub fn cleanup_bounded_request_adds_page_fields_test() {
  let root = "test/tmp/cleanup/bounded-json"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let eligible = archive_dir <> "/segment-1.jsonl"
  let assert Ok(Nil) = simplifile.write(eligible, "old")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let cursor =
    cleanup_cursor.encode(
      root,
      cleanup_cursor.Cursor("local_state", "cursor-1"),
    )
  let assert Ok(report) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.DryRun,
      root,
      now,
      Some(25),
      Some(cursor),
      Some(240_000),
    ))

  assert report.limit == Some(25)
  assert report.cursor == Some(cursor)
  assert report.max_runtime_ms == Some(240_000)
  assert report.truncated == False
  assert report.scanned != None
  assert report.applied == Some(0)

  let encoded = cleanup.cleanup_report_to_json(report) |> json.to_string
  assert string.contains(encoded, "\"truncated\":false")
  assert string.contains(encoded, "\"cursor\":\"" <> cursor <> "\"")
  assert string.contains(encoded, "\"limit\":25")
  assert string.contains(encoded, "\"max_runtime_ms\":240000")
  assert string.contains(encoded, "\"scanned\"")
  assert string.contains(encoded, "\"applied\":0")
}

pub fn cleanup_bounded_apply_deletes_selected_local_state_items_test() {
  let root = "test/tmp/cleanup/bounded-apply"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let eligible = archive_dir <> "/segment-1.jsonl"
  let assert Ok(Nil) = simplifile.write(eligible, "old")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let assert Ok(report) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.Apply,
      root,
      now,
      Some(1),
      None,
      None,
    ))

  let assert Ok(provider) = find_provider(report, "local_state")
  assert list.any(provider.items, fn(item) {
    item.status == "deleted" && item.display_path == eligible
  })
  assert report.applied == Some(1)
  let assert Ok(False) = simplifile.is_file(eligible)

  let second = cleanup.apply(root, now)
  let assert Ok(second_provider) = find_provider(second, "local_state")
  assert !list.any(second_provider.items, fn(item) {
    item.display_path == eligible
  })
}

pub fn cleanup_bounded_pagination_resumes_with_cursor_test() {
  let root = "test/tmp/cleanup/bounded-pagination"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let assert Ok(Nil) =
    simplifile.write(archive_dir <> "/segment-1.jsonl", "old-1")
  let assert Ok(Nil) =
    simplifile.write(archive_dir <> "/segment-2.jsonl", "old-2")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let assert Ok(first) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.DryRun,
      root,
      now,
      Some(1),
      None,
      None,
    ))

  assert first.truncated == True
  let assert Some(next_cursor) = first.next_cursor
  let assert Some(1) = first.scanned

  let assert Ok(second) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.DryRun,
      root,
      now,
      Some(1),
      Some(next_cursor),
      None,
    ))
  let assert Ok(first_provider) = find_provider(first, "local_state")
  let assert Ok(provider) = find_provider(second, "local_state")
  assert list.length(provider.items) == 1
  assert provider.items != first_provider.items
}

pub type SequenceClockMessage {
  NextTick(process.Subject(Int))
  StopClock
}

pub fn cleanup_runtime_budget_truncates_apply_and_resumes_test() {
  let root = "test/tmp/cleanup/runtime-budget"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let first = archive_dir <> "/segment-1.jsonl"
  let second = archive_dir <> "/segment-2.jsonl"
  let assert Ok(Nil) = simplifile.write(first, "old-1")
  let assert Ok(Nil) = simplifile.write(second, "old-2")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let clock = start_sequence_clock([0, 0, 1])
  let assert Ok(first_page) =
    cleanup.run_with_clock(
      cleanup.CleanupRequest(cleanup.Apply, root, now, None, None, Some(1)),
      fn() { next_tick(clock) },
    )
  stop_clock(clock)

  assert first_page.truncated == True
  assert first_page.truncated_reason == Some("runtime_budget")
  assert first_page.applied == Some(1)
  let assert Some(next_cursor) = first_page.next_cursor
  let assert Ok(True) = simplifile.is_file(second)

  let assert Ok(second_page) =
    cleanup.run_with_clock(
      cleanup.CleanupRequest(
        cleanup.Apply,
        root,
        now,
        None,
        Some(next_cursor),
        Some(10),
      ),
      fn() { 0 },
    )

  assert second_page.truncated == False
  let assert Ok(False) = simplifile.is_file(first)
  let assert Ok(False) = simplifile.is_file(second)
}

pub fn local_state_cleanup_page_applies_cursor_before_classifying_page_items_test() {
  let root = "test/tmp/cleanup/local-state-page-cursor"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let first = archive_dir <> "/segment-1.jsonl"
  let second = archive_dir <> "/segment-2.jsonl"
  let assert Ok(Nil) = simplifile.write(first, "old-1")
  let assert Ok(Nil) = simplifile.write(second, "old-2")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let first_key = "segment-1.jsonl:" <> first
  let page =
    local_state.cleanup_page(
      root,
      now,
      Some(first_key),
      Some(1),
      0,
      None,
      fn() { 0 },
      False,
    )

  assert page.scanned == 1
  assert page.applied == 0
  assert page.truncated == False
  let assert [local_state.LocalStateItem(decision: decision, ..)] = page.items
  assert decision.display_path == second
}

pub fn local_state_cleanup_page_runtime_budget_stops_at_item_boundary_test() {
  let root = "test/tmp/cleanup/local-state-page-runtime"
  let _ = simplifile.delete(root)
  let archive_dir = root <> "/.scherzo-state/ledger/archive"
  let assert Ok(Nil) = simplifile.create_directory_all(archive_dir)
  let first = archive_dir <> "/segment-1.jsonl"
  let second = archive_dir <> "/segment-2.jsonl"
  let assert Ok(Nil) = simplifile.write(first, "old-1")
  let assert Ok(Nil) = simplifile.write(second, "old-2")

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let clock = start_sequence_clock([0, 0, 1])
  let page =
    local_state.cleanup_page(
      root,
      now,
      None,
      None,
      0,
      Some(1),
      fn() { next_tick(clock) },
      True,
    )
  stop_clock(clock)

  assert page.scanned == 1
  assert page.applied == 1
  assert page.truncated == True
  assert page.truncated_reason == Some("runtime_budget")
  let assert [local_state.LocalStateItem(status: "deleted", ..)] = page.items
  let assert Ok(False) = simplifile.is_file(first)
  let assert Ok(True) = simplifile.is_file(second)
  let assert Some(_) = page.next_key
}

pub fn cleanup_rejects_wrong_root_cursor_test() {
  let root = "test/tmp/cleanup/wrong-root"
  let other_root = "test/tmp/cleanup/wrong-root-other"
  let _ = simplifile.delete(root)
  let _ = simplifile.delete(other_root)
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state/ledger/archive")
  let assert Ok(Nil) =
    simplifile.create_directory_all(
      other_root <> "/.scherzo-state/ledger/archive",
    )

  let now =
    local_artifacts.now_ms()
    + local_artifacts.workflow_artifact_retention_ms
    + 1000
  let assert Ok(report) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.DryRun,
      other_root,
      now,
      Some(1),
      None,
      None,
    ))
  let assert Some(cursor) = report.next_cursor
  let assert Error(cleanup.CleanupError("wrong_root", _)) =
    cleanup.run_request(cleanup.CleanupRequest(
      cleanup.DryRun,
      root,
      now,
      Some(1),
      Some(cursor),
      None,
    ))
}

fn start_sequence_clock(
  values: List(Int),
) -> process.Subject(SequenceClockMessage) {
  let ready = process.new_subject()
  let _ =
    process.spawn(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      sequence_clock_loop(subject, values)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn sequence_clock_loop(
  subject: process.Subject(SequenceClockMessage),
  values: List(Int),
) -> Nil {
  case process.receive(subject, within: 1000) {
    Ok(NextTick(reply)) ->
      case values {
        [value, ..rest] -> {
          process.send(reply, value)
          sequence_clock_loop(subject, case rest {
            [] -> [value]
            _ -> rest
          })
        }
        [] -> {
          process.send(reply, 0)
          sequence_clock_loop(subject, [])
        }
      }
    Ok(StopClock) -> Nil
    Error(_) -> Nil
  }
}

fn next_tick(clock: process.Subject(SequenceClockMessage)) -> Int {
  let reply = process.new_subject()
  process.send(clock, NextTick(reply))
  let assert Ok(now_ms) = process.receive(reply, within: 1000)
  now_ms
}

fn stop_clock(clock: process.Subject(SequenceClockMessage)) -> Nil {
  process.send(clock, StopClock)
}

fn provider_ids(report: cleanup.CleanupReport) -> List(String) {
  list.map(report.providers, fn(provider) { provider.provider_id })
}

fn find_provider(
  report: cleanup.CleanupReport,
  provider_id: String,
) -> Result(cleanup.CleanupProviderReport, Nil) {
  list.find(report.providers, fn(provider) {
    provider.provider_id == provider_id
  })
}
