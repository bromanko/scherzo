import gleam/json
import gleam/list
import gleam/string
import scherzo/cleanup
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
