import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/cleanup/local_state
import scherzo/cleanup/workspaces
import scherzo/session/event
import scherzo/state/local_artifacts

pub type CleanupMode {
  DryRun
  Apply
}

pub type CleanupProviderSelection {
  AllProviders
  SelectedProvider(CleanupProvider)
}

pub type CleanupProvider {
  LocalState
  Workspaces
  ArtifactStore
  TaskStore
  ProviderLive
  RemoteProviderCache
  Browser
}

pub type CleanupProviderSelectionError {
  InvalidCleanupProvider(value: String)
}

pub type CleanupReport {
  CleanupReport(
    mode: CleanupMode,
    workspace_root: String,
    now_ms: Int,
    providers: List(CleanupProviderReport),
    warnings: List(String),
  )
}

pub type CleanupProviderReport {
  CleanupProviderReport(
    provider_id: String,
    available: Bool,
    elapsed_ms: Int,
    roots: List(String),
    transcript_root_status: String,
    items: List(CleanupItemReport),
    warnings: List(String),
  )
}

pub type CleanupItemReport {
  CleanupItemReport(
    provider_id: String,
    item_id: String,
    item_kind: String,
    display_path: String,
    ownership_evidence: List(String),
    safety_checks: List(String),
    intended_action: String,
    status: String,
    reason: String,
    warnings: List(String),
    idempotency_key: String,
    recovery_status: Option(String),
    retention_until_ms: Option(Int),
  )
}

pub fn inventory(workspace_root: String, now_ms: Int) -> CleanupReport {
  inventory_for(workspace_root, now_ms, AllProviders)
}

pub fn inventory_for(
  workspace_root: String,
  now_ms: Int,
  provider_selection: CleanupProviderSelection,
) -> CleanupReport {
  CleanupReport(
    mode: DryRun,
    workspace_root: workspace_root,
    now_ms: now_ms,
    providers: cleanup_provider_reports(
      workspace_root,
      now_ms,
      DryRun,
      provider_selection,
    ),
    warnings: [],
  )
}

pub fn apply(workspace_root: String, now_ms: Int) -> CleanupReport {
  apply_for(workspace_root, now_ms, AllProviders)
}

pub fn apply_for(
  workspace_root: String,
  now_ms: Int,
  provider_selection: CleanupProviderSelection,
) -> CleanupReport {
  CleanupReport(
    mode: Apply,
    workspace_root: workspace_root,
    now_ms: now_ms,
    providers: cleanup_provider_reports(
      workspace_root,
      now_ms,
      Apply,
      provider_selection,
    ),
    warnings: [],
  )
}

pub fn parse_provider_selection(
  value: String,
) -> Result(CleanupProviderSelection, CleanupProviderSelectionError) {
  case normalized_provider_name(value) {
    "all" -> Ok(AllProviders)
    "local_state" -> Ok(SelectedProvider(LocalState))
    "workspaces" -> Ok(SelectedProvider(Workspaces))
    "artifact_store" -> Ok(SelectedProvider(ArtifactStore))
    "task_store" -> Ok(SelectedProvider(TaskStore))
    "provider_live" -> Ok(SelectedProvider(ProviderLive))
    "remote_provider_cache" -> Ok(SelectedProvider(RemoteProviderCache))
    "browser" -> Ok(SelectedProvider(Browser))
    _ -> Error(InvalidCleanupProvider(value))
  }
}

pub fn provider_selection_error_message(
  error: CleanupProviderSelectionError,
) -> String {
  case error {
    InvalidCleanupProvider(value) ->
      "invalid cleanup provider '"
      <> value
      <> "'; expected one of: "
      <> provider_selection_usage()
  }
}

fn cleanup_provider_reports(
  workspace_root: String,
  now_ms: Int,
  mode: CleanupMode,
  provider_selection: CleanupProviderSelection,
) -> List(CleanupProviderReport) {
  provider_selection
  |> selected_providers
  |> list.map(fn(provider) {
    cleanup_provider_report(workspace_root, now_ms, mode, provider)
  })
}

fn cleanup_provider_report(
  workspace_root: String,
  now_ms: Int,
  mode: CleanupMode,
  provider: CleanupProvider,
) -> CleanupProviderReport {
  timed_provider(fn() {
    case provider {
      LocalState ->
        local_state_provider_report(case mode {
          DryRun -> local_state.inventory(workspace_root, now_ms)
          Apply -> local_state.apply(workspace_root, now_ms)
        })
      Workspaces ->
        workspaces_provider_report(case mode {
          DryRun -> workspaces.inventory(workspace_root)
          Apply -> workspaces.apply(workspace_root)
        })
      ArtifactStore ->
        unavailable_provider_report(
          "artifact_store",
          workspace_root,
          "artifact repositories are read-only to generic cleanup",
        )
      TaskStore ->
        unavailable_provider_report(
          "task_store",
          workspace_root,
          "task stores are read-only to generic cleanup",
        )
      ProviderLive ->
        unavailable_provider_report(
          "provider_live",
          workspace_root,
          "provider-live state is not mutated by generic cleanup",
        )
      RemoteProviderCache ->
        unavailable_provider_report(
          "remote_provider_cache",
          workspace_root,
          "remote-provider cache cleanup requires an explicit owning provider",
        )
      Browser ->
        unavailable_provider_report(
          "browser",
          workspace_root,
          "browser and UI state are outside generic cleanup scope",
        )
    }
  })
}

fn selected_providers(
  provider_selection: CleanupProviderSelection,
) -> List(CleanupProvider) {
  case provider_selection {
    AllProviders -> all_providers()
    SelectedProvider(provider) -> [provider]
  }
}

fn all_providers() -> List(CleanupProvider) {
  [
    LocalState,
    Workspaces,
    ArtifactStore,
    TaskStore,
    ProviderLive,
    RemoteProviderCache,
    Browser,
  ]
}

fn timed_provider(
  provider_report: fn() -> CleanupProviderReport,
) -> CleanupProviderReport {
  let started_ms = local_artifacts.now_ms()
  let provider = provider_report()
  CleanupProviderReport(
    ..provider,
    elapsed_ms: elapsed_ms(started_ms, local_artifacts.now_ms()),
  )
}

fn elapsed_ms(started_ms: Int, finished_ms: Int) -> Int {
  let elapsed = finished_ms - started_ms
  case elapsed < 0 {
    True -> 0
    False -> elapsed
  }
}

fn normalized_provider_name(value: String) -> String {
  value
  |> string.trim
  |> string.lowercase
  |> string.replace(each: "-", with: "_")
}

fn provider_selection_usage() -> String {
  "all, local-state, workspaces (actionable), artifact-store, task-store, provider-live, remote-provider-cache, or browser (diagnostic-only unavailable)"
}

pub fn cleanup_report_to_json(report: CleanupReport) -> json.Json {
  json.object([
    #("mode", json.string(mode_to_string(report.mode))),
    #("dry_run", json.bool(report.mode == DryRun)),
    #("workspace_root", json.string(report.workspace_root)),
    #("now_ms", json.int(report.now_ms)),
    #(
      "providers",
      json.array(report.providers, of: cleanup_provider_report_to_json),
    ),
    #("warnings", json.array(report.warnings, of: json.string)),
    #("summary", cleanup_summary_to_json(report)),
  ])
}

pub fn cleanup_summary(report: CleanupReport) -> String {
  "cleanup "
  <> mode_to_string(report.mode)
  <> ": providers="
  <> int.to_string(list.length(report.providers))
  <> " would_delete="
  <> int.to_string(count_status(report, "would_delete"))
  <> " deleted="
  <> int.to_string(count_status(report, "deleted"))
  <> " retained="
  <> int.to_string(count_status(report, "retained"))
  <> " unavailable="
  <> int.to_string(count_status(report, "unavailable"))
  <> " failed="
  <> int.to_string(count_status(report, "failed"))
  <> " warnings="
  <> int.to_string(total_warning_count(report))
}

pub fn cleanup_provider_report_to_json(
  provider: CleanupProviderReport,
) -> json.Json {
  json.object([
    #("provider_id", json.string(provider.provider_id)),
    #("available", json.bool(provider.available)),
    #("elapsed_ms", json.int(provider.elapsed_ms)),
    #("roots", json.array(provider.roots, of: json.string)),
    #("transcript_root_status", json.string(provider.transcript_root_status)),
    #("items", json.array(provider.items, of: cleanup_item_report_to_json)),
    #("warnings", json.array(provider.warnings, of: json.string)),
    #("summary", provider_summary_to_json(provider)),
  ])
}

pub fn cleanup_item_report_to_json(item: CleanupItemReport) -> json.Json {
  json.object([
    #("provider_id", json.string(item.provider_id)),
    #("item_id", json.string(item.item_id)),
    #("item_kind", json.string(item.item_kind)),
    #("display_path", json.string(item.display_path)),
    #(
      "ownership_evidence",
      json.array(item.ownership_evidence, of: json.string),
    ),
    #("safety_checks", json.array(item.safety_checks, of: json.string)),
    #("intended_action", json.string(item.intended_action)),
    #("status", json.string(item.status)),
    #("reason", json.string(item.reason)),
    #("warnings", json.array(item.warnings, of: json.string)),
    #("idempotency_key", json.string(item.idempotency_key)),
    #("recovery_status", optional_string(item.recovery_status)),
    #("retention_until_ms", optional_int(item.retention_until_ms)),
  ])
}

fn local_state_provider_report(
  result: local_artifacts.CleanupResult,
) -> CleanupProviderReport {
  CleanupProviderReport(
    provider_id: "local_state",
    available: True,
    elapsed_ms: 0,
    roots: result.roots,
    transcript_root_status: result.transcript_root_status,
    items: list.flatten([
      list.map(result.would_delete, local_state_item(_, "would_delete")),
      list.map(result.deleted, local_state_item(_, "deleted")),
      list.map(result.retained, local_state_item(_, "retained")),
    ]),
    warnings: result.warnings,
  )
}

fn local_state_item(
  decision: local_artifacts.LocalArtifactDecision,
  status: String,
) -> CleanupItemReport {
  CleanupItemReport(
    provider_id: "local_state",
    item_id: decision.id,
    item_kind: decision.artifact_type,
    display_path: decision.display_path,
    ownership_evidence: ownership_evidence(decision),
    safety_checks: local_state_safety_checks(decision),
    intended_action: intended_action(status),
    status: status,
    reason: decision.reason,
    warnings: [],
    idempotency_key: "local_state:" <> decision.id,
    recovery_status: option.map(
      decision.recovery_status,
      event.recovery_status_to_string,
    ),
    retention_until_ms: decision.retention_until_ms,
  )
}

fn workspaces_provider_report(
  result: workspaces.WorkspaceProviderResult,
) -> CleanupProviderReport {
  CleanupProviderReport(
    provider_id: "workspaces",
    available: result.available,
    elapsed_ms: 0,
    roots: result.roots,
    transcript_root_status: "not_applicable",
    items: list.map(result.items, workspace_item),
    warnings: result.warnings,
  )
}

fn workspace_item(item: workspaces.WorkspaceItem) -> CleanupItemReport {
  CleanupItemReport(
    provider_id: "workspaces",
    item_id: item.item_id,
    item_kind: "workspace_run_root",
    display_path: item.run_root,
    ownership_evidence: item.ownership_evidence,
    safety_checks: item.safety_checks,
    intended_action: intended_action(item.status),
    status: item.status,
    reason: item.reason,
    warnings: item.warnings,
    idempotency_key: "workspaces:" <> item.run_root,
    recovery_status: None,
    retention_until_ms: None,
  )
}

fn unavailable_provider_report(
  provider_id: String,
  workspace_root: String,
  reason: String,
) -> CleanupProviderReport {
  CleanupProviderReport(
    provider_id: provider_id,
    available: False,
    elapsed_ms: 0,
    roots: [workspace_root],
    transcript_root_status: "not_applicable",
    items: [
      CleanupItemReport(
        provider_id: provider_id,
        item_id: provider_id,
        item_kind: provider_id,
        display_path: workspace_root,
        ownership_evidence: ["generic cleanup does not own this subsystem"],
        safety_checks: [
          "generic cleanup must not mutate remote stores, provider-live state, caches, or browser state without an owning provider capability",
        ],
        intended_action: "retain",
        status: "unavailable",
        reason: reason,
        warnings: [],
        idempotency_key: provider_id,
        recovery_status: None,
        retention_until_ms: None,
      ),
    ],
    warnings: [],
  )
}

fn ownership_evidence(
  decision: local_artifacts.LocalArtifactDecision,
) -> List(String) {
  let base = ["discovered under daemon-owned .scherzo-state cleanup roots"]
  case string.contains(decision.reason, "missing owner marker") {
    True -> ["owner marker missing; retained for safety", ..base]
    False ->
      case string.contains(decision.reason, "malformed") {
        True -> ["malformed metadata is retained for safety", ..base]
        False -> ["local-state metadata classified the artifact", ..base]
      }
  }
}

fn local_state_safety_checks(
  decision: local_artifacts.LocalArtifactDecision,
) -> List(String) {
  [
    "bounded to daemon-owned local-state roots",
    "path safety is checked before deletion",
    case decision.cleanup_phase {
      event.Eligible -> "retention expired and the item is eligible"
      event.Retained ->
        "retained because safety or retention checks did not pass"
      event.Deleting ->
        "deletion is delegated through the local-state cleanup path"
      event.Deleted -> "local-state cleanup reported the item as deleted"
    },
  ]
}

fn intended_action(status: String) -> String {
  case status {
    "would_delete" | "deleted" | "failed" -> "delete"
    _ -> "retain"
  }
}

fn mode_to_string(mode: CleanupMode) -> String {
  case mode {
    DryRun -> "dry_run"
    Apply -> "apply"
  }
}

fn cleanup_summary_to_json(report: CleanupReport) -> json.Json {
  json.object([
    #("providers", json.int(list.length(report.providers))),
    #("would_delete", json.int(count_status(report, "would_delete"))),
    #("deleted", json.int(count_status(report, "deleted"))),
    #("retained", json.int(count_status(report, "retained"))),
    #("unavailable", json.int(count_status(report, "unavailable"))),
    #("failed", json.int(count_status(report, "failed"))),
    #("warnings", json.int(total_warning_count(report))),
  ])
}

fn provider_summary_to_json(provider: CleanupProviderReport) -> json.Json {
  json.object([
    #("items", json.int(list.length(provider.items))),
    #("would_delete", json.int(count_provider_status(provider, "would_delete"))),
    #("deleted", json.int(count_provider_status(provider, "deleted"))),
    #("retained", json.int(count_provider_status(provider, "retained"))),
    #("unavailable", json.int(count_provider_status(provider, "unavailable"))),
    #("failed", json.int(count_provider_status(provider, "failed"))),
    #("warnings", json.int(list.length(provider.warnings))),
  ])
}

fn count_status(report: CleanupReport, status: String) -> Int {
  report.providers
  |> list.map(count_provider_status(_, status))
  |> list.fold(0, fn(total, next) { total + next })
}

fn count_provider_status(
  provider: CleanupProviderReport,
  status: String,
) -> Int {
  provider.items
  |> list.filter(fn(item) { item.status == status })
  |> list.length
}

fn total_warning_count(report: CleanupReport) -> Int {
  list.length(report.warnings)
  + list.fold(report.providers, 0, fn(total, provider) {
    total + list.length(provider.warnings)
  })
}

fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn optional_int(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}
