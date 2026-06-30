import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt}
import gleam/result
import gleam/string
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_runtime
import scherzo/cleanup/retention_marker
import scherzo/control/file
import scherzo/error
import scherzo/path
import scherzo/runtime_bundle
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/workspace_manifest
import scherzo/workspace_run
import simplifile

pub type WorkspaceProviderResult {
  WorkspaceProviderResult(
    available: Bool,
    roots: List(String),
    items: List(WorkspaceItem),
    warnings: List(String),
  )
}

pub type WorkspaceItem {
  WorkspaceItem(
    item_id: String,
    run_root: String,
    run_id: String,
    status: String,
    reason: String,
    ownership_evidence: List(String),
    safety_checks: List(String),
    warnings: List(String),
    retention_until_ms: Option(Int),
  )
}

pub type WorkspaceCleanupPage {
  WorkspaceCleanupPage(
    available: Bool,
    roots: List(String),
    items: List(WorkspaceItem),
    warnings: List(String),
    scanned: Int,
    applied: Int,
    budget_exhausted: Bool,
    truncated: Bool,
    next_key: Option(String),
    truncated_reason: Option(String),
  )
}

type CleanupPageState {
  CleanupPageState(
    items: List(WorkspaceItem),
    scanned: Int,
    applied: Int,
    budget_exhausted: Bool,
    truncated: Bool,
    next_key: Option(String),
    truncated_reason: Option(String),
    last_key: Option(String),
  )
}

type ManifestContext {
  ManifestContext(
    run_id: String,
    profile_name: String,
    driver_command: String,
    driver_capabilities: List(String),
  )
}

type LedgerContext {
  LedgerContext(
    active_roots: dict.Dict(String, Bool),
    publication_protections: dict.Dict(String, List(PublicationProtection)),
    run_statuses: dict.Dict(String, projection.WorkflowRunStatus),
    parked_issue_ids: dict.Dict(String, Bool),
  )
}

type PublicationProtection {
  PublicationProtection(
    publication_id: String,
    status: String,
    retryable: Bool,
    retry_execution_available: Bool,
    manifest_ref: Option(String),
    error_code: Option(String),
    error_message: Option(String),
  )
}

pub fn inventory(
  workspace_root: String,
  now_ms: Int,
) -> WorkspaceProviderResult {
  let root = path.absolute_or_original(workspace_root)
  let roots = [root]
  let run_roots = discover_run_roots(root)
  case ledger_context(root) {
    Error(reason) ->
      WorkspaceProviderResult(
        available: False,
        roots: roots,
        items: list.map(run_roots, unavailable_item(root, reason, _)),
        warnings: [reason],
      )
    Ok(context) ->
      WorkspaceProviderResult(
        available: True,
        roots: roots,
        items: list.map(run_roots, inventory_item(root, now_ms, context, _)),
        warnings: [],
      )
  }
}

pub fn apply(workspace_root: String, now_ms: Int) -> WorkspaceProviderResult {
  let dry = inventory(workspace_root, now_ms)
  let applied = list.map(dry.items, apply_item(workspace_root, now_ms, _))
  WorkspaceProviderResult(..dry, items: applied)
}

pub fn cleanup_page(
  workspace_root: String,
  now_ms: Int,
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
) -> WorkspaceCleanupPage {
  let root = path.absolute_or_original(workspace_root)
  let roots = [root]
  let initial = CleanupPageState([], 0, 0, False, False, None, None, None)
  case ledger_context(root) {
    Error(reason) ->
      cleanup_page_with_classifier(
        workspace_root,
        now_ms,
        root,
        roots,
        after_key,
        limit,
        started_ms,
        max_runtime_ms,
        clock,
        apply,
        initial,
        unavailable_item(root, reason, _),
        False,
        [reason],
      )
    Ok(context) ->
      cleanup_page_with_classifier(
        workspace_root,
        now_ms,
        root,
        roots,
        after_key,
        limit,
        started_ms,
        max_runtime_ms,
        clock,
        apply,
        initial,
        inventory_item(root, now_ms, context, _),
        True,
        [],
      )
  }
}

fn cleanup_page_with_classifier(
  workspace_root: String,
  now_ms: Int,
  root: String,
  roots: List(String),
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
  initial: CleanupPageState,
  classify_run_root: fn(String) -> WorkspaceItem,
  available: Bool,
  warnings: List(String),
) -> WorkspaceCleanupPage {
  let #(_, state) =
    cleanup_page_loop(
      workspace_root,
      now_ms,
      root,
      root,
      path.realpath(root) |> result.unwrap(root),
      dict.new(),
      after_key,
      limit,
      started_ms,
      max_runtime_ms,
      clock,
      apply,
      classify_run_root,
      initial,
    )
  WorkspaceCleanupPage(
    available: available,
    roots: roots,
    items: list.reverse(state.items),
    warnings: warnings,
    scanned: state.scanned,
    applied: state.applied,
    budget_exhausted: state.budget_exhausted,
    truncated: state.truncated,
    next_key: state.next_key,
    truncated_reason: state.truncated_reason,
  )
}

fn cleanup_page_loop(
  workspace_root: String,
  now_ms: Int,
  root: String,
  current: String,
  root_real: String,
  visited: dict.Dict(String, Bool),
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
  classify_run_root: fn(String) -> WorkspaceItem,
  state: CleanupPageState,
) -> #(dict.Dict(String, Bool), CleanupPageState) {
  case state.truncated {
    True -> #(visited, state)
    False -> {
      let current_real = path.realpath(current) |> result.unwrap(current)
      case dict.get(visited, current_real) {
        Ok(True) -> #(visited, state)
        _ -> {
          let visited = dict.insert(visited, current_real, True)
          case current != root && looks_like_run_root(current) {
            True -> #(
              visited,
              process_run_root(
                workspace_root,
                now_ms,
                current,
                after_key,
                limit,
                started_ms,
                max_runtime_ms,
                clock,
                apply,
                classify_run_root,
                state,
              ),
            )
            False ->
              case simplifile.read_directory(current) {
                Ok(entries) ->
                  entries
                  |> list.sort(by: string.compare)
                  |> list.fold(#(visited, state), fn(acc, entry) {
                    let #(visited, next_state) = acc
                    case next_state.truncated {
                      True -> #(visited, next_state)
                      False -> {
                        let child = path.join(current, entry)
                        case
                          ignored_directory_name(entry),
                          safe_discovery_directory(root_real, child)
                        {
                          True, _ | _, False -> #(visited, next_state)
                          False, True ->
                            cleanup_page_loop(
                              workspace_root,
                              now_ms,
                              root,
                              child,
                              root_real,
                              visited,
                              after_key,
                              limit,
                              started_ms,
                              max_runtime_ms,
                              clock,
                              apply,
                              classify_run_root,
                              next_state,
                            )
                        }
                      }
                    }
                  })
                Error(_) -> #(visited, state)
              }
          }
        }
      }
    }
  }
}

fn process_run_root(
  workspace_root: String,
  now_ms: Int,
  run_root: String,
  after_key: Option(String),
  limit: Option(Int),
  started_ms: Int,
  max_runtime_ms: Option(Int),
  clock: fn() -> Int,
  apply: Bool,
  classify_run_root: fn(String) -> WorkspaceItem,
  state: CleanupPageState,
) -> CleanupPageState {
  case skip_run_root_for_cursor(run_root, after_key) {
    True -> state
    False ->
      case
        cleanup_page_should_truncate(
          limit,
          state.scanned,
          state.budget_exhausted,
          max_runtime_ms,
          started_ms,
          clock,
        )
      {
        Some(reason) ->
          CleanupPageState(
            ..state,
            truncated: True,
            next_key: state.last_key,
            truncated_reason: Some(reason),
          )
        None -> {
          let item = classify_run_root(run_root)
          let item = case apply && item.status == "would_delete" {
            True -> apply_item(workspace_root, now_ms, item)
            False -> item
          }
          let budget_exhausted =
            cleanup_runtime_budget_hit(max_runtime_ms, started_ms, clock())
          CleanupPageState(
            items: [item, ..state.items],
            scanned: state.scanned + 1,
            applied: case apply {
              True -> state.applied + 1
              False -> 0
            },
            budget_exhausted: budget_exhausted,
            truncated: False,
            next_key: state.next_key,
            truncated_reason: state.truncated_reason,
            last_key: Some(run_root),
          )
        }
      }
  }
}

fn skip_run_root_for_cursor(
  run_root: String,
  after_key: Option(String),
) -> Bool {
  case after_key {
    Some(last_key) -> string.compare(run_root, last_key) != Gt
    None -> False
  }
}

fn cleanup_page_should_truncate(
  limit: Option(Int),
  scanned: Int,
  budget_exhausted: Bool,
  max_runtime_ms: Option(Int),
  started_ms: Int,
  clock: fn() -> Int,
) -> Option(String) {
  case hit_limit(limit, scanned) {
    True -> Some("limit")
    False ->
      case
        budget_exhausted
        || cleanup_runtime_budget_hit(max_runtime_ms, started_ms, clock())
      {
        True -> Some("runtime_budget")
        False -> None
      }
  }
}

fn cleanup_runtime_budget_hit(
  max_runtime_ms: Option(Int),
  started_ms: Int,
  now_ms: Int,
) -> Bool {
  case max_runtime_ms {
    Some(value) -> value > 0 && now_ms - started_ms >= value
    None -> False
  }
}

fn hit_limit(limit: Option(Int), scanned: Int) -> Bool {
  case limit {
    Some(value) -> value >= 0 && scanned >= value
    None -> False
  }
}

fn unavailable_item(
  workspace_root: String,
  reason: String,
  run_root: String,
) -> WorkspaceItem {
  WorkspaceItem(
    item_id: run_root,
    run_root: run_root,
    run_id: run_id_from_manifest_or_path(run_root),
    status: "unavailable",
    reason: "active-run ledger unavailable; retaining workspace run root: "
      <> reason,
    ownership_evidence: ownership_evidence(run_root, []),
    safety_checks: [
      "active-run ledger must be readable before workspace deletion",
      ..safety_checks(workspace_root, run_root)
    ],
    warnings: [reason],
    retention_until_ms: None,
  )
}

fn inventory_item(
  workspace_root: String,
  now_ms: Int,
  ledger_context: LedgerContext,
  run_root: String,
) -> WorkspaceItem {
  let base = fn(
    status: String,
    reason: String,
    extra: List(String),
    retention_until_ms: Option(Int),
  ) {
    WorkspaceItem(
      item_id: run_root,
      run_root: run_root,
      run_id: run_id_from_manifest_or_path(run_root),
      status: status,
      reason: reason,
      ownership_evidence: ownership_evidence(run_root, extra),
      safety_checks: safety_checks(workspace_root, run_root),
      warnings: [],
      retention_until_ms: retention_until_ms,
    )
  }
  case run_root == workspace_root || !path.contains(workspace_root, run_root) {
    True ->
      base(
        "retained",
        "workspace run root is outside the configured workspace root",
        [],
        None,
      )
    False ->
      case dict.get(ledger_context.active_roots, run_root) {
        Ok(True) ->
          base(
            "retained",
            "workspace run is active and protected from cleanup",
            ["active run root recorded in Scherzo ledger"],
            None,
          )
        Ok(False) | Error(Nil) ->
          case dict.get(ledger_context.publication_protections, run_root) {
            Ok([protection, ..]) ->
              base(
                "retained",
                publication_protection_reason(
                  run_id_from_manifest_or_path(run_root),
                  protection,
                  run_root,
                ),
                publication_protection_evidence(protection),
                None,
              )
            Ok([]) | Error(Nil) ->
              classify_with_retention_marker(
                workspace_root,
                now_ms,
                ledger_context,
                run_root,
                base,
              )
          }
      }
  }
}

fn classify_with_retention_marker(
  workspace_root: String,
  now_ms: Int,
  ledger_context: LedgerContext,
  run_root: String,
  base: fn(String, String, List(String), Option(Int)) -> WorkspaceItem,
) -> WorkspaceItem {
  let marker_path = workspace_run.cleanup_retention_marker(run_root)
  case simplifile.read(marker_path) {
    Ok(contents) ->
      case retention_marker.parse(contents) {
        retention_marker.LegacyManualHold ->
          base(
            "retained",
            "legacy workspace retention marker is present; retaining until an operator removes or migrates it",
            ["retention marker .scherzo-keep-workspace is a legacy/manual hold"],
            None,
          )
        retention_marker.Malformed(reason) ->
          base(
            "retained",
            "workspace retention marker is malformed; retaining fail-closed: "
              <> reason,
            [
              "retention marker .scherzo-keep-workspace could not be parsed safely",
            ],
            None,
          )
        retention_marker.SchemaMarker(
          review_state,
          created_at_ms,
          source_kind,
          source_ref,
        ) ->
          classify_schema_marker(
            workspace_root,
            now_ms,
            ledger_context,
            run_root,
            review_state,
            created_at_ms,
            source_kind,
            source_ref,
            base,
          )
      }
    Error(_) ->
      case
        hard_hold_reason(
          ledger_context,
          run_root,
          run_id_from_manifest_or_path(run_root),
        )
      {
        Some(reason) -> base("retained", reason, [], None)
        None -> classify_manifest_backed_run(workspace_root, run_root, base)
      }
  }
}

fn classify_schema_marker(
  workspace_root: String,
  now_ms: Int,
  ledger_context: LedgerContext,
  run_root: String,
  review_state: retention_marker.ReviewState,
  created_at_ms: Int,
  source_kind: String,
  source_ref: String,
  base: fn(String, String, List(String), Option(Int)) -> WorkspaceItem,
) -> WorkspaceItem {
  let evidence = [
    "retention marker schema is scherzo.retained-workspace.v1",
    "review_state=" <> retention_marker.review_state_to_string(review_state),
    "source_kind=" <> source_kind,
    "source=" <> source_ref,
  ]
  let run_id = run_id_from_manifest_or_path(run_root)
  case review_state {
    retention_marker.ManualHold ->
      base(
        "retained",
        "workspace retention marker review state is manual_hold",
        evidence,
        None,
      )
    retention_marker.SafeToDelete | retention_marker.Abandoned ->
      case hard_hold_reason(ledger_context, run_root, run_id) {
        Some(reason) -> base("retained", reason, evidence, None)
        None -> classify_manifest_backed_run(workspace_root, run_root, base)
      }
    retention_marker.PublicationGuard ->
      case hard_hold_reason(ledger_context, run_root, run_id) {
        Some(reason) -> base("retained", reason, evidence, None)
        None -> {
          let retention_until =
            created_at_ms + retention_marker.stale_publication_guard_ms
          case now_ms >= retention_until {
            True -> classify_manifest_backed_run(workspace_root, run_root, base)
            False ->
              base(
                "retained",
                "workspace retention marker publication_guard has not aged out yet",
                evidence,
                Some(retention_until),
              )
          }
        }
      }
  }
}

fn classify_manifest_backed_run(
  _workspace_root: String,
  run_root: String,
  base: fn(String, String, List(String), Option(Int)) -> WorkspaceItem,
) -> WorkspaceItem {
  case manifest_context(run_root) {
    Error("managed workspace manifest missing") ->
      base(
        "retained",
        "managed workspace manifest is missing; unmanifested directories are never deleted",
        [],
        None,
      )
    Error(reason) -> base("retained", reason, [], None)
    Ok(context) ->
      case
        workspace_manifest.cleanup_entries(
          run_root,
          context.profile_name,
          context.driver_command,
          context.driver_capabilities,
        )
      {
        Ok(_) ->
          base(
            "would_delete",
            "workspace cleanup is delegated through workspace_run.cleanup_run",
            manifest_evidence(run_root, context),
            None,
          )
        Error(error.WorkspaceIo(reason)) ->
          base("retained", reason, manifest_evidence(run_root, context), None)
        Error(error.WorkspaceOutsideRoot(reason)) ->
          base(
            "retained",
            "workspace manifest path escapes configured root: " <> reason,
            manifest_evidence(run_root, context),
            None,
          )
        Error(other) ->
          base(
            "retained",
            workspace_error_message(other),
            manifest_evidence(run_root, context),
            None,
          )
      }
  }
}

pub fn apply_item(
  workspace_root: String,
  now_ms: Int,
  item: WorkspaceItem,
) -> WorkspaceItem {
  case item.status {
    "would_delete" -> apply_eligible_item(workspace_root, now_ms, item)
    _ -> item
  }
}

fn apply_eligible_item(
  workspace_root: String,
  _now_ms: Int,
  item: WorkspaceItem,
) -> WorkspaceItem {
  case ledger_context(path.absolute_or_original(workspace_root)) {
    Error(reason) ->
      WorkspaceItem(
        ..item,
        status: "unavailable",
        reason: "active-run ledger unavailable; retaining workspace run root: "
          <> reason,
        warnings: [reason, ..item.warnings],
      )
    Ok(context) ->
      case dict.get(context.active_roots, item.run_root) {
        Ok(True) ->
          WorkspaceItem(
            ..item,
            status: "retained",
            reason: "workspace run became active and is protected from cleanup",
          )
        Ok(False) | Error(Nil) ->
          case dict.get(context.publication_protections, item.run_root) {
            Ok([protection, ..]) ->
              WorkspaceItem(
                ..item,
                status: "retained",
                reason: publication_protection_reason(
                  item.run_id,
                  protection,
                  item.run_root,
                ),
              )
            Ok([]) | Error(Nil) ->
              case hard_hold_reason(context, item.run_root, item.run_id) {
                Some(reason) ->
                  WorkspaceItem(..item, status: "retained", reason: reason)
                None -> delegate_cleanup_item(workspace_root, item)
              }
          }
      }
  }
}

fn delegate_cleanup_item(
  workspace_root: String,
  item: WorkspaceItem,
) -> WorkspaceItem {
  case load_bundle_for_workspace_root(workspace_root) {
    Error(reason) ->
      WorkspaceItem(
        ..item,
        status: "failed",
        reason: "workspace cleanup delegation unavailable: " <> reason,
      )
    Ok(bundle) ->
      case manifest_context(item.run_root) {
        Error(reason) -> WorkspaceItem(..item, status: "failed", reason: reason)
        Ok(context) ->
          case
            dict.get(
              bundle.orchestrator.workspace_profiles.profiles,
              context.profile_name,
            )
          {
            Error(Nil) ->
              WorkspaceItem(
                ..item,
                status: "failed",
                reason: "workspace profile is not configured: "
                  <> context.profile_name,
              )
            Ok(profile) ->
              case
                workspace_run.cleanup_run(
                  item.run_root,
                  bundle.orchestrator,
                  profile,
                )
              {
                Ok(Nil) ->
                  WorkspaceItem(
                    ..item,
                    status: "deleted",
                    reason: "workspace cleanup delegated through workspace_run.cleanup_run",
                  )
                Error(err) ->
                  WorkspaceItem(
                    ..item,
                    status: "failed",
                    reason: workspace_error_message(err),
                  )
              }
          }
      }
  }
}

fn discover_run_roots(workspace_root: String) -> List(String) {
  let root_real = path.realpath(workspace_root) |> result.unwrap(workspace_root)
  let #(_, roots) =
    discover_run_roots_loop(
      workspace_root,
      root_real,
      workspace_root,
      dict.new(),
      [],
    )
  roots
}

fn discover_run_roots_loop(
  workspace_root: String,
  workspace_root_real: String,
  current: String,
  visited: dict.Dict(String, Bool),
  acc: List(String),
) -> #(dict.Dict(String, Bool), List(String)) {
  let current_real = path.realpath(current) |> result.unwrap(current)
  case dict.get(visited, current_real) {
    Ok(True) -> #(visited, acc)
    _ -> {
      let visited = dict.insert(visited, current_real, True)
      case current != workspace_root && looks_like_run_root(current) {
        True -> #(visited, [current, ..acc])
        False ->
          case simplifile.read_directory(current) {
            Ok(entries) ->
              list.fold(entries, #(visited, acc), fn(state, entry) {
                let #(visited, found) = state
                let child = path.join(current, entry)
                case
                  ignored_discovery_subtree(
                    workspace_root,
                    current,
                    entry,
                    child,
                  ),
                  safe_discovery_directory(workspace_root_real, child)
                {
                  True, _ | _, False -> #(visited, found)
                  False, True ->
                    discover_run_roots_loop(
                      workspace_root,
                      workspace_root_real,
                      child,
                      visited,
                      found,
                    )
                }
              })
            Error(_) -> #(visited, acc)
          }
      }
    }
  }
}

fn safe_discovery_directory(
  workspace_root_real: String,
  child: String,
) -> Bool {
  case simplifile.link_info(child) {
    Ok(info) ->
      case simplifile.file_info_type(info) {
        simplifile.Directory ->
          case path.realpath(child) {
            Ok(child_real) -> path.contains(workspace_root_real, child_real)
            Error(Nil) -> False
          }
        _ -> False
      }
    Error(_) -> False
  }
}

fn ignored_discovery_subtree(
  workspace_root: String,
  current: String,
  entry: String,
  child: String,
) -> Bool {
  ignored_directory_name(entry)
  || generated_build_directory(workspace_root, entry, child)
  || nested_test_tmp(workspace_root, current, entry, child)
  || nested_scherzo_workspaces(workspace_root, current, entry, child)
}

fn ignored_directory_name(name: String) -> Bool {
  name == ".scherzo-state"
  || name == ".git"
  || name == ".jj"
  || name == ".direnv"
}

fn generated_build_directory(
  workspace_root: String,
  entry: String,
  child: String,
) -> Bool {
  entry == "build" && below_top_level_run_prefix(workspace_root, child)
}

fn nested_test_tmp(
  workspace_root: String,
  current: String,
  entry: String,
  child: String,
) -> Bool {
  entry == "tmp"
  && string.ends_with(current, "/test")
  && below_top_level_run_prefix(workspace_root, child)
}

fn nested_scherzo_workspaces(
  workspace_root: String,
  current: String,
  entry: String,
  child: String,
) -> Bool {
  entry == "workspaces"
  && child != workspace_root
  && string.ends_with(current, "/.scherzo")
  && below_top_level_run_prefix(workspace_root, child)
}

fn below_top_level_run_prefix(workspace_root: String, child: String) -> Bool {
  path_depth_below_workspace_root(workspace_root, child) > 3
}

fn path_depth_below_workspace_root(
  workspace_root: String,
  child: String,
) -> Int {
  let root = trim_trailing_slash(workspace_root)
  let child = trim_trailing_slash(child)
  case child == root {
    True -> 0
    False ->
      case string.starts_with(child, root <> "/") {
        True ->
          child
          |> string.drop_start(string.length(root) + 1)
          |> string.split(on: "/")
          |> list.filter(fn(segment) { segment != "" })
          |> list.length
        False -> 0
      }
  }
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}

fn looks_like_run_root(path_: String) -> Bool {
  run_root_workspaces_directory(path_)
  || result.unwrap(
    simplifile.is_file(workspace_manifest.manifest_path(path_)),
    False,
  )
  || result.unwrap(
    simplifile.is_file(workspace_run.cleanup_retention_marker(path_)),
    False,
  )
}

fn run_root_workspaces_directory(path_: String) -> Bool {
  !string.ends_with(path_, "/.scherzo")
  && result.unwrap(
    simplifile.is_directory(path.join(path_, "workspaces")),
    False,
  )
}

fn ledger_context(workspace_root: String) -> Result(LedgerContext, String) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(err) -> Error(ledger.ledger_error_to_string(err))
    Ok(paths) ->
      case ledger.replay(paths) {
        Error(err) -> Error(ledger.ledger_error_to_string(err))
        Ok(replayed) ->
          case replayed.truncated_tail {
            True ->
              Error(
                "active-run ledger has a truncated tail; retaining workspace run roots until activity can be proven",
              )
            False ->
              Ok(LedgerContext(
                active_roots: active_run_roots_from_projection(
                  replayed.projection,
                ),
                publication_protections: publication_protections(
                  workspace_root,
                  replayed.projection,
                ),
                run_statuses: workflow_run_statuses(replayed.projection),
                parked_issue_ids: replayed.projection.parked_issues
                  |> dict.keys
                  |> list.fold(dict.new(), fn(ids, issue_id) {
                    dict.insert(ids, issue_id, True)
                  }),
              ))
          }
      }
  }
}

fn active_run_roots_from_projection(
  projected: projection.Projection,
) -> dict.Dict(String, Bool) {
  let active_roots =
    projection.active_workflow_runs(projected)
    |> list.fold(dict.new(), fn(active, entry) {
      let #(_, status) = entry
      dict.insert(
        active,
        path.absolute_or_original(workflow_run_root(status)),
        True,
      )
    })

  projection.scheduled_statuses(projected)
  |> list.fold(active_roots, fn(active, status) {
    case status.state, status.current_run {
      projection.ScheduledActive, Some(run) ->
        case run.run_root {
          Some(run_root) ->
            dict.insert(active, path.absolute_or_original(run_root), True)
          None -> active
        }
      _, _ -> active
    }
  })
}

fn workflow_run_statuses(
  projected: projection.Projection,
) -> dict.Dict(String, projection.WorkflowRunStatus) {
  projected.workflow_runs
  |> dict.to_list
  |> list.fold(dict.new(), fn(found, entry) {
    let #(_, status) = entry
    dict.insert(
      found,
      path.absolute_or_original(workflow_run_root(status)),
      status,
    )
  })
}

fn hard_hold_reason(
  context: LedgerContext,
  run_root: String,
  _run_id: String,
) -> Option(String) {
  case dict.get(context.run_statuses, run_root) {
    Ok(projection.WorkflowRunInterrupted(issue_id: issue_id, ..)) ->
      case dict.get(context.parked_issue_ids, issue_id) {
        Ok(True) ->
          Some(
            "issue for retained workspace run is parked and must be released before cleanup",
          )
        _ ->
          Some(
            "workspace run was interrupted and still requires operator review",
          )
      }
    Ok(projection.WorkflowRunActive(issue_id: issue_id, ..))
    | Ok(projection.WorkflowRunFinished(issue_id: issue_id, ..))
    | Ok(projection.WorkflowRunSuperseded(issue_id: issue_id, ..)) ->
      case dict.get(context.parked_issue_ids, issue_id) {
        Ok(True) ->
          Some(
            "issue for retained workspace run is parked and must be released before cleanup",
          )
        _ -> None
      }
    Error(Nil) -> None
  }
}

fn publication_protections(
  workspace_root: String,
  projected: projection.Projection,
) -> dict.Dict(String, List(PublicationProtection)) {
  projected.workflow_runs
  |> dict.to_list
  |> list.fold(dict.new(), fn(protected, entry) {
    let #(run_id, status) = entry
    let run_root = path.absolute_or_original(workflow_run_root(status))
    let protections =
      publication_protections_for_run(workspace_root, projected, run_id)
    case protections {
      [] -> protected
      _ -> dict.insert(protected, run_root, protections)
    }
  })
}

fn publication_protections_for_run(
  workspace_root: String,
  projected: projection.Projection,
  run_id: String,
) -> List(PublicationProtection) {
  projection.publication_ids_for_run(projected, run_id)
  |> list.filter_map(fn(publication_id) {
    case
      projection.latest_publication_for_run(projected, run_id, publication_id)
    {
      Ok(attempt) ->
        case publication_protection_for_attempt(workspace_root, attempt) {
          Some(protection) -> Ok(protection)
          None -> Error(Nil)
        }
      Error(Nil) -> Error(Nil)
    }
  })
}

fn publication_protection_for_attempt(
  workspace_root: String,
  attempt: projection.PublicationAttempt,
) -> Option(PublicationProtection) {
  case attempt.required, attempt.status {
    True, "planned" | True, "failed" ->
      case attempt_is_commit_stack(workspace_root, attempt) {
        True ->
          Some(PublicationProtection(
            publication_id: attempt.publication_id,
            status: attempt.status,
            retryable: attempt.retryable,
            retry_execution_available: attempt.retry_execution_available,
            manifest_ref: attempt.manifest_ref,
            error_code: attempt.error_code,
            error_message: attempt.error_message,
          ))
        False -> None
      }
    _, _ -> None
  }
}

fn attempt_is_commit_stack(
  workspace_root: String,
  attempt: projection.PublicationAttempt,
) -> Bool {
  case attempt.manifest_ref {
    Some(ref) ->
      case
        artifact_store.read_artifact_unverified(
          artifact_store.new(workspace_root),
          ref,
        )
      {
        Ok(contents) ->
          case artifact_publication_manifest.decode_manifest_json(contents) {
            Ok(manifest) ->
              artifact_publication_runtime.publication_manifest_is_commit_stack(
                manifest,
              )
            Error(_) -> False
          }
        Error(_) -> False
      }
    None -> False
  }
}

fn workflow_run_root(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(run_root: run_root, ..)
    | projection.WorkflowRunFinished(run_root: run_root, ..)
    | projection.WorkflowRunInterrupted(run_root: run_root, ..)
    | projection.WorkflowRunSuperseded(run_root: run_root, ..) -> run_root
  }
}

fn publication_protection_reason(
  run_id: String,
  protection: PublicationProtection,
  run_root: String,
) -> String {
  "required same-repo commit_stack publication is "
  <> publication_state_label(protection.status)
  <> "; retained_workspace_path="
  <> retained_workspace_path_for_run_root(run_root)
  <> "; publication_id="
  <> protection.publication_id
  <> "; retryable="
  <> bool_string(protection.retryable)
  <> "; retry_execution_available="
  <> bool_string(protection.retry_execution_available)
  <> "; retry with: scherzoctl artifact publication retry --run "
  <> run_id
  <> " --publication "
  <> protection.publication_id
  <> "; abandon with: scherzoctl artifact publication abandon --run "
  <> run_id
  <> " --publication "
  <> protection.publication_id
  <> " --reason <reason> --yes"
  <> optional_error_suffix(protection.error_code, protection.error_message)
}

fn retained_workspace_path_for_run_root(run_root: String) -> String {
  case
    artifact_publication_runtime.retained_workspace_path_from_run_root(run_root)
  {
    Some(workspace_path) -> workspace_path
    None -> run_root
  }
}

fn publication_state_label(status: String) -> String {
  case status {
    "planned" -> "pending"
    other -> other
  }
}

fn publication_protection_evidence(
  protection: PublicationProtection,
) -> List(String) {
  [
    "required same-repo commit_stack publication is not published",
    "publication_id=" <> protection.publication_id,
    "publication_status=" <> protection.status,
    "publication_manifest_ref=" <> optional_string(protection.manifest_ref),
  ]
}

fn optional_error_suffix(
  error_code: Option(String),
  error_message: Option(String),
) -> String {
  case error_code, error_message {
    Some(code), Some(message) -> "; error=" <> code <> ": " <> message
    Some(code), None -> "; error=" <> code
    None, Some(message) -> "; error=" <> message
    None, None -> ""
  }
}

fn optional_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "-"
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn manifest_context(run_root: String) -> Result(ManifestContext, String) {
  case workspace_manifest.read_entries(run_root) {
    Error(err) -> Error(workspace_error_message(err))
    Ok(entries) ->
      case entries {
        [] -> Error("managed workspace manifest is empty")
        [entry, ..] ->
          Ok(ManifestContext(
            run_id: entry.run_id,
            profile_name: entry.workspace_profile,
            driver_command: entry.driver_command,
            driver_capabilities: entry.driver_capabilities,
          ))
      }
  }
}

fn manifest_evidence(
  run_root: String,
  context: ManifestContext,
) -> List(String) {
  [
    "managed workspace manifest is present",
    "run_id=" <> context.run_id,
    "workspace profile=" <> context.profile_name,
    "driver command=" <> context.driver_command,
    "run root=" <> run_root,
  ]
}

fn ownership_evidence(_run_root: String, extra: List(String)) -> List(String) {
  ["workspace run root discovered under the configured workspace root", ..extra]
  |> list.reverse
  |> list.reverse
}

fn safety_checks(workspace_root: String, run_root: String) -> List(String) {
  [
    "workspace run root must stay under the configured workspace root",
    "managed workspace manifest validation rejects path escapes and outside-root symlinks",
    "workspace deletion is delegated through workspace lifecycle remove hooks",
    "workspace_root=" <> workspace_root,
    "run_root=" <> run_root,
  ]
}

fn run_id_from_manifest_or_path(run_root: String) -> String {
  case manifest_context(run_root) {
    Ok(context) -> context.run_id
    Error(_) -> run_root
  }
}

fn load_bundle_for_workspace_root(
  workspace_root: String,
) -> Result(runtime_bundle.RuntimeBundle, String) {
  let config_dir =
    path.dirname(path.absolute_or_original(workspace_root))
    |> result.unwrap(".")
  let config_path = path.join(config_dir, "scherzo.yaml")
  case simplifile.is_file(config_path) {
    Ok(True) ->
      runtime_bundle.load_with_env(Some(config_path), file.get_env)
      |> result.map_error(fn(err) { bundle_error_message(err) })
    _ -> Error("config file not found: " <> config_path)
  }
}

fn bundle_error_message(error: runtime_bundle.BundleError) -> String {
  let runtime_bundle.BundleError(_, message) = error
  message
}

fn workspace_error_message(err: error.WorkspaceError) -> String {
  case err {
    error.WorkspaceOutsideRoot(path_) ->
      "workspace path is outside the workspace root: " <> path_
    error.WorkspaceIo(message) -> message
    error.PartialWorkspace(path_) ->
      "workspace is partially prepared: " <> path_
    error.UnsafeWorkspaceKey(key) -> "unsafe workspace key: " <> key
    error.WorkspaceCollision(message) -> message
  }
}
