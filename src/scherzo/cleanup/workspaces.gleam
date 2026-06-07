import gleam/dict
import gleam/list
import gleam/option.{Some}
import gleam/result
import scherzo/control/file
import scherzo/error
import scherzo/path
import scherzo/runtime_bundle
import scherzo/state/ledger
import scherzo/state/record
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

pub fn inventory(workspace_root: String) -> WorkspaceProviderResult {
  let root = path.absolute_or_original(workspace_root)
  let roots = [root]
  let run_roots = discover_run_roots(root)
  case active_run_roots(root) {
    Error(reason) ->
      WorkspaceProviderResult(
        available: False,
        roots: roots,
        items: list.map(run_roots, unavailable_item(root, reason, _)),
        warnings: [reason],
      )
    Ok(active) ->
      WorkspaceProviderResult(
        available: True,
        roots: roots,
        items: list.map(run_roots, inventory_item(root, active, _)),
        warnings: [],
      )
  }
}

pub fn apply(workspace_root: String) -> WorkspaceProviderResult {
  let dry = inventory(workspace_root)
  let applied = list.map(dry.items, apply_item(workspace_root, _))
  WorkspaceProviderResult(..dry, items: applied)
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
  )
}

fn inventory_item(
  workspace_root: String,
  active_roots: dict.Dict(String, Bool),
  run_root: String,
) -> WorkspaceItem {
  let base = fn(status: String, reason: String, extra: List(String)) {
    WorkspaceItem(
      item_id: run_root,
      run_root: run_root,
      run_id: run_id_from_manifest_or_path(run_root),
      status: status,
      reason: reason,
      ownership_evidence: ownership_evidence(run_root, extra),
      safety_checks: safety_checks(workspace_root, run_root),
      warnings: [],
    )
  }
  case run_root == workspace_root || !path.contains(workspace_root, run_root) {
    True ->
      base(
        "retained",
        "workspace run root is outside the configured workspace root",
        [],
      )
    False ->
      case dict.get(active_roots, run_root) {
        Ok(True) ->
          base(
            "retained",
            "workspace run is active and protected from cleanup",
            ["active run root recorded in Scherzo ledger"],
          )
        Ok(False) | Error(Nil) ->
          case
            simplifile.is_file(workspace_run.cleanup_retention_marker(run_root))
          {
            Ok(True) ->
              base("retained", "workspace retention marker is present", [
                "retention marker .scherzo-keep-workspace is present",
              ])
            _ -> classify_manifest_backed_run(workspace_root, run_root, base)
          }
      }
  }
}

fn classify_manifest_backed_run(
  _workspace_root: String,
  run_root: String,
  base: fn(String, String, List(String)) -> WorkspaceItem,
) -> WorkspaceItem {
  case manifest_context(run_root) {
    Error("managed workspace manifest missing") ->
      base(
        "retained",
        "managed workspace manifest is missing; unmanifested directories are never deleted",
        [],
      )
    Error(reason) -> base("retained", reason, [])
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
          )
        Error(error.WorkspaceIo(reason)) ->
          base("retained", reason, manifest_evidence(run_root, context))
        Error(error.WorkspaceOutsideRoot(reason)) ->
          base(
            "retained",
            "workspace manifest path escapes configured root: " <> reason,
            manifest_evidence(run_root, context),
          )
        Error(other) ->
          base(
            "retained",
            workspace_error_message(other),
            manifest_evidence(run_root, context),
          )
      }
  }
}

fn apply_item(workspace_root: String, item: WorkspaceItem) -> WorkspaceItem {
  case item.status {
    "would_delete" -> apply_eligible_item(workspace_root, item)
    _ -> item
  }
}

fn apply_eligible_item(
  workspace_root: String,
  item: WorkspaceItem,
) -> WorkspaceItem {
  case active_run_roots(path.absolute_or_original(workspace_root)) {
    Error(reason) ->
      WorkspaceItem(
        ..item,
        status: "unavailable",
        reason: "active-run ledger unavailable; retaining workspace run root: "
          <> reason,
        warnings: [reason, ..item.warnings],
      )
    Ok(active) ->
      case dict.get(active, item.run_root) {
        Ok(True) ->
          WorkspaceItem(
            ..item,
            status: "retained",
            reason: "workspace run became active and is protected from cleanup",
          )
        Ok(False) | Error(Nil) -> delegate_cleanup_item(workspace_root, item)
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
                  ignored_directory_name(entry),
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

fn ignored_directory_name(name: String) -> Bool {
  name == ".scherzo-state"
  || name == ".git"
  || name == ".jj"
  || name == ".direnv"
}

fn looks_like_run_root(path_: String) -> Bool {
  result.unwrap(simplifile.is_directory(path.join(path_, "workspaces")), False)
  || result.unwrap(
    simplifile.is_file(workspace_manifest.manifest_path(path_)),
    False,
  )
  || result.unwrap(
    simplifile.is_file(workspace_run.cleanup_retention_marker(path_)),
    False,
  )
}

fn active_run_roots(
  workspace_root: String,
) -> Result(dict.Dict(String, Bool), String) {
  case ledger.path_for_workspace_root(workspace_root) {
    Error(err) -> Error(ledger.ledger_error_to_string(err))
    Ok(paths) ->
      case ledger.read_records(paths) {
        Error(err) -> Error(ledger.ledger_error_to_string(err))
        Ok(read) ->
          case read.truncated_tail {
            True ->
              Error(
                "active-run ledger has a truncated tail; retaining workspace run roots until activity can be proven",
              )
            False -> Ok(active_run_roots_from_records(read.records, dict.new()))
          }
      }
  }
}

fn active_run_roots_from_records(
  records: List(record.LedgerRecord),
  active: dict.Dict(String, String),
) -> dict.Dict(String, Bool) {
  case records {
    [] ->
      active
      |> dict.values
      |> list.fold(dict.new(), fn(found, root) {
        dict.insert(found, root, True)
      })
    [next, ..rest] ->
      active_run_roots_from_records(rest, fold_active_record(active, next))
  }
}

fn fold_active_record(
  active: dict.Dict(String, String),
  ledger_record: record.LedgerRecord,
) -> dict.Dict(String, String) {
  case ledger_record.body {
    record.WorkflowRunStarted(run_id, _, _, _, _, _, _, run_root)
    | record.WorkflowRunStartedWithTask(run_id, _, _, _, _, _, _, _, run_root)
    | record.ScheduledRunStarted(_, _, _, _, run_id, _, _, run_root) ->
      dict.insert(active, run_id, path.absolute_or_original(run_root))

    record.WorkflowRunFinished(run_id, _, _, _, _, _)
    | record.WorkflowRunFinishedWithTask(run_id, _, _, _, _, _, _)
    | record.WorkflowRunInterrupted(run_id, _, _, _)
    | record.WorkflowRunSuperseded(run_id, _, _, _, _)
    | record.ScheduledRunSucceeded(_, _, _, run_id, _, _, _, _)
    | record.ScheduledRunFailed(_, _, _, run_id, _, _, _, _, _)
    | record.ScheduledRunPendingCancelled(_, _, _, run_id, _, _)
    | record.ScheduledRunRetryCancelled(_, run_id, _, _) ->
      dict.delete(active, run_id)

    _ -> active
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
