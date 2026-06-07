import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_runtime
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

pub fn inventory(workspace_root: String) -> WorkspaceProviderResult {
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
        items: list.map(run_roots, inventory_item(root, context, _)),
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
  ledger_context: LedgerContext,
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
      case dict.get(ledger_context.active_roots, run_root) {
        Ok(True) ->
          base(
            "retained",
            "workspace run is active and protected from cleanup",
            ["active run root recorded in Scherzo ledger"],
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
              )
            Ok([]) | Error(Nil) ->
              case
                simplifile.is_file(workspace_run.cleanup_retention_marker(
                  run_root,
                ))
              {
                Ok(True) ->
                  base("retained", "workspace retention marker is present", [
                    "retention marker .scherzo-keep-workspace is present",
                  ])
                _ ->
                  classify_manifest_backed_run(workspace_root, run_root, base)
              }
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
            Ok([]) | Error(Nil) -> delegate_cleanup_item(workspace_root, item)
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
