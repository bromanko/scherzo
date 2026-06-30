import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Gt}
import gleam/result
import gleam/string
import scherzo/cleanup/cursor as cleanup_cursor
import scherzo/cleanup/local_state
import scherzo/cleanup/workspaces
import scherzo/session/event
import scherzo/state/local_artifacts

pub type CleanupMode {
  DryRun
  Apply
}

pub type CleanupRequest {
  CleanupRequest(
    mode: CleanupMode,
    workspace_root: String,
    now_ms: Int,
    limit: Option(Int),
    cursor: Option(String),
    max_runtime_ms: Option(Int),
  )
}

pub type CleanupError {
  CleanupError(code: String, message: String)
}

pub type CleanupReport {
  CleanupReport(
    mode: CleanupMode,
    workspace_root: String,
    now_ms: Int,
    providers: List(CleanupProviderReport),
    warnings: List(String),
    cursor: Option(String),
    limit: Option(Int),
    max_runtime_ms: Option(Int),
    truncated: Bool,
    next_cursor: Option(String),
    scanned: Option(Int),
    applied: Option(Int),
    truncated_reason: Option(String),
  )
}

pub type CleanupProviderReport {
  CleanupProviderReport(
    provider_id: String,
    available: Bool,
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

type ProviderInput {
  ProviderInput(
    provider_id: String,
    available: Bool,
    roots: List(String),
    transcript_root_status: String,
    warnings: List(String),
    items: List(PageItem),
  )
}

type PageItem {
  LocalStatePageItem(
    key: String,
    decision: local_artifacts.LocalArtifactDecision,
    status: String,
  )
  WorkspacePageItem(key: String, item: workspaces.WorkspaceItem)
  BoundaryPageItem(key: String, item: CleanupItemReport)
}

type ResumeCursor {
  NoResume
  ResumeAt(provider_id: String, last_item_key: String)
  ResumeConsumed
}

type BoundedState {
  BoundedState(
    providers: List(CleanupProviderReport),
    scanned: Int,
    applied: Int,
    truncated: Bool,
    next_cursor: Option(String),
    truncated_reason: Option(String),
    budget_exhausted: Bool,
    last_provider_id: Option(String),
    last_item_key: Option(String),
  )
}

pub fn inventory(workspace_root: String, now_ms: Int) -> CleanupReport {
  run(CleanupRequest(DryRun, workspace_root, now_ms, None, None, None))
}

pub fn apply(workspace_root: String, now_ms: Int) -> CleanupReport {
  run(CleanupRequest(Apply, workspace_root, now_ms, None, None, None))
}

pub fn run(request: CleanupRequest) -> CleanupReport {
  case run_request(request) {
    Ok(report) -> report
    Error(CleanupError(code, message)) ->
      CleanupReport(
        mode: request.mode,
        workspace_root: request.workspace_root,
        now_ms: request.now_ms,
        providers: [],
        warnings: [code <> ": " <> message],
        cursor: request.cursor,
        limit: request.limit,
        max_runtime_ms: request.max_runtime_ms,
        truncated: False,
        next_cursor: None,
        scanned: some_if_bounded(request, 0),
        applied: some_if_bounded(request, 0),
        truncated_reason: None,
      )
  }
}

pub fn run_request(
  request: CleanupRequest,
) -> Result(CleanupReport, CleanupError) {
  run_with_clock(request, monotonic_ms)
}

pub fn run_with_clock(
  request: CleanupRequest,
  clock: fn() -> Int,
) -> Result(CleanupReport, CleanupError) {
  case cleanup_is_bounded(request) {
    False ->
      Ok(unbounded_report(
        request,
        provider_inputs(request.workspace_root, request.now_ms),
      ))
    True -> bounded_report(request, clock)
  }
}

fn bounded_report(
  request: CleanupRequest,
  clock: fn() -> Int,
) -> Result(CleanupReport, CleanupError) {
  use resume <- result.try(decode_resume_cursor(request))
  let started_ms = clock()
  let state =
    bounded_provider_loop(
      [
        "local_state",
        "workspaces",
        "artifact_store",
        "task_store",
        "provider_live",
        "remote_provider_cache",
        "browser",
      ],
      request,
      started_ms,
      clock,
      resume,
      BoundedState([], 0, 0, False, None, None, False, None, None),
    )
  Ok(CleanupReport(
    mode: request.mode,
    workspace_root: request.workspace_root,
    now_ms: request.now_ms,
    providers: list.reverse(state.providers),
    warnings: [],
    cursor: request.cursor,
    limit: request.limit,
    max_runtime_ms: request.max_runtime_ms,
    truncated: state.truncated,
    next_cursor: state.next_cursor,
    scanned: Some(state.scanned),
    applied: Some(state.applied),
    truncated_reason: state.truncated_reason,
  ))
}

fn decode_resume_cursor(
  request: CleanupRequest,
) -> Result(ResumeCursor, CleanupError) {
  case request.cursor {
    None -> Ok(NoResume)
    Some(value) -> {
      use decoded <- result.try(
        cleanup_cursor.decode(request.workspace_root, value)
        |> result.map_error(fn(err) {
          let cleanup_cursor.CursorError(code, message) = err
          CleanupError(code, message)
        }),
      )
      let cleanup_cursor.Cursor(provider_id, last_item_key) = decoded
      case provider_id_known(provider_id) {
        True -> Ok(ResumeAt(provider_id, last_item_key))
        False ->
          Error(CleanupError(
            "unknown_provider",
            "cleanup cursor provider is not recognized by this Scherzo tree",
          ))
      }
    }
  }
}

fn provider_id_known(provider_id: String) -> Bool {
  list.any(
    [
      "local_state",
      "workspaces",
      "artifact_store",
      "task_store",
      "provider_live",
      "remote_provider_cache",
      "browser",
    ],
    fn(candidate) { candidate == provider_id },
  )
}

fn bounded_provider_loop(
  provider_ids: List(String),
  request: CleanupRequest,
  started_ms: Int,
  clock: fn() -> Int,
  resume: ResumeCursor,
  state: BoundedState,
) -> BoundedState {
  case provider_ids {
    [] -> state
    [provider_id, ..rest] ->
      case state.truncated {
        True -> state
        False ->
          case resume, provider_id {
            ResumeAt(target, _), _ if target != provider_id ->
              bounded_provider_loop(
                rest,
                request,
                started_ms,
                clock,
                resume,
                state,
              )
            _, _ -> {
              let after_key = case resume {
                ResumeAt(target, last_key) if target == provider_id ->
                  Some(last_key)
                _ -> None
              }
              let next_resume = case resume {
                ResumeAt(target, _) if target == provider_id -> ResumeConsumed
                _ -> resume
              }
              let next_state =
                process_bounded_provider(
                  provider_id,
                  request,
                  started_ms,
                  clock,
                  after_key,
                  state,
                )
              bounded_provider_loop(
                rest,
                request,
                started_ms,
                clock,
                next_resume,
                next_state,
              )
            }
          }
      }
  }
}

fn process_bounded_provider(
  provider_id: String,
  request: CleanupRequest,
  started_ms: Int,
  clock: fn() -> Int,
  after_key: Option(String),
  state: BoundedState,
) -> BoundedState {
  case provider_id {
    "local_state" ->
      process_local_state_provider(request, started_ms, clock, after_key, state)
    "workspaces" ->
      process_workspace_provider(request, started_ms, clock, after_key, state)
    _ ->
      process_provider_input(
        boundary_provider_input(
          provider_id,
          request.workspace_root,
          boundary_provider_reason(provider_id),
        ),
        request,
        started_ms,
        clock,
        after_key,
        state,
      )
  }
}

fn process_local_state_provider(
  request: CleanupRequest,
  started_ms: Int,
  clock: fn() -> Int,
  after_key: Option(String),
  state: BoundedState,
) -> BoundedState {
  let page =
    local_state.cleanup_page(
      request.workspace_root,
      request.now_ms,
      after_key,
      remaining_limit(request.limit, state.scanned),
      started_ms,
      request.max_runtime_ms,
      clock,
      request.mode == Apply,
    )
  let provider =
    CleanupProviderReport(
      provider_id: "local_state",
      available: True,
      roots: page.roots,
      transcript_root_status: page.transcript_root_status,
      items: list.map(page.items, local_state_page_item_to_report),
      warnings: page.warnings,
    )
  let last_key = case last_key_from_reports(provider.items) {
    Some(value) -> Some(value)
    None -> state.last_item_key
  }
  BoundedState(
    providers: prepend_provider_if_needed(provider, state.providers),
    scanned: state.scanned + page.scanned,
    applied: state.applied + page.applied,
    truncated: page.truncated,
    next_cursor: case page.truncated, page.next_key {
      True, Some(next_key) ->
        Some(cleanup_cursor.encode(
          request.workspace_root,
          cleanup_cursor.Cursor("local_state", next_key),
        ))
      True, None ->
        truncation_cursor_for(
          request.workspace_root,
          provider.items,
          state.last_provider_id,
          state.last_item_key,
        )
      False, _ -> None
    },
    truncated_reason: page.truncated_reason,
    budget_exhausted: page.budget_exhausted,
    last_provider_id: case last_key {
      Some(_) -> Some("local_state")
      None -> state.last_provider_id
    },
    last_item_key: last_key,
  )
}

fn boundary_provider_reason(provider_id: String) -> String {
  case provider_id {
    "artifact_store" -> "artifact repositories are read-only to generic cleanup"
    "task_store" -> "task stores are read-only to generic cleanup"
    "provider_live" -> "provider-live state is not mutated by generic cleanup"
    "remote_provider_cache" ->
      "remote-provider cache cleanup requires an explicit owning provider"
    _ -> "browser and UI state are outside generic cleanup scope"
  }
}

fn process_workspace_provider(
  request: CleanupRequest,
  started_ms: Int,
  clock: fn() -> Int,
  after_key: Option(String),
  state: BoundedState,
) -> BoundedState {
  let page =
    workspaces.cleanup_page(
      request.workspace_root,
      request.now_ms,
      after_key,
      remaining_limit(request.limit, state.scanned),
      started_ms,
      request.max_runtime_ms,
      clock,
      request.mode == Apply,
    )
  let provider =
    CleanupProviderReport(
      provider_id: "workspaces",
      available: page.available,
      roots: page.roots,
      transcript_root_status: "not_applicable",
      items: list.map(page.items, workspace_item_to_report),
      warnings: page.warnings,
    )
  let last_key = case last_key_from_reports(provider.items) {
    Some(value) -> Some(value)
    None -> state.last_item_key
  }
  BoundedState(
    providers: prepend_provider_if_needed(provider, state.providers),
    scanned: state.scanned + page.scanned,
    applied: state.applied + page.applied,
    truncated: page.truncated,
    next_cursor: case page.truncated, page.next_key {
      True, Some(next_key) ->
        Some(cleanup_cursor.encode(
          request.workspace_root,
          cleanup_cursor.Cursor("workspaces", next_key),
        ))
      True, None ->
        truncation_cursor_for(
          request.workspace_root,
          provider.items,
          state.last_provider_id,
          state.last_item_key,
        )
      False, _ -> None
    },
    truncated_reason: page.truncated_reason,
    budget_exhausted: page.budget_exhausted,
    last_provider_id: case last_key {
      Some(_) -> Some("workspaces")
      None -> state.last_provider_id
    },
    last_item_key: last_key,
  )
}

fn process_provider_input(
  input: ProviderInput,
  request: CleanupRequest,
  started_ms: Int,
  clock: fn() -> Int,
  after_key: Option(String),
  state: BoundedState,
) -> BoundedState {
  let #(
    selected,
    scanned,
    applied,
    truncated,
    next_key,
    truncated_reason,
    budget_exhausted,
  ) =
    consume_provider_items(
      input.items,
      request,
      started_ms,
      clock,
      after_key,
      state.scanned,
      state.applied,
      state.budget_exhausted,
      [],
      None,
    )
  let provider =
    CleanupProviderReport(
      provider_id: input.provider_id,
      available: input.available,
      roots: input.roots,
      transcript_root_status: input.transcript_root_status,
      items: list.reverse(selected),
      warnings: input.warnings,
    )
  let last_selected_key = case last_key_from_reports(provider.items) {
    Some(value) -> Some(value)
    None -> state.last_item_key
  }
  BoundedState(
    providers: prepend_provider_if_needed(provider, state.providers),
    scanned: scanned,
    applied: applied,
    truncated: truncated,
    next_cursor: case truncated, next_key {
      True, Some(value) ->
        Some(cleanup_cursor.encode(
          request.workspace_root,
          cleanup_cursor.Cursor(input.provider_id, value),
        ))
      True, None ->
        truncation_cursor_for(
          request.workspace_root,
          provider.items,
          state.last_provider_id,
          state.last_item_key,
        )
      False, _ -> None
    },
    truncated_reason: truncated_reason,
    budget_exhausted: budget_exhausted,
    last_provider_id: case last_selected_key {
      Some(_) -> Some(input.provider_id)
      None -> state.last_provider_id
    },
    last_item_key: last_selected_key,
  )
}

fn consume_provider_items(
  items: List(PageItem),
  request: CleanupRequest,
  started_ms: Int,
  clock: fn() -> Int,
  after_key: Option(String),
  scanned: Int,
  applied: Int,
  budget_exhausted: Bool,
  selected: List(CleanupItemReport),
  last_key: Option(String),
) -> #(
  List(CleanupItemReport),
  Int,
  Int,
  Bool,
  Option(String),
  Option(String),
  Bool,
) {
  case items {
    [] -> #(selected, scanned, applied, False, None, None, budget_exhausted)
    [item, ..rest] -> {
      let item_key = page_item_key(item)
      case after_key {
        Some(cursor_key) ->
          case string.compare(item_key, cursor_key) == Gt {
            False ->
              consume_provider_items(
                rest,
                request,
                started_ms,
                clock,
                after_key,
                scanned,
                applied,
                budget_exhausted,
                selected,
                last_key,
              )
            True ->
              case
                should_truncate_before_item(
                  request.limit,
                  request.max_runtime_ms,
                  started_ms,
                  clock,
                  scanned,
                  budget_exhausted,
                )
              {
                Some(reason) -> #(
                  selected,
                  scanned,
                  applied,
                  True,
                  last_key,
                  Some(reason),
                  budget_exhausted,
                )
                None -> {
                  let report = case request.mode {
                    DryRun -> page_item_to_report(item)
                    Apply ->
                      apply_page_item(
                        request.workspace_root,
                        request.now_ms,
                        item,
                      )
                  }
                  let next_scanned = scanned + 1
                  let next_applied = case request.mode {
                    DryRun -> applied
                    Apply -> applied + 1
                  }
                  let next_budget_exhausted =
                    hit_runtime_budget(
                      request.max_runtime_ms,
                      started_ms,
                      clock(),
                    )
                  consume_provider_items(
                    rest,
                    request,
                    started_ms,
                    clock,
                    after_key,
                    next_scanned,
                    next_applied,
                    next_budget_exhausted,
                    [report, ..selected],
                    Some(item_key),
                  )
                }
              }
          }
        None ->
          case
            should_truncate_before_item(
              request.limit,
              request.max_runtime_ms,
              started_ms,
              clock,
              scanned,
              budget_exhausted,
            )
          {
            Some(reason) -> #(
              selected,
              scanned,
              applied,
              True,
              last_key,
              Some(reason),
              budget_exhausted,
            )
            None -> {
              let report = case request.mode {
                DryRun -> page_item_to_report(item)
                Apply ->
                  apply_page_item(request.workspace_root, request.now_ms, item)
              }
              let next_scanned = scanned + 1
              let next_applied = case request.mode {
                DryRun -> applied
                Apply -> applied + 1
              }
              let next_budget_exhausted =
                hit_runtime_budget(request.max_runtime_ms, started_ms, clock())
              consume_provider_items(
                rest,
                request,
                started_ms,
                clock,
                after_key,
                next_scanned,
                next_applied,
                next_budget_exhausted,
                [report, ..selected],
                Some(item_key),
              )
            }
          }
      }
    }
  }
}

fn should_truncate_before_item(
  limit: Option(Int),
  max_runtime_ms: Option(Int),
  started_ms: Int,
  clock: fn() -> Int,
  scanned: Int,
  budget_exhausted: Bool,
) -> Option(String) {
  case hit_limit(limit, scanned) {
    True -> Some("limit")
    False ->
      case
        budget_exhausted
        || hit_runtime_budget(max_runtime_ms, started_ms, clock())
      {
        True -> Some("runtime_budget")
        False -> None
      }
  }
}

fn prepend_provider_if_needed(
  provider: CleanupProviderReport,
  providers: List(CleanupProviderReport),
) -> List(CleanupProviderReport) {
  case provider.items {
    [] -> providers
    _ -> [provider, ..providers]
  }
}

fn last_key_from_reports(items: List(CleanupItemReport)) -> Option(String) {
  case list.reverse(items) {
    [item, ..] -> Some(report_item_key(item))
    [] -> None
  }
}

fn truncation_cursor_for(
  workspace_root: String,
  provider_items: List(CleanupItemReport),
  previous_provider_id: Option(String),
  previous_item_key: Option(String),
) -> Option(String) {
  case list.reverse(provider_items) {
    [item, ..] ->
      Some(cleanup_cursor.encode(
        workspace_root,
        cleanup_cursor.Cursor(item.provider_id, report_item_key(item)),
      ))
    [] ->
      case previous_provider_id, previous_item_key {
        Some(provider_id), Some(item_key) ->
          Some(cleanup_cursor.encode(
            workspace_root,
            cleanup_cursor.Cursor(provider_id, item_key),
          ))
        _, _ -> None
      }
  }
}

fn report_item_key(item: CleanupItemReport) -> String {
  case item.provider_id {
    "local_state" -> item.item_id <> ":" <> item.display_path
    "workspaces" -> item.display_path
    _ -> item.provider_id
  }
}

fn remaining_limit(limit: Option(Int), scanned: Int) -> Option(Int) {
  case limit {
    Some(value) -> Some(value - scanned)
    None -> None
  }
}

fn provider_inputs(workspace_root: String, now_ms: Int) -> List(ProviderInput) {
  let local_state_result =
    local_artifacts.inventory(workspace_root, now_ms, True)
  let workspaces_result = workspaces.inventory(workspace_root, now_ms)
  [
    ProviderInput(
      provider_id: "local_state",
      available: True,
      roots: local_state_result.roots,
      transcript_root_status: local_state_result.transcript_root_status,
      warnings: local_state_result.warnings,
      items: local_state_items(local_state_result),
    ),
    ProviderInput(
      provider_id: "workspaces",
      available: workspaces_result.available,
      roots: workspaces_result.roots,
      transcript_root_status: "not_applicable",
      warnings: workspaces_result.warnings,
      items: workspace_items_page(workspaces_result),
    ),
    boundary_provider_input(
      "artifact_store",
      workspace_root,
      "artifact repositories are read-only to generic cleanup",
    ),
    boundary_provider_input(
      "task_store",
      workspace_root,
      "task stores are read-only to generic cleanup",
    ),
    boundary_provider_input(
      "provider_live",
      workspace_root,
      "provider-live state is not mutated by generic cleanup",
    ),
    boundary_provider_input(
      "remote_provider_cache",
      workspace_root,
      "remote-provider cache cleanup requires an explicit owning provider",
    ),
    boundary_provider_input(
      "browser",
      workspace_root,
      "browser and UI state are outside generic cleanup scope",
    ),
  ]
}

fn hit_limit(limit: Option(Int), scanned: Int) -> Bool {
  case limit {
    Some(value) -> value >= 0 && scanned >= value
    None -> False
  }
}

fn hit_runtime_budget(
  max_runtime_ms: Option(Int),
  started_ms: Int,
  now_ms: Int,
) -> Bool {
  case max_runtime_ms {
    Some(value) -> value > 0 && now_ms - started_ms >= value
    None -> False
  }
}

fn unbounded_report(
  request: CleanupRequest,
  inputs: List(ProviderInput),
) -> CleanupReport {
  let providers = case request.mode {
    DryRun -> provider_inputs_to_reports(inputs)
    Apply ->
      provider_inputs_to_apply_reports(
        request.workspace_root,
        request.now_ms,
        inputs,
      )
  }
  CleanupReport(
    mode: request.mode,
    workspace_root: request.workspace_root,
    now_ms: request.now_ms,
    providers: providers,
    warnings: [],
    cursor: request.cursor,
    limit: request.limit,
    max_runtime_ms: request.max_runtime_ms,
    truncated: False,
    next_cursor: None,
    scanned: None,
    applied: None,
    truncated_reason: None,
  )
}

fn provider_inputs_to_reports(
  inputs: List(ProviderInput),
) -> List(CleanupProviderReport) {
  inputs
  |> list.map(fn(input) {
    CleanupProviderReport(
      provider_id: input.provider_id,
      available: input.available,
      roots: input.roots,
      transcript_root_status: input.transcript_root_status,
      items: input.items |> list.map(page_item_to_report),
      warnings: input.warnings,
    )
  })
}

fn provider_inputs_to_apply_reports(
  workspace_root: String,
  now_ms: Int,
  inputs: List(ProviderInput),
) -> List(CleanupProviderReport) {
  inputs
  |> list.map(fn(input) {
    CleanupProviderReport(
      provider_id: input.provider_id,
      available: input.available,
      roots: input.roots,
      transcript_root_status: input.transcript_root_status,
      items: input.items |> list.map(apply_page_item(workspace_root, now_ms, _)),
      warnings: input.warnings,
    )
  })
}

fn local_state_items(result: local_artifacts.CleanupResult) -> List(PageItem) {
  list.flatten([
    list.map(result.would_delete, fn(decision) {
      LocalStatePageItem(local_state_key(decision), decision, "would_delete")
    }),
    list.map(result.retained, fn(decision) {
      LocalStatePageItem(local_state_key(decision), decision, "retained")
    }),
  ])
  |> list.sort(by: compare_page_items)
}

fn workspace_items_page(
  result: workspaces.WorkspaceProviderResult,
) -> List(PageItem) {
  result.items
  |> list.map(fn(item) { WorkspacePageItem(item.run_root, item) })
  |> list.sort(by: compare_page_items)
}

fn boundary_provider_input(
  provider_id: String,
  workspace_root: String,
  reason: String,
) -> ProviderInput {
  ProviderInput(
    provider_id: provider_id,
    available: False,
    roots: [workspace_root],
    transcript_root_status: "not_applicable",
    warnings: [],
    items: [
      BoundaryPageItem(
        provider_id,
        boundary_item(provider_id, workspace_root, reason),
      ),
    ],
  )
}

fn boundary_item(
  provider_id: String,
  workspace_root: String,
  reason: String,
) -> CleanupItemReport {
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
  )
}

fn apply_page_item(
  workspace_root: String,
  now_ms: Int,
  item: PageItem,
) -> CleanupItemReport {
  case item {
    LocalStatePageItem(_, decision, "would_delete") -> {
      let #(applied, warnings) =
        local_artifacts.apply_decision(workspace_root, decision, now_ms)
      case applied {
        Some(next) -> local_state_item(next, "deleted")
        None ->
          CleanupItemReport(
            ..local_state_item(decision, "retained"),
            warnings: warnings,
            reason: case warnings {
              [first, ..] -> first
              [] -> decision.reason
            },
          )
      }
    }
    LocalStatePageItem(_, decision, status) ->
      local_state_item(decision, status)
    WorkspacePageItem(_, workspace_item) ->
      workspaces.apply_item(workspace_root, now_ms, workspace_item)
      |> workspace_item_to_report
    BoundaryPageItem(_, report) -> report
  }
}

fn page_item_to_report(item: PageItem) -> CleanupItemReport {
  case item {
    LocalStatePageItem(_, decision, status) ->
      local_state_item(decision, status)
    WorkspacePageItem(_, item) -> workspace_item_to_report(item)
    BoundaryPageItem(_, item) -> item
  }
}

fn local_state_page_item_to_report(
  item: local_state.LocalStateItem,
) -> CleanupItemReport {
  let local_state.LocalStateItem(decision, status, warnings) = item
  let report = local_state_item(decision, status)
  CleanupItemReport(..report, warnings: warnings, reason: case warnings {
    [first, ..] -> first
    [] -> report.reason
  })
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

fn workspace_item_to_report(
  item: workspaces.WorkspaceItem,
) -> CleanupItemReport {
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
    retention_until_ms: item.retention_until_ms,
  )
}

fn compare_page_items(left: PageItem, right: PageItem) -> Order {
  string.compare(page_item_key(left), page_item_key(right))
}

fn page_item_key(item: PageItem) -> String {
  case item {
    LocalStatePageItem(key, _, _) -> key
    WorkspacePageItem(key, _) -> key
    BoundaryPageItem(key, _) -> key
  }
}

fn local_state_key(decision: local_artifacts.LocalArtifactDecision) -> String {
  decision.id <> ":" <> decision.display_path
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

pub fn cleanup_report_to_json(report: CleanupReport) -> json.Json {
  case cleanup_report_has_bounds(report) {
    True ->
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
        #("truncated", json.bool(report.truncated)),
        #("next_cursor", optional_string(report.next_cursor)),
        #("cursor", optional_string(report.cursor)),
        #("limit", optional_int(report.limit)),
        #("max_runtime_ms", optional_int(report.max_runtime_ms)),
      ])
    False ->
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
}

pub fn cleanup_summary(report: CleanupReport) -> String {
  let base =
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
  case cleanup_report_has_bounds(report) {
    True ->
      base
      <> " scanned="
      <> int.to_string(option.unwrap(report.scanned, 0))
      <> " applied="
      <> int.to_string(option.unwrap(report.applied, 0))
      <> " truncated="
      <> bool_to_text(report.truncated)
    False -> base
  }
}

pub fn cleanup_provider_report_to_json(
  provider: CleanupProviderReport,
) -> json.Json {
  json.object([
    #("provider_id", json.string(provider.provider_id)),
    #("available", json.bool(provider.available)),
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

fn cleanup_summary_to_json(report: CleanupReport) -> json.Json {
  case cleanup_report_has_bounds(report) {
    True ->
      json.object([
        #("providers", json.int(list.length(report.providers))),
        #("would_delete", json.int(count_status(report, "would_delete"))),
        #("deleted", json.int(count_status(report, "deleted"))),
        #("retained", json.int(count_status(report, "retained"))),
        #("unavailable", json.int(count_status(report, "unavailable"))),
        #("failed", json.int(count_status(report, "failed"))),
        #("warnings", json.int(total_warning_count(report))),
        #("scanned", optional_int(report.scanned)),
        #("applied", optional_int(report.applied)),
        #("truncated_reason", optional_string(report.truncated_reason)),
      ])
    False ->
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

fn cleanup_is_bounded(request: CleanupRequest) -> Bool {
  request.limit != None
  || request.cursor != None
  || request.max_runtime_ms != None
}

fn cleanup_report_has_bounds(report: CleanupReport) -> Bool {
  report.limit != None || report.cursor != None || report.max_runtime_ms != None
}

fn some_if_bounded(request: CleanupRequest, value: Int) -> Option(Int) {
  case cleanup_is_bounded(request) {
    True -> Some(value)
    False -> None
  }
}

fn bool_to_text(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
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

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
