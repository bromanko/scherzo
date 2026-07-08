import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/query/cursor
import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/daemon_identity
import scherzo/state/projection
import scherzo/tracker/adapter
import scherzo/work_item
import scherzo/work_item/action_derivation

const daemon_state_query_timeout_ms = 1000

pub type DispatchPausedReader =
  fn(Int) -> Result(Bool, Nil)

pub type ProjectionReader =
  fn(Int) -> Result(projection.Projection, Nil)

pub fn run(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  tracker_adapter: adapter.TrackerAdapter,
  read_dispatch_paused: DispatchPausedReader,
  query: types.QueryRequest,
) -> Result(types.QueryResponse, types.QueryError) {
  run_with_projection(
    effective,
    identity,
    tracker_adapter,
    read_dispatch_paused,
    fn(_) { Ok(projection.new()) },
    query,
  )
}

pub fn run_with_projection(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  tracker_adapter: adapter.TrackerAdapter,
  read_dispatch_paused: DispatchPausedReader,
  read_projection: ProjectionReader,
  query: types.QueryRequest,
) -> Result(types.QueryResponse, types.QueryError) {
  case query {
    types.Status ->
      execute_status_query(effective, identity, read_dispatch_paused)
    types.Metrics ->
      Error(types.QueryError(
        types.UnsupportedQuery,
        "unsupported query type: metrics",
      ))
    types.TaskList(task_query) ->
      execute_task_list_query(tracker_adapter, task_query)
    types.TaskShow(task_query) ->
      execute_task_show_query(tracker_adapter, task_query)
    types.WorkItemList(work_item_query) ->
      execute_work_item_list_query(
        tracker_adapter,
        read_dispatch_paused,
        read_projection,
        work_item_query,
      )
    types.WorkItemShow(work_item_query) ->
      execute_work_item_show_query(
        tracker_adapter,
        read_dispatch_paused,
        read_projection,
        work_item_query,
      )
    types.ClaimList
    | types.OutboxList(_)
    | types.OutboxShow(_)
    | types.OperationStatus(_)
    | types.WorkflowList
    | types.WorkflowDetail(_) ->
      Error(types.QueryError(
        types.UnsupportedQuery,
        "unsupported query type: " <> types.query_type(query),
      ))
  }
}

fn execute_status_query(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  read_dispatch_paused: DispatchPausedReader,
) -> Result(types.QueryResponse, types.QueryError) {
  case read_dispatch_paused(daemon_state_query_timeout_ms) {
    Ok(dispatch_paused) ->
      Ok(
        types.StatusResponse(
          dto.status_from_source(
            types.StatusSource(
              daemon_id: identity.daemon_id,
              boot_id: identity.boot_id,
              dispatch_paused: dispatch_paused,
              ui_server_enabled: config_types.ui_server_enabled(
                effective.ui_server,
              ),
              supported_queries: types.supported_queries(),
              local_control_token: "",
              enrollment_token: "",
              tracker_payload: "",
              workflow_internals: [],
            ),
          ),
        ),
      )
    Error(Nil) ->
      Error(types.QueryError(
        types.QueryTimeout,
        "daemon status query timed out",
      ))
  }
}

fn execute_task_list_query(
  tracker_adapter: adapter.TrackerAdapter,
  query: types.TaskListQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  use offset <- try_query(decode_task_cursor(query.cursor))
  let limit = bounded_task_limit(query.limit)
  use page <- try_query(
    map_tracker_query_error(
      tracker_adapter.task_source.list_tasks(adapter.TaskListRequest(
        state_categories: query.states,
        limit: limit,
        offset: offset,
      )),
    ),
  )
  let next_cursor = case page.has_more {
    True -> Some(cursor.encode_offset(offset + list.length(page.items)))
    False -> None
  }
  Ok(
    types.TaskListResponse(types.TaskListDto(
      items: list.map(page.items, dto.task_summary_from_task),
      page: types.PageDto(next_cursor: next_cursor, has_more: page.has_more),
    )),
  )
}

fn execute_task_show_query(
  tracker_adapter: adapter.TrackerAdapter,
  query: types.TaskShowQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  use ref <- try_query(task_query_ref_to_adapter_ref(query.ref))
  use found <- try_query(
    map_tracker_query_error(tracker_adapter.task_source.lookup_task_detail(ref)),
  )
  case found {
    Some(task) -> Ok(types.TaskShowResponse(dto.task_detail_from_task(task)))
    None -> Error(types.QueryError(types.QueryNotFound, "task not found"))
  }
}

fn execute_work_item_list_query(
  tracker_adapter: adapter.TrackerAdapter,
  read_dispatch_paused: DispatchPausedReader,
  _read_projection: ProjectionReader,
  query: types.WorkItemListQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  use capability <- try_query(required_work_item_capability(tracker_adapter))
  let normalized_query = normalize_work_item_query(query)
  let fingerprint =
    work_item.query_fingerprint(
      normalized_query.state_filter,
      normalized_query.search,
      normalized_query.sort,
    )
  use offset <- try_query(decode_work_item_cursor(
    normalized_query.cursor,
    fingerprint,
  ))
  let limit = work_item.clamp_page_limit(normalized_query.limit)
  use page <- try_query(
    map_tracker_query_error(
      capability.list_work_items(work_item.WorkItemListRequest(
        state_categories: work_item.state_filter_categories(
          normalized_query.state_filter,
        ),
        search: normalized_query.search,
        sort: normalized_query.sort,
        limit: limit,
        offset: offset,
        subtask_limit: work_item.default_list_subtask_limit,
        label_limit: work_item.default_label_limit,
      )),
    ),
  )
  use dispatch_paused <- try_query(read_dispatch_paused_query(
    read_dispatch_paused,
  ))
  let page = action_derivation.page_with_actions(page, dispatch_paused)
  let next_cursor = case page.has_more {
    True ->
      Some(cursor.encode_work_item_offset(
        offset + list.length(page.items),
        fingerprint,
      ))
    False -> None
  }
  Ok(
    types.WorkItemListResponse(work_item.WorkItemPage(
      items: page.items,
      next_cursor: next_cursor,
      has_more: page.has_more,
    )),
  )
}

pub fn load_work_item_detail(
  tracker_adapter: adapter.TrackerAdapter,
  ref ref: types.TaskQueryRef,
) -> Result(Option(work_item.WorkItemDetail), types.QueryError) {
  use capability <- try_query(required_work_item_capability(tracker_adapter))
  use ref <- try_query(work_item_query_ref_to_adapter_ref(ref))
  map_tracker_query_error(
    capability.lookup_work_item(work_item.WorkItemShowRequest(
      ref: ref,
      subtask_limit: work_item.default_show_subtask_limit,
      label_limit: work_item.default_label_limit,
    )),
  )
}

fn execute_work_item_show_query(
  tracker_adapter: adapter.TrackerAdapter,
  read_dispatch_paused: DispatchPausedReader,
  read_projection: ProjectionReader,
  query: types.WorkItemShowQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  use found <- try_query(load_work_item_detail(tracker_adapter, ref: query.ref))
  use dispatch_paused <- try_query(read_dispatch_paused_query(
    read_dispatch_paused,
  ))
  use projection_state <- try_query(read_projection_query(read_projection))
  case found {
    Some(item) ->
      Ok(
        types.WorkItemShowResponse(
          action_derivation.detail_with_actions_in_projection(
            item,
            dispatch_paused,
            projection_state: projection_state,
          ),
        ),
      )
    None -> Error(types.QueryError(types.QueryNotFound, "task not found"))
  }
}

fn bounded_task_limit(limit: Int) -> Int {
  case limit < 1 {
    True -> 1
    False ->
      case limit > 100 {
        True -> 100
        False -> limit
      }
  }
}

fn decode_task_cursor(
  cursor_value: Option(String),
) -> Result(Int, types.QueryError) {
  case cursor_value {
    Some(cursor_value) -> cursor.decode_offset(cursor_value)
    None -> Ok(0)
  }
}

fn decode_work_item_cursor(
  cursor_value: Option(String),
  fingerprint: String,
) -> Result(Int, types.QueryError) {
  case cursor_value {
    Some(cursor_value) ->
      cursor.decode_work_item_offset(cursor_value, fingerprint)
    None -> Ok(0)
  }
}

fn normalize_work_item_query(
  query: types.WorkItemListQuery,
) -> types.WorkItemListQuery {
  types.WorkItemListQuery(
    state_filter: query.state_filter,
    search: work_item.normalize_search(query.search),
    sort: query.sort,
    limit: query.limit,
    cursor: query.cursor,
  )
}

fn task_query_ref_to_adapter_ref(
  ref: types.TaskQueryRef,
) -> Result(adapter.TaskLookupRef, types.QueryError) {
  case ref {
    types.TaskDisplayId(value) -> Ok(adapter.TaskLookupByDisplayId(value))
    types.TaskRemoteId(provider: provider, id: id) ->
      Ok(adapter.TaskLookupByRemoteId(provider: provider, id: id))
  }
}

fn required_work_item_capability(
  tracker_adapter: adapter.TrackerAdapter,
) -> Result(adapter.WorkItemReadCapability, types.QueryError) {
  case tracker_adapter.work_items {
    Some(capability) -> Ok(capability)
    None ->
      Error(types.QueryError(
        types.UnsupportedQuery,
        "tracker adapter does not support work_items",
      ))
  }
}

fn work_item_query_ref_to_adapter_ref(
  ref: types.TaskQueryRef,
) -> Result(work_item.WorkItemLookupRef, types.QueryError) {
  case ref {
    types.TaskDisplayId(value) -> Ok(work_item.WorkItemLookupByDisplayId(value))
    types.TaskRemoteId(provider: provider, id: id) ->
      Ok(work_item.WorkItemLookupByRemoteId(provider: provider, id: id))
  }
}

fn read_dispatch_paused_query(
  read_dispatch_paused: DispatchPausedReader,
) -> Result(Bool, types.QueryError) {
  case read_dispatch_paused(daemon_state_query_timeout_ms) {
    Ok(dispatch_paused) -> Ok(dispatch_paused)
    Error(Nil) ->
      Error(types.QueryError(
        types.QueryTimeout,
        "work item action state query timed out",
      ))
  }
}

fn read_projection_query(
  read_projection: ProjectionReader,
) -> Result(projection.Projection, types.QueryError) {
  case read_projection(daemon_state_query_timeout_ms) {
    Ok(projection_state) -> Ok(projection_state)
    Error(Nil) ->
      Error(types.QueryError(
        types.QueryTimeout,
        "work item action projection query timed out",
      ))
  }
}

fn map_tracker_query_error(
  result: Result(a, adapter.TrackerError),
) -> Result(a, types.QueryError) {
  case result {
    Ok(value) -> Ok(value)
    Error(error) -> Error(query_error_from_tracker(error))
  }
}

fn query_error_from_tracker(error: adapter.TrackerError) -> types.QueryError {
  case error {
    adapter.NotFound(_) ->
      types.QueryError(types.QueryNotFound, "task not found")
    adapter.Unauthorized(message)
    | adapter.Transient(message)
    | adapter.Permanent(message)
    | adapter.DecodeFailed(message) ->
      types.QueryError(
        types.QueryBackendFailed,
        sanitize_tracker_message(message),
      )
    adapter.UnsupportedCapability(capability) ->
      types.QueryError(
        types.UnsupportedQuery,
        "tracker adapter does not support " <> capability,
      )
  }
}

fn sanitize_tracker_message(message: String) -> String {
  case
    string.contains(message, "RAW_PROVIDER_BODY")
    || string.contains(message, "local_control_token")
    || string.contains(message, "enrollment_token")
    || string.contains(message, "api_key")
  {
    True -> "query backend failed"
    False -> message
  }
}

fn try_query(
  result: Result(a, types.QueryError),
  next: fn(a) -> Result(b, types.QueryError),
) -> Result(b, types.QueryError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
