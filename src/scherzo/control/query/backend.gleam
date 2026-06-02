import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/control/query/cursor
import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/daemon_identity
import scherzo/tracker/adapter

pub type DispatchPausedReader =
  fn(Int) -> Result(Bool, Nil)

pub fn run(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  tracker_adapter: adapter.TrackerAdapter,
  read_dispatch_paused: DispatchPausedReader,
  query: types.QueryRequest,
) -> Result(types.QueryResponse, types.QueryError) {
  case query {
    types.Status ->
      execute_status_query(effective, identity, read_dispatch_paused)
    types.TaskList(task_query) ->
      execute_task_list_query(tracker_adapter, task_query)
    types.TaskShow(task_query) ->
      execute_task_show_query(tracker_adapter, task_query)
  }
}

fn execute_status_query(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  read_dispatch_paused: DispatchPausedReader,
) -> Result(types.QueryResponse, types.QueryError) {
  case read_dispatch_paused(100) {
    Ok(dispatch_paused) ->
      Ok(
        types.StatusResponse(
          dto.status_from_source(
            types.StatusSource(
              daemon_id: identity.daemon_id,
              boot_id: identity.boot_id,
              dispatch_paused: dispatch_paused,
              ui_server_enabled: effective.ui_server.enabled,
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

fn task_query_ref_to_adapter_ref(
  ref: types.TaskQueryRef,
) -> Result(adapter.TaskLookupRef, types.QueryError) {
  case ref {
    types.TaskDisplayId(value) -> Ok(adapter.TaskLookupByDisplayId(value))
    types.TaskRemoteId(provider: provider, id: id) ->
      Ok(adapter.TaskLookupByRemoteId(provider: provider, id: id))
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
      types.QueryError(types.QueryBackendFailed, message)
    adapter.UnsupportedCapability(capability) ->
      types.QueryError(
        types.UnsupportedQuery,
        "tracker adapter does not support " <> capability,
      )
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
