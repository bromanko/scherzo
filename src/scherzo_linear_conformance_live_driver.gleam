import birl
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/file as control_file
import scherzo/linear
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/conformance
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/linear_adapter
import scherzo/tracker/state as issue_state
import simplifile

pub type Mode {
  Offline(fixture_file: String)
  Live(project: String)
}

pub type CliError {
  CliError(message: String)
}

pub fn run_cli(args: List(String)) -> Int {
  case parse_args(args) {
    Error(CliError(message)) -> {
      io.println_error(message)
      2
    }
    Ok(mode) ->
      case read_request() {
        Error(CliError(message)) -> {
          io.println_error(message)
          1
        }
        Ok(request) -> {
          emit_fake_diagnostics()
          let response = handle_request(mode, request)
          io.println(conformance.response_to_string(response))
          0
        }
      }
  }
}

pub fn handle_request(
  mode: Mode,
  request: types.DriverRequest,
) -> types.DriverResponse {
  let types.DriverRequest(
    request_id: request_id,
    operation: operation,
    payload: payload,
    ..,
  ) = request
  case request.schema_version != types.schema_version {
    True ->
      error_response(
        request_id,
        types.PermanentError,
        "unsupported schema_version",
        None,
        None,
      )
    False ->
      case build_adapter(mode) {
        Error(message) ->
          error_response(request_id, types.PermanentError, message, None, None)
        Ok(adapter_under_test) ->
          dispatch(adapter_under_test, request_id, operation, payload)
      }
  }
}

fn parse_args(args: List(String)) -> Result(Mode, CliError) {
  case args {
    ["--mode", "offline", "--fixture-file", fixture_file] ->
      Ok(Offline(fixture_file: fixture_file))
    ["--mode", "live", "--project", project] -> Ok(Live(project: project))
    _ ->
      Error(CliError(
        "Usage: scherzo-linear-conformance driver --mode offline --fixture-file <path> | --mode live --project <slug>",
      ))
  }
}

fn read_request() -> Result(types.DriverRequest, CliError) {
  use contents <- result.try(case simplifile.read("/dev/stdin") {
    Ok(contents) -> Ok(contents)
    Error(_) -> Error(CliError("could not read conformance request from stdin"))
  })
  case conformance.decode_request(contents) {
    Ok(request) -> Ok(request)
    Error(_) ->
      Error(CliError("stdin was not valid tracker-conformance request JSON"))
  }
}

fn emit_fake_diagnostics() -> Nil {
  case control_file.get_env("SCHERZO_LINEAR_CONFORMANCE_FAKE_DIAGNOSTIC") {
    Some(diagnostics) -> io.println_error(diagnostics)
    None -> Nil
  }
}

fn build_adapter(mode: Mode) -> Result(adapter.TrackerAdapter, String) {
  case mode {
    Offline(fixture_file: fixture_file) -> {
      use tasks <- result.try(load_fixture_tasks(fixture_file))
      Ok(linear_adapter.from_tracker_config(
        tracker_config(project: "scherzo-conformance-fixture", api_key: None),
        fake_transport(tasks),
      ))
    }
    Live(project: project) -> {
      use api_key <- result.try(require_live_api_key())
      Ok(linear_adapter.from_tracker_config(
        tracker_config(project: project, api_key: Some(api_key)),
        linear.http_transport,
      ))
    }
  }
}

fn tracker_config(
  project project: String,
  api_key api_key: Option(String),
) -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: live_endpoint(),
    api_key: api_key,
    project_slug: Some(project),
    task_scope: None,
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn live_endpoint() -> String {
  "https://api.linear.app/graphql"
}

fn require_live_api_key() -> Result(String, String) {
  case control_file.get_env("SCHERZO_LINEAR_CONFORMANCE_API_KEY") {
    Some(value) ->
      case string.trim(value) != "" {
        True -> Ok(value)
        False ->
          Error(
            "missing SCHERZO_LINEAR_CONFORMANCE_API_KEY for live Linear conformance",
          )
      }
    None ->
      Error(
        "missing SCHERZO_LINEAR_CONFORMANCE_API_KEY for live Linear conformance",
      )
  }
}

fn load_fixture_tasks(path: String) -> Result(List(task.Task), String) {
  use contents <- result.try(case simplifile.read(path) {
    Ok(contents) -> Ok(contents)
    Error(_) ->
      Error("could not read Linear conformance fixture file: " <> path)
  })
  case conformance.decode_response(contents) {
    Ok(types.DriverResponseSuccess(
      result: types.TaskListResult(tasks: tasks),
      ..,
    )) -> Ok(tasks)
    _ ->
      Error(
        "Linear conformance fixture file must decode to a task list response",
      )
  }
}

fn fake_transport(tasks: List(task.Task)) -> linear.Transport {
  fn(request: linear.Request) {
    let linear.Request(body: body, ..) = request
    let response_tasks = case string.contains(body, "CandidateIssues") {
      True -> tasks
      False -> select_refresh_tasks(tasks, body)
    }
    Ok(linear.Response(status: 200, body: issue_list_response(response_tasks)))
  }
}

fn select_refresh_tasks(
  tasks: List(task.Task),
  body: String,
) -> List(task.Task) {
  tasks
  |> list.filter(fn(item) {
    let task.Task(ref: ref, ..) = item
    let task.TaskRef(remote_id: remote_id, key: key, ..) = ref
    string.contains(body, remote_id)
    || case key {
      Some(identifier) -> string.contains(body, identifier)
      None -> False
    }
  })
}

fn issue_list_response(tasks: List(task.Task)) -> String {
  let task_json =
    tasks
    |> list.map(issue_json)
    |> string.join(with: ",")

  "{\"data\":{\"issues\":{\"nodes\":["
  <> task_json
  <> "],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn issue_json(item: task.Task) -> String {
  let task.Task(
    ref: ref,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_hint: branch_hint,
    labels: labels,
    blockers: blockers,
    blockers_complete: blockers_complete,
    created_at: created_at,
    updated_at: updated_at,
  ) = item
  let task.TaskRef(remote_id: remote_id, key: key, url: url, ..) = ref
  let task.TaskState(name: state_name, ..) = state
  let labels_json =
    labels
    |> list.map(fn(label) {
      let task.TaskLabel(name: name, ..) = label
      "{\"name\":" <> json_string(name) <> "}"
    })
    |> string.join(with: ",")
  let blockers_json = case blockers_complete {
    True -> "[]"
    False ->
      blockers
      |> list.map(blocker_json)
      |> string.join(with: ",")
      |> fn(nodes) { "[" <> nodes <> "]" }
  }
  "{"
  <> "\"id\":"
  <> json_string(remote_id)
  <> ","
  <> "\"identifier\":"
  <> json_string(option_string(key, remote_id))
  <> ","
  <> "\"title\":"
  <> json_string(title)
  <> ","
  <> "\"description\":"
  <> nullable_json_string(description)
  <> ","
  <> "\"priority\":"
  <> nullable_json_int(priority)
  <> ","
  <> "\"branchName\":"
  <> nullable_json_string(branch_hint)
  <> ","
  <> "\"url\":"
  <> nullable_json_string(url)
  <> ","
  <> "\"createdAt\":"
  <> nullable_json_time(created_at)
  <> ","
  <> "\"updatedAt\":"
  <> nullable_json_time(updated_at)
  <> ","
  <> "\"state\":{\"name\":"
  <> json_string(state_name)
  <> "},"
  <> "\"labels\":{\"nodes\":["
  <> labels_json
  <> "]},"
  <> "\"inverseRelations\":{\"nodes\":"
  <> blockers_json
  <> ",\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}"
  <> "}"
}

fn blocker_json(ref: task.TaskRef) -> String {
  let task.TaskRef(remote_id: remote_id, key: key, ..) = ref
  "{\"sourceIssue\":{\"id\":"
  <> json_string(remote_id)
  <> ",\"identifier\":"
  <> nullable_json_string(key)
  <> ",\"state\":null}}"
}

fn option_string(value: Option(String), fallback: String) -> String {
  case value {
    Some(value) ->
      case string.trim(value) != "" {
        True -> value
        False -> fallback
      }
    None -> fallback
  }
}

fn nullable_json_string(value: Option(String)) -> String {
  case value {
    Some(value) -> json_string(value)
    None -> "null"
  }
}

fn nullable_json_int(value: Option(Int)) -> String {
  case value {
    Some(value) -> int.to_string(value)
    None -> "null"
  }
}

fn nullable_json_time(value: Option(birl.Time)) -> String {
  case value {
    Some(value) -> json_string(birl.to_iso8601(value))
    None -> "null"
  }
}

fn json_string(value: String) -> String {
  value
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
  |> fn(value) { "\"" <> value <> "\"" }
}

fn dispatch(
  adapter_under_test: adapter.TrackerAdapter,
  request_id: String,
  operation: profile.AdapterOperation,
  payload: types.RequestPayload,
) -> types.DriverResponse {
  case operation, payload {
    profile.TaskSourceFetchCandidates,
      types.FetchCandidatesPayload(task_search: search)
    -> {
      let adapter.TrackerAdapter(task_source: task_source, ..) =
        adapter_under_test
      let types.TaskSearchPayload(
        active_states: active_states,
        dispatch_states: dispatch_states,
        terminal_states: terminal_states,
        workflow_labels: workflow_labels,
        limit: limit,
      ) = search
      case
        task_source.fetch_candidates(adapter.TaskSearchRequest(
          active_states: active_states,
          dispatch_states: dispatch_states,
          terminal_states: terminal_states,
          workflow_labels: workflow_labels,
          limit: limit,
        ))
      {
        Ok(tasks) ->
          success_response(request_id, types.TaskListResult(tasks: tasks))
        Error(error) -> from_tracker_error(request_id, error)
      }
    }
    profile.TaskSourceRefreshByRefs, types.RefreshByRefsPayload(refs: refs) -> {
      let adapter.TrackerAdapter(task_source: task_source, ..) =
        adapter_under_test
      case task_source.refresh_by_refs(refs) {
        Ok(tasks) ->
          success_response(request_id, types.TaskListResult(tasks: tasks))
        Error(error) -> from_tracker_error(request_id, error)
      }
    }
    profile.TaskSourceLookupByOperatorRef,
      types.LookupByOperatorRefPayload(operator_ref: operator_ref)
    -> {
      let adapter.TrackerAdapter(task_source: task_source, ..) =
        adapter_under_test
      case task_source.lookup_by_operator_ref(operator_ref) {
        Ok(task) ->
          success_response(request_id, types.OptionalTaskResult(task: task))
        Error(error) -> from_tracker_error(request_id, error)
      }
    }
    _, _ ->
      error_response(
        request_id,
        types.PermanentError,
        "Linear conformance driver supports task_source operations only",
        None,
        None,
      )
  }
}

fn success_response(
  request_id: String,
  result: types.ResponseResult,
) -> types.DriverResponse {
  types.DriverResponseSuccess(
    schema_version: types.schema_version,
    request_id: request_id,
    result: result,
  )
}

fn from_tracker_error(
  request_id: String,
  err: adapter.TrackerError,
) -> types.DriverResponse {
  case err {
    adapter.NotFound(ref: ref) ->
      error_response(
        request_id,
        types.NotFoundError,
        "task not found",
        Some(ref),
        None,
      )
    adapter.Transient(message: message) ->
      error_response(request_id, types.TransientError, message, None, None)
    adapter.Permanent(message: message)
    | adapter.DecodeFailed(message: message)
    | adapter.Unauthorized(message: message) ->
      error_response(request_id, types.PermanentError, message, None, None)
    adapter.UnsupportedCapability(capability: capability) ->
      error_response(
        request_id,
        types.PermanentError,
        "unsupported capability",
        None,
        Some(capability),
      )
  }
}

fn error_response(
  request_id: String,
  kind: types.DriverErrorKind,
  message: String,
  ref: Option(task.TaskRef),
  capability: Option(String),
) -> types.DriverResponse {
  types.DriverResponseError(
    schema_version: types.schema_version,
    request_id: request_id,
    error: types.DriverError(
      kind: kind,
      message: message,
      ref: ref,
      capability: capability,
    ),
  )
}
