import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/task
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn run(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(types.CaseResult) {
  [
    run_fetch_backend_kind_case(manifest),
    run_refresh_stable_identity_case(manifest, fixture_tasks),
    run_refresh_wrong_backend_ref_case(manifest, fixture_tasks),
    run_lookup_empty_operator_ref_case(manifest),
    run_lookup_known_operator_ref_case(manifest, fixture_tasks),
  ]
}

fn run_fetch_backend_kind_case(manifest: types.Manifest) -> types.CaseResult {
  let request_id = "req-task-source-fetch"
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceFetchCandidates,
      payload: types.FetchCandidatesPayload(
        task_search: types.TaskSearchPayload(
          active_states: ["Todo", "Doing"],
          dispatch_states: ["Todo"],
          terminal_states: ["Done"],
          workflow_labels: ["workflow:execplan"],
          limit: 25,
        ),
      ),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.fetch.backend_kind",
        operation: "task_source.fetch_candidates",
        request_id: request_id,
        failure: failure,
      )
    Ok(driver.DriverInvocation(response: response, diagnostics: diagnostics, ..)) ->
      case response {
        types.DriverResponseSuccess(
          result: types.TaskListResult(tasks: tasks),
          ..,
        ) -> {
          let types.Manifest(adapter_kind: adapter_kind, ..) = manifest
          case all_backend_kinds_match(tasks, adapter_kind) {
            True ->
              passed_case_result(
                id: "task_source.fetch.backend_kind",
                operation: "task_source.fetch_candidates",
                request_id: request_id,
                message: "fetch_candidates returned only declared backend refs",
                diagnostics: diagnostics,
              )
            False ->
              failed_case_result(
                id: "task_source.fetch.backend_kind",
                operation: "task_source.fetch_candidates",
                request_id: request_id,
                message: "fetch_candidates returned a task ref with the wrong backend kind",
                diagnostics: diagnostics,
              )
          }
        }
        types.DriverResponseSuccess(..) ->
          failed_case_result(
            id: "task_source.fetch.backend_kind",
            operation: "task_source.fetch_candidates",
            request_id: request_id,
            message: "fetch_candidates returned the wrong result shape",
            diagnostics: diagnostics,
          )
        types.DriverResponseError(error: error, ..) ->
          failed_case_result(
            id: "task_source.fetch.backend_kind",
            operation: "task_source.fetch_candidates",
            request_id: request_id,
            message: driver_error_message(error),
            diagnostics: diagnostics,
          )
      }
  }
}

fn run_refresh_stable_identity_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-task-source-refresh"
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceRefreshByRefs,
      payload: types.RefreshByRefsPayload(refs: task_refs(fixture_tasks)),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.refresh.stable_identity",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        failure: failure,
      )
    Ok(driver.DriverInvocation(response: response, diagnostics: diagnostics, ..)) ->
      case response {
        types.DriverResponseSuccess(
          result: types.TaskListResult(tasks: tasks),
          ..,
        ) ->
          case stable_identities_match(fixture_tasks, tasks) {
            True ->
              passed_case_result(
                id: "task_source.refresh.stable_identity",
                operation: "task_source.refresh_by_refs",
                request_id: request_id,
                message: "refresh_by_refs preserved stable (backend_kind, remote_id) identity",
                diagnostics: diagnostics,
              )
            False ->
              failed_case_result(
                id: "task_source.refresh.stable_identity",
                operation: "task_source.refresh_by_refs",
                request_id: request_id,
                message: "refresh_by_refs changed or dropped a stable task identity",
                diagnostics: diagnostics,
              )
          }
        types.DriverResponseSuccess(..) ->
          failed_case_result(
            id: "task_source.refresh.stable_identity",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "refresh_by_refs returned the wrong result shape",
            diagnostics: diagnostics,
          )
        types.DriverResponseError(error: error, ..) ->
          failed_case_result(
            id: "task_source.refresh.stable_identity",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: driver_error_message(error),
            diagnostics: diagnostics,
          )
      }
  }
}

fn run_refresh_wrong_backend_ref_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-task-source-refresh-wrong-backend"
  let wrong_ref = first_wrong_backend_ref(fixture_tasks)
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceRefreshByRefs,
      payload: types.RefreshByRefsPayload(refs: [wrong_ref]),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.refresh.wrong_backend_ref",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        failure: failure,
      )
    Ok(driver.DriverInvocation(response: response, diagnostics: diagnostics, ..)) ->
      case response {
        types.DriverResponseSuccess(result: types.TaskListResult(tasks: []), ..) ->
          passed_case_result(
            id: "task_source.refresh.wrong_backend_ref",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "refresh_by_refs omitted a wrong-backend ref",
            diagnostics: diagnostics,
          )
        types.DriverResponseError(
          error: types.DriverError(kind: types.NotFoundError, ..),
          ..,
        ) ->
          passed_case_result(
            id: "task_source.refresh.wrong_backend_ref",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "refresh_by_refs rejected a wrong-backend ref with not_found",
            diagnostics: diagnostics,
          )
        types.DriverResponseError(error: error, ..) ->
          failed_case_result(
            id: "task_source.refresh.wrong_backend_ref",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: driver_error_message(error),
            diagnostics: diagnostics,
          )
        types.DriverResponseSuccess(..) ->
          failed_case_result(
            id: "task_source.refresh.wrong_backend_ref",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "wrong-backend ref should fail with not_found or return an empty success result",
            diagnostics: diagnostics,
          )
      }
  }
}

fn run_lookup_empty_operator_ref_case(
  manifest: types.Manifest,
) -> types.CaseResult {
  let request_id = "req-task-source-lookup-empty"
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceLookupByOperatorRef,
      payload: types.LookupByOperatorRefPayload(operator_ref: "   "),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.lookup.empty_operator_ref",
        operation: "task_source.lookup_by_operator_ref",
        request_id: request_id,
        failure: failure,
      )
    Ok(driver.DriverInvocation(response: response, diagnostics: diagnostics, ..)) ->
      case response {
        types.DriverResponseSuccess(
          result: types.OptionalTaskResult(task: None),
          ..,
        ) ->
          passed_case_result(
            id: "task_source.lookup.empty_operator_ref",
            operation: "task_source.lookup_by_operator_ref",
            request_id: request_id,
            message: "lookup_by_operator_ref returned None for empty input",
            diagnostics: diagnostics,
          )
        types.DriverResponseSuccess(..) ->
          failed_case_result(
            id: "task_source.lookup.empty_operator_ref",
            operation: "task_source.lookup_by_operator_ref",
            request_id: request_id,
            message: "lookup_by_operator_ref should return None for empty input",
            diagnostics: diagnostics,
          )
        types.DriverResponseError(error: error, ..) ->
          failed_case_result(
            id: "task_source.lookup.empty_operator_ref",
            operation: "task_source.lookup_by_operator_ref",
            request_id: request_id,
            message: driver_error_message(error),
            diagnostics: diagnostics,
          )
      }
  }
}

fn run_lookup_known_operator_ref_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-task-source-lookup-known"
  let expected = first_task(fixture_tasks)
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceLookupByOperatorRef,
      payload: types.LookupByOperatorRefPayload(operator_ref: operator_ref(
        expected,
      )),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.lookup.known_operator_ref",
        operation: "task_source.lookup_by_operator_ref",
        request_id: request_id,
        failure: failure,
      )
    Ok(driver.DriverInvocation(response: response, diagnostics: diagnostics, ..)) ->
      case response {
        types.DriverResponseSuccess(
          result: types.OptionalTaskResult(task: Some(found)),
          ..,
        ) ->
          case same_identity(expected, found) {
            True ->
              passed_case_result(
                id: "task_source.lookup.known_operator_ref",
                operation: "task_source.lookup_by_operator_ref",
                request_id: request_id,
                message: "lookup_by_operator_ref resolved the same durable task identity",
                diagnostics: diagnostics,
              )
            False ->
              failed_case_result(
                id: "task_source.lookup.known_operator_ref",
                operation: "task_source.lookup_by_operator_ref",
                request_id: request_id,
                message: "lookup_by_operator_ref returned the wrong durable task identity",
                diagnostics: diagnostics,
              )
          }
        types.DriverResponseSuccess(..) ->
          failed_case_result(
            id: "task_source.lookup.known_operator_ref",
            operation: "task_source.lookup_by_operator_ref",
            request_id: request_id,
            message: "lookup_by_operator_ref did not resolve a known operator reference",
            diagnostics: diagnostics,
          )
        types.DriverResponseError(error: error, ..) ->
          failed_case_result(
            id: "task_source.lookup.known_operator_ref",
            operation: "task_source.lookup_by_operator_ref",
            request_id: request_id,
            message: driver_error_message(error),
            diagnostics: diagnostics,
          )
      }
  }
}

fn first_task(tasks: List(task.Task)) -> task.Task {
  case tasks {
    [first, ..] -> first
    [] ->
      task.Task(
        ref: task.TaskRef(
          backend_kind: "missing-fixture",
          remote_id: "missing-fixture",
          key: None,
          url: None,
        ),
        title: "missing fixture",
        description: None,
        priority: None,
        state: task.TaskState(id: None, name: "Missing", category: task.Unknown),
        branch_hint: None,
        labels: [],
        blockers: [],
        blockers_complete: True,
        created_at: None,
        updated_at: None,
      )
  }
}

fn operator_ref(value: task.Task) -> String {
  let task.Task(ref: ref, ..) = value
  let task.TaskRef(remote_id: remote_id, key: key, ..) = ref
  case key {
    Some(key) -> key
    None -> remote_id
  }
}

fn first_wrong_backend_ref(tasks: List(task.Task)) -> task.TaskRef {
  let task.Task(ref: ref, ..) = first_task(tasks)
  let task.TaskRef(remote_id: remote_id, key: key, url: url, ..) = ref
  task.TaskRef(
    backend_kind: "wrong-backend",
    remote_id: remote_id,
    key: key,
    url: url,
  )
}

fn task_refs(tasks: List(task.Task)) -> List(task.TaskRef) {
  case tasks {
    [] -> []
    [task.Task(ref: ref, ..), ..rest] -> [ref, ..task_refs(rest)]
  }
}

fn stable_identities_match(
  expected: List(task.Task),
  actual: List(task.Task),
) -> Bool {
  case expected, actual {
    [], [] -> True
    [expected_task, ..expected_rest], [actual_task, ..actual_rest] ->
      same_identity(expected_task, actual_task)
      && stable_identities_match(expected_rest, actual_rest)
    _, _ -> False
  }
}

fn same_identity(left: task.Task, right: task.Task) -> Bool {
  let task.Task(ref: left_ref, ..) = left
  let task.Task(ref: right_ref, ..) = right
  same_ref(left_ref, right_ref)
}

fn same_ref(left: task.TaskRef, right: task.TaskRef) -> Bool {
  let task.TaskRef(
    backend_kind: left_backend_kind,
    remote_id: left_remote_id,
    ..,
  ) = left
  let task.TaskRef(
    backend_kind: right_backend_kind,
    remote_id: right_remote_id,
    ..,
  ) = right
  left_backend_kind == right_backend_kind && left_remote_id == right_remote_id
}

fn all_backend_kinds_match(
  tasks: List(task.Task),
  adapter_kind: String,
) -> Bool {
  case tasks {
    [] -> True
    [task.Task(ref: task.TaskRef(backend_kind: backend_kind, ..), ..), ..rest] ->
      backend_kind == adapter_kind
      && all_backend_kinds_match(rest, adapter_kind)
  }
}

fn driver_error_message(error: types.DriverError) -> String {
  let types.DriverError(message: message, ..) = error
  message
}

fn driver_failure_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  failure failure: driver.DriverFailure,
) -> types.CaseResult {
  let driver.DriverFailure(
    message: message,
    diagnostics: diagnostics,
    stdout: stdout,
    exit_status: exit_status,
    ..,
  ) = failure
  let details =
    message <> stdout_details(stdout) <> exit_status_details(exit_status)
  failed_case_result(
    id: id,
    operation: operation,
    request_id: request_id,
    message: details,
    diagnostics: diagnostics,
  )
}

fn stdout_details(stdout: Option(String)) -> String {
  case stdout {
    Some(stdout) -> "; stdout=" <> stdout
    None -> ""
  }
}

fn exit_status_details(exit_status: Option(Int)) -> String {
  case exit_status {
    Some(status) -> "; exit_status=" <> int.to_string(status)
    None -> ""
  }
}

fn passed_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
) -> types.CaseResult {
  types.CaseResult(
    id: id,
    operation: operation,
    status: types.PassedStatus,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
  )
}

fn failed_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
) -> types.CaseResult {
  types.CaseResult(
    id: id,
    operation: operation,
    status: types.FailedStatus,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
  )
}
