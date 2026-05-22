import gleam/int
import gleam/option.{Some}
import scherzo/task
import scherzo/tracker/conformance/case_support
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn run(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(types.CaseResult) {
  [
    run_fetch_workflow_labels_case(manifest, fixture_tasks),
    run_refresh_blocker_refs_case(manifest, fixture_tasks),
  ]
}

fn run_fetch_workflow_labels_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-routing-fetch-workflow-labels"
  let operation = "task_source.fetch_candidates"
  let expected_summary =
    "fetch_candidates should return normalized tasks whose workflow labels match the fixture inventory."
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
      case_support.driver_failure_case_result(
        id: "routing_metadata.fetch.workflow_labels",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      workflow_labels_case_result(
        fixture_tasks: fixture_tasks,
        expected_summary: expected_summary,
        invocation: invocation,
      )
  }
}

fn run_refresh_blocker_refs_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-routing-refresh-blocker-refs"
  let operation = "task_source.refresh_by_refs"
  let expected_summary =
    "refresh_by_refs should return normalized blocker refs and blockers_complete values that match the fixture inventory."
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceRefreshByRefs,
      payload: types.RefreshByRefsPayload(refs: task_refs(fixture_tasks)),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      case_support.driver_failure_case_result(
        id: "routing_metadata.refresh.blocker_refs",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      blocker_refs_case_result(
        fixture_tasks: fixture_tasks,
        expected_summary: expected_summary,
        invocation: invocation,
      )
  }
}

fn workflow_labels_case_result(
  fixture_tasks fixture_tasks: List(task.Task),
  expected_summary expected_summary: String,
  invocation invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = case_support.response_request_id(response)
  case response {
    types.DriverResponseSuccess(result: types.TaskListResult(tasks: tasks), ..) ->
      case workflow_labels_match(fixture_tasks, tasks) {
        True ->
          case_support.passed_case_result(
            id: "routing_metadata.fetch.workflow_labels",
            operation: "task_source.fetch_candidates",
            request_id: request_id,
            message: "workflow labels matched fixture expectations",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "received "
              <> int.to_string(count_tasks(tasks))
              <> " task(s) with matching workflow labels",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        False ->
          case_support.failed_case_result(
            id: "routing_metadata.fetch.workflow_labels",
            operation: "task_source.fetch_candidates",
            request_id: request_id,
            message: "workflow labels did not match fixture expectations",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "at least one returned task was missing a required workflow label",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: "routing_metadata.fetch.workflow_labels",
        operation: "task_source.fetch_candidates",
        request_id: request_id,
        message: "fetch_candidates returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-task-list success payload",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      case_support.failed_case_result(
        id: "routing_metadata.fetch.workflow_labels",
        operation: "task_source.fetch_candidates",
        request_id: request_id,
        message: case_support.driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: case_support.driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn blocker_refs_case_result(
  fixture_tasks fixture_tasks: List(task.Task),
  expected_summary expected_summary: String,
  invocation invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = case_support.response_request_id(response)
  case response {
    types.DriverResponseSuccess(result: types.TaskListResult(tasks: tasks), ..) ->
      case blocker_refs_match(fixture_tasks, tasks) {
        True ->
          case_support.passed_case_result(
            id: "routing_metadata.refresh.blocker_refs",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "blocker refs matched fixture expectations",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "received "
              <> int.to_string(count_tasks(tasks))
              <> " refreshed task(s) with matching blocker refs",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        False ->
          case_support.failed_case_result(
            id: "routing_metadata.refresh.blocker_refs",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "blocker refs did not match fixture expectations",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "at least one returned task changed blockers or blockers_complete",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      case_support.failed_case_result(
        id: "routing_metadata.refresh.blocker_refs",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: "refresh_by_refs returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-task-list success payload",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      case_support.failed_case_result(
        id: "routing_metadata.refresh.blocker_refs",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: case_support.driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: case_support.driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn workflow_labels_match(
  expected: List(task.Task),
  actual: List(task.Task),
) -> Bool {
  case expected, actual {
    [], [] -> True
    [expected_task, ..expected_rest], [actual_task, ..actual_rest] ->
      labels_include_all(expected_task, actual_task)
      && workflow_labels_match(expected_rest, actual_rest)
    _, _ -> False
  }
}

fn blocker_refs_match(
  expected: List(task.Task),
  actual: List(task.Task),
) -> Bool {
  case expected, actual {
    [], [] -> True
    [expected_task, ..expected_rest], [actual_task, ..actual_rest] ->
      same_blockers(expected_task, actual_task)
      && blocker_refs_match(expected_rest, actual_rest)
    _, _ -> False
  }
}

fn labels_include_all(expected: task.Task, actual: task.Task) -> Bool {
  let task.Task(labels: expected_labels, ..) = expected
  let task.Task(labels: actual_labels, ..) = actual
  label_names_included(expected_labels, actual_labels)
}

fn label_names_included(
  expected_labels: List(task.TaskLabel),
  actual_labels: List(task.TaskLabel),
) -> Bool {
  case expected_labels {
    [] -> True
    [task.TaskLabel(name: expected_name, ..), ..rest] ->
      string_in_labels(actual_labels, expected_name)
      && label_names_included(rest, actual_labels)
  }
}

fn string_in_labels(labels: List(task.TaskLabel), target: String) -> Bool {
  case labels {
    [] -> False
    [task.TaskLabel(name: name, ..), ..rest] ->
      name == target || string_in_labels(rest, target)
  }
}

fn same_blockers(expected: task.Task, actual: task.Task) -> Bool {
  let task.Task(
    blockers: expected_blockers,
    blockers_complete: expected_complete,
    ..,
  ) = expected
  let task.Task(
    blockers: actual_blockers,
    blockers_complete: actual_complete,
    ..,
  ) = actual
  expected_complete == actual_complete
  && same_blocker_list(expected_blockers, actual_blockers)
}

fn same_blocker_list(
  expected: List(task.TaskRef),
  actual: List(task.TaskRef),
) -> Bool {
  case expected, actual {
    [], [] -> True
    [expected_ref, ..expected_rest], [actual_ref, ..actual_rest] ->
      case_support.same_ref(expected_ref, actual_ref)
      && same_blocker_list(expected_rest, actual_rest)
    _, _ -> False
  }
}

fn task_refs(tasks: List(task.Task)) -> List(task.TaskRef) {
  case tasks {
    [] -> []
    [task.Task(ref: ref, ..), ..rest] -> [ref, ..task_refs(rest)]
  }
}

fn count_tasks(tasks: List(task.Task)) -> Int {
  case tasks {
    [] -> 0
    [_, ..rest] -> 1 + count_tasks(rest)
  }
}
