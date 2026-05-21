import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/task
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

const adapter_recovery_guidance = "Inspect the adapter implementation for this public task_source operation; fixture and support checks are reported separately."

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
  let operation = "task_source.fetch_candidates"
  let expected_summary =
    "fetch_candidates should return only tasks whose refs use the manifest adapter_kind."
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
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      fetch_backend_kind_result(manifest, expected_summary, invocation)
  }
}

fn fetch_backend_kind_result(
  manifest: types.Manifest,
  expected_summary: String,
  invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = response_request_id(response)
  case response {
    types.DriverResponseSuccess(result: types.TaskListResult(tasks: tasks), ..) -> {
      let types.Manifest(adapter_kind: adapter_kind, ..) = manifest
      case all_backend_kinds_match(tasks, adapter_kind) {
        True ->
          passed_case_result(
            id: "task_source.fetch.backend_kind",
            operation: "task_source.fetch_candidates",
            request_id: request_id,
            message: "fetch_candidates returned only declared backend refs",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "received "
              <> int.to_string(count_tasks(tasks))
              <> " task(s) on backend "
              <> adapter_kind,
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        False ->
          failed_case_result(
            id: "task_source.fetch.backend_kind",
            operation: "task_source.fetch_candidates",
            request_id: request_id,
            message: "fetch_candidates returned a task ref with the wrong backend kind",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "received "
              <> int.to_string(count_tasks(tasks))
              <> " task(s) with at least one mismatched backend_kind",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
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
        expected_summary: expected_summary,
        actual_summary: "received an optional-task response instead of a task list",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      failed_case_result(
        id: "task_source.fetch.backend_kind",
        operation: "task_source.fetch_candidates",
        request_id: request_id,
        message: driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn run_refresh_stable_identity_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-task-source-refresh"
  let operation = "task_source.refresh_by_refs"
  let expected_tasks = refresh_expected_tasks(manifest, fixture_tasks)
  let expected_summary =
    "refresh_by_refs should preserve stable (backend_kind, remote_id) identity for "
    <> fixture_subject(manifest, expected_tasks)
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceRefreshByRefs,
      payload: types.RefreshByRefsPayload(refs: task_refs(expected_tasks)),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.refresh.stable_identity",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      refresh_stable_identity_result(
        expected_tasks,
        expected_summary,
        invocation,
      )
  }
}

fn refresh_stable_identity_result(
  expected_tasks: List(task.Task),
  expected_summary: String,
  invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = response_request_id(response)
  case response {
    types.DriverResponseSuccess(result: types.TaskListResult(tasks: tasks), ..) ->
      case stable_identities_match(expected_tasks, tasks) {
        True ->
          passed_case_result(
            id: "task_source.refresh.stable_identity",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "refresh_by_refs preserved stable (backend_kind, remote_id) identity",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "received "
              <> int.to_string(count_tasks(tasks))
              <> " refreshed task(s) with matching stable identity",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        False ->
          failed_case_result(
            id: "task_source.refresh.stable_identity",
            operation: "task_source.refresh_by_refs",
            request_id: request_id,
            message: "refresh_by_refs changed or dropped a stable task identity",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "received "
              <> int.to_string(count_tasks(tasks))
              <> " refreshed task(s) with a missing or changed stable identity",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      failed_case_result(
        id: "task_source.refresh.stable_identity",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: "refresh_by_refs returned the wrong result shape",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "received an optional-task response instead of a refreshed task list",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      failed_case_result(
        id: "task_source.refresh.stable_identity",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn run_refresh_wrong_backend_ref_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-task-source-refresh-wrong-backend"
  let wrong_ref =
    first_wrong_backend_ref(refresh_expected_tasks(manifest, fixture_tasks))
  let operation = "task_source.refresh_by_refs"
  let expected_summary =
    "refresh_by_refs should reject a ref from the wrong backend or omit it from a successful result."
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
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) -> wrong_backend_ref_result(expected_summary, invocation)
  }
}

fn wrong_backend_ref_result(
  expected_summary: String,
  invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = response_request_id(response)
  case response {
    types.DriverResponseSuccess(result: types.TaskListResult(tasks: []), ..) ->
      passed_case_result(
        id: "task_source.refresh.wrong_backend_ref",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: "refresh_by_refs omitted a wrong-backend ref",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "received an empty refreshed task list for the wrong-backend ref",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
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
        expected_summary: expected_summary,
        actual_summary: "driver returned not_found for the wrong-backend ref",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      failed_case_result(
        id: "task_source.refresh.wrong_backend_ref",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseSuccess(..) ->
      failed_case_result(
        id: "task_source.refresh.wrong_backend_ref",
        operation: "task_source.refresh_by_refs",
        request_id: request_id,
        message: "wrong-backend ref should fail with not_found or return an empty success result",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a non-empty success result for a wrong-backend ref",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn run_lookup_empty_operator_ref_case(
  manifest: types.Manifest,
) -> types.CaseResult {
  let request_id = "req-task-source-lookup-empty"
  let operation = "task_source.lookup_by_operator_ref"
  let expected_summary =
    "lookup_by_operator_ref should return no task for blank operator input."
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
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) -> empty_lookup_result(expected_summary, invocation)
  }
}

fn empty_lookup_result(
  expected_summary: String,
  invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = response_request_id(response)
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
        expected_summary: expected_summary,
        actual_summary: "driver returned no task for the blank operator_ref",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseSuccess(..) ->
      failed_case_result(
        id: "task_source.lookup.empty_operator_ref",
        operation: "task_source.lookup_by_operator_ref",
        request_id: request_id,
        message: "lookup_by_operator_ref should return None for empty input",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned a task for the blank operator_ref",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      failed_case_result(
        id: "task_source.lookup.empty_operator_ref",
        operation: "task_source.lookup_by_operator_ref",
        request_id: request_id,
        message: driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn run_lookup_known_operator_ref_case(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> types.CaseResult {
  let request_id = "req-task-source-lookup-known"
  let operation = "task_source.lookup_by_operator_ref"
  let expected = known_lookup_task(manifest, fixture_tasks)
  let operator_ref_value = known_operator_ref(manifest, expected)
  let expected_summary =
    "lookup_by_operator_ref should resolve the same durable task identity for "
    <> fixture_subject(manifest, [expected])
  let request =
    types.DriverRequest(
      schema_version: types.schema_version,
      request_id: request_id,
      operation: profile.TaskSourceLookupByOperatorRef,
      payload: types.LookupByOperatorRefPayload(
        operator_ref: operator_ref_value,
      ),
    )
  case driver.invoke(manifest, request) {
    Error(failure) ->
      driver_failure_case_result(
        id: "task_source.lookup.known_operator_ref",
        operation: operation,
        request_id: request_id,
        expected_summary: expected_summary,
        failure: failure,
      )
    Ok(invocation) ->
      known_lookup_result(expected, expected_summary, invocation)
  }
}

fn known_lookup_result(
  expected: task.Task,
  expected_summary: String,
  invocation: driver.DriverInvocation,
) -> types.CaseResult {
  let driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  ) = invocation
  let request_id = response_request_id(response)
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
            expected_summary: expected_summary,
            actual_summary: "driver resolved a task with matching durable identity",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
        False ->
          failed_case_result(
            id: "task_source.lookup.known_operator_ref",
            operation: "task_source.lookup_by_operator_ref",
            request_id: request_id,
            message: "lookup_by_operator_ref returned the wrong durable task identity",
            diagnostics: diagnostics,
            expected_summary: expected_summary,
            actual_summary: "driver returned a task whose durable identity did not match the fixture",
            request_transcript: request_transcript,
            response_transcript: Some(response_transcript),
          )
      }
    types.DriverResponseSuccess(..) ->
      failed_case_result(
        id: "task_source.lookup.known_operator_ref",
        operation: "task_source.lookup_by_operator_ref",
        request_id: request_id,
        message: "lookup_by_operator_ref did not resolve a known operator reference",
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: "driver returned no task for the known operator_ref",
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
    types.DriverResponseError(error: error, ..) ->
      failed_case_result(
        id: "task_source.lookup.known_operator_ref",
        operation: "task_source.lookup_by_operator_ref",
        request_id: request_id,
        message: driver_error_message(error),
        diagnostics: diagnostics,
        expected_summary: expected_summary,
        actual_summary: driver_error_actual_summary(error),
        request_transcript: request_transcript,
        response_transcript: Some(response_transcript),
      )
  }
}

fn refresh_expected_tasks(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> List(task.Task) {
  let types.Manifest(fixtures: fixtures, ..) = manifest
  let types.FixtureConfig(tasks: declarations, ..) = fixtures
  case declarations {
    [] -> fixture_tasks
    _ -> resolve_declared_tasks(declarations, fixture_tasks)
  }
}

fn known_lookup_task(
  manifest: types.Manifest,
  fixture_tasks: List(task.Task),
) -> task.Task {
  let types.Manifest(fixtures: fixtures, ..) = manifest
  let types.FixtureConfig(tasks: declarations, ..) = fixtures
  case declarations {
    [] -> first_task(fixture_tasks)
    [types.FixtureTaskDeclaration(ref: ref, ..), ..] ->
      find_task_by_ref(fixture_tasks, ref)
  }
}

fn known_operator_ref(manifest: types.Manifest, fallback: task.Task) -> String {
  let types.Manifest(fixtures: fixtures, ..) = manifest
  let types.FixtureConfig(tasks: declarations, ..) = fixtures
  case declarations {
    [] -> operator_ref(fallback)
    [types.FixtureTaskDeclaration(operator_refs: [first, ..], ..), ..] -> first
    _ -> operator_ref(fallback)
  }
}

fn fixture_subject(
  manifest: types.Manifest,
  fallback_tasks: List(task.Task),
) -> String {
  let types.Manifest(fixtures: fixtures, ..) = manifest
  let types.FixtureConfig(tasks: declarations, ..) = fixtures
  case declarations {
    [] ->
      int.to_string(count_tasks(fallback_tasks))
      <> " fixture task(s) from fixtures.task_file"
    _ -> declaration_subjects(declarations)
  }
}

fn declaration_subjects(
  declarations: List(types.FixtureTaskDeclaration),
) -> String {
  case declarations {
    [] -> "fixture declarations"
    [types.FixtureTaskDeclaration(name: name, ..)] ->
      "fixture declaration " <> name
    [types.FixtureTaskDeclaration(name: first, ..), ..rest] ->
      "fixture declarations " <> first <> declaration_subject_tail(rest)
  }
}

fn declaration_subject_tail(
  declarations: List(types.FixtureTaskDeclaration),
) -> String {
  case declarations {
    [] -> ""
    [types.FixtureTaskDeclaration(name: name, ..), ..rest] ->
      ", " <> name <> declaration_subject_tail(rest)
  }
}

fn resolve_declared_tasks(
  declarations: List(types.FixtureTaskDeclaration),
  fixture_tasks: List(task.Task),
) -> List(task.Task) {
  case declarations {
    [] -> []
    [types.FixtureTaskDeclaration(ref: ref, ..), ..rest] -> [
      find_task_by_ref(fixture_tasks, ref),
      ..resolve_declared_tasks(rest, fixture_tasks)
    ]
  }
}

fn find_task_by_ref(tasks: List(task.Task), target: task.TaskRef) -> task.Task {
  case tasks {
    [task_value, ..rest] -> {
      let task.Task(ref: ref, ..) = task_value
      case same_ref(ref, target) {
        True -> task_value
        False -> find_task_by_ref(rest, target)
      }
    }
    [] -> first_task(tasks)
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

fn count_tasks(tasks: List(task.Task)) -> Int {
  case tasks {
    [] -> 0
    [_, ..rest] -> 1 + count_tasks(rest)
  }
}

fn response_request_id(response: types.DriverResponse) -> String {
  case response {
    types.DriverResponseSuccess(request_id: request_id, ..) -> request_id
    types.DriverResponseError(request_id: request_id, ..) -> request_id
  }
}

fn driver_error_message(error: types.DriverError) -> String {
  let types.DriverError(message: message, ..) = error
  message
}

fn driver_error_actual_summary(error: types.DriverError) -> String {
  let types.DriverError(kind: kind, message: message, ..) = error
  "driver returned " <> driver_error_kind_name(kind) <> ": " <> message
}

fn driver_error_kind_name(kind: types.DriverErrorKind) -> String {
  case kind {
    types.UnauthorizedError -> "unauthorized"
    types.NotFoundError -> "not_found"
    types.TransientError -> "transient"
    types.PermanentError -> "permanent"
    types.UnsupportedCapabilityError -> "unsupported_capability"
    types.DecodeFailedError -> "decode_failed"
  }
}

fn driver_failure_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  expected_summary expected_summary: String,
  failure failure: driver.DriverFailure,
) -> types.CaseResult {
  let driver.DriverFailure(
    message: message,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
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
    expected_summary: expected_summary,
    actual_summary: "driver transport failed before a usable conformance response was accepted",
    request_transcript: request_transcript,
    response_transcript: response_transcript,
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
  expected_summary expected_summary: String,
  actual_summary actual_summary: String,
  request_transcript request_transcript: types.TranscriptEvidence,
  response_transcript response_transcript: Option(types.TranscriptEvidence),
) -> types.CaseResult {
  case_result(
    id: id,
    operation: operation,
    status: types.PassedStatus,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
  )
}

fn failed_case_result(
  id id: String,
  operation operation: String,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
  expected_summary expected_summary: String,
  actual_summary actual_summary: String,
  request_transcript request_transcript: types.TranscriptEvidence,
  response_transcript response_transcript: Option(types.TranscriptEvidence),
) -> types.CaseResult {
  case_result(
    id: id,
    operation: operation,
    status: types.FailedStatus,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
  )
}

fn case_result(
  id id: String,
  operation operation: String,
  status status: types.CaseStatus,
  request_id request_id: String,
  message message: String,
  diagnostics diagnostics: String,
  expected_summary expected_summary: String,
  actual_summary actual_summary: String,
  request_transcript request_transcript: types.TranscriptEvidence,
  response_transcript response_transcript: Option(types.TranscriptEvidence),
) -> types.CaseResult {
  types.CaseResult(
    id: id,
    operation: operation,
    status: status,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    recovery_guidance: adapter_recovery_guidance,
  )
}
