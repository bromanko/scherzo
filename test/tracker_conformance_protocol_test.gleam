import gleam/option.{None, Some}
import scherzo/task
import scherzo/tracker/conformance
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types
import simplifile

pub fn manifest_decoder_accepts_minimal_task_source_profile_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/minimal-task-source.manifest.json",
    )
  let assert Ok(manifest) = conformance.decode_manifest(contents)

  let types.Manifest(
    schema_version: manifest_schema_version,
    adapter_kind: adapter_kind,
    driver: driver,
    profile: manifest_profile,
    fixtures: fixtures,
    report: report,
    ..,
  ) = manifest
  let assert types.CliDriverConfig(command: command, timeout_ms: timeout_ms) =
    driver
  let types.DriverCommand(
    executable: executable,
    args: args,
    cwd: cwd,
    env: env,
  ) = command
  let types.ProfileConfig(
    name: name,
    capabilities: capabilities,
    adapter_operations: operations,
  ) = manifest_profile
  let types.FixtureConfig(task_file: task_file) = fixtures
  let types.ReportConfig(redact: redact) = report

  assert manifest_schema_version == 1
  assert adapter_kind == "test-memory"
  assert driver
    == types.CliDriverConfig(command: command, timeout_ms: timeout_ms)
  assert executable
    == "test/fixtures/tracker_conformance/fake_task_source_driver.sh"
  assert args == ["--mode", "serve"]
  assert cwd == "."
  assert env == [types.EnvVar(name: "TEST_MODE", value: "fixture")]
  assert timeout_ms == 5000
  assert name == profile.TaskSourceProfile
  assert capabilities == [profile.TaskSourceCapability]
  assert operations
    == [
      profile.TaskSourceFetchCandidates,
      profile.TaskSourceRefreshByRefs,
      profile.TaskSourceLookupByOperatorRef,
    ]
  assert task_file
    == "test/fixtures/tracker_conformance/task-source-fetch.response.json"
  assert redact == ["SECRET_TOKEN"]
}

pub fn manifest_decoder_accepts_minimal_http_task_source_profile_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/minimal-http-task-source.manifest.json",
    )
  let assert Ok(manifest) = conformance.decode_manifest(contents)

  let types.Manifest(driver: driver, ..) = manifest
  let assert types.HttpDriverConfig(endpoint: endpoint, timeout_ms: timeout_ms) =
    driver
  let types.HttpEndpointConfig(url: url, headers: headers, retry: retry) =
    endpoint
  let assert [header] = headers
  let types.HttpHeaderConfig(
    name: header_name,
    value_from_env: value_from_env,
    value_prefix: value_prefix,
  ) = header
  let types.HttpRetryConfig(max_attempts: max_attempts, backoff_ms: backoff_ms) =
    retry

  assert url == "http://127.0.0.1:8080/tracker-conformance"
  assert header_name == "authorization"
  assert value_from_env == "TEST_HTTP_DRIVER_TOKEN"
  assert value_prefix == "Bearer "
  assert max_attempts == 1
  assert backoff_ms == 0
  assert timeout_ms == 5000
}

pub fn manifest_decoder_rejects_invalid_http_manifest_fields_test() {
  assert_manifest_error(
    fixture: "test/fixtures/tracker_conformance/invalid-http-url-scheme.manifest.json",
    code: "invalid_http_url",
    message: "driver.endpoint.url must be an absolute http or https URL with a host and no userinfo or fragment",
  )
  assert_manifest_error(
    fixture: "test/fixtures/tracker_conformance/invalid-http-url-userinfo.manifest.json",
    code: "invalid_http_url",
    message: "driver.endpoint.url must be an absolute http or https URL with a host and no userinfo or fragment",
  )
  assert_manifest_error(
    fixture: "test/fixtures/tracker_conformance/invalid-http-url-fragment.manifest.json",
    code: "invalid_http_url",
    message: "driver.endpoint.url must be an absolute http or https URL with a host and no userinfo or fragment",
  )
  assert_manifest_error(
    fixture: "test/fixtures/tracker_conformance/invalid-http-header-name.manifest.json",
    code: "invalid_http_header_name",
    message: "driver.endpoint.headers[].name must be non-empty and must not contain colon, carriage return, or newline",
  )
  assert_manifest_error(
    fixture: "test/fixtures/tracker_conformance/invalid-http-header-env.manifest.json",
    code: "invalid_http_header_env",
    message: "driver.endpoint.headers[].value_from_env must be non-empty",
  )
  assert_manifest_error(
    fixture: "test/fixtures/tracker_conformance/invalid-http-retry.manifest.json",
    code: "invalid_http_retry",
    message: "driver.endpoint.retry.max_attempts must be between 1 and 3, and driver.endpoint.retry.backoff_ms must be between 0 and 1000",
  )
}

pub fn manifest_decoder_rejects_unknown_capabilities_with_stable_code_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/invalid-unknown-capability.manifest.json",
    )
  let assert Error(error) = conformance.decode_manifest(contents)
  let types.ManifestError(code: code, message: message) = error

  assert code == "unknown_capability"
  assert message == "profile.capabilities currently supports only task_source"
}

pub fn manifest_decoder_rejects_fixture_namespace_in_adapter_operations_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/invalid-fixture-operation-in-adapter-pack.manifest.json",
    )
  let assert Error(error) = conformance.decode_manifest(contents)
  let types.ManifestError(code: code, message: message) = error

  assert code == "fixture_operation_disallowed"
  assert message
    == "profile.adapter_operations must not include fixture/probe/hook operations: fixture.setup"
}

pub fn manifest_decoder_rejects_missing_required_adapter_operations_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/invalid-missing-operation.manifest.json",
    )
  let assert Error(error) = conformance.decode_manifest(contents)
  let types.ManifestError(code: code, message: message) = error

  assert code == "missing_operation"
  assert message
    == "profile.adapter_operations must include task_source.lookup_by_operator_ref"
}

pub fn manifest_decoder_rejects_out_of_range_driver_timeout_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/invalid-timeout-too-large.manifest.json",
    )
  let assert Error(error) = conformance.decode_manifest(contents)
  let types.ManifestError(code: code, message: message) = error

  assert code == "invalid_driver_timeout"
  assert message == "driver.timeout_ms must be between 1 and 60000"
}

pub fn manifest_decoder_rejects_backslash_parent_fixture_paths_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/invalid-fixture-backslash-parent.manifest.json",
    )
  let assert Error(error) = conformance.decode_manifest(contents)
  let types.ManifestError(code: code, message: message) = error

  assert code == "invalid_repository_relative_path"
  assert message
    == "fixtures.task_file must be repository-relative and confined to the repository"
}

pub fn request_roundtrip_for_task_source_fetch_candidates_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/task-source-fetch.request.json",
    )
  let assert Ok(request) = conformance.decode_request(contents)
  let assert Ok(decoded_again) =
    request |> conformance.request_to_string |> conformance.decode_request

  assert decoded_again == request

  let types.DriverRequest(
    schema_version: request_schema_version,
    request_id: request_id,
    operation: operation,
    payload: payload,
  ) = request

  assert request_schema_version == 1
  assert request_id == "req-fetch-1"
  assert operation == profile.TaskSourceFetchCandidates
  assert payload
    == types.FetchCandidatesPayload(task_search: types.TaskSearchPayload(
      active_states: ["Todo", "Doing"],
      dispatch_states: ["Todo"],
      terminal_states: ["Done"],
      workflow_labels: ["workflow:execplan"],
      limit: 25,
    ))
}

pub fn success_response_roundtrip_for_task_source_payload_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/task-source-fetch.response.json",
    )
  let assert Ok(response) = conformance.decode_response(contents)
  let assert Ok(decoded_again) =
    response |> conformance.response_to_string |> conformance.decode_response

  assert decoded_again == response
  assert response
    == types.DriverResponseSuccess(
      schema_version: 1,
      request_id: "req-fetch-1",
      result: types.TaskListResult(tasks: [fixture_task()]),
    )
}

pub fn normalized_error_response_roundtrip_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/task-source-error.response.json",
    )
  let assert Ok(response) = conformance.decode_response(contents)
  let assert Ok(decoded_again) =
    response |> conformance.response_to_string |> conformance.decode_response

  assert decoded_again == response
  assert response
    == types.DriverResponseError(
      schema_version: 1,
      request_id: "req-refresh-1",
      error: types.DriverError(
        kind: types.NotFoundError,
        message: "task ref was not found",
        ref: Some(task.TaskRef(
          backend_kind: "test-memory",
          remote_id: "missing-1",
          key: None,
          url: None,
        )),
        capability: None,
      ),
    )
}

fn assert_manifest_error(
  fixture fixture: String,
  code code: String,
  message message: String,
) -> Nil {
  let assert Ok(contents) = simplifile.read(fixture)
  let assert Error(types.ManifestError(
    code: actual_code,
    message: actual_message,
  )) = conformance.decode_manifest(contents)

  assert actual_code == code
  assert actual_message == message
}

fn fixture_task() -> task.Task {
  task.Task(
    ref: task.TaskRef(
      backend_kind: "test-memory",
      remote_id: "card-1",
      key: Some("CARD-1"),
      url: Some("https://tracker.example/tasks/CARD-1"),
    ),
    title: "Fake card",
    description: Some("Fixture task"),
    priority: Some(2),
    state: task.TaskState(id: Some("todo"), name: "Todo", category: task.Ready),
    branch_hint: Some("card-1-fake"),
    labels: [task.TaskLabel(id: None, name: "workflow:execplan")],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}
