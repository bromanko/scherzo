import gleam/option.{None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types
import scherzo_linear_conformance_live_driver as live_driver

pub fn linear_cli_driver_invokes_script_in_offline_mode_test() {
  let manifest = offline_manifest([])
  let request = fetch_request("req-linear-offline")
  let assert Ok(driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    ..,
  )) = driver.invoke(manifest, request)

  assert diagnostics == ""
  let assert types.DriverResponseSuccess(
    result: types.TaskListResult(tasks: tasks),
    ..,
  ) = response
  let assert [task] = tasks
  let task.Task(ref: ref, title: title, description: description, ..) = task
  assert ref
    == task.TaskRef(
      backend_kind: "linear",
      remote_id: "lin-fixture-1",
      key: Some("SCONF-1"),
      url: Some("https://linear.example/scherzo-conformance/SCONF-1"),
    )
  assert title == "[Scherzo conformance fixture] task_source primary"
  assert description
    == Some(
      "Synthetic Linear conformance fixture task. Contains linear-fixture-secret for redaction tests only.",
    )
}

pub fn linear_cli_driver_supports_refresh_and_operator_lookup_test() {
  let manifest = offline_manifest([])
  let refresh_request =
    types.DriverRequest(
      schema_version: 1,
      request_id: "req-linear-refresh",
      operation: profile.TaskSourceRefreshByRefs,
      payload: types.RefreshByRefsPayload(refs: [
        task.TaskRef(
          backend_kind: "linear",
          remote_id: "lin-fixture-1",
          key: Some("SCONF-1"),
          url: None,
        ),
      ]),
    )
  let lookup_request =
    types.DriverRequest(
      schema_version: 1,
      request_id: "req-linear-lookup",
      operation: profile.TaskSourceLookupByOperatorRef,
      payload: types.LookupByOperatorRefPayload(operator_ref: "SCONF-1"),
    )

  let assert Ok(driver.DriverInvocation(response: refresh_response, ..)) =
    driver.invoke(manifest, refresh_request)
  let assert Ok(driver.DriverInvocation(response: lookup_response, ..)) =
    driver.invoke(manifest, lookup_request)

  let assert types.DriverResponseSuccess(
    result: types.TaskListResult(tasks: refresh_tasks),
    ..,
  ) = refresh_response
  let assert [refreshed] = refresh_tasks
  let task.Task(ref: refreshed_ref, ..) = refreshed
  assert refreshed_ref.remote_id == "lin-fixture-1"

  let assert types.DriverResponseSuccess(
    result: types.OptionalTaskResult(task: Some(found)),
    ..,
  ) = lookup_response
  let task.Task(ref: found_ref, ..) = found
  assert found_ref.remote_id == "lin-fixture-1"
}

pub fn linear_cli_driver_rejects_missing_live_credential_test() {
  let manifest =
    types.Manifest(
      ..offline_manifest([]),
      driver: types.CliDriverConfig(
        command: types.DriverCommand(
          executable: "scripts/scherzo-linear-conformance",
          args: [
            "driver",
            "--mode",
            "live",
            "--project",
            "fixture-linear-conformance",
          ],
          cwd: ".",
          env: [],
        ),
        timeout_ms: 5000,
      ),
    )

  let assert Ok(driver.DriverInvocation(response: response, ..)) =
    driver.invoke(manifest, fetch_request("req-linear-live-missing"))
  let assert types.DriverResponseError(
    error: types.DriverError(kind: kind, message: message, ..),
    ..,
  ) = response

  assert kind == types.PermanentError
  assert string.contains(message, "SCHERZO_LINEAR_CONFORMANCE_API_KEY")
}

pub fn linear_live_driver_rejects_unsupported_schema_before_network_test() {
  let response =
    live_driver.handle_request(
      live_driver.Live(project: "fixture-linear-conformance"),
      types.DriverRequest(
        ..fetch_request("req-linear-schema"),
        schema_version: 2,
      ),
    )
  let assert types.DriverResponseError(
    schema_version: schema_version,
    error: types.DriverError(kind: kind, message: message, ..),
    ..,
  ) = response

  assert schema_version == types.schema_version
  assert kind == types.PermanentError
  assert string.contains(message, "unsupported schema_version")
}

pub fn linear_driver_rejects_unsupported_operations_directly_test() {
  let response =
    live_driver.handle_request(
      live_driver.Offline(
        fixture_file: "test/fixtures/tracker_conformance/linear-fixture-tasks.json",
      ),
      types.DriverRequest(
        schema_version: 1,
        request_id: "req-linear-comments-disabled",
        operation: profile.CommentsPostOrUpdate,
        payload: types.CommentsPostOrUpdatePayload(
          comment: types.CommentRequestPayload(
            task: task.TaskRef(
              backend_kind: "linear",
              remote_id: "lin-fixture-1",
              key: Some("SCONF-1"),
              url: None,
            ),
            body: "side effects stay disabled for Linear dogfood",
            mode: types.CreateOnlyComment,
          ),
        ),
      ),
    )
  let assert types.DriverResponseError(
    error: types.DriverError(kind: kind, message: message, ..),
    ..,
  ) = response

  assert kind == types.PermanentError
  assert string.contains(message, "task_source operations only")
}

pub fn linear_driver_translates_tracker_not_found_errors_test() {
  let response =
    live_driver.handle_request(
      live_driver.Offline(
        fixture_file: "test/fixtures/tracker_conformance/linear-fixture-tasks.json",
      ),
      types.DriverRequest(
        schema_version: 1,
        request_id: "req-linear-not-found",
        operation: profile.TaskSourceRefreshByRefs,
        payload: types.RefreshByRefsPayload(refs: [
          task.TaskRef(
            backend_kind: "github",
            remote_id: "GH-1",
            key: Some("GH-1"),
            url: None,
          ),
        ]),
      ),
    )
  let assert types.DriverResponseError(
    error: types.DriverError(kind: kind, ref: Some(ref), ..),
    ..,
  ) = response

  assert kind == types.NotFoundError
  assert ref.backend_kind == "github"
  assert ref.remote_id == "GH-1"
}

fn offline_manifest(env: List(types.EnvVar)) -> types.Manifest {
  types.Manifest(
    schema_version: 1,
    adapter_kind: "linear",
    driver: types.CliDriverConfig(
      command: types.DriverCommand(
        executable: "scripts/scherzo-linear-conformance",
        args: [
          "driver",
          "--mode",
          "offline",
          "--fixture-file",
          "test/fixtures/tracker_conformance/linear-fixture-tasks.json",
        ],
        cwd: ".",
        env: env,
      ),
      timeout_ms: 5000,
    ),
    profile: types.ProfileConfig(
      name: profile.TaskSourceProfile,
      capabilities: [profile.TaskSourceCapability],
      requested_packs: [profile.TaskSourcePack],
      adapter_operations: [
        profile.TaskSourceFetchCandidates,
        profile.TaskSourceRefreshByRefs,
        profile.TaskSourceLookupByOperatorRef,
      ],
      retry_behavior: None,
    ),
    fixtures: types.FixtureConfig(
      task_file: "test/fixtures/tracker_conformance/linear-fixture-tasks.json",
      tasks: [],
    ),
    probes: [],
    hooks: types.HooksConfig(setup: None, cleanup: None),
    report: types.ReportConfig(redact: []),
  )
}

fn fetch_request(request_id: String) -> types.DriverRequest {
  types.DriverRequest(
    schema_version: 1,
    request_id: request_id,
    operation: profile.TaskSourceFetchCandidates,
    payload: types.FetchCandidatesPayload(task_search: types.TaskSearchPayload(
      active_states: ["Todo"],
      dispatch_states: ["Todo"],
      terminal_states: ["Done"],
      workflow_labels: ["workflow:execplan"],
      limit: 25,
    )),
  )
}
