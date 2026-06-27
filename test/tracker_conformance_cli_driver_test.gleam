import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/path as scherzo_path
import scherzo/task
import scherzo/tracker/conformance
import scherzo/tracker/conformance/driver
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types
import simplifile
import support/test_helpers

pub fn cli_driver_invokes_fake_process_and_decodes_response_test() {
  let manifest =
    fixture_manifest(
      executable: "test/fixtures/tracker_conformance/fake_task_source_driver.sh",
      args: ["--mode", "serve", "--scenario", "pass"],
      timeout_ms: 5000,
    )
  let request = fetch_request("req-driver-pass")
  let assert Ok(driver.DriverInvocation(
    response: response,
    diagnostics: diagnostics,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  )) = driver.invoke(manifest, request)
  let types.TranscriptEvidence(body: request_body, ..) = request_transcript
  let types.TranscriptEvidence(body: response_body, ..) = response_transcript

  assert diagnostics != ""
  assert string.contains(request_body, "task_source.fetch_candidates")
  assert string.contains(response_body, "\"ok\":true")
  assert response
    == types.DriverResponseSuccess(
      schema_version: 1,
      request_id: "req-driver-pass",
      result: types.TaskListResult(tasks: [fixture_task()]),
    )
}

pub fn cli_driver_classifies_malformed_json_exit_and_timeout_failures_test() {
  let malformed_manifest =
    fixture_manifest(
      executable: "test/fixtures/tracker_conformance/fake_driver_malformed.sh",
      args: [],
      timeout_ms: 5000,
    )
  let exit_manifest =
    fixture_manifest(
      executable: "test/fixtures/tracker_conformance/fake_driver_exit_2.sh",
      args: [],
      timeout_ms: 5000,
    )
  let timeout_manifest =
    fixture_manifest(
      executable: "sh",
      args: ["-c", "while :; do :; done"],
      timeout_ms: 10,
    )
  let no_stdout_manifest =
    fixture_manifest(executable: "sh", args: ["-c", "exit 0"], timeout_ms: 5000)
  let request = fetch_request("req-driver-failure")

  let assert Error(driver.DriverFailure(
    kind: malformed_kind,
    request_transcript: malformed_request,
    response_transcript: malformed_response,
    ..,
  )) = driver.invoke(malformed_manifest, request)
  let assert Error(driver.DriverFailure(
    kind: exit_kind,
    request_transcript: exit_request,
    response_transcript: exit_response,
    exit_status: exit_status,
    ..,
  )) = driver.invoke(exit_manifest, request)
  let assert Error(driver.DriverFailure(
    kind: timeout_kind,
    request_transcript: timeout_request,
    response_transcript: timeout_response,
    ..,
  )) = driver.invoke(timeout_manifest, request)
  let assert Error(driver.DriverFailure(
    kind: no_stdout_kind,
    request_transcript: no_stdout_request,
    response_transcript: no_stdout_response,
    ..,
  )) = driver.invoke(no_stdout_manifest, request)

  assert malformed_kind == driver.MalformedResponseFailed
  assert transcript_contains(malformed_request, "task_source.fetch_candidates")
  assert option_transcript_contains(malformed_response, "{not-json}")
  assert exit_kind == driver.ExitStatusFailed
  assert transcript_contains(exit_request, "task_source.fetch_candidates")
  assert exit_response == None
  assert exit_status == Some(2)
  assert timeout_kind == driver.TimeoutFailed
  assert transcript_contains(timeout_request, "task_source.fetch_candidates")
  assert timeout_response == None
  assert no_stdout_kind == driver.MissingStdoutFailed
  assert transcript_contains(no_stdout_request, "task_source.fetch_candidates")
  assert no_stdout_response == None
}

pub fn cli_driver_rejects_stale_response_envelopes_test() {
  let manifest =
    fixture_manifest(
      executable: "test/fixtures/tracker_conformance/fake_driver_stale_envelope.sh",
      args: [],
      timeout_ms: 5000,
    )
  let request = fetch_request("req-driver-correlation")

  let assert Error(driver.DriverFailure(
    kind: kind,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    ..,
  )) = driver.invoke(manifest, request)

  assert kind == driver.MalformedResponseFailed
  assert transcript_contains(request_transcript, "task_source.fetch_candidates")
  assert option_transcript_contains(response_transcript, "stale-request")
}

pub fn cli_driver_truncates_large_diagnostics_test() {
  let manifest =
    fixture_manifest(
      executable: "test/fixtures/tracker_conformance/fake_driver_long_stderr.sh",
      args: [],
      timeout_ms: 5000,
    )
  let request = fetch_request("req-driver-long-stderr")
  let assert Ok(driver.DriverInvocation(diagnostics: diagnostics, ..)) =
    driver.invoke(manifest, request)

  assert string.length(diagnostics) > types.max_external_diagnostics_chars
  assert string.length(diagnostics) < 5002
  assert string.contains(diagnostics, "[truncated")
}

pub fn cli_driver_preserves_parent_path_for_env_shebangs_test() {
  let cwd = "test/tmp/tracker-conformance/path-env-driver"
  let bin = cwd <> "/bin"
  let runtime = bin <> "/fake-conformance-driver-runtime"
  let script = cwd <> "/driver"
  test_helpers.reset_dir(cwd)
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let assert Ok(Nil) =
    simplifile.write(runtime, fake_conformance_driver_runtime_script())
  let assert Ok(Nil) = simplifile.write(script, fake_env_shebang_script())
  test_helpers.chmod_executable(runtime)
  test_helpers.chmod_executable(script)

  let assert Ok(bin_path) = scherzo_path.absolute(bin)
  let result =
    driver.invoke(
      fixture_manifest_with_env(
        executable: script,
        args: [],
        timeout_ms: 5000,
        env: [
          types.EnvVar("PATH", prepend_path(bin_path, scherzo_path.env("PATH"))),
        ],
      ),
      fetch_request("req-driver-path"),
    )

  let assert Ok(driver.DriverInvocation(response: response, ..)) = result
  assert response
    == types.DriverResponseSuccess(
      schema_version: 1,
      request_id: "req-driver-path",
      result: types.TaskListResult(tasks: []),
    )
}

fn fake_conformance_driver_runtime_script() -> String {
  "#!/bin/sh\n"
  <> "cat >/dev/null\n"
  <> "printf '{\"schema_version\":1,\"request_id\":\"req-driver-path\",\"ok\":true,\"result\":{\"tasks\":[]}}\\n'\n"
}

fn fake_env_shebang_script() -> String {
  "#!/usr/bin/env fake-conformance-driver-runtime\n"
}

fn prepend_path(path: String, original_path: Option(String)) -> String {
  case original_path {
    Some(original_path) -> path <> ":" <> original_path
    None -> path
  }
}

fn fixture_manifest(
  executable executable: String,
  args args: List(String),
  timeout_ms timeout_ms: Int,
) -> types.Manifest {
  fixture_manifest_with_env(executable:, args:, timeout_ms:, env: [])
}

fn fixture_manifest_with_env(
  executable executable: String,
  args args: List(String),
  timeout_ms timeout_ms: Int,
  env env: List(types.EnvVar),
) -> types.Manifest {
  types.Manifest(
    schema_version: 1,
    adapter_kind: "test-memory",
    driver: types.CliDriverConfig(
      command: types.DriverCommand(
        executable: executable,
        args: args,
        cwd: ".",
        env: env,
      ),
      timeout_ms: timeout_ms,
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
      task_file: "test/fixtures/tracker_conformance/task-source-fetch.response.json",
      tasks: [],
    ),
    probes: [],
    hooks: types.HooksConfig(setup: None, cleanup: None),
    report: types.ReportConfig(redact: []),
  )
}

fn fetch_request(request_id request_id: String) -> types.DriverRequest {
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

fn transcript_contains(
  transcript: types.TranscriptEvidence,
  fragment: String,
) -> Bool {
  let types.TranscriptEvidence(body: body, ..) = transcript
  string.contains(body, fragment)
}

fn option_transcript_contains(
  transcript: Option(types.TranscriptEvidence),
  fragment: String,
) -> Bool {
  case transcript {
    Some(value) -> transcript_contains(value, fragment)
    None -> False
  }
}

fn fixture_task() -> task.Task {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/tracker_conformance/task-source-fetch.response.json",
    )
  let assert Ok(types.DriverResponseSuccess(
    result: types.TaskListResult(tasks: [task]),
    ..,
  )) = conformance.decode_response(contents)
  task
}
