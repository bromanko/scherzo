import gleam/int
import gleam/json
import gleam/string
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn build(
  manifest: types.Manifest,
  case_results: List(types.CaseResult),
  hook_results: List(types.HookResult),
  probe_results: List(types.ProbeResult),
) -> types.Report {
  let types.Manifest(adapter_kind: adapter_kind, profile: manifest_profile, ..) =
    manifest
  let types.ProfileConfig(name: name, ..) = manifest_profile
  types.Report(
    schema_version: types.schema_version,
    adapter_kind: adapter_kind,
    profile: profile.profile_name_to_string(name),
    passed: count_passed_cases(case_results),
    failed: count_failed_cases(case_results),
    skipped: count_skipped_cases(case_results),
    setup_failed: count_setup_failed(hook_results),
    probe_failed: count_probe_failed(probe_results),
    cleanup_failed: count_cleanup_failed(hook_results),
    case_results: case_results,
    hook_results: hook_results,
    probe_results: probe_results,
  )
}

pub fn to_string(report: types.Report, redact secrets: List(String)) -> String {
  to_json(report, redact: secrets) |> json.to_string
}

pub fn to_json(
  report: types.Report,
  redact secrets: List(String),
) -> json.Json {
  let types.Report(
    schema_version: schema_version,
    adapter_kind: adapter_kind,
    profile: profile_name,
    passed: passed,
    failed: failed,
    skipped: skipped,
    setup_failed: setup_failed,
    probe_failed: probe_failed,
    cleanup_failed: cleanup_failed,
    case_results: case_results,
    hook_results: hook_results,
    probe_results: probe_results,
  ) = report
  json.object([
    #("schema_version", json.int(schema_version)),
    #("adapter_kind", json.string(redact_string(adapter_kind, secrets))),
    #("profile", json.string(redact_string(profile_name, secrets))),
    #("passed", json.int(passed)),
    #("failed", json.int(failed)),
    #("skipped", json.int(skipped)),
    #("setup_failed", json.int(setup_failed)),
    #("probe_failed", json.int(probe_failed)),
    #("cleanup_failed", json.int(cleanup_failed)),
    #(
      "case_results",
      json.array(case_results, of: fn(case_result) {
        case_result_to_json(case_result, secrets)
      }),
    ),
    #(
      "hook_results",
      json.array(hook_results, of: fn(hook_result) {
        hook_result_to_json(hook_result, secrets)
      }),
    ),
    #(
      "probe_results",
      json.array(probe_results, of: fn(probe_result) {
        probe_result_to_json(probe_result, secrets)
      }),
    ),
  ])
}

pub fn summary(report: types.Report, redact secrets: List(String)) -> String {
  let types.Report(
    adapter_kind: adapter_kind,
    profile: profile_name,
    passed: passed,
    failed: failed,
    skipped: skipped,
    setup_failed: setup_failed,
    probe_failed: probe_failed,
    cleanup_failed: cleanup_failed,
    case_results: case_results,
    ..,
  ) = report
  "tracker-conformance adapter="
  <> redact_string(adapter_kind, secrets)
  <> " profile="
  <> redact_string(profile_name, secrets)
  <> " total_cases="
  <> int.to_string(count_cases(case_results))
  <> " passed="
  <> int.to_string(passed)
  <> " failed="
  <> int.to_string(failed)
  <> " skipped="
  <> int.to_string(skipped)
  <> " setup_failed="
  <> int.to_string(setup_failed)
  <> " probe_failed="
  <> int.to_string(probe_failed)
  <> " cleanup_failed="
  <> int.to_string(cleanup_failed)
}

pub fn exit_code(report: types.Report) -> Int {
  let types.Report(
    failed: failed,
    setup_failed: setup_failed,
    probe_failed: probe_failed,
    cleanup_failed: cleanup_failed,
    ..,
  ) = report
  case failed + setup_failed + probe_failed + cleanup_failed == 0 {
    True -> 0
    False -> 1
  }
}

pub fn status_to_string(status: types.CaseStatus) -> String {
  case status {
    types.PassedStatus -> "passed"
    types.FailedStatus -> "failed"
    types.SkippedStatus -> "skipped"
    types.SetupFailedStatus -> "setup_failed"
    types.ProbeFailedStatus -> "probe_failed"
    types.CleanupFailedStatus -> "cleanup_failed"
  }
}

fn case_result_to_json(
  case_result: types.CaseResult,
  secrets: List(String),
) -> json.Json {
  let types.CaseResult(
    id: id,
    operation: operation,
    status: status,
    request_id: request_id,
    message: message,
    diagnostics: diagnostics,
  ) = case_result
  json.object([
    #("id", json.string(redact_string(id, secrets))),
    #("operation", json.string(redact_string(operation, secrets))),
    #("status", json.string(status_to_string(status))),
    #("request_id", json.string(redact_string(request_id, secrets))),
    #("message", json.string(redact_string(message, secrets))),
    #("diagnostics", json.string(redact_string(diagnostics, secrets))),
  ])
}

fn hook_result_to_json(
  hook_result: types.HookResult,
  secrets: List(String),
) -> json.Json {
  let types.HookResult(
    phase: phase,
    status: status,
    message: message,
    diagnostics: diagnostics,
  ) = hook_result
  json.object([
    #("phase", json.string(redact_string(phase, secrets))),
    #("status", json.string(status_to_string(status))),
    #("message", json.string(redact_string(message, secrets))),
    #("diagnostics", json.string(redact_string(diagnostics, secrets))),
  ])
}

fn probe_result_to_json(
  probe_result: types.ProbeResult,
  secrets: List(String),
) -> json.Json {
  let types.ProbeResult(
    name: name,
    status: status,
    message: message,
    diagnostics: diagnostics,
  ) = probe_result
  json.object([
    #("name", json.string(redact_string(name, secrets))),
    #("status", json.string(status_to_string(status))),
    #("message", json.string(redact_string(message, secrets))),
    #("diagnostics", json.string(redact_string(diagnostics, secrets))),
  ])
}

fn redact_string(value: String, secrets: List(String)) -> String {
  case secrets {
    [] -> value
    [secret, ..rest] ->
      redact_string(
        string.replace(value, each: secret, with: "[REDACTED]"),
        rest,
      )
  }
}

fn count_cases(case_results: List(types.CaseResult)) -> Int {
  case case_results {
    [] -> 0
    [_, ..rest] -> 1 + count_cases(rest)
  }
}

fn count_passed_cases(case_results: List(types.CaseResult)) -> Int {
  count_case_status(case_results, types.PassedStatus)
}

fn count_failed_cases(case_results: List(types.CaseResult)) -> Int {
  count_case_status(case_results, types.FailedStatus)
}

fn count_skipped_cases(case_results: List(types.CaseResult)) -> Int {
  count_case_status(case_results, types.SkippedStatus)
}

fn count_case_status(
  case_results: List(types.CaseResult),
  target: types.CaseStatus,
) -> Int {
  case case_results {
    [] -> 0
    [types.CaseResult(status: status, ..), ..rest] -> {
      let current = case status == target {
        True -> 1
        False -> 0
      }
      current + count_case_status(rest, target)
    }
  }
}

fn count_setup_failed(hook_results: List(types.HookResult)) -> Int {
  count_hook_status(hook_results, types.SetupFailedStatus)
}

fn count_cleanup_failed(hook_results: List(types.HookResult)) -> Int {
  count_hook_status(hook_results, types.CleanupFailedStatus)
}

fn count_hook_status(
  hook_results: List(types.HookResult),
  target: types.CaseStatus,
) -> Int {
  case hook_results {
    [] -> 0
    [types.HookResult(status: status, ..), ..rest] -> {
      let current = case status == target {
        True -> 1
        False -> 0
      }
      current + count_hook_status(rest, target)
    }
  }
}

fn count_probe_failed(probe_results: List(types.ProbeResult)) -> Int {
  case probe_results {
    [] -> 0
    [types.ProbeResult(status: status, ..), ..rest] -> {
      let current = case status == types.ProbeFailedStatus {
        True -> 1
        False -> 0
      }
      current + count_probe_failed(rest)
    }
  }
}
