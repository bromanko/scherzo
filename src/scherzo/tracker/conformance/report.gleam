import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
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
      "counts",
      json.object([
        #("passed", json.int(passed)),
        #("failed", json.int(failed)),
        #("skipped", json.int(skipped)),
        #("setup_failed", json.int(setup_failed)),
        #("probe_failed", json.int(probe_failed)),
        #("cleanup_failed", json.int(cleanup_failed)),
      ]),
    ),
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
    hook_results: hook_results,
    probe_results: probe_results,
    ..,
  ) = report
  let base =
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
  let guidance_lines =
    recovery_guidance_lines(case_results, hook_results, probe_results, secrets)
  case guidance_lines {
    [] -> base
    _ ->
      base
      <> "\nrecovery guidance:\n"
      <> string.join(guidance_lines, with: "\n")
  }
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
    expected_summary: expected_summary,
    actual_summary: actual_summary,
    request_transcript: request_transcript,
    response_transcript: response_transcript,
    recovery_guidance: recovery_guidance,
  ) = case_result
  json.object([
    #("id", json.string(redact_string(id, secrets))),
    #("operation", json.string(redact_string(operation, secrets))),
    #("status", json.string(status_to_string(status))),
    #("request_id", json.string(redact_string(request_id, secrets))),
    #("message", json.string(redact_string(message, secrets))),
    #("diagnostics", json.string(redact_string(diagnostics, secrets))),
    #("expected_summary", json.string(redact_string(expected_summary, secrets))),
    #("actual_summary", json.string(redact_string(actual_summary, secrets))),
    #("request_transcript", transcript_to_json(request_transcript, secrets)),
    #(
      "response_transcript",
      option_transcript_to_json(response_transcript, secrets),
    ),
    #(
      "recovery_guidance",
      json.string(redact_string(recovery_guidance, secrets)),
    ),
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
    recovery_guidance: recovery_guidance,
  ) = hook_result
  json.object([
    #("phase", json.string(redact_string(phase, secrets))),
    #("status", json.string(status_to_string(status))),
    #("message", json.string(redact_string(message, secrets))),
    #("diagnostics", json.string(redact_string(diagnostics, secrets))),
    #(
      "recovery_guidance",
      json.string(redact_string(recovery_guidance, secrets)),
    ),
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
    recovery_guidance: recovery_guidance,
  ) = probe_result
  json.object([
    #("name", json.string(redact_string(name, secrets))),
    #("status", json.string(status_to_string(status))),
    #("message", json.string(redact_string(message, secrets))),
    #("diagnostics", json.string(redact_string(diagnostics, secrets))),
    #(
      "recovery_guidance",
      json.string(redact_string(recovery_guidance, secrets)),
    ),
  ])
}

fn transcript_to_json(
  transcript: types.TranscriptEvidence,
  secrets: List(String),
) -> json.Json {
  let types.TranscriptEvidence(
    body: body,
    truncated: truncated,
    original_chars: original_chars,
  ) = transcript
  json.object([
    #("body", json.string(redact_string(body, secrets))),
    #("truncated", json.bool(truncated)),
    #("original_chars", json.int(original_chars)),
  ])
}

fn option_transcript_to_json(
  transcript: Option(types.TranscriptEvidence),
  secrets: List(String),
) -> json.Json {
  case transcript {
    Some(value) -> transcript_to_json(value, secrets)
    None -> json.null()
  }
}

pub fn redact_string(value: String, secrets: List(String)) -> String {
  case secrets {
    [] -> value
    [secret, ..rest] ->
      redact_string(
        string.replace(value, each: secret, with: "[REDACTED]"),
        rest,
      )
  }
}

fn recovery_guidance_lines(
  case_results: List(types.CaseResult),
  hook_results: List(types.HookResult),
  probe_results: List(types.ProbeResult),
  secrets: List(String),
) -> List(String) {
  dedupe_lines(list_append(
    case_guidance_lines(case_results, secrets),
    list_append(
      hook_guidance_lines(hook_results, secrets),
      probe_guidance_lines(probe_results, secrets),
    ),
  ))
}

fn case_guidance_lines(
  case_results: List(types.CaseResult),
  secrets: List(String),
) -> List(String) {
  case case_results {
    [] -> []
    [
      types.CaseResult(
        status: status,
        operation: operation,
        recovery_guidance: recovery_guidance,
        ..,
      ),
      ..rest
    ] -> {
      let rest_lines = case_guidance_lines(rest, secrets)
      case status == types.FailedStatus {
        True -> [
          "- adapter "
            <> redact_string(operation, secrets)
            <> ": "
            <> redact_string(recovery_guidance, secrets),
          ..rest_lines
        ]
        False -> rest_lines
      }
    }
  }
}

fn hook_guidance_lines(
  hook_results: List(types.HookResult),
  secrets: List(String),
) -> List(String) {
  case hook_results {
    [] -> []
    [
      types.HookResult(
        phase: phase,
        status: status,
        recovery_guidance: recovery_guidance,
        ..,
      ),
      ..rest
    ] -> {
      let rest_lines = hook_guidance_lines(rest, secrets)
      case
        status == types.SetupFailedStatus || status == types.CleanupFailedStatus
      {
        True -> [
          "- "
            <> redact_string(phase, secrets)
            <> " hook: "
            <> redact_string(recovery_guidance, secrets),
          ..rest_lines
        ]
        False -> rest_lines
      }
    }
  }
}

fn probe_guidance_lines(
  probe_results: List(types.ProbeResult),
  secrets: List(String),
) -> List(String) {
  case probe_results {
    [] -> []
    [
      types.ProbeResult(
        name: name,
        status: status,
        recovery_guidance: recovery_guidance,
        ..,
      ),
      ..rest
    ] -> {
      let rest_lines = probe_guidance_lines(rest, secrets)
      case status == types.ProbeFailedStatus {
        True -> [
          "- probe "
            <> redact_string(name, secrets)
            <> ": "
            <> redact_string(recovery_guidance, secrets),
          ..rest_lines
        ]
        False -> rest_lines
      }
    }
  }
}

fn dedupe_lines(lines: List(String)) -> List(String) {
  case lines {
    [] -> []
    [first, ..rest] ->
      case string_in_list(rest, first) {
        True -> dedupe_lines(rest)
        False -> [first, ..dedupe_lines(rest)]
      }
  }
}

fn string_in_list(values: List(String), target: String) -> Bool {
  case values {
    [] -> False
    [value, ..rest] -> value == target || string_in_list(rest, target)
  }
}

fn list_append(left: List(a), right: List(a)) -> List(a) {
  case left {
    [] -> right
    [first, ..rest] -> [first, ..list_append(rest, right)]
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
