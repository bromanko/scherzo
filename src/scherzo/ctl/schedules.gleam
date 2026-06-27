import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/ctl/schedule_state
import scherzo/path
import scherzo/schedule_doctor
import scherzo/state/projection
import simplifile

type ScheduleDoctorReport {
  ScheduleDoctorReport(
    job_id: String,
    config_path: Option(String),
    diagnostics: List(schedule_doctor.Diagnostic),
  )
}

pub fn run_status(
  root: String,
  job_id: Option(String),
  json_output json_output: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(load_projection(root))
  let statuses = case job_id {
    None -> projection.scheduled_statuses(projected)
    Some(id) ->
      case projection.scheduled_status_for(projected, id) {
        Ok(status) -> [status]
        Error(Nil) -> []
      }
  }
  case json_output {
    True ->
      output_line(
        json.object([
          #("schedules", json.array(statuses, of: scheduled_status_to_json)),
        ])
        |> json.to_string,
      )
    False -> print_scheduled_statuses(statuses, output_line)
  }
  Ok(Nil)
}

pub fn run_history(
  root: String,
  job_id: String,
  json_output json_output: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use projected <- result.try(load_projection(root))
  let status = projection.scheduled_status_for(projected, job_id)
  case status {
    Error(_) -> Error(#("schedule_not_found", "scheduled job not found"))
    Ok(status) -> {
      case json_output {
        True -> output_line(scheduled_status_to_json(status) |> json.to_string)
        False -> print_scheduled_history(status, output_line)
      }
      Ok(Nil)
    }
  }
}

pub fn run_doctor(
  workspace_root: Result(String, #(String, String)),
  explicit_root: Option(String),
  job_id: String,
  json_output json_output: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  run_doctor_with_env(
    workspace_root,
    explicit_root,
    job_id,
    json_output: json_output,
    line: output_line,
    env: path.env,
  )
}

pub fn run_doctor_with_env(
  workspace_root: Result(String, #(String, String)),
  explicit_root: Option(String),
  job_id: String,
  json_output json_output: Bool,
  line output_line: fn(String) -> Nil,
  env env: fn(String) -> Option(String),
) -> Result(Nil, #(String, String)) {
  let report =
    build_schedule_doctor_report(workspace_root, explicit_root, job_id, env)
  case json_output {
    True ->
      output_line(schedule_doctor_report_to_json(report) |> json.to_string)
    False -> print_schedule_doctor_report(report, output_line)
  }
  Ok(Nil)
}

pub fn load_projection(
  root: String,
) -> Result(projection.Projection, #(String, String)) {
  schedule_state.load_projection(root, pair_error)
}

pub fn status_or_error(
  projected: projection.Projection,
  job_id: String,
) -> Result(projection.ScheduledJobStatus, #(String, String)) {
  case projection.scheduled_status_for(projected, job_id) {
    Ok(status) -> Ok(status)
    Error(_) -> Error(#("schedule_not_found", "scheduled job not found"))
  }
}

pub fn current_run_or_error(
  status: projection.ScheduledJobStatus,
) -> Result(projection.ScheduledRunSummary, #(String, String)) {
  case status.current_run {
    Some(run) -> Ok(run)
    None -> Error(#("schedule_no_runs", "scheduled job has no runs"))
  }
}

pub fn log_lookup_json(
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> String {
  scheduled_log_lookup_to_json(status, run) |> json.to_string
}

pub fn print_transcript_expired(
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
  line output_line: fn(String) -> Nil,
) -> Nil {
  output_line("job: " <> status.job_id)
  output_line("run_id: " <> run.run_id)
  output_line("session_id: " <> optional_string(run.session_id))
  output_line("run_root: " <> optional_string(run.run_root))
  output_line(
    "logs: latest scheduled session transcript is not available from the local event hub",
  )
}

fn build_schedule_doctor_report(
  workspace_root: Result(String, #(String, String)),
  explicit_root: Option(String),
  job_id: String,
  env: fn(String) -> Option(String),
) -> ScheduleDoctorReport {
  let config_path = schedule_config_path(explicit_root, env)
  let config_diagnostics =
    schedule_state.config_diagnostics(config_path, job_id)
  let projection_diagnostics =
    schedule_projection_diagnostics(workspace_root, job_id)
  ScheduleDoctorReport(
    job_id: job_id,
    config_path: config_path,
    diagnostics: list.append(config_diagnostics, projection_diagnostics),
  )
}

fn schedule_projection_diagnostics(
  workspace_root: Result(String, #(String, String)),
  job_id: String,
) -> List(schedule_doctor.Diagnostic) {
  case workspace_root {
    Error(err) -> [workspace_root_unavailable_diagnostic(job_id, err)]
    Ok(root) ->
      case load_projection(root) {
        Error(err) -> [projection_load_failed_diagnostic(root, job_id, err)]
        Ok(projected) ->
          case projection.scheduled_status_for(projected, job_id) {
            Error(Nil) -> [
              schedule_doctor.Diagnostic(
                name: "local_projection",
                severity: schedule_doctor.Pass,
                code: "ok",
                message: "local ledger projection is readable; no scheduled runs are recorded for this job yet",
                fields: [#("job_id", job_id), #("workspace_root", root)],
              ),
            ]
            Ok(status) -> scheduled_projection_status_diagnostics(root, status)
          }
      }
  }
}

fn workspace_root_unavailable_diagnostic(
  job_id: String,
  err: #(String, String),
) -> schedule_doctor.Diagnostic {
  let #(code, message) = err
  schedule_doctor.Diagnostic(
    name: "local_projection",
    severity: schedule_doctor.Skip,
    code: code,
    message: "local schedule history was not inspected: " <> message,
    fields: [#("job_id", job_id)],
  )
}

fn projection_load_failed_diagnostic(
  root: String,
  job_id: String,
  err: #(String, String),
) -> schedule_doctor.Diagnostic {
  let #(code, message) = err
  schedule_doctor.Diagnostic(
    name: "local_projection",
    severity: schedule_doctor.Warn,
    code: code,
    message: message,
    fields: [#("job_id", job_id), #("workspace_root", root)],
  )
}

fn scheduled_projection_status_diagnostics(
  root: String,
  status: projection.ScheduledJobStatus,
) -> List(schedule_doctor.Diagnostic) {
  let base = [
    schedule_doctor.Diagnostic(
      name: "local_projection",
      severity: schedule_doctor.Pass,
      code: "ok",
      message: "local ledger projection is readable",
      fields: [
        #("job_id", status.job_id),
        #("workspace_root", root),
        #("state", scheduled_state_to_string(status.state)),
      ],
    ),
  ]
  let base = case status.current_run {
    Some(run) -> [
      schedule_doctor.Diagnostic(
        name: "latest_run",
        severity: schedule_doctor.Pass,
        code: "ok",
        message: "latest scheduled run is visible in local history",
        fields: [
          #("job_id", status.job_id),
          #("run_id", run.run_id),
          #("run_status", run.status),
          #("session_id", optional_string(run.session_id)),
          #("run_root", optional_string(run.run_root)),
        ],
      ),
      ..base
    ]
    None -> [
      schedule_doctor.Diagnostic(
        name: "latest_run",
        severity: schedule_doctor.Pass,
        code: "ok",
        message: "no scheduled runs are recorded for this job yet",
        fields: [#("job_id", status.job_id)],
      ),
      ..base
    ]
  }
  let base = case status.failure_issue_id {
    Some(issue_id) -> [
      schedule_doctor.Diagnostic(
        name: "failure_issue",
        severity: schedule_doctor.Pass,
        code: "ok",
        message: "latest scheduled failure report remembered a failure task in Linear",
        fields: [#("job_id", status.job_id), #("linear_issue_id", issue_id)],
      ),
      ..base
    ]
    None -> base
  }
  let base = case status.report_retry {
    Some(retry) -> [
      schedule_doctor.Diagnostic(
        name: "failure_report_retry",
        severity: schedule_doctor.Warn,
        code: retry.error_code,
        message: "failure report retry is pending and will retry without rerunning the workflow",
        fields: [
          #("job_id", status.job_id),
          #("run_id", retry.run_id),
          #("next_retry_at_ms", int.to_string(retry.next_retry_at_ms)),
          #("generation", int.to_string(retry.generation)),
        ],
      ),
      ..base
    ]
    None -> base
  }
  list.reverse(base)
}

fn schedule_config_path(
  explicit_root: Option(String),
  env: fn(String) -> Option(String),
) -> Option(String) {
  schedule_config_candidates(explicit_root, env)
  |> first_existing_file
}

fn schedule_config_candidates(
  explicit_root: Option(String),
  env: fn(String) -> Option(String),
) -> List(String) {
  let caller_config = resolve_path_option("scherzo.yaml", env)
  case explicit_root {
    None -> [caller_config]
    Some(root) -> {
      let root = resolve_path_option(root, env)
      list.append(
        [path.join(root, "scherzo.yaml")],
        list.append(parent_config_candidates(root), [caller_config]),
      )
    }
  }
}

fn parent_config_candidates(root: String) -> List(String) {
  case path.dirname(root) {
    Ok(parent) -> [path.join(parent, "scherzo.yaml")]
    Error(Nil) -> []
  }
}

fn first_existing_file(paths: List(String)) -> Option(String) {
  case paths {
    [] -> None
    [candidate, ..rest] ->
      case is_file(candidate) {
        True -> Some(candidate)
        False -> first_existing_file(rest)
      }
  }
}

fn is_file(candidate: String) -> Bool {
  case simplifile.is_file(candidate) {
    Ok(True) -> True
    _ -> False
  }
}

fn scheduled_log_lookup_to_json(
  status: projection.ScheduledJobStatus,
  run: projection.ScheduledRunSummary,
) -> json.Json {
  json.object([
    #("job_id", json.string(status.job_id)),
    #("run_id", json.string(run.run_id)),
    #("session_id", optional_string_json(run.session_id)),
    #("run_root", optional_string_json(run.run_root)),
    #("status", json.string(run.status)),
  ])
}

fn schedule_doctor_report_to_json(report: ScheduleDoctorReport) -> json.Json {
  json.object([
    #("job_id", json.string(report.job_id)),
    #("config_path", optional_string_json(report.config_path)),
    #(
      "status",
      json.string(
        schedule_doctor.severity_to_string(schedule_doctor.most_severe(
          report.diagnostics,
        )),
      ),
    ),
    #(
      "checks",
      json.array(report.diagnostics, of: schedule_doctor_diagnostic_to_json),
    ),
  ])
}

fn schedule_doctor_diagnostic_to_json(
  diagnostic: schedule_doctor.Diagnostic,
) -> json.Json {
  json.object([
    #("name", json.string(diagnostic.name)),
    #(
      "status",
      json.string(schedule_doctor.severity_to_string(diagnostic.severity)),
    ),
    #("code", json.string(diagnostic.code)),
    #("message", json.string(diagnostic.message)),
    #(
      "fields",
      json.object(
        list.map(diagnostic.fields, fn(field) {
          let #(key, value) = field
          #(key, json.string(value))
        }),
      ),
    ),
  ])
}

fn print_schedule_doctor_report(
  report: ScheduleDoctorReport,
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("schedule doctor: " <> report.job_id)
  output_line("config: " <> optional_string(report.config_path))
  output_line(
    "status: "
    <> schedule_doctor.severity_to_string(schedule_doctor.most_severe(
      report.diagnostics,
    )),
  )
  list.each(report.diagnostics, fn(diagnostic) {
    output_line(
      "- "
      <> schedule_doctor_marker(diagnostic.severity)
      <> " "
      <> diagnostic.name
      <> ": "
      <> diagnostic.message
      <> " ("
      <> diagnostic.code
      <> ")",
    )
    case diagnostic.fields {
      [] -> Nil
      _ ->
        output_line(
          "  fields: "
          <> string.join(
            list.map(diagnostic.fields, fn(field) {
              let #(key, value) = field
              key <> "=" <> value
            }),
            with: " ",
          ),
        )
    }
  })
}

fn schedule_doctor_marker(severity: schedule_doctor.Severity) -> String {
  case severity {
    schedule_doctor.Pass -> "PASS"
    schedule_doctor.Warn -> "WARN"
    schedule_doctor.Fail -> "FAIL"
    schedule_doctor.Skip -> "SKIP"
  }
}

fn print_scheduled_statuses(
  statuses: List(projection.ScheduledJobStatus),
  output_line: fn(String) -> Nil,
) -> Nil {
  case statuses {
    [] -> output_line("No scheduled job history found.")
    _ -> {
      output_line(
        "JOB  WORKFLOW  STATUS  LAST SUCCESS  LAST FAILURE  SKIPPED  RECENT RUNS",
      )
      list.each(statuses, fn(status) {
        output_line(
          status.job_id
          <> "  "
          <> status.workflow_id
          <> "  "
          <> scheduled_state_to_string(status.state)
          <> "  "
          <> optional_ms(status.last_success_at_ms)
          <> "  "
          <> optional_ms(status.last_failure_at_ms)
          <> "  "
          <> int.to_string(scheduled_skipped_total(status))
          <> "  "
          <> string.join(status.recent_run_ids, with: ","),
        )
      })
    }
  }
}

fn print_scheduled_history(
  status: projection.ScheduledJobStatus,
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("job: " <> status.job_id)
  output_line("workflow: " <> status.workflow_id)
  output_line("status: " <> scheduled_state_to_string(status.state))
  output_line("last_due_at: " <> optional_ms(status.last_due_at_ms))
  output_line("last_success_at: " <> optional_ms(status.last_success_at_ms))
  output_line(
    "last_success_run_id: " <> optional_string(status.last_success_run_id),
  )
  output_line("last_failure_at: " <> optional_ms(status.last_failure_at_ms))
  output_line(
    "last_failure_run_id: " <> optional_string(status.last_failure_run_id),
  )
  output_line(
    "last_failure_reason: " <> optional_string(status.last_failure_reason),
  )
  output_line(
    "skipped_overlap_count: " <> int.to_string(status.skipped_overlap_count),
  )
  output_line(
    "skipped_catch_up_count: " <> int.to_string(status.skipped_catch_up_count),
  )
  output_line(
    "skipped_paused_count: " <> int.to_string(status.skipped_paused_count),
  )
  output_line(
    "skipped_capacity_count: " <> int.to_string(status.skipped_capacity_count),
  )
  output_line("failure_issue_id: " <> optional_string(status.failure_issue_id))
  output_line(
    "failure_dedupe_key: " <> optional_string(status.failure_dedupe_key),
  )
  case status.report_retry {
    None -> output_line("report_retry: -")
    Some(report_retry) -> {
      output_line("report_retry: " <> report_retry.run_id)
      output_line("report_retry_error: " <> report_retry.error_code)
      output_line(
        "report_retry_next_retry_at_ms: "
        <> int.to_string(report_retry.next_retry_at_ms),
      )
    }
  }
  output_line(
    "recent_run_ids: " <> string.join(status.recent_run_ids, with: ","),
  )
  case status.current_run {
    None -> output_line("current_run: -")
    Some(run) -> {
      output_line("current_run: " <> run.run_id)
      output_line("current_run_status: " <> run.status)
      output_line("current_run_trigger: " <> run.trigger)
      output_line("current_run_due_at: " <> int.to_string(run.due_at_ms))
      output_line("current_run_attempt: " <> int.to_string(run.attempt))
      output_line("current_run_reason: " <> optional_string(run.reason))
      output_line("current_run_session_id: " <> optional_string(run.session_id))
      output_line("current_run_root: " <> optional_string(run.run_root))
    }
  }
}

fn scheduled_status_to_json(
  status: projection.ScheduledJobStatus,
) -> json.Json {
  json.object([
    #("job_id", json.string(status.job_id)),
    #("workflow_id", json.string(status.workflow_id)),
    #("state", json.string(scheduled_state_to_string(status.state))),
    #("current_run", scheduled_run_to_json(status.current_run)),
    #("last_due_at_ms", optional_int_json(status.last_due_at_ms)),
    #("last_success_at_ms", optional_int_json(status.last_success_at_ms)),
    #("last_success_run_id", optional_string_json(status.last_success_run_id)),
    #("last_failure_at_ms", optional_int_json(status.last_failure_at_ms)),
    #("last_failure_run_id", optional_string_json(status.last_failure_run_id)),
    #("last_failure_reason", optional_string_json(status.last_failure_reason)),
    #("retry_count", json.int(status.retry_count)),
    #("skipped_overlap_count", json.int(status.skipped_overlap_count)),
    #("skipped_catch_up_count", json.int(status.skipped_catch_up_count)),
    #("skipped_paused_count", json.int(status.skipped_paused_count)),
    #("skipped_capacity_count", json.int(status.skipped_capacity_count)),
    #("failure_issue_id", optional_string_json(status.failure_issue_id)),
    #("failure_dedupe_key", optional_string_json(status.failure_dedupe_key)),
    #("report_retry", scheduled_report_retry_to_json(status.report_retry)),
    #("recent_run_ids", json.array(status.recent_run_ids, of: json.string)),
  ])
}

fn scheduled_report_retry_to_json(
  retry: Option(projection.ScheduledReportRetry),
) -> json.Json {
  case retry {
    None -> json.null()
    Some(retry) ->
      json.object([
        #("run_id", json.string(retry.run_id)),
        #("attempt", json.int(retry.attempt)),
        #("dedupe_key", json.string(retry.dedupe_key)),
        #("error_code", json.string(retry.error_code)),
        #("error_message", json.string(retry.error_message)),
        #("next_retry_at_ms", json.int(retry.next_retry_at_ms)),
        #("generation", json.int(retry.generation)),
      ])
  }
}

fn scheduled_run_to_json(
  run: Option(projection.ScheduledRunSummary),
) -> json.Json {
  case run {
    None -> json.null()
    Some(run) ->
      json.object([
        #("run_id", json.string(run.run_id)),
        #("due_at_ms", json.int(run.due_at_ms)),
        #("trigger", json.string(run.trigger)),
        #("attempt", json.int(run.attempt)),
        #("status", json.string(run.status)),
        #("reason", optional_string_json(run.reason)),
        #("session_id", optional_string_json(run.session_id)),
        #("run_root", optional_string_json(run.run_root)),
      ])
  }
}

fn scheduled_skipped_total(status: projection.ScheduledJobStatus) -> Int {
  status.skipped_overlap_count
  + status.skipped_catch_up_count
  + status.skipped_paused_count
  + status.skipped_capacity_count
}

fn scheduled_state_to_string(state: projection.ScheduledRunState) -> String {
  case state {
    projection.ScheduledIdle -> "idle"
    projection.ScheduledDuePending -> "due_pending"
    projection.ScheduledPaused -> "paused"
    projection.ScheduledWaitingForGlobalSlot -> "waiting_for_global_slot"
    projection.ScheduledActive -> "active"
    projection.ScheduledRetryWaiting -> "retry_waiting"
    projection.ScheduledReportRetryWaiting -> "report_retry_waiting"
    projection.ScheduledTerminalSuccess -> "terminal_success"
    projection.ScheduledTerminalFailure -> "terminal_failure"
  }
}

fn optional_ms(value: Option(Int)) -> String {
  case value {
    Some(ms) -> int.to_string(ms)
    None -> "-"
  }
}

fn optional_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "-"
  }
}

fn optional_int_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn optional_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn resolve_path_option(
  value: String,
  env: fn(String) -> Option(String),
) -> String {
  path.resolve_from_caller_cwd(value, env)
}

fn pair_error(code: String, message: String) -> #(String, String) {
  #(code, message)
}
