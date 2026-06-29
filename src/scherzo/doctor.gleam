import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/linear_task_scope
import scherzo/config/linear_task_scope_diagnostics
import scherzo/config/types as config_types
import scherzo/log

pub type CheckName {
  WorkflowConfig
  LinearTaskScope
  ScheduledJobs
  LinearContract
  LinearSmoke
  InstanceLock
  WorkspaceHooks
  PiProbe
}

pub type CheckStatus {
  Pass
  Warn
  Fail
  Skip
}

pub type CheckResult {
  CheckResult(
    check: CheckName,
    status: CheckStatus,
    code: String,
    message: String,
    fields: List(#(String, String)),
  )
}

pub type Report {
  Report(results: List(CheckResult))
}

pub type Summary {
  Summary(passed: Int, warned: Int, failed: Int, skipped: Int)
}

pub type OutputFormat {
  Human
  Logfmt
}

pub type Options {
  Options(
    path: Option(String),
    checks: List(String),
    list_checks: Bool,
    output: OutputFormat,
  )
}

pub fn default_checks() -> List(CheckName) {
  [
    WorkflowConfig,
    LinearTaskScope,
    ScheduledJobs,
    LinearContract,
    LinearSmoke,
    InstanceLock,
    WorkspaceHooks,
    PiProbe,
  ]
}

pub fn list_check_names() -> List(String) {
  default_checks()
  |> list.map(check_name_to_string)
}

pub fn check_name_to_string(check: CheckName) -> String {
  case check {
    WorkflowConfig -> "workflow-config"
    LinearTaskScope -> "tracker-scope"
    ScheduledJobs -> "scheduled-jobs"
    LinearContract -> "tracker-contract"
    LinearSmoke -> "tracker-smoke"
    InstanceLock -> "instance-lock"
    WorkspaceHooks -> "workspace-hooks"
    PiProbe -> "pi-probe"
  }
}

pub fn parse_check_name(name: String) -> Result(CheckName, String) {
  case name {
    "workflow-config" -> Ok(WorkflowConfig)
    "tracker-scope" -> Ok(LinearTaskScope)
    "scheduled-jobs" -> Ok(ScheduledJobs)
    "tracker-contract" -> Ok(LinearContract)
    "tracker-smoke" -> Ok(LinearSmoke)
    "instance-lock" -> Ok(InstanceLock)
    "workspace-hooks" -> Ok(WorkspaceHooks)
    "pi-probe" -> Ok(PiProbe)
    _ -> Error(name)
  }
}

pub fn selected_checks(raw: List(String)) -> Result(List(CheckName), String) {
  case raw {
    [] -> Ok(default_checks())
    _ -> parse_selected_checks(raw, [])
  }
}

pub fn canonical_checks(checks: List(CheckName)) -> List(CheckName) {
  default_checks()
  |> list.filter(fn(check) { contains_check(checks, check) })
}

pub fn summary(report: Report) -> Summary {
  list.fold(report.results, Summary(0, 0, 0, 0), fn(acc, result) {
    case result.status {
      Pass -> Summary(..acc, passed: acc.passed + 1)
      Warn -> Summary(..acc, warned: acc.warned + 1)
      Fail -> Summary(..acc, failed: acc.failed + 1)
      Skip -> Summary(..acc, skipped: acc.skipped + 1)
    }
  })
}

pub fn has_failures(report: Report) -> Bool {
  let counts = summary(report)
  counts.failed > 0
}

pub fn skip_after_workflow_failure(
  checks: List(CheckName),
  results: List(CheckResult),
) -> List(CheckResult) {
  case checks {
    [] -> results
    [WorkflowConfig, ..rest] -> skip_after_workflow_failure(rest, results)
    [check, ..rest] ->
      skip_after_workflow_failure(
        rest,
        list.append(results, [
          CheckResult(
            check: check,
            status: Skip,
            code: "workflow_config_failed",
            message: "workflow config did not load",
            fields: [],
          ),
        ]),
      )
  }
}

pub fn linear_task_scope_check_result(
  tracker: config_types.TrackerConfig,
  config_contents: String,
) -> CheckResult {
  case config_types.linear_task_scope_from_tracker_config(tracker) {
    Error(err) ->
      CheckResult(
        check: LinearTaskScope,
        status: Fail,
        code: "missing_tracker_project_slug",
        message: config_types.linear_task_scope_error_message(err),
        fields: [],
      )
    Ok(scope) -> {
      let source =
        linear_task_scope_diagnostics.source_from_yaml(config_contents)
      let warnings = linear_task_scope_diagnostics.overlap_warnings(scope)
      CheckResult(
        check: LinearTaskScope,
        status: task_scope_status(warnings),
        code: task_scope_code(warnings),
        message: linear_task_scope_diagnostics.message(scope, source, warnings),
        fields: linear_task_scope_fields(scope, source, warnings),
      )
    }
  }
}

pub fn result_event(result: CheckResult) -> String {
  case result.status {
    Pass -> "doctor_check_pass"
    Warn -> "doctor_check_warn"
    Fail -> "doctor_check_fail"
    Skip -> "doctor_check_skip"
  }
}

fn task_scope_status(warnings: List(String)) -> CheckStatus {
  case warnings {
    [] -> Pass
    _ -> Warn
  }
}

fn task_scope_code(warnings: List(String)) -> String {
  case warnings {
    [] -> "ok"
    _ -> "linear_task_scope_overlap"
  }
}

fn linear_task_scope_fields(
  scope: config_types.LinearTaskScope,
  source: linear_task_scope_diagnostics.Source,
  warnings: List(String),
) -> List(log.Field) {
  let base = [
    #("task_scope_summary", linear_task_scope.summary(scope)),
    #("task_scope_source", linear_task_scope_diagnostics.source_field(source)),
    #("overlap_warning_count", int.to_string(list.length(warnings))),
  ]
  let base = case linear_task_scope_diagnostics.legacy_path(source) {
    Some(path) -> list.append(base, [#("legacy_task_scope_path", path)])
    None -> base
  }
  list.append(base, overlap_warning_fields(warnings, 1))
}

fn overlap_warning_fields(
  warnings: List(String),
  index: Int,
) -> List(log.Field) {
  case warnings {
    [] -> []
    [warning, ..rest] -> {
      let name = "overlap_warning_" <> int.to_string(index)
      let fields = case index == 1 {
        True -> [#("first_overlap_warning", warning), #(name, warning)]
        False -> [#(name, warning)]
      }
      list.append(fields, overlap_warning_fields(rest, index + 1))
    }
  }
}

pub fn result_log_fields(result: CheckResult) -> List(log.Field) {
  [
    #("check", check_name_to_string(result.check)),
    #("code", result.code),
    #("message", result.message),
    ..result.fields
  ]
}

pub fn summary_log_fields(summary: Summary) -> List(log.Field) {
  [
    #("passed", int.to_string(summary.passed)),
    #("warned", int.to_string(summary.warned)),
    #("failed", int.to_string(summary.failed)),
    #("skipped", int.to_string(summary.skipped)),
  ]
}

pub fn human_report(report: Report, requested_path: Option(String)) -> String {
  let body =
    report.results
    |> list.map(human_result)
    |> string.join(with: "\n\n")
  string.join(
    [
      "Scherzo doctor",
      "Config: " <> report_config_path(report, requested_path),
      "",
      body,
      "",
      human_summary(summary(report)),
      "",
      human_conclusion(report),
    ],
    with: "\n",
  )
}

pub fn contains_check(checks: List(CheckName), wanted: CheckName) -> Bool {
  case checks {
    [] -> False
    [check, ..rest] -> check == wanted || contains_check(rest, wanted)
  }
}

fn human_result(result: CheckResult) -> String {
  string.join(
    [
      status_marker(result.status) <> " " <> check_title(result.check),
      human_result_body(result),
    ],
    with: "\n",
  )
}

fn human_result_body(result: CheckResult) -> String {
  case result.status {
    Pass -> human_pass_body(result)
    Warn -> human_problem_body(result, "Warning")
    Fail -> human_problem_body(result, "Problem")
    Skip -> human_skip_body(result)
  }
}

fn human_pass_body(result: CheckResult) -> String {
  case result.check {
    WorkflowConfig ->
      indent([
        "Loaded YAML orchestrator config and "
        <> field_or(result.fields, "workflow_count", "?")
        <> " workflow "
        <> plural(field_or(result.fields, "workflow_count", ""), "DAG", "DAGs")
        <> ".",
      ])
    LinearTaskScope -> indent(linear_task_scope_pass_lines(result))
    ScheduledJobs ->
      indent([
        "Scheduled job configuration is valid for the fixed-interval MVP.",
        "Jobs: "
          <> field_or(result.fields, "scheduled_job_count", "0")
          <> ", enabled: "
          <> field_or(result.fields, "enabled_job_count", "0")
          <> ".",
      ])
    LinearContract ->
      indent([
        "Tracker contract matches configured states and labels.",
        "Team count: "
          <> field_or(result.fields, "team_count", "?")
          <> ", states: "
          <> field_or(result.fields, "state_count", "?")
          <> ", labels: "
          <> field_or(result.fields, "label_count", "?")
          <> ".",
      ])
    LinearSmoke ->
      indent([
        "Read-only tracker API check succeeded.",
        "Candidates: "
          <> field_or(result.fields, "candidate_count", "?")
          <> ", terminal sample: "
          <> field_or(result.fields, "terminal_count", "?")
          <> ", refreshed: "
          <> field_or(result.fields, "refreshed_count", "?")
          <> ".",
      ])
    InstanceLock ->
      indent(["Local instance lock can be acquired and released."])
    WorkspaceHooks ->
      indent([
        "Scratch workspace was prepared and cleaned up with the default workspace profile.",
        "Profile: " <> field_or(result.fields, "workspace_profile", "?") <> ".",
      ])
    PiProbe -> indent(["pi RPC launched successfully and no prompt was sent."])
  }
}

fn human_problem_body(result: CheckResult, heading: String) -> String {
  indent([
    heading <> ": " <> result.message,
    "",
    "Code: " <> result.code,
    "Impact: " <> impact(result.check),
    "Try:",
    ..list.map(remediation(result.check, result.code), fn(line) { "  " <> line })
  ])
}

fn linear_task_scope_pass_lines(result: CheckResult) -> List(String) {
  let summary = field_or(result.fields, "task_scope_summary", "?")
  let lines = ["Linear task scope: " <> summary <> "."]
  case field_value(result.fields, "legacy_task_scope_path") {
    Some(path) ->
      list.append(lines, [
        "Legacy "
        <> path
        <> " desugars to tracker.linear.tasks_from: "
        <> summary
        <> ".",
      ])
    None -> lines
  }
}

fn human_skip_body(result: CheckResult) -> String {
  indent([
    "Skipped: " <> result.message,
    "Code: " <> result.code,
  ])
}

fn human_summary(counts: Summary) -> String {
  "Summary: "
  <> int.to_string(counts.passed)
  <> " passed, "
  <> int.to_string(counts.warned)
  <> " warnings, "
  <> int.to_string(counts.failed)
  <> " failed, "
  <> int.to_string(counts.skipped)
  <> " skipped"
}

fn human_conclusion(report: Report) -> String {
  let counts = summary(report)
  case counts.failed > 0 {
    True -> "Not ready."
    False ->
      case counts.skipped > 0 {
        True -> "Selected checks completed with skips."
        False ->
          case counts.warned > 0 {
            True -> "Ready with warnings."
            False ->
              case report_has_all_default_checks(report) {
                True -> "Ready for cautious real-board operation."
                False -> "Selected checks passed."
              }
          }
      }
  }
}

fn report_config_path(
  report: Report,
  requested_path: Option(String),
) -> String {
  case find_result(report.results, WorkflowConfig) {
    Some(result) ->
      field_or(result.fields, "config_path", option_path(requested_path))
    None -> option_path(requested_path)
  }
}

fn option_path(path: Option(String)) -> String {
  case path {
    Some(path) -> path
    None -> "auto"
  }
}

fn report_has_all_default_checks(report: Report) -> Bool {
  let Report(results) = report
  results_contain_all(default_checks(), results)
}

fn results_contain_all(
  checks: List(CheckName),
  results: List(CheckResult),
) -> Bool {
  case checks {
    [] -> True
    [check, ..rest] ->
      result_list_contains(results, check) && results_contain_all(rest, results)
  }
}

fn result_list_contains(results: List(CheckResult), check: CheckName) -> Bool {
  case results {
    [] -> False
    [result, ..rest] ->
      result.check == check || result_list_contains(rest, check)
  }
}

fn find_result(
  results: List(CheckResult),
  check: CheckName,
) -> Option(CheckResult) {
  case results {
    [] -> None
    [result, ..rest] ->
      case result.check == check {
        True -> Some(result)
        False -> find_result(rest, check)
      }
  }
}

fn status_marker(status: CheckStatus) -> String {
  case status {
    Pass -> "✓"
    Warn -> "!"
    Fail -> "✗"
    Skip -> "-"
  }
}

fn check_title(check: CheckName) -> String {
  case check {
    WorkflowConfig -> "Workflow config"
    LinearTaskScope -> "Tracker task scope"
    ScheduledJobs -> "Scheduled jobs"
    LinearContract -> "Tracker contract"
    LinearSmoke -> "Tracker smoke"
    InstanceLock -> "Instance lock"
    WorkspaceHooks -> "Workspace driver"
    PiProbe -> "Pi probe"
  }
}

fn impact(check: CheckName) -> String {
  case check {
    WorkflowConfig ->
      "Scherzo cannot safely start because config, workflow DAGs, or prompt templates did not load."
    LinearTaskScope ->
      "Another Scherzo daemon may claim the same Linear task if configured Linear task scopes overlap."
    ScheduledJobs ->
      "Scheduled jobs may fail before dispatch, create noisy failure tasks in Linear, or reference source-task variables (`issue.*`) that do not exist for scheduled runs."
    LinearContract ->
      "Configured tracker states or labels may not match the target board."
    LinearSmoke ->
      "Scherzo may not be able to read candidate tasks from the tracker."
    InstanceLock ->
      "Another local Scherzo process may be active, or a stale lock may need operator cleanup."
    WorkspaceHooks ->
      "The default workspace driver cannot safely prepare and clean up a scratch workspace."
    PiProbe ->
      "Scherzo may not be able to launch pi RPC in prepared workspaces."
  }
}

fn remediation(check: CheckName, code: String) -> List(String) {
  case check {
    WorkflowConfig -> [
      "- Confirm the YAML path is correct and ends in .yaml or .yml.",
      "- Confirm LINEAR_API_KEY and any referenced environment variables are set.",
      "- Confirm routed workflow DAG and prompt-template files exist.",
    ]
    LinearTaskScope -> [
      "- Prefer tracker.linear.tasks_from.project or tracker.linear.tasks_from.projects for new Linear task scopes.",
      "- Run only one daemon per non-overlapping Linear task scope/root.",
      "- Compare the canonical Linear task-scope summary with other running daemon configs.",
      "- See docs/specs/TRACKER_LINEAR_TASKS_FROM.md for supported task-scope predicates.",
    ]
    ScheduledJobs -> [
      "- Confirm schedules entries reference existing workflows and use every: <n><ms|s|m|h> with at least 1000ms.",
      "- Keep schedule-level input, vars, payload, catch_up: true, and non-skip overlap modes out of the MVP config.",
      "- Replace issue.* references in scheduled workflows with scheduled_job.*, schedule.*, or run.* variables.",
      "- When schedules[].on_failure.task.enabled is true, configure a Linear triage state and let Scherzo ensure reserved scheduled-job dedupe labels.",
    ]
    LinearContract -> [
      "- Confirm tracker.linear.tasks_from (or compatibility tracker.linear.project / tracker.linear.project_slug / tracker.project_slug) points to the expected Linear project scope.",
      "- Confirm configured active, terminal, required, and handoff states exist on the board.",
      "- Run: gleam run -- --tracker-contract-check <path-to-scherzo.yaml>",
    ]
    LinearSmoke -> [
      "- Confirm LINEAR_API_KEY is valid and can read the Linear project scope.",
      "- Confirm tracker.linear.tasks_from (or compatibility tracker.linear.project / tracker.linear.project_slug / tracker.project_slug) points to the expected Linear project scope.",
      "- Run: gleam run -- --tracker-smoke <path-to-scherzo.yaml>",
    ]
    InstanceLock -> [
      "- Stop any other Scherzo process using this workspace root.",
      "- If no process is active, remove the stale instance.lock file manually.",
    ]
    WorkspaceHooks -> [
      "- Ensure workspace.driver names a built-in driver or an entry under workspace.drivers.",
      "- If workflow-config reports workspace.hooks or workspace.drivers.<name>.hooks, remove that unsupported block or implement it in a type: custom driver command.",
      "- Read docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md for workspace driver examples.",
    ]
    PiProbe -> [
      "- Confirm pi is installed and the configured pi.command supports --mode rpc.",
      "- Run: gleam run -- doctor --check pi-probe <path-to-scherzo.yaml>",
    ]
  }
  |> append_code_specific_remediation(code)
}

fn append_code_specific_remediation(
  lines: List(String),
  code: String,
) -> List(String) {
  case code {
    "missing_tracker_api_key" ->
      list.append(lines, [
        "- Set LINEAR_API_KEY, tracker.credentials.api_key_env, or tracker.api_key before rerunning doctor.",
      ])
    "missing_tracker_project_slug" ->
      list.append(lines, [
        "- Set tracker.linear.tasks_from.project, tracker.linear.tasks_from.projects, or compatibility tracker.linear.project / tracker.linear.project_slug / tracker.project_slug before rerunning doctor.",
      ])
    _ -> lines
  }
}

fn field_or(
  fields: List(#(String, String)),
  key: String,
  default: String,
) -> String {
  case field_value(fields, key) {
    Some(value) -> value
    None -> default
  }
}

fn field_value(fields: List(#(String, String)), key: String) -> Option(String) {
  case fields {
    [] -> None
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> Some(value)
        False -> field_value(rest, key)
      }
  }
}

fn indent(lines: List(String)) -> String {
  lines
  |> list.map(fn(line) {
    case line == "" {
      True -> ""
      False -> "  " <> line
    }
  })
  |> string.join(with: "\n")
}

fn plural(value: String, singular: String, plural: String) -> String {
  case value == "1" {
    True -> singular
    False -> plural
  }
}

fn parse_selected_checks(
  raw: List(String),
  acc: List(CheckName),
) -> Result(List(CheckName), String) {
  case raw {
    [] -> Ok(list.reverse(acc))
    [name, ..rest] -> {
      use check <- result.try(parse_check_name(name))
      case contains_check(acc, check) {
        True -> parse_selected_checks(rest, acc)
        False -> parse_selected_checks(rest, [check, ..acc])
      }
    }
  }
}
