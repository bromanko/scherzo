import glance
import gleam/dict
import gleam/int
import gleam/list
import gleam/order.{type Order, Eq}
import gleam/string
import glinter/rule as lint_rule
import glinter/rules/discarded_result
import glinter/rules/error_context_lost
import glinter/rules/stringly_typed_error
import glinter/rules/thrown_away_error
import glinter/rules/unwrap_used
import glinter/runner
import glinter/source as glinter_source

pub const discarded_result_rule = "discarded_result"

pub const error_context_lost_rule = "error_context_lost"

pub const stringly_typed_error_rule = "stringly_typed_error"

pub const thrown_away_error_rule = "thrown_away_error"

pub const unwrap_used_rule = "unwrap_used"

pub type Finding {
  Finding(
    rule: String,
    path: String,
    module_name: String,
    subsystem: String,
    line: Int,
    message: String,
  )
}

pub type CountRow {
  CountRow(
    group: String,
    total: Int,
    discarded_result: Int,
    error_context_lost: Int,
    stringly_typed_error: Int,
    thrown_away_error: Int,
    unwrap_used: Int,
  )
}

pub type ModuleCountRow {
  ModuleCountRow(subsystem: String, module_name: String, counts: CountRow)
}

pub type InventoryError {
  ParseError(path: String, message: String)
}

type TrackedRule {
  TrackedRule(name: String, rule: lint_rule.Rule)
}

pub fn findings_for_source(
  path path: String,
  source source: String,
) -> Result(List(Finding), InventoryError) {
  case glance.module(source) {
    Error(error) -> Error(ParseError(path, describe_parse_error(error)))
    Ok(module_) ->
      tracked_rules()
      |> list.flat_map(fn(tracked) {
        findings_for_rule(tracked, module_, source, path)
      })
      |> runner.filter_annotations(source, module_)
      |> list.map(fn(result) { finding_from_lint_result(result, source) })
      |> sort_findings
      |> Ok
  }
}

pub fn sort_findings(findings: List(Finding)) -> List(Finding) {
  findings
  |> list.sort(by: fn(left, right) {
    compare_strings(finding_sort_key(left), finding_sort_key(right))
  })
}

pub fn subsystem_count_rows(findings: List(Finding)) -> List(CountRow) {
  all_subsystems()
  |> list.map(fn(group) {
    count_row(group, findings_for_group(findings, group))
  })
}

pub fn all_subsystems() -> List(String) {
  [
    "Agent / pi execution",
    "Orchestrator / daemon / transition / effect runner",
    "Workflow execution",
    "State ledger / projection / recovery / artifacts",
    "Tracker / Linear / control boundaries",
    "Config / parsing / operator CLI",
    "Workspace / workspace drivers",
    "Artifact publication / repository",
    "Workstream",
    "Top-level utilities / other",
  ]
}

pub fn module_count_rows(findings: List(Finding)) -> List(ModuleCountRow) {
  let keys =
    findings
    |> list.fold(from: dict.new(), with: fn(keys, finding) {
      dict.insert(keys, module_count_key(finding), #(
        finding.subsystem,
        finding.module_name,
      ))
    })
    |> dict.to_list
    |> list.map(fn(row) { row.1 })
    |> list.sort(by: fn(left, right) {
      compare_strings(left.0 <> "|" <> left.1, right.0 <> "|" <> right.1)
    })

  keys
  |> list.map(fn(key) {
    let #(subsystem, module_name) = key
    let module_findings =
      findings
      |> list.filter(keeping: fn(finding) {
        finding.subsystem == subsystem && finding.module_name == module_name
      })

    ModuleCountRow(
      subsystem: subsystem,
      module_name: module_name,
      counts: count_row(module_name, module_findings),
    )
  })
}

pub fn rule_count_rows(findings: List(Finding)) -> List(CountRow) {
  tracked_rule_names()
  |> list.map(fn(rule_name) {
    count_row(rule_name, findings_for_rule_name(findings, rule_name))
  })
}

pub fn total_count_row(findings: List(Finding)) -> CountRow {
  count_row("All tracked high-signal rules", findings)
}

pub fn tracked_rule_names() -> List(String) {
  [
    discarded_result_rule,
    error_context_lost_rule,
    stringly_typed_error_rule,
    thrown_away_error_rule,
    unwrap_used_rule,
  ]
}

pub fn subsystem_for_path(path: String) -> String {
  case is_agent_pi_path(path) {
    True -> "Agent / pi execution"
    False -> subsystem_after_agent_pi(path)
  }
}

pub fn is_agent_pi_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/agent/")
  || string.starts_with(path, "src/scherzo/pi/")
}

pub fn module_name_for_path(path: String) -> String {
  let without_src = case string.starts_with(path, "src/") {
    True -> string.drop_start(path, 4)
    False -> path
  }

  let without_extension = case string.ends_with(without_src, ".gleam") {
    True -> string.drop_end(without_src, 6)
    False -> without_src
  }

  string.replace(without_extension, each: "/", with: ".")
}

fn tracked_rules() -> List(TrackedRule) {
  [
    TrackedRule(discarded_result_rule, discarded_result.rule()),
    TrackedRule(error_context_lost_rule, error_context_lost.rule()),
    TrackedRule(stringly_typed_error_rule, stringly_typed_error.rule()),
    TrackedRule(thrown_away_error_rule, thrown_away_error.rule()),
    TrackedRule(unwrap_used_rule, unwrap_used.rule()),
  ]
}

fn findings_for_rule(
  tracked: TrackedRule,
  module_: glance.Module,
  source: String,
  path: String,
) -> List(lint_rule.LintResult) {
  lint_rule.run_on_module(rule: tracked.rule, module: module_, source: source)
  |> list.map(fn(error) {
    lint_rule.LintResult(
      rule: tracked.name,
      severity: lint_rule.default_severity(tracked.rule),
      file: path,
      location: lint_rule.error_location(error),
      message: lint_rule.error_message(error),
      details: lint_rule.error_details(error),
    )
  })
}

fn finding_from_lint_result(
  result: lint_rule.LintResult,
  source: String,
) -> Finding {
  Finding(
    rule: result.rule,
    path: result.file,
    module_name: module_name_for_path(result.file),
    subsystem: subsystem_for_path(result.file),
    line: glinter_source.byte_offset_to_line(source, result.location.start),
    message: result.message,
  )
}

fn count_row(group: String, findings: List(Finding)) -> CountRow {
  CountRow(
    group: group,
    total: list.length(findings),
    discarded_result: count_rule(findings, discarded_result_rule),
    error_context_lost: count_rule(findings, error_context_lost_rule),
    stringly_typed_error: count_rule(findings, stringly_typed_error_rule),
    thrown_away_error: count_rule(findings, thrown_away_error_rule),
    unwrap_used: count_rule(findings, unwrap_used_rule),
  )
}

fn count_rule(findings: List(Finding), rule_name: String) -> Int {
  findings
  |> list.filter(keeping: fn(finding) { finding.rule == rule_name })
  |> list.length
}

fn findings_for_group(findings: List(Finding), group: String) -> List(Finding) {
  findings
  |> list.filter(keeping: fn(finding) { finding.subsystem == group })
}

fn findings_for_rule_name(
  findings: List(Finding),
  rule_name: String,
) -> List(Finding) {
  findings
  |> list.filter(keeping: fn(finding) { finding.rule == rule_name })
}

fn module_count_key(finding: Finding) -> String {
  finding.subsystem <> "|" <> finding.module_name
}

fn finding_sort_key(finding: Finding) -> String {
  finding.subsystem
  <> "|"
  <> finding.module_name
  <> "|"
  <> finding.path
  <> "|"
  <> int.to_string(finding.line)
  <> "|"
  <> finding.rule
  <> "|"
  <> finding.message
}

fn subsystem_after_agent_pi(path: String) -> String {
  case is_orchestrator_path(path) {
    True -> "Orchestrator / daemon / transition / effect runner"
    False -> subsystem_after_orchestrator(path)
  }
}

fn subsystem_after_orchestrator(path: String) -> String {
  case is_state_path(path) {
    True -> "State ledger / projection / recovery / artifacts"
    False -> subsystem_after_state(path)
  }
}

fn subsystem_after_state(path: String) -> String {
  case is_workspace_path(path) {
    True -> "Workspace / workspace drivers"
    False -> subsystem_after_workspace(path)
  }
}

fn subsystem_after_workspace(path: String) -> String {
  case is_tracker_control_path(path) {
    True -> "Tracker / Linear / control boundaries"
    False -> subsystem_after_tracker_control(path)
  }
}

fn subsystem_after_tracker_control(path: String) -> String {
  case is_config_path(path) {
    True -> "Config / parsing / operator CLI"
    False -> subsystem_after_config(path)
  }
}

fn subsystem_after_config(path: String) -> String {
  case is_artifact_path(path) {
    True -> "Artifact publication / repository"
    False -> subsystem_after_artifact(path)
  }
}

fn subsystem_after_artifact(path: String) -> String {
  case is_workstream_path(path) {
    True -> "Workstream"
    False -> subsystem_after_workstream(path)
  }
}

fn subsystem_after_workstream(path: String) -> String {
  case is_workflow_path(path) {
    True -> "Workflow execution"
    False -> "Top-level utilities / other"
  }
}

fn is_orchestrator_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/orchestrator/")
  || path == "src/scherzo/session/hub.gleam"
  || path == "src/scherzo/hooks.gleam"
  || path == "src/scherzo/instance_lock.gleam"
  || path == "src/scherzo/signal.gleam"
}

fn is_state_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/state/")
  || path == "src/scherzo/step_artifact.gleam"
  || path == "src/scherzo/handoff_format.gleam"
}

fn is_workspace_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/workspace")
}

fn is_tracker_control_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/control/")
  || string.starts_with(path, "src/scherzo/ctl/")
  || string.starts_with(path, "src/scherzo/tracker/")
  || string.starts_with(path, "src/scherzo/linear")
  || string.starts_with(path, "src/scherzo_linear")
  || string.starts_with(path, "src/scherzo_tracker")
  || path == "src/scherzo/ctl.gleam"
  || path == "src/scherzo/port.gleam"
  || path == "src/scherzo/task.gleam"
}

fn is_config_path(path: String) -> Bool {
  path == "src/scherzo/config.gleam"
  || string.starts_with(path, "src/scherzo/config/")
  || path == "src/scherzo/doctor.gleam"
  || path == "src/scherzo/model_config.gleam"
  || path == "src/scherzo/review_lane_preflight_policy.gleam"
  || path == "src/scherzo/schedule_doctor.gleam"
  || string.starts_with(path, "src/scherzo/terminal/")
  || path == "src/scherzo/version.gleam"
}

fn is_artifact_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/artifact_publication")
  || string.starts_with(path, "src/scherzo/artifact_repository/")
  || path == "src/scherzo/result_artifact.gleam"
  || path == "src/scherzo/workflow_artifact_descriptor.gleam"
}

fn is_workstream_path(path: String) -> Bool {
  string.starts_with(path, "src/scherzo/workstream/")
}

fn is_workflow_path(path: String) -> Bool {
  path == "src/scherzo/command_step.gleam"
  || path == "src/scherzo/local_workflow_run.gleam"
  || path == "src/scherzo/runtime_bundle.gleam"
  || path == "src/scherzo/template.gleam"
  || string.starts_with(path, "src/scherzo/workflow_")
}

fn compare_strings(left: String, right: String) -> Order {
  case string.compare(left, right) {
    Eq -> Eq
    order -> order
  }
}

fn describe_parse_error(error: glance.Error) -> String {
  case error {
    glance.UnexpectedEndOfInput -> "unexpected end of input"
    glance.UnexpectedToken(..) -> "unexpected token"
  }
}
