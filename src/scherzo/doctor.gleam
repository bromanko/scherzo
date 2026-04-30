import gleam/int
import gleam/list
import gleam/option
import scherzo/log

pub type CheckName {
  WorkflowConfig
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

pub type Options {
  Options(path: option.Option(String), checks: List(String), list_checks: Bool)
}

pub fn default_checks() -> List(CheckName) {
  [
    WorkflowConfig,
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
    LinearContract -> "linear-contract"
    LinearSmoke -> "linear-smoke"
    InstanceLock -> "instance-lock"
    WorkspaceHooks -> "workspace-hooks"
    PiProbe -> "pi-probe"
  }
}

pub fn parse_check_name(name: String) -> Result(CheckName, String) {
  case name {
    "workflow-config" -> Ok(WorkflowConfig)
    "linear-contract" -> Ok(LinearContract)
    "linear-smoke" -> Ok(LinearSmoke)
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

pub fn result_event(result: CheckResult) -> String {
  case result.status {
    Pass -> "doctor_check_pass"
    Warn -> "doctor_check_warn"
    Fail -> "doctor_check_fail"
    Skip -> "doctor_check_skip"
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

pub fn contains_check(checks: List(CheckName), wanted: CheckName) -> Bool {
  case checks {
    [] -> False
    [check, ..rest] -> check == wanted || contains_check(rest, wanted)
  }
}

fn parse_selected_checks(
  raw: List(String),
  acc: List(CheckName),
) -> Result(List(CheckName), String) {
  case raw {
    [] -> Ok(list.reverse(acc))
    [name, ..rest] -> {
      use check <- result_try(parse_check_name(name))
      case contains_check(acc, check) {
        True -> parse_selected_checks(rest, acc)
        False -> parse_selected_checks(rest, [check, ..acc])
      }
    }
  }
}

fn result_try(result: Result(a, e), next: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
