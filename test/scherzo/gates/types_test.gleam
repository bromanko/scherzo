import gleam/option.{None, Some}
import gleeunit/should
import scherzo/gates/types.{
  AllPassed, Failed, GateFailed, GateFeedback, GatePassed, P0Critical, P1Major,
  P2Minor, P3Suggestion,
}

// Finding constructor tests

pub fn simple_finding_test() {
  let finding = types.simple_finding(P0Critical, "Something is wrong")

  finding.priority |> should.equal(P0Critical)
  finding.issue |> should.equal("Something is wrong")
  finding.location |> should.equal(None)
  finding.suggestion |> should.equal(None)
}

pub fn finding_at_test() {
  let finding = types.finding_at(P1Major, "src/main.gleam", 42, "Bug here")

  finding.priority |> should.equal(P1Major)
  finding.issue |> should.equal("Bug here")

  let assert Some(loc) = finding.location
  loc.file |> should.equal("src/main.gleam")
  loc.line |> should.equal(Some(42))
  loc.column |> should.equal(None)
}

pub fn finding_with_suggestion_test() {
  let finding =
    types.finding_with_suggestion(
      P2Minor,
      "Could be better",
      "Try this instead",
    )

  finding.priority |> should.equal(P2Minor)
  finding.issue |> should.equal("Could be better")
  finding.suggestion |> should.equal(Some("Try this instead"))
  finding.location |> should.equal(None)
}

// is_pass tests

pub fn is_pass_returns_true_for_passed_test() {
  let result = GatePassed("tests")
  types.is_pass(result) |> should.be_true
}

pub fn is_pass_returns_false_for_failed_test() {
  let feedback =
    GateFeedback(
      gate_name: "tests",
      findings: [],
      summary: "Failed",
      suggested_action: "Fix it",
    )
  let result = GateFailed("tests", feedback)
  types.is_pass(result) |> should.be_false
}

// all_passed tests

pub fn all_passed_returns_true_when_all_passed_test() {
  let result = AllPassed([GatePassed("tests"), GatePassed("check")])
  types.all_passed(result) |> should.be_true
}

pub fn all_passed_returns_false_when_failed_test() {
  let feedback =
    GateFeedback(
      gate_name: "tests",
      findings: [],
      summary: "Failed",
      suggested_action: "Fix it",
    )
  let result = Failed("tests", feedback, [])
  types.all_passed(result) |> should.be_false
}

// result_gate_name tests

pub fn result_gate_name_for_passed_test() {
  let result = GatePassed("my-gate")
  types.result_gate_name(result) |> should.equal("my-gate")
}

pub fn result_gate_name_for_failed_test() {
  let feedback =
    GateFeedback(
      gate_name: "my-gate",
      findings: [],
      summary: "Failed",
      suggested_action: "Fix it",
    )
  let result = GateFailed("my-gate", feedback)
  types.result_gate_name(result) |> should.equal("my-gate")
}

// count_by_priority tests

pub fn count_by_priority_empty_list_test() {
  types.count_by_priority([], P0Critical) |> should.equal(0)
}

pub fn count_by_priority_counts_correctly_test() {
  let findings = [
    types.simple_finding(P0Critical, "Critical 1"),
    types.simple_finding(P0Critical, "Critical 2"),
    types.simple_finding(P1Major, "Major 1"),
    types.simple_finding(P2Minor, "Minor 1"),
    types.simple_finding(P0Critical, "Critical 3"),
  ]

  types.count_by_priority(findings, P0Critical) |> should.equal(3)
  types.count_by_priority(findings, P1Major) |> should.equal(1)
  types.count_by_priority(findings, P2Minor) |> should.equal(1)
  types.count_by_priority(findings, P3Suggestion) |> should.equal(0)
}

// has_blocking_issues tests

pub fn has_blocking_issues_empty_list_test() {
  types.has_blocking_issues([]) |> should.be_false
}

pub fn has_blocking_issues_with_p0_test() {
  let findings = [types.simple_finding(P0Critical, "Critical")]
  types.has_blocking_issues(findings) |> should.be_true
}

pub fn has_blocking_issues_with_p1_test() {
  let findings = [types.simple_finding(P1Major, "Major")]
  types.has_blocking_issues(findings) |> should.be_true
}

pub fn has_blocking_issues_with_p2_only_test() {
  let findings = [types.simple_finding(P2Minor, "Minor")]
  types.has_blocking_issues(findings) |> should.be_false
}

pub fn has_blocking_issues_with_p3_only_test() {
  let findings = [types.simple_finding(P3Suggestion, "Suggestion")]
  types.has_blocking_issues(findings) |> should.be_false
}

pub fn has_blocking_issues_mixed_priorities_test() {
  let findings = [
    types.simple_finding(P2Minor, "Minor"),
    types.simple_finding(P3Suggestion, "Suggestion"),
    types.simple_finding(P1Major, "Major"),
  ]
  types.has_blocking_issues(findings) |> should.be_true
}
