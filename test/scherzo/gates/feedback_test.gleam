import gleam/string
import gleeunit/should
import scherzo/gates/feedback
import scherzo/gates/types.{GateFeedback, P0Critical, P1Major, P2Minor}

// format_summary tests

pub fn format_summary_counts_correctly_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "tests",
      findings: [
        types.simple_finding(P0Critical, "Critical 1"),
        types.simple_finding(P0Critical, "Critical 2"),
        types.simple_finding(P1Major, "Major 1"),
        types.simple_finding(P2Minor, "Minor 1"),
      ],
      summary: "Tests failed",
      suggested_action: "Fix tests",
    )

  let summary = feedback.format_summary(gate_feedback)
  summary |> should.equal("tests failed: 2 critical, 1 major, 1 minor")
}

pub fn format_summary_empty_findings_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "check",
      findings: [],
      summary: "Check failed",
      suggested_action: "Fix it",
    )

  let summary = feedback.format_summary(gate_feedback)
  summary |> should.equal("check failed: 0 critical, 0 major, 0 minor")
}

// format_for_agent tests

pub fn format_for_agent_includes_header_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "tests",
      findings: [],
      summary: "Tests failed with 3 errors",
      suggested_action: "",
    )

  let formatted = feedback.format_for_agent(gate_feedback)
  formatted |> string.contains("## Gate Failed: tests") |> should.be_true
  formatted |> string.contains("Tests failed with 3 errors") |> should.be_true
}

pub fn format_for_agent_groups_by_priority_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "review",
      findings: [
        types.simple_finding(P0Critical, "Critical issue"),
        types.simple_finding(P1Major, "Major issue"),
        types.simple_finding(P2Minor, "Minor issue"),
      ],
      summary: "Review found issues",
      suggested_action: "Address findings",
    )

  let formatted = feedback.format_for_agent(gate_feedback)

  // Should have priority sections
  formatted |> string.contains("### P0 Critical") |> should.be_true
  formatted |> string.contains("### P1 Major") |> should.be_true
  formatted |> string.contains("### P2 Minor") |> should.be_true

  // Should include issues
  formatted |> string.contains("Critical issue") |> should.be_true
  formatted |> string.contains("Major issue") |> should.be_true
  formatted |> string.contains("Minor issue") |> should.be_true
}

pub fn format_for_agent_includes_suggested_action_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "tests",
      findings: [],
      summary: "Failed",
      suggested_action: "Run gleam test to see errors",
    )

  let formatted = feedback.format_for_agent(gate_feedback)
  formatted |> string.contains("## Next Steps") |> should.be_true
  formatted
  |> string.contains("Run gleam test to see errors")
  |> should.be_true
}

pub fn format_for_agent_includes_location_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "lint",
      findings: [types.finding_at(P0Critical, "src/main.gleam", 42, "Bug here")],
      summary: "Lint failed",
      suggested_action: "",
    )

  let formatted = feedback.format_for_agent(gate_feedback)
  formatted |> string.contains("src/main.gleam:42") |> should.be_true
  formatted |> string.contains("Bug here") |> should.be_true
}

pub fn format_for_agent_includes_suggestion_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "review",
      findings: [
        types.finding_with_suggestion(
          P1Major,
          "Missing error handling",
          "Add a case for Error",
        ),
      ],
      summary: "Review found issues",
      suggested_action: "",
    )

  let formatted = feedback.format_for_agent(gate_feedback)
  formatted |> string.contains("Missing error handling") |> should.be_true
  formatted
  |> string.contains("Suggestion: Add a case for Error")
  |> should.be_true
}

pub fn format_for_agent_skips_empty_priority_sections_test() {
  let gate_feedback =
    GateFeedback(
      gate_name: "tests",
      findings: [types.simple_finding(P1Major, "Major only")],
      summary: "Failed",
      suggested_action: "",
    )

  let formatted = feedback.format_for_agent(gate_feedback)

  // Should have P1 section
  formatted |> string.contains("### P1 Major") |> should.be_true

  // Should NOT have P0, P2, P3 sections since no findings
  formatted |> string.contains("### P0 Critical") |> should.be_false
  formatted |> string.contains("### P2 Minor") |> should.be_false
  formatted |> string.contains("### P3 Suggestions") |> should.be_false
}
