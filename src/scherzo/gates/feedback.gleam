/// Feedback formatting for gate findings
///
/// Formats gate findings into structured feedback that agents can use
/// to fix issues. Groups findings by priority and includes actionable suggestions.

import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/gates/types.{
  type Finding, type GateFeedback, type Priority, P0Critical, P1Major, P2Minor,
  P3Suggestion,
}

/// Format gate feedback as a string for agent consumption
pub fn format_for_agent(feedback: GateFeedback) -> String {
  [
    format_header(feedback),
    format_findings_by_priority(feedback.findings),
    format_suggested_action(feedback.suggested_action),
  ]
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n\n")
}

/// Format a short summary suitable for task phase tracking
pub fn format_summary(feedback: GateFeedback) -> String {
  let p0_count = count_priority(feedback.findings, P0Critical)
  let p1_count = count_priority(feedback.findings, P1Major)
  let p2_count = count_priority(feedback.findings, P2Minor)

  feedback.gate_name
  <> " failed: "
  <> int.to_string(p0_count)
  <> " critical, "
  <> int.to_string(p1_count)
  <> " major, "
  <> int.to_string(p2_count)
  <> " minor"
}

/// Format the header section
fn format_header(feedback: GateFeedback) -> String {
  "## Gate Failed: " <> feedback.gate_name <> "\n\n" <> feedback.summary
}

/// Format findings grouped by priority
fn format_findings_by_priority(findings: List(Finding)) -> String {
  let p0 = filter_by_priority(findings, P0Critical)
  let p1 = filter_by_priority(findings, P1Major)
  let p2 = filter_by_priority(findings, P2Minor)
  let p3 = filter_by_priority(findings, P3Suggestion)

  [
    format_priority_section("### P0 Critical (must fix)", p0),
    format_priority_section("### P1 Major (should fix)", p1),
    format_priority_section("### P2 Minor (nice to fix)", p2),
    format_priority_section("### P3 Suggestions", p3),
  ]
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n\n")
}

/// Format a single priority section
fn format_priority_section(header: String, findings: List(Finding)) -> String {
  case findings {
    [] -> ""
    _ -> {
      let formatted =
        findings
        |> list.map(format_finding)
        |> string.join("\n")
      header <> "\n" <> formatted
    }
  }
}

/// Format a single finding
fn format_finding(finding: Finding) -> String {
  let location_str = case finding.location {
    None -> ""
    Some(loc) -> {
      let line_str = case loc.line {
        None -> ""
        Some(line) -> ":" <> int.to_string(line)
      }
      loc.file <> line_str <> " - "
    }
  }

  let suggestion_str = case finding.suggestion {
    None -> ""
    Some(suggestion) -> "\n  Suggestion: " <> suggestion
  }

  "- " <> location_str <> finding.issue <> suggestion_str
}

/// Format the suggested action section
fn format_suggested_action(action: String) -> String {
  case action {
    "" -> ""
    _ -> "## Next Steps\n\n" <> action
  }
}

/// Filter findings by priority
fn filter_by_priority(
  findings: List(Finding),
  priority: Priority,
) -> List(Finding) {
  list.filter(findings, fn(f) { f.priority == priority })
}

/// Count findings with a specific priority
fn count_priority(findings: List(Finding), priority: Priority) -> Int {
  findings
  |> filter_by_priority(priority)
  |> list.length()
}
