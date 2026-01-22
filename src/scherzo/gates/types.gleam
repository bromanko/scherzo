/// Gate types for completion gate evaluation
///
/// Defines findings, priorities, feedback structures, and gate results
/// used throughout the gate execution system.

import gleam/list
import gleam/option.{type Option}

/// Priority level for gate findings
pub type Priority {
  /// Critical issue - must fix before merge
  P0Critical
  /// Major issue - should fix before merge
  P1Major
  /// Minor issue - nice to fix, won't block
  P2Minor
  /// Suggestion - optional improvement
  P3Suggestion
}

/// Location reference for a finding
pub type Location {
  Location(
    /// File path
    file: String,
    /// Line number (if available)
    line: Option(Int),
    /// Column number (if available)
    column: Option(Int),
  )
}

/// An individual issue found by a gate
pub type Finding {
  Finding(
    /// Severity of the issue
    priority: Priority,
    /// Where the issue was found (if applicable)
    location: Option(Location),
    /// Description of the issue
    issue: String,
    /// Suggested fix (if available)
    suggestion: Option(String),
  )
}

/// Structured feedback from gate evaluation for agent consumption
pub type GateFeedback {
  GateFeedback(
    /// Name of the gate that produced this feedback
    gate_name: String,
    /// All findings from the gate
    findings: List(Finding),
    /// Human-readable summary
    summary: String,
    /// Suggested action for the agent
    suggested_action: String,
  )
}

/// Result of evaluating a single gate
pub type SingleGateResult {
  /// Gate passed
  GatePassed(gate_name: String)
  /// Gate failed with findings
  GateFailed(gate_name: String, feedback: GateFeedback)
}

/// Result of evaluating all gates for a task
pub type GateResult {
  /// All gates passed
  AllPassed(gate_results: List(SingleGateResult))
  /// A gate failed
  Failed(
    /// Which gate failed
    failed_gate: String,
    /// Feedback from the failed gate
    feedback: GateFeedback,
    /// Results from gates that passed before the failure
    passed_gates: List(SingleGateResult),
  )
}

/// Create a finding with just priority and issue
pub fn simple_finding(priority: Priority, issue: String) -> Finding {
  Finding(
    priority: priority,
    location: option.None,
    issue: issue,
    suggestion: option.None,
  )
}

/// Create a finding with file location
pub fn finding_at(
  priority: Priority,
  file: String,
  line: Int,
  issue: String,
) -> Finding {
  Finding(
    priority: priority,
    location: option.Some(Location(
      file: file,
      line: option.Some(line),
      column: option.None,
    )),
    issue: issue,
    suggestion: option.None,
  )
}

/// Create a finding with suggestion
pub fn finding_with_suggestion(
  priority: Priority,
  issue: String,
  suggestion: String,
) -> Finding {
  Finding(
    priority: priority,
    location: option.None,
    issue: issue,
    suggestion: option.Some(suggestion),
  )
}

/// Check if a gate result is a pass
pub fn is_pass(result: SingleGateResult) -> Bool {
  case result {
    GatePassed(_) -> True
    GateFailed(_, _) -> False
  }
}

/// Check if all gates passed
pub fn all_passed(result: GateResult) -> Bool {
  case result {
    AllPassed(_) -> True
    Failed(_, _, _) -> False
  }
}

/// Get the gate name from a single result
pub fn result_gate_name(result: SingleGateResult) -> String {
  case result {
    GatePassed(name) -> name
    GateFailed(name, _) -> name
  }
}

/// Count findings by priority
pub fn count_by_priority(findings: List(Finding), priority: Priority) -> Int {
  findings
  |> list.filter(fn(f) { f.priority == priority })
  |> list.length()
}

/// Check if findings have any blocking issues (P0 or P1)
pub fn has_blocking_issues(findings: List(Finding)) -> Bool {
  list.any(findings, fn(f) {
    case f.priority {
      P0Critical -> True
      P1Major -> True
      _ -> False
    }
  })
}
