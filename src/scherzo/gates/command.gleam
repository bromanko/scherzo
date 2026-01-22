/// Command gate - runs shell commands as completion gates
///
/// Executes configured commands (tests, lint, typecheck) and returns
/// pass/fail based on exit code. Extracts findings from output when possible.
///
/// Note: This module only handles CommandGate types. Other gate types
/// (ParallelReviewGate, MultiPassReviewGate, HumanGate) should be handled
/// by their respective executor modules.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config
import scherzo/core/shell
import scherzo/gates/types.{
  type Finding, type Location, type SingleGateResult, Finding, GateFailed,
  GateFeedback, GatePassed, Location, P0Critical, P1Major,
}

/// Default timeout for command gates (5 minutes)
pub const default_timeout_ms = 300_000

/// Execute a command gate.
///
/// Only handles CommandGate types. Other gate types will return an error
/// result indicating they should be handled by a different executor.
pub fn execute(
  gate: config.GateConfig,
  workspace_path: String,
) -> SingleGateResult {
  case gate {
    config.CommandGate(name, command, timeout_ms, _, _) -> {
      let effective_timeout = case timeout_ms {
        0 -> default_timeout_ms
        t -> t
      }

      // Parse command into executable and args
      let #(executable, args) = parse_command(command)

      // Run the command
      let result =
        shell.run_with_timeout(
          executable,
          args,
          workspace_path,
          effective_timeout,
        )

      case result {
        shell.Success(_output) -> GatePassed(name)

        shell.Failed(exit_code, output) -> {
          let findings = parse_output_for_findings(output, exit_code)
          let feedback =
            GateFeedback(
              gate_name: name,
              findings: findings,
              summary: "Command failed with exit code " <> int.to_string(exit_code),
              suggested_action: "Fix the issues reported by " <> name <> " and try again",
            )
          GateFailed(name, feedback)
        }

        shell.TimedOut -> {
          let feedback =
            GateFeedback(
              gate_name: name,
              findings: [
                Finding(
                  priority: P0Critical,
                  location: None,
                  issue: "Command timed out after "
                    <> int.to_string(effective_timeout / 1000)
                    <> " seconds",
                  suggestion: Some("Check for infinite loops or hanging processes"),
                ),
              ],
              summary: "Command timed out",
              suggested_action: "Investigate why the command is taking too long",
            )
          GateFailed(name, feedback)
        }
      }
    }

    // Other gate types should not be passed to this executor
    config.ParallelReviewGate(name, _, _) ->
      GateFailed(
        name,
        GateFeedback(
          gate_name: name,
          findings: [types.simple_finding(P0Critical, "ParallelReviewGate not yet implemented")],
          summary: "Gate type not implemented",
          suggested_action: "Use CommandGate instead or wait for implementation",
        ),
      )

    config.MultiPassReviewGate(name, _, _, _) ->
      GateFailed(
        name,
        GateFeedback(
          gate_name: name,
          findings: [types.simple_finding(P0Critical, "MultiPassReviewGate not yet implemented")],
          summary: "Gate type not implemented",
          suggested_action: "Use CommandGate instead or wait for implementation",
        ),
      )

    config.HumanGate(name, _) ->
      GateFailed(
        name,
        GateFeedback(
          gate_name: name,
          findings: [types.simple_finding(P0Critical, "HumanGate not yet implemented")],
          summary: "Gate type not implemented",
          suggested_action: "Use CommandGate instead or wait for implementation",
        ),
      )
  }
}

/// Parse a command string into executable and arguments.
///
/// NOTE: This uses simple space-splitting and does NOT handle quoted strings.
/// Commands with arguments containing spaces are not supported.
/// For complex commands, consider using a shell wrapper script.
fn parse_command(command: String) -> #(String, List(String)) {
  let parts = string.split(command, " ")
  case parts {
    [] -> #("", [])
    [exe] -> #(exe, [])
    [exe, ..args] -> #(exe, args)
  }
}

/// Parse command output to extract findings
/// Currently handles basic patterns - can be extended for specific tools
fn parse_output_for_findings(output: String, exit_code: Int) -> List(Finding) {
  let lines = string.split(output, "\n")

  // Look for common error patterns
  let findings =
    lines
    |> list.filter_map(fn(line) {
      case parse_error_line(line) {
        Some(finding) -> Ok(finding)
        None -> Error(Nil)
      }
    })

  // If no specific findings, create a generic one
  case findings {
    [] -> [
      Finding(
        priority: P1Major,
        location: None,
        issue: "Command failed with exit code " <> int.to_string(exit_code),
        suggestion: Some("Review the full output above for details"),
      ),
    ]
    _ -> findings
  }
}

/// Try to parse an error line into a finding
fn parse_error_line(line: String) -> Option(Finding) {
  let trimmed = string.trim(line)

  // Skip empty lines
  case trimmed {
    "" -> None
    _ -> {
      // Check for common error patterns
      case
        string.contains(trimmed, "error")
        || string.contains(trimmed, "Error")
        || string.contains(trimmed, "ERROR")
      {
        True ->
          Some(Finding(
            priority: P0Critical,
            location: try_extract_location(trimmed),
            issue: trimmed,
            suggestion: None,
          ))

        False ->
          case
            string.contains(trimmed, "warning")
            || string.contains(trimmed, "Warning")
            || string.contains(trimmed, "WARN")
          {
            True ->
              Some(Finding(
                priority: P1Major,
                location: try_extract_location(trimmed),
                issue: trimmed,
                suggestion: None,
              ))

            False -> None
          }
      }
    }
  }
}

/// Try to extract file:line location from an error message
fn try_extract_location(line: String) -> Option(Location) {
  // Common patterns:
  // - file.gleam:42: error
  // - file.rs:42:10: error
  // - src/file.ts(42,10): error

  // Look for colon-separated file:line pattern
  let parts = string.split(line, ":")
  case parts {
    [file, line_str, ..] -> {
      // Try to parse line number
      case int.parse(string.trim(line_str)) {
        Ok(line_num) ->
          Some(Location(
            file: string.trim(file),
            line: Some(line_num),
            column: None,
          ))
        Error(_) -> None
      }
    }
    _ -> None
  }
}
