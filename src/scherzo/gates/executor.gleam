/// Gate executor - main loop for running completion gates
///
/// Executes configured gates in order, collects results, and handles
/// pass/fail transitions. Always restarts from the beginning on retry
/// per design decision.
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config
import scherzo/gates/command
import scherzo/gates/types.{
  type GateFeedback, type GateResult, type SingleGateResult, AllPassed, Failed,
  GateFailed, GatePassed,
}

/// Error types for gate execution
pub type GateError {
  /// No gates configured
  NoGatesConfigured
  /// Configuration error
  ConfigError(message: String)
  /// Execution error
  ExecutionError(gate_name: String, message: String)
}

/// Execute all configured gates for a task
pub fn execute(
  gates_config: config.GatesConfig,
  workspace_path: String,
) -> Result(GateResult, GateError) {
  case gates_config.gates {
    [] -> Ok(AllPassed([]))
    gates -> execute_gates(gates, workspace_path, [])
  }
}

/// Execute gates sequentially, collecting results
fn execute_gates(
  gates: List(config.GateConfig),
  workspace_path: String,
  passed: List(SingleGateResult),
) -> Result(GateResult, GateError) {
  case gates {
    [] -> Ok(AllPassed(list.reverse(passed)))

    [gate, ..rest] -> {
      let result = execute_single_gate(gate, workspace_path)

      case result {
        GatePassed(name) -> {
          execute_gates(rest, workspace_path, [GatePassed(name), ..passed])
        }

        GateFailed(name, feedback) -> {
          Ok(Failed(
            failed_gate: name,
            feedback: feedback,
            passed_gates: list.reverse(passed),
          ))
        }
      }
    }
  }
}

/// Execute a single gate based on its type
fn execute_single_gate(
  gate: config.GateConfig,
  workspace_path: String,
) -> SingleGateResult {
  // Delegate all gate types to the command module
  // It handles CommandGate directly and returns "not implemented" for others
  command.execute(gate, workspace_path)
}

/// Create a Gleam-specific test configuration for development.
/// This config runs `gleam test` and `gleam check` as gates.
pub fn gleam_test_config() -> config.GatesConfig {
  config.GatesConfig(formula: None, gates: [
    config.CommandGate(
      name: "tests",
      command: "gleam test",
      timeout_ms: 0,
      serial: True,
      fail_fast: False,
    ),
    config.CommandGate(
      name: "check",
      command: "gleam check",
      timeout_ms: 0,
      serial: True,
      fail_fast: False,
    ),
  ])
}

/// Check if a gate result indicates all gates passed
pub fn all_gates_passed(result: GateResult) -> Bool {
  case result {
    AllPassed(_) -> True
    Failed(_, _, _) -> False
  }
}

/// Get feedback from a failed gate result
pub fn get_feedback(result: GateResult) -> Option(GateFeedback) {
  case result {
    AllPassed(_) -> None
    Failed(_, feedback, _) -> Some(feedback)
  }
}

/// Get the name of the failed gate
pub fn get_failed_gate_name(result: GateResult) -> Option(String) {
  case result {
    AllPassed(_) -> None
    Failed(name, _, _) -> Some(name)
  }
}
