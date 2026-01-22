import gleam/list
import gleam/string
import gleeunit/should
import scherzo/config/types as config
import scherzo/gates/command
import scherzo/gates/types.{GateFailed, GatePassed, P0Critical}

// Test with real (fast) commands

pub fn execute_passing_command_test() {
  let gate =
    config.CommandGate(
      name: "true-test",
      command: "true",
      timeout_ms: 5000,
      serial: True,
      fail_fast: False,
    )

  let result = command.execute(gate, "/tmp")

  let assert GatePassed(name) = result
  name |> should.equal("true-test")
}

pub fn execute_failing_command_test() {
  let gate =
    config.CommandGate(
      name: "false-test",
      command: "false",
      timeout_ms: 5000,
      serial: True,
      fail_fast: False,
    )

  let result = command.execute(gate, "/tmp")

  let assert GateFailed(name, feedback) = result
  name |> should.equal("false-test")
  feedback.gate_name |> should.equal("false-test")
  feedback.summary |> string.contains("exit code") |> should.be_true
}

pub fn execute_command_with_args_test() {
  let gate =
    config.CommandGate(
      name: "echo-test",
      command: "echo hello",
      timeout_ms: 5000,
      serial: True,
      fail_fast: False,
    )

  let result = command.execute(gate, "/tmp")

  let assert GatePassed(name) = result
  name |> should.equal("echo-test")
}

pub fn execute_uses_default_timeout_when_zero_test() {
  // This just verifies the gate runs - can't easily test the actual timeout
  let gate =
    config.CommandGate(
      name: "timeout-test",
      command: "true",
      timeout_ms: 0,
      serial: True,
      fail_fast: False,
    )

  let result = command.execute(gate, "/tmp")
  result |> types.is_pass |> should.be_true
}

// Test non-CommandGate types return proper errors

pub fn execute_parallel_review_gate_returns_error_test() {
  let gate =
    config.ParallelReviewGate(
      name: "review",
      dimensions: [],
      synthesis_prompt: "test",
    )

  let result = command.execute(gate, "/tmp")

  let assert GateFailed(name, feedback) = result
  name |> should.equal("review")
  feedback.summary |> should.equal("Gate type not implemented")

  let assert [finding] = feedback.findings
  finding.priority |> should.equal(P0Critical)
  finding.issue |> string.contains("ParallelReviewGate") |> should.be_true
}

pub fn execute_multipass_review_gate_returns_error_test() {
  let gate =
    config.MultiPassReviewGate(
      name: "multipass",
      passes: [],
      require_convergence: False,
      max_passes: 3,
    )

  let result = command.execute(gate, "/tmp")

  let assert GateFailed(name, feedback) = result
  name |> should.equal("multipass")
  feedback.summary |> should.equal("Gate type not implemented")

  let assert [finding] = feedback.findings
  finding.issue |> string.contains("MultiPassReviewGate") |> should.be_true
}

pub fn execute_human_gate_returns_error_test() {
  let gate = config.HumanGate(name: "approval", prompt: "Please approve")

  let result = command.execute(gate, "/tmp")

  let assert GateFailed(name, feedback) = result
  name |> should.equal("approval")
  feedback.summary |> should.equal("Gate type not implemented")

  let assert [finding] = feedback.findings
  finding.issue |> string.contains("HumanGate") |> should.be_true
}

// Test output parsing (using commands that produce known output)

pub fn execute_captures_error_output_test() {
  // sh -c 'echo "error: something failed"' will output "error: something failed"
  // followed by exit 1 to make it fail
  let gate =
    config.CommandGate(
      name: "error-output",
      command: "sh -c 'echo error: something failed && exit 1'",
      timeout_ms: 5000,
      serial: True,
      fail_fast: False,
    )

  let result = command.execute(gate, "/tmp")

  let assert GateFailed(_name, feedback) = result
  // Should have parsed the error line
  feedback.findings
  |> list.any(fn(f) { string.contains(f.issue, "error") })
  |> should.be_true
}

pub fn default_timeout_constant_test() {
  // Just verify the constant exists and is reasonable (5 minutes)
  command.default_timeout_ms |> should.equal(300_000)
}
