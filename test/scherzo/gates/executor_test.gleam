import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import scherzo/config/types as config
import scherzo/gates/executor
import scherzo/gates/types.{AllPassed, Failed, GatePassed}

// Execute tests

pub fn execute_empty_gates_returns_all_passed_test() {
  let gates_config = config.GatesConfig(gates: [])

  let result = executor.execute(gates_config, "/tmp")

  result |> should.be_ok
  let assert Ok(gate_result) = result
  let assert AllPassed(results) = gate_result
  results |> should.equal([])
}

pub fn execute_single_passing_gate_test() {
  let gates_config =
    config.GatesConfig(gates: [
      config.CommandGate(
        name: "pass",
        command: "true",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
    ])

  let result = executor.execute(gates_config, "/tmp")

  result |> should.be_ok
  let assert Ok(gate_result) = result
  let assert AllPassed(results) = gate_result
  results |> list.length |> should.equal(1)
}

pub fn execute_multiple_passing_gates_test() {
  let gates_config =
    config.GatesConfig(gates: [
      config.CommandGate(
        name: "pass1",
        command: "true",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
      config.CommandGate(
        name: "pass2",
        command: "true",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
    ])

  let result = executor.execute(gates_config, "/tmp")

  result |> should.be_ok
  let assert Ok(gate_result) = result
  let assert AllPassed(results) = gate_result
  results |> list.length |> should.equal(2)
}

pub fn execute_failing_gate_returns_failed_test() {
  let gates_config =
    config.GatesConfig(gates: [
      config.CommandGate(
        name: "fail",
        command: "false",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
    ])

  let result = executor.execute(gates_config, "/tmp")

  result |> should.be_ok
  let assert Ok(gate_result) = result
  let assert Failed(failed_gate, _feedback, passed_gates) = gate_result
  failed_gate |> should.equal("fail")
  passed_gates |> should.equal([])
}

pub fn execute_stops_on_first_failure_test() {
  let gates_config =
    config.GatesConfig(gates: [
      config.CommandGate(
        name: "pass",
        command: "true",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
      config.CommandGate(
        name: "fail",
        command: "false",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
      config.CommandGate(
        name: "never-runs",
        command: "true",
        timeout_ms: 5000,
        serial: True,
        fail_fast: False,
      ),
    ])

  let result = executor.execute(gates_config, "/tmp")

  result |> should.be_ok
  let assert Ok(gate_result) = result
  let assert Failed(failed_gate, _feedback, passed_gates) = gate_result
  failed_gate |> should.equal("fail")
  // Only the first gate should be in passed_gates
  passed_gates |> list.length |> should.equal(1)
  let assert [GatePassed(name)] = passed_gates
  name |> should.equal("pass")
}

// all_gates_passed helper tests

pub fn all_gates_passed_true_for_all_passed_test() {
  let result = AllPassed([GatePassed("test")])
  executor.all_gates_passed(result) |> should.be_true
}

pub fn all_gates_passed_false_for_failed_test() {
  let feedback =
    types.GateFeedback(
      gate_name: "test",
      findings: [],
      summary: "Failed",
      suggested_action: "Fix",
    )
  let result = Failed("test", feedback, [])
  executor.all_gates_passed(result) |> should.be_false
}

// get_feedback helper tests

pub fn get_feedback_returns_none_for_all_passed_test() {
  let result = AllPassed([])
  executor.get_feedback(result) |> should.equal(None)
}

pub fn get_feedback_returns_some_for_failed_test() {
  let feedback =
    types.GateFeedback(
      gate_name: "test",
      findings: [],
      summary: "Test failed",
      suggested_action: "Fix",
    )
  let result = Failed("test", feedback, [])

  let assert Some(got_feedback) = executor.get_feedback(result)
  got_feedback.summary |> should.equal("Test failed")
}

// get_failed_gate_name helper tests

pub fn get_failed_gate_name_returns_none_for_all_passed_test() {
  let result = AllPassed([])
  executor.get_failed_gate_name(result) |> should.equal(None)
}

pub fn get_failed_gate_name_returns_some_for_failed_test() {
  let feedback =
    types.GateFeedback(
      gate_name: "broken-gate",
      findings: [],
      summary: "Failed",
      suggested_action: "Fix",
    )
  let result = Failed("broken-gate", feedback, [])

  executor.get_failed_gate_name(result) |> should.equal(Some("broken-gate"))
}

// gleam_test_config tests

pub fn gleam_test_config_has_two_gates_test() {
  let config = executor.gleam_test_config()
  config.gates |> list.length |> should.equal(2)
}

pub fn gleam_test_config_has_tests_gate_test() {
  let config = executor.gleam_test_config()
  let assert [first, _] = config.gates
  let assert config.CommandGate(name, command, ..) = first
  name |> should.equal("tests")
  command |> should.equal("gleam test")
}

pub fn gleam_test_config_has_check_gate_test() {
  let config = executor.gleam_test_config()
  let assert [_, second] = config.gates
  let assert config.CommandGate(name, command, ..) = second
  name |> should.equal("check")
  command |> should.equal("gleam check")
}
