import gleeunit/should
import scherzo/config/types as config
import scherzo/orchestrator
import scherzo/orchestrator/coordinator.{
  BatchResult, CoordinatorConfig, RunExhausted, RunFailed, RunGatesFailed,
  RunSuccess, TaskResult,
}

pub fn default_config_sets_working_dir_test() {
  let config = orchestrator.default_config("/home/user/project")

  config.working_dir |> should.equal("/home/user/project")
}

pub fn default_config_sets_max_retries_test() {
  let config = orchestrator.default_config("/project")

  config.max_retries |> should.equal(3)
}

pub fn default_config_sets_max_continuations_test() {
  let config = orchestrator.default_config("/project")

  config.max_continuations |> should.equal(5)
}

pub fn orchestrator_config_can_be_customized_test() {
  let cfg =
    CoordinatorConfig(
      working_dir: "/custom/path",
      max_retries: 5,
      max_continuations: 10,
      gates_config: config.default_gates_config(),
      retry_config: config.default_retry_config(),
    )

  cfg.working_dir |> should.equal("/custom/path")
  cfg.max_retries |> should.equal(5)
  cfg.max_continuations |> should.equal(10)
}

pub fn run_result_types_exist_test() {
  // Verify RunResult constructors exist
  let _ = RunSuccess(output: "done", change_id: "abc")
  let _ = RunFailed(reason: "error")
  let _ =
    RunExhausted(continuations: 5, last_output: "partial", change_id: "xyz")
  let _ = RunGatesFailed(gate_name: "tests", feedback_summary: "1 error")
  should.be_true(True)
}

pub fn task_result_type_test() {
  let result =
    TaskResult(
      task_id: "t-1",
      title: "Test Task",
      result: RunSuccess(output: "completed", change_id: "abc123"),
    )

  result.task_id |> should.equal("t-1")
  result.title |> should.equal("Test Task")
}

pub fn batch_result_type_test() {
  let batch =
    BatchResult(
      completed: [
        TaskResult(
          task_id: "t-1",
          title: "Task 1",
          result: RunSuccess(output: "done", change_id: "a"),
        ),
      ],
      failed: [
        TaskResult(
          task_id: "t-2",
          title: "Task 2",
          result: RunFailed(reason: "error"),
        ),
      ],
      total: 2,
    )

  batch.total |> should.equal(2)
}

pub fn run_success_contains_output_test() {
  let result =
    RunSuccess(output: "Task completed successfully", change_id: "abc")

  let RunSuccess(output, change_id) = result
  output |> should.equal("Task completed successfully")
  change_id |> should.equal("abc")
}

pub fn run_failed_contains_reason_test() {
  let result = RunFailed(reason: "Command execution failed")

  let RunFailed(reason) = result
  reason |> should.equal("Command execution failed")
}

pub fn run_exhausted_contains_continuation_info_test() {
  let result =
    RunExhausted(
      continuations: 5,
      last_output: "partial work",
      change_id: "xyz",
    )

  let RunExhausted(continuations, last_output, change_id) = result
  continuations |> should.equal(5)
  last_output |> should.equal("partial work")
  change_id |> should.equal("xyz")
}

pub fn run_gates_failed_contains_info_test() {
  let result =
    RunGatesFailed(gate_name: "typecheck", feedback_summary: "2 errors found")

  let RunGatesFailed(gate_name, feedback_summary) = result
  gate_name |> should.equal("typecheck")
  feedback_summary |> should.equal("2 errors found")
}
