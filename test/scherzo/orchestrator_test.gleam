import gleeunit/should
import scherzo/orchestrator.{
  BatchResult, OrchestratorConfig, RunExhausted, RunFailed, RunSuccess,
  TaskResult,
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
  let config =
    OrchestratorConfig(
      working_dir: "/custom/path",
      max_retries: 5,
      max_continuations: 10,
    )

  config.working_dir |> should.equal("/custom/path")
  config.max_retries |> should.equal(5)
  config.max_continuations |> should.equal(10)
}

pub fn run_result_types_exist_test() {
  // Verify RunResult constructors exist
  let _ = RunSuccess(output: "done", change_id: "abc")
  let _ = RunFailed(reason: "error")
  let _ =
    RunExhausted(continuations: 5, last_output: "partial", change_id: "xyz")
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

  case result {
    RunSuccess(output, change_id) -> {
      output |> should.equal("Task completed successfully")
      change_id |> should.equal("abc")
    }
    _ -> should.fail()
  }
}

pub fn run_failed_contains_reason_test() {
  let result = RunFailed(reason: "Command execution failed")

  case result {
    RunFailed(reason) -> {
      reason |> should.equal("Command execution failed")
    }
    _ -> should.fail()
  }
}

pub fn run_exhausted_contains_continuation_info_test() {
  let result =
    RunExhausted(
      continuations: 5,
      last_output: "partial work",
      change_id: "xyz",
    )

  case result {
    RunExhausted(continuations, last_output, change_id) -> {
      continuations |> should.equal(5)
      last_output |> should.equal("partial work")
      change_id |> should.equal("xyz")
    }
    _ -> should.fail()
  }
}
