import scherzo/workflow_outcome

pub fn constants_and_terminal_predicates_test() {
  assert workflow_outcome.completed == "completed"
  assert workflow_outcome.failed_fatal == "failed_fatal"
  assert workflow_outcome.cancelled == "cancelled"
  assert workflow_outcome.succeeded_after_recovery == "succeeded_after_recovery"
  assert workflow_outcome.failed_after_recovery == "failed_after_recovery"

  assert workflow_outcome.terminal_success(workflow_outcome.NoStepRecovery)
    == "completed"
  assert workflow_outcome.terminal_success(workflow_outcome.StepRecoveryRan)
    == "succeeded_after_recovery"
  assert workflow_outcome.terminal_failed_fatal(workflow_outcome.NoStepRecovery)
    == "failed_fatal"
  assert workflow_outcome.terminal_failed_fatal(
      workflow_outcome.StepRecoveryRetryRequested,
    )
    == "failed_after_recovery"

  assert workflow_outcome.is_terminal_success("completed")
  assert workflow_outcome.is_terminal_success("succeeded_after_recovery")
  assert !workflow_outcome.is_terminal_success("failed_fatal")

  assert workflow_outcome.is_terminal_failure("failed_fatal")
  assert workflow_outcome.is_terminal_failure("failed_after_recovery")
  assert !workflow_outcome.is_terminal_failure("cancelled")
}
