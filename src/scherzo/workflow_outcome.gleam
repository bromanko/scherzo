import gleam/list

pub const completed = "completed"

pub const failed_fatal = "failed_fatal"

pub const cancelled = "cancelled"

pub const succeeded_after_recovery = "succeeded_after_recovery"

pub const failed_after_recovery = "failed_after_recovery"

pub type RecoveryEvidence {
  NoStepRecovery
  StepRecoveryRan
  StepRecoveryRetryRequested
}

pub fn terminal_success(recovery_evidence: RecoveryEvidence) -> String {
  case recovery_evidence {
    NoStepRecovery -> completed
    StepRecoveryRan | StepRecoveryRetryRequested -> succeeded_after_recovery
  }
}

pub fn terminal_failed_fatal(recovery_evidence: RecoveryEvidence) -> String {
  case recovery_evidence {
    NoStepRecovery -> failed_fatal
    StepRecoveryRan | StepRecoveryRetryRequested -> failed_after_recovery
  }
}

pub fn is_terminal_success(outcome: String) -> Bool {
  outcome == completed || outcome == succeeded_after_recovery
}

pub fn is_terminal_failure(outcome: String) -> Bool {
  outcome == failed_fatal || outcome == failed_after_recovery
}

pub fn recovery_attempted(recovery_evidence: RecoveryEvidence) -> Bool {
  case recovery_evidence {
    NoStepRecovery -> False
    StepRecoveryRan | StepRecoveryRetryRequested -> True
  }
}

pub fn is_known_terminal_outcome(outcome: String) -> Bool {
  list.contains(
    [
      completed,
      failed_fatal,
      cancelled,
      succeeded_after_recovery,
      failed_after_recovery,
    ],
    outcome,
  )
}
