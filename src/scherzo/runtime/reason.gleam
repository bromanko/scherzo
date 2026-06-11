pub type RetryReason {
  RetryAfterFailure
  RetryAfterContinuation
  RetryNoSlots
  RetryPollFailed
  RetryClaimStartLedgerAppendFailed
}

pub type ParkReason {
  ParkWorkerFailure
  ParkMaxRetryAttempts
  ParkMaxSessionsPerIssue
  ParkOperator(reason: String)
}

pub type StopReason {
  StopTerminal
  StopNonActive
}

pub fn retry_to_string(reason: RetryReason) -> String {
  case reason {
    RetryAfterFailure -> "failure"
    RetryAfterContinuation -> "continuation"
    RetryNoSlots -> "no available orchestrator slots"
    RetryPollFailed -> "retry poll failed"
    RetryClaimStartLedgerAppendFailed -> "claim_start_ledger_append_failed"
  }
}

pub fn park_to_string(reason: ParkReason) -> String {
  case reason {
    ParkWorkerFailure -> "worker_failure"
    ParkMaxRetryAttempts -> "max_retry_attempts"
    ParkMaxSessionsPerIssue -> "max_sessions_per_issue"
    ParkOperator(reason) -> reason
  }
}

pub fn stop_to_string(reason: StopReason) -> String {
  case reason {
    StopTerminal -> "terminal"
    StopNonActive -> "non_active"
  }
}
