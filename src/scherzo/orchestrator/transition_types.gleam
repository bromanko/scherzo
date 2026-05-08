import gleam/dict
import gleam/option.{type Option}
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/event as session_event
import scherzo/state/ledger
import scherzo/tracker/issue as tracker_issue

pub type State {
  State(
    runtime: orchestrator_state.RuntimeState,
    workers: WorkerDirectory,
    pending_claims: dict.Dict(String, PendingClaim),
  )
}

pub type Message {
  LedgerAppendCompleted(
    correlation_id: String,
    continuation: effects_types.LedgerContinuation,
    result: Result(Nil, ledger.LedgerError),
    now_ms: Int,
  )
}

pub type Outcome {
  Outcome(state: State, effects: List(effects_types.Effect))
}

pub type WorkerDirectory {
  WorkerDirectory(
    by_issue: dict.Dict(String, WorkerEntry),
    by_session: dict.Dict(String, String),
    route_to_session: dict.Dict(String, String),
  )
}

pub type WorkerEntry {
  WorkerEntry(
    issue_id: String,
    run_id: String,
    session_id: String,
    issue: tracker_issue.Issue,
    workspace_path: String,
    workflow_id: String,
    command_route_id: String,
    status: WorkerStatus,
    recovery: Option(session_event.RecoveryInfo),
  )
}

pub type WorkerStatus {
  WorkerStarting
  WorkerRunning
  WorkerStopping(reason: String)
  WorkerFinishedStatus
}

pub type PendingClaim {
  PendingClaim(
    issue_id: String,
    run_id: String,
    session_id: String,
    workspace_path: String,
    workflow_id: String,
    command_route_id: String,
    route_label: String,
    issue: tracker_issue.Issue,
    recovery: Option(session_event.RecoveryInfo),
    remaining_candidates: List(tracker_issue.Issue),
  )
}

pub fn new_worker_directory() -> WorkerDirectory {
  WorkerDirectory(
    by_issue: dict.new(),
    by_session: dict.new(),
    route_to_session: dict.new(),
  )
}
