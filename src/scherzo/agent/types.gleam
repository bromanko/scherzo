import gleam/option.{type Option}
import scherzo/agent/pi_event
import scherzo/domain
import scherzo/error
import scherzo/session/event as session_event

pub type FinalClassification {
  FinalActive
  FinalTerminal
  FinalNonActive
}

pub type WorkerSuccess {
  WorkerSuccess(
    final_issue: Option(domain.Issue),
    final_classification: FinalClassification,
    workspace_path: String,
    tokens: domain.TokenTotals,
    turns: Int,
    result: domain.ResultArtifact,
  )
}

pub type WorkerFailure {
  WorkerFailure(
    reason: error.AgentRunnerError,
    workspace_path: Option(String),
    tokens: domain.TokenTotals,
    final_issue: Option(domain.Issue),
  )
}

pub type PiUpdate {
  PiUpdate(
    event: pi_event.PiEvent,
    message: Option(String),
    raw_json: Option(session_event.RedactedRawJson),
    turn: Option(Int),
    request_id: Option(String),
    method: Option(String),
    pi_session_id: Option(String),
    tokens: domain.TokenTotals,
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
  )
}
