import gleam/option.{type Option}
import scherzo/agent/pi_event
import scherzo/error
import scherzo/result_artifact
import scherzo/session/event as session_event
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue
import scherzo/turn_telemetry

pub type FinalClassification {
  FinalActive
  FinalTerminal
  FinalNonActive
}

pub type WorkerSuccess {
  WorkerSuccess(
    final_issue: Option(tracker_issue.Issue),
    final_classification: FinalClassification,
    workspace_path: String,
    tokens: session_tokens.TokenTotals,
    turns: Int,
    result: result_artifact.ResultArtifact,
  )
}

pub type WorkerFailure {
  WorkerFailure(
    reason: error.AgentRunnerError,
    workspace_path: Option(String),
    tokens: session_tokens.TokenTotals,
    final_issue: Option(tracker_issue.Issue),
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
    tokens: session_tokens.TokenTotals,
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
  )
}

pub type RunnerUpdate {
  RunnerPiUpdate(PiUpdate)
  RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate)
}
