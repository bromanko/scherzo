import gleam/option.{type Option}
import scherzo/agent/pi_event
import scherzo/error
import scherzo/log
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

pub fn without_message_progress(
  emit_update: fn(String, RunnerUpdate) -> Nil,
) -> fn(String, RunnerUpdate) -> Nil {
  fn(issue_id, update) {
    case update {
      RunnerPiUpdate(PiUpdate(event: pi_event.MessageProgress, ..)) -> Nil
      _ -> emit_update(issue_id, update)
    }
  }
}

pub fn redact_runner_update(
  update: RunnerUpdate,
  secrets: List(String),
) -> RunnerUpdate {
  case update {
    RunnerTurnUpdate(_) -> update
    RunnerPiUpdate(pi_update) ->
      RunnerPiUpdate(redact_pi_update(pi_update, secrets))
  }
}

fn redact_pi_update(update: PiUpdate, secrets: List(String)) -> PiUpdate {
  PiUpdate(
    ..update,
    message: redact_optional(update.message, secrets),
    raw_json: redact_raw_json(update.raw_json, secrets),
    tool_input: redact_optional(update.tool_input, secrets),
    tool_output: redact_optional(update.tool_output, secrets),
  )
}

fn redact_optional(
  value: Option(String),
  secrets: List(String),
) -> Option(String) {
  option.map(value, fn(value) { log.redact("runner_update", value, secrets) })
}

fn redact_raw_json(
  value: Option(session_event.RedactedRawJson),
  secrets: List(String),
) -> Option(session_event.RedactedRawJson) {
  option.map(value, fn(raw) {
    let session_event.RedactedRawJson(value: value, truncated: truncated) = raw
    session_event.RedactedRawJson(
      value: log.redact("runner_update_raw_json", value, secrets),
      truncated: truncated,
    )
  })
}
