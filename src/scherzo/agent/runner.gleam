import gleam/erlang/process
import gleam/option.{type Option}
import scherzo/agent/run_attempt
import scherzo/agent/types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/session/tokens as session_tokens
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/turn_telemetry

pub type FinalClassification =
  types.FinalClassification

pub type WorkerSuccess =
  types.WorkerSuccess

pub type WorkerFailure =
  types.WorkerFailure

pub type PiUpdate =
  types.PiUpdate

pub type RunnerUpdate =
  types.RunnerUpdate

pub fn run_attempt(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  run_attempt.run_attempt(
    issue,
    attempt,
    prompt_template,
    config,
    tracker_client,
    emit_update,
  )
}

pub fn run_attempt_with_commands(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  run_attempt.run_attempt_with_commands(
    issue,
    attempt,
    prompt_template,
    config,
    tracker_client,
    emit_update,
    command_subject,
  )
}

pub fn run_attempt_with_command_ready(
  issue: tracker_issue.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  run_attempt.run_attempt_with_command_ready(
    issue,
    attempt,
    prompt_template,
    config,
    tracker_client,
    emit_update,
    command_subject,
    on_command_ready,
  )
}

pub fn run_prompt_in_workspace(
  issue: tracker_issue.Issue,
  prompt: String,
  config: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.RunnerUpdate) -> Nil,
  command_subject: process.Subject(worker_command.Command),
  on_command_ready: fn() -> Nil,
  workspace_path: String,
) -> Result(types.WorkerSuccess, types.WorkerFailure) {
  run_attempt.run_prompt_in_workspace(
    issue,
    prompt,
    config,
    tracker_client,
    emit_update,
    command_subject,
    on_command_ready,
    workspace_path,
  )
}

pub fn turn_started_update(turn: Int) -> types.RunnerUpdate {
  run_attempt.turn_started_update(turn)
}

pub fn turn_finished_update(
  turn: Int,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  run_attempt.turn_finished_update(turn, totals)
}

pub fn turn_stopped_update(
  turn: Int,
  reason: turn_telemetry.TurnReason,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  run_attempt.turn_stopped_update(turn, reason, totals)
}

pub fn turn_failed_update(
  turn: Int,
  reason: turn_telemetry.TurnReason,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  run_attempt.turn_failed_update(turn, reason, totals)
}

pub fn turn_timed_out_update(
  turn: Int,
  reason: turn_telemetry.TurnReason,
  totals: session_tokens.TokenTotals,
) -> types.RunnerUpdate {
  run_attempt.turn_timed_out_update(turn, reason, totals)
}
