import gleam/erlang/process
import gleam/option.{type Option}
import scherzo/agent/run_attempt
import scherzo/agent/types
import scherzo/agent/worker_command
import scherzo/domain
import scherzo/tracker

pub type FinalClassification =
  types.FinalClassification

pub type WorkerSuccess =
  types.WorkerSuccess

pub type WorkerFailure =
  types.WorkerFailure

pub type PiUpdate =
  types.PiUpdate

pub fn run_attempt(
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  issue: domain.Issue,
  attempt: Option(Int),
  prompt_template: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
  issue: domain.Issue,
  prompt: String,
  config: domain.EffectiveConfig,
  tracker_client: tracker.Client,
  emit_update: fn(String, types.PiUpdate) -> Nil,
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
