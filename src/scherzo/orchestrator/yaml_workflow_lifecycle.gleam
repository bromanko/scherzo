import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/daemon_capabilities
import scherzo/orchestrator/event_publisher
import scherzo/orchestrator/schedule_core
import scherzo/orchestrator/yaml_step_session
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/session/name as session_name
import scherzo/session/reason as session_reason
import scherzo/session/recovery as session_recovery
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_run

pub type LifecycleCallbacks {
  LifecycleCallbacks(
    step_started: fn(String, String, String, String, Int) -> Nil,
    step_update: fn(String, agent_types.RunnerUpdate) -> Nil,
    step_command_ready: fn(String, process.Subject(worker_command.Command)) ->
      Nil,
    step_finished: fn(String, session_tokens.TokenTotals) -> Nil,
  )
}

pub fn scheduled_workflow_dependencies(
  base: workflow_run.Dependencies,
  scheduled: schedule_core.ScheduledRunContext,
  callbacks: LifecycleCallbacks,
  capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
) -> workflow_run.Dependencies {
  workflow_dependencies(
    base,
    scheduled_session_issue(scheduled),
    scheduled.run_id,
    scheduled_session_id(scheduled.run_id, scheduled.attempt),
    callbacks,
    capabilities,
  )
}

pub fn workflow_dependencies(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  parent_session_id: String,
  callbacks: LifecycleCallbacks,
  capabilities: daemon_capabilities.DaemonCapabilities(state, message, timer),
) -> workflow_run.Dependencies {
  let event_hub =
    daemon_capabilities.event_hub(daemon_capabilities.daemon_events(
      capabilities,
    ))
  let now_ms = fn() {
    daemon_capabilities.now_ms(daemon_capabilities.daemon_clock(capabilities))
  }
  workflow_run.Dependencies(
    ..base,
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      run_command_step(
        base,
        issue,
        run_id,
        parent_session_id,
        context,
        command,
        timeout_ms,
        secrets,
        limits,
        callbacks,
        event_hub,
        now_ms,
      )
    },
    agent_step: fn(
      issue,
      context,
      prompt_mode,
      attempt_context,
      effective,
      tracker_client,
      _emit_update,
      _command_ready,
      record_pi_session,
    ) {
      run_agent_step(
        base,
        issue,
        run_id,
        parent_session_id,
        context,
        prompt_mode,
        attempt_context,
        effective,
        tracker_client,
        callbacks,
        event_hub,
        now_ms,
        record_pi_session,
      )
    },
  )
}

pub fn register_step_session(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  issue: tracker_issue.Issue,
  workspace_path: String,
  step_id: String,
  attempt_index: Int,
  now_ms: fn() -> Int,
) -> Nil {
  register_step_session_with_recovery(
    event_hub,
    session_id,
    issue,
    workspace_path,
    step_id,
    attempt_index,
    now_ms,
    recovery: None,
  )
}

fn register_workflow_step_session(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  issue: tracker_issue.Issue,
  workspace_path: String,
  run_id: String,
  parent_session_id: String,
  recovery_step_id: String,
  display_step_id: String,
  attempt_index: Int,
  now_ms: fn() -> Int,
) -> Nil {
  register_step_session_with_recovery(
    event_hub,
    session_id,
    issue,
    workspace_path,
    display_step_id,
    attempt_index,
    now_ms,
    recovery: Some(workflow_child_session_info(
      run_id,
      parent_session_id,
      recovery_step_id,
      attempt_index,
      Some(issue_state.to_string(issue.state)),
    )),
  )
}

fn register_step_session_with_recovery(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  issue: tracker_issue.Issue,
  workspace_path: String,
  step_id: String,
  attempt_index: Int,
  now_ms: fn() -> Int,
  recovery recovery: Option(session_event.RecoveryInfo),
) -> Nil {
  let started_at_ms = now_ms()
  hub.register_session(
    event_hub,
    session_event.SessionSummary(
      session_id: session_id,
      display_name: session_name.generate(issue.identifier, session_id),
      issue_id: issue.id,
      issue_identifier: issue.identifier,
      issue_title: issue.title,
      workspace_path: workspace_path,
      pi_session_id: None,
      status: session_event.Preparing,
      recovery: recovery,
      current_turn: 0,
      current_turn_status: None,
      current_turn_started_at_ms: None,
      last_turn_finished_at_ms: None,
      last_turn_duration_ms: None,
      last_turn_token_delta: session_tokens.zero_token_totals(),
      last_turn_reason: None,
      started_at_ms: started_at_ms,
      last_event_at_ms: started_at_ms,
      token_totals: session_tokens.zero_token_totals(),
    ),
  )
  hub.update_status(event_hub, session_id, session_event.Running)
  hub.publish(
    event_hub,
    session_id,
    session_event.lifecycle_payload(
      session_event.StepStarted,
      Some(step_id <> " attempt " <> int.to_string(attempt_index)),
      None,
    ),
  )
}

fn workflow_child_session_info(
  run_id: String,
  parent_session_id: String,
  step_id: String,
  attempt_index: Int,
  issue_state_name: Option(String),
) -> session_event.RecoveryInfo {
  session_event.RecoveryInfo(
    ..session_recovery.base_info(
      session_event.Resumed,
      "workflow.yaml_step_child",
      Some("active workflow child step is linked to parent workflow run"),
      [],
    ),
    workflow_run_id: Some(run_id),
    workflow_step_id: Some(step_id),
    workflow_attempt_index: Some(attempt_index),
    parent_session_id: Some(parent_session_id),
    issue_state: issue_state_name,
    recommended_action: Some("inspect_parent_run"),
  )
}

pub fn run_command_step(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  parent_session_id: String,
  context: workflow_run.StepContext,
  command: String,
  timeout_ms: Int,
  secrets: List(String),
  limits: config_types.ArtifactLimits,
  callbacks: LifecycleCallbacks,
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
) -> step_artifact.StepArtifact {
  let session_id =
    yaml_step_session.id(run_id, context.step_id, context.attempt_index)
  register_workflow_step_session(
    event_hub,
    session_id,
    issue,
    context.workspace_path,
    run_id,
    parent_session_id,
    context.step_id,
    context.step_id,
    context.attempt_index,
    now_ms,
  )
  callbacks.step_started(
    session_id,
    run_id,
    context.workflow_id,
    context.step_id,
    context.attempt_index,
  )
  let artifact =
    base.command_step(context, command, timeout_ms, secrets, limits)
  case step_artifact.succeeded(artifact.status) {
    True -> Nil
    False -> publish_command_failure(event_hub, session_id, artifact)
  }
  let reason = case step_artifact.succeeded(artifact.status) {
    True -> session_reason.Normal
    False -> session_reason.Failed
  }
  hub.finish_session(event_hub, session_id, reason)
  callbacks.step_finished(session_id, session_tokens.zero_token_totals())
  artifact
}

pub fn publish_command_failure(
  event_hub: process.Subject(hub.Message),
  session_id: String,
  artifact: step_artifact.StepArtifact,
) -> Nil {
  let summary = case step_artifact.command_failure_summary(artifact) {
    Some(summary) -> summary
    None -> "command step failed: step=" <> artifact.step_id
  }
  hub.publish(
    event_hub,
    session_id,
    session_event.error_payload(
      "command_failed",
      Some(summary),
      Some("workflow command " <> artifact.step_id),
      artifact.command,
      Some(step_artifact.command_failure_details(artifact)),
      Some("failed"),
    ),
  )
}

pub fn run_agent_step(
  base: workflow_run.Dependencies,
  issue: tracker_issue.Issue,
  run_id: String,
  parent_session_id: String,
  context: workflow_run.StepContext,
  prompt_mode: workflow_attempt.AgentPromptMode,
  attempt_context: workflow_attempt.StepAttemptContext,
  effective: config_types.EffectiveConfig,
  tracker_client: tracker.Client,
  callbacks: LifecycleCallbacks,
  event_hub: process.Subject(hub.Message),
  now_ms: fn() -> Int,
  record_pi_session: fn(workflow_attempt.PiSessionObservation) -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let session_step_id = case prompt_mode {
    workflow_attempt.StructuredOutputRetryPrompt(_) ->
      context.step_id <> "_structured_output_retry"
    _ -> context.step_id
  }
  let session_id =
    yaml_step_session.id(run_id, session_step_id, context.attempt_index)
  register_workflow_step_session(
    event_hub,
    session_id,
    issue,
    context.workspace_path,
    run_id,
    parent_session_id,
    session_step_id,
    context.step_id,
    context.attempt_index,
    now_ms,
  )
  callbacks.step_started(
    session_id,
    run_id,
    context.workflow_id,
    context.step_id,
    context.attempt_index,
  )
  let result =
    base.agent_step(
      issue,
      context,
      prompt_mode,
      attempt_context,
      effective,
      tracker_client,
      fn(update) { callbacks.step_update(session_id, update) },
      fn(command_subject) {
        callbacks.step_command_ready(session_id, command_subject)
      },
      record_pi_session,
    )
  case result {
    Ok(success) -> {
      hub.update_tokens(event_hub, session_id, success.tokens)
      hub.finish_session(event_hub, session_id, session_reason.Normal)
    }
    Error(failure) -> {
      case event_publisher.tokens_are_nonzero(failure.tokens) {
        True -> hub.update_tokens(event_hub, session_id, failure.tokens)
        False -> Nil
      }
      hub.finish_session(event_hub, session_id, session_reason.Failed)
    }
  }
  callbacks.step_finished(session_id, tokens_for_agent_step_result(result))
  result
}

fn tokens_for_agent_step_result(
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> session_tokens.TokenTotals {
  case result {
    Ok(success) -> success.tokens
    Error(failure) -> failure.tokens
  }
}

pub fn worker_failure(
  reason: String,
  workspace_path: Option(String),
  issue: tracker_issue.Issue,
) -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError(reason)),
    workspace_path: workspace_path,
    tokens: session_tokens.zero_token_totals(),
    final_issue: Some(issue),
  )
}

pub fn workflow_failure(
  failure: workflow_run.WorkflowRunFailure,
  issue: tracker_issue.Issue,
) -> agent_types.WorkerFailure {
  let report = workflow_run.failure_report(failure)
  case workflow_run.failed_command_failure(failure) {
    Some(#(code, step_id)) ->
      agent_types.WorkerFailure(
        reason: error.WorkflowCommandFailed(
          code: code,
          step_id: step_id,
          detail: report,
        ),
        workspace_path: failure.run_root,
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      )
    None ->
      case failure.agent_reason {
        Some(reason) ->
          agent_types.WorkerFailure(
            reason: reason,
            workspace_path: failure.run_root,
            tokens: session_tokens.zero_token_totals(),
            final_issue: Some(issue),
          )
        None -> worker_failure(report, failure.run_root, issue)
      }
  }
}

fn scheduled_session_id(run_id: String, attempt: Int) -> String {
  run_id <> "-a" <> int.to_string(attempt)
}

fn scheduled_session_issue(
  scheduled: schedule_core.ScheduledRunContext,
) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "",
    identifier: "scheduled-" <> scheduled.job_id,
    title: "Scheduled job " <> scheduled.job_id,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("scheduled"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}
