import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/run_attempt
import scherzo/agent/runner
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/event_publisher
import scherzo/path
import scherzo/session/event
import scherzo/session/tokens as session_tokens
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/turn_telemetry
import scherzo/workflow_attempt
import simplifile
import test_async

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

fn issue(state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Fix tests",
    description: Some("Broken"),
    priority: Some(1),
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: ["bug"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn config(
  root: String,
  command: String,
  probe: Bool,
  max_turns: Int,
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "endpoint",
      api_key: Some("key"),
      project_slug: Some("PROJ"),
      active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config_types.PollingConfig(interval_ms: 30_000),
    workspace: config_types.WorkspaceConfig(root: root),
    hooks: config_types.HooksConfig(
      after_create: Some("printf populated > POPULATED"),
      before_run: Some("test -f POPULATED"),
      after_run: Some("printf after > AFTER_RUN"),
      before_remove: None,
      timeout_ms: 2000,
    ),
    agent: config_types.AgentConfig(
      max_concurrent_agents: 1,
      max_turns: max_turns,
      max_retry_backoff_ms: 300_000,
      max_retry_attempts: 5,
      max_sessions_per_issue: 3,
      context_recovery_max_attempts: 1,
      context_recovery_prompt_char_limit: 40_000,
      max_concurrent_agents_by_state: dict.new(),
    ),
    pi: config_types.PiConfig(
      command: command,
      turn_timeout_ms: 5000,
      read_timeout_ms: 1000,
      stall_timeout_ms: 300_000,
      auto_retry: True,
      ui_request_policy: config_types.Cancel,
      ui_request_timeout_ms: 300_000,
      compatibility_probe: probe,
      rate_limit_payload: None,
      argv_command: None,
      session_persistence: config_types.PiSessionPersistenceConfig(
        enabled: False,
        recovery_prompt: "",
      ),
    ),
    handoff: config_types.HandoffConfig(
      enabled: False,
      comment_on_claim: False,
      comment_on_success: False,
      comment_on_failure: False,
      comment_on_park: False,
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
      include_result_on_success: False,
      attach_result_on_success: False,
      attachment_fallback_to_markdown_link: True,
      result_max_chars: 8000,
    ),
    linear_contract: config_types.LinearContractConfig(
      enabled: False,
      workflow_label_prefix: "workflow:",
      workflow_labels: [],
      support_labels: [],
      required_states: dict.new(),
      handoff_state_bindings: dict.new(),
      enforce_issue_workflow_labels: False,
      invalid_workflow_state_id: None,
      comment_on_invalid_workflow: False,
    ),
    linear_commands: config_types.LinearCommandConfig(
      enabled: False,
      prefix: "/scherzo",
      authorized_user_ids: [],
      poll_limit_per_issue: 25,
      max_comments_per_tick: 50,
      acknowledge_success: True,
      acknowledge_rejection: True,
    ),
  )
}

fn persistent_config(
  root: String,
  argv_env: List(#(String, String)),
) -> config_types.EffectiveConfig {
  let base = config(root, "unused-pi-command", False, 1)
  config_types.EffectiveConfig(
    ..base,
    pi: config_types.PiConfig(
      ..base.pi,
      compatibility_probe: False,
      argv_command: Some(config_types.PiArgvCommand(
        executable: fake_pi(),
        args: ["--mode", "rpc"],
        env: argv_env,
      )),
      session_persistence: config_types.PiSessionPersistenceConfig(
        enabled: True,
        recovery_prompt: "RECOVERY",
      ),
    ),
  )
}

fn workflow(prompt: String) -> String {
  prompt
}

fn tracker_returning(final_issue: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([final_issue]) },
  )
}

fn tracker_failing_state_refresh() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) {
      Error(error.LinearApiRequest("state refresh failed"))
    },
  )
}

fn emit(_issue_id: String, _update: agent_types.RunnerUpdate) -> Nil {
  Nil
}

fn drain_updates(
  subject: process.Subject(agent_types.RunnerUpdate),
  acc: List(agent_types.RunnerUpdate),
) -> List(agent_types.RunnerUpdate) {
  case process.receive(subject, within: 10) {
    Ok(update) -> drain_updates(subject, [update, ..acc])
    Error(_) -> list.reverse(acc)
  }
}

fn find_update(
  updates: List(agent_types.RunnerUpdate),
  name: String,
) -> Option(agent_types.PiUpdate) {
  case updates {
    [] -> None
    [agent_types.RunnerPiUpdate(update), ..rest] ->
      case pi_event.to_string(update.event) == name {
        True -> Some(update)
        False -> find_update(rest, name)
      }
    [_, ..rest] -> find_update(rest, name)
  }
}

fn find_update_with_tool_input(
  updates: List(agent_types.RunnerUpdate),
) -> Option(agent_types.PiUpdate) {
  case updates {
    [] -> None
    [agent_types.RunnerPiUpdate(update), ..rest] ->
      case update.tool_input {
        Some(_) -> Some(update)
        None -> find_update_with_tool_input(rest)
      }
    [_, ..rest] -> find_update_with_tool_input(rest)
  }
}

fn find_update_with_tool_output(
  updates: List(agent_types.RunnerUpdate),
) -> Option(agent_types.PiUpdate) {
  case updates {
    [] -> None
    [agent_types.RunnerPiUpdate(update), ..rest] ->
      case update.tool_output {
        Some(_) -> Some(update)
        None -> find_update_with_tool_output(rest)
      }
    [_, ..rest] -> find_update_with_tool_output(rest)
  }
}

fn occurrence_count(text: String, needle: String) -> Int {
  string.split(text, needle) |> list.length |> subtract_one
}

fn subtract_one(value: Int) -> Int {
  value - 1
}

fn turn_event_names(updates: List(agent_types.RunnerUpdate)) -> List(String) {
  updates
  |> list.filter_map(fn(update) {
    case update {
      agent_types.RunnerTurnUpdate(update) ->
        Ok(turn_telemetry.event_name_to_string(update.name))
      agent_types.RunnerPiUpdate(_) -> Error(Nil)
    }
  })
}

fn receive_update_named(
  subject: process.Subject(agent_types.RunnerUpdate),
  name: String,
  attempts: Int,
) -> Result(agent_types.PiUpdate, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 500) {
        Ok(agent_types.RunnerPiUpdate(update)) ->
          case pi_event.to_string(update.event) == name {
            True -> Ok(update)
            False -> receive_update_named(subject, name, attempts - 1)
          }
        Ok(_) -> receive_update_named(subject, name, attempts - 1)
        Error(_) -> Error(Nil)
      }
  }
}

pub fn turn_update_helpers_emit_sanitized_runner_updates_test() {
  let started = runner.turn_started_update(3)
  let assert agent_types.RunnerTurnUpdate(started_update) = started
  assert started_update.name == turn_telemetry.EventStarted
  assert started_update.turn == 3
  assert started_update.tokens == session_tokens.zero_token_totals()
  let started_payload = event_publisher.turn_update_payload(started_update)
  assert started_payload.kind == event.Turn
  assert started_payload.message == None
  assert started_payload.raw_json == None
  assert started_payload.tool_input == None

  let totals =
    session_tokens.TokenTotals(
      input: 10,
      output: 5,
      cache_read: 0,
      cache_write: 0,
      total: 15,
    )
  let assert agent_types.RunnerTurnUpdate(finished_update) =
    runner.turn_finished_update(3, totals)
  assert finished_update.name == turn_telemetry.EventFinished
  assert finished_update.tokens == totals

  let assert agent_types.RunnerTurnUpdate(stopped_update) =
    runner.turn_stopped_update(
      3,
      turn_telemetry.ReasonOperatorStopAfterCurrentTurn,
      totals,
    )
  assert stopped_update.reason
    == Some(turn_telemetry.ReasonOperatorStopAfterCurrentTurn)
  let assert agent_types.RunnerTurnUpdate(timeout_update) =
    runner.turn_timed_out_update(3, turn_telemetry.ReasonPiStallTimeout, totals)
  assert timeout_update.reason == Some(turn_telemetry.ReasonPiStallTimeout)
  let assert agent_types.RunnerTurnUpdate(failed_update) =
    runner.turn_failed_update(3, turn_telemetry.ReasonPiError, totals)
  assert failed_update.reason == Some(turn_telemetry.ReasonPiError)
}

pub fn state_refresh_failure_emits_failed_turn_without_finished_first_test() {
  let root = "test/tmp/runner-state-refresh-failure"
  reset_dir(root)
  let command = fake_pi()
  let cfg = config(root, command, False, 1)
  let update_subject = process.new_subject()

  let assert Error(failure) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Work on {{ issue.identifier }} {{ issue.title }}"),
      cfg,
      tracker_failing_state_refresh(),
      fn(_, update) { process.send(update_subject, update) },
    )

  assert failure.tokens.total == 3
  assert turn_event_names(drain_updates(update_subject, []))
    == ["turn_started", "turn_failed"]
}

pub fn successful_runner_probes_prompts_and_returns_terminal_state_test() {
  let root = "test/tmp/runner-success"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let cfg = config(root, command, True, 3)
  let update_subject = process.new_subject()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Work on {{ issue.identifier }} {{ issue.title }}"),
      cfg,
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )
  assert success.final_classification == agent_types.FinalTerminal
  assert success.tokens.total == 3
  assert success.result.final_response == Some("done")
  assert success.result.source == "completed_assistant_messages"
  assert turn_event_names(drain_updates(update_subject, []))
    == ["turn_started", "turn_finished"]
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "set_session_name")
  assert string.contains(contents, "get_state")
  assert string.contains(contents, "Work on ABC-123 Fix tests")
  let assert Ok(True) =
    simplifile.is_file(success.workspace_path <> "/POPULATED")
  let assert Ok(True) =
    simplifile.is_file(success.workspace_path <> "/AFTER_RUN")
}

pub fn runner_fails_when_pi_reports_stop_reason_error_test() {
  let root = "test/tmp/runner-stop-reason-error"
  reset_dir(root)
  let command = "FAKE_PI_STOP_REASON_ERROR=1 " <> fake_pi()
  let update_subject = process.new_subject()

  let assert Error(failure) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )

  assert failure.reason
    == error.PiFailed(error.PiProtocolError(
      "pi turn_end reported stopReason=error: terminated",
    ))
  assert turn_event_names(drain_updates(update_subject, []))
    == ["turn_started", "turn_failed"]
}

pub fn runner_recovers_context_exhaustion_with_pi_compaction_test() {
  let root = "test/tmp/runner-context-recovery-compact"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command =
    "FAKE_PI_CONTEXT_ERROR_ONCE=1 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let update_subject = process.new_subject()

  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it with SECRET_VALUE"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )

  assert success.final_classification == agent_types.FinalTerminal
  let updates = drain_updates(update_subject, [])
  let assert Some(_) = find_update(updates, "context_recovery_started")
  let assert Some(_) = find_update(updates, "context_recovery_succeeded")
  assert turn_event_names(updates)
    == ["turn_started", "turn_started", "turn_finished"]
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "compact")
  assert string.contains(contents, "attempt-1-prompt-excerpt.md")
  assert string.contains(contents, "context-window-exhausted.json")
}

pub fn runner_stops_after_repeated_context_exhaustion_test() {
  let root = "test/tmp/runner-context-recovery-exhausted"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command =
    "FAKE_PI_CONTEXT_ERROR_ALWAYS=1 FAKE_PI_COMPACT_FAIL=1 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let update_subject = process.new_subject()

  let assert Error(failure) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )

  let assert error.ContextRecoveryExhausted(
    recovery_method: "fresh_session",
    context_artifact_ref: Some(context_ref),
    result_artifact_ref: Some(result_ref),
    final_error: error.PiContextWindowExhausted(
      provider: Some("openai-codex"),
      provider_code: Some("context_length_exceeded"),
      detail: "Your input exceeds the context window of this model. Please adjust your input and try again.",
    ),
  ) = failure.reason
  let updates = drain_updates(update_subject, [])
  let assert Some(failed_update) =
    find_update(updates, "context_recovery_failed")
  let assert Some(failed_message) = failed_update.message
  assert string.contains(failed_message, "attempted but exhausted")
  assert string.contains(failed_message, "recovery_method=fresh_session")
  assert string.contains(failed_message, "terminal_diagnostics=")
  let assert Ok(context_contents) =
    simplifile.read(root <> "/.scherzo-state/artifacts/" <> context_ref)
  assert string.contains(context_contents, "\"recovery_exhausted\":true")
  assert string.contains(context_contents, "\"budget_exhausted\":true")
  assert string.contains(
    context_contents,
    "\"terminal_recovery_method\":\"fresh_session\"",
  )
  assert string.contains(context_contents, "\"final_failure\"")
  assert string.contains(context_contents, "\"provider\":\"openai-codex\"")
  assert string.contains(
    context_contents,
    "\"provider_code\":\"context_length_exceeded\"",
  )
  let assert Ok(result_contents) =
    simplifile.read(root <> "/.scherzo-state/artifacts/" <> result_ref)
  assert string.contains(result_contents, "\"outcome\":\"failed\"")
  assert string.contains(result_contents, "\"recovery_exhausted\":true")
  assert string.contains(
    result_contents,
    "\"recovery_method\":\"fresh_session\"",
  )
  assert string.contains(result_contents, "\"final_failure\"")
  let assert Ok(contents) = simplifile.read(transcript)
  assert occurrence_count(contents, "\"type\":\"prompt\"") == 2
}

pub fn runner_context_recovery_can_be_disabled_test() {
  let root = "test/tmp/runner-context-recovery-disabled"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command =
    "FAKE_PI_CONTEXT_ERROR_ONCE=1 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let base = config(root, command, False, 1)
  let cfg =
    config_types.EffectiveConfig(
      ..base,
      agent: config_types.AgentConfig(
        ..base.agent,
        context_recovery_max_attempts: 0,
      ),
    )

  let assert Error(failure) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      cfg,
      tracker_returning(issue("Done")),
      emit,
    )

  let assert error.PiFailed(error.PiContextWindowExhausted(..)) = failure.reason
  let assert Ok(contents) = simplifile.read(transcript)
  assert !string.contains(contents, "compact")
  assert occurrence_count(contents, "\"type\":\"prompt\"") == 1
}

pub fn recovery_prompt_reopens_recorded_session_without_original_prompt_test() {
  let root = "test/tmp/runner-recovery-prompt"
  reset_dir(root)
  let workspace = root <> "/workspace"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(abs_workspace) = path.absolute(workspace)
  let assert Ok(transcript) = path.absolute(root <> "/transcript.jsonl")
  let assert Ok(argv_log) = path.absolute(root <> "/argv.log")
  let session_file = abs_workspace <> "/captured.pi-session"
  let cfg =
    persistent_config(root, [
      #("FAKE_PI_TRANSCRIPT", transcript),
      #("FAKE_PI_ARGV_LOG", argv_log),
    ])
  let context =
    workflow_attempt.StepAttemptContext(
      run_id: "run-1",
      issue_id: "issue-id",
      issue_identifier: "ABC-123",
      workflow_id: "implementation",
      workflow_fingerprint: "wf-sha",
      step_id: "apply_feedback",
      workspace_name: "main",
      attempt_index: 1,
      workspace_path: abs_workspace,
      continuation_capable: True,
      continuation_session_file: Some(session_file),
    )

  let assert Ok(success) =
    run_attempt.run_prompt_mode_in_workspace(
      issue("Todo"),
      workflow_attempt.RecoveryPrompt("RECOVERY_PROMPT_MARKER"),
      context,
      cfg,
      tracker_returning(issue("Done")),
      emit,
      process.new_subject(),
      fn() { Nil },
      abs_workspace,
      fn(_) { Nil },
    )

  assert success.final_classification == agent_types.FinalTerminal
  let assert Ok(argv_contents) = simplifile.read(argv_log)
  assert string.contains(argv_contents, "cwd=" <> abs_workspace)
  assert string.contains(argv_contents, "argv[3]=--session")
  assert string.contains(argv_contents, "argv[4]=" <> session_file)
  let assert Ok(transcript_contents) = simplifile.read(transcript)
  assert string.contains(transcript_contents, "RECOVERY_PROMPT_MARKER")
  assert !string.contains(
    transcript_contents,
    "ORIGINAL_PROMPT_SHOULD_NOT_APPEAR",
  )
}

pub fn recovery_resume_validation_failure_returns_specific_failure_before_prompt_test() {
  let root = "test/tmp/runner-recovery-validation-failure"
  reset_dir(root)
  let workspace = root <> "/workspace"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(abs_workspace) = path.absolute(workspace)
  let assert Ok(transcript) = path.absolute(root <> "/transcript.jsonl")
  let session_file = abs_workspace <> "/captured.pi-session"
  let cfg =
    persistent_config(root, [
      #("FAKE_PI_TRANSCRIPT", transcript),
      #("FAKE_PI_SESSION_FILE_MISMATCH", abs_workspace <> "/other.pi-session"),
    ])
  let context =
    workflow_attempt.StepAttemptContext(
      run_id: "run-1",
      issue_id: "issue-id",
      issue_identifier: "ABC-123",
      workflow_id: "implementation",
      workflow_fingerprint: "wf-sha",
      step_id: "apply_feedback",
      workspace_name: "main",
      attempt_index: 1,
      workspace_path: abs_workspace,
      continuation_capable: True,
      continuation_session_file: Some(session_file),
    )

  let assert Error(failure) =
    run_attempt.run_prompt_mode_in_workspace(
      issue("Todo"),
      workflow_attempt.RecoveryPrompt("RECOVERY_PROMPT_MARKER"),
      context,
      cfg,
      tracker_returning(issue("Done")),
      emit,
      process.new_subject(),
      fn() { Nil },
      abs_workspace,
      fn(_) { Nil },
    )

  assert failure.reason
    == error.PiFailed(error.PiProtocolError(
      workflow_attempt.recovery_pi_resume_validation_failed,
    ))
  let assert Ok(transcript_contents) = simplifile.read(transcript)
  assert string.contains(transcript_contents, "get_state")
  assert !string.contains(transcript_contents, "prompt")
  assert !string.contains(transcript_contents, "RECOVERY_PROMPT_MARKER")
}

pub fn cancel_ui_policy_sends_extension_ui_cancel_test() {
  let root = "test/tmp/runner-ui-cancel"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command =
    "FAKE_PI_UI_DIALOG=1 FAKE_PI_UI_DIALOG_WAITS=1 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let update_subject = process.new_subject()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )
  assert success.final_classification == agent_types.FinalTerminal
  let updates = drain_updates(update_subject, [])
  let assert Some(response_update) =
    find_update(updates, "extension_ui_response")
  assert response_update.message == Some("cancelled")
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "extension_ui_response")
  assert string.contains(contents, "cancelled")
}

pub fn worker_success_result_redacts_secret_output_test() {
  let root = "test/tmp/runner-result-redacts"
  reset_dir(root)
  let command =
    "FAKE_PI_MESSAGE_SECRET=key FAKE_PI_NO_AGENT_END_MESSAGES=1 " <> fake_pi()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      emit,
    )
  let assert Some(text) = success.result.final_response
  assert string.contains(text, "[REDACTED]")
  assert !string.contains(text, "key")
  assert success.result.source == "completed_assistant_messages"
}

pub fn worker_success_result_includes_interleaved_skipped_records_test() {
  let root = "test/tmp/runner-result-skipped"
  reset_dir(root)
  let command =
    "FAKE_PI_INTERLEAVE_EVENT_BEFORE_PROMPT_RESPONSE=1 FAKE_PI_NO_AGENT_END_MESSAGES=1 "
    <> fake_pi()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      emit,
    )
  let assert Some(text) = success.result.final_response
  assert !string.contains(text, "interleaved")
  assert string.contains(text, "POPULATED")
}

pub fn prompt_render_failure_aborts_before_pi_launch_test() {
  let root = "test/tmp/runner-render-failure"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Error(agent_types.WorkerFailure(
    reason: _,
    workspace_path: Some(_),
    tokens: _,
    final_issue: _,
  )) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("{{ issue.unknown }}"),
      config(root, command, True, 3),
      tracker_returning(issue("Done")),
      emit,
    )
  let assert Ok(False) = simplifile.is_file(transcript)
}

pub fn before_run_and_probe_failures_abort_before_prompt_test() {
  let root = "test/tmp/runner-before-run-failure"
  reset_dir(root)
  let bad_hooks =
    config_types.HooksConfig(
      after_create: Some("printf populated > POPULATED"),
      before_run: Some("exit 9"),
      after_run: None,
      before_remove: None,
      timeout_ms: 1000,
    )
  let cfg =
    config_types.EffectiveConfig(
      ..config(root, fake_pi(), True, 3),
      hooks: bad_hooks,
    )
  let assert Error(_) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("ok"),
      cfg,
      tracker_returning(issue("Done")),
      emit,
    )

  let root2 = "test/tmp/runner-probe-failure"
  reset_dir(root2)
  let transcript_path = root2 <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command =
    "FAKE_PI_MALFORMED=1 FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Error(agent_types.WorkerFailure(
    reason: _,
    workspace_path: Some(_),
    tokens: _,
    final_issue: _,
  )) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("ok"),
      config(root2, command, True, 3),
      tracker_returning(issue("Done")),
      emit,
    )
  let assert Ok(contents) = simplifile.read(transcript)
  assert !string.contains(contents, "prompt")
}

pub fn runner_update_preserves_redacted_raw_pi_event_test() {
  let root = "test/tmp/runner-update-redaction"
  reset_dir(root)
  let command = "FAKE_PI_MESSAGE_SECRET=key " <> fake_pi()
  let update_subject = process.new_subject()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )
  assert success.final_classification == agent_types.FinalTerminal

  let updates = drain_updates(update_subject, [])
  let assert Some(update) = find_update(updates, "message_update")
  assert update.message == Some("POPULATED [REDACTED]")
  let assert Some(raw_json) = update.raw_json
  assert string.contains(raw_json.value, "message_update")
  assert !string.contains(raw_json.value, "key")
  assert raw_json.truncated == False
}

pub fn runner_redacts_normalized_tool_fields_test() {
  let root = "test/tmp/runner-tool-redaction"
  reset_dir(root)
  let command = "FAKE_PI_TOOL=1 FAKE_PI_TOOL_SECRET=key " <> fake_pi()
  let update_subject = process.new_subject()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Do it"),
      config(root, command, False, 1),
      tracker_returning(issue("Done")),
      fn(_, update) { process.send(update_subject, update) },
    )
  assert success.final_classification == agent_types.FinalTerminal

  let updates = drain_updates(update_subject, [])
  let assert Some(start) = find_update_with_tool_input(updates)
  assert start.event == pi_event.Message
  assert pi_event.to_string(start.event) == "message"
  assert start.tool_input == Some("gleam test [REDACTED]")
  let assert Some(result) = find_update_with_tool_output(updates)
  assert result.event == pi_event.Message
  assert result.tool_output == Some("2 failures [REDACTED]")
  assert result.tool_status == Some("failed")
}

pub fn runner_streams_update_before_agent_end_test() {
  let root = "test/tmp/runner-streaming"
  reset_dir(root)
  let command = "FAKE_PI_STALL_AFTER_PROMPT=1000 " <> fake_pi()
  let update_subject = process.new_subject()
  let finished_subject = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let _ =
        runner.run_attempt(
          issue("Todo"),
          None,
          workflow("Do it"),
          config(root, command, False, 1),
          tracker_returning(issue("Done")),
          fn(_, update) { process.send(update_subject, update) },
        )
      process.send(finished_subject, "finished")
    })

  let assert Ok(update) =
    receive_update_named(update_subject, "message_update", 8)
  assert update.message == Some("POPULATED")
  test_async.assert_no_extra_message_within(finished_subject, 50)
  process.kill(pid)
}

pub fn active_issue_continues_in_same_worker_until_max_turns_test() {
  let root = "test/tmp/runner-continuation"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Original task {{ issue.identifier }}"),
      config(root, command, False, 2),
      tracker_returning(issue("Todo")),
      emit,
    )
  assert success.final_classification == agent_types.FinalActive
  assert success.turns == 2
  assert success.result.final_response == Some("done\n\ndone")
  assert success.result.source == "combined_turns"
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "Original task ABC-123")
  assert string.contains(contents, "Continue working on ABC-123")
}
