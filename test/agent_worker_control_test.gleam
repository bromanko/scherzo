import birl
import gleam/dict
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/run_attempt as runner
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/path
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import simplifile
import support/test_helpers
import test_async

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

fn issue(state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "worker-control-issue",
    identifier: "ABC-WORKER",
    title: "Worker controls",
    description: Some("Exercise command-aware loop"),
    priority: Some(1),
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn config(
  root: String,
  command: String,
  max_turns: Int,
  ui_policy: config_types.UiRequestPolicy,
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
    control: config_types.ControlConfig(command_timeout_ms: 60_000),
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
      max_sessions_per_issue: 3,
      context_recovery_max_attempts: 1,
      context_recovery_prompt_char_limit: 40_000,
      max_concurrent_agents_by_state: dict.new(),
    ),
    pi: config_types.PiConfig(
      command: command,
      turn_timeout_ms: 5000,
      read_timeout_ms: 100,
      stall_timeout_ms: 1000,
      auto_retry: True,
      ui_request_policy: ui_policy,
      ui_request_timeout_ms: 1000,
      compatibility_probe: False,
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
      completion_states: None,
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
      invalid_workflow_state_target: None,
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
    ui_server: config_types.UiServerConfig(
      enabled: False,
      endpoint: None,
      credential_ref: None,
      daemon_label: None,
      command_bridge_enabled: False,
      heartbeat_interval_ms: 5000,
      state_interval_ms: 5000,
      retry_initial_ms: 500,
      retry_max_ms: 30_000,
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

const external_fixture_timeout_ms = 5000

// Keep active-turn stdout polling short enough that queued operator commands are
// observed promptly, while avoiding a 100ms external shell/JQ RPC deadline that
// flakes under scheduler load.
const external_fixture_read_timeout_ms = 500

fn receive_update_named(
  subject: process.Subject(agent_types.RunnerUpdate),
  name: String,
  attempts: Int,
) -> Result(agent_types.PiUpdate, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 200) {
        Ok(agent_types.RunnerPiUpdate(update)) ->
          case pi_event.to_string(update.event) == name {
            True -> Ok(update)
            False -> receive_update_named(subject, name, attempts - 1)
          }
        Ok(_) -> receive_update_named(subject, name, attempts - 1)
        Error(_) -> receive_update_named(subject, name, attempts - 1)
      }
  }
}

pub fn abort_command_stops_fake_pi_worker_test() {
  let root = "test/tmp/agent-worker-control-abort"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let pi_command =
    "FAKE_PI_ABORTABLE_STALL_MS=500 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let updates = process.new_subject()
  let result_subject = process.new_subject()
  let ready_subject = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let command_subject = process.new_subject()
      process.send(ready_subject, command_subject)
      let cfg = config(root, pi_command, 1, config_types.Cancel)
      let cfg =
        config_types.EffectiveConfig(
          ..cfg,
          pi: config_types.PiConfig(..cfg.pi, read_timeout_ms: 200),
        )
      let result =
        runner.run_attempt_with_commands(
          issue("Todo"),
          None,
          workflow("Do it"),
          cfg,
          tracker_returning(issue("Done")),
          fn(_, update) { process.send(updates, update) },
          command_subject,
        )
      process.send(result_subject, result)
    })
  let assert Ok(command_subject) = process.receive(ready_subject, within: 1000)

  let assert Ok(_) = receive_update_named(updates, "message_update", 50)
  let reply = process.new_subject()
  process.send(command_subject, worker_command.Abort(reply))
  let assert Ok(worker_command.Applied(_)) =
    process.receive(reply, within: 1000)
  let assert Ok(Error(agent_types.WorkerFailure(
    reason: error.OperatorAbort,
    workspace_path: Some(_),
    tokens: _,
    final_issue: None,
  ))) = process.receive(result_subject, within: 2000)
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "prompt")
}

pub fn operator_prompt_queued_during_turn_and_sent_next_turn_test() {
  let root = "test/tmp/agent-worker-control-prompt"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let assert Ok(release_after_message_update) =
    path.absolute(root <> "/release-after-message-update")
  let pi_command =
    "FAKE_PI_AFTER_MESSAGE_UPDATE_RELEASE="
    <> release_after_message_update
    <> " FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let updates = process.new_subject()
  let result_subject = process.new_subject()
  let ready_subject = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let command_subject = process.new_subject()
      process.send(ready_subject, command_subject)
      let cfg = config(root, pi_command, 2, config_types.Cancel)
      let cfg =
        config_types.EffectiveConfig(
          ..cfg,
          pi: config_types.PiConfig(
            ..cfg.pi,
            read_timeout_ms: external_fixture_read_timeout_ms,
            stall_timeout_ms: external_fixture_timeout_ms,
          ),
        )
      let result =
        runner.run_attempt_with_commands(
          issue("Todo"),
          None,
          workflow("Original task"),
          cfg,
          tracker_returning(issue("Todo")),
          fn(_, update) { process.send(updates, update) },
          command_subject,
        )
      process.send(result_subject, result)
    })
  let command_subject =
    test_async.expect_message_within(ready_subject, external_fixture_timeout_ms)

  let assert Ok(_) = receive_update_named(updates, "message_update", 50)
  let reply = process.new_subject()
  process.send(
    command_subject,
    worker_command.QueuePrompt("operator follow-up", reply),
  )
  let assert worker_command.Queued(_) =
    test_async.expect_message_within(reply, external_fixture_timeout_ms)
  let assert Ok(_) = receive_update_named(updates, "operator_prompt_queued", 50)
  let assert Ok(Nil) = simplifile.write(release_after_message_update, "release")
  let assert Ok(success) =
    test_async.expect_message_within(
      result_subject,
      external_fixture_timeout_ms,
    )
  assert success.turns == 2
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "Original task")
  assert string.contains(contents, "operator follow-up")
  assert !string.contains(contents, "Continue working on ABC-WORKER")
}

pub fn operator_ui_request_timeout_cancels_before_read_timeout_test() {
  let root = "test/tmp/agent-worker-control-ui-timeout"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let pi_command =
    "FAKE_PI_UI_DIALOG=1 FAKE_PI_UI_DIALOG_WAITS=1 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let updates = process.new_subject()
  let result_subject = process.new_subject()
  let ready_subject = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let command_subject = process.new_subject()
      let cfg = config(root, pi_command, 1, config_types.Operator)
      let cfg =
        config_types.EffectiveConfig(
          ..cfg,
          pi: config_types.PiConfig(
            ..cfg.pi,
            read_timeout_ms: 1000,
            ui_request_timeout_ms: 50,
          ),
        )
      let result =
        runner.run_attempt_with_command_ready(
          issue("Todo"),
          None,
          workflow("Original task"),
          cfg,
          tracker_returning(issue("Done")),
          fn(_, update) { process.send(updates, update) },
          command_subject,
          fn() { process.send(ready_subject, Nil) },
        )
      process.send(result_subject, result)
    })
  let _ =
    test_async.expect_message_within(ready_subject, external_fixture_timeout_ms)

  let assert Ok(_) = receive_update_named(updates, "extension_ui_request", 50)
  let assert Ok(timeout_update) =
    receive_update_named(updates, "operator_ui_timeout", 50)
  assert timeout_update.request_id == Some("ui-1")
  let assert Ok(Ok(success)) =
    process.receive(result_subject, within: external_fixture_timeout_ms)
  assert success.final_classification == agent_types.FinalTerminal
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "extension_ui_response")
  assert string.contains(contents, "cancelled")
}

pub fn operator_ui_request_cancel_response_test() {
  let root = "test/tmp/agent-worker-control-ui"
  test_helpers.reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  // Exercise the UI path with a fixture response delay longer than the old
  // 100ms test read timeout, matching the scheduler-load failure mode.
  let pi_command =
    "FAKE_PI_UI_DIALOG=1 FAKE_PI_UI_DIALOG_WAITS=1 FAKE_PI_DELAY_MS=150 FAKE_PI_TRANSCRIPT="
    <> transcript
    <> " "
    <> fake_pi()
  let updates = process.new_subject()
  let result_subject = process.new_subject()
  let ready_subject = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let command_subject = process.new_subject()
      let cfg = config(root, pi_command, 1, config_types.Operator)
      let cfg =
        config_types.EffectiveConfig(
          ..cfg,
          pi: config_types.PiConfig(
            ..cfg.pi,
            read_timeout_ms: external_fixture_timeout_ms,
            ui_request_timeout_ms: 2000,
          ),
        )
      let result =
        runner.run_attempt_with_command_ready(
          issue("Todo"),
          None,
          workflow("Original task"),
          cfg,
          tracker_returning(issue("Done")),
          fn(_, update) { process.send(updates, update) },
          command_subject,
          fn() { process.send(ready_subject, command_subject) },
        )
      process.send(result_subject, result)
    })
  let command_subject =
    test_async.expect_message_within(ready_subject, external_fixture_timeout_ms)

  let assert Ok(ui_update) =
    receive_update_named(updates, "extension_ui_request", 50)
  assert ui_update.request_id == Some("ui-1")
  assert ui_update.message == Some("continue?")
  let reply = process.new_subject()
  process.send(
    command_subject,
    worker_command.RespondToUi("ui-1", command.UiCancel, reply),
  )
  let assert worker_command.Applied(_) =
    test_async.expect_message_within(reply, external_fixture_timeout_ms)
  let assert Ok(Ok(success)) =
    process.receive(result_subject, within: external_fixture_timeout_ms)
  assert success.final_classification == agent_types.FinalTerminal
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "extension_ui_response")
  assert string.contains(contents, "cancelled")
}
