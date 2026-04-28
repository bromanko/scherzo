import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/path
import scherzo/tracker
import simplifile
import yay

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

fn issue(state: String) -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Fix tests",
    description: Some("Broken"),
    priority: Some(1),
    state: state,
    branch_name: None,
    url: None,
    labels: ["bug"],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn config(
  root: String,
  command: String,
  probe: Bool,
  max_turns: Int,
) -> domain.EffectiveConfig {
  domain.EffectiveConfig(
    tracker: domain.TrackerConfig(
      kind: "linear",
      endpoint: "endpoint",
      api_key: Some("key"),
      project_slug: Some("PROJ"),
      active_states: ["Todo", "In Progress"],
      terminal_states: ["Done"],
    ),
    polling: domain.PollingConfig(interval_ms: 30_000),
    workspace: domain.WorkspaceConfig(root: root),
    hooks: domain.HooksConfig(
      after_create: Some("printf populated > POPULATED"),
      before_run: Some("test -f POPULATED"),
      after_run: Some("printf after > AFTER_RUN"),
      before_remove: None,
      timeout_ms: 2000,
    ),
    agent: domain.AgentConfig(
      max_concurrent_agents: 1,
      max_turns: max_turns,
      max_retry_backoff_ms: 300_000,
      max_retry_attempts: 5,
      max_sessions_per_issue: 3,
      max_concurrent_agents_by_state: dict.new(),
    ),
    pi: domain.PiConfig(
      command: command,
      turn_timeout_ms: 5000,
      read_timeout_ms: 1000,
      stall_timeout_ms: 300_000,
      auto_retry: True,
      ui_request_policy: domain.Cancel,
      compatibility_probe: probe,
      rate_limit_payload: None,
    ),
    handoff: domain.HandoffConfig(
      enabled: False,
      comment_on_claim: False,
      comment_on_success: False,
      comment_on_failure: False,
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
    ),
  )
}

fn workflow(prompt: String) -> domain.WorkflowDefinition {
  domain.WorkflowDefinition(config: yay.NodeMap([]), prompt_template: prompt)
}

fn tracker_returning(final_issue: domain.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([final_issue]) },
  )
}

fn emit(_issue_id: String, _update: runner.PiUpdate) -> Nil {
  Nil
}

fn drain_updates(
  subject: process.Subject(runner.PiUpdate),
  acc: List(runner.PiUpdate),
) -> List(runner.PiUpdate) {
  case process.receive(subject, within: 10) {
    Ok(update) -> drain_updates(subject, [update, ..acc])
    Error(_) -> list.reverse(acc)
  }
}

fn find_update(
  updates: List(runner.PiUpdate),
  name: String,
) -> Option(runner.PiUpdate) {
  case updates {
    [] -> None
    [update, ..rest] ->
      case update.event == name {
        True -> Some(update)
        False -> find_update(rest, name)
      }
  }
}

fn receive_update_named(
  subject: process.Subject(runner.PiUpdate),
  name: String,
  attempts: Int,
) -> Result(runner.PiUpdate, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 500) {
        Ok(update) ->
          case update.event == name {
            True -> Ok(update)
            False -> receive_update_named(subject, name, attempts - 1)
          }
        Error(_) -> Error(Nil)
      }
  }
}

pub fn successful_runner_probes_prompts_and_returns_terminal_state_test() {
  let root = "test/tmp/runner-success"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let cfg = config(root, command, True, 3)
  let assert Ok(success) =
    runner.run_attempt(
      issue("Todo"),
      None,
      workflow("Work on {{ issue.identifier }} {{ issue.title }}"),
      cfg,
      tracker_returning(issue("Done")),
      emit,
    )
  assert success.final_classification == runner.FinalTerminal
  assert success.tokens.total == 3
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "set_session_name")
  assert string.contains(contents, "get_state")
  assert string.contains(contents, "Work on ABC-123 Fix tests")
  let assert Ok(True) =
    simplifile.is_file(success.workspace_path <> "/POPULATED")
  let assert Ok(True) =
    simplifile.is_file(success.workspace_path <> "/AFTER_RUN")
}

pub fn prompt_render_failure_aborts_before_pi_launch_test() {
  let root = "test/tmp/runner-render-failure"
  reset_dir(root)
  let transcript_path = root <> "/transcript.jsonl"
  let assert Ok(transcript) = path.absolute(transcript_path)
  let command = "FAKE_PI_TRANSCRIPT=" <> transcript <> " " <> fake_pi()
  let assert Error(runner.WorkerFailure(reason: _, workspace_path: Some(_))) =
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
    domain.HooksConfig(
      after_create: Some("printf populated > POPULATED"),
      before_run: Some("exit 9"),
      after_run: None,
      before_remove: None,
      timeout_ms: 1000,
    )
  let cfg =
    domain.EffectiveConfig(..config(root, fake_pi(), True, 3), hooks: bad_hooks)
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
  let assert Error(runner.WorkerFailure(reason: _, workspace_path: Some(_))) =
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
  assert success.final_classification == runner.FinalTerminal

  let updates = drain_updates(update_subject, [])
  let assert Some(update) = find_update(updates, "message_update")
  assert update.message == Some("POPULATED [REDACTED]")
  let assert Some(raw_json) = update.raw_json
  assert string.contains(raw_json.value, "message_update")
  assert !string.contains(raw_json.value, "key")
  assert raw_json.truncated == False
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
  assert process.receive(finished_subject, within: 50) == Error(Nil)
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
  assert success.final_classification == runner.FinalActive
  assert success.turns == 2
  let assert Ok(contents) = simplifile.read(transcript)
  assert string.contains(contents, "Original task ABC-123")
  assert string.contains(contents, "Continue working on ABC-123")
}
