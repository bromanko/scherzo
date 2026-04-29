import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/agent/worker_command
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/orchestrator/daemon
import scherzo/session/hub
import scherzo/tracker
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn issue(id: String, identifier: String, state: String) -> domain.Issue {
  domain.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: state,
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: None,
    updated_at: None,
  )
}

fn workflow_text(root: String, max_concurrent: Int) -> String {
  "---\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\nworkspace:\n  root: "
  <> root
  <> "\nhooks:\n  before_run: \"true\"\npolling:\n  interval_ms: 1000\nagent:\n  max_concurrent_agents: "
  <> int_to_string(max_concurrent)
  <> "\n  max_retry_attempts: 3\n  max_sessions_per_issue: 3\npi:\n  command: fake\nlinear_commands:\n  enabled: true\n  authorized_user_ids:\n    - user-1\n  poll_limit_per_issue: 10\n  max_comments_per_tick: 10\n  acknowledge_success: true\n  acknowledge_rejection: true\n---\nPrompt\n"
}

fn write_workflow(dir: String, max_concurrent: Int) -> String {
  reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, max_concurrent))
  workflow_path
}

fn linear_comment(
  id: String,
  issue_id: String,
  body: String,
) -> linear.LinearComment {
  linear.LinearComment(
    id: id,
    issue_id: issue_id,
    body: body,
    created_at_ms: 1000,
    updated_at_ms: 1000,
    author: linear.LinearCommentAuthor(
      id: "user-1",
      email: Some("operator@example.com"),
      name: Some("Operator"),
    ),
  )
}

fn tracker_with(candidate: domain.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(ids) {
      case list.contains(ids, candidate.id) {
        True -> Ok([candidate])
        False -> Ok([])
      }
    },
  )
}

type TrackerServerMessage {
  SetTrackerCandidate(domain.Issue)
  FetchTrackerCandidates(
    process.Subject(Result(List(domain.Issue), error.TrackerError)),
  )
  FetchTrackerByIds(
    List(String),
    process.Subject(Result(List(domain.Issue), error.TrackerError)),
  )
}

fn start_tracker_server(
  initial_candidate: domain.Issue,
) -> process.Subject(TrackerServerMessage) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      tracker_server_loop(subject, initial_candidate)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn tracker_server_loop(
  subject: process.Subject(TrackerServerMessage),
  candidate: domain.Issue,
) -> Nil {
  case process.receive(subject, within: 10_000) {
    Ok(SetTrackerCandidate(candidate)) ->
      tracker_server_loop(subject, candidate)
    Ok(FetchTrackerCandidates(reply)) -> {
      process.send(reply, Ok([candidate]))
      tracker_server_loop(subject, candidate)
    }
    Ok(FetchTrackerByIds(ids, reply)) -> {
      case list.contains(ids, candidate.id) {
        True -> process.send(reply, Ok([candidate]))
        False -> process.send(reply, Ok([]))
      }
      tracker_server_loop(subject, candidate)
    }
    Error(_) -> Nil
  }
}

fn dynamic_tracker(
  server: process.Subject(TrackerServerMessage),
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() {
      let reply = process.new_subject()
      process.send(server, FetchTrackerCandidates(reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(ids) {
      let reply = process.new_subject()
      process.send(server, FetchTrackerByIds(ids, reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
  )
}

type LinearServerMessage {
  SetNext(List(linear.LinearComment))
  FetchComments(
    List(String),
    process.Subject(Result(List(linear.LinearComment), error.TrackerError)),
  )
  PostAck(String, process.Subject(Result(Nil, error.TrackerError)))
}

fn start_linear_server(
  fetch_subject: process.Subject(List(String)),
  ack_subject: process.Subject(String),
) -> process.Subject(LinearServerMessage) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      linear_server_loop(subject, fetch_subject, ack_subject, [])
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn linear_server_loop(
  subject: process.Subject(LinearServerMessage),
  fetch_subject: process.Subject(List(String)),
  ack_subject: process.Subject(String),
  queued_batches: List(List(linear.LinearComment)),
) -> Nil {
  case process.receive(subject, within: 10_000) {
    Ok(SetNext(comments)) ->
      linear_server_loop(
        subject,
        fetch_subject,
        ack_subject,
        list.append(queued_batches, [comments]),
      )
    Ok(FetchComments(issue_ids, reply)) -> {
      process.send(fetch_subject, issue_ids)
      let #(comments, queued_batches) = pop_batch(queued_batches)
      process.send(reply, Ok(comments))
      linear_server_loop(subject, fetch_subject, ack_subject, queued_batches)
    }
    Ok(PostAck(body, reply)) -> {
      process.send(ack_subject, body)
      process.send(reply, Ok(Nil))
      linear_server_loop(subject, fetch_subject, ack_subject, queued_batches)
    }
    Error(_) -> Nil
  }
}

fn pop_batch(
  queued_batches: List(List(linear.LinearComment)),
) -> #(List(linear.LinearComment), List(List(linear.LinearComment))) {
  case queued_batches {
    [] -> #([], [])
    [batch, ..rest] -> #(batch, rest)
  }
}

fn linear_client(
  server: process.Subject(LinearServerMessage),
) -> linear.CommandClient {
  linear.CommandClient(
    fetch_comments: fn(issue_ids, _limit) {
      let reply = process.new_subject()
      process.send(server, FetchComments(issue_ids, reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
    post_ack: fn(_issue_id, body) {
      let reply = process.new_subject()
      process.send(server, PostAck(body, reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok(Nil)
      }
    },
  )
}

fn dependencies(
  tracker_client: tracker.Client,
  linear_command_client: linear.CommandClient,
  log_subject: process.Subject(String),
  agent_runner: fn(
    domain.Issue,
    Option(Int),
    domain.WorkflowDefinition,
    domain.EffectiveConfig,
    tracker.Client,
    fn(String, runner.PiUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) ->
    Result(runner.WorkerSuccess, runner.WorkerFailure),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    make_tracker: fn(_) { tracker_client },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    make_linear_commands: fn(_) { linear_command_client },
    agent_runner: agent_runner,
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, fields, _) {
      process.send(log_subject, log_value(event, fields))
      Ok(Nil)
    },
    now_ms: fn() { 1000 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { hub.start(50, fn() { 1000 }) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn log_value(event: String, fields: List(#(String, String))) -> String {
  case event {
    "linear_operator_command" ->
      event <> ":" <> field(fields, "command") <> ":" <> field(fields, "status")
    _ -> event
  }
}

fn field(fields: List(#(String, String)), key: String) -> String {
  case fields {
    [] -> ""
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> value
        False -> field(rest, key)
      }
  }
}

fn unused_agent(
  _issue: domain.Issue,
  _attempt: Option(Int),
  _definition: domain.WorkflowDefinition,
  _effective: domain.EffectiveConfig,
  _tracker_client: tracker.Client,
  _emit_update: fn(String, runner.PiUpdate) -> Nil,
  _command_subject: process.Subject(worker_command.Command),
  _ready: fn() -> Nil,
) -> Result(runner.WorkerSuccess, runner.WorkerFailure) {
  Error(runner.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError("not used")),
    workspace_path: None,
    tokens: domain.zero_token_totals(),
    final_issue: None,
  ))
}

fn prompt_agent(log_subject: process.Subject(String)) {
  fn(
    issue: domain.Issue,
    _attempt: Option(Int),
    _definition: domain.WorkflowDefinition,
    _effective: domain.EffectiveConfig,
    _tracker_client: tracker.Client,
    _emit_update: fn(String, runner.PiUpdate) -> Nil,
    command_subject: process.Subject(worker_command.Command),
    ready: fn() -> Nil,
  ) -> Result(runner.WorkerSuccess, runner.WorkerFailure) {
    ready()
    process.send(log_subject, "agent_running:" <> issue.id)
    case process.receive(command_subject, within: 5000) {
      Ok(worker_command.QueuePrompt(message, reply)) -> {
        process.send(log_subject, "prompt:" <> message)
        process.send(reply, worker_command.Queued(Some("queued")))
        process.sleep(5000)
        Error(runner.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: domain.zero_token_totals(),
          final_issue: None,
        ))
      }
      Ok(worker_command.Abort(reply)) -> {
        process.send(reply, worker_command.Applied(Some("aborted")))
        Error(runner.WorkerFailure(
          reason: error.OperatorAbort,
          workspace_path: None,
          tokens: domain.zero_token_totals(),
          final_issue: None,
        ))
      }
      _ ->
        Error(runner.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("no prompt")),
          workspace_path: None,
          tokens: domain.zero_token_totals(),
          final_issue: None,
        ))
    }
  }
}

pub fn linear_commands_run_before_candidate_dispatch_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_path = write_workflow("test/tmp/daemon-linear-park", 1)
  let log_subject = process.new_subject()
  let fetch_subject = process.new_subject()
  let ack_subject = process.new_subject()
  let linear_server = start_linear_server(fetch_subject, ack_subject)
  let deps =
    dependencies(
      tracker_with(candidate),
      linear_client(linear_server),
      log_subject,
      unused_agent,
    )
  process.send(
    linear_server,
    SetNext([
      linear_comment("c1", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(fetched_ids) = process.receive(fetch_subject, within: 1000)
  assert fetched_ids == ["issue-1"]
  let assert Ok(snapshot) = wait_for_parked(started.data, "issue-1", 20)
  assert dict.has_key(snapshot.parked, "issue-1")
  let assert Ok(parked_entry) = dict.get(snapshot.parked, "issue-1")
  assert parked_entry.release_policy == domain.ExplicitUnparkOnly
  assert dict.size(snapshot.running) == 0
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Status: applied")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn linear_runtime_issue_commands_poll_when_candidate_dispatch_skipped_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_path = write_workflow("test/tmp/daemon-linear-prompt", 1)
  let log_subject = process.new_subject()
  let fetch_subject = process.new_subject()
  let ack_subject = process.new_subject()
  let linear_server = start_linear_server(fetch_subject, ack_subject)
  let deps =
    dependencies(
      tracker_with(candidate),
      linear_client(linear_server),
      log_subject,
      prompt_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(first_fetch_ids) = process.receive(fetch_subject, within: 1000)
  assert first_fetch_ids == ["issue-1"]
  assert wait_for_log(log_subject, "agent_running:issue-1", 20)

  process.send(
    linear_server,
    SetNext([
      linear_comment("c2", "issue-1", "/scherzo prompt continue"),
    ]),
  )
  process.send(started.data, daemon.PollTick(2))
  let assert Ok(second_fetch_ids) = process.receive(fetch_subject, within: 1000)
  assert second_fetch_ids == ["issue-1"]
  assert wait_for_log(log_subject, "prompt:continue", 20)
  assert wait_for_log(log_subject, "linear_operator_command:prompt:queued", 20)
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Command: prompt")
  assert string.contains(ack, "Status: queued")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn linear_abort_ack_updated_at_does_not_redispatch_test() {
  let candidate =
    domain.Issue(
      ..issue("issue-1", "ABC-1", "Todo"),
      updated_at: Some(birl.from_unix(0)),
    )
  let updated_candidate =
    domain.Issue(..candidate, updated_at: Some(birl.from_unix(1)))
  let workflow_path = write_workflow("test/tmp/daemon-linear-abort", 1)
  let log_subject = process.new_subject()
  let fetch_subject = process.new_subject()
  let ack_subject = process.new_subject()
  let tracker_server = start_tracker_server(candidate)
  let linear_server = start_linear_server(fetch_subject, ack_subject)
  let deps =
    dependencies(
      dynamic_tracker(tracker_server),
      linear_client(linear_server),
      log_subject,
      prompt_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(first_fetch_ids) = process.receive(fetch_subject, within: 1000)
  assert first_fetch_ids == ["issue-1"]
  assert wait_for_log(log_subject, "agent_running:issue-1", 20)
  drain_logs(log_subject)

  process.send(tracker_server, SetTrackerCandidate(updated_candidate))
  process.send(
    linear_server,
    SetNext([
      linear_comment("abort-1", "issue-1", "/scherzo abort"),
    ]),
  )
  process.send(started.data, daemon.PollTick(2))
  let assert Ok(second_fetch_ids) = process.receive(fetch_subject, within: 1000)
  assert second_fetch_ids == ["issue-1"]
  assert wait_for_log(log_subject, "linear_operator_command:abort:applied", 20)
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Command: abort")
  assert string.contains(ack, "Status: applied")

  let assert Ok(snapshot) = wait_for_parked(started.data, "issue-1", 20)
  let assert Ok(parked_entry) = dict.get(snapshot.parked, "issue-1")
  assert parked_entry.release_policy == domain.ExplicitUnparkOnly
  assert dict.size(snapshot.running) == 0
  assert !wait_for_log(log_subject, "dispatch_started", 3)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

fn wait_for_parked(
  subject: process.Subject(daemon.Message),
  issue_id: String,
  attempts: Int,
) -> Result(domain.RuntimeState, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False -> {
      let assert Ok(snapshot) = daemon.get_snapshot(subject, 1000)
      case dict.has_key(snapshot.parked, issue_id) {
        True -> Ok(snapshot)
        False -> {
          process.sleep(50)
          wait_for_parked(subject, issue_id, attempts - 1)
        }
      }
    }
  }
}

fn drain_logs(subject: process.Subject(String)) -> Nil {
  case process.receive(subject, within: 10) {
    Ok(_) -> drain_logs(subject)
    Error(_) -> Nil
  }
}

fn wait_for_log(
  subject: process.Subject(String),
  event: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) ->
          case received == event {
            True -> True
            False -> wait_for_log(subject, event, attempts - 1)
          }
        Error(_) -> False
      }
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
