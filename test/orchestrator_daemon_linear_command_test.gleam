import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear_triage
import scherzo/orchestrator/daemon
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/hub
import scherzo/session/name as session_name
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy
import scherzo/workflow_run
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn workflow_text(root: String, max_concurrent: Int) -> String {
  "version: 1
tracker:
  kind: linear
  api_key: test-key
  project_slug: TEST
  active_states: [Todo]
  terminal_states: [Done]
workspace:
  root: " <> root <> "
  hooks:
    create: |
      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"
    before_step: |
      test -d \"$SCHERZO_WORKSPACE_PATH\"
    after_step: |
      true
    remove: |
      rm -rf \"$SCHERZO_WORKSPACE_PATH\"
    timeout_ms: 60000
polling:
  interval_ms: 1000
agent:
  max_concurrent_agents: " <> int_to_string(max_concurrent) <> "
  max_retry_attempts: 3
  max_sessions_per_issue: 3
pi:
  command: fake
routing:
  workflow_label_prefix: \"workflow:\"
  require_exactly_one_workflow_label: false
  default_workflow: implementation
  workflows:
    implementation: workflows/implementation.yaml
linear_commands:
  enabled: true
  authorized_user_ids:
    - user-1
  poll_limit_per_issue: 10
  max_comments_per_tick: 10
  acknowledge_success: true
  acknowledge_rejection: true
"
}

fn write_workflow(dir: String, max_concurrent: Int) -> String {
  reset_dir(dir)
  write_workflow_files(dir, workflow_text(dir <> "/workspaces", max_concurrent))
}

fn effective_workspace_root(workflow_dir: String) -> String {
  workflow_dir <> "/" <> workflow_dir <> "/workspaces"
}

fn write_enforcing_workflow(dir: String, max_concurrent: Int) -> String {
  reset_dir(dir)
  let contents =
    workflow_text(dir <> "/workspaces", max_concurrent)
    |> string.replace(
      each: "linear_commands:",
      with: "linear_contract:
  workflow_label_prefix: \"workflow:\"
  workflow_labels: [bugfix, research]
  enforce_issue_workflow_labels: true
linear_commands:",
    )
  write_workflow_files(dir, contents)
}

fn write_workflow_files(dir: String, config_text: String) -> String {
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(config_path, config_text)
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    workspace: main
",
    )
  config_path
}

fn linear_comment(
  id: String,
  issue_id: String,
  body: String,
) -> linear.LinearComment {
  linear_comment_at(id, issue_id, body, 1000)
}

fn linear_comment_at(
  id: String,
  issue_id: String,
  body: String,
  created_at_ms: Int,
) -> linear.LinearComment {
  linear.LinearComment(
    id: id,
    issue_id: issue_id,
    body: body,
    created_at_ms: created_at_ms,
    updated_at_ms: created_at_ms,
    author: linear.LinearCommentAuthor(
      id: "user-1",
      email: Some("operator@example.com"),
      name: Some("Operator"),
    ),
  )
}

fn tracker_with(candidate: tracker_issue.Issue) -> tracker.Client {
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
  SetTrackerCandidate(tracker_issue.Issue)
  FetchTrackerCandidates(
    process.Subject(Result(List(tracker_issue.Issue), error.TrackerError)),
  )
  FetchTrackerByIds(
    List(String),
    process.Subject(Result(List(tracker_issue.Issue), error.TrackerError)),
  )
}

fn start_tracker_server(
  initial_candidate: tracker_issue.Issue,
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
  candidate: tracker_issue.Issue,
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
  SetAckResults(List(Result(Nil, error.TrackerError)))
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
      linear_server_loop(subject, fetch_subject, ack_subject, [], [])
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn linear_server_loop(
  subject: process.Subject(LinearServerMessage),
  fetch_subject: process.Subject(List(String)),
  ack_subject: process.Subject(String),
  queued_batches: List(List(linear.LinearComment)),
  queued_ack_results: List(Result(Nil, error.TrackerError)),
) -> Nil {
  case process.receive(subject, within: 10_000) {
    Ok(SetNext(comments)) ->
      linear_server_loop(
        subject,
        fetch_subject,
        ack_subject,
        list.append(queued_batches, [comments]),
        queued_ack_results,
      )
    Ok(SetAckResults(results)) ->
      linear_server_loop(
        subject,
        fetch_subject,
        ack_subject,
        queued_batches,
        results,
      )
    Ok(FetchComments(issue_ids, reply)) -> {
      process.send(fetch_subject, issue_ids)
      let #(comments, queued_batches) = pop_batch(queued_batches)
      process.send(reply, Ok(comments))
      linear_server_loop(
        subject,
        fetch_subject,
        ack_subject,
        queued_batches,
        queued_ack_results,
      )
    }
    Ok(PostAck(body, reply)) -> {
      process.send(ack_subject, body)
      let #(result, queued_ack_results) = pop_ack_result(queued_ack_results)
      process.send(reply, result)
      linear_server_loop(
        subject,
        fetch_subject,
        ack_subject,
        queued_batches,
        queued_ack_results,
      )
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

fn pop_ack_result(
  queued_ack_results: List(Result(Nil, error.TrackerError)),
) -> #(Result(Nil, error.TrackerError), List(Result(Nil, error.TrackerError))) {
  case queued_ack_results {
    [] -> #(Ok(Nil), [])
    [result, ..rest] -> #(result, rest)
  }
}

fn fake_triage(subject: process.Subject(String)) -> linear_triage.TriageClient {
  linear_triage.TriageClient(report_invalid_workflow: fn(issue, violation) {
    process.send(
      subject,
      "triage:" <> issue.id <> ":" <> workflow_policy.violation_code(violation),
    )
    Ok(linear_triage.InvalidWorkflowReportNoop)
  })
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
    tracker_issue.Issue,
    Option(Int),
    String,
    config_types.EffectiveConfig,
    tracker.Client,
    fn(String, agent_types.RunnerUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    make_tracker: fn(_) { tracker_client },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    make_linear_commands: fn(_) { linear_command_client },
    make_triage: fn(_, _) { linear_triage.disabled_client() },
    workflow_run_dependencies: workflow_deps_from_agent(agent_runner),
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

fn workflow_deps_from_agent(
  agent_runner: fn(
    tracker_issue.Issue,
    Option(Int),
    String,
    config_types.EffectiveConfig,
    tracker.Client,
    fn(String, agent_types.RunnerUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    ..workflow_run.default_dependencies(),
    agent_step: fn(
      issue,
      _context,
      prompt,
      effective,
      tracker_client,
      emit_update,
      command_ready,
    ) {
      let command_subject = process.new_subject()
      agent_runner(
        issue,
        None,
        prompt,
        effective,
        tracker_client,
        fn(_, update) { emit_update(update) },
        command_subject,
        fn() { command_ready(command_subject) },
      )
    },
  )
}

fn unused_agent(
  _issue: tracker_issue.Issue,
  _attempt: Option(Int),
  _definition: String,
  _effective: config_types.EffectiveConfig,
  _tracker_client: tracker.Client,
  _emit_update: fn(String, agent_types.RunnerUpdate) -> Nil,
  _command_subject: process.Subject(worker_command.Command),
  _ready: fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  Error(agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError("not used")),
    workspace_path: None,
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
  ))
}

fn prompt_agent(log_subject: process.Subject(String)) {
  fn(
    issue: tracker_issue.Issue,
    _attempt: Option(Int),
    _definition: String,
    _effective: config_types.EffectiveConfig,
    _tracker_client: tracker.Client,
    _emit_update: fn(String, agent_types.RunnerUpdate) -> Nil,
    command_subject: process.Subject(worker_command.Command),
    ready: fn() -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
    ready()
    process.send(log_subject, "agent_running:" <> issue.id)
    case process.receive(command_subject, within: 5000) {
      Ok(worker_command.QueuePrompt(message, reply)) -> {
        process.send(log_subject, "prompt:" <> message)
        process.send(reply, worker_command.Queued(Some("queued")))
        process.sleep(5000)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      }
      Ok(worker_command.Abort(reply)) -> {
        process.send(reply, worker_command.Applied(Some("aborted")))
        Error(agent_types.WorkerFailure(
          reason: error.OperatorAbort,
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      }
      _ ->
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("no prompt")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
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
  assert parked_entry.release_policy == orchestrator_state.ExplicitUnparkOnly
  assert dict.size(snapshot.running) == 0
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Status: applied")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn park_command_suppresses_invalid_workflow_triage_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-linear-park-invalid", 1)
  let log_subject = process.new_subject()
  let fetch_subject = process.new_subject()
  let ack_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let linear_server = start_linear_server(fetch_subject, ack_subject)
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies(
        tracker_with(candidate),
        linear_client(linear_server),
        log_subject,
        unused_agent,
      ),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
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
  assert process.receive(triage_subject, within: 100) == Error(Nil)

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
  let canonical_session_id = "ABC-1-1000-1"
  let display_name = session_name.generate("ABC-1", canonical_session_id)
  assert string.contains(ack, "Target: " <> display_name)
  assert !string.contains(ack, "Target: " <> canonical_session_id)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn linear_abort_ack_updated_at_does_not_redispatch_test() {
  let candidate =
    tracker_issue.Issue(
      ..issue("issue-1", "ABC-1", "Todo"),
      updated_at: Some(birl.from_unix(0)),
    )
  let updated_candidate =
    tracker_issue.Issue(..candidate, updated_at: Some(birl.from_unix(1)))
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
  assert parked_entry.release_policy == orchestrator_state.ExplicitUnparkOnly
  assert dict.size(snapshot.running) == 0
  assert !wait_for_log(log_subject, "dispatch_started", 3)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn linear_command_receipts_are_persisted_in_order_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_dir = "test/tmp/daemon-linear-receipts"
  let workspace_root = effective_workspace_root(workflow_dir)
  let workflow_path = write_workflow(workflow_dir, 1)
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
      linear_comment("c-receipt", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  let assert Ok(_) = wait_for_parked(started.data, "issue-1", 20)
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Status: applied")
  assert wait_for_command_record_kinds(
    workspace_root,
    "c-receipt",
    ["seen", "started", "completed", "acked"],
    20,
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn linear_command_ack_failure_retries_on_later_poll_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_dir = "test/tmp/daemon-linear-ack-retry"
  let workspace_root = effective_workspace_root(workflow_dir)
  let workflow_path = write_workflow(workflow_dir, 1)
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
    SetAckResults([Error(error.LinearApiRequest("temporary")), Ok(Nil)]),
  )
  process.send(
    linear_server,
    SetNext([
      linear_comment("c-retry-ack", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  let assert Ok(_) = wait_for_parked(started.data, "issue-1", 20)
  assert wait_for_log(log_subject, "linear_operator_command:park:applied", 20)
  let assert Ok(first_ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(first_ack, "Status: applied")
  assert wait_for_log(log_subject, "linear_command_ack_failed", 20)
  drain_logs(log_subject)

  process.send(
    linear_server,
    SetNext([
      linear_comment("c-retry-ack", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  process.send(started.data, daemon.PollTick(2))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  let assert Ok(second_ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(second_ack, "Status: applied")
  assert !wait_for_log(log_subject, "linear_operator_command:park:applied", 3)
  assert wait_for_command_record_kinds(
    workspace_root,
    "c-retry-ack",
    ["seen", "started", "completed", "acked"],
    100,
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn completed_unacked_command_replays_ack_without_reapplying_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_dir = "test/tmp/daemon-linear-completed-unacked"
  let workspace_root = effective_workspace_root(workflow_dir)
  let workflow_path = write_workflow(workflow_dir, 1)
  append_ledger_bodies_for_root(workspace_root, [
    record.IssueParkedV2(
      issue_id: "issue-1",
      issue_identifier: "ABC-1",
      reason: "operator:hold",
      release_policy: "explicit_unpark_only",
      issue_fingerprint: "",
      observed_updated_at_ms: 100,
    ),
    record.LinearCommandSeen(
      comment_id: "c-replay",
      issue_id: "issue-1",
      author_id: "user-1",
      command_name: "park",
      excerpt: "hold",
    ),
    record.LinearCommandStarted(
      comment_id: "c-replay",
      issue_id: "issue-1",
      command_name: "park",
    ),
    record.LinearCommandCompleted(
      comment_id: "c-replay",
      issue_id: "issue-1",
      status: "applied",
      message_excerpt: "issue parked",
    ),
  ])
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
      linear_comment("c-replay", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Command: park")
  assert string.contains(ack, "Status: applied")
  assert !wait_for_log(log_subject, "linear_operator_command:park:applied", 3)
  assert wait_for_command_record_kinds(
    workspace_root,
    "c-replay",
    ["seen", "started", "completed", "acked"],
    20,
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn startup_ack_outbox_replay_suppresses_duplicate_receipt_ack_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_dir = "test/tmp/daemon-linear-outbox-ack-dedupe"
  let workspace_root = effective_workspace_root(workflow_dir)
  let workflow_path = write_workflow(workflow_dir, 1)
  append_ledger_bodies_for_root(workspace_root, [
    record.LinearCommandSeen(
      comment_id: "c-replay",
      issue_id: "issue-1",
      author_id: "user-1",
      command_name: "park",
      excerpt: "hold",
    ),
    record.LinearCommandStarted(
      comment_id: "c-replay",
      issue_id: "issue-1",
      command_name: "park",
    ),
    record.LinearCommandCompleted(
      comment_id: "c-replay",
      issue_id: "issue-1",
      status: "applied",
      message_excerpt: "issue parked",
    ),
    record.OutboxPendingV2(
      outbox_id: "c-replay",
      issue_id: "issue-1",
      outbox_kind: "linear_command_ack",
      dedupe_key: "linear_command_ack:c-replay",
      payload_json: "{\"type\":\"linear_command_ack\",\"source_comment_id\":\"c-replay\",\"body\":\"pending ack\"}",
    ),
  ])
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
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(startup_ack) = process.receive(ack_subject, within: 1000)
  assert startup_ack == "pending ack"
  process.send(
    linear_server,
    SetNext([
      linear_comment("c-replay", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  process.send(started.data, daemon.PollTick(1))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  assert process.receive(ack_subject, within: 200) == Error(Nil)
  assert !wait_for_log(log_subject, "linear_operator_command:park:applied", 3)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn started_uncompleted_command_gets_unknown_ack_without_reapplying_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_dir = "test/tmp/daemon-linear-started-uncompleted"
  let workspace_root = effective_workspace_root(workflow_dir)
  let workflow_path = write_workflow(workflow_dir, 1)
  append_ledger_bodies_for_root(workspace_root, [
    record.LinearCommandSeen(
      comment_id: "c-unknown",
      issue_id: "issue-1",
      author_id: "user-1",
      command_name: "park",
      excerpt: "hold",
    ),
    record.LinearCommandStarted(
      comment_id: "c-unknown",
      issue_id: "issue-1",
      command_name: "park",
    ),
  ])
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
      linear_comment("c-unknown", "issue-1", "/scherzo park --reason hold"),
    ]),
  )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Status: unknown_after_restart")
  assert !wait_for_log(log_subject, "linear_operator_command:park:applied", 3)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot.parked, "issue-1")
  assert wait_for_command_record_kinds(
    workspace_root,
    "c-unknown",
    ["seen", "started", "acked"],
    20,
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn old_unseen_comment_posted_while_down_is_processed_when_observed_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let workflow_path = write_workflow("test/tmp/daemon-linear-old-unseen", 1)
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
      linear_comment_at(
        "c-old",
        "issue-1",
        "/scherzo park --reason downtime",
        900,
      ),
    ]),
  )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(_) = process.receive(fetch_subject, within: 1000)
  let assert Ok(snapshot) = wait_for_parked(started.data, "issue-1", 20)
  assert dict.has_key(snapshot.parked, "issue-1")
  let assert Ok(ack) = process.receive(ack_subject, within: 1000)
  assert string.contains(ack, "Status: applied")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

fn append_ledger_bodies_for_root(
  workspace_root: String,
  bodies: List(record.RecordBody),
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      ledger_records_for_bodies(100, bodies),
      True,
    )
  Nil
}

fn ledger_records_for_bodies(
  at_ms: Int,
  bodies: List(record.RecordBody),
) -> List(record.LedgerRecord) {
  ledger_records_for_bodies_loop(bodies, at_ms, 1, [])
}

fn ledger_records_for_bodies_loop(
  bodies: List(record.RecordBody),
  at_ms: Int,
  sequence: Int,
  acc: List(record.LedgerRecord),
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> list.reverse(acc)
    [body, ..rest] ->
      ledger_records_for_bodies_loop(rest, at_ms + 1, sequence + 1, [
        record.new(at_ms, sequence, body),
        ..acc
      ])
  }
}

fn replay_records_for_root(
  workspace_root: String,
) -> List(record.LedgerRecord) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(replayed) = ledger.replay(ledger_path)
  replayed.records
}

fn command_record_kinds(
  records: List(record.LedgerRecord),
  comment_id: String,
) -> List(String) {
  records
  |> list.filter_map(fn(ledger_record) {
    case ledger_record.body {
      record.LinearCommandSeen(comment_id: id, ..) ->
        case id == comment_id {
          True -> Ok("seen")
          False -> Error(Nil)
        }
      record.LinearCommandStarted(comment_id: id, ..) ->
        case id == comment_id {
          True -> Ok("started")
          False -> Error(Nil)
        }
      record.LinearCommandCompleted(comment_id: id, ..) ->
        case id == comment_id {
          True -> Ok("completed")
          False -> Error(Nil)
        }
      record.LinearCommandAcked(comment_id: id, ..) ->
        case id == comment_id {
          True -> Ok("acked")
          False -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
}

fn wait_for_command_record_kinds(
  workspace_root: String,
  comment_id: String,
  expected: List(String),
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case
        command_record_kinds(
          replay_records_for_root(workspace_root),
          comment_id,
        )
        == expected
      {
        True -> True
        False -> {
          process.sleep(50)
          wait_for_command_record_kinds(
            workspace_root,
            comment_id,
            expected,
            attempts - 1,
          )
        }
      }
  }
}

fn wait_for_parked(
  subject: process.Subject(daemon.Message),
  issue_id: String,
  attempts: Int,
) -> Result(orchestrator_state.RuntimeState, Nil) {
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
