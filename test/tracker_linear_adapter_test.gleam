import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/result_artifact
import scherzo/scheduled_failure_reporter as reporter
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/adapter as tracker_adapter
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/linear_adapter
import scherzo/tracker/state as issue_state
import test_async

type CapturedRequest {
  CapturedRequest(String)
}

type SearchPhase {
  FirstSearch
  SecondSearch
}

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.test/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn effective_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "."),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

fn handoff_effective_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    ..effective_config(),
    handoff: config_types.HandoffConfig(
      enabled: True,
      comment_on_claim: True,
      comment_on_success: True,
      comment_on_failure: True,
      comment_on_park: True,
      claim_state_id: Some("claim-state"),
      success_state_id: Some("success-state"),
      failure_state_id: Some("failure-state"),
      include_result_on_success: True,
      attach_result_on_success: False,
      attachment_fallback_to_markdown_link: True,
      result_max_chars: 8000,
      completion_states: None,
    ),
  )
}

fn task_search_request() -> tracker_adapter.TaskSearchRequest {
  tracker_adapter.TaskSearchRequest(
    active_states: ["Todo", "In Progress"],
    dispatch_states: ["Todo"],
    terminal_states: ["Done"],
    workflow_labels: ["workflow:execplan"],
    limit: 10,
  )
}

fn linear_task_ref() -> task.TaskRef {
  task.TaskRef(
    backend_kind: "linear",
    remote_id: "issue-1",
    key: Some("LIV-266"),
    url: Some("https://linear.app/living-systems/issue/LIV-266"),
  )
}

fn linear_task() -> task.Task {
  task.Task(
    ref: linear_task_ref(),
    title: "Refresh architecture",
    description: Some("body"),
    priority: Some(2),
    state: task.TaskState(id: None, name: "Todo", category: task.Unknown),
    branch_hint: Some("liv-266-refresh"),
    labels: [task.TaskLabel(id: None, name: "workflow:implementation")],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn non_linear_task() -> task.Task {
  task.Task(
    ..linear_task(),
    ref: task.TaskRef(
      backend_kind: "github",
      remote_id: "issue-1",
      key: Some("GH-1"),
      url: None,
    ),
  )
}

fn linear_task_without_remote_id() -> task.Task {
  task.Task(
    ..linear_task(),
    ref: task.TaskRef(
      backend_kind: "linear",
      remote_id: "",
      key: Some("LIV-blank"),
      url: None,
    ),
  )
}

fn worker_success(task_context: task.Task) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(task.to_runtime_issue(task_context)),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace/main",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(
      Some("completed generic handoff"),
      False,
      "agent_end_messages",
    ),
  )
}

fn worker_failure() -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError("generic handoff failure")),
    workspace_path: Some("workspace/main"),
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
  )
}

fn receive_request(subject: process.Subject(CapturedRequest)) -> String {
  let assert Ok(CapturedRequest(body)) = process.receive(subject, within: 1000)
  body
}

pub fn linear_adapter_does_not_expose_remote_commands_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(_) {
      Error(error.LinearApiRequest("unexpected Linear transport call"))
    })

  let assert None = linear_tracker.remote_commands
}

pub fn linear_adapter_fetch_candidates_matches_linear_issue_decoder_test() {
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      assert string.contains(request.body, "CandidateIssues")
      Ok(linear.Response(status: 200, body: candidate_response()))
    })

  let assert Ok([candidate]) =
    linear_tracker.task_source.fetch_candidates(task_search_request())

  assert candidate.ref == linear_task_ref()
  assert candidate.title == "Refresh architecture"
  assert candidate.description == Some("body")
  assert candidate.priority == Some(2)
  assert candidate.state
    == task.TaskState(id: None, name: "Todo", category: task.Unknown)
  assert candidate.branch_hint == Some("liv-266-refresh")
  assert task.label_names(candidate) == ["workflow:execplan"]
  assert candidate.blockers == []
  assert candidate.blockers_complete == True
}

pub fn linear_adapter_lookup_operator_ref_falls_back_to_candidate_identifier_test() {
  let phases = process.new_subject()
  process.send(phases, FirstSearch)
  process.send(phases, SecondSearch)
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      let assert Ok(phase) = process.receive(phases, within: 1000)
      case phase {
        FirstSearch -> {
          assert string.contains(request.body, "IssueStates")
          Ok(linear.Response(status: 200, body: empty_issues_response()))
        }
        SecondSearch -> {
          assert string.contains(request.body, "CandidateIssues")
          Ok(linear.Response(status: 200, body: candidate_response()))
        }
      }
    })

  let assert Ok(Some(found)) =
    linear_tracker.task_source.lookup_by_operator_ref("LIV-266")
  assert found.ref == linear_task_ref()
}

pub fn linear_adapter_posts_comment_with_existing_linear_body_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_tracker_config(tracker_config(), fn(request) {
      process.send(captured, CapturedRequest(request.body))
      Ok(linear.Response(
        status: 200,
        body: comment_create_response("comment-1"),
      ))
    })
  let assert Some(tracker_adapter.CommentCapability(
    post_or_update: post_or_update,
  )) = linear_tracker.comments

  let assert Ok(receipt) =
    post_or_update(tracker_adapter.CommentRequest(
      task: linear_task_ref(),
      body: "hello from adapter",
      mode: tracker_adapter.CreateOnly,
    ))
  let assert Ok(expected_request) =
    linear.build_comment_create_request(
      tracker_config(),
      "issue-1",
      "hello from adapter",
    )

  assert receipt
    == tracker_adapter.CommentReceipt(
      id: "comment-1",
      task: linear_task_ref(),
      url: Some("https://linear.app/living-systems/issue/LIV-266"),
      created: True,
    )
  assert receive_request(captured) == expected_request.body
}

pub fn linear_adapter_generic_handoff_events_preserve_linear_behavior_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_effective_config(
      handoff_effective_config(),
      fn(request) {
        process.send(captured, CapturedRequest(request.body))
        case string.contains(request.body, "issueUpdate") {
          True ->
            Ok(linear.Response(
              status: 200,
              body: mutation_success_response("issueUpdate"),
            ))
          False ->
            Ok(linear.Response(
              status: 200,
              body: comment_create_response("comment-handoff"),
            ))
        }
      },
    )
  let assert Some(tracker_adapter.HandoffCapability(report: report)) =
    linear_tracker.handoff
  let task_context = linear_task()

  assert report(tracker_adapter.HandoffClaim(
      task_context,
      "workspace/main",
      "run-claim",
    ))
    == Ok(Nil)
  let claim_comment = receive_request(captured)
  let claim_state = receive_request(captured)
  assert string.contains(claim_comment, "Scherzo claimed this issue")
  assert string.contains(claim_comment, "LIV-266")
  assert string.contains(claim_comment, "run-claim")
  assert string.contains(claim_state, "claim-state")

  assert report(tracker_adapter.HandoffSuccess(
      task_context,
      worker_success(task_context),
      "run-success",
      "workflow:implementation",
    ))
    == Ok(Nil)
  let success_comment = receive_request(captured)
  let success_state = receive_request(captured)
  assert string.contains(success_comment, "Scherzo completed the run")
  assert string.contains(success_comment, "completed generic handoff")
  assert string.contains(success_comment, "run-success")
  assert string.contains(success_state, "success-state")

  assert report(tracker_adapter.HandoffFailure(
      task_context,
      worker_failure(),
      "run-failure",
      "workflow:implementation",
    ))
    == Ok(Nil)
  let failure_comment = receive_request(captured)
  let failure_state = receive_request(captured)
  assert string.contains(failure_comment, "Failure diagnostics")
  assert string.contains(failure_comment, "generic handoff failure")
  assert string.contains(failure_comment, "run-failure")
  assert string.contains(failure_state, "failure-state")

  assert report(
      tracker_adapter.HandoffPark(tracker_adapter.ParkReport(
        task: linear_task_ref(),
        issue_identifier: "LIV-266",
        reason: "needs operator input",
        release_policy: Some("explicit_unpark_only"),
        run_id: Some("run-park"),
      )),
    )
    == Ok(Nil)
  let park_comment = receive_request(captured)
  assert string.contains(park_comment, "Scherzo parked this issue")
  assert string.contains(park_comment, "needs operator input")
  assert string.contains(park_comment, "explicit_unpark_only")
  assert string.contains(park_comment, "run-park")
}

pub fn linear_adapter_rejects_generic_handoff_events_for_invalid_tasks_test() {
  let captured = process.new_subject()
  let linear_tracker =
    linear_adapter.from_effective_config(
      handoff_effective_config(),
      fn(request) {
        process.send(captured, CapturedRequest(request.body))
        Error(error.LinearApiRequest("unexpected Linear transport call"))
      },
    )
  let assert Some(tracker_adapter.HandoffCapability(report: report)) =
    linear_tracker.handoff
  let non_linear_task = non_linear_task()
  let blank_linear_task = linear_task_without_remote_id()

  assert report(tracker_adapter.HandoffClaim(
      non_linear_task,
      "workspace/main",
      "run-claim",
    ))
    == Error(tracker_adapter.NotFound(non_linear_task.ref))
  assert report(tracker_adapter.HandoffSuccess(
      non_linear_task,
      worker_success(non_linear_task),
      "run-success",
      "workflow:implementation",
    ))
    == Error(tracker_adapter.NotFound(non_linear_task.ref))
  assert report(tracker_adapter.HandoffFailure(
      non_linear_task,
      worker_failure(),
      "run-failure",
      "workflow:implementation",
    ))
    == Error(tracker_adapter.NotFound(non_linear_task.ref))
  assert report(tracker_adapter.HandoffClaim(
      blank_linear_task,
      "workspace/main",
      "run-blank",
    ))
    == Error(tracker_adapter.NotFound(blank_linear_task.ref))
  test_async.assert_no_extra_message_within(captured, 20)
}

pub fn linear_adapter_scheduled_failure_preserves_dedupe_marker_test() {
  let phases = process.new_subject()
  let linear_tracker =
    linear_adapter.from_dependencies(
      effective_config(),
      linear_adapter.Dependencies(
        transport: fn(_) {
          Error(error.LinearApiRequest("unexpected Linear transport call"))
        },
        handoff_client: handoff.disabled_client(),
        scheduled_failure_client: reporter.client(scheduled_failure_backend(
          phases,
        )),
      ),
    )
  let assert Some(tracker_adapter.ScheduledFailureCapability(publish: publish)) =
    linear_tracker.scheduled_failures

  process.send(phases, FirstSearch)
  let assert Ok(created) = publish(scheduled_publication("schedule-nightly-1"))
  assert created
    == tracker_adapter.ScheduledFailureReceipt(
      task: task.TaskRef(
        backend_kind: "linear",
        remote_id: "lin-nightly",
        key: None,
        url: None,
      ),
      created: True,
      comment_id: None,
    )

  process.send(phases, SecondSearch)
  let assert Ok(updated) = publish(scheduled_publication("schedule-nightly-2"))
  assert updated
    == tracker_adapter.ScheduledFailureReceipt(
      task: task.TaskRef(
        backend_kind: "linear",
        remote_id: "lin-nightly",
        key: None,
        url: None,
      ),
      created: False,
      comment_id: None,
    )
}

fn scheduled_publication(
  run_id: String,
) -> tracker_adapter.ScheduledFailurePublication {
  tracker_adapter.ScheduledFailurePublication(
    job_id: "nightly",
    workflow_id: "nightly",
    due_at_ms: 0,
    run_id: run_id,
    attempt: 1,
    max_attempts: 1,
    reason: "workflow_command_failed:inspect: command exited 1",
    run_root: None,
    session_id: None,
    dedupe_key: reporter.dedupe_key("nightly"),
    title: "Nightly failed",
    body: "workflow_command_failed:inspect: command exited 1",
    labels: ["job:nightly"],
    target_state_name: Some("Triage"),
    previous_task_remote_id: None,
  )
}

fn scheduled_failure_backend(
  phases: process.Subject(SearchPhase),
) -> reporter.Backend {
  reporter.Backend(
    ensure_label: fn(name) { Ok("id:" <> name) },
    find_open_issue_by_id: fn(_) { Ok(None) },
    find_open_issues_by_labels: fn(labels) {
      assert labels == ["scherzo:scheduled", "scherzo:scheduled-job:nightly"]
      let assert Ok(phase) = process.receive(phases, within: 1000)
      case phase {
        FirstSearch -> Ok([])
        SecondSearch ->
          Ok([
            reporter.ExistingFailureIssue(id: "lin-nightly", updated_at_ms: 2),
          ])
      }
    },
    create_issue: fn(title, body, state, label_ids) {
      assert title == "Scherzo scheduled job failed: nightly"
      assert state == "Triage"
      assert list.contains(label_ids, "id:scherzo:scheduled")
      assert list.contains(label_ids, "id:scherzo:scheduled-job:nightly")
      assert list.contains(label_ids, "id:job:nightly")
      assert string.contains(
        body,
        "<!-- scherzo-dedupe: scheduled-job:nightly -->",
      )
      assert string.contains(body, "Dedupe key: scheduled-job:nightly")
      assert string.contains(
        body,
        "Failure: workflow_command_failed:inspect: command exited 1",
      )
      Ok("lin-nightly")
    },
    comment_issue: fn(issue_id, body) {
      assert issue_id == "lin-nightly"
      assert string.contains(
        body,
        "<!-- scherzo-dedupe: scheduled-job:nightly -->",
      )
      assert string.contains(body, "Run ID: schedule-nightly-2")
      assert string.contains(body, "Current failure issue ID: lin-nightly")
      Ok(Nil)
    },
    move_issue_to_state: fn(issue_id, state) {
      assert issue_id == "lin-nightly"
      assert state == "Triage"
      Ok(Nil)
    },
  )
}

fn empty_issues_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn candidate_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-1\",\"identifier\":\"LIV-266\",\"title\":\"Refresh architecture\",\"description\":\"body\",\"priority\":2,\"branchName\":\"liv-266-refresh\",\"url\":\"https://linear.app/living-systems/issue/LIV-266\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[{\"name\":\"workflow:execplan\"}]},\"inverseRelations\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn mutation_success_response(field: String) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([#(field, json.object([#("success", json.bool(True))]))]),
      ),
    ]),
  )
}

fn comment_create_response(comment_id: String) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "commentCreate",
            json.object([
              #("success", json.bool(True)),
              #("comment", comment_json(comment_id)),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn comment_json(comment_id: String) -> json.Json {
  json.object([
    #("id", json.string(comment_id)),
    #("body", json.string("hello from adapter")),
    #("bodyData", json.string(empty_body_data())),
  ])
}

fn empty_body_data() -> String {
  json.to_string(
    json.object([
      #("type", json.string("doc")),
      #("content", json.preprocessed_array([])),
    ]),
  )
}
