import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/scheduled_failure_reporter as reporter
import scherzo/task
import scherzo/tracker/adapter as tracker_adapter
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/linear_adapter
import scherzo/tracker/state as issue_state

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

fn receive_request(subject: process.Subject(CapturedRequest)) -> String {
  let assert Ok(CapturedRequest(body)) = process.receive(subject, within: 1000)
  body
}

pub fn linear_adapter_fetch_candidates_matches_linear_parser_test() {
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

pub fn linear_adapter_scheduled_failure_preserves_dedupe_marker_test() {
  let phases = process.new_subject()
  let linear_tracker =
    linear_adapter.from_dependencies(
      effective_config(),
      linear_adapter.Dependencies(
        transport: fn(_) {
          Error(error.LinearApiRequest("unexpected Linear transport call"))
        },
        command_client: linear.command_client(tracker_config(), fn(_) {
          Error(error.LinearApiRequest("unexpected Linear transport call"))
        }),
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

fn candidate_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-1\",\"identifier\":\"LIV-266\",\"title\":\"Refresh architecture\",\"description\":\"body\",\"priority\":2,\"branchName\":\"liv-266-refresh\",\"url\":\"https://linear.app/living-systems/issue/LIV-266\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[{\"name\":\"workflow:execplan\"}]},\"inverseRelations\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
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
