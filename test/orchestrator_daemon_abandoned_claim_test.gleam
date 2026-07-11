import gleam/dict
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/error
import scherzo/orchestrator/control_plane_runtime
import scherzo/orchestrator/daemon
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/outbox_effects
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/hub
import scherzo/task
import scherzo/tracker/adapter
import simplifile
import support/test_helpers
import test_async

pub fn daemon_stale_claim_success_compensates_with_release_claim_test() {
  let comment_subject = process.new_subject()
  let daemon_subject =
    start_release_claim_daemon(
      "test/tmp/daemon-release-claim-stale",
      comment_subject,
    )

  process.send(
    daemon_subject,
    daemon.SideEffectCompleted(effect_runner.Finished(
      1,
      effect_runner.HandoffClaimFinished(
        claim_outbox("run-1"),
        "issue-1",
        "run-1",
        Ok(Nil),
      ),
    )),
  )

  let assert Ok(#("issue-1", body)) =
    process.receive(comment_subject, within: 1000)
  assert string.contains(body, "abandoned_claim:stale_claim_success")
  assert string.contains(body, "release_claim:linear:issue-1:run-1")
  assert_parked(daemon_subject, "abandoned_claim:stale_claim_success")
  assert daemon.shutdown(daemon_subject, 1000) == Ok(Nil)
}

pub fn daemon_permanent_claim_failure_compensates_with_release_claim_test() {
  let comment_subject = process.new_subject()
  let daemon_subject =
    start_release_claim_daemon(
      "test/tmp/daemon-release-claim-permanent-failure",
      comment_subject,
    )

  process.send(
    daemon_subject,
    daemon.SideEffectCompleted(effect_runner.Finished(
      1,
      effect_runner.HandoffClaimFinished(
        claim_outbox("run-1"),
        "issue-1",
        "run-1",
        Error(error.LinearApiStatus(400)),
      ),
    )),
  )

  let assert Ok(#("issue-1", body)) =
    process.receive(comment_subject, within: 1000)
  assert string.contains(
    body,
    "abandoned_claim:permanent_failure:linear_api_status",
  )
  assert string.contains(body, "release_claim:linear:issue-1:run-1")
  assert_parked(
    daemon_subject,
    "abandoned_claim:permanent_failure:linear_api_status",
  )
  assert daemon.shutdown(daemon_subject, 1000) == Ok(Nil)
}

pub fn daemon_retryable_claim_failure_does_not_compensate_test() {
  let comment_subject = process.new_subject()
  let daemon_subject =
    start_release_claim_daemon(
      "test/tmp/daemon-release-claim-retryable-failure",
      comment_subject,
    )

  process.send(
    daemon_subject,
    daemon.SideEffectCompleted(effect_runner.Finished(
      1,
      effect_runner.HandoffClaimFinished(
        claim_outbox("run-1"),
        "issue-1",
        "run-1",
        Error(error.LinearApiStatus(500)),
      ),
    )),
  )

  let assert Ok(snapshot) = daemon.get_snapshot(daemon_subject, 1000)
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  assert !dict.has_key(snapshot.parked, identity)
  test_async.assert_no_extra_message_within(comment_subject, 100)
  assert daemon.shutdown(daemon_subject, 1000) == Ok(Nil)
}

fn start_release_claim_daemon(
  dir: String,
  comment_subject: process.Subject(#(String, String)),
) -> process.Subject(daemon.Message) {
  let workflow_path = write_workflow(dir)
  let deps = runtime_dependencies(comment_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  started.data
}

fn runtime_dependencies(
  comment_subject: process.Subject(#(String, String)),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) { tracker_adapter(comment_subject) },
    logger: fn(_, _, _, _) { Ok(Nil) },
    now_ms: fn() { 42 },
    send_after: fn(_, delay_ms, _) { daemon.TestTimer(delay_ms) },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { hub.start(10, fn() { 42 }) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(control_plane_runtime.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn tracker_adapter(
  comment_subject: process.Subject(#(String, String)),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    kind: "linear",
    display_name: "Linear",
    task_source: adapter.TaskSourceCapability(
      fetch_candidates: fn(_) { Ok([]) },
      refresh_by_refs: fn(_) { Ok([]) },
      lookup_by_operator_ref: fn(_) { Ok(None) },
      list_tasks: fn(_) { Ok(adapter.TaskPage(items: [], has_more: False)) },
      lookup_task_detail: fn(_) { Ok(None) },
    ),
    work_items: None,
    comments: Some(comment_capability(comment_subject)),
    remote_commands: None,
    state_transitions: None,
    routing_metadata: Some(
      adapter.RoutingMetadataCapability(
        workflow_labels: fn(_) { [] },
        blocker_refs: fn(_) { [] },
      ),
    ),
    links: None,
    handoff: None,
    scheduled_failures: None,
    readiness: None,
    smoke: None,
    attachments: None,
  )
}

fn comment_capability(
  subject: process.Subject(#(String, String)),
) -> adapter.CommentCapability {
  adapter.CommentCapability(
    post_or_update: fn(request) {
      let adapter.CommentRequest(task: requested_task, body: body, ..) = request
      process.send(subject, #(requested_task.remote_id, body))
      Ok(adapter.CommentReceipt(
        id: "release-claim-comment",
        task: requested_task,
        url: None,
        created: True,
      ))
    },
    find_by_marker: fn(_) { Ok(None) },
  )
}

fn claim_outbox(run_id: String) -> outbox_effects.Intent {
  let task_ref = issue_task_ref()
  let key = "claim:linear:issue-1:" <> run_id
  outbox_effects.Intent(
    outbox_id: key,
    task_ref: outbox_effects.task_ref_fields(task_ref),
    outbox_kind: "claim",
    dedupe_key: key,
    payload_json: "{}",
  )
}

fn issue_task_ref() -> task.TaskRef {
  task.TaskRef(
    backend_kind: "linear",
    remote_id: "issue-1",
    key: Some("ABC-1"),
    url: None,
  )
}

fn assert_parked(
  daemon_subject: process.Subject(daemon.Message),
  expected_reason: String,
) -> Nil {
  let assert Ok(snapshot) = daemon.get_snapshot(daemon_subject, 1000)
  let identity = orchestrator_state.linear_issue_id_identity("issue-1")
  let assert Ok(parked) = dict.get(snapshot.parked, identity)
  assert orchestrator_reason.park_to_string(parked.reason) == expected_reason
  assert parked.release_policy == orchestrator_state.ExplicitUnparkOnly
}

fn write_workflow(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(config_path, workflow_text(dir <> "/workspaces"))
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
    run_in: main
",
    )
  config_path
}

fn workflow_text(root: String) -> String {
  "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
  polling:
    every: 1s
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  sessions_per_task: 1
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
"
}
