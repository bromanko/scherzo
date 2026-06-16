import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/state as issue_state
import scherzo/work_item
import simplifile
import support/fake_tracker_adapter

fn search_request() -> adapter.TaskSearchRequest {
  adapter.TaskSearchRequest(
    active_states: ["Todo", "In Progress"],
    dispatch_states: ["Todo"],
    terminal_states: ["Done", "Canceled"],
    workflow_labels: ["workflow:execplan"],
    limit: 10,
  )
}

fn validation_messages(
  errors: List(adapter.CapabilityValidationError),
) -> List(String) {
  list.map(errors, fn(error) {
    adapter.capability_validation_error_message(error)
  })
}

fn unsupported_dispatch_requirements() -> adapter.TrackerRequirements {
  adapter.TrackerRequirements(
    remote_commands_enabled: True,
    remote_commands_config_path: Some("remote_commands.enabled"),
    handoff_comments_enabled: True,
    handoff_state_moves_enabled: True,
    handoff_config_path: None,
    workflow_label_paths: [],
    scheduled_failure_paths: ["scheduled_jobs.nightly.on_failure"],
    readiness_checks_enabled: True,
    smoke_checks_enabled: True,
  )
}

pub fn fake_adapter_test_files_do_not_import_linear_test() {
  let forbidden_import = "import " <> "scherzo/" <> "linear"
  let assert Ok(test_source) =
    simplifile.read("test/tracker_fake_adapter_test.gleam")
  let assert Ok(support_source) =
    simplifile.read("test/support/fake_tracker_adapter.gleam")

  assert !string.contains(test_source, forbidden_import)
  assert !string.contains(support_source, forbidden_import)
}

pub fn fake_adapter_fetches_refreshes_and_looks_up_non_linear_task_test() {
  let tracker = fake_tracker_adapter.seam_adapter()
  let assert Ok([candidate]) =
    tracker.task_source.fetch_candidates(search_request())

  assert candidate.ref == fake_tracker_adapter.task_ref()
  assert candidate.ref.backend_kind == "test-memory"
  assert candidate.ref.remote_id == "card-1"
  assert candidate.ref.key == Some("CARD-1")
  assert candidate.title == "Fake non-Linear card"
  assert task.label_names(candidate) == ["workflow:execplan", "kind:test"]

  let assert Ok([refreshed]) =
    tracker.task_source.refresh_by_refs([candidate.ref])
  assert refreshed.ref == candidate.ref
  assert refreshed.title == candidate.title

  let assert Ok(Some(found_by_key)) =
    tracker.task_source.lookup_by_operator_ref("CARD-1")
  assert found_by_key.ref == candidate.ref

  let assert Ok(Some(found_by_id)) =
    tracker.task_source.lookup_by_operator_ref("card-1")
  assert found_by_id.ref == candidate.ref
}

pub fn fake_adapter_runtime_issue_source_refreshes_non_linear_task_test() {
  let tracker = fake_tracker_adapter.seam_adapter()

  let assert Ok([candidate]) = adapter.fetch_runtime_candidate_issues(tracker)
  assert candidate.id == "card-1"
  assert candidate.identifier == "CARD-1"

  let assert Ok([by_state]) =
    adapter.fetch_runtime_issues_by_states(
      tracker,
      issue_state.list_from_strings(["Todo"]),
    )
  assert by_state.id == "card-1"
  assert by_state.identifier == "CARD-1"

  let assert Ok([refreshed]) =
    adapter.refresh_runtime_issues_by_ids(tracker, ["card-1"])
  assert refreshed.id == "card-1"
  assert refreshed.identifier == "CARD-1"

  let assert Ok(Some(found_by_key)) =
    adapter.lookup_runtime_issue(tracker, "CARD-1")
  assert found_by_key.id == "card-1"
  assert found_by_key.identifier == "CARD-1"
}

pub fn adapter_runtime_helpers_reject_mismatched_backend_kind_test() {
  let tracker = mismatched_backend_adapter()

  let assert Error(adapter.Permanent(candidate_message)) =
    adapter.fetch_runtime_candidate_issues(tracker)
  assert_mismatched_backend_message(candidate_message)

  let assert Error(adapter.Permanent(state_message)) =
    adapter.fetch_runtime_issues_by_states(
      tracker,
      issue_state.list_from_strings(["Todo"]),
    )
  assert_mismatched_backend_message(state_message)

  let assert Error(adapter.Permanent(refresh_message)) =
    adapter.refresh_runtime_issues_by_ids(tracker, ["card-1"])
  assert_mismatched_backend_message(refresh_message)

  let assert Error(adapter.Permanent(lookup_message)) =
    adapter.lookup_runtime_issue(tracker, "CARD-1")
  assert_mismatched_backend_message(lookup_message)
}

pub fn adapter_runtime_helpers_preserve_task_source_errors_test() {
  let tracker = task_source_errors_adapter()

  let assert Error(adapter.Transient(candidate_message)) =
    adapter.fetch_runtime_candidate_issues(tracker)
  assert candidate_message == "fetch failed"

  let assert Error(adapter.Transient(state_message)) =
    adapter.fetch_runtime_issues_by_states(
      tracker,
      issue_state.list_from_strings(["Todo"]),
    )
  assert state_message == "fetch failed"

  let assert Error(adapter.Transient(refresh_message)) =
    adapter.refresh_runtime_issues_by_ids(tracker, ["card-1"])
  assert refresh_message == "refresh failed"

  let assert Error(adapter.Transient(lookup_message)) =
    adapter.lookup_runtime_issue(tracker, "CARD-1")
  assert lookup_message == "lookup failed"
}

fn mismatched_backend_adapter() -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..fake_tracker_adapter.read_only_adapter(),
    task_source: mismatched_backend_task_source(),
  )
}

fn mismatched_backend_task_source() -> adapter.TaskSourceCapability {
  let item =
    task.Task(
      ..fake_tracker_adapter.task(),
      ref: task.TaskRef(
        backend_kind: "foreign-memory",
        remote_id: "card-1",
        key: Some("CARD-1"),
        url: Some("https://tracker.test/cards/CARD-1"),
      ),
    )

  adapter.TaskSourceCapability(
    fetch_candidates: fn(_) { Ok([item]) },
    refresh_by_refs: fn(_) { Ok([item]) },
    lookup_by_operator_ref: fn(_) { Ok(Some(item)) },
    list_tasks: fn(_) { Ok(adapter.TaskPage(items: [item], has_more: False)) },
    lookup_task_detail: fn(_) { Ok(Some(item)) },
  )
}

fn task_source_errors_adapter() -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..fake_tracker_adapter.read_only_adapter(),
    task_source: task_source_errors(),
  )
}

fn task_source_errors() -> adapter.TaskSourceCapability {
  adapter.TaskSourceCapability(
    fetch_candidates: fn(_) { Error(adapter.Transient("fetch failed")) },
    refresh_by_refs: fn(_) { Error(adapter.Transient("refresh failed")) },
    lookup_by_operator_ref: fn(_) { Error(adapter.Transient("lookup failed")) },
    list_tasks: fn(_) { Error(adapter.Transient("list failed")) },
    lookup_task_detail: fn(_) { Error(adapter.Transient("detail failed")) },
  )
}

fn assert_mismatched_backend_message(message: String) -> Nil {
  assert string.contains(message, "foreign-memory")
  assert string.contains(message, fake_tracker_adapter.backend_kind)
}

pub fn fake_adapter_workflow_compat_client_refreshes_non_linear_task_test() {
  let client =
    fake_tracker_adapter.seam_adapter()
    |> adapter_legacy.workflow_compat_client

  let assert Ok([candidate]) = client.fetch_candidate_issues()
  assert candidate.id == "card-1"
  assert candidate.identifier == "CARD-1"

  let assert Ok([by_state]) =
    client.fetch_issues_by_states(issue_state.list_from_strings(["Todo"]))
  assert by_state.id == "card-1"
  assert by_state.identifier == "CARD-1"

  let assert Ok([refreshed]) = client.fetch_issue_states_by_ids(["card-1"])
  assert refreshed.id == "card-1"
  assert refreshed.identifier == "CARD-1"
}

pub fn fake_adapter_comment_remote_handoff_transition_and_scheduled_failure_seams_test() {
  let tracker = fake_tracker_adapter.seam_adapter()
  let ref = fake_tracker_adapter.task_ref()

  let assert Some(adapter.CommentCapability(post_or_update: post_or_update, ..)) =
    tracker.comments
  let assert Ok(comment_receipt) =
    post_or_update(adapter.CommentRequest(
      task: ref,
      body: "hello",
      mode: adapter.CreateOnly,
    ))
  assert comment_receipt
    == adapter.CommentReceipt(
      id: "fake-comment-1",
      task: ref,
      url: Some("https://tracker.test/cards/CARD-1"),
      created: True,
    )

  let assert Some(adapter.RemoteCommandCapability(
    fetch_events: fetch_events,
    post_ack: post_ack,
  )) = tracker.remote_commands
  let assert Ok([event]) =
    fetch_events(adapter.RemoteCommandFetch(
      task_refs: [ref],
      since_event_ids: [],
      limit_per_task: 10,
    ))
  assert event.task == ref
  assert event.command_name == "status"
  let assert Ok(ack_receipt) =
    post_ack(adapter.RemoteCommandAck(event: event, body: "ack"))
  assert ack_receipt.id == "fake-ack-fake-command-1"
  assert ack_receipt.task == ref

  let assert Some(adapter.HandoffCapability(report: report_handoff)) =
    tracker.handoff
  let assert Ok(Nil) =
    report_handoff(adapter.HandoffClaim(
      fake_tracker_adapter.task(),
      "root",
      "run-1",
    ))

  let assert Some(adapter.StateTransitionCapability(transition: transition)) =
    tracker.state_transitions
  let assert Ok(transition_receipt) =
    transition(adapter.StateTransitionRequest(
      task: ref,
      target_state_id: Some("done"),
      target_state_name: "Done",
      reason: "test seam",
    ))
  assert transition_receipt
    == adapter.StateTransitionReceipt(
      task: ref,
      state: task.TaskState(id: Some("done"), name: "Done", category: task.Done),
    )

  let assert Some(adapter.ScheduledFailureCapability(publish: publish)) =
    tracker.scheduled_failures
  let assert Ok(failure_receipt) =
    publish(adapter.ScheduledFailurePublication(
      job_id: "nightly",
      workflow_id: "nightly",
      due_at_ms: 0,
      run_id: "run-1",
      attempt: 1,
      max_attempts: 1,
      reason: "workflow failed",
      run_root: None,
      session_id: None,
      dedupe_key: "scheduled-job:nightly",
      title: "Nightly failed",
      body: "workflow failed",
      labels: ["job:nightly"],
      target_state_name: Some("Triage"),
      previous_task_remote_id: None,
    ))
  assert failure_receipt
    == adapter.ScheduledFailureReceipt(
      task: task.TaskRef(
        backend_kind: "test-memory",
        remote_id: "scheduled-scheduled-job:nightly",
        key: Some("nightly"),
        url: None,
      ),
      created: True,
      comment_id: Some("scheduled-comment-run-1"),
    )
}

pub fn fake_adapter_work_item_lookup_supports_display_and_remote_refs_test() {
  let tracker = fake_tracker_adapter.read_only_adapter()
  let assert Some(work_items) = tracker.work_items

  let assert Ok(Some(by_display)) =
    work_items.lookup_work_item(work_item.WorkItemShowRequest(
      ref: work_item.WorkItemLookupByDisplayId("CARD-1"),
      subtask_limit: work_item.default_show_subtask_limit,
      label_limit: work_item.default_label_limit,
    ))
  assert by_display.summary.source.id == "card-1"
  assert list.length(by_display.subtasks) == 2

  let assert Ok(Some(by_remote)) =
    work_items.lookup_work_item(work_item.WorkItemShowRequest(
      ref: work_item.WorkItemLookupByRemoteId(
        provider: Some(fake_tracker_adapter.backend_kind),
        id: "card-1",
      ),
      subtask_limit: work_item.default_show_subtask_limit,
      label_limit: work_item.default_label_limit,
    ))
  assert by_remote.summary.source.display_id == Some("CARD-1")
}

pub fn fake_adapter_validation_rejects_unsupported_dispatch_before_work_test() {
  let assert Error(errors) =
    adapter.validate_required_capabilities(
      fake_tracker_adapter.read_only_adapter(),
      unsupported_dispatch_requirements(),
    )

  assert validation_messages(errors)
    == [
      "tracker_capability_missing feature=remote_commands capability=remote_commands path=remote_commands.enabled backend=test-memory message=\"remote_commands.enabled requires tracker adapter test-memory to expose remote_commands\"",
      "tracker_capability_missing feature=remote_command_ack capability=comments path=remote_commands.enabled backend=test-memory message=\"remote command acknowledgements require comments capability\"",
      "tracker_capability_missing feature=handoff_comments capability=handoff path=handoff.comments backend=test-memory message=\"handoff comments require handoff capability\"",
      "tracker_capability_missing feature=handoff_state_moves capability=state_transitions path=handoff.states backend=test-memory message=\"handoff state moves require state_transitions capability\"",
      "tracker_capability_missing feature=scheduled_failures capability=scheduled_failures path=scheduled_jobs.nightly.on_failure backend=test-memory message=\"scheduled failure publication requires scheduled_failures capability\"",
      "tracker_capability_missing feature=tracker_contract capability=readiness path=doctor.checks.tracker-contract backend=test-memory message=\"tracker contract checks require readiness capability\"",
      "tracker_capability_missing feature=tracker_smoke capability=smoke path=doctor.checks.tracker-smoke backend=test-memory message=\"tracker smoke checks require smoke capability\"",
    ]
}
