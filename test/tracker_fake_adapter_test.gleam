import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/adapter
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

pub fn fake_adapter_comment_transition_and_scheduled_failure_seams_test() {
  let tracker = fake_tracker_adapter.seam_adapter()
  let ref = fake_tracker_adapter.task_ref()

  let assert Some(adapter.CommentCapability(post_or_update: post_or_update)) =
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
