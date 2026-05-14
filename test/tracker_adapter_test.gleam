import gleam/list
import gleam/option.{None, Some}
import scherzo/task
import scherzo/tracker/adapter

fn task_ref() -> task.TaskRef {
  task.TaskRef(
    backend_kind: "test-memory",
    remote_id: "card-1",
    key: Some("CARD-1"),
    url: None,
  )
}

fn fake_task() -> task.Task {
  task.Task(
    ref: task_ref(),
    title: "Fake card",
    description: None,
    priority: None,
    state: task.TaskState(id: None, name: "Todo", category: task.Unknown),
    branch_hint: None,
    labels: [task.TaskLabel(id: None, name: "workflow:execplan")],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn task_source_capability() -> adapter.TaskSourceCapability {
  adapter.TaskSourceCapability(
    fetch_candidates: fn(_) { Ok([fake_task()]) },
    refresh_by_refs: fn(_) { Ok([fake_task()]) },
    lookup_by_operator_ref: fn(_) { Ok(Some(fake_task())) },
  )
}

fn read_only_adapter() -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    kind: "test-memory",
    display_name: "Test memory",
    task_source: task_source_capability(),
    comments: None,
    remote_commands: None,
    state_transitions: None,
    routing_metadata: None,
    links: None,
    handoff: None,
    scheduled_failures: None,
    readiness: None,
    smoke: None,
    attachments: None,
  )
}

fn all_requirements() -> adapter.TrackerRequirements {
  adapter.TrackerRequirements(
    remote_commands_enabled: True,
    remote_commands_config_path: Some("linear_commands.enabled"),
    handoff_comments_enabled: True,
    handoff_state_moves_enabled: True,
    handoff_config_path: Some("handoff.states"),
    workflow_label_paths: ["workflows.<id>.label"],
    scheduled_failure_paths: ["scheduled_jobs.<id>.on_failure"],
    readiness_checks_enabled: True,
    smoke_checks_enabled: True,
  )
}

fn no_requirements() -> adapter.TrackerRequirements {
  adapter.TrackerRequirements(
    remote_commands_enabled: False,
    remote_commands_config_path: None,
    handoff_comments_enabled: False,
    handoff_state_moves_enabled: False,
    handoff_config_path: None,
    workflow_label_paths: [],
    scheduled_failure_paths: [],
    readiness_checks_enabled: False,
    smoke_checks_enabled: False,
  )
}

fn comment_capability() -> adapter.CommentCapability {
  adapter.CommentCapability(post_or_update: fn(request) {
    let adapter.CommentRequest(task: requested_task, ..) = request

    Ok(adapter.CommentReceipt(
      id: "comment-1",
      task: requested_task,
      url: None,
      created: True,
    ))
  })
}

fn comment_capability_without_update() -> adapter.CommentCapability {
  adapter.CommentCapability(post_or_update: fn(request) {
    let adapter.CommentRequest(task: requested_task, mode: mode, ..) = request

    case mode {
      adapter.CreateOnly ->
        Ok(adapter.CommentReceipt(
          id: "comment-new",
          task: requested_task,
          url: None,
          created: True,
        ))
      adapter.UpdateExisting(allow_create_fallback: True, ..) ->
        Ok(adapter.CommentReceipt(
          id: "comment-new",
          task: requested_task,
          url: None,
          created: True,
        ))
      adapter.UpdateExisting(allow_create_fallback: False, ..) ->
        Error(adapter.UnsupportedCapability("comments.update"))
    }
  })
}

fn remote_command_capability() -> adapter.RemoteCommandCapability {
  adapter.RemoteCommandCapability(
    fetch_events: fn(_) { Ok([]) },
    post_ack: fn(ack) {
      let adapter.RemoteCommandAck(event: event, ..) = ack
      let adapter.RemoteCommandEvent(task: command_task, ..) = event

      Ok(adapter.CommentReceipt(
        id: "ack-1",
        task: command_task,
        url: None,
        created: True,
      ))
    },
  )
}

fn state_transition_capability() -> adapter.StateTransitionCapability {
  adapter.StateTransitionCapability(transition: fn(request) {
    let adapter.StateTransitionRequest(
      task: requested_task,
      target_state_name: target_state_name,
      ..,
    ) = request

    Ok(adapter.StateTransitionReceipt(
      task: requested_task,
      state: task.TaskState(
        id: None,
        name: target_state_name,
        category: task.Unknown,
      ),
    ))
  })
}

fn routing_metadata_capability() -> adapter.RoutingMetadataCapability {
  adapter.RoutingMetadataCapability(
    workflow_labels: fn(value) { task.label_names(value) },
    blocker_refs: fn(value) { value.blockers },
  )
}

fn link_capability() -> adapter.LinkCapability {
  adapter.LinkCapability(upsert_link: fn(_, link) { Ok(link) })
}

fn handoff_capability() -> adapter.HandoffCapability {
  adapter.HandoffCapability(report: fn(_) { Ok(Nil) })
}

fn scheduled_failure_capability() -> adapter.ScheduledFailureCapability {
  adapter.ScheduledFailureCapability(publish: fn(_) {
    Ok(adapter.ScheduledFailureReceipt(
      task: task_ref(),
      created: True,
      comment_id: None,
    ))
  })
}

fn readiness_capability() -> adapter.ReadinessCapability {
  adapter.ReadinessCapability(check_contract: fn() { Ok([]) })
}

fn smoke_capability() -> adapter.SmokeCapability {
  adapter.SmokeCapability(run_smoke_check: fn() {
    Ok(
      adapter.SmokeReport(
        candidate_count: 1,
        refreshed_count: 1,
        terminal_sample_count: 0,
        messages: [],
      ),
    )
  })
}

fn attachment_capability() -> adapter.AttachmentCapability {
  adapter.AttachmentCapability(upload: fn(_, attachment) { Ok(attachment) })
}

fn full_adapter() -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..read_only_adapter(),
    comments: Some(comment_capability()),
    remote_commands: Some(remote_command_capability()),
    state_transitions: Some(state_transition_capability()),
    routing_metadata: Some(routing_metadata_capability()),
    links: Some(link_capability()),
    handoff: Some(handoff_capability()),
    scheduled_failures: Some(scheduled_failure_capability()),
    readiness: Some(readiness_capability()),
    smoke: Some(smoke_capability()),
    attachments: Some(attachment_capability()),
  )
}

fn validation_messages(
  errors: List(adapter.CapabilityValidationError),
) -> List(String) {
  list.map(errors, fn(error) {
    adapter.capability_validation_error_message(error)
  })
}

pub fn read_only_adapter_reports_all_required_missing_capabilities_test() {
  let assert Error(errors) =
    adapter.validate_required_capabilities(
      read_only_adapter(),
      all_requirements(),
    )

  assert validation_messages(errors)
    == [
      "tracker_capability_missing feature=remote_commands capability=remote_commands path=linear_commands.enabled backend=test-memory message=\"linear_commands.enabled requires tracker adapter test-memory to expose remote_commands\"",
      "tracker_capability_missing feature=remote_command_ack capability=comments path=linear_commands.enabled backend=test-memory message=\"remote command acknowledgements require comments capability\"",
      "tracker_capability_missing feature=handoff_state_moves capability=state_transitions path=handoff.states backend=test-memory message=\"handoff state moves require state_transitions capability\"",
      "tracker_capability_missing feature=workflow_label_routing capability=routing_metadata path=workflows.<id>.label backend=test-memory message=\"workflow label routing requires routing_metadata capability\"",
      "tracker_capability_missing feature=scheduled_failures capability=scheduled_failures path=scheduled_jobs.<id>.on_failure backend=test-memory message=\"scheduled failure publication requires scheduled_failures capability\"",
      "tracker_capability_missing feature=tracker_contract capability=readiness path=doctor.checks.tracker-contract backend=test-memory message=\"tracker contract checks require readiness capability\"",
      "tracker_capability_missing feature=tracker_smoke capability=smoke path=doctor.checks.tracker-smoke backend=test-memory message=\"tracker smoke checks require smoke capability\"",
    ]
}

pub fn read_only_adapter_satisfies_empty_requirements_test() {
  assert adapter.validate_required_capabilities(
      read_only_adapter(),
      no_requirements(),
    )
    == Ok(Nil)
}

pub fn full_adapter_satisfies_all_requirements_test() {
  assert adapter.validate_required_capabilities(
      full_adapter(),
      all_requirements(),
    )
    == Ok(Nil)
}

pub fn comment_update_without_fallback_reports_unsupported_test() {
  let adapter.CommentCapability(post_or_update: post_or_update) =
    comment_capability_without_update()

  assert post_or_update(adapter.CommentRequest(
      task: task_ref(),
      body: "updated body",
      mode: adapter.UpdateExisting(
        comment_id: "comment-1",
        allow_create_fallback: False,
      ),
    ))
    == Error(adapter.UnsupportedCapability("comments.update"))
}

pub fn comment_update_with_fallback_creates_new_receipt_test() {
  let adapter.CommentCapability(post_or_update: post_or_update) =
    comment_capability_without_update()
  let assert Ok(receipt) =
    post_or_update(adapter.CommentRequest(
      task: task_ref(),
      body: "updated body",
      mode: adapter.UpdateExisting(
        comment_id: "comment-1",
        allow_create_fallback: True,
      ),
    ))

  assert receipt
    == adapter.CommentReceipt(
      id: "comment-new",
      task: task_ref(),
      url: None,
      created: True,
    )
}
