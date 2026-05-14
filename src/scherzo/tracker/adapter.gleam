import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/task

pub type TrackerAdapter {
  TrackerAdapter(
    kind: String,
    display_name: String,
    task_source: TaskSourceCapability,
    comments: Option(CommentCapability),
    remote_commands: Option(RemoteCommandCapability),
    state_transitions: Option(StateTransitionCapability),
    routing_metadata: Option(RoutingMetadataCapability),
    links: Option(LinkCapability),
    handoff: Option(HandoffCapability),
    scheduled_failures: Option(ScheduledFailureCapability),
    readiness: Option(ReadinessCapability),
    smoke: Option(SmokeCapability),
    attachments: Option(AttachmentCapability),
  )
}

pub type TaskSearchRequest {
  TaskSearchRequest(
    active_states: List(String),
    dispatch_states: List(String),
    terminal_states: List(String),
    workflow_labels: List(String),
    limit: Int,
  )
}

pub type TaskSourceCapability {
  TaskSourceCapability(
    fetch_candidates: fn(TaskSearchRequest) ->
      Result(List(task.Task), TrackerError),
    refresh_by_refs: fn(List(task.TaskRef)) ->
      Result(List(task.Task), TrackerError),
    lookup_by_operator_ref: fn(String) ->
      Result(Option(task.Task), TrackerError),
  )
}

pub type CommentWriteMode {
  CreateOnly
  UpdateExisting(comment_id: String, allow_create_fallback: Bool)
}

pub type CommentRequest {
  CommentRequest(task: task.TaskRef, body: String, mode: CommentWriteMode)
}

pub type CommentReceipt {
  CommentReceipt(
    id: String,
    task: task.TaskRef,
    url: Option(String),
    created: Bool,
  )
}

pub type CommentCapability {
  CommentCapability(
    post_or_update: fn(CommentRequest) -> Result(CommentReceipt, TrackerError),
  )
}

pub type RemoteCommandEvent {
  RemoteCommandEvent(
    event_id: String,
    task: task.TaskRef,
    author_id: String,
    body: String,
    command_name: String,
    excerpt: String,
    observed_at_ms: Int,
  )
}

pub type RemoteCommandFetch {
  RemoteCommandFetch(
    task_refs: List(task.TaskRef),
    since_event_ids: List(String),
  )
}

pub type RemoteCommandAck {
  RemoteCommandAck(event: RemoteCommandEvent, body: String)
}

pub type RemoteCommandCapability {
  RemoteCommandCapability(
    fetch_events: fn(RemoteCommandFetch) ->
      Result(List(RemoteCommandEvent), TrackerError),
    post_ack: fn(RemoteCommandAck) -> Result(CommentReceipt, TrackerError),
  )
}

pub type StateTransitionRequest {
  StateTransitionRequest(
    task: task.TaskRef,
    target_state_id: Option(String),
    target_state_name: String,
    reason: String,
  )
}

pub type StateTransitionReceipt {
  StateTransitionReceipt(task: task.TaskRef, state: task.TaskState)
}

pub type StateTransitionCapability {
  StateTransitionCapability(
    transition: fn(StateTransitionRequest) ->
      Result(StateTransitionReceipt, TrackerError),
  )
}

pub type RoutingMetadataCapability {
  RoutingMetadataCapability(
    workflow_labels: fn(task.Task) -> List(String),
    blocker_refs: fn(task.Task) -> List(task.TaskRef),
  )
}

pub type LinkCapability {
  LinkCapability(
    upsert_link: fn(task.TaskRef, task.TaskLink) ->
      Result(task.TaskLink, TrackerError),
  )
}

pub type HandoffEvent {
  HandoffClaim(task: task.TaskRef, workspace_path: String, run_id: String)
  HandoffSuccess(task: task.TaskRef, run_id: String, summary: String)
  HandoffFailure(task: task.TaskRef, run_id: String, reason: String)
  HandoffPark(task: task.TaskRef, reason: String, release_policy: String)
}

pub type HandoffCapability {
  HandoffCapability(report: fn(HandoffEvent) -> Result(Nil, TrackerError))
}

pub type ScheduledFailurePublication {
  ScheduledFailurePublication(
    job_id: String,
    workflow_id: String,
    run_id: String,
    dedupe_key: String,
    title: String,
    body: String,
    labels: List(String),
    target_state_name: Option(String),
  )
}

pub type ScheduledFailureReceipt {
  ScheduledFailureReceipt(
    task: task.TaskRef,
    created: Bool,
    comment_id: Option(String),
  )
}

pub type ScheduledFailureCapability {
  ScheduledFailureCapability(
    publish: fn(ScheduledFailurePublication) ->
      Result(ScheduledFailureReceipt, TrackerError),
  )
}

pub type ReadinessFinding {
  ReadinessFinding(
    severity: String,
    code: String,
    message: String,
    config_path: Option(String),
  )
}

pub type ReadinessCapability {
  ReadinessCapability(
    check_contract: fn() -> Result(List(ReadinessFinding), TrackerError),
  )
}

pub type SmokeReport {
  SmokeReport(
    candidate_count: Int,
    refreshed_count: Int,
    terminal_sample_count: Int,
    messages: List(String),
  )
}

pub type SmokeCapability {
  SmokeCapability(run_smoke_check: fn() -> Result(SmokeReport, TrackerError))
}

pub type AttachmentCapability {
  AttachmentCapability(
    upload: fn(task.TaskRef, task.TaskAttachment) ->
      Result(task.TaskAttachment, TrackerError),
  )
}

pub type TrackerError {
  Unauthorized(message: String)
  NotFound(ref: task.TaskRef)
  Transient(message: String)
  Permanent(message: String)
  UnsupportedCapability(capability: String)
  DecodeFailed(message: String)
}

pub type TrackerRequirements {
  TrackerRequirements(
    remote_commands_enabled: Bool,
    remote_commands_config_path: Option(String),
    handoff_comments_enabled: Bool,
    handoff_state_moves_enabled: Bool,
    handoff_config_path: Option(String),
    workflow_label_paths: List(String),
    scheduled_failure_paths: List(String),
    readiness_checks_enabled: Bool,
    smoke_checks_enabled: Bool,
  )
}

pub type CapabilityValidationError {
  CapabilityValidationError(
    feature: String,
    capability: String,
    config_path: String,
    backend_kind: String,
    message: String,
  )
}

pub fn validate_required_capabilities(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
) -> Result(Nil, List(CapabilityValidationError)) {
  let errors = []
  let errors = validate_remote_commands(adapter, requirements, errors)
  let errors = validate_handoff(adapter, requirements, errors)
  let errors = validate_workflow_label_routing(adapter, requirements, errors)
  let errors = validate_scheduled_failures(adapter, requirements, errors)
  let errors = validate_readiness(adapter, requirements, errors)
  let errors = validate_smoke(adapter, requirements, errors)

  case errors {
    [] -> Ok(Nil)
    errors -> Error(errors)
  }
}

pub fn capability_validation_error_message(
  error: CapabilityValidationError,
) -> String {
  let CapabilityValidationError(
    feature: feature,
    capability: capability,
    config_path: config_path,
    backend_kind: backend_kind,
    message: message,
  ) = error

  "tracker_capability_missing feature="
  <> feature
  <> " capability="
  <> capability
  <> " path="
  <> config_path
  <> " backend="
  <> backend_kind
  <> " message=\""
  <> message
  <> "\""
}

fn validate_remote_commands(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
  errors: List(CapabilityValidationError),
) -> List(CapabilityValidationError) {
  let TrackerRequirements(
    remote_commands_enabled: remote_commands_enabled,
    remote_commands_config_path: remote_commands_config_path,
    ..,
  ) = requirements
  let path =
    option_with_default(remote_commands_config_path, "linear_commands.enabled")

  case remote_commands_enabled {
    False -> errors
    True -> {
      let errors =
        require_option(
          adapter.remote_commands,
          errors,
          missing_capability(
            adapter,
            "remote_commands",
            "remote_commands",
            path,
            "linear_commands.enabled requires tracker adapter "
              <> adapter.kind
              <> " to expose remote_commands",
          ),
        )
      require_option(
        adapter.comments,
        errors,
        missing_capability(
          adapter,
          "remote_command_ack",
          "comments",
          path,
          "remote command acknowledgements require comments capability",
        ),
      )
    }
  }
}

fn validate_handoff(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
  errors: List(CapabilityValidationError),
) -> List(CapabilityValidationError) {
  let TrackerRequirements(
    handoff_state_moves_enabled: handoff_state_moves_enabled,
    handoff_config_path: handoff_config_path,
    ..,
  ) = requirements
  let path = option_with_default(handoff_config_path, "handoff.states")

  case handoff_state_moves_enabled {
    False -> errors
    True ->
      require_option(
        adapter.state_transitions,
        errors,
        missing_capability(
          adapter,
          "handoff_state_moves",
          "state_transitions",
          path,
          "handoff state moves require state_transitions capability",
        ),
      )
  }
}

fn validate_workflow_label_routing(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
  errors: List(CapabilityValidationError),
) -> List(CapabilityValidationError) {
  let TrackerRequirements(workflow_label_paths: workflow_label_paths, ..) =
    requirements

  case adapter.routing_metadata {
    Some(_) -> errors
    None ->
      list.fold(workflow_label_paths, errors, fn(errors, path) {
        list.append(errors, [
          missing_capability(
            adapter,
            "workflow_label_routing",
            "routing_metadata",
            path,
            "workflow label routing requires routing_metadata capability",
          ),
        ])
      })
  }
}

fn validate_scheduled_failures(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
  errors: List(CapabilityValidationError),
) -> List(CapabilityValidationError) {
  let TrackerRequirements(scheduled_failure_paths: scheduled_failure_paths, ..) =
    requirements

  case adapter.scheduled_failures {
    Some(_) -> errors
    None ->
      list.fold(scheduled_failure_paths, errors, fn(errors, path) {
        list.append(errors, [
          missing_capability(
            adapter,
            "scheduled_failures",
            "scheduled_failures",
            path,
            "scheduled failure publication requires scheduled_failures capability",
          ),
        ])
      })
  }
}

fn validate_readiness(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
  errors: List(CapabilityValidationError),
) -> List(CapabilityValidationError) {
  let TrackerRequirements(
    readiness_checks_enabled: readiness_checks_enabled,
    ..,
  ) = requirements

  case readiness_checks_enabled {
    False -> errors
    True ->
      require_option(
        adapter.readiness,
        errors,
        missing_capability(
          adapter,
          "tracker_contract",
          "readiness",
          "doctor.checks.tracker-contract",
          "tracker contract checks require readiness capability",
        ),
      )
  }
}

fn validate_smoke(
  adapter: TrackerAdapter,
  requirements: TrackerRequirements,
  errors: List(CapabilityValidationError),
) -> List(CapabilityValidationError) {
  let TrackerRequirements(smoke_checks_enabled: smoke_checks_enabled, ..) =
    requirements

  case smoke_checks_enabled {
    False -> errors
    True ->
      require_option(
        adapter.smoke,
        errors,
        missing_capability(
          adapter,
          "tracker_smoke",
          "smoke",
          "doctor.checks.tracker-smoke",
          "tracker smoke checks require smoke capability",
        ),
      )
  }
}

fn require_option(
  capability: Option(a),
  errors: List(CapabilityValidationError),
  error: CapabilityValidationError,
) -> List(CapabilityValidationError) {
  case capability {
    Some(_) -> errors
    None -> list.append(errors, [error])
  }
}

fn option_with_default(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn missing_capability(
  adapter: TrackerAdapter,
  feature: String,
  capability: String,
  config_path: String,
  message: String,
) -> CapabilityValidationError {
  CapabilityValidationError(
    feature: feature,
    capability: capability,
    config_path: config_path,
    backend_kind: adapter.kind,
    message: message,
  )
}
