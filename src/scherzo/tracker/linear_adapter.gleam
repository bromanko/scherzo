import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config as config_defaults
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/scheduled_failure_reporter
import scherzo/smoke
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type Dependencies {
  Dependencies(
    transport: linear.Transport,
    command_client: linear.CommandClient,
    handoff_client: handoff.Client,
    scheduled_failure_client: scheduled_failure_reporter.Client,
  )
}

pub fn real(effective: config_types.EffectiveConfig) -> adapter.TrackerAdapter {
  from_effective_config(effective, linear.http_transport)
}

pub fn from_tracker_config(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.TrackerAdapter {
  from_effective_config(
    config_types.EffectiveConfig(
      tracker: config,
      polling: config_defaults.default_polling_config(),
      workspace: config_types.WorkspaceConfig(root: "."),
      hooks: config_defaults.default_hooks_config(),
      agent: config_defaults.default_agent_config(),
      pi: config_defaults.default_pi_config(),
      handoff: config_defaults.default_handoff_config(),
      linear_contract: config_defaults.default_linear_contract_config(),
      linear_commands: config_defaults.default_linear_command_config(),
    ),
    transport,
  )
}

pub fn from_effective_config(
  effective: config_types.EffectiveConfig,
  transport: linear.Transport,
) -> adapter.TrackerAdapter {
  from_dependencies(
    effective,
    Dependencies(
      transport: transport,
      command_client: linear.command_client(effective.tracker, transport),
      handoff_client: handoff.linear_client(
        effective.tracker,
        effective.handoff,
        transport,
      ),
      scheduled_failure_client: scheduled_failure_reporter.real_client_with_transport(
        effective.tracker,
        transport,
      ),
    ),
  )
}

pub fn from_dependencies(
  effective: config_types.EffectiveConfig,
  dependencies: Dependencies,
) -> adapter.TrackerAdapter {
  let config = effective.tracker
  let Dependencies(
    transport: transport,
    command_client: command_client,
    handoff_client: handoff_client,
    scheduled_failure_client: scheduled_failure_client,
  ) = dependencies

  adapter.TrackerAdapter(
    kind: "linear",
    display_name: "Linear",
    task_source: task_source_capability(config, transport),
    comments: Some(comment_capability(config, transport)),
    remote_commands: Some(remote_command_capability(command_client)),
    state_transitions: Some(state_transition_capability(config, transport)),
    routing_metadata: Some(routing_metadata_capability()),
    links: None,
    handoff: Some(handoff_capability(handoff_client)),
    scheduled_failures: Some(scheduled_failure_capability(
      scheduled_failure_client,
    )),
    readiness: None,
    smoke: Some(smoke_capability(config, transport)),
    attachments: None,
  )
}

fn task_source_capability(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.TaskSourceCapability {
  adapter.TaskSourceCapability(
    fetch_candidates: fn(request) {
      fetch_candidate_tasks(config, transport, request)
    },
    refresh_by_refs: fn(refs) { refresh_tasks_by_refs(config, transport, refs) },
    lookup_by_operator_ref: fn(operator_ref) {
      lookup_task_by_operator_ref(config, transport, operator_ref)
    },
  )
}

fn fetch_candidate_tasks(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  request: adapter.TaskSearchRequest,
) -> Result(List(task.Task), adapter.TrackerError) {
  let states = case request.dispatch_states {
    [] -> config.dispatch_states
    states -> issue_state.list_from_strings(states)
  }
  use issues <- try_adapter(linear.fetch_issues_by_states(
    config,
    states,
    transport,
  ))
  Ok(list.map(issues, task.from_legacy_issue))
}

fn refresh_tasks_by_refs(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  refs: List(task.TaskRef),
) -> Result(List(task.Task), adapter.TrackerError) {
  use remote_ids <- try_adapter_result(linear_remote_ids(refs))
  use issues <- try_adapter(linear.fetch_issue_states_by_ids(
    config,
    remote_ids,
    transport,
  ))
  Ok(list.map(issues, task.from_legacy_issue))
}

fn lookup_task_by_operator_ref(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  operator_ref: String,
) -> Result(Option(task.Task), adapter.TrackerError) {
  let operator_ref = string.trim(operator_ref)
  case operator_ref == "" {
    True -> Ok(None)
    False -> {
      use issues <- try_adapter(linear.fetch_issue_states_by_ids(
        config,
        [operator_ref],
        transport,
      ))
      case list.map(issues, task.from_legacy_issue) {
        [] ->
          lookup_candidate_task_by_identifier(config, transport, operator_ref)
        [task, ..] -> Ok(Some(task))
      }
    }
  }
}

fn lookup_candidate_task_by_identifier(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  identifier: String,
) -> Result(Option(task.Task), adapter.TrackerError) {
  let request =
    adapter.TaskSearchRequest(
      active_states: [],
      dispatch_states: [],
      terminal_states: [],
      workflow_labels: [],
      limit: 100,
    )
  case fetch_candidate_tasks(config, transport, request) {
    Error(err) -> Error(err)
    Ok(tasks) ->
      case
        list.find(tasks, fn(item) { task.display_key(item.ref) == identifier })
      {
        Ok(task) -> Ok(Some(task))
        Error(Nil) -> Ok(None)
      }
  }
}

fn comment_capability(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.CommentCapability {
  adapter.CommentCapability(post_or_update: fn(request) {
    post_or_update_comment(config, transport, request)
  })
}

fn post_or_update_comment(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  request: adapter.CommentRequest,
) -> Result(adapter.CommentReceipt, adapter.TrackerError) {
  let adapter.CommentRequest(task: task_ref, body: body, mode: mode) = request
  use issue_id <- try_adapter_result(require_linear_ref(task_ref))
  case mode {
    adapter.CreateOnly ->
      create_comment(config, transport, task_ref, issue_id, body)
    adapter.UpdateExisting(comment_id: comment_id, ..) ->
      update_comment(config, transport, task_ref, comment_id, body)
  }
}

fn create_comment(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  task_ref: task.TaskRef,
  issue_id: String,
  body: String,
) -> Result(adapter.CommentReceipt, adapter.TrackerError) {
  use comment <- try_adapter(create_linear_comment(
    config,
    transport,
    issue_id,
    body,
  ))
  Ok(adapter.CommentReceipt(
    id: comment.id,
    task: task_ref,
    url: task_ref.url,
    created: True,
  ))
}

fn update_comment(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  task_ref: task.TaskRef,
  comment_id: String,
  body: String,
) -> Result(adapter.CommentReceipt, adapter.TrackerError) {
  use comment <- try_adapter(update_linear_comment(
    config,
    transport,
    comment_id,
    body,
  ))
  Ok(adapter.CommentReceipt(
    id: comment.id,
    task: task_ref,
    url: task_ref.url,
    created: False,
  ))
}

fn remote_command_capability(
  client: linear.CommandClient,
) -> adapter.RemoteCommandCapability {
  adapter.RemoteCommandCapability(
    fetch_events: fn(request) { fetch_remote_command_events(client, request) },
    post_ack: fn(ack) { post_remote_command_ack(client, ack) },
  )
}

fn fetch_remote_command_events(
  client: linear.CommandClient,
  request: adapter.RemoteCommandFetch,
) -> Result(List(adapter.RemoteCommandEvent), adapter.TrackerError) {
  use issue_ids <- try_adapter_result(linear_remote_ids(request.task_refs))
  use comments <- try_adapter(client.fetch_comments(
    issue_ids,
    request.limit_per_task,
  ))
  Ok(list.map(comments, linear_comment_to_remote_command_event))
}

fn linear_comment_to_remote_command_event(
  comment: linear.LinearComment,
) -> adapter.RemoteCommandEvent {
  adapter.RemoteCommandEvent(
    event_id: comment.id,
    task: linear_task_ref(comment.issue_id, None),
    author_id: comment.author.id,
    body: comment.body,
    command_name: "",
    excerpt: comment.body,
    observed_at_ms: comment.created_at_ms,
  )
}

fn post_remote_command_ack(
  client: linear.CommandClient,
  ack: adapter.RemoteCommandAck,
) -> Result(adapter.CommentReceipt, adapter.TrackerError) {
  let adapter.RemoteCommandAck(event: event, body: body) = ack
  let adapter.RemoteCommandEvent(event_id: event_id, task: task_ref, ..) = event
  use issue_id <- try_adapter_result(require_linear_ref(task_ref))
  use Nil <- try_adapter(client.post_ack(issue_id, body))
  Ok(adapter.CommentReceipt(
    id: event_id,
    task: task_ref,
    url: task_ref.url,
    created: True,
  ))
}

fn handoff_capability(client: handoff.Client) -> adapter.HandoffCapability {
  adapter.HandoffCapability(report: fn(event) { report_handoff(client, event) })
}

fn report_handoff(
  client: handoff.Client,
  event: adapter.HandoffEvent,
) -> Result(Nil, adapter.TrackerError) {
  case event {
    adapter.HandoffClaim(task_context, _workspace_path, run_id) -> {
      use issue <- try_adapter_result(linear_handoff_issue(task_context))
      try_adapter(client.claim_issue(issue, run_id), fn(_) { Ok(Nil) })
    }
    adapter.HandoffSuccess(task_context, success, run_id, workflow_id) -> {
      use issue <- try_adapter_result(linear_handoff_issue(task_context))
      try_adapter(
        client.report_success_for_workflow(issue, success, run_id, workflow_id),
        fn(_) { Ok(Nil) },
      )
    }
    adapter.HandoffFailure(task_context, failure, run_id, workflow_id) -> {
      use issue <- try_adapter_result(linear_handoff_issue(task_context))
      try_adapter(
        client.report_failure_for_workflow(issue, failure, run_id, workflow_id),
        fn(_) { Ok(Nil) },
      )
    }
    adapter.HandoffPark(report) -> report_linear_park(client, report)
  }
}

fn linear_handoff_issue(
  task_context: task.Task,
) -> Result(tracker_issue.Issue, adapter.TrackerError) {
  let task.Task(ref: task_ref, ..) = task_context
  case require_linear_ref(task_ref) {
    Ok(_) -> Ok(task.to_runtime_issue(task_context))
    Error(err) -> Error(err)
  }
}

fn report_linear_park(
  client: handoff.Client,
  report: adapter.ParkReport,
) -> Result(Nil, adapter.TrackerError) {
  let adapter.ParkReport(
    task: task_ref,
    issue_identifier: issue_identifier,
    reason: reason,
    release_policy: release_policy,
    run_id: run_id,
  ) = report
  use issue_id <- try_adapter_result(require_linear_ref(task_ref))
  try_adapter(
    client.report_park(handoff.ParkReport(
      issue_id: issue_id,
      issue_identifier: issue_identifier,
      reason: reason,
      release_policy: release_policy,
      run_id: run_id,
    )),
    fn(_) { Ok(Nil) },
  )
}

fn state_transition_capability(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.StateTransitionCapability {
  adapter.StateTransitionCapability(transition: fn(request) {
    transition_state(config, transport, request)
  })
}

fn transition_state(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  request: adapter.StateTransitionRequest,
) -> Result(adapter.StateTransitionReceipt, adapter.TrackerError) {
  let adapter.StateTransitionRequest(
    task: task_ref,
    target_state_id: target_state_id,
    target_state_name: target_state_name,
    ..,
  ) = request
  use issue_id <- try_adapter_result(require_linear_ref(task_ref))
  use state_id <- try_adapter_result(resolve_state_id(
    config,
    transport,
    issue_id,
    target_state_id,
    target_state_name,
  ))
  use Nil <- try_adapter(update_linear_issue_state(
    config,
    transport,
    issue_id,
    state_id,
  ))
  Ok(adapter.StateTransitionReceipt(
    task: task_ref,
    state: task.TaskState(
      id: Some(state_id),
      name: target_state_name,
      category: task.Unknown,
    ),
  ))
}

fn routing_metadata_capability() -> adapter.RoutingMetadataCapability {
  adapter.RoutingMetadataCapability(
    workflow_labels: fn(value) { task.label_names(value) },
    blocker_refs: fn(value) { value.blockers },
  )
}

fn scheduled_failure_capability(
  client: scheduled_failure_reporter.Client,
) -> adapter.ScheduledFailureCapability {
  adapter.ScheduledFailureCapability(publish: fn(publication) {
    publish_scheduled_failure(client, publication)
  })
}

fn publish_scheduled_failure(
  client: scheduled_failure_reporter.Client,
  publication: adapter.ScheduledFailurePublication,
) -> Result(adapter.ScheduledFailureReceipt, adapter.TrackerError) {
  use target_state_name <- try_adapter_result(require_scheduled_target_state(
    publication.target_state_name,
  ))
  use outcome <- try_adapter(
    client.report_failure(scheduled_failure_reporter.FailureReportRequest(
      job_id: publication.job_id,
      workflow_id: publication.workflow_id,
      due_at_ms: publication.due_at_ms,
      run_id: publication.run_id,
      attempt: publication.attempt,
      max_attempts: publication.max_attempts,
      reason: publication.reason,
      run_root: publication.run_root,
      session_id: publication.session_id,
      dedupe_key: publication.dedupe_key,
      triage_state: target_state_name,
      configured_labels: publication.labels,
      previous_issue_id: publication.previous_task_remote_id,
    )),
  )
  scheduled_failure_receipt(outcome)
}

fn scheduled_failure_receipt(
  outcome: scheduled_failure_reporter.FailureReportOutcome,
) -> Result(adapter.ScheduledFailureReceipt, adapter.TrackerError) {
  case outcome {
    scheduled_failure_reporter.FailureReportCreated(issue_id) ->
      Ok(adapter.ScheduledFailureReceipt(
        task: linear_task_ref(issue_id, None),
        created: True,
        comment_id: None,
      ))
    scheduled_failure_reporter.FailureReportUpdated(issue_id) ->
      Ok(adapter.ScheduledFailureReceipt(
        task: linear_task_ref(issue_id, None),
        created: False,
        comment_id: None,
      ))
    scheduled_failure_reporter.FailureReportNoop ->
      Error(adapter.UnsupportedCapability("scheduled_failures.publish"))
  }
}

fn smoke_capability(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.SmokeCapability {
  adapter.SmokeCapability(run_smoke_check: fn() {
    use result <- try_adapter(smoke.linear_read_smoke(
      smoke.linear_reader(config, transport),
      config.terminal_states,
    ))
    Ok(
      adapter.SmokeReport(
        candidate_count: result.candidate_count,
        refreshed_count: result.refreshed_count,
        terminal_sample_count: result.terminal_count,
        messages: [],
      ),
    )
  })
}

fn create_linear_comment(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  body: String,
) -> Result(linear.LinearCommentDocument, error.TrackerError) {
  use request <- try_linear(linear.build_comment_create_request(
    config,
    issue_id,
    body,
  ))
  use response <- try_linear(transport(request))
  linear.parse_comment_create_response(response)
}

fn update_linear_comment(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  comment_id: String,
  body: String,
) -> Result(linear.LinearCommentDocument, error.TrackerError) {
  use request <- try_linear(linear.build_comment_update_body_request(
    config,
    comment_id,
    body,
  ))
  use response <- try_linear(transport(request))
  linear.parse_comment_update_response(response)
}

fn update_linear_issue_state(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state_id: String,
) -> Result(Nil, error.TrackerError) {
  use request <- try_linear(linear.build_issue_update_state_request(
    config,
    issue_id,
    state_id,
  ))
  use response <- try_linear(transport(request))
  linear.parse_mutation_response(response, "issueUpdate")
}

fn linear_remote_ids(
  refs: List(task.TaskRef),
) -> Result(List(String), adapter.TrackerError) {
  linear_remote_ids_loop(refs, [])
}

fn linear_remote_ids_loop(
  refs: List(task.TaskRef),
  acc: List(String),
) -> Result(List(String), adapter.TrackerError) {
  case refs {
    [] -> Ok(list.reverse(acc))
    [ref, ..rest] -> {
      use remote_id <- try_adapter_result(require_linear_ref(ref))
      linear_remote_ids_loop(rest, [remote_id, ..acc])
    }
  }
}

fn require_linear_ref(
  ref: task.TaskRef,
) -> Result(String, adapter.TrackerError) {
  let task.TaskRef(backend_kind: backend_kind, remote_id: remote_id, ..) = ref
  let remote_id = string.trim(remote_id)
  case backend_kind == "linear" && remote_id != "" {
    True -> Ok(remote_id)
    False -> Error(adapter.NotFound(ref))
  }
}

fn resolve_state_id(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state_id: Option(String),
  state_name: String,
) -> Result(String, adapter.TrackerError) {
  case normalized_optional(state_id) {
    Some(value) -> Ok(value)
    None -> resolve_state_name(config, transport, issue_id, state_name)
  }
}

fn resolve_state_name(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
  state_name: String,
) -> Result(String, adapter.TrackerError) {
  let state_name = string.trim(state_name)
  case state_name == "" {
    True ->
      Error(adapter.Permanent(
        "Linear state transitions require target_state_id or target_state_name",
      ))
    False -> {
      use request <- try_adapter(linear.build_issue_team_states_request(
        config,
        issue_id,
      ))
      use response <- try_adapter(transport(request))
      use states <- try_adapter(linear.parse_issue_team_states_response(
        response,
      ))
      case linear.resolve_state_name(states, state_name) {
        Ok(state_id) -> Ok(state_id)
        Error(linear.StateNameNotFound) ->
          Error(adapter.Permanent("Linear state not found: " <> state_name))
        Error(linear.StateNameAmbiguous) ->
          Error(adapter.Permanent("Linear state ambiguous: " <> state_name))
      }
    }
  }
}

fn normalized_optional(value: Option(String)) -> Option(String) {
  case value {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True -> None
        False -> Some(value)
      }
    }
    None -> None
  }
}

fn require_scheduled_target_state(
  value: Option(String),
) -> Result(String, adapter.TrackerError) {
  case value {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True ->
          Error(adapter.Permanent(
            "Linear scheduled failure publication requires target_state_name",
          ))
        False -> Ok(value)
      }
    }
    None ->
      Error(adapter.Permanent(
        "Linear scheduled failure publication requires target_state_name",
      ))
  }
}

fn linear_task_ref(remote_id: String, key: Option(String)) -> task.TaskRef {
  task.TaskRef(
    backend_kind: "linear",
    remote_id: remote_id,
    key: key,
    url: None,
  )
}

fn map_tracker_error(error: error.TrackerError) -> adapter.TrackerError {
  case error {
    error.LinearApiRequest(message) -> adapter.Permanent(message)
    error.LinearApiStatus(status) -> map_status_error(status, "Linear API")
    error.LinearGraphqlErrors(message) ->
      adapter.Permanent("Linear GraphQL errors: " <> message)
    error.LinearUnknownPayload(message) -> adapter.DecodeFailed(message)
    error.LinearMissingEndCursor ->
      adapter.DecodeFailed("Linear response missing pagination endCursor")
    error.LinearUploadStatus(status) ->
      map_status_error(status, "Linear upload")
    error.LinearAttachmentError(message) -> adapter.Permanent(message)
  }
}

fn map_status_error(status: Int, source: String) -> adapter.TrackerError {
  let message = source <> " returned status " <> int.to_string(status)
  case status == 401 || status == 403 {
    True -> adapter.Unauthorized(message)
    False ->
      case status >= 500 {
        True -> adapter.Transient(message)
        False -> adapter.Permanent(message)
      }
  }
}

fn try_adapter(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, adapter.TrackerError),
) -> Result(b, adapter.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(map_tracker_error(error))
  }
}

fn try_adapter_result(
  result: Result(a, adapter.TrackerError),
  next: fn(a) -> Result(b, adapter.TrackerError),
) -> Result(b, adapter.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(error)
  }
}

fn try_linear(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(error)
  }
}
