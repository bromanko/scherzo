import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/scheduled_failure_reporter
import scherzo/smoke
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/state as issue_state

pub type Dependencies {
  Dependencies(
    transport: linear.Transport,
    scheduled_failure_client: scheduled_failure_reporter.Client,
  )
}

pub fn real(config: config_types.TrackerConfig) -> adapter.TrackerAdapter {
  from_tracker_config(config, linear.http_transport)
}

pub fn from_tracker_config(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.TrackerAdapter {
  from_dependencies(
    config,
    Dependencies(
      transport: transport,
      scheduled_failure_client: scheduled_failure_reporter.real_client_with_transport(
        config,
        transport,
      ),
    ),
  )
}

pub fn from_dependencies(
  config: config_types.TrackerConfig,
  dependencies: Dependencies,
) -> adapter.TrackerAdapter {
  let Dependencies(
    transport: transport,
    scheduled_failure_client: scheduled_failure_client,
  ) = dependencies

  adapter.TrackerAdapter(
    kind: "linear",
    display_name: "Linear",
    task_source: task_source_capability(config, transport),
    comments: Some(comment_capability(config, transport)),
    remote_commands: None,
    state_transitions: Some(state_transition_capability(config, transport)),
    routing_metadata: Some(routing_metadata_capability()),
    links: None,
    handoff: None,
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
      use tasks <- try_adapter(linear.fetch_issue_states_by_ids(
        config,
        [operator_ref],
        transport,
      ))
      case list.map(tasks, task.from_legacy_issue) {
        [] -> Ok(None)
        [task, ..] -> Ok(Some(task))
      }
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
  use state_id <- try_adapter_result(require_state_id(target_state_id))
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
      due_at_ms: 0,
      run_id: publication.run_id,
      attempt: 1,
      max_attempts: 1,
      reason: publication.body,
      run_root: None,
      session_id: None,
      dedupe_key: publication.dedupe_key,
      triage_state: target_state_name,
      configured_labels: publication.labels,
      previous_issue_id: None,
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

fn require_state_id(
  value: Option(String),
) -> Result(String, adapter.TrackerError) {
  case value {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True ->
          Error(adapter.Permanent(
            "Linear state transitions require target_state_id",
          ))
        False -> Ok(value)
      }
    }
    None ->
      Error(adapter.Permanent(
        "Linear state transitions require target_state_id",
      ))
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
