import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config as config_defaults
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear/task_query as linear_task_query
import scherzo/scheduled_failure_reporter
import scherzo/smoke
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/idempotency
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type Dependencies {
  Dependencies(
    transport: linear.Transport,
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
      ui_server: config_defaults.default_ui_server_config(),
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
    handoff_client: handoff_client,
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
    list_tasks: fn(request) { list_tasks(config, transport, request) },
    lookup_task_detail: fn(ref) { lookup_task_detail(config, transport, ref) },
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

fn list_tasks(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  request: adapter.TaskListRequest,
) -> Result(adapter.TaskPage, adapter.TrackerError) {
  let state_names =
    linear_state_names_for_query(config, request.state_categories)
  case state_names {
    [] ->
      Error(adapter.UnsupportedCapability(
        "unfiltered Linear task list; pass at least one configured --state filter",
      ))
    state_names ->
      fetch_task_page_from_offset(
        config,
        transport,
        state_names,
        request.state_categories,
        request.offset,
        request.limit,
        None,
        0,
        [],
      )
  }
}

fn fetch_task_page_from_offset(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  state_names: List(issue_state.IssueState),
  categories: List(task.TaskStateCategory),
  offset: Int,
  limit: Int,
  after: Option(String),
  skipped: Int,
  acc: List(task.Task),
) -> Result(adapter.TaskPage, adapter.TrackerError) {
  use page <- try_adapter(linear_task_query.fetch_page(
    config,
    state_names,
    after,
    transport,
  ))
  let tasks =
    page.nodes
    |> list.map(fn(item) { categorize_task(config, item) })
    |> filter_categories(categories)
  let #(skipped, acc, reached_limit, has_buffered_more) =
    collect_page(tasks, offset, limit, skipped, acc)
  case reached_limit {
    True ->
      Ok(adapter.TaskPage(
        items: list.reverse(acc),
        has_more: has_buffered_more || page.has_next_page,
      ))
    False ->
      case page.has_next_page, page.end_cursor {
        True, Some(cursor) ->
          fetch_task_page_from_offset(
            config,
            transport,
            state_names,
            categories,
            offset,
            limit,
            Some(cursor),
            skipped,
            acc,
          )
        True, None ->
          Error(adapter.DecodeFailed(
            "Linear response missing pagination endCursor",
          ))
        False, _ ->
          Ok(adapter.TaskPage(items: list.reverse(acc), has_more: False))
      }
  }
}

fn lookup_task_detail(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  ref: adapter.TaskLookupRef,
) -> Result(Option(task.Task), adapter.TrackerError) {
  case ref {
    adapter.TaskLookupByDisplayId(display_id) -> {
      let display_id = string.trim(display_id)
      case display_id == "" {
        True -> Ok(None)
        False -> {
          use found <- try_adapter(linear_task_query.fetch_detail_by_identifier(
            config,
            display_id,
            transport,
          ))
          Ok(option_map(found, fn(item) { categorize_task(config, item) }))
        }
      }
    }
    adapter.TaskLookupByRemoteId(provider: provider, id: id) ->
      case provider_allowed(provider) {
        False -> Ok(None)
        True -> {
          use found <- try_adapter(linear_task_query.fetch_detail_by_id(
            config,
            normalize_linear_remote_id(id),
            transport,
          ))
          Ok(option_map(found, fn(item) { categorize_task(config, item) }))
        }
      }
  }
}

fn linear_state_names_for_query(
  config: config_types.TrackerConfig,
  categories: List(task.TaskStateCategory),
) -> List(issue_state.IssueState) {
  case categories {
    [] -> []
    categories ->
      case must_query_without_state_filter(categories) {
        True -> []
        False ->
          categories
          |> list.flat_map(fn(category) {
            states_for_category(config, category)
          })
          |> dedupe_states([])
      }
  }
}

fn must_query_without_state_filter(
  categories: List(task.TaskStateCategory),
) -> Bool {
  list.any(categories, fn(category) {
    case category {
      task.Backlog | task.Unknown -> True
      _ -> False
    }
  })
}

fn states_for_category(
  config: config_types.TrackerConfig,
  category: task.TaskStateCategory,
) -> List(issue_state.IssueState) {
  case category {
    task.Ready -> config.dispatch_states
    task.Active -> active_only_states(config)
    task.Done -> terminal_states_for(config, task.Done)
    task.Canceled -> terminal_states_for(config, task.Canceled)
    task.Duplicate -> terminal_states_for(config, task.Duplicate)
    task.Backlog | task.Unknown -> []
  }
}

fn active_only_states(
  config: config_types.TrackerConfig,
) -> List(issue_state.IssueState) {
  let active =
    list.filter(config.active_states, fn(state) {
      !issue_state.contains_normalized(config.dispatch_states, state)
    })
  case active {
    [] -> config.active_states
    _ -> active
  }
}

fn terminal_states_for(
  config: config_types.TrackerConfig,
  category: task.TaskStateCategory,
) -> List(issue_state.IssueState) {
  list.filter(config.terminal_states, fn(state) {
    state_name_category(issue_state.to_string(state)) == category
  })
}

fn dedupe_states(
  states: List(issue_state.IssueState),
  acc: List(issue_state.IssueState),
) -> List(issue_state.IssueState) {
  case states {
    [] -> list.reverse(acc)
    [state, ..rest] ->
      case issue_state.contains_normalized(acc, state) {
        True -> dedupe_states(rest, acc)
        False -> dedupe_states(rest, [state, ..acc])
      }
  }
}

fn categorize_task(
  config: config_types.TrackerConfig,
  item: task.Task,
) -> task.Task {
  let state = item.state
  let category = state_category(config, state)
  task.Task(
    ..item,
    state: task.TaskState(id: state.id, name: state.name, category: category),
  )
}

fn state_category(
  config: config_types.TrackerConfig,
  state: task.TaskState,
) -> task.TaskStateCategory {
  let state_value = issue_state.from_string_unchecked(state.name)
  case issue_state.contains_normalized(config.dispatch_states, state_value) {
    True -> task.Ready
    False ->
      case issue_state.contains_normalized(config.active_states, state_value) {
        True -> task.Active
        False ->
          case
            issue_state.contains_normalized(config.terminal_states, state_value)
          {
            True -> terminal_category(state.name)
            False ->
              case state.category {
                task.Unknown -> state_name_category(state.name)
                category -> category
              }
          }
      }
  }
}

fn terminal_category(name: String) -> task.TaskStateCategory {
  case state_name_category(name) {
    task.Canceled -> task.Canceled
    task.Duplicate -> task.Duplicate
    _ -> task.Done
  }
}

fn state_name_category(name: String) -> task.TaskStateCategory {
  let name = name |> string.trim |> string.lowercase
  case name {
    "backlog" -> task.Backlog
    "todo" | "to do" | "ready" | "triage" -> task.Ready
    "in progress" | "doing" | "started" -> task.Active
    "done" | "complete" | "completed" -> task.Done
    "canceled" | "cancelled" -> task.Canceled
    "duplicate" -> task.Duplicate
    _ -> task.Unknown
  }
}

fn filter_categories(
  tasks: List(task.Task),
  categories: List(task.TaskStateCategory),
) -> List(task.Task) {
  case categories {
    [] -> tasks
    categories ->
      list.filter(tasks, fn(item) {
        list.contains(categories, item.state.category)
      })
  }
}

fn collect_page(
  tasks: List(task.Task),
  offset: Int,
  limit: Int,
  skipped: Int,
  acc: List(task.Task),
) -> #(Int, List(task.Task), Bool, Bool) {
  case tasks {
    [] -> #(skipped, acc, False, False)
    [item, ..rest] ->
      case skipped < offset {
        True -> collect_page(rest, offset, limit, skipped + 1, acc)
        False ->
          case list.length(acc) >= limit {
            True -> #(skipped, acc, True, True)
            False -> collect_page(rest, offset, limit, skipped, [item, ..acc])
          }
      }
  }
}

fn provider_allowed(provider: Option(String)) -> Bool {
  case provider {
    Some("linear") | None -> True
    Some(_) -> False
  }
}

fn normalize_linear_remote_id(id: String) -> String {
  let id = string.trim(id)
  case string.starts_with(id, "linear:") {
    True -> string.drop_start(id, 7)
    False -> id
  }
}

fn option_map(value: Option(a), mapper: fn(a) -> b) -> Option(b) {
  case value {
    Some(value) -> Some(mapper(value))
    None -> None
  }
}

fn comment_capability(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> adapter.CommentCapability {
  adapter.CommentCapability(
    post_or_update: fn(request) {
      post_or_update_comment(config, transport, request)
    },
    find_by_marker: fn(request) {
      find_comment_by_marker(config, transport, request)
    },
  )
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
      case idempotency.extract_key(body) {
        Ok(marker) ->
          upsert_comment_by_marker(
            config,
            transport,
            task_ref,
            issue_id,
            marker,
            body,
          )
        Error(Nil) ->
          create_comment(config, transport, task_ref, issue_id, body)
      }
    adapter.UpdateExisting(
      comment_id: comment_id,
      allow_create_fallback: allow_create_fallback,
    ) ->
      case update_comment(config, transport, task_ref, comment_id, body) {
        Ok(receipt) -> Ok(receipt)
        Error(err) ->
          case allow_create_fallback {
            True -> create_comment(config, transport, task_ref, issue_id, body)
            False -> Error(err)
          }
      }
  }
}

fn find_comment_by_marker(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  request: adapter.CommentLookup,
) -> Result(Option(adapter.CommentReceipt), adapter.TrackerError) {
  let adapter.CommentLookup(task: task_ref, marker: marker) = request
  use issue_id <- try_adapter_result(require_linear_ref(task_ref))
  use found <- try_adapter(linear.find_issue_comment_by_marker(
    config,
    transport,
    issue_id,
    marker,
  ))
  Ok(
    option_map(found, fn(comment) {
      adapter.CommentReceipt(
        id: comment.id,
        task: task_ref,
        url: task_ref.url,
        created: False,
      )
    }),
  )
}

fn upsert_comment_by_marker(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  task_ref: task.TaskRef,
  issue_id: String,
  marker: String,
  body: String,
) -> Result(adapter.CommentReceipt, adapter.TrackerError) {
  case
    linear.find_issue_comment_by_marker(config, transport, issue_id, marker)
  {
    Ok(Some(comment)) ->
      update_comment(config, transport, task_ref, comment.id, body)
    Ok(None) -> create_comment(config, transport, task_ref, issue_id, body)
    Error(err) -> Error(map_tracker_error(err))
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
      case status == 429 || status >= 500 {
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
