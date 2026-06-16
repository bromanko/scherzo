import birl
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/linear_task_scope
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/task
import scherzo/tracker/state as issue_state
import scherzo/work_item

pub type Page {
  Page(
    items: List(work_item.WorkItemSummary),
    has_next_page: Bool,
    end_cursor: Option(String),
  )
}

type RawIssue {
  RawIssue(
    task: task.Task,
    labels_has_more: Bool,
    children: List(task.Task),
    children_has_more: Bool,
  )
}

pub fn fetch_page(
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  after: Option(String),
  subtask_limit: Int,
  label_limit: Int,
  transport: linear.Transport,
) -> Result(Page, error.TrackerError) {
  use request <- try_tracker(build_list_request(
    config,
    states,
    after,
    subtask_limit,
    label_limit,
  ))
  use response <- try_tracker(transport(request))
  use page <- try_tracker(parse_page_response(
    response,
    subtask_limit,
    label_limit,
  ))
  Ok(recategorize_page(page, config))
}

pub fn fetch_detail_by_id(
  config: config_types.TrackerConfig,
  id: String,
  subtask_limit: Int,
  label_limit: Int,
  transport: linear.Transport,
) -> Result(Option(work_item.WorkItemDetail), error.TrackerError) {
  use request <- try_tracker(build_detail_by_id_request(
    config,
    id,
    subtask_limit,
    label_limit,
  ))
  use response <- try_tracker(transport(request))
  use found <- try_tracker(parse_detail_by_id_response(
    response,
    subtask_limit,
    label_limit,
  ))
  Ok(option_map(found, fn(item) { recategorize_detail(item, config) }))
}

pub fn fetch_detail_by_identifier(
  config: config_types.TrackerConfig,
  identifier: String,
  subtask_limit: Int,
  label_limit: Int,
  transport: linear.Transport,
) -> Result(Option(work_item.WorkItemDetail), error.TrackerError) {
  use scope <- try_tracker(require_task_scope(config))
  use request <- try_tracker(build_detail_by_identifier_request(
    config,
    identifier,
    subtask_limit,
    label_limit,
  ))
  use response <- try_tracker(transport(request))
  use found <- try_tracker(parse_detail_by_identifier_response(
    response,
    scope,
    identifier,
    subtask_limit,
    label_limit,
  ))
  Ok(option_map(found, fn(item) { recategorize_detail(item, config) }))
}

pub fn build_list_request(
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  after: Option(String),
  _subtask_limit: Int,
  label_limit: Int,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use scope <- try_tracker(require_task_scope(config))
  let variables =
    linear_task_scope.issue_filter_variables(scope, "taskFilter")
    |> list.append([
      #(
        "stateNames",
        json.array(issue_state.to_strings(states), of: json.string),
      ),
      #("after", json.nullable(after, of: json.string)),
      #("labelLimit", json.int(label_limit + 1)),
    ])
  let body =
    json.object([
      #("query", json.string(list_query_for_scope(scope))),
      #("variables", json.object(variables)),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_detail_by_id_request(
  config: config_types.TrackerConfig,
  id: String,
  subtask_limit: Int,
  label_limit: Int,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use scope <- try_tracker(require_task_scope(config))
  let variables =
    linear_task_scope.issue_filter_variables(scope, "taskFilter")
    |> list.append([
      #("ids", json.array([id], of: json.string)),
      #("labelLimit", json.int(label_limit + 1)),
      #("childLimit", json.int(subtask_limit + 1)),
    ])
  let body =
    json.object([
      #("query", json.string(detail_by_id_query_for_scope(scope))),
      #("variables", json.object(variables)),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_detail_by_identifier_request(
  config: config_types.TrackerConfig,
  identifier: String,
  subtask_limit: Int,
  label_limit: Int,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(detail_by_identifier_query())),
      #(
        "variables",
        json.object([
          #("issueId", json.string(identifier)),
          #("labelLimit", json.int(label_limit + 1)),
          #("childLimit", json.int(subtask_limit + 1)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn list_query() -> String {
  list_query_for_scope(config_types.LinearTaskProject("projectSlug"))
}

fn list_query_for_scope(_scope: config_types.LinearTaskScope) -> String {
  "query ScherzoWorkItemList("
  <> linear_task_scope.issue_filter_declaration("taskFilter")
  <> ", $stateNames: [String!], $after: String, $labelLimit: Int!) { issues(first: 50, after: $after, filter: { and: [$taskFilter], state: { name: { in: $stateNames } } }) { nodes { "
  <> summary_issue_fields()
  <> " } pageInfo { hasNextPage endCursor } } }"
}

pub fn detail_by_id_query() -> String {
  detail_by_id_query_for_scope(config_types.LinearTaskProject("projectSlug"))
}

fn detail_by_id_query_for_scope(
  _scope: config_types.LinearTaskScope,
) -> String {
  "query ScherzoWorkItemDetailById("
  <> linear_task_scope.issue_filter_declaration("taskFilter")
  <> ", $ids: [ID!]!, $labelLimit: Int!, $childLimit: Int!) { issues(first: 2, filter: { and: [$taskFilter], id: { in: $ids } }) { nodes { "
  <> detail_issue_fields()
  <> " } pageInfo { hasNextPage endCursor } } }"
}

pub fn detail_by_identifier_query() -> String {
  "query ScherzoWorkItemDetailByIdentifier($issueId: String!, $labelLimit: Int!, $childLimit: Int!) { issue(id: $issueId) { project { slugId } "
  <> detail_issue_fields()
  <> " } }"
}

fn summary_issue_fields() -> String {
  "id identifier title url createdAt updatedAt state { id name type } labels(first: $labelLimit) { nodes { id name } pageInfo { hasNextPage endCursor } }"
}

fn detail_issue_fields() -> String {
  summary_issue_fields()
  <> " children(first: $childLimit) { nodes { id identifier title url createdAt updatedAt state { id name type } labels(first: $labelLimit) { nodes { id name } pageInfo { hasNextPage endCursor } } } pageInfo { hasNextPage endCursor } }"
}

pub fn parse_page_response(
  response: linear.Response,
  subtask_limit: Int,
  label_limit: Int,
) -> Result(Page, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, page_graphql_decoder()) {
        Ok(Ok(page)) -> page_to_work_item_page(page, subtask_limit, label_limit)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_detail_by_id_response(
  response: linear.Response,
  subtask_limit: Int,
  label_limit: Int,
) -> Result(Option(work_item.WorkItemDetail), error.TrackerError) {
  parse_optional_detail_response(
    response,
    detail_by_id_graphql_decoder(),
    subtask_limit,
    label_limit,
  )
}

pub fn parse_detail_by_identifier_response(
  response: linear.Response,
  expected_scope: config_types.LinearTaskScope,
  expected_identifier: String,
  subtask_limit: Int,
  label_limit: Int,
) -> Result(Option(work_item.WorkItemDetail), error.TrackerError) {
  parse_optional_detail_response(
    response,
    detail_by_identifier_graphql_decoder(expected_scope, expected_identifier),
    subtask_limit,
    label_limit,
  )
}

fn parse_optional_detail_response(
  response: linear.Response,
  decoder: decode.Decoder(Result(Option(RawIssue), String)),
  subtask_limit: Int,
  label_limit: Int,
) -> Result(Option(work_item.WorkItemDetail), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, decoder) {
        Ok(Ok(found)) ->
          Ok(
            option_map(found, fn(item) {
              raw_issue_to_detail(item, subtask_limit, label_limit)
            }),
          )
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

fn page_graphql_decoder() -> decode.Decoder(
  Result(#(List(RawIssue), linear.PageInfo), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(
        ["data", "issues"],
        issue_connection_decoder(include_children: False),
      )
      |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn detail_by_id_graphql_decoder() -> decode.Decoder(
  Result(Option(RawIssue), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(
        ["data", "issues"],
        issue_connection_decoder(include_children: True),
      )
      |> decode.map(page_to_unique_issue)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn detail_by_identifier_graphql_decoder(
  expected_scope: config_types.LinearTaskScope,
  expected_identifier: String,
) -> decode.Decoder(Result(Option(RawIssue), String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] -> {
      use issue <- decode.then(decode.at(
        ["data", "issue"],
        decode.optional(issue_detail_decoder()),
      ))
      let found = case issue {
        None -> Ok(None)
        Some(#(raw_issue, Some(project_slug), identifier)) ->
          case
            linear_task_scope.matches_project_slug(expected_scope, project_slug),
            identifier_matches(expected_identifier, identifier)
          {
            True, True -> Ok(Some(raw_issue))
            _, _ -> Ok(None)
          }
        Some(#(_, None, _)) -> Ok(None)
      }
      decode.success(found)
    }
    errors ->
      case graphql_errors_indicate_missing_issue(errors) {
        True -> decode.success(Ok(None))
        False -> decode.success(Error(string.join(errors, with: "; ")))
      }
  }
}

fn issue_connection_decoder(
  include_children include_children: Bool,
) -> decode.Decoder(#(List(RawIssue), linear.PageInfo)) {
  use nodes <- decode.field(
    "nodes",
    decode.list(raw_issue_decoder(include_children: include_children)),
  )
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(#(nodes, page_info))
}

fn issue_detail_decoder() -> decode.Decoder(#(RawIssue, Option(String), String)) {
  use id <- decode.field("id", decode.string)
  use identifier <- decode.field("identifier", decode.string)
  use title <- decode.field("title", decode.string)
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  use created_at <- decode.optional_field(
    "createdAt",
    None,
    decode.optional(decode.string),
  )
  use updated_at <- decode.optional_field(
    "updatedAt",
    None,
    decode.optional(decode.string),
  )
  use state <- decode.field("state", state_decoder())
  use labels <- decode.optional_field(
    "labels",
    #([], False),
    label_connection_decoder(),
  )
  use children <- decode.optional_field(
    "children",
    #([], False),
    child_connection_decoder(),
  )
  use project_slug <- decode.optional_field(
    "project",
    None,
    decode.optional(project_slug_decoder()),
  )
  let #(bounded_labels, labels_has_more) = labels
  let #(child_nodes, children_has_more) = children
  let item =
    RawIssue(
      task: task.Task(
        ref: task.TaskRef(
          backend_kind: "linear",
          remote_id: id,
          key: Some(identifier),
          url: url,
        ),
        title: title,
        description: None,
        priority: None,
        state: state,
        branch_hint: None,
        labels: bounded_labels,
        blockers: [],
        blockers_complete: True,
        created_at: parse_optional_time(created_at),
        updated_at: parse_optional_time(updated_at),
      ),
      labels_has_more: labels_has_more,
      children: child_nodes,
      children_has_more: children_has_more,
    )
  decode.success(#(item, project_slug, identifier))
}

fn raw_issue_decoder(
  include_children include_children: Bool,
) -> decode.Decoder(RawIssue) {
  use id <- decode.field("id", decode.string)
  use identifier <- decode.field("identifier", decode.string)
  use title <- decode.field("title", decode.string)
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  use created_at <- decode.optional_field(
    "createdAt",
    None,
    decode.optional(decode.string),
  )
  use updated_at <- decode.optional_field(
    "updatedAt",
    None,
    decode.optional(decode.string),
  )
  use state <- decode.field("state", state_decoder())
  use labels <- decode.optional_field(
    "labels",
    #([], False),
    label_connection_decoder(),
  )
  let child_decoder = case include_children {
    True -> child_connection_decoder()
    False -> decode.success(#([], False))
  }
  use children <- decode.optional_field("children", #([], False), child_decoder)
  let #(bounded_labels, labels_has_more) = labels
  let #(child_nodes, children_has_more) = children
  decode.success(RawIssue(
    task: task.Task(
      ref: task.TaskRef(
        backend_kind: "linear",
        remote_id: id,
        key: Some(identifier),
        url: url,
      ),
      title: title,
      description: None,
      priority: None,
      state: state,
      branch_hint: None,
      labels: bounded_labels,
      blockers: [],
      blockers_complete: True,
      created_at: parse_optional_time(created_at),
      updated_at: parse_optional_time(updated_at),
    ),
    labels_has_more: labels_has_more,
    children: child_nodes,
    children_has_more: children_has_more,
  ))
}

fn child_connection_decoder() -> decode.Decoder(#(List(task.Task), Bool)) {
  use nodes <- decode.field(
    "nodes",
    decode.list(raw_issue_decoder(include_children: False)),
  )
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(#(
    list.map(nodes, fn(item) {
      let RawIssue(task: task_value, ..) = item
      task_value
    }),
    page_info.has_next_page,
  ))
}

fn label_connection_decoder() -> decode.Decoder(#(List(task.TaskLabel), Bool)) {
  use nodes <- decode.field("nodes", decode.list(label_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(#(nodes, page_info.has_next_page))
}

fn state_decoder() -> decode.Decoder(task.TaskState) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  use type_ <- decode.optional_field(
    "type",
    None,
    decode.optional(decode.string),
  )
  decode.success(task.TaskState(
    id: id,
    name: name,
    category: linear_state_type_to_category(type_),
  ))
}

fn label_decoder() -> decode.Decoder(task.TaskLabel) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  decode.success(task.TaskLabel(id: id, name: name))
}

fn page_info_decoder() -> decode.Decoder(linear.PageInfo) {
  use has_next_page <- decode.field("hasNextPage", decode.bool)
  use end_cursor <- decode.field("endCursor", decode.optional(decode.string))
  decode.success(linear.PageInfo(
    has_next_page: has_next_page,
    end_cursor: end_cursor,
  ))
}

fn error_message_decoder() -> decode.Decoder(String) {
  use message <- decode.field("message", decode.string)
  decode.success(message)
}

fn project_slug_decoder() -> decode.Decoder(String) {
  use slug <- decode.field("slugId", decode.string)
  decode.success(slug)
}

fn page_to_work_item_page(
  raw_page: #(List(RawIssue), linear.PageInfo),
  subtask_limit: Int,
  label_limit: Int,
) -> Result(Page, error.TrackerError) {
  let #(items, page_info) = raw_page
  let _ = subtask_limit
  Ok(Page(
    items: list.map(items, fn(item) { raw_issue_to_summary(item, label_limit) }),
    has_next_page: page_info.has_next_page,
    end_cursor: page_info.end_cursor,
  ))
}

fn raw_issue_to_summary(
  raw_issue: RawIssue,
  label_limit: Int,
) -> work_item.WorkItemSummary {
  let RawIssue(task: item, labels_has_more: labels_has_more, ..) = raw_issue
  let summary = work_item.summary_from_task(item, label_limit)
  work_item.WorkItemSummary(
    ..summary,
    labels_truncated: summary.labels_truncated || labels_has_more,
  )
}

fn raw_issue_to_detail(
  raw_issue: RawIssue,
  subtask_limit: Int,
  label_limit: Int,
) -> work_item.WorkItemDetail {
  let RawIssue(
    task: item,
    labels_has_more: labels_has_more,
    children: children,
    children_has_more: children_has_more,
  ) = raw_issue
  let detail =
    work_item.detail_from_task_and_subtasks(
      item,
      children,
      label_limit,
      subtask_limit,
    )
  let summary =
    work_item.WorkItemSummary(
      ..detail.summary,
      labels_truncated: detail.summary.labels_truncated || labels_has_more,
    )
  work_item.WorkItemDetail(
    summary: summary,
    subtasks: detail.subtasks,
    subtasks_truncated: detail.subtasks_truncated || children_has_more,
  )
}

fn recategorize_page(page: Page, config: config_types.TrackerConfig) -> Page {
  Page(
    ..page,
    items: list.map(page.items, fn(item) { recategorize_summary(item, config) }),
  )
}

fn recategorize_detail(
  detail: work_item.WorkItemDetail,
  config: config_types.TrackerConfig,
) -> work_item.WorkItemDetail {
  work_item.WorkItemDetail(
    ..detail,
    summary: recategorize_summary(detail.summary, config),
    subtasks: list.map(detail.subtasks, fn(item) {
      recategorize_summary(item, config)
    }),
  )
}

fn recategorize_summary(
  summary: work_item.WorkItemSummary,
  config: config_types.TrackerConfig,
) -> work_item.WorkItemSummary {
  let state = summary.state
  work_item.WorkItemSummary(
    ..summary,
    state: task.TaskState(
      id: state.id,
      name: state.name,
      category: state_category(config, state),
    ),
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

fn page_to_unique_issue(
  raw_page: #(List(RawIssue), linear.PageInfo),
) -> Result(Option(RawIssue), String) {
  let #(items, _) = raw_page
  case items {
    [] -> Ok(None)
    [item] -> Ok(Some(item))
    [_, ..] -> Error("task identifier is not unique")
  }
}

fn graphql_errors_indicate_missing_issue(errors: List(String)) -> Bool {
  list.any(errors, fn(message) {
    let message = string.lowercase(message)
    string.contains(message, "not found")
    || string.contains(message, "could not find")
  })
}

fn identifier_matches(expected: String, returned: String) -> Bool {
  let expected = string.trim(expected) |> string.lowercase
  let returned = string.trim(returned) |> string.lowercase
  expected != "" && expected == returned
}

fn linear_state_type_to_category(
  type_: Option(String),
) -> task.TaskStateCategory {
  case type_ {
    Some("backlog") -> task.Backlog
    Some("unstarted") -> task.Ready
    Some("started") -> task.Active
    Some("completed") -> task.Done
    Some("canceled") | Some("cancelled") -> task.Canceled
    _ -> task.Unknown
  }
}

fn parse_optional_time(value: Option(String)) -> Option(birl.Time) {
  case value {
    Some(value) ->
      case birl.parse(value) {
        Ok(time) -> Some(time)
        Error(_) -> None
      }
    None -> None
  }
}

fn option_map(value: Option(a), mapper: fn(a) -> b) -> Option(b) {
  case value {
    Some(value) -> Some(mapper(value))
    None -> None
  }
}

fn graphql_request(
  endpoint: String,
  api_key: String,
  body: String,
) -> linear.Request {
  linear.Request(
    endpoint: endpoint,
    headers: [
      #("Authorization", api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
    timeout_ms: 30_000,
  )
}

fn require_task_scope(
  config: config_types.TrackerConfig,
) -> Result(config_types.LinearTaskScope, error.TrackerError) {
  case config_types.linear_task_scope_from_tracker_config(config) {
    Ok(scope) -> Ok(scope)
    Error(scope_error) ->
      Error(
        error.LinearApiRequest(config_types.linear_task_scope_error_message(
          scope_error,
        )),
      )
  }
}

fn require_https_endpoint(
  endpoint: String,
) -> Result(String, error.TrackerError) {
  let endpoint = string.trim(endpoint)
  case string.starts_with(string.lowercase(endpoint), "https://") {
    True -> Ok(endpoint)
    False -> Error(error.LinearApiRequest("tracker endpoint must use https://"))
  }
}

fn require_api_key(
  config: config_types.TrackerConfig,
) -> Result(String, error.TrackerError) {
  case config.api_key {
    Some(value) -> Ok(value)
    None -> Error(error.LinearApiRequest("missing api key"))
  }
}

fn try_tracker(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(error)
  }
}
