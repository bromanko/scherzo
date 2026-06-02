import birl
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/task
import scherzo/tracker/state as issue_state

pub type Page {
  Page(nodes: List(task.Task), has_next_page: Bool, end_cursor: Option(String))
}

pub fn fetch_page(
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  after: Option(String),
  transport: linear.Transport,
) -> Result(Page, error.TrackerError) {
  use request <- try_tracker(build_list_request(config, states, after))
  use response <- try_tracker(transport(request))
  parse_page_response(response)
}

pub fn fetch_detail_by_id(
  config: config_types.TrackerConfig,
  id: String,
  transport: linear.Transport,
) -> Result(Option(task.Task), error.TrackerError) {
  use request <- try_tracker(build_detail_by_id_request(config, id))
  use response <- try_tracker(transport(request))
  parse_detail_by_id_response(response)
}

pub fn fetch_detail_by_identifier(
  config: config_types.TrackerConfig,
  identifier: String,
  transport: linear.Transport,
) -> Result(Option(task.Task), error.TrackerError) {
  use request <- try_tracker(build_detail_by_identifier_request(
    config,
    identifier,
  ))
  use response <- try_tracker(transport(request))
  parse_detail_by_identifier_response(response)
}

pub fn build_list_request(
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  after: Option(String),
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use project_slug <- try_tracker(require_project_slug(config))
  let variables = case states {
    [] ->
      json.object([
        #("projectSlug", json.string(project_slug)),
        #("after", json.nullable(after, of: json.string)),
      ])
    _ ->
      json.object([
        #("projectSlug", json.string(project_slug)),
        #(
          "stateNames",
          json.array(issue_state.to_strings(states), of: json.string),
        ),
        #("after", json.nullable(after, of: json.string)),
      ])
  }
  let body =
    json.object([
      #("query", json.string(list_query(states))),
      #("variables", variables),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_detail_by_id_request(
  config: config_types.TrackerConfig,
  id: String,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use project_slug <- try_tracker(require_project_slug(config))
  let body =
    json.object([
      #("query", json.string(detail_by_id_query())),
      #(
        "variables",
        json.object([
          #("projectSlug", json.string(project_slug)),
          #("ids", json.array([id], of: json.string)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_detail_by_identifier_request(
  config: config_types.TrackerConfig,
  identifier: String,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use project_slug <- try_tracker(require_project_slug(config))
  let body =
    json.object([
      #("query", json.string(detail_by_identifier_query())),
      #(
        "variables",
        json.object([
          #("projectSlug", json.string(project_slug)),
          #("identifier", json.string(identifier)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn list_query(states: List(issue_state.IssueState)) -> String {
  case states {
    [] ->
      "query ScherzoTaskList($projectSlug: String!, $after: String) { issues(first: 50, after: $after, filter: { project: { slugId: { eq: $projectSlug } } }) { nodes { id identifier title priority branchName url createdAt updatedAt state { id name type } labels { nodes { id name } } } pageInfo { hasNextPage endCursor } } }"
    _ ->
      "query ScherzoTaskList($projectSlug: String!, $stateNames: [String!], $after: String) { issues(first: 50, after: $after, filter: { project: { slugId: { eq: $projectSlug } }, state: { name: { in: $stateNames } } }) { nodes { id identifier title priority branchName url createdAt updatedAt state { id name type } labels { nodes { id name } } } pageInfo { hasNextPage endCursor } } }"
  }
}

pub fn detail_by_id_query() -> String {
  "query ScherzoTaskDetailById($projectSlug: String!, $ids: [ID!]!) { issues(first: 2, filter: { project: { slugId: { eq: $projectSlug } }, id: { in: $ids } }) { nodes { id identifier title description priority branchName url createdAt updatedAt state { id name type } labels { nodes { id name } } } pageInfo { hasNextPage endCursor } } }"
}

pub fn detail_by_identifier_query() -> String {
  "query ScherzoTaskDetailByIdentifier($projectSlug: String!, $identifier: String!) { issues(first: 2, filter: { project: { slugId: { eq: $projectSlug } }, identifier: { eq: $identifier } }) { nodes { id identifier title description priority branchName url createdAt updatedAt state { id name type } labels { nodes { id name } } } pageInfo { hasNextPage endCursor } } }"
}

pub fn parse_page_response(
  response: linear.Response,
) -> Result(Page, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, page_graphql_decoder()) {
        Ok(Ok(page)) -> Ok(page)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_detail_by_id_response(
  response: linear.Response,
) -> Result(Option(task.Task), error.TrackerError) {
  parse_optional_task_response(response, detail_by_id_graphql_decoder())
}

pub fn parse_detail_by_identifier_response(
  response: linear.Response,
) -> Result(Option(task.Task), error.TrackerError) {
  parse_optional_task_response(response, detail_by_identifier_graphql_decoder())
}

fn parse_optional_task_response(
  response: linear.Response,
  decoder: decode.Decoder(Result(Option(task.Task), String)),
) -> Result(Option(task.Task), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, decoder) {
        Ok(Ok(task)) -> Ok(task)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

fn page_graphql_decoder() -> decode.Decoder(Result(Page, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] -> {
      use page <- decode.field("data", page_data_decoder())
      decode.success(Ok(page))
    }
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn page_data_decoder() -> decode.Decoder(Page) {
  use page <- decode.field("issues", connection_decoder())
  decode.success(page)
}

fn detail_by_id_graphql_decoder() -> decode.Decoder(
  Result(Option(task.Task), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", "issues"], connection_decoder())
      |> decode.map(page_to_unique_task)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn detail_by_identifier_graphql_decoder() -> decode.Decoder(
  Result(Option(task.Task), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", "issues"], connection_decoder())
      |> decode.map(page_to_unique_task)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn page_to_unique_task(page: Page) -> Result(Option(task.Task), String) {
  case page.nodes {
    [] -> Ok(None)
    [item] -> Ok(Some(item))
    [_, ..] -> Error("task identifier is not unique")
  }
}

fn connection_decoder() -> decode.Decoder(Page) {
  use nodes <- decode.field("nodes", decode.list(task_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(Page(
    nodes: nodes,
    has_next_page: page_info.has_next_page,
    end_cursor: page_info.end_cursor,
  ))
}

fn task_decoder() -> decode.Decoder(task.Task) {
  use id <- decode.field("id", decode.string)
  use identifier <- decode.field("identifier", decode.string)
  use title <- decode.field("title", decode.string)
  use description <- decode.optional_field(
    "description",
    None,
    decode.optional(decode.string),
  )
  use priority <- decode.optional_field(
    "priority",
    None,
    decode.optional(decode.int),
  )
  use branch_name <- decode.optional_field(
    "branchName",
    None,
    decode.optional(decode.string),
  )
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
  use labels <- decode.optional_field("labels", [], labels_decoder())
  decode.success(task.Task(
    ref: task.TaskRef(
      backend_kind: "linear",
      remote_id: id,
      key: Some(identifier),
      url: url,
    ),
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_hint: branch_name,
    labels: labels,
    blockers: [],
    blockers_complete: True,
    created_at: parse_optional_time(created_at),
    updated_at: parse_optional_time(updated_at),
  ))
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

fn labels_decoder() -> decode.Decoder(List(task.TaskLabel)) {
  use nodes <- decode.field("nodes", decode.list(label_decoder()))
  decode.success(nodes)
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
        Error(parse_error) -> invalid_optional_time(parse_error)
      }
    None -> None
  }
}

fn invalid_optional_time(_parse_error: Nil) -> Option(birl.Time) {
  None
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

fn require_project_slug(
  config: config_types.TrackerConfig,
) -> Result(String, error.TrackerError) {
  case config.project_slug {
    Some(value) -> Ok(value)
    None -> Error(error.LinearApiRequest("missing project slug"))
  }
}

fn try_tracker(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
