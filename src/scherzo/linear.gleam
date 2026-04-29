import birl
import gleam/dynamic/decode
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/error
import scherzo/tracker

pub type Request {
  Request(
    endpoint: String,
    headers: List(#(String, String)),
    body: String,
    timeout_ms: Int,
  )
}

pub type Response {
  Response(status: Int, body: String)
}

pub type LinearCommentAuthor {
  LinearCommentAuthor(id: String, email: Option(String), name: Option(String))
}

pub type LinearComment {
  LinearComment(
    id: String,
    issue_id: String,
    body: String,
    created_at_ms: Int,
    updated_at_ms: Int,
    author: LinearCommentAuthor,
  )
}

pub type CommandClient {
  CommandClient(
    fetch_comments: fn(List(String), Int) ->
      Result(List(LinearComment), error.TrackerError),
    post_ack: fn(String, String) -> Result(Nil, error.TrackerError),
  )
}

pub type Transport =
  fn(Request) -> Result(Response, error.TrackerError)

pub fn client(
  config: domain.TrackerConfig,
  transport: Transport,
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { fetch_candidate_issues(config, transport) },
    fetch_issues_by_states: fn(states) {
      fetch_issues_by_states(config, states, transport)
    },
    fetch_issue_states_by_ids: fn(ids) {
      fetch_issue_states_by_ids(config, ids, transport)
    },
  )
}

pub fn real_client(config: domain.TrackerConfig) -> tracker.Client {
  client(config, http_transport)
}

pub fn command_client(
  config: domain.TrackerConfig,
  transport: Transport,
) -> CommandClient {
  CommandClient(
    fetch_comments: fn(issue_ids, limit_per_issue) {
      fetch_issue_comments(config, issue_ids, limit_per_issue, transport)
    },
    post_ack: fn(issue_id, body) { post_ack(config, issue_id, body, transport) },
  )
}

pub fn real_command_client(config: domain.TrackerConfig) -> CommandClient {
  command_client(config, http_transport)
}

pub fn http_transport(request: Request) -> Result(Response, error.TrackerError) {
  use http_req <- try_tracker(
    http_request.to(request.endpoint)
    |> result_map_error(fn(_) { error.LinearApiRequest("invalid endpoint") }),
  )
  let http_req =
    http_req
    |> http_request.set_method(http.Post)
    |> http_request.set_body(request.body)
    |> set_headers(request.headers)
  case
    httpc.configure()
    |> httpc.timeout(request.timeout_ms)
    |> httpc.dispatch(http_req)
  {
    Ok(response) -> Ok(Response(status: response.status, body: response.body))
    Error(err) -> Error(error.LinearApiRequest(http_error_to_string(err)))
  }
}

pub fn http_error_to_string(err: httpc.HttpError) -> String {
  case err {
    httpc.InvalidUtf8Response -> "invalid utf8 response"
    httpc.ResponseTimeout -> "response timeout"
    httpc.FailedToConnect(_, _) -> "failed to connect"
  }
}

fn set_headers(
  request: http_request.Request(String),
  headers: List(#(String, String)),
) -> http_request.Request(String) {
  case headers {
    [] -> request
    [#(key, value), ..rest] ->
      set_headers(http_request.set_header(request, key, value), rest)
  }
}

pub type Page {
  Page(
    nodes: List(domain.Issue),
    has_next_page: Bool,
    end_cursor: Option(String),
  )
}

pub fn fetch_candidate_issues(
  config: domain.TrackerConfig,
  transport: Transport,
) -> Result(List(domain.Issue), error.TrackerError) {
  fetch_pages(config, config.active_states, None, transport, [])
}

pub fn fetch_issues_by_states(
  config: domain.TrackerConfig,
  states: List(String),
  transport: Transport,
) -> Result(List(domain.Issue), error.TrackerError) {
  case states {
    [] -> Ok([])
    _ -> fetch_pages(config, states, None, transport, [])
  }
}

pub fn fetch_issue_states_by_ids(
  config: domain.TrackerConfig,
  ids: List(String),
  transport: Transport,
) -> Result(List(domain.Issue), error.TrackerError) {
  case ids {
    [] -> Ok([])
    _ -> {
      use request <- try_tracker(build_state_refresh_request(config, ids))
      use response <- try_tracker(transport(request))
      parse_response(response)
    }
  }
}

pub fn fetch_issue_comments(
  config: domain.TrackerConfig,
  issue_ids: List(String),
  limit_per_issue: Int,
  transport: Transport,
) -> Result(List(LinearComment), error.TrackerError) {
  case issue_ids {
    [] -> Ok([])
    _ -> {
      use request <- try_tracker(build_issue_comments_request(
        config,
        issue_ids,
        limit_per_issue,
      ))
      use response <- try_tracker(transport(request))
      use comments <- try_tracker(parse_comments_response(response))
      Ok(list.sort(comments, by: compare_comments))
    }
  }
}

pub fn post_ack(
  config: domain.TrackerConfig,
  issue_id: String,
  body: String,
  transport: Transport,
) -> Result(Nil, error.TrackerError) {
  use request <- try_tracker(build_comment_create_request(
    config,
    issue_id,
    body,
  ))
  use response <- try_tracker(transport(request))
  parse_mutation_response(response, "commentCreate")
}

fn compare_comments(a: LinearComment, b: LinearComment) {
  int.compare(a.created_at_ms, b.created_at_ms)
}

fn fetch_pages(
  config: domain.TrackerConfig,
  states: List(String),
  after: Option(String),
  transport: Transport,
  acc: List(domain.Issue),
) -> Result(List(domain.Issue), error.TrackerError) {
  use request <- try_tracker(build_candidate_request(config, states, after))
  use response <- try_tracker(transport(request))
  use page <- try_tracker(parse_page_response(response))
  let acc = list.append(acc, page.nodes)
  case page.has_next_page {
    False -> Ok(acc)
    True ->
      case page.end_cursor {
        Some(cursor) ->
          fetch_pages(config, states, Some(cursor), transport, acc)
        None -> Error(error.LinearMissingEndCursor)
      }
  }
}

pub fn build_candidate_request(
  config: domain.TrackerConfig,
  states: List(String),
  after: Option(String),
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use project_slug <- try_tracker(require_project_slug(config))
  let body =
    json.object([
      #("query", json.string(candidate_query())),
      #(
        "variables",
        json.object([
          #("projectSlug", json.string(project_slug)),
          #("activeStates", json.array(states, of: json.string)),
          #("after", json.nullable(after, of: json.string)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_state_refresh_request(
  config: domain.TrackerConfig,
  ids: List(String),
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(state_refresh_query())),
      #("variables", json.object([#("ids", json.array(ids, of: json.string))])),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_issue_comments_request(
  config: domain.TrackerConfig,
  issue_ids: List(String),
  limit_per_issue: Int,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  case limit_per_issue <= 0 {
    True ->
      Error(error.LinearApiRequest("comment query limit must be positive"))
    False -> {
      let body =
        json.object([
          #("query", json.string(issue_comments_query())),
          #(
            "variables",
            json.object([
              #("issueIds", json.array(issue_ids, of: json.string)),
              #("first", json.int(limit_per_issue)),
            ]),
          ),
        ])
        |> json.to_string
      Ok(graphql_request(endpoint, api_key, body))
    }
  }
}

pub fn build_comment_create_request(
  config: domain.TrackerConfig,
  issue_id: String,
  body: String,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(comment_create_mutation())),
      #(
        "variables",
        json.object([
          #("issueId", json.string(issue_id)),
          #("body", json.string(body)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_issue_update_state_request(
  config: domain.TrackerConfig,
  issue_id: String,
  state_id: String,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(issue_update_state_mutation())),
      #(
        "variables",
        json.object([
          #("issueId", json.string(issue_id)),
          #("stateId", json.string(state_id)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

fn graphql_request(endpoint: String, api_key: String, body: String) -> Request {
  Request(
    endpoint: endpoint,
    headers: [
      #("Authorization", api_key),
      #("Content-Type", "application/json"),
    ],
    body: body,
    timeout_ms: 30_000,
  )
}

pub fn candidate_query() -> String {
  "query CandidateIssues($projectSlug: String!, $activeStates: [String!], $after: String) { issues(first: 50, after: $after, filter: { project: { slugId: { eq: $projectSlug } }, state: { name: { in: $activeStates } } }) { nodes { id identifier title description priority branchName url createdAt updatedAt state { name } labels { nodes { name } } relations { nodes { type relatedIssue { id identifier state { name } } } } } pageInfo { hasNextPage endCursor } } }"
}

pub fn state_refresh_query() -> String {
  "query IssueStates($ids: [ID!]!) { issues(filter: { id: { in: $ids } }) { nodes { id identifier title description priority branchName url createdAt updatedAt state { name } labels { nodes { name } } relations { nodes { type relatedIssue { id identifier state { name } } } } } pageInfo { hasNextPage endCursor } } }"
}

pub fn issue_comments_query() -> String {
  "query IssueComments($issueIds: [ID!]!, $first: Int!) { issues(filter: { id: { in: $issueIds } }) { nodes { id comments(first: $first) { nodes { id body createdAt updatedAt user { id email name } } } } } }"
}

pub fn comment_create_mutation() -> String {
  "mutation ScherzoCommentCreate($issueId: String!, $body: String!) { commentCreate(input: { issueId: $issueId, body: $body }) { success } }"
}

pub fn issue_update_state_mutation() -> String {
  "mutation ScherzoIssueUpdateState($issueId: String!, $stateId: String!) { issueUpdate(id: $issueId, input: { stateId: $stateId }) { success } }"
}

pub fn parse_response(
  response: Response,
) -> Result(List(domain.Issue), error.TrackerError) {
  use page <- try_tracker(parse_page_response(response))
  Ok(page.nodes)
}

pub fn parse_page_response(
  response: Response,
) -> Result(Page, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, graphql_decoder()) {
        Ok(Ok(page)) -> Ok(page)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_comments_response(
  response: Response,
) -> Result(List(LinearComment), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, comments_graphql_decoder()) {
        Ok(Ok(raw_issues)) -> raw_issue_comments_to_comments(raw_issues)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_mutation_response(
  response: Response,
  root_field: String,
) -> Result(Nil, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, mutation_decoder(root_field)) {
        Ok(Ok(True)) -> Ok(Nil)
        Ok(Ok(False)) ->
          Error(error.LinearUnknownPayload(root_field <> " returned false"))
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

fn graphql_decoder() -> decode.Decoder(Result(Page, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] -> {
      use page <- decode.field("data", data_decoder())
      decode.success(Ok(page))
    }
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn data_decoder() -> decode.Decoder(Page) {
  use page <- decode.field("issues", page_decoder())
  decode.success(page)
}

fn mutation_decoder(root_field: String) -> decode.Decoder(Result(Bool, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", root_field, "success"], decode.bool) |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

type RawIssueComments {
  RawIssueComments(issue_id: String, comments: List(RawLinearComment))
}

type RawLinearComment {
  RawLinearComment(
    id: String,
    body: String,
    created_at: String,
    updated_at: String,
    author: LinearCommentAuthor,
  )
}

fn comments_graphql_decoder() -> decode.Decoder(
  Result(List(RawIssueComments), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] -> {
      use issues <- decode.field("data", comments_data_decoder())
      decode.success(Ok(issues))
    }
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn comments_data_decoder() -> decode.Decoder(List(RawIssueComments)) {
  decode.at(["issues", "nodes"], decode.list(raw_issue_comments_decoder()))
}

fn raw_issue_comments_decoder() -> decode.Decoder(RawIssueComments) {
  use issue_id <- decode.field("id", decode.string)
  use comments <- decode.field("comments", raw_comment_page_decoder())
  decode.success(RawIssueComments(issue_id: issue_id, comments: comments))
}

fn raw_comment_page_decoder() -> decode.Decoder(List(RawLinearComment)) {
  use nodes <- decode.field("nodes", decode.list(raw_comment_decoder()))
  decode.success(nodes)
}

fn raw_comment_decoder() -> decode.Decoder(RawLinearComment) {
  use id <- decode.field("id", decode.string)
  use body <- decode.field("body", decode.string)
  use created_at <- decode.field("createdAt", decode.string)
  use updated_at <- decode.field("updatedAt", decode.string)
  use author <- decode.field("user", comment_author_decoder())
  decode.success(RawLinearComment(
    id: id,
    body: body,
    created_at: created_at,
    updated_at: updated_at,
    author: author,
  ))
}

fn comment_author_decoder() -> decode.Decoder(LinearCommentAuthor) {
  use id <- decode.field("id", decode.string)
  use email <- decode.optional_field(
    "email",
    None,
    decode.optional(decode.string),
  )
  use name <- decode.optional_field(
    "name",
    None,
    decode.optional(decode.string),
  )
  decode.success(LinearCommentAuthor(id: id, email: email, name: name))
}

fn raw_issue_comments_to_comments(
  raw_issues: List(RawIssueComments),
) -> Result(List(LinearComment), error.TrackerError) {
  raw_issues_to_comments(raw_issues, [])
}

fn raw_issues_to_comments(
  raw_issues: List(RawIssueComments),
  acc: List(LinearComment),
) -> Result(List(LinearComment), error.TrackerError) {
  case raw_issues {
    [] -> Ok(list.reverse(acc))
    [RawIssueComments(issue_id, comments), ..rest] ->
      case raw_comments_to_comments(issue_id, comments, acc) {
        Ok(acc) -> raw_issues_to_comments(rest, acc)
        Error(err) -> Error(err)
      }
  }
}

fn raw_comments_to_comments(
  issue_id: String,
  comments: List(RawLinearComment),
  acc: List(LinearComment),
) -> Result(List(LinearComment), error.TrackerError) {
  case comments {
    [] -> Ok(acc)
    [comment, ..rest] ->
      case raw_comment_to_comment(issue_id, comment) {
        Ok(comment) ->
          raw_comments_to_comments(issue_id, rest, [comment, ..acc])
        Error(err) -> Error(err)
      }
  }
}

fn raw_comment_to_comment(
  issue_id: String,
  raw: RawLinearComment,
) -> Result(LinearComment, error.TrackerError) {
  use created_at_ms <- try_tracker(parse_time_ms(raw.created_at, "createdAt"))
  use updated_at_ms <- try_tracker(parse_time_ms(raw.updated_at, "updatedAt"))
  Ok(LinearComment(
    id: raw.id,
    issue_id: issue_id,
    body: raw.body,
    created_at_ms: created_at_ms,
    updated_at_ms: updated_at_ms,
    author: raw.author,
  ))
}

fn parse_time_ms(
  value: String,
  field: String,
) -> Result(Int, error.TrackerError) {
  case birl.parse(value) {
    Ok(time) -> Ok(birl.to_unix_milli(time))
    Error(_) -> Error(error.LinearUnknownPayload("invalid comment " <> field))
  }
}

fn error_message_decoder() -> decode.Decoder(String) {
  use message <- decode.field("message", decode.string)
  decode.success(message)
}

pub type PageInfo {
  PageInfo(has_next_page: Bool, end_cursor: Option(String))
}

fn page_decoder() -> decode.Decoder(Page) {
  use nodes <- decode.field("nodes", decode.list(issue_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(Page(
    nodes: nodes,
    has_next_page: page_info.has_next_page,
    end_cursor: page_info.end_cursor,
  ))
}

fn page_info_decoder() -> decode.Decoder(PageInfo) {
  use has_next_page <- decode.field("hasNextPage", decode.bool)
  use end_cursor <- decode.field("endCursor", decode.optional(decode.string))
  decode.success(PageInfo(has_next_page: has_next_page, end_cursor: end_cursor))
}

fn issue_decoder() -> decode.Decoder(domain.Issue) {
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
  use state <- decode.field("state", state_name_decoder())
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
  use labels <- decode.optional_field("labels", [], labels_decoder())
  use blockers <- decode.optional_field("relations", [], blockers_decoder())
  decode.success(domain.Issue(
    id: id,
    identifier: identifier,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_name: branch_name,
    url: url,
    labels: list.map(labels, string.lowercase),
    blocked_by: blockers,
    created_at: parse_optional_time(created_at),
    updated_at: parse_optional_time(updated_at),
  ))
}

fn state_name_decoder() -> decode.Decoder(String) {
  use name <- decode.field("name", decode.string)
  decode.success(name)
}

fn labels_decoder() -> decode.Decoder(List(String)) {
  use nodes <- decode.field("nodes", decode.list(label_decoder()))
  decode.success(nodes)
}

fn label_decoder() -> decode.Decoder(String) {
  use name <- decode.field("name", decode.string)
  decode.success(name)
}

fn blockers_decoder() -> decode.Decoder(List(domain.BlockerRef)) {
  use nodes <- decode.field("nodes", decode.list(relation_decoder()))
  decode.success(
    list.filter_map(nodes, fn(rel) {
      case rel {
        Relation("blocks", blocker) -> Ok(blocker)
        _ -> Error(Nil)
      }
    }),
  )
}

pub type Relation {
  Relation(type_: String, blocker: domain.BlockerRef)
}

pub type RelatedIssue {
  RelatedIssue(
    id: Option(String),
    identifier: Option(String),
    state: Option(String),
  )
}

fn relation_decoder() -> decode.Decoder(Relation) {
  use type_ <- decode.field("type", decode.string)
  use related <- decode.field("relatedIssue", related_issue_decoder())
  decode.success(Relation(
    type_: type_,
    blocker: domain.BlockerRef(
      id: related.id,
      identifier: related.identifier,
      state: related.state,
    ),
  ))
}

fn related_issue_decoder() -> decode.Decoder(RelatedIssue) {
  use id <- decode.field("id", decode.optional(decode.string))
  use identifier <- decode.field("identifier", decode.optional(decode.string))
  use state <- decode.field("state", optional_state_name_decoder())
  decode.success(RelatedIssue(id: id, identifier: identifier, state: state))
}

fn optional_state_name_decoder() -> decode.Decoder(Option(String)) {
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(name)
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
  config: domain.TrackerConfig,
) -> Result(String, error.TrackerError) {
  case config.api_key {
    Some(value) -> Ok(value)
    None -> Error(error.LinearApiRequest("missing api key"))
  }
}

fn require_project_slug(
  config: domain.TrackerConfig,
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

fn result_map_error(result: Result(a, e), mapper: fn(e) -> f) -> Result(a, f) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(mapper(err))
  }
}
