import birl
import gleam/dynamic/decode
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear_body_data
import scherzo/linear_contract
import scherzo/tracker
import scherzo/tracker/idempotency
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

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

pub type LinearCommentDocument {
  LinearCommentDocument(
    id: String,
    body: String,
    body_data: linear_body_data.JsonValue,
  )
}

pub type UploadHeader {
  UploadHeader(key: String, value: String)
}

pub type UploadFile {
  UploadFile(
    filename: String,
    content_type: String,
    size: Int,
    upload_url: String,
    asset_url: String,
    headers: List(UploadHeader),
  )
}

pub type ContractClient {
  ContractClient(
    fetch_remote_contract: fn() ->
      Result(linear_contract.RemoteBoard, error.TrackerError),
  )
}

pub type StateNameResolutionError {
  StateNameNotFound
  StateNameAmbiguous
}

pub type Transport =
  fn(Request) -> Result(Response, error.TrackerError)

pub fn client(
  config: config_types.TrackerConfig,
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

pub fn real_client(config: config_types.TrackerConfig) -> tracker.Client {
  client(config, http_transport)
}

pub fn contract_client(
  config: config_types.TrackerConfig,
  transport: Transport,
) -> ContractClient {
  ContractClient(fetch_remote_contract: fn() {
    fetch_remote_contract(config, transport)
  })
}

pub fn real_contract_client(
  config: config_types.TrackerConfig,
) -> ContractClient {
  contract_client(config, http_transport)
}

pub fn http_transport(
  request: Request,
) -> Result(Response, error.TrackerError) {
  use http_req <- try_tracker(
    http_request.to(request.endpoint)
    |> result.replace_error(error.LinearApiRequest("invalid endpoint")),
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
    nodes: List(tracker_issue.Issue),
    has_next_page: Bool,
    end_cursor: Option(String),
  )
}

pub fn fetch_candidate_issues(
  config: config_types.TrackerConfig,
  transport: Transport,
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  fetch_pages(config, config.dispatch_states, None, transport, [])
}

pub fn fetch_issues_by_states(
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  transport: Transport,
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  case states {
    [] -> Ok([])
    _ -> fetch_pages(config, states, None, transport, [])
  }
}

pub fn fetch_issue_states_by_ids(
  config: config_types.TrackerConfig,
  ids: List(String),
  transport: Transport,
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  case ids {
    [] -> Ok([])
    _ -> {
      use request <- try_tracker(build_state_refresh_request(config, ids))
      use response <- try_tracker(transport(request))
      parse_response(response)
    }
  }
}

pub fn fetch_remote_contract(
  config: config_types.TrackerConfig,
  transport: Transport,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  use scope <- try_tracker(require_task_scope(config))
  use request <- try_tracker(build_contract_request(config))
  use response <- try_tracker(transport(request))
  parse_contract_response_for_scope(response, scope)
}

fn fetch_pages(
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  after: Option(String),
  transport: Transport,
  acc: List(tracker_issue.Issue),
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
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
  config: config_types.TrackerConfig,
  states: List(issue_state.IssueState),
  after: Option(String),
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use scope <- try_tracker(require_task_scope(config))
  let variables =
    config_types.linear_task_scope_graphql_variables(
      scope,
      "projectSlug",
      "projectSlugs",
    )
    |> list.append([
      #(
        "dispatchStates",
        json.array(issue_state.to_strings(states), of: json.string),
      ),
      #("after", json.nullable(after, of: json.string)),
    ])
  let body =
    json.object([
      #("query", json.string(candidate_query_for_scope(scope))),
      #("variables", json.object(variables)),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_state_refresh_request(
  config: config_types.TrackerConfig,
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

pub fn build_contract_request(
  config: config_types.TrackerConfig,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use scope <- try_tracker(require_task_scope(config))
  let body =
    json.object([
      #("query", json.string(contract_query_for_scope(scope))),
      #(
        "variables",
        json.object(config_types.linear_task_scope_graphql_variables(
          scope,
          "projectSlug",
          "projectSlugs",
        )),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_comment_create_request(
  config: config_types.TrackerConfig,
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

pub fn build_comment_fetch_request(
  config: config_types.TrackerConfig,
  comment_id: String,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(comment_fetch_query())),
      #("variables", json.object([#("commentId", json.string(comment_id))])),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_issue_comments_request(
  config: config_types.TrackerConfig,
  issue_id: String,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(issue_comments_query())),
      #("variables", json.object([#("issueId", json.string(issue_id))])),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn find_issue_comment_by_marker(
  config: config_types.TrackerConfig,
  transport: Transport,
  issue_id: String,
  marker_key: String,
) -> Result(Option(LinearCommentDocument), error.TrackerError) {
  use request <- try_tracker(build_issue_comments_request(config, issue_id))
  use response <- try_tracker(transport(request))
  use comments <- try_tracker(parse_issue_comments_response(response))
  Ok(
    list.find(comments, fn(comment) {
      idempotency.contains_marker(comment.body, marker_key)
    })
    |> option.from_result,
  )
}

pub fn build_file_upload_request(
  config: config_types.TrackerConfig,
  filename: String,
  content_type: String,
  size: Int,
  meta_data: json.Json,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(file_upload_mutation())),
      #(
        "variables",
        json.object([
          #("filename", json.string(filename)),
          #("contentType", json.string(content_type)),
          #("size", json.int(size)),
          #("metaData", meta_data),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_comment_update_body_data_request(
  config: config_types.TrackerConfig,
  comment_id: String,
  body_data: json.Json,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(comment_update_body_data_mutation())),
      #(
        "variables",
        json.object([
          #("commentId", json.string(comment_id)),
          #("bodyData", body_data),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_comment_update_body_request(
  config: config_types.TrackerConfig,
  comment_id: String,
  body: String,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(comment_update_body_mutation())),
      #(
        "variables",
        json.object([
          #("commentId", json.string(comment_id)),
          #("body", json.string(body)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_issue_team_states_request(
  config: config_types.TrackerConfig,
  issue_id: String,
) -> Result(Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(issue_team_states_query())),
      #("variables", json.object([#("issueId", json.string(issue_id))])),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

pub fn build_issue_update_state_request(
  config: config_types.TrackerConfig,
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
  candidate_query_for_scope(config_types.LinearTaskProject("projectSlug"))
}

fn candidate_query_for_scope(scope: config_types.LinearTaskScope) -> String {
  let project_declaration =
    config_types.linear_task_scope_variable_declaration(
      scope,
      "projectSlug",
      "projectSlugs",
    )
  let project_filter =
    config_types.linear_task_scope_project_filter(
      scope,
      "$projectSlug",
      "$projectSlugs",
    )
  "query CandidateIssues("
  <> project_declaration
  <> ", $dispatchStates: [String!], $after: String) { issues(first: 50, after: $after, filter: { "
  <> project_filter
  <> ", state: { name: { in: $dispatchStates } } }) { nodes { id identifier title description priority branchName url createdAt updatedAt state { name } labels { nodes { name } } inverseRelations(first: 100) { nodes { type issue { id identifier state { name } } } pageInfo { hasNextPage endCursor } } } pageInfo { hasNextPage endCursor } } }"
}

pub fn state_refresh_query() -> String {
  "query IssueStates($ids: [ID!]!) { issues(filter: { id: { in: $ids } }) { nodes { id identifier title description priority branchName url createdAt updatedAt state { name } labels { nodes { name } } inverseRelations(first: 100) { nodes { type issue { id identifier state { name } } } pageInfo { hasNextPage endCursor } } } pageInfo { hasNextPage endCursor } } }"
}

pub fn comment_create_mutation() -> String {
  "mutation ScherzoCommentCreate($issueId: String!, $body: String!) { commentCreate(input: { issueId: $issueId, body: $body }) { success comment { id body bodyData } } }"
}

pub fn comment_fetch_query() -> String {
  "query ScherzoCommentFetch($commentId: String!) { comment(id: $commentId) { id body bodyData } }"
}

pub fn issue_comments_query() -> String {
  "query ScherzoIssueComments($issueId: String!) { issue(id: $issueId) { comments(first: 100) { nodes { id body bodyData } pageInfo { hasNextPage endCursor } } } }"
}

pub fn file_upload_mutation() -> String {
  "mutation ScherzoFileUpload($filename: String!, $contentType: String!, $size: Int!, $metaData: JSON) { fileUpload(filename: $filename, contentType: $contentType, size: $size, metaData: $metaData) { success uploadFile { filename contentType size uploadUrl assetUrl headers { key value } } } }"
}

pub fn comment_update_body_data_mutation() -> String {
  "mutation ScherzoCommentUpdateBodyData($commentId: String!, $bodyData: JSON!) { commentUpdate(id: $commentId, input: { bodyData: $bodyData }, skipEditedAt: true) { success comment { id body bodyData } } }"
}

pub fn comment_update_body_mutation() -> String {
  "mutation ScherzoCommentUpdateBody($commentId: String!, $body: String!) { commentUpdate(id: $commentId, input: { body: $body }, skipEditedAt: true) { success comment { id body bodyData } } }"
}

pub fn issue_team_states_query() -> String {
  "query ScherzoIssueTeamStates($issueId: String!) { issue(id: $issueId) { id team { states(first: 50) { nodes { id name type } pageInfo { hasNextPage endCursor } } } } }"
}

pub fn issue_update_state_mutation() -> String {
  "mutation ScherzoIssueUpdateState($issueId: String!, $stateId: String!) { issueUpdate(id: $issueId, input: { stateId: $stateId }) { success } }"
}

pub fn contract_query() -> String {
  contract_query_for_scope(config_types.LinearTaskProject("projectSlug"))
}

fn contract_query_for_scope(scope: config_types.LinearTaskScope) -> String {
  let project_declaration =
    config_types.linear_task_scope_variable_declaration(
      scope,
      "projectSlug",
      "projectSlugs",
    )
  let project_filter = case scope {
    config_types.LinearTaskProject(_) -> "slugId: { eq: $projectSlug }"
    config_types.LinearTaskProjects(_) -> "slugId: { in: $projectSlugs }"
  }
  "query ScherzoLinearContract("
  <> project_declaration
  <> ") { projects(first: "
  <> config_types.linear_task_scope_contract_project_first(scope)
  <> ", filter: { "
  <> project_filter
  <> " }) { nodes { id name slugId teams(first: 10) { nodes { id key name states(first: 50) { nodes { id name type } pageInfo { hasNextPage endCursor } } labels(first: 140) { nodes { id name } pageInfo { hasNextPage endCursor } } } pageInfo { hasNextPage endCursor } } } } issueLabels(first: 100, filter: { team: { null: true } }) { nodes { id name } pageInfo { hasNextPage endCursor } } }"
}

pub fn parse_response(
  response: Response,
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
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

pub fn parse_contract_response(
  response: Response,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, contract_graphql_decoder()) {
        Ok(Ok(raw_data)) -> raw_contract_to_board(raw_data)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

fn parse_contract_response_for_scope(
  response: Response,
  scope: config_types.LinearTaskScope,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, contract_graphql_decoder()) {
        Ok(Ok(raw_data)) -> raw_contract_to_board_for_scope(raw_data, scope)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_issue_team_states_response(
  response: Response,
) -> Result(List(linear_contract.RemoteState), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, issue_team_states_graphql_decoder()) {
        Ok(Ok(states)) -> Ok(states)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn resolve_state_name(
  states: List(linear_contract.RemoteState),
  name: String,
) -> Result(String, StateNameResolutionError) {
  let matches =
    list.filter(states, fn(state) {
      string.trim(state.name) == string.trim(name)
    })
  case matches {
    [] -> Error(StateNameNotFound)
    [state] -> Ok(state.id)
    [_, ..] -> Error(StateNameAmbiguous)
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

pub fn parse_comment_fetch_response(
  response: Response,
) -> Result(LinearCommentDocument, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, comment_fetch_graphql_decoder()) {
        Ok(Ok(comment)) -> Ok(comment)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_issue_comments_response(
  response: Response,
) -> Result(List(LinearCommentDocument), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, issue_comments_graphql_decoder()) {
        Ok(Ok(comments)) -> Ok(comments)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_file_upload_response(
  response: Response,
) -> Result(UploadFile, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, file_upload_graphql_decoder()) {
        Ok(Ok(payload)) ->
          case payload.success, payload.upload_file {
            False, _ ->
              Error(error.LinearUnknownPayload("fileUpload returned false"))
            True, Some(upload_file) -> Ok(upload_file)
            True, None ->
              Error(error.LinearAttachmentError(
                "fileUpload succeeded without uploadFile",
              ))
          }
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

pub fn parse_comment_update_response(
  response: Response,
) -> Result(LinearCommentDocument, error.TrackerError) {
  parse_comment_payload_response(response, "commentUpdate")
}

pub fn parse_comment_create_response(
  response: Response,
) -> Result(LinearCommentDocument, error.TrackerError) {
  parse_comment_payload_response(response, "commentCreate")
}

fn parse_comment_payload_response(
  response: Response,
  root_field: String,
) -> Result(LinearCommentDocument, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case
        json.parse(response.body, comment_payload_graphql_decoder(root_field))
      {
        Ok(Ok(payload)) ->
          case payload.success {
            True -> Ok(payload.comment)
            False ->
              Error(error.LinearUnknownPayload(root_field <> " returned false"))
          }
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

fn issue_team_states_graphql_decoder() -> decode.Decoder(
  Result(List(linear_contract.RemoteState), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(
        ["data", "issue", "team", "states"],
        raw_state_connection_decoder(),
      )
      |> decode.map(fn(connection) {
        case connection.page_info.has_next_page {
          True -> Error("issue team states metadata truncated")
          False -> Ok(raw_states_to_remote(connection.nodes))
        }
      })
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn mutation_decoder(
  root_field: String,
) -> decode.Decoder(Result(Bool, String)) {
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

fn comment_fetch_graphql_decoder() -> decode.Decoder(
  Result(LinearCommentDocument, String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", "comment"], comment_document_decoder())
      |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn issue_comments_graphql_decoder() -> decode.Decoder(
  Result(List(LinearCommentDocument), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(
        ["data", "issue", "comments", "nodes"],
        decode.list(comment_document_decoder()),
      )
      |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

type UploadPayload {
  UploadPayload(success: Bool, upload_file: Option(UploadFile))
}

fn file_upload_graphql_decoder() -> decode.Decoder(
  Result(UploadPayload, String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", "fileUpload"], upload_payload_decoder())
      |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn upload_payload_decoder() -> decode.Decoder(UploadPayload) {
  use success <- decode.field("success", decode.bool)
  use upload_file <- decode.field(
    "uploadFile",
    decode.optional(upload_file_decoder()),
  )
  decode.success(UploadPayload(success: success, upload_file: upload_file))
}

fn upload_file_decoder() -> decode.Decoder(UploadFile) {
  use filename <- decode.field("filename", decode.string)
  use content_type <- decode.field("contentType", decode.string)
  use size <- decode.field("size", decode.int)
  use upload_url <- decode.field("uploadUrl", decode.string)
  use asset_url <- decode.field("assetUrl", decode.string)
  use headers <- decode.field("headers", decode.list(upload_header_decoder()))
  decode.success(UploadFile(
    filename: filename,
    content_type: content_type,
    size: size,
    upload_url: upload_url,
    asset_url: asset_url,
    headers: headers,
  ))
}

fn upload_header_decoder() -> decode.Decoder(UploadHeader) {
  use key <- decode.field("key", decode.string)
  use value <- decode.field("value", decode.string)
  decode.success(UploadHeader(key: key, value: value))
}

type CommentPayload {
  CommentPayload(success: Bool, comment: LinearCommentDocument)
}

fn comment_payload_graphql_decoder(
  root_field: String,
) -> decode.Decoder(Result(CommentPayload, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", root_field], comment_payload_decoder())
      |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn comment_payload_decoder() -> decode.Decoder(CommentPayload) {
  use success <- decode.field("success", decode.bool)
  use comment <- decode.field("comment", comment_document_decoder())
  decode.success(CommentPayload(success: success, comment: comment))
}

fn comment_document_decoder() -> decode.Decoder(LinearCommentDocument) {
  use id <- decode.field("id", decode.string)
  use body <- decode.field("body", decode.string)
  use body_data <- decode.field("bodyData", body_data_decoder())
  decode.success(LinearCommentDocument(id: id, body: body, body_data: body_data))
}

fn body_data_decoder() -> decode.Decoder(linear_body_data.JsonValue) {
  use data <- decode.then(decode.dynamic)
  case decode.run(data, decode.string) {
    Ok(body_data_json) ->
      case linear_body_data.parse_json(body_data_json) {
        Ok(value) -> decode.success(value)
        Error(_) -> decode.success(linear_body_data.JString(body_data_json))
      }
    Error(_) ->
      case decode.run(data, linear_body_data.json_value_decoder()) {
        Ok(value) -> decode.success(value)
        Error(_) ->
          decode.failure(linear_body_data.JNull, expected: "bodyData JSON")
      }
  }
}

type RawContractData {
  RawContractData(
    projects: List(RawProject),
    workspace_labels: RawConnection(RawLabel),
  )
}

type RawProject {
  RawProject(
    id: String,
    name: String,
    slug_id: String,
    teams: RawConnection(RawTeam),
  )
}

type RawTeam {
  RawTeam(
    id: String,
    key: String,
    name: String,
    states: RawConnection(RawState),
    labels: RawConnection(RawLabel),
  )
}

type RawState {
  RawState(id: String, name: String, type_: String)
}

type RawLabel {
  RawLabel(id: String, name: String)
}

type RawConnection(a) {
  RawConnection(nodes: List(a), page_info: PageInfo)
}

fn contract_graphql_decoder() -> decode.Decoder(Result(RawContractData, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] -> {
      use data <- decode.field("data", contract_data_decoder())
      decode.success(Ok(data))
    }
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn contract_data_decoder() -> decode.Decoder(RawContractData) {
  use projects <- decode.field("projects", raw_project_nodes_decoder())
  use workspace_labels <- decode.field(
    "issueLabels",
    raw_label_connection_decoder(),
  )
  decode.success(RawContractData(
    projects: projects,
    workspace_labels: workspace_labels,
  ))
}

fn raw_project_nodes_decoder() -> decode.Decoder(List(RawProject)) {
  use nodes <- decode.field("nodes", decode.list(raw_project_decoder()))
  decode.success(nodes)
}

fn raw_project_decoder() -> decode.Decoder(RawProject) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use slug_id <- decode.field("slugId", decode.string)
  use teams <- decode.field("teams", raw_team_connection_decoder())
  decode.success(RawProject(id: id, name: name, slug_id: slug_id, teams: teams))
}

fn raw_team_connection_decoder() -> decode.Decoder(RawConnection(RawTeam)) {
  use nodes <- decode.field("nodes", decode.list(raw_team_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(RawConnection(nodes: nodes, page_info: page_info))
}

fn raw_team_decoder() -> decode.Decoder(RawTeam) {
  use id <- decode.field("id", decode.string)
  use key <- decode.field("key", decode.string)
  use name <- decode.field("name", decode.string)
  use states <- decode.field("states", raw_state_connection_decoder())
  use labels <- decode.field("labels", raw_label_connection_decoder())
  decode.success(RawTeam(
    id: id,
    key: key,
    name: name,
    states: states,
    labels: labels,
  ))
}

fn raw_state_connection_decoder() -> decode.Decoder(RawConnection(RawState)) {
  use nodes <- decode.field("nodes", decode.list(raw_state_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(RawConnection(nodes: nodes, page_info: page_info))
}

fn raw_state_decoder() -> decode.Decoder(RawState) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use type_ <- decode.field("type", decode.string)
  decode.success(RawState(id: id, name: name, type_: type_))
}

fn raw_label_connection_decoder() -> decode.Decoder(RawConnection(RawLabel)) {
  use nodes <- decode.field("nodes", decode.list(raw_label_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  decode.success(RawConnection(nodes: nodes, page_info: page_info))
}

fn raw_label_decoder() -> decode.Decoder(RawLabel) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  decode.success(RawLabel(id: id, name: name))
}

fn raw_contract_to_board(
  data: RawContractData,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case data.projects {
    [] -> Error(error.LinearUnknownPayload("project slug not found"))
    [project] -> raw_project_to_board(project, data.workspace_labels)
    [_, ..] -> Error(error.LinearUnknownPayload("project slug is not unique"))
  }
}

fn raw_contract_to_board_for_scope(
  data: RawContractData,
  scope: config_types.LinearTaskScope,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case scope {
    config_types.LinearTaskProject(_) -> raw_contract_to_board(data)
    config_types.LinearTaskProjects(_) -> {
      let expected_slugs = config_types.linear_task_scope_project_slugs(scope)
      let matching_projects =
        list.filter_map(expected_slugs, fn(expected) {
          list.find(data.projects, fn(project) {
            config_types.linear_task_scope_matches_project_slug(
              config_types.LinearTaskProject(expected),
              project.slug_id,
            )
          })
        })
      case missing_project_slugs(expected_slugs, matching_projects) {
        [] -> raw_projects_to_board(matching_projects, data.workspace_labels)
        missing ->
          Error(error.LinearUnknownPayload(
            "project slug(s) not found: " <> string.join(missing, with: ", "),
          ))
      }
    }
  }
}

fn missing_project_slugs(
  expected_slugs: List(String),
  projects: List(RawProject),
) -> List(String) {
  list.filter(expected_slugs, fn(expected) {
    !list.any(projects, fn(project) {
      config_types.linear_task_scope_matches_project_slug(
        config_types.LinearTaskProject(expected),
        project.slug_id,
      )
    })
  })
}

fn raw_projects_to_board(
  projects: List(RawProject),
  workspace_labels: RawConnection(RawLabel),
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case projects {
    [] -> Error(error.LinearUnknownPayload("project slug not found"))
    [project] -> raw_project_to_board(project, workspace_labels)
    [_, ..] -> {
      use boards <- try_tracker(
        raw_projects_to_boards(projects, workspace_labels, []),
      )
      case boards {
        [] -> Error(error.LinearUnknownPayload("project slug not found"))
        [primary_board, ..] ->
          Ok(linear_contract.RemoteBoard(
            project_id: primary_board.project_id,
            project_slug: string.join(
              list.map(boards, fn(board) { board.project_slug }),
              with: ",",
            ),
            project_name: string.join(
              list.map(boards, fn(board) { board.project_name }),
              with: ",",
            ),
            teams: collect_board_teams(boards),
            workspace_labels: primary_board.workspace_labels,
          ))
      }
    }
  }
}

fn raw_projects_to_boards(
  projects: List(RawProject),
  workspace_labels: RawConnection(RawLabel),
  acc: List(linear_contract.RemoteBoard),
) -> Result(List(linear_contract.RemoteBoard), error.TrackerError) {
  case projects {
    [] -> Ok(list.reverse(acc))
    [project, ..rest] -> {
      use board <- try_tracker(raw_project_to_board(project, workspace_labels))
      raw_projects_to_boards(rest, workspace_labels, [board, ..acc])
    }
  }
}

fn collect_board_teams(
  boards: List(linear_contract.RemoteBoard),
) -> List(linear_contract.RemoteTeam) {
  boards
  |> list.map(fn(board) { board.teams })
  |> list.flatten
  |> collect_unique_teams([])
}

fn collect_unique_teams(
  teams: List(linear_contract.RemoteTeam),
  acc: List(linear_contract.RemoteTeam),
) -> List(linear_contract.RemoteTeam) {
  case teams {
    [] -> list.reverse(acc)
    [team, ..rest] ->
      case list.any(acc, fn(existing) { existing.id == team.id }) {
        True -> collect_unique_teams(rest, acc)
        False -> collect_unique_teams(rest, [team, ..acc])
      }
  }
}

fn raw_project_to_board(
  project: RawProject,
  workspace_labels: RawConnection(RawLabel),
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  case project.teams.page_info.has_next_page {
    True ->
      Error(error.LinearUnknownPayload("project teams metadata truncated"))
    False ->
      case list.is_empty(project.teams.nodes) {
        True -> Error(error.LinearUnknownPayload("project has no teams"))
        False ->
          case workspace_labels.page_info.has_next_page {
            True ->
              Error(error.LinearUnknownPayload(
                "workspace issue labels metadata truncated",
              ))
            False -> {
              use teams <- try_tracker(
                raw_teams_to_remote(project.teams.nodes, []),
              )
              Ok(linear_contract.RemoteBoard(
                project_id: project.id,
                project_slug: project.slug_id,
                project_name: project.name,
                teams: teams,
                workspace_labels: raw_labels_to_remote(workspace_labels.nodes),
              ))
            }
          }
      }
  }
}

fn raw_teams_to_remote(
  teams: List(RawTeam),
  acc: List(linear_contract.RemoteTeam),
) -> Result(List(linear_contract.RemoteTeam), error.TrackerError) {
  case teams {
    [] -> Ok(list.reverse(acc))
    [team, ..rest] -> {
      use remote <- try_tracker(raw_team_to_remote(team))
      raw_teams_to_remote(rest, [remote, ..acc])
    }
  }
}

fn raw_team_to_remote(
  team: RawTeam,
) -> Result(linear_contract.RemoteTeam, error.TrackerError) {
  case team.states.page_info.has_next_page {
    True ->
      Error(error.LinearUnknownPayload(
        "team " <> team.key <> " states metadata truncated",
      ))
    False ->
      case team.labels.page_info.has_next_page {
        True ->
          Error(error.LinearUnknownPayload(
            "team " <> team.key <> " labels metadata truncated",
          ))
        False ->
          Ok(linear_contract.RemoteTeam(
            id: team.id,
            key: team.key,
            name: team.name,
            states: raw_states_to_remote(team.states.nodes),
            labels: raw_labels_to_remote(team.labels.nodes),
          ))
      }
  }
}

fn raw_states_to_remote(
  states: List(RawState),
) -> List(linear_contract.RemoteState) {
  states
  |> list.map(fn(state) {
    linear_contract.RemoteState(
      id: state.id,
      name: state.name,
      type_: state.type_,
    )
  })
}

fn raw_labels_to_remote(
  labels: List(RawLabel),
) -> List(linear_contract.RemoteLabel) {
  labels
  |> list.map(fn(label) {
    linear_contract.RemoteLabel(id: label.id, name: label.name)
  })
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

fn issue_decoder() -> decode.Decoder(tracker_issue.Issue) {
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
  // Linear dependency direction matters: for candidate A, an incoming
  // inverseRelations `blocks` relation points at blocker B. Outgoing
  // `relations` means A blocks some other issue and is intentionally ignored.
  use blocker_page <- decode.field("inverseRelations", blockers_decoder())
  decode.success(tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_name: branch_name,
    url: url,
    labels: list.map(labels, string.lowercase),
    blocked_by: blocker_page.blockers,
    blocked_by_complete: blocker_page.complete,
    created_at: parse_optional_time(created_at),
    updated_at: parse_optional_time(updated_at),
  ))
}

fn state_name_decoder() -> decode.Decoder(issue_state.IssueState) {
  use name <- decode.field("name", decode.string)
  decode.success(issue_state.from_string_unchecked(name))
}

fn labels_decoder() -> decode.Decoder(List(String)) {
  use nodes <- decode.field("nodes", decode.list(label_decoder()))
  decode.success(nodes)
}

fn label_decoder() -> decode.Decoder(String) {
  use name <- decode.field("name", decode.string)
  decode.success(name)
}

pub type BlockerPage {
  BlockerPage(blockers: List(tracker_issue.BlockerRef), complete: Bool)
}

fn blockers_decoder() -> decode.Decoder(BlockerPage) {
  use nodes <- decode.field("nodes", decode.list(relation_decoder()))
  use page_info <- decode.field("pageInfo", page_info_decoder())
  let blockers =
    list.filter_map(nodes, fn(rel) {
      case rel {
        Relation("blocks", blocker) -> Ok(blocker)
        _ -> Error(Nil)
      }
    })
  decode.success(BlockerPage(
    blockers: blockers,
    complete: !page_info.has_next_page,
  ))
}

pub type Relation {
  Relation(type_: String, blocker: tracker_issue.BlockerRef)
}

pub type RelatedIssue {
  RelatedIssue(
    id: Option(String),
    identifier: Option(String),
    state: Option(issue_state.IssueState),
  )
}

fn relation_decoder() -> decode.Decoder(Relation) {
  use type_ <- decode.field("type", decode.string)
  use related <- decode.field("issue", related_issue_decoder())
  decode.success(Relation(
    type_: type_,
    blocker: tracker_issue.BlockerRef(
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

fn optional_state_name_decoder() -> decode.Decoder(
  Option(issue_state.IssueState),
) {
  use name <- decode.field("name", decode.optional(decode.string))
  decode.success(case name {
    Some(name) -> Some(issue_state.from_string_unchecked(name))
    None -> None
  })
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

fn require_task_scope(
  config: config_types.TrackerConfig,
) -> Result(config_types.LinearTaskScope, error.TrackerError) {
  config_types.linear_task_scope_from_tracker_config(config)
  |> result.map_error(fn(scope_error) {
    error.LinearApiRequest(config_types.linear_task_scope_error_message(
      scope_error,
    ))
  })
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
    Error(err) -> Error(err)
  }
}
