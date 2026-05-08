import birl
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/linear_contract
import scherzo/orchestrator/schedule_core

pub const body_marker_prefix = "<!-- scherzo-dedupe: "

pub type FailureReportRequest {
  FailureReportRequest(
    job_id: String,
    workflow_id: String,
    due_at_ms: Int,
    run_id: String,
    attempt: Int,
    max_attempts: Int,
    reason: String,
    run_root: Option(String),
    session_id: Option(String),
    dedupe_key: String,
    triage_state: String,
    configured_labels: List(String),
    previous_issue_id: Option(String),
  )
}

pub type FailureReportOutcome {
  FailureReportCreated(linear_issue_id: String)
  FailureReportUpdated(linear_issue_id: String)
  FailureReportNoop
}

pub type ExistingFailureIssue {
  ExistingFailureIssue(id: String, updated_at_ms: Int)
}

pub type Backend {
  Backend(
    ensure_label: fn(String) -> Result(String, error.TrackerError),
    find_open_issue_by_id: fn(String) ->
      Result(Option(ExistingFailureIssue), error.TrackerError),
    find_open_issues_by_labels: fn(List(String)) ->
      Result(List(ExistingFailureIssue), error.TrackerError),
    create_issue: fn(String, String, String, List(String)) ->
      Result(String, error.TrackerError),
    comment_issue: fn(String, String) -> Result(Nil, error.TrackerError),
    move_issue_to_state: fn(String, String) -> Result(Nil, error.TrackerError),
  )
}

pub type Client {
  Client(
    report_failure: fn(FailureReportRequest) ->
      Result(FailureReportOutcome, error.TrackerError),
  )
}

pub fn disabled_client() -> Client {
  Client(report_failure: fn(_) { Ok(FailureReportNoop) })
}

pub fn client(backend: Backend) -> Client {
  Client(report_failure: fn(request) { report_with_backend(request, backend) })
}

pub fn real_client(config: config_types.TrackerConfig) -> Client {
  real_client_with_transport(config, linear.http_transport)
}

pub fn real_client_with_transport(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> Client {
  client(real_backend(config, transport))
}

pub fn dedupe_key(job_id: String) -> String {
  "scheduled-job:" <> job_id
}

pub fn reserved_labels(job_id: String) -> List(String) {
  ["scherzo:scheduled", "scherzo:scheduled-job:" <> job_id]
}

pub fn body_marker(dedupe_key: String) -> String {
  body_marker_prefix <> dedupe_key <> " -->"
}

pub fn action(outcome: FailureReportOutcome) -> String {
  case outcome {
    FailureReportCreated(_) -> "created"
    FailureReportUpdated(_) -> "updated"
    FailureReportNoop -> "noop"
  }
}

pub fn issue_id(outcome: FailureReportOutcome) -> Option(String) {
  case outcome {
    FailureReportCreated(id) | FailureReportUpdated(id) -> Some(id)
    FailureReportNoop -> None
  }
}

pub fn report_with_backend(
  request: FailureReportRequest,
  backend: Backend,
) -> Result(FailureReportOutcome, error.TrackerError) {
  let label_names = all_label_names(request)
  use label_ids <- try_tracker(ensure_labels(label_names, backend))
  case request.previous_issue_id {
    Some(issue_id) -> {
      use existing_issue <- try_tracker(backend.find_open_issue_by_id(issue_id))
      case existing_issue {
        Some(issue) -> update_existing_issue(request, backend, issue.id, False)
        None -> report_with_reserved_label_dedupe(request, backend, label_ids)
      }
    }
    None -> report_with_reserved_label_dedupe(request, backend, label_ids)
  }
}

fn report_with_reserved_label_dedupe(
  request: FailureReportRequest,
  backend: Backend,
  label_ids: List(String),
) -> Result(FailureReportOutcome, error.TrackerError) {
  use matches <- try_tracker(
    backend.find_open_issues_by_labels(reserved_labels(request.job_id)),
  )
  case matches {
    [] -> create_issue(request, backend, label_ids)
    [issue, ..rest] -> {
      let duplicate_note = !list.is_empty(rest)
      let issue = most_recent_issue(issue, rest)
      update_existing_issue(request, backend, issue.id, duplicate_note)
    }
  }
}

fn most_recent_issue(
  first: ExistingFailureIssue,
  rest: List(ExistingFailureIssue),
) -> ExistingFailureIssue {
  list.fold(rest, first, fn(latest, issue) {
    case issue.updated_at_ms > latest.updated_at_ms {
      True -> issue
      False -> latest
    }
  })
}

fn create_issue(
  request: FailureReportRequest,
  backend: Backend,
  label_ids: List(String),
) -> Result(FailureReportOutcome, error.TrackerError) {
  use issue_id <- try_tracker(backend.create_issue(
    title(request.job_id),
    body(request),
    request.triage_state,
    label_ids,
  ))
  Ok(FailureReportCreated(issue_id))
}

fn update_existing_issue(
  request: FailureReportRequest,
  backend: Backend,
  issue_id: String,
  duplicate_note: Bool,
) -> Result(FailureReportOutcome, error.TrackerError) {
  use Nil <- try_tracker(backend.comment_issue(
    issue_id,
    update_comment(request, issue_id, duplicate_note),
  ))
  use Nil <- try_tracker(backend.move_issue_to_state(
    issue_id,
    request.triage_state,
  ))
  Ok(FailureReportUpdated(issue_id))
}

fn ensure_labels(
  label_names: List(String),
  backend: Backend,
) -> Result(List(String), error.TrackerError) {
  ensure_labels_loop(unique_strings(label_names), backend, [])
}

fn ensure_labels_loop(
  names: List(String),
  backend: Backend,
  acc: List(String),
) -> Result(List(String), error.TrackerError) {
  case names {
    [] -> Ok(list.reverse(acc))
    [name, ..rest] -> {
      use id <- try_tracker(backend.ensure_label(name))
      ensure_labels_loop(rest, backend, [id, ..acc])
    }
  }
}

fn all_label_names(request: FailureReportRequest) -> List(String) {
  list.append(reserved_labels(request.job_id), request.configured_labels)
}

fn unique_strings(values: List(String)) -> List(String) {
  unique_strings_loop(values, [])
}

fn unique_strings_loop(
  values: List(String),
  acc: List(String),
) -> List(String) {
  case values {
    [] -> list.reverse(acc)
    [value, ..rest] -> {
      let value = string.trim(value)
      case value == "" || list.contains(acc, value) {
        True -> unique_strings_loop(rest, acc)
        False -> unique_strings_loop(rest, [value, ..acc])
      }
    }
  }
}

pub fn title(job_id: String) -> String {
  "Scherzo scheduled job failed: " <> job_id
}

pub fn body(request: FailureReportRequest) -> String {
  string.join(
    [
      body_marker(request.dedupe_key),
      "Scheduled job: " <> request.job_id,
      "Workflow: " <> request.workflow_id,
      "Due at: " <> schedule_core.iso_utc(request.due_at_ms),
      "Run ID: " <> request.run_id,
      "Attempts: "
        <> int.to_string(request.attempt)
        <> " of "
        <> int.to_string(request.max_attempts),
      "Failure: " <> request.reason,
      "Run root: " <> option_string(request.run_root),
      "Latest session: " <> option_string(request.session_id),
      "Dedupe key: " <> request.dedupe_key,
      "Reserved labels: "
        <> string.join(reserved_labels(request.job_id), with: ", "),
      "",
      "Local diagnostics:",
      "  scherzoctl schedules status " <> request.job_id,
      "  scherzoctl schedules history " <> request.job_id,
      "  scherzoctl schedules logs " <> request.job_id <> " --last",
      "  scherzoctl schedules run " <> request.job_id <> " --now",
    ],
    with: "\n",
  )
}

fn update_comment(
  request: FailureReportRequest,
  issue_id: String,
  duplicate_note: Bool,
) -> String {
  let duplicate_text = case duplicate_note {
    True ->
      "\n\nNote: multiple open Linear issues carried Scherzo's reserved scheduled-job dedupe labels. This issue was updated instead of creating another duplicate."
    False -> ""
  }
  body(FailureReportRequest(..request, previous_issue_id: Some(issue_id)))
  <> "\nCurrent failure issue ID: "
  <> issue_id
  <> duplicate_text
}

fn option_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "-"
  }
}

fn build_issue_label_create_request(
  config: config_types.TrackerConfig,
  team_id: String,
  name: String,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(issue_label_create_mutation())),
      #(
        "variables",
        json.object([
          #("teamId", json.string(team_id)),
          #("name", json.string(name)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

fn build_scheduled_failure_issue_search_request(
  config: config_types.TrackerConfig,
  label_names: List(String),
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  use project_slug <- try_tracker(require_project_slug(config))
  let body =
    json.object([
      #("query", json.string(scheduled_failure_issue_search_query())),
      #(
        "variables",
        json.object([
          #("projectSlug", json.string(project_slug)),
          #("labelFilters", build_label_filter_inputs(label_names)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

fn build_label_filter_inputs(label_names: List(String)) -> json.Json {
  json.array(label_names, of: label_filter_input)
}

fn label_filter_input(label_name: String) -> json.Json {
  json.object([
    #(
      "labels",
      json.object([
        #("name", json.object([#("eq", json.string(label_name))])),
      ]),
    ),
  ])
}

fn build_scheduled_failure_issue_lookup_request(
  config: config_types.TrackerConfig,
  issue_id: String,
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(scheduled_failure_issue_lookup_query())),
      #(
        "variables",
        json.object([#("issueIds", json.array([issue_id], of: json.string))]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
}

fn build_scheduled_failure_issue_create_request(
  config: config_types.TrackerConfig,
  team_id: String,
  project_id: String,
  state_id: String,
  title: String,
  description: String,
  label_ids: List(String),
) -> Result(linear.Request, error.TrackerError) {
  use endpoint <- try_tracker(require_https_endpoint(config.endpoint))
  use api_key <- try_tracker(require_api_key(config))
  let body =
    json.object([
      #("query", json.string(scheduled_failure_issue_create_mutation())),
      #(
        "variables",
        json.object([
          #("teamId", json.string(team_id)),
          #("projectId", json.string(project_id)),
          #("stateId", json.string(state_id)),
          #("title", json.string(title)),
          #("description", json.string(description)),
          #("labelIds", json.array(label_ids, of: json.string)),
        ]),
      ),
    ])
    |> json.to_string
  Ok(graphql_request(endpoint, api_key, body))
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
  case string.starts_with(endpoint, "https://") {
    True -> Ok(endpoint)
    False -> Error(error.LinearApiRequest("Linear endpoint must use https"))
  }
}

fn require_api_key(
  config: config_types.TrackerConfig,
) -> Result(String, error.TrackerError) {
  case config.api_key {
    Some(api_key) ->
      case string.trim(api_key) == "" {
        True -> Error(error.LinearApiRequest("missing Linear API key"))
        False -> Ok(api_key)
      }
    None -> Error(error.LinearApiRequest("missing Linear API key"))
  }
}

fn require_project_slug(
  config: config_types.TrackerConfig,
) -> Result(String, error.TrackerError) {
  case config.project_slug {
    Some(project_slug) ->
      case string.trim(project_slug) == "" {
        True -> Error(error.LinearApiRequest("missing Linear project slug"))
        False -> Ok(project_slug)
      }
    None -> Error(error.LinearApiRequest("missing Linear project slug"))
  }
}

fn issue_label_create_mutation() -> String {
  "mutation ScherzoIssueLabelCreate($teamId: String!, $name: String!) { issueLabelCreate(input: { teamId: $teamId, name: $name }) { success issueLabel { id name } } }"
}

fn scheduled_failure_issue_search_query() -> String {
  "query ScherzoScheduledFailureIssues($projectSlug: String!, $labelFilters: [IssueFilter!]!) { issues(first: 50, filter: { project: { slugId: { eq: $projectSlug } }, state: { type: { nin: [\"completed\", \"canceled\", \"duplicate\"] } }, and: $labelFilters }, orderBy: updatedAt) { nodes { id updatedAt state { type } labels { nodes { name } } } } }"
}

fn scheduled_failure_issue_lookup_query() -> String {
  "query ScherzoScheduledFailureIssueById($issueIds: [ID!]!) { issues(first: 1, filter: { id: { in: $issueIds }, state: { type: { nin: [\"completed\", \"canceled\", \"duplicate\"] } } }) { nodes { id updatedAt state { type } labels { nodes { name } } } } }"
}

fn scheduled_failure_issue_create_mutation() -> String {
  "mutation ScherzoScheduledFailureIssueCreate($teamId: String!, $projectId: String!, $stateId: String!, $title: String!, $description: String!, $labelIds: [String!]) { issueCreate(input: { teamId: $teamId, projectId: $projectId, stateId: $stateId, title: $title, description: $description, labelIds: $labelIds }) { success issue { id identifier url } } }"
}

fn parse_issue_label_create_response(
  response: linear.Response,
) -> Result(String, error.TrackerError) {
  parse_string_mutation_response(response, issue_label_create_graphql_decoder())
}

fn parse_scheduled_failure_issue_search_response(
  response: linear.Response,
  required_labels: List(String),
) -> Result(List(ExistingFailureIssue), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, scheduled_issue_nodes_graphql_decoder()) {
        Ok(Ok(issues)) -> Ok(matching_open_issues(issues, required_labels))
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

fn parse_scheduled_failure_issue_lookup_response(
  response: linear.Response,
) -> Result(Option(ExistingFailureIssue), error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, scheduled_issue_nodes_graphql_decoder()) {
        Ok(Ok(issues)) -> Ok(first_open_issue(issues))
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

fn parse_scheduled_failure_issue_create_response(
  response: linear.Response,
) -> Result(String, error.TrackerError) {
  parse_string_mutation_response(
    response,
    scheduled_issue_create_graphql_decoder(),
  )
}

fn parse_string_mutation_response(
  response: linear.Response,
  decoder: decode.Decoder(Result(String, String)),
) -> Result(String, error.TrackerError) {
  case response.status == 200 {
    False -> Error(error.LinearApiStatus(response.status))
    True ->
      case json.parse(response.body, decoder) {
        Ok(Ok(id)) -> Ok(id)
        Ok(Error(message)) -> Error(error.LinearGraphqlErrors(message))
        Error(_) -> Error(error.LinearUnknownPayload("invalid JSON payload"))
      }
  }
}

type IdMutationPayload {
  IdMutationPayload(success: Bool, id: String)
}

fn issue_label_create_graphql_decoder() -> decode.Decoder(
  Result(String, String),
) {
  id_mutation_graphql_decoder("issueLabelCreate", "issueLabel")
}

fn scheduled_issue_create_graphql_decoder() -> decode.Decoder(
  Result(String, String),
) {
  id_mutation_graphql_decoder("issueCreate", "issue")
}

fn id_mutation_graphql_decoder(
  root_field: String,
  id_field: String,
) -> decode.Decoder(Result(String, String)) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(["data", root_field], id_mutation_payload_decoder(id_field))
      |> decode.map(fn(payload) {
        case payload.success {
          True -> Ok(payload.id)
          False -> Error(root_field <> " returned false")
        }
      })
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn id_mutation_payload_decoder(
  id_field: String,
) -> decode.Decoder(IdMutationPayload) {
  use success <- decode.field("success", decode.bool)
  use id <- decode.field(id_field, id_decoder())
  decode.success(IdMutationPayload(success: success, id: id))
}

fn id_decoder() -> decode.Decoder(String) {
  use id <- decode.field("id", decode.string)
  decode.success(id)
}

type RawExistingFailureIssue {
  RawExistingFailureIssue(
    id: String,
    updated_at_ms: Int,
    state_type: String,
    label_names: List(String),
  )
}

fn scheduled_issue_nodes_graphql_decoder() -> decode.Decoder(
  Result(List(RawExistingFailureIssue), String),
) {
  use errors <- decode.optional_field(
    "errors",
    [],
    decode.list(error_message_decoder()),
  )
  case errors {
    [] ->
      decode.at(
        ["data", "issues", "nodes"],
        decode.list(raw_existing_failure_issue_decoder()),
      )
      |> decode.map(Ok)
    errors -> decode.success(Error(string.join(errors, with: "; ")))
  }
}

fn raw_existing_failure_issue_decoder() -> decode.Decoder(
  RawExistingFailureIssue,
) {
  use id <- decode.field("id", decode.string)
  use updated_at <- decode.field("updatedAt", decode.string)
  use state_type <- decode.field("state", state_type_decoder())
  use label_names <- decode.field("labels", label_names_connection_decoder())
  let updated_at_ms = case birl.parse(updated_at) {
    Ok(time) -> birl.to_unix_milli(time)
    Error(parse_error) -> invalid_updated_at_ms(parse_error)
  }
  decode.success(RawExistingFailureIssue(
    id: id,
    updated_at_ms: updated_at_ms,
    state_type: state_type,
    label_names: label_names,
  ))
}

fn invalid_updated_at_ms(_parse_error: Nil) -> Int {
  0
}

fn state_type_decoder() -> decode.Decoder(String) {
  use type_ <- decode.field("type", decode.string)
  decode.success(type_)
}

fn label_names_connection_decoder() -> decode.Decoder(List(String)) {
  decode.at(["nodes"], decode.list(label_name_decoder()))
}

fn label_name_decoder() -> decode.Decoder(String) {
  use name <- decode.field("name", decode.string)
  decode.success(name)
}

fn matching_open_issues(
  issues: List(RawExistingFailureIssue),
  required_labels: List(String),
) -> List(ExistingFailureIssue) {
  issues
  |> list.filter(fn(issue) {
    raw_issue_is_open(issue)
    && has_all_labels(issue.label_names, required_labels)
  })
  |> list.map(raw_issue_to_existing)
}

fn first_open_issue(
  issues: List(RawExistingFailureIssue),
) -> Option(ExistingFailureIssue) {
  case list.filter(issues, raw_issue_is_open) {
    [] -> None
    [issue, ..] -> Some(raw_issue_to_existing(issue))
  }
}

fn raw_issue_to_existing(
  issue: RawExistingFailureIssue,
) -> ExistingFailureIssue {
  ExistingFailureIssue(id: issue.id, updated_at_ms: issue.updated_at_ms)
}

fn raw_issue_is_open(issue: RawExistingFailureIssue) -> Bool {
  case string.lowercase(issue.state_type) {
    "completed" | "canceled" | "duplicate" -> False
    _ -> True
  }
}

fn has_all_labels(
  label_names: List(String),
  required_labels: List(String),
) -> Bool {
  case required_labels {
    [] -> True
    [label, ..rest] ->
      list.contains(label_names, label) && has_all_labels(label_names, rest)
  }
}

fn error_message_decoder() -> decode.Decoder(String) {
  use message <- decode.field("message", decode.string)
  decode.success(message)
}

fn real_backend(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> Backend {
  Backend(
    ensure_label: fn(name) { real_ensure_label(config, transport, name) },
    find_open_issue_by_id: fn(issue_id) {
      real_find_open_issue_by_id(config, transport, issue_id)
    },
    find_open_issues_by_labels: fn(labels) {
      real_find_open_issues_by_labels(config, transport, labels)
    },
    create_issue: fn(title, body, state_name, label_ids) {
      real_create_issue(config, transport, title, body, state_name, label_ids)
    },
    comment_issue: fn(issue_id, body) {
      use request <- try_tracker(linear.build_comment_create_request(
        config,
        issue_id,
        body,
      ))
      use response <- try_tracker(transport(request))
      linear.parse_mutation_response(response, "commentCreate")
    },
    move_issue_to_state: fn(issue_id, state_name) {
      use state_id <- try_tracker(real_state_id(config, transport, state_name))
      use request <- try_tracker(linear.build_issue_update_state_request(
        config,
        issue_id,
        state_id,
      ))
      use response <- try_tracker(transport(request))
      linear.parse_mutation_response(response, "issueUpdate")
    },
  )
}

fn real_ensure_label(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  name: String,
) -> Result(String, error.TrackerError) {
  use board <- try_tracker(fetch_board(config, transport))
  case find_label_id(board, name) {
    Some(id) -> Ok(id)
    None -> {
      use team <- try_tracker(first_team(board))
      use request <- try_tracker(build_issue_label_create_request(
        config,
        team.id,
        name,
      ))
      use response <- try_tracker(transport(request))
      parse_issue_label_create_response(response)
    }
  }
}

fn real_find_open_issue_by_id(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  issue_id: String,
) -> Result(Option(ExistingFailureIssue), error.TrackerError) {
  use request <- try_tracker(build_scheduled_failure_issue_lookup_request(
    config,
    issue_id,
  ))
  use response <- try_tracker(transport(request))
  parse_scheduled_failure_issue_lookup_response(response)
}

fn real_find_open_issues_by_labels(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  labels: List(String),
) -> Result(List(ExistingFailureIssue), error.TrackerError) {
  use request <- try_tracker(build_scheduled_failure_issue_search_request(
    config,
    labels,
  ))
  use response <- try_tracker(transport(request))
  parse_scheduled_failure_issue_search_response(response, labels)
}

fn real_create_issue(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  title: String,
  body: String,
  state_name: String,
  label_ids: List(String),
) -> Result(String, error.TrackerError) {
  use board <- try_tracker(fetch_board(config, transport))
  use team <- try_tracker(first_team(board))
  use state_id <- try_tracker(state_id_for_team(team, state_name))
  use request <- try_tracker(build_scheduled_failure_issue_create_request(
    config,
    team.id,
    board.project_id,
    state_id,
    title,
    body,
    label_ids,
  ))
  use response <- try_tracker(transport(request))
  parse_scheduled_failure_issue_create_response(response)
}

fn real_state_id(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
  state_name: String,
) -> Result(String, error.TrackerError) {
  use board <- try_tracker(fetch_board(config, transport))
  use team <- try_tracker(first_team(board))
  state_id_for_team(team, state_name)
}

fn fetch_board(
  config: config_types.TrackerConfig,
  transport: linear.Transport,
) -> Result(linear_contract.RemoteBoard, error.TrackerError) {
  use request <- try_tracker(linear.build_contract_request(config))
  use response <- try_tracker(transport(request))
  linear.parse_contract_response(response)
}

fn first_team(
  board: linear_contract.RemoteBoard,
) -> Result(linear_contract.RemoteTeam, error.TrackerError) {
  case board.teams {
    [] ->
      Error(error.LinearUnknownPayload(
        "scheduled failure reporting requires a Linear project team",
      ))
    [team, ..] -> Ok(team)
  }
}

fn state_id_for_team(
  team: linear_contract.RemoteTeam,
  state_name: String,
) -> Result(String, error.TrackerError) {
  case list.find(team.states, fn(state) { state.name == state_name }) {
    Ok(state) -> Ok(state.id)
    Error(Nil) ->
      Error(error.LinearUnknownPayload(
        "scheduled failure reporting state not found: " <> state_name,
      ))
  }
}

fn find_label_id(
  board: linear_contract.RemoteBoard,
  label_name: String,
) -> Option(String) {
  case find_label_in_list(board.workspace_labels, label_name) {
    Some(id) -> Some(id)
    None -> find_label_in_teams(board.teams, label_name)
  }
}

fn find_label_in_teams(
  teams: List(linear_contract.RemoteTeam),
  label_name: String,
) -> Option(String) {
  case teams {
    [] -> None
    [team, ..rest] ->
      case find_label_in_list(team.labels, label_name) {
        Some(id) -> Some(id)
        None -> find_label_in_teams(rest, label_name)
      }
  }
}

fn find_label_in_list(
  labels: List(linear_contract.RemoteLabel),
  label_name: String,
) -> Option(String) {
  case labels {
    [] -> None
    [label, ..rest] ->
      case label.name == label_name {
        True -> Some(label.id)
        False -> find_label_in_list(rest, label_name)
      }
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
