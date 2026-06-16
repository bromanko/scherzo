import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/linear_task_scope
import scherzo/config/types as config_types
import scherzo/error
import scherzo/json_value
import scherzo/linear
import scherzo/linear/task_query
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import simplifile
import yay

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    task_scope: None,
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn multi_project_tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    ..tracker_config(),
    project_slug: None,
    task_scope: Some(config_types.LinearTaskProjects(["PROJ", "BUGS"])),
  )
}

fn composed_project_tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    ..tracker_config(),
    project_slug: None,
    task_scope: Some(composed_project_scope()),
  )
}

fn composed_project_scope() -> config_types.LinearTaskScope {
  config_types.LinearTaskOr([
    config_types.LinearTaskProject("PROJ"),
    config_types.LinearTaskAnd([
      config_types.LinearTaskProjects(["BUGS", "OPS"]),
      config_types.LinearTaskProject("BUGS"),
    ]),
  ])
}

fn composed_project_task_filter_json() -> String {
  "{\"or\":[{\"project\":{\"slugId\":{\"eq\":\"PROJ\"}}},{\"and\":[{\"project\":{\"slugId\":{\"in\":[\"BUGS\",\"OPS\"]}}},{\"project\":{\"slugId\":{\"eq\":\"BUGS\"}}}]}]}"
}

fn composed_project_filter_json() -> String {
  "{\"or\":[{\"slugId\":{\"eq\":\"PROJ\"}},{\"and\":[{\"slugId\":{\"in\":[\"BUGS\",\"OPS\"]}},{\"slugId\":{\"eq\":\"BUGS\"}}]}]}"
}

fn labelled_tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    ..tracker_config(),
    project_slug: None,
    task_scope: Some(labelled_task_scope()),
  )
}

fn labelled_task_scope() -> config_types.LinearTaskScope {
  config_types.LinearTaskAnd([
    config_types.LinearTaskProject("PROJ"),
    config_types.LinearTaskAllLabels([
      "workflow:implementation",
      "backend",
      "workflow:implementation",
    ]),
    config_types.LinearTaskAnyLabel(["customer-visible", "urgent"]),
  ])
}

fn labelled_task_filter_json() -> String {
  "{\"and\":[{\"project\":{\"slugId\":{\"eq\":\"PROJ\"}}},{\"and\":[{\"labels\":{\"some\":{\"name\":{\"eq\":\"workflow:implementation\"}}}},{\"labels\":{\"some\":{\"name\":{\"eq\":\"backend\"}}}}]},{\"or\":[{\"labels\":{\"some\":{\"name\":{\"eq\":\"customer-visible\"}}}},{\"labels\":{\"some\":{\"name\":{\"eq\":\"urgent\"}}}}]}]}"
}

fn labelled_project_filter_json() -> String {
  "{\"and\":[{\"slugId\":{\"eq\":\"PROJ\"}}]}"
}

pub fn direct_task_scope_validation_rejects_invalid_nested_scopes_test() {
  let invalid_scopes = [
    config_types.LinearTaskAnd([
      config_types.LinearTaskProject("PROJ"),
      config_types.LinearTaskOr([]),
    ]),
    config_types.LinearTaskAnd([
      config_types.LinearTaskProject("PROJ"),
      config_types.LinearTaskAllLabels([]),
    ]),
    config_types.LinearTaskAnd([
      config_types.LinearTaskProject("PROJ"),
      config_types.LinearTaskAnyLabel(["  "]),
    ]),
  ]

  list.each(invalid_scopes, fn(scope) {
    let config =
      config_types.TrackerConfig(
        ..tracker_config(),
        project_slug: None,
        task_scope: Some(scope),
      )
    let assert Error(config_types.MissingLinearTaskScopeProject) =
      config_types.linear_task_scope_from_tracker_config(config)
  })

  let assert Ok(valid_scope) =
    config_types.linear_task_scope_from_tracker_config(
      labelled_tracker_config(),
    )
  assert linear_task_scope.summary(valid_scope)
    == "and(project(PROJ), all_labels([workflow:implementation, backend]), any_label([customer-visible, urgent]))"
}

fn response_page(
  identifier: String,
  has_next: String,
  cursor: String,
) -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\""
  <> identifier
  <> "-id\",\"identifier\":\""
  <> identifier
  <> "\",\"title\":\"Title "
  <> identifier
  <> "\",\"description\":\"Desc\",\"priority\":1,\"branchName\":\"branch\",\"url\":\"https://linear/"
  <> identifier
  <> "\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[{\"name\":\"Bug\"}]},\"inverseRelations\":{\"nodes\":[{\"type\":\"blocks\",\"issue\":{\"id\":\"B-id\",\"identifier\":\"B-1\",\"state\":{\"name\":\"Done\"}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":"
  <> has_next
  <> ",\"endCursor\":"
  <> cursor
  <> "}}}}"
}

fn request_query(body: String) -> String {
  let assert Ok(query) = json.parse(body, decode.at(["query"], decode.string))
  query
}

fn string_variable(body: String, name: String) -> String {
  let assert Ok(value) =
    json.parse(body, decode.at(["variables", name], decode.string))
  value
}

fn string_list_variable(body: String, name: String) -> List(String) {
  let assert Ok(value) =
    json.parse(
      body,
      decode.at(["variables", name], decode.list(of: decode.string)),
    )
  value
}

fn optional_string_variable(body: String, name: String) -> Option(String) {
  let assert Ok(value) =
    json.parse(
      body,
      decode.at(["variables", name], decode.optional(decode.string)),
    )
  value
}

fn json_variable(body: String, name: String) -> String {
  let assert Ok(value) =
    json.parse(body, decode.at(["variables", name], json_value.decoder()))
  json_value.to_string(value)
}

fn variable_names(body: String) -> List(String) {
  let assert Ok(variables) =
    json.parse(
      body,
      decode.at(["variables"], decode.dict(decode.string, decode.dynamic)),
    )
  variables
  |> dict.keys
  |> list.sort(by: string.compare)
}

pub fn candidate_query_uses_task_filter_variable_test() {
  let assert Error(error.LinearApiRequest(_)) =
    linear.build_candidate_request(
      config_types.TrackerConfig(
        ..tracker_config(),
        endpoint: "http://linear.test",
      ),
      issue_state.list_from_strings(["Todo"]),
      Some("cursor"),
    )
  let assert Ok(request) =
    linear.build_candidate_request(
      tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      Some("cursor"),
    )
  let query = request_query(request.body)
  assert variable_names(request.body)
    == ["after", "dispatchStates", "taskFilter"]
  assert json_variable(request.body, "taskFilter")
    == "{\"project\":{\"slugId\":{\"eq\":\"PROJ\"}}}"
  assert string_list_variable(request.body, "dispatchStates") == ["Todo"]
  assert optional_string_variable(request.body, "after") == Some("cursor")
  assert string.contains(
    query,
    "query CandidateIssues($taskFilter: IssueFilter!, $dispatchStates: [String!], $after: String)",
  )
  assert string.contains(
    query,
    "issues(first: 50, after: $after, filter: { and: [$taskFilter], state: { name: { in: $dispatchStates } } })",
  )
  assert string.contains(
    query,
    "inverseRelations(first: 100) { nodes { type issue { id identifier state { name } } } pageInfo { hasNextPage endCursor } }",
  )
  assert request.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]
}

pub fn candidate_query_uses_multi_project_task_filter_test() {
  let assert Ok(request) =
    linear.build_candidate_request(
      multi_project_tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      Some("cursor"),
    )
  let query = request_query(request.body)
  assert variable_names(request.body)
    == ["after", "dispatchStates", "taskFilter"]
  assert json_variable(request.body, "taskFilter")
    == "{\"project\":{\"slugId\":{\"in\":[\"PROJ\",\"BUGS\"]}}}"
  assert string.contains(
    query,
    "query CandidateIssues($taskFilter: IssueFilter!, $dispatchStates: [String!], $after: String)",
  )
  assert string.contains(
    query,
    "issues(first: 50, after: $after, filter: { and: [$taskFilter], state: { name: { in: $dispatchStates } } })",
  )
}

pub fn composed_task_scope_uses_same_issue_filter_for_ownership_requests_test() {
  let assert Ok(candidate_request) =
    linear.build_candidate_request(
      composed_project_tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      None,
    )
  let assert Ok(list_request) =
    task_query.build_list_request(
      composed_project_tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      None,
    )
  let assert Ok(detail_request) =
    task_query.build_detail_by_id_request(
      composed_project_tracker_config(),
      "issue-id",
    )
  let assert Ok(identifier_request) =
    task_query.build_detail_by_identifier_request(
      composed_project_tracker_config(),
      "BUG-1",
    )

  assert json_variable(candidate_request.body, "taskFilter")
    == composed_project_task_filter_json()
  assert json_variable(list_request.body, "taskFilter")
    == composed_project_task_filter_json()
  assert json_variable(detail_request.body, "taskFilter")
    == composed_project_task_filter_json()
  assert json_variable(identifier_request.body, "taskFilter")
    == composed_project_task_filter_json()
  assert linear_task_scope.matches_project_slug(
    composed_project_scope(),
    "PROJ",
  )
  assert linear_task_scope.matches_project_slug(
    composed_project_scope(),
    "BUGS",
  )
  assert !linear_task_scope.matches_project_slug(
    composed_project_scope(),
    "OPS",
  )
}

pub fn labelled_task_scope_uses_same_issue_filter_for_ownership_requests_test() {
  let assert Ok(candidate_request) =
    linear.build_candidate_request(
      labelled_tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      None,
    )
  let assert Ok(list_request) =
    task_query.build_list_request(
      labelled_tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      None,
    )
  let assert Ok(detail_request) =
    task_query.build_detail_by_id_request(labelled_tracker_config(), "issue-id")
  let assert Ok(identifier_request) =
    task_query.build_detail_by_identifier_request(
      labelled_tracker_config(),
      "PROJ-1",
    )

  assert json_variable(candidate_request.body, "taskFilter")
    == labelled_task_filter_json()
  assert json_variable(list_request.body, "taskFilter")
    == labelled_task_filter_json()
  assert json_variable(detail_request.body, "taskFilter")
    == labelled_task_filter_json()
  assert json_variable(identifier_request.body, "taskFilter")
    == labelled_task_filter_json()
  assert linear_task_scope.matches_issue(labelled_task_scope(), "PROJ", [
    "workflow:implementation",
    "backend",
    "urgent",
  ])
  assert !linear_task_scope.matches_issue(labelled_task_scope(), "PROJ", [
    "workflow:implementation",
    "urgent",
  ])
}

pub fn state_refresh_query_uses_graphql_id_list_test() {
  let assert Ok(request) =
    linear.build_state_refresh_request(tracker_config(), ["id1", "id2"])
  let query = request_query(request.body)
  assert variable_names(request.body) == ["ids"]
  assert string_list_variable(request.body, "ids") == ["id1", "id2"]
  assert string.contains(query, "query IssueStates($ids: [ID!]!)")
  assert string.contains(query, "issues(filter: { id: { in: $ids } })")
  assert request.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]
}

pub fn normalizes_linear_payload_test() {
  let response =
    linear.Response(
      status: 200,
      body: response_page("ABC-123", "false", "null"),
    )
  let assert Ok(page) = linear.parse_page_response(response)
  let assert [issue] = page.nodes
  assert issue.identifier == "ABC-123"
  assert issue.labels == ["bug"]
  assert issue.blocked_by_complete == True
  let assert [blocker] = issue.blocked_by
  assert blocker.identifier == Some("B-1")
  assert issue.created_at != None
  assert page.has_next_page == False
}

pub fn blocked_issue_fixture_decodes_incoming_inverse_relation_test() {
  let assert Ok(body) =
    simplifile.read(
      "test/fixtures/linear/blocked_issue_candidate_response.json",
    )
  let assert Ok(page) =
    linear.parse_page_response(linear.Response(status: 200, body: body))
  let assert [issue] = page.nodes
  assert issue.identifier == "A-1"
  assert issue.blocked_by_complete == True
  let assert [blocker] = issue.blocked_by
  assert blocker.id == Some("B-id")
  assert blocker.identifier == Some("B-1")
  assert blocker.state == Some(issue_state.from_string_unchecked("In Progress"))
}

pub fn outgoing_blocks_relation_does_not_decode_as_blocker_test() {
  let response =
    linear.Response(
      status: 200,
      body: "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"ABC-123-id\",\"identifier\":\"ABC-123\",\"title\":\"Title ABC-123\",\"state\":{\"name\":\"Todo\"},\"relations\":{\"nodes\":[{\"type\":\"blocks\",\"relatedIssue\":{\"id\":\"B-id\",\"identifier\":\"B-1\",\"state\":{\"name\":\"Backlog\"}}}]},\"inverseRelations\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}",
    )
  let assert Ok(page) = linear.parse_page_response(response)
  let assert [issue] = page.nodes
  assert issue.blocked_by == []
}

pub fn truncated_inverse_relation_page_decodes_incomplete_test() {
  let body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"ABC-123-id\",\"identifier\":\"ABC-123\",\"title\":\"Title ABC-123\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[]},\"inverseRelations\":{\"nodes\":[{\"type\":\"blocks\",\"issue\":{\"id\":\"B-id\",\"identifier\":\"B-1\",\"state\":{\"name\":\"Done\"}}}],\"pageInfo\":{\"hasNextPage\":true,\"endCursor\":\"cursor\"}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
  let assert Ok(page) =
    linear.parse_page_response(linear.Response(status: 200, body: body))
  let assert [issue] = page.nodes
  assert issue.blocked_by_complete == False
}

pub fn missing_inverse_relations_is_rejected_test() {
  let body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"ABC-123-id\",\"identifier\":\"ABC-123\",\"title\":\"Title ABC-123\",\"state\":{\"name\":\"Todo\"}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_page_response(linear.Response(status: 200, body: body))
}

pub fn fetch_candidate_issues_uses_dispatch_states_test() {
  let captured = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(captured, request.body)
    Ok(linear.Response(
      status: 200,
      body: response_page("ABC-1", "false", "null"),
    ))
  }

  let assert Ok(_) = linear.fetch_candidate_issues(tracker_config(), transport)
  let assert Ok(body) = process.receive(captured, within: 1000)
  assert string_list_variable(body, "dispatchStates") == ["Todo"]
}

pub fn fetch_candidate_issues_uses_canonical_dispatch_state_names_test() {
  let captured = process.new_subject()
  let assert Ok([document]) =
    yay.parse_string(
      "tracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: PROJ\n  states:\n    active: [Todo]\n    ready: [\" todo \"]\n",
    )
  let assert Ok(effective) =
    config.resolve_with_env(
      yay.document_root(document),
      "test/tmp/scherzo.yaml",
      fn(name) {
        case name {
          "LINEAR_API_KEY" -> Some("secret-key")
          _ -> None
        }
      },
    )
  let transport = fn(request: linear.Request) {
    process.send(captured, request.body)
    Ok(linear.Response(
      status: 200,
      body: response_page("ABC-1", "false", "null"),
    ))
  }

  let assert Ok(_) = linear.fetch_candidate_issues(effective.tracker, transport)
  let assert Ok(body) = process.receive(captured, within: 1000)
  assert string_list_variable(body, "dispatchStates") == ["Todo"]
}

pub fn pagination_preserves_order_test() {
  let transport = fn(request: linear.Request) {
    case string.contains(request.body, "\"after\":null") {
      True ->
        Ok(linear.Response(
          status: 200,
          body: response_page("ABC-1", "true", "\"cursor-1\""),
        ))
      False ->
        Ok(linear.Response(
          status: 200,
          body: response_page("ABC-2", "false", "null"),
        ))
    }
  }

  let assert Ok(issues) =
    linear.fetch_candidate_issues(tracker_config(), transport)
  assert list_identifiers(issues) == ["ABC-1", "ABC-2"]
}

pub fn empty_state_fetch_does_not_call_transport_test() {
  let transport = fn(_request: linear.Request) {
    Error(error.LinearApiRequest("should not be called"))
  }
  let assert Ok([]) =
    linear.fetch_issues_by_states(tracker_config(), [], transport)
}

pub fn response_errors_are_mapped_test() {
  let assert Error(error.LinearApiStatus(500)) =
    linear.parse_page_response(linear.Response(status: 500, body: "{}"))
  let body = "{\"errors\":[{\"message\":\"bad query\"}],\"data\":null}"
  let assert Error(error.LinearGraphqlErrors(_)) =
    linear.parse_page_response(linear.Response(status: 200, body: body))
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_page_response(linear.Response(
      status: 200,
      body: "{\"data\":{}}",
    ))
}

pub fn mutation_request_builders_and_response_parsing_test() {
  let assert Ok(comment) =
    linear.build_comment_create_request(tracker_config(), "issue-id", "hello")
  let comment_query = request_query(comment.body)
  assert variable_names(comment.body) == ["body", "issueId"]
  assert string_variable(comment.body, "issueId") == "issue-id"
  assert string_variable(comment.body, "body") == "hello"
  assert string.contains(
    comment_query,
    "mutation ScherzoCommentCreate($issueId: String!, $body: String!)",
  )
  assert string.contains(
    comment_query,
    "commentCreate(input: { issueId: $issueId, body: $body })",
  )
  assert comment.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]

  let assert Ok(update) =
    linear.build_issue_update_state_request(
      tracker_config(),
      "issue-id",
      "state-id",
    )
  let update_query = request_query(update.body)
  assert variable_names(update.body) == ["issueId", "stateId"]
  assert string_variable(update.body, "issueId") == "issue-id"
  assert string_variable(update.body, "stateId") == "state-id"
  assert string.contains(
    update_query,
    "mutation ScherzoIssueUpdateState($issueId: String!, $stateId: String!)",
  )
  assert string.contains(
    update_query,
    "issueUpdate(id: $issueId, input: { stateId: $stateId })",
  )
  assert update.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]

  let assert Ok(Nil) =
    linear.parse_mutation_response(
      linear.Response(
        status: 200,
        body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
      ),
      "commentCreate",
    )
  let assert Error(error.LinearGraphqlErrors(_)) =
    linear.parse_mutation_response(
      linear.Response(
        status: 200,
        body: "{\"errors\":[{\"message\":\"bad mutation\"}],\"data\":null}",
      ),
      "commentCreate",
    )
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_mutation_response(
      linear.Response(status: 200, body: "{\"data\":{}}"),
      "commentCreate",
    )
}

pub fn issue_comment_marker_lookup_finds_existing_scherzo_outbox_comment_test() {
  let marker = "claim:linear:issue-id:run-1"
  let body =
    "{\"data\":{\"issue\":{\"comments\":{\"nodes\":[{\"id\":\"comment-1\",\"body\":\"ordinary comment\",\"bodyData\":\"{\\\"type\\\":\\\"doc\\\",\\\"content\\\":[]}\"},{\"id\":\"comment-2\",\"body\":\"claimed\\n\\n<!-- scherzo:outbox:claim:linear:issue-id:run-1 -->\",\"bodyData\":\"{\\\"type\\\":\\\"doc\\\",\\\"content\\\":[]}\"}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}}"
  let captured = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(captured, request.body)
    Ok(linear.Response(status: 200, body: body))
  }

  let assert Ok(Some(comment)) =
    linear.find_issue_comment_by_marker(
      tracker_config(),
      transport,
      "issue-id",
      marker,
    )
  let assert Ok(request_body) = process.receive(captured, within: 100)
  assert string.contains(request_query(request_body), "ScherzoIssueComments")
  assert string_variable(request_body, "issueId") == "issue-id"
  assert comment.id == "comment-2"
  assert string.contains(comment.body, "scherzo:outbox:" <> marker)
}

pub fn missing_end_cursor_is_error_test() {
  let transport = fn(_request: linear.Request) {
    Ok(linear.Response(
      status: 200,
      body: response_page("ABC-1", "true", "null"),
    ))
  }
  let assert Error(error.LinearMissingEndCursor) =
    linear.fetch_candidate_issues(tracker_config(), transport)
}

pub fn task_source_requests_use_multi_project_task_filters_test() {
  let assert Ok(list_request) =
    task_query.build_list_request(
      multi_project_tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      None,
    )
  let list_query = request_query(list_request.body)
  assert variable_names(list_request.body)
    == ["after", "stateNames", "taskFilter"]
  assert json_variable(list_request.body, "taskFilter")
    == "{\"project\":{\"slugId\":{\"in\":[\"PROJ\",\"BUGS\"]}}}"
  assert string.contains(
    list_query,
    "query ScherzoTaskList($taskFilter: IssueFilter!, $stateNames: [String!], $after: String)",
  )
  assert string.contains(
    list_query,
    "filter: { and: [$taskFilter], state: { name: { in: $stateNames } } }",
  )

  let assert Ok(detail_request) =
    task_query.build_detail_by_id_request(
      multi_project_tracker_config(),
      "issue-id",
    )
  let detail_query = request_query(detail_request.body)
  assert variable_names(detail_request.body) == ["ids", "taskFilter"]
  assert json_variable(detail_request.body, "taskFilter")
    == "{\"project\":{\"slugId\":{\"in\":[\"PROJ\",\"BUGS\"]}}}"
  assert string.contains(
    detail_query,
    "query ScherzoTaskDetailById($taskFilter: IssueFilter!, $ids: [ID!]!)",
  )
  assert string.contains(
    detail_query,
    "filter: { and: [$taskFilter], id: { in: $ids } }",
  )

  let assert Ok(identifier_request) =
    task_query.build_detail_by_identifier_request(
      multi_project_tracker_config(),
      "BUG-1",
    )
  let identifier_query = request_query(identifier_request.body)
  assert variable_names(identifier_request.body)
    == ["issueIdentifier", "issueRemoteId", "taskFilter"]
  assert json_variable(identifier_request.body, "taskFilter")
    == "{\"project\":{\"slugId\":{\"in\":[\"PROJ\",\"BUGS\"]}}}"
  assert string_variable(identifier_request.body, "issueRemoteId") == "BUG-1"
  assert string_variable(identifier_request.body, "issueIdentifier") == "BUG-1"
  assert string.contains(
    identifier_query,
    "query ScherzoTaskDetailByIdentifier($taskFilter: IssueFilter!, $issueRemoteId: ID!, $issueIdentifier: String!)",
  )
  assert string.contains(
    identifier_query,
    "filter: { and: [$taskFilter], or: [{ id: { eq: $issueRemoteId } }, { identifier: { eq: $issueIdentifier } }] }",
  )
}

pub fn task_source_detail_identifier_filters_multi_project_scope_test() {
  let scope = config_types.LinearTaskProjects(["PROJ", "BUGS"])
  let assert Ok(Some(found)) =
    task_query.parse_detail_by_identifier_response(
      linear.Response(
        status: 200,
        body: task_detail_by_identifier_response("BUG-1", "BUGS"),
      ),
      scope,
      "BUG-1",
    )
  assert found.ref.key == Some("BUG-1")

  let assert Ok(None) =
    task_query.parse_detail_by_identifier_response(
      linear.Response(
        status: 200,
        body: task_detail_by_identifier_response("BUG-1", "OTHER"),
      ),
      scope,
      "BUG-1",
    )
}

pub fn task_source_detail_identifier_decodes_labelled_scope_legacy_response_test() {
  let matching_labels =
    "[{\"id\":\"implementation\",\"name\":\"workflow:implementation\"},{\"id\":\"backend\",\"name\":\"backend\"},{\"id\":\"urgent\",\"name\":\"urgent\"}]"
  let missing_required_label =
    "[{\"id\":\"implementation\",\"name\":\"workflow:implementation\"},{\"id\":\"urgent\",\"name\":\"urgent\"}]"

  let assert Ok(Some(found)) =
    task_query.parse_detail_by_identifier_response(
      linear.Response(
        status: 200,
        body: task_detail_by_identifier_response_with_labels(
          "PROJ-1",
          "PROJ",
          matching_labels,
        ),
      ),
      labelled_task_scope(),
      "PROJ-1",
    )
  assert found.ref.key == Some("PROJ-1")

  let assert Ok(None) =
    task_query.parse_detail_by_identifier_response(
      linear.Response(
        status: 200,
        body: task_detail_by_identifier_response_with_labels(
          "PROJ-1",
          "PROJ",
          missing_required_label,
        ),
      ),
      labelled_task_scope(),
      "PROJ-1",
    )
}

pub fn task_source_detail_identifier_decodes_composed_scope_responses_test() {
  let scope = composed_project_scope()
  let assert Ok(Some(found)) =
    task_query.parse_detail_by_identifier_response(
      linear.Response(
        status: 200,
        body: task_detail_by_identifier_connection_response("BUG-1"),
      ),
      scope,
      "BUG-1",
    )
  assert found.ref.key == Some("BUG-1")

  let assert Ok(None) =
    task_query.parse_detail_by_identifier_response(
      linear.Response(
        status: 200,
        body: task_detail_by_identifier_response("OPS-1", "OPS"),
      ),
      scope,
      "OPS-1",
    )
}

pub fn contract_request_uses_project_slug_and_read_only_query_test() {
  let assert Ok(request) = linear.build_contract_request(tracker_config())
  let query = request_query(request.body)
  assert variable_names(request.body)
    == ["configuredProjectSlugs", "projectFilter"]
  assert json_variable(request.body, "projectFilter")
    == "{\"slugId\":{\"eq\":\"PROJ\"}}"
  assert string_list_variable(request.body, "configuredProjectSlugs")
    == ["PROJ"]
  assert string.contains(
    query,
    "query ScherzoLinearContract($projectFilter: ProjectFilter!, $configuredProjectSlugs: [String!]!)",
  )
  assert string.contains(query, "projects(first: 2, filter: $projectFilter)")
  assert string.contains(
    query,
    "configuredProjects: projects(first: 1, filter: { slugId: { in: $configuredProjectSlugs } })",
  )
  assert string.contains(
    query,
    "teams(first: 10) { nodes { id key name states(first: 50)",
  )
  assert string.contains(
    query,
    "labels(first: 140) { nodes { id name } pageInfo { hasNextPage endCursor } }",
  )
  assert string.contains(
    query,
    "issueLabels(first: 100, filter: { team: { null: true } })",
  )
  assert !string.contains(query, "mutation")
  assert request.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]
}

pub fn contract_request_uses_multi_project_scope_filter_test() {
  let assert Ok(request) =
    linear.build_contract_request(multi_project_tracker_config())
  let query = request_query(request.body)
  assert variable_names(request.body)
    == ["configuredProjectSlugs", "projectFilter"]
  assert json_variable(request.body, "projectFilter")
    == "{\"slugId\":{\"in\":[\"PROJ\",\"BUGS\"]}}"
  assert string_list_variable(request.body, "configuredProjectSlugs")
    == ["PROJ", "BUGS"]
  assert string.contains(
    query,
    "query ScherzoLinearContract($projectFilter: ProjectFilter!, $configuredProjectSlugs: [String!]!)",
  )
  assert string.contains(query, "projects(first: 2, filter: $projectFilter)")
  assert string.contains(
    query,
    "configuredProjects: projects(first: 2, filter: { slugId: { in: $configuredProjectSlugs } })",
  )
}

pub fn contract_request_uses_composed_scope_project_filter_test() {
  let assert Ok(request) =
    linear.build_contract_request(composed_project_tracker_config())
  let query = request_query(request.body)
  assert variable_names(request.body)
    == ["configuredProjectSlugs", "projectFilter"]
  assert json_variable(request.body, "projectFilter")
    == composed_project_filter_json()
  assert string_list_variable(request.body, "configuredProjectSlugs")
    == ["PROJ", "BUGS", "OPS"]
  assert string.contains(
    query,
    "query ScherzoLinearContract($projectFilter: ProjectFilter!, $configuredProjectSlugs: [String!]!)",
  )
  assert string.contains(query, "projects(first: 2, filter: $projectFilter)")
  assert string.contains(
    query,
    "configuredProjects: projects(first: 3, filter: { slugId: { in: $configuredProjectSlugs } })",
  )
}

pub fn contract_request_uses_labelled_scope_project_filter_test() {
  let assert Ok(request) =
    linear.build_contract_request(labelled_tracker_config())
  let query = request_query(request.body)
  assert variable_names(request.body)
    == ["configuredProjectSlugs", "projectFilter"]
  assert json_variable(request.body, "projectFilter")
    == labelled_project_filter_json()
  assert string_list_variable(request.body, "configuredProjectSlugs")
    == ["PROJ"]
  assert string.contains(query, "projects(first: 1, filter: $projectFilter)")
  assert string.contains(
    query,
    "configuredProjects: projects(first: 1, filter: { slugId: { in: $configuredProjectSlugs } })",
  )
}

pub fn contract_client_uses_labelled_scope_effective_projects_test() {
  let effective_projects =
    "["
    <> contract_project_for(
      "PROJ",
      "[" <> contract_team("PROJ", "false", "false") <> "]",
      "false",
    )
    <> "]"
  let client =
    linear.contract_client(labelled_tracker_config(), fn(request) {
      assert json_variable(request.body, "projectFilter")
        == labelled_project_filter_json()
      assert string_list_variable(request.body, "configuredProjectSlugs")
        == ["PROJ"]
      Ok(linear.Response(
        status: 200,
        body: contract_response_with_configured_projects(
          effective_projects,
          effective_projects,
          "false",
        ),
      ))
    })

  let assert Ok(board) = client.fetch_remote_contract()
  assert board.project_slug == "PROJ"
  assert list.map(board.teams, fn(team) { team.key }) == ["PROJ"]
}

pub fn contract_client_validates_missing_configured_slug_for_labelled_scope_test() {
  let effective_projects =
    "["
    <> contract_project_for(
      "PROJ",
      "[" <> contract_team("PROJ", "false", "false") <> "]",
      "false",
    )
    <> "]"
  let client =
    linear.contract_client(labelled_tracker_config(), fn(_) {
      Ok(linear.Response(
        status: 200,
        body: contract_response_with_configured_projects(
          effective_projects,
          "[]",
          "false",
        ),
      ))
    })

  let assert Error(error.LinearUnknownPayload(message)) =
    client.fetch_remote_contract()
  assert string.contains(message, "project slug(s) not found: PROJ")
}

pub fn contract_client_validates_all_configured_project_slugs_test() {
  let client =
    linear.contract_client(multi_project_tracker_config(), fn(_) {
      Ok(linear.Response(
        status: 200,
        body: contract_response(
          "["
            <> contract_project(
            "[" <> contract_team("ENG", "false", "false") <> "]",
            "false",
          )
            <> "]",
          "false",
        ),
      ))
    })

  let assert Error(error.LinearUnknownPayload(message)) =
    client.fetch_remote_contract()
  assert string.contains(message, "project slug(s) not found: BUGS")
}

pub fn contract_client_merges_multi_project_contract_without_synthetic_project_id_test() {
  let shared_team = "[" <> contract_team("ENG", "false", "false") <> "]"
  let client =
    linear.contract_client(multi_project_tracker_config(), fn(_) {
      Ok(linear.Response(
        status: 200,
        body: contract_response(
          "["
            <> contract_project_for("BUGS", shared_team, "false")
            <> ","
            <> contract_project_for("PROJ", shared_team, "false")
            <> "]",
          "false",
        ),
      ))
    })

  let assert Ok(board) = client.fetch_remote_contract()
  assert board.project_id == "project-PROJ"
  assert board.project_slug == "PROJ,BUGS"
  let assert [team] = board.teams
  assert team.id == "team-ENG"
}

pub fn contract_client_uses_composed_scope_effective_projects_test() {
  let effective_projects =
    "["
    <> contract_project_for(
      "PROJ",
      "[" <> contract_team("PROJ", "false", "false") <> "]",
      "false",
    )
    <> ","
    <> contract_project_for(
      "BUGS",
      "[" <> contract_team("BUGS", "false", "false") <> "]",
      "false",
    )
    <> "]"
  let configured_projects =
    "["
    <> contract_project_for("PROJ", "[]", "false")
    <> ","
    <> contract_project_for("BUGS", "[]", "false")
    <> ","
    <> contract_project_for("OPS", "[]", "false")
    <> "]"
  let client =
    linear.contract_client(composed_project_tracker_config(), fn(request) {
      assert json_variable(request.body, "projectFilter")
        == composed_project_filter_json()
      assert string_list_variable(request.body, "configuredProjectSlugs")
        == ["PROJ", "BUGS", "OPS"]
      Ok(linear.Response(
        status: 200,
        body: contract_response_with_configured_projects(
          effective_projects,
          configured_projects,
          "false",
        ),
      ))
    })

  let assert Ok(board) = client.fetch_remote_contract()
  assert board.project_slug == "PROJ,BUGS"
  assert list.map(board.teams, fn(team) { team.key }) == ["PROJ", "BUGS"]
}

pub fn contract_client_validates_missing_configured_slug_for_composed_scope_test() {
  let effective_projects =
    "["
    <> contract_project_for(
      "PROJ",
      "[" <> contract_team("PROJ", "false", "false") <> "]",
      "false",
    )
    <> ","
    <> contract_project_for(
      "BUGS",
      "[" <> contract_team("BUGS", "false", "false") <> "]",
      "false",
    )
    <> "]"
  let configured_projects =
    "["
    <> contract_project_for("PROJ", "[]", "false")
    <> ","
    <> contract_project_for("BUGS", "[]", "false")
    <> "]"
  let client =
    linear.contract_client(composed_project_tracker_config(), fn(_) {
      Ok(linear.Response(
        status: 200,
        body: contract_response_with_configured_projects(
          effective_projects,
          configured_projects,
          "false",
        ),
      ))
    })

  let assert Error(error.LinearUnknownPayload(message)) =
    client.fetch_remote_contract()
  assert string.contains(message, "project slug(s) not found: OPS")
}

pub fn contract_response_decodes_project_teams_and_workspace_labels_test() {
  let response =
    linear.Response(
      status: 200,
      body: contract_response(
        "["
          <> contract_project(
          "["
            <> contract_team("ENG", "false", "false")
            <> ","
            <> contract_team("OPS", "false", "false")
            <> "]",
          "false",
        )
          <> "]",
        "false",
      ),
    )
  let assert Ok(board) = linear.parse_contract_response(response)
  assert board.project_id == "project-PROJ"
  assert board.project_slug == "PROJ"
  let assert [eng, ops] = board.teams
  assert eng.key == "ENG"
  assert ops.key == "OPS"
  let assert [ready, _, _] = eng.states
  assert ready.id == "state-ready-ENG"
  let assert [workflow_label] = eng.labels
  assert workflow_label.name == "workflow:bugfix"
  let assert [workspace_label] = board.workspace_labels
  assert workspace_label.name == "workflow:research"
}

pub fn contract_client_fetches_remote_contract_through_transport_test() {
  let client =
    linear.contract_client(tracker_config(), fn(request) {
      assert string.contains(request.body, "ScherzoLinearContract")
      Ok(linear.Response(
        status: 200,
        body: contract_response(
          "["
            <> contract_project(
            "[" <> contract_team("ENG", "false", "false") <> "]",
            "false",
          )
            <> "]",
          "false",
        ),
      ))
    })
  let assert Ok(board) = client.fetch_remote_contract()
  assert board.project_slug == "PROJ"
}

pub fn contract_response_rejects_unknown_or_ambiguous_projects_test() {
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response("[]", "false"),
    ))

  let project =
    contract_project(
      "[" <> contract_team("ENG", "false", "false") <> "]",
      "false",
    )
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response("[" <> project <> "," <> project <> "]", "false"),
    ))
}

pub fn contract_response_rejects_no_teams_and_paginated_metadata_test() {
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response(
        "[" <> contract_project("[]", "false") <> "]",
        "false",
      ),
    ))

  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response(
        "["
          <> contract_project(
          "[" <> contract_team("ENG", "false", "false") <> "]",
          "true",
        )
          <> "]",
        "false",
      ),
    ))

  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response(
        "["
          <> contract_project(
          "[" <> contract_team("ENG", "true", "false") <> "]",
          "false",
        )
          <> "]",
        "false",
      ),
    ))

  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response(
        "["
          <> contract_project(
          "[" <> contract_team("ENG", "false", "true") <> "]",
          "false",
        )
          <> "]",
        "false",
      ),
    ))

  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: contract_response(
        "["
          <> contract_project(
          "[" <> contract_team("ENG", "false", "false") <> "]",
          "false",
        )
          <> "]",
        "true",
      ),
    ))
}

pub fn contract_response_maps_graphql_and_http_errors_test() {
  let assert Error(error.LinearApiStatus(500)) =
    linear.parse_contract_response(linear.Response(status: 500, body: "{}"))
  let body = "{\"errors\":[{\"message\":\"bad contract query\"}],\"data\":null}"
  let assert Error(error.LinearGraphqlErrors(_)) =
    linear.parse_contract_response(linear.Response(status: 200, body: body))
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_contract_response(linear.Response(
      status: 200,
      body: "{\"data\":{}}",
    ))
}

fn task_detail_by_identifier_response(
  identifier: String,
  project_slug: String,
) -> String {
  task_detail_by_identifier_response_with_labels(
    identifier,
    project_slug,
    "[{\"id\":\"label-id\",\"name\":\"workflow:bug\"}]",
  )
}

fn task_detail_by_identifier_response_with_labels(
  identifier: String,
  project_slug: String,
  label_nodes: String,
) -> String {
  "{\"data\":{\"issue\":{\"id\":\"issue-id\",\"identifier\":\""
  <> identifier
  <> "\",\"title\":\"Task Detail\",\"description\":\"Desc\",\"priority\":1,\"branchName\":\"branch\",\"url\":\"https://linear/issue\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"project\":{\"slugId\":\""
  <> project_slug
  <> "\"},\"state\":{\"id\":\"state-id\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":"
  <> label_nodes
  <> "}}}}"
}

fn task_detail_by_identifier_connection_response(identifier: String) -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-id\",\"identifier\":\""
  <> identifier
  <> "\",\"title\":\"Task Detail\",\"description\":\"Desc\",\"priority\":1,\"branchName\":\"branch\",\"url\":\"https://linear/issue\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"id\":\"state-id\",\"name\":\"Todo\",\"type\":\"unstarted\"},\"labels\":{\"nodes\":[{\"id\":\"label-id\",\"name\":\"workflow:bug\"}]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn contract_response(projects: String, workspace_has_next: String) -> String {
  contract_response_with_configured_projects(
    projects,
    projects,
    workspace_has_next,
  )
}

fn contract_response_with_configured_projects(
  projects: String,
  configured_projects: String,
  workspace_has_next: String,
) -> String {
  "{\"data\":{\"projects\":{\"nodes\":"
  <> projects
  <> "},\"configuredProjects\":{\"nodes\":"
  <> configured_projects
  <> "},\"issueLabels\":{\"nodes\":[{\"id\":\"workspace-research\",\"name\":\"workflow:research\"}],\"pageInfo\":"
  <> page_info(workspace_has_next)
  <> "}}}"
}

fn contract_project(teams: String, teams_has_next: String) -> String {
  contract_project_for("PROJ", teams, teams_has_next)
}

fn contract_project_for(
  slug: String,
  teams: String,
  teams_has_next: String,
) -> String {
  "{\"id\":\"project-"
  <> slug
  <> "\",\"name\":\"Project "
  <> slug
  <> "\",\"slugId\":\""
  <> slug
  <> "\",\"teams\":{\"nodes\":"
  <> teams
  <> ",\"pageInfo\":"
  <> page_info(teams_has_next)
  <> "}}"
}

fn contract_team(
  key: String,
  states_has_next: String,
  labels_has_next: String,
) -> String {
  "{\"id\":\"team-"
  <> key
  <> "\",\"key\":\""
  <> key
  <> "\",\"name\":\""
  <> key
  <> " Team\",\"states\":{\"nodes\":[{\"id\":\"state-ready-"
  <> key
  <> "\",\"name\":\"Ready for Agent\",\"type\":\"unstarted\"},{\"id\":\"state-progress-"
  <> key
  <> "\",\"name\":\"In Progress\",\"type\":\"started\"},{\"id\":\"state-done-"
  <> key
  <> "\",\"name\":\"Done\",\"type\":\"completed\"}],\"pageInfo\":"
  <> page_info(states_has_next)
  <> "},\"labels\":{\"nodes\":[{\"id\":\"label-bugfix-"
  <> key
  <> "\",\"name\":\"workflow:bugfix\"}],\"pageInfo\":"
  <> page_info(labels_has_next)
  <> "}}"
}

fn page_info(has_next: String) -> String {
  "{\"hasNextPage\":" <> has_next <> ",\"endCursor\":null}"
}

fn list_identifiers(issues: List(tracker_issue.Issue)) -> List(String) {
  case issues {
    [] -> []
    [issue, ..rest] -> [issue.identifier, ..list_identifiers(rest)]
  }
}
