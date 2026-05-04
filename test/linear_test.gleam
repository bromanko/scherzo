import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
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
  <> "\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[{\"name\":\"Bug\"}]},\"inverseRelations\":{\"nodes\":[{\"type\":\"blocks\",\"issue\":{\"id\":\"B-id\",\"identifier\":\"B-1\",\"state\":{\"name\":\"Done\"}}}]}}],\"pageInfo\":{\"hasNextPage\":"
  <> has_next
  <> ",\"endCursor\":"
  <> cursor
  <> "}}}}"
}

pub fn candidate_query_uses_project_slug_filter_test() {
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
  assert string.contains(request.body, "slugId")
  assert string.contains(request.body, "projectSlug")
  assert string.contains(request.body, "activeStates")
  assert string.contains(request.body, "cursor")
  assert request.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]
}

pub fn state_refresh_query_uses_graphql_id_list_test() {
  let assert Ok(request) =
    linear.build_state_refresh_request(tracker_config(), ["id1", "id2"])
  assert string.contains(request.body, "[ID!]!")
  assert string.contains(request.body, "id1")
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
  let assert [blocker] = issue.blocked_by
  assert blocker.identifier == Some("B-1")
  assert issue.created_at != None
  assert page.has_next_page == False
}

pub fn outgoing_blocks_relation_does_not_decode_as_blocker_test() {
  let response =
    linear.Response(
      status: 200,
      body: "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"ABC-123-id\",\"identifier\":\"ABC-123\",\"title\":\"Title ABC-123\",\"state\":{\"name\":\"Todo\"},\"relations\":{\"nodes\":[{\"type\":\"blocks\",\"relatedIssue\":{\"id\":\"B-id\",\"identifier\":\"B-1\",\"state\":{\"name\":\"Backlog\"}}}]},\"inverseRelations\":{\"nodes\":[]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}",
    )
  let assert Ok(page) = linear.parse_page_response(response)
  let assert [issue] = page.nodes
  assert issue.blocked_by == []
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
  assert string.contains(comment.body, "ScherzoCommentCreate")
  assert string.contains(comment.body, "issue-id")
  assert string.contains(comment.body, "hello")
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
  assert string.contains(update.body, "ScherzoIssueUpdateState")
  assert string.contains(update.body, "state-id")
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

pub fn contract_request_uses_project_slug_and_read_only_query_test() {
  let assert Ok(request) = linear.build_contract_request(tracker_config())
  assert string.contains(request.body, "ScherzoLinearContract")
  assert string.contains(request.body, "projects(first: 2")
  assert string.contains(request.body, "teams(first: 10")
  assert string.contains(request.body, "states(first: 50")
  assert string.contains(request.body, "labels(first: 100")
  assert string.contains(request.body, "issueLabels(first: 100")
  assert string.contains(request.body, "projectSlug")
  assert string.contains(request.body, "PROJ")
  assert !string.contains(request.body, "mutation")
  assert request.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]
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
  assert board.project_id == "project-id"
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

fn contract_response(projects: String, workspace_has_next: String) -> String {
  "{\"data\":{\"projects\":{\"nodes\":"
  <> projects
  <> "},\"issueLabels\":{\"nodes\":[{\"id\":\"workspace-research\",\"name\":\"workflow:research\"}],\"pageInfo\":"
  <> page_info(workspace_has_next)
  <> "}}}"
}

fn contract_project(teams: String, teams_has_next: String) -> String {
  "{\"id\":\"project-id\",\"name\":\"Project\",\"slugId\":\"PROJ\",\"teams\":{\"nodes\":"
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
