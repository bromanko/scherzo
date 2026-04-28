import gleam/option.{None, Some}
import gleam/string
import scherzo/domain
import scherzo/error
import scherzo/linear

fn tracker_config() -> domain.TrackerConfig {
  domain.TrackerConfig(
    kind: "linear",
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: ["Todo", "In Progress"],
    terminal_states: ["Done"],
  )
}

fn response_page(identifier: String, has_next: String, cursor: String) -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\""
  <> identifier
  <> "-id\",\"identifier\":\""
  <> identifier
  <> "\",\"title\":\"Title "
  <> identifier
  <> "\",\"description\":\"Desc\",\"priority\":1,\"branchName\":\"branch\",\"url\":\"https://linear/"
  <> identifier
  <> "\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T11:00:00Z\",\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[{\"name\":\"Bug\"}]},\"relations\":{\"nodes\":[{\"type\":\"blocks\",\"relatedIssue\":{\"id\":\"B-id\",\"identifier\":\"B-1\",\"state\":{\"name\":\"Done\"}}}]}}],\"pageInfo\":{\"hasNextPage\":"
  <> has_next
  <> ",\"endCursor\":"
  <> cursor
  <> "}}}}"
}

pub fn candidate_query_uses_project_slug_filter_test() {
  let assert Error(error.LinearApiRequest(_)) =
    linear.build_candidate_request(
      domain.TrackerConfig(..tracker_config(), endpoint: "http://linear.test"),
      ["Todo"],
      Some("cursor"),
    )
  let assert Ok(request) =
    linear.build_candidate_request(tracker_config(), ["Todo"], Some("cursor"))
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

fn list_identifiers(issues: List(domain.Issue)) -> List(String) {
  case issues {
    [] -> []
    [issue, ..rest] -> [issue.identifier, ..list_identifiers(rest)]
  }
}
