import gleam/option.{Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: issue_state.list_from_strings(["Todo"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn comments_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-1\",\"comments\":{\"nodes\":[{\"id\":\"c2\",\"body\":\"/scherzo abort\",\"createdAt\":\"2026-04-28T10:00:01Z\",\"updatedAt\":\"2026-04-28T10:00:02Z\",\"user\":{\"id\":\"user-1\",\"email\":\"one@example.com\",\"name\":\"One\"}},{\"id\":\"c1\",\"body\":\"/scherzo retry\",\"createdAt\":\"2026-04-28T10:00:00Z\",\"updatedAt\":\"2026-04-28T10:00:00Z\",\"user\":{\"id\":\"user-2\",\"email\":null,\"name\":null}}]}}]}}}"
}

pub fn issue_comments_request_is_bounded_and_authenticated_test() {
  let assert Ok(request) =
    linear.build_issue_comments_request(
      tracker_config(),
      ["issue-1", "issue-2"],
      7,
    )
  assert string.contains(request.body, "IssueComments")
  assert string.contains(request.body, "issue-1")
  assert string.contains(request.body, "issue-2")
  assert string.contains(request.body, "\"first\":7")
  assert request.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]
}

pub fn parses_linear_comment_response_with_author_and_timestamps_test() {
  let assert Ok(comments) =
    linear.parse_comments_response(linear.Response(200, comments_response()))
  let assert [first, second] = comments
  assert first.id == "c2"
  assert first.issue_id == "issue-1"
  assert first.body == "/scherzo abort"
  assert first.created_at_ms > 0
  assert first.updated_at_ms >= first.created_at_ms
  assert first.author.id == "user-1"
  assert first.author.email == Some("one@example.com")
  assert first.author.name == Some("One")
  assert second.author.id == "user-2"
}

pub fn fetch_issue_comments_sorts_by_created_time_test() {
  let transport = fn(_request: linear.Request) {
    Ok(linear.Response(status: 200, body: comments_response()))
  }
  let assert Ok(comments) =
    linear.fetch_issue_comments(tracker_config(), ["issue-1"], 10, transport)
  let assert [first, second] = comments
  assert first.id == "c1"
  assert second.id == "c2"
}

pub fn comment_response_errors_are_mapped_test() {
  let assert Error(error.LinearApiStatus(500)) =
    linear.parse_comments_response(linear.Response(500, "{}"))
  let body = "{\"errors\":[{\"message\":\"bad query\"}],\"data\":null}"
  let assert Error(error.LinearGraphqlErrors(_)) =
    linear.parse_comments_response(linear.Response(200, body))
  let invalid_time =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-1\",\"comments\":{\"nodes\":[{\"id\":\"c1\",\"body\":\"/scherzo retry\",\"createdAt\":\"not-time\",\"updatedAt\":\"2026-04-28T10:00:00Z\",\"user\":{\"id\":\"user-1\",\"email\":null,\"name\":null}}]}}]}}}"
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_comments_response(linear.Response(200, invalid_time))
}
