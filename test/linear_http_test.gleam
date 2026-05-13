import gleam/httpc
import gleam/option.{Some}
import gleam/string
import scherzo/config/types as config_types
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
    active_states: issue_state.list_from_strings(["Todo"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn response_page(identifier: String) -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\""
  <> identifier
  <> "-id\",\"identifier\":\""
  <> identifier
  <> "\",\"title\":\"Title\",\"description\":null,\"priority\":1,\"branchName\":null,\"url\":null,\"createdAt\":null,\"updatedAt\":null,\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[]},\"inverseRelations\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

pub fn real_client_delegates_candidate_terminal_and_refresh_queries_test() {
  let transport = fn(_request: linear.Request) {
    Ok(linear.Response(status: 200, body: response_page("ABC-1")))
  }
  let client = linear.client(tracker_config(), transport)

  let assert Ok(candidates) = client.fetch_candidate_issues()
  let assert Ok(terminals) =
    client.fetch_issues_by_states(issue_state.list_from_strings(["Done"]))
  let assert Ok(refreshed) = client.fetch_issue_states_by_ids(["ABC-1-id"])

  assert list_identifiers(candidates) == ["ABC-1"]
  assert list_identifiers(terminals) == ["ABC-1"]
  assert list_identifiers(refreshed) == ["ABC-1"]
}

pub fn http_transport_maps_httpc_errors_without_secret_values_test() {
  let timeout = linear.http_error_to_string(httpc.ResponseTimeout)
  let utf8 = linear.http_error_to_string(httpc.InvalidUtf8Response)

  assert !string.contains(timeout, "secret-key")
  assert !string.contains(utf8, "secret-key")
}

fn list_identifiers(issues: List(tracker_issue.Issue)) -> List(String) {
  case issues {
    [] -> []
    [issue, ..rest] -> [issue.identifier, ..list_identifiers(rest)]
  }
}
