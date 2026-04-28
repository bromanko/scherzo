import gleam/httpc
import gleam/option.{Some}
import gleam/string
import scherzo/domain
import scherzo/linear

fn tracker_config() -> domain.TrackerConfig {
  domain.TrackerConfig(
    kind: "linear",
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: ["Todo"],
    terminal_states: ["Done"],
  )
}

fn response_page(identifier: String) -> String {
  "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\""
  <> identifier
  <> "-id\",\"identifier\":\""
  <> identifier
  <> "\",\"title\":\"Title\",\"description\":null,\"priority\":1,\"branchName\":null,\"url\":null,\"createdAt\":null,\"updatedAt\":null,\"state\":{\"name\":\"Todo\"},\"labels\":{\"nodes\":[]},\"relations\":{\"nodes\":[]}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

pub fn real_client_delegates_candidate_terminal_and_refresh_queries_test() {
  let transport = fn(_request: linear.Request) {
    Ok(linear.Response(status: 200, body: response_page("ABC-1")))
  }
  let client = linear.client(tracker_config(), transport)

  let assert Ok(candidates) = client.fetch_candidate_issues()
  let assert Ok(terminals) = client.fetch_issues_by_states(["Done"])
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

fn list_identifiers(issues: List(domain.Issue)) -> List(String) {
  case issues {
    [] -> []
    [issue, ..rest] -> [issue.identifier, ..list_identifiers(rest)]
  }
}
