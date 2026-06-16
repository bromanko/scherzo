import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/linear/work_item_query
import scherzo/task
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import simplifile

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.test/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    task_scope: None,
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

pub fn work_item_query_builds_allowlisted_requests_test() {
  let assert Ok(request) =
    work_item_query.build_list_request(
      tracker_config(),
      issue_state.list_from_strings(["Todo"]),
      None,
      10,
      50,
    )

  assert string.contains(request.body, "ScherzoWorkItemList")
  assert !string.contains(request.body, "children(first: $childLimit)")
  assert !string.contains(request.body, "childLimit")
  assert !string.contains(request.body, "description")
  assert !string.contains(request.body, "comments")
  assert !string.contains(request.body, "bodyData")
}

pub fn work_item_query_parses_list_fixture_test() {
  let assert Ok(body) =
    simplifile.read("test/fixtures/linear_work_item/list_with_children.json")
  let assert Ok(page) =
    work_item_query.parse_page_response(
      linear.Response(status: 200, body: body),
      10,
      50,
    )

  let assert [first] = page.items
  assert first.source.display_id == Some("LIV-1168")
  assert first.state.id == Some("state-todo")
  assert first.state.name == "Todo"
  assert first.state.category == task.Ready
  assert first.labels_truncated == False
  assert page.has_next_page == False
}

pub fn work_item_query_parses_detail_fixture_with_truncation_test() {
  let assert Ok(body) =
    simplifile.read(
      "test/fixtures/linear_work_item/show_with_truncated_children.json",
    )
  let assert Ok(Some(detail)) =
    work_item_query.parse_detail_by_identifier_response(
      linear.Response(status: 200, body: body),
      config_types.LinearTaskProject("PROJ"),
      "LIV-1168",
      1,
      1,
    )

  assert detail.summary.labels_truncated == True
  assert detail.subtasks_truncated == True
  let assert [first] = detail.subtasks
  assert first.source.display_id == Some("LIV-1169")
}

pub fn work_item_query_parses_detail_by_id_response_test() {
  let body =
    "{\"data\":{\"issues\":{\"nodes\":[{\"id\":\"issue-parent-1\",\"identifier\":\"LIV-1168\",\"title\":\"Implement work items\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"

  let assert Ok(Some(detail)) =
    work_item_query.parse_detail_by_id_response(
      linear.Response(status: 200, body: body),
      50,
      50,
    )
  assert detail.summary.source.id == "issue-parent-1"
  assert detail.summary.source.display_id == Some("LIV-1168")
}

pub fn work_item_query_identifier_detail_filters_not_found_scope_and_identifier_test() {
  let missing_body = "{\"data\":{\"issue\":null}}"
  let wrong_project_body =
    "{\"data\":{\"issue\":{\"project\":{\"slugId\":\"OTHER\"},\"id\":\"issue-parent-1\",\"identifier\":\"LIV-1168\",\"title\":\"Implement work items\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"}}}}"
  let mismatched_identifier_body =
    "{\"data\":{\"issue\":{\"project\":{\"slugId\":\"PROJ\"},\"id\":\"issue-parent-1\",\"identifier\":\"LIV-9999\",\"title\":\"Implement work items\",\"state\":{\"id\":\"state-todo\",\"name\":\"Todo\",\"type\":\"unstarted\"}}}}"
  let not_found_error_body =
    "{\"errors\":[{\"message\":\"Issue not found\"}],\"data\":{\"issue\":null}}"

  assert work_item_query.parse_detail_by_identifier_response(
      linear.Response(status: 200, body: missing_body),
      config_types.LinearTaskProject("PROJ"),
      "LIV-1168",
      50,
      50,
    )
    == Ok(None)
  assert work_item_query.parse_detail_by_identifier_response(
      linear.Response(status: 200, body: wrong_project_body),
      config_types.LinearTaskProject("PROJ"),
      "LIV-1168",
      50,
      50,
    )
    == Ok(None)
  assert work_item_query.parse_detail_by_identifier_response(
      linear.Response(status: 200, body: mismatched_identifier_body),
      config_types.LinearTaskProject("PROJ"),
      "LIV-1168",
      50,
      50,
    )
    == Ok(None)
  assert work_item_query.parse_detail_by_identifier_response(
      linear.Response(status: 200, body: not_found_error_body),
      config_types.LinearTaskProject("PROJ"),
      "LIV-1168",
      50,
      50,
    )
    == Ok(None)
}

pub fn work_item_query_maps_graphql_errors_without_raw_body_test() {
  let response =
    linear.Response(
      status: 200,
      body: "{\"errors\":[{\"message\":\"denied\"}],\"data\":null}",
    )

  let assert Error(error.LinearGraphqlErrors(message)) =
    work_item_query.parse_page_response(response, 10, 50)
  assert message == "denied"
}

pub fn work_item_query_rejects_non_200_status_test() {
  let assert Error(error.LinearApiStatus(503)) =
    work_item_query.parse_page_response(
      linear.Response(status: 503, body: "RAW_PROVIDER_BODY_SECRET"),
      10,
      50,
    )
}
