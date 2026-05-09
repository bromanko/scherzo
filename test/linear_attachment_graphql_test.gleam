import gleam/json
import gleam/option.{Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/linear_body_data
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

pub fn attachment_request_builders_use_expected_operations_and_headers_test() {
  let assert Ok(fetch) =
    linear.build_comment_fetch_request(tracker_config(), "comment-id")
  assert string.contains(fetch.body, "ScherzoCommentFetch")
  assert string.contains(fetch.body, "comment-id")
  assert fetch.headers
    == [
      #("Authorization", "secret-key"),
      #("Content-Type", "application/json"),
    ]

  let assert Ok(upload) =
    linear.build_file_upload_request(
      tracker_config(),
      "result.md",
      "text/markdown",
      12,
      json.object([#("source", json.string("test"))]),
    )
  assert string.contains(upload.body, "ScherzoFileUpload")
  assert string.contains(upload.body, "contentType")
  assert string.contains(upload.body, "result.md")
  assert string.contains(upload.body, "text/markdown")
  assert string.contains(upload.body, "\"size\":12")
  assert string.contains(upload.body, "source")
  assert !string.contains(upload.body, "makePublic")
  assert upload.headers == fetch.headers

  let body_data = json.object([#("type", json.string("doc"))])
  let assert Ok(update_body_data) =
    linear.build_comment_update_body_data_request(
      tracker_config(),
      "comment-id",
      body_data,
    )
  assert string.contains(update_body_data.body, "ScherzoCommentUpdateBodyData")
  assert string.contains(update_body_data.body, "bodyData")
  assert string.contains(update_body_data.body, "comment-id")

  let assert Ok(update_body) =
    linear.build_comment_update_body_request(
      tracker_config(),
      "comment-id",
      "old body\n\n[result.md](https://asset)",
    )
  assert string.contains(update_body.body, "ScherzoCommentUpdateBody")
  assert string.contains(update_body.body, "body")
}

pub fn file_upload_response_decodes_upload_file_test() {
  let assert Ok(upload_file) =
    linear.parse_file_upload_response(linear.Response(
      status: 200,
      body: file_upload_response("true", upload_file_json()),
    ))
  assert upload_file.filename == "result.md"
  assert upload_file.content_type == "text/markdown"
  assert upload_file.size == 12
  assert upload_file.upload_url == "https://uploads.linear.app/presigned"
  assert upload_file.asset_url == "https://uploads.linear.app/asset.md"
  let assert [first, second] = upload_file.headers
  assert first == linear.UploadHeader(key: "x-amz-acl", value: "private")
  assert second == linear.UploadHeader(key: "x-amz-meta", value: "value")
}

pub fn file_upload_response_maps_failure_cases_test() {
  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_file_upload_response(linear.Response(
      status: 200,
      body: file_upload_response("false", upload_file_json()),
    ))

  let assert Error(error.LinearAttachmentError(_)) =
    linear.parse_file_upload_response(linear.Response(
      status: 200,
      body: file_upload_response("true", "null"),
    ))

  let assert Error(error.LinearGraphqlErrors(_)) =
    linear.parse_file_upload_response(linear.Response(
      status: 200,
      body: "{\"errors\":[{\"message\":\"bad upload\"}],\"data\":null}",
    ))

  let assert Error(error.LinearApiStatus(500)) =
    linear.parse_file_upload_response(linear.Response(status: 500, body: "{}"))

  let assert Error(error.LinearUnknownPayload(_)) =
    linear.parse_file_upload_response(linear.Response(status: 200, body: "{"))
}

pub fn comment_fetch_and_update_responses_decode_comment_document_test() {
  let assert Ok(fetched) =
    linear.parse_comment_fetch_response(linear.Response(
      status: 200,
      body: "{\"data\":{\"comment\":{" <> comment_json() <> "}}}",
    ))
  assert fetched.id == "comment-id"
  assert fetched.body == "hello"
  let assert Ok(_) = linear_body_data.validate_document(fetched.body_data)

  let assert Ok(fetched_object_body_data) =
    linear.parse_comment_fetch_response(linear.Response(
      status: 200,
      body: "{\"data\":{\"comment\":{\"id\":\"comment-id\",\"body\":\"hello\",\"bodyData\":{\"type\":\"doc\",\"content\":[]}}}}",
    ))
  let assert Ok(_) =
    linear_body_data.validate_document(fetched_object_body_data.body_data)

  let assert Ok(updated) =
    linear.parse_comment_update_response(linear.Response(
      status: 200,
      body: "{\"data\":{\"commentUpdate\":{\"success\":true,\"comment\":{"
        <> comment_json()
        <> "}}}}",
    ))
  assert updated.id == "comment-id"

  let assert Ok(created) =
    linear.parse_comment_create_response(linear.Response(
      status: 200,
      body: "{\"data\":{\"commentCreate\":{\"success\":true,\"comment\":{"
        <> comment_json()
        <> "}}}}",
    ))
  assert created.body == "hello"
}

fn file_upload_response(success: String, upload_file: String) -> String {
  "{\"data\":{\"fileUpload\":{\"success\":"
  <> success
  <> ",\"uploadFile\":"
  <> upload_file
  <> "}}}"
}

fn upload_file_json() -> String {
  "{\"filename\":\"result.md\",\"contentType\":\"text/markdown\",\"size\":12,\"uploadUrl\":\"https://uploads.linear.app/presigned\",\"assetUrl\":\"https://uploads.linear.app/asset.md\",\"headers\":[{\"key\":\"x-amz-acl\",\"value\":\"private\"},{\"key\":\"x-amz-meta\",\"value\":\"value\"}]}"
}

fn comment_json() -> String {
  "\"id\":\"comment-id\",\"body\":\"hello\",\"bodyData\":\"{\\\"type\\\":\\\"doc\\\",\\\"content\\\":[]}\""
}
