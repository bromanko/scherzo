import gleam/bit_array
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/linear_attachment
import scherzo/linear_body_data
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import simplifile

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn deps(
  graphql_subject: process.Subject(linear.Request),
  upload_subject: process.Subject(linear_attachment.UploadRequest),
  upload_status: Int,
  fetch_body: String,
  fetch_body_data: String,
) -> linear_attachment.Dependencies {
  linear_attachment.Dependencies(
    graphql_transport: fn(request) {
      process.send(graphql_subject, request)
      case string.contains(request.body, "ScherzoCommentFetch") {
        True ->
          Ok(linear.Response(
            status: 200,
            body: comment_fetch_response(fetch_body, fetch_body_data),
          ))
        False ->
          case string.contains(request.body, "ScherzoFileUpload") {
            True ->
              Ok(linear.Response(status: 200, body: file_upload_response()))
            False ->
              Ok(linear.Response(
                status: 200,
                body: comment_update_response(fetch_body, fetch_body_data),
              ))
          }
      }
    },
    upload_transport: fn(request) {
      process.send(upload_subject, request)
      Ok(linear_attachment.UploadResponse(
        status: upload_status,
        body: bit_array.from_string(""),
      ))
    },
    now_ms: fn() { 123 },
    nonce: fn() { "abc" },
  )
}

fn options(fallback: Bool, dedupe: Bool) -> linear_attachment.AttachOptions {
  linear_attachment.AttachOptions(
    fallback_to_markdown_link: fallback,
    dedupe_by_filename: dedupe,
  )
}

pub fn native_attachment_uses_required_upload_flow_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let body_bits = bit_array.from_string("hello 🌍")
  let assert Ok(linear_attachment.AttachedNative(
    comment_id: "comment-id",
    filename: "result.md",
    asset_url: "https://uploads.linear.app/asset.md",
  )) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      body_bits,
      options(True, True),
      deps(graphql_subject, upload_subject, 204, "hello", empty_body_data()),
    )

  let assert Ok(fetch_request) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload_request) =
    process.receive(graphql_subject, within: 100)
  let assert Ok(upload_request) = process.receive(upload_subject, within: 100)
  let assert Ok(update_request) = process.receive(graphql_subject, within: 100)

  assert string.contains(fetch_request.body, "ScherzoCommentFetch")
  assert string.contains(file_upload_request.body, "ScherzoFileUpload")
  assert string.contains(
    file_upload_request.body,
    "\"size\":" <> int_to_string(bit_array.byte_size(body_bits)),
  )
  assert upload_request.url == "https://uploads.linear.app/presigned"
  assert header_value(upload_request.headers, "x-amz-meta-test") == Some("yes")
  assert header_value(upload_request.headers, "Content-Type")
    == Some("text/markdown")
  assert header_value(upload_request.headers, "Authorization") == None
  assert upload_request.body == body_bits
  assert string.contains(update_request.body, "ScherzoCommentUpdateBodyData")
  assert string.contains(update_request.body, "uploadState")
  assert string.contains(update_request.body, "finished")
  assert string.contains(update_request.body, "upload-123-abc")
  assert string.contains(
    update_request.body,
    "https://uploads.linear.app/asset.md",
  )
  assert string.contains(update_request.body, "result.md")
  assert string.contains(update_request.body, "text/markdown")
  assert string.contains(
    update_request.body,
    "\"size\":" <> int_to_string(bit_array.byte_size(body_bits)),
  )
}

pub fn native_dedupe_skips_upload_when_file_already_present_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Ok(linear_attachment.AlreadyAttached("comment-id", "result.md")) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      bit_array.from_string("hello"),
      options(True, True),
      deps(
        graphql_subject,
        upload_subject,
        204,
        "hello",
        body_data_with_file("result.md"),
      ),
    )
  let assert Ok(fetch_request) = process.receive(graphql_subject, within: 100)
  assert string.contains(fetch_request.body, "ScherzoCommentFetch")
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert process.receive(upload_subject, within: 20) == Error(Nil)
}

pub fn fallback_dedupe_skips_upload_when_link_already_present_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let body =
    "body\n\n"
    <> linear_body_data.markdown_link(
      "result.md",
      "https://uploads.linear.app/existing.md",
    )
  let assert Ok(linear_attachment.AlreadyAttached("comment-id", "result.md")) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      bit_array.from_string("hello"),
      options(True, True),
      deps(graphql_subject, upload_subject, 204, body, "not json"),
    )
  let assert Ok(fetch_request) = process.receive(graphql_subject, within: 100)
  assert string.contains(fetch_request.body, "ScherzoCommentFetch")
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert process.receive(upload_subject, within: 20) == Error(Nil)
}

pub fn fallback_updates_body_when_body_data_is_invalid_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Ok(linear_attachment.AttachedMarkdownLink(
    comment_id: "comment-id",
    filename: "result.md",
    asset_url: "https://uploads.linear.app/asset.md",
  )) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      bit_array.from_string("hello"),
      options(True, False),
      deps(graphql_subject, upload_subject, 204, "old body", "not json"),
    )
  let assert Ok(_) = process.receive(graphql_subject, within: 100)
  let assert Ok(_) = process.receive(graphql_subject, within: 100)
  let assert Ok(_) = process.receive(upload_subject, within: 100)
  let assert Ok(update_request) = process.receive(graphql_subject, within: 100)
  assert string.contains(update_request.body, "ScherzoCommentUpdateBody")
  assert string.contains(update_request.body, "old body")
  assert string.contains(
    update_request.body,
    linear_body_data.markdown_link(
      "result.md",
      "https://uploads.linear.app/asset.md",
    ),
  )
  assert !string.contains(update_request.body, "ScherzoCommentUpdateBodyData")
}

pub fn invalid_body_data_without_fallback_fails_before_upload_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Error(error.LinearAttachmentError(_)) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      bit_array.from_string("hello"),
      options(False, False),
      deps(graphql_subject, upload_subject, 204, "old body", "not json"),
    )
  let assert Ok(fetch_request) = process.receive(graphql_subject, within: 100)
  assert string.contains(fetch_request.body, "ScherzoCommentFetch")
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert process.receive(upload_subject, within: 20) == Error(Nil)
}

pub fn upload_status_failure_stops_before_comment_update_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Error(error.LinearUploadStatus(403)) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      bit_array.from_string("hello"),
      options(True, False),
      deps(graphql_subject, upload_subject, 403, "hello", empty_body_data()),
    )
  let assert Ok(fetch_request) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload_request) =
    process.receive(graphql_subject, within: 100)
  let assert Ok(_) = process.receive(upload_subject, within: 100)
  assert string.contains(fetch_request.body, "ScherzoCommentFetch")
  assert string.contains(file_upload_request.body, "ScherzoFileUpload")
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
}

pub fn oversize_in_memory_attachment_fails_before_graphql_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Error(error.LinearAttachmentError(_)) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      string.repeat("a", times: linear_attachment.max_attachment_bytes() + 1)
        |> bit_array.from_string,
      options(True, False),
      deps(graphql_subject, upload_subject, 204, "hello", empty_body_data()),
    )
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert process.receive(upload_subject, within: 20) == Error(Nil)
}

pub fn invalid_asset_url_fails_before_upload_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let dependencies =
    linear_attachment.Dependencies(
      graphql_transport: fn(request) {
        process.send(graphql_subject, request)
        case string.contains(request.body, "ScherzoCommentFetch") {
          True ->
            Ok(linear.Response(
              status: 200,
              body: comment_fetch_response("hello", empty_body_data()),
            ))
          False ->
            Ok(linear.Response(
              status: 200,
              body: file_upload_response_with_asset_url(
                "http://asset.test/file.md",
              ),
            ))
        }
      },
      upload_transport: fn(request) {
        process.send(upload_subject, request)
        Ok(linear_attachment.UploadResponse(
          status: 204,
          body: bit_array.from_string(""),
        ))
      },
      now_ms: fn() { 123 },
      nonce: fn() { "abc" },
    )
  let assert Error(error.LinearAttachmentError(_)) =
    linear_attachment.attach_markdown_to_comment(
      tracker_config(),
      "comment-id",
      "result.md",
      bit_array.from_string("hello"),
      options(True, False),
      dependencies,
    )
  let assert Ok(fetch_request) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload_request) =
    process.receive(graphql_subject, within: 100)
  assert string.contains(fetch_request.body, "ScherzoCommentFetch")
  assert string.contains(file_upload_request.body, "ScherzoFileUpload")
  assert process.receive(upload_subject, within: 20) == Error(Nil)
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
}

pub fn attach_markdown_file_rejects_non_markdown_extension_before_graphql_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Error(error.LinearAttachmentError(_)) =
    linear_attachment.attach_markdown_file_to_comment(
      tracker_config(),
      "comment-id",
      "test/tmp/result.txt",
      options(True, False),
      deps(graphql_subject, upload_subject, 204, "hello", empty_body_data()),
    )
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert process.receive(upload_subject, within: 20) == Error(Nil)
}

pub fn attach_markdown_file_rejects_oversize_file_before_graphql_test() {
  let dir = "test/tmp/linear-attachment-too-large"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let path = dir <> "/too-large.md"
  let assert Ok(Nil) =
    simplifile.write(
      path,
      string.repeat("a", times: linear_attachment.max_attachment_bytes() + 1),
    )
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Error(error.LinearAttachmentError(_)) =
    linear_attachment.attach_markdown_file_to_comment(
      tracker_config(),
      "comment-id",
      path,
      options(True, False),
      deps(graphql_subject, upload_subject, 204, "hello", empty_body_data()),
    )
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert process.receive(upload_subject, within: 20) == Error(Nil)
}

pub fn attach_markdown_file_uses_utf8_byte_size_test() {
  let dir = "test/tmp/linear-attachment-file"
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let path = dir <> "/unicode.md"
  let content = "hello 🌍"
  let assert Ok(Nil) = simplifile.write(path, content)
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let assert Ok(linear_attachment.AttachedNative(_, "unicode.md", _)) =
    linear_attachment.attach_markdown_file_to_comment(
      tracker_config(),
      "comment-id",
      path,
      options(True, False),
      deps(graphql_subject, upload_subject, 204, "hello", empty_body_data()),
    )
  let assert Ok(_) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload_request) =
    process.receive(graphql_subject, within: 100)
  assert string.contains(
    file_upload_request.body,
    "\"size\":"
      <> int_to_string(bit_array.byte_size(bit_array.from_string(content))),
  )
}

fn empty_body_data() -> String {
  json.to_string(
    json.object([
      #("type", json.string("doc")),
      #("content", json.preprocessed_array([])),
    ]),
  )
}

fn body_data_with_file(filename: String) -> String {
  json.to_string(
    json.object([
      #("type", json.string("doc")),
      #(
        "content",
        json.preprocessed_array([
          json.object([
            #("type", json.string("file")),
            #(
              "attrs",
              json.object([
                #("name", json.string(filename)),
                #("mimetype", json.string("text/markdown")),
              ]),
            ),
          ]),
        ]),
      ),
    ]),
  )
}

fn comment_fetch_response(body: String, body_data: String) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #("comment", comment_json("comment-id", body, body_data)),
        ]),
      ),
    ]),
  )
}

fn comment_update_response(body: String, body_data: String) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "commentUpdate",
            json.object([
              #("success", json.bool(True)),
              #("comment", comment_json("comment-id", body, body_data)),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn comment_json(id: String, body: String, body_data: String) -> json.Json {
  json.object([
    #("id", json.string(id)),
    #("body", json.string(body)),
    #("bodyData", json.string(body_data)),
  ])
}

fn file_upload_response() -> String {
  file_upload_response_with_asset_url("https://uploads.linear.app/asset.md")
}

fn file_upload_response_with_asset_url(asset_url: String) -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "fileUpload",
            json.object([
              #("success", json.bool(True)),
              #(
                "uploadFile",
                json.object([
                  #("filename", json.string("result.md")),
                  #("contentType", json.string("text/markdown")),
                  #("size", json.int(10)),
                  #(
                    "uploadUrl",
                    json.string("https://uploads.linear.app/presigned"),
                  ),
                  #("assetUrl", json.string(asset_url)),
                  #(
                    "headers",
                    json.preprocessed_array([
                      json.object([
                        #("key", json.string("x-amz-meta-test")),
                        #("value", json.string("yes")),
                      ]),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn header_value(
  headers: List(#(String, String)),
  key: String,
) -> Option(String) {
  case list.key_find(headers, key) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
