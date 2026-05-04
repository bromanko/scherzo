import gleam/bit_array
import gleam/http
import gleam/http/request as http_request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/linear_body_data
import simplifile

pub type UploadRequest {
  UploadRequest(
    url: String,
    headers: List(#(String, String)),
    body: BitArray,
    timeout_ms: Int,
  )
}

pub type UploadResponse {
  UploadResponse(status: Int, body: BitArray)
}

pub type UploadTransport =
  fn(UploadRequest) -> Result(UploadResponse, error.TrackerError)

pub type Dependencies {
  Dependencies(
    graphql_transport: linear.Transport,
    upload_transport: UploadTransport,
    now_ms: fn() -> Int,
    nonce: fn() -> String,
  )
}

pub type AttachOptions {
  AttachOptions(fallback_to_markdown_link: Bool, dedupe_by_filename: Bool)
}

pub type AttachmentOutcome {
  AttachedNative(comment_id: String, filename: String, asset_url: String)
  AttachedMarkdownLink(comment_id: String, filename: String, asset_url: String)
  AlreadyAttached(comment_id: String, filename: String)
}

pub fn max_attachment_bytes() -> Int {
  1_000_000
}

pub fn validate_attachment_size(size: Int) -> Result(Nil, error.TrackerError) {
  case size > max_attachment_bytes() {
    False -> Ok(Nil)
    True ->
      Error(error.LinearAttachmentError(
        "attachment payload is too large: "
        <> int.to_string(size)
        <> " bytes exceeds "
        <> int.to_string(max_attachment_bytes())
        <> " byte limit",
      ))
  }
}

pub fn real_dependencies(graphql_transport: linear.Transport) -> Dependencies {
  Dependencies(
    graphql_transport: graphql_transport,
    upload_transport: real_upload_transport,
    now_ms: monotonic_ms,
    nonce: fn() { monotonic_ms() |> int.to_string },
  )
}

pub fn real_upload_transport(
  upload_request: UploadRequest,
) -> Result(UploadResponse, error.TrackerError) {
  case string.starts_with(string.lowercase(upload_request.url), "https://") {
    False -> Error(error.LinearApiRequest("upload URL must use https://"))
    True -> {
      use request <- try_tracker(
        http_request.to(upload_request.url)
        |> result_map_error(fn(_) {
          error.LinearApiRequest("invalid upload URL")
        }),
      )
      let request =
        request
        |> http_request.set_method(http.Put)
        |> http_request.set_body(upload_request.body)
        |> set_upload_headers(upload_request.headers)
      case
        httpc.configure()
        |> httpc.timeout(upload_request.timeout_ms)
        |> httpc.dispatch_bits(request)
      {
        Ok(response) ->
          Ok(UploadResponse(status: response.status, body: response.body))
        Error(err) ->
          Error(error.LinearApiRequest(linear.http_error_to_string(err)))
      }
    }
  }
}

pub fn attach_markdown_to_comment(
  config: config_types.TrackerConfig,
  comment_id: String,
  filename: String,
  body: BitArray,
  options: AttachOptions,
  dependencies: Dependencies,
) -> Result(AttachmentOutcome, error.TrackerError) {
  use filename <- try_tracker(validate_markdown_filename(filename))
  let content_type = "text/markdown"
  let size = bit_array.byte_size(body)
  use _ <- try_tracker(validate_attachment_size(size))
  use comment <- try_tracker(fetch_comment(config, comment_id, dependencies))
  use mode <- try_tracker(decide_attach_mode(
    comment,
    filename,
    content_type,
    options,
  ))
  case mode {
    AlreadyAttachedMode -> Ok(AlreadyAttached(comment_id, filename))
    NativeMode(document) ->
      attach_native(
        config,
        comment_id,
        filename,
        body,
        size,
        content_type,
        document,
        dependencies,
      )
    FallbackMode ->
      attach_fallback(
        config,
        comment,
        filename,
        body,
        size,
        content_type,
        dependencies,
      )
  }
}

pub fn attach_markdown_file_to_comment(
  config: config_types.TrackerConfig,
  comment_id: String,
  path: String,
  options: AttachOptions,
  dependencies: Dependencies,
) -> Result(AttachmentOutcome, error.TrackerError) {
  let filename = basename(path)
  use filename <- try_tracker(validate_markdown_filename(filename))
  use info <- try_tracker(
    simplifile.file_info(path)
    |> result_map_error(fn(err) {
      error.LinearAttachmentError(
        "failed to stat attachment file "
        <> path
        <> ": "
        <> simplifile.describe_error(err),
      )
    }),
  )
  case simplifile.file_info_type(info) {
    simplifile.File -> {
      use _ <- try_tracker(validate_attachment_size(info.size))
      use bits <- try_tracker(
        simplifile.read_bits(path)
        |> result_map_error(fn(err) {
          error.LinearAttachmentError(
            "failed to read attachment file "
            <> path
            <> ": "
            <> simplifile.describe_error(err),
          )
        }),
      )
      attach_markdown_to_comment(
        config,
        comment_id,
        filename,
        bits,
        options,
        dependencies,
      )
    }
    _ ->
      Error(error.LinearAttachmentError(
        "attachment path is not a regular file: " <> path,
      ))
  }
}

pub fn outcome_mode(outcome: AttachmentOutcome) -> String {
  case outcome {
    AttachedNative(_, _, _) -> "native"
    AttachedMarkdownLink(_, _, _) -> "markdown_link"
    AlreadyAttached(_, _) -> "already_attached"
  }
}

type ModeDecision {
  AlreadyAttachedMode
  NativeMode(linear_body_data.JsonValue)
  FallbackMode
}

fn fetch_comment(
  config: config_types.TrackerConfig,
  comment_id: String,
  dependencies: Dependencies,
) -> Result(linear.LinearCommentDocument, error.TrackerError) {
  use request <- try_tracker(linear.build_comment_fetch_request(
    config,
    comment_id,
  ))
  use response <- try_tracker(dependencies.graphql_transport(request))
  linear.parse_comment_fetch_response(response)
}

fn decide_attach_mode(
  comment: linear.LinearCommentDocument,
  filename: String,
  content_type: String,
  options: AttachOptions,
) -> Result(ModeDecision, error.TrackerError) {
  case linear_body_data.validate_document(comment.body_data) {
    Ok(document) ->
      case
        options.dedupe_by_filename
        && linear_body_data.has_file_named(document, filename, content_type)
      {
        True -> Ok(AlreadyAttachedMode)
        False -> Ok(NativeMode(document))
      }
    Error(message) ->
      case options.fallback_to_markdown_link {
        False -> Error(error.LinearAttachmentError(message))
        True ->
          case
            options.dedupe_by_filename
            && linear_body_data.body_has_markdown_link_for_filename(
              comment.body,
              filename,
            )
          {
            True -> Ok(AlreadyAttachedMode)
            False -> Ok(FallbackMode)
          }
      }
  }
}

fn attach_native(
  config: config_types.TrackerConfig,
  comment_id: String,
  filename: String,
  body: BitArray,
  size: Int,
  content_type: String,
  document: linear_body_data.JsonValue,
  dependencies: Dependencies,
) -> Result(AttachmentOutcome, error.TrackerError) {
  use upload_file <- try_tracker(upload_markdown(
    config,
    filename,
    body,
    size,
    content_type,
    dependencies,
  ))
  let attrs =
    linear_body_data.FileNodeAttrs(
      upload_id: upload_id(dependencies),
      href: upload_file.asset_url,
      name: filename,
      size: size,
      mimetype: content_type,
    )
  let body_data = case
    linear_body_data.append_file_node_to_document(document, attrs, False)
  {
    linear_body_data.Appended(value) -> value
    linear_body_data.AlreadyPresent -> document
  }
  use request <- try_tracker(linear.build_comment_update_body_data_request(
    config,
    comment_id,
    linear_body_data.to_json(body_data),
  ))
  use response <- try_tracker(dependencies.graphql_transport(request))
  use _comment <- try_tracker(linear.parse_comment_update_response(response))
  Ok(AttachedNative(comment_id, filename, upload_file.asset_url))
}

fn attach_fallback(
  config: config_types.TrackerConfig,
  comment: linear.LinearCommentDocument,
  filename: String,
  body: BitArray,
  size: Int,
  content_type: String,
  dependencies: Dependencies,
) -> Result(AttachmentOutcome, error.TrackerError) {
  use upload_file <- try_tracker(upload_markdown(
    config,
    filename,
    body,
    size,
    content_type,
    dependencies,
  ))
  let link = linear_body_data.markdown_link(filename, upload_file.asset_url)
  let next_body = append_markdown_link(comment.body, link)
  use request <- try_tracker(linear.build_comment_update_body_request(
    config,
    comment.id,
    next_body,
  ))
  use response <- try_tracker(dependencies.graphql_transport(request))
  use _comment <- try_tracker(linear.parse_comment_update_response(response))
  Ok(AttachedMarkdownLink(comment.id, filename, upload_file.asset_url))
}

fn upload_markdown(
  config: config_types.TrackerConfig,
  filename: String,
  body: BitArray,
  size: Int,
  content_type: String,
  dependencies: Dependencies,
) -> Result(linear.UploadFile, error.TrackerError) {
  use request <- try_tracker(linear.build_file_upload_request(
    config,
    filename,
    content_type,
    size,
    json.object([#("source", json.string("scherzo"))]),
  ))
  use response <- try_tracker(dependencies.graphql_transport(request))
  use upload_file <- try_tracker(linear.parse_file_upload_response(response))
  use _ <- try_tracker(validate_asset_url(upload_file.asset_url))
  use _ <- try_tracker(put_upload(upload_file, body, content_type, dependencies))
  Ok(upload_file)
}

fn validate_asset_url(asset_url: String) -> Result(Nil, error.TrackerError) {
  case string.starts_with(string.lowercase(asset_url), "https://") {
    True -> Ok(Nil)
    False ->
      Error(error.LinearAttachmentError("Linear assetUrl must use https://"))
  }
}

fn put_upload(
  upload_file: linear.UploadFile,
  body: BitArray,
  content_type: String,
  dependencies: Dependencies,
) -> Result(Nil, error.TrackerError) {
  let headers =
    upload_file.headers
    |> list.map(fn(header) { #(header.key, header.value) })
    |> list.append([#("Content-Type", content_type)])
  use response <- try_tracker(
    dependencies.upload_transport(UploadRequest(
      url: upload_file.upload_url,
      headers: headers,
      body: body,
      timeout_ms: 30_000,
    )),
  )
  case response.status >= 200 && response.status < 300 {
    True -> Ok(Nil)
    False -> Error(error.LinearUploadStatus(response.status))
  }
}

fn append_markdown_link(body: String, link: String) -> String {
  case string.trim(body) == "" {
    True -> link
    False -> body <> "\n\n" <> link
  }
}

fn upload_id(dependencies: Dependencies) -> String {
  "upload-"
  <> int.to_string(dependencies.now_ms())
  <> "-"
  <> dependencies.nonce()
}

fn validate_markdown_filename(
  filename: String,
) -> Result(String, error.TrackerError) {
  let filename = string.trim(filename)
  let lower = string.lowercase(filename)
  case
    filename == ""
    || string.contains(filename, "/")
    || string.contains(filename, "\\")
  {
    True ->
      Error(error.LinearAttachmentError(
        "attachment filename must be non-empty and must not contain path separators",
      ))
    False ->
      case
        string.ends_with(lower, ".md") || string.ends_with(lower, ".markdown")
      {
        True -> Ok(filename)
        False ->
          Error(error.LinearAttachmentError(
            "attachment filename must end with .md or .markdown",
          ))
      }
  }
}

fn basename(path: String) -> String {
  path
  |> string.split(on: "/")
  |> list.reverse
  |> first_or_empty
}

fn first_or_empty(values: List(String)) -> String {
  case values {
    [] -> ""
    [value, ..] -> value
  }
}

fn set_upload_headers(
  request: http_request.Request(BitArray),
  headers: List(#(String, String)),
) -> http_request.Request(BitArray) {
  case headers {
    [] -> request
    [#(key, value), ..rest] ->
      set_upload_headers(http_request.set_header(request, key, value), rest)
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

fn result_map_error(result: Result(a, e), mapper: fn(e) -> f) -> Result(a, f) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(mapper(err))
  }
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
