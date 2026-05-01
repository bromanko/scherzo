import birl
import gleam/bit_array
import gleam/erlang/process
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear_attachment

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

fn handoff_config() -> domain.HandoffConfig {
  domain.HandoffConfig(
    enabled: True,
    comment_on_claim: True,
    comment_on_success: True,
    comment_on_failure: True,
    claim_state_id: Some("claim-state"),
    success_state_id: None,
    failure_state_id: None,
    include_result_on_success: True,
    attach_result_on_success: False,
    attachment_fallback_to_markdown_link: True,
    result_max_chars: 8000,
  )
}

fn issue() -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-1",
    title: "Title",
    description: None,
    priority: None,
    state: "Todo",
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn success() -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: Some(domain.Issue(..issue(), state: "Done")),
    final_classification: runner.FinalTerminal,
    workspace_path: "workspace",
    tokens: domain.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 0,
      cache_write: 0,
      total: 3,
    ),
    turns: 1,
    result: domain.ResultArtifact(
      final_response: Some("Implemented secret-key"),
      truncated: False,
      source: "agent_end_messages",
    ),
  )
}

pub fn comments_only_and_state_handoff_builds_expected_mutations_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    case string.contains(request.body, "issueUpdate") {
      True ->
        Ok(linear.Response(
          status: 200,
          body: "{\"data\":{\"issueUpdate\":{\"success\":true}}}",
        ))
      False ->
        Ok(linear.Response(
          status: 200,
          body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
        ))
    }
  }
  let client =
    handoff.linear_client(tracker_config(), handoff_config(), transport)

  assert client.claim_issue(issue(), "run-1") == Ok(Nil)
  let assert Ok(claim_comment) = process.receive(subject, within: 100)
  let assert Ok(claim_state) = process.receive(subject, within: 100)
  assert string.contains(claim_comment, "ABC-1")
  assert string.contains(claim_comment, "run-1")
  assert string.contains(claim_state, "claim-state")

  assert client.report_success(issue(), success(), "run-2") == Ok(Nil)
  let assert Ok(success_comment) = process.receive(subject, within: 100)
  assert string.contains(success_comment, "run-2")
  assert string.contains(success_comment, "Result:")
  assert string.contains(success_comment, "Implemented [REDACTED]")
  assert string.contains(success_comment, "classification: terminal")
  assert string.contains(
    success_comment,
    "tokens: input=1 output=2 cache_read=0 cache_write=0 total=3",
  )
  assert !string.contains(success_comment, "secret-key")

  let failure =
    runner.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("secret details")),
      workspace_path: None,
      tokens: domain.zero_token_totals(),
      final_issue: None,
    )
  assert client.report_failure(issue(), failure, "run-3") == Ok(Nil)
  let assert Ok(failure_comment) = process.receive(subject, within: 100)
  assert string.contains(failure_comment, "run-3")
  assert string.contains(failure_comment, "agent_pi_failed")
  assert !string.contains(failure_comment, "secret details")
}

pub fn success_handoff_posts_single_structured_result_comment_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    Ok(linear.Response(
      status: 200,
      body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
    ))
  }
  let no_state =
    domain.HandoffConfig(
      ..handoff_config(),
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
    )
  let client = handoff.linear_client(tracker_config(), no_state, transport)

  assert client.report_success(issue(), success(), "run-structured") == Ok(Nil)
  let assert Ok(success_comment) = process.receive(subject, within: 100)
  assert process.receive(subject, within: 20) == Error(Nil)
  assert string.contains(success_comment, "commentCreate")
  assert string.contains(success_comment, "run-structured")
  assert string.contains(success_comment, "Result:")
  assert string.contains(success_comment, "Implemented [REDACTED]")
}

pub fn success_handoff_with_attachment_uploads_result_to_created_comment_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let client =
    handoff.linear_client_with_attachment_dependencies(
      tracker_config(),
      domain.HandoffConfig(
        ..handoff_config(),
        attach_result_on_success: True,
        success_state_id: None,
      ),
      attachment_deps(graphql_subject, upload_subject, 204),
    )

  assert client.report_success(issue(), success(), "run-attach") == Ok(Nil)
  let assert Ok(comment_create) = process.receive(graphql_subject, within: 100)
  let assert Ok(comment_fetch) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload) = process.receive(graphql_subject, within: 100)
  let assert Ok(upload_request) = process.receive(upload_subject, within: 100)
  let assert Ok(comment_update) = process.receive(graphql_subject, within: 100)
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
  assert string.contains(comment_create, "ScherzoCommentCreate")
  assert string.contains(comment_create, "run-attach")
  assert string.contains(comment_fetch, "created-comment")
  assert string.contains(file_upload, "ScherzoFileUpload")
  assert string.contains(file_upload, "abc-1-run-attach-result.md")
  assert upload_request.url == "https://uploads.linear.app/presigned"
  assert string.contains(comment_update, "ScherzoCommentUpdateBodyData")
}

pub fn success_handoff_attachment_respects_inline_result_toggle_and_state_order_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let client =
    handoff.linear_client_with_attachment_dependencies(
      tracker_config(),
      domain.HandoffConfig(
        ..handoff_config(),
        include_result_on_success: False,
        attach_result_on_success: True,
        success_state_id: Some("success-state"),
      ),
      attachment_deps(graphql_subject, upload_subject, 204),
    )

  assert client.report_success(issue(), success(), "Run 2") == Ok(Nil)
  let assert Ok(comment_create) = process.receive(graphql_subject, within: 100)
  let assert Ok(comment_fetch) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload) = process.receive(graphql_subject, within: 100)
  let assert Ok(upload_request) = process.receive(upload_subject, within: 100)
  let assert Ok(comment_update) = process.receive(graphql_subject, within: 100)
  let assert Ok(issue_update) = process.receive(graphql_subject, within: 100)
  let assert Ok(upload_markdown) = bit_array.to_string(upload_request.body)
  assert string.contains(comment_create, "Metadata:")
  assert !string.contains(comment_create, "Result:")
  assert !string.contains(comment_create, "Implemented [REDACTED]")
  assert string.contains(file_upload, "abc-1-run-2-result.md")
  assert string.contains(upload_markdown, "Implemented [REDACTED]")
  assert !string.contains(upload_markdown, "secret-key")
  assert string.contains(comment_fetch, "ScherzoCommentFetch")
  assert string.contains(comment_update, "ScherzoCommentUpdateBodyData")
  assert string.contains(issue_update, "ScherzoIssueUpdateState")
  assert string.contains(issue_update, "success-state")
}

pub fn success_handoff_attachment_failure_stops_before_state_update_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let client =
    handoff.linear_client_with_attachment_dependencies(
      tracker_config(),
      domain.HandoffConfig(
        ..handoff_config(),
        attach_result_on_success: True,
        success_state_id: Some("success-state"),
      ),
      attachment_deps(graphql_subject, upload_subject, 403),
    )

  let assert Error(error.LinearUploadStatus(403)) =
    client.report_success(issue(), success(), "run-fail")
  let assert Ok(comment_create) = process.receive(graphql_subject, within: 100)
  let assert Ok(comment_fetch) = process.receive(graphql_subject, within: 100)
  let assert Ok(file_upload) = process.receive(graphql_subject, within: 100)
  let assert Ok(_) = process.receive(upload_subject, within: 100)
  assert string.contains(comment_create, "ScherzoCommentCreate")
  assert string.contains(comment_fetch, "ScherzoCommentFetch")
  assert string.contains(file_upload, "ScherzoFileUpload")
  assert process.receive(graphql_subject, within: 20) == Error(Nil)
}

pub fn disabled_handoff_performs_no_transport_calls_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    Ok(linear.Response(status: 500, body: "{}"))
  }
  let disabled = domain.HandoffConfig(..handoff_config(), enabled: False)
  let client = handoff.linear_client(tracker_config(), disabled, transport)

  assert client.claim_issue(issue(), "run-1") == Ok(Nil)
  assert client.report_success(issue(), success(), "run-2") == Ok(Nil)
  assert process.receive(subject, within: 20) == Error(Nil)
}

fn attachment_deps(
  graphql_subject: process.Subject(String),
  upload_subject: process.Subject(linear_attachment.UploadRequest),
  upload_status: Int,
) -> linear_attachment.Dependencies {
  linear_attachment.Dependencies(
    graphql_transport: fn(request) {
      process.send(graphql_subject, request.body)
      case string.contains(request.body, "ScherzoCommentCreate") {
        True ->
          Ok(linear.Response(status: 200, body: comment_create_response()))
        False ->
          case string.contains(request.body, "ScherzoCommentFetch") {
            True ->
              Ok(linear.Response(status: 200, body: comment_fetch_response()))
            False ->
              case string.contains(request.body, "ScherzoFileUpload") {
                True ->
                  Ok(linear.Response(status: 200, body: file_upload_response()))
                False ->
                  case string.contains(request.body, "ScherzoCommentUpdate") {
                    True ->
                      Ok(linear.Response(
                        status: 200,
                        body: comment_update_response(),
                      ))
                    False ->
                      Ok(linear.Response(
                        status: 200,
                        body: "{\"data\":{\"issueUpdate\":{\"success\":true}}}",
                      ))
                  }
              }
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

fn comment_create_response() -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "commentCreate",
            json.object([
              #("success", json.bool(True)),
              #("comment", comment_json()),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn comment_fetch_response() -> String {
  json.to_string(
    json.object([
      #("data", json.object([#("comment", comment_json())])),
    ]),
  )
}

fn comment_update_response() -> String {
  json.to_string(
    json.object([
      #(
        "data",
        json.object([
          #(
            "commentUpdate",
            json.object([
              #("success", json.bool(True)),
              #("comment", comment_json()),
            ]),
          ),
        ]),
      ),
    ]),
  )
}

fn comment_json() -> json.Json {
  json.object([
    #("id", json.string("created-comment")),
    #("body", json.string("created body")),
    #("bodyData", json.string(empty_body_data())),
  ])
}

fn empty_body_data() -> String {
  json.to_string(
    json.object([
      #("type", json.string("doc")),
      #("content", json.preprocessed_array([])),
    ]),
  )
}

fn file_upload_response() -> String {
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
                  #("size", json.int(12)),
                  #(
                    "uploadUrl",
                    json.string("https://uploads.linear.app/presigned"),
                  ),
                  #(
                    "assetUrl",
                    json.string("https://uploads.linear.app/asset.md"),
                  ),
                  #("headers", json.preprocessed_array([])),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ]),
  )
}
