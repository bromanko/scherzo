import birl
import gleam/bit_array
import gleam/erlang/process
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear_attachment
import scherzo/path
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import simplifile
import test_async

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

fn handoff_config() -> config_types.HandoffConfig {
  config_types.HandoffConfig(
    enabled: True,
    comment_on_claim: True,
    comment_on_success: True,
    comment_on_failure: True,
    comment_on_park: True,
    claim_state_id: Some("claim-state"),
    success_state_id: None,
    failure_state_id: None,
    include_result_on_success: True,
    attach_result_on_success: False,
    attachment_fallback_to_markdown_link: True,
    result_max_chars: 8000,
  )
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-1",
    title: "Title",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn success() -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(
      tracker_issue.Issue(
        ..issue(),
        state: issue_state.from_string_unchecked("Done"),
      ),
    ),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace",
    tokens: session_tokens.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 0,
      cache_write: 0,
      total: 3,
    ),
    turns: 1,
    result: result_artifact.ResultArtifact(
      final_response: Some("Implemented secret-key"),
      truncated: False,
      source: "agent_end_messages",
    ),
  )
}

fn worker_failure(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
) -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: reason,
    workspace_path: workspace_path,
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
  )
}

fn capture_failure_comment(
  failure: agent_types.WorkerFailure,
  run_id: String,
) -> String {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    Ok(linear.Response(
      status: 200,
      body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
    ))
  }
  let client =
    handoff.linear_client(tracker_config(), handoff_config(), transport)

  assert client.report_failure(issue(), failure, run_id) == Ok(Nil)
  let assert Ok(failure_comment) = process.receive(subject, within: 100)
  failure_comment
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
  assert string.contains(success_comment, "## What Scherzo did")
  assert string.contains(success_comment, "Implemented [REDACTED]")
  assert string.contains(success_comment, "| Classification | `terminal` |")
  assert string.contains(success_comment, "## Token usage")
  assert string.contains(success_comment, "| Total | 3 |")
  assert !string.contains(success_comment, "secret-key")

  let failure =
    agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError(
        "secret-key blocked UI request",
      )),
      workspace_path: None,
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    )
  assert client.report_failure(issue(), failure, "run-3") == Ok(Nil)
  let assert Ok(failure_comment) = process.receive(subject, within: 100)
  assert string.contains(failure_comment, "run-3")
  assert string.contains(failure_comment, "Failure diagnostics")
  assert string.contains(failure_comment, "agent_pi_failed")
  assert string.contains(failure_comment, "pi_protocol_error")
  assert string.contains(failure_comment, "blocked UI request")
  assert !string.contains(failure_comment, "secret-key")

  assert client.report_park(handoff.ParkReport(
      issue_id: "issue-id",
      issue_identifier: "ABC-1",
      reason: "operator secret-key hold",
      release_policy: Some("explicit_unpark_only"),
      run_id: Some("run-4"),
    ))
    == Ok(Nil)
  let assert Ok(park_comment) = process.receive(subject, within: 100)
  assert string.contains(park_comment, "⏸️ Scherzo parked this issue")
  assert string.contains(park_comment, "| Reason | operator [REDACTED] hold |")
  assert string.contains(
    park_comment,
    "| Release policy | `explicit_unpark_only` |",
  )
  assert string.contains(park_comment, "| Run | `run-4` |")
  assert !string.contains(park_comment, "secret-key")
}

pub fn failure_handoff_includes_nested_pi_diagnostics_test() {
  let failure = worker_failure(error.PiFailed(error.PiExited(2)), None)
  let failure_comment = capture_failure_comment(failure, "run-pi-exit")

  assert string.contains(failure_comment, "Failure diagnostics")
  assert string.contains(failure_comment, "agent_pi_failed")
  assert string.contains(failure_comment, "pi_exited")
  assert string.contains(failure_comment, "status 2")
}

pub fn failure_handoff_includes_hook_diagnostics_test() {
  let failure =
    worker_failure(
      error.HookFailedError(error.HookFailed(
        "scripts/jj-workspace-after-create",
        17,
        "hook output",
      )),
      None,
    )
  let failure_comment = capture_failure_comment(failure, "run-hook")

  assert string.contains(failure_comment, "agent_hook_failed")
  assert string.contains(failure_comment, "hook_failed")
  assert string.contains(failure_comment, "scripts/jj-workspace-after-create")
  assert string.contains(failure_comment, "17")
  assert string.contains(failure_comment, "hook output")
}

pub fn failure_handoff_includes_workflow_hook_diagnostics_test() {
  let failure =
    worker_failure(
      error.WorkflowHookFailed(error.HookFailed(
        "before_step",
        23,
        "stderr details",
      )),
      None,
    )
  let failure_comment = capture_failure_comment(failure, "run-workflow-hook")

  assert string.contains(failure_comment, "workflow_hook_failed")
  assert string.contains(failure_comment, "hook_failed")
  assert string.contains(failure_comment, "before_step")
  assert string.contains(failure_comment, "23")
  assert string.contains(failure_comment, "stderr details")
}

pub fn failure_handoff_truncates_long_details_test() {
  let long_message = string.repeat("x", times: 800) <> "SHOULD_NOT_APPEAR"
  let failure =
    worker_failure(error.PiFailed(error.PiProtocolError(long_message)), None)
  let failure_comment = capture_failure_comment(failure, "run-long")

  assert string.contains(failure_comment, "pi_protocol_error")
  assert string.contains(failure_comment, "truncated")
  assert !string.contains(failure_comment, "SHOULD_NOT_APPEAR")
}

pub fn failure_handoff_escapes_control_characters_test() {
  let failure =
    worker_failure(
      error.PiFailed(error.PiProtocolError(
        "bad" <> "\u{0}" <> "stderr" <> "\u{1b}" <> "[31m\nnext line",
      )),
      None,
    )
  let failure_comment = capture_failure_comment(failure, "run-control")

  assert string.contains(failure_comment, "bad␀stderr␛[31m")
  assert string.contains(failure_comment, "next line")
  assert !string.contains(failure_comment, "\u{0}")
  assert !string.contains(failure_comment, "\u{1b}[31m")
}

pub fn failure_handoff_handles_workspace_path_safely_test() {
  let relative_workspace =
    "test/tmp/workflow-run/workspaces/implementation/ABC-123"
  let relative_failure =
    worker_failure(error.PiFailed(error.PiExited(1)), Some(relative_workspace))
  let relative_comment =
    capture_failure_comment(relative_failure, "run-relative-workspace")
  assert string.contains(relative_comment, relative_workspace)

  let absolute_workspace = "/" <> "operator-home/redacted-workspace"
  let absolute_failure =
    worker_failure(error.PiFailed(error.PiExited(1)), Some(absolute_workspace))
  let absolute_comment =
    capture_failure_comment(absolute_failure, "run-absolute-workspace")
  assert !string.contains(absolute_comment, "operator-home")
  assert string.contains(
    absolute_comment,
    "not shown because Scherzo recorded an absolute path",
  )
}

pub fn workflow_command_failure_handoff_renders_retained_workspace_context_test() {
  let relative_workspace =
    "test/tmp/handoff-retained/.scherzo/workspaces/implementation/ABC-1/run-1"
  let _ = simplifile.delete("test/tmp/handoff-retained")
  let assert Ok(Nil) = simplifile.create_directory_all(relative_workspace)
  let assert Ok(Nil) =
    simplifile.write(
      relative_workspace <> "/.scherzo-keep-workspace",
      "retained\n",
    )
  let assert Ok(absolute_workspace) = path.absolute(relative_workspace)
  let detail =
    "workflow_command_failed:publish_rebase_conflict\n"
    <> "workflow_step_failed\n"
    <> "command step failed: step=publish_pr failure_code=publish_rebase_conflict exit_code=1 stderr=conflict"
  let failure =
    worker_failure(
      error.WorkflowCommandFailed(
        code: "publish_rebase_conflict",
        step_id: "publish_pr",
        detail: detail,
      ),
      Some(absolute_workspace),
    )

  let failure_comment =
    capture_failure_comment(failure, "run-retained-workspace")

  assert string.contains(
    failure_comment,
    "| Error | `publish_rebase_conflict` |",
  )
  assert string.contains(failure_comment, "| Step | `publish_pr` |")
  assert string.contains(
    failure_comment,
    "| Failure code | `publish_rebase_conflict` |",
  )
  assert string.contains(failure_comment, "| Retained workspace | `yes` |")
  assert string.contains(failure_comment, "| Workspace | `.scherzo/workspaces/")
  assert string.contains(failure_comment, "Resolve the rebase conflicts")
  assert !string.contains(failure_comment, absolute_workspace)
  assert !string.contains(failure_comment, "agent_pi_failed")
  assert !string.contains(failure_comment, "pi_protocol_error")
}

pub fn workflow_command_failure_handoff_renders_revalidation_action_test() {
  let failure =
    worker_failure(
      error.WorkflowCommandFailed(
        code: "publish_revalidation_failed",
        step_id: "publish_pr",
        detail: "workflow_command_failed:publish_revalidation_failed\nvalidation failed",
      ),
      Some(".scherzo/workspaces/implementation/ABC-1/run-1"),
    )

  let failure_comment =
    capture_failure_comment(failure, "run-revalidation-failed")

  assert string.contains(
    failure_comment,
    "| Error | `publish_revalidation_failed` |",
  )
  assert string.contains(failure_comment, "| Step | `publish_pr` |")
  assert string.contains(
    failure_comment,
    "| Retained workspace | `not_detected` |",
  )
  assert string.contains(failure_comment, "post-rebase validation output")
  assert !string.contains(failure_comment, "agent_pi_failed")
  assert !string.contains(failure_comment, "pi_protocol_error")
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
    config_types.HandoffConfig(
      ..handoff_config(),
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
    )
  let client = handoff.linear_client(tracker_config(), no_state, transport)

  assert client.report_success(issue(), success(), "run-structured") == Ok(Nil)
  let assert Ok(success_comment) = process.receive(subject, within: 100)
  test_async.assert_no_extra_message_within(subject, 20)
  assert string.contains(success_comment, "commentCreate")
  assert string.contains(success_comment, "run-structured")
  assert string.contains(success_comment, "## What Scherzo did")
  assert string.contains(success_comment, "Implemented [REDACTED]")
}

pub fn success_handoff_with_attachment_uploads_result_to_created_comment_test() {
  let graphql_subject = process.new_subject()
  let upload_subject = process.new_subject()
  let client =
    handoff.linear_client_with_attachment_dependencies(
      tracker_config(),
      config_types.HandoffConfig(
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
  test_async.assert_no_extra_message_within(graphql_subject, 20)
  assert string.contains(comment_create, "ScherzoCommentCreate")
  assert string.contains(comment_create, "run-attach")
  assert string.contains(comment_create, "## Artifacts")
  assert string.contains(
    comment_create,
    "Scherzo will attempt to add `abc-1-run-attach-result.md`",
  )
  assert !string.contains(comment_create, "attached file")
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
      config_types.HandoffConfig(
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
  assert string.contains(comment_create, "## Artifacts")
  assert !string.contains(comment_create, "## What Scherzo did")
  assert !string.contains(comment_create, "Implemented [REDACTED]")
  assert string.contains(file_upload, "abc-1-run-2-result.md")
  assert string.contains(upload_markdown, "## Result")
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
      config_types.HandoffConfig(
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
  assert !string.contains(comment_create, "attached file")
  assert string.contains(comment_fetch, "ScherzoCommentFetch")
  assert string.contains(file_upload, "ScherzoFileUpload")
  test_async.assert_no_extra_message_within(graphql_subject, 20)
}

pub fn disabled_handoff_performs_no_transport_calls_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    Ok(linear.Response(status: 500, body: "{}"))
  }
  let disabled = config_types.HandoffConfig(..handoff_config(), enabled: False)
  let client = handoff.linear_client(tracker_config(), disabled, transport)

  assert client.claim_issue(issue(), "run-1") == Ok(Nil)
  assert client.report_success(issue(), success(), "run-2") == Ok(Nil)
  assert client.report_park(handoff.ParkReport(
      issue_id: "issue-id",
      issue_identifier: "ABC-1",
      reason: "operator_hold",
      release_policy: Some("explicit_unpark_only"),
      run_id: None,
    ))
    == Ok(Nil)
  test_async.assert_no_extra_message_within(subject, 20)
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
