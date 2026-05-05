import birl
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/handoff_format
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

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

fn success(
  result: result_artifact.ResultArtifact,
) -> agent_types.WorkerSuccess {
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
      input: 10,
      output: 20,
      cache_read: 30,
      cache_write: 40,
      total: 100,
    ),
    turns: 2,
    result: result,
  )
}

pub fn success_comment_includes_result_and_metadata_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: Some("Implemented the fix."),
        truncated: False,
        source: "agent_end_messages",
      )),
      "run-1",
      True,
      [],
    )

  assert string.contains(body, "Scherzo completed run run-1 for ABC-1.")
  assert string.contains(body, "Result:")
  assert string.contains(body, "Implemented the fix.")
  assert string.contains(body, "Metadata:")
  assert string.contains(body, "- classification: terminal")
  assert string.contains(body, "- turns: 2")
  assert string.contains(
    body,
    "- tokens: input=10 output=20 cache_read=30 cache_write=40 total=100",
  )
}

pub fn success_comment_omits_result_when_disabled_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: Some("Implemented the fix."),
        truncated: False,
        source: "agent_end_messages",
      )),
      "run-1",
      False,
      [],
    )

  assert string.contains(body, "Metadata:")
  assert !string.contains(body, "Result:")
  assert !string.contains(body, "Implemented the fix.")
}

pub fn success_comment_marks_truncated_result_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: Some("partial"),
        truncated: True,
        source: "message_update_delta",
      )),
      "run-1",
      True,
      [],
    )

  assert string.contains(body, "partial")
  assert string.contains(body, "_Result truncated by Scherzo._")
}

pub fn success_comment_redacts_tracker_secret_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: Some("answer secret-key"),
        truncated: False,
        source: "agent_end_messages",
      )),
      "run-1",
      True,
      ["secret-key"],
    )

  assert string.contains(body, "[REDACTED]")
  assert !string.contains(body, "secret-key")
}

pub fn success_comment_reports_missing_result_text_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: None,
        truncated: False,
        source: "none",
      )),
      "run-1",
      True,
      [],
    )

  assert string.contains(body, "_No assistant result text was captured._")
}

pub fn success_result_attachment_markdown_includes_result_metadata_and_redaction_test() {
  let assert Some(markdown) =
    handoff_format.success_result_attachment_markdown(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: Some("answer secret-key"),
        truncated: True,
        source: "agent_end_messages",
      )),
      "run-attachment",
      ["secret-key"],
    )

  assert string.contains(
    markdown,
    "# Scherzo result for ABC-1 run run-attachment",
  )
  assert string.contains(markdown, "Result:")
  assert string.contains(markdown, "answer [REDACTED]")
  assert !string.contains(markdown, "secret-key")
  assert string.contains(markdown, "_Result truncated by Scherzo._")
  assert string.contains(markdown, "Metadata:")
  assert string.contains(markdown, "- classification: terminal")
  assert string.contains(markdown, "- turns: 2")
}

pub fn success_result_attachment_markdown_returns_none_without_result_test() {
  assert handoff_format.success_result_attachment_markdown(
      issue(),
      success(result_artifact.ResultArtifact(
        final_response: None,
        truncated: False,
        source: "none",
      )),
      "run-attachment",
      [],
    )
    == None
}
