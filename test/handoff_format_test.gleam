import birl
import gleam/option.{type Option, None, Some}
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

fn result(
  final_response: Option(String),
  truncated: Bool,
  source: String,
) -> result_artifact.ResultArtifact {
  result_artifact.from_final_response(final_response, truncated, source)
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

fn options(include_result: Bool, attachment_filename: Option(String)) {
  handoff_format.SuccessCommentOptions(
    include_result: include_result,
    attachment_filename: attachment_filename,
  )
}

pub fn claim_comment_is_friendly_and_exact_test() {
  let body =
    handoff_format.claim_comment("LIV-38", "LIV-38--576460151305-2", [])

  assert body
    == "🛠️ Scherzo claimed this issue\n\n| Field | Value |\n| --- | --- |\n| Issue | `LIV-38` |\n| Run | `LIV-38--576460151305-2` |\n| Status | `claimed` |\n\n## Summary\nScherzo is starting work on `LIV-38`.\n\n## Next action\nNo action is needed right now. Scherzo will post another update when the run finishes, fails, or parks."
}

pub fn linear_handoff_comment_formatting_is_characterized_test() {
  let claim = handoff_format.claim_comment("LIV-266", "run-1", [])
  assert string.contains(claim, "🛠️ Scherzo claimed this issue")
  assert string.contains(claim, "| Issue | `LIV-266` |")
  assert string.contains(claim, "| Run | `run-1` |")

  let success_body =
    handoff_format.success_comment(
      issue(),
      success(result(Some("Implemented the fix."), False, "agent_end_messages")),
      "run-2",
      options(True, None),
      [],
    )
  assert string.contains(success_body, "✅ Scherzo completed the run")
  assert string.contains(success_body, "## What Scherzo did")
  assert string.contains(success_body, "## Token usage")

  let park =
    handoff_format.park_comment(
      "LIV-266",
      "operator hold",
      Some("explicit_unpark_only"),
      Some("run-3"),
      [],
    )
  assert string.contains(park, "⏸️ Scherzo parked this issue")
  assert string.contains(park, "| Reason | operator hold |")
  assert string.contains(park, "| Run | `run-3` |")
}

pub fn success_comment_includes_result_and_token_table_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result(Some("Implemented the fix."), False, "agent_end_messages")),
      "run-1",
      options(True, None),
      [],
    )

  assert string.contains(body, "✅ Scherzo completed the run")
  assert string.contains(body, "| Issue | `ABC-1` |")
  assert string.contains(body, "| Run | `run-1` |")
  assert string.contains(body, "| Classification | `terminal` |")
  assert string.contains(body, "## What Scherzo did")
  assert string.contains(body, "Implemented the fix.")
  assert string.contains(body, "- Turns: 2")
  assert string.contains(body, "- Result source: `agent_end_messages`")
  assert string.contains(body, "## Token usage")
  assert string.contains(body, "| Total | 100 |")
  assert !string.contains(body, "Metadata:")
}

pub fn success_comment_omits_inline_result_and_artifacts_when_disabled_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result(Some("Implemented the fix."), False, "agent_end_messages")),
      "run-1",
      options(False, None),
      [],
    )

  assert string.contains(body, "✅ Scherzo completed the run")
  assert !string.contains(body, "## What Scherzo did")
  assert !string.contains(body, "Implemented the fix.")
  assert !string.contains(body, "## Artifacts")
}

pub fn success_comment_reports_missing_and_truncated_result_text_test() {
  let missing =
    handoff_format.success_comment(
      issue(),
      success(result(None, False, "none")),
      "run-1",
      options(True, None),
      [],
    )
  assert string.contains(missing, "_No assistant result text was captured._")

  let truncated =
    handoff_format.success_comment(
      issue(),
      success(result(Some("partial"), True, "message_update_delta")),
      "run-1",
      options(True, None),
      [],
    )
  assert string.contains(truncated, "partial")
  assert string.contains(truncated, "_Result truncated by Scherzo._")
}

pub fn success_comment_redacts_tracker_secret_test() {
  let body =
    handoff_format.success_comment(
      issue(),
      success(result(Some("answer secret-key"), False, "agent_end_messages")),
      "run-1",
      options(True, None),
      ["secret-key"],
    )

  assert string.contains(body, "[REDACTED]")
  assert !string.contains(body, "secret-key")
}

pub fn success_result_filename_matches_attachment_expectations_test() {
  assert handoff_format.success_result_filename("ABC-1", "Run 2")
    == "abc-1-run-2-result.md"
  assert handoff_format.success_result_filename("!!!", "---")
    == "scherzo-result-result.md"
}

pub fn success_comment_with_attachment_intent_is_truthful_test() {
  let filename = "abc-1-run-attach-result.md"
  let without_inline =
    handoff_format.success_comment(
      issue(),
      success(result(Some("Implemented the fix."), False, "agent_end_messages")),
      "run-attach",
      options(False, Some(filename)),
      [],
    )
  assert string.contains(without_inline, "## Artifacts")
  assert string.contains(
    without_inline,
    "Scherzo will attempt to add `" <> filename <> "` to this comment",
  )
  assert !string.contains(without_inline, "attached file")
  assert !string.contains(without_inline, "## What Scherzo did")

  let with_inline =
    handoff_format.success_comment(
      issue(),
      success(result(Some("Implemented the fix."), False, "agent_end_messages")),
      "run-attach",
      options(True, Some(filename)),
      [],
    )
  assert string.contains(with_inline, "## What Scherzo did")
  assert string.contains(with_inline, "## Artifacts")
}

pub fn success_result_attachment_markdown_includes_result_metadata_and_redaction_test() {
  let assert Some(markdown) =
    handoff_format.success_result_attachment_markdown(
      issue(),
      success(result(Some("answer secret-key"), True, "agent_end_messages")),
      "run-attachment",
      ["secret-key"],
    )

  assert string.contains(markdown, "# Scherzo result for `ABC-1`")
  assert string.contains(markdown, "| Run | `run-attachment` |")
  assert string.contains(markdown, "## Result")
  assert string.contains(markdown, "answer [REDACTED]")
  assert !string.contains(markdown, "secret-key")
  assert string.contains(markdown, "_Result truncated by Scherzo._")
  assert string.contains(markdown, "## Run details")
  assert string.contains(markdown, "- Turns: 2")
  assert string.contains(markdown, "## Token usage")
}

pub fn success_result_attachment_markdown_returns_none_without_result_test() {
  assert handoff_format.success_result_attachment_markdown(
      issue(),
      success(result(None, False, "none")),
      "run-attachment",
      [],
    )
    == None
}

pub fn park_comment_includes_context_redacts_and_sanitizes_reason_test() {
  let body =
    handoff_format.park_comment(
      "ABC-1",
      "operator secret-key hold\nnext line",
      Some("explicit_unpark_only"),
      Some("run-park"),
      ["secret-key"],
    )

  assert string.contains(body, "⏸️ Scherzo parked this issue")
  assert string.contains(
    body,
    "| Reason | operator [REDACTED] hold next line |",
  )
  assert string.contains(body, "| Release policy | `explicit_unpark_only` |")
  assert string.contains(body, "| Run | `run-park` |")
  assert string.contains(body, "`scherzoctl unpark ABC-1`")
  assert !string.contains(body, "secret-key")
}

pub fn park_comment_truncates_long_reason_test() {
  let long_reason = string.repeat("x", times: 800) <> "SHOULD_NOT_APPEAR"
  let body =
    handoff_format.park_comment(
      "ABC-1",
      long_reason,
      Some("auto_unpark_on_issue_change"),
      None,
      [],
    )

  assert string.contains(body, "| Reason |")
  assert string.contains(body, "truncated")
  assert !string.contains(body, "SHOULD_NOT_APPEAR")
}
