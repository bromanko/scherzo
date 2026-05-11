import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/error
import scherzo/linear_comment_format as comment_format
import scherzo/path
import scherzo/session/tokens as session_tokens
import scherzo/tracker/issue as tracker_issue
import simplifile

const max_failure_detail_chars = 800

const failure_detail_truncated_suffix = "… [truncated]"

pub type SuccessCommentOptions {
  SuccessCommentOptions(
    include_result: Bool,
    attachment_filename: Option(String),
  )
}

pub fn claim_comment(
  issue_identifier: String,
  run_id: String,
  secrets: List(String),
) -> String {
  let body =
    [
      comment_format.title("🛠️", "Scherzo claimed this issue"),
      comment_format.summary_table([
        code_row("Issue", issue_identifier),
        code_row("Run", run_id),
        code_row("Status", "claimed"),
      ]),
      comment_format.section(
        "Summary",
        "Scherzo is starting work on "
          <> comment_format.code_span(issue_identifier, "this issue")
          <> ".",
      ),
      comment_format.section(
        "Next action",
        "No action is needed right now. Scherzo will post another update when the run finishes, fails, or parks.",
      ),
    ]
    |> join_blocks
  comment_format.finalize_body("claim_comment", body, secrets)
}

pub fn success_comment(
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  options: SuccessCommentOptions,
  secrets: List(String),
) -> String {
  let SuccessCommentOptions(include_result, attachment_filename) = options
  let blocks = [
    comment_format.title("✅", "Scherzo completed the run"),
    comment_format.summary_table([
      code_row("Issue", issue.identifier),
      code_row("Run", run_id),
      code_row("Status", "completed"),
      code_row(
        "Classification",
        classification_to_string(success.final_classification),
      ),
    ]),
    comment_format.section(
      "Summary",
      "Scherzo finished the run for "
        <> comment_format.code_span(issue.identifier, "this issue")
        <> ".",
    ),
  ]
  let blocks = case include_result {
    True ->
      list.append(blocks, [
        comment_format.section("What Scherzo did", result_body(success)),
      ])
    False -> blocks
  }
  let blocks = case attachment_filename {
    None -> blocks
    Some(filename) ->
      list.append(blocks, [
        comment_format.section(
          "Artifacts",
          "- Full result: Scherzo will attempt to add "
            <> comment_format.code_span(filename, "result.md")
            <> " to this comment. If fallback linking is used, a Markdown link appears below.",
        ),
      ])
  }
  let blocks =
    list.append(blocks, [
      run_details(success.turns, success.result.source),
      token_usage(success.tokens),
    ])
  let body = blocks |> join_blocks
  comment_format.finalize_body("success_comment", body, secrets)
}

pub fn success_result_filename(
  issue_identifier: String,
  run_id: String,
) -> String {
  let raw_base =
    safe_filename_component(issue_identifier)
    <> "-"
    <> safe_filename_component(run_id)
  let base =
    raw_base
    |> collapse_repeated_hyphens
    |> trim_hyphens
  let base = case base == "" {
    True -> "scherzo-result"
    False -> base
  }
  base <> "-result.md"
}

pub fn success_result_attachment_markdown(
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  secrets: List(String),
) -> Option(String) {
  case success.result.final_response {
    None -> None
    Some(_) -> {
      let body =
        [
          "# Scherzo result for "
            <> comment_format.code_span(issue.identifier, "this issue"),
          comment_format.summary_table([
            code_row("Issue", issue.identifier),
            code_row("Run", run_id),
            code_row(
              "Classification",
              classification_to_string(success.final_classification),
            ),
          ]),
          comment_format.section("Result", result_body(success)),
          run_details(success.turns, success.result.source),
          token_usage(success.tokens),
        ]
        |> join_blocks
      let body = comment_format.finalize_body("attachment_body", body, secrets)
      Some(body)
    }
  }
}

pub fn failure_comment(
  issue: tracker_issue.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
  secrets: List(String),
) -> String {
  let body = case failure.reason {
    error.WorkflowCommandFailed(code: code, step_id: step_id, ..) ->
      workflow_command_failure_comment(issue, failure, run_id, code, step_id)
    _ -> generic_failure_comment(issue, failure, run_id)
  }
  comment_format.finalize_body("failure_comment", body, secrets)
}

pub fn park_comment(
  issue_identifier: String,
  reason: String,
  release_policy: Option(String),
  run_id: Option(String),
  secrets: List(String),
) -> String {
  let rows = [
    code_row("Issue", issue_identifier),
    code_row("Status", "parked"),
    text_row("Reason", truncate_detail(reason)),
  ]
  let rows =
    list.append(
      rows,
      comment_format.optional_row("Release policy", release_policy),
    )
  let rows = list.append(rows, comment_format.optional_row("Run", run_id))
  let body =
    [
      comment_format.title("⏸️", "Scherzo parked this issue"),
      comment_format.summary_table(rows),
      comment_format.section(
        "Summary",
        "Scherzo paused automated work on "
          <> comment_format.code_span(issue_identifier, "this issue")
          <> " so it does not keep retrying an unsafe or blocked run.",
      ),
      comment_format.section(
        "Next action",
        "Inspect the recent Scherzo and Linear failure details. When the issue is safe to run again, use "
          <> comment_format.code_span(
          "scherzoctl unpark " <> issue_identifier,
          "scherzoctl unpark <issue>",
        )
          <> " or retry the workflow.",
      ),
    ]
    |> join_blocks
  comment_format.finalize_body("park_comment", body, secrets)
}

fn generic_failure_comment(
  issue: tracker_issue.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
) -> String {
  let diagnostic_rows = [
    code_row("Error code", error.agent_code(failure.reason)),
  ]
  let diagnostic_rows =
    list.append(
      diagnostic_rows,
      optional_code_row(
        "Underlying error",
        underlying_error_code(failure.reason),
      ),
    )
  let diagnostic_rows =
    list.append(diagnostic_rows, optional_workspace_row(failure.workspace_path))
  [
    comment_format.title("⚠️", "Scherzo run needs attention"),
    comment_format.summary_table([
      code_row("Issue", issue.identifier),
      code_row("Run", run_id),
      code_row("Status", "failed"),
      text_row("Error", friendly_error(failure.reason)),
    ]),
    comment_format.section(
      "Summary",
      "Scherzo stopped before completing this run.",
    ),
    comment_format.section(
      "Next action",
      "Inspect the failure diagnostics below, fix the underlying issue, then retry when safe.",
    ),
    failure_diagnostics_section(diagnostic_rows, failure),
    token_usage(failure.tokens),
  ]
  |> join_blocks
}

fn workflow_command_failure_comment(
  issue: tracker_issue.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
  code: String,
  step_id: String,
) -> String {
  let diagnostic_rows = [
    code_row("Failure code", code),
    code_row(
      "Retained workspace",
      retained_workspace_status(failure.workspace_path),
    ),
  ]
  let diagnostic_rows =
    list.append(diagnostic_rows, optional_workspace_row(failure.workspace_path))
  [
    comment_format.title("⚠️", "Scherzo workflow step needs attention"),
    comment_format.summary_table([
      code_row("Issue", issue.identifier),
      code_row("Run", run_id),
      code_row("Step", step_id),
      code_row("Error", code),
    ]),
    comment_format.section(
      "Summary",
      "Scherzo stopped during "
        <> comment_format.code_span(step_id, "workflow step")
        <> " because "
        <> comment_format.code_span(code, "workflow failure")
        <> " occurred.",
    ),
    comment_format.section("Next action", suggested_next_action(code)),
    failure_diagnostics_section(diagnostic_rows, failure),
    token_usage(failure.tokens),
  ]
  |> join_blocks
}

fn failure_diagnostics_section(
  rows: List(comment_format.SummaryRow),
  failure: agent_types.WorkerFailure,
) -> String {
  let table = comment_format.summary_table(rows)
  let body = case failure_detail(failure.reason, failure.workspace_path) {
    None -> table
    Some(detail) ->
      table <> "\n\n" <> comment_format.indented_block(truncate_detail(detail))
  }
  comment_format.section("Failure diagnostics", body)
}

fn friendly_error(reason: error.AgentRunnerError) -> String {
  case reason {
    error.PromptFailed(_) -> "Prompt failed"
    error.WorkspaceFailed(_) -> "Workspace setup failed"
    error.HookFailedError(_) -> "Hook failed"
    error.WorkflowHookFailed(_) -> "Workflow hook failed"
    error.ProbeFailed(_) | error.PiFailed(_) -> "Pi process failed"
    error.WorkflowCommandFailed(..) -> "Workflow command failed"
    error.StateRefreshFailed(_) -> "Tracker refresh failed"
    error.OperatorAbort -> "Operator stopped the run"
    error.OperatorStopAfterCurrentTurn -> "Operator stopped after this turn"
  }
}

fn run_details(turns: Int, source: String) -> String {
  comment_format.section(
    "Run details",
    "- Turns: "
      <> int.to_string(turns)
      <> "\n- Result source: "
      <> comment_format.code_span(source, "unknown"),
  )
}

fn token_usage(tokens: session_tokens.TokenTotals) -> String {
  comment_format.section(
    "Token usage",
    comment_format.token_usage_table(tokens),
  )
}

fn result_body(success: agent_types.WorkerSuccess) -> String {
  let result_text = case success.result.final_response {
    Some(text) ->
      comment_format.block_text(
        text,
        "_No assistant result text was captured._",
      )
    None -> "_No assistant result text was captured._"
  }
  let truncation_note = case success.result.truncated {
    True -> "\n\n_Result truncated by Scherzo._"
    False -> ""
  }
  result_text <> truncation_note
}

fn code_row(label: String, value: String) -> comment_format.SummaryRow {
  comment_format.SummaryRow(label, comment_format.table_code(value, "unknown"))
}

fn text_row(label: String, value: String) -> comment_format.SummaryRow {
  comment_format.SummaryRow(label, comment_format.table_text(value, "unknown"))
}

fn optional_code_row(
  label: String,
  value: Option(String),
) -> List(comment_format.SummaryRow) {
  case value {
    None -> []
    Some(value) -> [code_row(label, value)]
  }
}

fn optional_workspace_row(
  workspace_path: Option(String),
) -> List(comment_format.SummaryRow) {
  case display_workspace_path(workspace_path) {
    None -> []
    Some(path) -> [
      comment_format.SummaryRow("Workspace", workspace_value(path)),
    ]
  }
}

fn workspace_value(path: String) -> String {
  case string.starts_with(path, "_not shown") {
    True -> comment_format.table_text(path, "not shown")
    False -> comment_format.table_code(path, "unknown")
  }
}

fn join_blocks(blocks: List(String)) -> String {
  blocks |> string.join(with: "\n\n")
}

fn underlying_error_code(reason: error.AgentRunnerError) -> Option(String) {
  case reason {
    error.PromptFailed(template_error) ->
      Some(error.template_code(template_error))
    error.WorkspaceFailed(workspace_error) ->
      Some(error.workspace_code(workspace_error))
    error.HookFailedError(hook_error) -> Some(error.hook_code(hook_error))
    error.WorkflowHookFailed(hook_error) -> Some(error.hook_code(hook_error))
    error.ProbeFailed(pi_error) -> Some(error.pi_rpc_code(pi_error))
    error.PiFailed(pi_error) -> Some(error.pi_rpc_code(pi_error))
    error.WorkflowCommandFailed(..) -> None
    error.StateRefreshFailed(tracker_error) ->
      Some(error.tracker_code(tracker_error))
    error.OperatorAbort -> None
    error.OperatorStopAfterCurrentTurn -> None
  }
}

fn failure_detail(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
) -> Option(String) {
  case reason {
    error.PromptFailed(template_error) -> template_detail(template_error)
    error.WorkspaceFailed(workspace_error) -> workspace_detail(workspace_error)
    error.HookFailedError(hook_error) -> hook_detail(hook_error)
    error.WorkflowHookFailed(hook_error) -> hook_detail(hook_error)
    error.ProbeFailed(pi_error) -> pi_detail(pi_error)
    error.PiFailed(pi_error) -> pi_detail(pi_error)
    error.WorkflowCommandFailed(code: code, step_id: step_id, detail: detail) ->
      Some(workflow_command_detail(code, step_id, detail, workspace_path))
    error.StateRefreshFailed(tracker_error) -> tracker_detail(tracker_error)
    error.OperatorAbort -> Some("operator requested abort")
    error.OperatorStopAfterCurrentTurn ->
      Some("operator requested stop after current turn")
  }
}

fn template_detail(template_error: error.TemplateError) -> Option(String) {
  case template_error {
    error.TemplateRenderError(message) ->
      Some("template render error: " <> message)
  }
}

fn workspace_detail(workspace_error: error.WorkspaceError) -> Option(String) {
  case workspace_error {
    error.UnsafeWorkspaceKey(key) -> Some("unsafe workspace key: " <> key)
    error.WorkspaceOutsideRoot(path) ->
      Some("workspace path outside root: " <> path)
    error.WorkspaceCollision(path) -> Some("workspace collision: " <> path)
    error.WorkspaceIo(message) -> Some("workspace I/O error: " <> message)
    error.PartialWorkspace(message) -> Some("partial workspace: " <> message)
  }
}

fn hook_detail(hook_error: error.HookError) -> Option(String) {
  case hook_error {
    error.HookFailed(command, status, output) ->
      Some(
        "command "
        <> command
        <> " exited "
        <> int.to_string(status)
        <> ": "
        <> output,
      )
    error.HookTimedOut(command) -> Some("command " <> command <> " timed out")
    error.HookIo(message) -> Some(message)
  }
}

fn pi_detail(pi_error: error.PiRpcError) -> Option(String) {
  case pi_error {
    error.PiLaunchFailed(message) -> Some("launch failed: " <> message)
    error.PiMalformedJson(line) -> Some("pi emitted malformed JSON: " <> line)
    error.PiReadTimeout -> Some("timed out waiting for pi RPC response")
    error.PiTurnTimeout -> Some("pi turn timeout elapsed before agent_end")
    error.PiStallTimeout -> Some("pi stall timeout elapsed without output")
    error.PiExited(status) ->
      Some("pi process exited with status " <> int.to_string(status))
    error.PiProtocolError(message) -> Some("pi protocol error: " <> message)
    error.PiContextWindowExhausted(
      provider: _,
      provider_code: _,
      detail: detail,
    ) -> Some("pi context window exhausted: " <> detail)
  }
}

fn tracker_detail(tracker_error: error.TrackerError) -> Option(String) {
  case tracker_error {
    error.LinearApiRequest(message) ->
      Some("Linear API request failed: " <> message)
    error.LinearApiStatus(status) ->
      Some("Linear API returned status " <> int.to_string(status))
    error.LinearGraphqlErrors(message) ->
      Some("Linear GraphQL errors: " <> message)
    error.LinearUnknownPayload(payload) ->
      Some("Linear API returned unexpected payload: " <> payload)
    error.LinearMissingEndCursor ->
      Some("Linear API response missing endCursor")
    error.LinearUploadStatus(status) ->
      Some("Linear upload returned status " <> int.to_string(status))
    error.LinearAttachmentError(message) ->
      Some("Linear attachment error: " <> message)
  }
}

fn retained_workspace_status(workspace_path: Option(String)) -> String {
  case workspace_path {
    None -> "unknown"
    Some(raw_path) ->
      case string.trim(raw_path) == "" {
        True -> "unknown"
        False ->
          case
            simplifile.is_file(path.join(raw_path, ".scherzo-keep-workspace"))
          {
            Ok(True) -> "yes"
            _ -> "not_detected"
          }
      }
  }
}

fn suggested_next_action(code: String) -> String {
  case code {
    "prepare_plan_ambiguous" ->
      "Clarify the Linear issue so it references exactly one ExecPlan path, then retry the workflow."
    "base_refresh_conflict" ->
      "Refresh or reconcile the PR base, then retry the workflow."
    "publish_rebase_conflict" ->
      "Resolve the rebase conflicts in the retained workspace, rerun validation, then retry publish."
    "publish_revalidation_failed" ->
      "Inspect the post-rebase validation output in the retained workspace, fix the failures, then retry publish."
    _ ->
      "Inspect the retained workspace and command diagnostics, fix the failing command, then retry the workflow."
  }
}

fn workflow_command_detail(
  code: String,
  step_id: String,
  detail: String,
  workspace_path: Option(String),
) -> String {
  "workflow command step "
  <> step_id
  <> " failed with "
  <> code
  <> ": "
  <> sanitize_workspace_path_in_detail(detail, workspace_path)
}

fn sanitize_workspace_path_in_detail(
  detail: String,
  workspace_path: Option(String),
) -> String {
  case workspace_path, display_workspace_path(workspace_path) {
    Some(raw_path), Some(display_path) ->
      string.replace(detail, each: raw_path, with: display_path)
    _, _ -> detail
  }
}

fn display_workspace_path(workspace_path: Option(String)) -> Option(String) {
  case workspace_path {
    None -> None
    Some(raw_path) -> {
      let raw_path = string.trim(raw_path)
      case raw_path == "" {
        True -> None
        False ->
          case string.starts_with(raw_path, "/") {
            False -> Some(raw_path)
            True -> display_absolute_workspace_path(raw_path)
          }
      }
    }
  }
}

fn display_absolute_workspace_path(raw_path: String) -> Option(String) {
  case repo_relative_path(raw_path) {
    Some(relative) -> Some(relative)
    None ->
      case scherzo_workspace_relative_path(raw_path) {
        Some(relative) -> Some(relative)
        None ->
          Some(
            "_not shown because Scherzo recorded an absolute path outside the repository_",
          )
      }
  }
}

fn repo_relative_path(raw_path: String) -> Option(String) {
  case path.env("SCHERZO_REPO_ROOT") {
    Some(root) ->
      case relative_to_root(raw_path, root) {
        Some(relative) -> Some(relative)
        None -> cwd_relative_path(raw_path)
      }
    None -> cwd_relative_path(raw_path)
  }
}

fn cwd_relative_path(raw_path: String) -> Option(String) {
  case path.absolute(".") |> option.from_result {
    Some(root) -> relative_to_root(raw_path, root)
    None -> None
  }
}

fn relative_to_root(raw_path: String, root: String) -> Option(String) {
  let root_abs = path.absolute(root) |> result.unwrap(root)
  let root_abs = trim_trailing_slash(root_abs)
  case path.contains(root_abs, raw_path) {
    True ->
      case raw_path == root_abs {
        True -> Some(".")
        False -> Some(string.drop_start(raw_path, string.length(root_abs) + 1))
      }
    False -> None
  }
}

fn scherzo_workspace_relative_path(raw_path: String) -> Option(String) {
  case
    string.split_once(raw_path, on: "/.scherzo/workspaces/")
    |> option.from_result
  {
    Some(#(_, rest)) -> Some(".scherzo/workspaces/" <> rest)
    None -> None
  }
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}

fn truncate_detail(detail: String) -> String {
  case string.length(detail) > max_failure_detail_chars {
    True ->
      string.slice(detail, at_index: 0, length: max_failure_detail_chars)
      <> failure_detail_truncated_suffix
    False -> detail
  }
}

fn classification_to_string(
  classification: agent_types.FinalClassification,
) -> String {
  case classification {
    agent_types.FinalActive -> "active"
    agent_types.FinalTerminal -> "terminal"
    agent_types.FinalNonActive -> "non_active"
  }
}

fn safe_filename_component(value: String) -> String {
  value
  |> string.lowercase
  |> string.to_graphemes
  |> list.map(fn(ch) {
    case is_filename_char(ch) {
      True -> ch
      False -> "-"
    }
  })
  |> string.join(with: "")
}

fn is_filename_char(ch: String) -> Bool {
  is_between(ch, "a", "z")
  || is_between(ch, "0", "9")
  || ch == "."
  || ch == "_"
  || ch == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}

fn collapse_repeated_hyphens(value: String) -> String {
  value
  |> string.to_graphemes
  |> collapse_hyphen_graphemes(False, [])
  |> string.join(with: "")
}

fn collapse_hyphen_graphemes(
  graphemes: List(String),
  previous_was_hyphen: Bool,
  acc: List(String),
) -> List(String) {
  case graphemes {
    [] -> list.reverse(acc)
    ["-", ..rest] ->
      case previous_was_hyphen {
        True -> collapse_hyphen_graphemes(rest, True, acc)
        False -> collapse_hyphen_graphemes(rest, True, ["-", ..acc])
      }
    [ch, ..rest] -> collapse_hyphen_graphemes(rest, False, [ch, ..acc])
  }
}

fn trim_hyphens(value: String) -> String {
  value
  |> string.to_graphemes
  |> drop_leading_hyphens
  |> list.reverse
  |> drop_leading_hyphens
  |> list.reverse
  |> string.join(with: "")
}

fn drop_leading_hyphens(values: List(String)) -> List(String) {
  case values {
    ["-", ..rest] -> drop_leading_hyphens(rest)
    _ -> values
  }
}
