import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/error
import scherzo/log
import scherzo/path
import scherzo/session/tokens as session_tokens
import scherzo/terminal/sanitize as terminal_sanitize
import scherzo/tracker/issue as tracker_issue
import simplifile

const max_failure_detail_chars = 800

const failure_detail_truncated_suffix = "… [truncated]"

pub fn success_comment(
  issue: tracker_issue.Issue,
  success: agent_types.WorkerSuccess,
  run_id: String,
  include_result: Bool,
  secrets: List(String),
) -> String {
  let header =
    "Scherzo completed run " <> run_id <> " for " <> issue.identifier <> "."
  let body = case include_result {
    True ->
      header <> "\n\n" <> result_section(success) <> "\n\n" <> metadata(success)
    False -> header <> "\n\n" <> metadata(success)
  }
  let body = sanitize_comment_body(body)
  log.redact("comment_body", body, secrets)
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
        "# Scherzo result for "
        <> issue.identifier
        <> " run "
        <> run_id
        <> "\n\n"
        <> result_section(success)
        <> "\n\n"
        <> metadata(success)
      let body = sanitize_comment_body(body)
      Some(log.redact("attachment_body", body, secrets))
    }
  }
}

pub fn failure_comment(
  issue: tracker_issue.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
  secrets: List(String),
) -> String {
  let header =
    "Scherzo failed run " <> run_id <> " for " <> issue.identifier <> "."
  let body = header <> "\n\n" <> failure_diagnostics(failure)
  let body = sanitize_comment_body(body)
  log.redact("failure_comment", body, secrets)
}

fn sanitize_comment_body(body: String) -> String {
  body
  |> terminal_sanitize.block_lines
  |> list.intersperse("\n")
  |> string.concat
}

fn failure_diagnostics(failure: agent_types.WorkerFailure) -> String {
  "Failure diagnostics:\n- error: "
  <> error.agent_code(failure.reason)
  <> workflow_command_lines(failure.reason)
  <> retained_workspace_line(failure.reason, failure.workspace_path)
  <> workspace_line(failure.workspace_path)
  <> suggested_next_action_line(failure.reason)
  <> underlying_error_line(failure.reason)
  <> detail_line(failure)
  <> "\n- tokens: "
  <> token_totals(failure.tokens)
}

fn underlying_error_line(reason: error.AgentRunnerError) -> String {
  case underlying_error_code(reason) {
    None -> ""
    Some(code) -> "\n- underlying_error: " <> code
  }
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

fn detail_line(failure: agent_types.WorkerFailure) -> String {
  case failure_detail(failure.reason, failure.workspace_path) {
    None -> ""
    Some(detail) -> "\n- detail: " <> truncate_detail(detail)
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

fn workflow_command_lines(reason: error.AgentRunnerError) -> String {
  case reason {
    error.WorkflowCommandFailed(code: code, step_id: step_id, ..) ->
      "\n- step: " <> step_id <> "\n- failure_code: " <> code
    _ -> ""
  }
}

fn retained_workspace_line(
  reason: error.AgentRunnerError,
  workspace_path: Option(String),
) -> String {
  case reason {
    error.WorkflowCommandFailed(..) ->
      "\n- retained_workspace: " <> retained_workspace_status(workspace_path)
    _ -> ""
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

fn suggested_next_action_line(reason: error.AgentRunnerError) -> String {
  case reason {
    error.WorkflowCommandFailed(code: code, ..) ->
      "\n- suggested_next_action: " <> suggested_next_action(code)
    _ -> ""
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

fn workspace_line(workspace_path: Option(String)) -> String {
  case display_workspace_path(workspace_path) {
    None -> ""
    Some(path) -> "\n- workspace: " <> path
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
  case path.absolute(".") {
    Ok(root) -> relative_to_root(raw_path, root)
    Error(_) -> None
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
  case string.split_once(raw_path, on: "/.scherzo/workspaces/") {
    Ok(#(_, rest)) -> Some(".scherzo/workspaces/" <> rest)
    Error(_) -> None
  }
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}

fn token_totals(tokens: session_tokens.TokenTotals) -> String {
  "input="
  <> int.to_string(tokens.input)
  <> " output="
  <> int.to_string(tokens.output)
  <> " cache_read="
  <> int.to_string(tokens.cache_read)
  <> " cache_write="
  <> int.to_string(tokens.cache_write)
  <> " total="
  <> int.to_string(tokens.total)
}

fn truncate_detail(detail: String) -> String {
  case string.length(detail) > max_failure_detail_chars {
    True ->
      string.slice(detail, at_index: 0, length: max_failure_detail_chars)
      <> failure_detail_truncated_suffix
    False -> detail
  }
}

fn result_section(success: agent_types.WorkerSuccess) -> String {
  let result_text = case success.result.final_response {
    Some(text) -> text
    None -> "_No assistant result text was captured._"
  }
  let truncation_note = case success.result.truncated {
    True -> "\n\n_Result truncated by Scherzo._"
    False -> ""
  }
  "Result:\n" <> result_text <> truncation_note
}

fn metadata(success: agent_types.WorkerSuccess) -> String {
  "Metadata:\n"
  <> "- classification: "
  <> classification_to_string(success.final_classification)
  <> "\n- turns: "
  <> int.to_string(success.turns)
  <> "\n- tokens: input="
  <> int.to_string(success.tokens.input)
  <> " output="
  <> int.to_string(success.tokens.output)
  <> " cache_read="
  <> int.to_string(success.tokens.cache_read)
  <> " cache_write="
  <> int.to_string(success.tokens.cache_write)
  <> " total="
  <> int.to_string(success.tokens.total)
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
