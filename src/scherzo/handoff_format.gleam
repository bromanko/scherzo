import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/domain
import scherzo/error
import scherzo/log

const max_failure_detail_chars = 500

const failure_detail_truncated_suffix = "… [truncated]"

pub fn success_comment(
  issue: domain.Issue,
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
  log.redact("comment_body", body, secrets)
}

pub fn success_result_attachment_markdown(
  issue: domain.Issue,
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
      Some(log.redact("attachment_body", body, secrets))
    }
  }
}

pub fn failure_comment(
  issue: domain.Issue,
  failure: agent_types.WorkerFailure,
  run_id: String,
  secrets: List(String),
) -> String {
  let header =
    "Scherzo failed run " <> run_id <> " for " <> issue.identifier <> "."
  let body = header <> "\n\n" <> failure_diagnostics(failure)
  log.redact("failure_comment", body, secrets)
}

fn failure_diagnostics(failure: agent_types.WorkerFailure) -> String {
  "Failure diagnostics:\n- error: "
  <> error.agent_code(failure.reason)
  <> underlying_error_line(failure.reason)
  <> detail_line(failure.reason)
  <> workspace_line(failure.workspace_path)
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
    error.ProbeFailed(pi_error) -> Some(error.pi_rpc_code(pi_error))
    error.PiFailed(pi_error) -> Some(error.pi_rpc_code(pi_error))
    error.StateRefreshFailed(tracker_error) ->
      Some(error.tracker_code(tracker_error))
    error.OperatorAbort -> None
    error.OperatorStopAfterCurrentTurn -> None
  }
}

fn detail_line(reason: error.AgentRunnerError) -> String {
  case failure_detail(reason) {
    None -> ""
    Some(detail) -> "\n- detail: " <> truncate_detail(detail)
  }
}

fn failure_detail(reason: error.AgentRunnerError) -> Option(String) {
  case reason {
    error.PromptFailed(template_error) -> template_detail(template_error)
    error.WorkspaceFailed(workspace_error) -> workspace_detail(workspace_error)
    error.HookFailedError(hook_error) -> hook_detail(hook_error)
    error.ProbeFailed(pi_error) -> pi_detail(pi_error)
    error.PiFailed(pi_error) -> pi_detail(pi_error)
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

fn workspace_line(workspace_path: Option(String)) -> String {
  case workspace_path {
    None -> ""
    Some(path) ->
      case path == "" {
        True -> ""
        False ->
          case string.starts_with(path, "/") {
            True ->
              "\n- workspace: _not shown because Scherzo recorded an absolute path_"
            False -> "\n- workspace: " <> path
          }
      }
  }
}

fn token_totals(tokens: domain.TokenTotals) -> String {
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
