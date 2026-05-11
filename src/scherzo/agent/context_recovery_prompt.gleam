import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

pub type RecoveryMethod {
  PiRpcCompact
  FreshSession
  RecoveryDisabled
  NoRecovery
}

pub type RecoveryPromptInput {
  RecoveryPromptInput(
    workflow_id: String,
    run_id: String,
    step_id: String,
    step_attempt_index: Int,
    issue_identifier: Option(String),
    issue_title: Option(String),
    recovery_attempt: Int,
    recovery_method: RecoveryMethod,
    compaction_event_reasons: List(String),
    prompt_excerpt_ref: String,
    error_artifact_ref: String,
    prompt_excerpt_display_path: String,
    error_artifact_display_path: String,
    current_status: String,
    original_prompt_excerpt: String,
    max_chars: Int,
  )
}

pub fn recovery_method_to_string(method: RecoveryMethod) -> String {
  case method {
    PiRpcCompact -> "pi_rpc_compact"
    FreshSession -> "fresh_session"
    RecoveryDisabled -> "disabled"
    NoRecovery -> "none"
  }
}

pub fn build(input: RecoveryPromptInput) -> String {
  let method = recovery_method_to_string(input.recovery_method)
  let base =
    "Scherzo is retrying workflow agent step "
    <> input.step_id
    <> " after the model provider rejected the previous request as too large for its context window.\n\n"
    <> "Workflow: "
    <> input.workflow_id
    <> "\nRun: "
    <> input.run_id
    <> "\nStep attempt index: "
    <> int.to_string(input.step_attempt_index)
    <> "\nRecovery attempt: "
    <> int.to_string(input.recovery_attempt)
    <> "\nIssue: "
    <> issue_line(input.issue_identifier, input.issue_title)
    <> "\n\nRecovery method: "
    <> method
    <> method_explanation(input.recovery_method)
    <> compaction_line(input.compaction_event_reasons)
    <> "\n\nWork in the existing workspace. First run jj status --color=never and inspect the current files before editing. Do not restart from memory.\n\n"
    <> "Current jj status captured by Scherzo before recovery:\n"
    <> fenced(input.current_status)
    <> "\nThe redacted prompt evidence is retained at "
    <> input.prompt_excerpt_display_path
    <> " (artifact ref "
    <> input.prompt_excerpt_ref
    <> "). Read it in targeted chunks only when needed. The provider error is retained at "
    <> input.error_artifact_display_path
    <> " (artifact ref "
    <> input.error_artifact_ref
    <> ").\n\n"
    <> "Continue the step using compact context. Prefer reading repository files by path and line range over asking for large files to be inlined. If you cannot proceed safely, fail with a concise explanation rather than expanding the context again.\n\n"
    <> "Redacted excerpt of the oversized prompt follows. It may be truncated.\n\n"
  bounded_append(base, input.original_prompt_excerpt, input.max_chars)
}

fn method_explanation(method: RecoveryMethod) -> String {
  case method {
    PiRpcCompact ->
      ". Scherzo requested Pi RPC compaction before sending this prompt; any retained prior context should now be summarized by Pi."
    FreshSession ->
      ". Scherzo launched a fresh Pi session for this compact recovery prompt; do not assume prior Pi conversation history is available."
    RecoveryDisabled ->
      ". Automatic recovery is disabled; this prompt is diagnostic only."
    NoRecovery -> ". No automatic recovery method was selected."
  }
}

fn issue_line(identifier: Option(String), title: Option(String)) -> String {
  case identifier, title {
    Some(identifier), Some(title) -> identifier <> " " <> title
    Some(identifier), None -> identifier
    None, Some(title) -> title
    None, None -> "unknown"
  }
}

fn compaction_line(reasons: List(String)) -> String {
  case reasons {
    [] -> ""
    _ ->
      "\nCompaction event reasons observed: "
      <> string.join(reasons, with: ", ")
  }
}

fn fenced(contents: String) -> String {
  "```\n" <> contents <> "\n```\n"
}

fn bounded_append(base: String, excerpt: String, max_chars: Int) -> String {
  let max_chars = case max_chars <= 0 {
    True -> string.length(base)
    False -> max_chars
  }
  let suffix = "\n\n[recovery prompt excerpt truncated]\n"
  let remaining = max_chars - string.length(base)
  case remaining <= 0 {
    True -> string.slice(base, at_index: 0, length: max_chars)
    False ->
      case string.length(excerpt) <= remaining {
        True -> base <> excerpt
        False -> {
          let excerpt_limit = remaining - string.length(suffix)
          case excerpt_limit <= 0 {
            True -> string.slice(base, at_index: 0, length: max_chars)
            False ->
              base
              <> string.slice(excerpt, at_index: 0, length: excerpt_limit)
              <> suffix
          }
        }
      }
  }
}
