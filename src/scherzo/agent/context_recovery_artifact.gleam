import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/agent/context_recovery_prompt
import scherzo/config/types as config_types
import scherzo/error
import scherzo/log
import scherzo/pi/protocol
import scherzo/state/artifact_store
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_attempt

const prompt_artifact_char_limit = 200_000

const compact_error_detail_char_limit = 4000

const compact_response_raw_json_char_limit = 4000

pub type RecoveryArtifacts {
  RecoveryArtifacts(
    manifest_ref: String,
    prompt_excerpt_ref: String,
    error_ref: String,
    prompt_excerpt_display_path: String,
    error_display_path: String,
    prompt_excerpt: String,
    prompt_truncated: Bool,
  )
}

pub type RecoveryArtifactInput {
  RecoveryArtifactInput(
    store: artifact_store.Store,
    run_id: String,
    workflow_id: String,
    step_id: String,
    step_attempt_index: Int,
    pi_attempt: Int,
    context: context_exhaustion.ContextExhaustion,
    original_prompt: String,
    secrets: List(String),
    workspace_path: String,
    recovery_attempted: Bool,
    recovery_exhausted: Bool,
    recovery_method: context_recovery_prompt.RecoveryMethod,
  )
}

pub type RecoveryFailureDiagnostic {
  RecoveryFailureDiagnostic(
    scherzo_code: String,
    provider: Option(String),
    provider_code: Option(String),
    message: String,
  )
}

pub type TerminalExhaustionInput {
  TerminalExhaustionInput(
    store: artifact_store.Store,
    run_id: String,
    workflow_id: String,
    step_id: String,
    step_attempt_index: Int,
    pi_attempt: Int,
    context: context_exhaustion.ContextExhaustion,
    prompt_excerpt_ref: String,
    recovery_method: context_recovery_prompt.RecoveryMethod,
    failure: RecoveryFailureDiagnostic,
    result_ref: Option(String),
  )
}

pub type CompactionAttemptDiagnostic {
  CompactionAttemptDiagnostic(
    attempted: Bool,
    outcome: String,
    fallback_reason: Option(String),
    error_code: Option(String),
    error_detail: Option(String),
    error_detail_truncated: Bool,
    event_reasons: List(String),
    event_count: Int,
    response_type: Option(String),
    response_command: Option(String),
    response_success: Option(Bool),
    response_raw_json: Option(String),
    response_raw_json_truncated: Bool,
  )
}

pub fn write_initial(
  input: RecoveryArtifactInput,
) -> Result(RecoveryArtifacts, artifact_store.ArtifactError) {
  let #(excerpt, truncated) =
    redacted_excerpt(
      input.original_prompt,
      input.secrets,
      input.workspace_path,
      prompt_artifact_char_limit,
    )
  let manifest = manifest_json(input.original_prompt, excerpt, truncated)
  use manifest_ref <- result_try(write_artifact(
    input,
    "manifest.json",
    manifest,
  ))
  use prompt_ref <- result_try(write_artifact(
    input,
    "attempt-1-prompt-excerpt.md",
    prompt_excerpt_markdown(excerpt, truncated),
  ))
  use error_ref <- result_try(write_artifact(
    input,
    "context-window-exhausted.json",
    provider_error_json(input, prompt_ref.ref),
  ))
  Ok(RecoveryArtifacts(
    manifest_ref: manifest_ref.ref,
    prompt_excerpt_ref: prompt_ref.ref,
    error_ref: error_ref.ref,
    prompt_excerpt_display_path: artifact_store.context_recovery_display_path(
      prompt_ref.ref,
    ),
    error_display_path: artifact_store.context_recovery_display_path(
      error_ref.ref,
    ),
    prompt_excerpt: excerpt,
    prompt_truncated: truncated,
  ))
}

pub fn write_recovery_prompt(
  store: artifact_store.Store,
  run_id: String,
  workflow_id: String,
  step_id: String,
  step_attempt_index: Int,
  prompt: String,
) -> Result(artifact_store.StructuredArtifactRef, artifact_store.ArtifactError) {
  artifact_store.write_context_recovery_artifact(
    store,
    run_id,
    workflow_id,
    step_id,
    step_attempt_index,
    "attempt-2-recovery-prompt.md",
    prompt,
  )
}

pub fn write_result(
  store: artifact_store.Store,
  run_id: String,
  workflow_id: String,
  step_id: String,
  step_attempt_index: Int,
  outcome: String,
  recovery_method: context_recovery_prompt.RecoveryMethod,
  recovery_exhausted: Bool,
  compaction_event_reasons: List(String),
  compaction_attempt: CompactionAttemptDiagnostic,
  final_failure: Option(RecoveryFailureDiagnostic),
) -> Result(artifact_store.StructuredArtifactRef, artifact_store.ArtifactError) {
  let contents =
    json.object([
      #("schema_version", json.int(1)),
      #("artifact_type", json.string("context_recovery_result")),
      #("outcome", json.string(outcome)),
      #("recovery_attempted", json.bool(True)),
      #("recovery_exhausted", json.bool(recovery_exhausted)),
      #("budget_exhausted", json.bool(recovery_exhausted)),
      #(
        "recovery_method",
        json.string(context_recovery_prompt.recovery_method_to_string(
          recovery_method,
        )),
      ),
      #(
        "fallback_from_method",
        optional_string(fallback_from_method(
          recovery_method,
          compaction_attempt,
        )),
      ),
      #(
        "fallback_reason",
        optional_string(fallback_reason(recovery_method, compaction_attempt)),
      ),
      #(
        "compaction_event_reasons",
        json.array(compaction_event_reasons, of: json.string),
      ),
      #("final_failure", optional_failure_diagnostic(final_failure)),
      #("compact_rpc", compaction_attempt_json(compaction_attempt)),
    ])
    |> json.to_string
  artifact_store.write_context_recovery_artifact(
    store,
    run_id,
    workflow_id,
    step_id,
    step_attempt_index,
    "attempt-2-result.json",
    contents,
  )
}

pub fn write_terminal_exhausted(
  input: TerminalExhaustionInput,
) -> Result(artifact_store.StructuredArtifactRef, artifact_store.ArtifactError) {
  artifact_store.write_context_recovery_artifact(
    input.store,
    input.run_id,
    input.workflow_id,
    input.step_id,
    input.step_attempt_index,
    "context-window-exhausted.json",
    terminal_exhausted_json(input),
  )
}

pub fn failure_diagnostic(
  reason: error.AgentRunnerError,
) -> RecoveryFailureDiagnostic {
  case reason {
    error.ContextRecoveryExhausted(final_error: final_error, ..) ->
      pi_failure_diagnostic(error.agent_code(reason), final_error)
    error.PiFailed(pi_error) ->
      pi_failure_diagnostic(error.agent_code(reason), pi_error)
    error.ProbeFailed(pi_error) ->
      pi_failure_diagnostic(error.agent_code(reason), pi_error)
    _ ->
      RecoveryFailureDiagnostic(
        scherzo_code: error.agent_code(reason),
        provider: None,
        provider_code: None,
        message: string.trim(error.agent_detail_suffix(reason)),
      )
  }
}

pub fn compaction_succeeded(
  event_reasons: List(String),
  event_count: Int,
) -> CompactionAttemptDiagnostic {
  CompactionAttemptDiagnostic(
    attempted: True,
    outcome: "succeeded",
    fallback_reason: None,
    error_code: None,
    error_detail: None,
    error_detail_truncated: False,
    event_reasons: event_reasons,
    event_count: event_count,
    response_type: None,
    response_command: None,
    response_success: None,
    response_raw_json: None,
    response_raw_json_truncated: False,
  )
}

pub fn compaction_failed(
  err: error.PiRpcError,
  event_reasons: List(String),
  event_count: Int,
  response: Option(protocol.RpcRecord),
  secrets: List(String),
  workspace_path: String,
) -> CompactionAttemptDiagnostic {
  let #(detail, detail_truncated) =
    bounded_redacted(
      error.pi_rpc_detail(err),
      secrets,
      workspace_path,
      compact_error_detail_char_limit,
    )
  let #(raw_json, raw_json_truncated) =
    bounded_response_raw_json(response, secrets, workspace_path)
  CompactionAttemptDiagnostic(
    attempted: True,
    outcome: "failed",
    fallback_reason: Some(compact_fallback_reason(err)),
    error_code: Some(error.pi_rpc_code(err)),
    error_detail: Some(detail),
    error_detail_truncated: detail_truncated,
    event_reasons: event_reasons,
    event_count: event_count,
    response_type: response_type(response),
    response_command: response_command(response),
    response_success: response_success(response),
    response_raw_json: raw_json,
    response_raw_json_truncated: raw_json_truncated,
  )
}

pub fn compaction_event_reasons(
  records: List(protocol.RpcRecord),
) -> List(String) {
  records
  |> list.filter_map(fn(record) {
    protocol.compaction_reason(record) |> option_to_result
  })
}

pub fn compaction_event_count(records: List(protocol.RpcRecord)) -> Int {
  records
  |> list.filter(fn(record) {
    record.type_ == "compaction_start" || record.type_ == "compaction_end"
  })
  |> list.length
}

pub fn fresh_session_recovery_message(
  step_id: String,
  compaction_attempt: CompactionAttemptDiagnostic,
) -> String {
  let fallback_reason = case compaction_attempt.fallback_reason {
    Some(reason) -> reason
    None -> "compact_rpc_failed"
  }
  let error_code = case compaction_attempt.error_code {
    Some(code) -> " (" <> code <> ")"
    None -> ""
  }
  "context window exhausted; Pi RPC compaction fallback reason "
  <> fallback_reason
  <> error_code
  <> "; retrying step "
  <> step_id
  <> " in a fresh compact session"
}

pub fn build_recovery_prompt(
  issue: tracker_issue.Issue,
  config: config_types.EffectiveConfig,
  attempt_context: workflow_attempt.StepAttemptContext,
  recovery_attempt: Int,
  method: context_recovery_prompt.RecoveryMethod,
  compaction_event_reasons: List(String),
  artifacts: RecoveryArtifacts,
) -> String {
  context_recovery_prompt.build(context_recovery_prompt.RecoveryPromptInput(
    workflow_id: attempt_context.workflow_id,
    run_id: attempt_context.run_id,
    step_id: attempt_context.step_id,
    step_attempt_index: attempt_context.attempt_index,
    issue_identifier: Some(issue.identifier),
    issue_title: Some(issue.title),
    recovery_attempt: recovery_attempt,
    recovery_method: method,
    compaction_event_reasons: compaction_event_reasons,
    prompt_excerpt_ref: artifacts.prompt_excerpt_ref,
    error_artifact_ref: artifacts.error_ref,
    prompt_excerpt_display_path: artifacts.prompt_excerpt_display_path,
    error_artifact_display_path: artifacts.error_display_path,
    current_status: "Scherzo did not preload full status; run jj status --color=never before editing.",
    original_prompt_excerpt: artifacts.prompt_excerpt,
    max_chars: config.agent.context_recovery_prompt_char_limit,
  ))
}

fn option_to_result(value: Option(a)) -> Result(a, Nil) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(Nil)
  }
}

fn write_artifact(
  input: RecoveryArtifactInput,
  artifact_name: String,
  contents: String,
) -> Result(artifact_store.StructuredArtifactRef, artifact_store.ArtifactError) {
  artifact_store.write_context_recovery_artifact(
    input.store,
    input.run_id,
    input.workflow_id,
    input.step_id,
    input.step_attempt_index,
    artifact_name,
    contents,
  )
}

fn response_type(response: Option(protocol.RpcRecord)) -> Option(String) {
  case response {
    Some(record) -> Some(record.type_)
    None -> None
  }
}

fn response_command(response: Option(protocol.RpcRecord)) -> Option(String) {
  case response {
    Some(record) -> record.command
    None -> None
  }
}

fn response_success(response: Option(protocol.RpcRecord)) -> Option(Bool) {
  case response {
    Some(record) -> record.success
    None -> None
  }
}

fn fallback_from_method(
  recovery_method: context_recovery_prompt.RecoveryMethod,
  compaction_attempt: CompactionAttemptDiagnostic,
) -> Option(String) {
  case recovery_method, compaction_attempt.attempted {
    context_recovery_prompt.FreshSession, True ->
      Some(context_recovery_prompt.recovery_method_to_string(
        context_recovery_prompt.PiRpcCompact,
      ))
    _, _ -> None
  }
}

fn fallback_reason(
  recovery_method: context_recovery_prompt.RecoveryMethod,
  compaction_attempt: CompactionAttemptDiagnostic,
) -> Option(String) {
  case recovery_method {
    context_recovery_prompt.FreshSession -> compaction_attempt.fallback_reason
    _ -> None
  }
}

fn compaction_attempt_json(
  diagnostic: CompactionAttemptDiagnostic,
) -> json.Json {
  json.object([
    #("attempted", json.bool(diagnostic.attempted)),
    #("outcome", json.string(diagnostic.outcome)),
    #("fallback_reason", optional_string(diagnostic.fallback_reason)),
    #("error_code", optional_string(diagnostic.error_code)),
    #("error_detail", optional_string(diagnostic.error_detail)),
    #("error_detail_truncated", json.bool(diagnostic.error_detail_truncated)),
    #(
      "compaction_event_reasons",
      json.array(diagnostic.event_reasons, of: json.string),
    ),
    #("compaction_events_observed", json.int(diagnostic.event_count)),
    #("response", compaction_response_json(diagnostic)),
  ])
}

fn compaction_response_json(
  diagnostic: CompactionAttemptDiagnostic,
) -> json.Json {
  case
    diagnostic.response_type,
    diagnostic.response_command,
    diagnostic.response_success,
    diagnostic.response_raw_json
  {
    None, None, None, None -> json.null()
    _, _, _, _ ->
      json.object([
        #("type", optional_string(diagnostic.response_type)),
        #("command", optional_string(diagnostic.response_command)),
        #("success", optional_bool(diagnostic.response_success)),
        #("raw_json", optional_string(diagnostic.response_raw_json)),
        #(
          "raw_json_truncated",
          json.bool(diagnostic.response_raw_json_truncated),
        ),
      ])
  }
}

fn compact_fallback_reason(err: error.PiRpcError) -> String {
  case err {
    error.PiReadTimeout | error.PiTurnTimeout | error.PiStallTimeout ->
      "compact_rpc_timed_out"
    error.PiExited(_) -> "compact_rpc_process_exited"
    error.PiLaunchFailed(_) -> "compact_rpc_unavailable"
    error.PiMalformedJson(_) -> "compact_rpc_protocol_error"
    error.PiContextWindowExhausted(..) -> "compact_rpc_context_window_exhausted"
    error.PiProtocolError(message) ->
      case compact_unsupported_message(message) {
        True -> "compact_rpc_unavailable"
        False -> "compact_rpc_failed"
      }
  }
}

fn compact_unsupported_message(message: String) -> Bool {
  let lower = string.lowercase(message)
  string.contains(lower, "unknown command")
  || string.contains(lower, "unsupported")
  || string.contains(lower, "unrecognized")
}

fn bounded_response_raw_json(
  response: Option(protocol.RpcRecord),
  secrets: List(String),
  workspace_path: String,
) -> #(Option(String), Bool) {
  case response {
    None -> #(None, False)
    Some(record) -> {
      let #(value, truncated) =
        bounded_redacted(
          record.raw_json,
          secrets,
          workspace_path,
          compact_response_raw_json_char_limit,
        )
      #(Some(value), truncated)
    }
  }
}

fn bounded_redacted(
  text: String,
  secrets: List(String),
  workspace_path: String,
  limit: Int,
) -> #(String, Bool) {
  let redacted =
    log.redact("context_recovery_compact", text, secrets)
    |> redact_local_path_placeholder
    |> redact_workspace_path(workspace_path)
  case string.length(redacted) > limit {
    True -> #(log.truncate(redacted, limit), True)
    False -> #(redacted, False)
  }
}

fn redacted_excerpt(
  prompt: String,
  secrets: List(String),
  workspace_path: String,
  limit: Int,
) -> #(String, Bool) {
  let redacted =
    log.redact("prompt", prompt, secrets)
    |> redact_local_path_placeholder
    |> redact_workspace_path(workspace_path)
  case string.length(redacted) > limit {
    True -> #(string.slice(redacted, at_index: 0, length: limit), True)
    False -> #(redacted, False)
  }
}

fn redact_local_path_placeholder(text: String) -> String {
  string.replace(
    text,
    each: "<absolute-local-path>",
    with: "[REDACTED_LOCAL_PATH]",
  )
}

fn redact_workspace_path(text: String, workspace_path: String) -> String {
  case string.trim(workspace_path) == "" {
    True -> text
    False ->
      string.replace(text, each: workspace_path, with: "[REDACTED_WORKSPACE]")
  }
}

fn prompt_excerpt_markdown(excerpt: String, truncated: Bool) -> String {
  "# Context recovery prompt evidence\n\n"
  <> "Redacted: true\n"
  <> "Truncated: "
  <> bool_string(truncated)
  <> "\n\n"
  <> excerpt
}

fn manifest_json(
  original_prompt: String,
  excerpt: String,
  truncated: Bool,
) -> String {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("context_recovery")),
    #("prompt_evidence_redacted", json.bool(True)),
    #("prompt_evidence_truncated", json.bool(truncated)),
    #(
      "prompt_evidence_original_chars",
      json.int(string.length(original_prompt)),
    ),
    #("prompt_evidence_stored_chars", json.int(string.length(excerpt))),
  ])
  |> json.to_string
}

fn provider_error_json(
  input: RecoveryArtifactInput,
  prompt_ref: String,
) -> String {
  let context = input.context
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("context_window_exhausted")),
    #("scherzo_code", json.string("pi_context_window_exhausted")),
    #("workflow_id", json.string(input.workflow_id)),
    #("run_id", json.string(input.run_id)),
    #("step_id", json.string(input.step_id)),
    #("step_attempt_index", json.int(input.step_attempt_index)),
    #("pi_attempt", json.int(input.pi_attempt)),
    #("recovery_attempted", json.bool(input.recovery_attempted)),
    #("recovery_exhausted", json.bool(input.recovery_exhausted)),
    #(
      "recovery_method",
      json.string(context_recovery_prompt.recovery_method_to_string(
        input.recovery_method,
      )),
    ),
    #("provider", optional_string(context.provider)),
    #("provider_code", optional_string(context.provider_code)),
    #("message", json.string(context.message)),
    #(
      "artifact_refs",
      json.object([#("prompt_excerpt", json.string(prompt_ref))]),
    ),
  ])
  |> json.to_string
}

fn terminal_exhausted_json(input: TerminalExhaustionInput) -> String {
  let context = input.context
  let recovery_method =
    context_recovery_prompt.recovery_method_to_string(input.recovery_method)
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("context_window_exhausted")),
    #("scherzo_code", json.string("pi_context_window_exhausted")),
    #("workflow_id", json.string(input.workflow_id)),
    #("run_id", json.string(input.run_id)),
    #("step_id", json.string(input.step_id)),
    #("step_attempt_index", json.int(input.step_attempt_index)),
    #("pi_attempt", json.int(input.pi_attempt)),
    #("recovery_attempted", json.bool(True)),
    #("recovery_exhausted", json.bool(True)),
    #("budget_exhausted", json.bool(True)),
    #("recovery_outcome", json.string("failed")),
    #("terminal_outcome", json.string("failed")),
    #("recovery_method", json.string(recovery_method)),
    #("terminal_recovery_method", json.string(recovery_method)),
    #("provider", optional_string(context.provider)),
    #("provider_code", optional_string(context.provider_code)),
    #("message", json.string(context.message)),
    #("final_failure", failure_diagnostic_json(input.failure)),
    #(
      "artifact_refs",
      json.object(terminal_artifact_refs(
        input.prompt_excerpt_ref,
        input.result_ref,
      )),
    ),
  ])
  |> json.to_string
}

fn terminal_artifact_refs(
  prompt_ref: String,
  result_ref: Option(String),
) -> List(#(String, json.Json)) {
  let prompt_entry = #("prompt_excerpt", json.string(prompt_ref))
  case result_ref {
    Some(result_ref) -> [
      prompt_entry,
      #("recovery_result", json.string(result_ref)),
    ]
    None -> [prompt_entry]
  }
}

fn optional_failure_diagnostic(
  failure: Option(RecoveryFailureDiagnostic),
) -> json.Json {
  case failure {
    Some(failure) -> failure_diagnostic_json(failure)
    None -> json.null()
  }
}

fn failure_diagnostic_json(failure: RecoveryFailureDiagnostic) -> json.Json {
  json.object([
    #("scherzo_code", json.string(failure.scherzo_code)),
    #("provider", optional_string(failure.provider)),
    #("provider_code", optional_string(failure.provider_code)),
    #("message", json.string(failure.message)),
  ])
}

fn pi_failure_diagnostic(
  scherzo_code: String,
  pi_error: error.PiRpcError,
) -> RecoveryFailureDiagnostic {
  case pi_error {
    error.PiContextWindowExhausted(provider, provider_code, detail) ->
      RecoveryFailureDiagnostic(
        scherzo_code: scherzo_code,
        provider: provider,
        provider_code: provider_code,
        message: detail,
      )
    _ ->
      RecoveryFailureDiagnostic(
        scherzo_code: scherzo_code,
        provider: None,
        provider_code: None,
        message: error.pi_rpc_detail(pi_error),
      )
  }
}

fn optional_string(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn optional_bool(value: Option(Bool)) -> json.Json {
  case value {
    Some(value) -> json.bool(value)
    None -> json.null()
  }
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn result_try(
  result: Result(a, artifact_store.ArtifactError),
  next: fn(a) -> Result(b, artifact_store.ArtifactError),
) -> Result(b, artifact_store.ArtifactError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
