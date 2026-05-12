import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/context_exhaustion
import scherzo/agent/context_recovery_prompt
import scherzo/error
import scherzo/log
import scherzo/state/artifact_store

const prompt_artifact_char_limit = 200_000

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
        "compaction_event_reasons",
        json.array(compaction_event_reasons, of: json.string),
      ),
      #("final_failure", optional_failure_diagnostic(final_failure)),
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
