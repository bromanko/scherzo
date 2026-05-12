import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/error
import scherzo/log
import scherzo/result_artifact
import scherzo/step_artifact
import scherzo/structured_output_source
import scherzo/workflow_dag

pub fn transient_agent_failure_diagnostic(
  spec: Option(workflow_dag.StructuredOutputSpec),
  failure: agent_types.WorkerFailure,
  secrets: List(String),
) -> Option(
  #(
    workflow_dag.StructuredOutputSpec,
    step_artifact.StructuredOutputRetryDiagnostic,
  ),
) {
  case spec {
    Some(spec) ->
      case
        spec.required
        && spec.validation_retries > 0
        && is_transient_pi_termination(failure.reason)
      {
        True ->
          Some(#(
            spec,
            step_artifact.StructuredOutputRetryDiagnostic(
              attempt: 1,
              status: "agent_failure",
              failure_code: Some(error.agent_code(failure.reason)),
              message: failure_message(failure, secrets),
            ),
          ))
        False -> None
      }
    None -> None
  }
}

pub fn agent_failure_as_success(
  failure: agent_types.WorkerFailure,
) -> agent_types.WorkerSuccess {
  let workspace_path = case failure.workspace_path {
    Some(path) -> path
    None -> ""
  }
  agent_types.WorkerSuccess(
    final_issue: failure.final_issue,
    final_classification: agent_types.FinalNonActive,
    workspace_path: workspace_path,
    tokens: failure.tokens,
    turns: 0,
    result: result_artifact.from_final_response(None, False, "agent_failure"),
  )
}

pub fn agent_failure_artifact_with_structured_output(
  artifact: step_artifact.StepArtifact,
  failure: agent_types.WorkerFailure,
  spec: workflow_dag.StructuredOutputSpec,
  secrets: List(String),
) -> step_artifact.StepArtifact {
  step_artifact.StepArtifact(
    ..artifact,
    structured_output: Some(step_artifact.StructuredOutputError(
      spec.artifact_name,
      workflow_dag.structured_output_format_to_string(spec.format),
      failure_message(failure, secrets),
      None,
      None,
    )),
  )
}

pub fn retry_info(
  spec: workflow_dag.StructuredOutputSpec,
  outcome: String,
  diagnostics: List(step_artifact.StructuredOutputRetryDiagnostic),
) -> step_artifact.StructuredOutputRetryInfo {
  step_artifact.StructuredOutputRetryInfo(
    max_retries: spec.validation_retries,
    attempts: list.length(diagnostics),
    outcome: outcome,
    diagnostics: diagnostics,
  )
}

pub fn failure_message(
  failure: agent_types.WorkerFailure,
  secrets: List(String),
) -> String {
  error.agent_artifact_detail(failure.reason)
  |> log.redact("structured_output_retry", _, secrets)
  |> log.truncate(1000)
}

pub fn retry_prompt(
  step_id: String,
  run_root: String,
  workspace_path: String,
  spec: workflow_dag.StructuredOutputSpec,
  diagnostic: step_artifact.StructuredOutputRetryDiagnostic,
) -> String {
  let format = workflow_dag.structured_output_format_to_string(spec.format)
  let workflow_dag.StructuredObjectSchema(required_keys) = spec.schema
  let required_keys_text = case required_keys {
    [] -> "(no additional required keys)"
    _ -> string.join(required_keys, with: ", ")
  }
  let failure_code = case diagnostic.failure_code {
    Some(code) -> code
    None -> "structured_output_validation_failed"
  }
  "Scherzo structured-output retry for workflow step `"
  <> step_id
  <> "`.\n\n"
  <> "The previous attempt did not produce valid retained "
  <> format
  <> " structured output. This is the only automatic retry for this structured-output failure.\n"
  <> "Failure code: "
  <> failure_code
  <> "\nFailure summary: "
  <> log.truncate(diagnostic.message, 500)
  <> "\n\n"
  <> source_retry_instruction(spec)
  <> "Use retained local context instead of large inline context:\n"
  <> "- run root: "
  <> run_root
  <> "\n- workspace: "
  <> workspace_path
  <> "\nStructured-output artifact name: "
  <> spec.artifact_name
  <> "\nRequired top-level keys: "
  <> required_keys_text
  <> validator_retry_instruction(spec.validators)
}

fn validator_retry_instruction(
  validators: List(workflow_dag.StructuredOutputValidator),
) -> String {
  case list.any(validators, is_review_lane_draft_validator) {
    True ->
      "\nReview-lane draft nested contract reminders:\n"
      <> "- `artifact_type` must be exactly `review_lane_draft` and `remote_mutations` must be exactly `none`.\n"
      <> "- Every `input_refs` item must be an object with non-empty `artifact_type` and repository- or run-root-relative `path`.\n"
      <> "- Every `draft_findings` item must include non-empty `draft_finding_id`, `title`, `claim`, and `severity`, boolean `proposed_blocking`, list `locations`, and list `evidence_request_ids`.\n"
      <> "- Every `review_notes` item must include non-empty `id`, `kind`, `category`, `severity`, `summary`, `details`, `suggested_action`, and list `locations`.\n"
      <> "- Every `evidence_requests` item must include non-empty `request_id`, `draft_finding_id`, `evidence_key`, `claim`, and `expected_observation`, plus object `target` (`target.changed_file_path` or `target.artifact_path` when applicable).\n"
    False -> ""
  }
}

fn is_review_lane_draft_validator(
  validator: workflow_dag.StructuredOutputValidator,
) -> Bool {
  workflow_dag.structured_output_validator_name(validator)
  == "review_lane_draft"
}

fn source_retry_instruction(spec: workflow_dag.StructuredOutputSpec) -> String {
  case spec.source {
    structured_output_source.FinalResponseSource ->
      "Return JSON only: no Markdown, code fences, commentary, transcripts, or prior full responses.\n"
    structured_output_source.PiToolCallSource(
      tool_name,
      require_single,
      reject_sibling_tool_calls,
    ) ->
      "Call the Pi tool `"
      <> tool_name
      <> "` with the structured artifact as object-valued JSON arguments. Do not submit final assistant JSON instead.\n"
      <> case require_single {
        True -> "Submit exactly one `" <> tool_name <> "` call.\n"
        False -> "Submit the configured Pi tool call.\n"
      }
      <> case reject_sibling_tool_calls {
        True ->
          "Do not include sibling tool calls in the same assistant tool-call batch.\n"
        False -> ""
      }
  }
}

fn is_transient_pi_termination(reason: error.AgentRunnerError) -> Bool {
  case reason {
    error.PiFailed(error.PiProtocolError(message)) -> {
      let normalized = string.lowercase(message)
      string.contains(normalized, "stopreason=error")
      && string.contains(normalized, "terminated")
    }
    _ -> False
  }
}
