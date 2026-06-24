import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/error
import scherzo/log
import scherzo/result_artifact
import scherzo/retry_policy
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
        && retry_policy.retry_budget_remaining(0, spec.validation_retries)
        && is_transient_pi_termination(failure.reason)
      {
        True ->
          Some(#(
            spec,
            step_artifact.StructuredOutputRetryDiagnostic(
              attempt: retry_policy.first_attempt_index(),
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
      "\nReview-lane submission contract reminders:\n"
      <> "- Call `submit_review_lane_draft` with only model-owned fields: `draft_findings`, `review_notes`, `evidence_requests`, and `self_check`.\n"
      <> "- Do not include runner-owned metadata such as `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, or `remote_mutations`; Scherzo injects those after capture.\n"
      <> "- Every `draft_findings` item must include non-empty `draft_finding_id`, `title`, `claim`, and `severity`, boolean `proposed_blocking`, list `locations`, and list `evidence_request_ids`. Finding locations must use repository-relative paths.\n"
      <> "- Every `review_notes` item must include non-empty `id`, `kind`, `category`, `severity`, `summary`, `details`, `suggested_action`, and list `locations`; category must be one of `correctness`, `maintainability`, `security`, `performance`, `testing`, `workflow`, `documentation`, `artifact_contract`, or `other` (use `testing`, not `test-quality`).\n"
      <> "- Every `evidence_requests` item must include non-empty `request_id`, `draft_finding_id`, `evidence_key`, `claim`, and `expected_observation`, plus object `target`; target may contain only `test_name`, `fixture_id`, `artifact_path`, `changed_file_path`, or `static_scan_rule` (do not include `command`, `suggested_test_file`, or `suggested_test_name`).\n"
    False -> ""
  }
}

fn is_review_lane_draft_validator(
  validator: workflow_dag.StructuredOutputValidator,
) -> Bool {
  case validator {
    workflow_dag.CommandValidator(name: name, argv: argv, ..) ->
      name == "review_lane_draft_compat"
      || name == "review_lane_draft"
      || name == "review_lane_semantics"
      || list.any(argv, fn(arg) { arg == "review_lane_draft" })
    workflow_dag.JsonSchemaValidator(path: path, ..) ->
      string.contains(path, "review-lane-draft")
  }
}

fn source_retry_instruction(spec: workflow_dag.StructuredOutputSpec) -> String {
  case spec.source {
    structured_output_source.FinalResponseSource ->
      "Return JSON only: no Markdown, code fences, commentary, transcripts, or prior full responses.\n"
    structured_output_source.PiToolCallSource(tool_name, _) ->
      "Call the Pi tool `"
      <> tool_name
      <> "` with the structured artifact as object-valued JSON arguments. Do not submit final assistant JSON instead.\n"
      <> "Submit exactly one `"
      <> tool_name
      <> "` call.\n"
      <> "Do not include sibling tool calls in the same assistant tool-call batch.\n"
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
