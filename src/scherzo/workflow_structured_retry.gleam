import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/error
import scherzo/log
import scherzo/result_artifact
import scherzo/step_artifact
import scherzo/workflow_dag

pub fn transient_native_lane_agent_failure_diagnostic(
  step_id: String,
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
        && is_native_review_lane_step(step_id, spec.artifact_name)
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
  let required_keys =
    workflow_dag.structured_output_schema_required_keys(spec.schema)
  let required_keys_text = case required_keys {
    [] -> "(no additional required keys)"
    _ -> string.join(required_keys, with: ", ")
  }
  let validator_text = case spec.validator {
    None -> "(none)"
    Some(validator) ->
      workflow_dag.structured_output_validator_to_string(validator)
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
  <> " structured output. This is the only automatic retry for this lane-output failure.\n"
  <> "Failure code: "
  <> failure_code
  <> "\nFailure summary: "
  <> log.truncate(diagnostic.message, 500)
  <> "\n\n"
  <> "Return JSON only: no Markdown, code fences, commentary, transcripts, or prior full responses.\n"
  <> "Use retained local context instead of large inline context:\n"
  <> "- run root: "
  <> run_root
  <> "\n- workspace: "
  <> workspace_path
  <> "\n- native review inputs, when present: "
  <> run_root
  <> "/artifacts/review/prepare_review\n"
  <> native_review_lane_retry_hint(step_id, spec.artifact_name)
  <> "Structured-output artifact name: "
  <> spec.artifact_name
  <> "\nRequired top-level keys: "
  <> required_keys_text
  <> "\nNamed validator: "
  <> validator_text
  <> "\nRemote mutations are forbidden; set remote_mutations to \"none\" when the artifact schema includes it."
}

fn is_native_review_lane_step(step_id: String, artifact_name: String) -> Bool {
  case native_review_lane_id(step_id, artifact_name) {
    Some(_) -> True
    None -> False
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

fn native_review_lane_retry_hint(
  step_id: String,
  artifact_name: String,
) -> String {
  case native_review_lane_id(step_id, artifact_name) {
    Some(lane_id) ->
      "Native review lane id: "
      <> lane_id
      <> ". Produce a review_lane_draft artifact for that lane.\n"
    None ->
      "If this is a native review lane, re-read the retained review artifacts and produce the lane draft JSON required by the original lane contract.\n"
  }
}

fn native_review_lane_id(
  step_id: String,
  artifact_name: String,
) -> Option(String) {
  case step_id, artifact_name {
    "lane_correctness", _ -> Some("correctness")
    "lane_test_quality", _ -> Some("test-quality")
    "lane_idioms_maintainability", _ -> Some("idioms-maintainability")
    "lane_security_performance", _ -> Some("security-performance")
    _, "correctness_draft" -> Some("correctness")
    _, "test_quality_draft" -> Some("test-quality")
    _, "idioms_maintainability_draft" -> Some("idioms-maintainability")
    _, "security_performance_draft" -> Some("security-performance")
    _, _ -> None
  }
}
