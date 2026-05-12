import gleam/option.{type Option, None, Some}
import scherzo/workflow_dag

pub type ValidatorPass {
  ValidatorPass
}

pub type ValidatorFailure {
  ValidatorFailure(
    validator_name: String,
    validator_type: String,
    code: String,
    message: String,
    retryable: Bool,
    diagnostic_summary: String,
    stdout_truncated: Bool,
    stderr_truncated: Bool,
  )
}

pub type ValidatorContext {
  ValidatorContext(
    config_dir: String,
    repository_root: String,
    run_root: String,
    workflow_id: String,
    run_id: String,
    step_id: String,
    attempt_index: Int,
    workspace_path: String,
    artifact_name: String,
    format: String,
    source_type: String,
    source_tool_name: Option(String),
    validator_name: String,
    validator_type: String,
    validator_index: Int,
  )
}

pub fn base_context(
  config_dir: String,
  repository_root: String,
  run_root: String,
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_path: String,
  artifact_name: String,
  format: String,
  source_type: String,
  source_tool_name: Option(String),
) -> ValidatorContext {
  ValidatorContext(
    config_dir: config_dir,
    repository_root: repository_root,
    run_root: run_root,
    workflow_id: workflow_id,
    run_id: run_id,
    step_id: step_id,
    attempt_index: attempt_index,
    workspace_path: workspace_path,
    artifact_name: artifact_name,
    format: format,
    source_type: source_type,
    source_tool_name: source_tool_name,
    validator_name: "",
    validator_type: "",
    validator_index: 0,
  )
}

pub fn for_validator(
  context: ValidatorContext,
  validator: workflow_dag.StructuredOutputValidator,
  index: Int,
) -> ValidatorContext {
  ValidatorContext(
    ..context,
    validator_name: workflow_dag.structured_output_validator_name(validator),
    validator_type: workflow_dag.structured_output_validator_type_to_string(
      validator,
    ),
    validator_index: index,
  )
}

pub fn source_tool_name_text(context: ValidatorContext) -> String {
  case context.source_tool_name {
    Some(value) -> value
    None -> ""
  }
}
