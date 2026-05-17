import gleam/option.{Some}
import gleam/result
import scherzo/structured_output
import scherzo/structured_output_source
import scherzo/structured_output_validator
import scherzo/workflow_dag

pub type ArtifactValidationSpec {
  ArtifactValidationSpec(
    artifact_type: String,
    artifact_name: String,
    required_keys: List(String),
    validators: List(workflow_dag.StructuredOutputValidator),
  )
}

pub type ArtifactValidationError {
  ArtifactValidatorUnconfigured(artifact_type: String)
  ArtifactStructuredOutputError(error: structured_output.StructuredOutputError)
}

pub fn validate_json_artifact(
  spec: ArtifactValidationSpec,
  payload_json: String,
  context: structured_output_validator.ValidatorContext,
  secrets: List(String),
) -> Result(
  structured_output.StructuredOutputValidation,
  ArtifactValidationError,
) {
  case spec.validators {
    [] -> Error(ArtifactValidatorUnconfigured(spec.artifact_type))
    validators ->
      structured_output.validate_final_response(
        workflow_dag.StructuredOutputSpec(
          artifact_name: spec.artifact_name,
          required: True,
          source: structured_output_source.FinalResponseSource,
          format: workflow_dag.StructuredJson,
          schema: workflow_dag.StructuredObjectSchema(spec.required_keys),
          validators: validators,
          validation_retries: 0,
        ),
        Some(payload_json),
        False,
        secrets,
        structured_output.default_validator_runner(context, secrets),
      )
      |> result.map_error(ArtifactStructuredOutputError)
  }
}

pub fn error_code(error: ArtifactValidationError) -> String {
  case error {
    ArtifactValidatorUnconfigured(_) ->
      "workstream_artifact_validator_unconfigured"
    ArtifactStructuredOutputError(error) -> structured_output.error_code(error)
  }
}

pub fn error_message(error: ArtifactValidationError) -> String {
  case error {
    ArtifactValidatorUnconfigured(artifact_type) ->
      "no validator configured for workstream artifact type " <> artifact_type
    ArtifactStructuredOutputError(error) ->
      structured_output.error_message(error)
  }
}
