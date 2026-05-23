import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/log
import scherzo/result_artifact
import scherzo/step_artifact
import scherzo/structured_output_source
import scherzo/structured_output_tool_spec
import scherzo/workflow_dag

pub const tool_name = "submit_workflow_step_recovery_result"

pub const artifact_name = "workflow_step_recovery_result"

pub const artifact_type = "workflow_step_recovery_result"

pub const schema_version = 1

pub const provider_schema_path = ".scherzo/workflows/schemas/provider/workflow-step-recovery-result.v1.schema.json"

pub const canonical_schema_path = ".scherzo/workflows/schemas/workflow-step-recovery-result.v1.schema.json"

pub type Decision {
  RetryRequested(summary: String, reason: String)
  GaveUp(summary: String, reason: String)
}

pub type ProtocolError {
  ProtocolError(code: String, message: String)
}

type Payload {
  Payload(
    schema_version: Int,
    artifact_type: String,
    decision: String,
    summary: String,
    reason: String,
  )
}

pub fn describe_error(error: ProtocolError) -> String {
  let ProtocolError(code, _) = error
  code
}

pub fn error_message(error: ProtocolError) -> String {
  let ProtocolError(_, message) = error
  message
}

pub fn prompt(
  configured_prompt: String,
  step_id: String,
  attempt_index: Int,
  failed_artifact: step_artifact.StepArtifact,
) -> String {
  configured_prompt
  <> "\n\nFailure context:\n"
  <> "- step_id: "
  <> step_id
  <> "\n- failed_attempt_index: "
  <> int.to_string(attempt_index)
  <> "\n- status: "
  <> step_artifact.status_to_string(failed_artifact.status)
  <> "\n- failure_code: "
  <> option_text(failed_artifact.failure_code, "none")
  <> "\n- summary: "
  <> failed_artifact.summary_text
  <> stderr_block(failed_artifact.stderr)
}

fn stderr_block(stderr: String) -> String {
  let trimmed = string.trim(stderr)
  case trimmed == "" {
    True -> ""
    False -> "\n- stderr:\n" <> trimmed
  }
}

fn option_text(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}

pub fn tool_spec(
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  repository_root: String,
) -> Result(
  structured_output_tool_spec.ToolSpec,
  structured_output_tool_spec.ToolSpecError,
) {
  let spec =
    workflow_dag.StructuredOutputSpec(
      artifact_name: artifact_name,
      required: True,
      source: structured_output_source.PiToolCallSource(
        tool_name: tool_name,
        require_single: True,
        reject_sibling_tool_calls: True,
        parameters_schema_path: Some(provider_schema_path),
      ),
      format: workflow_dag.StructuredJson,
      schema: workflow_dag.StructuredObjectSchema([
        "schema_version",
        "artifact_type",
        "decision",
        "summary",
        "reason",
      ]),
      validators: [],
      validation_retries: 0,
    )
  structured_output_tool_spec.for_step(structured_output_tool_spec.BuildInput(
    workflow_id: workflow_id,
    run_id: run_id,
    step_id: step_id,
    attempt_index: attempt_index,
    repository_root: repository_root,
    spec: spec,
  ))
}

pub fn decision(
  success: agent_types.WorkerSuccess,
) -> Result(Decision, ProtocolError) {
  decision_from_result(success.result)
}

pub fn decision_from_result(
  artifact: result_artifact.ResultArtifact,
) -> Result(Decision, ProtocolError) {
  case recovery_tool_calls(artifact.tool_calls, []) {
    [] ->
      Error(ProtocolError(
        "recovery_result_missing",
        "submit_workflow_step_recovery_result was not called",
      ))
    [call] -> decode_call(call)
    _ ->
      Error(ProtocolError(
        "recovery_result_duplicate",
        "submit_workflow_step_recovery_result must be called exactly once",
      ))
  }
}

fn recovery_tool_calls(
  calls: List(result_artifact.ToolCallSubmission),
  acc: List(result_artifact.ToolCallSubmission),
) -> List(result_artifact.ToolCallSubmission) {
  case calls {
    [] -> list.reverse(acc)
    [call, ..rest] ->
      case call.name == tool_name {
        True -> recovery_tool_calls(rest, [call, ..acc])
        False -> recovery_tool_calls(rest, acc)
      }
  }
}

fn decode_call(
  call: result_artifact.ToolCallSubmission,
) -> Result(Decision, ProtocolError) {
  case call.sibling_count > 1 {
    True ->
      Error(ProtocolError(
        "recovery_result_duplicate",
        "recovery result included sibling tool calls",
      ))
    False ->
      case call.arguments_json {
        None ->
          Error(ProtocolError(
            "recovery_result_missing_arguments",
            "recovery result tool call had no arguments_json",
          ))
        Some(arguments_json) ->
          case json.parse(arguments_json, payload_decoder()) {
            Error(_) ->
              Error(ProtocolError(
                "recovery_result_malformed",
                "recovery result arguments were not valid JSON",
              ))
            Ok(payload) -> payload_to_decision(payload)
          }
      }
  }
}

fn payload_to_decision(payload: Payload) -> Result(Decision, ProtocolError) {
  case payload.artifact_type != artifact_type {
    True ->
      Error(ProtocolError(
        "recovery_result_wrong_artifact_type",
        "recovery result artifact_type must be `" <> artifact_type <> "`",
      ))
    False ->
      case payload.schema_version != schema_version {
        True ->
          Error(ProtocolError(
            "recovery_result_wrong_schema_version",
            "recovery result schema_version must be 1",
          ))
        False ->
          case payload.decision {
            "retry_requested" ->
              Ok(RetryRequested(
                summary: payload.summary,
                reason: payload.reason,
              ))
            "gave_up" ->
              Ok(GaveUp(summary: payload.summary, reason: payload.reason))
            other ->
              Error(ProtocolError(
                "recovery_result_invalid_decision",
                "unsupported recovery decision: " <> other,
              ))
          }
      }
  }
}

fn payload_decoder() -> decode.Decoder(Payload) {
  use schema_version <- decode.field("schema_version", decode.int)
  use artifact_type <- decode.field("artifact_type", decode.string)
  use decision <- decode.field("decision", decode.string)
  use summary <- decode.field("summary", decode.string)
  use reason <- decode.field("reason", decode.string)
  decode.success(Payload(
    schema_version: schema_version,
    artifact_type: artifact_type,
    decision: decision,
    summary: summary,
    reason: reason,
  ))
}

pub fn artifact_json(
  decision: String,
  summary: String,
  reason: String,
  redaction_secrets: List(String),
) -> String {
  json.object([
    #("artifact_type", json.string(artifact_type)),
    #("schema_version", json.int(schema_version)),
    #("decision", json.string(decision)),
    #(
      "summary",
      json.string(log.redact(
        "workflow_step_recovery",
        summary,
        redaction_secrets,
      )),
    ),
    #(
      "reason",
      json.string(log.redact(
        "workflow_step_recovery",
        reason,
        redaction_secrets,
      )),
    ),
  ])
  |> json.to_string
}
