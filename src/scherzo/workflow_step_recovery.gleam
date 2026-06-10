import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/log
import scherzo/path as scherzo_path
import scherzo/result_artifact
import scherzo/step_artifact
import scherzo/structured_output_source
import scherzo/structured_output_tool_spec
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workspace_run

pub const tool_name = "submit_workflow_step_recovery_result"

pub const artifact_name = "workflow_step_recovery_result"

pub const artifact_type = "workflow_step_recovery_result"

pub const schema_version = 1

pub const provider_schema_path = ".scherzo/workflows/schemas/provider/workflow-step-recovery-result.v1.schema.json"

pub const canonical_schema_path = ".scherzo/workflows/schemas/workflow-step-recovery-result.v1.schema.json"

pub const recovery_input_artifact_name = "workflow_step_recovery_input"

pub const recovery_input_schema_version = "scherzo.workflow_recovery_input.v1"

pub type Decision {
  Recheck(summary: String, reason: String)
  GaveUp(summary: String, reason: String)
}

pub type ProtocolError {
  ProtocolError(code: String, message: String)
}

pub type DecisionRecordError {
  RecoveryDecisionArtifactWriteFailed(workflow_checkpoint.CheckpointError)
  RecoveryDecisionFinishedCheckpointFailed(workflow_checkpoint.CheckpointError)
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

pub type RecoveryInputArtifact {
  RecoveryInputArtifact(ref: String, payload_json: String)
}

pub fn prompt(
  configured_prompt: String,
  recovery_input: RecoveryInputArtifact,
) -> String {
  configured_prompt
  <> "\n\nRepair-and-recheck contract:\n"
  <> "- You are not retrying the failed step. You are repairing the cause of the failure.\n"
  <> "- Use the structured failure context, diagnostics, and current workspace state.\n"
  <> "- Make the smallest safe local change needed to fix the failure.\n"
  <> "- Return recheck only when the original failed step should pass if rerun unchanged.\n"
  <> "- Return gave_up if the failure requires credentials, external service recovery, product decisions, unsafe side effects, broad redesign, missing context, or unclear scope.\n"
  <> "\nStructured recovery input artifact: "
  <> recovery_input.ref
  <> "\n\nStructured recovery input JSON:\n```json\n"
  <> recovery_input.payload_json
  <> "\n```"
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

pub fn tool_spec_env(
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  repository_root: String,
  run_root: String,
) -> Result(#(String, String), structured_output_tool_spec.ToolSpecError) {
  use spec <- result.try(tool_spec(
    workflow_id,
    run_id,
    step_id,
    attempt_index,
    repository_root,
  ))
  use written <- result.try(structured_output_tool_spec.write(spec, run_root))
  Ok(structured_output_tool_spec.env_pair(written))
}

pub fn detail(detail: String, redaction_secrets: List(String)) -> String {
  log.redact("workflow_step_recovery", detail, redaction_secrets)
  |> log.truncate(4000)
}

pub fn tool_spec_unavailable_reason(
  error: structured_output_tool_spec.ToolSpecError,
  redaction_secrets: List(String),
) -> String {
  detail(
    "recovery_tool_spec_unavailable:" <> error.code <> ":" <> error.message,
    redaction_secrets,
  )
}

pub fn record_recovery_input(
  checkpoint: workflow_checkpoint.Writer,
  workspace: workspace_run.PreparedStepWorkspace,
  step_id: String,
  recovery_attempt_number: Int,
  failed_artifact: step_artifact.StepArtifact,
  redaction_secrets: List(String),
) -> Result(RecoveryInputArtifact, workflow_checkpoint.CheckpointError) {
  let payload =
    recovery_input_json(
      workspace.workflow_id,
      workspace.run_id,
      step_id,
      workspace.attempt_index,
      failed_artifact,
      redaction_secrets,
    )
  let write =
    workflow_checkpoint.RecoveryArtifactWrite(
      run_id: workspace.run_id,
      workflow_id: workspace.workflow_id,
      step_id: step_id,
      failed_attempt_index: workspace.attempt_index,
      recovery_attempt_number: recovery_attempt_number,
      artifact_name: recovery_input_artifact_name,
      payload_json: payload,
    )
  checkpoint.write_recovery_artifact(write)
  |> result.map(fn(written) {
    RecoveryInputArtifact(ref: written.ref, payload_json: payload)
  })
}

pub fn recovery_input_json(
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  failed_artifact: step_artifact.StepArtifact,
  redaction_secrets: List(String),
) -> String {
  let fields = [
    #("schema_version", json.string(recovery_input_schema_version)),
    #("workflow_id", json.string(workflow_id)),
    #("run_id", json.string(run_id)),
    #("step_id", json.string(step_id)),
    #("attempt_index", json.int(attempt_index)),
    #(
      "failure_summary",
      json.string(log.redact(
        "workflow_step_recovery",
        failure_summary(failed_artifact),
        redaction_secrets,
      )),
    ),
    #(
      "diagnostic_refs",
      json.array(diagnostic_refs(failed_artifact), of: json.string),
    ),
    #(
      "structured_output_refs",
      json.array(
        structured_output_refs(failed_artifact),
        of: structured_output_ref_to_json,
      ),
    ),
    #("recovery_policy", recovery_policy_json()),
  ]
  let fields =
    list.append(fields, reason_code_fields(failed_artifact, redaction_secrets))
  fields
  |> json.object
  |> json.to_string
}

fn failure_summary(failed_artifact: step_artifact.StepArtifact) -> String {
  case string.trim(failed_artifact.summary_text) == "" {
    True ->
      "Workflow step failed. Inspect the retained step artifact and diagnostics."
    False -> failed_artifact.summary_text
  }
}

fn diagnostic_refs(
  failed_artifact: step_artifact.StepArtifact,
) -> List(String) {
  case failed_artifact.diagnostic_path {
    Some(path) -> [diagnostic_ref(path)]
    None -> []
  }
}

fn diagnostic_ref(value: String) -> String {
  case scherzo_path.is_absolute(value) {
    False -> value
    True ->
      case repo_relative_path(value) {
        Some(relative) -> relative
        None ->
          case scherzo_workspace_relative_path(value) {
            Some(relative) -> relative
            None -> "<absolute path hidden>"
          }
      }
  }
}

fn repo_relative_path(value: String) -> Option(String) {
  case scherzo_path.env("SCHERZO_REPO_ROOT") {
    Some(root) ->
      case relative_to_root(value, root) {
        Some(relative) -> Some(relative)
        None -> cwd_relative_path(value)
      }
    None -> cwd_relative_path(value)
  }
}

fn cwd_relative_path(value: String) -> Option(String) {
  case scherzo_path.absolute(".") {
    Ok(root) -> relative_to_root(value, root)
    Error(Nil) -> None
  }
}

fn relative_to_root(value: String, root: String) -> Option(String) {
  let root_abs = scherzo_path.absolute_or_original(root) |> trim_trailing_slash
  case scherzo_path.contains(root_abs, value) {
    True ->
      case value == root_abs {
        True -> Some(".")
        False -> Some(string.drop_start(value, string.length(root_abs) + 1))
      }
    False -> None
  }
}

fn scherzo_workspace_relative_path(value: String) -> Option(String) {
  case string.split_once(value, on: "/.scherzo/workspaces/") {
    Ok(#(_, rest)) -> Some(".scherzo/workspaces/" <> rest)
    Error(Nil) -> None
  }
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}

fn structured_output_refs(
  failed_artifact: step_artifact.StepArtifact,
) -> List(#(String, String)) {
  case failed_artifact.structured_output {
    Some(step_artifact.StructuredOutputValid(metadata)) -> [
      #(metadata.artifact_name, metadata.ref),
    ]
    _ -> []
  }
}

fn structured_output_ref_to_json(ref: #(String, String)) -> json.Json {
  let #(artifact_name, artifact_ref) = ref
  json.object([
    #("artifact_name", json.string(artifact_name)),
    #("ref", json.string(artifact_ref)),
  ])
}

fn recovery_policy_json() -> json.Json {
  json.object([
    #(
      "allowed_actions",
      json.array(
        ["inspect_workspace", "edit_workspace", "run_local_validation"],
        of: json.string,
      ),
    ),
    #(
      "forbidden_actions",
      json.array(
        [
          "publish",
          "push",
          "create_pr",
          "change_linear_issue",
          "manage_workspaces_or_branches",
          "change_recovery_policy_to_hide_failure",
        ],
        of: json.string,
      ),
    ),
  ])
}

fn reason_code_fields(
  failed_artifact: step_artifact.StepArtifact,
  redaction_secrets: List(String),
) -> List(#(String, json.Json)) {
  case failed_artifact.failure_code {
    Some(reason_code) ->
      case string.trim(reason_code) == "" {
        True -> []
        False -> [
          #(
            "reason_code",
            json.string(log.redact(
              "workflow_step_recovery",
              reason_code,
              redaction_secrets,
            )),
          ),
        ]
      }
    None -> []
  }
}

pub fn record_finished(
  checkpoint: workflow_checkpoint.Writer,
  workspace: workspace_run.PreparedStepWorkspace,
  step_id: String,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  result_text: String,
  summary: String,
  reason: String,
  retry_attempt_index: Option(Int),
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  checkpoint.step_recovery_finished(workflow_checkpoint.StepRecoveryFinished(
    run_id: workspace.run_id,
    workflow_id: workspace.workflow_id,
    step_id: step_id,
    failed_attempt_index: workspace.attempt_index,
    recovery_attempt_number: recovery_attempt_number,
    recovery_session_id: recovery_session_id,
    result: result_text,
    summary: summary,
    reason: reason,
    retry_attempt_index: retry_attempt_index,
  ))
}

pub fn record_decision(
  checkpoint: workflow_checkpoint.Writer,
  step_id: String,
  workspace: workspace_run.PreparedStepWorkspace,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  result_text: String,
  summary: String,
  reason: String,
  retry_attempt_index: Option(Int),
  redaction_secrets: List(String),
) -> Result(Nil, DecisionRecordError) {
  let payload = artifact_json(result_text, summary, reason, redaction_secrets)
  let redacted_summary =
    log.redact("workflow_step_recovery", summary, redaction_secrets)
  let redacted_reason =
    log.redact("workflow_step_recovery", reason, redaction_secrets)
  let write =
    workflow_checkpoint.RecoveryArtifactWrite(
      run_id: workspace.run_id,
      workflow_id: workspace.workflow_id,
      step_id: step_id,
      failed_attempt_index: workspace.attempt_index,
      recovery_attempt_number: recovery_attempt_number,
      artifact_name: artifact_name,
      payload_json: payload,
    )
  case checkpoint.write_recovery_artifact(write) {
    Error(error) -> {
      let reason =
        "artifact_write_failed:"
        <> workflow_checkpoint.describe_error(error)
        |> detail(redaction_secrets)
      let Nil =
        ignore_checkpoint_result(record_finished(
          checkpoint,
          workspace,
          step_id,
          recovery_attempt_number,
          recovery_session_id,
          "artifact_write_failed",
          "Recovery artifact write failed",
          reason,
          None,
        ))
      Error(RecoveryDecisionArtifactWriteFailed(error))
    }
    Ok(_) ->
      case
        record_finished(
          checkpoint,
          workspace,
          step_id,
          recovery_attempt_number,
          recovery_session_id,
          result_text,
          redacted_summary,
          redacted_reason,
          retry_attempt_index,
        )
      {
        Ok(Nil) -> Ok(Nil)
        Error(error) -> {
          let Nil =
            note_ignored_checkpoint_error(workflow_checkpoint.describe_error(
              error,
            ))
          Error(RecoveryDecisionFinishedCheckpointFailed(error))
        }
      }
  }
}

fn ignore_checkpoint_result(
  result: Result(Nil, workflow_checkpoint.CheckpointError),
) -> Nil {
  case result {
    Ok(Nil) -> Nil
    Error(error) ->
      note_ignored_checkpoint_error(workflow_checkpoint.describe_error(error))
  }
}

fn note_ignored_checkpoint_error(_message: String) -> Nil {
  Nil
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
        "recovery_result_has_sibling_tool_calls",
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
            "recheck" ->
              Ok(Recheck(summary: payload.summary, reason: payload.reason))
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
