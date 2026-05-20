import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workstream/artifacts
import scherzo/workstream/id as workstream_id
import scherzo/workstream/ledger
import scherzo/workstream/phase_metadata
import scherzo/workstream/types

pub type EmitError {
  EmitError(code: String, message: String)
}

pub type OutputEmission {
  OutputEmission(
    name: String,
    artifact_id: String,
    snapshot: types.ArtifactSnapshot,
  )
}

pub type GeneratedArtifactEmission {
  GeneratedArtifactEmission(
    artifact_id: String,
    artifact_type: String,
    snapshot_ref: String,
    snapshot_sha256: String,
    snapshot_bytes: Int,
    original_path: String,
  )
}

pub type EmitResult {
  NoHandoff
  Emitted(
    workstream_id: String,
    output: OutputEmission,
    next_actions: List(GeneratedArtifactEmission),
    handoff: GeneratedArtifactEmission,
    records: List(record.LedgerRecord),
  )
}

type ResolvedOutput {
  ResolvedOutput(
    name: String,
    contract_type: workflow_contract.ContractType,
    artifact_ref: String,
    sha256: String,
    bytes: Int,
    media_type: String,
    producer_step_id: String,
  )
}

type EmittedNextAction {
  EmittedNextAction(
    artifact_id: String,
    snapshot: GeneratedArtifactEmission,
    record: record.LedgerRecord,
  )
}

pub fn emit(
  workflow_id: String,
  run_id: String,
  workflow_fingerprint: String,
  issue_id: String,
  issue_identifier: String,
  issue_url: Option(String),
  metadata: phase_metadata.PhaseMetadata,
  manifest: workflow_contract_manifest.ContractOutputManifest,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(EmitResult, EmitError) {
  case metadata.handoff {
    None -> Ok(NoHandoff)
    Some(handoff) -> {
      use Nil <- result.try(validate_manifest_identity(
        workflow_id,
        run_id,
        workflow_fingerprint,
        manifest,
      ))
      use workstream_id <- result.try(
        workstream_id.linear_workstream_id(issue_identifier)
        |> result.map_error(fn(error) {
          EmitError(
            code: "workstream_id_invalid",
            message: workstream_id.error_code(error),
          )
        }),
      )
      use task_ref <- result.try(task_ref(issue_id, issue_identifier, issue_url))
      use Nil <- result.try(validate_handoff_artifact_type(
        handoff.artifact_type,
      ))
      use output <- result.try(resolve_output(handoff.output, manifest))
      use output_snapshot <- result.try(snapshot_output(
        workflow_id,
        run_id,
        metadata,
        output,
        checkpoint,
      ))
      let output_artifact_id =
        output_artifact_id(metadata.phase_id, output.name, run_id)
      let output_emission =
        OutputEmission(
          name: output.name,
          artifact_id: output_artifact_id,
          snapshot: output_snapshot,
        )
      let at_ms = checkpoint.now_ms()
      let base_idempotency_key =
        "workstream_phase_handoff:"
        <> metadata.phase_id
        <> ":"
        <> run_id
        <> ":"
        <> workflow_fingerprint
      let created_record =
        ledger.workstream_created(
          at_ms,
          workstream_id,
          task_ref,
          base_idempotency_key <> ":created",
        )
      let assigned_record =
        ledger.workstream_assigned(
          at_ms,
          workstream_id,
          "assignment:" <> workflow_id <> ":" <> metadata.phase_id,
          workflow_id,
          None,
          "workflow_phase_completed:" <> metadata.phase_id,
          base_idempotency_key <> ":assigned",
        )
      let output_record =
        ledger.workstream_artifact_recorded(
          at_ms,
          workstream_id,
          output_artifact_id,
          workflow_contract.type_to_string(output.contract_type),
          output_snapshot.ref,
          output_snapshot.sha256,
          output_snapshot.bytes,
          output_snapshot.original_path,
          workflow_contract.type_to_string(output.contract_type),
          output_snapshot.media_type,
          output_snapshot.producer.workflow_id,
          output_snapshot.producer.run_id,
          output_snapshot.producer.step_id,
          base_idempotency_key <> ":output:" <> output.name,
        )
      use emitted_next_actions <- result.try(
        emit_next_actions(
          metadata.next_actions,
          workstream_id,
          workflow_id,
          run_id,
          metadata.phase_id,
          output.producer_step_id,
          base_idempotency_key,
          at_ms,
          checkpoint,
          [],
        ),
      )
      let handoff_artifact_id = handoff_artifact_id(metadata.phase_id, run_id)
      let handoff_payload =
        types.HandoffArtifact(
          artifact_id: handoff_artifact_id,
          workstream_id: workstream_id,
          phase_id: metadata.phase_id,
          summary: metadata.display_name <> " completed",
          outputs: [
            types.HandoffOutput(name: output.name, snapshot: output_snapshot),
          ],
          recommended_next_actions: list.map(
            emitted_next_actions,
            fn(next_action) { next_action.artifact_id },
          ),
          open_questions: [],
        )
      use handoff_snapshot <- result.try(
        snapshot_generated_artifact(
          artifacts.handoff_to_string(handoff_payload),
          handoff_original_path(handoff_artifact_id),
          checkpoint,
        )
        |> result.map_error(map_emit_generated_error("handoff_snapshot_failed")),
      )
      use Nil <- result.try(validate_handoff_snapshot(
        handoff_snapshot.snapshot_ref,
        handoff_snapshot.snapshot_sha256,
        checkpoint,
      ))
      let handoff_record =
        ledger.workstream_handoff_recorded(
          at_ms,
          workstream_id,
          handoff_artifact_id,
          handoff_snapshot.snapshot_ref,
          handoff_snapshot.snapshot_sha256,
          handoff_snapshot.snapshot_bytes,
          workflow_id,
          run_id,
          base_idempotency_key <> ":handoff",
        )
      Ok(
        Emitted(
          workstream_id: workstream_id,
          output: output_emission,
          next_actions: list.map(emitted_next_actions, fn(next_action) {
            next_action.snapshot
          }),
          handoff: handoff_snapshot,
          records: [
            created_record,
            assigned_record,
            output_record,
            ..list.map(emitted_next_actions, fn(next_action) {
              next_action.record
            })
            |> list.append([handoff_record])
          ],
        ),
      )
    }
  }
}

pub fn error_code(error: EmitError) -> String {
  let EmitError(code, _) = error
  code
}

pub fn error_message(error: EmitError) -> String {
  let EmitError(_, message) = error
  message
}

fn validate_manifest_identity(
  workflow_id: String,
  run_id: String,
  workflow_fingerprint: String,
  manifest: workflow_contract_manifest.ContractOutputManifest,
) -> Result(Nil, EmitError) {
  case manifest.workflow_id != workflow_id {
    True ->
      error(
        "manifest_workflow_id_mismatch",
        "workflow id does not match output manifest",
      )
    False ->
      case manifest.run_id != run_id {
        True ->
          error(
            "manifest_run_id_mismatch",
            "run id does not match output manifest",
          )
        False ->
          case manifest.workflow_fingerprint != workflow_fingerprint {
            True ->
              error(
                "manifest_workflow_fingerprint_mismatch",
                "workflow fingerprint does not match output manifest",
              )
            False -> Ok(Nil)
          }
      }
  }
}

fn task_ref(
  issue_id: String,
  issue_identifier: String,
  issue_url: Option(String),
) -> Result(record.TaskRefFields, EmitError) {
  case string.trim(issue_id) == "" {
    True -> error("workstream_issue_id_missing", "issue id is required")
    False ->
      Ok(record.linear_task_ref_fields(
        issue_id,
        Some(issue_identifier),
        issue_url,
      ))
  }
}

fn validate_handoff_artifact_type(value: String) -> Result(Nil, EmitError) {
  case value == types.handoff_artifact_type {
    True -> Ok(Nil)
    False ->
      error(
        "workstream_handoff_artifact_type_unsupported",
        "unsupported handoff artifact_type: " <> value,
      )
  }
}

fn resolve_output(
  output_name: String,
  manifest: workflow_contract_manifest.ContractOutputManifest,
) -> Result(ResolvedOutput, EmitError) {
  case list.find(manifest.outputs, fn(output) { output.name == output_name }) {
    Error(Nil) ->
      error(
        "workstream_handoff_output_missing",
        "configured handoff output is missing from the manifest",
      )
    Ok(named) -> resolve_output_value(output_name, named.value)
  }
}

fn resolve_output_value(
  output_name: String,
  value: workflow_contract_manifest.ManifestValue,
) -> Result(ResolvedOutput, EmitError) {
  case value.status {
    workflow_contract_manifest.Absent ->
      error(
        "workstream_handoff_output_absent",
        "configured handoff output is absent",
      )
    workflow_contract_manifest.Present ->
      case value.ref_kind {
        Some(workflow_contract_manifest.RunArtifact) -> {
          use artifact_ref <- result.try(required_option(
            value.ref,
            "workstream_handoff_output_ref_missing",
            "configured handoff output is missing ref",
          ))
          use sha256 <- result.try(required_option(
            value.sha256,
            "workstream_handoff_output_sha256_missing",
            "configured handoff output is missing sha256",
          ))
          use bytes <- result.try(required_option(
            value.bytes,
            "workstream_handoff_output_bytes_missing",
            "configured handoff output is missing bytes",
          ))
          use media_type <- result.try(required_option(
            value.media_type,
            "workstream_handoff_output_media_type_missing",
            "configured handoff output is missing media_type",
          ))
          use source <- result.try(required_option(
            value.source,
            "workstream_handoff_output_source_missing",
            "configured handoff output is missing source metadata",
          ))
          use producer_step_id <- result.try(source_step_id(source))
          Ok(ResolvedOutput(
            name: output_name,
            contract_type: value.type_,
            artifact_ref: artifact_ref,
            sha256: sha256,
            bytes: bytes,
            media_type: media_type,
            producer_step_id: producer_step_id,
          ))
        }
        _ ->
          error(
            "workstream_handoff_output_not_run_artifact",
            "configured handoff output must be a retained run artifact",
          )
      }
  }
}

fn source_step_id(source: json_value.JsonValue) -> Result(String, EmitError) {
  case source {
    json_value.JObject(entries) ->
      case list.key_find(entries, "step_id") {
        Ok(json_value.JString(step_id)) -> Ok(step_id)
        _ ->
          error(
            "workstream_handoff_output_source_step_missing",
            "configured handoff output source is missing step_id",
          )
      }
    _ ->
      error(
        "workstream_handoff_output_source_invalid",
        "configured handoff output source must be an object",
      )
  }
}

fn snapshot_output(
  workflow_id: String,
  run_id: String,
  metadata: phase_metadata.PhaseMetadata,
  output: ResolvedOutput,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(types.ArtifactSnapshot, EmitError) {
  use snapshot <- result.try(
    checkpoint.snapshot_existing_artifact_ref(
      output.artifact_ref,
      output.sha256,
      output.bytes,
      output.artifact_ref,
      output.media_type,
    )
    |> result.map_error(fn(error) {
      EmitError(
        code: "workstream_output_snapshot_failed",
        message: workflow_checkpoint.describe_error(error),
      )
    }),
  )
  Ok(types.ArtifactSnapshot(
    ref: snapshot.ref,
    sha256: snapshot.sha256,
    bytes: snapshot.bytes,
    media_type: snapshot.media_type,
    original_path: snapshot.original_path,
    contract_type: workflow_contract.type_to_string(output.contract_type),
    producer: types.ProducerRef(
      workflow_id: workflow_id,
      run_id: run_id,
      step_id: output.producer_step_id,
    ),
    validation: types.ValidationSummary(
      status: "passed",
      validator: "workflow_contract_output_manifest",
      checked_at: "run:" <> run_id,
    ),
    summary: metadata.display_name <> " output " <> output.name,
  ))
}

fn emit_next_actions(
  next_actions: List(phase_metadata.PhaseNextAction),
  workstream_id: String,
  workflow_id: String,
  run_id: String,
  phase_id: String,
  producer_step_id: String,
  base_idempotency_key: String,
  at_ms: Int,
  checkpoint: workflow_checkpoint.Writer,
  acc: List(EmittedNextAction),
) -> Result(List(EmittedNextAction), EmitError) {
  case next_actions {
    [] -> Ok(list.reverse(acc))
    [next_action, ..rest] -> {
      let artifact_id =
        next_action_artifact_id(phase_id, next_action.action_id, run_id)
      let payload =
        types.NextActionArtifact(
          artifact_id: artifact_id,
          workstream_id: workstream_id,
          action_id: next_action.action_id,
          workflow_id: next_action.workflow_id,
          state: next_action.state,
          priority: next_action.priority,
          inputs: next_action.inputs,
          requires_gate: next_action.requires_gate,
          auto_enqueue: next_action.auto_enqueue,
        )
      let contents = artifacts.next_action_to_string(payload)
      use Nil <- result.try(validate_next_action_contents(contents))
      use snapshot <- result.try(
        snapshot_generated_artifact(
          contents,
          next_action_original_path(artifact_id),
          checkpoint,
        )
        |> result.map_error(map_emit_generated_error(
          "workstream_next_action_snapshot_failed",
        )),
      )
      let record =
        ledger.workstream_artifact_recorded(
          at_ms,
          workstream_id,
          artifact_id,
          types.next_action_artifact_type,
          snapshot.snapshot_ref,
          snapshot.snapshot_sha256,
          snapshot.snapshot_bytes,
          snapshot.original_path,
          workflow_contract.type_to_string(workflow_contract.Text),
          "application/json",
          workflow_id,
          run_id,
          producer_step_id,
          base_idempotency_key <> ":next_action:" <> next_action.action_id,
        )
      emit_next_actions(
        rest,
        workstream_id,
        workflow_id,
        run_id,
        phase_id,
        producer_step_id,
        base_idempotency_key,
        at_ms,
        checkpoint,
        [
          EmittedNextAction(
            artifact_id: artifact_id,
            snapshot: snapshot,
            record: record,
          ),
          ..acc
        ],
      )
    }
  }
}

fn validate_next_action_contents(contents: String) -> Result(Nil, EmitError) {
  artifacts.decode_next_action(contents)
  |> result.map(fn(_) { Nil })
  |> result.map_error(fn(error) {
    EmitError(
      code: "workstream_next_action_invalid",
      message: types.error_code(error),
    )
  })
}

fn validate_handoff_snapshot(
  ref: String,
  sha256: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, EmitError) {
  checkpoint.read_artifact(ref)
  |> result.map_error(fn(error) {
    EmitError(
      code: "workstream_handoff_read_failed",
      message: workflow_checkpoint.describe_error(error),
    )
  })
  |> result.try(fn(contents) {
    artifacts.decode_handoff(contents)
    |> result.map(fn(_) { Nil })
    |> result.map_error(fn(error) {
      EmitError(
        code: "workstream_handoff_invalid",
        message: types.error_code(error),
      )
    })
  })
  |> result.try(fn(_) {
    case string.trim(sha256) == "" {
      True ->
        error("workstream_handoff_sha256_missing", "handoff sha256 is required")
      False -> Ok(Nil)
    }
  })
}

fn snapshot_generated_artifact(
  contents: String,
  original_path: String,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(GeneratedArtifactEmission, workflow_checkpoint.CheckpointError) {
  use snapshot <- result.try(checkpoint.snapshot_workstream_bytes(
    original_path,
    "application/json",
    bit_array.from_string(contents),
  ))
  Ok(GeneratedArtifactEmission(
    artifact_id: artifact_id_from_original_path(original_path),
    artifact_type: generated_artifact_type_from_original_path(original_path),
    snapshot_ref: snapshot.ref,
    snapshot_sha256: snapshot.sha256,
    snapshot_bytes: snapshot.bytes,
    original_path: snapshot.original_path,
  ))
}

fn artifact_id_from_original_path(original_path: String) -> String {
  case string.split(original_path, on: "/") |> list.reverse {
    [file, ..] -> string.drop_end(file, 5)
    _ -> original_path
  }
}

fn generated_artifact_type_from_original_path(original_path: String) -> String {
  case string.starts_with(original_path, "workstream/next-actions/") {
    True -> types.next_action_artifact_type
    False -> types.handoff_artifact_type
  }
}

fn map_emit_generated_error(
  code: String,
) -> fn(workflow_checkpoint.CheckpointError) -> EmitError {
  fn(error) {
    EmitError(code: code, message: workflow_checkpoint.describe_error(error))
  }
}

fn required_option(
  value: Option(a),
  code: String,
  message: String,
) -> Result(a, EmitError) {
  case value {
    Some(value) -> Ok(value)
    None -> error(code, message)
  }
}

fn output_artifact_id(
  phase_id: String,
  output_name: String,
  run_id: String,
) -> String {
  "output:" <> phase_id <> ":" <> output_name <> ":" <> run_id
}

fn next_action_artifact_id(
  phase_id: String,
  action_id: String,
  run_id: String,
) -> String {
  "next-action:" <> phase_id <> ":" <> action_id <> ":" <> run_id
}

fn handoff_artifact_id(phase_id: String, run_id: String) -> String {
  "handoff:" <> phase_id <> ":" <> run_id
}

fn next_action_original_path(artifact_id: String) -> String {
  "workstream/next-actions/" <> artifact_id <> ".json"
}

fn handoff_original_path(artifact_id: String) -> String {
  "workstream/handoffs/" <> artifact_id <> ".json"
}

fn error(code: String, message: String) -> Result(a, EmitError) {
  Error(EmitError(code, message))
}
