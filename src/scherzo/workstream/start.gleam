import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/ledger as state_ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workflow_run
import scherzo/workstream/artifact_store
import scherzo/workstream/artifacts
import scherzo/workstream/decision
import scherzo/workstream/id as workstream_id
import scherzo/workstream/ledger
import scherzo/workstream/start_key
import scherzo/workstream/start_manual
import scherzo/workstream/start_requirements
import scherzo/workstream/types

pub type ManualArtifactInput {
  ManualArtifactInput(
    name: String,
    artifact_type: String,
    original_path: String,
    contract_type: Option(String),
    media_type: Option(String),
  )
}

pub type ManualStartContext {
  ManualStartContext(
    issue_id: String,
    issue_identifier: String,
    issue_url: Option(String),
    reason: String,
  )
}

pub type StartOutcome {
  StartOutcome(
    workstream_id: String,
    phase_run_id: String,
    workflow_id: String,
    action_id: String,
    input_bundle_ref: String,
    input_bundle_sha256: String,
    input_bundle_bytes: Int,
    idempotency_key: String,
    records: List(record.LedgerRecord),
    contract_values: workflow_run.ContractRunValues,
  )
}

pub type StartResult {
  Queued(StartOutcome)
  Duplicate(StartOutcome)
}

pub type StartError {
  StartError(code: String, message: String)
}

fn manual_artifact_inputs(
  inputs: List(ManualArtifactInput),
) -> List(start_manual.ArtifactInput) {
  list.map(inputs, fn(input) {
    start_manual.ArtifactInput(
      name: input.name,
      artifact_type: input.artifact_type,
      original_path: input.original_path,
      contract_type: input.contract_type,
      media_type: input.media_type,
    )
  })
}

type ResolvedInput {
  ResolvedInput(
    name: String,
    descriptor: types.ContractDescriptorRecord,
    contract_type: workflow_contract.ContractType,
    ref: String,
    sha256: String,
    bytes: Int,
    media_type: String,
    original_path: String,
    source_kind: String,
  )
}

fn resolved_manual_inputs(
  inputs: List(start_manual.ResolvedInput),
) -> List(ResolvedInput) {
  list.map(inputs, fn(input) {
    ResolvedInput(
      name: input.name,
      descriptor: types.ContractDescriptorRecord(
        kind: contract_kind(input.contract_type),
        ref_type: None,
        media_type: Some(input.media_type),
        artifact_type: Some(input.artifact_type),
        source: None,
        validation: None,
        metadata: None,
      ),
      contract_type: input.contract_type,
      ref: input.ref,
      sha256: input.sha256,
      bytes: input.bytes,
      media_type: input.media_type,
      original_path: input.original_path,
      source_kind: input.source_kind,
    )
  })
}

fn manual_error(error: start_manual.Error) -> StartError {
  let start_manual.ManualError(code, message) = error
  StartError(code, message)
}

type BundleWrite {
  BundleWrite(
    artifact_id: String,
    snapshot: artifact_store.Snapshot,
    record: record.LedgerRecord,
  )
}

pub fn from_handoff(
  workflow_id: String,
  action_id: String,
  handoff_ref: String,
  handoff_sha256: String,
  gate_decision_ids: List(String),
  contract: Option(workflow_contract.Contract),
  projected: projection.Projection,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(StartResult, StartError) {
  use payload <- result.try(read_verified_snapshot(
    checkpoint,
    handoff_ref,
    handoff_sha256,
  ))
  use handoff <- result.try(
    artifacts.decode_handoff(payload.contents)
    |> result.map_error(spec_error("handoff_invalid")),
  )
  use Nil <- result.try(require_recorded_handoff(
    projected,
    handoff.workstream_id,
    handoff_ref,
    handoff_sha256,
  ))
  use Nil <- result.try(require_recommended_next_action(
    projected,
    checkpoint,
    handoff,
    workflow_id,
    action_id,
  ))
  use inputs <- result.try(inputs_from_handoff(handoff.outputs, contract))
  use gate_decision_ids <- result.try(authorize_gate(
    checkpoint,
    projected,
    handoff.workstream_id,
    action_id,
    inputs,
    gate_decision_ids,
  ))
  queue_start(
    workstream_id: handoff.workstream_id,
    workflow_id: workflow_id,
    action_id: action_id,
    gate_decision_ids: gate_decision_ids,
    source_handoff_ref: handoff_ref,
    source_kind: "handoff",
    source_reason: Some("handoff:" <> handoff.artifact_id),
    inputs: inputs,
    preliminary_records: [],
    projected: projected,
    checkpoint: checkpoint,
  )
}

pub fn from_input_bundle(
  workflow_id: String,
  action_id: String,
  input_bundle_ref: String,
  input_bundle_sha256: String,
  gate_decision_ids: List(String),
  projected: projection.Projection,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(StartResult, StartError) {
  use payload <- result.try(read_verified_snapshot(
    checkpoint,
    input_bundle_ref,
    input_bundle_sha256,
  ))
  use bundle <- result.try(
    artifacts.decode_input_bundle(payload.contents)
    |> result.map_error(spec_error("input_bundle_invalid")),
  )
  use Nil <- result.try(require_recorded_input_bundle(
    projected,
    bundle.workstream_id,
    input_bundle_ref,
    input_bundle_sha256,
  ))
  use Nil <- result.try(require_input_bundle_workflow(bundle, workflow_id))
  use inputs <- result.try(inputs_from_bundle(bundle.inputs))
  use gate_decision_ids <- result.try(authorize_gate(
    checkpoint,
    projected,
    bundle.workstream_id,
    action_id,
    inputs,
    gate_decision_ids,
  ))
  let idempotency_key =
    derive_key(bundle.workstream_id, action_id, inputs, gate_decision_ids)
  let contract_values = contract_values_for(inputs)
  case
    start_key.existing_start(
      projected,
      bundle.workstream_id,
      action_id,
      idempotency_key,
    )
  {
    Some(existing) ->
      Ok(
        Duplicate(StartOutcome(
          workstream_id: bundle.workstream_id,
          phase_run_id: existing.phase_run_id,
          workflow_id: existing.workflow_id,
          action_id: existing.action_id,
          input_bundle_ref: existing.input_bundle_ref,
          input_bundle_sha256: existing.input_bundle_sha256,
          input_bundle_bytes: existing.input_bundle_bytes,
          idempotency_key: existing.idempotency_key,
          records: [],
          contract_values: contract_values,
        )),
      )
    None -> {
      use Nil <- result.try(reject_conflicting_start(
        projected,
        bundle.workstream_id,
        action_id,
        idempotency_key,
      ))
      let phase_run_id = start_key.phase_run_id(idempotency_key)
      let at_ms = checkpoint.now_ms()
      let queued =
        ledger.workstream_phase_run_queued(
          at_ms,
          bundle.workstream_id,
          phase_run_id,
          action_id,
          workflow_id,
          input_bundle_ref,
          input_bundle_sha256,
          payload.bytes,
          idempotency_key,
        )
      let outcome =
        StartOutcome(
          workstream_id: bundle.workstream_id,
          phase_run_id: phase_run_id,
          workflow_id: workflow_id,
          action_id: action_id,
          input_bundle_ref: input_bundle_ref,
          input_bundle_sha256: input_bundle_sha256,
          input_bundle_bytes: payload.bytes,
          idempotency_key: idempotency_key,
          records: [queued],
          contract_values: contract_values,
        )
      append_start_records(checkpoint, outcome, [queued], queued)
    }
  }
}

pub fn from_manual(
  workflow_id: String,
  action_id: String,
  gate_decision_ids: List(String),
  manual: ManualStartContext,
  artifact_inputs: List(ManualArtifactInput),
  contract: Option(workflow_contract.Contract),
  repo_root: String,
  store: state_artifact_store.Store,
  projected: projection.Projection,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(StartResult, StartError) {
  case artifact_inputs {
    [] -> error("manual_artifacts_missing", "at least one artifact is required")
    _ -> {
      use workstream_id <- result.try(
        workstream_id.linear_workstream_id(manual.issue_identifier)
        |> result.map_error(fn(err) {
          StartError("workstream_id_invalid", workstream_id.error_code(err))
        }),
      )
      let manual_inputs = manual_artifact_inputs(artifact_inputs)
      use preview_inputs <- result.try(
        start_manual.inspect_inputs(manual_inputs, contract, repo_root)
        |> result.map_error(manual_error),
      )
      let preview_inputs = resolved_manual_inputs(preview_inputs)
      use preview_gate_decision_ids <- result.try(authorize_gate(
        checkpoint,
        projected,
        workstream_id,
        action_id,
        preview_inputs,
        gate_decision_ids,
      ))
      let idempotency_key =
        derive_key(
          workstream_id,
          action_id,
          preview_inputs,
          preview_gate_decision_ids,
        )
      let contract_values = contract_values_for(preview_inputs)
      use existing <- result.try(projected_start_result(
        projected,
        workstream_id,
        action_id,
        idempotency_key,
        contract_values,
      ))
      case existing {
        Some(result) -> Ok(result)
        None -> {
          use inputs <- result.try(
            start_manual.snapshot_inputs(
              manual_inputs,
              contract,
              repo_root,
              store,
            )
            |> result.map_error(manual_error),
          )
          let inputs = resolved_manual_inputs(inputs)
          use gate_decision_ids <- result.try(authorize_gate(
            checkpoint,
            projected,
            workstream_id,
            action_id,
            inputs,
            gate_decision_ids,
          ))
          let at_ms = checkpoint.now_ms()
          let task_ref =
            record.linear_task_ref_fields(
              manual.issue_id,
              Some(manual.issue_identifier),
              manual.issue_url,
            )
          let created =
            ledger.workstream_created(
              at_ms,
              workstream_id,
              task_ref,
              "workstream_manual_start:" <> workstream_id,
            )
          let idempotency_key =
            derive_key(workstream_id, action_id, inputs, gate_decision_ids)
          let phase_run_id = start_key.phase_run_id(idempotency_key)
          let manual_records =
            list.map(inputs, fn(input) {
              ledger.workstream_artifact_recorded(
                at_ms,
                workstream_id,
                start_key.manual_artifact_id(input.name, input.sha256),
                ledger_artifact_type(input),
                input.ref,
                input.sha256,
                input.bytes,
                input.original_path,
                workflow_contract.type_to_string(input.contract_type),
                input.media_type,
                "manual",
                phase_run_id,
                "operator_import",
                idempotency_key <> ":manual:" <> input.name,
              )
            })
          let preliminary_records = [created, ..manual_records]
          let source_ref = first_input_ref(inputs)
          queue_start(
            workstream_id: workstream_id,
            workflow_id: workflow_id,
            action_id: action_id,
            gate_decision_ids: gate_decision_ids,
            source_handoff_ref: source_ref,
            source_kind: "manual",
            source_reason: Some(manual.reason),
            inputs: inputs,
            preliminary_records: preliminary_records,
            projected: projected,
            checkpoint: checkpoint,
          )
        }
      }
    }
  }
}

fn queue_start(
  workstream_id workstream_id: String,
  workflow_id workflow_id: String,
  action_id action_id: String,
  gate_decision_ids gate_decision_ids: List(String),
  source_handoff_ref source_handoff_ref: String,
  source_kind source_kind: String,
  source_reason source_reason: Option(String),
  inputs inputs: List(ResolvedInput),
  preliminary_records preliminary_records: List(record.LedgerRecord),
  projected projected: projection.Projection,
  checkpoint checkpoint: workflow_checkpoint.Writer,
) -> Result(StartResult, StartError) {
  let idempotency_key =
    derive_key(workstream_id, action_id, inputs, gate_decision_ids)
  let contract_values = contract_values_for(inputs)
  case
    start_key.existing_start(
      projected,
      workstream_id,
      action_id,
      idempotency_key,
    )
  {
    Some(existing) ->
      Ok(
        Duplicate(StartOutcome(
          workstream_id: workstream_id,
          phase_run_id: existing.phase_run_id,
          workflow_id: existing.workflow_id,
          action_id: existing.action_id,
          input_bundle_ref: existing.input_bundle_ref,
          input_bundle_sha256: existing.input_bundle_sha256,
          input_bundle_bytes: existing.input_bundle_bytes,
          idempotency_key: existing.idempotency_key,
          records: [],
          contract_values: contract_values,
        )),
      )
    None -> {
      use Nil <- result.try(reject_conflicting_start(
        projected,
        workstream_id,
        action_id,
        idempotency_key,
      ))
      let phase_run_id = start_key.phase_run_id(idempotency_key)
      let at_ms = checkpoint.now_ms()
      use bundle <- result.try(write_input_bundle(
        checkpoint,
        workstream_id,
        workflow_id,
        action_id,
        phase_run_id,
        idempotency_key,
        source_handoff_ref,
        source_kind,
        source_reason,
        inputs,
      ))
      let queued =
        ledger.workstream_phase_run_queued(
          at_ms,
          workstream_id,
          phase_run_id,
          action_id,
          workflow_id,
          bundle.snapshot.ref,
          bundle.snapshot.sha256,
          bundle.snapshot.bytes,
          idempotency_key,
        )
      let records = list.append(preliminary_records, [bundle.record, queued])
      let outcome =
        StartOutcome(
          workstream_id: workstream_id,
          phase_run_id: phase_run_id,
          workflow_id: workflow_id,
          action_id: action_id,
          input_bundle_ref: bundle.snapshot.ref,
          input_bundle_sha256: bundle.snapshot.sha256,
          input_bundle_bytes: bundle.snapshot.bytes,
          idempotency_key: idempotency_key,
          records: records,
          contract_values: contract_values,
        )
      append_start_records(checkpoint, outcome, records, queued)
    }
  }
}

fn write_input_bundle(
  checkpoint: workflow_checkpoint.Writer,
  workstream_id: String,
  workflow_id: String,
  action_id: String,
  phase_run_id: String,
  idempotency_key: String,
  source_handoff_ref: String,
  source_kind: String,
  source_reason: Option(String),
  inputs: List(ResolvedInput),
) -> Result(BundleWrite, StartError) {
  let artifact_id =
    start_key.input_bundle_artifact_id(action_id, idempotency_key)
  let payload =
    types.InputBundleArtifact(
      artifact_id: artifact_id,
      workstream_id: workstream_id,
      source_handoff_ref: source_handoff_ref,
      workflow_id: workflow_id,
      inputs: list.map(inputs, input_binding),
      source_kind: Some(source_kind),
      source_reason: source_reason,
    )
  let contents = artifacts.input_bundle_to_string(payload)
  use Nil <- result.try(
    artifacts.decode_input_bundle(contents)
    |> result.map(fn(_) { Nil })
    |> result.map_error(spec_error("input_bundle_invalid")),
  )
  use snapshot <- result.try(
    checkpoint.snapshot_workstream_bytes(
      start_key.input_bundle_original_path(artifact_id),
      "application/json",
      bit_array.from_string(contents),
    )
    |> result.map_error(checkpoint_error("input_bundle_snapshot_failed")),
  )
  let record =
    ledger.workstream_artifact_recorded(
      checkpoint.now_ms(),
      workstream_id,
      artifact_id,
      types.input_bundle_artifact_type,
      snapshot.ref,
      snapshot.sha256,
      snapshot.bytes,
      snapshot.original_path,
      workflow_contract.type_to_string(workflow_contract.ArtifactList),
      "application/json",
      "workstream-start",
      phase_run_id,
      "input_bundle",
      idempotency_key <> ":input_bundle",
    )
  Ok(BundleWrite(artifact_id: artifact_id, snapshot: snapshot, record: record))
}

fn append_start_records(
  checkpoint: workflow_checkpoint.Writer,
  outcome: StartOutcome,
  records: List(record.LedgerRecord),
  queued: record.LedgerRecord,
) -> Result(StartResult, StartError) {
  case checkpoint.append_workstream_start_records(records, queued) {
    Ok(state_ledger.WorkstreamStartRecordsAppended) -> Ok(Queued(outcome))
    Ok(state_ledger.WorkstreamStartRecordsDuplicate(existing)) ->
      Ok(
        Duplicate(duplicate_outcome(
          outcome.workstream_id,
          existing,
          outcome.contract_values,
        )),
      )
    Ok(state_ledger.WorkstreamStartRecordsConflict(existing)) ->
      conflict_error(outcome.action_id, existing)
    Error(err) ->
      Error(StartError(
        "ledger_append_failed",
        workflow_checkpoint.describe_error(err),
      ))
  }
}

fn read_verified_snapshot(
  checkpoint: workflow_checkpoint.Writer,
  ref: String,
  expected_sha256: String,
) -> Result(start_requirements.VerifiedPayload, StartError) {
  start_requirements.read_verified_snapshot(checkpoint, ref, expected_sha256)
  |> result.map_error(requirement_error)
}

fn require_recorded_handoff(
  projected: projection.Projection,
  workstream_id: String,
  handoff_ref: String,
  handoff_sha256: String,
) -> Result(Nil, StartError) {
  start_requirements.require_recorded_handoff(
    projected,
    workstream_id,
    handoff_ref,
    handoff_sha256,
  )
  |> result.map_error(requirement_error)
}

fn require_recorded_input_bundle(
  projected: projection.Projection,
  workstream_id: String,
  bundle_ref: String,
  bundle_sha256: String,
) -> Result(Nil, StartError) {
  start_requirements.require_recorded_input_bundle(
    projected,
    workstream_id,
    bundle_ref,
    bundle_sha256,
  )
  |> result.map_error(requirement_error)
}

fn require_input_bundle_workflow(
  bundle: types.InputBundleArtifact,
  workflow_id: String,
) -> Result(Nil, StartError) {
  start_requirements.require_input_bundle_workflow(bundle, workflow_id)
  |> result.map_error(requirement_error)
}

fn require_recommended_next_action(
  projected: projection.Projection,
  checkpoint: workflow_checkpoint.Writer,
  handoff: types.HandoffArtifact,
  workflow_id: String,
  action_id: String,
) -> Result(Nil, StartError) {
  start_requirements.require_recommended_next_action(
    projected,
    checkpoint,
    handoff,
    workflow_id,
    action_id,
  )
  |> result.map_error(requirement_error)
}

fn requirement_error(error: start_requirements.RequirementError) -> StartError {
  let start_requirements.RequirementError(code, message) = error
  StartError(code, message)
}

fn inputs_from_handoff(
  outputs: List(types.HandoffOutput),
  contract: Option(workflow_contract.Contract),
) -> Result(List(ResolvedInput), StartError) {
  case contract {
    None -> inputs_from_handoff_outputs(outputs, [])
    Some(contract) -> inputs_from_contract(contract.inputs, outputs, [])
  }
}

fn inputs_from_handoff_outputs(
  outputs: List(types.HandoffOutput),
  acc: List(ResolvedInput),
) -> Result(List(ResolvedInput), StartError) {
  case outputs {
    [] -> Ok(list.reverse(acc))
    [output, ..rest] -> {
      use input <- result.try(input_from_handoff_output(output, "handoff"))
      inputs_from_handoff_outputs(rest, [input, ..acc])
    }
  }
}

fn inputs_from_contract(
  specs: List(workflow_contract.InputSpec),
  outputs: List(types.HandoffOutput),
  acc: List(ResolvedInput),
) -> Result(List(ResolvedInput), StartError) {
  case specs {
    [] -> Ok(list.reverse(acc))
    [spec, ..rest] -> {
      case spec.source {
        Some(workflow_contract.MappedOutputSource) ->
          case handoff_output_named(outputs, spec.name) {
            Ok(output) -> {
              use input <- result.try(input_from_handoff_output(
                output,
                "handoff",
              ))
              use Nil <- result.try(require_contract_type(
                input,
                spec.type_,
                spec.name,
              ))
              inputs_from_contract(rest, outputs, [input, ..acc])
            }
            Error(Nil) ->
              case spec.required {
                True ->
                  error(
                    "contract_input_missing",
                    "handoff does not provide required input " <> spec.name,
                  )
                False -> inputs_from_contract(rest, outputs, acc)
              }
          }
        _ -> inputs_from_contract(rest, outputs, acc)
      }
    }
  }
}

fn handoff_output_named(
  outputs: List(types.HandoffOutput),
  name: String,
) -> Result(types.HandoffOutput, Nil) {
  list.find(outputs, fn(output) { output.name == name })
}

fn input_from_handoff_output(
  output: types.HandoffOutput,
  source_kind: String,
) -> Result(ResolvedInput, StartError) {
  use contract_type <- result.try(contract_type_from_descriptor(
    output.name,
    output.snapshot.descriptor,
    output.snapshot.contract_type,
  ))
  Ok(ResolvedInput(
    name: output.name,
    descriptor: output.snapshot.descriptor,
    contract_type: contract_type,
    ref: output.snapshot.ref,
    sha256: output.snapshot.sha256,
    bytes: output.snapshot.bytes,
    media_type: output.snapshot.media_type,
    original_path: output.snapshot.original_path,
    source_kind: source_kind,
  ))
}

fn inputs_from_bundle(
  bindings: List(types.InputBinding),
) -> Result(List(ResolvedInput), StartError) {
  inputs_from_bundle_loop(bindings, [])
}

fn inputs_from_bundle_loop(
  bindings: List(types.InputBinding),
  acc: List(ResolvedInput),
) -> Result(List(ResolvedInput), StartError) {
  case bindings {
    [] -> Ok(list.reverse(acc))
    [binding, ..rest] -> {
      use contract_type <- result.try(contract_type_from_descriptor(
        binding.name,
        binding.descriptor,
        binding.contract_type,
      ))
      use sha256 <- result.try(required_option(
        binding.sha256,
        "input_binding_sha256_missing",
        "input bundle binding is missing sha256: " <> binding.name,
      ))
      use bytes <- result.try(required_option(
        binding.bytes,
        "input_binding_bytes_missing",
        "input bundle binding is missing bytes: " <> binding.name,
      ))
      use media_type <- result.try(required_option(
        binding.media_type,
        "input_binding_media_type_missing",
        "input bundle binding is missing media_type: " <> binding.name,
      ))
      use original_path <- result.try(required_option(
        binding.original_path,
        "input_binding_original_path_missing",
        "input bundle binding is missing original_path: " <> binding.name,
      ))
      let source_kind = case binding.source_kind {
        Some(value) -> value
        None -> "input_bundle"
      }
      let input =
        ResolvedInput(
          name: binding.name,
          descriptor: binding.descriptor,
          contract_type: contract_type,
          ref: binding.value_ref,
          sha256: sha256,
          bytes: bytes,
          media_type: media_type,
          original_path: original_path,
          source_kind: source_kind,
        )
      inputs_from_bundle_loop(rest, [input, ..acc])
    }
  }
}

fn require_contract_type(
  input: ResolvedInput,
  expected: workflow_contract.ContractType,
  name: String,
) -> Result(Nil, StartError) {
  let expected_descriptor = workflow_contract.descriptor_for_type(expected)
  case base_descriptor_matches(input.descriptor, expected_descriptor) {
    False ->
      error(
        "contract_input_type_mismatch",
        "input " <> name <> " does not match target contract descriptor",
      )
    True ->
      case
        artifact_type_matches(
          input.descriptor.artifact_type,
          expected_descriptor.artifact_type,
        )
      {
        True -> Ok(Nil)
        False ->
          error(
            "contract_input_artifact_type_mismatch",
            "input " <> name <> " does not match target artifact_type",
          )
      }
  }
}

fn contract_values_for(
  inputs: List(ResolvedInput),
) -> workflow_run.ContractRunValues {
  workflow_run.ContractRunValues(
    inputs: dict.from_list(
      list.map(inputs, fn(input) { #(input.name, manifest_value_for(input)) }),
    ),
    context: dict.new(),
  )
}

fn manifest_value_for(
  input: ResolvedInput,
) -> workflow_contract_manifest.ManifestValue {
  workflow_contract_manifest.present_run_artifact(
    input.contract_type,
    workflow_contract_manifest.ArtifactWritten(
      ref: input.ref,
      sha256: input.sha256,
      bytes: input.bytes,
    ),
    input.media_type,
    Some(
      json_value.JObject([
        #("source", json_value.JString("workstream_input_bundle")),
        #("source_kind", json_value.JString(input.source_kind)),
        #("original_path", json_value.JString(input.original_path)),
        ..artifact_type_source_fields(input.descriptor.artifact_type)
      ]),
    ),
  )
}

fn input_binding(input: ResolvedInput) -> types.InputBinding {
  types.InputBinding(
    name: input.name,
    descriptor: input.descriptor,
    contract_type: None,
    value_ref: input.ref,
    sha256: Some(input.sha256),
    bytes: Some(input.bytes),
    media_type: Some(input.media_type),
    original_path: Some(input.original_path),
    source_kind: Some(input.source_kind),
  )
}

fn artifact_type_source_fields(
  artifact_type: Option(String),
) -> List(#(String, json_value.JsonValue)) {
  case artifact_type {
    Some(value) -> [#("artifact_type", json_value.JString(value))]
    None -> []
  }
}

fn contract_type_from_descriptor(
  name: String,
  descriptor: types.ContractDescriptorRecord,
  contract_type: Option(String),
) -> Result(workflow_contract.ContractType, StartError) {
  case contract_type {
    Some(contract_type) ->
      workflow_contract.type_from_string(contract_type)
      |> result.map_error(contract_type_error(name))
    None ->
      workflow_contract.infer_type_from_descriptor(
        workflow_contract.ContractDescriptorSpec(
          kind: Some(descriptor.kind),
          ref_type: descriptor.ref_type,
          media_type: descriptor.media_type,
          artifact_type: descriptor.artifact_type,
        ),
        "workstream input",
        name,
      )
      |> result.map_error(contract_type_error(name))
  }
}

fn base_descriptor_matches(
  actual: types.ContractDescriptorRecord,
  expected: workflow_contract.ContractDescriptorSpec,
) -> Bool {
  Some(actual.kind) == expected.kind
  && expected.ref_type == actual.ref_type
  && expected.media_type == actual.media_type
}

fn artifact_type_matches(
  actual: Option(String),
  expected: Option(String),
) -> Bool {
  case actual, expected {
    None, Some(_) -> True
    _, _ -> actual == expected
  }
}

fn contract_kind(type_: workflow_contract.ContractType) -> String {
  let descriptor = workflow_contract.descriptor_for_type(type_)
  case descriptor.kind {
    Some(kind) -> kind
    None -> "value"
  }
}

fn ledger_artifact_type(input: ResolvedInput) -> String {
  case input.descriptor.artifact_type {
    Some(value) -> value
    None -> workflow_contract.type_to_string(input.contract_type)
  }
}

fn authorize_gate(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  workstream_id: String,
  action_id: String,
  inputs: List(ResolvedInput),
  gate_decision_ids: List(String),
) -> Result(List(String), StartError) {
  decision.authorize_gate(
    checkpoint,
    projected,
    workstream_id,
    action_id,
    list.map(inputs, decision_input),
    gate_decision_ids,
  )
  |> result.map_error(decision_error)
}

fn decision_input(input: ResolvedInput) -> decision.DecisionInput {
  decision.DecisionInput(name: input.name, ref: input.ref, sha256: input.sha256)
}

fn decision_error(error: decision.DecisionError) -> StartError {
  StartError(decision.error_code(error), decision.error_message(error))
}

fn derive_key(
  workstream_id: String,
  action_id: String,
  inputs: List(ResolvedInput),
  gate_decision_ids: List(String),
) -> String {
  start_key.derive_idempotency_key(
    workstream_id,
    action_id,
    list.map(inputs, fn(input) { #(input.name, input.sha256) }),
    gate_decision_ids,
  )
}

fn projected_start_result(
  projected: projection.Projection,
  workstream_id: String,
  action_id: String,
  idempotency_key: String,
  contract_values: workflow_run.ContractRunValues,
) -> Result(Option(StartResult), StartError) {
  case
    start_key.existing_start(
      projected,
      workstream_id,
      action_id,
      idempotency_key,
    )
  {
    Some(existing) ->
      Ok(
        Some(
          Duplicate(duplicate_outcome(workstream_id, existing, contract_values)),
        ),
      )
    None -> {
      use Nil <- result.try(reject_conflicting_start(
        projected,
        workstream_id,
        action_id,
        idempotency_key,
      ))
      Ok(None)
    }
  }
}

fn duplicate_outcome(
  workstream_id: String,
  existing: projection.WorkstreamPhaseRun,
  contract_values: workflow_run.ContractRunValues,
) -> StartOutcome {
  StartOutcome(
    workstream_id: workstream_id,
    phase_run_id: existing.phase_run_id,
    workflow_id: existing.workflow_id,
    action_id: existing.action_id,
    input_bundle_ref: existing.input_bundle_ref,
    input_bundle_sha256: existing.input_bundle_sha256,
    input_bundle_bytes: existing.input_bundle_bytes,
    idempotency_key: existing.idempotency_key,
    records: [],
    contract_values: contract_values,
  )
}

fn reject_conflicting_start(
  projected: projection.Projection,
  workstream_id: String,
  action_id: String,
  idempotency_key: String,
) -> Result(Nil, StartError) {
  case
    start_key.conflicting_start(
      projected,
      workstream_id,
      action_id,
      idempotency_key,
    )
  {
    Some(run) -> conflict_error(action_id, run)
    None -> Ok(Nil)
  }
}

fn conflict_error(
  action_id: String,
  run: projection.WorkstreamPhaseRun,
) -> Result(a, StartError) {
  error(
    "start_conflict",
    "action "
      <> action_id
      <> " already has phase run "
      <> run.phase_run_id
      <> " with different inputs",
  )
}

fn first_input_ref(inputs: List(ResolvedInput)) -> String {
  case inputs {
    [input, ..] -> input.ref
    [] -> start_key.snapshot_ref(string.repeat("0", times: 64))
  }
}

fn required_option(
  value: Option(a),
  code: String,
  message: String,
) -> Result(a, StartError) {
  case value {
    Some(value) -> Ok(value)
    None -> error(code, message)
  }
}

fn spec_error(prefix: String) -> fn(types.SpecError) -> StartError {
  fn(err) {
    StartError(prefix <> ":" <> types.error_code(err), types.error_message(err))
  }
}

fn contract_type_error(
  name: String,
) -> fn(workflow_contract.ContractError) -> StartError {
  fn(err) {
    let workflow_contract.ContractError(_, message) = err
    StartError(
      "contract_type_unknown",
      "unknown contract type for input " <> name <> ": " <> message,
    )
  }
}

fn checkpoint_error(
  code: String,
) -> fn(workflow_checkpoint.CheckpointError) -> StartError {
  fn(err) { StartError(code, workflow_checkpoint.describe_error(err)) }
}

fn error(code: String, message: String) -> Result(a, StartError) {
  Error(StartError(code, message))
}
