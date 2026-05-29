import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/state/ledger as state_ledger
import scherzo/state/projection
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_identity
import scherzo/workstream/artifacts
import scherzo/workstream/ledger
import scherzo/workstream/start_key
import scherzo/workstream/types

const id_hash_chars = 12

pub type DecisionInput {
  DecisionInput(name: String, ref: String, sha256: String)
}

pub type RecordRequest {
  RecordRequest(
    workstream_id: String,
    action_id: String,
    gate_id: String,
    kind: String,
    decided_at_ms: Int,
    decided_by: String,
    rationale: String,
    inputs: List(DecisionInput),
    summary: String,
  )
}

pub type RecordedDecision {
  RecordedDecision(
    artifact_id: String,
    snapshot_ref: String,
    snapshot_sha256: String,
    snapshot_bytes: Int,
    record_id: String,
  )
}

pub type DecisionError {
  DecisionError(code: String, message: String)
}

type LoadedDecision {
  LoadedDecision(
    artifact: projection.WorkstreamArtifactSnapshot,
    decision: types.DecisionArtifact,
  )
}

pub fn error_code(error: DecisionError) -> String {
  let DecisionError(code, _) = error
  code
}

pub fn error_message(error: DecisionError) -> String {
  let DecisionError(_, message) = error
  message
}

pub fn command_kind(command: String) -> Result(String, DecisionError) {
  case command {
    "approve" -> Ok("approve")
    "request-changes" | "request_changes" -> Ok("request_changes")
    "reject" -> Ok("reject")
    "deviate" -> Ok("deviate")
    other -> error("decision_kind_unknown", "unknown decision kind: " <> other)
  }
}

pub fn record(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  request: RecordRequest,
) -> Result(RecordedDecision, DecisionError) {
  use status <- result.try(require_workstream(projected, request.workstream_id))
  use Nil <- result.try(require_gate_request(
    checkpoint,
    status,
    request.action_id,
    request.gate_id,
  ))
  use Nil <- result.try(validate_request(request))
  use Nil <- result.try(verify_inputs(checkpoint, request.inputs))
  let artifact_id = decision_artifact_id(request)
  let artifact =
    types.DecisionArtifact(
      artifact_id: artifact_id,
      workstream_id: request.workstream_id,
      action_id: request.action_id,
      gate_id: request.gate_id,
      kind: request.kind,
      decided_at_ms: request.decided_at_ms,
      decided_by: request.decided_by,
      rationale: request.rationale,
      inputs: list.map(request.inputs, decision_input_to_type),
      summary: request.summary,
    )
  let contents = artifacts.decision_to_string(artifact)
  use Nil <- result.try(
    artifacts.decode_decision(contents)
    |> result.map(fn(_) { Nil })
    |> result.map_error(spec_error("decision_invalid")),
  )
  use snapshot <- result.try(
    checkpoint.snapshot_workstream_bytes(
      decision_original_path(artifact_id),
      "application/json",
      bit_array.from_string(contents),
    )
    |> result.map_error(checkpoint_error("decision_snapshot_failed")),
  )
  let record =
    ledger.workstream_artifact_recorded(
      request.decided_at_ms,
      request.workstream_id,
      artifact_id,
      types.decision_artifact_type,
      snapshot.ref,
      snapshot.sha256,
      snapshot.bytes,
      snapshot.original_path,
      workflow_contract.type_to_string(workflow_contract.ArtifactList),
      "application/json",
      "operator",
      request.decided_by,
      "workstream_decision",
      artifact_id,
    )
  use append_result <- result.try(
    checkpoint.append_workstream_record_idempotent(record)
    |> result.map_error(checkpoint_error("decision_ledger_append_failed")),
  )
  case append_result {
    state_ledger.Appended | state_ledger.AlreadyRecorded(_) ->
      Ok(RecordedDecision(
        artifact_id: artifact_id,
        snapshot_ref: snapshot.ref,
        snapshot_sha256: snapshot.sha256,
        snapshot_bytes: snapshot.bytes,
        record_id: record.record_id,
      ))
  }
}

pub fn authorize_gate(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  workstream_id: String,
  action_id: String,
  inputs: List(DecisionInput),
  supplied_decision_ids: List(String),
) -> Result(List(String), DecisionError) {
  case dict.get(projected.workstreams, workstream_id) {
    Error(Nil) -> Ok(supplied_decision_ids)
    Ok(status) -> {
      use gate <- result.try(gate_requirement(checkpoint, status, action_id))
      case gate {
        None -> Ok(supplied_decision_ids)
        Some(gate_id) -> {
          use decision_id <- result.try(require_approved_decision(
            checkpoint,
            status,
            action_id,
            gate_id,
            inputs,
            supplied_decision_ids,
          ))
          Ok(add_unique(supplied_decision_ids, decision_id))
        }
      }
    }
  }
}

pub fn require_gate_request(
  checkpoint: workflow_checkpoint.Writer,
  status: projection.WorkstreamStatus,
  action_id: String,
  gate_id: String,
) -> Result(Nil, DecisionError) {
  use requirement <- result.try(gate_requirement(checkpoint, status, action_id))
  case requirement {
    Some(required_gate) if required_gate == gate_id -> Ok(Nil)
    Some(required_gate) ->
      error(
        "gate_request_mismatch",
        "next action "
          <> action_id
          <> " requires gate "
          <> required_gate
          <> ", not "
          <> gate_id,
      )
    None ->
      error(
        "gate_request_not_found",
        "next action " <> action_id <> " does not declare a gate requirement",
      )
  }
}

fn gate_requirement(
  checkpoint: workflow_checkpoint.Writer,
  status: projection.WorkstreamStatus,
  action_id: String,
) -> Result(Option(String), DecisionError) {
  status.artifacts
  |> dict.values
  |> list.filter(fn(artifact) {
    artifact.artifact_type == types.next_action_artifact_type
  })
  |> list.sort(by: compare_artifacts_newest_first)
  |> gate_requirement_loop(checkpoint, action_id)
}

fn gate_requirement_loop(
  artifacts: List(projection.WorkstreamArtifactSnapshot),
  checkpoint: workflow_checkpoint.Writer,
  action_id: String,
) -> Result(Option(String), DecisionError) {
  case artifacts {
    [] -> Ok(None)
    [artifact, ..rest] ->
      case read_next_action(checkpoint, artifact) {
        Ok(next_action) ->
          case next_action.action_id == action_id {
            True -> Ok(next_action.requires_gate)
            False -> gate_requirement_loop(rest, checkpoint, action_id)
          }
        Error(read_error) -> Error(read_error)
      }
  }
}

fn require_approved_decision(
  checkpoint: workflow_checkpoint.Writer,
  status: projection.WorkstreamStatus,
  action_id: String,
  gate_id: String,
  inputs: List(DecisionInput),
  supplied_decision_ids: List(String),
) -> Result(String, DecisionError) {
  let candidates =
    status.artifacts
    |> dict.values
    |> list.filter(fn(artifact) {
      artifact.artifact_type == types.decision_artifact_type
    })
    |> list.sort(by: compare_artifacts_newest_first)
  find_approved_decision(
    checkpoint,
    candidates,
    action_id,
    gate_id,
    inputs,
    supplied_decision_ids,
    False,
  )
}

fn find_approved_decision(
  checkpoint: workflow_checkpoint.Writer,
  artifacts: List(projection.WorkstreamArtifactSnapshot),
  action_id: String,
  gate_id: String,
  inputs: List(DecisionInput),
  supplied_decision_ids: List(String),
  saw_stale_candidate: Bool,
) -> Result(String, DecisionError) {
  case artifacts {
    [] ->
      case saw_stale_candidate {
        True ->
          error(
            "gate_decision_stale",
            "recorded decisions target older or different snapshot hashes",
          )
        False ->
          error(
            "gate_decision_missing",
            "no approving decision found for gate " <> gate_id,
          )
      }
    [artifact, ..rest] ->
      case read_decision(checkpoint, artifact) {
        Error(read_error) -> Error(read_error)
        Ok(LoadedDecision(_, decision)) ->
          case decision.action_id == action_id && decision.gate_id == gate_id {
            False ->
              find_approved_decision(
                checkpoint,
                rest,
                action_id,
                gate_id,
                inputs,
                supplied_decision_ids,
                saw_stale_candidate,
              )
            True ->
              case same_inputs(inputs, decision.inputs) {
                False ->
                  find_approved_decision(
                    checkpoint,
                    rest,
                    action_id,
                    gate_id,
                    inputs,
                    supplied_decision_ids,
                    True,
                  )
                True ->
                  case decision.kind {
                    "approve" ->
                      case
                        decision_matches_supplied(
                          decision,
                          artifact,
                          supplied_decision_ids,
                        )
                      {
                        True -> Ok(decision.artifact_id)
                        False ->
                          error(
                            "gate_decision_mismatch",
                            "latest approving decision was not supplied",
                          )
                      }
                    other ->
                      error(
                        "gate_decision_not_approved",
                        "latest decision for matching inputs is " <> other,
                      )
                  }
              }
          }
      }
  }
}

fn validate_request(request: RecordRequest) -> Result(Nil, DecisionError) {
  use Nil <- result.try(non_empty(
    request.workstream_id,
    "workstream_id_missing",
    "workstream_id is required",
  ))
  use Nil <- result.try(non_empty(
    request.action_id,
    "action_id_missing",
    "action_id is required",
  ))
  use Nil <- result.try(non_empty(
    request.gate_id,
    "gate_id_missing",
    "gate_id is required",
  ))
  use Nil <- result.try(command_kind(request.kind) |> result.map(fn(_) { Nil }))
  use Nil <- result.try(case request.decided_at_ms > 0 {
    True -> Ok(Nil)
    False -> error("decided_at_ms_invalid", "decided_at_ms must be positive")
  })
  use Nil <- result.try(non_empty(
    request.decided_by,
    "actor_missing",
    "actor is required",
  ))
  use Nil <- result.try(non_empty(
    request.rationale,
    "rationale_missing",
    "rationale is required",
  ))
  case request.inputs {
    [] -> error("decision_inputs_missing", "at least one input is required")
    _ -> Ok(Nil)
  }
}

fn verify_inputs(
  checkpoint: workflow_checkpoint.Writer,
  inputs: List(DecisionInput),
) -> Result(Nil, DecisionError) {
  case inputs {
    [] -> Ok(Nil)
    [input, ..rest] -> {
      use Nil <- result.try(verify_input(checkpoint, input))
      verify_inputs(checkpoint, rest)
    }
  }
}

fn verify_input(
  checkpoint: workflow_checkpoint.Writer,
  input: DecisionInput,
) -> Result(Nil, DecisionError) {
  use Nil <- result.try(validate_snapshot_ref(input.ref, input.sha256))
  use contents <- result.try(
    checkpoint.read_artifact(input.ref)
    |> result.map_error(checkpoint_error("decision_input_snapshot_read_failed")),
  )
  case hash.sha256_hex(contents) == input.sha256 {
    True -> Ok(Nil)
    False ->
      error(
        "decision_input_snapshot_hash_mismatch",
        "snapshot hash mismatch for " <> input.ref,
      )
  }
}

fn read_next_action(
  checkpoint: workflow_checkpoint.Writer,
  artifact: projection.WorkstreamArtifactSnapshot,
) -> Result(types.NextActionArtifact, DecisionError) {
  use contents <- result.try(read_verified_artifact(checkpoint, artifact))
  artifacts.decode_next_action(contents)
  |> result.map_error(spec_error("next_action_invalid"))
}

fn read_decision(
  checkpoint: workflow_checkpoint.Writer,
  artifact: projection.WorkstreamArtifactSnapshot,
) -> Result(LoadedDecision, DecisionError) {
  use contents <- result.try(read_verified_artifact(checkpoint, artifact))
  use decoded <- result.try(
    artifacts.decode_decision(contents)
    |> result.map_error(spec_error("decision_invalid")),
  )
  Ok(LoadedDecision(artifact: artifact, decision: decoded))
}

fn read_verified_artifact(
  checkpoint: workflow_checkpoint.Writer,
  artifact: projection.WorkstreamArtifactSnapshot,
) -> Result(String, DecisionError) {
  use Nil <- result.try(validate_snapshot_ref(
    artifact.snapshot_ref,
    artifact.snapshot_sha256,
  ))
  use contents <- result.try(
    checkpoint.read_artifact(artifact.snapshot_ref)
    |> result.map_error(checkpoint_error("gate_artifact_read_failed")),
  )
  case hash.sha256_hex(contents) == artifact.snapshot_sha256 {
    True -> Ok(contents)
    False ->
      error(
        "gate_artifact_hash_mismatch",
        "artifact hash mismatch for " <> artifact.snapshot_ref,
      )
  }
}

fn require_workstream(
  projected: projection.Projection,
  workstream_id: String,
) -> Result(projection.WorkstreamStatus, DecisionError) {
  case dict.get(projected.workstreams, workstream_id) {
    Ok(status) -> Ok(status)
    Error(Nil) ->
      error("workstream_not_recorded", "workstream is not recorded in ledger")
  }
}

fn validate_snapshot_ref(
  ref: String,
  sha256: String,
) -> Result(Nil, DecisionError) {
  case start_key.valid_snapshot_ref(ref, sha256) {
    True -> Ok(Nil)
    False ->
      error(
        "snapshot_ref_invalid",
        "snapshot ref must match a lowercase sha256",
      )
  }
}

fn decision_input_to_type(input: DecisionInput) -> types.DecisionInputRef {
  types.DecisionInputRef(name: input.name, ref: input.ref, sha256: input.sha256)
}

fn input_from_type(input: types.DecisionInputRef) -> DecisionInput {
  DecisionInput(name: input.name, ref: input.ref, sha256: input.sha256)
}

fn same_inputs(
  expected: List(DecisionInput),
  actual: List(types.DecisionInputRef),
) -> Bool {
  normalize_inputs(expected)
  == normalize_inputs(list.map(actual, input_from_type))
}

fn normalize_inputs(inputs: List(DecisionInput)) -> List(String) {
  inputs
  |> list.map(fn(input) {
    frame_string(input.name)
    <> frame_string(input.ref)
    <> frame_string(input.sha256)
  })
  |> list.sort(by: string.compare)
}

fn decision_matches_supplied(
  decision: types.DecisionArtifact,
  artifact: projection.WorkstreamArtifactSnapshot,
  supplied_decision_ids: List(String),
) -> Bool {
  case supplied_decision_ids {
    [] -> True
    _ ->
      list.contains(supplied_decision_ids, decision.artifact_id)
      || list.contains(supplied_decision_ids, artifact.snapshot_ref)
  }
}

fn add_unique(values: List(String), value: String) -> List(String) {
  case list.contains(values, value) {
    True -> values
    False -> [value, ..values]
  }
}

fn compare_artifacts_newest_first(
  a: projection.WorkstreamArtifactSnapshot,
  b: projection.WorkstreamArtifactSnapshot,
) -> order.Order {
  case a.recorded_at_ms > b.recorded_at_ms {
    True -> order.Lt
    False ->
      case a.recorded_at_ms < b.recorded_at_ms {
        True -> order.Gt
        False -> string.compare(a.snapshot_ref, b.snapshot_ref)
      }
  }
}

fn decision_artifact_id(request: RecordRequest) -> String {
  "decision:"
  <> workflow_identity.safe_component(request.kind, "kind")
  <> ":"
  <> workflow_identity.safe_component(request.action_id, "action")
  <> ":"
  <> workflow_identity.safe_component(request.gate_id, "gate")
  <> ":"
  <> hash.short_sha256_hex(decision_fingerprint(request), id_hash_chars)
}

fn decision_fingerprint(request: RecordRequest) -> String {
  "workstream_decision:v1"
  <> frame_string(request.workstream_id)
  <> frame_string(request.action_id)
  <> frame_string(request.gate_id)
  <> frame_string(request.kind)
  <> frame_string(request.decided_by)
  <> frame_string(request.rationale)
  <> frame_string(request.summary)
  <> frame_string(int_to_string(request.decided_at_ms))
  <> frame_list(normalize_inputs(request.inputs))
}

fn decision_original_path(artifact_id: String) -> String {
  "workstream/decisions/"
  <> workflow_identity.safe_component(artifact_id, "decision")
  <> ".json"
}

fn frame_list(values: List(String)) -> String {
  int_to_string(list.length(values))
  <> "["
  <> string.join(values, with: "")
  <> "]"
}

fn frame_string(value: String) -> String {
  int_to_string(string.length(value)) <> ":" <> value
}

fn int_to_string(value: Int) -> String {
  int.to_string(value)
}

fn non_empty(
  value: String,
  code: String,
  message: String,
) -> Result(Nil, DecisionError) {
  case string.trim(value) == "" {
    True -> error(code, message)
    False -> Ok(Nil)
  }
}

fn spec_error(code: String) -> fn(types.SpecError) -> DecisionError {
  fn(error) {
    DecisionError(
      code,
      types.error_code(error) <> ": " <> types.error_message(error),
    )
  }
}

fn checkpoint_error(
  code: String,
) -> fn(workflow_checkpoint.CheckpointError) -> DecisionError {
  fn(error) { DecisionError(code, workflow_checkpoint.describe_error(error)) }
}

fn error(code: String, message: String) -> Result(a, DecisionError) {
  Error(DecisionError(code, message))
}
