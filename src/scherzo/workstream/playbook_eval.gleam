import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/result
import gleam/string
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/projection as state_projection
import scherzo/workflow_contract
import scherzo/workstream/artifacts
import scherzo/workstream/playbook as playbook_def
import scherzo/workstream/projection_snapshot as snapshot
import scherzo/workstream/start_key
import scherzo/workstream/types

type AvailableInput {
  AvailableInput(
    name: String,
    source_phase_id: String,
    ref: String,
    sha256: String,
    bytes: Int,
    contract_type: String,
  )
}

type GateEvaluation {
  GateEvaluation(
    status: String,
    decision_kind: Option(String),
    decision_id: Option(String),
    blocked_reason: Option(String),
    skipped_by_decision_id: Option(String),
  )
}

type LoadedDecision {
  LoadedDecision(
    recorded_at_ms: Int,
    snapshot_ref: String,
    decision: types.DecisionArtifact,
  )
}

type PartialRecommendation {
  PartialRecommendation(
    action: playbook_def.PlaybookAction,
    state: String,
    missing_inputs: List(String),
    input_hashes: List(#(String, String)),
    requires_gate: Option(String),
    gate_status: String,
    gate_decision_id: Option(String),
    idempotency_key: Option(String),
    blocked_reasons: List(String),
    duplicate_phase_run_id: Option(String),
    skipped_by_decision_id: Option(String),
  )
}

pub fn evaluate(
  playbook: playbook_def.Playbook,
  status: state_projection.WorkstreamStatus,
  store: state_artifact_store.Store,
  policy: playbook_def.EvaluationPolicy,
) -> playbook_def.Evaluation {
  let #(inputs, input_warnings) = available_inputs(status, store)
  let #(decisions, decision_warnings) = loaded_decisions(status, store)
  let partials =
    playbook.next_actions
    |> list.map(fn(action) {
      evaluate_action(action, playbook.phases, status, inputs, decisions)
    })
  let recommendations = finalize_auto_enqueue(playbook, policy, partials)
  playbook_def.Evaluation(
    playbook_id: playbook.id,
    workstream_id: status.workstream_id,
    recommendations: recommendations,
    warnings: list.append(input_warnings, decision_warnings),
  )
}

fn available_inputs(
  status: state_projection.WorkstreamStatus,
  store: state_artifact_store.Store,
) -> #(Dict(String, AvailableInput), List(playbook_def.EvaluationWarning)) {
  status.handoffs
  |> dict.values
  |> list.sort(by: compare_handoffs_oldest_first)
  |> list.fold(#(dict.new(), []), fn(acc, handoff) {
    let #(inputs, warnings) = acc
    case
      snapshot.read_text(
        store,
        handoff.handoff_ref,
        handoff.handoff_sha256,
        handoff.handoff_bytes,
      )
    {
      snapshot.ReadProblem(warning) -> #(inputs, [
        warning_from_projection(warning),
        ..warnings
      ])
      snapshot.ReadOk(contents, ..) ->
        case artifacts.decode_handoff(contents) {
          Ok(decoded) -> #(
            insert_handoff_outputs(inputs, decoded.phase_id, decoded.outputs),
            warnings,
          )
          Error(error) -> #(inputs, [
            playbook_def.EvaluationWarning(
              code: "playbook_handoff_decode_failed",
              ref: handoff.handoff_ref,
              message: types.error_code(error)
                <> ": "
                <> types.error_message(error),
            ),
            ..warnings
          ])
        }
    }
  })
  |> reverse_warnings
}

fn insert_handoff_outputs(
  inputs: Dict(String, AvailableInput),
  phase_id: String,
  outputs: List(types.HandoffOutput),
) -> Dict(String, AvailableInput) {
  outputs
  |> list.fold(inputs, fn(acc, output) {
    dict.insert(acc, output.name, available_input(phase_id, output))
  })
}

fn available_input(
  phase_id: String,
  output: types.HandoffOutput,
) -> AvailableInput {
  AvailableInput(
    name: output.name,
    source_phase_id: phase_id,
    ref: output.snapshot.ref,
    sha256: output.snapshot.sha256,
    bytes: output.snapshot.bytes,
    contract_type: snapshot_contract_type(output.snapshot),
  )
}

fn snapshot_contract_type(snapshot: types.ArtifactSnapshot) -> String {
  case snapshot.contract_type {
    Some(contract_type) -> contract_type
    None ->
      workflow_contract.infer_type_from_descriptor(
        workflow_contract.ContractDescriptorSpec(
          kind: Some(snapshot.descriptor.kind),
          ref_type: snapshot.descriptor.ref_type,
          media_type: snapshot.descriptor.media_type,
          artifact_type: snapshot.descriptor.artifact_type,
        ),
        "playbook",
        snapshot.summary,
      )
      |> result.unwrap(workflow_contract.Text)
      |> workflow_contract.type_to_string
  }
}

fn loaded_decisions(
  status: state_projection.WorkstreamStatus,
  store: state_artifact_store.Store,
) -> #(List(LoadedDecision), List(playbook_def.EvaluationWarning)) {
  status.artifacts
  |> dict.values
  |> list.filter(fn(artifact) {
    artifact.artifact_type == types.decision_artifact_type
  })
  |> list.sort(by: compare_artifacts_newest_first)
  |> list.fold(#([], []), fn(acc, artifact) {
    let #(decisions, warnings) = acc
    case
      snapshot.read_text(
        store,
        artifact.snapshot_ref,
        artifact.snapshot_sha256,
        artifact.snapshot_bytes,
      )
    {
      snapshot.ReadProblem(warning) -> #(decisions, [
        warning_from_projection(warning),
        ..warnings
      ])
      snapshot.ReadOk(contents, ..) ->
        case artifacts.decode_decision(contents) {
          Ok(decision) -> #(
            [
              LoadedDecision(
                recorded_at_ms: artifact.recorded_at_ms,
                snapshot_ref: artifact.snapshot_ref,
                decision: decision,
              ),
              ..decisions
            ],
            warnings,
          )
          Error(error) -> #(decisions, [
            playbook_def.EvaluationWarning(
              code: "playbook_decision_decode_failed",
              ref: artifact.snapshot_ref,
              message: types.error_code(error)
                <> ": "
                <> types.error_message(error),
            ),
            ..warnings
          ])
        }
    }
  })
  |> reverse_decisions_and_warnings
}

fn evaluate_action(
  action: playbook_def.PlaybookAction,
  phases: List(playbook_def.PlaybookPhase),
  status: state_projection.WorkstreamStatus,
  inputs: Dict(String, AvailableInput),
  decisions: List(LoadedDecision),
) -> PartialRecommendation {
  let present_inputs = action_inputs(action.required_inputs, inputs)
  let missing_inputs = missing_inputs(action.required_inputs, inputs)
  let phase_reasons =
    from_phase_blocked_reasons(action.from_phase, phases, inputs)
  let gate = gate_evaluation(action, present_inputs, missing_inputs, decisions)
  let input_hashes =
    list.map(present_inputs, fn(input) { #(input.name, input.sha256) })
  let idempotency_key = case missing_inputs, phase_reasons {
    [], [] ->
      case gate.status {
        "approved" | "not_required" ->
          Some(start_key.derive_idempotency_key(
            status.workstream_id,
            action.action_id,
            input_hashes,
            option_to_list(gate.decision_id),
          ))
        _ -> None
      }
    _, _ -> None
  }
  let duplicate_phase_run_id =
    duplicate_phase_run(status, action.action_id, idempotency_key)
  let conflict =
    conflicting_phase_run(status, action.action_id, idempotency_key)
  let blocked_reasons =
    []
    |> append_reasons(missing_input_reasons(missing_inputs))
    |> append_reasons(phase_reasons)
    |> append_reason_option(gate.blocked_reason)
    |> append_reason_option(conflict_reason(conflict))
  let state = action_state(duplicate_phase_run_id, gate, blocked_reasons)
  PartialRecommendation(
    action: action,
    state: state,
    missing_inputs: missing_inputs,
    input_hashes: input_hashes,
    requires_gate: action.requires_gate,
    gate_status: gate.status,
    gate_decision_id: gate.decision_id,
    idempotency_key: idempotency_key,
    blocked_reasons: list.reverse(blocked_reasons),
    duplicate_phase_run_id: duplicate_phase_run_id,
    skipped_by_decision_id: gate.skipped_by_decision_id,
  )
}

fn action_inputs(
  required_inputs: List(String),
  inputs: Dict(String, AvailableInput),
) -> List(AvailableInput) {
  required_inputs
  |> list.filter_map(fn(name) {
    case dict.get(inputs, name) {
      Ok(input) -> Ok(input)
      Error(Nil) -> Error(Nil)
    }
  })
}

fn missing_inputs(
  required_inputs: List(String),
  inputs: Dict(String, AvailableInput),
) -> List(String) {
  required_inputs
  |> list.filter(fn(name) {
    case dict.get(inputs, name) {
      Ok(_) -> False
      Error(Nil) -> True
    }
  })
}

fn from_phase_blocked_reasons(
  from_phase: Option(String),
  phases: List(playbook_def.PlaybookPhase),
  inputs: Dict(String, AvailableInput),
) -> List(String) {
  case from_phase {
    None -> []
    Some(phase_id) ->
      case phase_by_id(phases, phase_id) {
        Error(Nil) -> []
        Ok(phase) ->
          phase.expected_outputs
          |> list.filter_map(fn(output) {
            from_phase_output_reason(phase_id, output.name, inputs)
          })
      }
  }
}

fn phase_by_id(
  phases: List(playbook_def.PlaybookPhase),
  phase_id: String,
) -> Result(playbook_def.PlaybookPhase, Nil) {
  list.find(phases, fn(phase) { phase.phase_id == phase_id })
}

fn from_phase_output_reason(
  phase_id: String,
  output_name: String,
  inputs: Dict(String, AvailableInput),
) -> Result(String, Nil) {
  case dict.get(inputs, output_name) {
    Error(Nil) ->
      Ok("from_phase_output_missing:" <> phase_id <> ":" <> output_name)
    Ok(input) ->
      case input.source_phase_id == phase_id {
        True -> Error(Nil)
        False ->
          Ok(
            "from_phase_output_from_unexpected_phase:"
            <> phase_id
            <> ":"
            <> output_name,
          )
      }
  }
}

fn gate_evaluation(
  action: playbook_def.PlaybookAction,
  inputs: List(AvailableInput),
  missing_inputs: List(String),
  decisions: List(LoadedDecision),
) -> GateEvaluation {
  case action.requires_gate {
    None -> GateEvaluation("not_required", None, None, None, None)
    Some(gate_id) ->
      case missing_inputs {
        [] ->
          matching_gate_decision(
            action.action_id,
            gate_id,
            inputs,
            decisions,
            False,
          )
        _ ->
          GateEvaluation(
            "pending",
            None,
            None,
            Some("gate_waiting_for_inputs:" <> gate_id),
            None,
          )
      }
  }
}

fn matching_gate_decision(
  action_id: String,
  gate_id: String,
  inputs: List(AvailableInput),
  decisions: List(LoadedDecision),
  saw_stale: Bool,
) -> GateEvaluation {
  case decisions {
    [] ->
      case saw_stale {
        True ->
          GateEvaluation(
            "stale",
            None,
            None,
            Some("gate_stale:" <> gate_id),
            None,
          )
        False ->
          GateEvaluation(
            "pending",
            None,
            None,
            Some("gate_pending:" <> gate_id),
            None,
          )
      }
    [loaded, ..rest] -> {
      let decision = loaded.decision
      case decision.action_id == action_id && decision.gate_id == gate_id {
        False ->
          matching_gate_decision(action_id, gate_id, inputs, rest, saw_stale)
        True ->
          case decision_inputs_match(inputs, decision.inputs) {
            False ->
              matching_gate_decision(action_id, gate_id, inputs, rest, True)
            True -> gate_from_decision(decision)
          }
      }
    }
  }
}

fn gate_from_decision(decision: types.DecisionArtifact) -> GateEvaluation {
  case decision.kind {
    "approve" ->
      GateEvaluation(
        "approved",
        Some(decision.kind),
        Some(decision.artifact_id),
        None,
        None,
      )
    "deviate" ->
      GateEvaluation(
        "deviated",
        Some(decision.kind),
        Some(decision.artifact_id),
        Some("deviated:" <> decision.artifact_id),
        Some(decision.artifact_id),
      )
    "reject" ->
      GateEvaluation(
        "rejected",
        Some(decision.kind),
        Some(decision.artifact_id),
        Some("gate_rejected:" <> decision.artifact_id),
        None,
      )
    "request_changes" ->
      GateEvaluation(
        "changes_requested",
        Some(decision.kind),
        Some(decision.artifact_id),
        Some("gate_changes_requested:" <> decision.artifact_id),
        None,
      )
    other ->
      GateEvaluation(
        "blocked",
        Some(other),
        Some(decision.artifact_id),
        Some("gate_decision_not_approved:" <> other),
        None,
      )
  }
}

fn decision_inputs_match(
  expected: List(AvailableInput),
  actual: List(types.DecisionInputRef),
) -> Bool {
  normalize_available_inputs(expected) == normalize_decision_inputs(actual)
}

fn normalize_available_inputs(inputs: List(AvailableInput)) -> List(String) {
  inputs
  |> list.map(fn(input) {
    frame_string(input.name)
    <> frame_string(input.ref)
    <> frame_string(input.sha256)
  })
  |> list.sort(by: string.compare)
}

fn normalize_decision_inputs(
  inputs: List(types.DecisionInputRef),
) -> List(String) {
  inputs
  |> list.map(fn(input) {
    frame_string(input.name)
    <> frame_string(input.ref)
    <> frame_string(input.sha256)
  })
  |> list.sort(by: string.compare)
}

fn duplicate_phase_run(
  status: state_projection.WorkstreamStatus,
  action_id: String,
  idempotency_key: Option(String),
) -> Option(String) {
  case idempotency_key {
    Some(key) ->
      case
        status.queued_phase_runs
        |> dict.values
        |> list.find(fn(run) {
          run.action_id == action_id && run.idempotency_key == key
        })
      {
        Ok(run) -> Some(run.phase_run_id)
        Error(Nil) -> None
      }
    None -> any_phase_run_for_action(status, action_id)
  }
}

fn any_phase_run_for_action(
  status: state_projection.WorkstreamStatus,
  action_id: String,
) -> Option(String) {
  case
    status.queued_phase_runs
    |> dict.values
    |> list.sort(by: compare_phase_runs_oldest_first)
    |> list.find(fn(run) { run.action_id == action_id })
  {
    Ok(run) -> Some(run.phase_run_id)
    Error(Nil) -> None
  }
}

fn conflicting_phase_run(
  status: state_projection.WorkstreamStatus,
  action_id: String,
  idempotency_key: Option(String),
) -> Option(String) {
  case idempotency_key {
    None -> None
    Some(key) ->
      case
        status.queued_phase_runs
        |> dict.values
        |> list.find(fn(run) {
          run.action_id == action_id && run.idempotency_key != key
        })
      {
        Ok(run) -> Some(run.phase_run_id)
        Error(Nil) -> None
      }
  }
}

fn conflict_reason(conflict: Option(String)) -> Option(String) {
  case conflict {
    Some(phase_run_id) -> Some("start_conflict:" <> phase_run_id)
    None -> None
  }
}

fn action_state(
  duplicate_phase_run_id: Option(String),
  gate: GateEvaluation,
  blocked_reasons: List(String),
) -> String {
  case duplicate_phase_run_id {
    Some(_) -> "queued"
    None ->
      case gate.skipped_by_decision_id {
        Some(_) -> "deviated"
        None ->
          case blocked_reasons {
            [] -> "available"
            _ -> "blocked"
          }
      }
  }
}

fn finalize_auto_enqueue(
  playbook: playbook_def.Playbook,
  policy: playbook_def.EvaluationPolicy,
  partials: List(PartialRecommendation),
) -> List(playbook_def.Recommendation) {
  partials
  |> list.fold(#([], 0), fn(acc, partial) {
    let #(recommendations, ready_count) = acc
    let #(recommendation, next_ready_count) =
      finalize_recommendation(playbook, policy, partial, ready_count)
    #([recommendation, ..recommendations], next_ready_count)
  })
  |> first
  |> list.reverse
}

fn finalize_recommendation(
  playbook: playbook_def.Playbook,
  policy: playbook_def.EvaluationPolicy,
  partial: PartialRecommendation,
  ready_count: Int,
) -> #(playbook_def.Recommendation, Int) {
  let action = partial.action
  let #(auto_status, next_ready_count) =
    auto_enqueue_status(playbook, policy, partial, ready_count)
  #(
    playbook_def.Recommendation(
      action_id: action.action_id,
      label: action.label,
      workflow_id: action.workflow_id,
      state: partial.state,
      priority: action.priority,
      reason: action.reason,
      required_inputs: action.required_inputs,
      missing_inputs: partial.missing_inputs,
      input_hashes: partial.input_hashes,
      requires_gate: partial.requires_gate,
      gate_status: partial.gate_status,
      gate_decision_id: partial.gate_decision_id,
      auto_enqueue: action.auto_enqueue,
      auto_enqueue_status: auto_status,
      idempotency_key: partial.idempotency_key,
      blocked_reasons: partial.blocked_reasons,
      duplicate_phase_run_id: partial.duplicate_phase_run_id,
      skipped_by_decision_id: partial.skipped_by_decision_id,
    ),
    next_ready_count,
  )
}

fn auto_enqueue_status(
  playbook: playbook_def.Playbook,
  policy: playbook_def.EvaluationPolicy,
  partial: PartialRecommendation,
  ready_count: Int,
) -> #(String, Int) {
  let action = partial.action
  case action.auto_enqueue {
    False -> #("not_requested", ready_count)
    True ->
      case playbook.auto_enqueue.enabled {
        False -> #("disabled_by_playbook", ready_count)
        True ->
          case policy.auto_enqueue_enabled {
            False -> #("disabled_by_policy", ready_count)
            True ->
              auto_enqueue_status_after_policy(
                playbook,
                policy,
                partial,
                ready_count,
              )
          }
      }
  }
}

fn auto_enqueue_status_after_policy(
  playbook: playbook_def.Playbook,
  policy: playbook_def.EvaluationPolicy,
  partial: PartialRecommendation,
  ready_count: Int,
) -> #(String, Int) {
  case policy.workstream_held {
    True -> #("blocked:workstream_held", ready_count)
    False ->
      case partial.duplicate_phase_run_id {
        Some(_) -> #("duplicate", ready_count)
        None ->
          case partial.state == "available", partial.idempotency_key {
            True, Some(_) ->
              case
                ready_count < playbook.auto_enqueue.max_actions_per_evaluation
              {
                True -> #("ready", ready_count + 1)
                False -> #("blocked:auto_enqueue_limit_reached", ready_count)
              }
            _, _ -> #("blocked", ready_count)
          }
      }
  }
}

fn missing_input_reasons(inputs: List(String)) -> List(String) {
  inputs |> list.map(fn(name) { "missing_input:" <> name })
}

fn append_reasons(existing: List(String), more: List(String)) -> List(String) {
  list.append(list.reverse(more), existing)
}

fn append_reason_option(
  existing: List(String),
  reason: Option(String),
) -> List(String) {
  case reason {
    Some(reason) -> [reason, ..existing]
    None -> existing
  }
}

fn option_to_list(value: Option(String)) -> List(String) {
  case value {
    Some(value) -> [value]
    None -> []
  }
}

fn warning_from_projection(
  warning: snapshot.ProjectionWarning,
) -> playbook_def.EvaluationWarning {
  playbook_def.EvaluationWarning(
    code: warning.code,
    ref: warning.ref,
    message: warning.message,
  )
}

fn reverse_warnings(
  value: #(Dict(String, AvailableInput), List(playbook_def.EvaluationWarning)),
) -> #(Dict(String, AvailableInput), List(playbook_def.EvaluationWarning)) {
  let #(inputs, warnings) = value
  #(inputs, list.reverse(warnings))
}

fn reverse_decisions_and_warnings(
  value: #(List(LoadedDecision), List(playbook_def.EvaluationWarning)),
) -> #(List(LoadedDecision), List(playbook_def.EvaluationWarning)) {
  let #(decisions, warnings) = value
  #(list.reverse(decisions), list.reverse(warnings))
}

fn first(value: #(a, b)) -> a {
  let #(first, _) = value
  first
}

fn compare_handoffs_oldest_first(
  a: state_projection.WorkstreamHandoffSnapshot,
  b: state_projection.WorkstreamHandoffSnapshot,
) -> Order {
  compare_time_then_string(
    a.recorded_at_ms,
    b.recorded_at_ms,
    a.handoff_ref,
    b.handoff_ref,
  )
}

fn compare_artifacts_newest_first(
  a: state_projection.WorkstreamArtifactSnapshot,
  b: state_projection.WorkstreamArtifactSnapshot,
) -> Order {
  case int.compare(a.recorded_at_ms, b.recorded_at_ms) {
    Eq -> string.compare(a.snapshot_ref, b.snapshot_ref)
    Lt -> Gt
    Gt -> Lt
  }
}

fn compare_phase_runs_oldest_first(
  a: state_projection.WorkstreamPhaseRun,
  b: state_projection.WorkstreamPhaseRun,
) -> Order {
  compare_time_then_string(
    a.queued_at_ms,
    b.queued_at_ms,
    a.phase_run_id,
    b.phase_run_id,
  )
}

fn compare_time_then_string(
  a_time: Int,
  b_time: Int,
  a_value: String,
  b_value: String,
) -> Order {
  case int.compare(a_time, b_time) {
    Eq -> string.compare(a_value, b_value)
    other -> other
  }
}

fn frame_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}
