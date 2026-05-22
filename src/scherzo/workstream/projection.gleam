import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/string
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/projection as state_projection
import scherzo/state/record
import scherzo/workstream/artifacts
import scherzo/workstream/projection_snapshot as snapshot
import scherzo/workstream/types

const max_summary_chars = 240

pub type ArtifactDetail {
  ArtifactUndecoded
  ArtifactDecodeFailed(code: String, message: String)
  WorkstreamDetail(status: String, summary: String, next_actions: List(String))
  AssignmentDetail(
    workflow_id: String,
    playbook_id: Option(String),
    reason: String,
  )
  NextActionDetail(
    action_id: String,
    workflow_id: String,
    state: String,
    priority: Int,
    inputs: List(String),
    requires_gate: Option(String),
    auto_enqueue: Bool,
    resolved_by_phase_run_id: Option(String),
  )
  DecisionDetail(kind: String, summary: String, decided_by: String)
  InputBundleDetail(
    workflow_id: String,
    source_handoff_ref: String,
    inputs: List(String),
    source_kind: Option(String),
    source_reason: Option(String),
  )
}

pub type ArtifactInspection {
  ArtifactInspection(
    artifact_id: String,
    artifact_type: String,
    snapshot_ref: String,
    snapshot_sha256: String,
    snapshot_bytes: Int,
    original_path: String,
    contract_type: String,
    media_type: String,
    producer_workflow_id: String,
    producer_run_id: String,
    producer_step_id: String,
    recorded_at_ms: Int,
    snapshot_status: snapshot.SnapshotStatus,
    detail: ArtifactDetail,
  )
}

pub type HandoffInspection {
  HandoffInspection(
    handoff_id: String,
    handoff_ref: String,
    handoff_sha256: String,
    handoff_bytes: Int,
    source_workflow_id: String,
    source_run_id: String,
    recorded_at_ms: Int,
    snapshot_status: snapshot.SnapshotStatus,
    phase_id: Option(String),
    summary: Option(String),
    outputs: List(String),
    recommended_next_actions: List(String),
    open_questions: List(String),
    decode_error: Option(snapshot.ProjectionWarning),
  )
}

pub type PhaseRunInspection {
  PhaseRunInspection(
    phase_run_id: String,
    action_id: String,
    workflow_id: String,
    input_bundle_ref: String,
    input_bundle_sha256: String,
    input_bundle_bytes: Int,
    queued_at_ms: Int,
  )
}

pub type PhaseInspection {
  PhaseInspection(
    phase_id: String,
    latest_handoff_ref: Option(String),
    latest_handoff_summary: Option(String),
    handoff_count: Int,
  )
}

pub type NextActionInspection {
  NextActionInspection(
    artifact_id: String,
    action_id: String,
    workflow_id: String,
    state: String,
    priority: Int,
    inputs: List(String),
    requires_gate: Option(String),
    auto_enqueue: Bool,
    snapshot_ref: String,
    snapshot_sha256: String,
    recorded_at_ms: Int,
    resolved_by_phase_run_id: Option(String),
  )
}

pub type DecisionInspection {
  DecisionInspection(
    artifact_id: String,
    kind: String,
    summary: String,
    decided_by: String,
    snapshot_ref: String,
    snapshot_sha256: String,
    recorded_at_ms: Int,
  )
}

pub type AssignmentInspection {
  AssignmentInspection(
    assignment_id: String,
    workflow_id: String,
    playbook_id: Option(String),
    reason: String,
    assigned_at_ms: Int,
  )
}

pub type WorkstreamSummary {
  WorkstreamSummary(
    workstream_id: String,
    task_ref: Option(record.TaskRefFields),
    status: String,
    created_at_ms: Option(Int),
    latest_assignment: Option(AssignmentInspection),
    artifact_count: Int,
    handoff_count: Int,
    queued_phase_run_count: Int,
  )
}

pub type WorkstreamInspection {
  WorkstreamInspection(
    workstream_id: String,
    task_ref: Option(record.TaskRefFields),
    status: String,
    created_at_ms: Option(Int),
    latest_assignment: Option(AssignmentInspection),
    phases: List(PhaseInspection),
    artifacts: List(ArtifactInspection),
    handoffs: List(HandoffInspection),
    queued_phase_runs: List(PhaseRunInspection),
    next_actions: List(NextActionInspection),
    unresolved_next_actions: List(NextActionInspection),
    decisions: List(DecisionInspection),
    warnings: List(snapshot.ProjectionWarning),
  )
}

pub fn summaries(
  projected: state_projection.Projection,
  store: state_artifact_store.Store,
) -> List(WorkstreamSummary) {
  projected.workstreams
  |> dict.to_list
  |> list.sort(by: compare_workstream_entries)
  |> list.map(fn(entry) {
    let #(_, status) = entry
    summary(store, status)
  })
}

pub fn summaries_for_ref(
  projected: state_projection.Projection,
  store: state_artifact_store.Store,
  task_or_workstream_ref: String,
) -> List(WorkstreamSummary) {
  projected
  |> workstreams_for_ref(task_or_workstream_ref)
  |> list.map(fn(status) { summary(store, status) })
}

pub fn inspect(
  store: state_artifact_store.Store,
  status: state_projection.WorkstreamStatus,
) -> WorkstreamInspection {
  let artifact_views = inspect_artifacts(store, status)
  let handoff_views = inspect_handoffs(store, status)
  let phase_runs = inspect_phase_runs(status)
  let next_actions = next_actions_from_artifacts(artifact_views, phase_runs)
  let unresolved_next_actions = unresolved_next_actions_from(next_actions)
  let decisions = decisions_from_artifacts(artifact_views)
  let warnings =
    []
    |> append_warnings(task_ref_warnings(status))
    |> append_warnings(artifact_warnings(artifact_views))
    |> append_warnings(handoff_warnings(handoff_views))
  WorkstreamInspection(
    workstream_id: status.workstream_id,
    task_ref: status.task_ref,
    status: current_status(artifact_views, status),
    created_at_ms: status.created_at_ms,
    latest_assignment: option_assignment(status.latest_assignment),
    phases: phases_from_handoffs(handoff_views),
    artifacts: artifact_views,
    handoffs: handoff_views,
    queued_phase_runs: phase_runs,
    next_actions: next_actions,
    unresolved_next_actions: unresolved_next_actions,
    decisions: decisions,
    warnings: warnings,
  )
}

pub fn inspect_by_ref(
  projected: state_projection.Projection,
  store: state_artifact_store.Store,
  task_or_workstream_ref: String,
) -> List(WorkstreamInspection) {
  projected
  |> workstreams_for_ref(task_or_workstream_ref)
  |> list.map(fn(status) { inspect(store, status) })
}

fn summary(
  store: state_artifact_store.Store,
  status: state_projection.WorkstreamStatus,
) -> WorkstreamSummary {
  WorkstreamSummary(
    workstream_id: status.workstream_id,
    task_ref: status.task_ref,
    status: summary_status(store, status),
    created_at_ms: status.created_at_ms,
    latest_assignment: option_assignment(status.latest_assignment),
    artifact_count: dict.size(status.artifacts),
    handoff_count: dict.size(status.handoffs),
    queued_phase_run_count: dict.size(status.queued_phase_runs),
  )
}

fn summary_status(
  store: state_artifact_store.Store,
  status: state_projection.WorkstreamStatus,
) -> String {
  let fallback = fallback_status(status)
  case latest_workstream_artifact(status.artifacts) {
    Some(artifact) -> decoded_workstream_status(store, artifact, fallback)
    None -> fallback
  }
}

fn latest_workstream_artifact(
  artifacts: Dict(String, state_projection.WorkstreamArtifactSnapshot),
) -> Option(state_projection.WorkstreamArtifactSnapshot) {
  artifacts
  |> dict.values
  |> list.fold(None, fn(best, artifact) {
    case artifact.artifact_type == types.workstream_artifact_type, best {
      False, _ -> best
      True, None -> Some(artifact)
      True, Some(existing) ->
        case artifact.recorded_at_ms >= existing.recorded_at_ms {
          True -> Some(artifact)
          False -> best
        }
    }
  })
}

fn decoded_workstream_status(
  store: state_artifact_store.Store,
  artifact: state_projection.WorkstreamArtifactSnapshot,
  fallback: String,
) -> String {
  let read =
    snapshot.read_text(
      store,
      artifact.snapshot_ref,
      artifact.snapshot_sha256,
      artifact.snapshot_bytes,
    )
  case read {
    snapshot.ReadOk(contents: contents, ..) ->
      case artifacts.decode_workstream(contents) {
        Ok(decoded) -> decoded.status
        Error(error) -> "decode_failed:" <> types.error_code(error)
      }
    snapshot.ReadProblem(_warning) -> fallback
  }
}

fn fallback_status(status: state_projection.WorkstreamStatus) -> String {
  case
    dict.size(status.artifacts) > 0
    || dict.size(status.handoffs) > 0
    || dict.size(status.queued_phase_runs) > 0
    || status.latest_assignment != None
  {
    True -> "active"
    False -> "unknown"
  }
}

fn inspect_artifacts(
  store: state_artifact_store.Store,
  status: state_projection.WorkstreamStatus,
) -> List(ArtifactInspection) {
  let phase_run_index = phase_run_index(status.queued_phase_runs)
  status.artifacts
  |> dict.to_list
  |> list.sort(by: compare_artifact_entries)
  |> list.map(fn(entry) {
    let #(_, artifact) = entry
    inspect_artifact(store, artifact, phase_run_index)
  })
}

fn inspect_artifact(
  store: state_artifact_store.Store,
  artifact: state_projection.WorkstreamArtifactSnapshot,
  phase_run_index: Dict(String, String),
) -> ArtifactInspection {
  let read =
    snapshot.read_text(
      store,
      artifact.snapshot_ref,
      artifact.snapshot_sha256,
      artifact.snapshot_bytes,
    )
  let snapshot_status = snapshot.status(read)
  let detail = case read {
    snapshot.ReadOk(contents: contents, ..) ->
      decode_artifact_detail(artifact.artifact_type, contents, phase_run_index)
    snapshot.ReadProblem(..) -> ArtifactUndecoded
  }
  ArtifactInspection(
    artifact_id: artifact.artifact_id,
    artifact_type: artifact.artifact_type,
    snapshot_ref: artifact.snapshot_ref,
    snapshot_sha256: artifact.snapshot_sha256,
    snapshot_bytes: artifact.snapshot_bytes,
    original_path: artifact.original_path,
    contract_type: artifact.contract_type,
    media_type: artifact.media_type,
    producer_workflow_id: artifact.producer_workflow_id,
    producer_run_id: artifact.producer_run_id,
    producer_step_id: artifact.producer_step_id,
    recorded_at_ms: artifact.recorded_at_ms,
    snapshot_status: snapshot_status,
    detail: detail,
  )
}

fn inspect_handoffs(
  store: state_artifact_store.Store,
  status: state_projection.WorkstreamStatus,
) -> List(HandoffInspection) {
  status.handoffs
  |> dict.to_list
  |> list.sort(by: compare_handoff_entries)
  |> list.map(fn(entry) {
    let #(_, handoff) = entry
    inspect_handoff(store, handoff)
  })
}

fn inspect_handoff(
  store: state_artifact_store.Store,
  handoff: state_projection.WorkstreamHandoffSnapshot,
) -> HandoffInspection {
  let read =
    snapshot.read_text(
      store,
      handoff.handoff_ref,
      handoff.handoff_sha256,
      handoff.handoff_bytes,
    )
  case read {
    snapshot.ReadProblem(warning) ->
      HandoffInspection(
        handoff_id: handoff.handoff_id,
        handoff_ref: handoff.handoff_ref,
        handoff_sha256: handoff.handoff_sha256,
        handoff_bytes: handoff.handoff_bytes,
        source_workflow_id: handoff.source_workflow_id,
        source_run_id: handoff.source_run_id,
        recorded_at_ms: handoff.recorded_at_ms,
        snapshot_status: snapshot.SnapshotProblem(warning.code, warning.message),
        phase_id: None,
        summary: None,
        outputs: [],
        recommended_next_actions: [],
        open_questions: [],
        decode_error: None,
      )
    snapshot.ReadOk(contents, display_path, local_path) ->
      case artifacts.decode_handoff(contents) {
        Ok(decoded) ->
          HandoffInspection(
            handoff_id: handoff.handoff_id,
            handoff_ref: handoff.handoff_ref,
            handoff_sha256: handoff.handoff_sha256,
            handoff_bytes: handoff.handoff_bytes,
            source_workflow_id: handoff.source_workflow_id,
            source_run_id: handoff.source_run_id,
            recorded_at_ms: handoff.recorded_at_ms,
            snapshot_status: snapshot.SnapshotOk(display_path, local_path),
            phase_id: Some(decoded.phase_id),
            summary: Some(truncate(decoded.summary)),
            outputs: list.map(decoded.outputs, fn(output) { output.name }),
            recommended_next_actions: decoded.recommended_next_actions,
            open_questions: list.map(decoded.open_questions, truncate),
            decode_error: None,
          )
        Error(error) -> {
          let warning =
            spec_warning("handoff_decode_failed", handoff.handoff_ref, error)
          HandoffInspection(
            handoff_id: handoff.handoff_id,
            handoff_ref: handoff.handoff_ref,
            handoff_sha256: handoff.handoff_sha256,
            handoff_bytes: handoff.handoff_bytes,
            source_workflow_id: handoff.source_workflow_id,
            source_run_id: handoff.source_run_id,
            recorded_at_ms: handoff.recorded_at_ms,
            snapshot_status: snapshot.SnapshotOk(display_path, local_path),
            phase_id: None,
            summary: None,
            outputs: [],
            recommended_next_actions: [],
            open_questions: [],
            decode_error: Some(warning),
          )
        }
      }
  }
}

fn inspect_phase_runs(
  status: state_projection.WorkstreamStatus,
) -> List(PhaseRunInspection) {
  status.queued_phase_runs
  |> dict.to_list
  |> list.sort(by: compare_phase_run_entries)
  |> list.map(fn(entry) {
    let #(_, run) = entry
    PhaseRunInspection(
      phase_run_id: run.phase_run_id,
      action_id: run.action_id,
      workflow_id: run.workflow_id,
      input_bundle_ref: run.input_bundle_ref,
      input_bundle_sha256: run.input_bundle_sha256,
      input_bundle_bytes: run.input_bundle_bytes,
      queued_at_ms: run.queued_at_ms,
    )
  })
}

fn decode_artifact_detail(
  artifact_type: String,
  contents: String,
  phase_run_index: Dict(String, String),
) -> ArtifactDetail {
  case artifact_type {
    type_ if type_ == types.workstream_artifact_type ->
      case artifacts.decode_workstream(contents) {
        Ok(decoded) ->
          WorkstreamDetail(
            status: decoded.status,
            summary: truncate(decoded.summary),
            next_actions: decoded.next_actions,
          )
        Error(error) -> spec_detail("artifact_decode_failed", error)
      }
    type_ if type_ == types.assignment_artifact_type ->
      case artifacts.decode_assignment(contents) {
        Ok(decoded) ->
          AssignmentDetail(
            workflow_id: decoded.workflow_id,
            playbook_id: decoded.playbook_id,
            reason: truncate(decoded.reason),
          )
        Error(error) -> spec_detail("artifact_decode_failed", error)
      }
    type_ if type_ == types.next_action_artifact_type ->
      case artifacts.decode_next_action(contents) {
        Ok(decoded) ->
          NextActionDetail(
            action_id: decoded.action_id,
            workflow_id: decoded.workflow_id,
            state: decoded.state,
            priority: decoded.priority,
            inputs: decoded.inputs,
            requires_gate: decoded.requires_gate,
            auto_enqueue: decoded.auto_enqueue,
            resolved_by_phase_run_id: phase_run_index
              |> dict.get(decoded.action_id)
              |> option.from_result,
          )
        Error(error) -> spec_detail("artifact_decode_failed", error)
      }
    type_ if type_ == types.decision_artifact_type ->
      case artifacts.decode_decision(contents) {
        Ok(decoded) ->
          DecisionDetail(
            kind: decoded.kind,
            summary: truncate(decoded.summary),
            decided_by: truncate(decoded.decided_by),
          )
        Error(error) -> spec_detail("artifact_decode_failed", error)
      }
    type_ if type_ == types.input_bundle_artifact_type ->
      case artifacts.decode_input_bundle(contents) {
        Ok(decoded) ->
          InputBundleDetail(
            workflow_id: decoded.workflow_id,
            source_handoff_ref: decoded.source_handoff_ref,
            inputs: list.map(decoded.inputs, fn(input) { input.name }),
            source_kind: decoded.source_kind,
            source_reason: option.map(decoded.source_reason, truncate),
          )
        Error(error) -> spec_detail("artifact_decode_failed", error)
      }
    _ -> ArtifactUndecoded
  }
}

fn spec_detail(code: String, error: types.SpecError) -> ArtifactDetail {
  ArtifactDecodeFailed(
    code: code,
    message: types.error_code(error) <> ": " <> types.error_message(error),
  )
}

fn spec_warning(
  code: String,
  ref: String,
  error: types.SpecError,
) -> snapshot.ProjectionWarning {
  snapshot.ProjectionWarning(
    code: code,
    ref: ref,
    message: types.error_code(error) <> ": " <> types.error_message(error),
  )
}

fn next_actions_from_artifacts(
  artifacts: List(ArtifactInspection),
  _phase_runs: List(PhaseRunInspection),
) -> List(NextActionInspection) {
  artifacts
  |> list.filter_map(fn(artifact) {
    case artifact.detail {
      NextActionDetail(
        action_id,
        workflow_id,
        state,
        priority,
        inputs,
        requires_gate,
        auto_enqueue,
        resolved_by_phase_run_id,
      ) ->
        Ok(NextActionInspection(
          artifact_id: artifact.artifact_id,
          action_id: action_id,
          workflow_id: workflow_id,
          state: resolved_next_action_state(state, resolved_by_phase_run_id),
          priority: priority,
          inputs: inputs,
          requires_gate: requires_gate,
          auto_enqueue: auto_enqueue,
          snapshot_ref: artifact.snapshot_ref,
          snapshot_sha256: artifact.snapshot_sha256,
          recorded_at_ms: artifact.recorded_at_ms,
          resolved_by_phase_run_id: resolved_by_phase_run_id,
        ))
      _ -> Error(Nil)
    }
  })
  |> list.sort(by: compare_next_actions)
}

fn resolved_next_action_state(
  state: String,
  resolved_by_phase_run_id: Option(String),
) -> String {
  case resolved_by_phase_run_id {
    Some(_) -> "queued"
    None -> state
  }
}

fn unresolved_next_actions_from(
  next_actions: List(NextActionInspection),
) -> List(NextActionInspection) {
  next_actions
  |> list.filter(fn(action) {
    action.resolved_by_phase_run_id == None && action.state != "completed"
  })
}

fn decisions_from_artifacts(
  artifacts: List(ArtifactInspection),
) -> List(DecisionInspection) {
  artifacts
  |> list.filter_map(fn(artifact) {
    case artifact.detail {
      DecisionDetail(kind, summary, decided_by) ->
        Ok(DecisionInspection(
          artifact_id: artifact.artifact_id,
          kind: kind,
          summary: summary,
          decided_by: decided_by,
          snapshot_ref: artifact.snapshot_ref,
          snapshot_sha256: artifact.snapshot_sha256,
          recorded_at_ms: artifact.recorded_at_ms,
        ))
      _ -> Error(Nil)
    }
  })
}

fn phases_from_handoffs(
  handoffs: List(HandoffInspection),
) -> List(PhaseInspection) {
  let phases =
    handoffs
    |> list.filter_map(fn(handoff) {
      case handoff.phase_id {
        Some(phase_id) -> Ok(#(phase_id, handoff))
        None -> Error(Nil)
      }
    })
    |> list.fold(dict.new(), fn(acc, entry) {
      let #(phase_id, handoff) = entry
      let existing_count = case dict.get(acc, phase_id) {
        Ok(#(_, _, count)) -> count
        Error(Nil) -> 0
      }
      dict.insert(acc, phase_id, #(
        handoff.handoff_ref,
        handoff.summary,
        existing_count + 1,
      ))
    })
  phases
  |> dict.to_list
  |> list.sort(by: fn(a, b) {
    let #(a_phase, _) = a
    let #(b_phase, _) = b
    string.compare(a_phase, b_phase)
  })
  |> list.map(fn(entry) {
    let #(phase_id, #(handoff_ref, summary, count)) = entry
    PhaseInspection(
      phase_id: phase_id,
      latest_handoff_ref: Some(handoff_ref),
      latest_handoff_summary: summary,
      handoff_count: count,
    )
  })
}

fn current_status(
  artifacts: List(ArtifactInspection),
  status: state_projection.WorkstreamStatus,
) -> String {
  case latest_workstream_detail(artifacts, None) {
    Some(workstream_status) -> workstream_status
    None -> fallback_status(status)
  }
}

fn latest_workstream_detail(
  artifacts: List(ArtifactInspection),
  best: Option(ArtifactInspection),
) -> Option(String) {
  case artifacts {
    [] ->
      case best {
        Some(artifact) ->
          case artifact.detail {
            WorkstreamDetail(status, _, _) -> Some(status)
            _ -> None
          }
        None -> None
      }
    [artifact, ..rest] -> {
      let best = case artifact.detail, best {
        WorkstreamDetail(..), None -> Some(artifact)
        WorkstreamDetail(..), Some(existing) ->
          case artifact.recorded_at_ms >= existing.recorded_at_ms {
            True -> Some(artifact)
            False -> best
          }
        _, _ -> best
      }
      latest_workstream_detail(rest, best)
    }
  }
}

fn phase_run_index(
  phase_runs: Dict(String, state_projection.WorkstreamPhaseRun),
) -> Dict(String, String) {
  phase_runs
  |> dict.values
  |> list.sort(by: compare_phase_runs)
  |> list.fold(dict.new(), fn(index, run) {
    case dict.get(index, run.action_id) {
      Ok(_) -> index
      Error(Nil) -> dict.insert(index, run.action_id, run.phase_run_id)
    }
  })
}

fn workstreams_for_ref(
  projected: state_projection.Projection,
  task_or_workstream_ref: String,
) -> List(state_projection.WorkstreamStatus) {
  let ref = string.trim(task_or_workstream_ref)
  projected.workstreams
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(workstream_id, status) = entry
    case workstream_matches_ref(workstream_id, status, ref) {
      True -> Ok(status)
      False -> Error(Nil)
    }
  })
  |> list.sort(by: compare_workstream_statuses)
}

fn workstream_matches_ref(
  workstream_id: String,
  status: state_projection.WorkstreamStatus,
  ref: String,
) -> Bool {
  workstream_id == ref
  || workstream_id == "linear:" <> ref
  || task_ref_matches(status.task_ref, ref)
}

fn task_ref_matches(
  task_ref: Option(record.TaskRefFields),
  ref: String,
) -> Bool {
  case task_ref {
    None -> False
    Some(task_ref) ->
      task_ref.task_remote_id == id_ref_value(ref)
      || option_string_equals(task_ref.task_key, ref)
      || option_string_equals(task_ref.task_url, ref)
  }
}

fn id_ref_value(ref: String) -> String {
  case string.starts_with(ref, "id:") {
    True -> string.drop_start(ref, 3)
    False -> ref
  }
}

fn task_ref_warnings(
  status: state_projection.WorkstreamStatus,
) -> List(snapshot.ProjectionWarning) {
  case status.task_ref {
    Some(_) -> []
    None -> [
      snapshot.ProjectionWarning(
        code: "workstream_task_ref_missing",
        ref: status.workstream_id,
        message: "workstream has no recorded task reference",
      ),
    ]
  }
}

fn artifact_warnings(
  artifacts: List(ArtifactInspection),
) -> List(snapshot.ProjectionWarning) {
  artifacts
  |> list.fold([], fn(acc, artifact) {
    []
    |> append_warnings(snapshot_warning(
      artifact.snapshot_ref,
      artifact.snapshot_status,
    ))
    |> append_warnings(artifact_detail_warnings(artifact))
    |> append_warnings(acc)
  })
  |> list.reverse
}

fn handoff_warnings(
  handoffs: List(HandoffInspection),
) -> List(snapshot.ProjectionWarning) {
  handoffs
  |> list.fold([], fn(acc, handoff) {
    []
    |> append_warnings(snapshot_warning(
      handoff.handoff_ref,
      handoff.snapshot_status,
    ))
    |> append_warning_option(handoff.decode_error)
    |> append_warnings(acc)
  })
  |> list.reverse
}

fn artifact_detail_warnings(
  artifact: ArtifactInspection,
) -> List(snapshot.ProjectionWarning) {
  case artifact.detail {
    ArtifactDecodeFailed(code, message) -> [
      snapshot.ProjectionWarning(
        code: code,
        ref: artifact.snapshot_ref,
        message: message,
      ),
    ]
    _ -> []
  }
}

fn snapshot_warning(
  ref: String,
  status: snapshot.SnapshotStatus,
) -> List(snapshot.ProjectionWarning) {
  case status {
    snapshot.SnapshotOk(..) -> []
    snapshot.SnapshotProblem(code, message) -> [
      snapshot.ProjectionWarning(code, ref, message),
    ]
  }
}

fn option_assignment(
  assignment: Option(state_projection.WorkstreamAssignment),
) -> Option(AssignmentInspection) {
  case assignment {
    None -> None
    Some(assignment) ->
      Some(AssignmentInspection(
        assignment_id: assignment.assignment_id,
        workflow_id: assignment.workflow_id,
        playbook_id: assignment.playbook_id,
        reason: truncate(assignment.reason),
        assigned_at_ms: assignment.assigned_at_ms,
      ))
  }
}

fn option_string_equals(value: Option(String), expected: String) -> Bool {
  case value {
    Some(value) -> value == expected
    None -> False
  }
}

fn append_warning_option(
  warnings: List(snapshot.ProjectionWarning),
  warning: Option(snapshot.ProjectionWarning),
) -> List(snapshot.ProjectionWarning) {
  case warning {
    Some(warning) -> [warning, ..warnings]
    None -> warnings
  }
}

fn append_warnings(
  warnings: List(snapshot.ProjectionWarning),
  more: List(snapshot.ProjectionWarning),
) -> List(snapshot.ProjectionWarning) {
  list.append(more, warnings)
}

fn truncate(value: String) -> String {
  case string.length(value) > max_summary_chars {
    True -> string.slice(value, at_index: 0, length: max_summary_chars) <> "…"
    False -> value
  }
}

fn compare_workstream_entries(
  a: #(String, state_projection.WorkstreamStatus),
  b: #(String, state_projection.WorkstreamStatus),
) -> Order {
  let #(a_id, _) = a
  let #(b_id, _) = b
  string.compare(a_id, b_id)
}

fn compare_workstream_statuses(
  a: state_projection.WorkstreamStatus,
  b: state_projection.WorkstreamStatus,
) -> Order {
  string.compare(a.workstream_id, b.workstream_id)
}

fn compare_artifact_entries(
  a: #(String, state_projection.WorkstreamArtifactSnapshot),
  b: #(String, state_projection.WorkstreamArtifactSnapshot),
) -> Order {
  let #(a_ref, a_artifact) = a
  let #(b_ref, b_artifact) = b
  compare_time_then_string(
    a_artifact.recorded_at_ms,
    b_artifact.recorded_at_ms,
    a_ref,
    b_ref,
  )
}

fn compare_handoff_entries(
  a: #(String, state_projection.WorkstreamHandoffSnapshot),
  b: #(String, state_projection.WorkstreamHandoffSnapshot),
) -> Order {
  let #(a_ref, a_handoff) = a
  let #(b_ref, b_handoff) = b
  compare_time_then_string(
    a_handoff.recorded_at_ms,
    b_handoff.recorded_at_ms,
    a_ref,
    b_ref,
  )
}

fn compare_phase_run_entries(
  a: #(String, state_projection.WorkstreamPhaseRun),
  b: #(String, state_projection.WorkstreamPhaseRun),
) -> Order {
  let #(a_id, a_run) = a
  let #(b_id, b_run) = b
  compare_time_then_string(a_run.queued_at_ms, b_run.queued_at_ms, a_id, b_id)
}

fn compare_phase_runs(
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

fn compare_next_actions(
  a: NextActionInspection,
  b: NextActionInspection,
) -> Order {
  case int.compare(a.priority, b.priority) {
    Eq ->
      compare_time_then_string(
        a.recorded_at_ms,
        b.recorded_at_ms,
        a.action_id,
        b.action_id,
      )
    other -> other
  }
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
