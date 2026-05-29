import gleam/json
import gleam/option.{type Option, None, Some}
import scherzo/state/record
import scherzo/workstream/projection
import scherzo/workstream/projection_snapshot as snapshot

pub fn inspection_to_json(
  inspection: projection.WorkstreamInspection,
) -> json.Json {
  json.object([
    #("workstream_id", json.string(inspection.workstream_id)),
    #("task_ref", option_task_ref_to_json(inspection.task_ref)),
    #("status", json.string(inspection.status)),
    #("created_at_ms", option_int_to_json(inspection.created_at_ms)),
    #(
      "latest_assignment",
      option_assignment_to_json(inspection.latest_assignment),
    ),
    #("phases", json.array(inspection.phases, of: phase_to_json)),
    #("artifacts", json.array(inspection.artifacts, of: artifact_to_json)),
    #("handoffs", json.array(inspection.handoffs, of: handoff_to_json)),
    #(
      "queued_phase_runs",
      json.array(inspection.queued_phase_runs, of: phase_run_to_json),
    ),
    #(
      "next_actions",
      json.array(inspection.next_actions, of: next_action_to_json),
    ),
    #(
      "unresolved_next_actions",
      json.array(inspection.unresolved_next_actions, of: next_action_to_json),
    ),
    #("decisions", json.array(inspection.decisions, of: decision_to_json)),
    #("warnings", json.array(inspection.warnings, of: snapshot.warning_to_json)),
  ])
}

pub fn summary_to_json(summary: projection.WorkstreamSummary) -> json.Json {
  json.object([
    #("workstream_id", json.string(summary.workstream_id)),
    #("task_ref", option_task_ref_to_json(summary.task_ref)),
    #("status", json.string(summary.status)),
    #("created_at_ms", option_int_to_json(summary.created_at_ms)),
    #("latest_assignment", option_assignment_to_json(summary.latest_assignment)),
    #("artifact_count", json.int(summary.artifact_count)),
    #("handoff_count", json.int(summary.handoff_count)),
    #("queued_phase_run_count", json.int(summary.queued_phase_run_count)),
  ])
}

fn artifact_to_json(artifact: projection.ArtifactInspection) -> json.Json {
  json.object([
    #("artifact_id", json.string(artifact.artifact_id)),
    #("artifact_type", json.string(artifact.artifact_type)),
    #("snapshot_ref", json.string(artifact.snapshot_ref)),
    #("snapshot_sha256", json.string(artifact.snapshot_sha256)),
    #("snapshot_bytes", json.int(artifact.snapshot_bytes)),
    #("original_path", json.string(artifact.original_path)),
    #("contract_type", json.string(artifact.contract_type)),
    #("media_type", json.string(artifact.media_type)),
    #(
      "producer",
      producer_to_json(
        artifact.producer_workflow_id,
        artifact.producer_run_id,
        artifact.producer_step_id,
      ),
    ),
    #("recorded_at_ms", json.int(artifact.recorded_at_ms)),
    #("snapshot_status", snapshot.status_to_json(artifact.snapshot_status)),
    #("detail", detail_to_json(artifact.detail)),
  ])
}

fn handoff_to_json(handoff: projection.HandoffInspection) -> json.Json {
  json.object([
    #("handoff_id", json.string(handoff.handoff_id)),
    #("handoff_ref", json.string(handoff.handoff_ref)),
    #("handoff_sha256", json.string(handoff.handoff_sha256)),
    #("handoff_bytes", json.int(handoff.handoff_bytes)),
    #(
      "source",
      json.object([
        #("workflow_id", json.string(handoff.source_workflow_id)),
        #("run_id", json.string(handoff.source_run_id)),
      ]),
    ),
    #("recorded_at_ms", json.int(handoff.recorded_at_ms)),
    #("snapshot_status", snapshot.status_to_json(handoff.snapshot_status)),
    #("phase_id", option_string_to_json(handoff.phase_id)),
    #("summary", option_string_to_json(handoff.summary)),
    #("outputs", json.array(handoff.outputs, of: json.string)),
    #(
      "recommended_next_actions",
      json.array(handoff.recommended_next_actions, of: json.string),
    ),
    #("open_questions", json.array(handoff.open_questions, of: json.string)),
    #("decode_error", option_warning_to_json(handoff.decode_error)),
  ])
}

fn phase_run_to_json(phase_run: projection.PhaseRunInspection) -> json.Json {
  json.object([
    #("phase_run_id", json.string(phase_run.phase_run_id)),
    #("action_id", json.string(phase_run.action_id)),
    #("workflow_id", json.string(phase_run.workflow_id)),
    #("input_bundle_ref", json.string(phase_run.input_bundle_ref)),
    #("input_bundle_sha256", json.string(phase_run.input_bundle_sha256)),
    #("input_bundle_bytes", json.int(phase_run.input_bundle_bytes)),
    #("queued_at_ms", json.int(phase_run.queued_at_ms)),
  ])
}

fn phase_to_json(phase: projection.PhaseInspection) -> json.Json {
  json.object([
    #("phase_id", json.string(phase.phase_id)),
    #("latest_handoff_ref", option_string_to_json(phase.latest_handoff_ref)),
    #(
      "latest_handoff_summary",
      option_string_to_json(phase.latest_handoff_summary),
    ),
    #("handoff_count", json.int(phase.handoff_count)),
  ])
}

fn next_action_to_json(action: projection.NextActionInspection) -> json.Json {
  json.object([
    #("artifact_id", json.string(action.artifact_id)),
    #("action_id", json.string(action.action_id)),
    #("workflow_id", json.string(action.workflow_id)),
    #("state", json.string(action.state)),
    #("priority", json.int(action.priority)),
    #("inputs", json.array(action.inputs, of: json.string)),
    #("requires_gate", option_string_to_json(action.requires_gate)),
    #("auto_enqueue", json.bool(action.auto_enqueue)),
    #("snapshot_ref", json.string(action.snapshot_ref)),
    #("snapshot_sha256", json.string(action.snapshot_sha256)),
    #("recorded_at_ms", json.int(action.recorded_at_ms)),
    #(
      "resolved_by_phase_run_id",
      option_string_to_json(action.resolved_by_phase_run_id),
    ),
  ])
}

fn decision_to_json(decision: projection.DecisionInspection) -> json.Json {
  json.object([
    #("artifact_id", json.string(decision.artifact_id)),
    #("action_id", json.string(decision.action_id)),
    #("gate_id", json.string(decision.gate_id)),
    #("kind", json.string(decision.kind)),
    #("decided_at_ms", json.int(decision.decided_at_ms)),
    #("decided_by", json.string(decision.decided_by)),
    #("rationale", json.string(decision.rationale)),
    #("inputs", json.array(decision.inputs, of: decision_input_to_json)),
    #("summary", json.string(decision.summary)),
    #("snapshot_ref", json.string(decision.snapshot_ref)),
    #("snapshot_sha256", json.string(decision.snapshot_sha256)),
    #("recorded_at_ms", json.int(decision.recorded_at_ms)),
  ])
}

fn decision_input_to_json(
  input: projection.DecisionInputInspection,
) -> json.Json {
  json.object([
    #("name", json.string(input.name)),
    #("ref", json.string(input.ref)),
    #("sha256", json.string(input.sha256)),
  ])
}

fn detail_to_json(detail: projection.ArtifactDetail) -> json.Json {
  case detail {
    projection.ArtifactUndecoded ->
      json.object([#("kind", json.string("undecoded"))])
    projection.ArtifactDecodeFailed(code, message) ->
      json.object([
        #("kind", json.string("decode_failed")),
        #("code", json.string(code)),
        #("message", json.string(message)),
      ])
    projection.WorkstreamDetail(status, summary, next_actions) ->
      json.object([
        #("kind", json.string("workstream")),
        #("status", json.string(status)),
        #("summary", json.string(summary)),
        #("next_actions", json.array(next_actions, of: json.string)),
      ])
    projection.AssignmentDetail(workflow_id, playbook_id, reason) ->
      json.object([
        #("kind", json.string("assignment")),
        #("workflow_id", json.string(workflow_id)),
        #("playbook_id", option_string_to_json(playbook_id)),
        #("reason", json.string(reason)),
      ])
    projection.NextActionDetail(
      action_id,
      workflow_id,
      state,
      priority,
      inputs,
      requires_gate,
      auto_enqueue,
      resolved_by_phase_run_id,
    ) ->
      json.object([
        #("kind", json.string("next_action")),
        #("action_id", json.string(action_id)),
        #("workflow_id", json.string(workflow_id)),
        #("state", json.string(state)),
        #("priority", json.int(priority)),
        #("inputs", json.array(inputs, of: json.string)),
        #("requires_gate", option_string_to_json(requires_gate)),
        #("auto_enqueue", json.bool(auto_enqueue)),
        #(
          "resolved_by_phase_run_id",
          option_string_to_json(resolved_by_phase_run_id),
        ),
      ])
    projection.DecisionDetail(
      action_id,
      gate_id,
      kind,
      decided_at_ms,
      decided_by,
      rationale,
      inputs,
      summary,
    ) ->
      json.object([
        #("kind", json.string("decision")),
        #("action_id", json.string(action_id)),
        #("gate_id", json.string(gate_id)),
        #("decision_kind", json.string(kind)),
        #("decided_at_ms", json.int(decided_at_ms)),
        #("decided_by", json.string(decided_by)),
        #("rationale", json.string(rationale)),
        #("inputs", json.array(inputs, of: decision_input_to_json)),
        #("summary", json.string(summary)),
      ])
    projection.InputBundleDetail(
      workflow_id,
      source_handoff_ref,
      inputs,
      source_kind,
      source_reason,
    ) ->
      json.object([
        #("kind", json.string("input_bundle")),
        #("workflow_id", json.string(workflow_id)),
        #("source_handoff_ref", json.string(source_handoff_ref)),
        #("inputs", json.array(inputs, of: json.string)),
        #("source_kind", option_string_to_json(source_kind)),
        #("source_reason", option_string_to_json(source_reason)),
      ])
  }
}

fn option_task_ref_to_json(
  task_ref: Option(record.TaskRefFields),
) -> json.Json {
  case task_ref {
    None -> json.null()
    Some(task_ref) ->
      json.object([
        #("backend_kind", json.string(task_ref.task_backend_kind)),
        #("remote_id", json.string(task_ref.task_remote_id)),
        #("key", option_string_to_json(task_ref.task_key)),
        #("url", option_string_to_json(task_ref.task_url)),
      ])
  }
}

fn option_assignment_to_json(
  assignment: Option(projection.AssignmentInspection),
) -> json.Json {
  case assignment {
    None -> json.null()
    Some(assignment) ->
      json.object([
        #("assignment_id", json.string(assignment.assignment_id)),
        #("workflow_id", json.string(assignment.workflow_id)),
        #("playbook_id", option_string_to_json(assignment.playbook_id)),
        #("reason", json.string(assignment.reason)),
        #("assigned_at_ms", json.int(assignment.assigned_at_ms)),
      ])
  }
}

fn producer_to_json(
  workflow_id: String,
  run_id: String,
  step_id: String,
) -> json.Json {
  json.object([
    #("workflow_id", json.string(workflow_id)),
    #("run_id", json.string(run_id)),
    #("step_id", json.string(step_id)),
  ])
}

fn option_warning_to_json(
  warning: Option(snapshot.ProjectionWarning),
) -> json.Json {
  case warning {
    None -> json.null()
    Some(warning) -> snapshot.warning_to_json(warning)
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}
