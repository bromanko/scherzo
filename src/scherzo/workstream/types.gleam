import gleam/option.{type Option}

pub const schema_version = 1

pub const workstream_artifact_type = "scherzo.workstream.v1"

pub const handoff_artifact_type = "scherzo.handoff.v1"

pub const decision_artifact_type = "scherzo.decision.v1"

pub const input_bundle_artifact_type = "scherzo.input_bundle.v1"

pub const assignment_artifact_type = "scherzo.assignment.v1"

pub const next_action_artifact_type = "scherzo.next_action.v1"

pub type SpecError {
  SpecError(code: String, message: String)
}

pub type TaskRef {
  TaskRef(
    backend_kind: String,
    remote_id: String,
    key: Option(String),
    url: Option(String),
  )
}

pub type ProducerRef {
  ProducerRef(workflow_id: String, run_id: String, step_id: String)
}

pub type ValidationSummary {
  ValidationSummary(status: String, validator: String, checked_at: String)
}

pub type ArtifactSnapshot {
  ArtifactSnapshot(
    ref: String,
    sha256: String,
    bytes: Int,
    media_type: String,
    original_path: String,
    contract_type: String,
    artifact_type: Option(String),
    producer: ProducerRef,
    validation: ValidationSummary,
    summary: String,
  )
}

pub type HandoffOutput {
  HandoffOutput(name: String, snapshot: ArtifactSnapshot)
}

pub type InputBinding {
  InputBinding(
    name: String,
    contract_type: String,
    value_ref: String,
    sha256: Option(String),
    bytes: Option(Int),
    media_type: Option(String),
    original_path: Option(String),
    artifact_type: Option(String),
    source_kind: Option(String),
  )
}

pub type WorkstreamArtifact {
  WorkstreamArtifact(
    artifact_id: String,
    workstream_id: String,
    task_ref: TaskRef,
    status: String,
    summary: String,
    produced_artifacts: List(ArtifactSnapshot),
    next_actions: List(String),
  )
}

pub type HandoffArtifact {
  HandoffArtifact(
    artifact_id: String,
    workstream_id: String,
    phase_id: String,
    summary: String,
    outputs: List(HandoffOutput),
    recommended_next_actions: List(String),
    open_questions: List(String),
  )
}

pub type DecisionInputRef {
  DecisionInputRef(name: String, ref: String, sha256: String)
}

pub type DecisionArtifact {
  DecisionArtifact(
    artifact_id: String,
    workstream_id: String,
    action_id: String,
    gate_id: String,
    kind: String,
    decided_at_ms: Int,
    decided_by: String,
    rationale: String,
    inputs: List(DecisionInputRef),
    summary: String,
  )
}

pub type InputBundleArtifact {
  InputBundleArtifact(
    artifact_id: String,
    workstream_id: String,
    source_handoff_ref: String,
    workflow_id: String,
    inputs: List(InputBinding),
    source_kind: Option(String),
    source_reason: Option(String),
  )
}

pub type AssignmentArtifact {
  AssignmentArtifact(
    artifact_id: String,
    workstream_id: String,
    workflow_id: String,
    playbook_id: Option(String),
    reason: String,
  )
}

pub type NextActionArtifact {
  NextActionArtifact(
    artifact_id: String,
    workstream_id: String,
    action_id: String,
    workflow_id: String,
    state: String,
    priority: Int,
    inputs: List(String),
    requires_gate: Option(String),
    auto_enqueue: Bool,
  )
}

pub fn error_code(error: SpecError) -> String {
  let SpecError(code, _) = error
  code
}

pub fn error_message(error: SpecError) -> String {
  let SpecError(_, message) = error
  message
}
