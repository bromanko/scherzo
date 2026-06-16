import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/work_item/action_fingerprint

pub const run_workflow_action_id = "work_item.run_workflow"

pub const cancel_action_id = "work_subtask.cancel"

pub const review_artifacts_action_id = "work_subtask.review_artifacts"

pub const fix_retry_action_id = "work_subtask.fix_retry"

pub type ActionKind {
  ReadOnly
  Mutating
}

pub type ActionDisabledReason {
  ActionDisabledReason(code: String, message: String)
}

pub type ActionTargetSummary {
  ActionTargetSummary(
    kind: String,
    provider: String,
    id: String,
    display_id: Option(String),
    workflow_id: Option(String),
    run_id: Option(String),
  )
}

pub type ActionArtifactSummary {
  ActionArtifactSummary(
    kind: String,
    ref: String,
    sha256: String,
    bytes: Int,
    display_path: String,
    run_id: Option(String),
    step_id: Option(String),
    publication_id: Option(String),
  )
}

pub type WorkItemAction {
  WorkItemAction(
    action_id: String,
    instance_id: String,
    label: String,
    kind: ActionKind,
    enabled: Bool,
    disabled_reason: Option(ActionDisabledReason),
    fingerprint: String,
    target: ActionTargetSummary,
    artifacts: List(ActionArtifactSummary),
  )
}

pub fn read_only(
  action_id: String,
  label: String,
  enabled: Bool,
  disabled_reason: Option(ActionDisabledReason),
  target: ActionTargetSummary,
  artifacts: List(ActionArtifactSummary),
) -> WorkItemAction {
  build(action_id, label, ReadOnly, enabled, disabled_reason, target, artifacts)
}

pub fn mutating(
  action_id: String,
  label: String,
  enabled: Bool,
  disabled_reason: Option(ActionDisabledReason),
  target: ActionTargetSummary,
) -> WorkItemAction {
  build(action_id, label, Mutating, enabled, disabled_reason, target, [])
}

pub fn build(
  action_id: String,
  label: String,
  kind: ActionKind,
  enabled: Bool,
  disabled_reason: Option(ActionDisabledReason),
  target: ActionTargetSummary,
  artifacts: List(ActionArtifactSummary),
) -> WorkItemAction {
  let instance_id =
    action_fingerprint.instance_id(action_id, target_key(target))
  let fingerprint =
    action_fingerprint.fingerprint([
      "action_id=" <> action_id,
      "label=" <> label,
      "kind=" <> kind_to_string(kind),
      "enabled=" <> bool.to_string(enabled),
      "disabled_reason=" <> disabled_reason_key(disabled_reason),
      "target=" <> target_key(target),
      "artifacts=" <> artifacts_key(artifacts),
    ])

  WorkItemAction(
    action_id: action_id,
    instance_id: instance_id,
    label: label,
    kind: kind,
    enabled: enabled,
    disabled_reason: disabled_reason,
    fingerprint: fingerprint,
    target: target,
    artifacts: artifacts,
  )
}

pub fn kind_to_string(kind: ActionKind) -> String {
  case kind {
    ReadOnly -> "read_only"
    Mutating -> "mutating"
  }
}

pub fn kind_from_string(value: String) -> Result(ActionKind, Nil) {
  case value {
    "read_only" -> Ok(ReadOnly)
    "mutating" -> Ok(Mutating)
    _ -> Error(Nil)
  }
}

pub fn stable_action_ids() -> List(String) {
  [
    run_workflow_action_id,
    cancel_action_id,
    review_artifacts_action_id,
    fix_retry_action_id,
  ]
}

pub fn has_action_id(action_id: String) -> Bool {
  list.contains(stable_action_ids(), action_id)
}

fn disabled_reason_key(reason: Option(ActionDisabledReason)) -> String {
  case reason {
    Some(reason) -> reason.code <> ":" <> reason.message
    None -> ""
  }
}

fn target_key(target: ActionTargetSummary) -> String {
  [
    "kind=" <> target.kind,
    "provider=" <> target.provider,
    "id=" <> target.id,
    "display_id=" <> option_string(target.display_id),
    "workflow_id=" <> option_string(target.workflow_id),
    "run_id=" <> option_string(target.run_id),
  ]
  |> string.join(with: ",")
}

fn artifacts_key(artifacts: List(ActionArtifactSummary)) -> String {
  artifacts
  |> list.map(fn(artifact) {
    [
      artifact.kind,
      artifact.ref,
      artifact.sha256,
      int.to_string(artifact.bytes),
      artifact.display_path,
      option_string(artifact.run_id),
      option_string(artifact.step_id),
      option_string(artifact.publication_id),
    ]
    |> string.join(with: ":")
  })
  |> string.join(with: ";")
}

fn option_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
