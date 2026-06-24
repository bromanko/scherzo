import gleam/dict
import gleam/option.{type Option, Some}
import scherzo/claim_abandonment
import scherzo/orchestrator/outbox_effects
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/task
import scherzo/tracker/adapter

pub type Compensation {
  Compensation(
    runtime: orchestrator_state.RuntimeState,
    parked: orchestrator_state.ParkedEntry,
    reason_text: String,
    release_intent: outbox_effects.Intent,
  )
}

pub fn claim_success_abandoned(
  pending_claims: dict.Dict(
    identity.TaskIdentity,
    transition_types.PendingClaim,
  ),
  backend_kind: String,
  issue_id: String,
) -> Bool {
  case
    dict.get(
      pending_claims,
      orchestrator_state.issue_id_identity_for_backend(issue_id, backend_kind),
    )
  {
    Error(Nil) -> True
    Ok(_) -> False
  }
}

pub fn compensate(
  runtime: orchestrator_state.RuntimeState,
  task_ref: task.TaskRef,
  run_id: String,
  abandonment_reason: String,
  parked_at_ms: Int,
  secrets: List(String),
) -> Compensation {
  let reason_text = claim_abandonment.reason_text(abandonment_reason)
  let source_run_id = claim_abandonment.source_run_id(run_id)
  let issue_identifier = claim_abandonment.task_identifier(task_ref)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task_ref,
      issue_id: task_ref.remote_id,
      identifier: issue_identifier,
      reason: orchestrator_reason.ParkOperator(reason_text),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: parked_at_ms,
    )
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  let runtime =
    orchestrator_state.mark_task_parked(runtime, task_identity, parked)
  let report = release_report(parked, reason_text, source_run_id)
  let release_intent =
    outbox_effects.release_claim_intent(
      report,
      claim_abandonment.release_source(source_run_id, abandonment_reason),
      secrets,
    )
  Compensation(runtime, parked, reason_text, release_intent)
}

fn release_report(
  parked: orchestrator_state.ParkedEntry,
  reason_text: String,
  source_run_id: Option(String),
) -> adapter.ParkReport {
  adapter.ParkReport(
    task: parked.task_ref,
    issue_identifier: parked.identifier,
    reason: reason_text,
    release_policy: Some(claim_abandonment.explicit_unpark_only),
    run_id: source_run_id,
  )
}
