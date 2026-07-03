import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/workflow_dag

pub type RetryPlannerInput {
  RetryPlannerInput(
    run_workflow_id: String,
    run_workflow_fingerprint: String,
    current_workflow_id: String,
    current_workflow_fingerprint: String,
    repair_step_id: String,
    dag: workflow_dag.WorkflowDag,
    attempted_step_ids: List(String),
    artifact_proofs: List(ArtifactProof),
    compatibility: WorkflowCompatibility,
    guards: RetryGuards,
  )
}

pub type RetryGuards {
  RetryGuards(
    operator_hold: Bool,
    terminal_issue: Bool,
    dispatch_paused: Bool,
    duplicate_active: Bool,
  )
}

pub type ArtifactProof {
  ArtifactVerified(step_id: String)
  ArtifactMissing(step_id: String)
  ArtifactShaMismatch(step_id: String)
  ArtifactUnverified(step_id: String, reason: String)
}

pub type WorkflowCompatibility {
  ExactWorkflowMatch
  CompatiblePrefix(preserved_step_ids: List(String), rewind_step_id: String)
  InterfaceSnapshotMissing
  WorkflowDagIncompatible
}

pub type SafePoint {
  ResumeFrom(step_id: String)
  RewindTo(step_id: String)
  FreshStart
  HardStop(reason: String)
}

pub type RetryPlan {
  RetryPlan(
    safe_point: SafePoint,
    preserved_step_ids: List(String),
    discarded_step_ids: List(String),
    reason: Option(String),
    summary: String,
  )
}

pub fn plan(input: RetryPlannerInput) -> RetryPlan {
  let RetryPlannerInput(
    run_workflow_id,
    run_workflow_fingerprint,
    current_workflow_id,
    current_workflow_fingerprint,
    repair_step_id,
    dag,
    attempted_step_ids,
    artifact_proofs,
    compatibility,
    guards,
  ) = input
  let discarded_step_ids = descendants_including_self(dag, repair_step_id)
  let fresh_discarded_step_ids =
    discarded_steps_for_fresh(dag, repair_step_id, attempted_step_ids)

  case hard_stop_reason(guards) {
    Some(reason) ->
      RetryPlan(
        safe_point: HardStop(reason),
        preserved_step_ids: [],
        discarded_step_ids: discarded_step_ids,
        reason: Some(reason),
        summary: "retry blocked: " <> reason,
      )

    None -> {
      let same_workflow_id = run_workflow_id == current_workflow_id
      let same_fingerprint =
        run_workflow_fingerprint == current_workflow_fingerprint
      let repair_prefix = repair_prefix(dag, repair_step_id)

      case same_workflow_id && same_fingerprint {
        True ->
          plan_from_prefix(
            repair_step_id: repair_step_id,
            repair_prefix: repair_prefix,
            discarded_step_ids: discarded_step_ids,
            proofs: artifact_proofs,
            fallback_reason: "exact_workflow_match",
            on_success: ResumeFrom(repair_step_id),
          )

        False ->
          case compatibility {
            CompatiblePrefix(preserved_step_ids, rewind_step_id) ->
              plan_from_prefix(
                repair_step_id: rewind_step_id,
                repair_prefix: preserved_step_ids,
                discarded_step_ids: descendants_including_self(
                  dag,
                  rewind_step_id,
                ),
                proofs: artifact_proofs,
                fallback_reason: "workflow_fingerprint_changed",
                on_success: RewindTo(rewind_step_id),
              )
            InterfaceSnapshotMissing ->
              fresh_plan(fresh_discarded_step_ids, "interface_snapshot_missing")
            WorkflowDagIncompatible ->
              fresh_plan(fresh_discarded_step_ids, "workflow_dag_incompatible")
            ExactWorkflowMatch ->
              fresh_plan(fresh_discarded_step_ids, "workflow_identity_changed")
          }
      }
    }
  }
}

fn plan_from_prefix(
  repair_step_id repair_step_id: String,
  repair_prefix repair_prefix: List(String),
  discarded_step_ids discarded_step_ids: List(String),
  proofs proofs: List(ArtifactProof),
  fallback_reason fallback_reason: String,
  on_success on_success: SafePoint,
) -> RetryPlan {
  case first_unverified_step(repair_prefix, proofs) {
    Some(#(step_id, reason)) -> {
      let preserved_step_ids = verified_prefix_before(repair_prefix, step_id)
      RetryPlan(
        safe_point: RewindTo(step_id),
        preserved_step_ids: preserved_step_ids,
        discarded_step_ids: discarded_step_ids,
        reason: Some(reason),
        summary: "rewind to "
          <> step_id
          <> " after losing proof for preserved prefix",
      )
    }

    None ->
      RetryPlan(
        safe_point: on_success,
        preserved_step_ids: repair_prefix,
        discarded_step_ids: discarded_step_ids,
        reason: Some(fallback_reason),
        summary: success_summary(on_success, repair_step_id),
      )
  }
}

fn success_summary(point: SafePoint, repair_step_id: String) -> String {
  case point {
    ResumeFrom(_) -> "resume from " <> repair_step_id
    RewindTo(step_id) -> "rewind to " <> step_id
    FreshStart -> "start fresh"
    HardStop(reason) -> "retry blocked: " <> reason
  }
}

fn fresh_plan(discarded_step_ids: List(String), reason: String) -> RetryPlan {
  RetryPlan(
    safe_point: FreshStart,
    preserved_step_ids: [],
    discarded_step_ids: discarded_step_ids,
    reason: Some(reason),
    summary: "start fresh: " <> reason,
  )
}

fn hard_stop_reason(guards: RetryGuards) -> Option(String) {
  let RetryGuards(
    operator_hold,
    terminal_issue,
    dispatch_paused,
    duplicate_active,
  ) = guards
  case operator_hold {
    True -> Some("operator_hold")
    False ->
      case terminal_issue {
        True -> Some("terminal_issue_state")
        False ->
          case dispatch_paused {
            True -> Some("dispatch_paused")
            False ->
              case duplicate_active {
                True -> Some("active_duplicate_work")
                False -> None
              }
          }
      }
  }
}

fn discarded_steps_for_fresh(
  dag: workflow_dag.WorkflowDag,
  repair_step_id: String,
  attempted_step_ids: List(String),
) -> List(String) {
  let descendants = descendants_including_self(dag, repair_step_id)
  let all = list.append(attempted_step_ids, descendants)
  unique_in_dag_order(dag, all)
}

fn repair_prefix(
  dag: workflow_dag.WorkflowDag,
  repair_step_id: String,
) -> List(String) {
  dag
  |> workflow_dag.steps
  |> list.map(fn(step) { step.id })
  |> list.filter(fn(step_id) { is_upstream_of(dag, step_id, repair_step_id) })
}

fn descendants_including_self(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
) -> List(String) {
  dag
  |> workflow_dag.steps
  |> list.map(fn(step) { step.id })
  |> list.filter(fn(candidate) { depends_on_path(dag, candidate, step_id) })
}

fn is_upstream_of(
  dag: workflow_dag.WorkflowDag,
  candidate_step_id: String,
  repair_step_id: String,
) -> Bool {
  candidate_step_id != repair_step_id
  && depends_on_path(dag, repair_step_id, candidate_step_id)
}

fn depends_on_path(
  dag: workflow_dag.WorkflowDag,
  from_step_id: String,
  target_step_id: String,
) -> Bool {
  case from_step_id == target_step_id {
    True -> True
    False ->
      case workflow_dag.step_by_id(dag, from_step_id) {
        Error(Nil) -> False
        Ok(step) ->
          step.depends_on
          |> list.any(fn(parent_id) {
            depends_on_path(dag, parent_id, target_step_id)
          })
      }
  }
}

fn first_unverified_step(
  repair_prefix: List(String),
  proofs: List(ArtifactProof),
) -> Option(#(String, String)) {
  case repair_prefix {
    [] -> None
    [step_id, ..rest] ->
      case proof_reason(proofs, step_id) {
        Some(reason) -> Some(#(step_id, reason))
        None -> first_unverified_step(rest, proofs)
      }
  }
}

fn proof_reason(
  proofs: List(ArtifactProof),
  step_id: String,
) -> Option(String) {
  case proofs {
    [] -> None
    [proof, ..rest] ->
      case proof {
        ArtifactVerified(proof_step_id) if proof_step_id == step_id -> None
        ArtifactMissing(proof_step_id) if proof_step_id == step_id ->
          Some("artifact_missing")
        ArtifactShaMismatch(proof_step_id) if proof_step_id == step_id ->
          Some("artifact_sha_mismatch")
        ArtifactUnverified(proof_step_id, reason) if proof_step_id == step_id ->
          Some(reason)
        _ -> proof_reason(rest, step_id)
      }
  }
}

fn verified_prefix_before(
  repair_prefix: List(String),
  boundary_step_id: String,
) -> List(String) {
  case repair_prefix {
    [] -> []
    [first, ..] if first == boundary_step_id -> []
    [first, ..rest] -> [first, ..verified_prefix_before(rest, boundary_step_id)]
  }
}

fn unique_in_dag_order(
  dag: workflow_dag.WorkflowDag,
  step_ids: List(String),
) -> List(String) {
  dag
  |> workflow_dag.steps
  |> list.map(fn(step) { step.id })
  |> list.filter(fn(step_id) { list.contains(step_ids, step_id) })
  |> dedupe([])
}

fn dedupe(step_ids: List(String), seen: List(String)) -> List(String) {
  case step_ids {
    [] -> []
    [first, ..rest] ->
      case list.contains(seen, first) {
        True -> dedupe(rest, seen)
        False -> [first, ..dedupe(rest, [first, ..seen])]
      }
  }
}
