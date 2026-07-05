import gleam/list
import scherzo/workflow_dag
import scherzo/workflow_retry_planner as planner

fn retry_yaml() -> String {
  "version: 1
id: retry-flow
steps:
  - id: collect
    kind: command
    run: collect
    run_in: main
  - id: analyze
    depends_on: [collect]
    kind: command
    run: analyze
    run_in: main
  - id: publish
    depends_on: [analyze]
    kind: command
    run: publish
    run_in: main
"
}

fn dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(retry_yaml())
  dag
}

fn base_input() -> planner.RetryPlannerInput {
  planner.RetryPlannerInput(
    run_workflow_id: "retry-flow",
    run_workflow_fingerprint: "fp-a",
    current_workflow_id: "retry-flow",
    current_workflow_fingerprint: "fp-a",
    repair_step_id: "publish",
    dag: dag(),
    attempted_step_ids: ["collect", "analyze", "publish"],
    artifact_proofs: [
      planner.ArtifactVerified("collect"),
      planner.ArtifactVerified("analyze"),
    ],
    compatibility: planner.ExactWorkflowMatch,
    guards: planner.RetryGuards(
      operator_hold: False,
      terminal_issue: False,
      dispatch_paused: False,
      duplicate_active: False,
    ),
  )
}

pub fn retained_run_matrix_keeps_common_retry_total_for_non_guarded_cases_test() {
  let non_guarded = [
    planner.plan(base_input()),
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        current_workflow_fingerprint: "fp-b",
        compatibility: planner.CompatiblePrefix(["collect"], "analyze"),
      ),
    ),
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        current_workflow_fingerprint: "fp-b",
        compatibility: planner.InterfaceSnapshotMissing,
      ),
    ),
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        current_workflow_fingerprint: "fp-b",
        compatibility: planner.WorkflowDagIncompatible,
      ),
    ),
    planner.plan(
      planner.RetryPlannerInput(..base_input(), artifact_proofs: [
        planner.ArtifactVerified("collect"),
        planner.ArtifactShaMismatch("analyze"),
      ]),
    ),
    planner.plan(
      planner.RetryPlannerInput(..base_input(), artifact_proofs: [
        planner.ArtifactMissing("collect"),
        planner.ArtifactVerified("analyze"),
      ]),
    ),
  ]

  assert list.length(non_guarded) == 6
  assert list.all(non_guarded, fn(plan) {
    case plan.safe_point {
      planner.HardStop(_) -> False
      planner.ResumeFrom(_) | planner.RewindTo(_) | planner.FreshStart -> True
    }
  })
}

pub fn retained_run_matrix_preserves_hard_stops_test() {
  let guarded = [
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        guards: planner.RetryGuards(
          operator_hold: True,
          terminal_issue: False,
          dispatch_paused: False,
          duplicate_active: False,
        ),
      ),
    ),
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        guards: planner.RetryGuards(
          operator_hold: False,
          terminal_issue: True,
          dispatch_paused: False,
          duplicate_active: False,
        ),
      ),
    ),
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        guards: planner.RetryGuards(
          operator_hold: False,
          terminal_issue: False,
          dispatch_paused: True,
          duplicate_active: False,
        ),
      ),
    ),
    planner.plan(
      planner.RetryPlannerInput(
        ..base_input(),
        guards: planner.RetryGuards(
          operator_hold: False,
          terminal_issue: False,
          dispatch_paused: False,
          duplicate_active: True,
        ),
      ),
    ),
  ]

  assert list.all(guarded, fn(plan) {
    case plan.safe_point {
      planner.HardStop(_) -> True
      _ -> False
    }
  })
}
