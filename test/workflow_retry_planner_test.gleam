import gleam/list
import gleam/option.{Some}
import scherzo/workflow_dag
import scherzo/workflow_retry_planner as planner

fn retry_yaml() -> String {
  "version: 1
id: retry-flow
concurrency: 1
steps:
  - id: collect
    kind: agent
    prompt: prompts/collect.md
    run_in:
      name: main
  - id: analyze
    depends_on: [collect]
    kind: agent
    prompt: prompts/analyze.md
    run_in:
      name: main
      from: main
  - id: publish
    depends_on: [analyze]
    kind: command
    run: echo publish
    run_in:
      name: main
      from: main
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
    guards: no_guards(),
  )
}

fn no_guards() -> planner.RetryGuards {
  planner.RetryGuards(
    operator_hold: False,
    terminal_issue: False,
    dispatch_paused: False,
    duplicate_active: False,
  )
}

pub fn exact_same_fingerprint_resume_test() {
  let plan = planner.plan(base_input())

  assert plan.safe_point == planner.ResumeFrom("publish")
  assert plan.preserved_step_ids == ["collect", "analyze"]
  assert plan.discarded_step_ids == ["publish"]
  assert plan.reason == Some("exact_workflow_match")
}

pub fn compatible_workflow_drift_rewinds_to_changed_boundary_test() {
  let input =
    planner.RetryPlannerInput(
      ..base_input(),
      current_workflow_fingerprint: "fp-b",
      compatibility: planner.CompatiblePrefix(["collect"], "analyze"),
    )

  let plan = planner.plan(input)

  assert plan.safe_point == planner.RewindTo("analyze")
  assert plan.preserved_step_ids == ["collect"]
  assert plan.discarded_step_ids == ["analyze", "publish"]
  assert plan.reason == Some("workflow_fingerprint_changed")
}

pub fn workflow_drift_without_snapshot_degrades_to_fresh_test() {
  let input =
    planner.RetryPlannerInput(
      ..base_input(),
      current_workflow_fingerprint: "fp-b",
      compatibility: planner.InterfaceSnapshotMissing,
    )

  let plan = planner.plan(input)

  assert plan.safe_point == planner.FreshStart
  assert plan.preserved_step_ids == []
  assert plan.discarded_step_ids == ["collect", "analyze", "publish"]
  assert plan.reason == Some("interface_snapshot_missing")
}

pub fn incompatible_workflow_degrades_to_fresh_test() {
  let input =
    planner.RetryPlannerInput(
      ..base_input(),
      current_workflow_fingerprint: "fp-b",
      compatibility: planner.WorkflowDagIncompatible,
    )

  let plan = planner.plan(input)

  assert plan.safe_point == planner.FreshStart
  assert plan.preserved_step_ids == []
  assert plan.discarded_step_ids == ["collect", "analyze", "publish"]
  assert plan.reason == Some("workflow_dag_incompatible")
}

pub fn corrupt_artifact_rewinds_before_first_unverified_step_test() {
  let input =
    planner.RetryPlannerInput(..base_input(), artifact_proofs: [
      planner.ArtifactVerified("collect"),
      planner.ArtifactShaMismatch("analyze"),
    ])

  let plan = planner.plan(input)

  assert plan.safe_point == planner.RewindTo("analyze")
  assert plan.preserved_step_ids == ["collect"]
  assert plan.discarded_step_ids == ["publish"]
  assert plan.reason == Some("artifact_sha_mismatch")
}

pub fn missing_artifact_rewinds_to_first_step_when_no_prefix_is_provable_test() {
  let input =
    planner.RetryPlannerInput(..base_input(), artifact_proofs: [
      planner.ArtifactMissing("collect"),
      planner.ArtifactVerified("analyze"),
    ])

  let plan = planner.plan(input)

  assert plan.safe_point == planner.RewindTo("collect")
  assert plan.preserved_step_ids == []
  assert plan.discarded_step_ids == ["publish"]
  assert plan.reason == Some("artifact_missing")
}

pub fn operator_hold_is_a_hard_stop_test() {
  assert_hard_stop(
    planner.RetryGuards(
      operator_hold: True,
      terminal_issue: False,
      dispatch_paused: False,
      duplicate_active: False,
    ),
    "operator_hold",
  )
}

pub fn terminal_issue_state_is_a_hard_stop_test() {
  assert_hard_stop(
    planner.RetryGuards(
      operator_hold: False,
      terminal_issue: True,
      dispatch_paused: False,
      duplicate_active: False,
    ),
    "terminal_issue_state",
  )
}

pub fn dispatch_pause_is_a_hard_stop_test() {
  assert_hard_stop(
    planner.RetryGuards(
      operator_hold: False,
      terminal_issue: False,
      dispatch_paused: True,
      duplicate_active: False,
    ),
    "dispatch_paused",
  )
}

pub fn active_duplicate_work_is_a_hard_stop_test() {
  assert_hard_stop(
    planner.RetryGuards(
      operator_hold: False,
      terminal_issue: False,
      dispatch_paused: False,
      duplicate_active: True,
    ),
    "active_duplicate_work",
  )
}

fn assert_hard_stop(guards: planner.RetryGuards, reason: String) {
  let plan =
    planner.plan(planner.RetryPlannerInput(..base_input(), guards: guards))

  assert plan.safe_point == planner.HardStop(reason)
  assert plan.preserved_step_ids == []
  assert plan.reason == Some(reason)
}

pub fn planner_matrix_is_complete_for_expected_cases_test() {
  let matrix = [
    exact_case(),
    changed_boundary_case(),
    missing_snapshot_case(),
    incompatible_case(),
    corrupt_artifact_case(),
    missing_artifact_case(),
    guard_case("operator_hold"),
    guard_case("terminal_issue_state"),
    guard_case("dispatch_paused"),
    guard_case("active_duplicate_work"),
  ]

  assert list.length(matrix) == 10
  assert list.all(matrix, fn(plan) {
    case plan.safe_point {
      planner.ResumeFrom(_)
      | planner.RewindTo(_)
      | planner.FreshStart
      | planner.HardStop(_) -> True
    }
  })
}

fn exact_case() -> planner.RetryPlan {
  planner.plan(base_input())
}

fn changed_boundary_case() -> planner.RetryPlan {
  planner.plan(
    planner.RetryPlannerInput(
      ..base_input(),
      current_workflow_fingerprint: "fp-b",
      compatibility: planner.CompatiblePrefix(["collect"], "analyze"),
    ),
  )
}

fn missing_snapshot_case() -> planner.RetryPlan {
  planner.plan(
    planner.RetryPlannerInput(
      ..base_input(),
      current_workflow_fingerprint: "fp-b",
      compatibility: planner.InterfaceSnapshotMissing,
    ),
  )
}

fn incompatible_case() -> planner.RetryPlan {
  planner.plan(
    planner.RetryPlannerInput(
      ..base_input(),
      current_workflow_fingerprint: "fp-b",
      compatibility: planner.WorkflowDagIncompatible,
    ),
  )
}

fn corrupt_artifact_case() -> planner.RetryPlan {
  planner.plan(
    planner.RetryPlannerInput(..base_input(), artifact_proofs: [
      planner.ArtifactVerified("collect"),
      planner.ArtifactShaMismatch("analyze"),
    ]),
  )
}

fn missing_artifact_case() -> planner.RetryPlan {
  planner.plan(
    planner.RetryPlannerInput(..base_input(), artifact_proofs: [
      planner.ArtifactMissing("collect"),
      planner.ArtifactVerified("analyze"),
    ]),
  )
}

fn guard_case(reason: String) -> planner.RetryPlan {
  case reason {
    "operator_hold" ->
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
      )
    "terminal_issue_state" ->
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
      )
    "dispatch_paused" ->
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
      )
    _ ->
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
      )
  }
}
