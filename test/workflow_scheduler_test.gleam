import gleam/dict
import gleam/list
import scherzo/config/types as config_types
import scherzo/step_artifact
import scherzo/workflow_dag
import scherzo/workflow_scheduler

fn command_artifact(
  step_id: String,
  exit_code: Int,
) -> step_artifact.StepArtifact {
  step_artifact.from_command_result(
    step_id,
    exit_code,
    "",
    "",
    False,
    [],
    config_types.ArtifactLimits(
      command_stream_max_chars: 100,
      template_field_max_chars: 100,
      workflow_summary_max_chars: 100,
    ),
  )
}

fn implementation_dag(max_parallel_steps: Int) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: "
      <> int_to_string(max_parallel_steps)
      <> "\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement.md\n    workspace: main\n  - id: test_after_implement\n    kind: command\n    depends_on: [implement]\n    run: gleam test\n    workspace: main\n    on_failure: continue\n  - id: code_review\n    kind: agent\n    depends_on: [implement]\n    prompt: code.md\n    workspace:\n      name: code-review\n      from: main\n  - id: security_review\n    kind: agent\n    depends_on: [implement]\n    prompt: security.md\n    workspace:\n      name: security-review\n      from: main\n  - id: apply_feedback\n    kind: agent\n    depends_on: [test_after_implement, code_review, security_review]\n    prompt: apply.md\n    workspace: main\n",
    )
  dag
}

fn ids(steps: List(workflow_dag.WorkflowStep)) -> List(String) {
  list.map(steps, fn(step) { step.id })
}

pub fn initial_ready_roots_test() {
  let dag = implementation_dag(4)
  let state = workflow_scheduler.init(dag)
  assert ids(workflow_scheduler.ready_steps(dag, state)) == ["implement"]
}

pub fn dependent_not_ready_before_dependency_success_test() {
  let dag = implementation_dag(4)
  let state = workflow_scheduler.init(dag)
  assert !list.contains(
    ids(workflow_scheduler.ready_steps(dag, state)),
    "code_review",
  )
}

pub fn fan_out_after_implement_success_test() {
  let dag = implementation_dag(4)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 0),
    )
  assert ids(workflow_scheduler.ready_steps(dag, state))
    == ["test_after_implement", "code_review", "security_review"]
}

pub fn fan_in_waits_for_all_dependencies_test() {
  let dag = implementation_dag(4)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 0),
    )
    |> workflow_scheduler.mark_finished(
      "test_after_implement",
      command_artifact("test_after_implement", 0),
    )
    |> workflow_scheduler.mark_finished(
      "code_review",
      command_artifact("code_review", 0),
    )
  assert !list.contains(
    ids(workflow_scheduler.ready_steps(dag, state)),
    "apply_feedback",
  )
  let state =
    workflow_scheduler.mark_finished(
      state,
      "security_review",
      command_artifact("security_review", 0),
    )
  assert ids(workflow_scheduler.ready_steps(dag, state)) == ["apply_feedback"]
}

pub fn same_workspace_serialization_test() {
  let dag = implementation_dag(4)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 0),
    )
    |> workflow_scheduler.mark_running("test_after_implement")
  assert ids(workflow_scheduler.ready_steps(dag, state))
    == ["code_review", "security_review"]
}

pub fn different_workspaces_can_be_ready_together_test() {
  let dag = implementation_dag(4)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 0),
    )
    |> workflow_scheduler.mark_running("code_review")
  assert ids(workflow_scheduler.ready_steps(dag, state))
    == ["test_after_implement", "security_review"]
}

pub fn max_parallel_steps_limits_readiness_test() {
  let dag = implementation_dag(2)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 0),
    )
  assert ids(workflow_scheduler.ready_steps(dag, state))
    == ["test_after_implement", "code_review"]
}

pub fn fatal_failure_stops_new_ready_steps_test() {
  let dag = implementation_dag(4)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 1),
    )
  assert workflow_scheduler.ready_steps(dag, state) == []
  assert workflow_scheduler.outcome(dag, state)
    == workflow_scheduler.WorkflowFailed
}

pub fn on_failure_continue_counts_as_complete_test() {
  let dag = implementation_dag(4)
  let state =
    workflow_scheduler.init(dag)
    |> workflow_scheduler.mark_finished(
      "implement",
      command_artifact("implement", 0),
    )
    |> workflow_scheduler.mark_finished(
      "test_after_implement",
      command_artifact("test_after_implement", 1),
    )
    |> workflow_scheduler.mark_finished(
      "code_review",
      command_artifact("code_review", 0),
    )
    |> workflow_scheduler.mark_finished(
      "security_review",
      command_artifact("security_review", 0),
    )
  let assert Ok(workflow_scheduler.FailedContinued) =
    workflow_scheduler.status_of(state, "test_after_implement")
  assert ids(workflow_scheduler.ready_steps(dag, state)) == ["apply_feedback"]
}

pub fn init_with_statuses_rejects_unknown_step_test() {
  let dag = implementation_dag(4)
  let recovered = dict.from_list([#("ghost", workflow_scheduler.Succeeded)])
  assert workflow_scheduler.init_with_statuses(dag, recovered)
    == Error("unknown_recovered_step:ghost")
}

pub fn init_with_statuses_rejects_running_step_test() {
  let dag = implementation_dag(4)
  let recovered = dict.from_list([#("implement", workflow_scheduler.Running)])
  assert workflow_scheduler.init_with_statuses(dag, recovered)
    == Error("running_recovered_step:implement")
}

pub fn init_with_statuses_defaults_missing_steps_to_pending_test() {
  let dag = implementation_dag(4)
  let recovered = dict.from_list([#("implement", workflow_scheduler.Succeeded)])
  let assert Ok(state) = workflow_scheduler.init_with_statuses(dag, recovered)
  let assert Ok(workflow_scheduler.Succeeded) =
    workflow_scheduler.status_of(state, "implement")
  let assert Ok(workflow_scheduler.Pending) =
    workflow_scheduler.status_of(state, "code_review")
  assert ids(workflow_scheduler.ready_steps(dag, state))
    == ["test_after_implement", "code_review", "security_review"]
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
