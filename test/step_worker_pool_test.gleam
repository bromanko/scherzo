import gleam/option.{None, Some}
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/workflow_dag
import scherzo/workflow_run/step_worker_pool
import scherzo/workspace_run
import test_async

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 200,
    template_field_max_chars: 200,
    workflow_summary_max_chars: 200,
  )
}

fn prepared_start(step_id: String) -> step_worker_pool.PreparedStart {
  let step =
    workflow_dag.WorkflowStep(
      id: step_id,
      kind: workflow_dag.CommandStep("sleep forever", Some(60_000)),
      depends_on: [],
      workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
      on_failure: workflow_dag.FailWorkflow,
      model_settings: model_config.default_settings(),
      recover: None,
    )
  let workspace =
    workspace_run.PreparedStepWorkspace(
      workflow_id: "workflow",
      run_id: "run",
      run_root: "test/tmp/step-worker-pool-timeout",
      workflow_bundle_dir: "test/tmp/step-worker-pool-timeout/bundle",
      attempt_index: 1,
      workspace_name: "main",
      path: "test/tmp/step-worker-pool-timeout/workspace",
      source_workspace_name: None,
      source_workspace_path: None,
      workspace_profile: "default",
    )
  step_worker_pool.prepared_start(step, workspace)
}

fn timeout_artifact(
  step_id: String,
  duration_ms: Int,
) -> step_artifact.StepArtifact {
  let stderr =
    "SCHERZO_FAILURE_CODE="
    <> step_worker_pool.step_batch_timeout_failure_code
    <> "\nstep batch deadline exceeded\n"
  step_artifact.from_command_result_with_metadata(
    step_id,
    None,
    124,
    Some(duration_ms),
    None,
    "",
    stderr,
    True,
    [],
    limits(),
    False,
    False,
  )
}

pub fn step_batch_deadline_returns_timeout_failure_test() {
  let barrier = test_async.new_barrier()
  let outcome_result =
    step_worker_pool.run_prepared_batch(
      [prepared_start("hung_step")],
      20,
      timeout_artifact,
      fn(_, _) { Ok(Nil) },
      fn(step, _) {
        test_async.block_until_released(barrier)
        #(
          step_artifact.from_command_result(
            step.id,
            0,
            "",
            "",
            False,
            [],
            limits(),
          ),
          session_tokens.zero_token_totals(),
          None,
          0,
        )
      },
    )
  test_async.release_barrier_if_waiting(barrier)

  let assert Ok(outcome) = outcome_result
  step_worker_pool.fold_step_batch_outcome(
    outcome,
    fn(_) { panic as "expected batch timeout to be fatal" },
    fn(result) {
      assert step_worker_pool.step_result_step_id(result) == "hung_step"
      let artifact = step_worker_pool.step_result_artifact(result)
      assert artifact.status == step_artifact.StepFailed
      assert artifact.exit_code == Some(124)
      assert artifact.timed_out == True
      assert artifact.failure_code
        == Some(step_worker_pool.step_batch_timeout_failure_code)
    },
  )
}

pub fn step_batch_deadline_uses_stable_timeout_step_id_test() {
  let barrier = test_async.new_barrier()
  let outcome_result =
    step_worker_pool.run_prepared_batch(
      [prepared_start("z_hung_step"), prepared_start("a_hung_step")],
      20,
      timeout_artifact,
      fn(_, _) { Ok(Nil) },
      fn(step, _) {
        test_async.block_until_released(barrier)
        #(
          step_artifact.from_command_result(
            step.id,
            0,
            "",
            "",
            False,
            [],
            limits(),
          ),
          session_tokens.zero_token_totals(),
          None,
          0,
        )
      },
    )
  test_async.release_barrier_if_waiting(barrier)
  test_async.release_barrier_if_waiting(barrier)

  let assert Ok(outcome) = outcome_result
  step_worker_pool.fold_step_batch_outcome(
    outcome,
    fn(_) { panic as "expected batch timeout to be fatal" },
    fn(result) {
      assert step_worker_pool.step_result_step_id(result) == "a_hung_step"
    },
  )
}
