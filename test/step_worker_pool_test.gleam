import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/workflow_dag
import scherzo/workflow_run/step_worker_pool
import scherzo/workspace
import scherzo/workspace_run
import test_async

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 1000,
  )
}

fn prepared_start(step_id: String) -> step_worker_pool.PreparedStart {
  prepared_start_with_command(step_id, "sleep forever")
}

fn prepared_start_with_command(
  step_id: String,
  command: String,
) -> step_worker_pool.PreparedStart {
  let step =
    workflow_dag.WorkflowStep(
      id: step_id,
      kind: workflow_dag.CommandStep(command, Some(60_000)),
      depends_on: [],
      workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
      on_failure: workflow_dag.FailWorkflow,
      model_settings: model_config.default_settings(),
      recover: None,
    )
  prepared_start_with_step(step)
}

fn prepared_agent_start(step_id: String) -> step_worker_pool.PreparedStart {
  let step =
    workflow_dag.WorkflowStep(
      id: step_id,
      kind: workflow_dag.AgentStep(workflow_dag.PromptInline("prompt"), None),
      depends_on: [],
      workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
      on_failure: workflow_dag.FailWorkflow,
      model_settings: model_config.default_settings(),
      recover: None,
    )
  prepared_start_with_step(step)
}

fn prepared_start_with_step(
  step: workflow_dag.WorkflowStep,
) -> step_worker_pool.PreparedStart {
  let workspace =
    workspace_run.PreparedStepWorkspace(
      workflow_id: "workflow",
      run_id: "run",
      run_root: "test/tmp/step-worker-pool-timeout",
      workflow_bundle_dir: "test/tmp/step-worker-pool-timeout/bundle",
      attempt_index: 1,
      workspace_name: "main",
      path: "test/tmp/step-worker-pool-timeout/workspace",
      source: workspace.FreshWorkspace,
      workspace_profile: "default",
    )
  step_worker_pool.prepared_start(step, workspace)
}

fn timeout_artifact(
  timeout: step_worker_pool.StepBatchTimeoutContext,
) -> step_artifact.StepArtifact {
  let stderr =
    "SCHERZO_FAILURE_CODE="
    <> step_worker_pool.step_batch_timeout_failure_code
    <> "\nstep batch deadline exceeded\n"
    <> timeout_metadata(timeout)
  step_artifact.from_command_result_with_metadata(
    timeout.step_id,
    timeout.command,
    124,
    Some(timeout.duration_ms),
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

fn secret_timeout_artifact(
  timeout: step_worker_pool.StepBatchTimeoutContext,
) -> step_artifact.StepArtifact {
  step_artifact.from_command_result_with_metadata(
    timeout.step_id,
    timeout.command,
    124,
    Some(timeout.duration_ms),
    None,
    "",
    "SCHERZO_FAILURE_CODE="
      <> step_worker_pool.step_batch_timeout_failure_code
      <> "\n",
    True,
    ["super-secret"],
    limits(),
    False,
    False,
  )
}

fn timeout_metadata(
  timeout: step_worker_pool.StepBatchTimeoutContext,
) -> String {
  option_string_line("diagnostic_step_id", timeout.diagnostic_step_id)
  <> "duration_ms: "
  <> int.to_string(timeout.duration_ms)
  <> "\n"
  <> "batch_started_monotonic_ms: "
  <> int.to_string(timeout.batch_started_monotonic_ms)
  <> "\n"
  <> "batch_deadline_monotonic_ms: "
  <> int.to_string(timeout.batch_deadline_monotonic_ms)
  <> "\n"
  <> "timeout_monotonic_ms: "
  <> int.to_string(timeout.timed_out_monotonic_ms)
  <> "\n"
}

fn option_string_line(label: String, value: Option(String)) -> String {
  case value {
    Some(value) -> label <> ": " <> value <> "\n"
    None -> ""
  }
}

pub fn step_batch_deadline_returns_timeout_failure_test() {
  let barrier = test_async.new_barrier()
  let outcome_result =
    step_worker_pool.run_prepared_batch(
      [prepared_start("hung_step")],
      20,
      timeout_artifact,
      fn(_) { False },
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
    fn(result, _, interrupted_step_ids, drained) {
      assert step_worker_pool.step_result_step_id(result) == "hung_step"
      assert interrupted_step_ids == []
      assert drained == False
      let artifact = step_worker_pool.step_result_artifact(result)
      assert artifact.status == step_artifact.StepFailed
      assert artifact.exit_code == Some(124)
      assert artifact.timed_out == True
      assert artifact.failure_code
        == Some(step_worker_pool.step_batch_timeout_failure_code)
      assert artifact.command == Some("sleep forever")
      assert string.contains(artifact.stderr, "diagnostic_step_id: hung_step")
      assert string.contains(artifact.stderr, "batch_started_monotonic_ms:")
      assert string.contains(artifact.stderr, "batch_deadline_monotonic_ms:")
      assert string.contains(artifact.stderr, "timeout_monotonic_ms:")
    },
  )
}

pub fn step_batch_deadline_agent_step_keeps_command_empty_test() {
  let barrier = test_async.new_barrier()
  let outcome_result =
    step_worker_pool.run_prepared_batch(
      [prepared_agent_start("agent_hung_step")],
      20,
      timeout_artifact,
      fn(_) { False },
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
    fn(result, _, interrupted_step_ids, drained) {
      assert step_worker_pool.step_result_step_id(result) == "agent_hung_step"
      assert interrupted_step_ids == []
      assert drained == False
      let artifact = step_worker_pool.step_result_artifact(result)
      assert artifact.command == None
      assert artifact.failure_code
        == Some(step_worker_pool.step_batch_timeout_failure_code)
      assert string.contains(
        artifact.stderr,
        "diagnostic_step_id: agent_hung_step",
      )
      assert string.contains(artifact.stderr, "timeout_monotonic_ms:")
    },
  )
}

pub fn step_batch_deadline_redacts_captured_command_identity_test() {
  let barrier = test_async.new_barrier()
  let outcome_result =
    step_worker_pool.run_prepared_batch(
      [prepared_start_with_command("secret_step", "echo super-secret; sleep 1")],
      20,
      secret_timeout_artifact,
      fn(_) { False },
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
    fn(result, _, _, _) {
      let artifact = step_worker_pool.step_result_artifact(result)
      assert artifact.command == Some("echo [REDACTED]; sleep 1")
      assert !string.contains(step_artifact.to_string(artifact), "super-secret")
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
      fn(_) { False },
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
    fn(result, _, interrupted_step_ids, drained) {
      assert step_worker_pool.step_result_step_id(result) == "a_hung_step"
      assert interrupted_step_ids == ["z_hung_step"]
      assert drained == False
      let artifact = step_worker_pool.step_result_artifact(result)
      assert artifact.command == None
      assert !string.contains(artifact.stderr, "diagnostic_step_id:")
    },
  )
}
