import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import scherzo/agent/runner
import scherzo/domain
import scherzo/step_artifact
import scherzo/template
import scherzo/workflow_dag

fn limits() -> domain.ArtifactLimits {
  domain.ArtifactLimits(
    command_stream_max_chars: 12,
    template_field_max_chars: 12,
    workflow_summary_max_chars: 200,
  )
}

fn agent_success(text: String) -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: None,
    final_classification: runner.FinalTerminal,
    workspace_path: "workspace",
    tokens: domain.zero_token_totals(),
    turns: 1,
    result: domain.ResultArtifact(
      final_response: Some(text),
      truncated: False,
      source: "test",
    ),
  )
}

fn lookup(
  locals: List(#(String, template.Value)),
  key: String,
) -> template.Value {
  let assert Ok(value) = list.key_find(locals, key)
  value
}

pub fn agent_success_artifact_exposes_template_local_test() {
  let artifact =
    step_artifact.from_agent_success(
      "code_review",
      agent_success("Looks good"),
      [],
      limits(),
    )
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("code_review", artifact)]),
    )
  assert lookup(locals, "steps.code_review.status")
    == template.VString("success")
  assert lookup(locals, "steps.code_review.final_response")
    == template.VString("Looks good")
}

pub fn command_success_artifact_exposes_exit_and_stdout_test() {
  let artifact =
    step_artifact.from_command_result(
      "test_after_implement",
      0,
      "all passed",
      "",
      False,
      [],
      limits(),
    )
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("test_after_implement", artifact)]),
    )
  assert lookup(locals, "steps.test_after_implement.status")
    == template.VString("success")
  assert lookup(locals, "steps.test_after_implement.exit_code")
    == template.VInt(0)
  assert lookup(locals, "steps.test_after_implement.stdout")
    == template.VString("all passed")
}

pub fn command_failure_and_timeout_are_artifacts_test() {
  let artifact =
    step_artifact.from_command_result(
      "test_after_implement",
      124,
      "",
      "timed out",
      True,
      [],
      limits(),
    )
  assert artifact.status == "failure"
  assert artifact.exit_code == Some(124)
  assert artifact.timed_out == True
}

pub fn truncates_command_streams_and_sets_flags_test() {
  let artifact =
    step_artifact.from_command_result(
      "long_command",
      0,
      "12345678901234567890",
      "abcdefghijklmnop",
      False,
      [],
      limits(),
    )
  assert artifact.stdout == "123456789012..."
  assert artifact.stderr == "abcdefghijkl..."
  assert artifact.stdout_truncated == True
  assert artifact.stderr_truncated == True
}

pub fn redacts_fake_secret_before_exposing_artifacts_test() {
  let artifact =
    step_artifact.from_command_result(
      "secret_command",
      0,
      "token test-key",
      "stderr test-key",
      False,
      ["test-key"],
      limits(),
    )
  assert artifact.stdout == "token [REDAC..."
  assert artifact.stderr == "stderr [REDA..."
}

pub fn workflow_result_uses_terminal_step_and_summary_test() {
  let dag =
    workflow_dag.WorkflowDag(
      id: "implementation",
      description: None,
      max_parallel_steps: 2,
      steps: [
        workflow_dag.WorkflowStep(
          id: "implement",
          kind: workflow_dag.AgentStep(workflow_dag.PromptInline("implement")),
          depends_on: [],
          workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
          on_failure: workflow_dag.FailWorkflow,
        ),
        workflow_dag.WorkflowStep(
          id: "final_test",
          kind: workflow_dag.CommandStep(run: "gleam test", timeout_ms: None),
          depends_on: ["implement"],
          workspace: workflow_dag.WorkspaceRef(name: "main", from: None),
          on_failure: workflow_dag.FailWorkflow,
        ),
      ],
    )
  let implement_artifact =
    step_artifact.from_agent_success(
      "implement",
      agent_success("implemented"),
      [],
      limits(),
    )
  let final =
    step_artifact.from_command_result(
      "final_test",
      0,
      "all passed",
      "",
      False,
      [],
      limits(),
    )
  let result =
    step_artifact.workflow_result_artifact(
      dag,
      dict.from_list([
        #("implement", implement_artifact),
        #("final_test", final),
      ]),
      limits(),
    )
  let assert Some(text) = result.final_response
  assert text
    == "all passed\n\nWorkflow step summary:\nimplement success agent\nfinal_test success command exit_code=0"
  assert result.source == "workflow_dag"
}
