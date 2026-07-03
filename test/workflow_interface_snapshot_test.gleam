import gleam/option.{None, Some}
import scherzo/model_config
import scherzo/workflow_dag
import scherzo/workflow_interface_snapshot

pub fn snapshot_roundtrip_and_prefix_compatibility_test() {
  let dag = sample_dag()
  let snapshot = workflow_interface_snapshot.from_dag(dag, "fp-1")
  let encoded = workflow_interface_snapshot.to_string(snapshot)
  let assert Ok(decoded) = workflow_interface_snapshot.decode_string(encoded)
  assert decoded == snapshot

  let changed_snapshot =
    workflow_interface_snapshot.from_dag(changed_dag(), "fp-2")
  assert workflow_interface_snapshot.compatible_prefix(
      snapshot,
      changed_snapshot,
      "publish",
    )
    == Some(#(["collect"], "analyze"))
}

pub fn incompatible_workflow_id_returns_none_test() {
  let recorded = workflow_interface_snapshot.from_dag(sample_dag(), "fp-1")
  let current =
    workflow_interface_snapshot.from_dag(other_workflow_dag(), "fp-1")
  assert workflow_interface_snapshot.compatible_prefix(
      recorded,
      current,
      "publish",
    )
    == None
}

fn sample_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.new(
      id: "workflow-a",
      description: None,
      workspace_profile: None,
      workspace_capabilities: [],
      max_parallel_steps: 1,
      recover: None,
      steps: [
        workflow_dag.WorkflowStep(
          id: "collect",
          kind: workflow_dag.CommandStep("echo collect", None),
          depends_on: [],
          workspace: workflow_dag.WorkspaceRef("main", None),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
        workflow_dag.WorkflowStep(
          id: "analyze",
          kind: workflow_dag.AgentStep(
            workflow_dag.PromptFile("prompts/analyze.md"),
            None,
          ),
          depends_on: ["collect"],
          workspace: workflow_dag.WorkspaceRef("analysis", Some("main")),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
        workflow_dag.WorkflowStep(
          id: "publish",
          kind: workflow_dag.CommandStep("echo publish", Some(1000)),
          depends_on: ["analyze"],
          workspace: workflow_dag.WorkspaceRef("publish", Some("analysis")),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
      ],
      contract: None,
      publication_routes: [],
      workstream_phase: None,
    )
  dag
}

fn changed_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.new(
      id: "workflow-a",
      description: None,
      workspace_profile: None,
      workspace_capabilities: [],
      max_parallel_steps: 1,
      recover: None,
      steps: [
        workflow_dag.WorkflowStep(
          id: "collect",
          kind: workflow_dag.CommandStep("echo collect", None),
          depends_on: [],
          workspace: workflow_dag.WorkspaceRef("main", None),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
        workflow_dag.WorkflowStep(
          id: "analyze",
          kind: workflow_dag.CommandStep("echo changed", None),
          depends_on: ["collect"],
          workspace: workflow_dag.WorkspaceRef("analysis", Some("main")),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
        workflow_dag.WorkflowStep(
          id: "publish",
          kind: workflow_dag.CommandStep("echo publish", Some(1000)),
          depends_on: ["analyze"],
          workspace: workflow_dag.WorkspaceRef("publish", Some("analysis")),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
      ],
      contract: None,
      publication_routes: [],
      workstream_phase: None,
    )
  dag
}

fn other_workflow_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.new(
      id: "workflow-b",
      description: None,
      workspace_profile: None,
      workspace_capabilities: [],
      max_parallel_steps: 1,
      recover: None,
      steps: [
        workflow_dag.WorkflowStep(
          id: "collect",
          kind: workflow_dag.CommandStep("echo collect", None),
          depends_on: [],
          workspace: workflow_dag.WorkspaceRef("main", None),
          on_failure: workflow_dag.FailWorkflow,
          model_settings: sample_model_settings(),
          recover: None,
        ),
      ],
      contract: None,
      publication_routes: [],
      workstream_phase: None,
    )
  dag
}

fn sample_model_settings() {
  model_config.default_settings()
}
