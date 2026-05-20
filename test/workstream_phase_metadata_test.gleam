import gleam/json
import gleam/option.{None, Some}
import gleam/string
import scherzo/workflow_dag
import scherzo/workstream/phase_metadata
import simplifile

fn parse_ok(source: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(source)
  dag
}

fn error_code(source: String) -> String {
  let assert Error(workflow_dag.DagError(code, _)) = workflow_dag.parse(source)
  code
}

fn base_workflow(extra: String) -> String {
  "version: 1\nid: execplan_implementation\ncontract:\n  version: 1\n  outputs:\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: implement\n        path: tmp/code-change-bundle.json\nsteps:\n  - id: implement\n    kind: command\n    run: echo ok\n    workspace: main\n"
  <> extra
}

fn read_fixture(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

pub fn parses_optional_workstream_phase_metadata_test() {
  let dag =
    parse_ok(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  display_name: Artifact Specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  gates: [human_review]\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      state: available\n      priority: 3\n      inputs: [code_change_bundle]\n      requires_gate: human_review\n      auto_enqueue: false\n",
    ))

  let assert Some(metadata) = dag.workstream_phase
  assert metadata.phase_id == "artifact_specs"
  assert metadata.display_name == "Artifact Specs"
  let assert Some(handoff) = metadata.handoff
  assert handoff.output == "code_change_bundle"
  let assert [next_action] = metadata.next_actions
  assert next_action.state == "available"
  assert next_action.priority == 3
  assert next_action.inputs == ["code_change_bundle"]
}

pub fn workflow_without_workstream_phase_returns_none_test() {
  let dag = parse_ok(base_workflow(""))
  assert dag.contract != None
  assert dag.workstream_phase == None
}

pub fn rejects_unknown_handoff_output_test() {
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: unknown_output\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n",
    ))
    == "workstream_phase_handoff_unknown_output"
}

pub fn rejects_unknown_next_action_input_test() {
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [unknown_output]\n",
    ))
    == "workstream_phase_next_action_unknown_input"
}

pub fn rejects_invalid_snapshot_requirement_test() {
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  handoff:\n    output: code_change_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: optional\n",
    ))
    == "workstream_phase_snapshot_invalid"
}

pub fn rejects_non_map_metadata_test() {
  assert error_code(base_workflow("workstream_phase: nope\n"))
    == "workstream_phase_not_map"
}

pub fn rejects_missing_and_invalid_phase_id_test() {
  assert error_code(base_workflow("workstream_phase: {}\n"))
    == "workstream_phase_id_missing"
  assert error_code(base_workflow("workstream_phase:\n  phase_id: Bad Phase\n"))
    == "workstream_phase_id_invalid"
}

pub fn rejects_wrong_typed_optional_metadata_fields_test() {
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  display_name: [Artifact Specs]\n",
    ))
    == "workstream_phase_display_name_invalid"
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  final_phase: soon\n",
    ))
    == "workstream_phase_final_phase_invalid"
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      requires_gate: [human_review]\n",
    ))
    == "workstream_phase_requires_gate_invalid"
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      inputs: [code_change_bundle]\n      auto_enqueue: soon\n",
    ))
    == "workstream_phase_auto_enqueue_invalid"
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      state: [suggested]\n      inputs: [code_change_bundle]\n",
    ))
    == "workstream_phase_next_action_state_invalid"
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      state: ready\n      inputs: [code_change_bundle]\n",
    ))
    == "workstream_phase_next_action_state_invalid"
  assert error_code(base_workflow(
      "workstream_phase:\n  phase_id: artifact_specs\n  next_actions:\n    - action_id: revise_plan\n      workflow_id: execplan-revision\n      priority: soon\n      inputs: [code_change_bundle]\n",
    ))
    == "workstream_phase_next_action_priority_invalid"
}

pub fn current_workflows_remain_compatible_with_execplan_opt_in_test() {
  let execplan = parse_ok(read_fixture(".scherzo/workflows/execplan.yaml"))
  let revision =
    parse_ok(read_fixture(".scherzo/workflows/execplan-revision.yaml"))
  let implementation =
    parse_ok(read_fixture(".scherzo/workflows/execplan-implementation.yaml"))

  assert execplan.contract != None
  assert revision.contract != None
  assert implementation.contract != None
  let assert Some(execplan_phase) = execplan.workstream_phase
  assert execplan_phase.phase_id == "execplan"
  let assert Some(handoff) = execplan_phase.handoff
  assert handoff.output == "exec_plan_bundle"
  let assert [next_action] = execplan_phase.next_actions
  assert next_action.action_id == "implement_exec_plan"
  assert next_action.workflow_id == "execplan-implementation"
  assert next_action.state == "suggested"
  assert next_action.priority == 0
  assert next_action.inputs == ["exec_plan_bundle"]
  assert next_action.requires_gate == Some("human_review")
  assert next_action.auto_enqueue == False
  assert revision.workstream_phase == None
  assert implementation.workstream_phase == None

  let minimal =
    parse_ok(
      "version: 1\nid: minimal\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n    workspace: main\n",
    )
  assert minimal.contract == None
  assert minimal.workstream_phase == None
}

pub fn canonical_phase_metadata_json_includes_expected_fields_test() {
  let metadata =
    phase_metadata.PhaseMetadata(
      phase_id: "artifact_specs",
      display_name: "Artifact Specs",
      handoff: Some(phase_metadata.PhaseHandoff(
        output: "code_change_bundle",
        artifact_type: "scherzo.handoff.v1",
        snapshot: phase_metadata.SnapshotRequired,
      )),
      gates: ["human_review"],
      next_actions: [
        phase_metadata.PhaseNextAction(
          action_id: "revise_plan",
          workflow_id: "execplan-revision",
          state: "suggested",
          priority: 0,
          inputs: ["code_change_bundle"],
          requires_gate: Some("human_review"),
          auto_enqueue: False,
        ),
      ],
      final_phase: False,
    )

  let encoded = phase_metadata.to_canonical_json(metadata) |> json.to_string
  assert string.contains(encoded, "\"phase_id\":\"artifact_specs\"")
  assert string.contains(encoded, "\"output\":\"code_change_bundle\"")
  assert string.contains(encoded, "\"state\":\"suggested\"")
  assert string.contains(encoded, "\"priority\":0")
}
