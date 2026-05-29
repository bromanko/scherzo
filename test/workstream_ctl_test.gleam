import gleam/option.{None}
import scherzo/ctl/workstream as ctl_workstream

pub fn parses_start_from_handoff_command_test() {
  let assert Ok(ctl_workstream.StartFromHandoff(
    control_path: None,
    root: None,
    json_output: False,
    workflow_id: "execplan-implementation",
    action_id: "implement_exec_plan",
    handoff_ref: "workstream-artifacts/sha256/abc.json",
    handoff_sha256: "abc",
    gate_decision_ids: ["decision-1"],
  )) =
    ctl_workstream.parse(
      [
        "start-from-handoff",
        "execplan-implementation",
        "implement_exec_plan",
        "workstream-artifacts/sha256/abc.json",
        "abc",
        "decision-1",
      ],
      None,
      None,
      False,
    )
}

pub fn parses_start_from_input_bundle_command_test() {
  let assert Ok(ctl_workstream.StartFromInputBundle(
    control_path: None,
    root: None,
    json_output: True,
    workflow_id: "execplan-implementation",
    action_id: "rerun_from_bundle",
    input_bundle_ref: "workstream-artifacts/sha256/def.json",
    input_bundle_sha256: "def",
    gate_decision_ids: [],
  )) =
    ctl_workstream.parse(
      [
        "start-from-input-bundle",
        "execplan-implementation",
        "rerun_from_bundle",
        "workstream-artifacts/sha256/def.json",
        "def",
      ],
      None,
      None,
      True,
    )
}
