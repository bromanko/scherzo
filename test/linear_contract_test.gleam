import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/linear_contract

fn state(id: String, name: String) -> linear_contract.RemoteState {
  linear_contract.RemoteState(id: id, name: name, type_: "started")
}

fn label(id: String, name: String) -> linear_contract.RemoteLabel {
  linear_contract.RemoteLabel(id: id, name: name)
}

fn team(
  key: String,
  states: List(linear_contract.RemoteState),
  labels: List(linear_contract.RemoteLabel),
) -> linear_contract.RemoteTeam {
  linear_contract.RemoteTeam(
    id: "team-" <> key,
    key: key,
    name: key <> " Team",
    states: states,
    labels: labels,
  )
}

fn board(
  teams: List(linear_contract.RemoteTeam),
  workspace_labels: List(linear_contract.RemoteLabel),
) -> linear_contract.RemoteBoard {
  linear_contract.RemoteBoard(
    project_id: "project-id",
    project_slug: "PROJ",
    project_name: "Project",
    teams: teams,
    workspace_labels: workspace_labels,
  )
}

fn handoff_config(
  enabled: Bool,
  claim: Option(String),
  success: Option(String),
  failure: Option(String),
) -> domain.HandoffConfig {
  domain.HandoffConfig(
    enabled: enabled,
    comment_on_claim: enabled,
    comment_on_success: enabled,
    comment_on_failure: enabled,
    claim_state_id: claim,
    success_state_id: success,
    failure_state_id: failure,
    include_result_on_success: enabled,
    result_max_chars: 8000,
  )
}

fn contract_config(enabled: Bool) -> domain.LinearContractConfig {
  domain.LinearContractConfig(
    enabled: enabled,
    workflow_label_prefix: "workflow:",
    workflow_labels: ["bugfix", "feature"],
    support_labels: ["needs-workflow"],
    required_states: dict.from_list([
      #("done", "Done"),
      #("in_progress", "In Progress"),
      #("ready", "Ready for Agent"),
    ]),
    handoff_state_bindings: dict.from_list([
      #("claim", "in_progress"),
      #("success", "done"),
      #("failure", "ready"),
    ]),
    enforce_issue_workflow_labels: False,
    invalid_workflow_state_id: None,
    comment_on_invalid_workflow: False,
  )
}

fn effective(
  contract: domain.LinearContractConfig,
  handoff: domain.HandoffConfig,
  active_states: List(String),
  terminal_states: List(String),
) -> domain.EffectiveConfig {
  domain.EffectiveConfig(
    tracker: domain.TrackerConfig(
      ..config.default_tracker_config(),
      api_key: Some("secret"),
      project_slug: Some("PROJ"),
      active_states: active_states,
      terminal_states: terminal_states,
    ),
    polling: config.default_polling_config(),
    workspace: config.default_workspace_config("scherzo.yaml"),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: handoff,
    linear_contract: contract,
    linear_commands: config.default_linear_command_config(),
  )
}

fn all_states() -> List(linear_contract.RemoteState) {
  [
    state("state-ready", "Ready for Agent"),
    state("state-progress", "In Progress"),
    state("state-done", "Done"),
  ]
}

fn all_labels() -> List(linear_contract.RemoteLabel) {
  [
    label("label-bugfix", "workflow:bugfix"),
    label("label-feature", "workflow:feature"),
    label("label-needs-workflow", "needs-workflow"),
  ]
}

pub fn all_clear_single_team_contract_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(True),
        handoff_config(True, Some("state-progress"), Some("state-done"), None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", all_states(), all_labels())], []),
    )
  assert diagnostics == []
  assert linear_contract.is_ok(diagnostics)
}

pub fn reports_missing_state_for_project_team_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(False),
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team("ENG", all_states(), []),
          team("OPS", [state("state-done", "Done")], []),
        ],
        [],
      ),
    )
  assert diagnostics
    == [
      linear_contract.MissingState(
        team_key: "OPS",
        name: "Ready for Agent",
        source: "tracker.active_states",
      ),
    ]
  assert string_contains(
    linear_contract.format_report(diagnostics),
    "missing_state",
  )
}

pub fn reports_missing_required_label_for_project_team_test() {
  let contract =
    domain.LinearContractConfig(
      ..contract_config(True),
      workflow_labels: ["research"],
      support_labels: [],
      required_states: dict.new(),
      handoff_state_bindings: dict.new(),
    )
  let diagnostics =
    linear_contract.check(
      effective(
        contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team("ENG", all_states(), [
            label("label-research", "workflow:research"),
          ]),
          team("OPS", all_states(), []),
        ],
        [],
      ),
    )
  assert diagnostics
    == [
      linear_contract.MissingLabel(
        team_key: "OPS",
        name: "workflow:research",
        source: "linear_contract.workflow_labels",
      ),
    ]
}

pub fn workspace_label_is_assignable_to_every_team_test() {
  let contract =
    domain.LinearContractConfig(
      ..contract_config(True),
      workflow_labels: ["research"],
      support_labels: [],
      required_states: dict.new(),
      handoff_state_bindings: dict.new(),
    )
  let diagnostics =
    linear_contract.check(
      effective(
        contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team("ENG", all_states(), []),
          team("OPS", all_states(), []),
        ],
        [label("workspace-research", "Workflow:Research")],
      ),
    )
  assert diagnostics == []
}

pub fn enforcement_requires_workflow_labels_without_support_labels_test() {
  let contract =
    domain.LinearContractConfig(
      ..contract_config(False),
      workflow_labels: ["bugfix"],
      support_labels: ["needs-workflow"],
      enforce_issue_workflow_labels: True,
    )
  let diagnostics =
    linear_contract.check(
      effective(
        contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", all_states(), [])], []),
    )
  assert diagnostics
    == [
      linear_contract.MissingLabel(
        team_key: "ENG",
        name: "workflow:bugfix",
        source: "linear_contract.workflow_labels",
      ),
    ]
}

pub fn invalid_workflow_state_id_diagnostics_test() {
  let missing_contract =
    domain.LinearContractConfig(
      ..contract_config(False),
      invalid_workflow_state_id: Some("state-needs-workflow"),
    )
  let diagnostics =
    linear_contract.check(
      effective(
        missing_contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", all_states(), [])], []),
    )
  assert diagnostics
    == [linear_contract.MissingInvalidWorkflowStateId("state-needs-workflow")]

  let matching_contract =
    domain.LinearContractConfig(
      ..missing_contract,
      required_states: dict.from_list([#("needs_workflow", "Needs Workflow")]),
    )
  let matching_diagnostics =
    linear_contract.check(
      effective(
        matching_contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team(
            "ENG",
            list.append(all_states(), [
              state("state-needs-workflow", "Needs Workflow"),
            ]),
            [],
          ),
        ],
        [],
      ),
    )
  assert matching_diagnostics == []

  let mismatch_diagnostics =
    linear_contract.check(
      effective(
        matching_contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", [state("state-needs-workflow", "Backlog")], [])], []),
    )
  assert mismatch_diagnostics
    == [
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Ready for Agent",
        source: "tracker.active_states",
      ),
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Done",
        source: "tracker.terminal_states",
      ),
      linear_contract.InvalidWorkflowStateNameMismatch(
        id: "state-needs-workflow",
        expected: "Needs Workflow",
        actual: "Backlog",
        actual_team_key: "ENG",
      ),
    ]
}

pub fn multi_team_invalid_workflow_state_fails_closed_test() {
  let contract =
    domain.LinearContractConfig(
      ..contract_config(False),
      invalid_workflow_state_id: Some("state-needs-workflow"),
    )
  let diagnostics =
    linear_contract.check(
      effective(
        contract,
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", all_states(), []), team("OPS", all_states(), [])], []),
    )
  assert diagnostics
    == [
      linear_contract.MultiTeamInvalidWorkflowStateUnsupported(
        id: "state-needs-workflow",
        team_keys: ["ENG", "OPS"],
      ),
    ]
}

pub fn disabled_handoff_skips_stale_state_ids_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(False),
        handoff_config(False, Some("stale"), Some("stale-success"), None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", all_states(), [])], []),
    )
  assert diagnostics == []
}

pub fn reports_missing_handoff_state_id_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(False),
        handoff_config(True, Some("state-claim"), None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board([team("ENG", all_states(), [])], []),
    )
  assert diagnostics
    == [
      linear_contract.MissingHandoffStateId(field: "claim", id: "state-claim"),
    ]
}

pub fn reports_handoff_state_name_mismatch_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(True),
        handoff_config(True, None, Some("state-done"), None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team(
            "ENG",
            [
              state("state-ready", "Ready for Agent"),
              state("state-progress", "In Progress"),
              state("state-done", "Closed"),
            ],
            all_labels(),
          ),
        ],
        [],
      ),
    )
  assert diagnostics
    == [
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Done",
        source: "tracker.terminal_states",
      ),
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Done",
        source: "linear_contract.required_states.done",
      ),
      linear_contract.HandoffStateNameMismatch(
        field: "success",
        id: "state-done",
        expected: "Done",
        actual: "Closed",
        actual_team_key: "ENG",
      ),
    ]
  assert linear_contract.diagnostic_code(
      list.last(diagnostics) |> option_unwrap,
    )
    == "handoff_state_name_mismatch"
}

pub fn multi_team_handoff_suppresses_secondary_id_diagnostics_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(False),
        handoff_config(True, Some("missing-state"), None, None),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team("ENG", all_states(), []),
          team("OPS", all_states(), []),
        ],
        [],
      ),
    )
  assert diagnostics
    == [
      linear_contract.MultiTeamHandoffStateUnsupported(
        field: "claim",
        id: "missing-state",
        team_keys: ["ENG", "OPS"],
      ),
    ]
  assert !contains_code(diagnostics, "missing_handoff_state_id")
  assert !contains_code(diagnostics, "handoff_state_name_mismatch")
}

fn contains_code(
  diagnostics: List(linear_contract.ContractDiagnostic),
  code: String,
) -> Bool {
  list.any(diagnostics, fn(diagnostic) {
    linear_contract.diagnostic_code(diagnostic) == code
  })
}

fn string_contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}

fn option_unwrap(value: Result(a, Nil)) -> a {
  let assert Ok(value) = value
  value
}
