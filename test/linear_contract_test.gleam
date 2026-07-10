import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/linear_contract
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy

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
) -> config_types.HandoffConfig {
  config_types.HandoffConfig(
    enabled: enabled,
    comment_on_claim: enabled,
    comment_on_success: enabled,
    comment_on_failure: enabled,
    comment_on_park: enabled,
    claim_state_id: option.map(claim, workflow_completion_policy.StateById),
    success_state_id: option.map(success, workflow_completion_policy.StateById),
    failure_state_id: option.map(failure, workflow_completion_policy.StateById),
    include_result_on_success: enabled,
    attach_result_on_success: False,
    attachment_fallback_to_markdown_link: True,
    result_max_chars: 8000,
    completion_states: None,
  )
}

fn named_handoff_config(
  enabled: Bool,
  claim: Option(String),
  success: Option(String),
  failure: Option(String),
) -> config_types.HandoffConfig {
  config_types.HandoffConfig(
    ..handoff_config(enabled, None, None, None),
    claim_state_id: option.map(claim, workflow_completion_policy.StateByName),
    success_state_id: option.map(
      success,
      workflow_completion_policy.StateByName,
    ),
    failure_state_id: option.map(
      failure,
      workflow_completion_policy.StateByName,
    ),
  )
}

fn completion_policy() -> workflow_completion_policy.CompletionStatePolicy {
  workflow_completion_policy.CompletionStatePolicy(
    default_completion_state: Some(workflow_completion_policy.StateByName(
      "In Review",
    )),
    no_review_completion_state: Some(workflow_completion_policy.StateByName(
      "Done",
    )),
    failure_state: Some(workflow_completion_policy.StateByName(
      "Needs Attention",
    )),
    partial_success_state: Some(workflow_completion_policy.StateByName(
      "Needs Attention",
    )),
    cancellation_state: None,
    workflows: dict.from_list([
      #(
        "execplan",
        workflow_completion_policy.WorkflowCompletionOverride(
          ..workflow_completion_policy.default_override(),
          success_state: Some(workflow_completion_policy.StateById(
            "state-custom-review",
          )),
        ),
      ),
    ]),
  )
}

fn contract_config(enabled: Bool) -> config_types.LinearContractConfig {
  config_types.LinearContractConfig(
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
    invalid_workflow_state_target: None,
    comment_on_invalid_workflow: False,
  )
}

fn effective(
  contract: config_types.LinearContractConfig,
  handoff: config_types.HandoffConfig,
  active_states: List(String),
  terminal_states: List(String),
) -> config_types.EffectiveConfig {
  effective_with_dispatch(
    contract,
    handoff,
    active_states,
    active_states,
    terminal_states,
  )
}

fn effective_with_dispatch(
  contract: config_types.LinearContractConfig,
  handoff: config_types.HandoffConfig,
  active_states: List(String),
  dispatch_states: List(String),
  terminal_states: List(String),
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      ..config.default_tracker_config(),
      api_key: Some("secret"),
      project_slug: Some("PROJ"),
      task_scope: None,
      active_states: issue_state.list_from_strings(active_states),
      dispatch_states: issue_state.list_from_strings(dispatch_states),
      terminal_states: issue_state.list_from_strings(terminal_states),
    ),
    polling: config.default_polling_config(),
    workspace: config.default_workspace_config("scherzo.yaml"),
    control: config.default_control_config(),
    ledger_compaction: config.default_ledger_compaction_config(),
    hooks: config.default_hooks_config(),
    agent: config.default_agent_config(),
    pi: config.default_pi_config(),
    handoff: handoff,
    linear_contract: contract,
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
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
        source: "tracker.states.active",
      ),
      linear_contract.MissingState(
        team_key: "OPS",
        name: "Ready for Agent",
        source: "tracker.states.ready",
      ),
    ]
  assert string_contains(
    linear_contract.format_report(diagnostics),
    "missing_state",
  )
}

pub fn reports_missing_dispatch_state_source_test() {
  let diagnostics =
    linear_contract.check(
      effective_with_dispatch(
        contract_config(False),
        handoff_config(False, None, None, None),
        ["Ready for Agent"],
        ["Dispatch Only"],
        ["Done"],
      ),
      board([team("ENG", all_states(), [])], []),
    )
  assert diagnostics
    == [
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Dispatch Only",
        source: "tracker.states.ready",
      ),
    ]
}

pub fn reports_missing_required_label_for_project_team_test() {
  let contract =
    config_types.LinearContractConfig(
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
        source: "workflows",
      ),
    ]
}

pub fn workspace_label_is_assignable_to_every_team_test() {
  let contract =
    config_types.LinearContractConfig(
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
    config_types.LinearContractConfig(
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
        source: "workflows",
      ),
    ]
}

pub fn invalid_workflow_state_id_diagnostics_test() {
  let missing_contract =
    config_types.LinearContractConfig(
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
    config_types.LinearContractConfig(
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
        source: "tracker.states.active",
      ),
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Ready for Agent",
        source: "tracker.states.ready",
      ),
      linear_contract.MissingState(
        team_key: "ENG",
        name: "Done",
        source: "tracker.states.terminal",
      ),
      linear_contract.InvalidWorkflowStateNameMismatch(
        id: "state-needs-workflow",
        expected: "Needs Workflow",
        actual: "Backlog",
        actual_team_key: "ENG",
      ),
    ]
}

pub fn invalid_workflow_state_name_diagnostics_test() {
  let contract =
    config_types.LinearContractConfig(
      ..contract_config(False),
      invalid_workflow_state_id: Some("Triage"),
      invalid_workflow_state_target: Some(config_types.InvalidWorkflowStateName(
        " Triage ",
      )),
    )
  let configured =
    effective(
      contract,
      handoff_config(False, None, None, None),
      ["Ready for Agent"],
      ["Done"],
    )

  let matching_diagnostics =
    linear_contract.check(
      configured,
      board(
        [
          team(
            "ENG",
            list.append(all_states(), [state("state-triage", "Triage")]),
            [],
          ),
        ],
        [],
      ),
    )
  assert matching_diagnostics == []

  let missing_diagnostics =
    linear_contract.check(
      configured,
      board([team("ENG", all_states(), [])], []),
    )
  assert missing_diagnostics
    == [linear_contract.MissingInvalidWorkflowStateId("Triage")]

  let ambiguous_diagnostics =
    linear_contract.check(
      configured,
      board(
        [
          team(
            "ENG",
            list.append(all_states(), [
              state("state-triage-a", "Triage"),
              state("state-triage-b", "Triage"),
            ]),
            [],
          ),
        ],
        [],
      ),
    )
  assert ambiguous_diagnostics
    == [linear_contract.MissingInvalidWorkflowStateId("Triage")]
}

pub fn multi_team_invalid_workflow_state_fails_closed_test() {
  let contract =
    config_types.LinearContractConfig(
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
        source: "tracker.states.terminal",
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

pub fn task_update_state_names_are_checked_test() {
  let diagnostics =
    linear_contract.check(
      effective(
        contract_config(False),
        named_handoff_config(
          True,
          Some("Missing Claim"),
          Some("In Review"),
          Some("Needs Attention"),
        ),
        ["Ready for Agent"],
        ["Done"],
      ),
      board(
        [
          team(
            "ENG",
            list.append(all_states(), [
              state("review-a", "In Review"),
              state("review-b", "In Review"),
            ]),
            [],
          ),
        ],
        [],
      ),
    )

  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingState(
      team_key: "ENG",
      name: "Missing Claim",
      source: "task_updates.states.claim",
    )
  })
  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingState(
      team_key: "ENG",
      name: "Needs Attention",
      source: "task_updates.states.failure",
    )
  })
  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.AmbiguousCompletionStateName(
      team_key: "ENG",
      name: "In Review",
      source: "task_updates.states.success",
    )
  })
}

pub fn completion_state_policy_names_are_checked_test() {
  let handoff =
    config_types.HandoffConfig(
      ..handoff_config(True, None, None, None),
      completion_states: Some(completion_policy()),
    )
  let diagnostics =
    linear_contract.check(
      effective(contract_config(False), handoff, ["Ready for Agent"], ["Done"]),
      board([team("ENG", all_states(), [])], []),
    )

  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingState(
      team_key: "ENG",
      name: "In Review",
      source: "task_updates.states.success",
    )
  })
  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingState(
      team_key: "ENG",
      name: "Needs Attention",
      source: "task_updates.states.failure",
    )
  })
  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingCompletionStateId(
      source: "task_updates.workflows.execplan.states.success",
      id: "state-custom-review",
    )
  })
}

pub fn completion_state_policy_workflow_override_names_are_checked_test() {
  let policy =
    workflow_completion_policy.CompletionStatePolicy(
      ..completion_policy(),
      workflows: dict.from_list([
        #(
          "merge-conflict-resolution",
          workflow_completion_policy.WorkflowCompletionOverride(
            ..workflow_completion_policy.default_override(),
            no_review_completion_state: Some(
              workflow_completion_policy.StateByName("Conflict Resolved"),
            ),
          ),
        ),
      ]),
    )
  let handoff =
    config_types.HandoffConfig(
      ..handoff_config(True, None, None, None),
      completion_states: Some(policy),
    )
  let diagnostics =
    linear_contract.check(
      effective(contract_config(False), handoff, ["Ready for Agent"], ["Done"]),
      board([team("ENG", all_states(), [])], []),
    )

  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingState(
      team_key: "ENG",
      name: "Conflict Resolved",
      source: "task_updates.workflows.merge-conflict-resolution.states.no_review_success",
    )
  })
}

pub fn completion_state_policy_ids_are_checked_test() {
  let id_policy =
    workflow_completion_policy.CompletionStatePolicy(
      ..completion_policy(),
      default_completion_state: Some(workflow_completion_policy.StateById(
        "state-review",
      )),
      failure_state: Some(workflow_completion_policy.StateById(
        "state-attention",
      )),
      partial_success_state: Some(workflow_completion_policy.StateById(
        "missing-attention",
      )),
      workflows: dict.new(),
    )
  let handoff =
    config_types.HandoffConfig(
      ..handoff_config(True, None, None, None),
      completion_states: Some(id_policy),
    )
  let diagnostics =
    linear_contract.check(
      effective(contract_config(False), handoff, ["Ready for Agent"], ["Done"]),
      board(
        [
          team(
            "ENG",
            list.append(all_states(), [
              state("state-review", "In Review"),
              state("state-attention", "Needs Attention"),
            ]),
            [],
          ),
        ],
        [],
      ),
    )

  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.MissingCompletionStateId(
      source: "task_updates.states.partial_success",
      id: "missing-attention",
    )
  })
}

pub fn completion_state_policy_ambiguous_names_fail_doctor_test() {
  let handoff =
    config_types.HandoffConfig(
      ..handoff_config(True, None, None, None),
      completion_states: Some(
        workflow_completion_policy.CompletionStatePolicy(
          ..completion_policy(),
          workflows: dict.new(),
        ),
      ),
    )
  let diagnostics =
    linear_contract.check(
      effective(contract_config(False), handoff, ["Ready for Agent"], ["Done"]),
      board(
        [
          team(
            "ENG",
            list.append(all_states(), [
              state("state-review-a", "In Review"),
              state("state-review-b", "In Review"),
              state("state-attention", "Needs Attention"),
            ]),
            [],
          ),
        ],
        [],
      ),
    )

  assert list.any(diagnostics, fn(diagnostic) {
    diagnostic
    == linear_contract.AmbiguousCompletionStateName(
      team_key: "ENG",
      name: "In Review",
      source: "task_updates.states.success",
    )
  })
  assert string.contains(
    linear_contract.format_report(diagnostics),
    "docs/runbooks/linear-completion-states.md",
  )
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
