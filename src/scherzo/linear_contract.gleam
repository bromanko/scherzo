import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/string
import scherzo/config/linear_task_scope
import scherzo/config/types as config_types
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy

pub fn task_scope_issue_filter_variables(
  scope: config_types.LinearTaskScope,
  name: String,
) -> List(#(String, json.Json)) {
  linear_task_scope.issue_filter_variables(scope, name)
}

pub fn task_scope_issue_filter_declaration(name: String) -> String {
  linear_task_scope.issue_filter_declaration(name)
}

pub fn task_scope_project_filter_variables(
  scope: config_types.LinearTaskScope,
  name: String,
) -> List(#(String, json.Json)) {
  linear_task_scope.project_filter_variables(scope, name)
}

pub fn task_scope_project_filter_declaration(name: String) -> String {
  linear_task_scope.project_filter_declaration(name)
}

pub fn task_scope_configured_project_slug_variables(
  scope: config_types.LinearTaskScope,
  name: String,
) -> List(#(String, json.Json)) {
  linear_task_scope.configured_project_slug_variables(scope, name)
}

pub fn task_scope_graphql_variables(
  scope: config_types.LinearTaskScope,
  single_name: String,
  multi_name: String,
) -> List(#(String, json.Json)) {
  linear_task_scope.graphql_variables(scope, single_name, multi_name)
}

pub fn task_scope_variable_declaration(
  scope: config_types.LinearTaskScope,
  single_name: String,
  multi_name: String,
) -> String {
  linear_task_scope.variable_declaration(scope, single_name, multi_name)
}

pub fn task_scope_contract_project_first(
  scope: config_types.LinearTaskScope,
) -> String {
  linear_task_scope.contract_project_first(scope)
}

pub fn task_scope_contract_configured_project_first(
  scope: config_types.LinearTaskScope,
) -> String {
  linear_task_scope.contract_configured_project_first(scope)
}

pub fn task_scope_project_slugs(
  scope: config_types.LinearTaskScope,
) -> List(String) {
  linear_task_scope.project_slugs(scope)
}

pub fn task_scope_matching_project_slugs(
  scope: config_types.LinearTaskScope,
) -> List(String) {
  linear_task_scope.matching_project_slugs(scope)
}

pub fn task_scope_matches_project_slug(
  scope: config_types.LinearTaskScope,
  returned_slug: String,
) -> Bool {
  linear_task_scope.matches_project_slug(scope, returned_slug)
}

pub type RemoteBoard {
  RemoteBoard(
    project_id: String,
    project_slug: String,
    project_name: String,
    teams: List(RemoteTeam),
    workspace_labels: List(RemoteLabel),
  )
}

pub type RemoteTeam {
  RemoteTeam(
    id: String,
    key: String,
    name: String,
    states: List(RemoteState),
    labels: List(RemoteLabel),
  )
}

pub type RemoteState {
  RemoteState(id: String, name: String, type_: String)
}

pub type RemoteLabel {
  RemoteLabel(id: String, name: String)
}

pub type ContractDiagnostic {
  MissingState(team_key: String, name: String, source: String)
  MissingLabel(team_key: String, name: String, source: String)
  MissingHandoffStateId(field: String, id: String)
  MultiTeamHandoffStateUnsupported(
    field: String,
    id: String,
    team_keys: List(String),
  )
  HandoffStateNameMismatch(
    field: String,
    id: String,
    expected: String,
    actual: String,
    actual_team_key: String,
  )
  MissingInvalidWorkflowStateId(id: String)
  MultiTeamInvalidWorkflowStateUnsupported(id: String, team_keys: List(String))
  InvalidWorkflowStateNameMismatch(
    id: String,
    expected: String,
    actual: String,
    actual_team_key: String,
  )
  MissingCompletionStateId(source: String, id: String)
  AmbiguousCompletionStateName(team_key: String, name: String, source: String)
}

type StateRequirement {
  StateRequirement(name: String, source: String)
}

type LabelRequirement {
  LabelRequirement(name: String, source: String)
}

pub fn check(
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  []
  |> append_state_diagnostics(state_requirements(effective), remote.teams)
  |> append_label_diagnostics(label_requirements(effective), remote)
  |> append_handoff_diagnostics(effective, remote)
  |> append_completion_state_policy_diagnostics(effective, remote)
  |> append_invalid_workflow_state_diagnostics(effective, remote)
  |> list.reverse
}

pub fn is_ok(diagnostics: List(ContractDiagnostic)) -> Bool {
  list.is_empty(diagnostics)
}

pub fn diagnostic_code(diagnostic: ContractDiagnostic) -> String {
  case diagnostic {
    MissingState(_, _, _) -> "missing_state"
    MissingLabel(_, _, _) -> "missing_label"
    MissingHandoffStateId(_, _) -> "missing_handoff_state_id"
    MultiTeamHandoffStateUnsupported(_, _, _) ->
      "multi_team_handoff_state_unsupported"
    HandoffStateNameMismatch(_, _, _, _, _) -> "handoff_state_name_mismatch"
    MissingInvalidWorkflowStateId(_) -> "missing_invalid_workflow_state_id"
    MultiTeamInvalidWorkflowStateUnsupported(_, _) ->
      "multi_team_invalid_workflow_state_unsupported"
    InvalidWorkflowStateNameMismatch(_, _, _, _) ->
      "invalid_workflow_state_name_mismatch"
    MissingCompletionStateId(_, _) -> "missing_completion_state_id"
    AmbiguousCompletionStateName(_, _, _) -> "ambiguous_completion_state_name"
  }
}

pub fn diagnostic_message(diagnostic: ContractDiagnostic) -> String {
  format_diagnostic(diagnostic)
}

pub fn format_report(diagnostics: List(ContractDiagnostic)) -> String {
  case diagnostics {
    [] -> ""
    _ -> string.join(list.map(diagnostics, format_diagnostic), with: "\n")
  }
}

fn state_requirements(
  effective: config_types.EffectiveConfig,
) -> List(StateRequirement) {
  let tracker_states =
    [
      state_requirements_from_list(
        effective.tracker.active_states,
        "tracker.states.active",
      ),
      state_requirements_from_list(
        effective.tracker.dispatch_states,
        "tracker.states.ready",
      ),
      state_requirements_from_list(
        effective.tracker.terminal_states,
        "tracker.states.terminal",
      ),
    ]
    |> list.flatten
  case effective.linear_contract.enabled {
    False -> tracker_states
    True -> list.append(tracker_states, contract_required_states(effective))
  }
}

fn state_requirements_from_list(
  names: List(issue_state.IssueState),
  source: String,
) -> List(StateRequirement) {
  names
  |> list.map(fn(name) {
    StateRequirement(name: issue_state.to_string(name) |> string.trim, source:)
  })
  |> list.filter(fn(req) { req.name != "" })
}

fn contract_required_states(
  effective: config_types.EffectiveConfig,
) -> List(StateRequirement) {
  effective.linear_contract.required_states
  |> dict.to_list
  |> list.sort(by: compare_string_pairs)
  |> list.map(fn(entry) {
    let #(key, name) = entry
    StateRequirement(
      name: string.trim(name),
      source: "linear_contract.required_states." <> key,
    )
  })
  |> list.filter(fn(req) { req.name != "" })
}

fn label_requirements(
  effective: config_types.EffectiveConfig,
) -> List(LabelRequirement) {
  let workflow_requirements = case
    effective.linear_contract.enabled
    || effective.linear_contract.enforce_issue_workflow_labels
  {
    True -> workflow_label_requirements(effective.linear_contract)
    False -> []
  }
  let support_requirements = case effective.linear_contract.enabled {
    True -> support_label_requirements(effective.linear_contract)
    False -> []
  }
  list.append(workflow_requirements, support_requirements)
}

fn workflow_label_requirements(
  contract: config_types.LinearContractConfig,
) -> List(LabelRequirement) {
  contract.workflow_labels
  |> list.map(fn(suffix) {
    LabelRequirement(
      name: normalize_label(contract.workflow_label_prefix <> suffix),
      source: "workflows",
    )
  })
  |> list.filter(fn(req) { req.name != "" })
}

fn support_label_requirements(
  contract: config_types.LinearContractConfig,
) -> List(LabelRequirement) {
  contract.support_labels
  |> list.map(fn(name) {
    LabelRequirement(
      name: normalize_label(name),
      source: "tracker.linear.labels.support",
    )
  })
  |> list.filter(fn(req) { req.name != "" })
}

fn append_state_diagnostics(
  acc: List(ContractDiagnostic),
  requirements: List(StateRequirement),
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case requirements {
    [] -> acc
    [requirement, ..rest] ->
      append_state_diagnostics(
        append_missing_states(acc, requirement, teams),
        rest,
        teams,
      )
  }
}

fn append_missing_states(
  acc: List(ContractDiagnostic),
  requirement: StateRequirement,
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case teams {
    [] -> acc
    [team, ..rest] -> {
      let acc = case team_has_state(team, requirement.name) {
        True -> acc
        False -> [
          MissingState(team.key, requirement.name, requirement.source),
          ..acc
        ]
      }
      append_missing_states(acc, requirement, rest)
    }
  }
}

fn append_label_diagnostics(
  acc: List(ContractDiagnostic),
  requirements: List(LabelRequirement),
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  case requirements {
    [] -> acc
    [requirement, ..rest] ->
      append_label_diagnostics(
        append_missing_labels(
          acc,
          requirement,
          remote.teams,
          remote.workspace_labels,
        ),
        rest,
        remote,
      )
  }
}

fn append_missing_labels(
  acc: List(ContractDiagnostic),
  requirement: LabelRequirement,
  teams: List(RemoteTeam),
  workspace_labels: List(RemoteLabel),
) -> List(ContractDiagnostic) {
  case teams {
    [] -> acc
    [team, ..rest] -> {
      let acc = case
        label_assignable(team, workspace_labels, requirement.name)
      {
        True -> acc
        False -> [
          MissingLabel(team.key, requirement.name, requirement.source),
          ..acc
        ]
      }
      append_missing_labels(acc, requirement, rest, workspace_labels)
    }
  }
}

fn append_handoff_diagnostics(
  acc: List(ContractDiagnostic),
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  case effective.handoff.enabled {
    False -> acc
    True -> {
      let acc =
        append_handoff_field(
          acc,
          "claim",
          effective.handoff.claim_state_id,
          effective,
          remote,
        )
      case effective.handoff.completion_states {
        Some(_) -> acc
        None ->
          acc
          |> append_handoff_field(
            "success",
            effective.handoff.success_state_id,
            effective,
            remote,
          )
          |> append_handoff_field(
            "failure",
            effective.handoff.failure_state_id,
            effective,
            remote,
          )
      }
    }
  }
}

fn append_handoff_field(
  acc: List(ContractDiagnostic),
  field: String,
  maybe_state: Option(workflow_completion_policy.LinearStateRef),
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  case maybe_state {
    None -> acc
    Some(workflow_completion_policy.StateById(id)) ->
      append_handoff_state_id_field(acc, field, id, effective, remote)
    Some(workflow_completion_policy.StateByName(name)) ->
      append_completion_state_name_diagnostics(
        acc,
        "task_updates.states." <> field,
        name,
        remote.teams,
      )
  }
}

fn append_handoff_state_id_field(
  acc: List(ContractDiagnostic),
  field: String,
  id: String,
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  let id = string.trim(id)
  case id == "" {
    True -> acc
    False ->
      case list.length(remote.teams) > 1 {
        True -> [
          MultiTeamHandoffStateUnsupported(field, id, team_keys(remote.teams)),
          ..acc
        ]
        False ->
          append_single_team_handoff_diagnostic(
            acc,
            field,
            id,
            effective,
            remote.teams,
          )
      }
  }
}

fn append_single_team_handoff_diagnostic(
  acc: List(ContractDiagnostic),
  field: String,
  id: String,
  effective: config_types.EffectiveConfig,
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case teams {
    [team] -> {
      case find_state_by_id(team.states, id) {
        None -> [MissingHandoffStateId(field, id), ..acc]
        Some(state) ->
          append_handoff_name_mismatch(acc, field, id, effective, team, state)
      }
    }
    _ -> [MissingHandoffStateId(field, id), ..acc]
  }
}

fn append_handoff_name_mismatch(
  acc: List(ContractDiagnostic),
  field: String,
  id: String,
  effective: config_types.EffectiveConfig,
  team: RemoteTeam,
  state: RemoteState,
) -> List(ContractDiagnostic) {
  case dict.get(effective.linear_contract.handoff_state_bindings, field) {
    Error(_) -> acc
    Ok(required_state_key) ->
      case
        dict.get(effective.linear_contract.required_states, required_state_key)
      {
        Error(_) -> acc
        Ok(expected) -> {
          let expected = string.trim(expected)
          let actual = string.trim(state.name)
          case actual == expected {
            True -> acc
            False -> [
              HandoffStateNameMismatch(field, id, expected, actual, team.key),
              ..acc
            ]
          }
        }
      }
  }
}

fn append_completion_state_policy_diagnostics(
  acc: List(ContractDiagnostic),
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  case effective.handoff.completion_states {
    None -> acc
    Some(policy) ->
      append_completion_state_refs(
        acc,
        completion_state_refs(policy),
        remote.teams,
      )
  }
}

fn completion_state_refs(
  policy: workflow_completion_policy.CompletionStatePolicy,
) -> List(#(String, workflow_completion_policy.LinearStateRef)) {
  let globals =
    []
    |> append_optional_state_ref(
      "task_updates.states.success",
      policy.default_completion_state,
    )
    |> append_optional_state_ref(
      "task_updates.states.no_review_success",
      policy.no_review_completion_state,
    )
    |> append_optional_state_ref(
      "task_updates.states.failure",
      policy.failure_state,
    )
    |> append_optional_state_ref(
      "task_updates.states.partial_success",
      policy.partial_success_state,
    )
    |> append_optional_state_ref(
      "task_updates.states.cancellation",
      policy.cancellation_state,
    )
  policy.workflows
  |> dict.to_list
  |> list.sort(by: compare_workflow_override_pairs)
  |> list.fold(globals, fn(acc, entry) {
    let #(workflow_id, override) = entry
    acc
    |> append_optional_state_ref(
      workflow_task_update_state_ref_source(workflow_id, "success"),
      override.success_state,
    )
    |> append_optional_state_ref(
      workflow_task_update_state_ref_source(workflow_id, "no_review_success"),
      override.no_review_completion_state,
    )
    |> append_optional_state_ref(
      workflow_task_update_state_ref_source(workflow_id, "failure"),
      override.failure_state,
    )
    |> append_optional_state_ref(
      workflow_task_update_state_ref_source(workflow_id, "partial_success"),
      override.partial_success_state,
    )
    |> append_optional_state_ref(
      workflow_task_update_state_ref_source(workflow_id, "cancellation"),
      override.cancellation_state,
    )
  })
}

fn append_optional_state_ref(
  acc: List(#(String, workflow_completion_policy.LinearStateRef)),
  source: String,
  maybe_ref: Option(workflow_completion_policy.LinearStateRef),
) -> List(#(String, workflow_completion_policy.LinearStateRef)) {
  case maybe_ref {
    None -> acc
    Some(ref) -> [#(source, ref), ..acc]
  }
}

fn workflow_task_update_state_ref_source(
  workflow_id: String,
  key: String,
) -> String {
  "task_updates.workflows." <> workflow_id <> ".states." <> key
}

fn compare_workflow_override_pairs(
  a: #(String, workflow_completion_policy.WorkflowCompletionOverride),
  b: #(String, workflow_completion_policy.WorkflowCompletionOverride),
) -> Order {
  let #(a_key, _) = a
  let #(b_key, _) = b
  string.compare(a_key, b_key)
}

fn append_completion_state_refs(
  acc: List(ContractDiagnostic),
  refs: List(#(String, workflow_completion_policy.LinearStateRef)),
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case refs {
    [] -> acc
    [#(source, ref), ..rest] ->
      append_completion_state_refs(
        append_completion_state_ref(acc, source, ref, teams),
        rest,
        teams,
      )
  }
}

fn append_completion_state_ref(
  acc: List(ContractDiagnostic),
  source: String,
  ref: workflow_completion_policy.LinearStateRef,
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case ref {
    workflow_completion_policy.StateById(id) ->
      case any_team_has_state_id(teams, id) {
        True -> acc
        False -> [MissingCompletionStateId(source, id), ..acc]
      }
    workflow_completion_policy.StateByName(name) ->
      append_completion_state_name_diagnostics(acc, source, name, teams)
  }
}

fn append_completion_state_name_diagnostics(
  acc: List(ContractDiagnostic),
  source: String,
  name: String,
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case teams {
    [] -> acc
    [team, ..rest] -> {
      let matches = matching_state_names(team.states, name)
      let acc = case matches {
        [] -> [MissingState(team.key, name, source), ..acc]
        [_] -> acc
        [_, ..] -> [AmbiguousCompletionStateName(team.key, name, source), ..acc]
      }
      append_completion_state_name_diagnostics(acc, source, name, rest)
    }
  }
}

fn any_team_has_state_id(teams: List(RemoteTeam), id: String) -> Bool {
  list.any(teams, fn(team) {
    case find_state_by_id(team.states, id) {
      Some(_) -> True
      None -> False
    }
  })
}

fn matching_state_names(
  states: List(RemoteState),
  name: String,
) -> List(RemoteState) {
  let expected = string.trim(name)
  list.filter(states, fn(state) { string.trim(state.name) == expected })
}

fn append_invalid_workflow_state_diagnostics(
  acc: List(ContractDiagnostic),
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  case
    config_types.normalized_invalid_workflow_state_target(
      effective.linear_contract,
    )
  {
    None -> acc
    Some(config_types.InvalidWorkflowStateId(id)) ->
      append_invalid_workflow_state_id_diagnostics(acc, id, effective, remote)
    Some(config_types.InvalidWorkflowStateName(name)) ->
      append_invalid_workflow_state_name_diagnostics(acc, name, remote.teams)
  }
}

fn append_invalid_workflow_state_id_diagnostics(
  acc: List(ContractDiagnostic),
  id: String,
  effective: config_types.EffectiveConfig,
  remote: RemoteBoard,
) -> List(ContractDiagnostic) {
  case list.length(remote.teams) > 1 {
    True -> [
      MultiTeamInvalidWorkflowStateUnsupported(id, team_keys(remote.teams)),
      ..acc
    ]
    False ->
      append_single_team_invalid_workflow_state_diagnostic(
        acc,
        id,
        effective,
        remote.teams,
      )
  }
}

fn append_invalid_workflow_state_name_diagnostics(
  acc: List(ContractDiagnostic),
  name: String,
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case teams {
    [] -> [MissingInvalidWorkflowStateId(name), ..acc]
    _ ->
      case
        list.all(teams, fn(team) {
          case matching_state_names(team.states, name) {
            [_] -> True
            _ -> False
          }
        })
      {
        True -> acc
        False -> [MissingInvalidWorkflowStateId(name), ..acc]
      }
  }
}

fn append_single_team_invalid_workflow_state_diagnostic(
  acc: List(ContractDiagnostic),
  id: String,
  effective: config_types.EffectiveConfig,
  teams: List(RemoteTeam),
) -> List(ContractDiagnostic) {
  case teams {
    [team] -> {
      case find_state_by_id(team.states, id) {
        None -> [MissingInvalidWorkflowStateId(id), ..acc]
        Some(state) ->
          append_invalid_workflow_state_name_mismatch(
            acc,
            id,
            effective,
            team,
            state,
          )
      }
    }
    _ -> [MissingInvalidWorkflowStateId(id), ..acc]
  }
}

fn append_invalid_workflow_state_name_mismatch(
  acc: List(ContractDiagnostic),
  id: String,
  effective: config_types.EffectiveConfig,
  team: RemoteTeam,
  state: RemoteState,
) -> List(ContractDiagnostic) {
  case dict.get(effective.linear_contract.required_states, "needs_workflow") {
    Error(_) -> acc
    Ok(expected) -> {
      let expected = string.trim(expected)
      let actual = string.trim(state.name)
      case actual == expected {
        True -> acc
        False -> [
          InvalidWorkflowStateNameMismatch(id, expected, actual, team.key),
          ..acc
        ]
      }
    }
  }
}

fn team_has_state(team: RemoteTeam, expected_name: String) -> Bool {
  let expected_name = string.trim(expected_name)
  list.any(team.states, fn(state) { string.trim(state.name) == expected_name })
}

fn label_assignable(
  team: RemoteTeam,
  workspace_labels: List(RemoteLabel),
  expected_name: String,
) -> Bool {
  has_label(team.labels, expected_name)
  || has_label(workspace_labels, expected_name)
}

fn has_label(labels: List(RemoteLabel), expected_name: String) -> Bool {
  let expected_name = normalize_label(expected_name)
  list.any(labels, fn(label) { normalize_label(label.name) == expected_name })
}

fn find_state_by_id(
  states: List(RemoteState),
  id: String,
) -> Option(RemoteState) {
  case states {
    [] -> None
    [state, ..rest] ->
      case state.id == id {
        True -> Some(state)
        False -> find_state_by_id(rest, id)
      }
  }
}

fn team_keys(teams: List(RemoteTeam)) -> List(String) {
  teams
  |> list.map(fn(team) { team.key })
  |> list.sort(by: string.compare)
}

fn compare_string_pairs(a: #(String, String), b: #(String, String)) -> Order {
  let #(a_key, _) = a
  let #(b_key, _) = b
  string.compare(a_key, b_key)
}

fn normalize_label(value: String) -> String {
  value |> string.trim |> string.lowercase
}

fn format_diagnostic(diagnostic: ContractDiagnostic) -> String {
  case diagnostic {
    MissingState(team_key, name, source) ->
      "missing_state team="
      <> team_key
      <> " source="
      <> source
      <> " name="
      <> quote(name)
    MissingLabel(team_key, name, source) ->
      "missing_label team="
      <> team_key
      <> " source="
      <> source
      <> " name="
      <> quote(name)
    MissingHandoffStateId(field, id) ->
      "missing_handoff_state_id field=" <> field <> " id=" <> quote(id)
    MultiTeamHandoffStateUnsupported(field, id, team_keys) ->
      "multi_team_handoff_state_unsupported field="
      <> field
      <> " id="
      <> quote(id)
      <> " teams="
      <> quote(string.join(team_keys, with: ","))
    HandoffStateNameMismatch(field, id, expected, actual, actual_team_key) ->
      "handoff_state_name_mismatch field="
      <> field
      <> " id="
      <> quote(id)
      <> " expected="
      <> quote(expected)
      <> " actual="
      <> quote(actual)
      <> " actual_team="
      <> actual_team_key
    MissingInvalidWorkflowStateId(id) ->
      "missing_invalid_workflow_state_id id=" <> quote(id)
    MultiTeamInvalidWorkflowStateUnsupported(id, team_keys) ->
      "multi_team_invalid_workflow_state_unsupported id="
      <> quote(id)
      <> " teams="
      <> quote(string.join(team_keys, with: ","))
    InvalidWorkflowStateNameMismatch(id, expected, actual, actual_team_key) ->
      "invalid_workflow_state_name_mismatch id="
      <> quote(id)
      <> " expected="
      <> quote(expected)
      <> " actual="
      <> quote(actual)
      <> " actual_team="
      <> actual_team_key
    MissingCompletionStateId(source, id) ->
      "missing_completion_state_id source="
      <> source
      <> " id="
      <> quote(id)
      <> " remediation="
      <> quote(
        "run scherzo doctor --check tracker-contract and see docs/runbooks/linear-completion-states.md",
      )
    AmbiguousCompletionStateName(team_key, name, source) ->
      "ambiguous_completion_state_name team="
      <> team_key
      <> " source="
      <> source
      <> " name="
      <> quote(name)
      <> " remediation="
      <> quote(
        "run scherzo doctor --check tracker-contract and see docs/runbooks/linear-completion-states.md",
      )
  }
}

fn quote(value: String) -> String {
  let escaped =
    value
    |> string.replace(each: "\\", with: "\\\\")
    |> string.replace(each: "\"", with: "\\\"")
    |> string.replace(each: "\n", with: "\\n")
    |> string.replace(each: "\r", with: "\\r")
    |> string.replace(each: "\t", with: "\\t")
  "\"" <> escaped <> "\""
}
