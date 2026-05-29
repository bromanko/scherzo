import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/hash
import scherzo/workflow_contract
import scherzo/workflow_identity
import scherzo/workstream/types
import yay

pub const playbook_version = 1

pub type Playbook {
  Playbook(
    id: String,
    display_name: String,
    auto_enqueue: AutoEnqueueConfig,
    phases: List(PlaybookPhase),
    next_actions: List(PlaybookAction),
  )
}

pub type AutoEnqueueConfig {
  AutoEnqueueConfig(enabled: Bool, max_actions_per_evaluation: Int)
}

pub type PlaybookPhase {
  PlaybookPhase(
    phase_id: String,
    display_name: String,
    workflow_id: String,
    required_inputs: List(ArtifactRequirement),
    expected_outputs: List(ArtifactRequirement),
    gates: List(String),
  )
}

pub type ArtifactRequirement {
  ArtifactRequirement(
    name: String,
    contract_type: String,
    artifact_type: Option(String),
  )
}

pub type PlaybookAction {
  PlaybookAction(
    action_id: String,
    label: String,
    from_phase: Option(String),
    to_phase: String,
    workflow_id: String,
    reason: String,
    required_inputs: List(String),
    expected_outputs: List(String),
    requires_gate: Option(String),
    priority: Int,
    auto_enqueue: Bool,
  )
}

pub type EvaluationPolicy {
  EvaluationPolicy(auto_enqueue_enabled: Bool, workstream_held: Bool)
}

pub type Evaluation {
  Evaluation(
    playbook_id: String,
    workstream_id: String,
    recommendations: List(Recommendation),
    warnings: List(EvaluationWarning),
  )
}

pub type Recommendation {
  Recommendation(
    action_id: String,
    label: String,
    workflow_id: String,
    state: String,
    priority: Int,
    reason: String,
    required_inputs: List(String),
    missing_inputs: List(String),
    input_hashes: List(#(String, String)),
    requires_gate: Option(String),
    gate_status: String,
    gate_decision_id: Option(String),
    auto_enqueue: Bool,
    auto_enqueue_status: String,
    idempotency_key: Option(String),
    blocked_reasons: List(String),
    duplicate_phase_run_id: Option(String),
    skipped_by_decision_id: Option(String),
  )
}

pub type EvaluationWarning {
  EvaluationWarning(code: String, ref: String, message: String)
}

pub type PlaybookError {
  PlaybookError(code: String, message: String)
}

pub fn suggest_only_policy() -> EvaluationPolicy {
  EvaluationPolicy(auto_enqueue_enabled: False, workstream_held: False)
}

pub fn auto_enqueue_policy(
  workstream_held workstream_held: Bool,
) -> EvaluationPolicy {
  EvaluationPolicy(auto_enqueue_enabled: True, workstream_held: workstream_held)
}

pub fn parse(contents: String) -> Result(Playbook, PlaybookError) {
  case yay.parse_string(contents) {
    Error(parse_error) ->
      error("playbook_yaml_parse_error", yaml_error_message(parse_error))
    Ok([document]) -> parse_root(yay.document_root(document))
    Ok(_) -> error("playbook_multiple_documents", "expected one YAML document")
  }
}

pub fn parse_root(root: yay.Node) -> Result(Playbook, PlaybookError) {
  case root {
    yay.NodeMap(_) -> {
      use version <- result.try(required_int(
        root,
        "version",
        "playbook_version_missing",
      ))
      use Nil <- result.try(case version == playbook_version {
        True -> Ok(Nil)
        False -> error("playbook_version_invalid", "playbook version must be 1")
      })
      use id <- result.try(required_string(root, "id", "playbook_id_missing"))
      use Nil <- result.try(validate_name(id, "playbook_id_invalid", "id"))
      use display_name_value <- result.try(optional_string(
        root,
        "display_name",
        "playbook_display_name_invalid",
      ))
      let display_name = option_default(display_name_value, id)
      use auto_enqueue <- result.try(parse_auto_enqueue(root))
      use phases <- result.try(parse_phases(root))
      use next_actions <- result.try(parse_actions(root))
      let playbook =
        Playbook(
          id: id,
          display_name: display_name,
          auto_enqueue: auto_enqueue,
          phases: phases,
          next_actions: next_actions,
        )
      validate_references(playbook)
    }
    _ -> error("playbook_root_not_map", "playbook must be a map")
  }
}

pub fn recommendation_to_next_action_artifact(
  workstream_id: String,
  recommendation: Recommendation,
) -> types.NextActionArtifact {
  types.NextActionArtifact(
    artifact_id: recommendation_artifact_id(recommendation),
    workstream_id: workstream_id,
    action_id: recommendation.action_id,
    workflow_id: recommendation.workflow_id,
    state: artifact_state(recommendation.state),
    priority: recommendation.priority,
    inputs: recommendation.required_inputs,
    requires_gate: recommendation.requires_gate,
    auto_enqueue: recommendation.auto_enqueue,
  )
}

pub fn error_code(error: PlaybookError) -> String {
  let PlaybookError(code, _) = error
  code
}

pub fn error_message(error: PlaybookError) -> String {
  let PlaybookError(_, message) = error
  message
}

fn parse_auto_enqueue(
  root: yay.Node,
) -> Result(AutoEnqueueConfig, PlaybookError) {
  case get_node(root, "auto_enqueue") {
    None -> Ok(AutoEnqueueConfig(enabled: False, max_actions_per_evaluation: 1))
    Some(yay.NodeMap(_) as node) -> {
      use enabled_value <- result.try(optional_bool(
        node,
        "enabled",
        "playbook_auto_enqueue_enabled_invalid",
      ))
      let enabled = option_default(enabled_value, False)
      use max_value <- result.try(optional_int(
        node,
        "max_actions_per_evaluation",
        "playbook_auto_enqueue_max_invalid",
      ))
      let max_actions = option_default(max_value, 1)
      use Nil <- result.try(case max_actions > 0 {
        True -> Ok(Nil)
        False ->
          error(
            "playbook_auto_enqueue_max_invalid",
            "max_actions_per_evaluation must be positive",
          )
      })
      Ok(AutoEnqueueConfig(
        enabled: enabled,
        max_actions_per_evaluation: max_actions,
      ))
    }
    Some(_) ->
      error("playbook_auto_enqueue_not_map", "auto_enqueue must be a map")
  }
}

fn parse_phases(root: yay.Node) -> Result(List(PlaybookPhase), PlaybookError) {
  case get_node(root, "phases") {
    None -> error("playbook_phases_missing", "phases is required")
    Some(yay.NodeSeq(values)) -> {
      use phases <- result.try(parse_phase_list(values, []))
      case phases {
        [] -> error("playbook_phases_empty", "phases must not be empty")
        _ -> Ok(phases)
      }
    }
    Some(_) -> error("playbook_phases_not_list", "phases must be a list")
  }
}

fn parse_phase_list(
  values: List(yay.Node),
  acc: List(PlaybookPhase),
) -> Result(List(PlaybookPhase), PlaybookError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeMap(_) as phase, ..rest] -> {
      use parsed <- result.try(parse_phase(phase))
      parse_phase_list(rest, [parsed, ..acc])
    }
    [_, ..] -> error("playbook_phase_not_map", "phase entries must be maps")
  }
}

fn parse_phase(node: yay.Node) -> Result(PlaybookPhase, PlaybookError) {
  use phase_id <- result.try(required_string(
    node,
    "phase_id",
    "playbook_phase_id_missing",
  ))
  use Nil <- result.try(validate_name(
    phase_id,
    "playbook_phase_id_invalid",
    "phase_id",
  ))
  use display_name_value <- result.try(optional_string(
    node,
    "display_name",
    "playbook_phase_display_name_invalid",
  ))
  let display_name = option_default(display_name_value, phase_id)
  use workflow_id <- result.try(required_string(
    node,
    "workflow_id",
    "playbook_phase_workflow_id_missing",
  ))
  use required_inputs <- result.try(parse_requirements(
    node,
    "required_inputs",
    "playbook_phase_required_inputs_not_list",
    "playbook_phase_required_input_not_map",
  ))
  use expected_outputs <- result.try(parse_requirements(
    node,
    "expected_outputs",
    "playbook_phase_expected_outputs_not_list",
    "playbook_phase_expected_output_not_map",
  ))
  use gates <- result.try(parse_name_list(
    node,
    "gates",
    "playbook_phase_gates_not_list",
    "playbook_phase_gate_not_string",
    "playbook_phase_gate_invalid",
  ))
  Ok(PlaybookPhase(
    phase_id: phase_id,
    display_name: display_name,
    workflow_id: workflow_id,
    required_inputs: required_inputs,
    expected_outputs: expected_outputs,
    gates: gates,
  ))
}

fn parse_requirements(
  node: yay.Node,
  key: String,
  list_code: String,
  entry_code: String,
) -> Result(List(ArtifactRequirement), PlaybookError) {
  case get_node(node, key) {
    None -> Ok([])
    Some(yay.NodeSeq(values)) -> parse_requirement_list(values, entry_code, [])
    Some(_) -> error(list_code, key <> " must be a list")
  }
}

fn parse_requirement_list(
  values: List(yay.Node),
  entry_code: String,
  acc: List(ArtifactRequirement),
) -> Result(List(ArtifactRequirement), PlaybookError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeMap(_) as requirement, ..rest] -> {
      use parsed <- result.try(parse_requirement(requirement))
      parse_requirement_list(rest, entry_code, [parsed, ..acc])
    }
    [_, ..] -> error(entry_code, "artifact requirements must be maps")
  }
}

fn parse_requirement(
  node: yay.Node,
) -> Result(ArtifactRequirement, PlaybookError) {
  use name <- result.try(required_string(
    node,
    "name",
    "playbook_artifact_name_missing",
  ))
  use Nil <- result.try(validate_name(
    name,
    "playbook_artifact_name_invalid",
    "artifact name",
  ))
  use contract_type <- result.try(required_string(
    node,
    "contract_type",
    "playbook_contract_type_missing",
  ))
  use Nil <- result.try(validate_contract_type(contract_type))
  use artifact_type <- result.try(optional_string(
    node,
    "artifact_type",
    "playbook_artifact_type_invalid",
  ))
  Ok(ArtifactRequirement(
    name: name,
    contract_type: contract_type,
    artifact_type: artifact_type,
  ))
}

fn parse_actions(
  root: yay.Node,
) -> Result(List(PlaybookAction), PlaybookError) {
  case get_node(root, "next_actions") {
    None -> error("playbook_next_actions_missing", "next_actions is required")
    Some(yay.NodeSeq(values)) -> parse_action_list(values, [])
    Some(_) ->
      error("playbook_next_actions_not_list", "next_actions must be a list")
  }
}

fn parse_action_list(
  values: List(yay.Node),
  acc: List(PlaybookAction),
) -> Result(List(PlaybookAction), PlaybookError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeMap(_) as action, ..rest] -> {
      use parsed <- result.try(parse_action(action))
      parse_action_list(rest, [parsed, ..acc])
    }
    [_, ..] ->
      error("playbook_next_action_not_map", "next_actions entries must be maps")
  }
}

fn parse_action(node: yay.Node) -> Result(PlaybookAction, PlaybookError) {
  use action_id <- result.try(required_string(
    node,
    "action_id",
    "playbook_action_id_missing",
  ))
  use Nil <- result.try(validate_name(
    action_id,
    "playbook_action_id_invalid",
    "action_id",
  ))
  use label_value <- result.try(optional_string(
    node,
    "label",
    "playbook_action_label_invalid",
  ))
  let label = option_default(label_value, action_id)
  use from_phase <- result.try(optional_named_string(
    node,
    "from_phase",
    "playbook_action_from_phase_invalid",
  ))
  use to_phase <- result.try(required_string(
    node,
    "to_phase",
    "playbook_action_to_phase_missing",
  ))
  use Nil <- result.try(validate_name(
    to_phase,
    "playbook_action_to_phase_invalid",
    "to_phase",
  ))
  use workflow_id <- result.try(required_string(
    node,
    "workflow_id",
    "playbook_action_workflow_id_missing",
  ))
  use reason <- result.try(required_string(
    node,
    "reason",
    "playbook_action_reason_missing",
  ))
  use required_inputs <- result.try(parse_name_list(
    node,
    "required_inputs",
    "playbook_action_required_inputs_not_list",
    "playbook_action_required_input_not_string",
    "playbook_action_required_input_invalid",
  ))
  use expected_outputs <- result.try(parse_name_list(
    node,
    "expected_outputs",
    "playbook_action_expected_outputs_not_list",
    "playbook_action_expected_output_not_string",
    "playbook_action_expected_output_invalid",
  ))
  use requires_gate <- result.try(optional_named_string(
    node,
    "requires_gate",
    "playbook_action_requires_gate_invalid",
  ))
  use priority_value <- result.try(optional_int(
    node,
    "priority",
    "playbook_action_priority_invalid",
  ))
  let priority = option_default(priority_value, 0)
  use auto_enqueue_value <- result.try(optional_bool(
    node,
    "auto_enqueue",
    "playbook_action_auto_enqueue_invalid",
  ))
  let auto_enqueue = option_default(auto_enqueue_value, False)
  Ok(PlaybookAction(
    action_id: action_id,
    label: label,
    from_phase: from_phase,
    to_phase: to_phase,
    workflow_id: workflow_id,
    reason: reason,
    required_inputs: required_inputs,
    expected_outputs: expected_outputs,
    requires_gate: requires_gate,
    priority: priority,
    auto_enqueue: auto_enqueue,
  ))
}

fn validate_references(playbook: Playbook) -> Result(Playbook, PlaybookError) {
  let phase_ids = list.map(playbook.phases, fn(phase) { phase.phase_id })
  let action_ids =
    list.map(playbook.next_actions, fn(action) { action.action_id })
  let output_names =
    playbook.phases
    |> list.flat_map(fn(phase) {
      list.map(phase.expected_outputs, fn(output) { output.name })
    })
  let gate_names = playbook.phases |> list.flat_map(fn(phase) { phase.gates })
  use Nil <- result.try(validate_unique_names(
    phase_ids,
    "playbook_phase_duplicate",
    "phase_id",
  ))
  use Nil <- result.try(validate_unique_names(
    action_ids,
    "playbook_action_duplicate",
    "action_id",
  ))
  use Nil <- result.try(validate_actions(
    playbook.next_actions,
    phase_ids,
    output_names,
    gate_names,
  ))
  Ok(playbook)
}

fn validate_actions(
  actions: List(PlaybookAction),
  phase_ids: List(String),
  output_names: List(String),
  gate_names: List(String),
) -> Result(Nil, PlaybookError) {
  case actions {
    [] -> Ok(Nil)
    [action, ..rest] -> {
      use Nil <- result.try(validate_action_references(
        action,
        phase_ids,
        output_names,
        gate_names,
      ))
      validate_actions(rest, phase_ids, output_names, gate_names)
    }
  }
}

fn validate_action_references(
  action: PlaybookAction,
  phase_ids: List(String),
  output_names: List(String),
  gate_names: List(String),
) -> Result(Nil, PlaybookError) {
  use Nil <- result.try(validate_optional_phase(action.from_phase, phase_ids))
  use Nil <- result.try(validate_phase(action.to_phase, phase_ids))
  use Nil <- result.try(validate_required_inputs(
    action.required_inputs,
    output_names,
  ))
  validate_optional_gate(action.requires_gate, gate_names)
}

fn validate_optional_phase(
  phase_id: Option(String),
  phase_ids: List(String),
) -> Result(Nil, PlaybookError) {
  case phase_id {
    None -> Ok(Nil)
    Some(phase_id) -> validate_phase(phase_id, phase_ids)
  }
}

fn validate_phase(
  phase_id: String,
  phase_ids: List(String),
) -> Result(Nil, PlaybookError) {
  case list.contains(phase_ids, phase_id) {
    True -> Ok(Nil)
    False ->
      error(
        "playbook_phase_reference_unknown",
        "unknown phase reference: " <> phase_id,
      )
  }
}

fn validate_required_inputs(
  required_inputs: List(String),
  output_names: List(String),
) -> Result(Nil, PlaybookError) {
  case required_inputs {
    [] -> Ok(Nil)
    [name, ..rest] ->
      case list.contains(output_names, name) {
        True -> validate_required_inputs(rest, output_names)
        False ->
          error(
            "playbook_required_input_unknown",
            "required input is not an expected output: " <> name,
          )
      }
  }
}

fn validate_optional_gate(
  gate: Option(String),
  gate_names: List(String),
) -> Result(Nil, PlaybookError) {
  case gate {
    None -> Ok(Nil)
    Some(gate) ->
      case list.contains(gate_names, gate) {
        True -> Ok(Nil)
        False ->
          error(
            "playbook_gate_reference_unknown",
            "unknown gate reference: " <> gate,
          )
      }
  }
}

fn validate_unique_names(
  values: List(String),
  code: String,
  label: String,
) -> Result(Nil, PlaybookError) {
  validate_unique_names_loop(values, [], code, label)
}

fn validate_unique_names_loop(
  values: List(String),
  seen: List(String),
  code: String,
  label: String,
) -> Result(Nil, PlaybookError) {
  case values {
    [] -> Ok(Nil)
    [value, ..rest] ->
      case list.contains(seen, value) {
        True -> error(code, "duplicate " <> label <> ": " <> value)
        False -> validate_unique_names_loop(rest, [value, ..seen], code, label)
      }
  }
}

fn artifact_state(state: String) -> String {
  case state {
    "available" | "queued" -> "available"
    "blocked" | "deviated" -> "blocked"
    _ -> "suggested"
  }
}

fn recommendation_artifact_id(recommendation: Recommendation) -> String {
  let fingerprint =
    recommendation.action_id
    <> "|"
    <> option_default(recommendation.idempotency_key, recommendation.state)
  "playbook-next-action:"
  <> workflow_identity.safe_component(recommendation.action_id, "action")
  <> ":"
  <> hash.short_sha256_hex(fingerprint, 12)
}

fn parse_name_list(
  node: yay.Node,
  key: String,
  not_list_code: String,
  entry_code: String,
  name_code: String,
) -> Result(List(String), PlaybookError) {
  case get_node(node, key) {
    None -> Ok([])
    Some(yay.NodeSeq(values)) ->
      read_name_values(values, entry_code, name_code, [])
    Some(_) -> error(not_list_code, key <> " must be a list")
  }
}

fn read_name_values(
  values: List(yay.Node),
  entry_code: String,
  name_code: String,
  acc: List(String),
) -> Result(List(String), PlaybookError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeStr(value), ..rest] -> {
      use Nil <- result.try(validate_name(value, name_code, "name"))
      read_name_values(rest, entry_code, name_code, [value, ..acc])
    }
    [_, ..] -> error(entry_code, "expected a string entry")
  }
}

fn required_string(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(String, PlaybookError) {
  case get_node(node, key) {
    Some(yay.NodeStr(value)) -> Ok(value)
    None -> error(code, key <> " is required")
    Some(_) -> error(code, key <> " must be a string")
  }
}

fn optional_string(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Option(String), PlaybookError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) -> error(code, key <> " must be a string")
  }
}

fn optional_named_string(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Option(String), PlaybookError) {
  use value <- result.try(optional_string(node, key, code))
  case value {
    None -> Ok(None)
    Some(value) -> {
      use Nil <- result.try(validate_name(value, code, key))
      Ok(Some(value))
    }
  }
}

fn required_int(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Int, PlaybookError) {
  case get_node(node, key) {
    Some(yay.NodeInt(value)) -> Ok(value)
    None -> error(code, key <> " is required")
    Some(_) -> error(code, key <> " must be an integer")
  }
}

fn optional_int(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Option(Int), PlaybookError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeInt(value)) -> Ok(Some(value))
    Some(_) -> error(code, key <> " must be an integer")
  }
}

fn optional_bool(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Option(Bool), PlaybookError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeBool(value)) -> Ok(Some(value))
    Some(_) -> error(code, key <> " must be a boolean")
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}

fn validate_name(
  value: String,
  code: String,
  field: String,
) -> Result(Nil, PlaybookError) {
  case workflow_contract.valid_contract_name(value) {
    True -> Ok(Nil)
    False -> error(code, field <> " must use lower-case name syntax")
  }
}

fn validate_contract_type(value: String) -> Result(Nil, PlaybookError) {
  case workflow_contract.type_from_string(value) {
    Ok(_) -> Ok(Nil)
    Error(workflow_contract.ContractError(_, message)) ->
      error("playbook_contract_type_unknown", message)
  }
}

fn option_default(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn yaml_error_message(error: yay.YamlError) -> String {
  case error {
    yay.UnexpectedParsingError -> "YAML parse error"
    yay.ParsingError(message, yay.YamlErrorLoc(line, column)) ->
      message
      <> " at line "
      <> int.to_string(line)
      <> ", column "
      <> int.to_string(column)
  }
}

fn error(code: String, message: String) -> Result(a, PlaybookError) {
  Error(PlaybookError(code, message))
}
