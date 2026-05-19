import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/workflow_contract
import yay

pub type PhaseMetadata {
  PhaseMetadata(
    phase_id: String,
    display_name: String,
    handoff: Option(PhaseHandoff),
    gates: List(String),
    next_actions: List(PhaseNextAction),
    final_phase: Bool,
  )
}

pub type PhaseHandoff {
  PhaseHandoff(
    output: String,
    artifact_type: String,
    snapshot: SnapshotRequirement,
  )
}

pub type SnapshotRequirement {
  SnapshotRequired
}

pub type PhaseNextAction {
  PhaseNextAction(
    action_id: String,
    workflow_id: String,
    inputs: List(String),
    requires_gate: Option(String),
    auto_enqueue: Bool,
  )
}

pub type PhaseMetadataError {
  PhaseMetadataError(code: String, message: String)
}

pub fn parse(
  root: yay.Node,
  contract: Option(workflow_contract.Contract),
) -> Result(Option(PhaseMetadata), PhaseMetadataError) {
  case get_node(root, "workstream_phase") {
    None -> Ok(None)
    Some(node) -> parse_phase_metadata(node, contract) |> result.map(Some)
  }
}

pub fn to_canonical_json(metadata: PhaseMetadata) -> json.Json {
  let fields = [
    #("phase_id", json.string(metadata.phase_id)),
    #("display_name", json.string(metadata.display_name)),
    #("gates", json.array(metadata.gates, of: json.string)),
    #(
      "next_actions",
      json.array(metadata.next_actions, of: next_action_to_json),
    ),
    #("final_phase", json.bool(metadata.final_phase)),
  ]
  let fields = case metadata.handoff {
    Some(handoff) ->
      list.append(fields, [#("handoff", handoff_to_json(handoff))])
    None -> fields
  }
  json.object(fields)
}

pub fn error_code(error: PhaseMetadataError) -> String {
  let PhaseMetadataError(code, _) = error
  code
}

pub fn error_message(error: PhaseMetadataError) -> String {
  let PhaseMetadataError(_, message) = error
  message
}

fn parse_phase_metadata(
  node: yay.Node,
  contract: Option(workflow_contract.Contract),
) -> Result(PhaseMetadata, PhaseMetadataError) {
  case node {
    yay.NodeMap(_) -> {
      use phase_id <- result.try(required_string(
        node,
        "phase_id",
        "workstream_phase_id_missing",
      ))
      use Nil <- result.try(validate_name(
        phase_id,
        "workstream_phase_id_invalid",
        "phase_id",
      ))
      use display_name_value <- result.try(optional_string(
        node,
        "display_name",
        "workstream_phase_display_name_invalid",
      ))
      let display_name = display_name_value |> option_default(phase_id)
      use handoff <- result.try(parse_handoff(node, contract))
      use gates <- result.try(parse_string_list(
        node,
        "gates",
        "workstream_phase_gates_not_list",
        "workstream_phase_gate_not_string",
      ))
      use next_actions <- result.try(parse_next_actions(node, contract))
      use final_phase_value <- result.try(optional_bool(
        node,
        "final_phase",
        "workstream_phase_final_phase_invalid",
      ))
      let final_phase = final_phase_value |> option_default(False)
      Ok(PhaseMetadata(
        phase_id: phase_id,
        display_name: display_name,
        handoff: handoff,
        gates: gates,
        next_actions: next_actions,
        final_phase: final_phase,
      ))
    }
    _ -> error("workstream_phase_not_map", "workstream_phase must be a map")
  }
}

fn parse_handoff(
  node: yay.Node,
  contract: Option(workflow_contract.Contract),
) -> Result(Option(PhaseHandoff), PhaseMetadataError) {
  case get_node(node, "handoff") {
    None -> Ok(None)
    Some(yay.NodeMap(_) as handoff) -> {
      use output <- result.try(required_string(
        handoff,
        "output",
        "workstream_phase_handoff_output_missing",
      ))
      use Nil <- result.try(validate_contract_output(
        contract,
        output,
        "workstream_phase_handoff_unknown_output",
      ))
      use artifact_type <- result.try(required_string(
        handoff,
        "artifact_type",
        "workstream_phase_handoff_artifact_type_missing",
      ))
      use snapshot_text <- result.try(required_string(
        handoff,
        "snapshot",
        "workstream_phase_snapshot_missing",
      ))
      use snapshot <- result.try(parse_snapshot_requirement(snapshot_text))
      Ok(
        Some(PhaseHandoff(
          output: output,
          artifact_type: artifact_type,
          snapshot: snapshot,
        )),
      )
    }
    Some(_) ->
      error(
        "workstream_phase_handoff_not_map",
        "workstream_phase.handoff must be a map",
      )
  }
}

fn parse_snapshot_requirement(
  value: String,
) -> Result(SnapshotRequirement, PhaseMetadataError) {
  case string.lowercase(string.trim(value)) {
    "required" -> Ok(SnapshotRequired)
    other ->
      error(
        "workstream_phase_snapshot_invalid",
        "unsupported handoff snapshot requirement: " <> other,
      )
  }
}

fn parse_next_actions(
  node: yay.Node,
  contract: Option(workflow_contract.Contract),
) -> Result(List(PhaseNextAction), PhaseMetadataError) {
  case get_node(node, "next_actions") {
    None -> Ok([])
    Some(yay.NodeSeq(values)) -> parse_next_action_list(values, contract, [])
    Some(_) ->
      error(
        "workstream_phase_next_actions_not_list",
        "workstream_phase.next_actions must be a list",
      )
  }
}

fn parse_next_action_list(
  values: List(yay.Node),
  contract: Option(workflow_contract.Contract),
  acc: List(PhaseNextAction),
) -> Result(List(PhaseNextAction), PhaseMetadataError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeMap(_) as next_action, ..rest] -> {
      use action_id <- result.try(required_string(
        next_action,
        "action_id",
        "workstream_phase_next_action_id_missing",
      ))
      use Nil <- result.try(validate_name(
        action_id,
        "workstream_phase_next_action_id_invalid",
        "action_id",
      ))
      use workflow_id <- result.try(required_string(
        next_action,
        "workflow_id",
        "workstream_phase_next_action_workflow_id_missing",
      ))
      use inputs <- result.try(parse_string_list(
        next_action,
        "inputs",
        "workstream_phase_next_action_inputs_not_list",
        "workstream_phase_next_action_input_not_string",
      ))
      use Nil <- result.try(validate_contract_outputs(contract, inputs))
      use requires_gate <- result.try(optional_string(
        next_action,
        "requires_gate",
        "workstream_phase_requires_gate_invalid",
      ))
      use auto_enqueue_value <- result.try(optional_bool(
        next_action,
        "auto_enqueue",
        "workstream_phase_auto_enqueue_invalid",
      ))
      let auto_enqueue = auto_enqueue_value |> option_default(False)
      use other <- result.try(
        parse_next_action_list(rest, contract, [
          PhaseNextAction(
            action_id: action_id,
            workflow_id: workflow_id,
            inputs: inputs,
            requires_gate: requires_gate,
            auto_enqueue: auto_enqueue,
          ),
          ..acc
        ]),
      )
      Ok(other)
    }
    [_, ..] ->
      error(
        "workstream_phase_next_action_not_map",
        "workstream_phase.next_actions entries must be maps",
      )
  }
}

fn validate_contract_output(
  contract: Option(workflow_contract.Contract),
  output: String,
  code: String,
) -> Result(Nil, PhaseMetadataError) {
  case output_exists(contract, output) {
    True -> Ok(Nil)
    False -> error(code, "unknown contract output: " <> output)
  }
}

fn validate_contract_outputs(
  contract: Option(workflow_contract.Contract),
  outputs: List(String),
) -> Result(Nil, PhaseMetadataError) {
  case outputs {
    [] -> Ok(Nil)
    [output, ..rest] -> {
      use Nil <- result.try(validate_contract_output(
        contract,
        output,
        "workstream_phase_next_action_unknown_input",
      ))
      validate_contract_outputs(contract, rest)
    }
  }
}

fn output_exists(
  contract: Option(workflow_contract.Contract),
  output: String,
) -> Bool {
  case contract {
    None -> False
    Some(contract) ->
      list.any(contract.outputs, fn(candidate) { candidate.name == output })
  }
}

fn validate_name(
  value: String,
  code: String,
  field: String,
) -> Result(Nil, PhaseMetadataError) {
  case workflow_contract.valid_contract_name(value) {
    True -> Ok(Nil)
    False -> error(code, field <> " must use workflow contract name syntax")
  }
}

fn parse_string_list(
  node: yay.Node,
  key: String,
  not_list_code: String,
  entry_code: String,
) -> Result(List(String), PhaseMetadataError) {
  case get_node(node, key) {
    None -> Ok([])
    Some(yay.NodeSeq(values)) -> read_string_values(values, entry_code, [])
    Some(_) -> error(not_list_code, key <> " must be a list")
  }
}

fn read_string_values(
  values: List(yay.Node),
  entry_code: String,
  acc: List(String),
) -> Result(List(String), PhaseMetadataError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeStr(value), ..rest] ->
      read_string_values(rest, entry_code, [value, ..acc])
    [_, ..] -> error(entry_code, "expected a string entry")
  }
}

fn handoff_to_json(handoff: PhaseHandoff) -> json.Json {
  json.object([
    #("output", json.string(handoff.output)),
    #("artifact_type", json.string(handoff.artifact_type)),
    #("snapshot", json.string(snapshot_requirement_to_string(handoff.snapshot))),
  ])
}

fn next_action_to_json(next_action: PhaseNextAction) -> json.Json {
  let fields = [
    #("action_id", json.string(next_action.action_id)),
    #("workflow_id", json.string(next_action.workflow_id)),
    #("inputs", json.array(next_action.inputs, of: json.string)),
    #("auto_enqueue", json.bool(next_action.auto_enqueue)),
  ]
  let fields = case next_action.requires_gate {
    Some(gate) -> list.append(fields, [#("requires_gate", json.string(gate))])
    None -> fields
  }
  json.object(fields)
}

fn snapshot_requirement_to_string(requirement: SnapshotRequirement) -> String {
  case requirement {
    SnapshotRequired -> "required"
  }
}

fn required_string(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(String, PhaseMetadataError) {
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
) -> Result(Option(String), PhaseMetadataError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) -> error(code, key <> " must be a string")
  }
}

fn optional_bool(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Option(Bool), PhaseMetadataError) {
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

fn option_default(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn error(code: String, message: String) -> Result(a, PhaseMetadataError) {
  Error(PhaseMetadataError(code, message))
}
