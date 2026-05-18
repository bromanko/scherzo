import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/workflow_contract
import scherzo/workstream/artifact_values
import scherzo/workstream/types

pub fn decode_workstream(
  contents: String,
) -> Result(types.WorkstreamArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(
    entries,
    types.workstream_artifact_type,
  ))
  use artifact_id <- result.try(required_string(
    entries,
    "artifact_id",
    "workstream_artifact_id_missing",
  ))
  use workstream_id <- result.try(required_string(
    entries,
    "workstream_id",
    "workstream_workstream_id_missing",
  ))
  use issue_id <- result.try(required_nested_string(
    entries,
    "issue",
    "id",
    "workstream_issue_id_missing",
  ))
  use status <- result.try(required_string(
    entries,
    "status",
    "workstream_status_missing",
  ))
  use summary <- result.try(required_string(
    entries,
    "summary",
    "workstream_summary_missing",
  ))
  use produced_artifacts <- result.try(required_list(
    entries,
    "produced_artifacts",
    "workstream_produced_artifacts_missing",
  ))
  use produced_artifacts <- result.try(decode_snapshots(produced_artifacts))
  use next_actions <- result.try(required_string_list(
    entries,
    "next_actions",
    "workstream_next_actions_missing",
  ))
  Ok(types.WorkstreamArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    issue: types.IssueRef(id: issue_id),
    status: status,
    summary: summary,
    produced_artifacts: produced_artifacts,
    next_actions: next_actions,
  ))
}

pub fn decode_handoff(
  contents: String,
) -> Result(types.HandoffArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(entries, types.handoff_artifact_type))
  use artifact_id <- result.try(required_string(
    entries,
    "artifact_id",
    "workstream_artifact_id_missing",
  ))
  use workstream_id <- result.try(required_string(
    entries,
    "workstream_id",
    "workstream_workstream_id_missing",
  ))
  use phase_id <- result.try(required_string(
    entries,
    "phase_id",
    "workstream_phase_id_missing",
  ))
  use summary <- result.try(required_string(
    entries,
    "summary",
    "workstream_summary_missing",
  ))
  use outputs <- result.try(required_list(
    entries,
    "outputs",
    "workstream_outputs_missing",
  ))
  use outputs <- result.try(decode_handoff_outputs(outputs))
  use next_actions <- result.try(required_string_list(
    entries,
    "recommended_next_actions",
    "workstream_recommended_next_actions_missing",
  ))
  use open_questions <- result.try(required_string_list(
    entries,
    "open_questions",
    "workstream_open_questions_missing",
  ))
  Ok(types.HandoffArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    phase_id: phase_id,
    summary: summary,
    outputs: outputs,
    recommended_next_actions: next_actions,
    open_questions: open_questions,
  ))
}

pub fn decode_decision(
  contents: String,
) -> Result(types.DecisionArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(entries, types.decision_artifact_type))
  use artifact_id <- result.try(required_string(
    entries,
    "artifact_id",
    "workstream_artifact_id_missing",
  ))
  use workstream_id <- result.try(required_string(
    entries,
    "workstream_id",
    "workstream_workstream_id_missing",
  ))
  use kind <- result.try(required_string(
    entries,
    "kind",
    "workstream_decision_kind_missing",
  ))
  use Nil <- result.try(validate_decision_kind(kind))
  use summary <- result.try(required_string(
    entries,
    "summary",
    "workstream_summary_missing",
  ))
  use decided_by <- result.try(required_string(
    entries,
    "decided_by",
    "workstream_decided_by_missing",
  ))
  use rationale <- result.try(required_string(
    entries,
    "rationale",
    "workstream_rationale_missing",
  ))
  Ok(types.DecisionArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    kind: kind,
    summary: summary,
    decided_by: decided_by,
    rationale: rationale,
  ))
}

pub fn decode_input_bundle(
  contents: String,
) -> Result(types.InputBundleArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(
    entries,
    types.input_bundle_artifact_type,
  ))
  use artifact_id <- result.try(required_string(
    entries,
    "artifact_id",
    "workstream_artifact_id_missing",
  ))
  use workstream_id <- result.try(required_string(
    entries,
    "workstream_id",
    "workstream_workstream_id_missing",
  ))
  use source_handoff_ref <- result.try(required_string(
    entries,
    "source_handoff_ref",
    "workstream_snapshot_ref_missing",
  ))
  use Nil <- result.try(validate_snapshot_ref(source_handoff_ref))
  use workflow_id <- result.try(required_string(
    entries,
    "workflow_id",
    "workstream_workflow_id_missing",
  ))
  use inputs <- result.try(required_list(
    entries,
    "inputs",
    "workstream_inputs_missing",
  ))
  use inputs <- result.try(decode_input_bindings(inputs))
  Ok(types.InputBundleArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    source_handoff_ref: source_handoff_ref,
    workflow_id: workflow_id,
    inputs: inputs,
  ))
}

pub fn decode_assignment(
  contents: String,
) -> Result(types.AssignmentArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(
    entries,
    types.assignment_artifact_type,
  ))
  use artifact_id <- result.try(required_string(
    entries,
    "artifact_id",
    "workstream_artifact_id_missing",
  ))
  use workstream_id <- result.try(required_string(
    entries,
    "workstream_id",
    "workstream_workstream_id_missing",
  ))
  use workflow_id <- result.try(required_string(
    entries,
    "workflow_id",
    "workstream_workflow_id_missing",
  ))
  use reason <- result.try(required_string(
    entries,
    "reason",
    "workstream_reason_missing",
  ))
  use playbook_id <- result.try(optional_string_field(
    entries,
    "playbook_id",
    "workstream_playbook_id_invalid",
  ))
  Ok(types.AssignmentArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    workflow_id: workflow_id,
    playbook_id: playbook_id,
    reason: reason,
  ))
}

pub fn decode_next_action(
  contents: String,
) -> Result(types.NextActionArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(
    entries,
    types.next_action_artifact_type,
  ))
  use artifact_id <- result.try(required_string(
    entries,
    "artifact_id",
    "workstream_artifact_id_missing",
  ))
  use workstream_id <- result.try(required_string(
    entries,
    "workstream_id",
    "workstream_workstream_id_missing",
  ))
  use action_id <- result.try(required_string(
    entries,
    "action_id",
    "workstream_action_id_missing",
  ))
  use workflow_id <- result.try(required_string(
    entries,
    "workflow_id",
    "workstream_workflow_id_missing",
  ))
  use state <- result.try(required_string(
    entries,
    "state",
    "workstream_next_action_state_missing",
  ))
  use Nil <- result.try(validate_next_action_state(state))
  use priority <- result.try(required_int(
    entries,
    "priority",
    "workstream_priority_missing",
  ))
  use inputs <- result.try(required_string_list(
    entries,
    "inputs",
    "workstream_inputs_missing",
  ))
  use requires_gate <- result.try(optional_string_field(
    entries,
    "requires_gate",
    "workstream_requires_gate_invalid",
  ))
  use auto_enqueue <- result.try(required_bool(
    entries,
    "auto_enqueue",
    "workstream_auto_enqueue_missing",
  ))
  Ok(types.NextActionArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    action_id: action_id,
    workflow_id: workflow_id,
    state: state,
    priority: priority,
    inputs: inputs,
    requires_gate: requires_gate,
    auto_enqueue: auto_enqueue,
  ))
}

pub fn workstream_to_json(value: types.WorkstreamArtifact) -> json.Json {
  json_value.to_json(artifact_values.workstream_to_value(value))
}

pub fn workstream_to_string(value: types.WorkstreamArtifact) -> String {
  workstream_to_json(value) |> json.to_string
}

pub fn handoff_to_json(value: types.HandoffArtifact) -> json.Json {
  json_value.to_json(artifact_values.handoff_to_value(value))
}

pub fn handoff_to_string(value: types.HandoffArtifact) -> String {
  handoff_to_json(value) |> json.to_string
}

pub fn decision_to_json(value: types.DecisionArtifact) -> json.Json {
  json_value.to_json(artifact_values.decision_to_value(value))
}

pub fn decision_to_string(value: types.DecisionArtifact) -> String {
  decision_to_json(value) |> json.to_string
}

pub fn input_bundle_to_json(value: types.InputBundleArtifact) -> json.Json {
  json_value.to_json(artifact_values.input_bundle_to_value(value))
}

pub fn input_bundle_to_string(value: types.InputBundleArtifact) -> String {
  input_bundle_to_json(value) |> json.to_string
}

pub fn assignment_to_json(value: types.AssignmentArtifact) -> json.Json {
  json_value.to_json(artifact_values.assignment_to_value(value))
}

pub fn assignment_to_string(value: types.AssignmentArtifact) -> String {
  assignment_to_json(value) |> json.to_string
}

pub fn next_action_to_json(value: types.NextActionArtifact) -> json.Json {
  json_value.to_json(artifact_values.next_action_to_value(value))
}

pub fn next_action_to_string(value: types.NextActionArtifact) -> String {
  next_action_to_json(value) |> json.to_string
}

fn validate_headers(
  entries: List(#(String, json_value.JsonValue)),
  expected_type: String,
) -> Result(Nil, types.SpecError) {
  use version <- result.try(required_int(
    entries,
    "schema_version",
    "workstream_schema_version_missing",
  ))
  use Nil <- result.try(case version == types.schema_version {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_schema_version_invalid",
        "schema_version must be 1",
      )
  })
  use artifact_type <- result.try(required_string(
    entries,
    "artifact_type",
    "workstream_artifact_type_missing",
  ))
  case artifact_type == expected_type {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_artifact_type_invalid",
        "artifact_type must be " <> expected_type,
      )
  }
}

fn parse_object(
  contents: String,
) -> Result(List(#(String, json_value.JsonValue)), types.SpecError) {
  case json_value.parse(contents) {
    Ok(json_value.JObject(entries)) -> Ok(entries)
    Ok(_) ->
      spec_error(
        "workstream_json_not_object",
        "artifact JSON must be an object",
      )
    Error(Nil) ->
      spec_error("workstream_json_parse_failed", "artifact JSON parse failed")
  }
}

fn decode_snapshots(
  values: List(json_value.JsonValue),
) -> Result(List(types.ArtifactSnapshot), types.SpecError) {
  decode_snapshots_loop(values, [])
}

fn decode_snapshots_loop(
  values: List(json_value.JsonValue),
  acc: List(types.ArtifactSnapshot),
) -> Result(List(types.ArtifactSnapshot), types.SpecError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] -> {
      use snapshot <- result.try(decode_snapshot(value))
      decode_snapshots_loop(rest, [snapshot, ..acc])
    }
  }
}

fn decode_handoff_outputs(
  values: List(json_value.JsonValue),
) -> Result(List(types.HandoffOutput), types.SpecError) {
  case values {
    [] -> Ok([])
    [json_value.JObject(entries), ..rest] -> {
      use name <- result.try(required_string(
        entries,
        "name",
        "workstream_output_name_missing",
      ))
      use snapshot_value <- result.try(required_value(
        entries,
        "snapshot",
        "workstream_snapshot_missing",
      ))
      use snapshot <- result.try(decode_snapshot(snapshot_value))
      use other <- result.try(decode_handoff_outputs(rest))
      Ok([types.HandoffOutput(name: name, snapshot: snapshot), ..other])
    }
    [_, ..] ->
      spec_error("workstream_output_invalid", "handoff outputs must be objects")
  }
}

fn decode_input_bindings(
  values: List(json_value.JsonValue),
) -> Result(List(types.InputBinding), types.SpecError) {
  case values {
    [] -> Ok([])
    [json_value.JObject(entries), ..rest] -> {
      use name <- result.try(required_string(
        entries,
        "name",
        "workstream_input_name_missing",
      ))
      use contract_type <- result.try(required_string(
        entries,
        "contract_type",
        "workstream_contract_type_missing",
      ))
      use Nil <- result.try(validate_contract_type(contract_type))
      use value_ref <- result.try(required_string(
        entries,
        "value_ref",
        "workstream_snapshot_ref_missing",
      ))
      use Nil <- result.try(validate_snapshot_ref(value_ref))
      use other <- result.try(decode_input_bindings(rest))
      Ok([
        types.InputBinding(
          name: name,
          contract_type: contract_type,
          value_ref: value_ref,
        ),
        ..other
      ])
    }
    [_, ..] ->
      spec_error(
        "workstream_input_invalid",
        "input bundle inputs must be objects",
      )
  }
}

fn decode_snapshot(
  value: json_value.JsonValue,
) -> Result(types.ArtifactSnapshot, types.SpecError) {
  case value {
    json_value.JObject(entries) -> {
      use ref <- result.try(required_string(
        entries,
        "ref",
        "workstream_snapshot_ref_missing",
      ))
      use Nil <- result.try(validate_snapshot_ref(ref))
      use sha256 <- result.try(required_string(
        entries,
        "sha256",
        "workstream_sha256_missing",
      ))
      use Nil <- result.try(validate_sha256(sha256))
      use Nil <- result.try(validate_snapshot_ref_sha256(ref, sha256))
      use bytes <- result.try(required_int(
        entries,
        "bytes",
        "workstream_bytes_missing",
      ))
      use Nil <- result.try(validate_bytes(bytes))
      use media_type <- result.try(required_string(
        entries,
        "media_type",
        "workstream_media_type_missing",
      ))
      use original_path <- result.try(required_string(
        entries,
        "original_path",
        "workstream_original_path_missing",
      ))
      use Nil <- result.try(validate_original_path(original_path))
      use contract_type <- result.try(required_string(
        entries,
        "contract_type",
        "workstream_contract_type_missing",
      ))
      use Nil <- result.try(validate_contract_type(contract_type))
      use producer <- result.try(decode_producer(entries))
      use validation <- result.try(decode_validation(entries))
      use summary <- result.try(required_string(
        entries,
        "summary",
        "workstream_summary_missing",
      ))
      Ok(types.ArtifactSnapshot(
        ref: ref,
        sha256: sha256,
        bytes: bytes,
        media_type: media_type,
        original_path: original_path,
        contract_type: contract_type,
        producer: producer,
        validation: validation,
        summary: summary,
      ))
    }
    _ -> spec_error("workstream_snapshot_invalid", "snapshot must be an object")
  }
}

fn decode_producer(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(types.ProducerRef, types.SpecError) {
  use producer_value <- result.try(required_value(
    entries,
    "producer",
    "workstream_producer_missing",
  ))
  case producer_value {
    json_value.JObject(producer_entries) -> {
      use workflow_id <- result.try(required_string(
        producer_entries,
        "workflow_id",
        "workstream_producer_workflow_id_missing",
      ))
      use run_id <- result.try(required_string(
        producer_entries,
        "run_id",
        "workstream_producer_run_id_missing",
      ))
      use step_id <- result.try(required_string(
        producer_entries,
        "step_id",
        "workstream_producer_step_id_missing",
      ))
      Ok(types.ProducerRef(
        workflow_id: workflow_id,
        run_id: run_id,
        step_id: step_id,
      ))
    }
    _ -> spec_error("workstream_producer_invalid", "producer must be an object")
  }
}

fn decode_validation(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(types.ValidationSummary, types.SpecError) {
  use validation_value <- result.try(required_value(
    entries,
    "validation",
    "workstream_validation_missing",
  ))
  case validation_value {
    json_value.JObject(validation_entries) -> {
      use status <- result.try(required_string(
        validation_entries,
        "status",
        "workstream_validation_status_missing",
      ))
      use validator <- result.try(required_string(
        validation_entries,
        "validator",
        "workstream_validation_validator_missing",
      ))
      use checked_at <- result.try(required_string(
        validation_entries,
        "checked_at",
        "workstream_validation_checked_at_missing",
      ))
      Ok(types.ValidationSummary(
        status: status,
        validator: validator,
        checked_at: checked_at,
      ))
    }
    _ ->
      spec_error(
        "workstream_validation_invalid",
        "validation must be an object",
      )
  }
}

fn required_value(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(json_value.JsonValue, types.SpecError) {
  case lookup(entries, key) {
    Some(value) -> Ok(value)
    None -> spec_error(code, key <> " is required")
  }
}

fn required_string(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(String, types.SpecError) {
  case lookup(entries, key) {
    Some(json_value.JString(value)) -> Ok(value)
    _ -> spec_error(code, key <> " is required")
  }
}

fn required_nested_string(
  entries: List(#(String, json_value.JsonValue)),
  parent: String,
  key: String,
  code: String,
) -> Result(String, types.SpecError) {
  case lookup(entries, parent) {
    Some(json_value.JObject(parent_entries)) ->
      required_string(parent_entries, key, code)
    _ -> spec_error(code, parent <> "." <> key <> " is required")
  }
}

fn required_int(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(Int, types.SpecError) {
  case lookup(entries, key) {
    Some(json_value.JInt(value)) -> Ok(value)
    _ -> spec_error(code, key <> " is required")
  }
}

fn required_bool(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(Bool, types.SpecError) {
  case lookup(entries, key) {
    Some(json_value.JBool(value)) -> Ok(value)
    _ -> spec_error(code, key <> " is required")
  }
}

fn required_list(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(List(json_value.JsonValue), types.SpecError) {
  case lookup(entries, key) {
    None -> spec_error(code, key <> " is required")
    Some(json_value.JArray(values)) -> Ok(values)
    Some(_) -> spec_error("workstream_list_invalid", key <> " must be a list")
  }
}

fn required_string_list(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(List(String), types.SpecError) {
  use values <- result.try(required_list(entries, key, code))
  decode_string_list(values, key, [])
}

fn decode_string_list(
  values: List(json_value.JsonValue),
  key: String,
  acc: List(String),
) -> Result(List(String), types.SpecError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [json_value.JString(value), ..rest] ->
      decode_string_list(rest, key, [value, ..acc])
    [_, ..] ->
      spec_error(
        "workstream_string_list_invalid",
        key <> " entries must be strings",
      )
  }
}

fn optional_string_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  code: String,
) -> Result(Option(String), types.SpecError) {
  case lookup(entries, key) {
    None -> Ok(None)
    Some(json_value.JString(value)) -> Ok(Some(value))
    Some(_) -> spec_error(code, key <> " must be a string")
  }
}

fn lookup(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(entry_key, value), ..rest] ->
      case entry_key == key {
        True -> Some(value)
        False -> lookup(rest, key)
      }
  }
}

fn validate_snapshot_ref(ref: String) -> Result(Nil, types.SpecError) {
  use hash <- result.try(snapshot_ref_hash(ref))
  case is_lower_hex(hash) && string.length(hash) == 64 {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_snapshot_ref_invalid",
        "snapshot ref must include a lowercase sha256 path",
      )
  }
}

fn validate_snapshot_ref_sha256(
  ref: String,
  sha256: String,
) -> Result(Nil, types.SpecError) {
  use hash <- result.try(snapshot_ref_hash(ref))
  case hash == sha256 {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_snapshot_hash_mismatch",
        "snapshot ref hash must match sha256",
      )
  }
}

fn snapshot_ref_hash(ref: String) -> Result(String, types.SpecError) {
  case
    string.starts_with(ref, "workstream-artifacts/sha256/")
    && string.ends_with(ref, ".json")
  {
    True -> Ok(string.slice(ref, 28, string.length(ref) - 33))
    False ->
      spec_error(
        "workstream_snapshot_ref_invalid",
        "snapshot ref must be repository relative",
      )
  }
}

fn validate_original_path(path: String) -> Result(Nil, types.SpecError) {
  case
    path == ""
    || string.starts_with(path, "/")
    || path == ".."
    || string.starts_with(path, "../")
    || string.contains(path, "/../")
    || string.ends_with(path, "/..")
  {
    True ->
      spec_error(
        "workstream_original_path_invalid",
        "original_path must be repository relative",
      )
    False -> Ok(Nil)
  }
}

fn validate_sha256(value: String) -> Result(Nil, types.SpecError) {
  case string.length(value) == 64 && is_lower_hex(value) {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_sha256_invalid",
        "sha256 must be 64 lowercase hex characters",
      )
  }
}

fn validate_bytes(value: Int) -> Result(Nil, types.SpecError) {
  case value > 0 {
    True -> Ok(Nil)
    False -> spec_error("workstream_bytes_invalid", "bytes must be positive")
  }
}

fn validate_contract_type(value: String) -> Result(Nil, types.SpecError) {
  case workflow_contract.type_from_string(value) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      spec_error(
        "workstream_contract_type_unknown",
        "unknown contract type: " <> value,
      )
  }
}

fn validate_decision_kind(value: String) -> Result(Nil, types.SpecError) {
  case value {
    "approve" | "request_changes" | "reject" | "deviate" -> Ok(Nil)
    _ ->
      spec_error(
        "workstream_decision_kind_unknown",
        "unknown decision kind: " <> value,
      )
  }
}

fn validate_next_action_state(value: String) -> Result(Nil, types.SpecError) {
  case value {
    "suggested" | "available" | "blocked" -> Ok(Nil)
    _ ->
      spec_error(
        "workstream_next_action_state_unknown",
        "unknown next action state: " <> value,
      )
  }
}

fn is_lower_hex(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.all(fn(char) {
    case char {
      "0"
      | "1"
      | "2"
      | "3"
      | "4"
      | "5"
      | "6"
      | "7"
      | "8"
      | "9"
      | "a"
      | "b"
      | "c"
      | "d"
      | "e"
      | "f" -> True
      _ -> False
    }
  })
}

fn spec_error(code: String, message: String) -> Result(a, types.SpecError) {
  Error(types.SpecError(code, message))
}
