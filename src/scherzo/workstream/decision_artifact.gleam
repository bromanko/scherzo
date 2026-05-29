import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/workstream/artifact_values
import scherzo/workstream/types

pub fn decode(
  contents: String,
) -> Result(types.DecisionArtifact, types.SpecError) {
  use entries <- result.try(parse_object(contents))
  use Nil <- result.try(validate_headers(entries))
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
  use gate_id <- result.try(required_string(
    entries,
    "gate_id",
    "workstream_gate_id_missing",
  ))
  use kind <- result.try(required_string(
    entries,
    "kind",
    "workstream_decision_kind_missing",
  ))
  use Nil <- result.try(validate_decision_kind(kind))
  use decided_at_ms <- result.try(required_int(
    entries,
    "decided_at_ms",
    "workstream_decided_at_ms_missing",
  ))
  use Nil <- result.try(validate_decided_at_ms(decided_at_ms))
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
  use inputs <- result.try(required_list(
    entries,
    "inputs",
    "workstream_inputs_missing",
  ))
  use Nil <- result.try(validate_non_empty_inputs(inputs))
  use inputs <- result.try(decode_inputs(inputs))
  use summary <- result.try(required_string(
    entries,
    "summary",
    "workstream_summary_missing",
  ))
  Ok(types.DecisionArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    action_id: action_id,
    gate_id: gate_id,
    kind: kind,
    decided_at_ms: decided_at_ms,
    decided_by: decided_by,
    rationale: rationale,
    inputs: inputs,
    summary: summary,
  ))
}

pub fn to_json(value: types.DecisionArtifact) -> json.Json {
  json_value.to_json(artifact_values.decision_to_value(value))
}

pub fn to_string(value: types.DecisionArtifact) -> String {
  to_json(value) |> json.to_string
}

fn validate_headers(
  entries: List(#(String, json_value.JsonValue)),
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
  case artifact_type == types.decision_artifact_type {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_artifact_type_invalid",
        "artifact_type must be " <> types.decision_artifact_type,
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

fn validate_non_empty_inputs(
  values: List(json_value.JsonValue),
) -> Result(Nil, types.SpecError) {
  case values {
    [] ->
      spec_error("workstream_inputs_empty", "decision inputs must not be empty")
    _ -> Ok(Nil)
  }
}

fn decode_inputs(
  values: List(json_value.JsonValue),
) -> Result(List(types.DecisionInputRef), types.SpecError) {
  case values {
    [] -> Ok([])
    [json_value.JObject(entries), ..rest] -> {
      use name <- result.try(required_string(
        entries,
        "name",
        "workstream_input_name_missing",
      ))
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
      use other <- result.try(decode_inputs(rest))
      Ok([types.DecisionInputRef(name: name, ref: ref, sha256: sha256), ..other])
    }
    [_, ..] ->
      spec_error("workstream_input_invalid", "decision inputs must be objects")
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

fn validate_decided_at_ms(value: Int) -> Result(Nil, types.SpecError) {
  case value > 0 {
    True -> Ok(Nil)
    False ->
      spec_error(
        "workstream_decided_at_ms_invalid",
        "decided_at_ms must be positive",
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
