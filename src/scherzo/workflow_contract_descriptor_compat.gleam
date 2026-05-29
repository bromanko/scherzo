import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import yay

pub type DescriptorCompatError {
  DescriptorCompatError(code: String, message: String)
}

pub fn type_name_from_entries(
  entries: List(#(String, yay.Node)),
  kind: String,
  name: String,
) -> Result(String, DescriptorCompatError) {
  case get_entry(entries, "kind") {
    Some(yay.NodeStr(raw_kind)) ->
      infer_descriptor_type(
        string.trim(raw_kind) |> string.lowercase,
        read_optional_string_entry(entries, "media_type", kind, name),
        read_optional_string_entry(entries, "artifact_type", kind, name),
        read_optional_string_entry(entries, "ref_type", kind, name),
        kind,
        name,
      )
    Some(_) ->
      error(
        "contract_descriptor_kind_not_string",
        "contract " <> kind <> " " <> name <> " kind must be a string",
      )
    None ->
      error(
        "missing_contract_entry_type",
        "contract "
          <> kind
          <> " "
          <> name
          <> " type or descriptor kind is required",
      )
  }
}

fn infer_descriptor_type(
  descriptor_kind: String,
  media_type: Result(Option(String), DescriptorCompatError),
  artifact_type: Result(Option(String), DescriptorCompatError),
  ref_type: Result(Option(String), DescriptorCompatError),
  kind: String,
  name: String,
) -> Result(String, DescriptorCompatError) {
  use media_type <- result.try(media_type)
  use artifact_type <- result.try(artifact_type)
  use ref_type <- result.try(ref_type)
  case
    descriptor_kind,
    media_type,
    normalize_artifact_type(artifact_type),
    ref_type
  {
    "file", Some("text/plain"), _, _ -> Ok("text")
    "file", Some("text/markdown"), Some("exec_plan"), _ -> Ok("exec_plan")
    "file", Some("text/markdown"), _, _ -> Ok("document.markdown")
    "file", Some("application/json"), Some("implementation_pack"), _ ->
      Ok("implementation_pack")
    "artifact_set", _, Some("artifact[]"), _ -> Ok("artifact[]")
    "artifact_set", Some("application/json"), Some("exec_plan_bundle"), _ ->
      Ok("exec_plan_bundle")
    "artifact_set", Some("application/json"), Some("code_change_bundle"), _ ->
      Ok("code_change_bundle")
    "ref", _, _, Some("url") -> Ok("url")
    "ref", _, _, Some("git_ref") -> Ok("git_ref")
    _, _, _, _ ->
      error(
        "unknown_contract_descriptor_type",
        "contract "
          <> kind
          <> " "
          <> name
          <> " descriptor does not map to a supported legacy contract type",
      )
  }
}

fn read_optional_string_entry(
  entries: List(#(String, yay.Node)),
  key: String,
  kind: String,
  name: String,
) -> Result(Option(String), DescriptorCompatError) {
  case get_entry(entries, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(string.trim(value) |> string.lowercase))
    Some(_) ->
      error(
        "contract_descriptor_field_not_string",
        "contract " <> kind <> " " <> name <> " " <> key <> " must be a string",
      )
  }
}

pub fn legacy_artifact_type_name(artifact_type: String) -> String {
  case string.trim(artifact_type) |> string.lowercase {
    "scherzo.exec_plan.v1" -> "exec_plan"
    "scherzo.exec_plan_bundle.v2" -> "exec_plan_bundle"
    "scherzo.implementation_pack.v1" -> "implementation_pack"
    "scherzo.implementation_pack.v2" -> "implementation_pack"
    "scherzo.code_change_bundle.v2" -> "code_change_bundle"
    other -> other
  }
}

fn normalize_artifact_type(artifact_type: Option(String)) -> Option(String) {
  case artifact_type {
    Some(value) -> Some(legacy_artifact_type_name(value))
    None -> None
  }
}

fn get_entry(
  entries: List(#(String, yay.Node)),
  key: String,
) -> Option(yay.Node) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> get_entry(rest, key)
      }
  }
}

fn error(code: String, message: String) -> Result(a, DescriptorCompatError) {
  Error(DescriptorCompatError(code, message))
}
