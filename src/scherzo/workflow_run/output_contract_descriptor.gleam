import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/workflow_artifact_descriptor
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest as contract_manifest

pub fn extension_and_media(
  spec: workflow_contract.OutputSpec,
) -> #(String, String) {
  let #(default_extension, default_media_type) =
    default_extension_and_media(spec.type_)
  let media_type = case spec.descriptor {
    Some(workflow_contract.ContractDescriptorSpec(
      media_type: Some(media_type),
      ..,
    )) -> media_type
    _ -> default_media_type
  }
  #(extension_for_media_type(media_type, default_extension), media_type)
}

pub fn source_for_descriptor(
  spec: workflow_contract.OutputSpec,
) -> Option(json_value.JsonValue) {
  case descriptor_source_required(spec) {
    True -> Some(source_with_descriptor(spec, json_value.JObject([])))
    False -> None
  }
}

pub fn source_with_descriptor(
  spec: workflow_contract.OutputSpec,
  source: json_value.JsonValue,
) -> json_value.JsonValue {
  case descriptor_source_required(spec), spec.descriptor, source {
    True, Some(descriptor), json_value.JObject(entries) ->
      json_value.JObject(list.append(
        entries,
        descriptor_source_entries(
          descriptor,
          full_descriptor_source_required(spec),
        ),
      ))
    _, _, _ -> source
  }
}

fn descriptor_source_required(spec: workflow_contract.OutputSpec) -> Bool {
  case spec.descriptor {
    Some(workflow_contract.ContractDescriptorSpec(artifact_type: Some(_), ..)) ->
      True
    Some(_) -> full_descriptor_source_required(spec)
    None -> False
  }
}

fn full_descriptor_source_required(spec: workflow_contract.OutputSpec) -> Bool {
  case spec.descriptor {
    Some(descriptor) ->
      descriptor != workflow_contract.descriptor_for_type(spec.type_)
    None -> False
  }
}

fn descriptor_source_entries(
  descriptor: workflow_contract.ContractDescriptorSpec,
  include_descriptor: Bool,
) -> List(#(String, json_value.JsonValue)) {
  let artifact_type_entries = case descriptor.artifact_type {
    Some(artifact_type) -> [
      #("contract_artifact_type", json_value.JString(artifact_type)),
    ]
    None -> []
  }
  case include_descriptor {
    True ->
      list.append(artifact_type_entries, [
        #("contract_descriptor", contract_descriptor_json(descriptor)),
      ])
    False -> artifact_type_entries
  }
}

fn contract_descriptor_json(
  descriptor: workflow_contract.ContractDescriptorSpec,
) -> json_value.JsonValue {
  []
  |> put_optional_string("kind", descriptor.kind)
  |> put_optional_string("ref_type", descriptor.ref_type)
  |> put_optional_string("media_type", descriptor.media_type)
  |> put_optional_string("artifact_type", descriptor.artifact_type)
  |> list.reverse
  |> json_value.JObject
}

fn put_optional_string(
  fields: List(#(String, json_value.JsonValue)),
  key: String,
  value: Option(String),
) -> List(#(String, json_value.JsonValue)) {
  case value {
    Some(value) -> [#(key, json_value.JString(value)), ..fields]
    None -> fields
  }
}

pub fn validate_retained_output_descriptor(
  spec: workflow_contract.OutputSpec,
  value: contract_manifest.ManifestValue,
  contents: BitArray,
) -> Result(Nil, String) {
  case spec.type_ {
    workflow_contract.GenericArtifactSet ->
      validate_artifact_set(spec, value, contents)
    _ -> Ok(Nil)
  }
}

fn validate_artifact_set(
  spec: workflow_contract.OutputSpec,
  value: contract_manifest.ManifestValue,
  contents: BitArray,
) -> Result(Nil, String) {
  case contract_manifest.descriptor_for_named_value(spec.name, value) {
    Some(descriptor) -> validate_artifact_set_descriptor(descriptor, contents)
    None -> Ok(Nil)
  }
}

fn validate_artifact_set_descriptor(
  descriptor: workflow_artifact_descriptor.ArtifactDescriptor,
  contents: BitArray,
) -> Result(Nil, String) {
  case descriptor.kind {
    workflow_artifact_descriptor.ArtifactSetKind ->
      case bit_array.to_string(contents) {
        Ok(text) -> verify_artifact_set_text(text, descriptor)
        Error(_) -> Error("workflow_output_artifact_set_invalid:invalid_utf8")
      }
    _ -> Ok(Nil)
  }
}

fn verify_artifact_set_text(
  text: String,
  descriptor: workflow_artifact_descriptor.ArtifactDescriptor,
) -> Result(Nil, String) {
  case
    workflow_artifact_descriptor.parse_retained_artifact_set(text, descriptor)
  {
    Ok(parsed) ->
      workflow_artifact_descriptor.verify_retained_integrity(
        parsed,
        artifact_store.new("."),
      )
      |> result.map_error(descriptor_error_diagnostic)
    Error(error) -> Error(descriptor_error_diagnostic(error))
  }
}

fn descriptor_error_diagnostic(
  error: workflow_artifact_descriptor.DescriptorError,
) -> String {
  let workflow_artifact_descriptor.DescriptorError(code, _) = error
  "workflow_output_artifact_set_invalid:" <> code
}

fn default_extension_and_media(
  type_: workflow_contract.ContractType,
) -> #(String, String) {
  case type_ {
    workflow_contract.DocumentMarkdown | workflow_contract.ExecPlan -> #(
      ".md",
      "text/markdown",
    )
    workflow_contract.Text | workflow_contract.Url | workflow_contract.GitRef -> #(
      ".txt",
      "text/plain",
    )
    workflow_contract.CommitStack -> #(
      ".json",
      "application/vnd.scherzo.git-commit-stack+json",
    )
    workflow_contract.CodeChange
    | workflow_contract.ExecPlanBundle
    | workflow_contract.ImplementationPack
    | workflow_contract.CodeChangeBundle
    | workflow_contract.ArtifactList
    | workflow_contract.GenericArtifactSet
    | workflow_contract.GenericValue -> #(".json", "application/json")
    workflow_contract.GenericFile -> #(".bin", "application/octet-stream")
    workflow_contract.GenericRef -> #(".txt", "text/plain")
  }
}

fn extension_for_media_type(
  media_type: String,
  default_extension: String,
) -> String {
  case media_type {
    "text/markdown" -> ".md"
    "text/plain" -> ".txt"
    "application/json" -> ".json"
    "image/png" -> ".png"
    "video/webm" -> ".webm"
    _ -> default_extension
  }
}
