import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/json_value
import scherzo/workflow_contract

pub fn extension_and_media(
  spec: workflow_contract.OutputSpec,
) -> #(String, String) {
  let #(default_extension, default_media_type) =
    default_extension_and_media(spec.type_)
  let media_type = case spec.descriptor {
    Some(workflow_contract.OutputDescriptorSpec(
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
  case spec.descriptor {
    Some(workflow_contract.OutputDescriptorSpec(artifact_type: Some(_), ..)) ->
      Some(source_with_descriptor(spec, json_value.JObject([])))
    _ -> None
  }
}

pub fn source_with_descriptor(
  spec: workflow_contract.OutputSpec,
  source: json_value.JsonValue,
) -> json_value.JsonValue {
  case spec.descriptor, source {
    Some(workflow_contract.OutputDescriptorSpec(
      artifact_type: Some(artifact_type),
      ..,
    )),
      json_value.JObject(entries)
    ->
      json_value.JObject(
        list.append(entries, [
          #("contract_artifact_type", json_value.JString(artifact_type)),
        ]),
      )
    _, _ -> source
  }
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
    | workflow_contract.ArtifactList -> #(".json", "application/json")
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
    _ -> default_extension
  }
}
