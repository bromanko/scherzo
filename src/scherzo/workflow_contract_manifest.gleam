import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/workflow_artifact_descriptor as artifact_descriptor
import scherzo/workflow_contract

pub const schema_version = 1

pub type ArtifactWritten {
  ArtifactWritten(ref: String, sha256: String, bytes: Int)
}

pub type ManifestStatus {
  Present
  Absent
}

pub type RefKind {
  RunArtifact
  UrlRef
  GitRefRef
  InlineJsonRef
}

pub type ManifestValue {
  ManifestValue(
    type_: workflow_contract.ContractType,
    status: ManifestStatus,
    ref_kind: Option(RefKind),
    ref: Option(String),
    sha256: Option(String),
    bytes: Option(Int),
    media_type: Option(String),
    value: Option(json_value.JsonValue),
    source: Option(json_value.JsonValue),
    diagnostic: Option(String),
  )
}

pub type NamedManifestValue {
  NamedManifestValue(name: String, value: ManifestValue)
}

pub type ContractInputManifest {
  ContractInputManifest(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    inputs: List(NamedManifestValue),
    context: List(NamedManifestValue),
    diagnostics: List(String),
  )
}

pub type ContractOutputManifest {
  ContractOutputManifest(
    run_id: String,
    workflow_id: String,
    workflow_fingerprint: String,
    outputs: List(NamedManifestValue),
    diagnostics: List(String),
  )
}

pub type ManifestError {
  ManifestError(code: String, message: String)
}

pub fn absent(
  type_: workflow_contract.ContractType,
  diagnostic: Option(String),
) -> ManifestValue {
  ManifestValue(
    type_: type_,
    status: Absent,
    ref_kind: None,
    ref: None,
    sha256: None,
    bytes: None,
    media_type: None,
    value: None,
    source: None,
    diagnostic: diagnostic,
  )
}

pub fn present_run_artifact(
  type_: workflow_contract.ContractType,
  artifact: ArtifactWritten,
  media_type: String,
  source: Option(json_value.JsonValue),
) -> ManifestValue {
  ManifestValue(
    type_: type_,
    status: Present,
    ref_kind: Some(RunArtifact),
    ref: Some(artifact.ref),
    sha256: Some(artifact.sha256),
    bytes: Some(artifact.bytes),
    media_type: Some(media_type),
    value: None,
    source: source,
    diagnostic: None,
  )
}

pub fn present_url(
  type_: workflow_contract.ContractType,
  url: String,
) -> ManifestValue {
  ManifestValue(
    type_: type_,
    status: Present,
    ref_kind: Some(UrlRef),
    ref: Some(url),
    sha256: None,
    bytes: None,
    media_type: None,
    value: None,
    source: None,
    diagnostic: None,
  )
}

pub fn present_git_ref(
  type_: workflow_contract.ContractType,
  ref: String,
) -> ManifestValue {
  ManifestValue(
    type_: type_,
    status: Present,
    ref_kind: Some(GitRefRef),
    ref: Some(ref),
    sha256: None,
    bytes: None,
    media_type: None,
    value: None,
    source: None,
    diagnostic: None,
  )
}

pub fn present_inline_json(
  type_: workflow_contract.ContractType,
  value: json_value.JsonValue,
  source: Option(json_value.JsonValue),
) -> ManifestValue {
  ManifestValue(
    type_: type_,
    status: Present,
    ref_kind: Some(InlineJsonRef),
    ref: None,
    sha256: None,
    bytes: None,
    media_type: Some("application/json"),
    value: Some(value),
    source: source,
    diagnostic: None,
  )
}

pub fn type_matches(
  value: ManifestValue,
  expected: workflow_contract.ContractType,
) -> Bool {
  value.type_ == expected
}

pub fn artifact_type_matches(
  value: ManifestValue,
  expected: workflow_contract.ContractType,
) -> Bool {
  case source_artifact_type(value) {
    None -> True
    Some(artifact_type) -> artifact_type_string_matches(artifact_type, expected)
  }
}

pub fn artifact_type_string_matches(
  artifact_type: String,
  expected: workflow_contract.ContractType,
) -> Bool {
  case descriptor_artifact_type(expected) {
    Some(descriptor_type) ->
      artifact_type == descriptor_type
      || artifact_type == workflow_contract.type_to_string(expected)
    None -> artifact_type == workflow_contract.type_to_string(expected)
  }
}

pub fn semantic_type_matches(
  value: ManifestValue,
  expected: workflow_contract.ContractType,
) -> Bool {
  type_matches(value, expected) && artifact_type_matches(value, expected)
}

pub fn descriptor_artifact_type(
  type_: workflow_contract.ContractType,
) -> Option(String) {
  legacy_descriptor_artifact_type(type_)
}

pub fn source_artifact_type(value: ManifestValue) -> Option(String) {
  case value.source {
    Some(json_value.JObject(entries)) ->
      source_string_field(entries, "artifact_type")
    _ -> None
  }
}

pub fn validate_required_output_value(
  name: String,
  value: ManifestValue,
) -> Result(Nil, ManifestError) {
  validate_value(name, value, True)
}

pub fn validate_value(
  name: String,
  value: ManifestValue,
  required required: Bool,
) -> Result(Nil, ManifestError) {
  case value.status {
    Absent ->
      case required {
        True ->
          error(
            "manifest_required_value_absent",
            name <> " is required but absent",
          )
        False -> Ok(Nil)
      }
    Present -> {
      use Nil <- result.try(validate_present_reference(name, value))
      validate_code_change_value(name, value)
    }
  }
}

pub fn input_manifest_to_json(manifest: ContractInputManifest) -> json.Json {
  json.object([
    #("schema_version", json.int(schema_version)),
    #("artifact_type", json.string("workflow_contract_inputs")),
    #("run_id", json.string(manifest.run_id)),
    #("workflow_id", json.string(manifest.workflow_id)),
    #("workflow_fingerprint", json.string(manifest.workflow_fingerprint)),
    #("inputs", named_values_to_json(manifest.inputs)),
    #("context", named_values_to_json(manifest.context)),
    #("diagnostics", json.array(manifest.diagnostics, of: json.string)),
  ])
}

pub fn output_manifest_to_json(manifest: ContractOutputManifest) -> json.Json {
  json.object([
    #("schema_version", json.int(schema_version)),
    #("artifact_type", json.string("workflow_contract_outputs")),
    #("run_id", json.string(manifest.run_id)),
    #("workflow_id", json.string(manifest.workflow_id)),
    #("workflow_fingerprint", json.string(manifest.workflow_fingerprint)),
    #("outputs", named_values_to_json(manifest.outputs)),
    #("diagnostics", json.array(manifest.diagnostics, of: json.string)),
  ])
}

pub fn input_manifest_to_string(manifest: ContractInputManifest) -> String {
  input_manifest_to_json(manifest) |> json.to_string
}

pub fn output_manifest_to_string(manifest: ContractOutputManifest) -> String {
  output_manifest_to_json(manifest) |> json.to_string
}

pub fn decode_manifest_value(contents: String) -> Result(ManifestValue, Nil) {
  json.parse(contents, manifest_value_decoder())
  |> result.replace_error(Nil)
}

pub fn decode_input_manifest(
  contents: String,
) -> Result(ContractInputManifest, Nil) {
  json.parse(contents, input_manifest_decoder())
  |> result.replace_error(Nil)
}

pub fn decode_output_manifest(
  contents: String,
) -> Result(ContractOutputManifest, Nil) {
  json.parse(contents, output_manifest_decoder())
  |> result.replace_error(Nil)
}

pub fn manifest_value_decoder() -> decode.Decoder(ManifestValue) {
  use type_text <- decode.field("type", decode.string)
  let type_ = case workflow_contract.type_from_string(type_text) {
    Ok(type_) -> type_
    Error(workflow_contract.ContractError(_, _)) -> workflow_contract.Text
  }
  use status <- decode.field("status", status_decoder())
  use ref_kind <- decode.optional_field(
    "ref_kind",
    None,
    decode.optional(ref_kind_decoder()),
  )
  use ref <- decode.optional_field("ref", None, decode.optional(decode.string))
  use sha256 <- decode.optional_field(
    "sha256",
    None,
    decode.optional(decode.string),
  )
  use bytes <- decode.optional_field("bytes", None, decode.optional(decode.int))
  use media_type <- decode.optional_field(
    "media_type",
    None,
    decode.optional(decode.string),
  )
  use raw_value <- decode.optional_field(
    "value",
    None,
    optional_json_value_decoder(),
  )
  use source <- decode.optional_field(
    "source",
    None,
    decode.optional(json_value.decoder()),
  )
  use diagnostic <- decode.optional_field(
    "diagnostic",
    None,
    decode.optional(decode.string),
  )
  decode.success(ManifestValue(
    type_: type_,
    status: status,
    ref_kind: ref_kind,
    ref: ref,
    sha256: sha256,
    bytes: bytes,
    media_type: media_type,
    value: manifest_json_value(ref_kind, raw_value),
    source: source,
    diagnostic: diagnostic,
  ))
}

pub fn input_manifest_decoder() -> decode.Decoder(ContractInputManifest) {
  use Nil <- decode.then(manifest_header_decoder("workflow_contract_inputs"))
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use workflow_fingerprint <- decode.field(
    "workflow_fingerprint",
    decode.string,
  )
  use inputs <- decode.field(
    "inputs",
    decode.list(named_manifest_value_decoder()),
  )
  use context <- decode.field(
    "context",
    decode.list(named_manifest_value_decoder()),
  )
  use diagnostics <- decode.optional_field(
    "diagnostics",
    [],
    decode.list(decode.string),
  )
  decode.success(ContractInputManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    inputs: inputs,
    context: context,
    diagnostics: diagnostics,
  ))
}

pub fn output_manifest_decoder() -> decode.Decoder(ContractOutputManifest) {
  use Nil <- decode.then(manifest_header_decoder("workflow_contract_outputs"))
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use workflow_fingerprint <- decode.field(
    "workflow_fingerprint",
    decode.string,
  )
  use outputs <- decode.field(
    "outputs",
    decode.list(named_manifest_value_decoder()),
  )
  use diagnostics <- decode.optional_field(
    "diagnostics",
    [],
    decode.list(decode.string),
  )
  decode.success(ContractOutputManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    outputs: outputs,
    diagnostics: diagnostics,
  ))
}

fn manifest_header_decoder(
  expected_artifact_type: String,
) -> decode.Decoder(Nil) {
  use version <- decode.field("schema_version", decode.int)
  use artifact_type <- decode.field("artifact_type", decode.string)
  case version == schema_version && artifact_type == expected_artifact_type {
    True -> decode.success(Nil)
    False -> decode.failure(Nil, expected: "workflow contract manifest header")
  }
}

fn named_values_to_json(values: List(NamedManifestValue)) -> json.Json {
  json.array(values, of: named_value_to_json)
}

fn named_value_to_json(value: NamedManifestValue) -> json.Json {
  json.object([
    #("name", json.string(value.name)),
    #("value", manifest_value_to_json(value.value)),
    #(
      "descriptor",
      option_descriptor_to_json(descriptor_for_named_value(
        value.name,
        value.value,
      )),
    ),
  ])
}

pub fn descriptor_for_named_value(
  name: String,
  value: ManifestValue,
) -> Option(artifact_descriptor.ArtifactDescriptor) {
  case value.status {
    Absent -> None
    Present -> validated_descriptor(manifest_value_descriptor(name, value))
  }
}

pub fn manifest_value_to_json(value: ManifestValue) -> json.Json {
  json.object([
    #("type", json.string(workflow_contract.type_to_string(value.type_))),
    #("status", json.string(status_to_string(value.status))),
    #("ref_kind", option_ref_kind_to_json(value.ref_kind)),
    #("ref", option_string_to_json(value.ref)),
    #("sha256", option_string_to_json(value.sha256)),
    #("bytes", option_int_to_json(value.bytes)),
    #("media_type", option_string_to_json(value.media_type)),
    #("value", option_json_value_to_json(value.value)),
    #("source", option_json_value_to_json(value.source)),
    #("diagnostic", option_string_to_json(value.diagnostic)),
  ])
}

fn named_manifest_value_decoder() -> decode.Decoder(NamedManifestValue) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", manifest_value_decoder())
  use descriptor <- decode.optional_field(
    "descriptor",
    None,
    decode.optional(artifact_descriptor.decoder()),
  )
  case descriptor_matches_manifest_value(name, value, descriptor) {
    True -> decode.success(NamedManifestValue(name: name, value: value))
    False ->
      decode.failure(
        NamedManifestValue(name: name, value: value),
        expected: "descriptor compatible with manifest entry value",
      )
  }
}

fn status_decoder() -> decode.Decoder(ManifestStatus) {
  use status <- decode.then(decode.string)
  case status {
    "present" -> decode.success(Present)
    "absent" -> decode.success(Absent)
    _ -> decode.failure(Absent, expected: "ManifestStatus")
  }
}

fn ref_kind_decoder() -> decode.Decoder(RefKind) {
  use kind <- decode.then(decode.string)
  case kind {
    "run_artifact" -> decode.success(RunArtifact)
    "url" -> decode.success(UrlRef)
    "git_ref" -> decode.success(GitRefRef)
    "inline_json" -> decode.success(InlineJsonRef)
    _ -> decode.failure(RunArtifact, expected: "RefKind")
  }
}

fn status_to_string(status: ManifestStatus) -> String {
  case status {
    Present -> "present"
    Absent -> "absent"
  }
}

fn ref_kind_to_string(kind: RefKind) -> String {
  case kind {
    RunArtifact -> "run_artifact"
    UrlRef -> "url"
    GitRefRef -> "git_ref"
    InlineJsonRef -> "inline_json"
  }
}

fn validate_present_reference(
  name: String,
  value: ManifestValue,
) -> Result(Nil, ManifestError) {
  case value.ref_kind {
    Some(RunArtifact) ->
      case value.ref {
        Some(ref) ->
          case valid_run_artifact_ref(ref) {
            True -> Ok(Nil)
            False ->
              error(
                "manifest_invalid_run_artifact_ref",
                name <> " has invalid run artifact ref",
              )
          }
        None -> error("manifest_missing_ref", name <> " is missing ref")
      }
    Some(UrlRef) ->
      case value.ref {
        Some(ref) ->
          case valid_http_url(ref) {
            True -> Ok(Nil)
            False ->
              error("manifest_invalid_url_ref", name <> " has invalid URL ref")
          }
        None -> error("manifest_missing_ref", name <> " is missing ref")
      }
    Some(GitRefRef) ->
      case value.ref {
        Some(ref) ->
          workflow_contract.valid_git_ref(ref)
          |> bool_result(
            "manifest_invalid_git_ref",
            name <> " has invalid git ref",
          )
        None -> error("manifest_missing_ref", name <> " is missing ref")
      }
    Some(InlineJsonRef) ->
      case value.value {
        Some(_) -> Ok(Nil)
        None ->
          error(
            "manifest_missing_inline_json",
            name <> " is missing inline JSON value",
          )
      }
    None -> error("manifest_missing_ref_kind", name <> " is missing ref_kind")
  }
}

fn validate_code_change_value(
  name: String,
  value: ManifestValue,
) -> Result(Nil, ManifestError) {
  case value.type_, value.ref_kind, value.value {
    workflow_contract.CodeChange,
      Some(InlineJsonRef),
      Some(json_value.JObject(entries))
    ->
      case
        has_any_key(entries, ["pr_url", "branch", "merge_commit", "patch_ref"])
      {
        True -> Ok(Nil)
        False ->
          error(
            "manifest_code_change_missing_reference",
            name
              <> " code_change must include pr_url, branch, merge_commit, or patch_ref",
          )
      }
    workflow_contract.CodeChange, Some(InlineJsonRef), _ ->
      error(
        "manifest_code_change_not_object",
        name <> " code_change inline_json must be an object",
      )
    _, _, _ -> Ok(Nil)
  }
}

fn has_any_key(
  entries: List(#(String, json_value.JsonValue)),
  keys: List(String),
) -> Bool {
  case keys {
    [] -> False
    [key, ..rest] ->
      json_value.object_has_key(entries, key) || has_any_key(entries, rest)
  }
}

fn valid_run_artifact_ref(ref: String) -> Bool {
  string.starts_with(ref, "runs/") && !is_absolute_or_placeholder(ref)
}

fn is_absolute_or_placeholder(ref: String) -> Bool {
  string.starts_with(ref, "/")
  || string.starts_with(ref, "<absolute-local-path>")
  || starts_with_drive_path(ref)
}

fn starts_with_drive_path(value: String) -> Bool {
  case string.to_graphemes(value) {
    [letter, ":", separator, ..] ->
      is_letter(letter) && { separator == "/" || separator == "\\" }
    _ -> False
  }
}

fn is_letter(ch: String) -> Bool {
  is_lower(ch) || is_upper(ch)
}

fn is_lower(ch: String) -> Bool {
  string.compare(ch, "a") != Lt && string.compare(ch, "z") != Gt
}

fn is_upper(ch: String) -> Bool {
  string.compare(ch, "A") != Lt && string.compare(ch, "Z") != Gt
}

fn valid_http_url(value: String) -> Bool {
  string.starts_with(value, "https://") || string.starts_with(value, "http://")
}

fn bool_result(
  condition: Bool,
  code: String,
  message: String,
) -> Result(Nil, ManifestError) {
  case condition {
    True -> Ok(Nil)
    False -> error(code, message)
  }
}

fn option_ref_kind_to_json(value: Option(RefKind)) -> json.Json {
  case value {
    Some(kind) -> json.string(ref_kind_to_string(kind))
    None -> json.null()
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn option_json_value_to_json(value: Option(json_value.JsonValue)) -> json.Json {
  case value {
    Some(value) -> json_value.to_json(value)
    None -> json.null()
  }
}

fn optional_json_value_decoder() -> decode.Decoder(Option(json_value.JsonValue)) {
  json_value.decoder() |> decode.map(Some)
}

fn manifest_json_value(
  ref_kind: Option(RefKind),
  value: Option(json_value.JsonValue),
) -> Option(json_value.JsonValue) {
  case ref_kind, value {
    Some(InlineJsonRef), Some(value) -> Some(value)
    _, Some(json_value.JNull) -> None
    _, _ -> value
  }
}

fn option_descriptor_to_json(
  value: Option(artifact_descriptor.ArtifactDescriptor),
) -> json.Json {
  case value {
    Some(value) -> artifact_descriptor.to_json(value)
    None -> json.null()
  }
}

fn validated_descriptor(
  descriptor: Option(artifact_descriptor.ArtifactDescriptor),
) -> Option(artifact_descriptor.ArtifactDescriptor) {
  case descriptor {
    Some(descriptor) ->
      case artifact_descriptor.validate(descriptor) {
        Ok(Nil) -> Some(descriptor)
        Error(_) -> None
      }
    None -> None
  }
}

fn descriptor_matches_manifest_value(
  name: String,
  value: ManifestValue,
  descriptor: Option(artifact_descriptor.ArtifactDescriptor),
) -> Bool {
  case descriptor {
    None -> True
    Some(descriptor) ->
      case descriptor_for_named_value(name, value) {
        Some(expected) -> descriptors_compatible(descriptor, expected)
        None -> False
      }
  }
}

fn descriptors_compatible(
  descriptor: artifact_descriptor.ArtifactDescriptor,
  expected: artifact_descriptor.ArtifactDescriptor,
) -> Bool {
  descriptor.name == expected.name
  && descriptor.kind == expected.kind
  && descriptor.artifact_type == expected.artifact_type
  && descriptor.ref_type == expected.ref_type
  && descriptor.ref == expected.ref
  && descriptor.sha256 == expected.sha256
  && descriptor.bytes == expected.bytes
  && descriptor.media_type == expected.media_type
  && descriptor.value == expected.value
}

fn manifest_value_descriptor(
  name: String,
  value: ManifestValue,
) -> Option(artifact_descriptor.ArtifactDescriptor) {
  case value.ref_kind {
    Some(RunArtifact) -> Some(run_artifact_descriptor(name, value))
    Some(UrlRef) ->
      Some(
        artifact_descriptor.ArtifactDescriptor(
          name: name,
          kind: artifact_descriptor.RefKind,
          artifact_type: legacy_descriptor_artifact_type(value.type_),
          description: None,
          source: value.source,
          validation: None,
          metadata: None,
          ref_type: Some("url"),
          ref: value.ref,
          sha256: None,
          bytes: None,
          media_type: None,
          value: None,
          entries: [],
        ),
      )
    Some(GitRefRef) ->
      Some(
        artifact_descriptor.ArtifactDescriptor(
          name: name,
          kind: artifact_descriptor.RefKind,
          artifact_type: legacy_descriptor_artifact_type(value.type_),
          description: None,
          source: value.source,
          validation: None,
          metadata: None,
          ref_type: Some("git_ref"),
          ref: value.ref,
          sha256: None,
          bytes: None,
          media_type: None,
          value: None,
          entries: [],
        ),
      )
    Some(InlineJsonRef) ->
      Some(
        artifact_descriptor.ArtifactDescriptor(
          name: name,
          kind: artifact_descriptor.ValueKind,
          artifact_type: legacy_descriptor_artifact_type(value.type_),
          description: None,
          source: value.source,
          validation: None,
          metadata: None,
          ref_type: None,
          ref: None,
          sha256: None,
          bytes: None,
          media_type: value.media_type,
          value: value.value,
          entries: [],
        ),
      )
    None -> None
  }
}

fn run_artifact_descriptor(
  name: String,
  value: ManifestValue,
) -> artifact_descriptor.ArtifactDescriptor {
  case value.type_ {
    workflow_contract.ArtifactList
    | workflow_contract.ExecPlanBundle
    | workflow_contract.CodeChangeBundle ->
      artifact_descriptor.ArtifactDescriptor(
        name: name,
        kind: artifact_descriptor.ArtifactSetKind,
        artifact_type: legacy_descriptor_artifact_type(value.type_),
        description: None,
        source: value.source,
        validation: None,
        metadata: None,
        ref_type: None,
        ref: value.ref,
        sha256: value.sha256,
        bytes: value.bytes,
        media_type: value.media_type,
        value: None,
        entries: [],
      )
    _ ->
      artifact_descriptor.ArtifactDescriptor(
        name: name,
        kind: artifact_descriptor.FileKind,
        artifact_type: legacy_descriptor_artifact_type(value.type_),
        description: None,
        source: value.source,
        validation: None,
        metadata: None,
        ref_type: None,
        ref: value.ref,
        sha256: value.sha256,
        bytes: value.bytes,
        media_type: value.media_type,
        value: None,
        entries: [],
      )
  }
}

fn legacy_descriptor_artifact_type(
  type_: workflow_contract.ContractType,
) -> Option(String) {
  case type_ {
    workflow_contract.ExecPlan -> Some("scherzo.exec_plan.v1")
    workflow_contract.ExecPlanBundle -> Some("scherzo.exec_plan_bundle.v2")
    workflow_contract.ImplementationPack ->
      Some("scherzo.implementation_pack.v2")
    workflow_contract.CodeChangeBundle -> Some("scherzo.code_change_bundle.v2")
    workflow_contract.CodeChange -> Some("code_change")
    workflow_contract.ArtifactList -> Some("artifact[]")
    workflow_contract.DocumentMarkdown -> Some("document.markdown")
    workflow_contract.Url -> Some("url")
    workflow_contract.GitRef -> Some("git_ref")
    workflow_contract.Text -> None
  }
}

fn source_string_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(String) {
  case entries {
    [] -> None
    [#(current, json_value.JString(value)), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> source_string_field(rest, key)
      }
    [_, ..rest] -> source_string_field(rest, key)
  }
}

fn error(code: String, message: String) -> Result(a, ManifestError) {
  Error(ManifestError(code, message))
}
