import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/json_value
import scherzo/workflow_contract

pub type ArtifactKind {
  FileKind
  ValueKind
  RefKind
  ArtifactSetKind
}

pub type ArtifactDescriptor {
  ArtifactDescriptor(
    name: String,
    kind: ArtifactKind,
    artifact_type: Option(String),
    description: Option(String),
    source: Option(json_value.JsonValue),
    validation: Option(json_value.JsonValue),
    metadata: Option(json_value.JsonValue),
    ref_type: Option(String),
    ref: Option(String),
    sha256: Option(String),
    bytes: Option(Int),
    media_type: Option(String),
    value: Option(json_value.JsonValue),
    entries: List(ArtifactDescriptor),
  )
}

pub type DescriptorError {
  DescriptorError(code: String, message: String)
}

pub fn kind_to_string(kind: ArtifactKind) -> String {
  case kind {
    FileKind -> "file"
    ValueKind -> "value"
    RefKind -> "ref"
    ArtifactSetKind -> "artifact_set"
  }
}

pub fn kind_from_string(raw: String) -> Result(ArtifactKind, DescriptorError) {
  case string.trim(raw) |> string.lowercase {
    "file" -> Ok(FileKind)
    "value" -> Ok(ValueKind)
    "ref" -> Ok(RefKind)
    "artifact_set" -> Ok(ArtifactSetKind)
    other ->
      error(
        "artifact_descriptor_unknown_kind",
        "unknown artifact descriptor kind: " <> other,
      )
  }
}

pub fn to_json(descriptor: ArtifactDescriptor) -> json.Json {
  descriptor
  |> descriptor_fields
  |> json.object
}

pub fn to_string(descriptor: ArtifactDescriptor) -> String {
  descriptor |> to_json |> json.to_string
}

pub fn parse(contents: String) -> Result(ArtifactDescriptor, DescriptorError) {
  use value <- result.try(case json_value.parse(contents) {
    Ok(value) -> Ok(value)
    Error(_) ->
      Error(DescriptorError(
        "artifact_descriptor_invalid_json",
        "artifact descriptor must be valid JSON",
      ))
  })
  from_json_value(value)
}

pub fn parse_retained_artifact_set(
  contents: String,
  descriptor: ArtifactDescriptor,
) -> Result(ArtifactDescriptor, DescriptorError) {
  use value <- result.try(case json_value.parse(contents) {
    Ok(value) -> Ok(value)
    Error(_) ->
      Error(DescriptorError(
        "artifact_descriptor_invalid_json",
        "artifact_set payload must be valid JSON",
      ))
  })
  case value {
    json_value.JObject(entries) -> {
      use child_entries <- result.try(parse_child_entries(entries))
      let descriptor = ArtifactDescriptor(..descriptor, entries: child_entries)
      use Nil <- result.try(validate(descriptor))
      Ok(descriptor)
    }
    _ ->
      error(
        "artifact_descriptor_not_object",
        "artifact_set payload must be a JSON object",
      )
  }
}

pub fn decoder() -> decode.Decoder(ArtifactDescriptor) {
  use value <- decode.then(json_value.decoder())
  case from_json_value(value) {
    Ok(descriptor) -> decode.success(descriptor)
    Error(DescriptorError(_, message)) ->
      decode.failure(placeholder_descriptor(), expected: message)
  }
}

pub fn validate(
  descriptor: ArtifactDescriptor,
) -> Result(Nil, DescriptorError) {
  use Nil <- result.try(validate_name(descriptor.name))
  case descriptor.kind {
    FileKind -> validate_file(descriptor)
    ValueKind -> validate_value_descriptor(descriptor)
    RefKind -> validate_ref_descriptor(descriptor)
    ArtifactSetKind -> validate_artifact_set(descriptor)
  }
}

fn from_json_value(
  value: json_value.JsonValue,
) -> Result(ArtifactDescriptor, DescriptorError) {
  case value {
    json_value.JObject(entries) -> {
      use name <- result.try(required_string_field(entries, "name"))
      use raw_kind <- result.try(required_string_field(entries, "kind"))
      use kind <- result.try(kind_from_string(raw_kind))
      let descriptor =
        ArtifactDescriptor(
          name: name,
          kind: kind,
          artifact_type: optional_string_field(entries, "artifact_type"),
          description: optional_string_field(entries, "description"),
          source: optional_json_field(entries, "source"),
          validation: optional_json_field(entries, "validation"),
          metadata: optional_json_field(entries, "metadata"),
          ref_type: optional_string_field(entries, "ref_type"),
          ref: optional_string_field(entries, "ref"),
          sha256: optional_string_field(entries, "sha256"),
          bytes: optional_int_field(entries, "bytes"),
          media_type: optional_string_field(entries, "media_type"),
          value: present_json_field(entries, "value"),
          entries: [],
        )
        |> normalize
      use child_entries <- result.try(parse_child_entries(entries))
      let descriptor = ArtifactDescriptor(..descriptor, entries: child_entries)
      use Nil <- result.try(validate(descriptor))
      Ok(descriptor)
    }
    _ ->
      error(
        "artifact_descriptor_not_object",
        "artifact descriptor must be a JSON object",
      )
  }
}

fn normalize(descriptor: ArtifactDescriptor) -> ArtifactDescriptor {
  case descriptor.kind, descriptor.media_type {
    ValueKind, None ->
      ArtifactDescriptor(..descriptor, media_type: Some("application/json"))
    _, _ -> descriptor
  }
}

fn descriptor_fields(
  descriptor: ArtifactDescriptor,
) -> List(#(String, json.Json)) {
  let base = [
    #("name", json.string(descriptor.name)),
    #("kind", json.string(kind_to_string(descriptor.kind))),
  ]
  let base =
    put_optional_string(base, "artifact_type", descriptor.artifact_type)
  let base = put_optional_string(base, "description", descriptor.description)
  let base = put_optional_json_value(base, "source", descriptor.source)
  let base = put_optional_json_value(base, "validation", descriptor.validation)
  let base = put_optional_json_value(base, "metadata", descriptor.metadata)
  case descriptor.kind {
    FileKind ->
      base
      |> put_optional_string("ref", descriptor.ref)
      |> put_optional_string("sha256", descriptor.sha256)
      |> put_optional_int("bytes", descriptor.bytes)
      |> put_optional_string("media_type", descriptor.media_type)
    ValueKind ->
      base
      |> put_optional_string("media_type", descriptor.media_type)
      |> put_optional_json_value("value", descriptor.value)
    RefKind ->
      base
      |> put_optional_string("ref_type", descriptor.ref_type)
      |> put_optional_string("ref", descriptor.ref)
    ArtifactSetKind -> {
      let with_retained =
        base
        |> put_optional_string("ref", descriptor.ref)
        |> put_optional_string("sha256", descriptor.sha256)
        |> put_optional_int("bytes", descriptor.bytes)
        |> put_optional_string("media_type", descriptor.media_type)
      list.append(with_retained, [
        #(
          "entries",
          json.array(descriptor.entries, of: fn(entry) { to_json(entry) }),
        ),
      ])
    }
  }
}

fn validate_name(name: String) -> Result(Nil, DescriptorError) {
  case string.trim(name) != "" && !has_control_character(name) {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_descriptor_invalid_name",
        "artifact descriptor name must be non-empty and contain no control characters",
      )
  }
}

fn validate_file(
  descriptor: ArtifactDescriptor,
) -> Result(Nil, DescriptorError) {
  use ref <- result.try(require_field(
    descriptor.ref,
    "artifact_descriptor_file_missing_ref",
    descriptor.name <> " file descriptor is missing ref",
  ))
  use sha256 <- result.try(require_field(
    descriptor.sha256,
    "artifact_descriptor_file_missing_sha256",
    descriptor.name <> " file descriptor is missing sha256",
  ))
  use bytes <- result.try(require_field(
    descriptor.bytes,
    "artifact_descriptor_file_missing_bytes",
    descriptor.name <> " file descriptor is missing bytes",
  ))
  use media_type <- result.try(require_field(
    descriptor.media_type,
    "artifact_descriptor_file_missing_media_type",
    descriptor.name <> " file descriptor is missing media_type",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.ref_type,
    "artifact_descriptor_file_unexpected_ref_type",
    descriptor.name <> " file descriptor must not include ref_type",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.value,
    "artifact_descriptor_file_unexpected_value",
    descriptor.name <> " file descriptor must not include value",
  ))
  use Nil <- result.try(forbid_non_empty_entries(
    descriptor,
    "artifact_descriptor_file_unexpected_entries",
  ))
  use Nil <- result.try(validate_run_ref(ref, descriptor.name))
  use Nil <- result.try(validate_sha256(sha256, descriptor.name))
  use Nil <- result.try(validate_non_negative_bytes(bytes, descriptor.name))
  validate_media_type(media_type, descriptor.name)
}

fn validate_value_descriptor(
  descriptor: ArtifactDescriptor,
) -> Result(Nil, DescriptorError) {
  use _value <- result.try(require_field(
    descriptor.value,
    "artifact_descriptor_value_missing_value",
    descriptor.name <> " value descriptor is missing value",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.ref_type,
    "artifact_descriptor_value_unexpected_ref_type",
    descriptor.name <> " value descriptor must not include ref_type",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.ref,
    "artifact_descriptor_value_unexpected_ref",
    descriptor.name <> " value descriptor must not include ref",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.sha256,
    "artifact_descriptor_value_unexpected_sha256",
    descriptor.name <> " value descriptor must not include sha256",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.bytes,
    "artifact_descriptor_value_unexpected_bytes",
    descriptor.name <> " value descriptor must not include bytes",
  ))
  use Nil <- result.try(forbid_non_empty_entries(
    descriptor,
    "artifact_descriptor_value_unexpected_entries",
  ))
  case descriptor.media_type {
    Some("application/json") -> Ok(Nil)
    Some(_) ->
      error(
        "artifact_descriptor_value_invalid_media_type",
        descriptor.name
          <> " value descriptor media_type must be application/json",
      )
    None ->
      error(
        "artifact_descriptor_value_missing_media_type",
        descriptor.name <> " value descriptor is missing media_type",
      )
  }
}

fn validate_ref_descriptor(
  descriptor: ArtifactDescriptor,
) -> Result(Nil, DescriptorError) {
  use ref_type <- result.try(require_field(
    descriptor.ref_type,
    "artifact_descriptor_ref_missing_ref_type",
    descriptor.name <> " ref descriptor is missing ref_type",
  ))
  use ref <- result.try(require_field(
    descriptor.ref,
    "artifact_descriptor_ref_missing_ref",
    descriptor.name <> " ref descriptor is missing ref",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.sha256,
    "artifact_descriptor_ref_unexpected_sha256",
    descriptor.name <> " ref descriptor must not include sha256",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.bytes,
    "artifact_descriptor_ref_unexpected_bytes",
    descriptor.name <> " ref descriptor must not include bytes",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.media_type,
    "artifact_descriptor_ref_unexpected_media_type",
    descriptor.name <> " ref descriptor must not include media_type",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.value,
    "artifact_descriptor_ref_unexpected_value",
    descriptor.name <> " ref descriptor must not include value",
  ))
  use Nil <- result.try(forbid_non_empty_entries(
    descriptor,
    "artifact_descriptor_ref_unexpected_entries",
  ))
  case ref_type {
    "url" -> validate_http_url(ref, descriptor.name)
    "git_ref" -> validate_git_ref(ref, descriptor.name)
    _ -> Ok(Nil)
  }
}

fn validate_artifact_set(
  descriptor: ArtifactDescriptor,
) -> Result(Nil, DescriptorError) {
  use Nil <- result.try(forbid_field(
    descriptor.ref_type,
    "artifact_descriptor_artifact_set_unexpected_ref_type",
    descriptor.name <> " artifact_set descriptor must not include ref_type",
  ))
  use Nil <- result.try(forbid_field(
    descriptor.value,
    "artifact_descriptor_artifact_set_unexpected_value",
    descriptor.name <> " artifact_set descriptor must not include value",
  ))
  use Nil <- result.try(
    validate_child_names(descriptor.name, descriptor.entries, []),
  )
  use Nil <- result.try(validate_entries(descriptor.entries))
  validate_retained_artifact_set_metadata(descriptor)
}

fn validate_entries(
  entries: List(ArtifactDescriptor),
) -> Result(Nil, DescriptorError) {
  case entries {
    [] -> Ok(Nil)
    [entry, ..rest] -> {
      use Nil <- result.try(validate(entry))
      validate_entries(rest)
    }
  }
}

fn validate_child_names(
  parent_name: String,
  entries: List(ArtifactDescriptor),
  seen: List(String),
) -> Result(Nil, DescriptorError) {
  case entries {
    [] -> Ok(Nil)
    [entry, ..rest] ->
      case list.contains(seen, entry.name) {
        True ->
          error(
            "artifact_descriptor_duplicate_entry_name",
            parent_name
              <> " artifact_set has duplicate entry name: "
              <> entry.name,
          )
        False -> validate_child_names(parent_name, rest, [entry.name, ..seen])
      }
  }
}

fn validate_retained_artifact_set_metadata(
  descriptor: ArtifactDescriptor,
) -> Result(Nil, DescriptorError) {
  let present_count =
    count_optional_string(descriptor.ref)
    + count_optional_string(descriptor.sha256)
    + count_optional_int(descriptor.bytes)
    + count_optional_string(descriptor.media_type)
  case present_count {
    0 -> Ok(Nil)
    4 ->
      case
        descriptor.ref,
        descriptor.sha256,
        descriptor.bytes,
        descriptor.media_type
      {
        Some(ref), Some(sha256), Some(bytes), Some(media_type) -> {
          use Nil <- result.try(validate_run_ref(ref, descriptor.name))
          use Nil <- result.try(validate_sha256(sha256, descriptor.name))
          use Nil <- result.try(validate_non_negative_bytes(
            bytes,
            descriptor.name,
          ))
          case media_type {
            "application/json" -> Ok(Nil)
            _ ->
              error(
                "artifact_descriptor_artifact_set_invalid_media_type",
                descriptor.name
                  <> " retained artifact_set media_type must be application/json",
              )
          }
        }
        _, _, _, _ ->
          error(
            "artifact_descriptor_artifact_set_incomplete_retained_metadata",
            descriptor.name
              <> " artifact_set retained metadata must be all present or all absent",
          )
      }
    _ ->
      error(
        "artifact_descriptor_artifact_set_incomplete_retained_metadata",
        descriptor.name
          <> " artifact_set retained metadata must be all present or all absent",
      )
  }
}

fn validate_run_ref(ref: String, name: String) -> Result(Nil, DescriptorError) {
  case string.starts_with(ref, "runs/") && !is_absolute_or_placeholder(ref) {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_descriptor_invalid_ref",
        name <> " ref must be a Scherzo artifact-store ref starting with runs/",
      )
  }
}

fn validate_sha256(
  sha256: String,
  name: String,
) -> Result(Nil, DescriptorError) {
  case string.length(sha256) == 64 && all_hex_lowercase(sha256) {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_descriptor_invalid_sha256",
        name <> " sha256 must be 64 lowercase hex characters",
      )
  }
}

fn validate_non_negative_bytes(
  bytes: Int,
  name: String,
) -> Result(Nil, DescriptorError) {
  case bytes >= 0 {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_descriptor_negative_bytes",
        name <> " bytes must be non-negative",
      )
  }
}

fn validate_media_type(
  media_type: String,
  name: String,
) -> Result(Nil, DescriptorError) {
  case string.trim(media_type) != "" {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_descriptor_invalid_media_type",
        name <> " media_type must be non-empty",
      )
  }
}

fn validate_http_url(
  ref: String,
  name: String,
) -> Result(Nil, DescriptorError) {
  case
    string.starts_with(ref, "https://") || string.starts_with(ref, "http://")
  {
    True -> Ok(Nil)
    False ->
      error(
        "artifact_descriptor_invalid_url_ref",
        name <> " url ref must be http or https",
      )
  }
}

fn validate_git_ref(ref: String, name: String) -> Result(Nil, DescriptorError) {
  workflow_contract.valid_git_ref(ref)
  |> bool_result(
    "artifact_descriptor_invalid_git_ref",
    name <> " git_ref must be non-empty and contain no control characters",
  )
}

fn bool_result(
  condition: Bool,
  code: String,
  message: String,
) -> Result(Nil, DescriptorError) {
  case condition {
    True -> Ok(Nil)
    False -> error(code, message)
  }
}

fn require_field(
  value: Option(a),
  code: String,
  message: String,
) -> Result(a, DescriptorError) {
  case value {
    Some(value) -> Ok(value)
    None -> error(code, message)
  }
}

fn forbid_field(
  value: Option(a),
  code: String,
  message: String,
) -> Result(Nil, DescriptorError) {
  case value {
    Some(_) -> error(code, message)
    None -> Ok(Nil)
  }
}

fn forbid_non_empty_entries(
  descriptor: ArtifactDescriptor,
  code: String,
) -> Result(Nil, DescriptorError) {
  case descriptor.entries {
    [] -> Ok(Nil)
    _ -> error(code, descriptor.name <> " descriptor must not include entries")
  }
}

fn required_string_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Result(String, DescriptorError) {
  case json_field(entries, key) {
    Some(json_value.JString(value)) -> Ok(value)
    Some(_) ->
      error(
        "artifact_descriptor_invalid_field",
        "artifact descriptor field " <> key <> " must be a string",
      )
    None ->
      error(
        "artifact_descriptor_missing_field",
        "artifact descriptor is missing required field: " <> key,
      )
  }
}

fn optional_string_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(String) {
  case json_field(entries, key) {
    Some(json_value.JString(value)) -> Some(value)
    _ -> None
  }
}

fn optional_int_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(Int) {
  case json_field(entries, key) {
    Some(json_value.JInt(value)) -> Some(value)
    _ -> None
  }
}

fn optional_json_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case json_field(entries, key) {
    Some(json_value.JNull) | None -> None
    Some(value) -> Some(value)
  }
}

fn present_json_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case json_field(entries, key) {
    Some(value) -> Some(value)
    None -> None
  }
}

fn parse_child_entries(
  entries: List(#(String, json_value.JsonValue)),
) -> Result(List(ArtifactDescriptor), DescriptorError) {
  case json_field(entries, "entries") {
    None -> Ok([])
    Some(json_value.JArray(values)) -> parse_child_entry_values(values, [])
    Some(_) ->
      error(
        "artifact_descriptor_invalid_entries",
        "artifact descriptor field entries must be a JSON array",
      )
  }
}

fn parse_child_entry_values(
  values: List(json_value.JsonValue),
  acc: List(ArtifactDescriptor),
) -> Result(List(ArtifactDescriptor), DescriptorError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [value, ..rest] -> {
      use descriptor <- result.try(from_json_value(value))
      parse_child_entry_values(rest, [descriptor, ..acc])
    }
  }
}

fn json_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> json_field(rest, key)
      }
  }
}

fn count_optional_string(value: Option(String)) -> Int {
  case value {
    Some(_) -> 1
    None -> 0
  }
}

fn count_optional_int(value: Option(Int)) -> Int {
  case value {
    Some(_) -> 1
    None -> 0
  }
}

fn has_control_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) { ch == "\n" || ch == "\r" || ch == "\t" })
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

fn all_hex_lowercase(value: String) -> Bool {
  value |> string.to_graphemes |> list.all(is_hex_lowercase)
}

fn is_hex_lowercase(ch: String) -> Bool {
  is_digit(ch) || is_lower_hex(ch)
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != Lt && string.compare(ch, "9") != Gt
}

fn is_lower_hex(ch: String) -> Bool {
  string.compare(ch, "a") != Lt && string.compare(ch, "f") != Gt
}

fn put_optional_string(
  fields: List(#(String, json.Json)),
  key: String,
  value: Option(String),
) -> List(#(String, json.Json)) {
  case value {
    Some(value) -> list.append(fields, [#(key, json.string(value))])
    None -> fields
  }
}

fn put_optional_int(
  fields: List(#(String, json.Json)),
  key: String,
  value: Option(Int),
) -> List(#(String, json.Json)) {
  case value {
    Some(value) -> list.append(fields, [#(key, json.int(value))])
    None -> fields
  }
}

fn put_optional_json_value(
  fields: List(#(String, json.Json)),
  key: String,
  value: Option(json_value.JsonValue),
) -> List(#(String, json.Json)) {
  case value {
    Some(value) -> list.append(fields, [#(key, json_value.to_json(value))])
    None -> fields
  }
}

fn placeholder_descriptor() -> ArtifactDescriptor {
  ArtifactDescriptor(
    name: "",
    kind: ValueKind,
    artifact_type: None,
    description: None,
    source: None,
    validation: None,
    metadata: None,
    ref_type: None,
    ref: None,
    sha256: None,
    bytes: None,
    media_type: Some("application/json"),
    value: Some(json_value.JNull),
    entries: [],
  )
}

fn error(code: String, message: String) -> Result(a, DescriptorError) {
  Error(DescriptorError(code, message))
}
