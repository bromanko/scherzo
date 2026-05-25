import gleam/bit_array
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/json_decode_error
import scherzo/json_value
import scherzo/path
import scherzo/step_artifact
import scherzo/structured_output_metadata
import scherzo/workflow_identity
import simplifile

pub opaque type Store {
  Store(kind: String, callbacks: StoreCallbacks)
}

pub type StoreCallbacks {
  StoreCallbacks(
    write: fn(String, String) -> Result(Nil, ArtifactError),
    read: fn(String) -> Result(String, ArtifactError),
    write_immutable_bytes: fn(String, BitArray) ->
      Result(ImmutableWriteResult, ArtifactError),
    read_bytes: fn(String) -> Result(BitArray, ArtifactError),
    locate: fn(String) -> Result(ArtifactLocation, ArtifactError),
  )
}

pub type ArtifactLocation {
  ArtifactLocation(
    ref: String,
    uri: String,
    display_path: String,
    local_path: Option(String),
  )
}

pub type ArtifactRef {
  ArtifactRef(ref: String, sha256: String, bytes: Int)
}

pub type StructuredArtifactRef {
  StructuredArtifactRef(
    ref: String,
    path: String,
    uri: String,
    display_path: String,
    local_path: Option(String),
    sha256: String,
    bytes: Int,
  )
}

pub type ImmutableArtifactRef {
  ImmutableArtifactRef(
    ref: String,
    path: String,
    uri: String,
    display_path: String,
    local_path: Option(String),
    sha256: String,
    bytes: Int,
  )
}

pub type StructuredOutputArtifact {
  StructuredOutputArtifact(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    artifact_name: String,
    format: String,
    source_type: String,
    source_tool_name: Option(String),
    schema_required_keys: List(String),
    validation: structured_output_metadata.ValidationMetadata,
    payload: json_value.JsonValue,
  )
}

pub type ArtifactWriteError {
  InvalidPath(reason: String)
  OpenTempFailed(reason: String)
  WriteTempFailed(reason: String)
  SyncTempFailed(reason: String)
  CloseTempFailed(reason: String)
  RenameFailed(reason: String)
  SyncParentFailed(reason: String)
  CleanupTempFailed(reason: String)
  UnexpectedFfiFailure(function: String, detail: String)
}

pub type ImmutableWriteResult {
  ImmutableWritten
  ImmutableExisting
  ImmutableConflict
}

pub type ArtifactError {
  ArtifactIo(String)
  ArtifactWriteFailed(ArtifactWriteError)
  MissingStepArtifact(String)
  CorruptStepArtifact(String)
  InvalidArtifactRef(String)
  DecodeArtifactFailed(String)
  DirectorySyncUnsupported(String)
}

type StoredArtifactDecodeError {
  InvalidStoredStepArtifact(json.DecodeError)
}

type StoredArtifact {
  StoredArtifact(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    artifact: step_artifact.StepArtifact,
  )
}

pub fn custom(kind: String, callbacks: StoreCallbacks) -> Store {
  Store(kind: kind, callbacks: callbacks)
}

pub fn filesystem(workspace_root: String) -> Store {
  let root = artifact_root(workspace_root)
  custom(
    "filesystem",
    StoreCallbacks(
      write: fn(ref, contents) { filesystem_write(root, ref, contents) },
      read: fn(ref) { filesystem_read(root, ref) },
      write_immutable_bytes: fn(ref, contents) {
        filesystem_write_immutable_bytes(root, ref, contents)
      },
      read_bytes: fn(ref) { filesystem_read_bytes(root, ref) },
      locate: fn(ref) { Ok(filesystem_location(root, ref)) },
    ),
  )
}

pub fn new(workspace_root: String) -> Store {
  filesystem(workspace_root)
}

pub fn location(
  store: Store,
  ref: String,
) -> Result(ArtifactLocation, ArtifactError) {
  use valid_ref <- result.try(validated_ref(ref))
  store.callbacks.locate(valid_ref)
}

pub fn read_artifact_unverified(
  store: Store,
  ref: String,
) -> Result(String, ArtifactError) {
  use valid_ref <- result.try(validated_ref(ref))
  store.callbacks.read(valid_ref)
}

pub fn read_artifact_bytes_unverified(
  store: Store,
  ref: String,
) -> Result(BitArray, ArtifactError) {
  use valid_ref <- result.try(validated_ref(ref))
  store.callbacks.read_bytes(valid_ref)
}

pub fn write_immutable_artifact_bytes(
  store: Store,
  ref: String,
  contents: BitArray,
) -> Result(ImmutableWriteResult, ArtifactError) {
  use valid_ref <- result.try(validated_ref(ref))
  use write_result <- result.try(store.callbacks.write_immutable_bytes(
    valid_ref,
    contents,
  ))
  case write_result {
    ImmutableConflict -> Ok(ImmutableConflict)
    ImmutableExisting | ImmutableWritten -> {
      use final <- result.try(store.callbacks.read_bytes(valid_ref))
      case final == contents {
        True -> Ok(write_result)
        False -> Ok(ImmutableConflict)
      }
    }
  }
}

pub fn write_step_artifact(
  store: Store,
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  artifact: step_artifact.StepArtifact,
) -> Result(ArtifactRef, ArtifactError) {
  let ref = artifact_ref(run_id, step_id, attempt_index)
  let bytes =
    stored_to_string(StoredArtifact(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      artifact: artifact,
    ))
  write_ref(store, ref, bytes)
}

pub fn write_structured_output_artifact(
  store: Store,
  run_id: String,
  workflow_id: String,
  step_id: String,
  attempt_index: Int,
  artifact_name: String,
  format: String,
  schema_required_keys: List(String),
  validation: structured_output_metadata.ValidationMetadata,
  payload_json: String,
) -> Result(StructuredArtifactRef, ArtifactError) {
  let ref =
    structured_output_artifact_ref(
      run_id,
      step_id,
      attempt_index,
      artifact_name,
    )
  use payload <- result.try(parse_structured_payload_json(payload_json))
  let bytes =
    structured_output_to_string(StructuredOutputArtifact(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      artifact_name: artifact_name,
      format: format,
      source_type: validation.source_type,
      source_tool_name: validation.source_tool_name,
      schema_required_keys: schema_required_keys,
      validation: validation,
      payload: payload,
    ))
  use written <- result.try(write_ref(store, ref, bytes))
  use artifact_location <- result.try(location(store, written.ref))
  Ok(structured_ref_from_artifact(written, artifact_location))
}

pub fn write_recovery_artifact_json(
  store: Store,
  run_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  artifact_name: String,
  payload_json: String,
) -> Result(ImmutableArtifactRef, ArtifactError) {
  let ref =
    recovery_artifact_ref(
      run_id,
      step_id,
      failed_attempt_index,
      recovery_attempt_number,
      artifact_name,
    )
  let bytes = bit_array.from_string(payload_json)
  let sha256 = hash.sha256_hex(payload_json)
  use write_result <- result.try(write_immutable_artifact_bytes(
    store,
    ref,
    bytes,
  ))
  case write_result {
    ImmutableConflict ->
      Error(DecodeArtifactFailed("immutable_recovery_artifact_conflict"))
    ImmutableExisting | ImmutableWritten -> {
      use artifact_location <- result.try(location(store, ref))
      Ok(ImmutableArtifactRef(
        ref: ref,
        path: location_path_or_ref(artifact_location, ref),
        uri: artifact_location.uri,
        display_path: artifact_location.display_path,
        local_path: artifact_location.local_path,
        sha256: sha256,
        bytes: bit_array.byte_size(bytes),
      ))
    }
  }
}

fn parse_structured_payload_json(
  payload_json: String,
) -> Result(json_value.JsonValue, ArtifactError) {
  case json_value.parse(payload_json) {
    Ok(payload) -> Ok(payload)
    Error(Nil) -> Error(DecodeArtifactFailed("invalid_payload_json"))
  }
}

pub fn read_structured_output_artifact(
  store: Store,
  ref: String,
  expected_sha256: String,
) -> Result(StructuredOutputArtifact, ArtifactError) {
  use contents <- result.try(read_artifact_unverified(store, ref))
  let actual_sha = hash.sha256_hex(contents)
  case actual_sha == expected_sha256 {
    False -> Error(CorruptStepArtifact(ref))
    True -> decode_structured_output_contents(contents)
  }
}

pub fn read_step_artifact(
  store: Store,
  ref: String,
  expected_sha256: String,
) -> Result(step_artifact.StepArtifact, ArtifactError) {
  use contents <- result.try(read_artifact_unverified(store, ref))
  let actual_sha = hash.sha256_hex(contents)
  case actual_sha == expected_sha256 {
    False -> Error(CorruptStepArtifact(ref))
    True -> decode_step_artifact_contents(contents)
  }
}

pub fn read_step_artifact_unverified(
  store: Store,
  ref: String,
) -> Result(step_artifact.StepArtifact, ArtifactError) {
  use contents <- result.try(read_artifact_unverified(store, ref))
  decode_step_artifact_contents(contents)
}

pub fn decode_step_artifact_contents(
  contents: String,
) -> Result(step_artifact.StepArtifact, ArtifactError) {
  case decode_stored_string(contents) {
    Ok(stored) -> Ok(stored.artifact)
    Error(error) ->
      Error(DecodeArtifactFailed(stored_artifact_decode_error_to_string(error)))
  }
}

pub fn artifact_ref(
  run_id: String,
  step_id: String,
  attempt_index: Int,
) -> String {
  "runs/"
  <> workflow_identity.safe_component(run_id, "run")
  <> "/"
  <> workflow_identity.step_component(step_id)
  <> "/attempt-"
  <> int.to_string(attempt_index)
  <> ".json"
}

pub fn structured_output_artifact_ref(
  run_id: String,
  step_id: String,
  attempt_index: Int,
  artifact_name: String,
) -> String {
  "runs/"
  <> workflow_identity.safe_component(run_id, "run")
  <> "/"
  <> workflow_identity.safe_component(step_id, "step")
  <> "/attempt-"
  <> int.to_string(attempt_index)
  <> "/structured/"
  <> workflow_identity.safe_component(artifact_name, "artifact")
  <> ".json"
}

pub fn recovery_artifact_ref(
  run_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  artifact_name: String,
) -> String {
  "runs/"
  <> workflow_identity.safe_component(run_id, "run")
  <> "/"
  <> workflow_identity.step_component(step_id)
  <> "/attempt-"
  <> int.to_string(failed_attempt_index)
  <> "/recovery-"
  <> int.to_string(recovery_attempt_number)
  <> "/"
  <> workflow_identity.safe_component(artifact_name, "artifact")
  <> ".json"
}

pub fn input_manifest_ref(run_id: String) -> String {
  "runs/"
  <> workflow_identity.safe_component(run_id, "run")
  <> "/inputs.v1.json"
}

pub fn output_manifest_ref(run_id: String) -> String {
  output_manifest_ref_for_generation(run_id, 0)
}

pub fn output_manifest_ref_for_generation(
  run_id: String,
  repair_generation: Int,
) -> String {
  case repair_generation <= 0 {
    True ->
      "runs/"
      <> workflow_identity.safe_component(run_id, "run")
      <> "/outputs.v1.json"
    False ->
      "runs/"
      <> workflow_identity.safe_component(run_id, "run")
      <> "/repairs/"
      <> int.to_string(repair_generation)
      <> "/outputs.v1.json"
  }
}

pub fn output_blob_ref(
  run_id: String,
  output_name: String,
  extension: String,
) -> String {
  output_blob_ref_for_generation(run_id, output_name, extension, 0)
}

pub fn output_blob_ref_for_generation(
  run_id: String,
  output_name: String,
  extension: String,
  repair_generation: Int,
) -> String {
  case repair_generation <= 0 {
    True ->
      "runs/"
      <> workflow_identity.safe_component(run_id, "run")
      <> "/outputs/"
      <> workflow_identity.safe_component(output_name, "output")
      <> extension
    False ->
      "runs/"
      <> workflow_identity.safe_component(run_id, "run")
      <> "/repairs/"
      <> int.to_string(repair_generation)
      <> "/outputs/"
      <> workflow_identity.safe_component(output_name, "output")
      <> extension
  }
}

pub fn write_input_manifest(
  store: Store,
  run_id: String,
  contents: String,
) -> Result(ArtifactRef, ArtifactError) {
  write_ref(store, input_manifest_ref(run_id), contents)
}

pub fn write_output_manifest(
  store: Store,
  run_id: String,
  contents: String,
) -> Result(ArtifactRef, ArtifactError) {
  write_output_manifest_for_generation(store, run_id, 0, contents)
}

pub fn write_output_manifest_for_generation(
  store: Store,
  run_id: String,
  repair_generation: Int,
  contents: String,
) -> Result(ArtifactRef, ArtifactError) {
  write_ref(
    store,
    output_manifest_ref_for_generation(run_id, repair_generation),
    contents,
  )
}

pub fn write_output_blob(
  store: Store,
  run_id: String,
  output_name: String,
  extension: String,
  contents: String,
) -> Result(ArtifactRef, ArtifactError) {
  write_output_blob_for_generation(
    store,
    run_id,
    output_name,
    extension,
    0,
    contents,
  )
}

pub fn write_output_blob_for_generation(
  store: Store,
  run_id: String,
  output_name: String,
  extension: String,
  repair_generation: Int,
  contents: String,
) -> Result(ArtifactRef, ArtifactError) {
  write_ref(
    store,
    output_blob_ref_for_generation(
      run_id,
      output_name,
      extension,
      repair_generation,
    ),
    contents,
  )
}

pub fn context_recovery_artifact_ref(
  run_id: String,
  step_id: String,
  step_attempt_index: Int,
  artifact_name: String,
) -> String {
  "runs/"
  <> workflow_identity.safe_component(run_id, "run")
  <> "/"
  <> workflow_identity.step_component(step_id)
  <> "/attempt-"
  <> int.to_string(step_attempt_index)
  <> "/context-recovery/"
  <> workflow_identity.safe_component(artifact_name, "artifact")
}

pub fn context_recovery_display_path(ref: String) -> String {
  ".scherzo-state/artifacts/" <> ref
}

pub fn write_context_recovery_artifact(
  store: Store,
  run_id: String,
  _workflow_id: String,
  step_id: String,
  step_attempt_index: Int,
  artifact_name: String,
  contents: String,
) -> Result(StructuredArtifactRef, ArtifactError) {
  let ref =
    context_recovery_artifact_ref(
      run_id,
      step_id,
      step_attempt_index,
      artifact_name,
    )
  use written <- result.try(write_ref(store, ref, contents))
  use artifact_location <- result.try(location(store, written.ref))
  Ok(structured_ref_from_artifact(written, artifact_location))
}

fn write_ref(
  store: Store,
  ref: String,
  contents: String,
) -> Result(ArtifactRef, ArtifactError) {
  use valid_ref <- result.try(validated_ref(ref))
  use Nil <- result.try(store.callbacks.write(valid_ref, contents))
  use final <- result.try(store.callbacks.read(valid_ref))
  let sha = hash.sha256_hex(final)
  case final == contents {
    True ->
      Ok(ArtifactRef(
        ref: valid_ref,
        sha256: sha,
        bytes: bit_array.byte_size(bit_array.from_string(final)),
      ))
    False -> Error(CorruptStepArtifact(valid_ref))
  }
}

fn location_path_or_ref(
  location: ArtifactLocation,
  fallback_ref: String,
) -> String {
  case location.local_path {
    Some(local_path) -> local_path
    None -> fallback_ref
  }
}

fn structured_ref_from_artifact(
  artifact: ArtifactRef,
  artifact_location: ArtifactLocation,
) -> StructuredArtifactRef {
  StructuredArtifactRef(
    ref: artifact.ref,
    path: legacy_path_for_location(artifact_location),
    uri: artifact_location.uri,
    display_path: artifact_location.display_path,
    local_path: artifact_location.local_path,
    sha256: artifact.sha256,
    bytes: artifact.bytes,
  )
}

fn legacy_path_for_location(location: ArtifactLocation) -> String {
  case location.local_path {
    Some(local_path) -> local_path
    None -> location.display_path
  }
}

fn stored_to_string(stored: StoredArtifact) -> String {
  stored_to_json(stored) |> json.to_string
}

fn stored_to_json(stored: StoredArtifact) -> json.Json {
  json.object([
    #("schema_version", json.int(2)),
    #("run_id", json.string(stored.run_id)),
    #("workflow_id", json.string(stored.workflow_id)),
    #("step_id", json.string(stored.step_id)),
    #("attempt_index", json.int(stored.attempt_index)),
    #("artifact", step_artifact.to_json(stored.artifact)),
  ])
}

fn structured_output_to_string(artifact: StructuredOutputArtifact) -> String {
  structured_output_to_json(artifact) |> json.to_string
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn structured_output_to_json(artifact: StructuredOutputArtifact) -> json.Json {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("structured_output")),
    #("run_id", json.string(artifact.run_id)),
    #("workflow_id", json.string(artifact.workflow_id)),
    #("step_id", json.string(artifact.step_id)),
    #("attempt_index", json.int(artifact.attempt_index)),
    #("artifact_name", json.string(artifact.artifact_name)),
    #("format", json.string(artifact.format)),
    #("source_type", json.string(artifact.source_type)),
    #("source_tool_name", option_string_to_json(artifact.source_tool_name)),
    #(
      "schema",
      json.object([
        #("type", json.string("object")),
        #(
          "required",
          json.array(artifact.schema_required_keys, of: json.string),
        ),
      ]),
    ),
    #("validation", structured_output_metadata.to_json(artifact.validation)),
    #("payload", json_value.to_json(artifact.payload)),
  ])
}

fn decode_structured_output_contents(
  contents: String,
) -> Result(StructuredOutputArtifact, ArtifactError) {
  case json.parse(contents, structured_output_decoder()) {
    Ok(artifact) -> Ok(artifact)
    Error(error) ->
      Error(DecodeArtifactFailed(
        "invalid_structured_output_artifact:"
        <> json_decode_error.to_string(error),
      ))
  }
}

fn structured_output_decoder() -> decode.Decoder(StructuredOutputArtifact) {
  use schema_version <- decode.field("schema_version", decode.int)
  case schema_version == 1 {
    False ->
      decode.failure(
        empty_structured_output_artifact(),
        expected: "StructuredOutputArtifact",
      )
    True -> {
      use artifact_type <- decode.field("artifact_type", decode.string)
      case artifact_type == "structured_output" {
        False ->
          decode.failure(
            empty_structured_output_artifact(),
            expected: "structured_output artifact_type",
          )
        True -> {
          use run_id <- decode.field("run_id", decode.string)
          use workflow_id <- decode.field("workflow_id", decode.string)
          use step_id <- decode.field("step_id", decode.string)
          use attempt_index <- decode.field("attempt_index", decode.int)
          use artifact_name <- decode.field("artifact_name", decode.string)
          use format <- decode.field("format", decode.string)
          use source_type <- decode.optional_field(
            "source_type",
            "final_response",
            decode.string,
          )
          use source_tool_name <- decode.optional_field(
            "source_tool_name",
            None,
            decode.optional(decode.string),
          )
          use schema_required_keys <- decode.optional_field(
            "schema",
            [],
            structured_output_schema_required_decoder(),
          )
          use legacy_required_keys <- decode.optional_field(
            "schema_required_keys",
            schema_required_keys,
            decode.list(decode.string),
          )
          use validation <- decode.optional_field(
            "validation",
            structured_output_metadata.baseline_only(legacy_required_keys),
            structured_output_metadata.decoder(),
          )
          use payload <- decode.field("payload", json_value.decoder())
          decode.success(StructuredOutputArtifact(
            run_id: run_id,
            workflow_id: workflow_id,
            step_id: step_id,
            attempt_index: attempt_index,
            artifact_name: artifact_name,
            format: format,
            source_type: source_type,
            source_tool_name: source_tool_name,
            schema_required_keys: structured_output_metadata.required_keys(
              validation,
            ),
            validation: validation,
            payload: payload,
          ))
        }
      }
    }
  }
}

fn structured_output_schema_required_decoder() -> decode.Decoder(List(String)) {
  use required_keys <- decode.field("required", decode.list(decode.string))
  decode.success(required_keys)
}

fn empty_structured_output_artifact() -> StructuredOutputArtifact {
  StructuredOutputArtifact(
    run_id: "",
    workflow_id: "",
    step_id: "",
    attempt_index: 0,
    artifact_name: "",
    format: "",
    source_type: "final_response",
    source_tool_name: None,
    schema_required_keys: [],
    validation: structured_output_metadata.baseline_only([]),
    payload: json_value.JNull,
  )
}

fn decode_stored_string(
  contents: String,
) -> Result(StoredArtifact, StoredArtifactDecodeError) {
  case json.parse(contents, stored_decoder()) {
    Ok(stored) -> Ok(stored)
    Error(error) -> Error(InvalidStoredStepArtifact(error))
  }
}

fn stored_artifact_decode_error_to_string(
  error: StoredArtifactDecodeError,
) -> String {
  case error {
    InvalidStoredStepArtifact(error) ->
      "invalid_stored_step_artifact:" <> json_decode_error.to_string(error)
  }
}

fn stored_decoder() -> decode.Decoder(StoredArtifact) {
  use schema_version <- decode.field("schema_version", decode.int)
  case schema_version == 2 {
    False ->
      decode.failure(
        StoredArtifact("", "", "", 0, empty_artifact()),
        expected: "StoredArtifact",
      )
    True -> {
      use run_id <- decode.field("run_id", decode.string)
      use workflow_id <- decode.field("workflow_id", decode.string)
      use step_id <- decode.field("step_id", decode.string)
      use attempt_index <- decode.field("attempt_index", decode.int)
      use artifact <- decode.field("artifact", step_artifact.decoder())
      decode.success(StoredArtifact(
        run_id,
        workflow_id,
        step_id,
        attempt_index,
        artifact,
      ))
    }
  }
}

fn empty_artifact() -> step_artifact.StepArtifact {
  step_artifact.StepArtifact(
    step_id: "",
    status: step_artifact.StepFailed,
    final_response: None,
    exit_code: None,
    command: None,
    duration_ms: None,
    diagnostic_path: None,
    failure_code: None,
    stdout: "",
    stderr: "",
    timed_out: False,
    final_response_truncated: False,
    stdout_truncated: False,
    stderr_truncated: False,
    summary_text: "",
    structured_output: None,
  )
}

fn validated_ref(ref: String) -> Result(String, ArtifactError) {
  use Nil <- result.try(validate_ref(ref))
  Ok(string.trim(ref))
}

fn artifact_root(workspace_root: String) -> String {
  path.join(path.join(workspace_root, ".scherzo-state"), "artifacts")
}

fn filesystem_write(
  root: String,
  ref: String,
  contents: String,
) -> Result(Nil, ArtifactError) {
  let final_path = path.join(root, ref)
  use Nil <- result.try(ensure_parent(final_path))
  write_atomic(final_path, contents)
  |> result.map_error(fn(error) { ArtifactWriteFailed(error) })
}

fn filesystem_read(root: String, ref: String) -> Result(String, ArtifactError) {
  let final_path = path.join(root, ref)
  simplifile.read(final_path)
  |> result.map_error(fn(error) {
    case error {
      simplifile.Enoent -> MissingStepArtifact(ref)
      _ -> ArtifactIo("read artifact: " <> simplifile.describe_error(error))
    }
  })
}

fn filesystem_write_immutable_bytes(
  root: String,
  ref: String,
  contents: BitArray,
) -> Result(ImmutableWriteResult, ArtifactError) {
  let final_path = path.join(root, ref)
  write_immutable(final_path, contents)
  |> result.map_error(fn(error) { ArtifactWriteFailed(error) })
}

fn filesystem_read_bytes(
  root: String,
  ref: String,
) -> Result(BitArray, ArtifactError) {
  let final_path = path.join(root, ref)
  read_file_bytes(final_path)
  |> result.map_error(fn(error) {
    case error {
      MissingStepArtifact(_) -> MissingStepArtifact(ref)
      _ -> error
    }
  })
}

fn filesystem_location(root: String, ref: String) -> ArtifactLocation {
  let final_path = path.join(root, ref)
  ArtifactLocation(
    ref: ref,
    uri: filesystem_uri(final_path, ref),
    display_path: context_recovery_display_path(ref),
    local_path: Some(final_path),
  )
}

fn filesystem_uri(final_path: String, ref: String) -> String {
  case path.absolute(final_path) {
    Ok(absolute_path) -> "file://" <> uri_path_encode(absolute_path)
    Error(Nil) -> "artifact://filesystem/" <> ref
  }
}

fn uri_path_encode(path: String) -> String {
  uri_path_encode_bytes(bit_array.from_string(path), "")
}

fn uri_path_encode_bytes(bytes: BitArray, accumulator: String) -> String {
  case bytes {
    <<>> -> accumulator
    <<byte, rest:bytes>> ->
      uri_path_encode_bytes(rest, accumulator <> uri_path_encode_byte(byte))
    _ -> accumulator
  }
}

fn uri_path_encode_byte(byte: Int) -> String {
  case uri_path_safe_byte(byte) {
    True -> ascii_byte_to_string(byte)
    False -> "%" <> hex_digit(byte / 16) <> hex_digit(byte % 16)
  }
}

fn uri_path_safe_byte(byte: Int) -> Bool {
  byte == 47
  || byte == 45
  || byte == 46
  || byte == 95
  || byte == 126
  || { byte >= 48 && byte <= 57 }
  || { byte >= 65 && byte <= 90 }
  || { byte >= 97 && byte <= 122 }
}

fn ascii_byte_to_string(byte: Int) -> String {
  case string.utf_codepoint(byte) {
    Ok(codepoint) -> string.from_utf_codepoints([codepoint])
    Error(Nil) -> ""
  }
}

fn hex_digit(value: Int) -> String {
  case value {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "A"
    11 -> "B"
    12 -> "C"
    13 -> "D"
    14 -> "E"
    _ -> "F"
  }
}

fn validate_ref(ref: String) -> Result(Nil, ArtifactError) {
  let trimmed = string.trim(ref)
  case
    trimmed == ""
    || string.starts_with(trimmed, "/")
    || has_parent_segment(trimmed)
  {
    True -> Error(InvalidArtifactRef(ref))
    False -> Ok(Nil)
  }
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn ensure_parent(final_path: String) -> Result(Nil, ArtifactError) {
  let dir = dirname_or_original(final_path)
  simplifile.create_directory_all(dir)
  |> result.map_error(fn(error) {
    ArtifactIo(
      "create artifact directory: " <> simplifile.describe_error(error),
    )
  })
}

fn dirname_or_original(final_path: String) -> String {
  case path.dirname(final_path) {
    Ok(directory) -> directory
    Error(Nil) -> final_path
  }
}

pub fn write_atomic(
  final_path: String,
  contents: String,
) -> Result(Nil, ArtifactWriteError) {
  ffi_write_atomic(final_path, contents)
  |> result.map_error(fn(error) { raw_write_error("write_atomic", error) })
}

pub fn write_atomic_bytes(
  final_path: String,
  contents: BitArray,
) -> Result(Nil, ArtifactWriteError) {
  ffi_write_atomic_bytes(final_path, contents)
  |> result.map_error(fn(error) { raw_write_error("write_atomic_bytes", error) })
}

pub fn read_file_bytes(path: String) -> Result(BitArray, ArtifactError) {
  ffi_read_file(path)
  |> result.map_error(fn(error) {
    case split_tag(error) {
      #("read", "enoent") -> MissingStepArtifact(path)
      #(tag, detail) ->
        case detail != "" {
          True -> ArtifactIo("read file: " <> detail)
          False -> ArtifactIo("read file: " <> tag)
        }
    }
  })
}

pub fn write_immutable(
  final_path: String,
  contents: BitArray,
) -> Result(ImmutableWriteResult, ArtifactWriteError) {
  case ffi_write_immutable(final_path, contents) {
    Ok("written") -> Ok(ImmutableWritten)
    Ok("existing") -> Ok(ImmutableExisting)
    Ok("conflict") -> Ok(ImmutableConflict)
    Ok(status) ->
      Error(UnexpectedFfiFailure(
        "write_immutable",
        "unexpected status: " <> status,
      ))
    Error(error) -> Error(raw_write_error("write_immutable", error))
  }
}

pub fn restore_filesystem_artifact_bytes(
  workspace_root: String,
  ref: String,
  contents: BitArray,
) -> Result(Nil, ArtifactError) {
  use valid_ref <- result.try(validated_ref(ref))
  let final_path = path.join(artifact_root(workspace_root), valid_ref)
  use Nil <- result.try(ensure_parent(final_path))
  use Nil <- result.try(
    write_atomic_bytes(final_path, contents)
    |> result.map_error(fn(error) { ArtifactWriteFailed(error) }),
  )
  use final <- result.try(read_file_bytes(final_path))
  case final == contents {
    True -> Ok(Nil)
    False ->
      Error(ArtifactIo("restored bytes did not match for ref: " <> valid_ref))
  }
}

pub fn artifact_write_error_to_string(error: ArtifactWriteError) -> String {
  case error {
    InvalidPath(reason) -> "invalid artifact path: " <> reason
    OpenTempFailed(reason) -> "open temporary artifact failed: " <> reason
    WriteTempFailed(reason) -> "write temporary artifact failed: " <> reason
    SyncTempFailed(reason) -> "sync temporary artifact failed: " <> reason
    CloseTempFailed(reason) -> "close temporary artifact failed: " <> reason
    RenameFailed(reason) -> "rename artifact failed: " <> reason
    SyncParentFailed(reason) -> "sync artifact parent failed: " <> reason
    CleanupTempFailed(reason) -> "cleanup temporary artifact failed: " <> reason
    UnexpectedFfiFailure(function, detail) ->
      function <> " failed unexpectedly: " <> detail
  }
}

fn raw_write_error(function: String, error: String) -> ArtifactWriteError {
  let #(tag, detail) = split_tag(error)
  case tag {
    "invalid_path" -> InvalidPath(detail)
    "open_temp" -> OpenTempFailed(detail)
    "write_temp" -> WriteTempFailed(detail)
    "sync_temp" -> SyncTempFailed(detail)
    "close_temp" -> CloseTempFailed(detail)
    "rename" -> RenameFailed(detail)
    "sync_parent" -> SyncParentFailed(detail)
    "cleanup_temp" -> CleanupTempFailed(detail)
    "unexpected_ffi_failure" -> UnexpectedFfiFailure(function, detail)
    _ -> UnexpectedFfiFailure(function, error)
  }
}

fn split_tag(error: String) -> #(String, String) {
  case string.split_once(error, on: ":") {
    Ok(#(tag, detail)) -> #(tag, detail)
    Error(Nil) -> #(error, "")
  }
}

// nolint: stringly_typed_error -- leaf FFI returns tagged strings that write_atomic immediately normalizes into ArtifactWriteError.
@external(erlang, "scherzo_artifact_store_ffi", "write_atomic")
fn ffi_write_atomic(final_path: String, contents: String) -> Result(Nil, String)

// nolint: stringly_typed_error -- leaf FFI returns tagged strings that write_atomic_bytes immediately normalizes into ArtifactWriteError.
@external(erlang, "scherzo_artifact_store_ffi", "write_atomic")
fn ffi_write_atomic_bytes(
  final_path: String,
  contents: BitArray,
) -> Result(Nil, String)

// nolint: stringly_typed_error -- leaf FFI returns tagged strings that read_file_bytes immediately normalizes into ArtifactError.
@external(erlang, "scherzo_artifact_store_ffi", "read_file")
fn ffi_read_file(path: String) -> Result(BitArray, String)

// nolint: stringly_typed_error -- leaf FFI returns tagged strings that write_immutable immediately normalizes into ArtifactWriteError.
@external(erlang, "scherzo_artifact_store_ffi", "write_immutable")
fn ffi_write_immutable(
  final_path: String,
  contents: BitArray,
) -> Result(String, String)
