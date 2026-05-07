import gleam/bit_array
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{None}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/path
import scherzo/step_artifact
import scherzo/workflow_identity
import simplifile

pub type Store {
  Store(workspace_root: String)
}

pub type ArtifactRef {
  ArtifactRef(ref: String, sha256: String, bytes: Int)
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

pub type ArtifactError {
  ArtifactIo(String)
  ArtifactWriteFailed(ArtifactWriteError)
  MissingStepArtifact(String)
  CorruptStepArtifact(String)
  InvalidArtifactRef(String)
  DecodeArtifactFailed(String)
  DirectorySyncUnsupported(String)
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

pub fn new(workspace_root: String) -> Store {
  Store(workspace_root: workspace_root)
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
  use final_path <- result.try(resolve_ref_for_write(store, ref))
  use Nil <- result.try(ensure_parent(final_path))
  let bytes =
    stored_to_string(StoredArtifact(
      run_id: run_id,
      workflow_id: workflow_id,
      step_id: step_id,
      attempt_index: attempt_index,
      artifact: artifact,
    ))
  use Nil <- result.try(
    write_atomic(final_path, bytes)
    |> result.map_error(fn(error) { ArtifactWriteFailed(error) }),
  )
  use final <- result.try(
    simplifile.read(final_path)
    |> result.replace_error(MissingStepArtifact(ref)),
  )
  let sha = hash.sha256_hex(final)
  case final == bytes {
    True ->
      Ok(ArtifactRef(
        ref: ref,
        sha256: sha,
        bytes: bit_array.byte_size(bit_array.from_string(final)),
      ))
    False -> Error(CorruptStepArtifact(ref))
  }
}

pub fn read_step_artifact(
  store: Store,
  ref: String,
  expected_sha256: String,
) -> Result(step_artifact.StepArtifact, ArtifactError) {
  use contents <- result.try(read_step_artifact_contents(store, ref))
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
  use contents <- result.try(read_step_artifact_contents(store, ref))
  decode_step_artifact_contents(contents)
}

fn read_step_artifact_contents(
  store: Store,
  ref: String,
) -> Result(String, ArtifactError) {
  use final_path <- result.try(resolve_ref_for_read(store, ref))
  simplifile.read(final_path)
  |> result.map_error(fn(error) {
    case error {
      simplifile.Enoent -> MissingStepArtifact(ref)
      _ -> ArtifactIo("read artifact: " <> simplifile.describe_error(error))
    }
  })
}

fn decode_step_artifact_contents(
  contents: String,
) -> Result(step_artifact.StepArtifact, ArtifactError) {
  case decode_stored_string(contents) {
    Ok(stored) -> Ok(stored.artifact)
    Error(reason) -> Error(DecodeArtifactFailed(reason))
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

fn decode_stored_string(contents: String) -> Result(StoredArtifact, String) {
  case json.parse(contents, stored_decoder()) {
    Ok(stored) -> Ok(stored)
    Error(_) -> Error("invalid_stored_step_artifact")
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
  )
}

fn resolve_ref_for_write(
  store: Store,
  ref: String,
) -> Result(String, ArtifactError) {
  use Nil <- result.try(validate_ref(ref))
  let root = artifact_root(store)
  Ok(path.join(root, ref))
}

fn resolve_ref_for_read(
  store: Store,
  ref: String,
) -> Result(String, ArtifactError) {
  use Nil <- result.try(validate_ref(ref))
  Ok(path.join(artifact_root(store), ref))
}

fn artifact_root(store: Store) -> String {
  path.join(path.join(store.workspace_root, ".scherzo-state"), "artifacts")
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
  let dir = path.dirname(final_path) |> result.unwrap(final_path)
  simplifile.create_directory_all(dir)
  |> result.map_error(fn(error) {
    ArtifactIo(
      "create artifact directory: " <> simplifile.describe_error(error),
    )
  })
}

pub fn write_atomic(
  final_path: String,
  contents: String,
) -> Result(Nil, ArtifactWriteError) {
  ffi_write_atomic(final_path, contents)
  |> result.map_error(fn(error) { raw_write_error("write_atomic", error) })
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

@external(erlang, "scherzo_artifact_store_ffi", "write_atomic")
fn ffi_write_atomic(final_path: String, contents: String) -> Result(Nil, String)
