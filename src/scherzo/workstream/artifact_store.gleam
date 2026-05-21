import gleam/bit_array
import gleam/list
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/path
import scherzo/state/artifact_store as state_artifact_store

pub type Snapshot {
  Snapshot(
    ref: String,
    sha256: String,
    bytes: Int,
    original_path: String,
    media_type: String,
  )
}

pub type SnapshotError {
  InvalidOriginalPath
  MissingSourcePath(path: String)
  SourcePathEscapesRepo(path: String)
  InvalidExistingArtifactRef(ref: String)
  MissingExistingArtifact(ref: String)
  ExistingArtifactMismatch(ref: String)
  SnapshotIo(reason: String)
  SnapshotWriteConflict(ref: String)
  CorruptSnapshot(ref: String)
}

pub fn snapshot_repository_path(
  store: state_artifact_store.Store,
  repo_root: String,
  original_path: String,
  media_type: String,
) -> Result(Snapshot, SnapshotError) {
  use contents <- result.try(read_repository_path(repo_root, original_path))
  snapshot_bytes(store, original_path, media_type, contents)
}

pub fn inspect_repository_path(
  repo_root: String,
  original_path: String,
  media_type: String,
) -> Result(Snapshot, SnapshotError) {
  use contents <- result.try(read_repository_path(repo_root, original_path))
  let sha256 = hash.sha256_hex_bytes(contents)
  Ok(Snapshot(
    ref: snapshot_ref(sha256),
    sha256: sha256,
    bytes: bit_array.byte_size(contents),
    original_path: original_path,
    media_type: media_type,
  ))
}

fn read_repository_path(
  repo_root: String,
  original_path: String,
) -> Result(BitArray, SnapshotError) {
  use Nil <- result.try(validate_original_path(original_path))
  use repo_root_abs <- result.try(normalize_root(repo_root))
  let source_path = path.join(repo_root, original_path)
  case path.realpath(source_path) {
    Error(_) -> Error(MissingSourcePath(original_path))
    Ok(source_path_abs) ->
      case path.contains(repo_root_abs, source_path_abs) {
        False -> Error(SourcePathEscapesRepo(original_path))
        True ->
          state_artifact_store.read_file_bytes(source_path_abs)
          |> result.map_error(map_source_read_error(original_path))
      }
  }
}

pub fn snapshot_bytes(
  store: state_artifact_store.Store,
  original_path: String,
  media_type: String,
  contents: BitArray,
) -> Result(Snapshot, SnapshotError) {
  use Nil <- result.try(validate_original_path(original_path))
  let sha256 = hash.sha256_hex_bytes(contents)
  let bytes = bit_array.byte_size(contents)
  let ref = snapshot_ref(sha256)
  use write_result <- result.try(
    state_artifact_store.write_immutable_artifact_bytes(store, ref, contents)
    |> result.map_error(map_store_error),
  )
  case write_result {
    state_artifact_store.ImmutableConflict -> Error(SnapshotWriteConflict(ref))
    state_artifact_store.ImmutableExisting
    | state_artifact_store.ImmutableWritten ->
      Ok(Snapshot(
        ref: ref,
        sha256: sha256,
        bytes: bytes,
        original_path: original_path,
        media_type: media_type,
      ))
  }
}

pub fn snapshot_existing_artifact_ref(
  store: state_artifact_store.Store,
  artifact_ref: String,
  expected_sha256: String,
  expected_bytes: Int,
  original_ref: String,
  media_type: String,
) -> Result(Snapshot, SnapshotError) {
  use Nil <- result.try(validate_original_path(original_ref))
  use contents <- result.try(
    state_artifact_store.read_artifact_bytes_unverified(store, artifact_ref)
    |> result.map_error(map_existing_artifact_error(artifact_ref)),
  )
  let actual_sha256 = hash.sha256_hex_bytes(contents)
  let actual_bytes = bit_array.byte_size(contents)
  case actual_sha256 == expected_sha256 && actual_bytes == expected_bytes {
    False -> Error(ExistingArtifactMismatch(artifact_ref))
    True -> snapshot_bytes(store, original_ref, media_type, contents)
  }
}

pub fn read_snapshot(
  store: state_artifact_store.Store,
  ref: String,
  expected_sha256: String,
) -> Result(BitArray, SnapshotError) {
  use Nil <- result.try(validate_snapshot_ref(ref, expected_sha256))
  use contents <- result.try(
    state_artifact_store.read_artifact_bytes_unverified(store, ref)
    |> result.map_error(fn(error) {
      case error {
        state_artifact_store.MissingStepArtifact(_) -> MissingSourcePath(ref)
        _ -> SnapshotIo(describe_artifact_error(error))
      }
    }),
  )
  let actual_sha256 = hash.sha256_hex_bytes(contents)
  case actual_sha256 == expected_sha256 {
    True -> Ok(contents)
    False -> Error(CorruptSnapshot(ref))
  }
}

fn validate_original_path(original_path: String) -> Result(Nil, SnapshotError) {
  let trimmed = string.trim(original_path)
  case
    trimmed == ""
    || string.starts_with(trimmed, "/")
    || trimmed == ".."
    || string.starts_with(trimmed, "../")
    || string.ends_with(trimmed, "/..")
    || string.contains(trimmed, "/../")
  {
    True -> Error(InvalidOriginalPath)
    False -> Ok(Nil)
  }
}

fn normalize_root(root: String) -> Result(String, SnapshotError) {
  case path.realpath(root) {
    Ok(resolved) -> Ok(resolved)
    Error(Nil) -> Error(SnapshotIo("path normalization failed"))
  }
}

fn validate_snapshot_ref(
  ref: String,
  expected_sha256: String,
) -> Result(Nil, SnapshotError) {
  case
    string.length(expected_sha256) == 64
    && is_lower_hex(expected_sha256)
    && ref == snapshot_ref(expected_sha256)
  {
    True -> Ok(Nil)
    False -> Error(CorruptSnapshot(ref))
  }
}

fn snapshot_ref(sha256: String) -> String {
  "workstream-artifacts/sha256/" <> sha256 <> ".json"
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

fn map_source_read_error(
  original_path: String,
) -> fn(state_artifact_store.ArtifactError) -> SnapshotError {
  fn(error) {
    case error {
      state_artifact_store.MissingStepArtifact(_) ->
        MissingSourcePath(original_path)
      _ -> SnapshotIo(describe_artifact_error(error))
    }
  }
}

fn map_existing_artifact_error(
  artifact_ref: String,
) -> fn(state_artifact_store.ArtifactError) -> SnapshotError {
  fn(error) {
    case error {
      state_artifact_store.InvalidArtifactRef(_) ->
        InvalidExistingArtifactRef(artifact_ref)
      state_artifact_store.MissingStepArtifact(_) ->
        MissingExistingArtifact(artifact_ref)
      state_artifact_store.CorruptStepArtifact(_) ->
        ExistingArtifactMismatch(artifact_ref)
      _ -> SnapshotIo(describe_artifact_error(error))
    }
  }
}

fn map_store_error(error: state_artifact_store.ArtifactError) -> SnapshotError {
  SnapshotIo(describe_artifact_error(error))
}

fn describe_artifact_error(
  error: state_artifact_store.ArtifactError,
) -> String {
  case error {
    state_artifact_store.ArtifactIo(reason) -> reason
    state_artifact_store.ArtifactWriteFailed(write_error) ->
      state_artifact_store.artifact_write_error_to_string(write_error)
    state_artifact_store.MissingStepArtifact(ref) -> "missing artifact: " <> ref
    state_artifact_store.CorruptStepArtifact(ref) -> "corrupt artifact: " <> ref
    state_artifact_store.InvalidArtifactRef(ref) ->
      "invalid artifact ref: " <> ref
    state_artifact_store.DecodeArtifactFailed(reason) -> reason
    state_artifact_store.DirectorySyncUnsupported(reason) -> reason
  }
}
