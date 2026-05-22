import gleam/bit_array
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/hash
import scherzo/state/artifact_store as state_artifact_store

pub type SnapshotStatus {
  SnapshotOk(display_path: String, local_path: Option(String))
  SnapshotProblem(code: String, message: String)
}

pub type ProjectionWarning {
  ProjectionWarning(code: String, ref: String, message: String)
}

pub type Read {
  ReadOk(contents: String, display_path: String, local_path: Option(String))
  ReadProblem(ProjectionWarning)
}

const max_inspection_snapshot_bytes = 262_144

pub fn read_text(
  store: state_artifact_store.Store,
  ref: String,
  expected_sha256: String,
  expected_bytes: Int,
) -> Read {
  case validate_snapshot_ref(ref, expected_sha256) {
    Error(warning) -> ReadProblem(warning)
    Ok(Nil) ->
      case validate_expected_bytes(ref, expected_bytes) {
        Error(warning) -> ReadProblem(warning)
        Ok(Nil) ->
          read_validated_snapshot(store, ref, expected_sha256, expected_bytes)
      }
  }
}

fn read_validated_snapshot(
  store: state_artifact_store.Store,
  ref: String,
  expected_sha256: String,
  expected_bytes: Int,
) -> Read {
  case state_artifact_store.read_artifact_unverified(store, ref) {
    Error(error) -> ReadProblem(snapshot_read_warning(ref, error))
    Ok(contents) -> {
      let actual_sha256 = hash.sha256_hex(contents)
      let actual_bytes = bit_array.byte_size(bit_array.from_string(contents))
      case actual_sha256 == expected_sha256, actual_bytes == expected_bytes {
        False, _ ->
          ReadProblem(ProjectionWarning(
            code: "snapshot_hash_mismatch",
            ref: ref,
            message: "snapshot hash does not match ledger record",
          ))
        True, False ->
          ReadProblem(ProjectionWarning(
            code: "snapshot_bytes_mismatch",
            ref: ref,
            message: "snapshot byte count does not match ledger record",
          ))
        True, True -> {
          let location = state_artifact_store.location(store, ref)
          case location {
            Ok(location) ->
              ReadOk(
                contents: contents,
                display_path: location.display_path,
                local_path: location.local_path,
              )
            Error(error) -> ReadProblem(snapshot_read_warning(ref, error))
          }
        }
      }
    }
  }
}

pub fn status(read: Read) -> SnapshotStatus {
  case read {
    ReadOk(_, display_path, local_path) -> SnapshotOk(display_path, local_path)
    ReadProblem(warning) -> SnapshotProblem(warning.code, warning.message)
  }
}

pub fn warning_to_json(warning: ProjectionWarning) -> json.Json {
  json.object([
    #("code", json.string(warning.code)),
    #("ref", json.string(warning.ref)),
    #("message", json.string(warning.message)),
  ])
}

pub fn status_to_json(status: SnapshotStatus) -> json.Json {
  case status {
    SnapshotOk(display_path, local_path) ->
      json.object([
        #("status", json.string("ok")),
        #("display_path", json.string(display_path)),
        #("local_path", option_string_to_json(local_path)),
      ])
    SnapshotProblem(code, message) ->
      json.object([
        #("status", json.string("error")),
        #("code", json.string(code)),
        #("message", json.string(message)),
      ])
  }
}

fn validate_expected_bytes(
  ref: String,
  expected_bytes: Int,
) -> Result(Nil, ProjectionWarning) {
  case expected_bytes < 0 {
    True ->
      Error(ProjectionWarning(
        code: "snapshot_bytes_invalid",
        ref: ref,
        message: "ledger snapshot byte count is negative",
      ))
    False ->
      case expected_bytes > max_inspection_snapshot_bytes {
        True ->
          Error(ProjectionWarning(
            code: "snapshot_too_large",
            ref: ref,
            message: "snapshot exceeds bounded inspection read limit",
          ))
        False -> Ok(Nil)
      }
  }
}

fn validate_snapshot_ref(
  ref: String,
  expected_sha256: String,
) -> Result(Nil, ProjectionWarning) {
  case string.length(expected_sha256) == 64 && is_lower_hex(expected_sha256) {
    False ->
      Error(ProjectionWarning(
        code: "snapshot_sha256_invalid",
        ref: ref,
        message: "ledger snapshot sha256 is not a lowercase SHA-256 hex digest",
      ))
    True ->
      case ref == expected_snapshot_ref(expected_sha256) {
        True -> Ok(Nil)
        False ->
          Error(ProjectionWarning(
            code: "snapshot_ref_mismatch",
            ref: ref,
            message: "snapshot ref does not match ledger snapshot sha256",
          ))
      }
  }
}

fn expected_snapshot_ref(sha256: String) -> String {
  "workstream-artifacts/sha256/" <> sha256 <> ".json"
}

fn snapshot_read_warning(
  ref: String,
  error: state_artifact_store.ArtifactError,
) -> ProjectionWarning {
  case error {
    state_artifact_store.MissingStepArtifact(_) ->
      ProjectionWarning(
        code: "snapshot_missing",
        ref: ref,
        message: "snapshot artifact is missing from local artifact store",
      )
    state_artifact_store.CorruptStepArtifact(_) ->
      ProjectionWarning(
        code: "snapshot_corrupt",
        ref: ref,
        message: "snapshot artifact is corrupt",
      )
    state_artifact_store.InvalidArtifactRef(_) ->
      ProjectionWarning(
        code: "snapshot_ref_invalid",
        ref: ref,
        message: "snapshot ref is invalid",
      )
    _ ->
      ProjectionWarning(
        code: "snapshot_read_failed",
        ref: ref,
        message: describe_artifact_error(error),
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

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}
