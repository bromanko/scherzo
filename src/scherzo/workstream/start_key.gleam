import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/hash
import scherzo/state/projection
import scherzo/workflow_identity
import scherzo/workstream/artifact_store

const id_hash_chars = 12

pub fn derive_idempotency_key(
  workstream_id: String,
  action_id: String,
  input_hashes: List(#(String, String)),
  gate_decision_ids: List(String),
) -> String {
  let input_keys =
    input_hashes
    |> list.map(fn(input) {
      let #(name, sha256) = input
      frame_pair(name, sha256)
    })
    |> list.sort(by: string.compare)
  let gate_keys =
    gate_decision_ids
    |> list.sort(by: string.compare)
    |> list.map(frame_string)
  hash.sha256_hex(
    "workstream_start:v2"
    <> frame_string(workstream_id)
    <> frame_string(action_id)
    <> frame_list(input_keys)
    <> frame_list(gate_keys),
  )
}

fn frame_pair(name: String, sha256: String) -> String {
  frame_string(name) <> frame_string(sha256)
}

fn frame_list(values: List(String)) -> String {
  int.to_string(list.length(values))
  <> "["
  <> string.join(values, with: "")
  <> "]"
}

fn frame_string(value: String) -> String {
  int.to_string(string.length(value)) <> ":" <> value
}

pub fn existing_start(
  projected: projection.Projection,
  workstream_id: String,
  action_id: String,
  idempotency_key: String,
) -> Option(projection.WorkstreamPhaseRun) {
  case dict.get(projected.workstreams, workstream_id) {
    Error(Nil) -> None
    Ok(workstream) ->
      case
        workstream.queued_phase_runs
        |> dict.values
        |> list.find(fn(run) {
          run.action_id == action_id && run.idempotency_key == idempotency_key
        })
      {
        Ok(run) -> Some(run)
        Error(Nil) -> None
      }
  }
}

pub fn conflicting_start(
  projected: projection.Projection,
  workstream_id: String,
  action_id: String,
  idempotency_key: String,
) -> Option(projection.WorkstreamPhaseRun) {
  case dict.get(projected.workstreams, workstream_id) {
    Error(Nil) -> None
    Ok(workstream) ->
      case
        workstream.queued_phase_runs
        |> dict.values
        |> list.find(fn(run) {
          run.action_id == action_id && run.idempotency_key != idempotency_key
        })
      {
        Ok(run) -> Some(run)
        Error(Nil) -> None
      }
  }
}

pub fn input_bundle_artifact_id(
  action_id: String,
  idempotency_key: String,
) -> String {
  "input-bundle:"
  <> workflow_identity.safe_component(action_id, "action")
  <> ":"
  <> string.slice(idempotency_key, 0, id_hash_chars)
}

pub fn input_bundle_original_path(artifact_id: String) -> String {
  "workstream/input-bundles/"
  <> workflow_identity.safe_component(artifact_id, "input-bundle")
  <> ".json"
}

pub fn manual_artifact_id(name: String, sha256: String) -> String {
  "manual:"
  <> workflow_identity.safe_component(name, "artifact")
  <> ":"
  <> string.slice(sha256, 0, id_hash_chars)
}

pub fn phase_run_id(idempotency_key: String) -> String {
  "workstream-phase-" <> hash.short_sha256_hex(idempotency_key, id_hash_chars)
}

pub fn snapshot_ref(sha256: String) -> String {
  "workstream-artifacts/sha256/" <> sha256 <> ".json"
}

pub fn media_type_for_path(path: String) -> String {
  case string.ends_with(path, ".json") {
    True -> "application/json"
    False ->
      case
        string.ends_with(path, ".md") || string.ends_with(path, ".markdown")
      {
        True -> "text/markdown"
        False -> "application/octet-stream"
      }
  }
}

pub fn valid_snapshot_ref(ref: String, expected_sha256: String) -> Bool {
  string.length(expected_sha256) == 64
  && is_lower_hex(expected_sha256)
  && ref == snapshot_ref(expected_sha256)
}

pub fn describe_snapshot_error(err: artifact_store.SnapshotError) -> String {
  case err {
    artifact_store.InvalidOriginalPath -> "invalid original path"
    artifact_store.MissingSourcePath(path) -> "missing source path: " <> path
    artifact_store.SourcePathEscapesRepo(path) ->
      "source path escapes repository: " <> path
    artifact_store.InvalidExistingArtifactRef(ref) ->
      "invalid existing artifact ref: " <> ref
    artifact_store.MissingExistingArtifact(ref) ->
      "missing existing artifact: " <> ref
    artifact_store.ExistingArtifactMismatch(ref) ->
      "existing artifact mismatch: " <> ref
    artifact_store.SnapshotIo(reason) -> reason
    artifact_store.SnapshotWriteConflict(ref) ->
      "snapshot write conflict: " <> ref
    artifact_store.CorruptSnapshot(ref) -> "corrupt snapshot: " <> ref
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
