import gleam/dict
import gleam/list
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/path
import simplifile

pub type Lookup {
  Pruned
  Unknown
}

pub type ArchiveIndexError {
  ArchiveIndexUnavailable(path: String, reason: String)
}

pub fn root(archive_dir: String) -> String {
  path.join(archive_dir, "pruned-runs/v1")
}

pub fn marker_path(archive_dir: String, run_id: String) -> String {
  marker_path_with_hash(archive_dir, run_id, hash.sha256_hex)
}

pub fn marker_path_with_hash(
  archive_dir: String,
  run_id: String,
  hash_run_id: fn(String) -> String,
) -> String {
  let digest = hash_run_id(run_id)
  path.join(
    path.join(
      path.join(root(archive_dir), string.slice(digest, 0, 2)),
      string.slice(digest, 2, 2),
    ),
    digest,
  )
}

/// Look up one exact id with a fixed path calculation. This function never
/// enumerates or opens JSONL archive segments.
pub fn lookup(
  archive_dir: String,
  run_id: String,
) -> Result(Lookup, ArchiveIndexError) {
  lookup_with_hash(archive_dir, run_id, hash.sha256_hex)
}

pub fn lookup_with_hash(
  archive_dir: String,
  run_id: String,
  hash_run_id: fn(String) -> String,
) -> Result(Lookup, ArchiveIndexError) {
  lookup_with_hash_observed(archive_dir, run_id, hash_run_id, fn(_) { Nil })
}

/// Look up one id while reporting the sole path opened by the lookup. This is
/// an observability seam for proving that archive JSONL is never consulted.
pub fn lookup_with_hash_observed(
  archive_dir: String,
  run_id: String,
  hash_run_id: fn(String) -> String,
  observe_open: fn(String) -> Nil,
) -> Result(Lookup, ArchiveIndexError) {
  let marker = marker_path_with_hash(archive_dir, run_id, hash_run_id)
  observe_open(marker)
  case simplifile.read(marker) {
    Error(simplifile.Enoent) -> Ok(Unknown)
    Error(error) -> Error(unavailable(marker, "read", error))
    Ok(contents) ->
      case decode_marker(contents) {
        Error(reason) -> Error(ArchiveIndexUnavailable(marker, reason))
        Ok(ids) ->
          case list.contains(ids, run_id) {
            True -> Ok(Pruned)
            False -> Ok(Unknown)
          }
      }
  }
}

pub fn write_run_ids(
  archive_dir: String,
  run_ids: List(String),
) -> Result(Nil, ArchiveIndexError) {
  write_run_ids_with_hash(archive_dir, run_ids, hash.sha256_hex)
}

pub fn write_run_ids_with_hash(
  archive_dir: String,
  run_ids: List(String),
  hash_run_id: fn(String) -> String,
) -> Result(Nil, ArchiveIndexError) {
  run_ids
  |> unique_sorted
  |> list.fold(Ok(Nil), fn(acc, run_id) {
    use Nil <- result.try(acc)
    write_one(archive_dir, run_id, hash_run_id)
  })
}

fn write_one(
  archive_dir: String,
  run_id: String,
  hash_run_id: fn(String) -> String,
) -> Result(Nil, ArchiveIndexError) {
  let marker = marker_path_with_hash(archive_dir, run_id, hash_run_id)
  let parent = path.dirname(marker) |> result.unwrap(root(archive_dir))
  use Nil <- result.try(
    simplifile.create_directory_all(parent)
    |> result.map_error(fn(error) {
      unavailable(marker, "create directory", error)
    }),
  )
  use existing <- result.try(read_existing_marker(marker))
  let ids = unique_sorted([run_id, ..existing])
  case ids == existing {
    True -> Ok(Nil)
    False -> atomic_write(marker, encode_marker(ids))
  }
}

fn read_existing_marker(
  marker: String,
) -> Result(List(String), ArchiveIndexError) {
  case simplifile.read(marker) {
    Error(simplifile.Enoent) -> Ok([])
    Error(error) -> Error(unavailable(marker, "read", error))
    Ok(contents) ->
      decode_marker(contents)
      |> result.map_error(fn(reason) { ArchiveIndexUnavailable(marker, reason) })
  }
}

fn atomic_write(
  marker: String,
  contents: String,
) -> Result(Nil, ArchiveIndexError) {
  let temporary = marker <> ".tmp"
  use Nil <- result.try(
    simplifile.write(temporary, contents)
    |> result.map_error(fn(error) {
      unavailable(marker, "write temporary marker", error)
    }),
  )
  simplifile.rename(temporary, marker)
  |> result.map_error(fn(error) { unavailable(marker, "rename marker", error) })
}

fn encode_marker(ids: List(String)) -> String {
  ids |> string.join(with: "\n") |> fn(contents) { contents <> "\n" }
}

// nolint: stringly_typed_error -- private parser reason is wrapped in ArchiveIndexUnavailable
fn decode_marker(contents: String) -> Result(List(String), String) {
  let lines =
    contents
    |> string.split(on: "\n")
    |> list.filter(fn(line) { line != "" })
  case contents == "" || !string.ends_with(contents, "\n") {
    True -> Error("malformed pruned-run marker")
    False ->
      case
        list.any(lines, fn(line) { string.trim(line) != line || line == "" })
      {
        True -> Error("malformed pruned-run marker")
        False ->
          case lines == unique_sorted(lines) {
            True -> Ok(lines)
            False -> Error("pruned-run marker ids are not sorted and unique")
          }
      }
  }
}

fn unique_sorted(values: List(String)) -> List(String) {
  values
  |> list.fold(dict.new(), fn(seen, value) { dict.insert(seen, value, True) })
  |> dict.keys
  |> list.sort(by: string.compare)
}

fn unavailable(
  marker: String,
  operation: String,
  error: simplifile.FileError,
) -> ArchiveIndexError {
  ArchiveIndexUnavailable(
    marker,
    operation <> ": " <> simplifile.describe_error(error),
  )
}
