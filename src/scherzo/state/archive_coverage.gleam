import gleam/bit_array
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/path
import simplifile

pub const schema_version = 1

pub type Segment {
  Segment(number: Int, name: String, bytes: Int, sha256: String)
}

pub type Manifest {
  Manifest(segments: List(Segment))
}

pub type CoverageError {
  CoverageUnavailable(reason: String)
  CoverageIncomplete(reason: String)
}

pub fn manifest_path(archive_dir: String) -> String {
  path.join(archive_dir, "coverage.json")
}

pub fn manifest_exists(archive_dir: String) -> Result(Bool, CoverageError) {
  case simplifile.is_file(manifest_path(archive_dir)) {
    Ok(exists) -> Ok(exists)
    Error(simplifile.Enoent) -> Ok(False)
    Error(error) -> Error(unavailable("inspect coverage manifest", error))
  }
}

/// Return archive segments in numeric order, rejecting duplicate numbers and
/// malformed names instead of relying on lexicographic filesystem order.
pub fn segment_paths_numeric(
  archive_dir: String,
) -> Result(List(#(Int, String, String)), CoverageError) {
  case simplifile.read_directory(archive_dir) {
    Error(simplifile.Enoent) -> Ok([])
    Error(error) -> Error(unavailable("read archive directory", error))
    Ok(entries) -> {
      let names =
        entries
        |> list.filter(fn(name) {
          string.starts_with(name, "segment-")
          && string.ends_with(name, ".jsonl")
        })
      use numbered <- result.try(parse_segment_names(names, []))
      let ordered =
        numbered |> list.sort(by: fn(a, b) { int.compare(a.0, b.0) })
      use Nil <- result.try(ensure_unique_numbers(ordered, None))
      ordered
      |> list.map(fn(entry) {
        #(entry.0, entry.1, path.join(archive_dir, entry.1))
      })
      |> Ok
    }
  }
}

pub fn build(archive_dir: String) -> Result(Manifest, CoverageError) {
  use paths <- result.try(segment_paths_numeric(archive_dir))
  use segments <- result.try(
    list.try_map(paths, fn(entry) {
      let #(number, name, segment_path) = entry
      case simplifile.read(segment_path) {
        Error(error) -> Error(unavailable("read " <> name, error))
        Ok(contents) ->
          Ok(Segment(
            number: number,
            name: name,
            bytes: contents |> bit_array.from_string |> bit_array.byte_size,
            sha256: hash.sha256_hex(contents),
          ))
      }
    }),
  )
  Ok(Manifest(segments))
}

pub fn write(
  archive_dir: String,
  manifest: Manifest,
) -> Result(Nil, CoverageError) {
  let target = manifest_path(archive_dir)
  let temporary = target <> ".tmp"
  use Nil <- result.try(
    simplifile.create_directory_all(archive_dir)
    |> result.map_error(fn(error) {
      unavailable("create archive directory", error)
    }),
  )
  use Nil <- result.try(
    simplifile.write(temporary, encode(manifest) <> "\n")
    |> result.map_error(fn(error) {
      unavailable("write coverage manifest", error)
    }),
  )
  simplifile.rename(temporary, target)
  |> result.map_error(fn(error) {
    unavailable("rename coverage manifest", error)
  })
}

pub fn read(archive_dir: String) -> Result(Manifest, CoverageError) {
  let target = manifest_path(archive_dir)
  case simplifile.read(target) {
    Error(simplifile.Enoent) ->
      Error(CoverageIncomplete("coverage manifest is absent"))
    Error(error) -> Error(unavailable("read coverage manifest", error))
    Ok(contents) ->
      case json.parse(contents, manifest_decoder()) {
        Error(_) -> Error(CoverageIncomplete("coverage manifest is malformed"))
        Ok(manifest) -> Ok(manifest)
      }
  }
}

pub fn verify(
  archive_dir: String,
  manifest: Manifest,
) -> Result(Nil, CoverageError) {
  use actual <- result.try(build(archive_dir))
  case actual == manifest {
    True -> Ok(Nil)
    False ->
      Error(CoverageIncomplete(
        "archive segment names, sizes, order, or hashes do not match coverage manifest",
      ))
  }
}

pub fn verify_stored(archive_dir: String) -> Result(Manifest, CoverageError) {
  use manifest <- result.try(read(archive_dir))
  use Nil <- result.try(verify(archive_dir, manifest))
  Ok(manifest)
}

pub fn encode(manifest: Manifest) -> String {
  json.object([
    #("schema_version", json.int(schema_version)),
    #("kind", json.string("ledger_archive_coverage")),
    #("segments", json.array(manifest.segments, of: segment_json)),
  ])
  |> json.to_string
}

fn segment_json(segment: Segment) -> json.Json {
  json.object([
    #("number", json.int(segment.number)),
    #("name", json.string(segment.name)),
    #("bytes", json.int(segment.bytes)),
    #("sha256", json.string(segment.sha256)),
  ])
}

fn manifest_decoder() -> decode.Decoder(Manifest) {
  use version <- decode.field("schema_version", decode.int)
  use kind <- decode.field("kind", decode.string)
  use segments <- decode.field("segments", decode.list(segment_decoder()))
  case version == schema_version && kind == "ledger_archive_coverage" {
    True -> decode.success(Manifest(segments))
    False -> decode.failure(Manifest([]), "unsupported coverage manifest")
  }
}

fn segment_decoder() -> decode.Decoder(Segment) {
  use number <- decode.field("number", decode.int)
  use name <- decode.field("name", decode.string)
  use bytes <- decode.field("bytes", decode.int)
  use sha256 <- decode.field("sha256", decode.string)
  case
    number > 0
    && bytes >= 0
    && segment_name(number) == name
    && string.length(sha256) == 64
  {
    True -> decode.success(Segment(number, name, bytes, sha256))
    False -> decode.failure(Segment(0, "", 0, ""), "invalid coverage segment")
  }
}

fn parse_segment_names(
  names: List(String),
  acc: List(#(Int, String)),
) -> Result(List(#(Int, String)), CoverageError) {
  case names {
    [] -> Ok(acc)
    [name, ..rest] ->
      case parse_segment_number(name) {
        Error(Nil) ->
          Error(CoverageIncomplete("invalid archive segment name: " <> name))
        Ok(number) -> parse_segment_names(rest, [#(number, name), ..acc])
      }
  }
}

fn parse_segment_number(name: String) -> Result(Int, Nil) {
  let value =
    name
    |> string.drop_start(string.length("segment-"))
    |> string.drop_end(string.length(".jsonl"))
  case int.parse(value) {
    Ok(number) ->
      case number > 0 && segment_name(number) == name {
        True -> Ok(number)
        False -> Error(Nil)
      }
    Error(Nil) -> Error(Nil)
  }
}

fn segment_name(number: Int) -> String {
  "segment-" <> int.to_string(number) <> ".jsonl"
}

fn ensure_unique_numbers(
  entries: List(#(Int, String)),
  previous: Option(Int),
) -> Result(Nil, CoverageError) {
  case entries {
    [] -> Ok(Nil)
    [entry, ..rest] ->
      case previous == Some(entry.0) {
        True ->
          Error(CoverageIncomplete(
            "duplicate archive segment number " <> int.to_string(entry.0),
          ))
        False -> ensure_unique_numbers(rest, Some(entry.0))
      }
  }
}

fn unavailable(
  operation: String,
  error: simplifile.FileError,
) -> CoverageError {
  CoverageUnavailable(operation <> ": " <> simplifile.describe_error(error))
}
