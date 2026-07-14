import gleam/list
import gleam/result
import gleam/string
import scherzo/path
import simplifile

pub type FileFingerprint {
  Missing
  FileFingerprint(
    size: Int,
    modified_at_ms: Int,
    changed_at_ms: Int,
    inode: Int,
  )
}

pub type ArchiveSegmentFingerprint {
  ArchiveSegmentFingerprint(name: String, metadata: FileFingerprint)
}

pub type LedgerFingerprint {
  LedgerFingerprint(
    snapshot: FileFingerprint,
    current: FileFingerprint,
    archive_segments: List(ArchiveSegmentFingerprint),
  )
}

pub fn capture(
  snapshot_path: String,
  current_path: String,
  archive_dir: String,
) -> Result(LedgerFingerprint, String) {
  use snapshot <- result.try(file_fingerprint(
    snapshot_path,
    "inspect ledger snapshot",
  ))
  use current <- result.try(file_fingerprint(
    current_path,
    "inspect current ledger",
  ))
  use archive_segments <- result.try(archive_segment_fingerprints(archive_dir))
  Ok(LedgerFingerprint(
    snapshot: snapshot,
    current: current,
    archive_segments: archive_segments,
  ))
}

fn archive_segment_fingerprints(
  archive_dir: String,
) -> Result(List(ArchiveSegmentFingerprint), String) {
  case simplifile.read_directory(archive_dir) {
    Ok(entries) ->
      entries
      |> list.filter(fn(entry) {
        string.starts_with(entry, "segment-")
        && string.ends_with(entry, ".jsonl")
      })
      |> list.sort(by: string.compare)
      |> list.try_map(fn(entry) {
        use metadata <- result.try(file_fingerprint(
          path.join(archive_dir, entry),
          "inspect ledger archive segment",
        ))
        Ok(ArchiveSegmentFingerprint(name: entry, metadata: metadata))
      })
    Error(simplifile.Enoent) | Error(simplifile.Enotdir) -> Ok([])
    Error(error) -> Error(file_error("read ledger archive directory", error))
  }
}

fn file_fingerprint(
  file_path: String,
  operation: String,
) -> Result(FileFingerprint, String) {
  case ffi_file_fingerprint(file_path) {
    Ok(#(False, _, _, _, _)) -> Ok(Missing)
    Ok(#(True, size, modified_at_ms, changed_at_ms, inode)) ->
      Ok(FileFingerprint(
        size: size,
        modified_at_ms: modified_at_ms,
        changed_at_ms: changed_at_ms,
        inode: inode,
      ))
    Error(error) -> Error(operation <> ": " <> error)
  }
}

fn file_error(operation: String, error: simplifile.FileError) -> String {
  operation <> ": " <> simplifile.describe_error(error)
}

@external(erlang, "scherzo_state_ffi", "file_fingerprint")
fn ffi_file_fingerprint(
  path: String,
) -> Result(#(Bool, Int, Int, Int, Int), String)
