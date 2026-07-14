import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/string
import scherzo/hash
import scherzo/state/record

pub const snapshot_metadata_field = "record_id_index_metadata"

pub const snapshot_metadata_version = 1

pub type RecordIndex {
  RecordIndex(entries: Dict(String, RecordIndexEntry))
}

pub type RecordIndexEntry {
  RecordIndexEntry(
    record_id: String,
    body_sha256: String,
    at_ms: Int,
    storage: String,
  )
}

pub type InsertResult {
  Inserted(RecordIndex)
  Duplicate(existing: RecordIndexEntry)
  Conflict(existing: RecordIndexEntry)
}

pub fn new() -> RecordIndex {
  RecordIndex(dict.new())
}

pub fn size(index: RecordIndex) -> Int {
  let RecordIndex(entries) = index
  dict.size(entries)
}

pub fn get(
  index: RecordIndex,
  record_id: String,
) -> Result(RecordIndexEntry, Nil) {
  let RecordIndex(entries) = index
  dict.get(entries, record_id)
}

pub fn to_list(index: RecordIndex) -> List(RecordIndexEntry) {
  let RecordIndex(entries) = index
  entries |> dict.values |> list.sort(by: compare_entries)
}

pub fn body_sha256(ledger_record: record.LedgerRecord) -> String {
  record.body_to_string(ledger_record.body) |> hash.sha256_hex
}

pub fn entry_for_record(
  ledger_record: record.LedgerRecord,
  storage: String,
) -> RecordIndexEntry {
  RecordIndexEntry(
    record_id: ledger_record.record_id,
    body_sha256: body_sha256(ledger_record),
    at_ms: ledger_record.at_ms,
    storage: storage,
  )
}

pub fn insert(
  index: RecordIndex,
  ledger_record: record.LedgerRecord,
  storage: String,
) -> InsertResult {
  insert_entry(index, entry_for_record(ledger_record, storage))
}

pub fn insert_entry(
  index: RecordIndex,
  entry: RecordIndexEntry,
) -> InsertResult {
  let RecordIndex(entries) = index
  case dict.get(entries, entry.record_id) {
    Ok(existing) ->
      case existing.body_sha256 == entry.body_sha256 {
        True -> Duplicate(existing)
        False -> Conflict(existing)
      }
    Error(Nil) ->
      Inserted(RecordIndex(dict.insert(entries, entry.record_id, entry)))
  }
}

pub fn snapshot_metadata_json(index: RecordIndex) -> json.Json {
  json.object([
    #("schema_version", json.int(snapshot_metadata_version)),
    #("entries", json.array(to_list(index), of: entry_to_json)),
  ])
}

pub fn decode_snapshot_metadata(
  contents: String,
) -> Result(Option(RecordIndex), String) {
  case json.parse(contents, metadata_decoder()) {
    Ok(None) -> Ok(None)
    Ok(Some(#(version, entries))) ->
      case version == snapshot_metadata_version {
        True -> Ok(Some(entries_to_index(entries)))
        False -> Error("invalid record-id metadata")
      }
    Error(_) -> Error("invalid record-id metadata")
  }
}

fn metadata_decoder() -> decode.Decoder(Option(#(Int, List(RecordIndexEntry)))) {
  use metadata <- decode.optional_field(
    snapshot_metadata_field,
    None,
    decode.optional(metadata_body_decoder()),
  )
  decode.success(metadata)
}

fn metadata_body_decoder() -> decode.Decoder(#(Int, List(RecordIndexEntry))) {
  use version <- decode.field("schema_version", decode.int)
  use entries <- decode.field("entries", decode.list(of: entry_decoder()))
  decode.success(#(version, entries))
}

fn entry_decoder() -> decode.Decoder(RecordIndexEntry) {
  use record_id <- decode.field("record_id", decode.string)
  use body_sha256 <- decode.field("body_sha256", decode.string)
  use at_ms <- decode.field("at_ms", decode.int)
  use storage <- decode.optional_field("storage", "snapshot", decode.string)
  decode.success(RecordIndexEntry(record_id, body_sha256, at_ms, storage))
}

fn entries_to_index(entries: List(RecordIndexEntry)) -> RecordIndex {
  list.fold(entries, new(), fn(index, entry) {
    let RecordIndex(index_entries) = index
    RecordIndex(dict.insert(index_entries, entry.record_id, entry))
  })
}

fn entry_to_json(entry: RecordIndexEntry) -> json.Json {
  json.object([
    #("record_id", json.string(entry.record_id)),
    #("body_sha256", json.string(entry.body_sha256)),
    #("at_ms", json.int(entry.at_ms)),
    #("storage", json.string(entry.storage)),
  ])
}

fn compare_entries(left: RecordIndexEntry, right: RecordIndexEntry) -> Order {
  string.compare(left.record_id, right.record_id)
}
