import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/path
import simplifile

const store_version = 1

const config_dir = ".config/scherzo/daemon-credentials"

const temp_dir = "scherzo-daemon-credentials"

pub type CredentialRef {
  CredentialRef(profile: String)
}

pub type DaemonCredential {
  DaemonCredential(credential_id: Option(String), secret: String)
}

pub type StoredCredential {
  StoredCredential(
    server_url: String,
    daemon_id: String,
    credential_id: Option(String),
    secret: String,
  )
}

pub type WriteResult {
  CredentialWritten(path: String)
  CredentialAlreadyStored(path: String)
}

pub type StoreError {
  InvalidCredentialRef(message: String)
  InvalidStoredCredential(message: String)
  StoreReadFailed(path: String, message: String)
  StoreWriteFailed(path: String, message: String)
  StorePermissionFailed(path: String, message: String)
  StoreInvalid(path: String, message: String)
  ReplaceRequired(path: String)
}

pub type Dependencies {
  Dependencies(
    home_dir: fn() -> Result(String, Nil),
    temp_dir: fn() -> Result(String, Nil),
    is_file: fn(String) -> Bool,
    read: fn(String) -> Result(String, String),
    write: fn(String, String) -> Result(Nil, String),
    create_directory_all: fn(String) -> Result(Nil, String),
    chmod_private: fn(String) -> Result(Nil, String),
  )
}

type PersistedStore {
  PersistedStore(entries: List(StoredCredential))
}

pub fn normalize_credential_ref(
  value: String,
) -> Result(CredentialRef, String) {
  let value = string.trim(value)
  case value == "" {
    True -> Error("ui_server.credential_ref must be non-empty when enabled")
    False ->
      case valid_profile_name(value) {
        True -> Ok(CredentialRef(profile: value))
        False ->
          Error(
            "ui_server.credential_ref must use letters, digits, dot, underscore, or hyphen",
          )
      }
  }
}

pub fn default_dependencies() -> Dependencies {
  Dependencies(
    home_dir: path.home_dir,
    temp_dir: path.temp_dir,
    is_file: file_exists,
    read: fn(path) {
      simplifile.read(path) |> result.map_error(simplifile.describe_error)
    },
    write: fn(path, contents) {
      simplifile.write(path, contents)
      |> result.map_error(simplifile.describe_error)
    },
    create_directory_all: fn(path) {
      simplifile.create_directory_all(path)
      |> result.map_error(simplifile.describe_error)
    },
    chmod_private: chmod_private,
  )
}

pub fn path_for_ref(ref: CredentialRef) -> String {
  path_for_ref_with(ref, default_dependencies())
}

pub fn path_for_ref_with(ref: CredentialRef, deps: Dependencies) -> String {
  let CredentialRef(profile:) = ref
  let root = case deps.home_dir() {
    Ok(home) -> path.join(home, config_dir)
    Error(Nil) ->
      case deps.temp_dir() {
        Ok(tmp) -> path.join(tmp, temp_dir)
        Error(Nil) -> "/tmp/" <> temp_dir
      }
  }
  path.join(root, profile <> ".json")
}

pub fn read_credential(
  ref: CredentialRef,
  server_url: String,
  daemon_id: String,
) -> Result(Option(StoredCredential), StoreError) {
  read_credential_with(ref, server_url, daemon_id, default_dependencies())
}

pub fn read_credential_with(
  ref: CredentialRef,
  server_url: String,
  daemon_id: String,
  deps: Dependencies,
) -> Result(Option(StoredCredential), StoreError) {
  use _ <- result.try(validate_lookup(server_url, daemon_id))
  let path = path_for_ref_with(ref, deps)
  use store <- result.try(load_store(path, deps))
  Ok(find_entry(store.entries, server_url, daemon_id))
}

pub fn write_credential(
  ref: CredentialRef,
  server_url: String,
  daemon_id: String,
  credential: DaemonCredential,
  allow_replace: Bool,
) -> Result(WriteResult, StoreError) {
  write_credential_with(
    ref,
    server_url,
    daemon_id,
    credential,
    allow_replace,
    default_dependencies(),
  )
}

pub fn write_credential_with(
  ref: CredentialRef,
  server_url: String,
  daemon_id: String,
  credential: DaemonCredential,
  allow_replace: Bool,
  deps: Dependencies,
) -> Result(WriteResult, StoreError) {
  use _ <- result.try(validate_lookup(server_url, daemon_id))
  use _ <- result.try(validate_daemon_credential(credential))
  let path = path_for_ref_with(ref, deps)
  use store <- result.try(load_store(path, deps))
  case find_entry(store.entries, server_url, daemon_id) {
    Some(existing) ->
      case same_credential(existing, credential) {
        True -> Ok(CredentialAlreadyStored(path))
        False ->
          case allow_replace {
            True ->
              persist_store(
                path,
                PersistedStore(entries: replace_entry(
                  store.entries,
                  server_url,
                  daemon_id,
                  credential,
                )),
                deps,
              )
              |> result.map(fn(_) { CredentialWritten(path) })
            False -> Error(ReplaceRequired(path))
          }
      }
    None ->
      persist_store(
        path,
        PersistedStore(entries: [
          StoredCredential(
            server_url: server_url,
            daemon_id: daemon_id,
            credential_id: credential.credential_id,
            secret: credential.secret,
          ),
          ..store.entries
        ]),
        deps,
      )
      |> result.map(fn(_) { CredentialWritten(path) })
  }
}

pub fn error_message(error: StoreError) -> String {
  case error {
    InvalidCredentialRef(message) -> message
    InvalidStoredCredential(message) -> message
    StoreReadFailed(path, message) ->
      "failed to read daemon credential store " <> path <> ": " <> message
    StoreWriteFailed(path, message) ->
      "failed to write daemon credential store " <> path <> ": " <> message
    StorePermissionFailed(path, message) ->
      "failed to secure daemon credential store " <> path <> ": " <> message
    StoreInvalid(path, message) ->
      "invalid daemon credential store " <> path <> ": " <> message
    ReplaceRequired(path) ->
      "stored daemon credential differs; rerun with explicit replace for "
      <> path
  }
}

fn validate_lookup(
  server_url: String,
  daemon_id: String,
) -> Result(Nil, StoreError) {
  case string.trim(server_url), string.trim(daemon_id) {
    "", _ -> Error(InvalidStoredCredential("server_url must be non-empty"))
    _, "" -> Error(InvalidStoredCredential("daemon_id must be non-empty"))
    _, _ -> Ok(Nil)
  }
}

fn validate_daemon_credential(
  credential: DaemonCredential,
) -> Result(Nil, StoreError) {
  case string.trim(credential.secret) == "" {
    True ->
      Error(InvalidStoredCredential(
        "daemon credential secret must be non-empty",
      ))
    False -> Ok(Nil)
  }
}

fn valid_profile_name(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    chars -> list.all(chars, is_profile_char)
  }
}

fn is_profile_char(char: String) -> Bool {
  case char {
    "." | "_" | "-" -> True
    _ -> is_alpha_numeric(char)
  }
}

fn is_alpha_numeric(char: String) -> Bool {
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
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z"
    | "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    _ -> False
  }
}

fn load_store(
  path: String,
  deps: Dependencies,
) -> Result(PersistedStore, StoreError) {
  case deps.is_file(path) {
    False -> Ok(PersistedStore(entries: []))
    True ->
      case deps.read(path) {
        Error(message) -> Error(StoreReadFailed(path, message))
        Ok(contents) -> decode_store(path, contents)
      }
  }
}

fn decode_store(
  path: String,
  contents: String,
) -> Result(PersistedStore, StoreError) {
  case json.parse(contents, persisted_store_decoder()) {
    Ok(store) -> Ok(store)
    Error(_) ->
      Error(StoreInvalid(path, "invalid daemon credential store JSON"))
  }
}

fn persisted_store_decoder() -> decode.Decoder(PersistedStore) {
  use version <- decode.field("version", decode.int)
  use entries <- decode.field(
    "entries",
    decode.list(stored_credential_decoder()),
  )
  case version == store_version {
    True -> decode.success(PersistedStore(entries: entries))
    False ->
      decode.failure(
        PersistedStore(entries: []),
        expected: "credential store version 1",
      )
  }
}

fn stored_credential_decoder() -> decode.Decoder(StoredCredential) {
  use server_url <- decode.field("server_url", decode.string)
  use daemon_id <- decode.field("daemon_id", decode.string)
  use credential_id <- decode.optional_field(
    "credential_id",
    None,
    decode.optional(decode.string),
  )
  use secret <- decode.field("secret", decode.string)
  decode.success(StoredCredential(
    server_url:,
    daemon_id:,
    credential_id:,
    secret:,
  ))
}

fn persist_store(
  path: String,
  store: PersistedStore,
  deps: Dependencies,
) -> Result(Nil, StoreError) {
  let dir = directory_name(path)
  use _ <- result.try(
    deps.create_directory_all(dir)
    |> result.map_error(fn(message) { StoreWriteFailed(path, message) }),
  )
  use _ <- result.try(
    deps.write(path, store_to_string(store))
    |> result.map_error(fn(message) { StoreWriteFailed(path, message) }),
  )
  deps.chmod_private(path)
  |> result.map_error(fn(message) { StorePermissionFailed(path, message) })
}

fn store_to_string(store: PersistedStore) -> String {
  json.object([
    #("version", json.int(store_version)),
    #("entries", json.array(store.entries, of: stored_credential_to_json)),
  ])
  |> json.to_string
}

fn stored_credential_to_json(credential: StoredCredential) -> json.Json {
  let id_json = case credential.credential_id {
    Some(id) -> json.string(id)
    None -> json.null()
  }
  json.object([
    #("server_url", json.string(credential.server_url)),
    #("daemon_id", json.string(credential.daemon_id)),
    #("credential_id", id_json),
    #("secret", json.string(credential.secret)),
  ])
}

fn replace_entry(
  entries: List(StoredCredential),
  server_url: String,
  daemon_id: String,
  credential: DaemonCredential,
) -> List(StoredCredential) {
  case entries {
    [] -> []
    [entry, ..rest]
      if entry.server_url == server_url && entry.daemon_id == daemon_id
    -> [
      StoredCredential(
        server_url: server_url,
        daemon_id: daemon_id,
        credential_id: credential.credential_id,
        secret: credential.secret,
      ),
      ..rest
    ]
    [entry, ..rest] -> [
      entry,
      ..replace_entry(rest, server_url, daemon_id, credential)
    ]
  }
}

fn find_entry(
  entries: List(StoredCredential),
  server_url: String,
  daemon_id: String,
) -> Option(StoredCredential) {
  case entries {
    [] -> None
    [entry, ..rest] ->
      case entry.server_url == server_url && entry.daemon_id == daemon_id {
        True -> Some(entry)
        False -> find_entry(rest, server_url, daemon_id)
      }
  }
}

fn same_credential(
  existing: StoredCredential,
  incoming: DaemonCredential,
) -> Bool {
  existing.secret == incoming.secret
  && existing.credential_id == incoming.credential_id
}

fn directory_name(file_path: String) -> String {
  case path.dirname(file_path) {
    Ok(dir) -> dir
    Error(Nil) -> "."
  }
}

fn file_exists(path: String) -> Bool {
  case simplifile.is_file(path) {
    Ok(True) -> True
    _ -> False
  }
}

@external(erlang, "scherzo_control_ffi", "chmod_private")
fn chmod_private(path: String) -> Result(Nil, String)
