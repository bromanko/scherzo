import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/error
import scherzo/path
import scherzo/workspace
import simplifile

pub const schema_version = 1

pub const artifact_type = "managed_workspace_manifest"

pub const max_manifest_bytes = 262_144

pub const max_manifest_entries = 256

pub type EntryState {
  Planned
  Ready
}

pub type Entry {
  Entry(
    run_id: String,
    workflow_id: String,
    step_id: String,
    attempt_index: Int,
    workspace_name: String,
    relative_path: String,
    workspace_profile: String,
    driver_command: String,
    driver_capabilities: List(String),
    source_workspace_name: Option(String),
    source_workspace_relative_path: Option(String),
    state: EntryState,
  )
}

type Manifest {
  Manifest(run_id: String, workflow_id: String, entries: List(Entry))
}

pub type CleanupEntry {
  CleanupEntry(
    entry: Entry,
    workspace_path: String,
    source_workspace_path: Option(String),
    exists: Bool,
  )
}

pub fn manifest_path(run_root: String) -> String {
  path.join(path.join(run_root, ".scherzo"), "managed-workspaces.json")
}

pub fn relative_path_from_run_root(
  run_root: String,
  target_path: String,
) -> Result(String, error.WorkspaceError) {
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  let target_abs = path.absolute(target_path) |> result.unwrap(target_path)
  case path.contains(run_root_abs, target_abs) && target_abs != run_root_abs {
    True -> Ok(string.drop_start(target_abs, string.length(run_root_abs) + 1))
    False -> Error(error.WorkspaceIo("managed workspace path outside run root"))
  }
}

pub fn write_entry(
  run_root: String,
  run_id: String,
  workflow_id: String,
  entry: Entry,
) -> Result(Nil, error.WorkspaceError) {
  let manifest_file = manifest_path(run_root)
  let manifest_dir = path.dirname(manifest_file) |> result.unwrap(run_root)
  use _ <- result.try(
    simplifile.create_directory_all(manifest_dir)
    |> result.map_error(fn(_) {
      error.WorkspaceIo("managed workspace manifest directory create failed")
    }),
  )
  let existing = load_for_write(run_root, run_id, workflow_id)
  use manifest <- result.try(existing)
  let contents =
    manifest_to_string(Manifest(
      run_id: run_id,
      workflow_id: workflow_id,
      entries: upsert_entries(manifest.entries, entry, []),
    ))
  simplifile.write(manifest_file, contents)
  |> result.map_error(fn(_) {
    error.WorkspaceIo("managed workspace manifest write failed")
  })
}

pub fn cleanup_entries(
  run_root: String,
  profile_name: String,
  driver_command: String,
  driver_capabilities: List(String),
) -> Result(List(CleanupEntry), error.WorkspaceError) {
  use manifest <- result.try(read_manifest(run_root))
  cleanup_entries_from_manifest(
    run_root,
    manifest,
    profile_name,
    driver_command,
    driver_capabilities,
    [],
  )
}

pub fn decode_manifest(contents: String) -> Result(List(Entry), Nil) {
  parse_manifest(contents)
  |> result.map(fn(manifest) { manifest.entries })
  |> result.replace_error(Nil)
}

pub fn encode_manifest(
  entries: List(Entry),
  run_id: String,
  workflow_id: String,
) -> String {
  manifest_to_string(Manifest(
    run_id: run_id,
    workflow_id: workflow_id,
    entries: entries,
  ))
}

fn load_for_write(
  run_root: String,
  run_id: String,
  workflow_id: String,
) -> Result(Manifest, error.WorkspaceError) {
  case read_manifest_file(run_root) {
    Ok(Some(contents)) -> {
      use manifest <- result.try(parse_manifest(contents))
      validate_manifest_identity(manifest, run_id, workflow_id)
    }
    Ok(None) -> Ok(Manifest(run_id, workflow_id, []))
    Error(err) -> Error(err)
  }
}

fn read_manifest(run_root: String) -> Result(Manifest, error.WorkspaceError) {
  case read_manifest_file(run_root) {
    Ok(Some(contents)) -> parse_manifest(contents)
    Ok(None) -> Error(error.WorkspaceIo("managed workspace manifest missing"))
    Error(err) -> Error(err)
  }
}

fn read_manifest_file(
  run_root: String,
) -> Result(Option(String), error.WorkspaceError) {
  let file = manifest_path(run_root)
  case simplifile.file_info(file) {
    Ok(info) -> {
      use _ <- result.try(validate_manifest_size(info.size))
      simplifile.read(file)
      |> result.map(Some)
      |> result.map_error(fn(_) {
        error.WorkspaceIo("managed workspace manifest read failed")
      })
    }
    Error(simplifile.Enoent) -> Ok(None)
    Error(_) ->
      Error(error.WorkspaceIo("managed workspace manifest read failed"))
  }
}

fn validate_manifest_size(size: Int) -> Result(Nil, error.WorkspaceError) {
  case size > max_manifest_bytes {
    True -> Error(error.WorkspaceIo("managed workspace manifest too large"))
    False -> Ok(Nil)
  }
}

fn parse_manifest(contents: String) -> Result(Manifest, error.WorkspaceError) {
  use manifest <- result.try(
    json.parse(contents, manifest_decoder())
    |> result.map_error(fn(_) {
      error.WorkspaceIo("managed workspace manifest decode failed")
    }),
  )
  validate_manifest_entry_count(manifest)
}

fn validate_manifest_entry_count(
  manifest: Manifest,
) -> Result(Manifest, error.WorkspaceError) {
  case list.length(manifest.entries) > max_manifest_entries {
    True ->
      Error(error.WorkspaceIo("managed workspace manifest has too many entries"))
    False -> Ok(manifest)
  }
}

fn validate_manifest_identity(
  manifest: Manifest,
  run_id: String,
  workflow_id: String,
) -> Result(Manifest, error.WorkspaceError) {
  case manifest.run_id == run_id && manifest.workflow_id == workflow_id {
    True -> Ok(manifest)
    False ->
      Error(error.WorkspaceIo("managed workspace manifest identity mismatch"))
  }
}

fn cleanup_entries_from_manifest(
  run_root: String,
  manifest: Manifest,
  profile_name: String,
  driver_command: String,
  driver_capabilities: List(String),
  acc: List(CleanupEntry),
) -> Result(List(CleanupEntry), error.WorkspaceError) {
  case manifest.entries {
    [] -> Ok(list.reverse(acc))
    [entry, ..rest] -> {
      use validated <- result.try(validate_cleanup_entry(
        run_root,
        manifest.run_id,
        manifest.workflow_id,
        profile_name,
        driver_command,
        driver_capabilities,
        entry,
      ))
      cleanup_entries_from_manifest(
        run_root,
        Manifest(
          run_id: manifest.run_id,
          workflow_id: manifest.workflow_id,
          entries: rest,
        ),
        profile_name,
        driver_command,
        driver_capabilities,
        [validated, ..acc],
      )
    }
  }
}

fn validate_cleanup_entry(
  run_root: String,
  run_id: String,
  workflow_id: String,
  profile_name: String,
  driver_command: String,
  driver_capabilities: List(String),
  entry: Entry,
) -> Result(CleanupEntry, error.WorkspaceError) {
  case
    entry.run_id == run_id
    && entry.workflow_id == workflow_id
    && entry.workspace_profile == profile_name
  {
    False ->
      Error(error.WorkspaceIo("managed workspace manifest entry mismatch"))
    True ->
      case
        entry.driver_command == driver_command
        && entry.driver_capabilities == driver_capabilities
      {
        False ->
          Error(error.WorkspaceIo(
            "managed workspace manifest driver context mismatch",
          ))
        True -> {
          use workspace_path <- result.try(validated_absolute_path(
            run_root,
            entry.relative_path,
          ))
          use source_workspace_path <- result.try(
            validated_optional_absolute_path(
              run_root,
              entry.source_workspace_relative_path,
            ),
          )
          use _ <- result.try(validate_entry_paths(entry))
          use exists <- result.try(existing_path_within_run_root(
            run_root,
            workspace_path,
          ))
          Ok(CleanupEntry(
            entry: entry,
            workspace_path: workspace_path,
            source_workspace_path: source_workspace_path,
            exists: exists,
          ))
        }
      }
  }
}

fn validate_entry_paths(entry: Entry) -> Result(Nil, error.WorkspaceError) {
  use _ <- result.try(validate_workspace_relative_path(
    entry.workspace_name,
    entry.relative_path,
  ))
  case entry.source_workspace_name, entry.source_workspace_relative_path {
    None, None -> Ok(Nil)
    Some(source_name), Some(source_relative_path) ->
      validate_workspace_relative_path(source_name, source_relative_path)
    _, _ -> Error(error.WorkspaceIo("managed workspace source entry mismatch"))
  }
}

fn validate_workspace_relative_path(
  workspace_name: String,
  relative_path: String,
) -> Result(Nil, error.WorkspaceError) {
  use workspace_key <- result.try(
    workspace.sanitize(workspace_name)
    |> result.map_error(fn(_) {
      error.WorkspaceIo("managed workspace name is unsafe")
    }),
  )
  let expected = path.join("workspaces", workspace_key)
  case relative_path == expected {
    True -> Ok(Nil)
    False ->
      Error(error.WorkspaceIo(
        "managed workspace path does not match workspace name",
      ))
  }
}

fn validated_optional_absolute_path(
  run_root: String,
  relative_path: Option(String),
) -> Result(Option(String), error.WorkspaceError) {
  case relative_path {
    None -> Ok(None)
    Some(value) -> {
      use absolute <- result.try(validated_absolute_path(run_root, value))
      Ok(Some(absolute))
    }
  }
}

fn existing_path_within_run_root(
  run_root: String,
  workspace_path: String,
) -> Result(Bool, error.WorkspaceError) {
  use _ <- result.try(validate_existing_realpath(run_root, workspace_path))
  case simplifile.is_directory(workspace_path) {
    Ok(False) -> Ok(False)
    Error(simplifile.Enoent) -> Ok(False)
    Error(_) -> Error(error.WorkspaceIo("managed workspace inspect failed"))
    Ok(True) -> Ok(True)
  }
}

fn validate_existing_realpath(
  run_root: String,
  workspace_path: String,
) -> Result(Nil, error.WorkspaceError) {
  case path.realpath(workspace_path) {
    Error(_) -> Ok(Nil)
    Ok(real) -> {
      let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
      case path.contains(run_root_abs, real) && real != run_root_abs {
        True -> Ok(Nil)
        False ->
          Error(error.WorkspaceIo("managed workspace realpath escapes run root"))
      }
    }
  }
}

fn validated_absolute_path(
  run_root: String,
  relative_path: String,
) -> Result(String, error.WorkspaceError) {
  use _ <- result.try(validate_relative_path(relative_path))
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  let candidate = path.join(run_root_abs, relative_path)
  let candidate_abs = path.absolute(candidate) |> result.unwrap(candidate)
  case
    path.contains(run_root_abs, candidate_abs) && candidate_abs != run_root_abs
  {
    True -> Ok(candidate_abs)
    False -> Error(error.WorkspaceIo("managed workspace path escapes run root"))
  }
}

fn validate_relative_path(
  relative_path: String,
) -> Result(Nil, error.WorkspaceError) {
  let trimmed = string.trim(relative_path)
  case
    trimmed == "" || path.is_absolute(trimmed) || has_parent_segment(trimmed)
  {
    True -> Error(error.WorkspaceIo("managed workspace path is unsafe"))
    False -> Ok(Nil)
  }
}

fn has_parent_segment(relative_path: String) -> Bool {
  relative_path
  |> string.split(on: "/")
  |> list.any(fn(segment) { segment == ".." || segment == "." || segment == "" })
}

fn upsert_entries(
  entries: List(Entry),
  replacement: Entry,
  acc: List(Entry),
) -> List(Entry) {
  case entries {
    [] -> list.reverse([replacement, ..acc])
    [entry, ..rest] ->
      case entry.relative_path == replacement.relative_path {
        True -> list.reverse(acc) |> list.append([replacement, ..rest])
        False -> upsert_entries(rest, replacement, [entry, ..acc])
      }
  }
}

fn manifest_to_string(manifest: Manifest) -> String {
  manifest_to_json(manifest) |> json.to_string
}

fn manifest_to_json(manifest: Manifest) -> json.Json {
  json.object([
    #("schema_version", json.int(schema_version)),
    #("artifact_type", json.string(artifact_type)),
    #("run_id", json.string(manifest.run_id)),
    #("workflow_id", json.string(manifest.workflow_id)),
    #("entries", json.array(manifest.entries, of: entry_to_json)),
  ])
}

fn entry_to_json(entry: Entry) -> json.Json {
  json.object([
    #("run_id", json.string(entry.run_id)),
    #("workflow_id", json.string(entry.workflow_id)),
    #("step_id", json.string(entry.step_id)),
    #("attempt_index", json.int(entry.attempt_index)),
    #("workspace_name", json.string(entry.workspace_name)),
    #("relative_path", json.string(entry.relative_path)),
    #("workspace_profile", json.string(entry.workspace_profile)),
    #("driver_command", json.string(entry.driver_command)),
    #(
      "driver_capabilities",
      json.array(entry.driver_capabilities, of: json.string),
    ),
    #(
      "source_workspace_name",
      option_string_to_json(entry.source_workspace_name),
    ),
    #(
      "source_workspace_relative_path",
      option_string_to_json(entry.source_workspace_relative_path),
    ),
    #("state", json.string(state_to_string(entry.state))),
  ])
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn manifest_decoder() -> decode.Decoder(Manifest) {
  use version <- decode.field("schema_version", decode.int)
  use type_ <- decode.field("artifact_type", decode.string)
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use entries <- decode.field("entries", decode.list(entry_decoder()))
  case version == schema_version && type_ == artifact_type {
    True ->
      decode.success(Manifest(
        run_id: run_id,
        workflow_id: workflow_id,
        entries: entries,
      ))
    False ->
      decode.failure(
        Manifest(run_id: "", workflow_id: "", entries: []),
        expected: "managed workspace manifest",
      )
  }
}

fn entry_decoder() -> decode.Decoder(Entry) {
  use run_id <- decode.field("run_id", decode.string)
  use workflow_id <- decode.field("workflow_id", decode.string)
  use step_id <- decode.field("step_id", decode.string)
  use attempt_index <- decode.field("attempt_index", decode.int)
  use workspace_name <- decode.field("workspace_name", decode.string)
  use relative_path <- decode.field("relative_path", decode.string)
  use workspace_profile <- decode.field("workspace_profile", decode.string)
  use driver_command <- decode.field("driver_command", decode.string)
  use driver_capabilities <- decode.optional_field(
    "driver_capabilities",
    [],
    decode.list(decode.string),
  )
  use source_workspace_name <- decode.optional_field(
    "source_workspace_name",
    None,
    decode.optional(decode.string),
  )
  use source_workspace_relative_path <- decode.optional_field(
    "source_workspace_relative_path",
    None,
    decode.optional(decode.string),
  )
  use state <- decode.field("state", state_decoder())
  decode.success(Entry(
    run_id: run_id,
    workflow_id: workflow_id,
    step_id: step_id,
    attempt_index: attempt_index,
    workspace_name: workspace_name,
    relative_path: relative_path,
    workspace_profile: workspace_profile,
    driver_command: driver_command,
    driver_capabilities: driver_capabilities,
    source_workspace_name: source_workspace_name,
    source_workspace_relative_path: source_workspace_relative_path,
    state: state,
  ))
}

fn state_decoder() -> decode.Decoder(EntryState) {
  use state <- decode.then(decode.string)
  case state {
    "planned" -> decode.success(Planned)
    "ready" -> decode.success(Ready)
    _ -> decode.failure(Planned, expected: "managed workspace state")
  }
}

fn state_to_string(state: EntryState) -> String {
  case state {
    Planned -> "planned"
    Ready -> "ready"
  }
}
