import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/task
import scherzo/tracker/conformance/types

pub fn validate_tasks(
  tasks: List(types.FixtureTaskDeclaration),
  adapter_kind: String,
) -> Result(Nil, types.ManifestError) {
  case tasks {
    [] -> Ok(Nil)
    [fixture_task, ..rest] -> {
      use Nil <- result.try(validate_task(fixture_task, adapter_kind))
      use Nil <- result.try(validate_unique_name(fixture_task, rest))
      validate_tasks(rest, adapter_kind)
    }
  }
}

pub fn tasks_to_json(tasks: List(types.FixtureTaskDeclaration)) -> json.Json {
  json.array(tasks, of: fixture_task_declaration_to_json)
}

pub fn tasks_decoder() -> decode.Decoder(List(types.FixtureTaskDeclaration)) {
  decode.list(fixture_task_declaration_decoder())
}

fn validate_task(
  fixture_task: types.FixtureTaskDeclaration,
  adapter_kind: String,
) -> Result(Nil, types.ManifestError) {
  let types.FixtureTaskDeclaration(
    name: name,
    ref: ref,
    operator_refs: operator_refs,
    ..,
  ) = fixture_task
  let task.TaskRef(backend_kind: backend_kind, remote_id: remote_id, ..) = ref

  use Nil <- result.try(case string.trim(name) == "" {
    True ->
      Error(types.ManifestError(
        "invalid_fixture_task_name",
        "fixtures.tasks[].name must be non-empty",
      ))
    False -> Ok(Nil)
  })
  use Nil <- result.try(case string.trim(backend_kind) == "" {
    True ->
      Error(types.ManifestError(
        "invalid_fixture_task_backend_kind",
        "fixtures.tasks[].ref.backend_kind must be non-empty",
      ))
    False -> Ok(Nil)
  })
  use Nil <- result.try(case string.trim(remote_id) == "" {
    True ->
      Error(types.ManifestError(
        "invalid_fixture_task_remote_id",
        "fixtures.tasks[].ref.remote_id must be non-empty",
      ))
    False -> Ok(Nil)
  })
  use Nil <- result.try(case backend_kind == adapter_kind {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "fixture_task_backend_kind_mismatch",
        "fixtures.tasks[].ref.backend_kind must match adapter_kind",
      ))
  })
  use Nil <- result.try(case has_blank_string(operator_refs) {
    True ->
      Error(types.ManifestError(
        "invalid_fixture_task_operator_ref",
        "fixtures.tasks[].operator_refs must contain only non-empty values",
      ))
    False -> Ok(Nil)
  })
  case operator_refs {
    [] ->
      Error(types.ManifestError(
        "missing_fixture_task_operator_ref",
        "fixtures.tasks[].operator_refs must not be empty",
      ))
    _ -> Ok(Nil)
  }
}

fn validate_unique_name(
  fixture_task: types.FixtureTaskDeclaration,
  others: List(types.FixtureTaskDeclaration),
) -> Result(Nil, types.ManifestError) {
  let types.FixtureTaskDeclaration(name: name, ..) = fixture_task
  case task_name_in_list(others, name) {
    True ->
      Error(types.ManifestError(
        "duplicate_fixture_task_name",
        "fixtures.tasks[].name must be unique: " <> name,
      ))
    False -> Ok(Nil)
  }
}

fn task_name_in_list(
  tasks: List(types.FixtureTaskDeclaration),
  target: String,
) -> Bool {
  case tasks {
    [] -> False
    [types.FixtureTaskDeclaration(name: name, ..), ..rest] ->
      name == target || task_name_in_list(rest, target)
  }
}

fn has_blank_string(values: List(String)) -> Bool {
  case values {
    [] -> False
    [value, ..rest] -> string.trim(value) == "" || has_blank_string(rest)
  }
}

fn fixture_task_declaration_to_json(
  fixture_task: types.FixtureTaskDeclaration,
) -> json.Json {
  let types.FixtureTaskDeclaration(
    name: name,
    ref: ref,
    operator_refs: operator_refs,
    purpose: purpose,
  ) = fixture_task
  json.object([
    #("name", json.string(name)),
    #("ref", task_ref_to_json(ref)),
    #("operator_refs", json.array(operator_refs, of: json.string)),
    #("purpose", json.string(purpose)),
  ])
}

fn fixture_task_declaration_decoder() -> decode.Decoder(
  types.FixtureTaskDeclaration,
) {
  use name <- decode.field("name", decode.string)
  use ref <- decode.field("ref", task_ref_decoder())
  use operator_refs <- decode.field("operator_refs", decode.list(decode.string))
  use purpose <- decode.field("purpose", decode.string)
  decode.success(types.FixtureTaskDeclaration(
    name: name,
    ref: ref,
    operator_refs: operator_refs,
    purpose: purpose,
  ))
}

fn task_ref_to_json(ref: task.TaskRef) -> json.Json {
  let task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ) = ref
  json.object([
    #("backend_kind", json.string(backend_kind)),
    #("remote_id", json.string(remote_id)),
    #("key", option_json(key, json.string)),
    #("url", option_json(url, json.string)),
  ])
}

fn task_ref_decoder() -> decode.Decoder(task.TaskRef) {
  use backend_kind <- decode.field("backend_kind", decode.string)
  use remote_id <- decode.field("remote_id", decode.string)
  use key <- decode.optional_field("key", None, decode.optional(decode.string))
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  decode.success(task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ))
}

fn option_json(value: Option(a), mapper: fn(a) -> json.Json) -> json.Json {
  case value {
    Some(value) -> mapper(value)
    None -> json.null()
  }
}
