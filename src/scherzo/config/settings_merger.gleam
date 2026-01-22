/// Settings merger for combining auto-generated and custom Claude Code settings
///
/// Merges settings.json files with special handling for hooks:
/// - Hooks are merged by event type (auto-generated first, custom appended)
/// - Other top-level fields: custom overrides auto-generated
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result

/// Merge auto-generated settings with custom overrides.
/// - Hooks array: auto-generated first, custom appended per event type
/// - Other fields: custom overrides auto-generated
pub fn merge(auto_generated: String, custom: String) -> Result(String, String) {
  use base_dict <- result.try(parse_object(auto_generated, "auto-generated"))
  use custom_dict <- result.try(parse_object(custom, "custom"))

  let merged = merge_objects(base_dict, custom_dict)
  Ok(encode_object(merged))
}

/// Parse a JSON string as an object (Dict)
fn parse_object(
  content: String,
  label: String,
) -> Result(Dict(String, Dynamic), String) {
  case json.parse(content, decode.dict(decode.string, decode.dynamic)) {
    Ok(dict) -> Ok(dict)
    Error(_) -> Error("Invalid JSON in " <> label <> " settings")
  }
}

/// Merge two objects, with special handling for "hooks" key
fn merge_objects(
  base: Dict(String, Dynamic),
  custom: Dict(String, Dynamic),
) -> Dict(String, Dynamic) {
  // Start with auto-generated as base
  // Then apply custom overrides
  dict.fold(custom, base, fn(acc, key, custom_value) {
    case key {
      "hooks" -> {
        // Special merge for hooks: combine by event type
        let base_hooks = dict.get(acc, "hooks")
        let merged_hooks = merge_hooks(base_hooks, custom_value)
        dict.insert(acc, "hooks", merged_hooks)
      }
      _ -> {
        // Other fields: custom overrides
        dict.insert(acc, key, custom_value)
      }
    }
  })
}

/// Merge hooks objects by event type
/// Each event (SessionStart, Stop, etc.) has its arrays concatenated
fn merge_hooks(
  base_hooks: Result(Dynamic, Nil),
  custom_hooks: Dynamic,
) -> Dynamic {
  let base_dict = case base_hooks {
    Ok(hooks) -> decode_hooks_dict(hooks)
    Error(_) -> dict.new()
  }
  let custom_dict = decode_hooks_dict(custom_hooks)

  // Merge: for each event, concatenate the arrays
  let merged =
    dict.fold(custom_dict, base_dict, fn(acc, event, custom_array) {
      case dict.get(acc, event) {
        Ok(base_array) -> {
          // Concatenate arrays: base first, then custom
          let merged_array = concat_dynamic_arrays(base_array, custom_array)
          dict.insert(acc, event, merged_array)
        }
        Error(_) -> {
          // New event from custom
          dict.insert(acc, event, custom_array)
        }
      }
    })

  // Convert merged dict back to Dynamic
  unsafe_coerce(merged)
}

/// Decode a hooks object as Dict(String, Dynamic)
/// Returns empty dict if hooks is not a valid object (graceful degradation)
fn decode_hooks_dict(hooks: Dynamic) -> Dict(String, Dynamic) {
  case decode.run(hooks, decode.dict(decode.string, decode.dynamic)) {
    Ok(dict) -> dict
    Error(_) -> dict.new()
  }
}

/// Concatenate two dynamic arrays
fn concat_dynamic_arrays(a: Dynamic, b: Dynamic) -> Dynamic {
  let list_a = decode_dynamic_list(a)
  let list_b = decode_dynamic_list(b)
  unsafe_coerce(list.append(list_a, list_b))
}

/// Decode a dynamic value as a list of dynamics
/// Returns empty list if value is not a valid array (graceful degradation)
fn decode_dynamic_list(value: Dynamic) -> List(Dynamic) {
  case decode.run(value, decode.list(decode.dynamic)) {
    Ok(lst) -> lst
    Error(_) -> []
  }
}

/// Unsafe coerce a value to Dynamic
/// This is safe because Dynamic is just a wrapper around any Erlang term
@external(erlang, "scherzo_ffi", "identity")
fn unsafe_coerce(a: a) -> Dynamic

/// Encode a Dict back to a JSON string
fn encode_object(dict: Dict(String, Dynamic)) -> String {
  dict
  |> dict.to_list
  |> list.map(fn(pair) { #(pair.0, encode_dynamic(pair.1)) })
  |> json.object
  |> json.to_string
}

/// Encode a Dynamic value back to json.Json
/// This handles the common JSON types by trying each decoder in sequence
fn encode_dynamic(value: Dynamic) -> json.Json {
  try_encode_string(value)
  |> result.lazy_or(fn() { try_encode_int(value) })
  |> result.lazy_or(fn() { try_encode_float(value) })
  |> result.lazy_or(fn() { try_encode_bool(value) })
  |> result.lazy_or(fn() { try_encode_null(value) })
  |> result.lazy_or(fn() { try_encode_object(value) })
  |> result.lazy_or(fn() { try_encode_array(value) })
  |> result.unwrap(json.null())
}

fn try_encode_string(value: Dynamic) -> Result(json.Json, Nil) {
  decode.run(value, decode.string)
  |> result.map(json.string)
  |> result.map_error(fn(_) { Nil })
}

fn try_encode_int(value: Dynamic) -> Result(json.Json, Nil) {
  decode.run(value, decode.int)
  |> result.map(json.int)
  |> result.map_error(fn(_) { Nil })
}

fn try_encode_float(value: Dynamic) -> Result(json.Json, Nil) {
  decode.run(value, decode.float)
  |> result.map(json.float)
  |> result.map_error(fn(_) { Nil })
}

fn try_encode_bool(value: Dynamic) -> Result(json.Json, Nil) {
  decode.run(value, decode.bool)
  |> result.map(json.bool)
  |> result.map_error(fn(_) { Nil })
}

fn try_encode_null(value: Dynamic) -> Result(json.Json, Nil) {
  case is_null(value) {
    True -> Ok(json.null())
    False -> Error(Nil)
  }
}

fn try_encode_object(value: Dynamic) -> Result(json.Json, Nil) {
  decode.run(value, decode.dict(decode.string, decode.dynamic))
  |> result.map(encode_object_as_json)
  |> result.map_error(fn(_) { Nil })
}

fn try_encode_array(value: Dynamic) -> Result(json.Json, Nil) {
  decode.run(value, decode.list(decode.dynamic))
  |> result.map(fn(lst) { json.array(lst, encode_dynamic) })
  |> result.map_error(fn(_) { Nil })
}

/// Check if a dynamic value is null
fn is_null(value: Dynamic) -> Bool {
  case dynamic.classify(value) {
    "Atom" ->
      case decode.run(value, atom_decoder()) {
        Ok(atom_str) -> atom_str == "null" || atom_str == "nil"
        Error(_) -> False
      }
    "Nil" -> True
    _ -> False
  }
}

/// Decode an atom as its string representation
fn atom_decoder() -> decode.Decoder(String) {
  decode.new_primitive_decoder("Atom", fn(dyn) {
    case dynamic.classify(dyn) {
      "Atom" -> Ok(atom_to_string(dyn))
      _ -> Error("not an atom")
    }
  })
}

@external(erlang, "erlang", "atom_to_list")
fn atom_to_string(atom: Dynamic) -> String

/// Encode a dict as a json.Json object
fn encode_object_as_json(dict: Dict(String, Dynamic)) -> json.Json {
  dict
  |> dict.to_list
  |> list.map(fn(pair) { #(pair.0, encode_dynamic(pair.1)) })
  |> json.object
}
