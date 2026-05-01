import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/string

pub type JsonValue {
  JObject(List(#(String, JsonValue)))
  JArray(List(JsonValue))
  JString(String)
  JInt(Int)
  JFloat(Float)
  JBool(Bool)
  JNull
}

pub type FileNodeAttrs {
  FileNodeAttrs(
    upload_id: String,
    href: String,
    name: String,
    size: Int,
    mimetype: String,
  )
}

pub type AppendResult {
  Appended(JsonValue)
  AlreadyPresent
}

pub fn parse_json(body_data_json: String) -> Result(JsonValue, String) {
  case json.parse(body_data_json, json_value_decoder()) {
    Error(_) -> Error("invalid bodyData JSON")
    Ok(value) -> Ok(value)
  }
}

pub fn parse_document(body_data_json: String) -> Result(JsonValue, String) {
  use value <- result_try(parse_json(body_data_json))
  validate_document(value)
}

pub fn append_file_node_to_document(
  body_data: JsonValue,
  attrs: FileNodeAttrs,
  dedupe_by_filename: Bool,
) -> AppendResult {
  case
    dedupe_by_filename && has_file_named(body_data, attrs.name, attrs.mimetype)
  {
    True -> AlreadyPresent
    False -> Appended(append_file_node_value(body_data, attrs))
  }
}

pub fn append_file_node(
  body_data_json: String,
  attrs: FileNodeAttrs,
  dedupe_by_filename: Bool,
) -> Result(AppendResult, String) {
  use document <- result_try(parse_document(body_data_json))
  Ok(append_file_node_to_document(document, attrs, dedupe_by_filename))
}

pub fn has_file_named(
  body_data: JsonValue,
  filename: String,
  mimetype: String,
) -> Bool {
  case body_data {
    JObject(entries) ->
      case get_field(entries, "content") {
        Ok(JArray(nodes)) -> has_file_named_in_nodes(nodes, filename, mimetype)
        _ -> False
      }
    _ -> False
  }
}

pub fn to_json(value: JsonValue) -> json.Json {
  case value {
    JObject(entries) ->
      json.object(
        list.map(entries, fn(entry) {
          let #(key, value) = entry
          #(key, to_json(value))
        }),
      )
    JArray(values) -> json.array(values, of: to_json)
    JString(value) -> json.string(value)
    JInt(value) -> json.int(value)
    JFloat(value) -> json.float(value)
    JBool(value) -> json.bool(value)
    JNull -> json.null()
  }
}

pub fn markdown_link(filename: String, href: String) -> String {
  "[" <> escape_markdown_link_label(filename) <> "](" <> href <> ")"
}

pub fn body_has_markdown_link_for_filename(
  body: String,
  filename: String,
) -> Bool {
  string.contains(body, "[" <> escape_markdown_link_label(filename) <> "](")
}

pub fn json_value_decoder() -> decode.Decoder(JsonValue) {
  use data <- decode.then(decode.dynamic)
  case decode.run(data, decode.string) {
    Ok(value) -> decode.success(JString(value))
    Error(_) ->
      case decode.run(data, decode.int) {
        Ok(value) -> decode.success(JInt(value))
        Error(_) ->
          case decode.run(data, decode.float) {
            Ok(value) -> decode.success(JFloat(value))
            Error(_) ->
              case decode.run(data, decode.bool) {
                Ok(value) -> decode.success(JBool(value))
                Error(_) ->
                  case decode.run(data, decode.list(json_value_decoder())) {
                    Ok(values) -> decode.success(JArray(values))
                    Error(_) ->
                      case
                        decode.run(
                          data,
                          decode.dict(decode.string, json_value_decoder()),
                        )
                      {
                        Ok(entries) ->
                          decode.success(JObject(dict.to_list(entries)))
                        Error(_) ->
                          case
                            decode.run(data, decode.optional(decode.dynamic))
                          {
                            Ok(None) -> decode.success(JNull)
                            _ -> decode.failure(JNull, expected: "JSON value")
                          }
                      }
                  }
              }
          }
      }
  }
}

pub fn validate_document(value: JsonValue) -> Result(JsonValue, String) {
  case value {
    JObject(entries) ->
      case get_field(entries, "type"), get_field(entries, "content") {
        Ok(JString("doc")), Ok(JArray(_)) -> Ok(value)
        _, Ok(JArray(_)) ->
          Error("bodyData must be an object with type \"doc\"")
        Ok(JString("doc")), _ ->
          Error("bodyData doc must have an array content field")
        _, _ ->
          Error(
            "bodyData must be an object with type \"doc\" and array content",
          )
      }
    _ -> Error("bodyData must be an object with type \"doc\" and array content")
  }
}

fn append_file_node_value(
  body_data: JsonValue,
  attrs: FileNodeAttrs,
) -> JsonValue {
  case body_data {
    JObject(entries) -> JObject(update_content(entries, attrs))
    _ -> body_data
  }
}

fn update_content(
  entries: List(#(String, JsonValue)),
  attrs: FileNodeAttrs,
) -> List(#(String, JsonValue)) {
  case entries {
    [] -> []
    [#("content", JArray(nodes)), ..rest] -> [
      #("content", JArray(list.append(nodes, [file_node(attrs)]))),
      ..rest
    ]
    [entry, ..rest] -> [entry, ..update_content(rest, attrs)]
  }
}

fn file_node(attrs: FileNodeAttrs) -> JsonValue {
  JObject([
    #("type", JString("file")),
    #(
      "attrs",
      JObject([
        #("uploadState", JString("finished")),
        #("uploadId", JString(attrs.upload_id)),
        #("href", JString(attrs.href)),
        #("name", JString(attrs.name)),
        #("size", JInt(attrs.size)),
        #("mimetype", JString(attrs.mimetype)),
      ]),
    ),
  ])
}

fn has_file_named_in_nodes(
  nodes: List(JsonValue),
  filename: String,
  mimetype: String,
) -> Bool {
  case nodes {
    [] -> False
    [node, ..rest] ->
      is_matching_file_node(node, filename, mimetype)
      || has_file_named_in_nodes(rest, filename, mimetype)
  }
}

fn is_matching_file_node(
  node: JsonValue,
  filename: String,
  mimetype: String,
) -> Bool {
  case node {
    JObject(entries) ->
      case get_field(entries, "type"), get_field(entries, "attrs") {
        Ok(JString("file")), Ok(JObject(attrs)) ->
          get_field(attrs, "name") == Ok(JString(filename))
          && get_field(attrs, "mimetype") == Ok(JString(mimetype))
        _, _ -> False
      }
    _ -> False
  }
}

fn get_field(
  entries: List(#(String, JsonValue)),
  key: String,
) -> Result(JsonValue, Nil) {
  list.key_find(entries, key)
}

fn escape_markdown_link_label(filename: String) -> String {
  filename
  |> string.replace("\\", "\\\\")
  |> string.replace("[", "\\[")
  |> string.replace("]", "\\]")
}

fn result_try(result: Result(a, e), next: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
