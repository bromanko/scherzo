import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None}
import gleam/result

pub type JsonValue {
  JObject(List(#(String, JsonValue)))
  JArray(List(JsonValue))
  JString(String)
  JInt(Int)
  JFloat(Float)
  JBool(Bool)
  JNull
}

pub fn parse(contents: String) -> Result(JsonValue, Nil) {
  json.parse(contents, decoder())
  |> result.replace_error(Nil)
}

pub fn to_json(value: JsonValue) -> json.Json {
  case value {
    JObject(entries) ->
      json.object(
        list.map(entries, fn(entry) {
          let #(key, child) = entry
          #(key, to_json(child))
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

pub fn to_string(value: JsonValue) -> String {
  value |> to_json |> json.to_string
}

pub fn object_has_key(
  entries: List(#(String, JsonValue)),
  key: String,
) -> Bool {
  case entries {
    [] -> False
    [#(current, _), ..rest] -> current == key || object_has_key(rest, key)
  }
}

pub fn decoder() -> decode.Decoder(JsonValue) {
  use data <- decode.then(decode.dynamic)
  case decode.run(data, decode.string) {
    Ok(value) -> decode.success(JString(value))
    // nolint: thrown_away_error -- JSON value decoding probes each supported JSON type in order.
    Error(_) ->
      case decode.run(data, decode.int) {
        Ok(value) -> decode.success(JInt(value))
        // nolint: thrown_away_error -- JSON value decoding probes each supported JSON type in order.
        Error(_) ->
          case decode.run(data, decode.float) {
            Ok(value) -> decode.success(JFloat(value))
            // nolint: thrown_away_error -- JSON value decoding probes each supported JSON type in order.
            Error(_) ->
              case decode.run(data, decode.bool) {
                Ok(value) -> decode.success(JBool(value))
                // nolint: thrown_away_error -- JSON value decoding probes each supported JSON type in order.
                Error(_) ->
                  case decode.run(data, decode.list(decoder())) {
                    Ok(values) -> decode.success(JArray(values))
                    // nolint: thrown_away_error -- JSON value decoding probes each supported JSON type in order.
                    Error(_) ->
                      case
                        decode.run(data, decode.dict(decode.string, decoder()))
                      {
                        Ok(entries) ->
                          decode.success(JObject(dict.to_list(entries)))
                        // nolint: thrown_away_error -- JSON value decoding probes null after all concrete JSON container types fail.
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
