import gleam/list
import gleam/string

pub type Field =
  #(String, String)

pub fn info(event: String, fields: List(Field)) -> String {
  format("info", event, fields, [])
}

pub fn warn(event: String, fields: List(Field)) -> String {
  format("warn", event, fields, [])
}

pub fn error(event: String, fields: List(Field)) -> String {
  format("error", event, fields, [])
}

pub fn debug(event: String, fields: List(Field)) -> String {
  format("debug", event, fields, [])
}

pub fn info_with_secrets(
  event: String,
  fields: List(Field),
  secrets: List(String),
) -> String {
  format("info", event, fields, secrets)
}

pub fn format(
  level: String,
  event: String,
  fields: List(Field),
  secrets: List(String),
) -> String {
  [#("level", level), #("service", "scherzo"), #("event", event), ..fields]
  |> list.map(fn(field) { render_field(field, secrets) })
  |> string.join(with: " ")
}

pub fn truncate(value: String, max: Int) -> String {
  case string.length(value) > max {
    True -> string.slice(value, 0, max) <> "..."
    False -> value
  }
}

fn render_field(field: Field, secrets: List(String)) -> String {
  let #(key, value) = field
  key <> "=" <> escape(redact(key, value, secrets))
}

pub fn redact(key: String, value: String, secrets: List(String)) -> String {
  case sensitive_key(key) {
    True -> "[REDACTED]"
    False ->
      list.fold(secrets, value, fn(acc, secret) {
        case secret == "" {
          True -> acc
          False -> string.replace(acc, each: secret, with: "[REDACTED]")
        }
      })
  }
}

fn sensitive_key(key: String) -> Bool {
  let key = string.lowercase(key)
  string.contains(key, "token")
  || string.contains(key, "api_key")
  || string.contains(key, "authorization")
  || string.contains(key, "secret")
}

pub fn escape(value: String) -> String {
  case needs_quoting(value) {
    False -> value
    True -> "\"" <> json_escape(value) <> "\""
  }
}

fn needs_quoting(value: String) -> Bool {
  value == ""
  || string.contains(value, " ")
  || string.contains(value, "\n")
  || string.contains(value, "\r")
  || string.contains(value, "\t")
  || string.contains(value, "=")
  || string.contains(value, "\"")
}

fn json_escape(value: String) -> String {
  value
  |> string.replace(each: "\\", with: "\\\\")
  |> string.replace(each: "\"", with: "\\\"")
  |> string.replace(each: "\n", with: "\\n")
  |> string.replace(each: "\r", with: "\\r")
  |> string.replace(each: "\t", with: "\\t")
}
