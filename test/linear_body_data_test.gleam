import gleam/json
import gleam/string
import scherzo/linear_body_data

fn attrs(name: String) -> linear_body_data.FileNodeAttrs {
  linear_body_data.FileNodeAttrs(
    upload_id: "upload-123-abc",
    href: "https://uploads.linear.app/new.md",
    name: name,
    size: 1234,
    mimetype: "text/markdown",
  )
}

fn existing_document() -> String {
  "{\"type\":\"doc\",\"content\":[{\"type\":\"file\",\"attrs\":{\"uploadState\":\"finished\",\"uploadId\":\"old-upload\",\"href\":\"https://uploads.linear.app/old.md\",\"name\":\"old.md\",\"size\":7,\"mimetype\":\"text/markdown\",\"unknown\":{\"kept\":true}}},{\"type\":\"paragraph\",\"attrs\":{\"level\":1},\"content\":[{\"type\":\"text\",\"text\":\"hello paragraph\"}]}],\"unknownDocField\":{\"nested\":[1,2.5,null,false]}}"
}

pub fn append_file_node_preserves_existing_content_test() {
  let assert Ok(linear_body_data.Appended(value)) =
    linear_body_data.append_file_node(
      existing_document(),
      attrs("result.md"),
      False,
    )

  let encoded = linear_body_data.to_json(value) |> json.to_string
  let assert Ok(reparsed) = linear_body_data.parse_document(encoded)
  assert linear_body_data.has_file_named(reparsed, "old.md", "text/markdown")
  assert linear_body_data.has_file_named(reparsed, "result.md", "text/markdown")
  assert json_contains_text(reparsed, "hello paragraph")
  assert json_contains_text(reparsed, "old-upload")
  assert json_contains_text(reparsed, "upload-123-abc")
  assert json_contains_text(reparsed, "https://uploads.linear.app/new.md")
}

pub fn append_file_node_dedupes_existing_native_file_test() {
  let assert Ok(linear_body_data.AlreadyPresent) =
    linear_body_data.append_file_node(
      existing_document(),
      attrs("old.md"),
      True,
    )
}

pub fn parse_document_rejects_malformed_json_test() {
  let assert Error(message) = linear_body_data.parse_document("{not-json")
  assert string.contains(message, "invalid bodyData JSON")
}

pub fn parse_document_requires_doc_shape_test() {
  let assert Error(missing_type) =
    linear_body_data.parse_document("{\"type\":\"paragraph\",\"content\":[]}")
  assert string.contains(missing_type, "type \"doc\"")

  let assert Error(missing_content) =
    linear_body_data.parse_document("{\"type\":\"doc\",\"content\":{}}")
  assert string.contains(missing_content, "array content")
}

pub fn markdown_link_escapes_label_and_dedupes_body_test() {
  let link =
    linear_body_data.markdown_link("a[b].md", "https://uploads.linear.app/file")
  assert link == "[a\\[b\\].md](https://uploads.linear.app/file)"
  assert linear_body_data.body_has_markdown_link_for_filename(
    "see " <> link,
    "a[b].md",
  )
  assert !linear_body_data.body_has_markdown_link_for_filename(
    "see " <> link,
    "other.md",
  )
}

fn json_contains_text(value: linear_body_data.JsonValue, text: String) -> Bool {
  case value {
    linear_body_data.JObject(entries) ->
      json_entries_contain_text(entries, text)
    linear_body_data.JArray(values) -> json_values_contain_text(values, text)
    linear_body_data.JString(value) -> value == text
    linear_body_data.JInt(_) -> False
    linear_body_data.JFloat(_) -> False
    linear_body_data.JBool(_) -> False
    linear_body_data.JNull -> False
  }
}

fn json_entries_contain_text(
  entries: List(#(String, linear_body_data.JsonValue)),
  text: String,
) -> Bool {
  case entries {
    [] -> False
    [#(key, value), ..rest] ->
      key == text
      || json_contains_text(value, text)
      || json_entries_contain_text(rest, text)
  }
}

fn json_values_contain_text(
  values: List(linear_body_data.JsonValue),
  text: String,
) -> Bool {
  case values {
    [] -> False
    [value, ..rest] ->
      json_contains_text(value, text) || json_values_contain_text(rest, text)
  }
}
