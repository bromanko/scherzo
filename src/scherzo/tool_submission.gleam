import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/json_value
import scherzo/log
import scherzo/pi/protocol
import scherzo/workflow_dag

pub const missing_code = "review_lane_draft_tool_missing"

pub const arguments_invalid_code = "review_lane_draft_tool_arguments_invalid"

pub const failed_code = "review_lane_draft_tool_failed"

pub const multiple_submissions_code = "review_lane_draft_multiple_submissions"

pub const extra_tool_call_code = "review_lane_draft_extra_tool_call"

pub type CapturedToolPayload {
  CapturedToolPayload(
    payload_json: String,
    tool_name: String,
    submission_source: String,
  )
}

pub type ToolSubmissionError {
  ToolSubmissionMissing(tool_name: String)
  ToolSubmissionArgumentsInvalid(tool_name: String, message: String)
  ToolSubmissionFailed(tool_name: String, message: String)
  ToolSubmissionMultiple(tool_name: String, count: Int)
  ToolSubmissionExtraToolCall(tool_name: String, sibling_name: String)
}

type Candidate {
  Candidate(payload_json: String)
}

pub fn extract_required_tool_payload(
  records: List(protocol.RpcRecord),
  spec: workflow_dag.ToolSubmissionSpec,
  secrets: List(String),
) -> Result(CapturedToolPayload, ToolSubmissionError) {
  let target_records = target_records(records, spec.tool_name)
  case target_records {
    [] -> Error(ToolSubmissionMissing(spec.tool_name))
    _ -> {
      case failed_status(target_records) {
        Some(message) -> Error(ToolSubmissionFailed(spec.tool_name, message))
        None -> {
          case same_record_sibling(target_records, spec.tool_name) {
            Some(sibling) ->
              Error(ToolSubmissionExtraToolCall(spec.tool_name, sibling))
            None -> extract_candidates(target_records, spec.tool_name, secrets)
          }
        }
      }
    }
  }
}

pub fn error_code(error: ToolSubmissionError) -> String {
  case error {
    ToolSubmissionMissing(_) -> missing_code
    ToolSubmissionArgumentsInvalid(_, _) -> arguments_invalid_code
    ToolSubmissionFailed(_, _) -> failed_code
    ToolSubmissionMultiple(_, _) -> multiple_submissions_code
    ToolSubmissionExtraToolCall(_, _) -> extra_tool_call_code
  }
}

pub fn error_message(error: ToolSubmissionError) -> String {
  case error {
    ToolSubmissionMissing(tool_name) ->
      "required Pi tool submission was not found: " <> tool_name
    ToolSubmissionArgumentsInvalid(tool_name, message) ->
      "required Pi tool submission arguments were invalid for "
      <> tool_name
      <> ": "
      <> message
    ToolSubmissionFailed(tool_name, message) ->
      "required Pi tool execution failed for " <> tool_name <> ": " <> message
    ToolSubmissionMultiple(tool_name, count) ->
      "expected exactly one successful "
      <> tool_name
      <> " call, found "
      <> int_to_string(count)
    ToolSubmissionExtraToolCall(tool_name, sibling_name) ->
      "required Pi tool "
      <> tool_name
      <> " was batched with another tool call: "
      <> sibling_name
  }
}

fn target_records(
  records: List(protocol.RpcRecord),
  tool_name: String,
) -> List(protocol.RpcRecord) {
  records
  |> list.filter(fn(record) { record.tool_name == Some(tool_name) })
}

fn failed_status(records: List(protocol.RpcRecord)) -> Option(String) {
  case records {
    [] -> None
    [record, ..rest] -> {
      let failed = case record.tool_status {
        Some(status) -> string.lowercase(string.trim(status)) == "failed"
        None -> record.success == Some(False)
      }
      case failed {
        True -> Some(log.truncate(status_message(record), 500))
        False -> failed_status(rest)
      }
    }
  }
}

fn status_message(record: protocol.RpcRecord) -> String {
  case
    first_non_empty([record.error_message, record.tool_output, record.message])
  {
    Some(message) -> message
    None -> "tool execution status was failed"
  }
}

fn same_record_sibling(
  records: List(protocol.RpcRecord),
  tool_name: String,
) -> Option(String) {
  case records {
    [] -> None
    [record, ..rest] -> {
      case sibling_tool_name(record.raw_json, tool_name) {
        Some(sibling) -> Some(sibling)
        None -> same_record_sibling(rest, tool_name)
      }
    }
  }
}

fn extract_candidates(
  records: List(protocol.RpcRecord),
  tool_name: String,
  secrets: List(String),
) -> Result(CapturedToolPayload, ToolSubmissionError) {
  case collect_candidates(records, tool_name, []) {
    [] ->
      Error(ToolSubmissionArgumentsInvalid(
        tool_name,
        "tool record did not contain object-valued arguments in raw JSON",
      ))
    [candidate] -> {
      let payload_json =
        log.redact("tool_submission", candidate.payload_json, secrets)
      Ok(CapturedToolPayload(
        payload_json: payload_json,
        tool_name: tool_name,
        submission_source: "pi_tool",
      ))
    }
    candidates ->
      Error(ToolSubmissionMultiple(tool_name, list.length(candidates)))
  }
}

fn collect_candidates(
  records: List(protocol.RpcRecord),
  tool_name: String,
  acc: List(Candidate),
) -> List(Candidate) {
  case records {
    [] -> list.reverse(acc)
    [record, ..rest] -> {
      let acc = case extract_arguments(record.raw_json, tool_name) {
        Ok(payload_json) -> [Candidate(payload_json), ..acc]
        Error(Nil) -> acc
      }
      collect_candidates(rest, tool_name, acc)
    }
  }
}

fn extract_arguments(
  raw_json: String,
  tool_name: String,
) -> Result(String, Nil) {
  use value <- result_try(json_value.parse(raw_json))
  case argument_value(value, tool_name) {
    Some(payload) ->
      case payload {
        json_value.JObject(_) -> Ok(json_value.to_string(payload))
        _ -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

fn argument_value(
  value: json_value.JsonValue,
  tool_name: String,
) -> Option(json_value.JsonValue) {
  case value {
    json_value.JObject(entries) -> {
      case direct_argument_value(entries) {
        Some(payload) -> Some(payload)
        None -> content_tool_argument_value(entries, tool_name)
      }
    }
    _ -> None
  }
}

fn direct_argument_value(
  entries: List(#(String, json_value.JsonValue)),
) -> Option(json_value.JsonValue) {
  case object_get(entries, "input") {
    Some(payload) ->
      case payload {
        json_value.JObject(_) -> Some(payload)
        _ -> direct_args_argument_value(entries)
      }
    None -> direct_args_argument_value(entries)
  }
}

fn direct_args_argument_value(
  entries: List(#(String, json_value.JsonValue)),
) -> Option(json_value.JsonValue) {
  case object_get(entries, "args") {
    Some(payload) ->
      case payload {
        json_value.JObject(_) -> Some(payload)
        _ -> direct_data_argument_value(entries)
      }
    None -> direct_data_argument_value(entries)
  }
}

fn direct_data_argument_value(
  entries: List(#(String, json_value.JsonValue)),
) -> Option(json_value.JsonValue) {
  case object_get(entries, "data") {
    Some(json_value.JObject(data_entries)) ->
      direct_argument_value(data_entries)
    _ -> None
  }
}

fn content_tool_argument_value(
  entries: List(#(String, json_value.JsonValue)),
  tool_name: String,
) -> Option(json_value.JsonValue) {
  case object_get(entries, "message") {
    Some(json_value.JObject(message_entries)) ->
      content_tool_argument_value(message_entries, tool_name)
    _ ->
      case object_get(entries, "content") {
        Some(json_value.JArray(items)) ->
          content_item_argument_value(items, tool_name)
        _ -> None
      }
  }
}

fn content_item_argument_value(
  items: List(json_value.JsonValue),
  tool_name: String,
) -> Option(json_value.JsonValue) {
  case items {
    [] -> None
    [json_value.JObject(entries), ..rest] -> {
      let name = object_string(entries, "name")
      case name == Some(tool_name) {
        True -> direct_argument_value(entries)
        False -> content_item_argument_value(rest, tool_name)
      }
    }
    [_, ..rest] -> content_item_argument_value(rest, tool_name)
  }
}

fn sibling_tool_name(raw_json: String, tool_name: String) -> Option(String) {
  case json_value.parse(raw_json) {
    Ok(json_value.JObject(entries)) ->
      sibling_tool_name_in_object(entries, tool_name)
    _ -> None
  }
}

fn sibling_tool_name_in_object(
  entries: List(#(String, json_value.JsonValue)),
  tool_name: String,
) -> Option(String) {
  case object_get(entries, "message") {
    Some(json_value.JObject(message_entries)) ->
      sibling_tool_name_in_object(message_entries, tool_name)
    _ ->
      case object_get(entries, "content") {
        Some(json_value.JArray(items)) ->
          sibling_tool_name_in_content(items, tool_name)
        _ -> None
      }
  }
}

fn sibling_tool_name_in_content(
  items: List(json_value.JsonValue),
  tool_name: String,
) -> Option(String) {
  let names = content_tool_names(items, [])
  case list.contains(names, tool_name) {
    False -> None
    True -> first_other_name(names, tool_name)
  }
}

fn content_tool_names(
  items: List(json_value.JsonValue),
  acc: List(String),
) -> List(String) {
  case items {
    [] -> list.reverse(acc)
    [json_value.JObject(entries), ..rest] -> {
      let acc = case object_string(entries, "name") {
        Some(name) -> [name, ..acc]
        None -> acc
      }
      content_tool_names(rest, acc)
    }
    [_, ..rest] -> content_tool_names(rest, acc)
  }
}

fn first_other_name(names: List(String), tool_name: String) -> Option(String) {
  case names {
    [] -> None
    [name, ..rest] ->
      case name != tool_name {
        True -> Some(name)
        False -> first_other_name(rest, tool_name)
      }
  }
}

fn object_get(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> object_get(rest, key)
      }
  }
}

fn object_string(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(String) {
  case object_get(entries, key) {
    Some(json_value.JString(value)) -> Some(value)
    _ -> None
  }
}

fn first_non_empty(values: List(Option(String))) -> Option(String) {
  case values {
    [] -> None
    [Some(value), ..rest] ->
      case string.trim(value) == "" {
        True -> first_non_empty(rest)
        False -> Some(value)
      }
    [None, ..rest] -> first_non_empty(rest)
  }
}

fn result_try(
  result: Result(a, Nil),
  next: fn(a) -> Result(b, Nil),
) -> Result(b, Nil) {
  case result {
    Ok(value) -> next(value)
    Error(Nil) -> Error(Nil)
  }
}

fn int_to_string(value: Int) -> String {
  value |> int.to_string
}
