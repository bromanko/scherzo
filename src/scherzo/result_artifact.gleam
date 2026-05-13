import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/json_value
import scherzo/log
import scherzo/pi/protocol

pub type ToolCallSubmission {
  ToolCallSubmission(
    name: String,
    arguments_json: Option(String),
    status: Option(String),
    sibling_count: Int,
  )
}

/// Captured assistant result text.
///
/// `final_response` is the display-safe response used by handoff and step
/// summaries. `structured_response` is redacted but not display-truncated so
/// workflow structured-output validation can run before presentation caps.
pub type ResultArtifact {
  ResultArtifact(
    final_response: Option(String),
    truncated: Bool,
    source: String,
    structured_response: Option(String),
    structured_response_truncated: Bool,
    tool_calls: List(ToolCallSubmission),
  )
}

pub fn empty() -> ResultArtifact {
  from_final_response(None, False, "none")
}

pub fn from_final_response(
  final_response: Option(String),
  truncated: Bool,
  source: String,
) -> ResultArtifact {
  from_final_response_with_tool_calls(final_response, truncated, source, [])
}

pub fn from_final_response_with_tool_calls(
  final_response: Option(String),
  truncated: Bool,
  source: String,
  tool_calls: List(ToolCallSubmission),
) -> ResultArtifact {
  ResultArtifact(
    final_response: final_response,
    truncated: truncated,
    source: source,
    structured_response: final_response,
    structured_response_truncated: truncated,
    tool_calls: tool_calls,
  )
}

pub fn from_records(
  records: List(protocol.RpcRecord),
  secrets: List(String),
  max_chars: Int,
) -> ResultArtifact {
  let tool_calls = tool_call_submissions(records)
  let result = case last_non_empty(assistant_messages(records)) {
    Some(text) ->
      build_result(text, "completed_assistant_messages", secrets, max_chars)
    None -> empty()
  }
  ResultArtifact(..result, tool_calls: tool_calls)
}

pub fn append(
  existing: ResultArtifact,
  next: ResultArtifact,
  max_chars: Int,
) -> ResultArtifact {
  let #(final_response, truncated, source) =
    append_display(existing, next, max_chars)
  let #(structured_response, structured_response_truncated) =
    append_structured(existing, next)
  ResultArtifact(
    final_response: final_response,
    truncated: truncated,
    source: source,
    structured_response: structured_response,
    structured_response_truncated: structured_response_truncated,
    tool_calls: list.append(existing.tool_calls, next.tool_calls),
  )
}

pub fn structured_final_response(artifact: ResultArtifact) -> Option(String) {
  case artifact.structured_response {
    Some(response) -> Some(response)
    None -> artifact.final_response
  }
}

pub fn structured_final_response_truncated(artifact: ResultArtifact) -> Bool {
  artifact.structured_response_truncated
  || { artifact.structured_response == None && artifact.truncated }
}

fn append_display(
  existing: ResultArtifact,
  next: ResultArtifact,
  max_chars: Int,
) -> #(Option(String), Bool, String) {
  case existing.final_response, next.final_response {
    None, None -> #(None, False, "none")
    Some(text), None ->
      cap_existing(text, existing.truncated, existing.source, max_chars)
    None, Some(text) ->
      cap_existing(text, next.truncated, next.source, max_chars)
    Some(left), Some(right) -> {
      let combined = left <> "\n\n" <> right
      let newly_truncated = string.length(combined) > max_chars
      #(
        Some(log.truncate(combined, max_chars)),
        existing.truncated || next.truncated || newly_truncated,
        "combined_turns",
      )
    }
  }
}

fn append_structured(
  existing: ResultArtifact,
  next: ResultArtifact,
) -> #(Option(String), Bool) {
  let existing_response = structured_final_response(existing)
  let next_response = structured_final_response(next)
  let existing_truncated = structured_final_response_truncated(existing)
  let next_truncated = structured_final_response_truncated(next)
  case existing_response, next_response {
    None, None -> #(None, existing_truncated || next_truncated)
    Some(text), None -> #(Some(text), existing_truncated)
    None, Some(text) -> #(Some(text), next_truncated)
    Some(left), Some(right) -> #(
      Some(left <> "\n\n" <> right),
      existing_truncated || next_truncated,
    )
  }
}

fn build_result(
  text: String,
  source: String,
  secrets: List(String),
  max_chars: Int,
) -> ResultArtifact {
  let redacted = log.redact("assistant_output", text, secrets)
  ResultArtifact(
    final_response: Some(log.truncate(redacted, max_chars)),
    truncated: string.length(redacted) > max_chars,
    source: source,
    structured_response: Some(redacted),
    structured_response_truncated: False,
    tool_calls: [],
  )
}

fn cap_existing(
  text: String,
  already_truncated: Bool,
  source: String,
  max_chars: Int,
) -> #(Option(String), Bool, String) {
  let newly_truncated = string.length(text) > max_chars
  #(
    Some(log.truncate(text, max_chars)),
    already_truncated || newly_truncated,
    source,
  )
}

fn assistant_messages(records: List(protocol.RpcRecord)) -> List(String) {
  case records {
    [] -> []
    [record, ..rest] ->
      list.append(record.assistant_messages, assistant_messages(rest))
  }
}

type ObservedToolCall {
  ObservedToolCall(
    origin: ToolCallOrigin,
    call: protocol.ToolCallRecord,
    status: Option(String),
  )
}

type ToolCallOrigin {
  AssistantToolCall
  ToolExecutionStart
}

fn tool_call_submissions(
  records: List(protocol.RpcRecord),
) -> List(ToolCallSubmission) {
  let observed =
    records
    |> list.flat_map(fn(record) {
      record.tool_calls
      |> list.map(fn(call) {
        ObservedToolCall(
          origin: tool_call_origin(record),
          call: call,
          status: tool_status_for_call(records, call),
        )
      })
    })

  observed
  |> list.filter(fn(call) { !duplicate_execution_start_alias(call, observed) })
  |> unique_tool_observations([])
  |> list.map(fn(observed) {
    ToolCallSubmission(
      name: observed.call.name,
      arguments_json: observed.call.arguments_json,
      status: observed.status,
      sibling_count: observed.call.sibling_count,
    )
  })
}

fn tool_call_origin(record: protocol.RpcRecord) -> ToolCallOrigin {
  case string.starts_with(record.type_, "tool_execution_start") {
    True -> ToolExecutionStart
    False -> AssistantToolCall
  }
}

fn duplicate_execution_start_alias(
  observed: ObservedToolCall,
  all_observed: List(ObservedToolCall),
) -> Bool {
  case observed.origin {
    AssistantToolCall -> False
    ToolExecutionStart ->
      list.any(all_observed, fn(candidate) {
        candidate.origin == AssistantToolCall
        && same_tool_call_alias(candidate.call, observed.call)
      })
  }
}

fn unique_tool_observations(
  observed: List(ObservedToolCall),
  accepted: List(ObservedToolCall),
) -> List(ObservedToolCall) {
  case observed {
    [] -> list.reverse(accepted)
    [call, ..rest] -> {
      let #(replaced, accepted) = replace_same_id_observation(call, accepted)
      case replaced {
        True -> unique_tool_observations(rest, accepted)
        False ->
          case duplicate_of_accepted(call, accepted) {
            True -> unique_tool_observations(rest, accepted)
            False -> unique_tool_observations(rest, [call, ..accepted])
          }
      }
    }
  }
}

fn replace_same_id_observation(
  observed: ObservedToolCall,
  accepted: List(ObservedToolCall),
) -> #(Bool, List(ObservedToolCall)) {
  case accepted {
    [] -> #(False, [])
    [candidate, ..rest] ->
      case same_present_tool_call_id(candidate.call, observed.call) {
        True -> #(True, [observed, ..rest])
        False -> {
          let #(replaced, rest) = replace_same_id_observation(observed, rest)
          #(replaced, [candidate, ..rest])
        }
      }
  }
}

fn same_present_tool_call_id(
  left: protocol.ToolCallRecord,
  right: protocol.ToolCallRecord,
) -> Bool {
  case left.id, right.id {
    Some(left_id), Some(right_id) -> left_id == right_id
    _, _ -> False
  }
}

fn duplicate_of_accepted(
  observed: ObservedToolCall,
  accepted: List(ObservedToolCall),
) -> Bool {
  list.any(accepted, fn(candidate) {
    same_logical_tool_call(candidate.call, observed.call)
  })
}

fn same_logical_tool_call(
  left: protocol.ToolCallRecord,
  right: protocol.ToolCallRecord,
) -> Bool {
  case left.id, right.id {
    Some(left_id), Some(right_id) -> left_id == right_id
    None, None -> same_tool_call_name_and_arguments(left, right)
    _, _ -> False
  }
}

fn same_tool_call_alias(
  left: protocol.ToolCallRecord,
  right: protocol.ToolCallRecord,
) -> Bool {
  case left.id, right.id {
    Some(left_id), Some(right_id) -> left_id == right_id
    _, _ -> same_tool_call_name_and_arguments(left, right)
  }
}

fn same_tool_call_name_and_arguments(
  left: protocol.ToolCallRecord,
  right: protocol.ToolCallRecord,
) -> Bool {
  left.name == right.name
  && same_arguments_json(left.arguments_json, right.arguments_json)
}

fn same_arguments_json(left: Option(String), right: Option(String)) -> Bool {
  case left, right {
    Some(left), Some(right) ->
      case json_value.parse(left), json_value.parse(right) {
        Ok(left), Ok(right) -> left == right
        _, _ -> left == right
      }
    _, _ -> left == right
  }
}

fn tool_status_for_call(
  records: List(protocol.RpcRecord),
  call: protocol.ToolCallRecord,
) -> Option(String) {
  case call.id {
    Some(id) ->
      case tool_status_for_id(records, id) {
        Some(status) -> Some(status)
        None -> tool_status_for_name(records, call.name)
      }
    None -> tool_status_for_name(records, call.name)
  }
}

fn tool_status_for_id(
  records: List(protocol.RpcRecord),
  id: String,
) -> Option(String) {
  case records {
    [] -> None
    [record, ..rest] ->
      case record.tool_call_id == Some(id), record.tool_status {
        True, Some(status) -> Some(status)
        _, _ -> tool_status_for_id(rest, id)
      }
  }
}

fn tool_status_for_name(
  records: List(protocol.RpcRecord),
  name: String,
) -> Option(String) {
  case records {
    [] -> None
    [record, ..rest] ->
      case record.tool_name == Some(name), record.tool_status {
        True, Some(status) -> Some(status)
        _, _ -> tool_status_for_name(rest, name)
      }
  }
}

fn last_non_empty(values: List(String)) -> Option(String) {
  list.fold(values, None, fn(acc, value) {
    case non_empty(value) {
      Some(value) -> Some(value)
      None -> acc
    }
  })
}

fn non_empty(value: String) -> Option(String) {
  case string.trim(value) == "" {
    True -> None
    False -> Some(value)
  }
}
