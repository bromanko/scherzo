import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/log
import scherzo/pi/protocol

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
  ResultArtifact(
    final_response: final_response,
    truncated: truncated,
    source: source,
    structured_response: final_response,
    structured_response_truncated: truncated,
  )
}

pub fn from_records(
  records: List(protocol.RpcRecord),
  secrets: List(String),
  max_chars: Int,
) -> ResultArtifact {
  case last_non_empty(assistant_messages(records)) {
    Some(text) ->
      build_result(text, "completed_assistant_messages", secrets, max_chars)
    None -> empty()
  }
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
