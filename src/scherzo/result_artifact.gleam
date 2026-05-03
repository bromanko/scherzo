import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/log
import scherzo/pi/protocol

pub fn empty() -> domain.ResultArtifact {
  domain.ResultArtifact(final_response: None, truncated: False, source: "none")
}

pub fn from_records(
  records: List(protocol.RpcRecord),
  secrets: List(String),
  max_chars: Int,
) -> domain.ResultArtifact {
  case last_non_empty(assistant_messages(records)) {
    Some(text) -> build_result(text, "agent_end_messages", secrets, max_chars)
    None ->
      case concatenated_deltas(records) {
        Some(text) ->
          build_result(text, "message_update_delta", secrets, max_chars)
        None -> empty()
      }
  }
}

pub fn append(
  existing: domain.ResultArtifact,
  next: domain.ResultArtifact,
  max_chars: Int,
) -> domain.ResultArtifact {
  case existing.final_response, next.final_response {
    None, None -> empty()
    Some(text), None ->
      cap_existing(text, existing.truncated, existing.source, max_chars)
    None, Some(text) ->
      cap_existing(text, next.truncated, next.source, max_chars)
    Some(left), Some(right) -> {
      let combined = left <> "\n\n" <> right
      let newly_truncated = string.length(combined) > max_chars
      domain.ResultArtifact(
        final_response: Some(log.truncate(combined, max_chars)),
        truncated: existing.truncated || next.truncated || newly_truncated,
        source: "combined_turns",
      )
    }
  }
}

fn build_result(
  text: String,
  source: String,
  secrets: List(String),
  max_chars: Int,
) -> domain.ResultArtifact {
  let redacted = log.redact("assistant_output", text, secrets)
  domain.ResultArtifact(
    final_response: Some(log.truncate(redacted, max_chars)),
    truncated: string.length(redacted) > max_chars,
    source: source,
  )
}

fn cap_existing(
  text: String,
  already_truncated: Bool,
  source: String,
  max_chars: Int,
) -> domain.ResultArtifact {
  let newly_truncated = string.length(text) > max_chars
  domain.ResultArtifact(
    final_response: Some(log.truncate(text, max_chars)),
    truncated: already_truncated || newly_truncated,
    source: source,
  )
}

fn assistant_messages(records: List(protocol.RpcRecord)) -> List(String) {
  case records {
    [] -> []
    [record, ..rest] ->
      list.append(record.assistant_messages, assistant_messages(rest))
  }
}

fn concatenated_deltas(records: List(protocol.RpcRecord)) -> Option(String) {
  let values = delta_values(records)
  case list.is_empty(values) {
    True -> None
    False -> {
      let text = string.join(values, with: "")
      case string.length(text) == 0 {
        True -> None
        False -> Some(text)
      }
    }
  }
}

fn delta_values(records: List(protocol.RpcRecord)) -> List(String) {
  case records {
    [] -> []
    [record, ..rest] -> {
      let rest = delta_values(rest)
      case record.type_, record.delta {
        "message_update", Some(delta) -> [delta, ..rest]
        _, _ -> rest
      }
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
