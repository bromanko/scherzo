import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub const stale_publication_guard_ms = 2_592_000_000

pub type ReviewState {
  PublicationGuard
  SafeToDelete
  ManualHold
  Abandoned
}

pub type Marker {
  LegacyManualHold
  SchemaMarker(
    review_state: ReviewState,
    created_at_ms: Int,
    source_kind: String,
    source_ref: String,
  )
  Malformed(reason: String)
}

pub fn parse(contents: String) -> Marker {
  let lines = string.split(contents, on: "\n")
  case line_value(lines, "Schema") {
    None -> LegacyManualHold
    Some("scherzo.retained-workspace.v1") -> parse_schema(lines)
    Some(other) ->
      Malformed("unsupported workspace retention marker schema: " <> other)
  }
}

pub fn review_state_to_string(state: ReviewState) -> String {
  case state {
    PublicationGuard -> "publication_guard"
    SafeToDelete -> "safe_to_delete"
    ManualHold -> "manual_hold"
    Abandoned -> "abandoned"
  }
}

fn parse_schema(lines: List(String)) -> Marker {
  case
    line_value(lines, "Review state"),
    line_value(lines, "Created at ms"),
    line_value(lines, "Source kind"),
    line_value(lines, "Source")
  {
    Some(review_state), Some(created_at_ms), Some(source_kind), Some(source_ref)
    ->
      case parse_review_state(review_state), int.parse(created_at_ms) {
        Ok(state), Ok(created_at_ms) ->
          SchemaMarker(state, created_at_ms, source_kind, source_ref)
        Error(_), _ ->
          Malformed(
            "unknown workspace retention review state: " <> review_state,
          )
        _, Error(_) ->
          Malformed(
            "workspace retention marker Created at ms is not an integer",
          )
      }
    _, _, _, _ ->
      Malformed(
        "workspace retention marker is missing required schema-backed fields",
      )
  }
}

fn parse_review_state(value: String) -> Result(ReviewState, Nil) {
  case string.trim(value) {
    "publication_guard" -> Ok(PublicationGuard)
    "safe_to_delete" -> Ok(SafeToDelete)
    "manual_hold" -> Ok(ManualHold)
    "abandoned" -> Ok(Abandoned)
    _ -> Error(Nil)
  }
}

fn line_value(lines: List(String), label: String) -> Option(String) {
  case
    lines
    |> list.filter_map(fn(line) {
      case string.starts_with(line, label <> ":") {
        True ->
          Ok(string.trim(string.drop_start(line, string.length(label) + 1)))
        False -> Error(Nil)
      }
    })
    |> list.first
  {
    Ok(value) -> Some(value)
    Error(Nil) -> None
  }
}
