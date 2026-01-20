/// Ticket markdown file parser
///
/// Parses .tickets/*.md files which have:
/// - YAML frontmatter (between --- markers)
/// - Markdown body with # title and description
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

/// Parsed ticket from a markdown file
pub type ParsedTicket {
  ParsedTicket(
    id: String,
    title: String,
    description: String,
    status: String,
    priority: Int,
    deps: List(String),
    tags: List(String),
  )
}

/// Parse a ticket markdown file
pub fn parse_file(path: String) -> Result(ParsedTicket, String) {
  case simplifile.read(path) {
    Error(err) ->
      Error("Failed to read ticket file: " <> simplifile.describe_error(err))
    Ok(content) -> parse_content(content)
  }
}

/// Parse ticket content (frontmatter + markdown body)
pub fn parse_content(content: String) -> Result(ParsedTicket, String) {
  case split_frontmatter(content) {
    Error(err) -> Error(err)
    Ok(#(frontmatter, body)) -> {
      let metadata = parse_frontmatter(frontmatter)
      let #(title, description) = parse_body(body)

      case metadata.id {
        None -> Error("Ticket missing required 'id' field")
        Some(id) ->
          Ok(ParsedTicket(
            id: id,
            title: title,
            description: description,
            status: option.unwrap(metadata.status, "open"),
            priority: option.unwrap(metadata.priority, 2),
            deps: metadata.deps,
            tags: metadata.tags,
          ))
      }
    }
  }
}

/// Split content into frontmatter and body
fn split_frontmatter(content: String) -> Result(#(String, String), String) {
  // Frontmatter is between --- markers
  case string.split(content, "---") {
    ["", frontmatter, ..rest] -> {
      let body = string.join(rest, "---")
      Ok(#(string.trim(frontmatter), string.trim(body)))
    }
    _ -> Error("Invalid ticket format: missing frontmatter")
  }
}

/// Intermediate type for parsing frontmatter fields
type FrontmatterData {
  FrontmatterData(
    id: Option(String),
    status: Option(String),
    priority: Option(Int),
    deps: List(String),
    tags: List(String),
  )
}

/// Parse YAML-like frontmatter (simple key: value format)
fn parse_frontmatter(frontmatter: String) -> FrontmatterData {
  let initial =
    FrontmatterData(id: None, status: None, priority: None, deps: [], tags: [])

  frontmatter
  |> string.split("\n")
  |> list.fold(initial, fn(acc, line) {
    let trimmed = string.trim(line)
    case string.split_once(trimmed, ":") {
      Error(_) -> acc
      Ok(#(key, value)) -> {
        let key = string.trim(key)
        let value = string.trim(value)
        case key {
          "id" -> FrontmatterData(..acc, id: Some(value))
          "status" -> FrontmatterData(..acc, status: Some(value))
          "priority" ->
            FrontmatterData(
              ..acc,
              priority: int.parse(value)
                |> result.map(Some)
                |> result.unwrap(acc.priority),
            )
          "deps" -> FrontmatterData(..acc, deps: parse_list(value))
          "tags" -> FrontmatterData(..acc, tags: parse_list(value))
          _ -> acc
        }
      }
    }
  })
}

/// Parse a YAML-style list: [item1, item2] or []
fn parse_list(value: String) -> List(String) {
  value
  |> string.replace("[", "")
  |> string.replace("]", "")
  |> string.split(",")
  |> list.map(string.trim)
  |> list.filter(fn(s) { s != "" })
}

/// Parse markdown body to extract title and description
fn parse_body(body: String) -> #(String, String) {
  let lines = string.split(body, "\n")

  // Find first # heading as title
  let #(title, rest) =
    lines
    |> list.fold_until(#("", lines), fn(acc, line) {
      let trimmed = string.trim(line)
      case string.starts_with(trimmed, "# ") {
        True -> {
          let title = string.drop_start(trimmed, 2)
          let remaining =
            acc.1
            |> list.drop_while(fn(l) { string.trim(l) != trimmed })
            |> list.drop(1)
          list.Stop(#(title, remaining))
        }
        False -> list.Continue(acc)
      }
    })

  // Rest is description
  let description =
    rest
    |> string.join("\n")
    |> string.trim

  #(title, description)
}

/// Read all tickets from a directory
pub fn read_all(tickets_dir: String) -> Result(List(ParsedTicket), String) {
  case simplifile.read_directory(tickets_dir) {
    Error(err) ->
      Error(
        "Failed to read tickets directory: " <> simplifile.describe_error(err),
      )
    Ok(files) -> {
      files
      |> list.filter(fn(f) { string.ends_with(f, ".md") })
      |> list.try_map(fn(filename) {
        parse_file(tickets_dir <> "/" <> filename)
      })
    }
  }
}
