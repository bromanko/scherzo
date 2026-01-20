/// tk CLI wrapper - interfaces with the ticket system CLI
///
/// Provides low-level access to tk commands for ticket management.
/// Note: Shell injection is not possible because shellout.command()
/// passes arguments as a list, not through shell interpolation.
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import scherzo/agent/workspace
import shellout

/// Maximum length for note content
const max_note_length = 10_000

/// Raw ticket data from tk query (JSON output)
pub type TicketJson {
  TicketJson(
    id: String,
    status: String,
    deps: List(String),
    ticket_type: String,
    priority: String,
    tags: List(String),
  )
}

/// Query all tickets as JSON
/// Returns one TicketJson per ticket (without title/description)
pub fn query() -> Result(List(TicketJson), String) {
  case run_tk(["query"]) {
    Error(err) -> Error(err)
    Ok(output) -> parse_query_output(output)
  }
}

/// Get IDs of tickets ready to work on (dependencies resolved)
pub fn ready() -> Result(List(String), String) {
  case run_tk(["ready"]) {
    Error(err) -> Error(err)
    Ok(output) -> {
      // Output format: "id   [status] - title <- [deps]" per line
      // We just need the IDs (first column)
      // Safe: non-empty lines (filtered above) always have at least one element
      // after split(" "), and we filter empty IDs as extra safety
      let ids =
        output
        |> string.split("\n")
        |> list.filter(fn(line) { string.trim(line) != "" })
        |> list.map(fn(line) {
          line
          |> string.split(" ")
          |> list.first
          |> result.unwrap("")
        })
        |> list.filter(fn(id) { id != "" })
      Ok(ids)
    }
  }
}

/// Start a ticket (set status to in_progress)
/// ID is sanitized to prevent path traversal or injection
pub fn start(id: String) -> Result(Nil, String) {
  let safe_id = workspace.sanitize_id(id)
  case run_tk(["start", safe_id]) {
    Error(err) -> Error(err)
    Ok(_) -> Ok(Nil)
  }
}

/// Close a ticket (set status to closed)
/// ID is sanitized to prevent path traversal or injection
pub fn close(id: String) -> Result(Nil, String) {
  let safe_id = workspace.sanitize_id(id)
  case run_tk(["close", safe_id]) {
    Error(err) -> Error(err)
    Ok(_) -> Ok(Nil)
  }
}

/// Add a note to a ticket
/// ID is sanitized and note is truncated to max_note_length
/// Note: Shell injection is not possible - shellout uses arg lists
pub fn add_note(id: String, note: String) -> Result(Nil, String) {
  let safe_id = workspace.sanitize_id(id)
  let safe_note = string.slice(note, 0, max_note_length)
  case run_tk(["add-note", safe_id, safe_note]) {
    Error(err) -> Error(err)
    Ok(_) -> Ok(Nil)
  }
}

/// Get ticket by ID (returns raw show output)
/// ID is sanitized to prevent path traversal or injection
pub fn show(id: String) -> Result(String, String) {
  let safe_id = workspace.sanitize_id(id)
  run_tk(["show", safe_id])
}

/// Run a tk command and return output
fn run_tk(args: List(String)) -> Result(String, String) {
  case shellout.command(run: "tk", with: args, in: ".", opt: []) {
    Ok(output) -> Ok(output)
    Error(#(_, err_output)) -> Error("tk error: " <> err_output)
  }
}

/// Parse the JSONL output from tk query
fn parse_query_output(output: String) -> Result(List(TicketJson), String) {
  output
  |> string.split("\n")
  |> list.filter(fn(line) { string.trim(line) != "" })
  |> list.try_map(parse_ticket_json)
}

/// Parse a single ticket JSON line
fn parse_ticket_json(line: String) -> Result(TicketJson, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use status <- decode.field("status", decode.string)
    use deps <- decode.field("deps", decode.list(decode.string))
    use ticket_type <- decode.field("type", decode.string)
    use priority <- decode.field("priority", decode.string)
    use tags <- decode.field("tags", decode.list(decode.string))
    decode.success(TicketJson(
      id: id,
      status: status,
      deps: deps,
      ticket_type: ticket_type,
      priority: priority,
      tags: tags,
    ))
  }

  case json.parse(line, decoder) {
    Error(_) -> Error("Failed to parse ticket JSON: " <> line)
    Ok(ticket) -> Ok(ticket)
  }
}

/// Convert tk status string to a simple status enum
pub type TicketStatus {
  Open
  InProgress
  Closed
}

/// Parse status string from tk
pub fn parse_status(status_str: String) -> TicketStatus {
  case status_str {
    "in_progress" -> InProgress
    "closed" -> Closed
    _ -> Open
  }
}
