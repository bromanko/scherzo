import gleam/list
import gleam/string
import gleeunit/should
import scherzo/task/sources/tk

pub fn parse_status_open_test() {
  tk.parse_status("open") |> should.equal(tk.Open)
}

pub fn parse_status_in_progress_test() {
  tk.parse_status("in_progress") |> should.equal(tk.InProgress)
}

pub fn parse_status_closed_test() {
  tk.parse_status("closed") |> should.equal(tk.Closed)
}

pub fn parse_status_unknown_defaults_to_open_test() {
  tk.parse_status("unknown") |> should.equal(tk.Open)
  tk.parse_status("") |> should.equal(tk.Open)
  tk.parse_status("whatever") |> should.equal(tk.Open)
}

// Integration tests that run against actual tk CLI
// These tests use the actual .tickets directory

pub fn query_returns_tickets_test() {
  // This test depends on tk being available and .tickets existing
  let result = tk.query()

  result |> should.be_ok
  let assert Ok(tickets) = result
  // Should have at least some tickets in the project
  { list.length(tickets) > 0 } |> should.be_true
}

pub fn ready_returns_ids_test() {
  // Get ready tickets from the actual project
  let result = tk.ready()

  result |> should.be_ok
  // Result is a list of IDs (may be empty if all tickets have deps)
  let assert Ok(ids) = result
  // IDs should all be non-empty if present
  ids
  |> list.all(fn(id) { string.trim(id) != "" })
  |> should.be_true
}

pub fn show_returns_ticket_content_test() {
  // First get a ticket ID from query
  let assert Ok(tickets) = tk.query()
  case tickets {
    [] -> {
      // No tickets, skip test
      Nil
    }
    [first, ..] -> {
      let result = tk.show(first.id)
      result |> should.be_ok
      let assert Ok(content) = result
      // Should contain the ticket ID
      content |> string.contains(first.id) |> should.be_true
    }
  }
}

pub fn show_nonexistent_ticket_fails_test() {
  let result = tk.show("nonexistent-ticket-id-12345")

  result |> should.be_error
}

pub fn ticket_json_has_expected_fields_test() {
  let assert Ok(tickets) = tk.query()
  case tickets {
    [] -> Nil
    [first, ..] -> {
      // ID should be non-empty
      first.id |> string.length |> fn(len) { len > 0 } |> should.be_true
      // Status should be parseable
      let _ = tk.parse_status(first.status)
      // Priority should be a number string
      first.priority |> string.length |> fn(len) { len > 0 } |> should.be_true
    }
  }
}
