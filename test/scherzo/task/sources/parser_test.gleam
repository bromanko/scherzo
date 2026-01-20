import gleeunit/should
import scherzo/task/sources/parser

pub fn parse_content_extracts_id_test() {
  let content =
    "---
id: s-1234
status: open
---
# Test Ticket

Description here"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.id |> should.equal("s-1234")
}

pub fn parse_content_extracts_title_test() {
  let content =
    "---
id: s-1234
---
# My Test Title

Description here"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.title |> should.equal("My Test Title")
}

pub fn parse_content_extracts_description_test() {
  let content =
    "---
id: s-1234
---
# Title

This is the description.
It spans multiple lines."

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.description
  |> should.equal("This is the description.\nIt spans multiple lines.")
}

pub fn parse_content_extracts_status_test() {
  let content =
    "---
id: s-1234
status: in_progress
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.status |> should.equal("in_progress")
}

pub fn parse_content_defaults_status_to_open_test() {
  let content =
    "---
id: s-1234
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.status |> should.equal("open")
}

pub fn parse_content_extracts_priority_test() {
  let content =
    "---
id: s-1234
priority: 1
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.priority |> should.equal(1)
}

pub fn parse_content_defaults_priority_to_2_test() {
  let content =
    "---
id: s-1234
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.priority |> should.equal(2)
}

pub fn parse_content_extracts_deps_test() {
  let content =
    "---
id: s-1234
deps: [s-0001, s-0002]
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.deps |> should.equal(["s-0001", "s-0002"])
}

pub fn parse_content_handles_empty_deps_test() {
  let content =
    "---
id: s-1234
deps: []
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.deps |> should.equal([])
}

pub fn parse_content_extracts_tags_test() {
  let content =
    "---
id: s-1234
tags: [bug, urgent]
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.tags |> should.equal(["bug", "urgent"])
}

pub fn parse_content_fails_without_id_test() {
  let content =
    "---
status: open
---
# Title"

  let result = parser.parse_content(content)

  result |> should.be_error
}

pub fn parse_content_fails_without_frontmatter_test() {
  let content = "# Just a title\n\nNo frontmatter"

  let result = parser.parse_content(content)

  result |> should.be_error
}

pub fn parse_content_handles_empty_body_test() {
  let content =
    "---
id: s-1234
---"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.title |> should.equal("")
  ticket.description |> should.equal("")
}

pub fn parse_content_handles_colons_in_title_test() {
  let content =
    "---
id: s-1234
---
# Fix: Something is broken

Details"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.title |> should.equal("Fix: Something is broken")
}

pub fn parse_content_ignores_unknown_frontmatter_fields_test() {
  let content =
    "---
id: s-1234
unknown_field: some value
another: thing
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.id |> should.equal("s-1234")
}

pub fn parse_content_handles_dashes_in_body_test() {
  let content =
    "---
id: s-1234
---
# Title

Some text with --- dashes in body"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  ticket.description |> should.equal("Some text with --- dashes in body")
}

pub fn parse_content_handles_invalid_priority_gracefully_test() {
  let content =
    "---
id: s-1234
priority: not-a-number
---
# Title"

  let result = parser.parse_content(content)
  let assert Ok(ticket) = result

  // Should fall back to default priority
  ticket.priority |> should.equal(2)
}
