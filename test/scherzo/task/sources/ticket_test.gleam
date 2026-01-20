import gleam/list
import gleeunit/should
import scherzo/core/task.{InProgress, Pending}
import scherzo/task/source
import scherzo/task/sources/ticket
import simplifile

pub fn new_creates_ticket_source_test() {
  let src = ticket.new(".tickets")

  source.name(src) |> should.equal("ticket")
}

pub fn default_tickets_dir_returns_correct_path_test() {
  let result = ticket.default_tickets_dir("/home/user/project")

  result |> should.equal("/home/user/project/.tickets")
}

pub fn default_tickets_dir_with_trailing_slash_test() {
  // Note: doesn't strip trailing slash, just appends
  let result = ticket.default_tickets_dir("/project/")

  result |> should.equal("/project//.tickets")
}

pub fn fetch_tasks_from_test_directory_test() {
  // Create a temp directory with test tickets
  let test_dir = "/tmp/claude/ticket_test_fetch"
  let _ = simplifile.create_directory_all(test_dir)

  // Write a test ticket
  let ticket_content =
    "---
id: test-001
status: open
priority: 1
deps: []
tags: [test]
---
# Test Ticket

This is a test."

  let _ = simplifile.write(test_dir <> "/test-001.md", ticket_content)

  let src = ticket.new(test_dir)
  let result = source.fetch_tasks(src)

  result |> should.be_ok
  let assert Ok(tasks) = result
  list.length(tasks) |> should.equal(1)
  let assert [task] = tasks
  task.id |> should.equal("test-001")
  task.title |> should.equal("Test Ticket")
  task.description |> should.equal("This is a test.")

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn fetch_tasks_returns_empty_for_empty_dir_test() {
  let test_dir = "/tmp/claude/ticket_test_empty"
  let _ = simplifile.create_directory_all(test_dir)

  let src = ticket.new(test_dir)
  let result = source.fetch_tasks(src)

  result |> should.be_ok
  let assert Ok(tasks) = result
  list.length(tasks) |> should.equal(0)

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn fetch_tasks_converts_priority_test() {
  let test_dir = "/tmp/claude/ticket_test_priority"
  let _ = simplifile.create_directory_all(test_dir)

  // Priority 1 = High
  let ticket_high =
    "---
id: high-task
status: open
priority: 1
deps: []
tags: []
---
# High Priority"

  let _ = simplifile.write(test_dir <> "/high.md", ticket_high)

  let src = ticket.new(test_dir)
  let assert Ok(tasks) = source.fetch_tasks(src)
  let assert [task] = tasks
  task.priority |> should.equal(task.High)

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn fetch_tasks_converts_status_pending_test() {
  let test_dir = "/tmp/claude/ticket_test_status_pending"
  let _ = simplifile.create_directory_all(test_dir)

  let ticket_content =
    "---
id: pending-task
status: open
priority: 2
deps: []
tags: []
---
# Pending Task"

  let _ = simplifile.write(test_dir <> "/pending.md", ticket_content)

  let src = ticket.new(test_dir)
  let assert Ok(tasks) = source.fetch_tasks(src)
  let assert [task] = tasks
  case task.status {
    Pending -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn fetch_tasks_converts_status_in_progress_test() {
  let test_dir = "/tmp/claude/ticket_test_status_progress"
  let _ = simplifile.create_directory_all(test_dir)

  let ticket_content =
    "---
id: progress-task
status: in_progress
priority: 2
deps: []
tags: []
---
# In Progress Task"

  let _ = simplifile.write(test_dir <> "/progress.md", ticket_content)

  let src = ticket.new(test_dir)
  let assert Ok(tasks) = source.fetch_tasks(src)
  let assert [task] = tasks
  case task.status {
    InProgress(_, _) -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn fetch_tasks_includes_dependencies_test() {
  let test_dir = "/tmp/claude/ticket_test_deps"
  let _ = simplifile.create_directory_all(test_dir)

  let ticket_content =
    "---
id: dep-task
status: open
priority: 2
deps: [task-a, task-b]
tags: []
---
# Task with Deps"

  let _ = simplifile.write(test_dir <> "/dep.md", ticket_content)

  let src = ticket.new(test_dir)
  let assert Ok(tasks) = source.fetch_tasks(src)
  let assert [task] = tasks
  task.dependencies |> should.equal(["task-a", "task-b"])

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn fetch_tasks_fails_for_nonexistent_dir_test() {
  let src = ticket.new("/nonexistent/path/to/tickets")
  let result = source.fetch_tasks(src)

  result |> should.be_error
}
