import gleeunit/should
import scherzo/core/task.{
  Assigned, Blocked, Completed, Failed, InProgress, Pending, Ready,
}

pub fn new_creates_pending_task_test() {
  let t = task.new("task-1", "Test Task", "A test task description")

  t.id |> should.equal("task-1")
  t.title |> should.equal("Test Task")
  t.description |> should.equal("A test task description")
  t.status |> should.equal(Pending)
  t.dependencies |> should.equal([])
}

pub fn is_terminal_returns_true_for_completed_test() {
  let t =
    task.new("task-1", "Test", "desc")
    |> fn(t) { task.Task(..t, status: Completed("agent-1", 1000)) }

  task.is_terminal(t) |> should.be_true
}

pub fn is_terminal_returns_true_for_failed_test() {
  let t =
    task.new("task-1", "Test", "desc")
    |> fn(t) { task.Task(..t, status: Failed("agent-1", "error", 1000)) }

  task.is_terminal(t) |> should.be_true
}

pub fn is_terminal_returns_false_for_pending_test() {
  let t = task.new("task-1", "Test", "desc")

  task.is_terminal(t) |> should.be_false
}

pub fn is_terminal_returns_false_for_in_progress_test() {
  let t =
    task.new("task-1", "Test", "desc")
    |> fn(t) { task.Task(..t, status: InProgress("agent-1", 1000)) }

  task.is_terminal(t) |> should.be_false
}

pub fn is_assignable_returns_true_for_ready_test() {
  let t =
    task.new("task-1", "Test", "desc")
    |> fn(t) { task.Task(..t, status: Ready) }

  task.is_assignable(t) |> should.be_true
}

pub fn is_assignable_returns_false_for_pending_test() {
  let t = task.new("task-1", "Test", "desc")

  task.is_assignable(t) |> should.be_false
}

pub fn is_assignable_returns_false_for_assigned_test() {
  let t =
    task.new("task-1", "Test", "desc")
    |> fn(t) { task.Task(..t, status: Assigned("agent-1")) }

  task.is_assignable(t) |> should.be_false
}

pub fn is_assignable_returns_false_for_blocked_test() {
  let t =
    task.new("task-1", "Test", "desc")
    |> fn(t) { task.Task(..t, status: Blocked("missing dep")) }

  task.is_assignable(t) |> should.be_false
}
