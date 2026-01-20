/// TicketSource - TaskSource implementation for the ticket system
///
/// Combines tk CLI wrapper and ticket parser to provide task management
/// backed by .tickets/ markdown files.
import gleam/list
import gleam/option.{None, Some}
import scherzo/core/task.{
  type Task, type TaskStatus, Completed, Failed, InProgress, Normal, Pending,
  Ready, Task,
}
import scherzo/core/types.{type Id}
import scherzo/task/source.{type TaskSource, TaskSource}
import scherzo/task/sources/parser
import scherzo/task/sources/tk

/// Create a TicketSource for a given tickets directory
pub fn new(tickets_dir: String) -> TaskSource {
  TaskSource(
    name: "ticket",
    fetch_tasks: fn() { fetch_tasks(tickets_dir) },
    update_status: fn(id, status) { update_status(id, status) },
    get_ready_tasks: fn() { get_ready_tasks(tickets_dir) },
  )
}

/// Fetch all tasks from tickets
fn fetch_tasks(tickets_dir: String) -> Result(List(Task), String) {
  case parser.read_all(tickets_dir) {
    Error(err) -> Error(err)
    Ok(tickets) -> Ok(list.map(tickets, ticket_to_task))
  }
}

/// Update task status in the ticket system
fn update_status(id: Id, status: TaskStatus) -> Result(Nil, String) {
  case status {
    InProgress(_, _) -> tk.start(id)
    Completed(_, _) -> tk.close(id)
    Failed(_, reason, _) -> {
      // Add error note then keep open
      let _ = tk.add_note(id, "Failed: " <> reason)
      Ok(Nil)
    }
    _ -> Ok(Nil)
  }
}

/// Get tasks that are ready (dependencies resolved)
fn get_ready_tasks(tickets_dir: String) -> Result(List(Task), String) {
  // Get ready IDs from tk
  case tk.ready() {
    Error(err) -> Error(err)
    Ok(ready_ids) -> {
      // Read all tickets and filter to ready ones
      case parser.read_all(tickets_dir) {
        Error(err) -> Error(err)
        Ok(tickets) -> {
          let ready_tasks =
            tickets
            |> list.filter(fn(t) { list.contains(ready_ids, t.id) })
            |> list.map(ticket_to_task)
            // Mark them as Ready status
            |> list.map(fn(t) { Task(..t, status: Ready) })
          Ok(ready_tasks)
        }
      }
    }
  }
}

/// Convert a parsed ticket to a Task
fn ticket_to_task(ticket: parser.ParsedTicket) -> Task {
  let status = case ticket.status {
    "in_progress" -> InProgress("", 0)
    "closed" -> Completed("", 0)
    _ -> Pending
  }

  let priority = case ticket.priority {
    0 -> task.Critical
    1 -> task.High
    3 -> task.Low
    _ -> Normal
  }

  Task(
    id: ticket.id,
    title: ticket.title,
    description: ticket.description,
    status: status,
    priority: priority,
    dependencies: ticket.deps,
    created_at: 0,
    updated_at: 0,
    source_id: Some(ticket.id),
    jj_change_id: None,
  )
}

/// Get the default tickets directory for a working directory
pub fn default_tickets_dir(working_dir: String) -> String {
  working_dir <> "/.tickets"
}
