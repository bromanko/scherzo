/// Task source interface - abstraction for task backends
///
/// Enables pluggable task sources (Ticket, Beans, etc.)
import scherzo/core/task.{type Task, type TaskStatus}
import scherzo/core/types.{type Id}

/// Abstract interface for task backends
/// Each source (Ticket, Beans, etc.) implements these callbacks
pub type TaskSource {
  TaskSource(
    /// Name of the source (e.g., "ticket", "beans")
    name: String,
    /// Fetch all tasks from the source
    fetch_tasks: fn() -> Result(List(Task), String),
    /// Update status of a task in the source
    update_status: fn(Id, TaskStatus) -> Result(Nil, String),
    /// Get tasks that are ready to work on (dependencies resolved)
    get_ready_tasks: fn() -> Result(List(Task), String),
  )
}

/// Get the name of a task source
pub fn name(source: TaskSource) -> String {
  source.name
}

/// Fetch all tasks from a source
pub fn fetch_tasks(source: TaskSource) -> Result(List(Task), String) {
  source.fetch_tasks()
}

/// Update task status in a source
pub fn update_status(
  source: TaskSource,
  task_id: Id,
  status: TaskStatus,
) -> Result(Nil, String) {
  source.update_status(task_id, status)
}

/// Get ready tasks from a source (dependencies resolved)
pub fn get_ready_tasks(source: TaskSource) -> Result(List(Task), String) {
  source.get_ready_tasks()
}
