/// Task types and lifecycle management

import scherzo/core/types.{type Id, type Timestamp}

/// Task lifecycle status
pub type TaskStatus {
  /// Task created but dependencies not resolved
  Pending
  /// Dependencies resolved, ready to be assigned
  Ready
  /// Assigned to an agent but not started
  Assigned(agent_id: Id)
  /// Currently being worked on
  InProgress(agent_id: Id, started_at: Timestamp)
  /// Successfully completed
  Completed(agent_id: Id, completed_at: Timestamp)
  /// Failed with error
  Failed(agent_id: Id, reason: String, failed_at: Timestamp)
  /// Blocked and cannot proceed
  Blocked(reason: String)
}

/// Priority level for task ordering
pub type Priority {
  Low
  Normal
  High
  Critical
}

/// A task to be executed by an agent
pub type Task {
  Task(
    id: Id,
    title: String,
    description: String,
    status: TaskStatus,
    priority: Priority,
    dependencies: List(Id),
    created_at: Timestamp,
    updated_at: Timestamp,
    /// Source identifier (e.g., ticket ID)
    source_id: Option(String),
    /// jj change ID when work started
    jj_change_id: Option(String),
  )
}

/// Create a new task with default values
pub fn new(id: Id, title: String, description: String) -> Task {
  Task(
    id: id,
    title: title,
    description: description,
    status: Pending,
    priority: Normal,
    dependencies: [],
    created_at: 0,
    updated_at: 0,
    source_id: None,
    jj_change_id: None,
  )
}

/// Check if a task is in a terminal state
pub fn is_terminal(task: Task) -> Bool {
  case task.status {
    Completed(_, _) -> True
    Failed(_, _, _) -> True
    _ -> False
  }
}

/// Check if a task can be assigned
pub fn is_assignable(task: Task) -> Bool {
  case task.status {
    Ready -> True
    _ -> False
  }
}
