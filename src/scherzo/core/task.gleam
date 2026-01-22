/// Task types and lifecycle management
import gleam/option.{type Option, None, Some}
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

/// Type of task for categorization
pub type TaskType {
  /// Regular task (default)
  RegularTask
  /// Epic (container for related tasks)
  Epic
  /// Bug fix
  Bug
  /// New feature
  Feature
}

/// A task to be executed by an agent
pub type Task {
  Task(
    id: Id,
    title: String,
    description: String,
    status: TaskStatus,
    priority: Priority,
    task_type: TaskType,
    dependencies: List(Id),
    created_at: Timestamp,
    updated_at: Timestamp,
    /// Source identifier (e.g., ticket ID)
    source_id: Option(String),
    /// jj change ID when work started
    jj_change_id: Option(String),
    /// Parent task ID for hierarchical organization
    parent: Option(Id),
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
    task_type: RegularTask,
    dependencies: [],
    created_at: 0,
    updated_at: 0,
    source_id: None,
    jj_change_id: None,
    parent: None,
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

/// Task execution phase - detailed state during task lifecycle
/// This tracks where a task is in the agent work -> gate evaluation cycle
///
/// TODO(Phase 5f): Integrate TaskPhase with event system for real-time
/// gate evaluation tracking. Currently the coordinator tracks gate iteration
/// as parameters; this type enables future event-driven architecture.
pub type TaskPhase {
  /// Agent is actively working on the task
  Working(agent_id: Id, iteration: Int)
  /// Agent signaled done, gates are being evaluated
  Evaluating(agent_id: Id, iteration: Int, gate_index: Int)
  /// Gates failed, feedback sent back to agent
  NeedsFixes(agent_id: Id, iteration: Int, feedback_summary: String)
  /// All gates passed, ready for merge queue
  GatesPassed(agent_id: Id, completed_at: Timestamp)
  /// Max iterations exceeded, needs human intervention
  Stuck(agent_id: Id, iterations: Int, last_feedback_summary: String)
}

/// Extended task with phase tracking for gate evaluation
///
/// TODO(Phase 5f): Use TaskWithPhase in supervisor for event-driven
/// task state management. Currently unused as coordinator tracks state
/// via function parameters.
pub type TaskWithPhase {
  TaskWithPhase(task: Task, phase: Option(TaskPhase))
}

/// Create a task with phase from a task
pub fn with_phase(task: Task, phase: TaskPhase) -> TaskWithPhase {
  TaskWithPhase(task: task, phase: Some(phase))
}

/// Create a task without phase tracking
pub fn without_phase(task: Task) -> TaskWithPhase {
  TaskWithPhase(task: task, phase: None)
}

/// Check if a task phase indicates gates are evaluating
pub fn is_evaluating(phase: TaskPhase) -> Bool {
  case phase {
    Evaluating(_, _, _) -> True
    _ -> False
  }
}

/// Check if a task phase indicates it needs fixes
pub fn needs_fixes(phase: TaskPhase) -> Bool {
  case phase {
    NeedsFixes(_, _, _) -> True
    _ -> False
  }
}

/// Check if a task phase indicates it's stuck
pub fn is_stuck(phase: TaskPhase) -> Bool {
  case phase {
    Stuck(_, _, _) -> True
    _ -> False
  }
}

/// Get the iteration count from a phase
pub fn phase_iteration(phase: TaskPhase) -> Int {
  case phase {
    Working(_, iteration) -> iteration
    Evaluating(_, iteration, _) -> iteration
    NeedsFixes(_, iteration, _) -> iteration
    GatesPassed(_, _) -> 0
    Stuck(_, iterations, _) -> iterations
  }
}
