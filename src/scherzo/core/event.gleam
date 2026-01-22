/// Event types for the event bus
import gleam/option.{type Option}
import scherzo/core/task.{type TaskStatus}
import scherzo/core/types.{type AgentStatus, type Id, type Timestamp}

/// Events that flow through the system
pub type Event {
  // Task events
  TaskCreated(task_id: Id, title: String, timestamp: Timestamp)
  TaskStatusChanged(
    task_id: Id,
    from: TaskStatus,
    to: TaskStatus,
    timestamp: Timestamp,
  )
  TaskAssigned(task_id: Id, agent_id: Id, timestamp: Timestamp)
  TaskCompleted(task_id: Id, agent_id: Id, timestamp: Timestamp)
  TaskFailed(task_id: Id, agent_id: Id, reason: String, timestamp: Timestamp)

  // Agent events
  AgentSpawned(agent_id: Id, timestamp: Timestamp)
  AgentStatusChanged(
    agent_id: Id,
    from: AgentStatus,
    to: AgentStatus,
    timestamp: Timestamp,
  )
  AgentStopped(agent_id: Id, reason: String, timestamp: Timestamp)

  // Checkpoint events
  CheckpointCreated(
    task_id: Id,
    agent_id: Id,
    checkpoint_id: String,
    timestamp: Timestamp,
  )
  HandoffInitiated(
    task_id: Id,
    from_agent: Id,
    to_agent: Id,
    timestamp: Timestamp,
  )

  // System events
  OrchestratorStarted(timestamp: Timestamp)
  OrchestratorStopping(timestamp: Timestamp)
  OrchestratorStopped(timestamp: Timestamp)

  // Gate events
  // TODO(Phase 5f): Emit these events from coordinator/supervisor for
  // real-time gate evaluation tracking and UI updates. Currently gates
  // are executed synchronously without event emission.
  /// Task completion signal received (agent thinks it's done)
  TaskCompletionSignaled(task_id: Id, agent_id: Id, timestamp: Timestamp)
  /// Gate evaluation started
  GateEvaluationStarted(task_id: Id, iteration: Int, timestamp: Timestamp)
  /// Individual gate completed
  GateCompleted(
    task_id: Id,
    gate_name: String,
    passed: Bool,
    timestamp: Timestamp,
  )
  /// All gates passed
  AllGatesPassed(task_id: Id, iteration: Int, timestamp: Timestamp)
  /// Gate failed, retry triggered
  GateFailedRetrying(
    task_id: Id,
    gate_name: String,
    iteration: Int,
    timestamp: Timestamp,
  )
  /// Max iterations reached
  GateFailedMaxIterations(task_id: Id, iterations: Int, timestamp: Timestamp)
  /// Human review requested
  HumanReviewRequested(task_id: Id, prompt: String, timestamp: Timestamp)
  /// Human approved/rejected
  HumanReviewCompleted(
    task_id: Id,
    approved: Bool,
    comment: Option(String),
    timestamp: Timestamp,
  )
}

/// Wrap an event with metadata
pub type EventEnvelope {
  EventEnvelope(id: Id, event: Event, timestamp: Timestamp)
}

/// Create a new event envelope
pub fn wrap(id: Id, event: Event, timestamp: Timestamp) -> EventEnvelope {
  EventEnvelope(id: id, event: event, timestamp: timestamp)
}
