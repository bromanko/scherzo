/// Event types for the event bus

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
}

/// Wrap an event with metadata
pub type EventEnvelope {
  EventEnvelope(id: Id, event: Event, timestamp: Timestamp)
}

/// Create a new event envelope
pub fn wrap(id: Id, event: Event, timestamp: Timestamp) -> EventEnvelope {
  EventEnvelope(id: id, event: event, timestamp: timestamp)
}
