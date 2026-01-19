/// Core domain types for Scherzo
/// Unique identifier for entities
pub type Id =
  String

/// Unix timestamp in milliseconds
pub type Timestamp =
  Int

/// Agent providers supported by Scherzo
pub type AgentProvider {
  Claude
  Codex
  Gemini
}

/// Current status of an agent
pub type AgentStatus {
  Idle
  Running(task_id: Id, started_at: Timestamp)
  Failed(reason: String)
}

/// Configuration for an agent instance
pub type AgentConfig {
  AgentConfig(
    id: Id,
    provider: AgentProvider,
    working_dir: String,
    max_retries: Int,
  )
}
