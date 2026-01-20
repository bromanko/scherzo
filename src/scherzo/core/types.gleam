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
    /// Command execution timeout in milliseconds (0 = no timeout, default: 30 minutes)
    timeout_ms: Int,
  )
}

/// Default timeout: 30 minutes in milliseconds
pub const default_timeout_ms = 1_800_000
