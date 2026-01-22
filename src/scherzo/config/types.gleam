/// Configuration types for Scherzo
///
/// Defines the configuration structures for completion gates, retry behavior,
/// and review dimensions.
import gleam/option.{type Option}

/// Top-level Scherzo configuration
pub type ScherzoConfig {
  ScherzoConfig(
    /// Gate configuration
    gates: GatesConfig,
    /// Retry configuration
    retry: RetryConfig,
  )
}

/// Configuration for completion gates
pub type GatesConfig {
  GatesConfig(
    /// Formula name to use as base ("code-review", "security-audit", "quick-review", "none")
    formula: Option(String),
    /// List of gates to execute in order
    gates: List(GateConfig),
  )
}

/// Configuration for an individual gate
pub type GateConfig {
  /// Command gate - runs a shell command
  CommandGate(
    name: String,
    command: String,
    /// Timeout in milliseconds (0 = use default)
    timeout_ms: Int,
    /// If true, must pass before subsequent gates run
    serial: Bool,
    /// If true, stop task immediately on failure (don't retry)
    fail_fast: Bool,
  )
  /// Parallel review gate - spawns multiple review agents
  ParallelReviewGate(
    name: String,
    dimensions: List(ReviewDimension),
    synthesis_prompt: String,
  )
  /// Multi-pass review gate - sequential refinement passes
  MultiPassReviewGate(
    name: String,
    passes: List(ReviewPass),
    require_convergence: Bool,
    max_passes: Int,
  )
  /// Human approval gate - blocks until human approves
  HumanGate(name: String, prompt: String)
}

/// A dimension for parallel code review
pub type ReviewDimension {
  ReviewDimension(
    /// Unique identifier (e.g., "correctness", "security")
    id: String,
    /// What this dimension focuses on
    focus: String,
    /// Prompt template for the review agent
    prompt: String,
  )
}

/// A pass in multi-pass review (Rule of Five)
pub type ReviewPass {
  ReviewPass(
    /// Focus area for this pass
    focus: String,
    /// Prompt for this pass
    prompt: String,
  )
}

/// Configuration for retry behavior
pub type RetryConfig {
  RetryConfig(
    /// Strategy for retries: "same", "fresh", or "auto"
    strategy: RetryStrategy,
    /// Switch to fresh agent after this many same-agent failures
    fresh_after_failures: Int,
    /// Maximum total iterations before marking task as stuck
    max_iterations: Int,
  )
}

/// Strategy for choosing retry agent
pub type RetryStrategy {
  /// Re-trigger same agent (default)
  SameAgent
  /// Always spawn fresh agent
  FreshAgent
  /// Auto-decide based on failure count
  Auto
}

/// Create default configuration
pub fn default_config() -> ScherzoConfig {
  ScherzoConfig(gates: default_gates_config(), retry: default_retry_config())
}

/// Create default gates configuration (no gates)
pub fn default_gates_config() -> GatesConfig {
  GatesConfig(formula: option.None, gates: [])
}

/// Create default retry configuration
pub fn default_retry_config() -> RetryConfig {
  RetryConfig(strategy: Auto, fresh_after_failures: 2, max_iterations: 3)
}

/// Get gate name for any gate type
pub fn gate_name(gate: GateConfig) -> String {
  case gate {
    CommandGate(name, ..) -> name
    ParallelReviewGate(name, ..) -> name
    MultiPassReviewGate(name, ..) -> name
    HumanGate(name, ..) -> name
  }
}
