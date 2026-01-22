/// Orchestrator - coordinates tasks and agents
///
/// This module re-exports from the split orchestrator modules for
/// backward compatibility. New code should import directly from
/// scherzo/orchestrator/coordinator or scherzo/orchestrator/supervisor.
import scherzo/core/task.{type Task}
import scherzo/orchestrator/coordinator
import scherzo/task/source.{type TaskSource}

/// Re-export types from coordinator
pub type OrchestratorConfig =
  coordinator.CoordinatorConfig

pub type RunResult =
  coordinator.RunResult

pub type TaskResult =
  coordinator.TaskResult

pub type BatchResult =
  coordinator.BatchResult

/// Create default config - delegates to coordinator
pub fn default_config(working_dir: String) -> OrchestratorConfig {
  coordinator.default_config(working_dir)
}

/// Create config with gates enabled - delegates to coordinator
pub fn config_with_gates(working_dir: String) -> OrchestratorConfig {
  coordinator.config_with_gates(working_dir)
}

/// Run a single task - delegates to coordinator
pub fn run_task(
  config: OrchestratorConfig,
  title: String,
  description: String,
) -> RunResult {
  coordinator.run_task(config, title, description)
}

/// Run tasks from a source - delegates to coordinator
pub fn run_from_source(
  config: OrchestratorConfig,
  task_source: TaskSource,
  max_tasks: Int,
) -> Result(BatchResult, String) {
  coordinator.run_from_source(config, task_source, max_tasks)
}

/// Run an existing task - delegates to coordinator
pub fn run_existing_task(config: OrchestratorConfig, t: Task) -> RunResult {
  coordinator.run_existing_task(config, t)
}
