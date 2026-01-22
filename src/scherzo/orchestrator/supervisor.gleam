/// Orchestrator supervisor - manages the supervision tree
///
/// This module will eventually contain OTP supervisor logic.
/// For now, it provides the entry point and delegates to the coordinator.
///
/// TODO(Phase 5f): Implement OTP supervisor with:
/// - Event emission for task state changes
/// - Actor-based gate evaluation with supervision
/// - Parallel gate execution for independent gates
/// - Integration with TaskPhase for event-driven state tracking
import scherzo/core/task.{type Task}
import scherzo/orchestrator/coordinator.{
  type BatchResult, type CoordinatorConfig, type RunResult,
}
import scherzo/task/source.{type TaskSource}

/// Start the orchestrator supervisor
/// Currently a no-op since we don't have OTP actors yet
pub fn start() -> Result(Nil, String) {
  Ok(Nil)
}

/// Stop the orchestrator supervisor
pub fn stop() -> Result(Nil, String) {
  Ok(Nil)
}

/// Run a single task through the orchestrator
pub fn run_task(
  config: CoordinatorConfig,
  title: String,
  description: String,
) -> RunResult {
  coordinator.run_task(config, title, description)
}

/// Run tasks from a source through the orchestrator
pub fn run_from_source(
  config: CoordinatorConfig,
  task_source: TaskSource,
  max_tasks: Int,
) -> Result(BatchResult, String) {
  coordinator.run_from_source(config, task_source, max_tasks)
}

/// Run an existing task through the orchestrator
pub fn run_existing_task(config: CoordinatorConfig, task: Task) -> RunResult {
  coordinator.run_existing_task(config, task)
}
