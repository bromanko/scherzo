/// Handoff logic for agent continuity
///
/// When an agent exhausts its context, this module builds the continuation
/// prompt that allows a fresh agent to pick up where the previous one left off.
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/checkpoint.{
  type Checkpoint, type CheckpointConfig, type FileChange, Added, Deleted,
  Modified,
}
import scherzo/core/event
import scherzo/core/task.{type Task}
import scherzo/core/types.{type Id, type Timestamp}

/// Information needed to continue a task after agent handoff
pub type HandoffContext {
  HandoffContext(
    /// The task being continued
    task: Task,
    /// The checkpoint from the previous agent
    checkpoint: Checkpoint,
    /// Number of times this task has been handed off
    continuation_count: Int,
  )
}

/// Build a continuation prompt for a new agent
pub fn build_continuation_prompt(ctx: HandoffContext) -> String {
  let task = ctx.task
  let cp = ctx.checkpoint

  let header = "# Continue Task: " <> task.title

  let task_section = "## Task Description\n\n" <> task.description

  let previous_work_section = "## Previous Agent's Work\n\n" <> cp.work_summary

  let files_section = case cp.files_modified {
    [] -> ""
    files -> "## Files Modified\n\n" <> format_file_changes(files)
  }

  let next_steps_section = case cp.next_steps {
    None -> ""
    Some(steps) -> "## Remaining Work\n\n" <> steps
  }

  let context_section =
    "## Context\n\n"
    <> "- Task ID: "
    <> task.id
    <> "\n"
    <> "- jj Change ID: "
    <> cp.jj_change_id
    <> "\n"
    <> "- Continuation #"
    <> int.to_string(ctx.continuation_count)
    <> "\n"
    <> "- Review previous changes: `jj diff -r "
    <> cp.jj_change_id
    <> "`"

  let guidelines_section =
    "## Guidelines\n\n"
    <> "- Continue from where the previous agent left off\n"
    <> "- Review the modified files to understand current state\n"
    <> "- Focus on completing the remaining work\n"
    <> "- When complete, summarize what you accomplished"

  // Combine all sections
  [
    header,
    "",
    task_section,
    "",
    previous_work_section,
    "",
    files_section,
    next_steps_section,
    context_section,
    "",
    guidelines_section,
  ]
  |> list.filter(fn(s) { s != "" })
  |> string.join("\n")
}

/// Load handoff context from a checkpoint
pub fn load_handoff_context(
  config: CheckpointConfig,
  task: Task,
  continuation_count: Int,
) -> Result(HandoffContext, String) {
  case checkpoint.load_latest(config, task.id) {
    Error(err) -> Error("Failed to load checkpoint: " <> err)
    Ok(cp) ->
      Ok(HandoffContext(
        task: task,
        checkpoint: cp,
        continuation_count: continuation_count,
      ))
  }
}

/// Check if a task has exceeded max continuations
/// With max_continuations=5, we allow up to 5 continuations (counts 1-5),
/// plus the original run (count 0), for 6 total runs. Fails at count > 5.
pub fn should_fail_task(continuation_count: Int, max_continuations: Int) -> Bool {
  continuation_count > max_continuations
}

/// Create a HandoffInitiated event
pub fn to_event(
  task_id: Id,
  from_agent: Id,
  to_agent: Id,
  timestamp: Timestamp,
) -> event.Event {
  event.HandoffInitiated(
    task_id: task_id,
    from_agent: from_agent,
    to_agent: to_agent,
    timestamp: timestamp,
  )
}

/// Format file changes for display
fn format_file_changes(files: List(FileChange)) -> String {
  files
  |> list.map(fn(fc) {
    let status = case fc.change_type {
      Added -> "[A]"
      Modified -> "[M]"
      Deleted -> "[D]"
    }
    let summary = case fc.summary {
      None -> ""
      Some(s) -> " - " <> s
    }
    status <> " " <> fc.path <> summary
  })
  |> string.join("\n")
}
