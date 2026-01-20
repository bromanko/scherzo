import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import scherzo/agent/checkpoint.{
  type Checkpoint, Added, Checkpoint, Deleted, FileChange, Final, Modified,
}
import scherzo/agent/handoff.{HandoffContext}
import scherzo/core/event
import scherzo/core/task.{type Task, Normal, Pending, Task}

fn make_test_task() -> Task {
  Task(
    id: "task-123",
    title: "Test Task Title",
    description: "This is a test task description.",
    status: Pending,
    priority: Normal,
    dependencies: [],
    created_at: 1000,
    updated_at: 1000,
    source_id: None,
    jj_change_id: None,
  )
}

fn make_test_checkpoint() -> Checkpoint {
  Checkpoint(
    task_id: "task-123",
    agent_id: "agent-1",
    sequence: 1,
    timestamp: 2000,
    checkpoint_type: Final,
    files_modified: [],
    work_summary: "Did some work on the task.",
    next_steps: None,
    jj_change_id: "abc123",
  )
}

pub fn build_continuation_prompt_includes_header_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("# Continue Task: Test Task Title") |> should.be_true
}

pub fn build_continuation_prompt_includes_task_description_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Task Description") |> should.be_true
  prompt
  |> string.contains("This is a test task description.")
  |> should.be_true
}

pub fn build_continuation_prompt_includes_previous_work_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Previous Agent's Work") |> should.be_true
  prompt |> string.contains("Did some work on the task.") |> should.be_true
}

pub fn build_continuation_prompt_includes_context_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 3,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Context") |> should.be_true
  prompt |> string.contains("Task ID: task-123") |> should.be_true
  prompt |> string.contains("jj Change ID: abc123") |> should.be_true
  prompt |> string.contains("Continuation #3") |> should.be_true
}

pub fn build_continuation_prompt_includes_guidelines_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Guidelines") |> should.be_true
  prompt
  |> string.contains("Continue from where the previous agent left off")
  |> should.be_true
}

pub fn build_continuation_prompt_includes_files_when_present_test() {
  let checkpoint =
    Checkpoint(
      ..make_test_checkpoint(),
      files_modified: [
        FileChange(path: "src/main.gleam", change_type: Modified, summary: None),
        FileChange(path: "src/new.gleam", change_type: Added, summary: None),
      ],
    )
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: checkpoint,
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Files Modified") |> should.be_true
  prompt |> string.contains("[M] src/main.gleam") |> should.be_true
  prompt |> string.contains("[A] src/new.gleam") |> should.be_true
}

pub fn build_continuation_prompt_omits_files_when_empty_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Files Modified") |> should.be_false
}

pub fn build_continuation_prompt_includes_next_steps_when_present_test() {
  let checkpoint =
    Checkpoint(..make_test_checkpoint(), next_steps: Some("Finish the tests"))
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: checkpoint,
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Remaining Work") |> should.be_true
  prompt |> string.contains("Finish the tests") |> should.be_true
}

pub fn build_continuation_prompt_omits_next_steps_when_none_test() {
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: make_test_checkpoint(),
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("## Remaining Work") |> should.be_false
}

pub fn build_continuation_prompt_formats_deleted_files_test() {
  let checkpoint =
    Checkpoint(
      ..make_test_checkpoint(),
      files_modified: [
        FileChange(path: "src/old.gleam", change_type: Deleted, summary: None),
      ],
    )
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: checkpoint,
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt |> string.contains("[D] src/old.gleam") |> should.be_true
}

pub fn build_continuation_prompt_includes_file_summary_test() {
  let checkpoint =
    Checkpoint(
      ..make_test_checkpoint(),
      files_modified: [
        FileChange(
          path: "src/main.gleam",
          change_type: Modified,
          summary: Some("Added error handling"),
        ),
      ],
    )
  let ctx =
    HandoffContext(
      task: make_test_task(),
      checkpoint: checkpoint,
      continuation_count: 1,
    )

  let prompt = handoff.build_continuation_prompt(ctx)

  prompt
  |> string.contains("[M] src/main.gleam - Added error handling")
  |> should.be_true
}

pub fn should_fail_task_returns_true_when_exceeded_test() {
  handoff.should_fail_task(5, 5) |> should.be_true
  handoff.should_fail_task(6, 5) |> should.be_true
}

pub fn should_fail_task_returns_false_when_under_limit_test() {
  handoff.should_fail_task(0, 5) |> should.be_false
  handoff.should_fail_task(4, 5) |> should.be_false
}

pub fn to_event_creates_handoff_initiated_event_test() {
  let evt = handoff.to_event("task-1", "agent-old", "agent-new", 3000)

  case evt {
    event.HandoffInitiated(task_id, from_agent, to_agent, timestamp) -> {
      task_id |> should.equal("task-1")
      from_agent |> should.equal("agent-old")
      to_agent |> should.equal("agent-new")
      timestamp |> should.equal(3000)
    }
    _ -> should.fail()
  }
}
