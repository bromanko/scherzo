import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import scherzo/agent/checkpoint.{
  type Checkpoint, type CheckpointConfig, Added, Checkpoint, CheckpointConfig,
  Deleted, FileChange, Final, Incremental, Modified, PreCompact,
}
import scherzo/core/event
import simplifile

fn make_test_checkpoint() -> Checkpoint {
  Checkpoint(
    task_id: "task-123",
    agent_id: "agent-1",
    sequence: 5,
    timestamp: 1000,
    checkpoint_type: Incremental,
    files_modified: [],
    work_summary: "Did some work",
    next_steps: None,
    jj_change_id: "abc123",
  )
}

pub fn default_config_sets_checkpoints_dir_test() {
  let config = checkpoint.default_config("/home/user/repo")

  config.checkpoints_dir
  |> should.equal("/home/user/repo/.scherzo/checkpoints")
  config.repo_dir |> should.equal("/home/user/repo")
}

pub fn to_event_creates_checkpoint_created_event_test() {
  let cp = make_test_checkpoint()
  let evt = checkpoint.to_event(cp)

  case evt {
    event.CheckpointCreated(task_id, agent_id, checkpoint_id, timestamp) -> {
      task_id |> should.equal("task-123")
      agent_id |> should.equal("agent-1")
      checkpoint_id |> should.equal("task-123-5")
      timestamp |> should.equal(1000)
    }
    _ -> should.fail()
  }
}

pub fn to_event_includes_sequence_in_checkpoint_id_test() {
  let cp = Checkpoint(..make_test_checkpoint(), sequence: 42)
  let evt = checkpoint.to_event(cp)

  case evt {
    event.CheckpointCreated(_, _, checkpoint_id, _) -> {
      checkpoint_id |> should.equal("task-123-42")
    }
    _ -> should.fail()
  }
}

pub fn encode_produces_valid_json_test() {
  let cp = make_test_checkpoint()
  let json_str = checkpoint.encode(cp)

  json_str |> string.starts_with("{") |> should.be_true
  json_str |> string.ends_with("}") |> should.be_true
}

pub fn encode_includes_task_id_test() {
  let cp = make_test_checkpoint()
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"task_id\":\"task-123\"") |> should.be_true
}

pub fn encode_includes_agent_id_test() {
  let cp = make_test_checkpoint()
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"agent_id\":\"agent-1\"") |> should.be_true
}

pub fn encode_includes_sequence_test() {
  let cp = make_test_checkpoint()
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"sequence\":5") |> should.be_true
}

pub fn encode_includes_checkpoint_type_incremental_test() {
  let cp = Checkpoint(..make_test_checkpoint(), checkpoint_type: Incremental)
  let json_str = checkpoint.encode(cp)

  json_str
  |> string.contains("\"checkpoint_type\":\"incremental\"")
  |> should.be_true
}

pub fn encode_includes_checkpoint_type_pre_compact_test() {
  let cp = Checkpoint(..make_test_checkpoint(), checkpoint_type: PreCompact)
  let json_str = checkpoint.encode(cp)

  json_str
  |> string.contains("\"checkpoint_type\":\"pre_compact\"")
  |> should.be_true
}

pub fn encode_includes_checkpoint_type_final_test() {
  let cp = Checkpoint(..make_test_checkpoint(), checkpoint_type: Final)
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"checkpoint_type\":\"final\"") |> should.be_true
}

pub fn encode_includes_work_summary_test() {
  let cp = make_test_checkpoint()
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"work_summary\":\"Did some work\"") |> should.be_true
}

pub fn encode_includes_jj_change_id_test() {
  let cp = make_test_checkpoint()
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"jj_change_id\":\"abc123\"") |> should.be_true
}

pub fn encode_includes_next_steps_when_some_test() {
  let cp = Checkpoint(..make_test_checkpoint(), next_steps: Some("Finish tests"))
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"next_steps\":\"Finish tests\"") |> should.be_true
}

pub fn encode_includes_next_steps_null_when_none_test() {
  let cp = Checkpoint(..make_test_checkpoint(), next_steps: None)
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"next_steps\":null") |> should.be_true
}

pub fn encode_includes_files_modified_test() {
  let cp =
    Checkpoint(
      ..make_test_checkpoint(),
      files_modified: [
        FileChange(path: "src/main.gleam", change_type: Modified, summary: None),
        FileChange(
          path: "src/new.gleam",
          change_type: Added,
          summary: Some("New file"),
        ),
      ],
    )
  let json_str = checkpoint.encode(cp)

  json_str |> string.contains("\"files_modified\"") |> should.be_true
  json_str |> string.contains("src/main.gleam") |> should.be_true
  json_str |> string.contains("src/new.gleam") |> should.be_true
}

pub fn save_and_load_roundtrip_test() {
  // Create a temp checkpoint directory
  let test_dir = "/tmp/claude/checkpoint_test_roundtrip"
  let task_dir = test_dir <> "/task-test"
  let _ = simplifile.create_directory_all(task_dir)

  // Create and save a checkpoint
  let cp =
    Checkpoint(
      task_id: "task-test",
      agent_id: "agent-test",
      sequence: 1,
      timestamp: 5000,
      checkpoint_type: Final,
      files_modified: [
        FileChange(path: "test.gleam", change_type: Modified, summary: None),
      ],
      work_summary: "Test work summary",
      next_steps: Some("Next steps here"),
      jj_change_id: "test-change-id",
    )

  let config = CheckpointConfig(checkpoints_dir: test_dir, repo_dir: ".")

  // Save the checkpoint
  let save_result = checkpoint.save(config, cp)
  save_result |> should.be_ok

  // Load it back
  let load_result = checkpoint.load_latest(config, "task-test")
  load_result |> should.be_ok
  let assert Ok(loaded) = load_result

  loaded.task_id |> should.equal("task-test")
  loaded.agent_id |> should.equal("agent-test")
  loaded.sequence |> should.equal(1)
  loaded.timestamp |> should.equal(5000)
  loaded.work_summary |> should.equal("Test work summary")
  loaded.jj_change_id |> should.equal("test-change-id")

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn load_latest_returns_error_for_missing_task_test() {
  let config =
    CheckpointConfig(
      checkpoints_dir: "/tmp/claude/nonexistent-checkpoints",
      repo_dir: ".",
    )

  let result = checkpoint.load_latest(config, "nonexistent-task")

  result |> should.be_error
}

pub fn checkpoint_type_constructors_test() {
  // Just verify the types exist and can be constructed
  let _ = Incremental
  let _ = PreCompact
  let _ = Final
  should.be_true(True)
}

pub fn file_change_type_constructors_test() {
  // Verify file change types
  let _ = Added
  let _ = Modified
  let _ = Deleted
  should.be_true(True)
}
