import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import scherzo/core/task
import scherzo/core/types.{AgentConfig, Claude, Idle}
import scherzo/state/store.{AgentState, StoreConfig}
import simplifile

pub fn start_creates_store_test() {
  let result = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  result |> should.be_ok
}

pub fn save_and_get_task_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let t = task.new("task-1", "Test Task", "A test description")

  // Save the task
  store.save_task(s, t)

  // Retrieve it
  let retrieved = store.get_task(s, "task-1")
  retrieved |> should.equal(Some(t))

  // Cleanup
  store.stop(s)
}

pub fn get_nonexistent_task_returns_none_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let retrieved = store.get_task(s, "nonexistent")
  retrieved |> should.equal(None)

  store.stop(s)
}

pub fn get_all_tasks_returns_all_saved_tasks_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let t1 = task.new("task-1", "Task 1", "desc")
  let t2 = task.new("task-2", "Task 2", "desc")

  store.save_task(s, t1)
  store.save_task(s, t2)

  let all_tasks = store.get_all_tasks(s)
  all_tasks |> list.length |> should.equal(2)

  store.stop(s)
}

pub fn delete_task_removes_task_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let t = task.new("task-1", "Test Task", "desc")
  store.save_task(s, t)

  // Verify it exists
  store.get_task(s, "task-1") |> should.equal(Some(t))

  // Delete it
  store.delete_task(s, "task-1")

  // Verify it's gone
  store.get_task(s, "task-1") |> should.equal(None)

  store.stop(s)
}

pub fn save_and_get_agent_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp",
      max_retries: 3,
      timeout_ms: 60_000,
    )
  let agent = AgentState(config: config, status: Idle)

  store.save_agent(s, agent)

  let retrieved = store.get_agent(s, "agent-1")
  retrieved |> should.equal(Some(agent))

  store.stop(s)
}

pub fn get_nonexistent_agent_returns_none_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let retrieved = store.get_agent(s, "nonexistent")
  retrieved |> should.equal(None)

  store.stop(s)
}

pub fn get_all_agents_returns_all_saved_agents_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let config1 =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp",
      max_retries: 3,
      timeout_ms: 60_000,
    )
  let config2 =
    AgentConfig(
      id: "agent-2",
      provider: Claude,
      working_dir: "/tmp",
      max_retries: 3,
      timeout_ms: 60_000,
    )
  let agent1 = AgentState(config: config1, status: Idle)
  let agent2 = AgentState(config: config2, status: Idle)

  store.save_agent(s, agent1)
  store.save_agent(s, agent2)

  let all_agents = store.get_all_agents(s)
  all_agents |> list.length |> should.equal(2)

  store.stop(s)
}

pub fn delete_agent_removes_agent_test() {
  let assert Ok(s) = store.start(StoreConfig(state_dir: "/tmp/scherzo-test"))

  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp",
      max_retries: 3,
      timeout_ms: 60_000,
    )
  let agent = AgentState(config: config, status: Idle)

  store.save_agent(s, agent)
  store.get_agent(s, "agent-1") |> should.equal(Some(agent))

  store.delete_agent(s, "agent-1")
  store.get_agent(s, "agent-1") |> should.equal(None)

  store.stop(s)
}

pub fn persist_and_load_roundtrip_test() {
  // Use unique directory for this test
  let test_dir = "/tmp/claude/store_persist_test"
  let _ = simplifile.create_directory_all(test_dir)
  let _ = simplifile.delete(test_dir <> "/tasks.json")
  let _ = simplifile.delete(test_dir <> "/agents.json")

  // Start store and add data
  let assert Ok(s1) = store.start(StoreConfig(state_dir: test_dir))

  let t = task.new("persist-task-1", "Persist Test", "Test persistence")
  store.save_task(s1, t)

  let config =
    AgentConfig(
      id: "persist-agent-1",
      provider: Claude,
      working_dir: "/tmp",
      max_retries: 3,
      timeout_ms: 60_000,
    )
  let agent = AgentState(config: config, status: Idle)
  store.save_agent(s1, agent)

  // Persist and stop
  store.persist(s1)
  store.stop(s1)

  // Start new store and load
  let assert Ok(s2) = store.start(StoreConfig(state_dir: test_dir))
  store.load(s2)

  // Give time for async load message to process
  process.sleep(100)

  // Verify data was persisted
  let loaded_task = store.get_task(s2, "persist-task-1")
  loaded_task |> option.is_some |> should.be_true

  let loaded_agent = store.get_agent(s2, "persist-agent-1")
  loaded_agent |> option.is_some |> should.be_true

  store.stop(s2)

  // Cleanup
  let _ = simplifile.delete(test_dir)
  Nil
}

pub fn load_handles_missing_files_gracefully_test() {
  // Use a directory without any JSON files
  let test_dir = "/tmp/claude/store_empty_test"
  let _ = simplifile.create_directory_all(test_dir)
  let _ = simplifile.delete(test_dir <> "/tasks.json")
  let _ = simplifile.delete(test_dir <> "/agents.json")

  let assert Ok(s) = store.start(StoreConfig(state_dir: test_dir))
  store.load(s)

  // Should have no tasks or agents
  let tasks = store.get_all_tasks(s)
  tasks |> list.length |> should.equal(0)

  let agents = store.get_all_agents(s)
  agents |> list.length |> should.equal(0)

  store.stop(s)
}

pub fn persist_with_various_task_statuses_test() {
  let test_dir = "/tmp/claude/store_status_test"
  let _ = simplifile.create_directory_all(test_dir)
  let _ = simplifile.delete(test_dir <> "/tasks.json")

  let assert Ok(s1) = store.start(StoreConfig(state_dir: test_dir))

  // Save tasks with different statuses
  let t1 = task.new("status-1", "Pending", "desc")
  let t2 = task.Task(..t1, id: "status-2", status: task.Ready)
  let t3 =
    task.Task(..t1, id: "status-3", status: task.InProgress("agent-1", 1000))
  let t4 =
    task.Task(..t1, id: "status-4", status: task.Completed("agent-1", 2000))
  let t5 =
    task.Task(
      ..t1,
      id: "status-5",
      status: task.Failed("agent-1", "error", 3000),
    )

  store.save_task(s1, t1)
  store.save_task(s1, t2)
  store.save_task(s1, t3)
  store.save_task(s1, t4)
  store.save_task(s1, t5)

  store.persist(s1)
  store.stop(s1)

  // Reload and verify
  let assert Ok(s2) = store.start(StoreConfig(state_dir: test_dir))
  store.load(s2)
  process.sleep(100)

  let all_tasks = store.get_all_tasks(s2)
  all_tasks |> list.length |> should.equal(5)

  store.stop(s2)

  let _ = simplifile.delete(test_dir)
  Nil
}
