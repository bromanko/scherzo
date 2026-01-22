import gleam/option.{None}
import gleeunit/should
import scherzo/core/task.{type Task, Normal, Pending, Ready, RegularTask, Task}
import scherzo/task/source.{type TaskSource, TaskSource}

fn make_test_task(id: String) -> Task {
  Task(
    id: id,
    title: "Task " <> id,
    description: "Test task",
    status: Pending,
    priority: Normal,
    task_type: RegularTask,
    dependencies: [],
    created_at: 1000,
    updated_at: 1000,
    source_id: None,
    jj_change_id: None,
    parent: None,
  )
}

fn make_mock_source(tasks: List(Task)) -> TaskSource {
  TaskSource(
    name: "mock",
    fetch_tasks: fn() { Ok(tasks) },
    update_status: fn(_id, _status) { Ok(Nil) },
    get_ready_tasks: fn() { Ok(tasks) },
  )
}

pub fn name_returns_source_name_test() {
  let src = make_mock_source([])

  source.name(src) |> should.equal("mock")
}

pub fn fetch_tasks_delegates_to_source_test() {
  let tasks = [make_test_task("t-1"), make_test_task("t-2")]
  let src = make_mock_source(tasks)

  let result = source.fetch_tasks(src)

  result |> should.be_ok
  let assert Ok(fetched) = result
  fetched |> should.equal(tasks)
}

pub fn fetch_tasks_returns_error_from_source_test() {
  let src =
    TaskSource(
      name: "failing",
      fetch_tasks: fn() { Error("Database error") },
      update_status: fn(_id, _status) { Ok(Nil) },
      get_ready_tasks: fn() { Ok([]) },
    )

  let result = source.fetch_tasks(src)

  result |> should.be_error
  let assert Error(msg) = result
  msg |> should.equal("Database error")
}

pub fn update_status_delegates_to_source_test() {
  let src = make_mock_source([])

  let result = source.update_status(src, "t-1", Ready)

  result |> should.be_ok
}

pub fn update_status_returns_error_from_source_test() {
  let src =
    TaskSource(
      name: "failing",
      fetch_tasks: fn() { Ok([]) },
      update_status: fn(_id, _status) { Error("Permission denied") },
      get_ready_tasks: fn() { Ok([]) },
    )

  let result = source.update_status(src, "t-1", Ready)

  result |> should.be_error
  let assert Error(msg) = result
  msg |> should.equal("Permission denied")
}

pub fn get_ready_tasks_delegates_to_source_test() {
  let tasks = [make_test_task("ready-1")]
  let src = make_mock_source(tasks)

  let result = source.get_ready_tasks(src)

  result |> should.be_ok
  let assert Ok(ready) = result
  ready |> should.equal(tasks)
}

pub fn get_ready_tasks_returns_error_from_source_test() {
  let src =
    TaskSource(
      name: "failing",
      fetch_tasks: fn() { Ok([]) },
      update_status: fn(_id, _status) { Ok(Nil) },
      get_ready_tasks: fn() { Error("Network error") },
    )

  let result = source.get_ready_tasks(src)

  result |> should.be_error
  let assert Error(msg) = result
  msg |> should.equal("Network error")
}

pub fn source_with_custom_name_test() {
  let src =
    TaskSource(
      name: "custom-source",
      fetch_tasks: fn() { Ok([]) },
      update_status: fn(_id, _status) { Ok(Nil) },
      get_ready_tasks: fn() { Ok([]) },
    )

  source.name(src) |> should.equal("custom-source")
}
