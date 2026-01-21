import gleam/option.{None, Some}
import gleeunit/should
import scherzo/core/task.{type Task, Critical, High, Low, Normal, Pending, Task}
import scherzo/task/queue

fn make_task(id: String, priority: task.Priority) -> Task {
  Task(
    id: id,
    title: "Task " <> id,
    description: "Test task",
    status: Pending,
    priority: priority,
    dependencies: [],
    created_at: 1000,
    updated_at: 1000,
    source_id: None,
    jj_change_id: None,
    parent: None,
  )
}

pub fn start_creates_queue_test() {
  let result = queue.start()

  result |> should.be_ok
}

pub fn enqueue_and_dequeue_returns_task_test() {
  let assert Ok(q) = queue.start()
  let task = make_task("t-1", Normal)

  queue.enqueue(q, task)
  let result = queue.dequeue(q)

  result |> should.equal(Some(task))

  queue.stop(q)
}

pub fn dequeue_empty_queue_returns_none_test() {
  let assert Ok(q) = queue.start()

  let result = queue.dequeue(q)

  result |> should.equal(None)

  queue.stop(q)
}

pub fn dequeue_removes_task_from_queue_test() {
  let assert Ok(q) = queue.start()
  let task = make_task("t-1", Normal)

  queue.enqueue(q, task)
  let _ = queue.dequeue(q)
  let result = queue.dequeue(q)

  result |> should.equal(None)

  queue.stop(q)
}

pub fn priority_ordering_critical_first_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("low", Low))
  queue.enqueue(q, make_task("critical", Critical))
  queue.enqueue(q, make_task("normal", Normal))

  let assert Some(first) = queue.dequeue(q)
  first.id |> should.equal("critical")

  queue.stop(q)
}

pub fn priority_ordering_high_before_normal_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("normal", Normal))
  queue.enqueue(q, make_task("high", High))
  queue.enqueue(q, make_task("low", Low))

  let assert Some(first) = queue.dequeue(q)
  first.id |> should.equal("high")

  let assert Some(second) = queue.dequeue(q)
  second.id |> should.equal("normal")

  let assert Some(third) = queue.dequeue(q)
  third.id |> should.equal("low")

  queue.stop(q)
}

pub fn full_priority_order_test() {
  let assert Ok(q) = queue.start()

  // Enqueue in reverse priority order
  queue.enqueue(q, make_task("low", Low))
  queue.enqueue(q, make_task("normal", Normal))
  queue.enqueue(q, make_task("high", High))
  queue.enqueue(q, make_task("critical", Critical))

  let assert Some(t1) = queue.dequeue(q)
  let assert Some(t2) = queue.dequeue(q)
  let assert Some(t3) = queue.dequeue(q)
  let assert Some(t4) = queue.dequeue(q)

  t1.id |> should.equal("critical")
  t2.id |> should.equal("high")
  t3.id |> should.equal("normal")
  t4.id |> should.equal("low")

  queue.stop(q)
}

pub fn get_stats_empty_queue_test() {
  let assert Ok(q) = queue.start()

  let stats = queue.get_stats(q)

  stats.pending |> should.equal(0)
  stats.in_progress |> should.equal(0)

  queue.stop(q)
}

pub fn get_stats_with_pending_tasks_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("t-1", Normal))
  queue.enqueue(q, make_task("t-2", Normal))

  let stats = queue.get_stats(q)

  stats.pending |> should.equal(2)
  stats.in_progress |> should.equal(0)

  queue.stop(q)
}

pub fn get_stats_with_in_progress_tasks_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("t-1", Normal))
  let _ = queue.dequeue(q)

  let stats = queue.get_stats(q)

  stats.pending |> should.equal(0)
  stats.in_progress |> should.equal(1)

  queue.stop(q)
}

pub fn complete_removes_from_in_progress_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("t-1", Normal))
  let _ = queue.dequeue(q)
  queue.complete(q, "t-1")

  // Give actor time to process
  let stats = queue.get_stats(q)

  stats.in_progress |> should.equal(0)

  queue.stop(q)
}

pub fn fail_removes_from_in_progress_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("t-1", Normal))
  let _ = queue.dequeue(q)
  queue.fail(q, "t-1", "Test failure")

  let stats = queue.get_stats(q)

  stats.in_progress |> should.equal(0)

  queue.stop(q)
}

pub fn fifo_order_for_same_priority_test() {
  let assert Ok(q) = queue.start()

  queue.enqueue(q, make_task("first", Normal))
  queue.enqueue(q, make_task("second", Normal))
  queue.enqueue(q, make_task("third", Normal))

  let assert Some(t1) = queue.dequeue(q)
  let assert Some(t2) = queue.dequeue(q)
  let assert Some(t3) = queue.dequeue(q)

  t1.id |> should.equal("first")
  t2.id |> should.equal("second")
  t3.id |> should.equal("third")

  queue.stop(q)
}
