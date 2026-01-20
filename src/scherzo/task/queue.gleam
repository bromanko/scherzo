/// TaskQueue actor - priority queue for task management
///
/// Manages a queue of tasks respecting priority and dependencies.
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import scherzo/core/task.{type Task, Critical, High, Low, Normal}
import scherzo/core/types.{type Id}
import scherzo/task/source.{type TaskSource}

/// Messages the queue can receive
pub type Message {
  /// Add a task to the queue
  Enqueue(task: Task)
  /// Get the next ready task (highest priority)
  Dequeue(reply_to: Subject(Option(Task)))
  /// Mark a task as complete and re-evaluate queue
  Complete(task_id: Id)
  /// Mark a task as failed
  Fail(task_id: Id, reason: String)
  /// Refresh queue from task source
  Refresh(source: TaskSource, reply_to: Subject(Result(Int, String)))
  /// Get queue stats
  GetStats(reply_to: Subject(QueueStats))
  /// Shutdown the queue
  Shutdown
}

/// Queue statistics
pub type QueueStats {
  QueueStats(pending: Int, in_progress: Int)
}

/// Internal state of the queue
pub type State {
  State(
    /// Tasks waiting to be processed, sorted by priority
    pending: List(Task),
    /// Tasks currently being worked on
    in_progress: Dict(Id, Task),
  )
}

/// Start a new task queue actor
pub fn start() -> Result(Subject(Message), actor.StartError) {
  let initial_state = State(pending: [], in_progress: dict.new())

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

/// Enqueue a task
pub fn enqueue(queue: Subject(Message), task: Task) -> Nil {
  actor.send(queue, Enqueue(task))
}

/// Dequeue the next task
pub fn dequeue(queue: Subject(Message)) -> Option(Task) {
  actor.call(queue, 5000, Dequeue)
}

/// Mark a task as complete
pub fn complete(queue: Subject(Message), task_id: Id) -> Nil {
  actor.send(queue, Complete(task_id))
}

/// Mark a task as failed
pub fn fail(queue: Subject(Message), task_id: Id, reason: String) -> Nil {
  actor.send(queue, Fail(task_id, reason))
}

/// Refresh the queue from a task source
pub fn refresh(
  queue: Subject(Message),
  source: TaskSource,
) -> Result(Int, String) {
  actor.call(queue, 30_000, Refresh(source, _))
}

/// Get queue statistics
pub fn get_stats(queue: Subject(Message)) -> QueueStats {
  actor.call(queue, 5000, GetStats)
}

/// Stop the queue
pub fn stop(queue: Subject(Message)) -> Nil {
  actor.send(queue, Shutdown)
}

/// Handle incoming messages
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Enqueue(task) -> {
      let new_pending = insert_by_priority(state.pending, task)
      actor.continue(State(..state, pending: new_pending))
    }

    Dequeue(reply_to) -> {
      case state.pending {
        [] -> {
          process.send(reply_to, None)
          actor.continue(state)
        }
        [task, ..rest] -> {
          process.send(reply_to, Some(task))
          let new_in_progress = dict.insert(state.in_progress, task.id, task)
          actor.continue(State(pending: rest, in_progress: new_in_progress))
        }
      }
    }

    Complete(task_id) -> {
      let new_in_progress = dict.delete(state.in_progress, task_id)
      actor.continue(State(..state, in_progress: new_in_progress))
    }

    Fail(task_id, _reason) -> {
      // For now, just remove from in_progress
      // Could add retry logic here later
      let new_in_progress = dict.delete(state.in_progress, task_id)
      actor.continue(State(..state, in_progress: new_in_progress))
    }

    Refresh(source, reply_to) -> {
      case source.get_ready_tasks() {
        Error(err) -> {
          process.send(reply_to, Error(err))
          actor.continue(state)
        }
        Ok(tasks) -> {
          // Filter out tasks already in progress
          let in_progress_ids = dict.keys(state.in_progress)
          let new_tasks =
            tasks
            |> list.filter(fn(t) { !list.contains(in_progress_ids, t.id) })

          // Merge with existing pending, avoiding duplicates
          let existing_ids = list.map(state.pending, fn(t) { t.id })
          let tasks_to_add =
            new_tasks
            |> list.filter(fn(t) { !list.contains(existing_ids, t.id) })

          let new_pending =
            list.fold(tasks_to_add, state.pending, insert_by_priority)

          process.send(reply_to, Ok(list.length(tasks_to_add)))
          actor.continue(State(..state, pending: new_pending))
        }
      }
    }

    GetStats(reply_to) -> {
      let stats =
        QueueStats(
          pending: list.length(state.pending),
          in_progress: dict.size(state.in_progress),
        )
      process.send(reply_to, stats)
      actor.continue(state)
    }

    Shutdown -> actor.stop()
  }
}

/// Insert a task into the pending list maintaining priority order
/// (lower priority number = higher priority = earlier in list)
fn insert_by_priority(tasks: List(Task), task: Task) -> List(Task) {
  let task_priority = priority_to_int(task.priority)

  case tasks {
    [] -> [task]
    [first, ..rest] -> {
      let first_priority = priority_to_int(first.priority)
      case task_priority < first_priority {
        True -> [task, first, ..rest]
        False -> [first, ..insert_by_priority(rest, task)]
      }
    }
  }
}

/// Convert priority to int for comparison (lower = higher priority)
fn priority_to_int(priority: task.Priority) -> Int {
  case priority {
    Critical -> 0
    High -> 1
    Normal -> 2
    Low -> 3
  }
}
