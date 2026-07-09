import gleam/dict.{type Dict}
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/task

pub opaque type State(timer) {
  State(timers: Dict(identity.TaskIdentity, timer))
}

pub fn new() -> State(timer) {
  State(timers: dict.new())
}

pub fn schedule_timer(
  state: State(timer),
  issue_id: String,
  timer: timer,
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  schedule_task_timer(
    state,
    orchestrator_state.linear_issue_id_ref(issue_id),
    timer,
    cancel,
  )
}

pub fn schedule_task_timer(
  state: State(timer),
  ref: task.TaskRef,
  timer: timer,
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  let identity = orchestrator_state.task_ref_identity(ref)
  case dict.get(state.timers, identity) {
    Ok(existing_timer) -> cancel(existing_timer)
    Error(Nil) -> Nil
  }
  State(timers: dict.insert(state.timers, identity, timer))
}

pub fn cancel_timer(
  state: State(timer),
  issue_id: String,
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  cancel_task_timer(
    state,
    orchestrator_state.linear_issue_id_ref(issue_id),
    cancel,
  )
}

pub fn cancel_task_timer(
  state: State(timer),
  ref: task.TaskRef,
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  let identity = orchestrator_state.task_ref_identity(ref)
  case dict.get(state.timers, identity) {
    Ok(timer) -> cancel(timer)
    Error(Nil) -> Nil
  }
  State(timers: dict.delete(state.timers, identity))
}

pub fn remove_timer(state: State(timer), issue_id: String) -> State(timer) {
  remove_task_timer(state, orchestrator_state.linear_issue_id_ref(issue_id))
}

pub fn remove_task_timer(
  state: State(timer),
  ref: task.TaskRef,
) -> State(timer) {
  State(timers: dict.delete(
    state.timers,
    orchestrator_state.task_ref_identity(ref),
  ))
}

pub fn cancel_all(
  state: State(timer),
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  dict.each(state.timers, fn(_, timer) { cancel(timer) })
  new()
}

pub fn timer_count(state: State(timer)) -> Int {
  dict.size(state.timers)
}

pub fn timer_for_issue(
  state: State(timer),
  issue_id: String,
) -> Result(timer, Nil) {
  timer_for_task_ref(state, orchestrator_state.linear_issue_id_ref(issue_id))
}

pub fn timer_for_task_ref(
  state: State(timer),
  ref: task.TaskRef,
) -> Result(timer, Nil) {
  dict.get(state.timers, orchestrator_state.task_ref_identity(ref))
}
