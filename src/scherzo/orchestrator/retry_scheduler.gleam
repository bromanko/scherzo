import gleam/dict.{type Dict}
import scherzo/orchestrator/state as orchestrator_state
import scherzo/task

pub opaque type State(timer) {
  State(timers: Dict(String, timer), refreshes_in_flight: Dict(String, Int))
}

pub fn new() -> State(timer) {
  State(timers: dict.new(), refreshes_in_flight: dict.new())
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
  State(..state, timers: dict.insert(state.timers, identity, timer))
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
  State(..state, timers: dict.delete(state.timers, identity))
}

pub fn remove_timer(state: State(timer), issue_id: String) -> State(timer) {
  remove_task_timer(state, orchestrator_state.linear_issue_id_ref(issue_id))
}

pub fn remove_task_timer(
  state: State(timer),
  ref: task.TaskRef,
) -> State(timer) {
  State(
    ..state,
    timers: dict.delete(state.timers, orchestrator_state.task_ref_identity(ref)),
  )
}

pub fn begin_refresh(
  state: State(timer),
  issue_id: String,
  generation: Int,
) -> Result(State(timer), Nil) {
  begin_task_refresh(
    state,
    orchestrator_state.linear_issue_id_ref(issue_id),
    generation,
  )
}

pub fn begin_task_refresh(
  state: State(timer),
  ref: task.TaskRef,
  generation: Int,
) -> Result(State(timer), Nil) {
  let identity = orchestrator_state.task_ref_identity(ref)
  case dict.get(state.refreshes_in_flight, identity) {
    Ok(_) -> Error(Nil)
    Error(Nil) ->
      Ok(
        State(
          ..state,
          refreshes_in_flight: dict.insert(
            state.refreshes_in_flight,
            identity,
            generation,
          ),
        ),
      )
  }
}

pub fn finish_refresh(state: State(timer), issue_id: String) -> State(timer) {
  finish_task_refresh(state, orchestrator_state.linear_issue_id_ref(issue_id))
}

pub fn finish_task_refresh(
  state: State(timer),
  ref: task.TaskRef,
) -> State(timer) {
  State(
    ..state,
    refreshes_in_flight: dict.delete(
      state.refreshes_in_flight,
      orchestrator_state.task_ref_identity(ref),
    ),
  )
}

pub fn cancel_all(
  state: State(timer),
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  dict.each(state.timers, fn(_, timer) { cancel(timer) })
  new()
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

pub fn refresh_generation(
  state: State(timer),
  issue_id: String,
) -> Result(Int, Nil) {
  refresh_generation_for_task_ref(
    state,
    orchestrator_state.linear_issue_id_ref(issue_id),
  )
}

pub fn refresh_generation_for_task_ref(
  state: State(timer),
  ref: task.TaskRef,
) -> Result(Int, Nil) {
  dict.get(state.refreshes_in_flight, orchestrator_state.task_ref_identity(ref))
}
