import gleam/dict.{type Dict}

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
  case dict.get(state.timers, issue_id) {
    Ok(existing_timer) -> cancel(existing_timer)
    Error(_) -> Nil
  }
  State(..state, timers: dict.insert(state.timers, issue_id, timer))
}

pub fn cancel_timer(
  state: State(timer),
  issue_id: String,
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  case dict.get(state.timers, issue_id) {
    Ok(timer) -> cancel(timer)
    Error(_) -> Nil
  }
  State(..state, timers: dict.delete(state.timers, issue_id))
}

pub fn remove_timer(state: State(timer), issue_id: String) -> State(timer) {
  State(..state, timers: dict.delete(state.timers, issue_id))
}

pub fn begin_refresh(
  state: State(timer),
  issue_id: String,
  generation: Int,
) -> Result(State(timer), Nil) {
  case dict.get(state.refreshes_in_flight, issue_id) {
    Ok(_) -> Error(Nil)
    Error(_) ->
      Ok(
        State(
          ..state,
          refreshes_in_flight: dict.insert(
            state.refreshes_in_flight,
            issue_id,
            generation,
          ),
        ),
      )
  }
}

pub fn finish_refresh(state: State(timer), issue_id: String) -> State(timer) {
  State(
    ..state,
    refreshes_in_flight: dict.delete(state.refreshes_in_flight, issue_id),
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
  dict.get(state.timers, issue_id)
}

pub fn refresh_generation(
  state: State(timer),
  issue_id: String,
) -> Result(Int, Nil) {
  dict.get(state.refreshes_in_flight, issue_id)
}
