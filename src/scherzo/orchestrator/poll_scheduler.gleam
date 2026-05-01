import gleam/option.{type Option, None, Some}

pub opaque type State(timer) {
  State(generation: Int, in_flight: Option(Int), timer: Option(timer))
}

pub fn start(schedule_initial: fn(Int) -> timer) -> State(timer) {
  let generation = 1
  State(
    generation: generation,
    in_flight: None,
    timer: Some(schedule_initial(generation)),
  )
}

pub fn accept_tick(
  state: State(timer),
  generation: Int,
) -> Result(State(timer), Nil) {
  case generation != state.generation || state.in_flight != None {
    True -> Error(Nil)
    False -> Ok(State(..state, in_flight: Some(generation)))
  }
}

pub fn result_is_stale(state: State(timer), generation: Int) -> Bool {
  generation != state.generation || state.in_flight != Some(generation)
}

pub fn schedule_next(
  state: State(timer),
  schedule: fn(Int) -> timer,
  cancel: fn(timer) -> Nil,
) -> State(timer) {
  case state.timer {
    Some(timer) -> cancel(timer)
    None -> Nil
  }
  let generation = state.generation + 1
  State(
    generation: generation,
    in_flight: None,
    timer: Some(schedule(generation)),
  )
}

pub fn cancel_all(state: State(timer), cancel: fn(timer) -> Nil) -> State(timer) {
  case state.timer {
    Some(timer) -> cancel(timer)
    None -> Nil
  }
  State(..state, in_flight: None, timer: None)
}

pub fn generation(state: State(timer)) -> Int {
  state.generation
}

pub fn in_flight(state: State(timer)) -> Option(Int) {
  state.in_flight
}

pub fn timer(state: State(timer)) -> Option(timer) {
  state.timer
}
