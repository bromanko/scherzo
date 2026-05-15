import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/orchestrator/poll_jitter

pub opaque type State(timer) {
  State(generation: Int, in_flight: Option(Int), timer: Option(timer))
}

pub opaque type JitteredDelay {
  JitteredDelay(
    generation: Int,
    interval_ms: Int,
    jitter_bound_ms: Int,
    delay_ms: Int,
  )
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

pub fn mark_in_flight(state: State(timer), generation: Int) -> State(timer) {
  State(..state, in_flight: Some(generation))
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

pub fn schedule_next_jittered(
  state: State(timer),
  interval_ms: Int,
  seed: String,
  schedule: fn(Int, Int) -> timer,
  cancel: fn(timer) -> Nil,
) -> #(State(timer), JitteredDelay) {
  let generation = state.generation + 1
  let delay =
    JitteredDelay(
      generation: generation,
      interval_ms: interval_ms,
      jitter_bound_ms: poll_jitter.jitter_bound_ms(interval_ms),
      delay_ms: poll_jitter.delay_ms(interval_ms, seed, generation),
    )
  let state =
    schedule_next(
      state,
      fn(generation) { schedule(generation, delay.delay_ms) },
      cancel,
    )
  #(state, delay)
}

pub fn schedule_next_jittered_message(
  state: State(timer),
  interval_ms: Int,
  seed: String,
  target: target,
  make_message: fn(Int) -> message,
  send_after: fn(target, Int, message) -> timer,
  cancel: fn(timer) -> Nil,
) -> #(State(timer), JitteredDelay) {
  schedule_next_jittered(
    state,
    interval_ms,
    seed,
    fn(generation, delay_ms) {
      send_after(target, delay_ms, make_message(generation))
    },
    cancel,
  )
}

pub fn jitter_log_fields(delay: JitteredDelay) -> List(#(String, String)) {
  [
    #("generation", int.to_string(delay.generation)),
    #("polling_interval_ms", int.to_string(delay.interval_ms)),
    #("polling_jitter_bound_ms", int.to_string(delay.jitter_bound_ms)),
    #("next_poll_delay_ms", int.to_string(delay.delay_ms)),
  ]
}

pub fn cancel_all(
  state: State(timer),
  cancel: fn(timer) -> Nil,
) -> State(timer) {
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
