import gleam/option.{type Option, None, Some}

const base_backoff_ms = 10_000

const default_defer_ms = 1000

const default_max_backoff_ms_value = 300_000

pub type TimerTick {
  TimerMissing
  TimerGenerationMismatch(stored_generation: Int, tick_generation: Int)
  TimerAccepted(generation: Int)
}

/// Attempt indexes are one-based throughout Scherzo retry surfaces.
pub fn first_attempt_index() -> Int {
  1
}

pub fn next_attempt_index(current_attempt_index: Int) -> Int {
  current_attempt_index + 1
}

pub fn retry_budget_remaining(
  completed_retry_count: Int,
  max_retry_count: Int,
) -> Bool {
  completed_retry_count < max_retry_count
}

pub fn completed_attempts_exhausted(
  completed_attempt_count: Int,
  max_attempt_count: Int,
) -> Bool {
  completed_attempt_count >= max_attempt_count
}

pub fn next_attempt_exhausted(
  next_attempt_index: Int,
  max_attempt_count: Int,
) -> Bool {
  next_attempt_index > max_attempt_count
}

pub fn initial_generation() -> Int {
  1
}

pub fn next_generation(current_generation: Option(Int)) -> Int {
  case current_generation {
    Some(generation) -> generation + 1
    None -> initial_generation()
  }
}

pub fn next_generation_after_reserved(
  next_reserved_generation: Int,
  used_generation: Int,
) -> Int {
  case next_reserved_generation > used_generation {
    True -> next_reserved_generation
    False -> used_generation + 1
  }
}

pub fn classify_timer_tick(
  stored_generation: Option(Int),
  tick_generation: Int,
) -> TimerTick {
  case stored_generation {
    None -> TimerMissing
    Some(generation) ->
      case generation == tick_generation {
        True -> TimerAccepted(generation)
        False -> TimerGenerationMismatch(generation, tick_generation)
      }
  }
}

pub fn backoff_delay(attempt_index: Int, max_backoff_ms: Int) -> Int {
  backoff_delay_loop(base_backoff_ms, attempt_index - 1, max_backoff_ms)
}

pub fn default_max_backoff_ms() -> Int {
  default_max_backoff_ms_value
}

pub fn defer_delay_ms() -> Int {
  default_defer_ms
}

fn backoff_delay_loop(
  delay_ms: Int,
  remaining_doubles: Int,
  max_ms: Int,
) -> Int {
  case delay_ms >= max_ms {
    True -> max_ms
    False ->
      case remaining_doubles <= 0 {
        True -> delay_ms
        False -> backoff_delay_loop(delay_ms * 2, remaining_doubles - 1, max_ms)
      }
  }
}
