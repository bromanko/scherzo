import scherzo/orchestrator/poll_jitter

pub fn jitter_delay_is_deterministic_for_seed_and_generation_test() {
  assert poll_jitter.delay_ms(1000, "seed-a", 2) == 1043
  assert poll_jitter.delay_ms(1000, "seed-a", 3) == 995
  assert poll_jitter.delay_ms(1000, "seed-b", 2) == 971
}

pub fn jitter_delay_stays_positive_and_within_documented_bound_test() {
  assert_in_documented_bound(1, "seed-a", 2)
  assert_in_documented_bound(10, "seed-a", 2)
  assert_in_documented_bound(1000, "seed-a", 2)
  assert_in_documented_bound(60_000, "seed-a", 2)
}

pub fn jitter_bound_is_ten_percent_with_one_ms_floor_test() {
  assert poll_jitter.jitter_bound_ms(1) == 1
  assert poll_jitter.jitter_bound_ms(9) == 1
  assert poll_jitter.jitter_bound_ms(10) == 1
  assert poll_jitter.jitter_bound_ms(1000) == 100
}

fn assert_in_documented_bound(
  interval_ms: Int,
  seed: String,
  generation: Int,
) -> Nil {
  let bound_ms = poll_jitter.jitter_bound_ms(interval_ms)
  let delay_ms = poll_jitter.delay_ms(interval_ms, seed, generation)
  assert delay_ms > 0
  assert delay_ms >= interval_ms - bound_ms
  assert delay_ms <= interval_ms + bound_ms
}
