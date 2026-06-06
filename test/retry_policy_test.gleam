import gleam/option.{None, Some}
import scherzo/retry_policy

pub fn backoff_delay_is_one_based_and_capped_test() {
  assert retry_policy.backoff_delay(1, 60_000) == 10_000
  assert retry_policy.backoff_delay(2, 60_000) == 20_000
  assert retry_policy.backoff_delay(3, 60_000) == 40_000
  assert retry_policy.backoff_delay(4, 60_000) == 60_000
  assert retry_policy.backoff_delay(100, 60_000) == 60_000
}

pub fn attempt_exhaustion_helpers_keep_inclusive_and_next_attempt_semantics_test() {
  assert retry_policy.first_attempt_index() == 1
  assert retry_policy.next_attempt_index(1) == 2

  assert !retry_policy.completed_attempts_exhausted(2, 3)
  assert retry_policy.completed_attempts_exhausted(3, 3)

  assert !retry_policy.next_attempt_exhausted(3, 3)
  assert retry_policy.next_attempt_exhausted(4, 3)
}

pub fn retry_budget_remaining_uses_completed_retry_count_test() {
  assert retry_policy.retry_budget_remaining(0, 1)
  assert !retry_policy.retry_budget_remaining(1, 1)
  assert !retry_policy.retry_budget_remaining(0, 0)
}

pub fn generations_start_at_one_and_advance_after_reserved_test() {
  assert retry_policy.initial_generation() == 1
  assert retry_policy.next_generation(None) == 1
  assert retry_policy.next_generation(Some(4)) == 5
  assert retry_policy.next_generation_after_reserved(7, 4) == 7
  assert retry_policy.next_generation_after_reserved(4, 7) == 8
}

pub fn timer_tick_classification_distinguishes_missing_stale_and_accepted_test() {
  assert retry_policy.classify_timer_tick(None, 1) == retry_policy.TimerMissing
  assert retry_policy.classify_timer_tick(Some(2), 1)
    == retry_policy.TimerGenerationMismatch(2, 1)
  assert retry_policy.classify_timer_tick(Some(2), 2)
    == retry_policy.TimerAccepted(2)
}

pub fn shared_defer_delay_matches_retry_tick_deferral_test() {
  assert retry_policy.defer_delay_ms() == 1000
}
