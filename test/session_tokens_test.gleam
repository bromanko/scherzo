import scherzo/session/tokens as session_tokens

pub fn default_token_totals_are_zero_test() {
  let totals = session_tokens.zero_token_totals()
  assert totals.input == 0
  assert totals.output == 0
  assert totals.cache_read == 0
  assert totals.cache_write == 0
  assert totals.total == 0
}

pub fn token_totals_adds_each_bucket_test() {
  let total =
    session_tokens.add(
      session_tokens.TokenTotals(
        input: 1,
        output: 2,
        cache_read: 3,
        cache_write: 4,
        total: 10,
      ),
      session_tokens.TokenTotals(
        input: 5,
        output: 6,
        cache_read: 7,
        cache_write: 8,
        total: 30,
      ),
    )

  assert total
    == session_tokens.TokenTotals(
      input: 6,
      output: 8,
      cache_read: 10,
      cache_write: 12,
      total: 40,
    )
}

pub fn token_totals_positive_delta_clamps_regressions_test() {
  let delta =
    session_tokens.positive_delta(
      session_tokens.TokenTotals(
        input: 10,
        output: 4,
        cache_read: 0,
        cache_write: 2,
        total: 16,
      ),
      session_tokens.TokenTotals(
        input: 7,
        output: 6,
        cache_read: 1,
        cache_write: 2,
        total: 18,
      ),
    )

  assert delta
    == session_tokens.TokenTotals(
      input: 3,
      output: 0,
      cache_read: 0,
      cache_write: 0,
      total: 0,
    )
}
