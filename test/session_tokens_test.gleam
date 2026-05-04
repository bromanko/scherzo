import scherzo/session/tokens as session_tokens

pub fn default_token_totals_are_zero_test() {
  let totals = session_tokens.zero_token_totals()
  assert totals.input == 0
  assert totals.output == 0
  assert totals.cache_read == 0
  assert totals.cache_write == 0
  assert totals.total == 0
}
