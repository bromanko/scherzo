pub type TokenTotals {
  TokenTotals(
    input: Int,
    output: Int,
    cache_read: Int,
    cache_write: Int,
    total: Int,
  )
}

pub fn zero_token_totals() -> TokenTotals {
  TokenTotals(input: 0, output: 0, cache_read: 0, cache_write: 0, total: 0)
}

pub fn add(left: TokenTotals, right: TokenTotals) -> TokenTotals {
  TokenTotals(
    input: left.input + right.input,
    output: left.output + right.output,
    cache_read: left.cache_read + right.cache_read,
    cache_write: left.cache_write + right.cache_write,
    total: left.total + right.total,
  )
}

pub fn nonzero(tokens: TokenTotals) -> Bool {
  tokens.input > 0
  || tokens.output > 0
  || tokens.cache_read > 0
  || tokens.cache_write > 0
  || tokens.total > 0
}

pub fn positive_delta(
  current: TokenTotals,
  previous: TokenTotals,
) -> TokenTotals {
  TokenTotals(
    input: positive_difference(current.input, previous.input),
    output: positive_difference(current.output, previous.output),
    cache_read: positive_difference(current.cache_read, previous.cache_read),
    cache_write: positive_difference(current.cache_write, previous.cache_write),
    total: positive_difference(current.total, previous.total),
  )
}

fn positive_difference(current: Int, previous: Int) -> Int {
  case current > previous {
    True -> current - previous
    False -> 0
  }
}
