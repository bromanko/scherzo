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
