import scherzo/orchestrator/session_metrics
import scherzo/session/tokens as session_tokens

fn token_totals(total: Int) -> session_tokens.TokenTotals {
  session_tokens.TokenTotals(
    input: total,
    output: 0,
    cache_read: 0,
    cache_write: 0,
    total: total,
  )
}

pub fn update_tokens_ignores_unknown_step_sessions_test() {
  let entries =
    session_metrics.update_tokens(
      session_metrics.new(),
      "unknown-step",
      token_totals(7),
    )

  assert session_metrics.total(entries) == session_tokens.zero_token_totals()
}

pub fn registered_step_tokens_are_totaled_and_removed_by_run_test() {
  let entries =
    session_metrics.new()
    |> session_metrics.register_step("step-1", "run-1", "parent-1")
    |> session_metrics.update_tokens("step-1", token_totals(11))

  assert session_metrics.total(entries).total == 11
  assert session_metrics.total_for_run(entries, "run-1").total == 11
  assert session_metrics.total_for_run(entries, "other-run").total == 0
  assert session_metrics.total(session_metrics.remove_run(entries, "run-1"))
    == session_tokens.zero_token_totals()
}
