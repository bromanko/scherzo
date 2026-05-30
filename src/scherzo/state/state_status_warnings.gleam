import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import scherzo/state/ledger
import scherzo/state/projection

pub fn orphan_step_attempt_warnings(paths: ledger.LedgerPath) -> List(String) {
  case ledger.load_projection(paths) {
    Ok(projected) -> format_orphan_step_attempt_warnings(projected)
    Error(error) -> [
      "orphan_step_attempts_without_workflow_run: diagnostics unavailable: "
      <> ledger.ledger_error_to_string(error),
    ]
  }
}

fn format_orphan_step_attempt_warnings(
  projected: projection.Projection,
) -> List(String) {
  projected.step_attempts
  |> dict.values
  |> list.fold(dict.new(), fn(counts, status) {
    let run_id = step_attempt_run_id(status)
    case dict.has_key(projected.workflow_runs, run_id) {
      True -> counts
      False ->
        dict.insert(counts, run_id, case dict.get(counts, run_id) {
          Ok(count) -> count + 1
          Error(Nil) -> 1
        })
    }
  })
  |> dict.to_list
  |> list.sort(fn(left, right) { string.compare(left.0, right.0) })
  |> list.map(fn(entry) {
    let #(run_id, attempt_count) = entry
    "orphan_step_attempts_without_workflow_run: run_id="
    <> run_id
    <> " attempt_count="
    <> int.to_string(attempt_count)
    <> " read-only inspection is still allowed; repair is deferred"
  })
}

fn step_attempt_run_id(status: projection.StepAttemptStatus) -> String {
  case status {
    projection.StepAttemptPending(run_id, _, _, _, _, _, _, _, _, _) -> run_id
    projection.StepAttemptRunning(
      run_id,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
    ) -> run_id
    projection.StepAttemptFinishedStatus(
      run_id,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
    ) -> run_id
    projection.StepAttemptInterruptedStatus(
      run_id,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
      _,
    ) -> run_id
    projection.StepAttemptSupersededStatus(run_id, _, _, _, _, _, _) -> run_id
  }
}
