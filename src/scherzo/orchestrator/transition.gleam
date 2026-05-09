import gleam/dict
import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_types
import scherzo/state/ledger
import scherzo/state/record

pub fn handle(
  message: transition_types.Message,
  state: transition_types.State,
) -> transition_types.Outcome {
  case message {
    transition_types.SnapshotRequested ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ReplySnapshot(state.runtime),
      ])
    transition_types.ClaimLedgerAppendRequested(
      correlation_id,
      issue_id,
      run_id,
      session_id,
      bodies,
      failure_event,
    ) ->
      handle_claim_ledger_append_requested(
        state,
        correlation_id,
        issue_id,
        run_id,
        session_id,
        bodies,
        failure_event,
      )
    transition_types.LedgerAppendCompleted(
      correlation_id,
      continuation,
      result,
      _,
    ) ->
      handle_ledger_append_completed(
        state,
        correlation_id,
        continuation,
        result,
      )
  }
}

pub fn snapshot(
  state: transition_types.State,
) -> orchestrator_state.RuntimeState {
  state.runtime
}

fn handle_claim_ledger_append_requested(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  run_id: String,
  session_id: String,
  bodies: List(record.RecordBody),
  failure_event: String,
) -> transition_types.Outcome {
  case dict.get(state.pending_claims, issue_id) {
    Error(Nil) ->
      stale_claim_continuation(state, correlation_id, issue_id, run_id)
    Ok(pending) ->
      case pending.run_id == run_id && pending.session_id == session_id {
        False ->
          stale_claim_continuation(state, correlation_id, issue_id, run_id)
        True ->
          transition_types.Outcome(state: state, effects: [
            effects_types.AppendLedger(effects_types.LedgerAppend(
              correlation_id: correlation_id,
              bodies: bodies,
              failure_event: failure_event,
              policy: effects_types.ContinueWith(
                effects_types.SpawnClaimedWorker(issue_id, run_id, session_id),
              ),
            )),
          ])
      }
  }
}

fn handle_ledger_append_completed(
  state: transition_types.State,
  correlation_id: String,
  continuation: effects_types.LedgerContinuation,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case continuation {
    effects_types.SpawnClaimedWorker(issue_id, run_id, session_id) ->
      handle_spawn_claimed_worker(
        state,
        correlation_id,
        issue_id,
        run_id,
        session_id,
        result,
      )
    effects_types.NoLedgerContinuation ->
      transition_types.Outcome(state: state, effects: [])
  }
}

fn handle_spawn_claimed_worker(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  run_id: String,
  session_id: String,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case dict.get(state.pending_claims, issue_id) {
    Error(Nil) ->
      stale_claim_continuation(state, correlation_id, issue_id, run_id)
    Ok(pending) ->
      case pending.run_id == run_id && pending.session_id == session_id {
        False ->
          stale_claim_continuation(state, correlation_id, issue_id, run_id)
        True ->
          case result {
            Error(err) ->
              transition_types.Outcome(
                state: clear_pending_claim(state, issue_id),
                effects: [
                  effects_types.Log("warn", "ledger_append_failed", [
                    #("issue_id", issue_id),
                    #("run_id", run_id),
                    #("correlation_id", correlation_id),
                    #("error", ledger_error_code(err)),
                  ]),
                ],
              )
            Ok(Nil) -> start_claimed_worker(state, pending)
          }
      }
  }
}

fn start_claimed_worker(
  state: transition_types.State,
  pending: transition_types.PendingClaim,
) -> transition_types.Outcome {
  let worker_entry =
    transition_types.WorkerEntry(
      issue_id: pending.issue_id,
      run_id: pending.run_id,
      session_id: pending.session_id,
      issue: pending.issue,
      workspace_path: pending.workspace_path,
      workflow_id: pending.workflow_id,
      command_route_id: pending.command_route_id,
      status: transition_types.WorkerStarting,
      recovery: pending.recovery,
    )
  let workers = state.workers
  let workers =
    transition_types.WorkerDirectory(
      by_issue: dict.insert(workers.by_issue, pending.issue_id, worker_entry),
      by_session: dict.insert(
        workers.by_session,
        pending.session_id,
        pending.issue_id,
      ),
      route_to_session: dict.insert(
        workers.route_to_session,
        pending.command_route_id,
        pending.session_id,
      ),
    )
  let state =
    transition_types.State(
      pending_claims: dict.delete(state.pending_claims, pending.issue_id),
      runtime: core.apply_worker_start(
        state.runtime,
        pending.issue,
        pending.workspace_path,
      ),
      workers: workers,
    )
  transition_types.Outcome(state: state, effects: [
    effects_types.StartWorker(effects_types.WorkerStart(
      issue_id: pending.issue_id,
      run_id: pending.run_id,
      session_id: pending.session_id,
      command_route_id: pending.command_route_id,
      issue: pending.issue,
      workspace_path: pending.workspace_path,
      workflow_id: pending.workflow_id,
      route_label: pending.route_label,
      recovery: pending.recovery,
    )),
  ])
}

fn stale_claim_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  run_id: String,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", "claim_ledger_continuation_stale", [
      #("issue_id", issue_id),
      #("run_id", run_id),
      #("correlation_id", correlation_id),
    ]),
  ])
}

fn clear_pending_claim(
  state: transition_types.State,
  issue_id: String,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_claims: dict.delete(state.pending_claims, issue_id),
  )
}

fn ledger_error_code(err: ledger.LedgerError) -> String {
  case err {
    ledger.Io(_) -> "io"
    ledger.LedgerFfiFailed(_) -> "ledger_ffi_failed"
    ledger.UnsupportedVersion(_) -> "unsupported_version"
    ledger.CorruptRecord(_, _) -> "corrupt_record"
  }
}
