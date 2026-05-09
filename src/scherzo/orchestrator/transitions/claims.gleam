import gleam/dict
import gleam/int
import gleam/option
import scherzo/error
import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/helpers
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker/issue as tracker_issue
import scherzo/workspace

pub type Callbacks {
  Callbacks(
    dispatch_candidates: fn(
      List(tracker_issue.Issue),
      transition_types.State,
      transition_types.DispatchContext,
    ) -> transition_types.Outcome,
  )
}

pub fn begin_for_issue(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(dispatch_candidates: dispatch) = callbacks
  case helpers.select_workflow_route(context, issue) {
    Error(#(code, message)) -> {
      let outcome = dispatch(remaining_candidates, state, context)
      transition_types.Outcome(state: outcome.state, effects: [
        effects_types.Log("warn", "workflow_route_failed", [
          #("issue_id", issue.id),
          #("error", code),
          #("message", message),
        ]),
        ..outcome.effects
      ])
    }
    Ok(workflow_id) ->
      case workspace.workspace_path(context.workspace_root, issue.identifier) {
        Error(err) -> {
          let outcome = dispatch(remaining_candidates, state, context)
          transition_types.Outcome(state: outcome.state, effects: [
            effects_types.Log("warn", "dispatch_workspace_path_failed", [
              #("issue_id", issue.id),
              #("error", error.workspace_code(err)),
            ]),
            ..outcome.effects
          ])
        }
        Ok(#(_, workspace_path)) -> {
          let sequence = state.next_session_sequence
          let run_id = helpers.make_run_id(issue, context.now_ms, sequence)
          let session_id =
            helpers.make_session_id(issue.identifier, run_id, sequence)
          let recovery =
            dict.get(context.recovery_by_issue, issue.id)
            |> option.from_result
          let pending =
            transition_types.PendingClaim(
              issue_id: issue.id,
              run_id: run_id,
              session_id: session_id,
              workspace_path: workspace_path,
              workflow_id: workflow_id,
              command_route_id: "worker:"
                <> run_id
                <> ":"
                <> int.to_string(sequence),
              route_label: issue.identifier,
              issue: issue,
              recovery: recovery,
              remaining_candidates: remaining_candidates,
              dispatch_context: context,
            )
          transition_types.Outcome(
            state: transition_types.State(
              ..state,
              pending_claims: dict.insert(
                state.pending_claims,
                issue.id,
                pending,
              ),
              next_session_sequence: sequence + 1,
            ),
            effects: [
              effects_types.ReserveSessionSequence(sequence),
              effects_types.ClaimIssue(issue, workspace_path, run_id),
            ],
          )
        }
      }
  }
}

pub fn dispatch_validation_error_reason(
  err: transition_types.DispatchValidationError,
) -> String {
  helpers.dispatch_validation_error_reason(err)
}

pub fn blocker_summary(
  issue: tracker_issue.Issue,
  decision: core.BlockerDecision,
) -> String {
  helpers.blocker_summary(issue, decision)
}

pub fn bool_field(value: Bool) -> String {
  helpers.bool_field(value)
}

pub fn claim_correlation_id(issue_id: String, run_id: String) -> String {
  helpers.claim_correlation_id(issue_id, run_id)
}

pub fn handle_requested(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  run_id: String,
  session_id: String,
  bodies: List(record.RecordBody),
  failure_event: String,
) -> transition_types.Outcome {
  case dict.get(state.pending_claims, issue_id) {
    Error(Nil) -> stale_continuation(state, correlation_id, issue_id, run_id)
    Ok(pending) ->
      case pending.run_id == run_id && pending.session_id == session_id {
        False -> stale_continuation(state, correlation_id, issue_id, run_id)
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

pub fn handle_spawn(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  run_id: String,
  session_id: String,
  result: Result(Nil, ledger.LedgerError),
  callbacks: Callbacks,
) -> transition_types.Outcome {
  case dict.get(state.pending_claims, issue_id) {
    Error(Nil) -> stale_continuation(state, correlation_id, issue_id, run_id)
    Ok(pending) ->
      case pending.run_id == run_id && pending.session_id == session_id {
        False -> stale_continuation(state, correlation_id, issue_id, run_id)
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
            Ok(Nil) -> start_worker(state, pending, callbacks)
          }
      }
  }
}

fn start_worker(
  state: transition_types.State,
  pending: transition_types.PendingClaim,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(dispatch_candidates: dispatch) = callbacks
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
      yaml_step_runs: workers.yaml_step_runs,
      stopped_yaml_runs: workers.stopped_yaml_runs,
    )
  let state =
    transition_types.State(
      ..state,
      pending_claims: dict.delete(state.pending_claims, pending.issue_id),
      runtime: core.apply_worker_start(
        state.runtime,
        pending.issue,
        pending.workspace_path,
      ),
      workers: workers,
    )
  let continued =
    dispatch(pending.remaining_candidates, state, pending.dispatch_context)
  transition_types.Outcome(state: continued.state, effects: [
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
    ..continued.effects
  ])
}

fn stale_continuation(
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
