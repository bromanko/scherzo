import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/error
import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/helpers
import scherzo/path as scherzo_path
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_gate
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/structured_output
import scherzo/task
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
      case review_lane_claim_gate(context, workflow_id) {
        review_lane_preflight_gate.ClaimBlocked(code, message, park_on_failure) ->
          block_for_review_lane_preflight(
            state,
            issue,
            remaining_candidates,
            context,
            callbacks,
            workflow_id,
            code,
            message,
            park_on_failure,
          )
        review_lane_preflight_gate.ClaimAllowed ->
          case
            workspace.workspace_path(context.workspace_root, issue.identifier)
          {
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
              let task_ref =
                orchestrator_state.issue_ref_for_backend(
                  issue,
                  context.tracker_backend_kind,
                )
              let identity = orchestrator_state.task_ref_identity(task_ref)
              let recovery =
                dict.get(context.recovery_by_issue, issue.id)
                |> option.from_result
              let pending =
                transition_types.PendingClaim(
                  task_ref: task_ref,
                  issue_id: issue.id,
                  run_id: identity.run_id_to_string(identity.run_id_from_string(
                    run_id,
                  )),
                  session_id: identity.session_id_to_string(
                    identity.session_id_from_string(session_id),
                  ),
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
                    identity,
                    pending,
                  ),
                  next_session_sequence: sequence + 1,
                ),
                effects: [
                  effects_types.ReserveSessionSequence(sequence),
                  effects_types.ClaimIssue(
                    task_ref,
                    issue,
                    workspace_path,
                    run_id,
                  ),
                ],
              )
            }
          }
      }
  }
}

fn review_lane_claim_gate(
  context: transition_types.DispatchContext,
  workflow_id: String,
) -> review_lane_preflight_gate.ClaimGateResult {
  let preflight = context.review_lane_preflight
  let result = case preflight.override {
    Some(result) -> result
    None ->
      case dict.get(preflight.workflow_dags, workflow_id) {
        Ok(dag) ->
          review_lane_preflight.for_workflow(
            workflow_id,
            dag,
            structured_output.validator_repo_root(preflight.config_dir, "."),
            workflow_path_for_preflight(context, workflow_id),
            scherzo_path.join(context.workspace_root, ".scherzo-state"),
            context.effective,
            preflight.policy,
            context.now_ms,
          )
        Error(Nil) ->
          review_lane_preflight.failed(
            "missing-workflow-dag",
            "review_lane_preflight_workflow_missing",
            "review-lane preflight could not find loaded workflow DAG "
              <> workflow_id,
            True,
          )
      }
  }
  review_lane_preflight_gate.before_claim(preflight.policy, result)
}

fn workflow_path_for_preflight(
  context: transition_types.DispatchContext,
  workflow_id: String,
) -> String {
  case dict.get(context.routing.workflows, workflow_id) {
    Error(Nil) -> workflow_id
    Ok(workflow_path) ->
      case
        string.starts_with(workflow_path, "/")
        || string.starts_with(workflow_path, ".")
      {
        True -> workflow_path
        False ->
          scherzo_path.join(
            context.review_lane_preflight.config_dir,
            workflow_path,
          )
      }
  }
}

fn block_for_review_lane_preflight(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  workflow_id: String,
  code: String,
  message: String,
  park_on_failure: Bool,
) -> transition_types.Outcome {
  let Callbacks(dispatch_candidates: dispatch) = callbacks
  let #(state, park_effects) = case park_on_failure {
    False -> #(state, [])
    True -> park_preflight_failure(state, issue, context.now_ms)
  }
  let outcome = dispatch(remaining_candidates, state, context)
  transition_types.Outcome(state: outcome.state, effects: [
    effects_types.Log("warn", "review_infrastructure_preflight_failed", [
      #("issue_id", issue.id),
      #("issue_identifier", issue.identifier),
      #("workflow_id", workflow_id),
      #("code", code),
      #("message", message),
      #("park_on_failure", helpers.bool_field(park_on_failure)),
    ]),
    ..list_append(park_effects, outcome.effects)
  ])
}

fn park_preflight_failure(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  now_ms: Int,
) -> #(transition_types.State, List(effects_types.Effect)) {
  let task_ref = task.from_legacy_issue(issue).ref
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator(
        "review_infrastructure_preflight_failed",
      ),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: now_ms,
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      parked: dict.insert(
        state.runtime.parked,
        orchestrator_state.task_ref_identity(task_ref),
        parked,
      ),
    )
  #(transition_types.State(..state, runtime: runtime), [
    effects_types.ParkIssue(parked, None),
  ])
}

fn list_append(left: List(a), right: List(a)) -> List(a) {
  case left {
    [] -> right
    [item, ..rest] -> [item, ..list_append(rest, right)]
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
  task_identity: identity.TaskIdentity,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  session_id: identity.SessionId,
  batch: ledger_batch.LedgerBatch,
  failure_event: String,
) -> transition_types.Outcome {
  case dict.get(state.pending_claims, task_identity) {
    Error(Nil) -> stale_continuation(state, correlation_id, issue_id, run_id)
    Ok(pending) ->
      case
        pending.run_id == identity.run_id_to_string(run_id)
        && pending.session_id == identity.session_id_to_string(session_id)
      {
        False -> stale_continuation(state, correlation_id, issue_id, run_id)
        True ->
          case ledger_batch.to_bodies(batch) {
            [] ->
              transition_types.Outcome(
                state: clear_pending_claim(state, task_identity),
                effects: [
                  effects_types.Log("warn", "claim_ledger_append_empty", [
                    #("issue_id", identity.issue_id_to_string(issue_id)),
                    #("run_id", identity.run_id_to_string(run_id)),
                    #("correlation_id", correlation_id),
                  ]),
                ],
              )
            _ ->
              case claim_started_batch_is_valid(batch, pending.run_id) {
                False ->
                  transition_types.Outcome(
                    state: clear_pending_claim(state, task_identity),
                    effects: [
                      effects_types.Log(
                        "warn",
                        "claim_ledger_append_invalid_claim_started",
                        [
                          #("issue_id", identity.issue_id_to_string(issue_id)),
                          #("run_id", identity.run_id_to_string(run_id)),
                          #("correlation_id", correlation_id),
                        ],
                      ),
                    ],
                  )
                True ->
                  transition_types.Outcome(state: state, effects: [
                    effects_types.AppendLedger(effects_types.LedgerAppend(
                      correlation_id: correlation_id,
                      batch: batch,
                      failure_event: failure_event,
                      policy: effects_types.SpawnClaimedWorkerAfterAppend(
                        task_identity,
                        issue_id,
                        run_id,
                        session_id,
                      ),
                    )),
                  ])
              }
          }
      }
  }
}

pub fn handle_spawn(
  state: transition_types.State,
  correlation_id: String,
  task_identity: identity.TaskIdentity,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  session_id: identity.SessionId,
  result: Result(Nil, ledger.LedgerError),
  callbacks: Callbacks,
) -> transition_types.Outcome {
  case dict.get(state.pending_claims, task_identity) {
    Error(Nil) -> stale_continuation(state, correlation_id, issue_id, run_id)
    Ok(pending) ->
      case
        pending.run_id == identity.run_id_to_string(run_id)
        && pending.session_id == identity.session_id_to_string(session_id)
      {
        False -> stale_continuation(state, correlation_id, issue_id, run_id)
        True ->
          case result {
            Error(err) ->
              transition_types.Outcome(
                state: clear_pending_claim(state, task_identity),
                effects: [
                  effects_types.Log("warn", "ledger_append_failed", [
                    #("issue_id", identity.issue_id_to_string(issue_id)),
                    #("run_id", identity.run_id_to_string(run_id)),
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
      task_ref: pending.task_ref,
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
  let identity = orchestrator_state.task_ref_identity(pending.task_ref)
  let workers =
    transition_types.WorkerDirectory(
      by_issue: dict.insert(workers.by_issue, identity, worker_entry),
      by_session: dict.insert(workers.by_session, pending.session_id, identity),
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
      pending_claims: dict.delete(state.pending_claims, identity),
      runtime: core.apply_task_ref_start(
        state.runtime,
        pending.task_ref,
        pending.issue,
        pending.workspace_path,
      ),
      workers: workers,
    )
  let continued =
    dispatch(pending.remaining_candidates, state, pending.dispatch_context)
  transition_types.Outcome(state: continued.state, effects: [
    effects_types.StartWorker(effects_types.WorkerStart(
      task_ref: pending.task_ref,
      issue_id: identity.issue_id_from_string(pending.issue_id),
      run_id: identity.run_id_from_string(pending.run_id),
      session_id: identity.session_id_from_string(pending.session_id),
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
  issue_id: identity.IssueId,
  run_id: identity.RunId,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", "claim_ledger_continuation_stale", [
      #("issue_id", identity.issue_id_to_string(issue_id)),
      #("run_id", identity.run_id_to_string(run_id)),
      #("correlation_id", correlation_id),
    ]),
  ])
}

fn clear_pending_claim(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_claims: dict.delete(state.pending_claims, task_identity),
  )
}

fn claim_started_batch_is_valid(
  batch: ledger_batch.LedgerBatch,
  pending_run_id: String,
) -> Bool {
  ledger_batch.to_bodies(batch)
  |> list.any(fn(body) {
    case body {
      record.WorkflowRunStarted(run_id, _, _, _, _, _, _, _)
      | record.WorkflowRunStartedWithTask(run_id, _, _, _, _, _, _, _, _) ->
        run_id == pending_run_id
      _ -> False
    }
  })
}

fn ledger_error_code(err: ledger.LedgerError) -> String {
  ledger.ledger_error_code(err)
}
