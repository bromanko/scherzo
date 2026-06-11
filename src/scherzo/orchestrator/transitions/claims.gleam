import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/error
import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/task_lifecycle_legacy
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/helpers
import scherzo/path as scherzo_path
import scherzo/review_lane_preflight
import scherzo/review_lane_preflight_gate
import scherzo/review_lane_preflight_policy
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
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

// nolint: missing_type_annotation -- return type is constrained by transition_types.Outcome constructor while keeping this oversized transition module within its source guardrail baseline
pub fn sync_outcome(outcome: transition_types.Outcome) {
  transition_types.Outcome(
    state: sync_state(outcome.state),
    effects: outcome.effects,
  )
}

pub fn has_dispatch_blocker(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> Bool {
  task_lifecycle_legacy.has_dispatch_blocker(state, task_identity)
}

pub fn has_tracker_claim(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> Bool {
  task_lifecycle_legacy.has_tracker_claim(state, task_identity)
}

// nolint: missing_type_annotation -- type is constrained by task_lifecycle_legacy.pending_count_for_state without adding another import to this oversized transition module
pub fn pending_count_for_state(
  state: transition_types.State,
  normalized_state,
) {
  task_lifecycle_legacy.pending_count_for_state(state, normalized_state)
}

pub fn add_pending_dispatch_validation(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
  pending: transition_types.PendingDispatchValidation,
  next_generation: Int,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_dispatch_validations: dict.insert(
      state.pending_dispatch_validations,
      task_identity,
      pending,
    ),
    next_dispatch_validation_generation: next_generation,
  )
  |> sync_state
}

pub fn remove_pending_dispatch_validation(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_dispatch_validations: dict.delete(
      state.pending_dispatch_validations,
      task_identity,
    ),
  )
  |> sync_state
}

pub fn add_pending_review_lane_preflight(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
  pending: transition_types.PendingReviewLanePreflight,
  next_generation: Int,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_review_lane_preflights: dict.insert(
      state.pending_review_lane_preflights,
      task_identity,
      pending,
    ),
    next_dispatch_validation_generation: next_generation,
  )
  |> sync_state
}

pub fn remove_pending_review_lane_preflight(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> transition_types.State {
  transition_types.State(
    ..state,
    pending_review_lane_preflights: dict.delete(
      state.pending_review_lane_preflights,
      task_identity,
    ),
  )
  |> sync_state
}

pub fn park_runtime(
  state: transition_types.State,
  ref: task.TaskRef,
  issue: tracker_issue.Issue,
  reason: orchestrator_reason.ParkReason,
  now_ms: Int,
) -> transition_types.State {
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: reason,
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: now_ms,
    )
  let task_identity = orchestrator_state.task_ref_identity(ref)
  transition_types.State(
    ..state,
    runtime: orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, task_identity),
      claimed: dict.delete(state.runtime.claimed, task_identity),
      retry_attempts: dict.delete(state.runtime.retry_attempts, task_identity),
      issue_counters: dict.delete(state.runtime.issue_counters, task_identity),
      parked: dict.insert(state.runtime.parked, task_identity, parked),
    ),
    retry_refresh_generations: dict.delete(
      state.retry_refresh_generations,
      orchestrator_state.issue_id_identity_for_backend(
        issue.id,
        ref.backend_kind,
      ),
    ),
  )
  |> sync_state
}

pub fn complete_runtime_failure(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
  issue: tracker_issue.Issue,
  tokens: session_tokens.TokenTotals,
) -> transition_types.State {
  transition_types.State(
    ..state,
    runtime: orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, task_identity),
      completed: dict.insert(state.runtime.completed, task_identity, issue),
      aggregate_pi_totals: session_tokens.add(
        state.runtime.aggregate_pi_totals,
        tokens,
      ),
    ),
  )
  |> sync_state
}

pub fn begin_for_issue(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  begin_for_issue_with_retry_metadata(
    state,
    issue,
    remaining_candidates,
    context,
    callbacks,
    0,
    None,
  )
}

pub fn begin_for_issue_after_retry(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  previous_retry_generation: Int,
  retry_cancellation: transition_types.RetryCancellation,
) -> transition_types.Outcome {
  begin_for_issue_with_retry_metadata(
    state,
    issue,
    remaining_candidates,
    context,
    callbacks,
    previous_retry_generation,
    Some(retry_cancellation),
  )
}

fn begin_for_issue_with_retry_metadata(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  previous_retry_generation: Int,
  retry_cancellation: Option(transition_types.RetryCancellation),
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
      case
        begin_review_lane_preflight_if_needed(
          state,
          issue,
          remaining_candidates,
          context,
          callbacks,
          workflow_id,
          previous_retry_generation,
          retry_cancellation,
        )
      {
        Some(outcome) -> outcome
        None ->
          begin_claim_for_workflow(
            state,
            issue,
            remaining_candidates,
            context,
            callbacks,
            workflow_id,
            previous_retry_generation,
            retry_cancellation,
          )
      }
  }
}

fn begin_review_lane_preflight_if_needed(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  workflow_id: String,
  previous_retry_generation: Int,
  retry_cancellation: Option(transition_types.RetryCancellation),
) -> Option(transition_types.Outcome) {
  let preflight = context.review_lane_preflight
  case preflight.policy.mode, preflight.override {
    review_lane_preflight_policy.Off, _ -> None
    _, Some(result) ->
      Some(handle_review_lane_preflight_result(
        state,
        issue,
        remaining_candidates,
        context,
        callbacks,
        workflow_id,
        previous_retry_generation,
        retry_cancellation,
        result,
      ))
    _, None ->
      Some(begin_review_lane_preflight(
        state,
        issue,
        remaining_candidates,
        context,
        callbacks,
        workflow_id,
        previous_retry_generation,
        retry_cancellation,
      ))
  }
}

fn begin_review_lane_preflight(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  workflow_id: String,
  previous_retry_generation: Int,
  retry_cancellation: Option(transition_types.RetryCancellation),
) -> transition_types.Outcome {
  let task_ref =
    orchestrator_state.issue_ref_for_backend(
      issue,
      context.tracker_backend_kind,
    )
  let task_identity = orchestrator_state.task_ref_identity(task_ref)
  let generation = state.next_dispatch_validation_generation
  case
    review_lane_preflight_request(
      context,
      task_identity,
      issue,
      workflow_id,
      generation,
    )
  {
    Ok(request) -> {
      let pending =
        transition_types.PendingReviewLanePreflight(
          task_ref: task_ref,
          issue: issue,
          remaining_candidates: remaining_candidates,
          generation: generation,
          workflow_id: workflow_id,
          previous_retry_generation: previous_retry_generation,
          retry_cancellation: retry_cancellation,
        )
      transition_types.Outcome(
        state: add_pending_review_lane_preflight(
          state,
          task_identity,
          pending,
          generation + 1,
        ),
        effects: [effects_types.BeginReviewLanePreflight(request)],
      )
    }
    Error(result) ->
      handle_review_lane_preflight_result(
        state,
        issue,
        remaining_candidates,
        context,
        callbacks,
        workflow_id,
        previous_retry_generation,
        retry_cancellation,
        result,
      )
  }
}

fn review_lane_preflight_request(
  context: transition_types.DispatchContext,
  task_identity: identity.TaskIdentity,
  issue: tracker_issue.Issue,
  workflow_id: String,
  generation: Int,
) -> Result(
  effects_types.ReviewLanePreflightRequest,
  review_lane_preflight.PreflightResult,
) {
  let preflight = context.review_lane_preflight
  case dict.get(preflight.workflow_dags, workflow_id) {
    Ok(dag) ->
      Ok(effects_types.ReviewLanePreflightRequest(
        task_identity: task_identity,
        issue_id: issue.id,
        generation: generation,
        workflow_id: workflow_id,
        workflow_dag: dag,
        config_dir: preflight.config_dir,
        workflow_path: workflow_path_for_preflight(context, workflow_id),
        state_root: scherzo_path.join(context.workspace_root, ".scherzo-state"),
        effective: context.effective,
        policy: preflight.policy,
        now_ms: context.now_ms,
      ))
    Error(Nil) ->
      Error(review_lane_preflight.failed(
        "missing-workflow-dag",
        "review_lane_preflight_workflow_missing",
        "review-lane preflight could not find loaded workflow DAG "
          <> workflow_id,
        True,
      ))
  }
}

pub fn resume_after_review_lane_preflight(
  state: transition_types.State,
  pending: transition_types.PendingReviewLanePreflight,
  context: transition_types.DispatchContext,
  result: review_lane_preflight.PreflightResult,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  handle_review_lane_preflight_result(
    state,
    pending.issue,
    pending.remaining_candidates,
    transition_types.DispatchContext(
      ..context,
      review_lane_preflight: transition_types.ReviewLanePreflightContext(
        ..context.review_lane_preflight,
        override: Some(result),
      ),
    ),
    callbacks,
    pending.workflow_id,
    pending.previous_retry_generation,
    pending.retry_cancellation,
    result,
  )
}

fn handle_review_lane_preflight_result(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  workflow_id: String,
  previous_retry_generation: Int,
  retry_cancellation: Option(transition_types.RetryCancellation),
  result: review_lane_preflight.PreflightResult,
) -> transition_types.Outcome {
  case
    review_lane_preflight_gate.before_claim(
      context.review_lane_preflight.policy,
      result,
    )
  {
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
      begin_claim_for_workflow(
        state,
        issue,
        remaining_candidates,
        context,
        callbacks,
        workflow_id,
        previous_retry_generation,
        retry_cancellation,
      )
  }
}

fn begin_claim_for_workflow(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
  callbacks: Callbacks,
  workflow_id: String,
  previous_retry_generation: Int,
  retry_cancellation: Option(transition_types.RetryCancellation),
) -> transition_types.Outcome {
  let Callbacks(dispatch_candidates: dispatch) = callbacks
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
          run_id: identity.run_id_to_string(identity.run_id_from_string(run_id)),
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
          previous_retry_generation: previous_retry_generation,
          retry_cancellation: retry_cancellation,
        )
      transition_types.Outcome(
        state: transition_types.State(
          ..state,
          pending_claims: dict.insert(state.pending_claims, identity, pending),
          next_session_sequence: sequence + 1,
        )
          |> sync_state,
        effects: [
          effects_types.ReserveSessionSequence(sequence),
          effects_types.ClaimIssue(task_ref, issue, workspace_path, run_id),
        ],
      )
    }
  }
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
  #(transition_types.State(..state, runtime: runtime) |> sync_state, [
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
              handle_claim_append_failed(
                state,
                pending,
                task_identity,
                issue_id,
                run_id,
                correlation_id,
                err,
              )
            Ok(Nil) -> start_worker(state, pending, callbacks)
          }
      }
  }
}

fn handle_claim_append_failed(
  state: transition_types.State,
  pending: transition_types.PendingClaim,
  task_identity: identity.TaskIdentity,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  correlation_id: String,
  err: ledger.LedgerError,
) -> transition_types.Outcome {
  case pending.retry_cancellation {
    Some(retry_cancellation) ->
      restore_retry_after_claim_append_failure(
        state,
        pending,
        task_identity,
        issue_id,
        run_id,
        correlation_id,
        err,
        retry_cancellation,
      )
    None ->
      schedule_claim_start_recovery_retry(
        state,
        pending,
        task_identity,
        issue_id,
        run_id,
        correlation_id,
        err,
      )
  }
}

fn restore_retry_after_claim_append_failure(
  state: transition_types.State,
  pending: transition_types.PendingClaim,
  task_identity: identity.TaskIdentity,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  correlation_id: String,
  err: ledger.LedgerError,
  retry_cancellation: transition_types.RetryCancellation,
) -> transition_types.Outcome {
  let transition_types.RetryCancellation(previous_retry: previous_retry, ..) =
    retry_cancellation
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      retry_attempts: dict.insert(
        state.runtime.retry_attempts,
        task_identity,
        previous_retry,
      ),
      claimed: dict.insert(
        state.runtime.claimed,
        task_identity,
        pending.issue.identifier,
      ),
    )
  let state =
    transition_types.State(
      ..state,
      pending_claims: dict.delete(state.pending_claims, task_identity),
      runtime: runtime,
    )
    |> sync_state
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", "ledger_append_failed", [
      #("issue_id", identity.issue_id_to_string(issue_id)),
      #("run_id", identity.run_id_to_string(run_id)),
      #("correlation_id", correlation_id),
      #("error", ledger_error_code(err)),
    ]),
    effects_types.DeferRetryTimer(
      pending.issue_id,
      previous_retry.timer_generation,
      previous_retry.delay_ms,
    ),
  ])
}

fn schedule_claim_start_recovery_retry(
  state: transition_types.State,
  pending: transition_types.PendingClaim,
  task_identity: identity.TaskIdentity,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  correlation_id: String,
  err: ledger.LedgerError,
) -> transition_types.Outcome {
  let retry_reason = orchestrator_reason.RetryClaimStartLedgerAppendFailed
  let generation =
    next_retry_generation(
      state.runtime,
      task_identity,
      pending.previous_retry_generation,
    )
  let delay_ms =
    core.backoff_delay(
      generation,
      pending.dispatch_context.effective.agent.max_retry_backoff_ms,
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      retry_attempts: dict.insert(
        state.runtime.retry_attempts,
        task_identity,
        orchestrator_state.RetryEntry(
          task_ref: pending.task_ref,
          issue_id: pending.issue_id,
          delay_ms: delay_ms,
          timer_generation: generation,
        ),
      ),
      claimed: dict.insert(
        state.runtime.claimed,
        task_identity,
        pending.issue.identifier,
      ),
    )
  let state =
    transition_types.State(
      ..state,
      pending_claims: dict.delete(state.pending_claims, task_identity),
      runtime: runtime,
    )
    |> sync_state
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", "ledger_append_failed", [
      #("issue_id", identity.issue_id_to_string(issue_id)),
      #("run_id", identity.run_id_to_string(run_id)),
      #("correlation_id", correlation_id),
      #("error", ledger_error_code(err)),
    ]),
    effects_types.Log("warn", "claim_start_recovery_retry_scheduled", [
      #("issue_id", identity.issue_id_to_string(issue_id)),
      #("run_id", identity.run_id_to_string(run_id)),
      #("delay_ms", int.to_string(delay_ms)),
    ]),
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "claim_start_retry_schedule:"
        <> pending.issue_id
        <> ":"
        <> int.to_string(generation),
      batch: ledger_batch.retry_scheduled(
        pending.issue_id,
        pending.issue.identifier,
        delay_ms,
        generation,
        orchestrator_reason.retry_to_string(retry_reason),
      ),
      failure_event: "ledger_append_failed",
      policy: effects_types.ScheduleRetryTimerAfterAppend(
        pending.issue_id,
        delay_ms,
        generation,
        retry_reason,
        None,
      ),
    )),
  ])
}

fn next_retry_generation(
  runtime: orchestrator_state.RuntimeState,
  task_identity: identity.TaskIdentity,
  previous_retry_generation: Int,
) -> Int {
  case dict.get(runtime.retry_attempts, task_identity) {
    Ok(entry) -> entry.timer_generation + 1
    Error(Nil) ->
      case previous_retry_generation > 0 {
        True -> previous_retry_generation + 1
        False -> 1
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
    |> sync_state
  let continued =
    dispatch(pending.remaining_candidates, state, pending.dispatch_context)
  let retry_effects = case pending.retry_cancellation {
    Some(transition_types.RetryCancellation(
      generation: generation,
      reason: reason,
      ..,
    )) -> [
      effects_types.CancelRetryTimer(pending.issue_id, generation, reason),
    ]
    None -> []
  }
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
    ..list_append(retry_effects, continued.effects)
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
  |> sync_state
}

fn sync_state(state: transition_types.State) -> transition_types.State {
  case task_lifecycle_legacy.from_transition_state(state) {
    Ok(directory) -> transition_types.State(..state, lifecycle: directory)
    Error(_) -> state
  }
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
