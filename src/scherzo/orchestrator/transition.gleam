import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/error
import scherzo/log

import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/claims
import scherzo/orchestrator/transitions/commands
import scherzo/review_lane_preflight
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens

import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_policy

pub fn handle(
  message: transition_types.Message,
  state: transition_types.State,
) -> transition_types.Outcome {
  let outcome = case message {
    transition_types.SnapshotRequested ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ReplySnapshot(state.runtime),
      ])
    transition_types.StartupRecoveryApplied(
      retry_timers,
      cleanup_workspaces,
      outbox_to_replay,
      park_reports,
      warnings,
      secrets,
    ) ->
      handle_startup_recovery_applied(
        state,
        retry_timers,
        cleanup_workspaces,
        outbox_to_replay,
        park_reports,
        warnings,
        secrets,
      )
    transition_types.PollTick(generation, poll) ->
      handle_poll_tick(state, generation, poll)
    transition_types.CandidateFetchStartRequested(generation, context) ->
      handle_candidate_fetch_start_requested(state, generation, context)
    transition_types.RunningRefreshCompleted(generation, poll, result, context) ->
      handle_running_refresh_completed(state, generation, poll, result, context)
    transition_types.CandidateFetchCompleted(generation, poll, result, context) ->
      handle_candidate_fetch_completed(state, generation, poll, result, context)
    transition_types.OperatorCommandSubmitted(
      request,
      context,
      issue_resolution,
      parked_issue_resolution,
    ) ->
      commands.handle_operator_submitted(
        state,
        request,
        context,
        issue_resolution,
        parked_issue_resolution,
        operator_callbacks(),
      )
    transition_types.DispatchCandidates(candidates, context) ->
      dispatch_candidates_outcome(candidates, state, context)
    transition_types.DispatchValidationCompleted(
      issue_id,
      generation,
      result,
      context,
    ) ->
      handle_dispatch_validation_completed(
        state,
        issue_id,
        generation,
        result,
        context,
      )
    transition_types.ReviewLanePreflightCompleted(
      task_identity,
      issue_id,
      generation,
      workflow_id,
      context,
      result,
    ) ->
      handle_review_lane_preflight_completed(
        state,
        task_identity,
        issue_id,
        generation,
        workflow_id,
        context,
        result,
      )
    transition_types.HandoffClaimCompleted(
      task_identity,
      issue_id,
      run_id,
      result,
    ) ->
      handle_handoff_claim_completed(
        state,
        task_identity,
        issue_id,
        run_id,
        result,
      )
    transition_types.RetryTick(issue_id, generation, context) ->
      handle_retry_tick(state, issue_id, generation, context)
    transition_types.RetryRefreshCompleted(
      issue_id,
      generation,
      result,
      context,
    ) ->
      handle_retry_refresh_completed(
        state,
        issue_id,
        generation,
        result,
        context,
      )
    transition_types.ClaimLedgerAppendRequested(
      correlation_id,
      task_identity,
      issue_id,
      run_id,
      session_id,
      batch,
      failure_event,
    ) ->
      claims.handle_requested(
        state,
        correlation_id,
        task_identity,
        issue_id,
        run_id,
        session_id,
        batch,
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
    transition_types.WorkerStartSucceeded(issue_id, run_id, session_id) ->
      handle_worker_start_succeeded(state, issue_id, run_id, session_id)
    transition_types.WorkerStartFailed(issue_id, run_id, session_id, reason) ->
      handle_worker_start_failed(state, issue_id, run_id, session_id, reason)
    transition_types.WorkerCommandReady(issue_id, run_id) ->
      handle_worker_command_ready(state, issue_id, run_id)
    transition_types.WorkerFinished(issue_id, run_id, result, context) ->
      handle_worker_finished(state, issue_id, run_id, result, context)
    transition_types.WorkerDown(resolution, context) ->
      handle_worker_down(state, resolution, context)
    transition_types.WorkerStopRequested(session_id, reason, context) ->
      handle_worker_stop_requested(state, session_id, reason, context)
    transition_types.YamlStepStarted(session_id, run_id) ->
      handle_yaml_step_started(state, session_id, run_id)
    transition_types.YamlStepFinished(session_id) ->
      handle_yaml_step_finished(state, session_id)
    transition_types.ShutdownRequested(stop_effect_runner) ->
      handle_shutdown_requested(state, stop_effect_runner)
  }
  normalize_outcome(outcome)
}

fn normalize_outcome(
  outcome: transition_types.Outcome,
) -> transition_types.Outcome {
  claims.sync_outcome(outcome)
}

pub fn snapshot(
  state: transition_types.State,
) -> orchestrator_state.RuntimeState {
  state.runtime
}

fn handle_startup_recovery_applied(
  state: transition_types.State,
  retry_timers: List(recovery.RecoveredRetry),
  cleanup_workspaces: List(recovery.CleanupRequest),
  _outbox_to_replay: List(recovery.OutboxReplay),
  park_reports: List(adapter.ParkReport),
  warnings: List(String),
  secrets: List(String),
) -> transition_types.Outcome {
  let effects =
    list.append(
      startup_recovered_retry_effects(retry_timers, secrets),
      list.append(
        startup_cleanup_effects(cleanup_workspaces),
        list.append(
          startup_park_report_effects(park_reports),
          startup_warning_effects(warnings, secrets),
        ),
      ),
    )
  transition_types.Outcome(state: state, effects: effects)
}

fn startup_recovered_retry_effects(
  retry_timers: List(recovery.RecoveredRetry),
  secrets: List(String),
) -> List(effects_types.Effect) {
  list.flat_map(retry_timers, fn(retry) {
    let recovery.RecoveredRetry(
      issue_id,
      issue_identifier,
      delay_ms,
      generation,
      reason_text,
    ) = retry
    let safe_reason =
      safe_recovery_text("recovery_reason", reason_text, secrets)
    [
      effects_types.Log("info", "workflow_recovery_status", [
        #("issue_id", issue_id),
        #("issue_identifier", issue_identifier),
        #("status", "recovered"),
        #("source", "recovery.recovered_retry"),
        #("delay_ms", int.to_string(delay_ms)),
        #("generation", int.to_string(generation)),
        #("reason", safe_reason),
      ]),
      effects_types.Log("info", "recovered_retry_scheduled", [
        #("issue_id", issue_id),
        #("delay_ms", int.to_string(delay_ms)),
        #("generation", int.to_string(generation)),
        #("reason", safe_reason),
      ]),
      effects_types.ScheduleRecoveredRetryTimer(issue_id, delay_ms, generation),
    ]
  })
}

fn startup_cleanup_effects(
  cleanup_workspaces: List(recovery.CleanupRequest),
) -> List(effects_types.Effect) {
  list.flat_map(cleanup_workspaces, fn(cleanup) {
    let recovery.CleanupRequest(issue_id, issue_identifier, workspace_path) =
      cleanup
    [
      effects_types.Log("info", "workflow_recovery_status", [
        #("issue_id", issue_id),
        #("issue_identifier", issue_identifier),
        #("status", "cleanup"),
        #("source", "recovery.cleanup_request"),
        #("reason", "terminal interrupted run cleanup queued"),
      ]),
      effects_types.Log("info", "recovered_workspace_cleanup", [
        #("issue_id", issue_id),
        #("workspace_path", workspace_path),
      ]),
      effects_types.CleanupWorkspace(workspace_path),
    ]
  })
}

fn startup_park_report_effects(
  park_reports: List(adapter.ParkReport),
) -> List(effects_types.Effect) {
  list.map(park_reports, effects_types.ReportPark)
}

fn startup_warning_effects(
  warnings: List(String),
  secrets: List(String),
) -> List(effects_types.Effect) {
  list.flat_map(warnings, fn(warning) {
    let safe_warning = safe_recovery_text("recovery_warning", warning, secrets)
    [
      effects_types.Log("warn", "workflow_recovery_status", [
        #("status", "recovered"),
        #("source", "recovery.warning"),
        #("reason", safe_warning),
      ]),
      effects_types.Log("warn", "startup_recovery_warning", [
        #("warning", safe_warning),
      ]),
    ]
  })
}

fn safe_recovery_text(
  label: String,
  value: String,
  secrets: List(String),
) -> String {
  log.redact(label, value, secrets)
  |> log.truncate(200)
}

fn handle_shutdown_requested(
  state: transition_types.State,
  stop_effect_runner: Bool,
) -> transition_types.Outcome {
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.new(),
      claimed: dict.new(),
      retry_attempts: dict.new(),
    )
  transition_types.Outcome(
    state: transition_types.State(
      ..state,
      runtime: runtime,
      workers: transition_types.new_worker_directory(),
      pending_claims: dict.new(),
      pending_dispatch_validations: dict.new(),
      pending_review_lane_preflights: dict.new(),
      lifecycle: transition_types.empty_lifecycle(),
      retry_refresh_generations: dict.new(),
    ),
    effects: [effects_types.ShutdownRuntime(stop_effect_runner)],
  )
}

fn operator_callbacks() -> commands.OperatorCallbacks {
  commands.operator_callbacks(
    issue_is_running_claimed_or_pending,
    can_reserve_dispatch_slot,
    dispatch_candidates_outcome,
  )
}

fn claim_callbacks() -> claims.Callbacks {
  claims.Callbacks(dispatch_candidates: dispatch_candidates_outcome)
}

fn handle_poll_tick(
  state: transition_types.State,
  generation: Int,
  poll: transition_types.PollSnapshot,
) -> transition_types.Outcome {
  case poll_tick_is_stale(poll, generation) {
    True -> transition_types.Outcome(state: state, effects: [])
    False ->
      transition_types.Outcome(state: state, effects: [
        effects_types.MarkPollInFlight(generation),
        effects_types.Log("info", "tick_started", [
          #("generation", int.to_string(generation)),
        ]),
      ])
  }
}

fn poll_tick_is_stale(
  poll: transition_types.PollSnapshot,
  generation: Int,
) -> Bool {
  generation != poll.generation || poll.in_flight != None
}

fn poll_result_is_stale(
  poll: transition_types.PollSnapshot,
  generation: Int,
) -> Bool {
  generation != poll.generation || poll.in_flight != Some(generation)
}

fn handle_candidate_fetch_start_requested(
  state: transition_types.State,
  generation: Int,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case candidate_fetch_allowed(state, context) {
    True ->
      transition_types.Outcome(state: state, effects: [
        effects_types.FetchCandidates(generation),
      ])
    False -> finish_candidate_phase(state, generation, [], False, context)
  }
}

fn candidate_fetch_allowed(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> Bool {
  !context.operator_paused
  && context.dispatch_enabled
  && context.effective.agent.max_concurrent_agents != 0
  && slots_remain(state, context)
}

fn handle_running_refresh_completed(
  state: transition_types.State,
  generation: Int,
  poll: transition_types.PollSnapshot,
  result: Result(List(tracker_issue.Issue), String),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case poll_result_is_stale(poll, generation) {
    True -> transition_types.Outcome(state: state, effects: [])
    False -> {
      let transition_types.Outcome(state: state, effects: refresh_effects) = case
        result
      {
        Error(err) ->
          transition_types.Outcome(state: state, effects: [
            effects_types.Log("warn", "running_refresh_failed", [
              #("error", err),
            ]),
          ])
        Ok(issues) -> reconcile_running_issues(state, issues, context)
      }
      let transition_types.Outcome(state: state, effects: fetch_effects) =
        handle_candidate_fetch_start_requested(state, generation, context)
      transition_types.Outcome(
        state: state,
        effects: list.append(refresh_effects, fetch_effects),
      )
    }
  }
}

fn reconcile_running_issues(
  state: transition_types.State,
  issues: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  list.fold(
    issues,
    transition_types.Outcome(state: state, effects: []),
    fn(outcome, issue) {
      let core.Transition(state: runtime, effects: core_effects) =
        core.reconcile_task_issue(
          outcome.state.runtime,
          context.effective,
          orchestrator_state.issue_ref_for_backend(
            issue,
            context.tracker_backend_kind,
          ),
          issue,
        )
      let state = transition_types.State(..outcome.state, runtime: runtime)
      let transition_types.Outcome(state: state, effects: effects) =
        map_core_effects(state, context, core_effects)
      transition_types.Outcome(
        state: state,
        effects: list.append(outcome.effects, effects),
      )
    },
  )
}

fn handle_candidate_fetch_completed(
  state: transition_types.State,
  generation: Int,
  poll: transition_types.PollSnapshot,
  result: Result(List(tracker_issue.Issue), String),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case poll_result_is_stale(poll, generation) {
    True -> transition_types.Outcome(state: state, effects: [])
    False ->
      case result {
        Error(err) -> {
          let outcome =
            finish_candidate_phase(state, generation, [], False, context)
          transition_types.Outcome(state: outcome.state, effects: [
            effects_types.Log("warn", "candidate_fetch_failed", [
              #("error", err),
            ]),
            ..outcome.effects
          ])
        }
        Ok(candidates) -> {
          let candidates = core.sort_candidates(candidates)
          let outcome =
            finish_candidate_phase(state, generation, candidates, True, context)
          transition_types.Outcome(state: outcome.state, effects: [
            effects_types.Log("info", "candidates_fetched", [
              #("count", int.to_string(list.length(candidates))),
            ]),
            ..outcome.effects
          ])
        }
      }
  }
}

fn finish_candidate_phase(
  state: transition_types.State,
  _generation: Int,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  finish_dispatch_phase(state, candidates, dispatch_after, context)
}

fn finish_dispatch_phase(
  state: transition_types.State,
  candidates: List(tracker_issue.Issue),
  dispatch_after: Bool,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let outcome = case dispatch_after {
    True -> dispatch_candidates_outcome(candidates, state, context)
    False -> transition_types.Outcome(state: state, effects: [])
  }
  transition_types.Outcome(
    state: outcome.state,
    effects: list.append(outcome.effects, [effects_types.ScheduleNextPoll]),
  )
}

fn dispatch_candidates_outcome(
  issues: List(tracker_issue.Issue),
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case !context.operator_paused && context.dispatch_enabled {
    False -> transition_types.Outcome(state: state, effects: [])
    True ->
      case issues {
        [] -> transition_types.Outcome(state: state, effects: [])
        [issue, ..rest] -> dispatch_candidate(issue, rest, state, context)
      }
  }
}

fn dispatch_candidate(
  issue: tracker_issue.Issue,
  rest: List(tracker_issue.Issue),
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case core.is_dispatch_state(context.effective, issue.state) {
    False -> dispatch_candidates_outcome(rest, state, context)
    True -> {
      let state =
        transition_types.State(
          ..state,
          runtime: core.unpark_if_issue_changed(state.runtime, issue),
        )
      let decision = core.blocker_decision(context.effective, issue)
      case decision {
        core.BlockedByDependency(_, _) -> {
          let reported =
            report_blocked_dependency(
              state,
              context,
              issue,
              "candidate",
              "linear_dependency_blocked_candidate",
              decision,
            )
          append_with_next(reported, fn(state) {
            dispatch_candidates_outcome(rest, state, context)
          })
        }
        core.BlockersSatisfied -> {
          let state =
            transition_types.State(
              ..state,
              runtime: core.clear_blocked_dependency_report(
                state.runtime,
                issue.id,
                "candidate",
              ),
            )
          case dispatch_preconditions_without_slot(state, context, issue) {
            False -> dispatch_candidates_outcome(rest, state, context)
            True ->
              case
                workflow_policy.classify_issue(
                  context.effective.linear_contract,
                  issue,
                )
              {
                workflow_policy.WorkflowInvalid(violation) -> {
                  let reported =
                    report_invalid_workflow_candidate(
                      state,
                      context,
                      issue,
                      violation,
                    )
                  append_with_next(reported, fn(state) {
                    dispatch_candidates_outcome(rest, state, context)
                  })
                }
                workflow_policy.WorkflowPolicyDisabled
                | workflow_policy.WorkflowSelected(_, _) -> {
                  let state =
                    transition_types.State(
                      ..state,
                      runtime: core.clear_invalid_workflow_report(
                        state.runtime,
                        issue.id,
                      ),
                    )
                  case can_reserve_dispatch_slot(state, context, issue) {
                    False -> dispatch_candidates_outcome(rest, state, context)
                    True ->
                      begin_dispatch_validation(state, issue, rest, context)
                  }
                }
              }
          }
        }
      }
    }
  }
}

fn begin_dispatch_validation(
  state: transition_types.State,
  issue: tracker_issue.Issue,
  remaining_candidates: List(tracker_issue.Issue),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case issue_is_running_claimed_or_pending(state, context, issue.id) {
    True -> dispatch_candidates_outcome(remaining_candidates, state, context)
    False -> {
      let generation = state.next_dispatch_validation_generation
      let task_ref =
        orchestrator_state.issue_ref_for_backend(
          issue,
          context.tracker_backend_kind,
        )
      let identity = orchestrator_state.task_ref_identity(task_ref)
      let pending =
        transition_types.PendingDispatchValidation(
          task_ref: task_ref,
          issue: issue,
          remaining_candidates: remaining_candidates,
          generation: generation,
          requested_at_ms: context.now_ms,
        )
      transition_types.Outcome(
        state: claims.add_pending_dispatch_validation(
          state,
          identity,
          pending,
          generation + 1,
        ),
        effects: [
          effects_types.Log(
            "info",
            "linear_dependency_claim_validation_started",
            [
              #("issue_id", issue.id),
              #("issue_identifier", issue.identifier),
              #("generation", int.to_string(generation)),
            ],
          ),
          effects_types.BeginDispatchValidation(issue.id, generation),
        ],
      )
    }
  }
}

fn handle_dispatch_validation_completed(
  state: transition_types.State,
  issue_id: String,
  generation: Int,
  result: Result(tracker_issue.Issue, transition_types.DispatchValidationError),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      context.tracker_backend_kind,
    )
  case dict.get(state.pending_dispatch_validations, identity) {
    Error(Nil) -> stale_dispatch_validation(state, issue_id, generation)
    Ok(pending) ->
      case pending.generation != generation {
        True -> stale_dispatch_validation(state, issue_id, generation)
        False -> {
          let state = claims.remove_pending_dispatch_validation(state, identity)
          case result {
            Error(err) -> {
              let outcome =
                dispatch_candidates_outcome(
                  pending.remaining_candidates,
                  state,
                  context,
                )
              transition_types.Outcome(state: outcome.state, effects: [
                effects_types.Log(
                  "warn",
                  "linear_dependency_claim_validation_failed",
                  [
                    #("issue_id", issue_id),
                    #("generation", int.to_string(generation)),
                    #("reason", claims.dispatch_validation_error_reason(err)),
                  ],
                ),
                ..outcome.effects
              ])
            }
            Ok(refreshed_issue) ->
              handle_successful_dispatch_validation(
                state,
                pending,
                refreshed_issue,
                context,
              )
          }
        }
      }
  }
}

fn stale_dispatch_validation(
  state: transition_types.State,
  issue_id: String,
  generation: Int,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("info", "dispatch_validation_stale", [
      #("issue_id", issue_id),
      #("generation", int.to_string(generation)),
    ]),
  ])
}

fn handle_review_lane_preflight_completed(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
  issue_id: String,
  generation: Int,
  workflow_id: String,
  context: transition_types.DispatchContext,
  result: review_lane_preflight.PreflightResult,
) -> transition_types.Outcome {
  case dict.get(state.pending_review_lane_preflights, task_identity) {
    Error(Nil) -> stale_review_lane_preflight(state, issue_id, generation)
    Ok(pending) ->
      case
        pending.issue.id != issue_id
        || pending.generation != generation
        || pending.workflow_id != workflow_id
      {
        True -> stale_review_lane_preflight(state, issue_id, generation)
        False ->
          claims.resume_after_review_lane_preflight(
            claims.remove_pending_review_lane_preflight(state, task_identity),
            pending,
            context,
            result,
            claim_callbacks(),
          )
      }
  }
}

fn stale_review_lane_preflight(
  state: transition_types.State,
  issue_id: String,
  generation: Int,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("info", "review_lane_preflight_stale", [
      #("issue_id", issue_id),
      #("generation", int.to_string(generation)),
    ]),
  ])
}

fn handle_successful_dispatch_validation(
  state: transition_types.State,
  pending: transition_types.PendingDispatchValidation,
  refreshed_issue: tracker_issue.Issue,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case core.is_dispatch_state(context.effective, refreshed_issue.state) {
    False ->
      dispatch_candidates_outcome(pending.remaining_candidates, state, context)
    True -> {
      let state =
        transition_types.State(
          ..state,
          runtime: core.unpark_if_issue_changed(state.runtime, refreshed_issue),
        )
      let decision = core.blocker_decision(context.effective, refreshed_issue)
      case decision {
        core.BlockedByDependency(_, _) -> {
          let reported =
            report_blocked_dependency(
              state,
              context,
              refreshed_issue,
              "claim_validation",
              "linear_dependency_claim_validation_blocked",
              decision,
            )
          append_with_next(reported, fn(state) {
            dispatch_candidates_outcome(
              pending.remaining_candidates,
              state,
              context,
            )
          })
        }
        core.BlockersSatisfied -> {
          let state =
            transition_types.State(
              ..state,
              runtime: core.clear_blocked_dependency_report(
                state.runtime,
                refreshed_issue.id,
                "claim_validation",
              ),
            )
          case
            dispatch_validation_precondition_failure(
              state,
              context,
              refreshed_issue,
            )
          {
            Some(reason) -> {
              let outcome =
                dispatch_candidates_outcome(
                  pending.remaining_candidates,
                  state,
                  context,
                )
              transition_types.Outcome(state: outcome.state, effects: [
                effects_types.Log(
                  "info",
                  "dispatch_validation_precondition_failed",
                  [
                    #("issue_id", refreshed_issue.id),
                    #("generation", int.to_string(pending.generation)),
                    #("reason", reason),
                  ],
                ),
                ..outcome.effects
              ])
            }
            None ->
              case
                workflow_policy.classify_issue(
                  context.effective.linear_contract,
                  refreshed_issue,
                )
              {
                workflow_policy.WorkflowInvalid(violation) -> {
                  let reported =
                    report_invalid_workflow_candidate(
                      state,
                      context,
                      refreshed_issue,
                      violation,
                    )
                  append_with_next(reported, fn(state) {
                    dispatch_candidates_outcome(
                      pending.remaining_candidates,
                      state,
                      context,
                    )
                  })
                }
                workflow_policy.WorkflowPolicyDisabled
                | workflow_policy.WorkflowSelected(_, _) ->
                  case
                    can_reserve_dispatch_slot(state, context, refreshed_issue)
                  {
                    False -> {
                      let outcome =
                        dispatch_candidates_outcome(
                          pending.remaining_candidates,
                          state,
                          context,
                        )
                      transition_types.Outcome(state: outcome.state, effects: [
                        effects_types.Log(
                          "info",
                          "dispatch_validation_slot_unavailable",
                          [
                            #("issue_id", refreshed_issue.id),
                            #("generation", int.to_string(pending.generation)),
                          ],
                        ),
                        ..outcome.effects
                      ])
                    }
                    True ->
                      claims.begin_for_issue(
                        state,
                        refreshed_issue,
                        pending.remaining_candidates,
                        context,
                        claim_callbacks(),
                      )
                  }
              }
          }
        }
      }
    }
  }
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

fn handle_handoff_claim_completed(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  result: transition_types.HandoffClaimResult,
) -> transition_types.Outcome {
  let issue_id_text = identity.issue_id_to_string(issue_id)
  let run_id_text = identity.run_id_to_string(run_id)
  case dict.get(state.pending_claims, task_identity) {
    Error(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "handoff_claim_stale", [
          #("issue_id", issue_id_text),
        ]),
      ])
    Ok(pending) ->
      case pending.run_id != run_id_text {
        True ->
          transition_types.Outcome(state: state, effects: [
            effects_types.Log("warn", "handoff_claim_stale", [
              #("issue_id", issue_id_text),
              #("run_id", run_id_text),
            ]),
          ])
        False ->
          case result {
            transition_types.HandoffClaimFailed(err) -> {
              let state = clear_pending_claim(state, task_identity)
              let outcome =
                dispatch_candidates_outcome(
                  pending.remaining_candidates,
                  state,
                  pending.dispatch_context,
                )
              transition_types.Outcome(state: outcome.state, effects: [
                effects_types.Log("warn", "handoff_claim_failed", [
                  #("issue_id", issue_id_text),
                  #("error", err),
                ]),
                ..outcome.effects
              ])
            }
            transition_types.HandoffClaimStartRecordFailed(reason) -> {
              let state = clear_pending_claim(state, task_identity)
              let outcome =
                dispatch_candidates_outcome(
                  pending.remaining_candidates,
                  state,
                  pending.dispatch_context,
                )
              transition_types.Outcome(state: outcome.state, effects: [
                effects_types.Log("warn", "workflow_checkpoint_start_failed", [
                  #("issue_id", issue_id_text),
                  #("error", reason),
                ]),
                ..outcome.effects
              ])
            }
            transition_types.HandoffClaimSucceeded(batch) ->
              claims.handle_requested(
                state,
                claims.claim_correlation_id(issue_id_text, run_id_text),
                task_identity,
                issue_id,
                run_id,
                identity.session_id_from_string(pending.session_id),
                batch,
                "ledger_append_failed",
              )
          }
      }
  }
}

fn handle_retry_tick(
  state: transition_types.State,
  issue_id: String,
  generation: Int,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      context.tracker_backend_kind,
    )
  case dict.get(state.runtime.retry_attempts, identity) {
    Error(Nil) -> retry_timer_stale(state, issue_id, generation, False)
    Ok(entry) ->
      case entry.timer_generation != generation {
        True -> retry_timer_stale(state, issue_id, generation, True)
        False -> {
          let accepted_effects = [effects_types.RemoveRetryTimer(issue_id)]
          case retry_dispatch_available(state, context) {
            False ->
              transition_types.Outcome(
                state: state,
                effects: list.append(accepted_effects, [
                  effects_types.Log(
                    "warn",
                    "retry_deferred_dispatch_unavailable",
                    [#("issue_id", issue_id)],
                  ),
                  effects_types.DeferRetryTimer(issue_id, generation, 1000),
                ]),
              )
            True ->
              transition_types.Outcome(
                state: transition_types.State(
                  ..state,
                  retry_refresh_generations: dict.insert(
                    state.retry_refresh_generations,
                    identity,
                    generation,
                  ),
                ),
                effects: list.append(accepted_effects, [
                  effects_types.BeginRetryRefresh(issue_id, generation),
                ]),
              )
          }
        }
      }
  }
}

fn retry_timer_stale(
  state: transition_types.State,
  issue_id: String,
  generation: Int,
  include_generation: Bool,
) -> transition_types.Outcome {
  let fields = case include_generation {
    True -> [
      #("issue_id", issue_id),
      #("generation", int.to_string(generation)),
    ]
    False -> [#("issue_id", issue_id)]
  }
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("info", "retry_timer_stale", fields),
  ])
}

fn retry_dispatch_available(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> Bool {
  !context.operator_paused
  && context.dispatch_enabled
  && slots_remain(state, context)
}

fn handle_retry_refresh_completed(
  state: transition_types.State,
  issue_id: String,
  generation: Int,
  result: Result(List(tracker_issue.Issue), String),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      context.tracker_backend_kind,
    )
  let finish_effect = effects_types.FinishRetryRefresh(issue_id)
  let state =
    transition_types.State(
      ..state,
      retry_refresh_generations: dict.delete(
        state.retry_refresh_generations,
        identity,
      ),
    )
  let outcome = case dict.get(state.runtime.retry_attempts, identity) {
    Error(Nil) -> retry_timer_stale(state, issue_id, generation, False)
    Ok(entry) ->
      case entry.timer_generation != generation {
        True -> retry_timer_stale(state, issue_id, generation, True)
        False ->
          case context.dispatch_enabled {
            False ->
              transition_types.Outcome(state: state, effects: [
                effects_types.Log(
                  "warn",
                  "retry_deferred_dispatch_unavailable",
                  [#("issue_id", issue_id)],
                ),
                effects_types.DeferRetryTimer(issue_id, generation, 1000),
              ])
            True ->
              handle_retry_candidate_after_refresh(
                state,
                issue_id,
                retry_candidate_result(result),
                context,
              )
          }
      }
  }
  transition_types.Outcome(state: outcome.state, effects: [
    finish_effect,
    ..outcome.effects
  ])
}

fn retry_candidate_result(
  result: Result(List(tracker_issue.Issue), String),
) -> Result(Option(tracker_issue.Issue), String) {
  case result {
    Error(err) -> Error(err)
    Ok([issue]) -> Ok(Some(issue))
    Ok(_) -> Ok(None)
  }
}

fn handle_retry_candidate_after_refresh(
  state: transition_types.State,
  issue_id: String,
  candidate: Result(Option(tracker_issue.Issue), String),
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  case candidate {
    Error(reason) -> {
      let outcome =
        schedule_retry_with_backoff(
          state,
          context,
          issue_id,
          orchestrator_reason.RetryPollFailed,
        )
      transition_types.Outcome(state: outcome.state, effects: [
        effects_types.Log("warn", "retry_refresh_failed", [
          #("issue_id", issue_id),
          #("error", reason),
        ]),
        ..outcome.effects
      ])
    }
    Ok(None) ->
      release_retry_claim(state, issue_id, "retry_issue_missing", context)
    Ok(Some(issue)) ->
      handle_retry_issue_candidate(state, issue_id, issue, context)
  }
}

fn handle_retry_issue_candidate(
  state: transition_types.State,
  issue_id: String,
  issue: tracker_issue.Issue,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let state =
    transition_types.State(
      ..state,
      runtime: core.unpark_if_issue_changed(state.runtime, issue),
    )
  let decision = core.blocker_decision(context.effective, issue)
  case decision {
    core.BlockedByDependency(_, _) -> {
      let reported =
        report_blocked_dependency(
          state,
          context,
          issue,
          "retry",
          "linear_dependency_retry_blocked",
          decision,
        )
      let stopped =
        stop_retry_for_dependency_blocked(reported.state, issue_id, context)
      transition_types.Outcome(
        state: stopped.state,
        effects: list.append(
          reported.effects,
          list.append(stopped.effects, [effects_types.ClearRecovery(issue_id)]),
        ),
      )
    }
    core.BlockersSatisfied -> {
      let state =
        transition_types.State(
          ..state,
          runtime: core.clear_blocked_dependency_report(
            state.runtime,
            issue.id,
            "retry",
          ),
        )
      case
        core.retry_candidate_precondition_failure(
          state.runtime,
          context.effective,
          issue_id,
          issue,
        )
      {
        Some(reason) -> release_retry_claim(state, issue_id, reason, context)
        None ->
          case
            workflow_policy.classify_issue(
              context.effective.linear_contract,
              issue,
            )
          {
            workflow_policy.WorkflowInvalid(violation) -> {
              let stopped =
                stop_retry_for_policy_invalid(state, issue_id, context)
              let reported =
                report_invalid_workflow_candidate(
                  stopped.state,
                  context,
                  issue,
                  violation,
                )
              transition_types.Outcome(
                state: reported.state,
                effects: list.append(stopped.effects, reported.effects),
              )
            }
            workflow_policy.WorkflowPolicyDisabled
            | workflow_policy.WorkflowSelected(_, _) ->
              case can_reserve_dispatch_slot(state, context, issue) {
                False ->
                  schedule_retry_with_backoff(
                    state,
                    context,
                    issue_id,
                    orchestrator_reason.RetryNoSlots,
                  )
                True -> dispatch_retry_claim(state, issue_id, issue, context)
              }
          }
      }
    }
  }
}

fn release_retry_claim(
  state: transition_types.State,
  issue_id: String,
  cancel_reason: String,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let generation =
    current_retry_generation(
      state.runtime,
      issue_id,
      context.tracker_backend_kind,
    )
  let previous_retry =
    current_retry_entry(state.runtime, issue_id, context.tracker_backend_kind)
    |> option.from_result
  transition_types.Outcome(
    state: clear_retry_refresh_generation(
      transition_types.State(
        ..state,
        runtime: release_claim(
          clear_retry(state.runtime, issue_id, context.tracker_backend_kind),
          issue_id,
          context.tracker_backend_kind,
        ),
      ),
      issue_id,
      context.tracker_backend_kind,
    ),
    effects: list.append(
      cancel_retry_effects(issue_id, generation, cancel_reason, previous_retry),
      [effects_types.ReleaseClaim(issue_id)],
    ),
  )
}

fn dispatch_retry_claim(
  state: transition_types.State,
  issue_id: String,
  issue: tracker_issue.Issue,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  let retry_identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      context.tracker_backend_kind,
    )
  case dict.get(state.runtime.retry_attempts, retry_identity) {
    Error(Nil) -> retry_timer_stale(state, issue_id, 0, False)
    Ok(previous_retry) -> {
      let generation = previous_retry.timer_generation
      let retry_cancellation =
        transition_types.RetryCancellation(
          issue_id: issue_id,
          generation: generation,
          reason: "retry_dispatch",
          previous_retry: previous_retry,
        )
      let state =
        clear_retry_refresh_generation(
          transition_types.State(
            ..state,
            runtime: clear_retry(
              state.runtime,
              issue_id,
              context.tracker_backend_kind,
            ),
          ),
          issue_id,
          context.tracker_backend_kind,
        )
      claims.begin_for_issue_after_retry(
        state,
        issue,
        [],
        context,
        claim_callbacks(),
        generation,
        retry_cancellation,
      )
    }
  }
}

fn current_retry_generation(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  backend_kind: String,
) -> Int {
  case current_retry_entry(runtime, issue_id, backend_kind) {
    Ok(entry) -> entry.timer_generation
    Error(Nil) -> 0
  }
}

fn current_retry_entry(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  backend_kind: String,
) -> Result(orchestrator_state.RetryEntry, Nil) {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(issue_id, backend_kind)
  dict.get(runtime.retry_attempts, identity)
}

fn schedule_retry_with_backoff(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue_id: String,
  retry_reason: orchestrator_reason.RetryReason,
) -> transition_types.Outcome {
  let retry_identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      context.tracker_backend_kind,
    )
  let retry_entry = dict.get(state.runtime.retry_attempts, retry_identity)
  let task_ref = case retry_entry {
    Ok(entry) -> entry.task_ref
    Error(Nil) ->
      orchestrator_state.issue_id_ref_for_backend(
        issue_id,
        context.tracker_backend_kind,
      )
  }
  let attempt = case retry_entry {
    Ok(entry) -> entry.timer_generation + 1
    Error(Nil) -> 1
  }
  let delay_ms = core.backoff_delay(attempt, core.default_max_backoff_ms())
  let core.Transition(state: runtime, effects: core_effects) =
    core.schedule_task_retry(
      state.runtime,
      task_ref,
      issue_id,
      delay_ms,
      retry_reason,
    )
  map_lifecycle_core_effects(
    clear_retry_refresh_generation(
      transition_types.State(..state, runtime: runtime),
      issue_id,
      context.tracker_backend_kind,
    ),
    core_effects,
    None,
    Some(task_ref),
  )
}

fn stop_retry_for_policy_invalid(
  state: transition_types.State,
  issue_id: String,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  stop_retry_for_issue(
    state,
    issue_id,
    context.tracker_backend_kind,
    "policy_invalid",
  )
}

fn stop_retry_for_dependency_blocked(
  state: transition_types.State,
  issue_id: String,
  context: transition_types.DispatchContext,
) -> transition_types.Outcome {
  stop_retry_for_issue(
    state,
    issue_id,
    context.tracker_backend_kind,
    "linear_dependency_blocked",
  )
}

fn stop_retry_for_issue(
  state: transition_types.State,
  issue_id: String,
  backend_kind: String,
  cancel_reason: String,
) -> transition_types.Outcome {
  let generation =
    current_retry_generation(state.runtime, issue_id, backend_kind)
  let runtime =
    release_claim(
      clear_retry(state.runtime, issue_id, backend_kind),
      issue_id,
      backend_kind,
    )
  let previous_retry =
    current_retry_entry(state.runtime, issue_id, backend_kind)
    |> option.from_result
  transition_types.Outcome(
    state: clear_retry_refresh_generation(
      transition_types.State(..state, runtime: runtime),
      issue_id,
      backend_kind,
    ),
    effects: list.append(
      cancel_retry_effects(issue_id, generation, cancel_reason, previous_retry),
      [
        effects_types.ReleaseClaim(issue_id),
      ],
    ),
  )
}

fn map_core_effects(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  effects: List(core.Effect),
) -> transition_types.Outcome {
  list.fold(
    effects,
    transition_types.Outcome(state: state, effects: []),
    fn(outcome, effect) {
      let transition_types.Outcome(state: state, effects: mapped) =
        map_core_effect(outcome.state, context, effect)
      transition_types.Outcome(
        state: state,
        effects: list.append(outcome.effects, mapped),
      )
    },
  )
}

fn map_lifecycle_core_effects(
  state: transition_types.State,
  effects: List(core.Effect),
  source_run_id: Option(String),
  source_task_ref: Option(task.TaskRef),
) -> transition_types.Outcome {
  list.fold(
    effects,
    transition_types.Outcome(state: state, effects: []),
    fn(outcome, effect) {
      let transition_types.Outcome(state: state, effects: mapped) =
        map_lifecycle_core_effect(
          outcome.state,
          effect,
          source_run_id,
          source_task_ref,
        )
      transition_types.Outcome(
        state: state,
        effects: list.append(outcome.effects, mapped),
      )
    },
  )
}

fn map_lifecycle_core_effect(
  state: transition_types.State,
  effect: core.Effect,
  source_run_id: Option(String),
  source_task_ref: Option(task.TaskRef),
) -> transition_types.Outcome {
  case effect {
    core.ScheduleRetry(
      issue_id,
      delay_ms,
      generation,
      retry_reason,
      previous_retry,
    ) ->
      transition_types.Outcome(
        state: state,
        effects: schedule_retry_effects_for_ref(
          state.runtime,
          issue_id,
          delay_ms,
          generation,
          retry_reason,
          previous_retry,
          source_task_ref,
        ),
      )
    core.CancelRetry(issue_id, generation, cancel_reason, previous_retry) ->
      transition_types.Outcome(
        state: state,
        effects: cancel_retry_effects(
          issue_id,
          generation,
          cancel_reason,
          previous_retry,
        ),
      )
    core.ReleaseClaim(issue_id) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ReleaseClaim(issue_id),
      ])
    core.CleanupWorkspace(workspace_path) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.CleanupWorkspace(workspace_path),
      ])
    core.ParkIssue(issue_id, _) ->
      transition_types.Outcome(
        state: state,
        effects: park_issue_effects_for_ref(
          state.runtime,
          issue_id,
          source_task_ref,
          source_run_id,
        ),
      )
    core.StopWorker(issue_id, reason) ->
      stop_worker_after_issue_refresh(state, issue_id, reason)
    core.Dispatch(issue) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "transition_unhandled_lifecycle_dispatch", [
          #("issue_id", issue.id),
        ]),
      ])
  }
}

fn map_core_effect(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  effect: core.Effect,
) -> transition_types.Outcome {
  case effect {
    core.Dispatch(issue) ->
      claims.begin_for_issue(state, issue, [], context, claim_callbacks())
    core.ScheduleRetry(
      issue_id,
      delay_ms,
      generation,
      retry_reason,
      previous_retry,
    ) ->
      transition_types.Outcome(
        state: state,
        effects: schedule_retry_effects(
          state.runtime,
          issue_id,
          delay_ms,
          generation,
          retry_reason,
          previous_retry,
        ),
      )
    core.CancelRetry(issue_id, generation, cancel_reason, previous_retry) ->
      transition_types.Outcome(
        state: state,
        effects: cancel_retry_effects(
          issue_id,
          generation,
          cancel_reason,
          previous_retry,
        ),
      )
    core.ReleaseClaim(issue_id) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ReleaseClaim(issue_id),
      ])
    core.CleanupWorkspace(workspace_path) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.CleanupWorkspace(workspace_path),
      ])
    core.StopWorker(issue_id, reason) ->
      stop_worker_after_issue_refresh(state, issue_id, reason)
    core.ParkIssue(issue_id, _) ->
      transition_types.Outcome(
        state: state,
        effects: park_issue_effects(state.runtime, issue_id, None),
      )
  }
}

fn stop_worker_after_issue_refresh(
  state: transition_types.State,
  issue_id: String,
  reason: orchestrator_reason.StopReason,
) -> transition_types.Outcome {
  case worker_entry_for_refreshed_issue(state, issue_id) {
    Error(Nil) -> transition_types.Outcome(state: state, effects: [])
    Ok(entry) -> {
      let identity = worker_identity(entry)
      let state = remove_worker_from_directory(state, entry)
      transition_types.Outcome(
        state: remove_yaml_step_runs_for_run(state, entry.run_id),
        effects: [effects_types.StopWorkerAfterIssueRefresh(identity, reason)],
      )
    }
  }
}

fn worker_entry_for_refreshed_issue(
  state: transition_types.State,
  issue_id: String,
) -> Result(transition_types.WorkerEntry, Nil) {
  case
    dict.get(
      state.workers.by_issue,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(entry) -> Ok(entry)
    Error(Nil) ->
      state.workers.by_issue
      |> dict.values
      |> list.find(fn(entry) { entry.issue_id == issue_id })
  }
}

fn schedule_retry_effects(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  delay_ms: Int,
  generation: Int,
  retry_reason: orchestrator_reason.RetryReason,
  previous_retry: Option(orchestrator_state.RetryEntry),
) -> List(effects_types.Effect) {
  schedule_retry_effects_for_ref(
    runtime,
    issue_id,
    delay_ms,
    generation,
    retry_reason,
    previous_retry,
    None,
  )
}

fn schedule_retry_effects_for_ref(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  delay_ms: Int,
  generation: Int,
  retry_reason: orchestrator_reason.RetryReason,
  previous_retry: Option(orchestrator_state.RetryEntry),
  ref: Option(task.TaskRef),
) -> List(effects_types.Effect) {
  let reason_text = orchestrator_reason.retry_to_string(retry_reason)
  [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "retry_schedule:"
        <> issue_id
        <> ":"
        <> int.to_string(generation),
      batch: ledger_batch.retry_scheduled(
        issue_id,
        identifier_for_runtime_ref(runtime, issue_id, ref),
        delay_ms,
        generation,
        reason_text,
      ),
      failure_event: "ledger_append_failed",
      policy: effects_types.ScheduleRetryTimerAfterAppend(
        issue_id,
        delay_ms,
        generation,
        retry_reason,
        previous_retry,
      ),
    )),
  ]
}

fn cancel_retry_effects(
  issue_id: String,
  generation: Int,
  cancel_reason: String,
  previous_retry: Option(orchestrator_state.RetryEntry),
) -> List(effects_types.Effect) {
  [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "retry_cancel:"
        <> issue_id
        <> ":"
        <> int.to_string(generation),
      batch: ledger_batch.retry_cancelled(issue_id, generation, cancel_reason),
      failure_event: "ledger_append_failed",
      policy: effects_types.CancelRetryTimerAfterAppend(
        issue_id,
        generation,
        cancel_reason,
        previous_retry,
      ),
    )),
  ]
}

fn handle_ledger_append_completed(
  state: transition_types.State,
  correlation_id: String,
  continuation: effects_types.LedgerPolicy,
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case continuation {
    effects_types.ScheduleRetryTimerAfterAppend(
      issue_id,
      delay_ms,
      generation,
      retry_reason,
      previous_retry,
    ) ->
      handle_retry_schedule_append_completed(
        state,
        correlation_id,
        issue_id,
        delay_ms,
        generation,
        retry_reason,
        previous_retry,
        result,
      )
    effects_types.CancelRetryTimerAfterAppend(
      issue_id,
      generation,
      cancel_reason,
      previous_retry,
    ) ->
      handle_retry_cancel_append_completed(
        state,
        correlation_id,
        issue_id,
        generation,
        cancel_reason,
        previous_retry,
        result,
      )
    effects_types.SpawnClaimedWorkerAfterAppend(
      task_identity,
      issue_id,
      run_id,
      session_id,
    ) ->
      claims.handle_spawn(
        state,
        correlation_id,
        task_identity,
        issue_id,
        run_id,
        session_id,
        result,
        claim_callbacks(),
      )
    effects_types.ReportParkAfterAppend(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      source_run_id,
    ) ->
      commands.handle_operator_report_park_continuation(
        state,
        correlation_id,
        issue_id,
        issue_identifier,
        reason,
        release_policy,
        source_run_id,
        result,
      )
    effects_types.ContinueRegardless | effects_types.StopBatchOnFailure ->
      transition_types.Outcome(state: state, effects: [])
  }
}

fn handle_retry_schedule_append_completed(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  delay_ms: Int,
  generation: Int,
  retry_reason: orchestrator_reason.RetryReason,
  previous_retry: Option(orchestrator_state.RetryEntry),
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Ok(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ScheduleRetryTimer(
          issue_id,
          delay_ms,
          generation,
          retry_reason,
        ),
      ])
    Error(err) ->
      retry_append_failed_outcome(
        state,
        correlation_id,
        issue_id,
        generation,
        previous_retry,
        err,
      )
  }
}

fn handle_retry_cancel_append_completed(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  generation: Int,
  cancel_reason: String,
  previous_retry: Option(orchestrator_state.RetryEntry),
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Ok(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.CancelRetryTimer(issue_id, generation, cancel_reason),
      ])
    Error(err) ->
      retry_append_failed_outcome(
        state,
        correlation_id,
        issue_id,
        generation,
        previous_retry,
        err,
      )
  }
}

fn retry_append_failed_outcome(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  generation: Int,
  previous_retry: Option(orchestrator_state.RetryEntry),
  err: ledger.LedgerError,
) -> transition_types.Outcome {
  let effects = [
    effects_types.Log("warn", "ledger_append_failed", [
      #("issue_id", issue_id),
      #("generation", int.to_string(generation)),
      #("correlation_id", correlation_id),
      #("error", ledger.ledger_error_code(err)),
    ]),
  ]
  case previous_retry {
    Some(previous_retry) -> {
      let identity =
        orchestrator_state.task_ref_identity(previous_retry.task_ref)
      let runtime =
        orchestrator_state.RuntimeState(
          ..state.runtime,
          retry_attempts: dict.insert(
            state.runtime.retry_attempts,
            identity,
            previous_retry,
          ),
          claimed: dict.insert(
            state.runtime.claimed,
            identity,
            retry_entry_identifier(previous_retry),
          ),
        )
      transition_types.Outcome(
        state: transition_types.State(..state, runtime: runtime),
        effects: list.append(effects, [
          effects_types.DeferRetryTimer(
            issue_id,
            previous_retry.timer_generation,
            previous_retry.delay_ms,
          ),
        ]),
      )
    }
    None -> {
      let runtime =
        remove_failed_retry_generation(state.runtime, issue_id, generation)
      transition_types.Outcome(
        state: transition_types.State(..state, runtime: runtime),
        effects: effects,
      )
    }
  }
}

fn remove_failed_retry_generation(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  generation: Int,
) -> orchestrator_state.RuntimeState {
  list.fold(dict.to_list(runtime.retry_attempts), runtime, fn(runtime, entry) {
    let #(task_identity, retry) = entry
    case retry.issue_id == issue_id && retry.timer_generation == generation {
      False -> runtime
      True ->
        orchestrator_state.RuntimeState(
          ..runtime,
          retry_attempts: dict.delete(runtime.retry_attempts, task_identity),
          claimed: dict.delete(runtime.claimed, task_identity),
        )
    }
  })
}

fn retry_entry_identifier(retry: orchestrator_state.RetryEntry) -> String {
  case retry.task_ref.key {
    Some(identifier) -> identifier
    None -> retry.issue_id
  }
}

fn handle_worker_start_succeeded(
  state: transition_types.State,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  session_id: identity.SessionId,
) -> transition_types.Outcome {
  let issue_id_text = identity.issue_id_to_string(issue_id)
  let run_id_text = identity.run_id_to_string(run_id)
  let session_id_text = identity.session_id_to_string(session_id)
  case worker_entry_for_issue(state, issue_id_text, run_id_text) {
    Error(Nil) ->
      stale_worker_lifecycle(
        state,
        "worker_start_stale",
        issue_id_text,
        run_id_text,
      )
    Ok(entry) ->
      case entry.session_id == session_id_text {
        False ->
          stale_worker_lifecycle(
            state,
            "worker_start_stale",
            issue_id_text,
            run_id_text,
          )
        True -> {
          let entry =
            transition_types.WorkerEntry(
              ..entry,
              status: transition_types.WorkerRunning,
            )
          let workers =
            transition_types.WorkerDirectory(
              ..state.workers,
              by_issue: dict.insert(
                state.workers.by_issue,
                entry_identity(entry),
                entry,
              ),
            )
          transition_types.Outcome(
            state: transition_types.State(..state, workers: workers),
            effects: [],
          )
        }
      }
  }
}

fn handle_worker_start_failed(
  state: transition_types.State,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  session_id: identity.SessionId,
  reason: String,
) -> transition_types.Outcome {
  let issue_id_text = identity.issue_id_to_string(issue_id)
  let run_id_text = identity.run_id_to_string(run_id)
  let session_id_text = identity.session_id_to_string(session_id)
  case worker_entry_for_issue(state, issue_id_text, run_id_text) {
    Error(Nil) ->
      stale_worker_lifecycle(
        state,
        "worker_start_failed_stale",
        issue_id_text,
        run_id_text,
      )
    Ok(entry) ->
      case entry.session_id == session_id_text {
        False ->
          stale_worker_lifecycle(
            state,
            "worker_start_failed_stale",
            issue_id_text,
            run_id_text,
          )
        True -> {
          let state = remove_worker_from_directory(state, entry)
          let identity = entry_identity(entry)
          let runtime =
            orchestrator_state.RuntimeState(
              ..state.runtime,
              running: dict.delete(state.runtime.running, identity),
              claimed: dict.delete(state.runtime.claimed, identity),
            )
          transition_types.Outcome(
            state: transition_types.State(..state, runtime: runtime),
            effects: [
              effects_types.WorkerStartFailed(
                worker_start_from_entry(entry),
                reason,
              ),
              effects_types.Log("warn", "worker_start_failed", [
                #("issue_id", issue_id_text),
                #("run_id", run_id_text),
                #("reason", reason),
              ]),
            ],
          )
        }
      }
  }
}

fn handle_worker_command_ready(
  state: transition_types.State,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
) -> transition_types.Outcome {
  let issue_id_text = identity.issue_id_to_string(issue_id)
  let run_id_text = identity.run_id_to_string(run_id)
  case worker_entry_for_issue(state, issue_id_text, run_id_text) {
    Error(Nil) ->
      stale_worker_lifecycle(
        state,
        "worker_command_ready_stale",
        issue_id_text,
        run_id_text,
      )
    Ok(entry) -> {
      let entry =
        transition_types.WorkerEntry(
          ..entry,
          status: transition_types.WorkerRunning,
        )
      let workers =
        transition_types.WorkerDirectory(
          ..state.workers,
          by_issue: dict.insert(
            state.workers.by_issue,
            entry_identity(entry),
            entry,
          ),
        )
      transition_types.Outcome(
        state: transition_types.State(..state, workers: workers),
        effects: [],
      )
    }
  }
}

fn handle_worker_finished(
  state: transition_types.State,
  issue_id: identity.IssueId,
  run_id: identity.RunId,
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  handle_worker_finished_result(
    state,
    identity.issue_id_to_string(issue_id),
    identity.run_id_to_string(run_id),
    worker_finish_result(result, context.secrets),
    context,
  )
}

fn handle_worker_finished_result(
  state: transition_types.State,
  issue_id: String,
  run_id: String,
  result: transition_types.WorkerFinishResult,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  case worker_entry_for_issue(state, issue_id, run_id) {
    Error(Nil) ->
      stale_worker_lifecycle(state, "worker_finished_stale", issue_id, run_id)
    Ok(entry) -> {
      let identity = worker_identity(entry)
      let state = remove_worker_from_directory(state, entry)
      let state = remove_yaml_step_runs_for_run(state, entry.run_id)
      let remove_effect = effects_types.RemoveWorker(identity, True)
      let transition_types.Outcome(state: state, effects: effects) =
        finish_worker_entry(state, entry, identity, result, context)
      transition_types.Outcome(state: state, effects: [remove_effect, ..effects])
    }
  }
}

fn worker_finish_result(
  result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
  secrets: List(String),
) -> transition_types.WorkerFinishResult {
  case result {
    Ok(success) -> transition_types.WorkerSucceeded(success)
    Error(failure) ->
      transition_types.WorkerFailed(
        failure,
        worker_failure_kind(failure, secrets),
      )
  }
}

fn worker_failure_kind(
  failure: agent_types.WorkerFailure,
  secrets: List(String),
) -> transition_types.WorkerFailureKind {
  case is_recovery_resume_validation_worker_failure(failure) {
    True ->
      transition_types.RecoveryResumeValidationFailure(
        workflow_attempt.recovery_pi_resume_validation_failed,
      )
    False ->
      case failure.reason {
        error.OperatorAbort ->
          transition_types.OperatorWorkerFailure(session_reason.OperatorAbort)
        error.OperatorStopAfterCurrentTurn ->
          transition_types.OperatorWorkerFailure(
            session_reason.OperatorStopAfterCurrentTurn,
          )
        _ ->
          transition_types.StandardWorkerFailure(worker_failure_message(
            failure,
            secrets,
          ))
      }
  }
}

fn worker_failure_message(
  failure: agent_types.WorkerFailure,
  secrets: List(String),
) -> String {
  let code = error.agent_code(failure.reason)
  case failure.reason {
    error.PiFailed(error.PiProtocolError(reason)) ->
      code <> ":pi_protocol_error:" <> log.redact("failure", reason, secrets)
    error.PiFailed(pi_error) -> code <> ":" <> error.pi_rpc_code(pi_error)
    error.ContextRecoveryExhausted(
      recovery_method: recovery_method,
      final_error: final_error,
      ..,
    ) ->
      code
      <> ":context_recovery_exhausted:"
      <> recovery_method
      <> ":"
      <> error.pi_rpc_code(final_error)
    error.WorkflowCommandFailed(step_id: step_id, detail: detail, ..) ->
      code
      <> ":workflow_command_failed:"
      <> step_id
      <> ":"
      <> log.redact("failure", detail, secrets)
    error.ProbeFailed(pi_error) -> code <> ":" <> error.pi_rpc_code(pi_error)
    error.PromptFailed(template_error) ->
      code <> ":" <> error.template_code(template_error)
    error.WorkspaceFailed(workspace_error) ->
      code <> ":" <> error.workspace_code(workspace_error)
    error.HookFailedError(hook_error) ->
      code <> ":" <> hook_failure_message(hook_error, secrets)
    error.WorkflowHookFailed(hook_error) ->
      code <> ":" <> hook_failure_message(hook_error, secrets)
    error.StateRefreshFailed(tracker_error) ->
      code <> ":" <> error.tracker_code(tracker_error)
    error.OperatorAbort | error.OperatorStopAfterCurrentTurn -> code
  }
}

fn hook_failure_message(
  hook_error: error.HookError,
  secrets: List(String),
) -> String {
  let detail = case hook_error {
    error.HookFailed(command, status, output) ->
      error.hook_code(hook_error)
      <> ":"
      <> command
      <> " exited "
      <> int.to_string(status)
      <> ": "
      <> output
    error.HookTimedOut(command) ->
      error.hook_code(hook_error) <> ":" <> command <> " timed out"
    error.HookIo(message) -> error.hook_code(hook_error) <> ":" <> message
  }
  log.redact("failure", detail, secrets)
  |> log.truncate(4000)
}

fn is_recovery_resume_validation_worker_failure(
  failure: agent_types.WorkerFailure,
) -> Bool {
  case failure.reason {
    error.PiFailed(error.PiProtocolError(reason)) ->
      reason == workflow_attempt.recovery_pi_resume_validation_failed
    _ -> False
  }
}

fn finish_worker_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  identity: effects_types.WorkerIdentity,
  result: transition_types.WorkerFinishResult,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  case result {
    transition_types.WorkerSucceeded(success) ->
      finish_worker_success_entry(state, entry, identity, success, context)
    transition_types.WorkerFailed(failure, kind) ->
      finish_worker_failure_entry(
        state,
        entry,
        identity,
        failure,
        kind,
        context,
      )
  }
}

fn finish_worker_success_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  identity: effects_types.WorkerIdentity,
  success: agent_types.WorkerSuccess,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let final_issue = case success.final_issue {
    Some(issue) -> issue
    None -> entry.issue
  }
  let core.Transition(state: runtime, effects: core_effects) =
    core.apply_task_workflow_success(
      state.runtime,
      context.effective,
      entry.task_ref,
      entry.issue_id,
      final_issue,
      success.tokens,
      context.now_ms,
      core.AlreadyCleaned,
    )
  let state = transition_types.State(..state, runtime: runtime)
  let batch =
    ledger_batch.worker_succeeded(counter_record_for_entry(
      runtime,
      entry,
      final_issue.identifier,
      Some(entry.run_id),
      context.now_ms,
    ))
  let transition_types.Outcome(state: state, effects: follow_ups) =
    map_lifecycle_core_effects(
      state,
      core_effects,
      Some(entry.run_id),
      Some(entry.task_ref),
    )
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("info", "worker_exited", [
      #("issue_id", entry.issue_id),
      #("run_id", entry.run_id),
      #("reason", "normal"),
    ]),
    effects_types.PublishWorkerExited(effects_types.WorkerExitPublication(
      identity: identity,
      reason_text: "normal",
      exit_reason: session_reason.Normal,
      tokens: success.tokens,
      update_tokens: True,
    )),
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "worker_finish:" <> entry.issue_id <> ":" <> entry.run_id,
      batch: batch,
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
    effects_types.ReportWorkerSuccess(identity, success),
    ..follow_ups
  ])
}

fn finish_worker_failure_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  identity: effects_types.WorkerIdentity,
  failure: agent_types.WorkerFailure,
  kind: transition_types.WorkerFailureKind,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  case kind {
    transition_types.StandardWorkerFailure(reason_text) ->
      finish_standard_worker_failure_entry(
        state,
        entry,
        identity,
        failure,
        reason_text,
        context,
      )
    transition_types.WorkerDownFailure ->
      finish_standard_worker_failure_entry(
        state,
        entry,
        identity,
        failure,
        "worker_down",
        context,
      )
    transition_types.RecoveryResumeValidationFailure(reason_text) ->
      finish_recovery_validation_failure_entry(
        state,
        entry,
        identity,
        failure,
        reason_text,
        context,
      )
    transition_types.OperatorWorkerFailure(reason) ->
      finish_operator_worker_failure_entry(
        state,
        entry,
        identity,
        failure,
        reason,
        context,
      )
  }
}

fn finish_standard_worker_failure_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  identity: effects_types.WorkerIdentity,
  failure: agent_types.WorkerFailure,
  reason_text: String,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let baseline_issue = baseline_issue_for_failure(entry, failure)
  let core.Transition(state: runtime, effects: core_effects) =
    core.apply_task_worker_failure(
      state.runtime,
      context.effective,
      entry.task_ref,
      entry.issue_id,
      baseline_issue,
      context.now_ms,
    )
  let state = transition_types.State(..state, runtime: runtime)
  let batch =
    ledger_batch.worker_failed(counter_record_for_entry(
      runtime,
      entry,
      baseline_issue.identifier,
      Some(entry.run_id),
      context.now_ms,
    ))
  let transition_types.Outcome(state: state, effects: follow_ups) =
    map_lifecycle_core_effects(
      state,
      core_effects,
      Some(entry.run_id),
      Some(entry.task_ref),
    )
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", "worker_exited", [
      #("issue_id", entry.issue_id),
      #("run_id", entry.run_id),
      #("reason", reason_text),
    ]),
    effects_types.PublishWorkerExited(effects_types.WorkerExitPublication(
      identity: identity,
      reason_text: reason_text,
      exit_reason: session_reason.Failed,
      tokens: failure.tokens,
      update_tokens: False,
    )),
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "worker_failure:" <> entry.issue_id <> ":" <> entry.run_id,
      batch: batch,
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
    effects_types.ReportWorkerFailure(identity, failure),
    ..follow_ups
  ])
}

fn finish_recovery_validation_failure_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  identity: effects_types.WorkerIdentity,
  failure: agent_types.WorkerFailure,
  reason_text: String,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let baseline_issue = baseline_issue_for_failure(entry, failure)
  let runtime_identity = entry_identity(entry)
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, runtime_identity),
      retry_attempts: dict.delete(
        state.runtime.retry_attempts,
        runtime_identity,
      ),
    )
  let state = transition_types.State(..state, runtime: runtime)
  let state =
    park_runtime(
      state,
      entry.task_ref,
      baseline_issue,
      orchestrator_reason.ParkOperator(reason_text),
      context.now_ms,
    )
  let park_effects =
    park_task_ref_effects(state.runtime, entry.task_ref, Some(entry.run_id))
  transition_types.Outcome(
    state: state,
    effects: list.append(
      [
        effects_types.Log("warn", "worker_exited", [
          #("issue_id", entry.issue_id),
          #("run_id", entry.run_id),
          #("reason", reason_text),
        ]),
        effects_types.PublishWorkerExited(effects_types.WorkerExitPublication(
          identity: identity,
          reason_text: reason_text,
          exit_reason: session_reason.Failed,
          tokens: failure.tokens,
          update_tokens: False,
        )),
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "worker_failure:"
            <> entry.issue_id
            <> ":"
            <> entry.run_id,
          batch: ledger_batch.worker_failed(counter_record_for_entry(
            runtime,
            entry,
            baseline_issue.identifier,
            Some(entry.run_id),
            context.now_ms,
          )),
          failure_event: "ledger_append_failed",
          policy: effects_types.ContinueRegardless,
        )),
      ],
      park_effects,
    ),
  )
}

fn finish_operator_worker_failure_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  identity: effects_types.WorkerIdentity,
  failure: agent_types.WorkerFailure,
  reason: session_reason.WorkerExitReason,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let reason_text = session_reason.to_string(reason)
  let final_issue = baseline_issue_for_failure(entry, failure)
  let runtime_identity = entry_identity(entry)
  let state =
    claims.complete_runtime_failure(
      state,
      runtime_identity,
      final_issue,
      failure.tokens,
      context.now_ms,
    )
    |> park_runtime(
      entry.task_ref,
      final_issue,
      orchestrator_reason.ParkOperator(reason_text),
      context.now_ms,
    )
  let park_effects =
    park_task_ref_effects(state.runtime, entry.task_ref, Some(entry.run_id))
  transition_types.Outcome(
    state: state,
    effects: list.append(
      [
        effects_types.Log("warn", "worker_exited", [
          #("issue_id", entry.issue_id),
          #("run_id", entry.run_id),
          #("reason", reason_text),
        ]),
        effects_types.PublishWorkerExited(effects_types.WorkerExitPublication(
          identity: identity,
          reason_text: reason_text,
          exit_reason: reason,
          tokens: failure.tokens,
          update_tokens: True,
        )),
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "workflow_cancelled:"
            <> entry.issue_id
            <> ":"
            <> entry.run_id,
          batch: ledger_batch.workflow_cancelled(
            #(entry.run_id, entry.workflow_id, entry.task_ref),
            failure.tokens.total,
          ),
          failure_event: "workflow_terminal_append_failed",
          policy: effects_types.ContinueRegardless,
        )),
      ],
      park_effects,
    ),
  )
}

fn handle_worker_down(
  state: transition_types.State,
  resolution: transition_types.WorkerDownResolution,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  case resolution {
    transition_types.UnknownWorkerDown ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "worker_down_stale", []),
      ])
    transition_types.WorkerDownStale(issue_id) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "worker_down_stale", [
          #("issue_id", identity.issue_id_to_string(issue_id)),
        ]),
      ])
    transition_types.KnownWorkerDown(issue_id, run_id, session_id) -> {
      let issue_id_text = identity.issue_id_to_string(issue_id)
      let run_id_text = identity.run_id_to_string(run_id)
      let session_id_text = identity.session_id_to_string(session_id)
      case worker_entry_for_issue(state, issue_id_text, run_id_text) {
        Ok(entry) ->
          case entry.session_id == session_id_text {
            False ->
              transition_types.Outcome(state: state, effects: [
                effects_types.Log("warn", "worker_down_stale", [
                  #("issue_id", issue_id_text),
                  #("run_id", run_id_text),
                  #("session_id", session_id_text),
                ]),
              ])
            True ->
              finish_known_worker_down(
                state,
                issue_id_text,
                run_id_text,
                context,
              )
          }
        Error(Nil) ->
          finish_known_worker_down(state, issue_id_text, run_id_text, context)
      }
    }
  }
}

fn finish_known_worker_down(
  state: transition_types.State,
  issue_id_text: String,
  run_id_text: String,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let failure = worker_down_failure(state, issue_id_text, run_id_text)
  let result =
    transition_types.WorkerFailed(failure, transition_types.WorkerDownFailure)
  let transition_types.Outcome(state: state, effects: effects) =
    handle_worker_finished_result(
      state,
      issue_id_text,
      run_id_text,
      result,
      context,
    )
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", "worker_down", [#("issue_id", issue_id_text)]),
    ..effects
  ])
}

fn handle_worker_stop_requested(
  state: transition_types.State,
  session_id: identity.SessionId,
  reason: session_reason.WorkerExitReason,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let session_id_text = identity.session_id_to_string(session_id)
  case dict.get(state.workers.by_session, session_id_text) {
    Error(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "worker_stop_stale", [
          #("session_id", session_id_text),
        ]),
      ])
    Ok(issue_id) ->
      case dict.get(state.workers.by_issue, issue_id) {
        Error(Nil) ->
          transition_types.Outcome(state: state, effects: [
            effects_types.Log("warn", "worker_stop_stale", [
              #("session_id", session_id_text),
            ]),
          ])
        Ok(entry) -> stop_worker_entry(state, entry, reason, context)
      }
  }
}

fn stop_worker_entry(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
  reason: session_reason.WorkerExitReason,
  context: transition_types.WorkerLifecycleContext,
) -> transition_types.Outcome {
  let reason_text = session_reason.to_string(reason)
  let state = remove_worker_from_directory(state, entry)
  let state = remove_yaml_step_runs_for_run(state, entry.run_id)
  let state =
    park_runtime(
      state,
      entry.task_ref,
      entry.issue,
      orchestrator_reason.ParkOperator(reason_text),
      context.now_ms,
    )
  let identity = worker_identity(entry)
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "workflow_cancelled:"
        <> entry.issue_id
        <> ":"
        <> entry.run_id,
      batch: ledger_batch.workflow_cancelled(
        #(entry.run_id, entry.workflow_id, entry.task_ref),
        0,
      ),
      failure_event: "workflow_terminal_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
    effects_types.StopWorker(identity, reason),
    effects_types.FinishYamlStepSessionsForRun(
      identity.run_id_from_string(entry.run_id),
      reason,
    ),
    effects_types.ClearYamlStepRoutesForRun(identity.run_id_from_string(
      entry.run_id,
    )),
    effects_types.RemoveWorker(identity, False),
    ..park_task_ref_effects(state.runtime, entry.task_ref, Some(entry.run_id))
  ])
}

fn handle_yaml_step_started(
  state: transition_types.State,
  session_id: identity.SessionId,
  run_id: identity.RunId,
) -> transition_types.Outcome {
  let session_id_text = identity.session_id_to_string(session_id)
  let run_id_text = identity.run_id_to_string(run_id)
  case dict.get(state.workers.stopped_yaml_runs, run_id_text) {
    Ok(reason) ->
      transition_types.Outcome(
        state: clear_stopped_yaml_run(state, run_id_text),
        effects: [effects_types.FinishYamlStepSession(session_id, reason)],
      )
    Error(Nil) ->
      case worker_for_run(state.workers, run_id_text) {
        Error(Nil) -> transition_types.Outcome(state: state, effects: [])
        Ok(_) -> {
          let workers =
            transition_types.WorkerDirectory(
              ..state.workers,
              yaml_step_runs: dict.insert(
                state.workers.yaml_step_runs,
                session_id_text,
                run_id_text,
              ),
            )
          transition_types.Outcome(
            state: transition_types.State(..state, workers: workers),
            effects: [effects_types.RegisterYamlStepStarted(session_id, run_id)],
          )
        }
      }
  }
}

fn handle_yaml_step_finished(
  state: transition_types.State,
  session_id: identity.SessionId,
) -> transition_types.Outcome {
  let session_id_text = identity.session_id_to_string(session_id)
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      yaml_step_runs: dict.delete(state.workers.yaml_step_runs, session_id_text),
    )
  transition_types.Outcome(
    state: transition_types.State(..state, workers: workers),
    effects: [effects_types.FinishYamlStepRoute(session_id)],
  )
}

fn worker_entry_for_issue(
  state: transition_types.State,
  issue_id: String,
  run_id: String,
) -> Result(transition_types.WorkerEntry, Nil) {
  case
    dict.get(
      state.workers.by_issue,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  {
    Ok(entry) ->
      case entry.run_id == run_id {
        True -> Ok(entry)
        False -> worker_entry_for_issue_and_run(state.workers, issue_id, run_id)
      }
    Error(Nil) ->
      worker_entry_for_issue_and_run(state.workers, issue_id, run_id)
  }
}

fn worker_entry_for_issue_and_run(
  workers: transition_types.WorkerDirectory,
  issue_id: String,
  run_id: String,
) -> Result(transition_types.WorkerEntry, Nil) {
  workers.by_issue
  |> dict.values
  |> list.filter(fn(entry) {
    entry.issue_id == issue_id && entry.run_id == run_id
  })
  |> first_worker_entry
}

fn entry_identity(
  entry: transition_types.WorkerEntry,
) -> identity.TaskIdentity {
  orchestrator_state.task_ref_identity(entry.task_ref)
}

fn worker_for_run(
  workers: transition_types.WorkerDirectory,
  run_id: String,
) -> Result(transition_types.WorkerEntry, Nil) {
  workers.by_issue
  |> dict.values
  |> list.filter(fn(entry) { entry.run_id == run_id })
  |> first_worker_entry
}

fn first_worker_entry(
  entries: List(transition_types.WorkerEntry),
) -> Result(transition_types.WorkerEntry, Nil) {
  case entries {
    [] -> Error(Nil)
    [entry, ..] -> Ok(entry)
  }
}

fn worker_identity(
  entry: transition_types.WorkerEntry,
) -> effects_types.WorkerIdentity {
  effects_types.WorkerIdentity(
    task_ref: entry.task_ref,
    issue_id: identity.issue_id_from_string(entry.issue_id),
    run_id: identity.run_id_from_string(entry.run_id),
    session_id: identity.session_id_from_string(entry.session_id),
    issue: entry.issue,
    workspace_path: entry.workspace_path,
    workflow_id: entry.workflow_id,
    command_route_id: entry.command_route_id,
  )
}

fn worker_start_from_entry(
  entry: transition_types.WorkerEntry,
) -> effects_types.WorkerStart {
  effects_types.WorkerStart(
    task_ref: entry.task_ref,
    issue_id: identity.issue_id_from_string(entry.issue_id),
    run_id: identity.run_id_from_string(entry.run_id),
    session_id: identity.session_id_from_string(entry.session_id),
    command_route_id: entry.command_route_id,
    issue: entry.issue,
    workspace_path: entry.workspace_path,
    workflow_id: entry.workflow_id,
    route_label: entry.issue.identifier,
    recovery: entry.recovery,
  )
}

fn remove_worker_from_directory(
  state: transition_types.State,
  entry: transition_types.WorkerEntry,
) -> transition_types.State {
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      by_issue: dict.delete(state.workers.by_issue, entry_identity(entry)),
      by_session: dict.delete(state.workers.by_session, entry.session_id),
      route_to_session: dict.delete(
        state.workers.route_to_session,
        entry.command_route_id,
      ),
    )
  transition_types.State(..state, workers: workers)
}

fn remove_yaml_step_runs_for_run(
  state: transition_types.State,
  run_id: String,
) -> transition_types.State {
  let yaml_step_runs =
    state.workers.yaml_step_runs
    |> dict.to_list
    |> list.filter(fn(entry) {
      let #(_, step_run_id) = entry
      step_run_id != run_id
    })
    |> dict.from_list
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      yaml_step_runs: yaml_step_runs,
      stopped_yaml_runs: dict.delete(state.workers.stopped_yaml_runs, run_id),
    )
  transition_types.State(..state, workers: workers)
}

fn clear_stopped_yaml_run(
  state: transition_types.State,
  run_id: String,
) -> transition_types.State {
  let workers =
    transition_types.WorkerDirectory(
      ..state.workers,
      stopped_yaml_runs: dict.delete(state.workers.stopped_yaml_runs, run_id),
    )
  transition_types.State(..state, workers: workers)
}

fn baseline_issue_for_failure(
  entry: transition_types.WorkerEntry,
  failure: agent_types.WorkerFailure,
) -> tracker_issue.Issue {
  case failure.final_issue {
    Some(issue) ->
      case issue.id == entry.issue_id {
        True -> issue
        False -> entry.issue
      }
    None -> entry.issue
  }
}

fn counter_record_for_entry(
  runtime: orchestrator_state.RuntimeState,
  entry: transition_types.WorkerEntry,
  issue_identifier: String,
  source_run_id: Option(String),
  now_ms: Int,
) -> record.RecordBody {
  counter_record_for_identity(
    runtime,
    entry_identity(entry),
    entry.issue_id,
    issue_identifier,
    source_run_id,
    now_ms,
  )
}

fn counter_record_for_identity(
  runtime: orchestrator_state.RuntimeState,
  identity: identity.TaskIdentity,
  issue_id: String,
  issue_identifier: String,
  source_run_id: Option(String),
  now_ms: Int,
) -> record.RecordBody {
  let counter = case dict.get(runtime.issue_counters, identity) {
    Ok(counter) -> counter
    Error(Nil) -> orchestrator_state.new_issue_counter()
  }
  record.IssueCounterUpdated(
    issue_id,
    issue_identifier,
    counter.failure_attempts,
    counter.worker_sessions,
    now_ms,
    source_run_id,
  )
}

fn park_issue_effects(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  source_run_id: Option(String),
) -> List(effects_types.Effect) {
  park_issue_effects_for_ref(runtime, issue_id, None, source_run_id)
}

fn park_issue_effects_for_ref(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  ref: Option(task.TaskRef),
  source_run_id: Option(String),
) -> List(effects_types.Effect) {
  case ref {
    Some(ref) -> park_task_ref_effects(runtime, ref, source_run_id)
    None ->
      case
        dict.get(
          runtime.parked,
          orchestrator_state.linear_issue_id_identity(issue_id),
        )
      {
        Ok(parked) -> [effects_types.ParkIssue(parked, source_run_id)]
        Error(Nil) -> []
      }
  }
}

fn park_task_ref_effects(
  runtime: orchestrator_state.RuntimeState,
  ref: task.TaskRef,
  source_run_id: Option(String),
) -> List(effects_types.Effect) {
  case dict.get(runtime.parked, orchestrator_state.task_ref_identity(ref)) {
    Ok(parked) -> [effects_types.ParkIssue(parked, source_run_id)]
    Error(Nil) -> []
  }
}

fn park_runtime(
  state: transition_types.State,
  ref: task.TaskRef,
  issue: tracker_issue.Issue,
  reason: orchestrator_reason.ParkReason,
  now_ms: Int,
) -> transition_types.State {
  claims.park_runtime(state, ref, issue, reason, now_ms)
}

fn worker_down_failure(
  state: transition_types.State,
  issue_id: String,
  run_id: String,
) -> agent_types.WorkerFailure {
  let #(workspace_path, final_issue) = case
    worker_entry_for_issue(state, issue_id, run_id)
  {
    Ok(entry) -> #(Some(entry.workspace_path), Some(entry.issue))
    Error(Nil) -> #(None, None)
  }
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError("worker_down")),
    workspace_path: workspace_path,
    tokens: session_tokens.zero_token_totals(),
    final_issue: final_issue,
  )
}

fn stale_worker_lifecycle(
  state: transition_types.State,
  event: String,
  issue_id: String,
  run_id: String,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.Log("warn", event, [
      #("issue_id", issue_id),
      #("run_id", run_id),
    ]),
  ])
}

fn report_blocked_dependency(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  phase: String,
  event: String,
  decision: core.BlockerDecision,
) -> transition_types.Outcome {
  case
    core.already_reported_blocked_dependency(
      state.runtime,
      context.effective,
      issue,
      phase,
      decision,
    )
  {
    True -> transition_types.Outcome(state: state, effects: [])
    False -> {
      let runtime =
        core.mark_blocked_dependency_reported(
          state.runtime,
          context.effective,
          issue,
          phase,
          decision,
          context.now_ms,
        )
      transition_types.Outcome(
        state: transition_types.State(..state, runtime: runtime),
        effects: [
          effects_types.Log("warn", event, [
            #("issue_id", issue.id),
            #("issue_identifier", issue.identifier),
            #("phase", phase),
            #(
              "blocker_fingerprint",
              core.blocked_dependency_fingerprint(
                context.effective,
                issue,
                phase,
                decision,
              ),
            ),
            #("blockers", claims.blocker_summary(issue, decision)),
            #(
              "incomplete",
              claims.bool_field(core.blocker_decision_incomplete(decision)),
            ),
          ]),
        ],
      )
    }
  }
}

fn report_invalid_workflow_candidate(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
) -> transition_types.Outcome {
  case
    core.already_attempted_invalid_workflow(
      state.runtime,
      issue,
      violation,
      context.effective.linear_contract,
    )
  {
    True -> transition_types.Outcome(state: state, effects: [])
    False -> {
      let fingerprint = workflow_policy.violation_fingerprint(violation)
      let reporting_policy_fingerprint =
        workflow_policy.reporting_policy_fingerprint(
          context.effective.linear_contract,
        )
      let runtime =
        core.mark_invalid_workflow_report_pending(
          state.runtime,
          issue,
          violation,
          context.effective.linear_contract,
          context.now_ms,
        )
      transition_types.Outcome(
        state: transition_types.State(..state, runtime: runtime),
        effects: [
          effects_types.Log("warn", "invalid_workflow_candidate", [
            #("issue_id", issue.id),
            #("issue_identifier", issue.identifier),
            #("violation", workflow_policy.violation_code(violation)),
            #("violation_fingerprint", fingerprint),
          ]),
          effects_types.ReportInvalidWorkflow(
            issue,
            violation,
            fingerprint,
            reporting_policy_fingerprint,
          ),
        ],
      )
    }
  }
}

fn append_with_next(
  outcome: transition_types.Outcome,
  next: fn(transition_types.State) -> transition_types.Outcome,
) -> transition_types.Outcome {
  let next_outcome = next(outcome.state)
  transition_types.Outcome(
    state: next_outcome.state,
    effects: list.append(outcome.effects, next_outcome.effects),
  )
}

fn dispatch_preconditions_without_slot(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Bool {
  let identity = orchestrator_state.issue_identity(issue)
  core.dispatch_preconditions_satisfied_without_slot_capacity(
    state.runtime,
    context.effective,
    issue,
  )
  && !claims.has_dispatch_blocker(state, identity)
  && !list.contains(active_issue_ids(state, context), issue.id)
}

fn dispatch_validation_precondition_failure(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Option(String) {
  case
    string.trim(issue.id) == ""
    || string.trim(issue.identifier) == ""
    || string.trim(issue.title) == ""
    || string.trim(issue_state.to_string(issue.state)) == ""
  {
    True -> Some("missing_required_fields")
    False ->
      case core.is_active(context.effective, issue.state) {
        False -> Some("inactive_state")
        True ->
          case core.is_terminal(context.effective, issue.state) {
            True -> Some("terminal_state")
            False ->
              case list.contains(active_issue_ids(state, context), issue.id) {
                True -> Some("already_running")
                False -> {
                  let identity = orchestrator_state.issue_identity(issue)
                  case claims.has_tracker_claim(state, identity) {
                    True -> Some("already_claimed")
                    False ->
                      case
                        core.dispatch_preconditions_satisfied_without_slot_capacity(
                          state.runtime,
                          context.effective,
                          issue,
                        )
                      {
                        True -> None
                        False -> Some("parked")
                      }
                  }
                }
              }
          }
      }
  }
}

fn issue_is_running_claimed_or_pending(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue_id: String,
) -> Bool {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(
      issue_id,
      context.tracker_backend_kind,
    )
  list.contains(active_issue_ids(state, context), issue_id)
  || claims.has_dispatch_blocker(state, identity)
  || claims.has_tracker_claim(state, identity)
}

fn can_reserve_dispatch_slot(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Bool {
  let identity = orchestrator_state.issue_identity(issue)
  !list.contains(active_issue_ids(state, context), issue.id)
  && !claims.has_dispatch_blocker(state, identity)
  && slots_remain(state, context)
  && per_state_dispatch_slot_available(state, context, issue.state)
}

fn slots_remain(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> Bool {
  context.effective.agent.max_concurrent_agents != 0
  && dispatch_slots_used(state, context)
  < context.effective.agent.max_concurrent_agents
}

fn dispatch_slots_used(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> Int {
  list.length(active_issue_ids(state, context))
  + dict.size(state.pending_claims)
  + dict.size(state.pending_dispatch_validations)
  + dict.size(state.pending_review_lane_preflights)
  + context.reserved_non_issue_slots
}

fn per_state_dispatch_slot_available(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  issue_state_value: issue_state.IssueState,
) -> Bool {
  let key = issue_state.key(issue_state_value)
  case dict.get(context.effective.agent.max_concurrent_agents_by_state, key) {
    Error(Nil) -> True
    Ok(limit) -> dispatch_count_for_state(state, context, key) < limit
  }
}

fn dispatch_count_for_state(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  running_count_for_state(state, context, normalized_state)
  + claims.pending_count_for_state(state, normalized_state)
}

fn running_count_for_state(
  state: transition_types.State,
  context: transition_types.DispatchContext,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  active_issues(state, context)
  |> list.filter(fn(issue) { issue_state.key(issue.state) == normalized_state })
  |> list.length
}

fn active_issue_ids(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> List(String) {
  []
  |> append_unique_list(
    state.runtime.running
    |> dict.values
    |> list.map(fn(entry) { entry.issue.id }),
  )
  |> append_unique_list(active_context_issue_ids(state, context))
}

fn active_context_issue_ids(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> List(String) {
  context.active_issue_ids
  |> list.filter(fn(issue_id) {
    dict.has_key(
      state.workers.by_issue,
      orchestrator_state.linear_issue_id_identity(issue_id),
    )
  })
}

fn active_issues(
  state: transition_types.State,
  context: transition_types.DispatchContext,
) -> List(tracker_issue.Issue) {
  []
  |> append_unique_issues(
    state.runtime.running |> dict.values |> list.map(fn(entry) { entry.issue }),
  )
  |> append_unique_issues(
    context.active_issues
    |> list.filter(fn(issue) {
      dict.has_key(
        state.workers.by_issue,
        orchestrator_state.issue_identity(issue),
      )
    }),
  )
}

fn append_unique_list(
  existing: List(String),
  values: List(String),
) -> List(String) {
  list.fold(values, existing, fn(acc, value) {
    case list.contains(acc, value) {
      True -> acc
      False -> list.append(acc, [value])
    }
  })
}

fn append_unique_issues(
  existing: List(tracker_issue.Issue),
  values: List(tracker_issue.Issue),
) -> List(tracker_issue.Issue) {
  list.fold(values, existing, fn(acc, issue) {
    case list.contains(list.map(acc, fn(item) { item.id }), issue.id) {
      True -> acc
      False -> list.append(acc, [issue])
    }
  })
}

fn clear_retry_refresh_generation(
  state: transition_types.State,
  issue_id: String,
  backend_kind: String,
) -> transition_types.State {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(issue_id, backend_kind)
  transition_types.State(
    ..state,
    retry_refresh_generations: dict.delete(
      state.retry_refresh_generations,
      identity,
    ),
  )
}

fn clear_retry(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  backend_kind: String,
) -> orchestrator_state.RuntimeState {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(issue_id, backend_kind)
  orchestrator_state.RuntimeState(
    ..state,
    retry_attempts: dict.delete(state.retry_attempts, identity),
  )
}

fn release_claim(
  state: orchestrator_state.RuntimeState,
  issue_id: String,
  backend_kind: String,
) -> orchestrator_state.RuntimeState {
  let identity =
    orchestrator_state.issue_id_identity_for_backend(issue_id, backend_kind)
  orchestrator_state.RuntimeState(
    ..state,
    claimed: dict.delete(state.claimed, identity),
    retry_attempts: dict.delete(state.retry_attempts, identity),
  )
}

fn identifier_for_runtime_ref(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
  ref: Option(task.TaskRef),
) -> String {
  let identity = case ref {
    Some(ref) -> orchestrator_state.task_ref_identity(ref)
    None -> orchestrator_state.linear_issue_id_identity(issue_id)
  }
  identifier_for_identity(runtime, identity, issue_id)
}

fn identifier_for_identity(
  runtime: orchestrator_state.RuntimeState,
  identity: identity.TaskIdentity,
  issue_id: String,
) -> String {
  case dict.get(runtime.claimed, identity) {
    Ok(identifier) -> identifier
    Error(Nil) ->
      case dict.get(runtime.completed, identity) {
        Ok(issue) -> issue.identifier
        Error(Nil) ->
          case dict.get(runtime.parked, identity) {
            Ok(parked) -> parked.identifier
            Error(Nil) -> issue_id
          }
      }
  }
}
