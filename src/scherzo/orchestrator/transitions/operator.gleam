import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/command
import scherzo/orchestrator/control_command_handler
import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/transitions/claims
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/event
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub type Callbacks {
  Callbacks(
    issue_is_running_claimed_or_pending: fn(
      transition_types.State,
      transition_types.DispatchContext,
      String,
    ) -> Bool,
    can_reserve_dispatch_slot: fn(
      transition_types.State,
      transition_types.DispatchContext,
      tracker_issue.Issue,
    ) -> Bool,
    dispatch_candidates: fn(
      List(tracker_issue.Issue),
      transition_types.State,
      transition_types.DispatchContext,
    ) -> transition_types.Outcome,
  )
}

pub fn handle_submitted(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue_resolution: transition_types.OperatorIssueResolution,
  parked_issue_resolution: transition_types.ParkedIssueResolution,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  case request.operator_command {
    command.PauseDispatch -> {
      let pending = dict.size(state.pending_claims)
      let result =
        command.applied(
          request.operator_command,
          Some("dispatch paused; pending_claims=" <> int.to_string(pending)),
        )
      transition_types.Outcome(state: state, effects: [
        effects_types.SetOperatorPaused(True),
        operator_pause_append_effect(True, request, result),
      ])
    }
    command.ResumeDispatch -> {
      let result =
        command.applied(request.operator_command, Some("dispatch resumed"))
      transition_types.Outcome(state: state, effects: [
        operator_pause_append_effect(False, request, result),
      ])
    }
    command.RetryIssue(_) ->
      handle_retry(state, request, context, issue_resolution, callbacks)
    command.RetryIssueStartFresh(_, reason) ->
      handle_retry_start_fresh(
        state,
        request,
        context,
        issue_resolution,
        reason,
        callbacks,
      )
    command.RetryWorkflowStep(_, _)
    | command.RetryWorkflowStepExact(_, _)
    | command.RecollectWorkflowOutputs(_)
    | command.RunFinalize(..)
    | command.RetryArtifactPublication(_, _) -> shell_command(state, request)
    command.ParkIssue(_, reason) ->
      handle_park(state, request, context, issue_resolution, reason, callbacks)
    command.UnparkIssue(_) ->
      handle_unpark(state, request, context, parked_issue_resolution)
    command.PromptSession(_, message) ->
      case control_command_handler.operator_prompt_too_large(message) {
        True ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              "prompt_too_large",
              Some("operator prompt is too large"),
            ),
          )
        False -> shell_command(state, request)
      }
    command.RespondUi(_, _, response) ->
      case control_command_handler.ui_response_too_large(response) {
        True ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              "ui_response_too_large",
              Some("operator UI response value is too large"),
            ),
          )
        False -> shell_command(state, request)
      }
    command.ReloadWorkflow
    | command.AbortSession(_)
    | command.StopAfterCurrentTurn(_)
    | command.CleanupOrphanSteps(_, _)
    | command.RunScheduleNow(_)
    | command.ReenableSchedule(_)
    | command.WorkItemAction(_) -> shell_command(state, request)
  }
}

fn operator_pause_append_effect(
  paused: Bool,
  request: effects_types.OperatorCommandRequest,
  result: command.CommandResult,
) -> effects_types.Effect {
  effects_types.AppendLedger(effects_types.LedgerAppend(
    correlation_id: "operator_dispatch_pause:"
      <> record.dispatch_pause_status(paused),
    batch: ledger_batch.dispatch_pause_changed(paused),
    failure_event: "operator_dispatch_pause_ledger_append_failed",
    policy: effects_types.SetOperatorPausedAfterAppend(
      paused,
      request,
      result,
      command.rejected(
        request.operator_command,
        "ledger_append_failed",
        Some(dispatch_pause_failure_message(paused)),
      ),
    ),
  ))
}

fn dispatch_pause_failure_message(paused: Bool) -> String {
  case paused {
    True ->
      "dispatch pause is active in memory but was not persisted because the ledger append failed"
    False -> "dispatch resume was not applied because the ledger append failed"
  }
}

pub fn handle_report_park_continuation(
  state: transition_types.State,
  correlation_id: String,
  issue_id: String,
  issue_identifier: String,
  reason: String,
  release_policy: String,
  source_run_id: Option(String),
  result: Result(Nil, ledger.LedgerError),
) -> transition_types.Outcome {
  case result {
    Error(err) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.Log("warn", "ledger_append_failed", [
          #("issue_id", issue_id),
          #("correlation_id", correlation_id),
          #("error", ledger_error_code(err)),
        ]),
      ])
    Ok(Nil) ->
      transition_types.Outcome(state: state, effects: [
        effects_types.ReportParkEffect(
          issue_id,
          issue_identifier,
          reason,
          release_policy,
          source_run_id,
        ),
      ])
  }
}

fn shell_command(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.ApplyOperatorCommand(request),
  ])
}

fn finish(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  result: command.CommandResult,
) -> transition_types.Outcome {
  transition_types.Outcome(state: state, effects: [
    effects_types.FinishOperatorCommand(request, result),
  ])
}

fn handle_retry(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue_resolution: transition_types.OperatorIssueResolution,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(issue_is_running_claimed_or_pending: active, ..) = callbacks
  case issue_resolution_to_result(request.operator_command, issue_resolution) {
    Error(result) -> finish(state, request, result)
    Ok(issue) ->
      case active(state, context, issue.id) {
        True ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              "issue_already_active",
              Some("issue is running, claimed, or pending claim"),
            ),
          )
        False ->
          case context.operator_paused {
            True ->
              finish(
                state,
                request,
                command.rejected(
                  request.operator_command,
                  "dispatch_paused",
                  Some("dispatch is paused"),
                ),
              )
            False -> retry_issue(state, request, context, issue, callbacks)
          }
      }
  }
}

fn retry_issue(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(
    can_reserve_dispatch_slot: can_reserve,
    dispatch_candidates: dispatch,
    ..,
  ) = callbacks
  case context.dispatch_enabled {
    False ->
      finish(
        state,
        request,
        command.rejected(
          request.operator_command,
          "dispatch_disabled",
          Some("dispatch is not currently enabled"),
        ),
      )
    True -> {
      let released_park =
        core.retry_releasable_park_for_issue(state.runtime, issue)
      case
        core.retry_candidate_precondition_failure(
          state.runtime,
          context.effective,
          issue.id,
          issue,
        )
      {
        Some(reason) ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              reason,
              Some(retry_rejection_message(reason, state.runtime, issue)),
            ),
          )
        None ->
          case core.workflow_policy_satisfied(context.effective, issue) {
            False ->
              finish(
                state,
                request,
                command.rejected(
                  request.operator_command,
                  "retry_workflow_policy_invalid",
                  Some("retry rejected: workflow policy is not satisfied"),
                ),
              )
            True ->
              case can_reserve(state, context, issue) {
                False ->
                  finish(
                    state,
                    request,
                    command.rejected(
                      request.operator_command,
                      "retry_no_dispatch_slots",
                      Some("retry deferred: no dispatch slots are available"),
                    ),
                  )
                True -> {
                  let state = reset_issue_for_operator_retry(state, issue)
                  let effects =
                    operator_retry_effects(issue, context, released_park)
                  let claim_context =
                    context_without_retried_recovery(context, issue.id)
                  let claim =
                    claims.begin_for_issue(
                      state,
                      issue,
                      [],
                      claim_context,
                      claims.Callbacks(dispatch_candidates: dispatch),
                    )
                  transition_types.Outcome(
                    state: claim.state,
                    effects: list.append(
                      effects,
                      list.append(claim.effects, [
                        effects_types.FinishOperatorCommand(
                          request,
                          command.applied(
                            request.operator_command,
                            Some("retry accepted"),
                          ),
                        ),
                      ]),
                    ),
                  )
                }
              }
          }
      }
    }
  }
}

fn handle_retry_start_fresh(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue_resolution: transition_types.OperatorIssueResolution,
  reason: String,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(issue_is_running_claimed_or_pending: active, ..) = callbacks
  case issue_resolution_to_result(request.operator_command, issue_resolution) {
    Error(result) -> finish(state, request, result)
    Ok(issue) ->
      case active(state, context, issue.id) {
        True ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              "issue_already_active",
              Some("issue is running, claimed, or pending claim"),
            ),
          )
        False ->
          case context.operator_paused {
            True ->
              finish(
                state,
                request,
                command.rejected(
                  request.operator_command,
                  "dispatch_paused",
                  Some("dispatch is paused"),
                ),
              )
            False ->
              retry_issue_start_fresh(
                state,
                request,
                context,
                issue,
                reason,
                callbacks,
              )
          }
      }
  }
}

fn retry_issue_start_fresh(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  reason: String,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(
    can_reserve_dispatch_slot: can_reserve,
    dispatch_candidates: dispatch,
    ..,
  ) = callbacks
  case context.dispatch_enabled {
    False ->
      finish(
        state,
        request,
        command.rejected(
          request.operator_command,
          "dispatch_disabled",
          Some("dispatch is not currently enabled"),
        ),
      )
    True ->
      case start_fresh_block_reason(state.runtime, context, issue) {
        Error(block_reason) ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              block_reason,
              Some(start_fresh_rejection_message(block_reason)),
            ),
          )
        Ok(Nil) ->
          case
            core.retry_candidate_precondition_failure(
              state.runtime,
              context.effective,
              issue.id,
              issue,
            )
          {
            Some("retry_issue_parked") | None ->
              case core.workflow_policy_satisfied(context.effective, issue) {
                False ->
                  finish(
                    state,
                    request,
                    command.rejected(
                      request.operator_command,
                      "retry_workflow_policy_invalid",
                      Some("retry rejected: workflow policy is not satisfied"),
                    ),
                  )
                True ->
                  case can_reserve(state, context, issue) {
                    False ->
                      finish(
                        state,
                        request,
                        command.rejected(
                          request.operator_command,
                          "retry_no_dispatch_slots",
                          Some(
                            "retry deferred: no dispatch slots are available",
                          ),
                        ),
                      )
                    True -> {
                      let state = reset_issue_for_operator_retry(state, issue)
                      let claim_context =
                        context_without_retried_recovery(context, issue.id)
                      let claim =
                        claims.begin_for_issue(
                          state,
                          issue,
                          [],
                          claim_context,
                          claims.Callbacks(dispatch_candidates: dispatch),
                        )
                      transition_types.Outcome(
                        state: claim.state,
                        effects: list.append(
                          start_fresh_retry_effects(
                            state.runtime,
                            issue,
                            context,
                          ),
                          list.append(claim.effects, [
                            effects_types.FinishOperatorCommand(
                              request,
                              command.applied(
                                request.operator_command,
                                Some(
                                  "retry accepted; starts a fresh run; reason: "
                                  <> reason,
                                ),
                              ),
                            ),
                          ]),
                        ),
                      )
                    }
                  }
              }
            Some(other) ->
              finish(
                state,
                request,
                command.rejected(
                  request.operator_command,
                  other,
                  Some(retry_rejection_message(other, state.runtime, issue)),
                ),
              )
          }
      }
  }
}

fn start_fresh_block_reason(
  runtime: orchestrator_state.RuntimeState,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Result(Nil, String) {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue.id),
    )
  {
    Ok(orchestrator_state.ParkedEntry(reason: reason, ..)) ->
      case
        qualifying_start_fresh_reason(orchestrator_reason.park_to_string(reason))
      {
        True -> Ok(Nil)
        False -> Error("start_fresh_not_allowed")
      }
    Error(Nil) ->
      case parked_reason_from_projection(context.workspace_root, issue.id) {
        Some(reason) ->
          case qualifying_start_fresh_reason(reason) {
            True -> Ok(Nil)
            False -> Error("start_fresh_not_allowed")
          }
        None ->
          case dict.get(context.recovery_by_issue, issue.id) {
            Ok(recovery) ->
              case recovery_allows_start_fresh(recovery) {
                True -> Ok(Nil)
                False -> Error("start_fresh_not_allowed")
              }
            Error(Nil) -> Ok(Nil)
          }
      }
  }
}

fn parked_reason_from_projection(
  workspace_root: String,
  issue_id: String,
) -> Option(String) {
  case ledger.path_for_workspace_root(workspace_root) {
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(projected) ->
          case dict.get(projected.parked_issues, issue_id) {
            Ok(parked) -> Some(parked.reason)
            Error(Nil) -> None
          }
        Error(_) -> None
      }
    Error(_) -> None
  }
}

fn recovery_allows_start_fresh(recovery: event.RecoveryInfo) -> Bool {
  case recovery.park_reason {
    Some(reason) -> qualifying_start_fresh_reason(reason)
    None ->
      case recovery.drift_kind {
        Some(_) -> True
        None ->
          case recovery.status {
            event.Blocked
            | event.Parked
            | event.DriftDetected
            | event.OldStateResetRequired -> True
            _ -> False
          }
      }
  }
}

fn qualifying_start_fresh_reason(reason: String) -> Bool {
  string.starts_with(reason, "workflow_definition_drift")
  || string.starts_with(reason, "issue_content_drift")
  || string.starts_with(reason, "issue_state_drift")
  || string.starts_with(reason, "dispatch_recovery")
}

fn start_fresh_rejection_message(reason: String) -> String {
  case reason {
    "start_fresh_not_allowed" ->
      "start-fresh retry only clears retained drift or recovery-blocked state"
    _ -> "retry rejected: " <> reason
  }
}

fn start_fresh_retry_effects(
  runtime: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
  context: transition_types.DispatchContext,
) -> List(effects_types.Effect) {
  let unpark_effects = case
    start_fresh_has_recovery_state(runtime, context, issue)
  {
    True -> [
      effects_types.AppendLedger(effects_types.LedgerAppend(
        correlation_id: "operator_retry_start_fresh_unpark:" <> issue.id,
        batch: ledger_batch.issue_unparked(
          issue.id,
          issue.identifier,
          "start_fresh",
          context.now_ms,
        ),
        failure_event: "ledger_append_failed",
        policy: effects_types.ContinueRegardless,
      )),
    ]
    False -> []
  }
  list.append(unpark_effects, [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "operator_retry_start_fresh_counter_reset:" <> issue.id,
      batch: ledger_batch.operator_retry_counter_reset(
        issue.id,
        issue.identifier,
        context.now_ms,
      ),
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
    effects_types.CancelRetryTimer(issue.id, 0, "operator_retry_start_fresh"),
    effects_types.ClearRecovery(issue.id),
  ])
}

fn start_fresh_has_recovery_state(
  runtime: orchestrator_state.RuntimeState,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
) -> Bool {
  case
    dict.get(
      runtime.parked,
      orchestrator_state.linear_issue_id_identity(issue.id),
    )
  {
    Ok(_) -> True
    Error(Nil) ->
      case parked_reason_from_projection(context.workspace_root, issue.id) {
        Some(_) -> True
        None -> dict.has_key(context.recovery_by_issue, issue.id)
      }
  }
}

fn context_without_retried_recovery(
  context: transition_types.DispatchContext,
  issue_id: String,
) -> transition_types.DispatchContext {
  transition_types.DispatchContext(
    ..context,
    recovery_by_issue: dict.delete(context.recovery_by_issue, issue_id),
  )
}

fn reset_issue_for_operator_retry(
  state: transition_types.State,
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let identity = orchestrator_state.issue_identity(issue)
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_state.clear_task_lifecycle(state.runtime, identity),
      issue_counters: dict.delete(state.runtime.issue_counters, identity),
    )
  transition_types.State(..state, runtime: runtime)
}

fn operator_retry_effects(
  issue: tracker_issue.Issue,
  context: transition_types.DispatchContext,
  released_park: Option(orchestrator_state.ParkedEntry),
) -> List(effects_types.Effect) {
  let batch = case released_park {
    Some(_) ->
      ledger_batch.issue_unparked(
        issue.id,
        issue.identifier,
        "operator_retry",
        context.now_ms,
      )
    None ->
      ledger_batch.operator_retry_counter_reset(
        issue.id,
        issue.identifier,
        context.now_ms,
      )
  }
  [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "operator_retry:" <> issue.id,
      batch: batch,
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
    effects_types.CancelRetryTimer(issue.id, 0, "operator_retry"),
    effects_types.ClearRecovery(issue.id),
  ]
}

fn retry_rejection_message(
  reason: String,
  runtime: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> String {
  case reason {
    "retry_issue_parked" -> retry_parked_rejection_message(runtime, issue)
    _ -> "retry rejected: " <> reason
  }
}

fn retry_parked_rejection_message(
  runtime: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
) -> String {
  case dict.get(runtime.parked, orchestrator_state.issue_identity(issue)) {
    Ok(parked) ->
      "retry rejected: issue is parked for "
      <> orchestrator_reason.park_to_string(parked.reason)
      <> "; run `"
      <> core.parked_unpark_command(parked)
      <> "` before retry"
    Error(Nil) -> "retry rejected: retry_issue_parked"
  }
}

fn handle_park(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue_resolution: transition_types.OperatorIssueResolution,
  reason: String,
  callbacks: Callbacks,
) -> transition_types.Outcome {
  let Callbacks(issue_is_running_claimed_or_pending: active, ..) = callbacks
  case issue_resolution_to_result(request.operator_command, issue_resolution) {
    Error(result) -> finish(state, request, result)
    Ok(issue) ->
      case active(state, context, issue.id) {
        True ->
          finish(
            state,
            request,
            command.rejected(
              request.operator_command,
              "issue_active",
              Some(
                "running, claimed, or pending issues must be stopped before parking",
              ),
            ),
          )
        False -> park_issue(state, request, context, issue, reason)
      }
  }
}

fn park_issue(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  issue: tracker_issue.Issue,
  reason: String,
) -> transition_types.Outcome {
  let task_ref = task.from_legacy_issue(issue).ref
  let identity = orchestrator_state.task_ref_identity(task_ref)
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator(reason),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: context.now_ms,
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_state.mark_task_parked(state.runtime, identity, parked),
      issue_counters: dict.delete(state.runtime.issue_counters, identity),
    )
  let state = transition_types.State(..state, runtime: runtime)
  let reason_text = orchestrator_reason.park_to_string(parked.reason)
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "operator_park:" <> issue.id,
      batch: ledger_batch.issue_parked(
        issue.id,
        issue.identifier,
        reason_text,
        "explicit_unpark_only",
        "",
        context.now_ms,
      ),
      failure_event: "ledger_append_failed",
      policy: effects_types.ReportParkAfterAppend(
        issue.id,
        issue.identifier,
        reason_text,
        "explicit_unpark_only",
        None,
      ),
    )),
    effects_types.Log("warn", "issue_parked", [
      #("issue_id", issue.id),
      #("reason", reason),
    ]),
    effects_types.FinishOperatorCommand(
      request,
      command.applied(request.operator_command, Some("issue parked")),
    ),
  ])
}

fn handle_unpark(
  state: transition_types.State,
  request: effects_types.OperatorCommandRequest,
  context: transition_types.DispatchContext,
  parked_issue_resolution: transition_types.ParkedIssueResolution,
) -> transition_types.Outcome {
  case
    parked_resolution_to_result(
      request.operator_command,
      parked_issue_resolution,
    )
  {
    Error(result) -> finish(state, request, result)
    Ok(issue_id) -> {
      let identity = orchestrator_state.linear_issue_id_identity(issue_id)
      let issue_identifier = identifier_for_runtime(state.runtime, issue_id)
      let runtime =
        orchestrator_state.RuntimeState(
          ..orchestrator_state.clear_task_lifecycle(state.runtime, identity),
          issue_counters: dict.delete(state.runtime.issue_counters, identity),
        )
      let state = transition_types.State(..state, runtime: runtime)
      transition_types.Outcome(state: state, effects: [
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "operator_unpark:" <> issue_id,
          batch: ledger_batch.issue_unparked(
            issue_id,
            issue_identifier,
            "operator",
            context.now_ms,
          ),
          failure_event: "ledger_append_failed",
          policy: effects_types.ContinueRegardless,
        )),
        effects_types.CancelRetryTimer(issue_id, 0, "operator"),
        effects_types.ClearRecovery(issue_id),
        effects_types.Log("info", "issue_unparked", [#("issue_id", issue_id)]),
        effects_types.FinishOperatorCommand(
          request,
          command.applied(request.operator_command, Some("issue unparked")),
        ),
      ])
    }
  }
}

fn issue_resolution_to_result(
  operator_command: command.OperatorCommand,
  resolution: transition_types.OperatorIssueResolution,
) -> Result(tracker_issue.Issue, command.CommandResult) {
  case resolution {
    transition_types.OperatorIssueResolved(issue) -> Ok(issue)
    transition_types.OperatorIssueNotFound ->
      Error(command.not_found(operator_command, Some("issue not found")))
    transition_types.OperatorIssueRejected(reason) ->
      Error(command.rejected(operator_command, reason, Some(reason)))
    transition_types.OperatorIssueNotAllowed(reason) ->
      Error(command.not_allowed(operator_command, reason, Some(reason)))
    transition_types.OperatorIssueResolutionFailed ->
      Error(command.rejected(operator_command, "issue_resolution_failed", None))
    transition_types.OperatorIssueNotResolved ->
      Error(command.rejected(operator_command, "issue_resolution_missing", None))
  }
}

fn parked_resolution_to_result(
  operator_command: command.OperatorCommand,
  resolution: transition_types.ParkedIssueResolution,
) -> Result(String, command.CommandResult) {
  case resolution {
    transition_types.ParkedIssueResolved(issue_id) -> Ok(issue_id)
    transition_types.ParkedIssueNotFound ->
      Error(command.not_found(operator_command, Some("parked issue not found")))
    transition_types.ParkedIssueRejected(reason) ->
      Error(command.rejected(operator_command, reason, Some(reason)))
    transition_types.ParkedIssueNotAllowed(reason) ->
      Error(command.not_allowed(operator_command, reason, Some(reason)))
    transition_types.ParkedIssueResolutionFailed ->
      Error(command.rejected(operator_command, "issue_resolution_failed", None))
    transition_types.ParkedIssueNotResolved ->
      Error(command.rejected(operator_command, "issue_resolution_missing", None))
  }
}

fn identifier_for_runtime(
  runtime: orchestrator_state.RuntimeState,
  issue_id: String,
) -> String {
  let identity = orchestrator_state.linear_issue_id_identity(issue_id)
  case dict.get(runtime.claimed, identity) {
    Ok(identifier) -> identifier
    Error(Nil) ->
      case dict.get(runtime.completed, identity) {
        Ok(completed) ->
          orchestrator_state.completed_issue(completed).identifier
        Error(Nil) ->
          case dict.get(runtime.parked, identity) {
            Ok(parked) -> parked.identifier
            Error(Nil) -> issue_id
          }
      }
  }
}

fn ledger_error_code(err: ledger.LedgerError) -> String {
  ledger.ledger_error_code(err)
}
