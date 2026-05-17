import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/control/command
import scherzo/orchestrator/control_command_handler
import scherzo/orchestrator/core
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/reason as orchestrator_reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_types
import scherzo/state/ledger
import scherzo/state/record
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
        effects_types.FinishOperatorCommand(request, result),
      ])
    }
    command.ResumeDispatch -> {
      let result =
        command.applied(request.operator_command, Some("dispatch resumed"))
      transition_types.Outcome(state: state, effects: [
        effects_types.SetOperatorPaused(False),
        effects_types.FinishOperatorCommand(request, result),
      ])
    }
    command.RetryIssue(_) ->
      handle_retry(state, request, context, issue_resolution, callbacks)
    command.RetryWorkflowStep(_, _) -> shell_command(state, request)
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
    | command.RunScheduleNow(_) -> shell_command(state, request)
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
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      parked: dict.delete(state.runtime.parked, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
    )
  let state = transition_types.State(..state, runtime: runtime)
  let effects = [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "operator_retry:" <> issue.id,
      bodies: [
        record.IssueUnparked(issue.id, issue.identifier, "operator_retry"),
        record.IssueCounterUpdated(
          issue.id,
          issue.identifier,
          0,
          0,
          context.now_ms,
          None,
        ),
      ],
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueRegardless,
    )),
    effects_types.CancelRetryTimer(issue.id, 0, "operator_retry"),
    effects_types.ClearRecovery(issue.id),
  ]
  case
    context.dispatch_enabled
    && core.should_dispatch(state.runtime, context.effective, issue)
    && can_reserve(state, context, issue)
  {
    True -> {
      let dispatched = dispatch([issue], state, context)
      transition_types.Outcome(
        state: dispatched.state,
        effects: list.append(
          effects,
          list.append(dispatched.effects, [
            effects_types.FinishOperatorCommand(
              request,
              command.applied(
                request.operator_command,
                Some("retry dispatched"),
              ),
            ),
          ]),
        ),
      )
    }
    False ->
      transition_types.Outcome(
        state: state,
        effects: list.append(effects, [
          effects_types.FinishOperatorCommand(
            request,
            command.rejected(
              request.operator_command,
              "not_dispatchable",
              Some("issue is not currently dispatchable"),
            ),
          ),
        ]),
      )
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
  let parked =
    orchestrator_state.ParkedEntry(
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator(reason),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: context.now_ms,
    )
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      running: dict.delete(state.runtime.running, issue.id),
      claimed: dict.delete(state.runtime.claimed, issue.id),
      retry_attempts: dict.delete(state.runtime.retry_attempts, issue.id),
      issue_counters: dict.delete(state.runtime.issue_counters, issue.id),
      parked: dict.insert(state.runtime.parked, issue.id, parked),
    )
  let state = transition_types.State(..state, runtime: runtime)
  let reason_text = orchestrator_reason.park_to_string(parked.reason)
  transition_types.Outcome(state: state, effects: [
    effects_types.AppendLedger(effects_types.LedgerAppend(
      correlation_id: "operator_park:" <> issue.id,
      bodies: [
        record.IssueParkedV2(
          issue.id,
          issue.identifier,
          reason_text,
          "explicit_unpark_only",
          "",
          context.now_ms,
        ),
      ],
      failure_event: "ledger_append_failed",
      policy: effects_types.ContinueWith(effects_types.ReportParkAfterLedger(
        issue.id,
        issue.identifier,
        reason_text,
        "explicit_unpark_only",
        None,
      )),
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
      let issue_identifier = identifier_for_runtime(state.runtime, issue_id)
      let runtime =
        orchestrator_state.RuntimeState(
          ..state.runtime,
          parked: dict.delete(state.runtime.parked, issue_id),
          retry_attempts: dict.delete(state.runtime.retry_attempts, issue_id),
          issue_counters: dict.delete(state.runtime.issue_counters, issue_id),
        )
      let state = transition_types.State(..state, runtime: runtime)
      transition_types.Outcome(state: state, effects: [
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "operator_unpark:" <> issue_id,
          bodies: [
            record.IssueUnparked(issue_id, issue_identifier, "operator"),
            record.IssueCounterUpdated(
              issue_id,
              issue_identifier,
              0,
              0,
              context.now_ms,
              None,
            ),
          ],
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
  case dict.get(runtime.claimed, issue_id) {
    Ok(identifier) -> identifier
    Error(Nil) ->
      case dict.get(runtime.completed, issue_id) {
        Ok(issue) -> issue.identifier
        Error(Nil) ->
          case dict.get(runtime.parked, issue_id) {
            Ok(parked) -> parked.identifier
            Error(Nil) -> issue_id
          }
      }
  }
}

fn ledger_error_code(err: ledger.LedgerError) -> String {
  case err {
    ledger.Io(_) -> "io"
    ledger.LedgerFfiFailed(_) -> "ledger_ffi_failed"
    ledger.UnsupportedVersion(_) -> "unsupported_version"
    ledger.CorruptRecord(_, _) -> "corrupt_record"
  }
}
