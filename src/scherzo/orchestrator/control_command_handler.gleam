import gleam/erlang/process
import gleam/int
import gleam/option.{Some}
import gleam/string
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/log

pub const max_worker_command_wait_ms = 500

pub type Context(state) {
  Context(
    state: state,
    pending_claim_count: fn(state) -> Int,
    set_paused: fn(state, Bool) -> state,
    reload_workflow: fn(state, command.OperatorCommand) ->
      #(state, command.CommandResult),
    retry_issue: fn(state, command.OperatorCommand, command.IssueRef) ->
      #(state, command.CommandResult),
    park_issue: fn(state, command.OperatorCommand, command.IssueRef, String) ->
      #(state, command.CommandResult),
    unpark_issue: fn(state, command.OperatorCommand, command.IssueRef) ->
      #(state, command.CommandResult),
    // Long-running daemon processes can construct this tuple from old code while
    // this module is reloaded from newer code. Do not insert new fields before
    // log_result; append new callbacks after it and update the compatibility
    // shims at the bottom of this module.
    run_schedule_now: fn(state, command.OperatorCommand, String) ->
      #(state, command.CommandResult),
    abort_session: fn(state, command.OperatorCommand, String, Int) ->
      #(state, command.CommandResult),
    route_worker_command: fn(
      state,
      command.OperatorCommand,
      String,
      Int,
      fn(
        process.Subject(worker_command.Command),
        process.Subject(worker_command.Reply),
      ) -> Nil,
    ) -> #(state, command.CommandResult),
    log_result: fn(state, command.CommandResult, List(log.Field)) -> Nil,
  )
}

pub fn apply(
  context: Context(state),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
) -> #(state, command.CommandResult) {
  case operator_command {
    command.PauseDispatch -> {
      let pending = context.pending_claim_count(context.state)
      let state = context.set_paused(context.state, True)
      let result =
        command.applied(
          operator_command,
          Some("dispatch paused; pending_claims=" <> int.to_string(pending)),
        )
      log_context_result(context, state, result, [
        #("pending_claims", int.to_string(pending)),
      ])
      #(state, result)
    }
    command.ResumeDispatch -> {
      let state = context.set_paused(context.state, False)
      let result = command.applied(operator_command, Some("dispatch resumed"))
      log_context_result(context, state, result, [])
      #(state, result)
    }
    command.ReloadWorkflow ->
      log_transition(
        context,
        context.reload_workflow(context.state, operator_command),
      )
    command.RetryIssue(issue_ref)
    | command.RetryIssueStartFresh(issue_ref, _) ->
      log_transition(
        context,
        context.retry_issue(context.state, operator_command, issue_ref),
      )
    command.RetryWorkflowStep(_, _) | command.RetryWorkflowStepExact(_, _) -> {
      let result =
        command.rejected(
          operator_command,
          "retry_step_requires_daemon_shell",
          Some("retry-step must be handled by the daemon shell path"),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
    command.RecollectWorkflowOutputs(_) -> {
      let result =
        command.rejected(
          operator_command,
          "recollect_outputs_requires_daemon_shell",
          Some("recollect-outputs must be handled by the daemon shell path"),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
    command.RunFinalize(..) -> {
      let result =
        command.rejected(
          operator_command,
          "run_finalize_requires_daemon_shell",
          Some("run finalize must be handled by the daemon shell path"),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
    command.RetryArtifactPublication(_, _) -> {
      let result =
        command.rejected(
          operator_command,
          "retry_artifact_publication_requires_daemon_shell",
          Some(
            "artifact publication retry must be handled by the daemon shell path",
          ),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
    command.CleanupOrphanSteps(_, _) -> {
      let result =
        command.rejected(
          operator_command,
          "cleanup_orphan_steps_requires_daemon_shell",
          Some("cleanup-orphan-steps must be handled by the daemon shell path"),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
    command.ParkIssue(issue_ref, reason) ->
      log_transition(
        context,
        context.park_issue(context.state, operator_command, issue_ref, reason),
      )
    command.UnparkIssue(issue_ref) ->
      log_transition(
        context,
        context.unpark_issue(context.state, operator_command, issue_ref),
      )
    command.AbortSession(session_id) ->
      context.abort_session(
        context.state,
        operator_command,
        session_id,
        timeout_ms,
      )
    command.StopAfterCurrentTurn(session_id) ->
      context.route_worker_command(
        context.state,
        operator_command,
        session_id,
        timeout_ms,
        fn(subject, reply) {
          process.send(subject, worker_command.StopAfterCurrentTurn(reply))
        },
      )
    command.PromptSession(session_id, message) ->
      case operator_prompt_too_large(message) {
        True -> {
          let result =
            command.rejected(
              operator_command,
              "prompt_too_large",
              Some("operator prompt is too large"),
            )
          log_context_result(context, context.state, result, [])
          #(context.state, result)
        }
        False ->
          context.route_worker_command(
            context.state,
            operator_command,
            session_id,
            timeout_ms,
            fn(subject, reply) {
              process.send(subject, worker_command.QueuePrompt(message, reply))
            },
          )
      }
    command.RespondUi(session_id, request_id, response) ->
      case ui_response_too_large(response) {
        True -> {
          let result =
            command.rejected(
              operator_command,
              "ui_response_too_large",
              Some("operator UI response value is too large"),
            )
          log_context_result(context, context.state, result, [])
          #(context.state, result)
        }
        False ->
          context.route_worker_command(
            context.state,
            operator_command,
            session_id,
            timeout_ms,
            fn(subject, reply) {
              process.send(
                subject,
                worker_command.RespondToUi(request_id, response, reply),
              )
            },
          )
      }
    command.RunScheduleNow(job_id) ->
      case ffi_supports_run_schedule_now(context) {
        True ->
          log_transition(
            context,
            context.run_schedule_now(context.state, operator_command, job_id),
          )
        False -> {
          let result =
            command.rejected(
              operator_command,
              "daemon_code_stale",
              Some("restart the daemon before running schedules manually"),
            )
          log_context_result(context, context.state, result, [])
          #(context.state, result)
        }
      }
    command.ReenableSchedule(_) -> {
      let result =
        command.rejected(
          operator_command,
          "daemon_shell_required",
          Some("schedule re-enable must be handled by the daemon shell path"),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
    command.WorkItemAction(_) -> {
      let result =
        command.rejected(
          operator_command,
          "work_item_action_requires_daemon_shell",
          Some("work item actions must be handled by the daemon shell path"),
        )
      log_context_result(context, context.state, result, [])
      #(context.state, result)
    }
  }
}

fn log_transition(
  context: Context(state),
  transition: #(state, command.CommandResult),
) -> #(state, command.CommandResult) {
  let #(state, result) = transition
  log_context_result(context, state, result, [])
  #(state, result)
}

fn log_context_result(
  context: Context(state),
  state: state,
  result: command.CommandResult,
  fields: List(log.Field),
) -> Nil {
  ffi_log_result(context, state, result, fields)
}

pub fn worker_command_timeout(timeout_ms: Int) -> Int {
  let client_timeout = case timeout_ms > 25 {
    True -> timeout_ms - 25
    False ->
      case timeout_ms > 1 {
        True -> timeout_ms - 1
        False -> timeout_ms
      }
  }
  min_int(client_timeout, max_worker_command_wait_ms)
}

pub fn operator_prompt_too_large(message: String) -> Bool {
  string.length(message) > worker_command.max_operator_prompt_chars
}

pub fn ui_response_too_large(response: command.UiResponse) -> Bool {
  case response {
    command.UiCancel -> False
    command.UiValue(value) ->
      string.length(value) > worker_command.max_operator_ui_value_chars
  }
}

pub fn worker_reply_to_command_result(
  operator_command: command.OperatorCommand,
  reply: worker_command.Reply,
) -> command.CommandResult {
  case reply {
    worker_command.Applied(message) ->
      command.applied(operator_command, message)
    worker_command.Queued(message) -> command.queued(operator_command, message)
    worker_command.Rejected(reason, message) ->
      command.rejected(operator_command, reason, message)
    worker_command.NotFound(message) ->
      command.not_found(operator_command, message)
    worker_command.NotAllowed(reason, message) ->
      command.not_allowed(operator_command, reason, message)
  }
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

@external(erlang, "scherzo_control_command_handler_ffi", "log_result")
fn ffi_log_result(
  context: Context(state),
  state: state,
  result: command.CommandResult,
  fields: List(log.Field),
) -> Nil

@external(erlang, "scherzo_control_command_handler_ffi", "supports_run_schedule_now")
fn ffi_supports_run_schedule_now(context: Context(state)) -> Bool
