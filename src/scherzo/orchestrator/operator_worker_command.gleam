import gleam/erlang/process
import gleam/option.{Some}
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/orchestrator/control_command_handler

pub type Outcome(state) {
  Immediate(state, command.CommandResult)
  Pending(state)
}

pub type WorkerLookup {
  NoWorker
  WorkerWithCommandSubject(process.Subject(worker_command.Command))
  WorkerWithoutCommandSubject(run_id: String)
}

pub type StepRunCommandSubject {
  StepRunCommandSubjectFound(process.Subject(worker_command.Command))
  NoActiveStepCommandSubject
  MultipleActiveStepCommandSubjects
}

pub opaque type Context(state, message) {
  Context(
    state: state,
    daemon_subject: process.Subject(message),
    route_session_id: fn(state, String) -> String,
    worker_for_session: fn(state, String) -> WorkerLookup,
    step_subject_for_run: fn(state, String) -> StepRunCommandSubject,
    step_subject_for_session: fn(state, String) ->
      Result(process.Subject(worker_command.Command), Nil),
    stop_for_abort: fn(state, command.OperatorCommand, String) ->
      #(state, command.CommandResult),
    completion_message: fn(
      command.OperatorCommand,
      worker_command.Reply,
      process.Subject(command.CommandResult),
    ) -> message,
    timeout_message: fn(
      command.OperatorCommand,
      process.Subject(command.CommandResult),
    ) -> message,
    abort_timeout_message: fn(
      command.OperatorCommand,
      String,
      process.Subject(command.CommandResult),
    ) -> message,
  )
}

pub fn context(
  state state: state,
  daemon_subject daemon_subject: process.Subject(message),
  route_session_id route_session_id: fn(state, String) -> String,
  worker_for_session worker_for_session: fn(state, String) -> WorkerLookup,
  step_subject_for_run step_subject_for_run: fn(state, String) ->
    StepRunCommandSubject,
  step_subject_for_session step_subject_for_session: fn(state, String) ->
    Result(process.Subject(worker_command.Command), Nil),
  stop_for_abort stop_for_abort: fn(state, command.OperatorCommand, String) ->
    #(state, command.CommandResult),
  completion_message completion_message: fn(
    command.OperatorCommand,
    worker_command.Reply,
    process.Subject(command.CommandResult),
  ) -> message,
  timeout_message timeout_message: fn(
    command.OperatorCommand,
    process.Subject(command.CommandResult),
  ) -> message,
  abort_timeout_message abort_timeout_message: fn(
    command.OperatorCommand,
    String,
    process.Subject(command.CommandResult),
  ) -> message,
) -> Context(state, message) {
  Context(
    state: state,
    daemon_subject: daemon_subject,
    route_session_id: route_session_id,
    worker_for_session: worker_for_session,
    step_subject_for_run: step_subject_for_run,
    step_subject_for_session: step_subject_for_session,
    stop_for_abort: stop_for_abort,
    completion_message: completion_message,
    timeout_message: timeout_message,
    abort_timeout_message: abort_timeout_message,
  )
}

pub fn apply(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  reply: process.Subject(command.CommandResult),
) -> Outcome(state) {
  case operator_command {
    command.AbortSession(session_id) ->
      abort_session(context, operator_command, session_id, timeout_ms, reply)
    command.StopAfterCurrentTurn(session_id) ->
      route_worker_command(
        context,
        operator_command,
        session_id,
        timeout_ms,
        reply,
        fn(subject, worker_reply) {
          process.send(
            subject,
            worker_command.StopAfterCurrentTurn(worker_reply),
          )
        },
      )
    command.PromptSession(session_id, message) ->
      case control_command_handler.operator_prompt_too_large(message) {
        True ->
          Immediate(
            context.state,
            command.rejected(
              operator_command,
              "prompt_too_large",
              Some("operator prompt is too large"),
            ),
          )
        False ->
          route_worker_command(
            context,
            operator_command,
            session_id,
            timeout_ms,
            reply,
            fn(subject, worker_reply) {
              process.send(
                subject,
                worker_command.QueuePrompt(message, worker_reply),
              )
            },
          )
      }
    command.RespondUi(session_id, request_id, response) ->
      case control_command_handler.ui_response_too_large(response) {
        True ->
          Immediate(
            context.state,
            command.rejected(
              operator_command,
              "ui_response_too_large",
              Some("operator UI response value is too large"),
            ),
          )
        False ->
          route_worker_command(
            context,
            operator_command,
            session_id,
            timeout_ms,
            reply,
            fn(subject, worker_reply) {
              process.send(
                subject,
                worker_command.RespondToUi(request_id, response, worker_reply),
              )
            },
          )
      }
    _ ->
      Immediate(
        context.state,
        command.rejected(
          operator_command,
          "not_worker_command",
          Some("operator command is not a worker command"),
        ),
      )
  }
}

pub fn reply_result(
  operator_command: command.OperatorCommand,
  reply: worker_command.Reply,
) -> command.CommandResult {
  control_command_handler.worker_reply_to_command_result(
    operator_command,
    reply,
  )
}

pub fn timeout_result(
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  command.rejected(
    operator_command,
    "worker_command_timeout",
    Some("worker command timed out"),
  )
}

pub fn reject_sync_worker_command_for_operator(
  state: state,
  operator_command: command.OperatorCommand,
  _session_id: String,
  _timeout_ms: Int,
) -> #(state, command.CommandResult) {
  #(state, sync_worker_command_rejection(operator_command))
}

pub fn reject_sync_routed_worker_command(
  state: state,
  operator_command: command.OperatorCommand,
  _session_id: String,
  _timeout_ms: Int,
  _send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> #(state, command.CommandResult) {
  #(state, sync_worker_command_rejection(operator_command))
}

fn sync_worker_command_rejection(
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  command.rejected(
    operator_command,
    "worker_command_requires_async_reply",
    Some("worker command must be handled by the asynchronous reply path"),
  )
}

fn route_worker_command(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  operator_reply: process.Subject(command.CommandResult),
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> Outcome(state) {
  let session_id = context.route_session_id(context.state, session_id)
  case context.worker_for_session(context.state, session_id) {
    NoWorker ->
      route_step_command(
        context,
        operator_command,
        session_id,
        timeout_ms,
        operator_reply,
        send,
      )
    WorkerWithoutCommandSubject(run_id) ->
      case context.step_subject_for_run(context.state, run_id) {
        NoActiveStepCommandSubject ->
          Immediate(
            context.state,
            command.not_allowed(
              operator_command,
              "worker_command_subject_unavailable",
              Some("session worker does not accept operator commands"),
            ),
          )
        MultipleActiveStepCommandSubjects ->
          Immediate(
            context.state,
            command.not_allowed(
              operator_command,
              "multiple_step_command_subjects",
              Some(
                "multiple active step sessions accept operator commands; target a step session",
              ),
            ),
          )
        StepRunCommandSubjectFound(subject) ->
          send_worker_command(
            context,
            operator_command,
            timeout_ms,
            operator_reply,
            send,
            subject,
            context.timeout_message(operator_command, operator_reply),
          )
      }
    WorkerWithCommandSubject(subject) ->
      send_worker_command(
        context,
        operator_command,
        timeout_ms,
        operator_reply,
        send,
        subject,
        context.timeout_message(operator_command, operator_reply),
      )
  }
}

fn route_step_command(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  operator_reply: process.Subject(command.CommandResult),
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
) -> Outcome(state) {
  case context.step_subject_for_session(context.state, session_id) {
    Error(Nil) ->
      Immediate(
        context.state,
        command.not_found(operator_command, Some("session not found")),
      )
    Ok(subject) ->
      send_worker_command(
        context,
        operator_command,
        timeout_ms,
        operator_reply,
        send,
        subject,
        context.timeout_message(operator_command, operator_reply),
      )
  }
}

fn send_worker_command(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  timeout_ms: Int,
  operator_reply: process.Subject(command.CommandResult),
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
  subject: process.Subject(worker_command.Command),
  timeout_message: message,
) -> Outcome(state) {
  let daemon_subject = context.daemon_subject
  let completion_message = context.completion_message
  start_wait(
    daemon_subject,
    operator_command,
    operator_reply,
    timeout_ms,
    send,
    subject,
    completion_message,
    timeout_message,
  )
  Pending(context.state)
}

fn start_wait(
  daemon_subject: process.Subject(message),
  operator_command: command.OperatorCommand,
  operator_reply: process.Subject(command.CommandResult),
  timeout_ms: Int,
  send: fn(
    process.Subject(worker_command.Command),
    process.Subject(worker_command.Reply),
  ) -> Nil,
  worker_subject: process.Subject(worker_command.Command),
  completion_message: fn(
    command.OperatorCommand,
    worker_command.Reply,
    process.Subject(command.CommandResult),
  ) -> message,
  timeout_message: message,
) -> Nil {
  let wait_ms = control_command_handler.worker_command_timeout(timeout_ms)
  let _pid =
    process.spawn_unlinked(fn() {
      let worker_reply = process.new_subject()
      send(worker_subject, worker_reply)
      case process.receive(worker_reply, within: wait_ms) {
        Ok(reply) ->
          process.send(
            daemon_subject,
            completion_message(operator_command, reply, operator_reply),
          )
        Error(Nil) -> process.send(daemon_subject, timeout_message)
      }
      Nil
    })
  Nil
}

fn abort_session(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  operator_reply: process.Subject(command.CommandResult),
) -> Outcome(state) {
  let session_id = context.route_session_id(context.state, session_id)
  case context.worker_for_session(context.state, session_id) {
    NoWorker ->
      abort_step_session(
        context,
        operator_command,
        session_id,
        timeout_ms,
        operator_reply,
      )
    WorkerWithoutCommandSubject(run_id) ->
      abort_run_step_command(
        context,
        operator_command,
        session_id,
        run_id,
        timeout_ms,
        operator_reply,
      )
    WorkerWithCommandSubject(subject) ->
      send_worker_command(
        context,
        operator_command,
        timeout_ms,
        operator_reply,
        fn(subject, worker_reply) {
          process.send(subject, worker_command.Abort(worker_reply))
        },
        subject,
        context.abort_timeout_message(
          operator_command,
          session_id,
          operator_reply,
        ),
      )
  }
}

fn abort_run_step_command(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  session_id: String,
  run_id: String,
  timeout_ms: Int,
  operator_reply: process.Subject(command.CommandResult),
) -> Outcome(state) {
  case context.step_subject_for_run(context.state, run_id) {
    StepRunCommandSubjectFound(subject) ->
      send_worker_command(
        context,
        operator_command,
        timeout_ms,
        operator_reply,
        fn(subject, worker_reply) {
          process.send(subject, worker_command.Abort(worker_reply))
        },
        subject,
        context.abort_timeout_message(
          operator_command,
          session_id,
          operator_reply,
        ),
      )
    NoActiveStepCommandSubject | MultipleActiveStepCommandSubjects -> {
      let #(state, result) =
        context.stop_for_abort(context.state, operator_command, session_id)
      Immediate(state, result)
    }
  }
}

fn abort_step_session(
  context: Context(state, message),
  operator_command: command.OperatorCommand,
  session_id: String,
  timeout_ms: Int,
  operator_reply: process.Subject(command.CommandResult),
) -> Outcome(state) {
  case context.step_subject_for_session(context.state, session_id) {
    Error(Nil) ->
      Immediate(
        context.state,
        command.not_found(operator_command, Some("session not found")),
      )
    Ok(subject) ->
      send_worker_command(
        context,
        operator_command,
        timeout_ms,
        operator_reply,
        fn(subject, worker_reply) {
          process.send(subject, worker_command.Abort(worker_reply))
        },
        subject,
        context.timeout_message(operator_command, operator_reply),
      )
  }
}
