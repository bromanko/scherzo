import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/orchestrator/control_command_handler
import test_async

type TestState {
  TestState(paused: Bool, pending: Int, routed: Int)
}

type DispatcherCall {
  ReloadCalled(command.OperatorCommand)
  RetryCalled(command.OperatorCommand, command.IssueRef)
  ParkCalled(command.OperatorCommand, command.IssueRef, String)
  UnparkCalled(command.OperatorCommand, command.IssueRef)
  RunScheduleCalled(command.OperatorCommand, String)
  AbortCalled(command.OperatorCommand, String, Int)
  RouteCalled(command.OperatorCommand, String, Int)
}

fn base_context(
  state: TestState,
  log_subject: process.Subject(#(String, String, List(#(String, String)))),
  route_subject: process.Subject(String),
) -> control_command_handler.Context(TestState) {
  control_command_handler.Context(
    state: state,
    pending_claim_count: fn(state) { state.pending },
    set_paused: fn(state, paused) { TestState(..state, paused: paused) },
    reload_workflow: fn(state, operator_command) {
      #(state, command.applied(operator_command, Some("reloaded")))
    },
    retry_issue: fn(state, operator_command, _) {
      #(state, command.applied(operator_command, Some("retried")))
    },
    park_issue: fn(state, operator_command, _, _) {
      #(state, command.applied(operator_command, Some("parked")))
    },
    unpark_issue: fn(state, operator_command, _) {
      #(state, command.applied(operator_command, Some("unparked")))
    },
    run_schedule_now: fn(state, operator_command, _) {
      #(state, command.applied(operator_command, Some("scheduled")))
    },
    abort_session: fn(state, operator_command, _, _) {
      #(state, command.applied(operator_command, Some("aborted")))
    },
    route_worker_command: fn(
      state,
      operator_command,
      session_id,
      timeout_ms,
      send,
    ) {
      process.send(
        route_subject,
        session_id <> ":" <> int.to_string(timeout_ms),
      )
      let subject = process.new_subject()
      let reply = process.new_subject()
      send(subject, reply)
      #(
        TestState(..state, routed: state.routed + 1),
        command.queued(operator_command, Some("routed")),
      )
    },
    log_result: fn(_, result, fields) {
      process.send(log_subject, #(
        result.command,
        command.status_to_string(result.status),
        fields,
      ))
    },
  )
}

fn legacy_base_context(
  state: TestState,
  log_subject: process.Subject(#(String, String, List(#(String, String)))),
  route_subject: process.Subject(String),
) -> control_command_handler.Context(TestState) {
  legacy_context(
    state,
    fn(state) { state.pending },
    fn(state, paused) { TestState(..state, paused: paused) },
    fn(state, operator_command) {
      #(state, command.applied(operator_command, Some("reloaded")))
    },
    fn(state, operator_command, _) {
      #(state, command.applied(operator_command, Some("retried")))
    },
    fn(state, operator_command, _, _) {
      #(state, command.applied(operator_command, Some("parked")))
    },
    fn(state, operator_command, _) {
      #(state, command.applied(operator_command, Some("unparked")))
    },
    fn(state, operator_command, _, _) {
      #(state, command.applied(operator_command, Some("aborted")))
    },
    fn(state, operator_command, session_id, timeout_ms, send) {
      process.send(
        route_subject,
        session_id <> ":" <> int.to_string(timeout_ms),
      )
      let subject = process.new_subject()
      let reply = process.new_subject()
      send(subject, reply)
      #(
        TestState(..state, routed: state.routed + 1),
        command.queued(operator_command, Some("routed")),
      )
    },
    fn(_, result, fields) {
      process.send(log_subject, #(
        result.command,
        command.status_to_string(result.status),
        fields,
      ))
    },
  )
}

fn recording_context(
  state: TestState,
  log_subject: process.Subject(#(String, String, List(#(String, String)))),
  callback_subject: process.Subject(DispatcherCall),
  worker_subject: process.Subject(worker_command.Command),
) -> control_command_handler.Context(TestState) {
  control_command_handler.Context(
    state: state,
    pending_claim_count: fn(state) { state.pending },
    set_paused: fn(state, paused) { TestState(..state, paused: paused) },
    reload_workflow: fn(state, operator_command) {
      process.send(callback_subject, ReloadCalled(operator_command))
      #(state, command.applied(operator_command, Some("reloaded")))
    },
    retry_issue: fn(state, operator_command, issue_ref) {
      process.send(callback_subject, RetryCalled(operator_command, issue_ref))
      #(state, command.applied(operator_command, Some("retried")))
    },
    park_issue: fn(state, operator_command, issue_ref, reason) {
      process.send(
        callback_subject,
        ParkCalled(operator_command, issue_ref, reason),
      )
      #(state, command.applied(operator_command, Some("parked")))
    },
    unpark_issue: fn(state, operator_command, issue_ref) {
      process.send(callback_subject, UnparkCalled(operator_command, issue_ref))
      #(state, command.applied(operator_command, Some("unparked")))
    },
    run_schedule_now: fn(state, operator_command, job_id) {
      process.send(
        callback_subject,
        RunScheduleCalled(operator_command, job_id),
      )
      #(state, command.applied(operator_command, Some("scheduled")))
    },
    abort_session: fn(state, operator_command, session_id, timeout_ms) {
      process.send(
        callback_subject,
        AbortCalled(operator_command, session_id, timeout_ms),
      )
      #(state, command.applied(operator_command, Some("aborted")))
    },
    route_worker_command: fn(
      state,
      operator_command,
      session_id,
      timeout_ms,
      send,
    ) {
      process.send(
        callback_subject,
        RouteCalled(operator_command, session_id, timeout_ms),
      )
      let reply = process.new_subject()
      send(worker_subject, reply)
      #(
        TestState(..state, routed: state.routed + 1),
        command.queued(operator_command, Some("routed")),
      )
    },
    log_result: fn(_, result, fields) {
      process.send(log_subject, #(
        result.command,
        command.status_to_string(result.status),
        fields,
      ))
    },
  )
}

pub fn control_command_handler_pause_resume_logs_results_test() {
  let log_subject = process.new_subject()
  let route_subject = process.new_subject()
  let context =
    base_context(
      TestState(paused: False, pending: 3, routed: 0),
      log_subject,
      route_subject,
    )

  let #(state, result) =
    control_command_handler.apply(context, command.PauseDispatch, 1000)
  assert state.paused
  assert result.command == "pause"
  assert command.status_to_string(result.status) == "applied"
  let assert Ok(#("pause", "applied", pause_fields)) =
    process.receive(log_subject, within: 100)
  assert list.contains(pause_fields, #("pending_claims", "3"))

  let context = base_context(state, log_subject, route_subject)
  let #(state, result) =
    control_command_handler.apply(context, command.ResumeDispatch, 1000)
  assert !state.paused
  assert result.command == "resume"
  let assert Ok(#("resume", "applied", [])) =
    process.receive(log_subject, within: 100)
}

pub fn control_command_handler_logs_reload_transition_test() {
  let log_subject = process.new_subject()
  let route_subject = process.new_subject()
  let context =
    base_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      route_subject,
    )

  let #(_, result) =
    control_command_handler.apply(context, command.ReloadWorkflow, 1000)
  assert result.command == "reload"
  let assert Ok(#("reload", "applied", [])) =
    process.receive(log_subject, within: 100)
}

pub fn control_command_handler_logs_legacy_context_transition_test() {
  let log_subject = process.new_subject()
  let route_subject = process.new_subject()
  let context =
    legacy_base_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      route_subject,
    )

  let #(_, result) =
    control_command_handler.apply(context, command.ReloadWorkflow, 1000)
  assert result.command == "reload"
  let assert Ok(#("reload", "applied", [])) =
    process.receive(log_subject, within: 100)
}

pub fn control_command_handler_rejects_run_schedule_now_for_legacy_context_test() {
  let log_subject = process.new_subject()
  let route_subject = process.new_subject()
  let context =
    legacy_base_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      route_subject,
    )

  let #(_, result) =
    control_command_handler.apply(
      context,
      command.RunScheduleNow("nightly"),
      1000,
    )
  assert result.command == "schedule_run_now"
  assert result.status == command.Rejected("daemon_code_stale")
  let assert Ok(#("schedule_run_now", "rejected", [])) =
    process.receive(log_subject, within: 100)
}

pub fn control_command_handler_delegates_state_commands_to_callbacks_test() {
  let log_subject = process.new_subject()
  let callback_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let context =
    recording_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      callback_subject,
      worker_subject,
    )

  let #(_, reload_result) =
    control_command_handler.apply(context, command.ReloadWorkflow, 1000)
  assert reload_result.command == "reload"
  assert process.receive(callback_subject, within: 100)
    == Ok(ReloadCalled(command.ReloadWorkflow))

  let retry_ref = command.IssueIdentifier("SCH-1")
  let #(_, retry_result) =
    control_command_handler.apply(context, command.RetryIssue(retry_ref), 1001)
  assert retry_result.command == "retry"
  assert process.receive(callback_subject, within: 100)
    == Ok(RetryCalled(command.RetryIssue(retry_ref), retry_ref))

  let park_ref = command.IssueId("issue-1")
  let #(_, park_result) =
    control_command_handler.apply(
      context,
      command.ParkIssue(park_ref, "operator requested"),
      1002,
    )
  assert park_result.command == "park"
  assert process.receive(callback_subject, within: 100)
    == Ok(ParkCalled(
      command.ParkIssue(park_ref, "operator requested"),
      park_ref,
      "operator requested",
    ))

  let unpark_ref = command.IssueIdentifier("SCH-2")
  let #(_, unpark_result) =
    control_command_handler.apply(
      context,
      command.UnparkIssue(unpark_ref),
      1003,
    )
  assert unpark_result.command == "unpark"
  assert process.receive(callback_subject, within: 100)
    == Ok(UnparkCalled(command.UnparkIssue(unpark_ref), unpark_ref))
}

pub fn control_command_handler_delegates_abort_timeout_to_callback_test() {
  let log_subject = process.new_subject()
  let callback_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let context =
    recording_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      callback_subject,
      worker_subject,
    )

  let #(state, result) =
    control_command_handler.apply(
      context,
      command.AbortSession("session-1"),
      913,
    )

  assert state.routed == 0
  assert result.command == "abort"
  assert result.status == command.Applied
  assert process.receive(callback_subject, within: 100)
    == Ok(AbortCalled(command.AbortSession("session-1"), "session-1", 913))
  test_async.assert_no_extra_message_within(worker_subject, 50)
}

pub fn control_command_handler_rejects_too_large_prompt_without_routing_test() {
  let log_subject = process.new_subject()
  let route_subject = process.new_subject()
  let context =
    base_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      route_subject,
    )
  let too_large =
    string.repeat("x", times: worker_command.max_operator_prompt_chars + 1)

  let #(state, result) =
    control_command_handler.apply(
      context,
      command.PromptSession("session-1", too_large),
      1000,
    )

  assert state.routed == 0
  assert result.command == "prompt"
  assert result.status == command.Rejected("prompt_too_large")
  test_async.assert_no_extra_message_within(route_subject, 50)
  let assert Ok(#("prompt", "rejected", [])) =
    process.receive(log_subject, within: 100)
}

pub fn control_command_handler_rejects_too_large_ui_response_without_routing_test() {
  let log_subject = process.new_subject()
  let route_subject = process.new_subject()
  let context =
    base_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      route_subject,
    )
  let too_large =
    string.repeat("x", times: worker_command.max_operator_ui_value_chars + 1)

  let #(state, result) =
    control_command_handler.apply(
      context,
      command.RespondUi("session-1", "request-1", command.UiValue(too_large)),
      1000,
    )

  assert state.routed == 0
  assert result.command == "respond_ui"
  assert result.status == command.Rejected("ui_response_too_large")
  test_async.assert_no_extra_message_within(route_subject, 50)
  let assert Ok(#("respond_ui", "rejected", [])) =
    process.receive(log_subject, within: 100)
}

pub fn control_command_handler_routes_worker_commands_test() {
  let log_subject = process.new_subject()
  let callback_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let context =
    recording_context(
      TestState(paused: False, pending: 0, routed: 0),
      log_subject,
      callback_subject,
      worker_subject,
    )

  let #(state, result) =
    control_command_handler.apply(
      context,
      command.StopAfterCurrentTurn("session-stop"),
      250,
    )
  assert state.routed == 1
  assert result.status == command.Queued
  assert process.receive(callback_subject, within: 100)
    == Ok(RouteCalled(
      command.StopAfterCurrentTurn("session-stop"),
      "session-stop",
      250,
    ))
  let assert Ok(worker_command.StopAfterCurrentTurn(_)) =
    process.receive(worker_subject, within: 100)

  let context =
    recording_context(state, log_subject, callback_subject, worker_subject)
  let #(state, result) =
    control_command_handler.apply(
      context,
      command.PromptSession("session-prompt", "hello"),
      251,
    )
  assert state.routed == 2
  assert result.status == command.Queued
  assert process.receive(callback_subject, within: 100)
    == Ok(RouteCalled(
      command.PromptSession("session-prompt", "hello"),
      "session-prompt",
      251,
    ))
  let assert Ok(worker_command.QueuePrompt("hello", _)) =
    process.receive(worker_subject, within: 100)

  let context =
    recording_context(state, log_subject, callback_subject, worker_subject)
  let response = command.UiValue("accepted")
  let #(state, result) =
    control_command_handler.apply(
      context,
      command.RespondUi("session-ui", "request-1", response),
      252,
    )
  assert state.routed == 3
  assert result.status == command.Queued
  assert process.receive(callback_subject, within: 100)
    == Ok(RouteCalled(
      command.RespondUi("session-ui", "request-1", response),
      "session-ui",
      252,
    ))
  let assert Ok(worker_command.RespondToUi(
    "request-1",
    command.UiValue("accepted"),
    _,
  )) = process.receive(worker_subject, within: 100)
}

pub fn control_command_handler_worker_reply_and_timeout_helpers_test() {
  assert control_command_handler.worker_command_timeout(1000) == 500
  assert control_command_handler.worker_command_timeout(100) == 75
  assert control_command_handler.worker_command_timeout(2) == 1

  let result =
    control_command_handler.worker_reply_to_command_result(
      command.StopAfterCurrentTurn("session-1"),
      worker_command.NotAllowed("busy", Some("not now")),
    )

  assert result.command == "stop_after_current_turn"
  assert result.status == command.NotAllowed("busy")
  assert result.message == Some("not now")
}

@external(erlang, "scherzo_control_command_handler_test_ffi", "legacy_context")
fn legacy_context(
  state: TestState,
  pending_claim_count: fn(TestState) -> Int,
  set_paused: fn(TestState, Bool) -> TestState,
  reload_workflow: fn(TestState, command.OperatorCommand) ->
    #(TestState, command.CommandResult),
  retry_issue: fn(TestState, command.OperatorCommand, command.IssueRef) ->
    #(TestState, command.CommandResult),
  park_issue: fn(TestState, command.OperatorCommand, command.IssueRef, String) ->
    #(TestState, command.CommandResult),
  unpark_issue: fn(TestState, command.OperatorCommand, command.IssueRef) ->
    #(TestState, command.CommandResult),
  abort_session: fn(TestState, command.OperatorCommand, String, Int) ->
    #(TestState, command.CommandResult),
  route_worker_command: fn(
    TestState,
    command.OperatorCommand,
    String,
    Int,
    fn(
      process.Subject(worker_command.Command),
      process.Subject(worker_command.Reply),
    ) -> Nil,
  ) -> #(TestState, command.CommandResult),
  log_result: fn(TestState, command.CommandResult, List(#(String, String))) ->
    Nil,
) -> control_command_handler.Context(TestState)
