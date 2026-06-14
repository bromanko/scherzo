import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/orchestrator/effects/interpreter as transition_interpreter
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/outbox_effects
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/review_lane_preflight
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/state/outbox
import scherzo/state/recovery
import scherzo/structured_output
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/idempotency
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

pub type Effect {
  FetchCandidates(generation: Int, tracker_adapter: adapter.TrackerAdapter)
  RefreshRunning(
    generation: Int,
    ids: List(String),
    tracker_adapter: adapter.TrackerAdapter,
  )
  RefreshRetry(
    issue_id: String,
    generation: Int,
    tracker_adapter: adapter.TrackerAdapter,
  )
  ValidateDispatchClaim(
    issue_id: String,
    generation: Int,
    tracker_adapter: adapter.TrackerAdapter,
  )
  ReviewLanePreflight(request: transition_effects.ReviewLanePreflightRequest)
  ClaimIssue(
    outbox: outbox_effects.Intent,
    task_ref: task.TaskRef,
    issue: tracker_issue.Issue,
    workspace_path: String,
    run_id: String,
    capability: adapter.HandoffCapability,
  )
  ReportSuccess(
    outbox: outbox_effects.Intent,
    task_ref: task.TaskRef,
    issue_id: String,
    issue: tracker_issue.Issue,
    success: agent_types.WorkerSuccess,
    run_id: String,
    workflow_id: String,
    capability: adapter.HandoffCapability,
  )
  ReportFailure(
    outbox: outbox_effects.Intent,
    task_ref: task.TaskRef,
    issue_id: String,
    issue: tracker_issue.Issue,
    failure: agent_types.WorkerFailure,
    run_id: String,
    workflow_id: String,
    capability: adapter.HandoffCapability,
  )
  ReportPark(
    outbox: outbox_effects.Intent,
    report: adapter.ParkReport,
    capability: adapter.HandoffCapability,
  )
  ReportInvalidWorkflow(
    outbox: outbox_effects.Intent,
    issue: tracker_issue.Issue,
    violation: workflow_policy.IssueWorkflowViolation,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    contract_config: config_types.LinearContractConfig,
    comments: Option(adapter.CommentCapability),
    state_transitions: Option(adapter.StateTransitionCapability),
  )
  ReplayOutbox(
    outbox: recovery.OutboxReplay,
    comments: Option(adapter.CommentCapability),
    state_transitions: Option(adapter.StateTransitionCapability),
  )
  ReportScheduledFailure(
    generation: Int,
    publication: adapter.ScheduledFailurePublication,
    capability: adapter.ScheduledFailureCapability,
  )
  CleanupWorkspace(
    root: String,
    workspace_path: String,
    hooks: config_types.HooksConfig,
    cleanup: fn(String, String, config_types.HooksConfig) ->
      Result(Nil, error.WorkspaceError),
  )
}

pub type DispatchClaimValidationError {
  DispatchValidationTrackerError(error.TrackerError)
  DispatchValidationMissingIssue
  DispatchValidationDuplicateIssue
  DispatchValidationIdMismatch(expected: String, actual: String)
}

pub type InvalidWorkflowReportOutcome {
  InvalidWorkflowReportNoop
  InvalidWorkflowReportComment
  InvalidWorkflowReportState
  InvalidWorkflowReportCommentAndState
}

pub type EffectResult {
  CandidateFetchFinished(
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  )
  RunningRefreshFinished(
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  )
  RetryRefreshFinished(
    String,
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  )
  DispatchClaimValidationFinished(
    issue_id: String,
    generation: Int,
    result: Result(tracker_issue.Issue, DispatchClaimValidationError),
  )
  ReviewLanePreflightFinished(
    task_identity: identity.TaskIdentity,
    issue_id: String,
    generation: Int,
    workflow_id: String,
    result: review_lane_preflight.PreflightResult,
  )
  HandoffClaimFinished(
    outbox: outbox_effects.Intent,
    issue_id: String,
    run_id: String,
    result: Result(Nil, error.TrackerError),
  )
  HandoffSuccessFinished(
    outbox: outbox_effects.Intent,
    issue_id: String,
    run_id: String,
    result: Result(Nil, error.TrackerError),
  )
  HandoffFailureFinished(
    outbox: outbox_effects.Intent,
    issue_id: String,
    run_id: String,
    result: Result(Nil, error.TrackerError),
  )
  HandoffParkFinished(
    outbox: outbox_effects.Intent,
    issue_id: String,
    result: Result(Nil, error.TrackerError),
  )
  InvalidWorkflowReportFinished(
    outbox: outbox_effects.Intent,
    issue_id: String,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    result: Result(InvalidWorkflowReportOutcome, error.TrackerError),
  )
  OutboxReplayFinished(
    outbox: recovery.OutboxReplay,
    result: Result(Nil, error.TrackerError),
  )
  ScheduledFailureReportFinished(
    generation: Int,
    publication: adapter.ScheduledFailurePublication,
    result: Result(adapter.ScheduledFailureReceipt, adapter.TrackerError),
  )
  CleanupFinished(String, Result(Nil, error.WorkspaceError))
}

pub type Completion {
  Finished(id: Int, result: EffectResult)
  Crashed(id: Int, effect: Effect, reason: String)
}

const transition_runner_message_limit = 32

pub fn reply_snapshot(
  runtime: orchestrator_state.RuntimeState,
  reply: process.Subject(orchestrator_state.RuntimeState),
) -> Nil {
  let transition_state =
    transition_types.State(
      runtime: runtime,
      workers: transition_types.new_worker_directory(),
      pending_claims: dict.new(),
      pending_dispatch_validations: dict.new(),
      pending_review_lane_preflights: dict.new(),
      lifecycle: task_lifecycle.new(),
      retry_refresh_generations: dict.new(),
      next_dispatch_validation_generation: 1,
      next_session_sequence: 1,
    )
  let shell =
    transition_interpreter.new_production_shell_state(
      data: Nil,
      append_ledger: fn(data, _) { #(data, Ok(Nil)) },
      now_ms: fn(_) { 0 },
      log_effect: fn(data, _, _, _) { data },
      start_worker: fn(data, _) { #(data, Ok(Nil)) },
      reply_snapshot: fn(data, snapshot) {
        process.send(reply, snapshot)
        data
      },
      mark_poll_in_flight: fn(data, _) { data },
      schedule_next_poll: fn(data) { data },
      fetch_candidates: fn(data, _) { data },
      begin_dispatch_validation: fn(data, _, _) { data },
      begin_review_lane_preflight: fn(data, _) { data },
      reserve_session_sequence: fn(data, _) { data },
      claim_issue: fn(data, _, _, _, _) { data },
      report_invalid_workflow: fn(data, _, _, _, _) { data },
      replay_outbox: fn(data, _) { data },
      remove_retry_timer: fn(data, _) { data },
      finish_retry_refresh: fn(data, _) { data },
      defer_retry_timer: fn(data, _, _, _) { data },
      begin_retry_refresh: fn(data, _, _) { data },
      schedule_retry_timer: fn(data, _, _, _, _) { data },
      schedule_recovered_retry_timer: fn(data, _, _, _) { data },
      cancel_retry_timer: fn(data, _, _, _) { data },
      release_claim: fn(data, _) { data },
      clear_recovery: fn(data, _) { data },
      worker_start_failed: fn(data, _, _) { data },
      remove_worker: fn(data, _, _) { data },
      publish_worker_exited: fn(data, _) { data },
      report_worker_success: fn(data, _, _) { data },
      report_worker_failure: fn(data, _, _) { data },
      cleanup_workspace: fn(data, _) { data },
      park_issue: fn(data, _, _) { data },
      report_park: fn(data, _) { data },
      stop_worker: fn(data, _, _) { data },
      stop_worker_after_issue_refresh: fn(data, _, _) { data },
      register_yaml_step_started: fn(data, _, _) { data },
      finish_yaml_step_route: fn(data, _) { data },
      finish_yaml_step_session: fn(data, _, _) { data },
      finish_yaml_step_sessions_for_run: fn(data, _, _) { data },
      clear_yaml_step_routes_for_run: fn(data, _) { data },
      mark_yaml_run_stopping: fn(data, _, _) { data },
      shutdown_runtime: fn(data, _) { data },
      set_operator_paused: fn(data, _) { data },
      apply_operator_command: fn(data, request) {
        #(
          data,
          command.rejected(
            request.operator_command,
            "operator_command_unhandled",
            None,
          ),
          [],
        )
      },
      finish_operator_command: fn(data, _, _) { #(data, []) },
      report_park_effect: fn(data, _, _, _, _, _) { data },
    )
  let transition_runner.RunResult(exhausted: _, ..) =
    transition_runner.run(
      state: transition_state,
      shell: shell,
      messages: [transition_types.SnapshotRequested],
      max_messages: transition_runner_message_limit,
    )
  Nil
}

pub type Dependencies {
  Dependencies(max_concurrent: Int, notify: fn(Completion) -> Nil)
}

pub opaque type Handle {
  Handle(subject: process.Subject(Message), pid: process.Pid)
}

type Message {
  Enqueue(Effect)
  WorkerFinished(Int, EffectResult)
  WorkerDown(process.Down)
  Shutdown(process.Subject(Nil))
}

type QueuedEffect {
  QueuedEffect(id: Int, effect: Effect)
}

type InFlightEffect {
  InFlightEffect(
    id: Int,
    effect: Effect,
    pid: process.Pid,
    monitor: process.Monitor,
  )
}

type State {
  State(
    subject: process.Subject(Message),
    next_id: Int,
    queue: List(QueuedEffect),
    in_flight: Dict(Int, InFlightEffect),
    monitors: Dict(process.Monitor, Int),
    max_concurrent: Int,
    notify: fn(Completion) -> Nil,
    accepting: Bool,
    shutdown_reply: Option(process.Subject(Nil)),
  )
}

pub fn start(dependencies: Dependencies) -> Result(Handle, Nil) {
  let max_concurrent = case dependencies.max_concurrent <= 0 {
    True -> 1
    False -> dependencies.max_concurrent
  }
  let builder =
    actor.new_with_initialiser(1000, fn(subject) {
      let state =
        State(
          subject: subject,
          next_id: 1,
          queue: [],
          in_flight: dict.new(),
          monitors: dict.new(),
          max_concurrent: max_concurrent,
          notify: dependencies.notify,
          accepting: True,
          shutdown_reply: None,
        )
      let selector =
        process.new_selector()
        |> process.select(subject)
        |> process.select_monitors(WorkerDown)
      actor.initialised(state)
      |> actor.selecting(selector)
      |> actor.returning(Handle(subject, process.self()))
      |> Ok
    })
    |> actor.on_message(handle_message)

  case actor.start(builder) {
    Ok(started) -> {
      // Owners monitor the runner explicitly; avoid linked exits bypassing
      // their Down handling path.
      process.unlink(started.pid)
      Ok(started.data)
    }
    Error(_) -> Error(Nil)
  }
}

pub fn monitor(handle: Handle) -> process.Monitor {
  let Handle(_, pid) = handle
  process.monitor(pid)
}

pub fn is_alive(handle: Handle) -> Bool {
  let Handle(_, pid) = handle
  process.is_alive(pid)
}

pub fn enqueue(handle: Handle, effect: Effect) -> Nil {
  let Handle(subject, _) = handle
  process.send(subject, Enqueue(effect))
}

pub fn shutdown(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil) {
  let Handle(subject, _) = handle
  let reply = process.new_subject()
  process.send(subject, Shutdown(reply))
  process.receive(reply, within: timeout_ms)
}

pub fn effect_kind(effect: Effect) -> String {
  case effect {
    FetchCandidates(_, _) -> "fetch_candidates"
    RefreshRunning(_, _, _) -> "refresh_running"
    RefreshRetry(_, _, _) -> "refresh_retry"
    ValidateDispatchClaim(_, _, _) -> "validate_dispatch_claim"
    ReviewLanePreflight(_) -> "review_lane_preflight"
    ClaimIssue(_, _, _, _, _, _) -> "claim_issue"
    ReportSuccess(_, _, _, _, _, _, _, _) -> "report_success"
    ReportFailure(_, _, _, _, _, _, _, _) -> "report_failure"
    ReportPark(_, _, _) -> "report_park"
    ReportInvalidWorkflow(_, _, _, _, _, _, _, _) -> "report_invalid_workflow"
    ReplayOutbox(_, _, _) -> "replay_outbox"
    ReportScheduledFailure(_, _, _) -> "report_scheduled_failure"
    CleanupWorkspace(_, _, _, _) -> "cleanup_workspace"
  }
}

fn handle_message(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case message {
    Enqueue(effect) -> actor.continue(enqueue_effect(state, effect))
    WorkerFinished(id, result) ->
      continue_or_stop_when_shutdown_drained(finish_effect(state, id, result))
    WorkerDown(down) ->
      continue_or_stop_when_shutdown_drained(handle_worker_down(state, down))
    Shutdown(reply) ->
      continue_or_stop_when_shutdown_drained(begin_shutdown(state, reply))
  }
}

fn enqueue_effect(state: State, effect: Effect) -> State {
  case state.accepting {
    False -> state
    True -> {
      let queued = QueuedEffect(id: state.next_id, effect: effect)
      State(
        ..state,
        next_id: state.next_id + 1,
        queue: list.append(state.queue, [queued]),
      )
      |> drain
    }
  }
}

fn drain(state: State) -> State {
  case dict.size(state.in_flight) >= state.max_concurrent {
    True -> state
    False ->
      case next_startable_effect(state.queue, preflight_in_flight(state)) {
        Error(Nil) -> state
        Ok(#(queued, rest)) -> {
          let #(pid, monitor) = spawn_effect_worker(state.subject, queued)
          let in_flight =
            InFlightEffect(
              id: queued.id,
              effect: queued.effect,
              pid: pid,
              monitor: monitor,
            )
          State(
            ..state,
            queue: rest,
            in_flight: dict.insert(state.in_flight, queued.id, in_flight),
            monitors: dict.insert(state.monitors, monitor, queued.id),
          )
          |> drain
        }
      }
  }
}

fn next_startable_effect(
  queue: List(QueuedEffect),
  review_lane_preflight_busy: Bool,
) -> Result(#(QueuedEffect, List(QueuedEffect)), Nil) {
  next_startable_effect_loop(queue, review_lane_preflight_busy, [])
}

fn next_startable_effect_loop(
  queue: List(QueuedEffect),
  review_lane_preflight_busy: Bool,
  skipped: List(QueuedEffect),
) -> Result(#(QueuedEffect, List(QueuedEffect)), Nil) {
  case queue {
    [] -> Error(Nil)
    [queued, ..rest] ->
      case effect_startable(queued.effect, review_lane_preflight_busy) {
        True -> Ok(#(queued, list.append(list.reverse(skipped), rest)))
        False ->
          next_startable_effect_loop(rest, review_lane_preflight_busy, [
            queued,
            ..skipped
          ])
      }
  }
}

fn effect_startable(effect: Effect, review_lane_preflight_busy: Bool) -> Bool {
  case effect, review_lane_preflight_busy {
    ReviewLanePreflight(_), True -> False
    _, _ -> True
  }
}

fn preflight_in_flight(state: State) -> Bool {
  state.in_flight
  |> dict.values
  |> list.any(fn(in_flight) {
    case in_flight.effect {
      ReviewLanePreflight(_) -> True
      _ -> False
    }
  })
}

fn spawn_effect_worker(
  subject: process.Subject(Message),
  queued: QueuedEffect,
) -> #(process.Pid, process.Monitor) {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let start = process.new_subject()
      process.send(ready, start)
      case process.receive(start, within: 5000) {
        Ok(Nil) ->
          process.send(
            subject,
            WorkerFinished(queued.id, run_side_effect(queued.effect)),
          )
        Error(Nil) -> Nil
      }
    })
  case process.receive(ready, within: 1000) {
    Ok(start) -> {
      let monitor = process.monitor(pid)
      process.send(start, Nil)
      #(pid, monitor)
    }
    Error(Nil) -> #(pid, process.monitor(pid))
  }
}

fn finish_effect(state: State, id: Int, result: EffectResult) -> State {
  case dict.get(state.in_flight, id) {
    Error(Nil) -> state
    Ok(in_flight) -> {
      process.demonitor_process(in_flight.monitor)
      let state =
        State(
          ..state,
          in_flight: dict.delete(state.in_flight, id),
          monitors: dict.delete(state.monitors, in_flight.monitor),
        )
      state.notify(Finished(id: id, result: result))
      drain(state)
    }
  }
}

fn handle_worker_down(state: State, down: process.Down) -> State {
  case down {
    process.ProcessDown(monitor, _, reason) ->
      case dict.get(state.monitors, monitor) {
        Error(Nil) -> state
        Ok(id) ->
          case dict.get(state.in_flight, id) {
            Error(Nil) ->
              State(..state, monitors: dict.delete(state.monitors, monitor))
            Ok(in_flight) -> {
              let reason = down_reason(reason)
              let state =
                State(
                  ..state,
                  in_flight: dict.delete(state.in_flight, id),
                  monitors: dict.delete(state.monitors, monitor),
                )
              state.notify(Crashed(
                id: id,
                effect: in_flight.effect,
                reason: reason,
              ))
              drain(state)
            }
          }
      }
    process.PortDown(_, _, _) -> state
  }
}

fn down_reason(reason: process.ExitReason) -> String {
  case reason {
    process.Normal -> "side_effect_exited_without_result"
    process.Killed -> "side_effect_killed"
    process.Abnormal(_) -> "side_effect_crashed"
  }
}

fn begin_shutdown(state: State, reply: process.Subject(Nil)) -> State {
  State(..state, accepting: False, queue: [], shutdown_reply: Some(reply))
}

fn continue_or_stop_when_shutdown_drained(
  state: State,
) -> actor.Next(State, Message) {
  case state.shutdown_reply {
    Some(reply) ->
      case dict.size(state.in_flight) == 0 {
        True -> {
          process.send(reply, Nil)
          actor.stop()
        }
        False -> actor.continue(state)
      }
    None -> actor.continue(state)
  }
}

fn normalize_dispatch_claim_validation(
  expected_issue_id: String,
  result: Result(List(tracker_issue.Issue), error.TrackerError),
) -> Result(tracker_issue.Issue, DispatchClaimValidationError) {
  case result {
    Error(err) -> Error(DispatchValidationTrackerError(err))
    Ok([]) -> Error(DispatchValidationMissingIssue)
    Ok([issue]) ->
      case issue.id == expected_issue_id {
        True -> Ok(issue)
        False ->
          Error(DispatchValidationIdMismatch(
            expected: expected_issue_id,
            actual: issue.id,
          ))
      }
    Ok([_, ..]) -> Error(DispatchValidationDuplicateIssue)
  }
}

fn adapter_result(
  result: Result(a, adapter.TrackerError),
) -> Result(a, error.TrackerError) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(adapter_error_to_tracker_error(err))
  }
}

fn fetch_candidate_issues(
  tracker_adapter: adapter.TrackerAdapter,
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  adapter_result(adapter.fetch_runtime_candidate_issues(tracker_adapter))
}

fn refresh_issue_states_by_ids(
  tracker_adapter: adapter.TrackerAdapter,
  ids: List(String),
) -> Result(List(tracker_issue.Issue), error.TrackerError) {
  adapter_result(adapter.refresh_runtime_issues_by_ids(tracker_adapter, ids))
}

fn adapter_error_to_tracker_error(
  err: adapter.TrackerError,
) -> error.TrackerError {
  error.LinearApiRequest(adapter_error_message(err))
}

fn adapter_error_message(err: adapter.TrackerError) -> String {
  case err {
    adapter.Unauthorized(message) -> message
    adapter.NotFound(ref) -> "task not found: " <> ref.remote_id
    adapter.Transient(message) -> message
    adapter.Permanent(message) -> message
    adapter.UnsupportedCapability(capability) ->
      "unsupported tracker capability: " <> capability
    adapter.DecodeFailed(message) -> message
  }
}

fn report_invalid_workflow(
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  contract_config: config_types.LinearContractConfig,
  comments: Option(adapter.CommentCapability),
  state_transitions: Option(adapter.StateTransitionCapability),
  marker: Option(String),
) -> Result(InvalidWorkflowReportOutcome, error.TrackerError) {
  let comment_enabled = contract_config.comment_on_invalid_workflow
  let state_target =
    config_types.normalized_invalid_workflow_state_target(contract_config)
  case comment_enabled, state_target {
    False, None -> Ok(InvalidWorkflowReportNoop)
    True, None -> {
      use Nil <- try_tracker_adapter(post_invalid_workflow_comment(
        issue,
        violation,
        contract_config,
        comments,
        marker,
      ))
      Ok(InvalidWorkflowReportComment)
    }
    False, Some(state_target) -> {
      use Nil <- try_tracker_adapter(transition_invalid_workflow_state(
        issue,
        state_target,
        state_transitions,
      ))
      Ok(InvalidWorkflowReportState)
    }
    True, Some(state_target) -> {
      use Nil <- try_tracker_adapter(post_invalid_workflow_comment(
        issue,
        violation,
        contract_config,
        comments,
        marker,
      ))
      use Nil <- try_tracker_adapter(transition_invalid_workflow_state(
        issue,
        state_target,
        state_transitions,
      ))
      Ok(InvalidWorkflowReportCommentAndState)
    }
  }
}

fn post_invalid_workflow_comment(
  issue: tracker_issue.Issue,
  violation: workflow_policy.IssueWorkflowViolation,
  contract_config: config_types.LinearContractConfig,
  comments: Option(adapter.CommentCapability),
  marker: Option(String),
) -> Result(Nil, adapter.TrackerError) {
  case comments {
    None -> Error(adapter.UnsupportedCapability("comments"))
    Some(comments) -> {
      let body =
        workflow_policy.violation_comment(
          issue.identifier,
          violation,
          contract_config,
        )
      let body = case marker {
        Some(marker) -> idempotency.append_marker(body, marker)
        None -> body
      }
      use _ <- try_adapter(
        comments.post_or_update(adapter.CommentRequest(
          task: task.from_legacy_issue(issue).ref,
          body: body,
          mode: adapter.CreateOnly,
        )),
      )
      Ok(Nil)
    }
  }
}

fn transition_invalid_workflow_state(
  issue: tracker_issue.Issue,
  state_target: config_types.InvalidWorkflowStateTarget,
  state_transitions: Option(adapter.StateTransitionCapability),
) -> Result(Nil, adapter.TrackerError) {
  case state_transitions {
    None -> Error(adapter.UnsupportedCapability("state_transitions"))
    Some(state_transitions) -> {
      let state_ref =
        config_types.invalid_workflow_state_target_value(state_target)
      let target_state_id = case state_target {
        config_types.InvalidWorkflowStateId(value) -> Some(value)
        config_types.InvalidWorkflowStateName(_) -> None
      }
      use _ <- try_adapter(
        state_transitions.transition(adapter.StateTransitionRequest(
          task: task.from_legacy_issue(issue).ref,
          target_state_id: target_state_id,
          target_state_name: state_ref,
          reason: "invalid_workflow",
        )),
      )
      Ok(Nil)
    }
  }
}

fn replay_outbox_update(
  outbox_replay: recovery.OutboxReplay,
  comments: Option(adapter.CommentCapability),
  state_transitions: Option(adapter.StateTransitionCapability),
) -> Result(Nil, error.TrackerError) {
  let recovery.OutboxReplay(_, task_ref, _, _, payload_json) = outbox_replay
  let task_ref = outbox_effects.task_ref_from_fields(task_ref)
  use payload <- result.try(
    case outbox.decode_tracker_update_payload(payload_json) {
      Ok(payload) -> Ok(payload)
      Error(_) -> Error(error.LinearUnknownPayload("invalid outbox payload"))
    },
  )
  use _ <- result.try(replay_comment(task_ref, payload, comments))
  replay_state_transition(task_ref, payload, state_transitions)
}

fn replay_comment(
  task_ref: task.TaskRef,
  payload: outbox.TrackerUpdatePayload,
  comments: Option(adapter.CommentCapability),
) -> Result(Nil, error.TrackerError) {
  let outbox.TrackerUpdatePayload(marker: marker, body: body, ..) = payload
  case string_is_empty(body) {
    True -> Ok(Nil)
    False ->
      case comments {
        None ->
          Error(
            adapter_error_to_tracker_error(adapter.UnsupportedCapability(
              "comments",
            )),
          )
        Some(comments) -> {
          use _ <- try_tracker_adapter(
            comments.post_or_update(adapter.CommentRequest(
              task: task_ref,
              body: idempotency.append_marker(body, marker),
              mode: adapter.CreateOnly,
            )),
          )
          Ok(Nil)
        }
      }
  }
}

fn replay_state_transition(
  task_ref: task.TaskRef,
  payload: outbox.TrackerUpdatePayload,
  state_transitions: Option(adapter.StateTransitionCapability),
) -> Result(Nil, error.TrackerError) {
  let outbox.TrackerUpdatePayload(
    target_state_id: target_state_id,
    target_state_name: target_state_name,
    kind: kind,
    ..,
  ) = payload
  case target_state_id, target_state_name {
    None, None -> Ok(Nil)
    _, Some(target_state_name) ->
      run_replay_state_transition(
        task_ref,
        target_state_id,
        target_state_name,
        kind,
        state_transitions,
      )
    Some(target_state_id), None ->
      run_replay_state_transition(
        task_ref,
        Some(target_state_id),
        "",
        kind,
        state_transitions,
      )
  }
}

fn run_replay_state_transition(
  task_ref: task.TaskRef,
  target_state_id: Option(String),
  target_state_name: String,
  kind: String,
  state_transitions: Option(adapter.StateTransitionCapability),
) -> Result(Nil, error.TrackerError) {
  case state_transitions {
    None ->
      Error(
        adapter_error_to_tracker_error(adapter.UnsupportedCapability(
          "state_transitions",
        )),
      )
    Some(state_transitions) -> {
      use _ <- try_tracker_adapter(
        state_transitions.transition(adapter.StateTransitionRequest(
          task: task_ref,
          target_state_id: target_state_id,
          target_state_name: target_state_name,
          reason: "outbox_replay:" <> kind,
        )),
      )
      Ok(Nil)
    }
  }
}

fn string_is_empty(value: String) -> Bool {
  value == ""
}

fn try_adapter(
  result: Result(a, adapter.TrackerError),
  next: fn(a) -> Result(b, adapter.TrackerError),
) -> Result(b, adapter.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

fn try_tracker_adapter(
  result: Result(a, adapter.TrackerError),
  next: fn(a) -> Result(b, error.TrackerError),
) -> Result(b, error.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(adapter_error_to_tracker_error(err))
  }
}

fn run_review_lane_preflight(
  request: transition_effects.ReviewLanePreflightRequest,
) -> EffectResult {
  ReviewLanePreflightFinished(
    task_identity: request.task_identity,
    issue_id: request.issue_id,
    generation: request.generation,
    workflow_id: request.workflow_id,
    result: review_lane_preflight.for_workflow(
      request.workflow_id,
      request.workflow_dag,
      structured_output.validator_repo_root(request.config_dir, "."),
      request.workflow_path,
      request.state_root,
      request.effective,
      request.policy,
      request.now_ms,
    ),
  )
}

fn run_side_effect(effect: Effect) -> EffectResult {
  case effect {
    FetchCandidates(generation, tracker_adapter) ->
      CandidateFetchFinished(
        generation,
        fetch_candidate_issues(tracker_adapter),
      )
    RefreshRunning(generation, ids, tracker_adapter) ->
      RunningRefreshFinished(
        generation,
        refresh_issue_states_by_ids(tracker_adapter, ids),
      )
    RefreshRetry(issue_id, generation, tracker_adapter) ->
      RetryRefreshFinished(
        issue_id,
        generation,
        refresh_issue_states_by_ids(tracker_adapter, [issue_id]),
      )
    ValidateDispatchClaim(issue_id, generation, tracker_adapter) ->
      DispatchClaimValidationFinished(
        issue_id: issue_id,
        generation: generation,
        result: normalize_dispatch_claim_validation(
          issue_id,
          refresh_issue_states_by_ids(tracker_adapter, [issue_id]),
        ),
      )
    ReviewLanePreflight(request) -> run_review_lane_preflight(request)
    ClaimIssue(outbox, task_ref, issue, workspace_path, run_id, capability) ->
      HandoffClaimFinished(
        outbox,
        issue.id,
        run_id,
        adapter_result(
          capability.report(adapter.HandoffClaim(
            task.Task(..task.from_legacy_issue(issue), ref: task_ref),
            workspace_path,
            run_id,
          )),
        ),
      )
    ReportSuccess(
      outbox,
      task_ref,
      issue_id,
      issue,
      success,
      run_id,
      workflow_id,
      capability,
    ) ->
      HandoffSuccessFinished(
        outbox,
        issue_id,
        run_id,
        adapter_result(
          capability.report(adapter.HandoffSuccess(
            task.Task(..task.from_legacy_issue(issue), ref: task_ref),
            success,
            run_id,
            workflow_id,
          )),
        ),
      )
    ReportFailure(
      outbox,
      task_ref,
      issue_id,
      issue,
      failure,
      run_id,
      workflow_id,
      capability,
    ) ->
      HandoffFailureFinished(
        outbox,
        issue_id,
        run_id,
        adapter_result(
          capability.report(adapter.HandoffFailure(
            task.Task(..task.from_legacy_issue(issue), ref: task_ref),
            failure,
            run_id,
            workflow_id,
          )),
        ),
      )
    ReportPark(outbox, report, capability) ->
      HandoffParkFinished(
        outbox,
        report.task.remote_id,
        adapter_result(capability.report(adapter.HandoffPark(report))),
      )
    ReportInvalidWorkflow(
      outbox,
      issue,
      violation,
      violation_fingerprint,
      reporting_policy_fingerprint,
      contract_config,
      comments,
      state_transitions,
    ) ->
      InvalidWorkflowReportFinished(
        outbox,
        issue.id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        report_invalid_workflow(
          issue,
          violation,
          contract_config,
          comments,
          state_transitions,
          Some(outbox.dedupe_key),
        ),
      )
    ReplayOutbox(outbox_replay, comments, state_transitions) ->
      OutboxReplayFinished(
        outbox_replay,
        replay_outbox_update(outbox_replay, comments, state_transitions),
      )
    ReportScheduledFailure(generation, publication, capability) ->
      ScheduledFailureReportFinished(
        generation,
        publication,
        capability.publish(publication),
      )
    CleanupWorkspace(root, workspace_path, hooks, cleanup) ->
      CleanupFinished(workspace_path, cleanup(root, workspace_path, hooks))
  }
}
