import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/otp/actor
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear_triage
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

pub type Effect {
  FetchCandidates(generation: Int, client: tracker.Client)
  FetchLinearCommands(
    generation: Int,
    issue_ids: List(String),
    candidates: List(tracker_issue.Issue),
    dispatch_after: Bool,
    client: linear.CommandClient,
    limit_per_issue: Int,
  )
  RefreshRunning(generation: Int, ids: List(String), client: tracker.Client)
  RefreshRetry(issue_id: String, generation: Int, client: tracker.Client)
  ValidateDispatchClaim(
    issue_id: String,
    generation: Int,
    client: tracker.Client,
  )
  ClaimIssue(
    issue: tracker_issue.Issue,
    workspace_path: String,
    run_id: String,
    client: handoff.Client,
  )
  ReportSuccess(
    issue_id: String,
    issue: tracker_issue.Issue,
    success: agent_types.WorkerSuccess,
    run_id: String,
    client: handoff.Client,
  )
  ReportFailure(
    issue_id: String,
    issue: tracker_issue.Issue,
    failure: agent_types.WorkerFailure,
    run_id: String,
    client: handoff.Client,
  )
  PostLinearCommandAck(
    issue_id: String,
    source_comment_id: String,
    body: String,
    client: linear.CommandClient,
  )
  ReportInvalidWorkflow(
    issue: tracker_issue.Issue,
    violation: workflow_policy.IssueWorkflowViolation,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    client: linear_triage.TriageClient,
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

pub type EffectResult {
  CandidateFetchFinished(
    Int,
    Result(List(tracker_issue.Issue), error.TrackerError),
  )
  LinearCommandFetchFinished(
    Int,
    List(tracker_issue.Issue),
    Bool,
    Result(List(linear.LinearComment), error.TrackerError),
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
  HandoffClaimFinished(String, String, Result(Nil, error.TrackerError))
  HandoffSuccessFinished(String, String, Result(Nil, error.TrackerError))
  HandoffFailureFinished(String, String, Result(Nil, error.TrackerError))
  LinearCommandAckFinished(String, String, Result(Nil, error.TrackerError))
  InvalidWorkflowReportFinished(
    issue_id: String,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    result: Result(
      linear_triage.InvalidWorkflowReportOutcome,
      error.TrackerError,
    ),
  )
  CleanupFinished(String, Result(Nil, error.WorkspaceError))
}

pub type Completion {
  Finished(id: Int, result: EffectResult)
  Crashed(id: Int, effect: Effect, reason: String)
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
    FetchLinearCommands(_, _, _, _, _, _) -> "fetch_linear_commands"
    RefreshRunning(_, _, _) -> "refresh_running"
    RefreshRetry(_, _, _) -> "refresh_retry"
    ValidateDispatchClaim(_, _, _) -> "validate_dispatch_claim"
    ClaimIssue(_, _, _, _) -> "claim_issue"
    ReportSuccess(_, _, _, _, _) -> "report_success"
    ReportFailure(_, _, _, _, _) -> "report_failure"
    PostLinearCommandAck(_, _, _, _) -> "post_linear_command_ack"
    ReportInvalidWorkflow(_, _, _, _, _) -> "report_invalid_workflow"
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
      actor.continue(finish_effect(state, id, result))
    WorkerDown(down) -> actor.continue(handle_worker_down(state, down))
    Shutdown(reply) -> {
      shutdown_in_flight(state)
      process.send(reply, Nil)
      actor.stop()
    }
  }
}

fn enqueue_effect(state: State, effect: Effect) -> State {
  let queued = QueuedEffect(id: state.next_id, effect: effect)
  State(
    ..state,
    next_id: state.next_id + 1,
    queue: list.append(state.queue, [queued]),
  )
  |> drain
}

fn drain(state: State) -> State {
  case dict.size(state.in_flight) >= state.max_concurrent {
    True -> state
    False ->
      case state.queue {
        [] -> state
        [queued, ..rest] -> {
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
        Error(_) -> Nil
      }
    })
  case process.receive(ready, within: 1000) {
    Ok(start) -> {
      let monitor = process.monitor(pid)
      process.send(start, Nil)
      #(pid, monitor)
    }
    Error(_) -> #(pid, process.monitor(pid))
  }
}

fn finish_effect(state: State, id: Int, result: EffectResult) -> State {
  case dict.get(state.in_flight, id) {
    Error(_) -> state
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
        Error(_) -> state
        Ok(id) ->
          case dict.get(state.in_flight, id) {
            Error(_) ->
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

fn shutdown_in_flight(state: State) -> Nil {
  dict.each(state.in_flight, fn(_, in_flight) {
    process.demonitor_process(in_flight.monitor)
    process.kill(in_flight.pid)
  })
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

fn run_side_effect(effect: Effect) -> EffectResult {
  case effect {
    FetchCandidates(generation, client) ->
      CandidateFetchFinished(generation, client.fetch_candidate_issues())
    FetchLinearCommands(
      generation,
      issue_ids,
      candidates,
      dispatch_after,
      client,
      limit_per_issue,
    ) ->
      LinearCommandFetchFinished(
        generation,
        candidates,
        dispatch_after,
        client.fetch_comments(issue_ids, limit_per_issue),
      )
    RefreshRunning(generation, ids, client) ->
      RunningRefreshFinished(generation, client.fetch_issue_states_by_ids(ids))
    RefreshRetry(issue_id, generation, client) ->
      RetryRefreshFinished(
        issue_id,
        generation,
        client.fetch_issue_states_by_ids([issue_id]),
      )
    ValidateDispatchClaim(issue_id, generation, client) ->
      DispatchClaimValidationFinished(
        issue_id: issue_id,
        generation: generation,
        result: normalize_dispatch_claim_validation(
          issue_id,
          client.fetch_issue_states_by_ids([issue_id]),
        ),
      )
    ClaimIssue(issue, _workspace_path, run_id, client) ->
      HandoffClaimFinished(issue.id, run_id, client.claim_issue(issue, run_id))
    ReportSuccess(issue_id, issue, success, run_id, client) ->
      HandoffSuccessFinished(
        issue_id,
        run_id,
        client.report_success(issue, success, run_id),
      )
    ReportFailure(issue_id, issue, failure, run_id, client) ->
      HandoffFailureFinished(
        issue_id,
        run_id,
        client.report_failure(issue, failure, run_id),
      )
    PostLinearCommandAck(issue_id, source_comment_id, body, client) ->
      LinearCommandAckFinished(
        issue_id,
        source_comment_id,
        client.post_ack(issue_id, body),
      )
    ReportInvalidWorkflow(
      issue,
      violation,
      violation_fingerprint,
      reporting_policy_fingerprint,
      client,
    ) ->
      InvalidWorkflowReportFinished(
        issue.id,
        violation_fingerprint,
        reporting_policy_fingerprint,
        client.report_invalid_workflow(issue, violation),
      )
    CleanupWorkspace(root, workspace_path, hooks, cleanup) ->
      CleanupFinished(workspace_path, cleanup(root, workspace_path, hooks))
  }
}
