import gleam/option.{type Option, None, Some}
import scherzo/agent/pi_event
import scherzo/error

const decision_grace_max_ms = 1000

pub type State {
  NoPendingAutoRetry
  PendingAutoRetry(
    error: Option(error.PiRpcError),
    started: Bool,
    decision_deadline_ms: Option(Int),
    agent_end_seen: Bool,
    retry_output_seen: Bool,
  )
}

pub fn initial() -> State {
  NoPendingAutoRetry
}

pub fn awaits_pi_terminal_decision(err: error.PiRpcError) -> Bool {
  case err {
    error.PiProtocolError(_) -> True
    error.PiLaunchFailed(_)
    | error.PiMalformedJson(_)
    | error.PiReadTimeout
    | error.PiTurnTimeout
    | error.PiStallTimeout
    | error.PiExited(_)
    | error.PiContextWindowExhausted(..) -> False
  }
}

pub fn defer_failure(
  pending_auto_retry: State,
  err: error.PiRpcError,
  deadline_ms: Int,
) -> State {
  let deadline = Some(deadline_ms)
  case pending_auto_retry {
    NoPendingAutoRetry ->
      PendingAutoRetry(
        error: Some(err),
        started: False,
        decision_deadline_ms: deadline,
        agent_end_seen: False,
        retry_output_seen: False,
      )
    PendingAutoRetry(
      started: started,
      agent_end_seen: agent_end_seen,
      retry_output_seen: retry_output_seen,
      ..,
    ) ->
      PendingAutoRetry(
        error: Some(err),
        started: started,
        decision_deadline_ms: deadline,
        agent_end_seen: agent_end_seen,
        retry_output_seen: retry_output_seen,
      )
  }
}

pub fn mark_started(pending_auto_retry: State) -> State {
  case pending_auto_retry {
    NoPendingAutoRetry ->
      PendingAutoRetry(
        error: None,
        started: True,
        decision_deadline_ms: None,
        agent_end_seen: False,
        retry_output_seen: False,
      )
    PendingAutoRetry(error: err, ..) ->
      PendingAutoRetry(
        error: err,
        started: True,
        decision_deadline_ms: None,
        agent_end_seen: False,
        retry_output_seen: False,
      )
  }
}

pub fn mark_retry_output(pending_auto_retry: State) -> State {
  case pending_auto_retry {
    NoPendingAutoRetry -> NoPendingAutoRetry
    PendingAutoRetry(
      error: err,
      started: True,
      decision_deadline_ms: deadline,
      agent_end_seen: agent_end_seen,
      ..,
    ) ->
      PendingAutoRetry(
        error: err,
        started: True,
        decision_deadline_ms: deadline,
        agent_end_seen: agent_end_seen,
        retry_output_seen: True,
      )
    PendingAutoRetry(..) -> pending_auto_retry
  }
}

pub fn mark_output_event(
  pending_auto_retry: State,
  event: pi_event.PiEvent,
) -> State {
  case event {
    pi_event.MessageStart
    | pi_event.MessageUpdate
    | pi_event.MessageEnd
    | pi_event.Message
    | pi_event.ToolExecutionStart
    | pi_event.ToolExecutionUpdate
    | pi_event.ToolExecutionEnd
    | pi_event.ExtensionUiRequest
    | pi_event.TurnEnd -> mark_retry_output(pending_auto_retry)
    _ -> pending_auto_retry
  }
}

pub fn mark_agent_end(
  pending_auto_retry: State,
  will_retry: Option(Bool),
  deadline_ms: Int,
) -> State {
  case pending_auto_retry {
    NoPendingAutoRetry -> NoPendingAutoRetry
    PendingAutoRetry(started: False, error: err, ..) ->
      mark_initial_agent_end(err, will_retry, deadline_ms)
    PendingAutoRetry(retry_output_seen: False, ..) -> pending_auto_retry
    PendingAutoRetry(error: err, started: started, ..) ->
      PendingAutoRetry(
        error: err,
        started: started,
        decision_deadline_ms: Some(deadline_ms),
        agent_end_seen: True,
        retry_output_seen: True,
      )
  }
}

fn mark_initial_agent_end(
  err: Option(error.PiRpcError),
  will_retry: Option(Bool),
  deadline_ms: Int,
) -> State {
  case will_retry {
    Some(True) ->
      PendingAutoRetry(
        error: err,
        started: False,
        decision_deadline_ms: Some(deadline_ms),
        agent_end_seen: True,
        retry_output_seen: False,
      )
    Some(False) -> terminal_pending_retry(err, deadline_ms)
    None ->
      terminal_pending_retry(
        Some(error.PiProtocolError(
          "pi agent_end missing willRetry after stopReason=error",
        )),
        deadline_ms,
      )
  }
}

fn terminal_pending_retry(
  err: Option(error.PiRpcError),
  deadline_ms: Int,
) -> State {
  PendingAutoRetry(
    error: err,
    started: False,
    decision_deadline_ms: Some(deadline_ms - decision_grace_max_ms),
    agent_end_seen: True,
    retry_output_seen: False,
  )
}

pub fn agent_end_seen(pending_auto_retry: State) -> Bool {
  case pending_auto_retry {
    PendingAutoRetry(agent_end_seen: True, ..) -> True
    _ -> False
  }
}

pub fn deadline_expired(pending_auto_retry: State, now_ms: Int) -> Bool {
  case pending_auto_retry {
    PendingAutoRetry(decision_deadline_ms: Some(deadline), ..) ->
      now_ms >= deadline
    _ -> False
  }
}

pub fn effective_stall_deadline(
  pending_auto_retry: State,
  base_stall_deadline: Int,
) -> Int {
  case pending_auto_retry {
    PendingAutoRetry(decision_deadline_ms: Some(deadline), ..) ->
      min_int(base_stall_deadline, deadline)
    _ -> base_stall_deadline
  }
}

pub fn decision_deadline_ms(now_ms: Int, read_timeout_ms: Int) -> Int {
  now_ms + min_int(read_timeout_ms, decision_grace_max_ms)
}

pub fn deferred_error(
  pending_auto_retry: State,
  final_error: Option(String),
) -> error.PiRpcError {
  case pending_auto_retry {
    PendingAutoRetry(error: Some(err), ..) -> err
    _ -> exhausted_error(final_error)
  }
}

fn exhausted_error(final_error: Option(String)) -> error.PiRpcError {
  case final_error {
    Some(message) ->
      error.PiProtocolError("pi auto-retry exhausted: " <> message)
    None -> error.PiProtocolError("pi auto-retry exhausted")
  }
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}
