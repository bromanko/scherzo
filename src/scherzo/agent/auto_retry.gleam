import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/error
import scherzo/pi/retry_event

const decision_grace_max_ms = 1000

pub type State {
  NoPendingAutoRetry
  PendingAutoRetry(
    error: Option(error.PiRpcError),
    started: Bool,
    decision_deadline_ms: Option(Int),
    agent_end_seen: Bool,
  )
}

pub fn initial() -> State {
  NoPendingAutoRetry
}

pub fn should_defer(
  config: config_types.PiConfig,
  err: error.PiRpcError,
) -> Bool {
  config.auto_retry && retry_event.retryable_pi_error(err)
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
      )
    PendingAutoRetry(started: started, agent_end_seen: agent_end_seen, ..) ->
      PendingAutoRetry(
        error: Some(err),
        started: started,
        decision_deadline_ms: deadline,
        agent_end_seen: agent_end_seen,
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
      )
    PendingAutoRetry(error: err, agent_end_seen: agent_end_seen, ..) ->
      PendingAutoRetry(
        error: err,
        started: True,
        decision_deadline_ms: None,
        agent_end_seen: agent_end_seen,
      )
  }
}

pub fn mark_agent_end(pending_auto_retry: State, deadline_ms: Int) -> State {
  let deadline = Some(deadline_ms)
  case pending_auto_retry {
    NoPendingAutoRetry -> NoPendingAutoRetry
    PendingAutoRetry(error: err, started: started, ..) ->
      PendingAutoRetry(
        error: err,
        started: started,
        decision_deadline_ms: deadline,
        agent_end_seen: True,
      )
  }
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
