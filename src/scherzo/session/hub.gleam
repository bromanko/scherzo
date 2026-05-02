import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import scherzo/domain
import scherzo/session/event
import scherzo/session/reason

pub const default_max_events_per_session = 2000

pub const default_max_sessions = 500

pub type HubError {
  HubUnavailable
  SessionNotFound(String)
  InvalidLimit(Int)
  ActorCallTimeout
}

pub type Message {
  RegisterSession(event.SessionSummary)
  UpdateStatus(String, event.SessionStatus)
  UpdatePiSession(String, String)
  UpdateTokens(String, domain.TokenTotals)
  Publish(String, event.EventPayload)
  FinishSession(String, reason.WorkerExitReason)
  ListSessions(process.Subject(Result(List(event.SessionSummary), HubError)))
  ListSessionsSnapshot(process.Subject(Result(event.SessionList, HubError)))
  GetSession(
    String,
    process.Subject(Result(Option(event.SessionSummary), HubError)),
  )
  EventsAfter(
    String,
    Int,
    Int,
    process.Subject(Result(event.EventPage, HubError)),
  )
  Stop
}

type State {
  State(
    max_sessions: Int,
    max_events_per_session: Int,
    now_ms: fn() -> Int,
    summaries: Dict(String, event.SessionSummary),
    events: Dict(String, List(event.SessionEvent)),
    dropped_old_events: Dict(String, Bool),
    session_order: List(String),
    next_cursor: Int,
  )
}

pub fn start(
  max_events_per_session: Int,
  now_ms: fn() -> Int,
) -> Result(process.Subject(Message), HubError) {
  start_with_limits(default_max_sessions, max_events_per_session, now_ms)
}

pub fn start_with_limits(
  max_sessions: Int,
  max_events_per_session: Int,
  now_ms: fn() -> Int,
) -> Result(process.Subject(Message), HubError) {
  case max_sessions <= 0 {
    True -> Error(InvalidLimit(max_sessions))
    False ->
      case max_events_per_session <= 0 {
        True -> Error(InvalidLimit(max_events_per_session))
        False -> {
          let state =
            State(
              max_sessions: max_sessions,
              max_events_per_session: max_events_per_session,
              now_ms: now_ms,
              summaries: dict.new(),
              events: dict.new(),
              dropped_old_events: dict.new(),
              session_order: [],
              next_cursor: 1,
            )
          let builder = actor.new(state) |> actor.on_message(handle_message)
          case actor.start(builder) {
            Ok(started) -> Ok(started.data)
            Error(_) -> Error(HubUnavailable)
          }
        }
      }
  }
}

pub fn stop(subject: process.Subject(Message)) -> Nil {
  process.send(subject, Stop)
}

pub fn register_session(
  subject: process.Subject(Message),
  summary: event.SessionSummary,
) -> Nil {
  process.send(subject, RegisterSession(summary))
}

pub fn update_status(
  subject: process.Subject(Message),
  session_id: String,
  status: event.SessionStatus,
) -> Nil {
  process.send(subject, UpdateStatus(session_id, status))
}

pub fn update_pi_session(
  subject: process.Subject(Message),
  session_id: String,
  pi_session_id: String,
) -> Nil {
  process.send(subject, UpdatePiSession(session_id, pi_session_id))
}

pub fn update_tokens(
  subject: process.Subject(Message),
  session_id: String,
  tokens: domain.TokenTotals,
) -> Nil {
  process.send(subject, UpdateTokens(session_id, tokens))
}

pub fn publish(
  subject: process.Subject(Message),
  session_id: String,
  payload: event.EventPayload,
) -> Nil {
  process.send(subject, Publish(session_id, payload))
}

pub fn finish_session(
  subject: process.Subject(Message),
  session_id: String,
  reason: reason.WorkerExitReason,
) -> Nil {
  process.send(subject, FinishSession(session_id, reason))
}

pub fn list_sessions(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(List(event.SessionSummary), HubError) {
  let reply = process.new_subject()
  process.send(subject, ListSessions(reply))
  receive_reply(reply, timeout_ms)
}

pub fn list_sessions_snapshot(
  subject: process.Subject(Message),
  timeout_ms: Int,
) -> Result(event.SessionList, HubError) {
  let reply = process.new_subject()
  process.send(subject, ListSessionsSnapshot(reply))
  receive_reply(reply, timeout_ms)
}

pub fn get_session(
  subject: process.Subject(Message),
  session_id: String,
  timeout_ms: Int,
) -> Result(Option(event.SessionSummary), HubError) {
  let reply = process.new_subject()
  process.send(subject, GetSession(session_id, reply))
  receive_reply(reply, timeout_ms)
}

pub fn events_after(
  subject: process.Subject(Message),
  session_id: String,
  cursor: Int,
  limit: Int,
  timeout_ms: Int,
) -> Result(event.EventPage, HubError) {
  let reply = process.new_subject()
  process.send(subject, EventsAfter(session_id, cursor, limit, reply))
  receive_reply(reply, timeout_ms)
}

fn receive_reply(
  reply: process.Subject(Result(a, HubError)),
  timeout_ms: Int,
) -> Result(a, HubError) {
  case process.receive(reply, within: timeout_ms) {
    Ok(result) -> result
    Error(_) -> Error(ActorCallTimeout)
  }
}

fn handle_message(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case message {
    RegisterSession(summary) -> actor.continue(register_summary(state, summary))
    UpdateStatus(session_id, status) ->
      actor.continue(update_summary_status(state, session_id, status))
    UpdatePiSession(session_id, pi_session_id) ->
      actor.continue(update_summary_pi_session(state, session_id, pi_session_id))
    UpdateTokens(session_id, tokens) ->
      actor.continue(update_summary_tokens(state, session_id, tokens))
    Publish(session_id, payload) ->
      actor.continue(publish_payload(state, session_id, payload))
    FinishSession(session_id, reason) ->
      actor.continue(update_summary_status(
        state,
        session_id,
        event.Exited(reason),
      ))
    ListSessions(reply) -> {
      process.send(reply, Ok(summaries_in_order(state)))
      actor.continue(state)
    }
    ListSessionsSnapshot(reply) -> {
      process.send(reply, Ok(session_list(state)))
      actor.continue(state)
    }
    GetSession(session_id, reply) -> {
      let summary = case dict.get(state.summaries, session_id) {
        Ok(summary) -> Some(summary)
        Error(_) -> None
      }
      process.send(reply, Ok(summary))
      actor.continue(state)
    }
    EventsAfter(session_id, cursor, limit, reply) -> {
      process.send(reply, replay_events(state, session_id, cursor, limit))
      actor.continue(state)
    }
    Stop -> actor.stop()
  }
}

fn summaries_in_order(state: State) -> List(event.SessionSummary) {
  list.filter_map(state.session_order, fn(session_id) {
    dict.get(state.summaries, session_id)
  })
}

fn session_list(state: State) -> event.SessionList {
  event.SessionList(sessions: summaries_in_order(state), now_ms: state.now_ms())
}

fn register_summary(state: State, summary: event.SessionSummary) -> State {
  let session_id = summary.session_id
  let session_order = touch_session_order(state.session_order, session_id)
  State(
    ..state,
    summaries: dict.insert(state.summaries, session_id, summary),
    events: dict.insert(state.events, session_id, []),
    dropped_old_events: dict.insert(state.dropped_old_events, session_id, False),
    session_order: session_order,
  )
  |> prune_old_sessions
}

fn touch_session_order(
  session_order: List(String),
  session_id: String,
) -> List(String) {
  session_order
  |> list.filter(fn(existing) { existing != session_id })
  |> list.append([session_id])
}

fn prune_old_sessions(state: State) -> State {
  case list.length(state.session_order) > state.max_sessions {
    False -> state
    True ->
      case state.session_order {
        [] -> state
        [expired_session_id, ..remaining_session_ids] ->
          State(
            ..state,
            summaries: dict.delete(state.summaries, expired_session_id),
            events: dict.delete(state.events, expired_session_id),
            dropped_old_events: dict.delete(
              state.dropped_old_events,
              expired_session_id,
            ),
            session_order: remaining_session_ids,
          )
          |> prune_old_sessions
      }
  }
}

fn update_summary_status(
  state: State,
  session_id: String,
  status: event.SessionStatus,
) -> State {
  update_summary(state, session_id, fn(summary) {
    event.SessionSummary(
      ..summary,
      status: status,
      last_event_at_ms: state.now_ms(),
    )
  })
}

fn update_summary_pi_session(
  state: State,
  session_id: String,
  pi_session_id: String,
) -> State {
  update_summary(state, session_id, fn(summary) {
    event.SessionSummary(
      ..summary,
      pi_session_id: Some(pi_session_id),
      last_event_at_ms: state.now_ms(),
    )
  })
}

fn update_summary_tokens(
  state: State,
  session_id: String,
  tokens: domain.TokenTotals,
) -> State {
  update_summary(state, session_id, fn(summary) {
    event.SessionSummary(
      ..summary,
      token_totals: tokens,
      last_event_at_ms: state.now_ms(),
    )
  })
}

fn update_summary(
  state: State,
  session_id: String,
  change: fn(event.SessionSummary) -> event.SessionSummary,
) -> State {
  case dict.get(state.summaries, session_id) {
    Error(_) -> state
    Ok(summary) ->
      State(
        ..state,
        summaries: dict.insert(state.summaries, session_id, change(summary)),
        session_order: touch_session_order(state.session_order, session_id),
      )
  }
}

fn publish_payload(
  state: State,
  session_id: String,
  payload: event.EventPayload,
) -> State {
  case dict.get(state.summaries, session_id) {
    Error(_) -> state
    Ok(summary) -> {
      let now = state.now_ms()
      let stored_event =
        event.SessionEvent(
          cursor: state.next_cursor,
          at_ms: now,
          session_id: session_id,
          issue_id: summary.issue_id,
          payload: payload,
        )
      let existing_events = case dict.get(state.events, session_id) {
        Ok(events) -> events
        Error(_) -> []
      }
      let all_events = list.append(existing_events, [stored_event])
      let dropped_now = list.length(all_events) > state.max_events_per_session
      let retained_events =
        retain_latest(all_events, state.max_events_per_session)
      let dropped_before = case dict.get(state.dropped_old_events, session_id) {
        Ok(value) -> value
        Error(_) -> False
      }
      let summary = update_summary_after_payload(summary, payload, now)
      State(
        ..state,
        summaries: dict.insert(state.summaries, session_id, summary),
        events: dict.insert(state.events, session_id, retained_events),
        session_order: touch_session_order(state.session_order, session_id),
        dropped_old_events: dict.insert(
          state.dropped_old_events,
          session_id,
          dropped_before || dropped_now,
        ),
        next_cursor: state.next_cursor + 1,
      )
    }
  }
}

fn update_summary_after_payload(
  summary: event.SessionSummary,
  payload: event.EventPayload,
  now: Int,
) -> event.SessionSummary {
  let current_turn = case payload.turn {
    Some(turn) -> turn
    None -> summary.current_turn
  }
  let token_totals = case payload.tokens.total > 0 {
    True -> payload.tokens
    False -> summary.token_totals
  }
  event.SessionSummary(
    ..summary,
    current_turn: current_turn,
    token_totals: token_totals,
    last_event_at_ms: now,
  )
}

fn retain_latest(
  events: List(event.SessionEvent),
  max_events: Int,
) -> List(event.SessionEvent) {
  let count = list.length(events)
  case count > max_events {
    True -> list.drop(events, count - max_events)
    False -> events
  }
}

fn replay_events(
  state: State,
  session_id: String,
  cursor: Int,
  limit: Int,
) -> Result(event.EventPage, HubError) {
  case limit <= 0 {
    True -> Error(InvalidLimit(limit))
    False ->
      case dict.get(state.summaries, session_id) {
        Error(_) -> Error(SessionNotFound(session_id))
        Ok(_) -> {
          let events = case dict.get(state.events, session_id) {
            Ok(events) -> events
            Error(_) -> []
          }
          let max_limit = min_int(limit, state.max_events_per_session)
          let page_events =
            events
            |> list.filter(fn(stored_event) { stored_event.cursor > cursor })
            |> list.take(max_limit)
          Ok(event.EventPage(
            events: page_events,
            next_cursor: next_cursor(page_events, cursor),
            truncated: replay_truncated(state, session_id, events, cursor),
          ))
        }
      }
  }
}

fn next_cursor(events: List(event.SessionEvent), input_cursor: Int) -> Int {
  case list.last(events) {
    Ok(stored_event) -> stored_event.cursor
    Error(_) -> input_cursor
  }
}

fn replay_truncated(
  state: State,
  session_id: String,
  events: List(event.SessionEvent),
  requested_cursor: Int,
) -> Bool {
  let dropped = case dict.get(state.dropped_old_events, session_id) {
    Ok(value) -> value
    Error(_) -> False
  }
  case dropped {
    False -> False
    True ->
      case list.first(events) {
        Ok(first_event) -> requested_cursor < first_event.cursor
        Error(_) -> False
      }
  }
}

fn min_int(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}
