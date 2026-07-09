import gleam/erlang/process
import gleam/option.{type Option}
import scherzo/agent/types as agent_types
import scherzo/log
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/event_publisher as session_events
import scherzo/orchestrator/outbox_effects
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/state/ledger
import scherzo/state/record

pub opaque type Clock {
  Clock(now_ms: fn() -> Int)
}

pub fn clock(now_ms now_ms: fn() -> Int) -> Clock {
  Clock(now_ms: now_ms)
}

pub fn now_ms(clock: Clock) -> Int {
  let Clock(now_ms: now_ms) = clock
  now_ms()
}

pub opaque type Logger {
  Logger(
    write: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
  )
}

pub fn logger(
  write write: fn(String, String, List(log.Field), List(String)) ->
    Result(Nil, Nil),
) -> Logger {
  Logger(write: write)
}

pub fn write(
  logger: Logger,
  level: String,
  event: String,
  fields: List(log.Field),
  secrets: List(String),
) -> Result(Nil, Nil) {
  let Logger(write: write) = logger
  write(level, event, fields, secrets)
}

pub opaque type EventPublisher {
  EventPublisher(event_hub: process.Subject(hub.Message), now_ms: fn() -> Int)
}

pub fn event_publisher(
  event_hub event_hub: process.Subject(hub.Message),
  now_ms now_ms: fn() -> Int,
) -> EventPublisher {
  EventPublisher(event_hub: event_hub, now_ms: now_ms)
}

pub fn event_hub(events: EventPublisher) -> process.Subject(hub.Message) {
  let EventPublisher(event_hub: event_hub, ..) = events
  event_hub
}

pub fn event_now_ms(events: EventPublisher) -> Int {
  let EventPublisher(now_ms: now_ms, ..) = events
  now_ms()
}

pub fn lifecycle(
  events: EventPublisher,
  session_id: String,
  name: session_event.LifecycleEventName,
  message: Option(String),
) -> Nil {
  session_events.lifecycle(event_hub(events), session_id, name, message)
}

pub fn lifecycle_with_recovery(
  events: EventPublisher,
  session_id: String,
  name: session_event.LifecycleEventName,
  message: Option(String),
  recovery: Option(session_event.RecoveryInfo),
) -> Nil {
  session_events.lifecycle_with_recovery(
    event_hub(events),
    session_id,
    name,
    message,
    recovery,
  )
}

pub fn worker_update(
  events: EventPublisher,
  session_id: String,
  update: agent_types.RunnerUpdate,
) -> Nil {
  session_events.worker_update(event_hub(events), session_id, update)
}

pub fn recovery_lifecycle(
  events: EventPublisher,
  session_id: String,
  recovery: Option(session_event.RecoveryInfo),
) -> Nil {
  case recovery {
    option.None -> Nil
    option.Some(info) ->
      lifecycle_with_recovery(
        events,
        session_id,
        lifecycle_name_for_recovery(info.status),
        info.message,
        option.Some(info),
      )
  }
}

pub fn lifecycle_name_for_recovery(
  status: session_event.RecoveryStatus,
) -> session_event.LifecycleEventName {
  case status {
    session_event.Interrupted -> session_event.RecoveryInterrupted
    session_event.Parked -> session_event.RecoveryParked
    session_event.Cleanup -> session_event.RecoveryCleanup
    session_event.OldStateResetRequired ->
      session_event.OldStateResetRequiredEvent
    session_event.Recovered
    | session_event.Resumed
    | session_event.InspectionNeeded
    | session_event.Blocked
    | session_event.DriftDetected -> session_event.RecoveryDetected
  }
}

pub fn publish(
  events: EventPublisher,
  session_id: String,
  payload: session_event.EventPayload,
) -> Nil {
  hub.publish(event_hub(events), session_id, payload)
}

pub fn update_status(
  events: EventPublisher,
  session_id: String,
  status: session_event.SessionStatus,
) -> Nil {
  hub.update_status(event_hub(events), session_id, status)
}

pub opaque type LedgerWriter(state) {
  LedgerWriter(
    append_bodies: fn(state, List(record.RecordBody), String) -> #(state, Bool),
    append_bodies_best_effort: fn(state, List(record.RecordBody), String) ->
      state,
    append_records: fn(state, List(record.LedgerRecord), String) ->
      #(state, Result(Nil, ledger.LedgerError)),
  )
}

pub fn ledger_writer(
  append_bodies append_bodies: fn(state, List(record.RecordBody), String) ->
    #(state, Bool),
  append_bodies_best_effort append_bodies_best_effort: fn(
    state,
    List(record.RecordBody),
    String,
  ) -> state,
  append_records append_records: fn(state, List(record.LedgerRecord), String) ->
    #(state, Result(Nil, ledger.LedgerError)),
) -> LedgerWriter(state) {
  LedgerWriter(
    append_bodies: append_bodies,
    append_bodies_best_effort: append_bodies_best_effort,
    append_records: append_records,
  )
}

pub fn append_bodies(
  writer: LedgerWriter(state),
  state: state,
  bodies: List(record.RecordBody),
  event: String,
) -> #(state, Bool) {
  let LedgerWriter(append_bodies: append_bodies, ..) = writer
  append_bodies(state, bodies, event)
}

pub fn append_bodies_best_effort(
  writer: LedgerWriter(state),
  state: state,
  bodies: List(record.RecordBody),
  event: String,
) -> state {
  let LedgerWriter(append_bodies_best_effort: append_bodies_best_effort, ..) =
    writer
  append_bodies_best_effort(state, bodies, event)
}

pub fn append_records(
  writer: LedgerWriter(state),
  state: state,
  records: List(record.LedgerRecord),
  event: String,
) -> #(state, Result(Nil, ledger.LedgerError)) {
  let LedgerWriter(append_records: append_records, ..) = writer
  append_records(state, records, event)
}

pub opaque type EffectQueue(state) {
  EffectQueue(
    enqueue: fn(state, effect_runner.Effect) -> state,
    enqueue_outbox: fn(
      state,
      outbox_effects.Intent,
      fn(outbox_effects.Intent) -> effect_runner.Effect,
    ) -> state,
    enqueue_outbox_with_attempt_count: fn(
      state,
      outbox_effects.Intent,
      Int,
      fn(outbox_effects.Intent) -> effect_runner.Effect,
    ) -> state,
    enqueue_outbox_with_attempt_count_result: fn(
      state,
      outbox_effects.Intent,
      Int,
      fn(outbox_effects.Intent) -> effect_runner.Effect,
    ) -> #(state, Bool),
  )
}

pub fn effect_queue(
  enqueue enqueue: fn(state, effect_runner.Effect) -> state,
  enqueue_outbox enqueue_outbox: fn(
    state,
    outbox_effects.Intent,
    fn(outbox_effects.Intent) -> effect_runner.Effect,
  ) -> state,
  enqueue_outbox_with_attempt_count enqueue_outbox_with_attempt_count: fn(
    state,
    outbox_effects.Intent,
    Int,
    fn(outbox_effects.Intent) -> effect_runner.Effect,
  ) -> state,
  enqueue_outbox_with_attempt_count_result enqueue_outbox_with_attempt_count_result: fn(
    state,
    outbox_effects.Intent,
    Int,
    fn(outbox_effects.Intent) -> effect_runner.Effect,
  ) -> #(state, Bool),
) -> EffectQueue(state) {
  EffectQueue(
    enqueue: enqueue,
    enqueue_outbox: enqueue_outbox,
    enqueue_outbox_with_attempt_count: enqueue_outbox_with_attempt_count,
    enqueue_outbox_with_attempt_count_result: enqueue_outbox_with_attempt_count_result,
  )
}

pub fn enqueue(
  queue: EffectQueue(state),
  state: state,
  effect: effect_runner.Effect,
) -> state {
  let EffectQueue(enqueue: enqueue, ..) = queue
  enqueue(state, effect)
}

pub fn enqueue_outbox(
  queue: EffectQueue(state),
  state: state,
  intent: outbox_effects.Intent,
  make_effect: fn(outbox_effects.Intent) -> effect_runner.Effect,
) -> state {
  let EffectQueue(enqueue_outbox: enqueue_outbox, ..) = queue
  enqueue_outbox(state, intent, make_effect)
}

pub fn enqueue_outbox_with_attempt_count(
  queue: EffectQueue(state),
  state: state,
  intent: outbox_effects.Intent,
  attempt_count: Int,
  make_effect: fn(outbox_effects.Intent) -> effect_runner.Effect,
) -> state {
  let EffectQueue(
    enqueue_outbox_with_attempt_count: enqueue_outbox_with_attempt_count,
    ..,
  ) = queue
  enqueue_outbox_with_attempt_count(state, intent, attempt_count, make_effect)
}

pub fn enqueue_outbox_with_attempt_count_result(
  queue: EffectQueue(state),
  state: state,
  intent: outbox_effects.Intent,
  attempt_count: Int,
  make_effect: fn(outbox_effects.Intent) -> effect_runner.Effect,
) -> #(state, Bool) {
  let EffectQueue(
    enqueue_outbox_with_attempt_count_result: enqueue_outbox_with_attempt_count_result,
    ..,
  ) = queue
  enqueue_outbox_with_attempt_count_result(
    state,
    intent,
    attempt_count,
    make_effect,
  )
}

pub opaque type Timers(message, timer) {
  Timers(
    send_after: fn(process.Subject(message), Int, message) -> timer,
    cancel_timer: fn(timer) -> Nil,
  )
}

pub fn timers(
  send_after send_after: fn(process.Subject(message), Int, message) -> timer,
  cancel_timer cancel_timer: fn(timer) -> Nil,
) -> Timers(message, timer) {
  Timers(send_after: send_after, cancel_timer: cancel_timer)
}

pub fn send_after(
  timers: Timers(message, timer),
  subject: process.Subject(message),
  delay_ms: Int,
  message: message,
) -> timer {
  let Timers(send_after: send_after, ..) = timers
  send_after(subject, delay_ms, message)
}

pub fn cancel_timer(timers: Timers(message, timer), timer: timer) -> Nil {
  let Timers(cancel_timer: cancel_timer, ..) = timers
  cancel_timer(timer)
}

pub opaque type DaemonCapabilities(state, message, timer) {
  DaemonCapabilities(
    clock: Clock,
    logger: Logger,
    events: EventPublisher,
    ledger: LedgerWriter(state),
    effects: EffectQueue(state),
    timers: Timers(message, timer),
  )
}

pub fn daemon_capabilities(
  clock clock: Clock,
  logger logger: Logger,
  events events: EventPublisher,
  ledger ledger: LedgerWriter(state),
  effects effects: EffectQueue(state),
  timers timers: Timers(message, timer),
) -> DaemonCapabilities(state, message, timer) {
  DaemonCapabilities(
    clock: clock,
    logger: logger,
    events: events,
    ledger: ledger,
    effects: effects,
    timers: timers,
  )
}

pub fn daemon_clock(
  capabilities: DaemonCapabilities(state, message, timer),
) -> Clock {
  let DaemonCapabilities(clock: clock, ..) = capabilities
  clock
}

pub fn daemon_logger(
  capabilities: DaemonCapabilities(state, message, timer),
) -> Logger {
  let DaemonCapabilities(logger: logger, ..) = capabilities
  logger
}

pub fn daemon_events(
  capabilities: DaemonCapabilities(state, message, timer),
) -> EventPublisher {
  let DaemonCapabilities(events: events, ..) = capabilities
  events
}

pub fn daemon_ledger(
  capabilities: DaemonCapabilities(state, message, timer),
) -> LedgerWriter(state) {
  let DaemonCapabilities(ledger: ledger, ..) = capabilities
  ledger
}

pub fn daemon_effects(
  capabilities: DaemonCapabilities(state, message, timer),
) -> EffectQueue(state) {
  let DaemonCapabilities(effects: effects, ..) = capabilities
  effects
}

pub fn daemon_timers(
  capabilities: DaemonCapabilities(state, message, timer),
) -> Timers(message, timer) {
  let DaemonCapabilities(timers: timers, ..) = capabilities
  timers
}
