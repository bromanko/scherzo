import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import scherzo/config/types as config_types
import scherzo/log
import scherzo/orchestrator/daemon_capabilities
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/outbox_effects
import scherzo/session/hub
import scherzo/state/record
import test_async

pub type TestMessage {
  TimerFired(String)
}

pub type TestTimer {
  TestTimer(Int)
}

type TestState {
  TestState(events: List(String))
}

type LogObservation {
  LogObservation(String, String, List(log.Field), List(String))
}

type TimerObservation {
  SentAfter(Int, TestMessage)
  Cancelled(TestTimer)
}

type LedgerObservation {
  LedgerBodies(String, Int)
  LedgerBestEffort(String, Int)
  LedgerRecords(String, Int)
}

type EffectObservation {
  EffectEnqueued
  OutboxEnqueued(String)
  OutboxAttempted(String, Int)
}

fn new_state() -> TestState {
  TestState(events: [])
}

fn record_event(state: TestState, event: String) -> TestState {
  let TestState(events:) = state
  TestState(events: [event, ..events])
}

fn state_events(state: TestState) -> List(String) {
  let TestState(events:) = state
  list.reverse(events)
}

fn test_hub_subject() -> process.Subject(hub.Message) {
  process.new_subject()
}

fn test_intent(id: String) -> outbox_effects.Intent {
  outbox_effects.Intent(
    outbox_id: id,
    task_ref: record.linear_task_ref_fields(id, Some("LIV-1454"), None),
    outbox_kind: "test",
    dedupe_key: "dedupe-" <> id,
    payload_json: "{}",
  )
}

fn test_effect(intent: outbox_effects.Intent) -> effect_runner.Effect {
  effect_runner.CleanupWorkspace(
    root: "root",
    workspace_path: intent.outbox_id,
    hooks: config_types.HooksConfig(None, None, None, None, 0),
    cleanup: fn(_, _, _) { Ok(Nil) },
  )
}

pub fn daemon_capability_accessors_use_runtime_fakes_test() {
  let log_subject = process.new_subject()
  let timer_subject = process.new_subject()
  let ledger_subject = process.new_subject()
  let effect_subject = process.new_subject()
  let target_subject = process.new_subject()

  let capabilities =
    daemon_capabilities.daemon_capabilities(
      clock: daemon_capabilities.clock(fn() { 1234 }),
      logger: daemon_capabilities.logger(fn(level, event, fields, secrets) {
        process.send(log_subject, LogObservation(level, event, fields, secrets))
        Ok(Nil)
      }),
      events: daemon_capabilities.event_publisher(test_hub_subject(), fn() {
        1234
      }),
      ledger: daemon_capabilities.ledger_writer(
        append_bodies: fn(state, bodies, event) {
          process.send(ledger_subject, LedgerBodies(event, list.length(bodies)))
          #(record_event(state, "bodies:" <> event), True)
        },
        append_bodies_best_effort: fn(state, bodies, event) {
          process.send(
            ledger_subject,
            LedgerBestEffort(event, list.length(bodies)),
          )
          record_event(state, "best_effort:" <> event)
        },
        append_records: fn(state, records, event) {
          process.send(
            ledger_subject,
            LedgerRecords(event, list.length(records)),
          )
          #(record_event(state, "records:" <> event), Ok(Nil))
        },
      ),
      effects: daemon_capabilities.effect_queue(
        enqueue: fn(state, _effect) {
          process.send(effect_subject, EffectEnqueued)
          record_event(state, "effect")
        },
        enqueue_outbox: fn(state, intent, make_effect) {
          let _ = make_effect(intent)
          process.send(effect_subject, OutboxEnqueued(intent.outbox_id))
          record_event(state, "outbox:" <> intent.outbox_id)
        },
        enqueue_outbox_with_attempt_count: fn(
          state,
          intent,
          attempt_count,
          make_effect,
        ) {
          let _ = make_effect(intent)
          process.send(
            effect_subject,
            OutboxAttempted(intent.outbox_id, attempt_count),
          )
          record_event(state, "attempt:" <> intent.outbox_id)
        },
        enqueue_outbox_with_attempt_count_result: fn(
          state,
          intent,
          attempt_count,
          make_effect,
        ) {
          let _ = make_effect(intent)
          process.send(
            effect_subject,
            OutboxAttempted(intent.outbox_id, attempt_count),
          )
          #(
            record_event(state, "attempt_result:" <> intent.outbox_id),
            attempt_count == 1,
          )
        },
      ),
      timers: daemon_capabilities.timers(
        send_after: fn(subject, delay_ms, message) {
          process.send(timer_subject, SentAfter(delay_ms, message))
          process.send(subject, message)
          TestTimer(delay_ms)
        },
        cancel_timer: fn(timer) {
          process.send(timer_subject, Cancelled(timer))
          Nil
        },
      ),
    )

  assert daemon_capabilities.now_ms(daemon_capabilities.daemon_clock(
      capabilities,
    ))
    == 1234
  assert daemon_capabilities.event_now_ms(daemon_capabilities.daemon_events(
      capabilities,
    ))
    == 1234

  let assert Ok(Nil) =
    daemon_capabilities.write(
      daemon_capabilities.daemon_logger(capabilities),
      "info",
      "capability_test",
      [#("count", "1")],
      ["secret"],
    )
  assert test_async.expect_message(log_subject)
    == LogObservation("info", "capability_test", [#("count", "1")], ["secret"])

  let timer =
    daemon_capabilities.send_after(
      daemon_capabilities.daemon_timers(capabilities),
      target_subject,
      25,
      TimerFired("ready"),
    )
  assert timer == TestTimer(25)
  assert test_async.expect_message(timer_subject)
    == SentAfter(25, TimerFired("ready"))
  assert test_async.expect_message(target_subject) == TimerFired("ready")
  daemon_capabilities.cancel_timer(
    daemon_capabilities.daemon_timers(capabilities),
    timer,
  )
  assert test_async.expect_message(timer_subject) == Cancelled(TestTimer(25))
  test_async.assert_no_extra_message(timer_subject)

  let state0 = new_state()
  let #(state1, appended) =
    daemon_capabilities.append_bodies(
      daemon_capabilities.daemon_ledger(capabilities),
      state0,
      [record.RunInterrupted("run-1", "issue-1", "stop")],
      "ledger_bodies",
    )
  assert appended == True
  let state2 =
    daemon_capabilities.append_bodies_best_effort(
      daemon_capabilities.daemon_ledger(capabilities),
      state1,
      [record.RunInterrupted("run-2", "issue-2", "retry")],
      "ledger_best_effort",
    )
  let #(state3, records_result) =
    daemon_capabilities.append_records(
      daemon_capabilities.daemon_ledger(capabilities),
      state2,
      [
        record.LedgerRecord(
          record_id: "rec-1",
          at_ms: 1,
          body: record.RunInterrupted("run-3", "issue-3", "done"),
        ),
      ],
      "ledger_records",
    )
  assert records_result == Ok(Nil)
  assert test_async.expect_message(ledger_subject)
    == LedgerBodies("ledger_bodies", 1)
  assert test_async.expect_message(ledger_subject)
    == LedgerBestEffort("ledger_best_effort", 1)
  assert test_async.expect_message(ledger_subject)
    == LedgerRecords("ledger_records", 1)

  let state4 =
    daemon_capabilities.enqueue(
      daemon_capabilities.daemon_effects(capabilities),
      state3,
      test_effect(test_intent("effect-only")),
    )
  let state5 =
    daemon_capabilities.enqueue_outbox(
      daemon_capabilities.daemon_effects(capabilities),
      state4,
      test_intent("outbox-1"),
      test_effect,
    )
  let state6 =
    daemon_capabilities.enqueue_outbox_with_attempt_count(
      daemon_capabilities.daemon_effects(capabilities),
      state5,
      test_intent("outbox-2"),
      2,
      test_effect,
    )
  let #(state7, first_outbox_append) =
    daemon_capabilities.enqueue_outbox_with_attempt_count_result(
      daemon_capabilities.daemon_effects(capabilities),
      state6,
      test_intent("outbox-3"),
      1,
      test_effect,
    )
  let #(state8, duplicate_outbox_append) =
    daemon_capabilities.enqueue_outbox_with_attempt_count_result(
      daemon_capabilities.daemon_effects(capabilities),
      state7,
      test_intent("outbox-3"),
      2,
      test_effect,
    )
  assert first_outbox_append == True
  assert duplicate_outbox_append == False
  assert test_async.expect_message(effect_subject) == EffectEnqueued
  assert test_async.expect_message(effect_subject) == OutboxEnqueued("outbox-1")
  assert test_async.expect_message(effect_subject)
    == OutboxAttempted("outbox-2", 2)
  assert test_async.expect_message(effect_subject)
    == OutboxAttempted("outbox-3", 1)
  assert test_async.expect_message(effect_subject)
    == OutboxAttempted("outbox-3", 2)

  assert state_events(state8)
    == [
      "bodies:ledger_bodies",
      "best_effort:ledger_best_effort",
      "records:ledger_records",
      "effect",
      "outbox:outbox-1",
      "attempt:outbox-2",
      "attempt_result:outbox-3",
      "attempt_result:outbox-3",
    ]
}
