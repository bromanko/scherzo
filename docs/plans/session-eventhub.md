# Add live session events and an EventHub

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo has a first-class in-memory record of every live daemon worker session and every important worker event. This phase does not add a public control API, `scherzoctl attach`, a terminal renderer, or an operator pi skill. Its observable outcome is internal and testable: deterministic daemon and hub tests can start Scherzo with fake workers and fake pi, list the live session summary, replay recent events by cursor, prove pi events are published before the worker exits, and prove old events are bounded by the configured retention limit.

This is the foundation for terminal attach, a local control API, and an operator pi skill. Those later surfaces need a stable session/event model first, but they are deliberately out of scope here.

## Problem Framing and Constraints

The current daemon can run workers and route `runner.PiUpdate` messages through `src/scherzo/orchestrator/daemon.gleam`, but visibility is still mostly structured logs and `domain.RuntimeState` snapshots. Logs are useful for operators reading stderr, but they are a poor programmatic interface: future clients cannot ask for current sessions, cannot replay one worker's recent history by cursor, cannot see bounded event retention, and cannot distinguish lifecycle state from raw pi event payloads without scraping log text.

The operator need is concrete even though the interface is internal in this phase: before building `scherzoctl attach` or a pi-based operator assistant, Scherzo must keep structured session summaries and event history inside the daemon. The implementation must remain deterministic, must not block worker progress when observers or the hub are slow, must not leak secrets through replayable event payloads, and must continue to run without any observer attached.

This plan is written against the current post-daemon tree. `src/scherzo/orchestrator/daemon.gleam` must exist with `WorkerUpdate(String, runner.PiUpdate)`, `WorkerFinished`, monitored worker handles, `RuntimeDependencies`, and `daemon.start`. If a future checkout lacks that daemon seam, stop before implementing this plan and first restore the daemon behavior described in the Context and Orientation section below; do not invent a new orchestration architecture inside this phase.

## Strategy Overview

Add a small session package under `src/scherzo/session/`. The package defines stable summary and event types, exact JSON serializers, a raw-payload redaction helper, and an `EventHub` actor. The hub owns session summaries, globally monotonic event cursors, event timestamps, bounded per-session event buffers, and replay queries. The daemon remains the owner of scheduling state; the hub owns only observability state.

The hub must assign cursor and timestamp itself. Callers publish an unsequenced `EventPayload`; the stored `SessionEvent` is created inside the hub with the next cursor and the hub's injected `now_ms` clock. This avoids split ownership where the daemon, worker, or tests can accidentally create duplicate or out-of-order cursors.

Hub mutation APIs are fire-and-forget actor sends. `register_session`, `update_status`, `update_pi_session`, `publish`, and `finish_session` must not wait for a reply on the worker or daemon hot path. Query APIs such as `list_sessions`, `get_session`, and `events_after` use request/reply with a timeout and are intended for tests and future control clients. This means a dead hub can lose events after startup, but it cannot block worker completion. Daemon startup must fail clearly if the hub cannot be started.

The design is intentionally in-memory for this phase. Durable event files are attractive for postmortem analysis, but they add filesystem retention, cleanup, and privacy concerns before live attach exists. A bounded per-session buffer gives enough value for attaching to currently running sessions and replaying recent context without making event storage part of Scherzo's correctness story.

The most important sequencing choice is to enrich and stream pi events before wiring the hub into the daemon. Current `runner.PiUpdate` carries only `event` and optional `message`, and current `pi_rpc.prompt` still returns a list of events after `agent_end`. This phase changes `pi_rpc.prompt` to invoke an `on_event` callback as each event line is decoded, expands `runner.PiUpdate` to carry redacted raw JSON and normalized fields, and then maps those updates into EventHub payloads.

## Alternatives Considered

The simplest alternative is to parse structured stderr logs in a future `scherzoctl attach`. That is fragile because log formats are optimized for humans and process managers, not lossless replay. It would also force every future client to duplicate log parsing and would make per-session cursor semantics hard.

Another alternative is to add the local control API first and have it read daemon state directly. That would couple network protocol details to scheduler internals and make it harder to test event retention without a server. A standalone EventHub actor keeps the later API small and replaceable.

A third alternative is to persist every event as JSONL immediately. That may be useful later, but it is not required for live attach and creates new cleanup and privacy obligations. This phase keeps storage in memory and records a future extension point in the interfaces.

A fourth alternative is to add subscriber fan-out now. That is deferred. Replay by cursor is enough for this internal phase, and a future control API can add subscriptions after the retention, redaction, and cursor model is stable. Deferring subscribers also avoids slow-subscriber mailbox growth in the first version.

## Risks and Countermeasures

The main correctness risk is blocking worker progress while publishing events. Countermeasure: workers continue to send `WorkerUpdate` to the daemon through the existing asynchronous process message path, and the daemon publishes to the hub through fire-and-forget sends. Query calls are never used from worker code.

The main data-loss risk is throwing away raw pi details that the pretty renderer or operator assistant will need later. Countermeasure: `pi_rpc.RpcRecord` preserves the original raw JSON line, and `runner.PiUpdate` carries a redacted/truncated raw JSON value plus normalized fields such as pi event type, delta text, request id, method, pi session id, turn number, and token totals. Unknown pi event types are retained as `PiRaw` payloads instead of rejected.

The main secrecy risk is storing secrets in raw event payloads. Countermeasure: raw pi payloads are redacted before they leave `runner` in a `PiUpdate`. Redaction must recursively replace values under sensitive JSON keys whose names contain `token`, `api_key`, `authorization`, or `secret`; it must also replace every configured secret from `config.resolved_secrets(effective)` wherever that secret appears in a string value. Stored raw JSON is capped at 16 KiB per event and records whether truncation happened. Tests must prove that replayed event JSON does not contain the original secret values.

The main memory risk is unbounded event retention for long sessions. Countermeasure: the hub stores at most `default_max_events_per_session = 2000` events per session unless tests start it with a smaller limit. If the limit is exceeded, the oldest events are dropped. `events_after` reports `truncated: True` when the caller asks for a cursor older than the first retained event for that session.

The main race risk is inconsistent session status between the scheduler and hub. Countermeasure: the daemon publishes lifecycle transitions at the same points where it mutates runtime or worker-handle state: after handoff claim success and before spawning, after worker spawn, when receiving worker updates, when stop/shutdown is requested, on worker finish, and on worker down. The hub is observability-only, so scheduler correctness cannot depend on the hub being perfectly up to date.

The main compatibility risk is introducing a parallel session model while `src/scherzo/domain.gleam` still contains `LiveSession` and `RunningEntry.session`. Countermeasure: this phase leaves `domain.LiveSession` untouched as legacy runtime state and does not populate it. The new source of truth for attach/control visibility is `scherzo/session/event.gleam` plus `scherzo/session/hub.gleam`. A later cleanup may remove or repurpose `LiveSession` after the control API is built.

## Progress

- [x] (2026-04-28 18:40Z) Confirmed that the original session event plan depended on the long-lived daemon work.
- [x] (2026-04-28 18:40Z) Read the baseline modules `src/scherzo/domain.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/agent/runner.gleam`, and `src/scherzo/agent/pi_rpc.gleam` before writing the original plan.
- [x] (2026-04-28 20:45Z) Re-read the current post-daemon tree and verified that `src/scherzo/orchestrator/daemon.gleam`, `test/orchestrator_daemon_test.gleam`, daemon `WorkerUpdate`, monitored worker handles, and daemon startup/shutdown APIs now exist.
- [x] (2026-04-28 20:45Z) Ran `direnv exec . gleam test`; the current baseline reports `101 passed, no failures`.
- [x] (2026-04-28 20:45Z) Integrated review feedback by making the phase explicitly internal, making the plan self-contained against the current daemon seam, giving the EventHub ownership of cursors/timestamps, deferring subscriptions, adding raw-payload redaction/truncation rules, and strengthening live-publish tests.
- [x] (2026-04-28 21:10Z) Re-ran the baseline with `direnv exec . gleam test`; it still reports `101 passed, no failures` before implementation edits.
- [x] (2026-04-28 21:18Z) Added `src/scherzo/session/event.gleam`, `src/scherzo/session/json.gleam`, `src/scherzo/session/redaction.gleam`, `src/scherzo_redaction_ffi.erl`, and deterministic event/redaction tests; `direnv exec . gleam test` reports `106 passed, no failures`.
- [x] (2026-04-28 21:28Z) Enriched `pi_rpc.RpcRecord` and `runner.PiUpdate` with raw redacted payloads, turn/request/session/token metadata, lifecycle updates, and redacted messages; `direnv exec . gleam test` reports `108 passed, no failures`.
- [x] (2026-04-28 21:28Z) Changed `pi_rpc.prompt` to invoke an `on_event` callback as event lines are decoded, and added a fake-pi stall test proving `message_update` reaches the runner before the worker finishes.
- [x] (2026-04-28 21:35Z) Added `src/scherzo/session/hub.gleam` with asynchronous mutations, request/reply queries, hub-owned cursors/timestamps, retention truncation, invalid-limit handling, and deterministic hub tests; `direnv exec . gleam test` reports `113 passed, no failures`.
- [x] (2026-04-28 21:50Z) Wired daemon startup, dispatch, worker updates, worker finish/down, stop requests, retry scheduling, and token updates into the EventHub; daemon dependencies now fail startup when the hub cannot start.
- [x] (2026-04-28 21:50Z) Added deterministic tests for registration, live publishing before worker exit, replay ordering, truncation, redaction, summary updates, and hub-failure containment; `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, and `direnv exec . gleam run -- --help` all pass with `116 passed, no failures` for the test suite.

## Surprises & Discoveries

- Observation: The pre-implementation `runner.PiUpdate` type contained only `event` and optional `message`, so it was insufficient for a future renderer that needs request ids, tool metadata, UI methods, pi session id, token totals, or raw payloads.
  Evidence: Before this implementation, `src/scherzo/agent/runner.gleam` defined `PiUpdate(event: String, message: Option(String))` and emitted it after `pi_rpc.prompt` returned events. It now carries redacted raw JSON, turn, request id, method, pi session id, token totals, and optional tool name.

- Observation: The pre-implementation `pi_rpc.prompt` enforced turn and stall timeouts, but returned the full list of events only after `agent_end`, which was too late for live attach.
  Evidence: Before this implementation, `src/scherzo/agent/pi_rpc.gleam` accumulated events in `read_events_until_agent_end` and returned `List(RpcRecord)` when `agent_end` was decoded. It now also invokes `on_event` for each decoded event line before reading the next line.

- Observation: The pre-implementation daemon already routed worker updates through a single seam, but logged only selected updates and did not persist them.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` had `WorkerUpdate(String, runner.PiUpdate)` and `handle_worker_update`, and the worker callback sent `WorkerUpdate(issue.id, update)`. It now publishes enriched updates to the EventHub before applying the existing logging policy.

- Observation: The daemon live-publish test must poll the hub with small delays rather than issuing all replay queries in a tight loop.
  Evidence: Actor sends from the daemon and replay queries from the test process are independent senders; a tight loop can exhaust attempts before the worker has emitted the stalled update. The final test uses bounded 50 ms polling and still proves `message_update` is visible before `worker_exited`.

- Observation: `src/scherzo/domain.gleam` still contains `LiveSession`, but current daemon visibility does not use it as a stable attach model.
  Evidence: `domain.RunningEntry` has `session: Option(LiveSession)`, while daemon worker handles and snapshots do not provide event replay.

## Decision Log

- Decision: Implement session visibility as an in-memory EventHub actor before adding a control API or terminal renderer.
  Rationale: Multiple future surfaces need the same source of truth. A hub avoids log scraping and keeps protocol/UI code out of the scheduler.
  Date: 2026-04-28

- Decision: Store bounded in-memory event buffers and defer durable JSONL event archives.
  Rationale: Live attach needs recent replay and streaming, not durable audit storage. Durability adds cleanup and privacy risks that are not necessary for the first visibility increment.
  Date: 2026-04-28

- Decision: Preserve raw pi event payloads only after redaction and truncation.
  Rationale: Pi event shapes can evolve, and a later renderer may need fields that Scherzo does not normalize yet. Keeping redacted raw payloads makes unknown events debuggable without replaying secrets or unbounded data.
  Date: 2026-04-28

- Decision: The EventHub, not callers, owns event cursors and event timestamps.
  Rationale: Monotonic replay is the hub's central correctness property. Accepting pre-cursored events from the daemon or tests would make duplicate or out-of-order cursors possible.
  Date: 2026-04-28

- Decision: Hub mutation APIs are asynchronous and query APIs are request/reply.
  Rationale: Worker progress must not depend on observer speed or hub query latency. Tests and future clients can use read APIs after sends; actor mailbox ordering keeps same-sender mutation/query sequences deterministic.
  Date: 2026-04-28

- Decision: Defer `subscribe` until the control API phase.
  Rationale: Replay by cursor is enough to validate the event model now. Subscriber fan-out needs slow-subscriber, unsubscribe, and mailbox-growth rules that are better designed with the actual control transport.
  Date: 2026-04-28

- Decision: Leave `domain.LiveSession` untouched in this phase.
  Rationale: Removing or repurposing it would expand the migration blast radius. The new session package becomes the source of truth for attach/control visibility, and legacy runtime state can be cleaned up later.
  Date: 2026-04-28

- Decision: Add `RuntimeDependencies.start_event_hub` instead of constructing the hub only in `service.start_daemon`.
  Rationale: Production startup still creates a required hub, while daemon tests can inject small-retention hubs or a failing startup result without changing scheduler behavior or adding a network/control surface.
  Date: 2026-04-28

- Decision: Redact configured secrets from normalized assistant message text as well as raw JSON.
  Rationale: The EventHub stores both `payload.message` and `payload.raw_json`; redacting only raw JSON would still allow configured secrets in replayable message deltas.
  Date: 2026-04-28

## Outcomes & Retrospective

Implemented the internal session event foundation. The tree now has stable session/event types and JSON serializers, recursive raw JSON redaction with 16 KiB truncation, raw pi record preservation, streaming pi event callbacks before `agent_end`, enriched `runner.PiUpdate` values, an in-memory EventHub actor with bounded replay by cursor, and daemon publication of lifecycle and worker update events. The README documents that the model is internal and in-memory until a control API or attach surface is intentionally added.

Final validation on 2026-04-28 passed: `direnv exec . gleam format --check src test`, `direnv exec . gleam test` with `116 passed, no failures`, and `direnv exec . gleam run -- --help`. The main event-shape mismatch discovered during implementation was that normalized message deltas needed their own configured-secret redaction, not only raw JSON redaction. The live-before-exit tests caught the original buffered pi behavior at the runner layer and now prove both runner callbacks and daemon hub publication happen before worker completion. No durable event storage, subscriptions, public control API, or `scherzoctl attach` surface was added. No jj commit was created in this session because the working copy already contains unrelated documentation changes, including `docs/TODO.md` and `docs/plans/linear-session-results.md`, so commit grouping should be handled separately.

## Context and Orientation

Scherzo is a Gleam Erlang-target project. Runtime modules live under `src/scherzo/`, tests live under `test/`, and validation is run from the repository root with `direnv exec . gleam test`. The scheduler logic is split between pure scheduling in `src/scherzo/orchestrator/core.gleam` and runtime orchestration in `src/scherzo/orchestrator/daemon.gleam`. The pi runner lives in `src/scherzo/agent/runner.gleam` and pi JSON Lines RPC handling lives in `src/scherzo/agent/pi_rpc.gleam`.

A worker session is one Scherzo worker attempt for one Linear issue. It has a Scherzo session id, a Linear issue id and identifier, a workspace path, an optional pi session id returned by pi's `get_state`, a status, token totals, current turn, and recent events. A session event is a structured record with a hub-assigned cursor, hub-assigned timestamp, session id, issue id, kind, event name, optional turn number, optional normalized text, optional request/method/tool identity, token totals, and optional redacted raw pi payload.

The daemon is the only process that mutates scheduling state. The EventHub introduced by this plan is not allowed to claim issues, dispatch workers, retry, cleanup workspaces, or decide whether a worker should stop. It only records observability state and serves read requests from later API and CLI layers.

The current daemon seam relevant to this plan is in `src/scherzo/orchestrator/daemon.gleam`. Its public `Message` type includes `PollTick`, `RetryTick`, `WorkerFinished`, `WorkerUpdate(String, runner.PiUpdate)`, `WorkerDown`, `SideEffectFinished`, `Shutdown`, and `GetSnapshot`. Its `WorkerHandle` includes issue id, issue, run id, pid, monitor, workspace path, Scherzo session id, and optional command subject. Its `RuntimeDependencies` includes tracker and handoff constructors, `agent_runner`, cleanup, structured logger, `now_ms`, timer send, timer cancellation, and `start_event_hub`. The EventHub subject lives in daemon state and records observability without becoming a scheduler.

## Preconditions and Verified Facts

The current baseline commands from the repository root are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

On 2026-04-28 before implementation, `direnv exec . gleam test` ended with `101 passed, no failures`. After implementation, `direnv exec . gleam test` ended with `116 passed, no failures`.

The current tree contains `src/scherzo/orchestrator/daemon.gleam`; daemon mode is long-lived; workers send `WorkerUpdate(issue_id, runner.PiUpdate)` messages to the daemon; daemon workers are monitored; and `daemon.shutdown` is testable. If any of these facts are false in the checkout being modified, stop and restore this daemon seam before continuing.

Facts changed by this implementation:

- `src/scherzo/agent/runner.gleam` now defines an enriched `PiUpdate` with event name, optional redacted message, optional redacted raw JSON, turn, request id, method, pi session id, token totals, and optional tool name.
- `src/scherzo/agent/pi_rpc.gleam` now preserves the original raw JSON line in `RpcRecord.raw_json` and calls a streaming `on_event` callback as each prompt event line is decoded.
- `src/scherzo/session/redaction.gleam` and `src/scherzo_redaction_ffi.erl` provide recursive raw JSON redaction and truncation for replayable event payloads.
- `src/scherzo/session/hub.gleam` now owns in-memory session summaries, hub-assigned cursors/timestamps, bounded per-session buffers, and replay queries.
- `src/scherzo/domain.gleam` still defines `LiveSession`; this implementation leaves it in place and creates the new session event model separately.

No new package dependency should be required. A small Erlang FFI module may be added if it is the simplest way to recursively redact arbitrary raw JSON using Erlang/OTP 27's JSON support, which is already required by `gleam_json`.

## Scope Boundaries

In scope: session summary types; session event and unsequenced payload types; exact JSON serialization; raw pi payload redaction and truncation; bounded per-session event buffers; monotonic hub-assigned event cursors; EventHub actor start/stop; asynchronous APIs to register, update, publish, and finish; request/reply APIs to list, get, and replay; daemon lifecycle publishing; worker pi update publishing; tests for event ordering, replay, truncation, redaction, summary updates, live-before-exit streaming, and startup containment.

Out of scope: local TCP or HTTP control server; `scherzoctl`; pretty terminal rendering; mutating controls; pi operator skill; EventHub subscriptions; durable event archives; web dashboard; cross-process session recovery after daemon restart; distributed event storage; removal of `domain.LiveSession`.

## Milestones

Milestone 1 defines stable session and event data. At the end, tests can construct a `SessionSummary`, an unsequenced `EventPayload`, a hub-stored `SessionEvent`, and JSON-compatible maps without starting a daemon. This comes first because later actor and protocol code should depend on these types rather than inventing local shapes.

Milestone 2 adds redaction and enriches pi event propagation. At the end, fake pi events preserve the raw JSON line in `RpcRecord`, convert it into a redacted/truncated raw value in `runner.PiUpdate`, and carry normalized fields from `pi_rpc` through the runner update path.

Milestone 3 streams pi events as they are decoded. At the end, a fake pi transcript can emit `message_update` and then stall before `agent_end`, while a test receives the update before the runner exits. This de-risks the live-attach claim before the hub is integrated.

Milestone 4 implements the EventHub actor in isolation. At the end, tests can start a hub, register sessions, publish unsequenced payloads, replay from cursors, observe truncation, update summary status and pi session id, and finish sessions without a real daemon or pi.

Milestone 5 wires the daemon to the hub. At the end, daemon tests show lifecycle events around dispatch and worker finish, and pi event updates appear in the hub in order while existing scheduling behavior remains unchanged.

Milestone 6 documents the internal event model and validates the full phase. At the end, all deterministic tests pass, `README.md` documents that session visibility is in-memory and internal until the control API phase, and this plan records the final event interface.

## Plan of Work

Create `src/scherzo/session/event.gleam`. Define `SessionStatus` variants for `Preparing`, `Probing`, `Running`, `WaitingUi`, `Stopping`, and `Exited(reason: String)`. Define `EventKind` variants for `Lifecycle`, `Pi`, `AssistantMessage`, `Tool`, `UiRequest`, `UiResponse`, `TokenStats`, `Error`, and `PiRaw`. Define `RedactedRawJson`, `EventPayload`, `SessionSummary`, `SessionEvent`, `EventPage`, and helper constructors. `EventPayload` must not contain cursor or timestamp fields. `SessionEvent` must contain cursor and timestamp and must be constructed by the hub.

Create `src/scherzo/session/json.gleam` with pure functions that convert summaries, events, and event pages to JSON strings. Do not add a transport protocol here. Tests must parse the serialized JSON and assert exact field names and values, not only string containment.

Create `src/scherzo/session/redaction.gleam`. Expose `redact_raw_json(raw: String, secrets: List(String)) -> RedactedRawJson`. The helper parses JSON, recursively redacts values under sensitive keys, replaces configured secret values inside all string values, canonicalizes back to JSON, truncates to 16 KiB, and records whether truncation occurred. If parsing unexpectedly fails, return a redacted/truncated JSON string value containing `"[unavailable malformed raw json]"`; normal malformed pi lines still fail in `pi_rpc.decode_record` and are not published as events.

Modify `src/scherzo/agent/pi_rpc.gleam` so `RpcRecord` preserves the original raw JSON line. When decoding succeeds, store the raw line. When decoding fails, return the existing malformed JSON error without publishing an event.

Modify `src/scherzo/agent/pi_rpc.gleam` so `prompt` accepts an `on_event: fn(RpcRecord) -> Nil` callback. Keep returning the full list of events for compatibility with existing tests, but invoke `on_event(record)` immediately after each event record is decoded and before waiting for the next line. For `extension_ui_request`, invoke the callback before sending Scherzo's automatic `extension_ui_response`, then continue recording the event.

Modify `src/scherzo/agent/runner.gleam` so `PiUpdate` includes `event`, `message`, `raw_json`, `turn`, `request_id`, `method`, `pi_session_id`, `tokens`, and optional `tool_name`. Runner creates `PiUpdate` values in the pi event callback, using `session/redaction.redact_raw_json(record.raw_json, config.resolved_secrets(effective_config))` before emitting; if the current function argument named `config` shadows the imported `scherzo/config` module, rename that argument to `effective_config` as part of this edit. Emit lifecycle-style updates for `probe_started`, `probe_finished`, and `pi_session_started` where the runner has that information.

Create `src/scherzo/session/hub.gleam`. Implement an OTP actor that stores a dictionary of session id to summary, a dictionary of session id to retained events, a dictionary of whether that session has dropped older events, and a global next cursor. Export asynchronous mutation functions `register_session`, `update_status`, `update_pi_session`, `update_tokens`, `publish`, and `finish_session`. Export request/reply functions `list_sessions`, `get_session`, and `events_after` that take a timeout and return `Result`. `events_after(session_id, cursor, limit)` returns only events with `event.cursor > cursor`; `EventPage.next_cursor` is the last returned event cursor, or the input cursor if no events are returned. `EventPage.truncated` is true when older events were dropped and the requested cursor is older than the first retained cursor.

Modify `src/scherzo/orchestrator/daemon.gleam` so `RuntimeDependencies` or daemon state contains a required EventHub subject. Production `service.start_daemon` must start a hub with `hub.default_max_events_per_session` and pass the subject into daemon dependencies; daemon startup must fail if the hub cannot be started. Tests may start a hub with a smaller retention limit and pass it to the daemon.

Modify daemon dispatch and worker handling. After handoff claim success and before spawning the worker, register a `SessionSummary` with status `Preparing`, computed workspace path, issue id, issue identifier, issue title, session id, zero tokens, current turn zero, and timestamps from the daemon/hub clocks. Use a Scherzo session id from a helper such as `make_session_id(issue.identifier, run_id)`, not the pi session id. Publish lifecycle payloads for `dispatch_started`, `worker_started`, `stop_requested`, `worker_exited`, `worker_down`, `retry_scheduled`, and cleanup-related events where the daemon already handles those state transitions.

Modify daemon worker handles or an adjacent dictionary so each issue id maps to the Scherzo session id. On `WorkerUpdate`, convert the enriched `runner.PiUpdate` to an EventHub payload. Update summary status to `Probing` for probe events, `Running` for pi start/normal pi events, `WaitingUi` for blocking UI request methods, `Running` again after automatic UI response or the next non-UI event, and `Exited` when the worker finishes. When a pi session id becomes available, call `hub.update_pi_session`. When token totals become available, call `hub.update_tokens`.

Update `README.md` with a short section named `Session event model` explaining that Scherzo records in-memory session summaries and recent event buffers for future control clients, that replay is internal until the control API phase, that raw pi payloads are redacted and truncated, and that event history is not durable across daemon restart.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current passing test count in this plan's Progress section. The current expected output is `101 passed, no failures`.

2. Create `test/session_event_test.gleam`. Add `session_summary_serializes_exact_required_fields_test` that constructs a summary for issue `ABC-123` with workspace `test/tmp/workspaces/ABC-123`, serializes it, parses the JSON, and asserts exact values for `session_id`, `issue_id`, `issue_identifier`, `workspace_path`, `status`, `current_turn`, and `tokens.total`.

3. In `test/session_event_test.gleam`, add `event_page_serializes_cursor_and_truncation_test` that constructs two stored events with cursors 1 and 2, creates `EventPage(events, next_cursor: 2, truncated: True)`, serializes it, parses the JSON, and asserts that event cursors are `[1, 2]`, `next_cursor` is `2`, and `truncated` is boolean `true`.

4. In `test/session_event_test.gleam`, add `event_payload_has_no_cursor_or_timestamp_test` that constructs an `EventPayload` and verifies the payload type/serializer cannot accept caller-provided cursor or timestamp fields.

5. Create `src/scherzo/session/event.gleam` and `src/scherzo/session/json.gleam` with the types and serializers described above. Run `direnv exec . gleam test`; the new event tests should pass.

6. Create `test/session_redaction_test.gleam`. Add `raw_json_redaction_removes_sensitive_keys_and_configured_secrets_test` with raw JSON containing nested `token`, `api_key`, `authorization`, and `secret` fields plus a configured secret embedded in a normal string. Assert the redacted output contains `[REDACTED]` and does not contain the original secret values.

7. In the same file, add `raw_json_redaction_truncates_large_payload_test` with a payload larger than 16 KiB. Assert `truncated == True` and the stored value length is at or below the configured cap.

8. Implement `src/scherzo/session/redaction.gleam` and any small FFI needed for recursive JSON redaction. Run `direnv exec . gleam test`; redaction tests should pass.

9. Update `test/pi_rpc_test.gleam` so `decode_response_and_event_test` asserts that a decoded event includes the original raw JSON string and that the raw JSON string contains `message_update`.

10. Modify `src/scherzo/agent/pi_rpc.gleam` to add the raw field to `RpcRecord` and set it in `decode_record`. Run `direnv exec . gleam test` and update constructor sites that now need the new field.

11. Update `test/agent_runner_test.gleam` or create `test/runner_update_test.gleam` with `runner_update_preserves_redacted_raw_pi_event_test`. Use the fake pi transcript and an `emit_update` callback that records updates in a test subject. Assert that at least one update has `event == "message_update"`, `message == Some("POPULATED")`, `raw_json` containing `message_update`, and no configured secret values.

12. Modify `src/scherzo/agent/runner.gleam` so `PiUpdate` includes the normalized and redacted raw fields. Run `direnv exec . gleam test` and fix all references to the old `PiUpdate` shape, including daemon tests.

13. Add `runner_streams_update_before_agent_end_test`. Configure fake pi with `FAKE_PI_STALL_AFTER_PROMPT` so it emits `message_update` and then stalls before `turn_end`/`agent_end`. Run `runner.run_attempt` in a spawned process, receive the `message_update` from the test subject before the runner returns, and assert that the worker has not yet sent its finished marker. This test should fail before the streaming callback change.

14. Modify `src/scherzo/agent/pi_rpc.gleam` so `prompt` accepts and invokes `on_event` as each event is decoded. Modify `runner.run_attempt` to emit updates from that callback instead of only iterating the final event list. Run `direnv exec . gleam test`; the live-before-exit test must pass.

15. Create `test/session_hub_test.gleam`. Add `hub_registers_lists_and_finishes_session_test`: start a hub with retention limit 10 and deterministic `now_ms`, register one session, assert `list_sessions` returns it as `Preparing`, update it to `Running`, finish it with reason `normal`, and assert `get_session` returns `Exited("normal")`.

16. In `test/session_hub_test.gleam`, add `hub_assigns_monotonic_cursors_and_timestamps_test`: publish three unsequenced payloads, call `events_after(session_id, 0, 10)`, and assert returned cursors are `[1, 2, 3]` and timestamps come from the injected clock.

17. In `test/session_hub_test.gleam`, add `hub_replays_events_after_cursor_without_duplicates_test`: after publishing three events, call `events_after(session_id, 1, 10)` and assert only cursors `[2, 3]` are returned with `next_cursor == 3`.

18. In `test/session_hub_test.gleam`, add `hub_truncates_old_events_test`: start a hub with retention limit 2, publish three events, call `events_after(session_id, 0, 10)`, and assert only cursors `[2, 3]` are returned and `truncated == True`.

19. In `test/session_hub_test.gleam`, add `hub_rejects_invalid_replay_limit_test`: call `events_after(session_id, 0, 0)` and assert `Error(InvalidLimit(0))`. Also assert a limit larger than retention is clamped rather than growing memory.

20. Implement `src/scherzo/session/hub.gleam` with the actor APIs. Run `direnv exec . gleam test`; the hub tests should pass and no existing tests should hang.

21. Update `src/scherzo/orchestrator/daemon.gleam` dependencies/state to include the hub subject and an issue-id-to-session-id mapping. Update `test/orchestrator_daemon_test.gleam` helpers to start a test hub and pass it through daemon dependencies.

22. Add `test/orchestrator_daemon_session_event_test.gleam`. Use fake dependencies to start the daemon, dispatch one issue, and then query the hub. Assert that the hub lists one session with issue identifier `ABC-123`, workspace path from `workspace.workspace_path`, and status eventually `Exited("normal")`.

23. In the same daemon session test, assert that replayed events include `dispatch_started`, `worker_started`, a fake pi event such as `message_update`, and `worker_exited` in increasing cursor order.

24. Add a daemon live-publish test where fake pi emits `message_update` and stalls before `agent_end`. Query the hub during the stall and assert the `message_update` event is present before `worker_exited` exists.

25. Modify daemon dispatch, worker update, worker finish, worker down, stop, and cleanup effect interpretation to publish the lifecycle events required by the tests. Use asynchronous hub mutation calls only; do not block daemon worker handling on hub queries.

26. Add a daemon startup test or service test proving that production daemon startup returns an error if the hub cannot be started. If hub publication after startup is best-effort and the hub dies, document that later queries time out or return unavailable rather than killing workers.

27. Run `direnv exec . gleam format` and `direnv exec . gleam test`. Expect all tests to pass. Record the final pass count in Progress.

28. Update `README.md` with the internal session event model note. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test` again.

29. Update this plan's Outcomes & Retrospective with the final test count, any event-shape mismatches discovered, and whether live-before-exit publication passed without sleeps that make tests flaky.

30. Commit the phase with a message such as `Add Scherzo session event hub` after the tree is green.

## Testing and Falsifiability

The event model is falsified if serialization loses required fields, if unknown pi events are rejected instead of retained as `PiRaw`, if raw pi JSON is not available after redaction to future renderers, or if replayed JSON contains configured secrets or sensitive-key values.

The EventHub is falsified if cursors are not monotonic, if callers can provide their own cursors, if replay from a cursor returns duplicate or missing retained events, if retention grows without bound, if `truncated` is false after older requested events were dropped, or if a query with an invalid limit succeeds silently.

The live-session claim is falsified if pi updates are only visible after `agent_end` or worker completion. The required fake-pi stall tests must prove `message_update` reaches the runner update callback and the daemon EventHub before `worker_exited` is published.

Add deterministic tests in `test/session_event_test.gleam`, `test/session_redaction_test.gleam`, `test/session_hub_test.gleam`, `test/pi_rpc_test.gleam`, `test/agent_runner_test.gleam` or `test/runner_update_test.gleam`, and `test/orchestrator_daemon_session_event_test.gleam` as described in the Concrete Steps. No test may require real Linear, a real pi executable, or network access. Use `test/fixtures/fake_pi_rpc.sh` for pi events, extending it only with deterministic environment-variable-controlled behavior.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, new tests that reference `scherzo/session/event`, `scherzo/session/redaction`, `scherzo/session/hub`, raw `RpcRecord` fields, enriched `PiUpdate`, or streaming prompt callbacks should fail to compile or fail behaviorally. After implementation, the full test suite should pass.

## Validation and Acceptance

Accept this phase when all deterministic tests pass and a daemon integration test proves this behavior: one fake Linear issue dispatch creates one session summary; pi event updates appear in the hub before worker completion; `events_after` replays ordered events by cursor without duplicates; old events are bounded and report truncation; replayed raw payloads are redacted and capped; finishing the worker marks the session as exited without deleting its recent events.

There is no manual operator acceptance command in this phase beyond tests because no control API or CLI exists yet. The README must explicitly say the event model is internal until the control API phase and in-memory only until durable archives are intentionally designed.

## Rollout, Recovery, and Idempotence

This phase is additive. If the EventHub fails to start in production daemon mode, daemon startup fails clearly because later phases require the hub. If hub publication fails after startup or the hub subject becomes unavailable, Scherzo should continue scheduling and workers should continue running; the consequence is lost observability events, not failed work. Query APIs return `HubUnavailable` or `ActorCallTimeout` when the hub cannot answer.

The change is reversible by removing the session modules and daemon publication calls. Workspaces and Linear state are unaffected. Re-running tests is idempotent because event state lives only inside test-started actors.

## Artifacts and Notes

Expected event replay shape for later phases:

    cursor=1 kind=lifecycle name=dispatch_started session_id=ABC-123-run-42
    cursor=2 kind=lifecycle name=worker_started session_id=ABC-123-run-42
    cursor=3 kind=pi name=agent_start pi_type=agent_start session_id=ABC-123-run-42
    cursor=4 kind=pi name=message_update pi_type=message_update message=POPULATED session_id=ABC-123-run-42
    cursor=5 kind=lifecycle name=worker_exited session_id=ABC-123-run-42

Keep the exact serialized JSON stable once the control API phase starts using it. Before that phase, any schema adjustment must be made here and covered by exact JSON tests.

## Interfaces and Dependencies

In `src/scherzo/session/event.gleam`, define types equivalent to:

    pub type SessionStatus {
      Preparing
      Probing
      Running
      WaitingUi
      Stopping
      Exited(reason: String)
    }

    pub type EventKind {
      Lifecycle
      Pi
      AssistantMessage
      Tool
      UiRequest
      UiResponse
      TokenStats
      Error
      PiRaw
    }

    pub type RedactedRawJson {
      RedactedRawJson(value: String, truncated: Bool)
    }

    pub type EventPayload {
      EventPayload(
        kind: EventKind,
        name: String,
        turn: Option(Int),
        pi_type: Option(String),
        message: Option(String),
        request_id: Option(String),
        method: Option(String),
        tool_name: Option(String),
        tokens: domain.TokenTotals,
        raw_json: Option(RedactedRawJson),
      )
    }

    pub type SessionSummary {
      SessionSummary(
        session_id: String,
        issue_id: String,
        issue_identifier: String,
        issue_title: String,
        workspace_path: String,
        pi_session_id: Option(String),
        status: SessionStatus,
        current_turn: Int,
        started_at_ms: Int,
        last_event_at_ms: Int,
        token_totals: domain.TokenTotals,
      )
    }

    pub type SessionEvent {
      SessionEvent(
        cursor: Int,
        at_ms: Int,
        session_id: String,
        issue_id: String,
        payload: EventPayload,
      )
    }

    pub type EventPage {
      EventPage(
        events: List(SessionEvent),
        next_cursor: Int,
        truncated: Bool,
      )
    }

In `src/scherzo/session/hub.gleam`, expose functions equivalent to:

    pub const default_max_events_per_session = 2000

    pub type HubError {
      HubUnavailable
      SessionNotFound(String)
      InvalidLimit(Int)
      ActorCallTimeout
    }

    pub fn start(
      max_events_per_session: Int,
      now_ms: fn() -> Int,
    ) -> Result(process.Subject(Message), HubError)

    pub fn stop(subject: process.Subject(Message)) -> Nil

    pub fn register_session(
      subject: process.Subject(Message),
      summary: event.SessionSummary,
    ) -> Nil

    pub fn update_status(
      subject: process.Subject(Message),
      session_id: String,
      status: event.SessionStatus,
    ) -> Nil

    pub fn update_pi_session(
      subject: process.Subject(Message),
      session_id: String,
      pi_session_id: String,
    ) -> Nil

    pub fn update_tokens(
      subject: process.Subject(Message),
      session_id: String,
      tokens: domain.TokenTotals,
    ) -> Nil

    pub fn publish(
      subject: process.Subject(Message),
      session_id: String,
      payload: event.EventPayload,
    ) -> Nil

    pub fn finish_session(
      subject: process.Subject(Message),
      session_id: String,
      reason: String,
    ) -> Nil

    pub fn list_sessions(
      subject: process.Subject(Message),
      timeout_ms: Int,
    ) -> Result(List(event.SessionSummary), HubError)

    pub fn get_session(
      subject: process.Subject(Message),
      session_id: String,
      timeout_ms: Int,
    ) -> Result(Option(event.SessionSummary), HubError)

    pub fn events_after(
      subject: process.Subject(Message),
      session_id: String,
      cursor: Int,
      limit: Int,
      timeout_ms: Int,
    ) -> Result(event.EventPage, HubError)

In `src/scherzo/agent/pi_rpc.gleam`, extend `RpcRecord` with `raw_json: String`. Change `prompt` to the equivalent of:

    pub fn prompt(
      session: Session,
      message: String,
      read_timeout_ms: Int,
      turn_timeout_ms: Int,
      stall_timeout_ms: Int,
      on_event: fn(RpcRecord) -> Nil,
    ) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)

In `src/scherzo/agent/runner.gleam`, define `PiUpdate` equivalent to:

    pub type PiUpdate {
      PiUpdate(
        event: String,
        message: Option(String),
        raw_json: Option(event.RedactedRawJson),
        turn: Option(Int),
        request_id: Option(String),
        method: Option(String),
        pi_session_id: Option(String),
        tokens: domain.TokenTotals,
        tool_name: Option(String),
      )
    }

No new package dependency should be required. Use existing `gleam_otp`, `gleam_erlang`, `gleam_json`, and Scherzo logging/config redaction helpers. If recursive raw JSON redaction is simpler in Erlang, add a small local FFI module rather than adding a dependency.

## Revision Notes

2026-04-28: This revision integrates the adversarial plan review. It reframes the phase as an internal foundation rather than an operator-facing feature, updates the preconditions to the current daemon tree, gives the EventHub sole ownership of cursors and timestamps, removes `subscribe` from this phase, defines redaction/truncation for replayable raw payloads, records the `domain.LiveSession` compatibility decision, and strengthens tests so live publication before `agent_end` is falsifiable.
