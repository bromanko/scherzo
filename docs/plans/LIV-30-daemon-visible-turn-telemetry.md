# Add daemon-visible turn telemetry

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Operators need a central, low-risk way to see what an active Scherzo worker is doing at the level of turns: which turn is running, whether a turn has finished, whether it stopped or failed, and how token usage is changing. Today the daemon-visible session state has a `current_turn` integer and bounded event replay, but it does not provide a normalized turn lifecycle that is independent of raw pi records. After this change, `scherzoctl ps`, `scherzoctl session`, and `scherzoctl attach` will show bounded, sanitized turn telemetry without making the daemon the owner of turn execution, prompts, transcripts, or raw pi JSON.

The observable behavior is deliberately small. A running session should show a current turn number and status. Attach/event replay should include explicit turn lifecycle lines such as turn started, turn finished, turn stopped, turn failed, or turn timed out. JSON output should expose the same summary fields with numeric timestamps and token totals or deltas. No central daemon state should contain full prompts, full transcripts, full tool payloads, unbounded turn records, or unredacted raw pi JSON.

## Problem Framing and Constraints

A Scherzo daemon coordinates issue work and exposes a local control API used by `gleam run -- ctl ...`. A worker runner owns pi execution in its workspace. Operators can currently inspect sessions and replay compact event lines, but they cannot reliably answer simple central questions such as "is turn 3 still running?", "did the last turn stop because I asked for stop-after-turn?", or "how many tokens were added by the last finished turn?" without reading runner-local details or interpreting low-level pi events.

The constraint that shapes this design is the telemetry boundary. The daemon may own bounded summaries and event metadata because those are operational facts. The daemon must not own turns themselves, pi transcripts, full tool inputs or outputs, prompts, raw JSON streams, or persistent per-turn records. Those remain runner-local because they can be sensitive, large, and tightly coupled to pi internals. The change must also preserve worker independence: failing to publish telemetry, a slow `scherzoctl attach`, or a busy event hub must not control or backpressure the turn loop beyond the existing asynchronous hub message path.

This plan does not implement the feature. It defines the behavior, boundaries, implementation path, tests, rollout, and recovery guidance for a later implementation.

## Strategy Overview


Add a Scherzo-owned turn telemetry schema above raw pi events. The schema should live in a small pure module, `src/scherzo/turn_telemetry.gleam`, so both the runner side and the session/control side can use the same compile-safe names without coupling the runner to session storage. This module must define distinct turn event-name constructors, status constructors, and whitelisted reason constructors. It must not reuse or extend `scherzo/agent/pi_event.gleam`; pi events remain producer-specific diagnostics, while turn lifecycle events are the Scherzo operator contract.

The runner update callback should evolve from carrying only `runner.PiUpdate` to carrying a small `runner.RunnerUpdate` union. Existing pi and diagnostic updates travel as `RunnerPiUpdate(PiUpdate)`. New sanitized lifecycle updates travel as `RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate)`. The event publisher then has an explicit bridge from runner updates into session event payloads. This avoids overloading `pi_event.PiEvent`, avoids duplicate Gleam constructor names, and gives the hub a reliable way to recognize turn lifecycle payloads.

The runner should emit normalized turn lifecycle updates at the points where it already knows Scherzo turn boundaries: just after a prompt is accepted for a turn, after token stats are read at turn completion, when operator stop-after-turn ends execution, and when a turn exits through timeout, abort, or failure cleanup. The event publisher should translate those updates into `EventKind.Turn` payloads. The session hub should sanitize those payloads, store bounded events, compute per-turn token deltas atomically from the previous summary totals, and maintain a compact `SessionSummary` turn snapshot. The control protocol and CLI renderers should decode and expose the snapshot and event lines.

This is proportionate because it reuses existing paths: the runner update callback, `orchestrator/event_publisher.gleam`, `session/hub.gleam`, `session/json.gleam`, and the control API already carry session updates from workers to the daemon. The plan does not introduce a new database, a daemon-owned turn table, an RPC from the daemon into pi, or a transcript store. The only new central data is a bounded summary and bounded event metadata.

The explicit lifecycle events are needed even though existing pi events include names such as `TurnStart`, `TurnEnd`, and `TurnFinished`. Pi-originated events are implementation details of the pi stream and may include raw JSON diagnostics. `pi_event.TurnFinished` currently represents token stats after `pi_rpc.get_session_stats`, not a complete lifecycle model. Scherzo-owned lifecycle events give a stable operator contract for started, finished, stopped, failed, and timed-out turns while allowing pi raw events to remain diagnostic and optional.

## Alternatives Considered

The simplest alternative is to leave the current `current_turn` and token total fields alone and teach operators to infer state from pi event replay. That is insufficient because event names and ordering are pi-specific, failure and stop paths do not produce a single normalized turn outcome, and `ps` cannot present a clear current status.

A second alternative is to centralize full per-turn records in the daemon, including raw pi JSON, prompts, tool payloads, transcripts, and result artifacts. That would answer many questions, but it is too large and too risky for this problem. It creates privacy and storage risk, couples daemon behavior to pi record shape, and would make the daemon a transcript owner. This plan rejects that approach unless a future, stronger requirement justifies a separate sensitive-data design.

A third alternative is to add only CLI formatting around the existing session event stream. That would improve human readability but would not make JSON control output, event replay semantics, or tests reliable. The chosen design adds a small schema first and then renders it.

## Risks and Countermeasures


The main privacy risk is accidentally centralizing prompt text, transcript text, raw pi JSON, or full tool payloads while adding turn visibility. The countermeasure is two layers of enforcement. First, turn lifecycle updates use the whitelisted `turn_telemetry.TurnReason` type and bounded numeric fields rather than arbitrary text. Second, `src/scherzo/session/hub.gleam` must sanitize any `EventKind.Turn` payload before storing it, and `src/scherzo/session/json.gleam` must also serialize a sanitized view for `EventKind.Turn` so direct JSON tests cannot leak generic `EventPayload` fields. For turn events, `message`, `pi_type`, `request_id`, `method`, all tool fields, and `raw_json` must be forced to `None` regardless of what a buggy publisher supplied. Tests must deliberately attempt to leak `SECRET_PROMPT`, transcript text, tool input/output, raw JSON-looking data, and an arbitrary reason string through turn payloads.

The main token correctness risk is computing a per-turn token delta after cumulative totals have already been overwritten. The countermeasure is explicit ownership: `event_publisher.worker_update` must not call `hub.update_tokens` before publishing a `RunnerTurnUpdate`. For turn lifecycle updates, the hub owns the atomic transition that reads previous summary totals, computes `token_delta`, updates cumulative `token_totals`, enriches the retained event, and updates the summary. The existing pre-publish `hub.update_tokens` behavior remains only for non-turn `RunnerPiUpdate` diagnostic events. A bridge-level test must publish through `event_publisher.worker_update` and a real hub actor so this ordering is covered.

The main operational risk is that observability could slow or control the turn loop. The countermeasure is to keep the runner emission path as a best-effort call that sends bounded messages to the existing hub actor and never waits for control clients. Do not add synchronous calls from the runner to `hub.list_sessions`, `hub.get_session`, `hub.events_after`, control protocol handlers, or `scherzoctl` rendering. If a telemetry event cannot be published, the worker must continue under the same failure behavior that exists today for other updates.

The main compatibility risk is changing the control JSON shape used by operators or scripts. The countermeasure is to make protocol fields additive and decoder defaults explicit. Existing fields such as `current_turn`, `status`, and `tokens` remain present with the same names and types. New summary and payload fields decode to `None` or zero-token defaults when talking to an older daemon. `kind = "turn"` and turn event names decode to the new turn representation; unknown future turn names must decode to a safe generic turn name rather than crashing or becoming raw pi JSON. Human output changes should be readable but not required for machine parsing; machine consumers should use `--json`.

The main correctness risk is reporting a turn as running forever after a timeout, stop, abort, or failure. The countermeasure is to add tests for every terminal path named in this plan and to ensure the hub has a single summary transition function that moves a turn from running to a terminal status. If implementation discovers an existing failure path without the necessary turn number, update this plan's Surprises & Discoveries and either thread the turn number through that path or mark the summary as failed with the last known current turn.

## Progress


- [x] (2026-05-02 00:00Z) Read the repo-local ExecPlan authoring guidance in `.pi/skills/exec-plan/SKILL.md`.
- [x] (2026-05-02 00:00Z) Verified the working copy was clean with `jj status --color=never` before drafting.
- [x] (2026-05-02 00:00Z) Checked that `docs/plans/LIV-30-daemon-visible-turn-telemetry.md` did not already exist.
- [x] (2026-05-02 00:00Z) Inspected the current runner, event publisher, session hub, session JSON, control protocol, and CLI usage surfaces needed to draft this plan.
- [x] (2026-05-02 00:00Z) Drafted this ExecPlan for review. No source code, tests, configuration, or existing documentation were changed.
- [x] (2026-05-02 00:00Z) Incorporated adversarial review findings for compile-safe turn representation, token-delta ordering, turn-payload privacy tests, explicit protocol decoding, and bridge-level event publisher/hub validation.
- [x] (2026-05-03 00:00Z) Created implementation ticket `LIV-53` because `LIV-30` tracked plan authoring only and is already complete.
- [x] (2026-05-04 09:20Z) Implemented the shared turn telemetry schema, added turn fields to session summaries and payloads, and added JSON privacy tests for sanitized turn payloads.
- [x] (2026-05-04 09:35Z) Implemented hub-owned turn summary transitions, token-delta computation, and turn-payload sanitization with hub tests for started, finished, failed, stopped, and timed-out turns.
- [x] (2026-05-04 09:45Z) Added `RunnerUpdate`, routed turn updates through `event_publisher.worker_update` without pre-updating tokens, and updated daemon/workflow callback types.
- [x] (2026-05-04 09:55Z) Emitted runner turn lifecycle updates for prompt-accepted starts, token-stats finishes, operator stop/abort, pi failures, pi timeouts, and state-refresh failures.
- [x] (2026-05-04 10:00Z) Added protocol decoding and `scherzoctl`/attach rendering for turn summary fields, turn event lines, compact raw turn fields, unknown future turn names, and non-whitelisted reason rejection.
- [x] (2026-05-04 09:59Z) Ran `direnv exec . gleam test`; 606 tests passed.
- [x] (2026-05-04 10:02Z) Ran `direnv exec . gleam format src test`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`; formatting passed and 606 tests passed.
- [x] (2026-05-04 10:10Z) Applied review feedback for runner turn lifecycle ordering: state-refresh failure now emits `turn_started` then `turn_failed` without an intermediate `turn_finished`, and abort skipped-record emission uses the active turn number.
- [x] (2026-05-04 10:15Z) Re-ran final post-review validation with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`; formatting passed and 607 tests passed.

## Surprises & Discoveries

- Observation: The session hub already stores bounded per-session event replay and has bounded session retention, so daemon-visible turn telemetry can reuse an existing bounded event mechanism rather than introduce storage.
  Evidence: `src/scherzo/session/hub.gleam` defines `default_max_events_per_session = 2000`, `default_max_sessions = 500`, `Publish`, `EventsAfter`, and `retain_latest`.

- Observation: `current_turn` already exists in `SessionSummary`, but it is only an integer updated from payload turns. It is not enough to distinguish running, stopped, failed, timed out, or finished turns.
  Evidence: `src/scherzo/session/event.gleam` defines `SessionSummary(... current_turn: Int, ... token_totals: domain.TokenTotals)`, and `src/scherzo/session/hub.gleam` updates `current_turn` from `payload.turn` in `update_summary_after_payload`.

- Observation: The runner already has Scherzo-owned control states for prompt queues, stop-after-turn, UI requests, timeouts, and token stats, so turn lifecycle telemetry can be emitted from the runner without asking the daemon to inspect pi internals.
  Evidence: `src/scherzo/agent/runner.gleam` defines `ActiveCommandState`, `ActiveTurn`, `loop_turns`, `active_turn_loop`, `finish_after_turn`, `handle_active_command`, `handle_operator_ui_timeout`, `stop_failure`, and `token_update`.

- Observation: Runner implementation is split across `src/scherzo/agent/types.gleam`, `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/agent/turn_loop.gleam`, and the re-exporting `src/scherzo/agent/runner.gleam`, so the concrete `RunnerUpdate` union belongs in `agent/types.gleam` and is re-exported by `agent/runner.gleam`.
  Evidence: `src/scherzo/agent/runner.gleam` already re-exported `PiUpdate`, `WorkerSuccess`, and `WorkerFailure` from `src/scherzo/agent/types.gleam`; placing `RunnerUpdate` beside `PiUpdate` avoided an import cycle while preserving the public runner API.

- Observation: Hub tests cannot drive `now_ms` with a normal `process.Subject` owned by the test process because the hub actor calls `now_ms` from the actor process, and Gleam panics if a different process receives from that subject.
  Evidence: An early bridge test using a subject-backed clock crashed with `Cannot receive with a subject owned by another process`; the final tests use constant hub clocks and pre-seeded `current_turn_started_at_ms` summaries when they need deterministic durations.

## Decision Log


- Decision: Add explicit Scherzo-owned turn lifecycle telemetry instead of relying only on existing pi events.
  Rationale: Operators need stable started/finished/stopped/failed/timed-out semantics that cover Scherzo control and failure paths. Existing pi events are diagnostic and producer-specific, and `pi_event.TurnFinished` currently means token stats were collected rather than a complete operator lifecycle.
  Date: 2026-05-02

- Decision: Put compile-safe turn event names, statuses, lifecycle updates, and whitelisted reasons in `src/scherzo/turn_telemetry.gleam`, and extend `runner` with a `RunnerUpdate` union instead of reusing `pi_event.PiEvent`.
  Rationale: Gleam constructors share a module namespace, so status constructors and event-name constructors must not duplicate names. Reusing `pi_event.PiEvent` would also conflate raw pi diagnostics with Scherzo's operator contract. A small shared turn telemetry module and an explicit runner update union keep the boundary clear without adding a service or broad abstraction.
  Date: 2026-05-02

- Decision: Keep daemon state to bounded summaries and bounded event metadata; do not centralize full turns, prompts, transcripts, raw pi JSON, or full tool payloads.
  Rationale: The operator problem is central visibility, not transcript storage. Sensitive or unbounded data would increase blast radius and couple the daemon to pi internals.
  Date: 2026-05-02

- Decision: Let `session/hub.gleam` own token-delta computation for `RunnerTurnUpdate` events, and require `event_publisher.worker_update` to skip pre-publish `hub.update_tokens` for those events.
  Rationale: The existing worker update path updates cumulative token totals before publishing diagnostic events. If that ordering is reused for turn-finished telemetry, the hub sees already-updated totals and computes a zero delta. Publishing turn lifecycle events directly to the hub lets one actor transition compute the delta and cumulative total atomically.
  Date: 2026-05-02

- Decision: Sanitize `EventKind.Turn` payloads in both the hub and JSON serializer, and represent terminal reasons with a whitelist instead of free-form strings.
  Rationale: `EventPayload` remains a generic shape that can hold messages, tool fields, and raw JSON for other event kinds. Defense in depth prevents a buggy or malicious turn payload from leaking sensitive values even if runner helper constructors are bypassed.
  Date: 2026-05-02

- Decision: Keep control protocol `version = 1` while making decoder compatibility explicit.
  Rationale: The change is additive. New clients should read older daemon responses by defaulting absent fields, and old clients should ignore new JSON fields. The decoder must still learn `kind = "turn"` and turn event names so typed CLI paths do not degrade turn events into raw pi diagnostics.
  Date: 2026-05-02

- Decision: Do not add artifact or result references in the first implementation.
  Rationale: The current operator need is turn lifecycle and token visibility. Result artifacts can contain sensitive model output and require a separate safe-reference policy. A future plan may add an explicit bounded artifact reference if there is a concrete operator workflow requiring it.
  Date: 2026-05-02

- Decision: Track implementation separately as `LIV-53`.
  Rationale: `LIV-30` was the plan-writing issue and is complete. Keeping a separate implementation issue prevents this checked-in, unimplemented plan from looking completed in the backlog.
  Date: 2026-05-03

- Decision: Define `RunnerUpdate` in `src/scherzo/agent/types.gleam` and re-export it from `src/scherzo/agent/runner.gleam` rather than defining the union directly in `runner.gleam`.
  Rationale: This repository stores shared runner data types in `agent/types.gleam`, while `runner.gleam` is a thin public facade over `agent/run_attempt.gleam`. Defining the union next to `PiUpdate` avoids an import cycle and keeps all callback signatures consistent.
  Date: 2026-05-04

- Decision: Do not create the intermediate commits named in this plan while running under Scherzo's `workflow:execplan-implementation` contract.
  Rationale: The workflow contract explicitly says not to create jj/git commits because the publish step creates the final logical jj commit after review and validation. The implementation still followed the same milestone boundaries in the working tree and tests.
  Date: 2026-05-04

## Outcomes & Retrospective

Implementation is tracked by `LIV-53`. The implementation now exposes bounded turn status, timing, reason, cumulative tokens, and token deltas through session summaries, event replay, control decoding, `scherzoctl ps`, `scherzoctl session`, `scherzoctl events`, and `scherzoctl attach`. Tests cover sanitized JSON serialization, hub sanitization, event-publisher token-delta ordering, runner helper and successful-run emission, state-refresh failure telemetry ordering, protocol compatibility, non-whitelisted reason rejection, raw compact turn lines, and pretty turn rendering. No lifecycle statuses from the plan were deferred. Final post-review validation passed on 2026-05-04 with `direnv exec . gleam format --check src test` and `direnv exec . gleam test` reporting 607 passing tests.

## Context and Orientation


Scherzo is written in Gleam. The daemon coordinates issue work and exposes a local control API. The command-line entry point is `gleam run -- ctl ...`, documented by `src/scherzo/main.gleam` and implemented in `src/scherzo/ctl.gleam`.

The runner owns pi execution. In `src/scherzo/agent/runner.gleam`, `run_attempt` and `run_attempt_with_commands` currently accept an `emit_update: fn(String, PiUpdate) -> Nil` callback. `PiUpdate` currently carries a pi event name, optional message, optional redacted raw JSON, optional turn number, optional request and method, optional pi session id, token totals, and bounded tool text fields. This plan changes that callback to `fn(String, RunnerUpdate) -> Nil`, where `RunnerPiUpdate(PiUpdate)` preserves existing diagnostic behavior and `RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate)` carries sanitized Scherzo-owned turn lifecycle updates. The runner loops through turns in `loop_turns`, reads pi records in `active_turn_loop`, collects token stats in `finish_after_turn`, handles operator prompts and stop-after-turn in `handle_between_turn_commands` and `handle_active_command`, handles operator UI requests and timeouts in `handle_blocking_ui_policy`, `handle_ui_response_command`, and `handle_operator_ui_timeout`, and cleans up failures through `cleanup_failure`, `fail_pi`, `stop_failure`, and `handle_abort_command`.

The new pure turn schema should live in `src/scherzo/turn_telemetry.gleam`. It is not a store and it does not talk to pi. It only names the turn lifecycle event, status, terminal reason, and bounded token/timing fields that are safe to carry to the daemon.

The event publisher translates runner updates into session events. In `src/scherzo/orchestrator/event_publisher.gleam`, `worker_update` currently updates session status, pi session id, tokens, and then calls `hub.publish` with `update_payload(update)`. `kind_for_update` maps pi events to event kinds such as `Lifecycle`, `AssistantMessage`, `Tool`, `UiRequest`, `UiResponse`, `TokenStats`, and `PiRaw`. This plan keeps that behavior for `RunnerPiUpdate` and adds a separate branch for `RunnerTurnUpdate` that publishes `EventKind.Turn` without pre-updating cumulative tokens.

The session event hub is the daemon's bounded in-memory event store. In `src/scherzo/session/hub.gleam`, `RegisterSession`, `Publish`, `ListSessions`, `GetSession`, and `EventsAfter` are actor messages. The hub stores `SessionSummary` values, stores retained `SessionEvent` values per session, increments event cursors, and updates `current_turn`, `token_totals`, and `last_event_at_ms` from payloads. It keeps at most `default_max_events_per_session` events per session and at most `default_max_sessions` session summaries. This plan adds one hub-owned transition for turn payloads that sanitizes the payload, computes token deltas, updates turn summary fields, and stores the enriched event.

The session schema lives in `src/scherzo/session/event.gleam` and `src/scherzo/session/json.gleam`. `SessionSummary` currently exposes issue identity, workspace path, pi session id, status, `current_turn`, timestamps, and token totals. `EventPayload` currently exposes event kind, name, turn, pi type, message, request and method, tool fields, token totals, and optional redacted raw JSON.

The control API schema is in `src/scherzo/control/protocol.gleam`. It defines requests such as `ListSessions`, `GetSession`, `GetEvents`, `StreamEvents`, `AbortSession`, `StopAfterCurrentTurn`, `PromptSession`, and `RespondUi`. Its response decoders build typed `SessionSummary`, `SessionEvent`, and `EventPayload` records, so the decoder must explicitly understand new turn fields and `kind = "turn"`. The CLI in `src/scherzo/ctl.gleam` uses those requests to implement `ps`, `session`, `events`, `attach`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`. Pretty or compact rendering is reached from `run_events`, `run_attach`, `client.compact_event_line`, and the pretty rendering helpers used by `ctl`.

A "turn" in this plan means one Scherzo runner cycle that sends a prompt to a pi session, consumes pi records until pi reports the agent turn ended, collects token stats, refreshes issue state, and then either starts another turn or exits. A "daemon-visible turn telemetry event" means a sanitized metadata event stored in the session hub. It is not a transcript and does not grant the daemon control over pi.

## Preconditions and Verified Facts


This plan assumes the repository is used from its root directory. If `direnv exec . <command>` fails because `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the command through `direnv exec .`.

The working copy was clean before this plan was drafted:

    jj status --color=never
    The working copy has no changes.

The following files and facts were checked in the current tree before drafting and review incorporation:

`src/scherzo/agent/runner.gleam` defines `pub type PiUpdate` with fields `event`, `message`, `raw_json`, `turn`, `request_id`, `method`, `pi_session_id`, `tokens`, `tool_name`, `tool_input`, `tool_output`, and `tool_status`. It emits updates through an injected `emit_update` callback. It does not yet define `RunnerUpdate`; adding that union is part of this plan.

`src/scherzo/agent/pi_event.gleam` is the pi event namespace used by the runner and event publisher. Existing code already treats `pi_event.TurnStart`, `pi_event.TurnEnd`, and `pi_event.TurnFinished` as pi-originated diagnostics or token stats. Do not add Scherzo lifecycle constructors to this module and do not interpret `pi_event.TurnFinished` as the new operator lifecycle event.

`src/scherzo/orchestrator/event_publisher.gleam` defines `worker_update`, `lifecycle`, `update_payload`, `kind_for_update`, `status_for_update`, and `tokens_are_nonzero`. Today `worker_update` calls `hub.update_tokens` before `hub.publish` when an update has non-zero tokens; the turn lifecycle branch added by this plan must not use that ordering.

`src/scherzo/session/event.gleam` defines `SessionStatus`, `EventKind`, `LifecycleEventName`, `EventName`, `EventPayload`, `SessionSummary`, `SessionEvent`, and `EventPage`. `EventName` currently has only `LifecycleName` and `PiName`; this plan adds `TurnName` using the separate turn telemetry name type.

`src/scherzo/session/hub.gleam` defines bounded session and event retention, updates summaries after `Publish`, and supports list, get, and event replay requests.

`src/scherzo/session/json.gleam` serializes session summaries and events to JSON and currently includes `current_turn` and token totals.

`src/scherzo/control/protocol.gleam` defines the local control protocol request and response types used by `scherzoctl`. Its decoders currently construct `SessionSummary` and `EventPayload` without turn fields and decode unknown event kinds to `PiRaw`; this plan changes that behavior for `kind = "turn"`.

`src/scherzo/ctl.gleam` lists operator commands and routes `ps`, `session`, `events`, and `attach` through the control client, replay, compact, and pretty rendering paths.

## Scope Boundaries


In scope: add normalized turn lifecycle statuses, compile-safe turn event names, bounded turn timestamps and durations, token total and token delta visibility, session summary JSON fields, event replay JSON fields, human `ps` and `session` turn display, and human `events` and `attach` lines for turn lifecycle. Add tests that prove the operator-visible behavior, token-delta ordering, compatibility decoding, and privacy boundary.

Out of scope: storing full pi records in the daemon; storing prompts, full transcripts, or tool payloads centrally; making the daemon responsible for turn execution; adding durable persistence for telemetry; changing pi RPC protocol semantics; adding artifact or result references; changing Linear issue state transitions; changing workspace creation or hooks; changing YAML workflow scheduling beyond ensuring agent steps still produce telemetry through the existing runner path; adding Scherzo turn lifecycle constructors to `scherzo/agent/pi_event.gleam`; or using duplicate constructor names in `session/event.gleam`.

Existing diagnostic pi events that come directly from pi should remain available under their current bounded/redacted behavior. The new turn lifecycle events should not remove or rename raw pi events. The old synthetic token stats event currently produced from session stats may be replaced by the new turn-finished lifecycle event if the new event carries the same cumulative token totals; if it is retained, it must be emitted after the turn event and covered by an ordering test. Existing `current_turn` should remain, but it should become part of a richer summary rather than the only turn field.

## Milestones


Milestone 1 defines the compile-safe schema and serialization boundary. At the end, `src/scherzo/turn_telemetry.gleam`, `src/scherzo/session/event.gleam`, and `src/scherzo/session/json.gleam` can represent turn statuses, turn event names, whitelisted reasons, bounded timing fields, and token deltas without reusing `pi_event.PiEvent` or duplicating constructors. JSON output includes additive fields and strips incompatible fields from `EventKind.Turn` payloads. This milestone comes first because it makes the privacy and compatibility contract explicit before runner behavior changes.

Milestone 2 teaches the hub to sanitize turn payloads and maintain turn summaries from normalized events. At the end, synthetic turn payloads can publish turn started and terminal events, and `ListSessions`, `GetSession`, and `EventsAfter` return coherent turn state. The hub computes token deltas from previous summary totals before cumulative totals are overwritten. This de-risks daemon-side state transitions before touching the runner loop.

Milestone 3 adds the runner/event-publisher bridge. At the end, `runner.RunnerUpdate` cleanly separates existing pi updates from turn lifecycle updates, `event_publisher.worker_update` publishes `RunnerTurnUpdate` values without pre-updating tokens, and a real hub actor test proves started, finished, and terminal turn events flow through the bridge with the expected token delta and sanitization.

Milestone 4 emits lifecycle telemetry from the real runner paths. At the end, real turn execution emits started, finished, stopped, failed, and timed-out statuses through the same worker update callback used by existing pi updates. This milestone is sequenced after schema, hub, and bridge tests so failures show up as clear contract violations.

Milestone 5 renders the telemetry for operators and validates protocol compatibility. At the end, `scherzoctl ps`, `scherzoctl session`, `scherzoctl events`, and `scherzoctl attach` show useful turn information in human modes, JSON modes expose additive fields for scripts, and new clients can decode older daemon responses with missing turn fields.

Milestone 6 validates rollout, compatibility, and privacy. At the end, all targeted tests and full validation commands pass, and manual checks show that telemetry is bounded and does not include sensitive transcript data.

## Plan of Work


Create `src/scherzo/turn_telemetry.gleam` as a tiny pure module. It should define `TurnStatus` with constructors `StatusRunning`, `StatusFinished`, `StatusFailed`, `StatusStopped`, and `StatusTimedOut`; `TurnEventName` with constructors `EventStarted`, `EventFinished`, `EventFailed`, `EventStopped`, `EventTimedOut`, and `EventUnknown(String)`; `TurnReason` with constructors `ReasonOperatorStopAfterCurrentTurn`, `ReasonOperatorAbort`, `ReasonPiStallTimeout`, `ReasonPiTurnTimeout`, `ReasonPiError`, and `ReasonStateRefreshFailed`; and `TurnLifecycleUpdate(name: TurnEventName, turn: Int, tokens: domain.TokenTotals, reason: Option(TurnReason))`. Add `status_to_string`, `status_from_string`, `event_name_to_string`, `event_name_from_string`, `reason_to_string`, `reason_from_string`, and `status_for_event_name`. JSON strings are `running`, `finished`, `failed`, `stopped`, `timed_out`; `turn_started`, `turn_finished`, `turn_failed`, `turn_stopped`, `turn_timed_out`; and `operator_stop_after_current_turn`, `operator_abort`, `pi_stall_timeout`, `pi_turn_timeout`, `pi_error`, and `state_refresh_failed`. `reason_from_string` must return `None` for any non-whitelisted string.

In `src/scherzo/session/event.gleam`, import `scherzo/turn_telemetry`. Extend `EventKind` with a `Turn` variant serialized as `turn`. Extend `EventName` with `TurnName(turn_telemetry.TurnEventName)`. Do not add turn lifecycle names to `LifecycleEventName`, and do not add them to `pi_event.PiEvent`. Update `name_to_string` so `TurnName(name)` uses `turn_telemetry.event_name_to_string`. Keep existing lifecycle and pi names unchanged.

Extend `EventPayload` in `src/scherzo/session/event.gleam` with `turn_status: Option(turn_telemetry.TurnStatus)`, `turn_started_at_ms: Option(Int)`, `turn_finished_at_ms: Option(Int)`, `turn_duration_ms: Option(Int)`, `token_delta: domain.TokenTotals`, and `reason: Option(turn_telemetry.TurnReason)`. Keep the existing `turn: Option(Int)` field for compatibility. Add a helper `empty_payload` default for all new fields: status and timestamps `None`, `token_delta = domain.zero_token_totals()`, and `reason = None`.

Extend `SessionSummary` in `src/scherzo/session/event.gleam` with `current_turn_status: Option(turn_telemetry.TurnStatus)`, `current_turn_started_at_ms: Option(Int)`, `last_turn_finished_at_ms: Option(Int)`, `last_turn_duration_ms: Option(Int)`, `last_turn_token_delta: domain.TokenTotals`, and `last_turn_reason: Option(turn_telemetry.TurnReason)`. The existing `current_turn` field remains an `Int` for compatibility. A newly registered session should have `current_turn = 0`, `current_turn_status = None`, timestamp fields `None`, `last_turn_token_delta = domain.zero_token_totals()`, and `last_turn_reason = None`.

Update `src/scherzo/session/json.gleam` so `summary_to_json` emits the new summary fields and `payload_entries` emits the new payload fields. Add helper functions for optional turn status, optional turn reason, optional int, optional string reuse, and `token_delta` serialization using the same `tokens_to_json` shape as token totals. For `EventKind.Turn`, serialize a sanitized view: `pi_type`, `message`, `request_id`, `method`, `tool_name`, `tool_input`, `tool_output`, `tool_status`, and `raw_json` must be `null` even if the in-memory payload was constructed with values. Turn telemetry JSON should expose only `kind`, `name`, `turn`, turn status, timing fields, cumulative `tokens`, `token_delta`, and whitelisted `reason`.

Update `src/scherzo/session/hub.gleam` so `publish_payload` routes `EventKind.Turn` payloads through a single `apply_turn_payload` helper before storing them. That helper must sanitize incompatible generic fields, fill hub timestamps, compute duration from `current_turn_started_at_ms` when possible, compute `token_delta` as the clamped difference between incoming cumulative totals and the previous summary `token_totals`, update the summary, and store the enriched event. On `EventStarted`, set `current_turn` to the event turn, `current_turn_status` to `Some(StatusRunning)`, `current_turn_started_at_ms` to the event timestamp, clear `last_turn_reason`, and store an event payload with `turn_started_at_ms` set. On `EventFinished`, compute `token_delta`, set `current_turn_status` to `Some(StatusFinished)`, set `last_turn_finished_at_ms` and `last_turn_duration_ms`, set `last_turn_token_delta`, and update cumulative `token_totals`. On `EventStopped`, `EventFailed`, and `EventTimedOut`, set the terminal status and whitelisted reason. If a terminal event arrives without a known start timestamp, leave duration `None` rather than using an incorrect value. If a payload has `TurnName(EventUnknown(_))`, store it as a sanitized turn event without changing summary status.

Add a small token difference helper in `src/scherzo/session/hub.gleam` or `src/scherzo/domain.gleam`. It should subtract each token field and clamp negative results to zero so out-of-order or reset stats do not produce negative JSON. Keep this helper pure and covered by tests.

Update `src/scherzo/agent/runner.gleam` to define `pub type RunnerUpdate { RunnerPiUpdate(PiUpdate) RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate) }`. Change `run_attempt`, `run_attempt_with_commands`, and internal runner callbacks from `fn(String, PiUpdate) -> Nil` to `fn(String, RunnerUpdate) -> Nil`. Wrap every existing pi update emission as `RunnerPiUpdate(update)` so existing diagnostic behavior stays the same. Update callers, including orchestrator code and tests, to pass the widened callback.

Update `src/scherzo/orchestrator/event_publisher.gleam`. Change `worker_update` to accept `runner.RunnerUpdate` and branch on it. For `RunnerPiUpdate(update)`, preserve the existing status, pi session id, token update, and `hub.publish` behavior. For `RunnerTurnUpdate(update)`, update session status to `Running` for `EventStarted` and `EventFinished`, do not call `hub.update_tokens`, and publish a sanitized turn payload via `hub.publish`. Add `turn_update_payload(update: turn_telemetry.TurnLifecycleUpdate) -> session_event.EventPayload` that sets `kind = session_event.Turn`, `name = session_event.TurnName(update.name)`, `turn = Some(update.turn)`, `turn_status = turn_telemetry.status_for_event_name(update.name)`, `tokens = update.tokens`, `reason = update.reason`, no message, no raw JSON, and no tool fields. Terminal turn updates should not by themselves finish the session; final session status remains owned by the existing worker exit/finalization path. Preserve `UiRequest` and `UiResponse` status behavior for pi updates.

Update `src/scherzo/agent/runner.gleam` to emit Scherzo-owned turn lifecycle updates. Add helper functions `turn_started_update(turn: Int)`, `turn_finished_update(turn: Int, totals: domain.TokenTotals)`, `turn_stopped_update(turn: Int, reason: turn_telemetry.TurnReason, totals: domain.TokenTotals)`, `turn_failed_update(turn: Int, reason: turn_telemetry.TurnReason, totals: domain.TokenTotals)`, and `turn_timed_out_update(turn: Int, reason: turn_telemetry.TurnReason, totals: domain.TokenTotals)`. These functions should return `RunnerTurnUpdate` values with no message text, no raw JSON, no request or method, no tool fields, the appropriate turn number, token totals only where known, and a whitelisted reason.

In `loop_turns`, emit `turn_started_update(turn)` after `pi_rpc.send_prompt` succeeds and after any skipped records have been emitted, immediately before the active turn deadline is established. Do not emit `EventStarted` before `send_prompt` succeeds because the operator should not see a running turn that pi never accepted.

In `finish_after_turn`, emit `turn_finished_update(turn, totals)` after token stats are read. Prefer replacing the existing `token_update(pi_event.TurnFinished, turn, totals)` with the new turn lifecycle update. If implementation retains the legacy token stats diagnostic for compatibility, emit the turn lifecycle update first so the hub computes the delta before any diagnostic `hub.update_tokens` message can overwrite the cumulative total; add a test for that ordering if the legacy event remains.

In active failure paths, emit a terminal turn event before cleanup returns the worker failure. For `error.PiStallTimeout` while no operator UI is pending, emit `turn_timed_out_update(turn, ReasonPiStallTimeout, totals)`. For a turn deadline timeout surfaced by `pi_rpc.read_turn_record`, use `ReasonPiTurnTimeout` if the error type distinguishes it; otherwise use `ReasonPiError`. For other pi errors during an active turn, emit `turn_failed_update(turn, ReasonPiError, totals)`. For stop-after-turn, emit `turn_stopped_update(turn, ReasonOperatorStopAfterCurrentTurn, totals)` when the current turn has completed and the queued stop is about to terminate before the next turn. For abort during an active turn, emit `turn_stopped_update(turn, ReasonOperatorAbort, totals)` before `handle_abort_command` terminates the pi session. For issue state refresh failure after a completed turn, use `ReasonStateRefreshFailed` with `turn_failed_update` unless implementation confirms the turn already finished and the worker failure belongs outside the turn lifecycle; record any such discovery in this plan. Do not emit a terminal turn event for a queued prompt; queued prompts should continue to use the existing sanitized prompt queued/sent/dropped lifecycle events.

Thread the current turn number into any cleanup helper that needs it and does not currently receive it. Prefer adding an `Option(Int)` argument to failure cleanup helpers over storing mutable global state. If a failure occurs before a turn starts, do not emit a turn terminal event; the session status and existing lifecycle event are sufficient.

Update `src/scherzo/control/protocol.gleam` explicitly. Decode `kind = "turn"` to `event.Turn`. Decode turn names with `turn_telemetry.event_name_from_string`; unknown turn names become `event.TurnName(turn_telemetry.EventUnknown(name_string))` and must not be treated as `PiRaw` or `pi_event.UnknownPiEvent`. Decode new summary fields with safe defaults so older daemon responses work: absent `current_turn_status`, timestamp fields, and `last_turn_reason` become `None`; absent `last_turn_token_delta` becomes `domain.zero_token_totals()`. Decode new payload fields similarly: absent `turn_status`, timestamps, and `reason` become `None`; absent `token_delta` becomes zero totals. If `reason` is present but not whitelisted, decode it to `None`. Preserve `version = 1` because the change is backward-compatible.

Update operator rendering in `src/scherzo/ctl.gleam` and the control rendering modules it calls. `ps` should show at least session id, issue identifier/title as it does today, status, and a turn column like `turn 3 running` or `turn 3 finished 12.4s +1834 tok`. `session <session-id>` should show the same fields plus started/finished timestamps when present. `events` and `attach` pretty output should render one readable line per turn lifecycle event. Compact raw event lines should include `kind=turn`, `name=turn_started` or terminal name, `turn=<n>`, `turn_status=<status>`, and `duration_ms=<n>` when present. JSON modes should rely on the session/event JSON fields and should not invent a different shape.

Preserve operator command interactions. `stop-after-turn` remains a command to the runner, not to the daemon summary. A queued stop should appear as an operator command or lifecycle event immediately if that already exists, then the current turn should remain `running` until the turn terminal event is emitted. Queued prompts should not change turn status until the prompt is actually sent and the next turn starts. UI requests should continue to set `SessionStatus` to `WaitingUi` while the turn status remains `running`. YAML agent steps should require no special code: each agent step that uses `runner.run_attempt` receives telemetry through the same callback.

## Concrete Steps


1. From the repository root, verify the working tree is clean:

       jj status --color=never

   Expect either `The working copy has no changes.` or only the intentional plan file before implementation begins. Do not start implementation on top of unrelated changes.

2. If the environment is not already allowed, run:

       direnv allow .

   Then use `direnv exec .` for validation commands.

3. Add schema and privacy tests in `test/scherzo/session/json_test.gleam`. If the file does not exist, create it. Write `summary_json_includes_bounded_turn_fields` that constructs an `event.SessionSummary` with `current_turn = 2`, `current_turn_status = Some(turn_telemetry.StatusRunning)`, `current_turn_started_at_ms = Some(1000)`, `last_turn_finished_at_ms = None`, `last_turn_duration_ms = None`, `last_turn_token_delta = domain.zero_token_totals()`, and `last_turn_reason = None`. Assert that `session_json.summary_to_string` contains JSON fields `"current_turn":2`, `"current_turn_status":"running"`, `"current_turn_started_at_ms":1000`, and no `raw_json` field.

4. In the same test file, write `turn_event_json_strips_sensitive_generic_fields`. Construct an `event.EventPayload` with `kind = event.Turn`, `name = event.TurnName(turn_telemetry.EventStarted)`, `turn = Some(2)`, `message = Some("SECRET_PROMPT")`, `tool_input = Some("tool_input_value")`, `tool_output = Some("full transcript")`, `tool_status = Some("secret status")`, `raw_json = Some(event.RedactedRawJson(value: "{\"secret\":true}", truncated: False))`, and otherwise valid turn fields. Assert that `session_json.payload_to_string` includes `"kind":"turn"`, `"name":"turn_started"`, and `"turn":2`, and does not include `SECRET_PROMPT`, `full transcript`, `tool_input_value`, `secret status`, or `{"secret":true}`.

5. In the same test file, write `turn_reason_decoder_rejects_free_form_secret_reason` if a protocol test helper can decode payload JSON directly; otherwise put this test in `test/scherzo/control/protocol_test.gleam` in step 27. Decode a turn event JSON object with `"reason":"SECRET_PROMPT in reason"` and assert the decoded payload has `reason = None` and re-serialization does not include `SECRET_PROMPT`.

6. Run the targeted schema tests and expect them to fail to compile because the new types and fields do not exist yet:

       direnv exec . gleam test

   Record the failure in this plan's Progress section when implementing.

7. Create `src/scherzo/turn_telemetry.gleam` and implement the turn event, status, reason, and lifecycle update types and string conversion helpers exactly as described in Plan of Work.

8. Implement the schema additions in `src/scherzo/session/event.gleam` and JSON additions in `src/scherzo/session/json.gleam`. Update existing tests that construct `SessionSummary` or `EventPayload` by adding safe defaults for the new fields.

9. Re-run `direnv exec . gleam test` and expect the schema and privacy tests to pass or the next missing constructor updates to surface.

10. Add hub tests in `test/scherzo/session/hub_test.gleam`. If the file does not exist, create it. Write `turn_started_updates_session_summary` that starts a hub with a deterministic `now_ms` function returning `1000`, registers a summary, publishes a turn-start payload for turn 1, then calls `hub.get_session`. Assert that `current_turn = 1`, `current_turn_status = Some(turn_telemetry.StatusRunning)`, and `current_turn_started_at_ms = Some(1000)`.

11. In the same file, write `turn_finished_computes_duration_and_token_delta`. Use deterministic times `1000` for start and `2500` for finish. Publish turn started, then publish turn finished with token totals `{input: 10, output: 5, cache_read: 0, cache_write: 0, total: 15}`. Assert that the summary has `current_turn_status = Some(turn_telemetry.StatusFinished)`, `last_turn_duration_ms = Some(1500)`, `token_totals.total = 15`, and `last_turn_token_delta.total = 15`. Also call `hub.events_after` and assert the retained turn-finished event has `turn_duration_ms = Some(1500)` and `token_delta.total = 15`.

12. Add hub tests for terminal paths and sanitization: `turn_failed_sets_reason_without_raw_payload`, `turn_stopped_sets_operator_reason`, `turn_timed_out_sets_timeout_status`, and `turn_payload_sanitization_strips_message_tool_and_raw_json`. Each terminal test should publish a started event followed by the terminal event and assert the status, reason code, and absence of `raw_json`. The sanitization test should publish a malicious `EventKind.Turn` payload with message/tool/raw JSON fields populated and assert both `hub.events_after` and `session_json.event_to_string` omit the secret strings.

13. Implement hub summary enrichment, sanitization, duration calculation, and token-delta computation in `src/scherzo/session/hub.gleam`. Keep all event retention behavior unchanged.

14. Run `direnv exec . gleam test` and expect the hub tests to pass.

15. Commit the schema and hub work after tests pass. Suggested commit message: `Add bounded session turn telemetry schema`.

16. Add event publisher tests in `test/scherzo/orchestrator/event_publisher_test.gleam`. If the file does not exist, create it. Write tests that call the new turn payload helper with synthetic `turn_telemetry.TurnLifecycleUpdate` values for started, finished, failed, stopped, and timed out. Assert `kind = event.Turn`, the turn names match, `turn` is set, `raw_json = None`, no message/tool fields are set, and the whitelisted reason is present only for terminal events.

17. In the same file, add bridge-level tests that use a real hub actor and `event_publisher.worker_update`. Register a session summary with zero token totals. Send `runner.RunnerTurnUpdate` for `EventStarted` turn 1, then `EventFinished` turn 1 with cumulative totals total `15`. Assert `hub.get_session` reports `last_turn_token_delta.total = 15` and `token_totals.total = 15`, and `hub.events_after` reports a retained turn-finished event with `token_delta.total = 15`. Add a second bridge test for one terminal update, such as `EventStopped` with `ReasonOperatorStopAfterCurrentTurn`, and assert the summary does not stay `StatusRunning` and no secret fields are retained.

18. Introduce `runner.RunnerUpdate` in `src/scherzo/agent/runner.gleam`, update existing pi emissions to wrap `RunnerPiUpdate`, and update existing callback types and call sites to accept `RunnerUpdate`. Preserve all existing pi diagnostic behavior.

19. Implement the event publisher `RunnerUpdate` branch and turn payload helper in `src/scherzo/orchestrator/event_publisher.gleam`. Ensure `RunnerTurnUpdate` does not call `hub.update_tokens` before `hub.publish`.

20. Run `direnv exec . gleam test` and expect event publisher and bridge tests to pass.

21. Commit the bridge work. Suggested commit message: `Route runner turn telemetry through event publisher`.

22. Add runner emission tests in the existing runner test file if one exists under `test/scherzo/agent/`; otherwise create `test/scherzo/agent/runner_turn_telemetry_test.gleam`. Test helper functions directly where possible. At minimum, test that `turn_started_update(3)` returns `RunnerTurnUpdate` with `EventStarted`, turn `3`, zero tokens, no message, no raw JSON, and no tool fields after conversion through `event_publisher.turn_update_payload`; `turn_finished_update(3, totals)` carries token totals; and terminal helpers carry only whitelisted reasons.

23. Add a pure terminal-decision test if the existing runner harness cannot fake pi RPC. The test should cover stop-after-turn, abort, timeout, and generic pi failure decisions by calling the smallest pure helper available and asserting the returned update uses `ReasonOperatorStopAfterCurrentTurn`, `ReasonOperatorAbort`, `ReasonPiStallTimeout` or `ReasonPiTurnTimeout`, and `ReasonPiError` respectively. If a full fake pi harness already exists, add an integration-style runner test that simulates one successful turn and asserts the collected `emit_update` list contains `EventStarted` before `EventFinished` for the same turn. Record in Surprises & Discoveries which seam was used.

24. Implement runner helper functions and emission points in `src/scherzo/agent/runner.gleam`.

25. Run `direnv exec . gleam test` and expect all runner, event publisher, hub, and JSON tests to pass.

26. Commit runner emission work. Suggested commit message: `Emit sanitized turn lifecycle updates from runner`.

27. Add protocol compatibility tests in `test/scherzo/control/protocol_test.gleam`. Add a new-daemon response decode test for `get_events` or `stream_events` containing `"kind":"turn"`, `"name":"turn_finished"`, `"turn_status":"finished"`, `"token_delta":{"total":15}`, and a whitelisted reason where appropriate. Assert the decoded event has `event.Turn`, `event.TurnName(turn_telemetry.EventFinished)`, the expected status, and token delta. Add old-daemon decode tests for `list_sessions`, `get_session`, and `get_events` responses that omit all new turn fields. Assert absent summary fields decode to `None` or zero-token defaults, absent payload `token_delta` decodes to zero totals, and protocol `version` remains `1`. Add an unknown future turn-name decode test and assert it becomes `TurnName(EventUnknown(_))`, not `PiRaw`.

28. Add CLI rendering tests. In the existing `ctl` test file, or in `test/scherzo/ctl_turn_telemetry_test.gleam` if no suitable file exists, add tests for `ps`, `session`, `events --pretty --no-follow`, and `attach --raw --no-follow` using a fake `ControlClient`. Assert human output contains `turn 3 running` for a running summary and `turn 3 finished` with duration/token delta for a finished event. Assert compact/raw output includes `kind=turn`, `name=turn_finished`, `turn=3`, and `turn_status=finished`.

29. Implement protocol decoder changes in `src/scherzo/control/protocol.gleam` and rendering changes in `src/scherzo/ctl.gleam` and the control rendering modules it calls.

30. Run `direnv exec . gleam test` and expect protocol and CLI tests to pass.

31. Commit control rendering work. Suggested commit message: `Show turn telemetry in scherzoctl`.

32. Run formatting and full validation from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expect formatting to report success and all tests to pass. If `gleam format --check` is unsupported in the pinned toolchain, run `direnv exec . gleam format src test` and inspect the diff before committing.

33. Perform a manual smoke check if a local daemon fixture is available. Start the daemon according to the repository's normal development workflow, run a short agent session, then run:

       direnv exec . gleam run -- ctl ps
       direnv exec . gleam run -- ctl session <session-id> --json
       direnv exec . gleam run -- ctl events --pretty <session-id>
       direnv exec . gleam run -- ctl attach --raw --no-follow <session-id>

   Expect to see turn status in `ps`, additive JSON fields in `session --json`, turn lifecycle lines in `events --pretty`, and compact `kind=turn` lines in raw attach output. Do not include real secrets or production prompts in the manual run.

34. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections before finishing the implementation.

## Testing and Falsifiability


The plan is falsified if operators still cannot see a current turn status from `scherzoctl ps`, if event replay does not show normalized turn lifecycle events, if terminal paths leave a turn stuck as running, if per-turn token deltas are zero because cumulative totals were updated first, if new JSON fields contain raw pi JSON or prompt/transcript/tool payload text, if new clients cannot decode old daemon responses with missing turn fields, or if the runner waits on control clients to publish telemetry.

Required schema and JSON tests live in `test/scherzo/session/json_test.gleam`. They must verify summary fields, turn event JSON fields, safe string conversions, and absence of sensitive values. The privacy test must be adversarial: it must populate `message`, `tool_input`, `tool_output`, `tool_status`, and `raw_json` on an `EventKind.Turn` payload and assert serialization strips those values. Before schema implementation these tests should fail to compile because fields such as `turn_telemetry.TurnStatus`, `event.Turn`, and `current_turn_status` do not exist. After implementation they should pass.

Required hub tests live in `test/scherzo/session/hub_test.gleam`. They must use deterministic `now_ms` values to prove start timestamps, finish timestamps, duration, token delta, terminal reasons, event replay retention, summary updates, and hub-level sanitization of malicious turn payloads. The exact assertions are listed in Concrete Steps.

Required event publisher tests live in `test/scherzo/orchestrator/event_publisher_test.gleam`. They must prove synthetic runner turn updates become `EventKind.Turn` payloads and never include `raw_json`, message text, or tool fields. They must also include bridge-level tests through `event_publisher.worker_update` and a real hub actor for started, finished, and at least one terminal turn event. The finished bridge test must prove `SessionSummary.last_turn_token_delta` and the retained event `token_delta` are non-zero when cumulative totals increase.

Required runner tests live in the existing runner test module under `test/scherzo/agent/` or in a new `test/scherzo/agent/runner_turn_telemetry_test.gleam`. Helper-level tests must prove each turn update constructor is sanitized and uses whitelisted reasons. The runner emission ordering must be tested either with an existing fake pi RPC harness or with a small pure terminal-decision seam that covers stop-after-turn, abort, timeout, and pi failure.

Required control tests live in `test/scherzo/control/protocol_test.gleam` and the existing `ctl` test module, or in `test/scherzo/ctl_turn_telemetry_test.gleam` if no suitable file exists. Protocol tests must cover new responses with `kind = "turn"`, old responses missing every turn field, missing `token_delta`, unknown future turn names, and non-whitelisted reason strings. CLI tests must use fake control responses so no real daemon is required. They must assert human `ps`, `session`, `events`, and `attach` output includes turn visibility and that JSON output remains additive.

Run the full validation commands from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expected success is a zero exit code from both commands. Any new test that passes before implementation is suspect unless it is a pure compatibility test; the main telemetry tests should fail before the relevant code exists and pass after.

Privacy falsification is explicit: add sample sensitive strings to tests, including `SECRET_PROMPT`, `full transcript`, `tool_input_value`, `secret status`, and a raw JSON-looking string. If any turn summary or turn lifecycle JSON contains those strings, the implementation fails this plan. If `reason_from_string` accepts a non-whitelisted prompt-like reason, the implementation also fails this plan.

Backpressure falsification is also explicit: inspect the final code and tests to ensure no runner turn telemetry path calls `hub.list_sessions`, `hub.get_session`, `hub.events_after`, control protocol request handling, `ctl` rendering, file IO for telemetry storage, or network IO. The runner may call only its existing `emit_update` callback with bounded `RunnerUpdate` values.

## Validation and Acceptance


Acceptance is met when a reviewer can observe these behaviors from tests and, where possible, a manual smoke run:

A newly started turn appears in session summary JSON with `current_turn` set to the turn number, `current_turn_status` set to `running`, `current_turn_started_at_ms` set to a timestamp, and no raw pi or transcript fields.

A finished turn appears in event replay as a turn lifecycle event with `kind = turn`, `name = turn_finished`, `turn_status = finished`, `turn_duration_ms` set when the start timestamp is known, and `token_delta` showing the tokens added by that turn. Session summary token totals remain cumulative. A bridge-level test through `event_publisher.worker_update` and the real hub must prove this delta remains non-zero when incoming cumulative totals increase.

A stopped, failed, or timed-out turn appears with the appropriate terminal status and a whitelisted reason code. Stop-after-turn should not mark the turn stopped before the current turn actually reaches its terminal point. UI waiting should leave the turn status as running while session status may be `waiting_ui`.

`scherzoctl ps` shows enough turn information for an operator to identify active work without attaching. `scherzoctl session <session-id> --json` exposes additive turn fields. `scherzoctl events --pretty <session-id>` and `scherzoctl attach <session-id>` render turn lifecycle lines. `scherzoctl attach --raw --no-follow <session-id>` prints compact event lines containing `kind=turn` and `turn=<n>` for retained turn events.

A new typed client can decode older `list_sessions`, `get_session`, and `get_events` responses that omit every new turn field. A typed client can decode newer responses with `kind = "turn"` and known turn names. Unknown future turn names decode to a safe `EventUnknown` turn name, not to `PiRaw` and not to raw pi JSON.

No central summary or turn event stores full prompts, full transcripts, full tool input/output, full raw pi JSON, arbitrary free-form reason text, or unbounded turn records. Existing bounded/redacted diagnostic events may continue to exist, but they are not the turn telemetry contract.

All validation commands pass:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

## Rollout, Recovery, and Idempotence

Roll out as an additive protocol and CLI change. Keep existing fields and existing pi diagnostic events. Do not bump `src/scherzo/control/protocol.gleam` `version` unless implementation discovers a non-additive change; this plan expects no version bump.

The safest recovery path is revert-by-commit. Because the change is additive and in-memory, there is no database migration or durable data cleanup. If human CLI formatting causes confusion, keep the JSON fields and temporarily reduce human rendering to a simpler `turn <n> <status>` display. If the new runner events cause unexpected noise, disable only the new turn event emission while leaving existing pi events and session summaries in place.

Event replay remains bounded by the existing hub limits. If a session emits many events, older turn lifecycle events may be dropped just like other events; the current summary should still show the latest known turn state. Re-running tests and formatting commands is idempotent. Re-running manual smoke checks creates new sessions/events only in the local development daemon.

If implementation stops halfway, the system should remain safe at each commit point. Milestone 1 only changes schema and tests. Milestone 2 changes hub handling for synthetic events. Milestone 3 emits runner events. Milestone 4 renders fields. Each commit must pass `direnv exec . gleam test` before moving on.

## Artifacts and Notes

Source-control baseline before drafting:

    jj status --color=never
    The working copy has no changes.
    Working copy  (@) : nwomytvw 6cd18d25 (empty) (no description set)
    Parent commit (@-): xoqmotsl 294468ca (no description set)

Relevant current CLI usage excerpt from `src/scherzo/ctl.gleam` includes these operator surfaces:

    ctl ps [--json]
    ctl session <session-id> [--json]
    ctl events <session-id> [--json]
    ctl attach --raw <session-id>
    ctl stop-after-turn <session-id> --yes
    ctl prompt <session-id> <text>
    ctl ui respond <session-id> <request-id> (--cancel | --value <text>)

Relevant current session JSON fields include:

    session_id, issue_id, issue_identifier, issue_title, workspace_path,
    pi_session_id, status, exit_reason, current_turn, started_at_ms,
    last_event_at_ms, tokens

Expected compact event line shape after implementation should be similar to this, with exact ordering left to the existing compact renderer:

    cursor=42 kind=turn name=turn_finished session=<session-id> turn=3 turn_status=finished duration_ms=12400 token_delta_total=1834

The example above is illustrative; tests should assert fields or substrings rather than brittle full-line formatting unless the existing compact renderer already has a stable order contract.

## Interfaces and Dependencies


No new package dependencies are required. Add one small project module, `src/scherzo/turn_telemetry.gleam`, and use existing Gleam standard library modules already present in these files, plus existing project helpers for JSON, strings, options, and token totals.

In `src/scherzo/turn_telemetry.gleam`, define:

    pub type TurnStatus {
      StatusRunning
      StatusFinished
      StatusFailed
      StatusStopped
      StatusTimedOut
    }

    pub type TurnEventName {
      EventStarted
      EventFinished
      EventFailed
      EventStopped
      EventTimedOut
      EventUnknown(String)
    }

    pub type TurnReason {
      ReasonOperatorStopAfterCurrentTurn
      ReasonOperatorAbort
      ReasonPiStallTimeout
      ReasonPiTurnTimeout
      ReasonPiError
      ReasonStateRefreshFailed
    }

    pub type TurnLifecycleUpdate {
      TurnLifecycleUpdate(
        name: TurnEventName,
        turn: Int,
        tokens: domain.TokenTotals,
        reason: Option(TurnReason),
      )
    }

    pub fn status_to_string(status: TurnStatus) -> String
    pub fn status_from_string(value: String) -> Option(TurnStatus)
    pub fn event_name_to_string(name: TurnEventName) -> String
    pub fn event_name_from_string(value: String) -> Option(TurnEventName)
    pub fn reason_to_string(reason: TurnReason) -> String
    pub fn reason_from_string(value: String) -> Option(TurnReason)
    pub fn status_for_event_name(name: TurnEventName) -> Option(TurnStatus)

`event_name_from_string` should return `None` for unknown strings so protocol decoding can explicitly create `EventUnknown(name_string)`. `reason_from_string` should return `None` for every non-whitelisted string.

In `src/scherzo/session/event.gleam`, extend existing types rather than replacing them:

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
      Turn
    }

    pub type EventName {
      LifecycleName(LifecycleEventName)
      PiName(pi_event.PiEvent)
      TurnName(turn_telemetry.TurnEventName)
    }

Extend `SessionSummary` with:

    current_turn_status: Option(turn_telemetry.TurnStatus)
    current_turn_started_at_ms: Option(Int)
    last_turn_finished_at_ms: Option(Int)
    last_turn_duration_ms: Option(Int)
    last_turn_token_delta: domain.TokenTotals
    last_turn_reason: Option(turn_telemetry.TurnReason)

Extend `EventPayload` with:

    turn_status: Option(turn_telemetry.TurnStatus)
    turn_started_at_ms: Option(Int)
    turn_finished_at_ms: Option(Int)
    turn_duration_ms: Option(Int)
    token_delta: domain.TokenTotals
    reason: Option(turn_telemetry.TurnReason)

In `src/scherzo/agent/runner.gleam`, keep `PiUpdate` for pi-originated diagnostics and add:

    pub type RunnerUpdate {
      RunnerPiUpdate(PiUpdate)
      RunnerTurnUpdate(turn_telemetry.TurnLifecycleUpdate)
    }

The runner update constructors must ensure turn lifecycle updates have no generic payload fields. After conversion in `event_publisher.turn_update_payload`, turn events must have:

    pi_type: None
    message: None
    request_id: None
    method: None
    tool_name: None
    tool_input: None
    tool_output: None
    tool_status: None
    raw_json: None

In `src/scherzo/orchestrator/event_publisher.gleam`, keep `worker_update` as the single worker-to-hub bridge, but change its update argument to `runner.RunnerUpdate`. `RunnerPiUpdate` uses the existing behavior. `RunnerTurnUpdate` publishes a turn payload and does not call `hub.update_tokens` before publishing. Do not let control protocol code call runner internals. Do not let runner code call session hub query functions.

In `src/scherzo/session/hub.gleam`, add a pure clamped token-delta helper and a single turn transition helper used by `publish_payload` for `EventKind.Turn`. This helper owns turn payload sanitization, timestamp enrichment, summary status transitions, duration calculation, token delta calculation, and cumulative token total update.

In `src/scherzo/control/protocol.gleam`, keep `pub const version = 1` and treat all new fields as additive. New clients must tolerate absent turn fields when talking to an older daemon, and old clients must be able to ignore new fields. Decode `kind = "turn"` to `event.Turn`; decode known turn names to `TurnName(EventStarted)` and related constructors; decode unknown future turn names to `TurnName(EventUnknown(name_string))`; decode missing summary/payload fields to `None` or zero-token defaults; and decode non-whitelisted reasons to `None`.

## Open Questions and Clarifications Needed

None.
