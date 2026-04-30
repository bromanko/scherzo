# Add authenticated mutating operator controls

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo has one transport-independent operator command model and an authenticated local implementation of that model through the control API and `scherzoctl`. A local operator can safely pause and resume dispatch, reload the workflow, abort or stop a specific worker, queue an operator follow-up prompt for a live session, respond to supported pi extension UI requests when the workflow opts into operator handling, retry an issue now, and park or unpark an issue. The visible proof is that `scherzoctl pause`, `scherzoctl resume`, `scherzoctl abort <session> --yes`, `scherzoctl prompt <session> "..."`, and `scherzoctl ui respond <session> <request-id> --cancel` work against a fake daemon/worker in deterministic tests and produce auditable session events or structured daemon audit logs as appropriate.

This phase introduces the canonical command API that future transports must reuse. `scherzoctl` over the local control API is the first client. A later Linear command transport will parse Linear comments and submit the same command values to the same daemon handlers; it must not mutate scheduler state through a separate Linear-specific path.

This plan introduces destructive and state-changing operations, so it is intentionally sequenced after read-only visibility and pretty attach are already working.

## Problem Framing and Constraints

Read-only attach helps an operator understand what a worker is doing, but it does not let them intervene. Real operation needs a small set of control actions: pause new dispatch during an incident, stop a runaway worker, ask a worker to continue with extra context, answer a pi UI dialog, retry a fixed issue, or unpark an issue after manual cleanup.

The constraints are sharper than the read-only API. Mutating controls must be authenticated, auditable, serialized through the daemon, and safe when a worker is busy. They must not write directly to pi stdin from the control server, must not create duplicate dispatch, must not bypass workspace safety, and must not assume a worker can process a command while blocked on a long pi read unless the worker loop is designed for it.

There is also an architectural constraint: Scherzo should not grow one command implementation per client. The local control API, the pi operator skill, and a future Linear comment transport should all call the same daemon command handlers and receive the same stable command result statuses. Transport-specific work is limited to authentication, parsing, request/response encoding, confirmation, idempotency, and audit acknowledgements.

This plan assumes the prior phases are complete: a long-lived daemon exists, the EventHub records sessions and events, the local control API authenticates read-only requests, and `scherzoctl attach` renders sessions.

## Strategy Overview

Introduce a pure command module, `src/scherzo/control/command.gleam`, that defines Scherzo operator commands independently of any wire protocol. It should contain command target types, command variants, command names, target extraction helpers, and command result statuses such as `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`. Local TCP requests, `scherzoctl`, future pi skills, and future Linear comments map into these values.

Treat command results as domain results, not transport failures. Once a request is authenticated and decoded into an `OperatorCommand`, the control protocol returns `ok: true` with a command result payload even when the command status is `rejected`, `not_found`, or `not_allowed`. Reserve `ok: false` for malformed JSON, invalid protocol shape, wrong token, server timeout, or other failures where no command result can be trusted. This keeps future transports from conflating an expected operator rejection with a broken control channel.

Extend the control protocol with mutating request shapes, but route every decoded request to the daemon through the shared command model. The current control server only receives an EventHub read store and is started before the daemon actor subject exists. This plan must refactor startup so the control server is started from the actor initialiser, after the daemon subject is available, with a backend that includes both the EventHub read functions and an `apply_command` function. `apply_command` sends `ApplyOperatorCommand(command, reply)` to the daemon and waits for a bounded command timeout.

The daemon remains the only owner of scheduling state. Add an operator pause flag to daemon `State`, not to `domain.RuntimeState`, so pause is clearly runtime-only. Scheduler-level commands are handled directly by the daemon. Worker-level commands are sent by the daemon to a worker command subject stored in the worker handle. The control server never writes directly to pi RPC stdin and never mutates `domain.RuntimeState` itself.

Resolve issue references through one daemon helper before applying `retry`, `park`, or `unpark`. `IssueId` can use `tracker.fetch_issue_states_by_ids([id])` when the issue is not already known in runtime state. `IssueIdentifier` can be resolved from running workers, parked entries, completed entries, or the current candidate set fetched through the existing tracker client. Do not add a new tracker API in this phase; if the existing client cannot resolve exactly one issue, return `not_found` or `rejected` with a clear message rather than guessing.

Refactor worker execution so live workers can observe commands. This requires more than wrapping `runner.run_attempt` in an actor: the current `pi_rpc.prompt` owns the turn read loop, so the implementation must either split `pi_rpc` into stepwise send/read helpers or add a command-aware prompt loop that checks the worker command mailbox after every read timeout and after every pi event. Abort latency is bounded by the configured read timeout; tests should use a small read timeout rather than requiring instantaneous abort. Immediate abort can still kill the worker process as a fallback, but graceful abort should send pi's `abort` RPC command when possible.

Follow-up prompts are queued and sent only when the worker is between turns. Use a FIFO queue with a small fixed cap, such as 10 prompts per session, and return `rejected` with reason `prompt_queue_full` if the cap is reached. If a turn is actively streaming, the command returns `queued` rather than interrupting the current turn. If the session exits before a queued prompt is sent, publish an `operator_prompt_dropped` event.

Make operator-managed UI requests an explicit addition to the existing UI policy. `domain.UiRequestPolicy` currently has `Cancel`, `Fail`, and `Ignore`; add `Operator` plus `pi.ui_request_timeout_ms`. The worker loop must continue to honor the existing cancel/fail/ignore semantics instead of silently treating every blocking UI request as operator-managed. Only `Operator` creates pending UI state and accepts `RespondUi` commands.

Operator abort and stop-after-current-turn are safety controls, not normal worker failures. They should mark the session exited with an operator reason, release any claim, cancel retry timers for that issue, and park the issue with reason `operator_abort` or `operator_stop_after_current_turn` so it does not immediately redispatch. An operator can intentionally resume work later with `unpark` or `retry`.

Audit session-targeted commands through EventHub session events and audit global scheduler commands, such as pause, resume, and reload, through structured daemon logs. The EventHub is session-scoped today, so this phase should not invent a fake global session. Redact and truncate command content, especially prompt text and UI values, before logging or storing audit messages.

## Alternatives Considered

One alternative is to implement mutating controls directly in `scherzoctl` by finding and killing OS processes or writing to workspace files. That is unsafe because it bypasses the daemon's claims, retry counters, and cleanup logic.

Another alternative is to allow prompt injection at any time by writing to pi stdin concurrently. That can corrupt JSONL ordering and race with the worker's command/response correlation. Queueing prompts through the worker loop is slower but protocol-safe.

A third alternative is to expose mutating controls only through Linear comments. That would be asynchronous and ambiguous, and it would require Scherzo to poll and interpret human text commands before the command model has proven itself. Local authenticated controls are clearer and easier to audit as the first transport.

A fourth alternative is to let each transport implement command semantics independently. That would drift quickly: `/scherzo retry` in Linear might not match `scherzoctl retry`, and safety fixes would have to be duplicated. This plan instead makes the command model and daemon handlers transport-independent.

## Risks and Countermeasures

The main safety risk is duplicate dispatch or corrupted scheduler state. Countermeasure: all scheduler mutations go through daemon command handling, and every command rechecks current runtime state before applying. `retry-now` must not dispatch if the issue is running, already claimed, or in `pending_claims`; if it targets a parked issue, it must explicitly clear the parked state before dispatching. `pause` blocks new candidate fetch/claim/dispatch work, but it does not cancel claims already in flight; the response message should mention the number of pending claims if any exist. Reconciliation, retry bookkeeping, cleanup, and shutdown continue while paused.

The main startup risk is accidentally giving the local control server no safe path back to the daemon. Today the server is started before the actor subject exists and only receives an EventHub store. Countermeasure: move control server startup into the daemon actor initialiser or otherwise make the daemon subject available before accepting mutating commands. Add a command timeout so a stuck worker command cannot hang a local TCP connection forever.

The main worker risk is corrupting pi RPC command ordering. Countermeasure: worker commands are handled by the worker process that owns the pi session. It sends `abort`, `prompt`, or `extension_ui_response` using the same `pi_rpc.Session` next-id sequence as normal prompts. The control server never writes to the port. The worker loop must check commands inside the pi read loop, not only before and after a full `pi_rpc.prompt` call.

The main retry risk is that an operator abort is treated like an ordinary failure and immediately starts another worker for the same runaway issue. Countermeasure: operator abort and stop-after-current-turn use explicit operator exit reasons, cancel outstanding retry timers, release the claim, and park the issue until an operator un-parks or retries it.

The main UX risk is an operator thinking a prompt interrupted the current turn when it was only queued. Countermeasure: command responses distinguish `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`, and every accepted session-targeted command publishes a session event with the same status. Valid command rejections are returned as `ok: true` command results, not `ok: false` protocol failures.

The main security risk is a local process abusing the control token. Countermeasure: preserve token authentication, reject wrong-token mutating requests before command dispatch, log every mutating command with command type and session/issue id, redact command content where necessary, and require `scherzoctl --yes` for destructive commands such as `abort`, `stop-after-turn`, and `park`.

The main UI-request risk is hanging a worker forever while waiting for an operator response or accidentally changing existing cancel/fail/ignore behavior. Countermeasure: keep the default `pi.ui_request_policy: cancel`; implement existing `fail` and `ignore` behavior in the refactored worker loop; add `operator` policy only when configured. Operator-handled UI requests have a timeout, after which Scherzo sends a cancel response, records an event, and returns the session to running or exited state according to pi's response.

The main audit risk is claiming every command has a session event even when pause, resume, and reload are global commands with no session id. Countermeasure: session-targeted commands publish EventHub events; scheduler-level commands publish structured daemon logs. Do not create a fake session solely for global audit in this phase.

The main architecture risk is coupling command behavior to the local TCP protocol. Countermeasure: tests must exercise daemon command handling through the pure command type without going through TCP, and protocol tests must only prove request decoding/encoding maps to the same command type. Future transports, including Linear comments, reuse the same command type and handlers.

Transport-level idempotency is not solved by the command model itself. `scherzoctl` is a synchronous local client and does not need comment-style deduplication. Future Linear comment commands must add their own idempotency layer before submitting commands to the daemon.

## Progress

- [x] (2026-04-28 18:40Z) Confirmed this phase should follow read-only control and terminal attach.
- [x] (2026-04-28 23:05Z) Revised the plan so mutating controls define the shared command API that future Linear and pi-skill transports reuse instead of treating `scherzoctl` as the only command surface.
- [x] (2026-04-28 23:36Z) Normalized the tree after the prior visibility phases; baseline `direnv exec . gleam test` passed with 141 tests before new implementation tests were added.
- [x] (2026-04-28 23:48Z) Added transport-independent command and command-result types in `src/scherzo/control/command.gleam` with tests in `test/control_command_test.gleam`.
- [x] (2026-04-29 00:07Z) Extended the control protocol, control client, control server backend, and `scherzoctl` parsing/execution for mutating commands. Wrong-token mutating commands are rejected before backend dispatch, backend timeouts return `command_timeout`, and command-level rejections encode as `ok: true` command results.
- [x] (2026-04-29 00:24Z) Moved control-server startup into the daemon actor initialiser so authenticated command requests route to `ApplyOperatorCommand` after the daemon subject exists.
- [x] (2026-04-29 00:39Z) Added daemon-level scheduler controls for pause, resume, reload, retry-now, park, and unpark. Pause is runtime-only and blocks new dispatch; retry rejects active issues and clears parked state before dispatch; park/unpark mutate daemon-owned runtime state and cancel retry timers.
- [x] (2026-04-29 00:46Z) Implemented daemon-level abort and stop-after-current-turn safety handling by marking the session exited with an operator reason, killing the worker process, releasing daemon state, cancelling retry timers, and parking the issue. Graceful pi `abort` through a command-aware worker subject remains unfinished.
- [x] (2026-04-29 17:05Z) Implemented queued operator prompts between turns in `docs/plans/command-aware-worker-loop.md`; daemon-spawned workers now expose command subjects, prompts can be queued or applied through the worker loop, queue overflow is rejected, and dropped prompts are audited.
- [x] (2026-04-29 17:05Z) Implemented operator-managed UI request responses in `docs/plans/command-aware-worker-loop.md`; `pi.ui_request_policy: operator` routes `scherzoctl ui respond`, enforces timeout cancellation, and preserves the existing cancel/fail/ignore policies.
- [x] (2026-04-29 00:58Z) Updated README and wrapper comments, then ran `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, which passed with 154 tests.

## Surprises & Discoveries

- Observation: The initial mutating-control slice exposed worker-command placeholders before daemon-spawned workers could use them.
  Evidence: At the first slice, `src/scherzo/orchestrator/daemon.gleam` defined worker command routing concepts but spawned workers did not yet have command subjects. The later command-aware worker loop filled that gap.

- Observation: The control server bootstrap had to move inside daemon actor initialization before mutating commands could be safe.
  Evidence: The read-only control server originally received only EventHub read functions. Mutating commands now route through the daemon-owned `ApplyOperatorCommand` path after token authentication.

- Observation: Operator UI handling was not just a config parser change; it needed pending UI state owned by the worker that owns the pi RPC session.
  Evidence: The completed command-aware worker loop routes `RespondUi` through worker command subjects, enforces UI timeouts, and keeps `cancel`, `fail`, and `ignore` behavior covered separately from the opt-in `operator` policy.

- Observation: EventHub events are session-scoped, so global commands cannot be represented as session events without adding a new global audit stream or inventing a fake session.
  Evidence: `src/scherzo/session/event.gleam` requires `SessionEvent(session_id, issue_id, payload)` and `src/scherzo/session/hub.gleam` stores events under registered sessions.

- Observation: The follow-up command-aware worker loop completed the prompt and operator-UI controls deferred from the first mutating-control slice.
  Evidence: `docs/plans/command-aware-worker-loop.md` records worker-owned command subjects, FIFO prompt queueing, queue-cap enforcement, operator-managed UI waits, response routing, timeout cancellation, README updates, and validation with 175 tests.

## Decision Log

- Decision: Route all mutating controls through the daemon actor.
  Rationale: The daemon is the only owner of scheduler state; bypassing it would reintroduce duplicate dispatch and cleanup races.
  Date: 2026-04-28

- Decision: Queue follow-up prompts until the worker is between turns.
  Rationale: Pi RPC command ordering must remain single-owner and deterministic. Mid-turn prompt injection is not safe unless pi explicitly supports it.
  Date: 2026-04-28

- Decision: Keep operator UI handling opt-in with `pi.ui_request_policy: operator`.
  Rationale: The current safe default is to cancel dialogs. Waiting for humans changes liveness and should be explicit in workflow config.
  Date: 2026-04-28

- Decision: Define a transport-independent command model before implementing Linear-sourced commands.
  Rationale: `scherzoctl`, pi skills, and Linear comments should all call the same daemon handlers. This keeps safety checks, result statuses, audit events, and tests consistent across clients.
  Date: 2026-04-28

- Decision: Return valid command rejections as successful protocol responses containing `CommandResult`.
  Rationale: `rejected`, `not_found`, and `not_allowed` are expected command outcomes. Reserving `ok: false` for malformed requests, authentication failures, timeouts, and server failures gives all transports the same semantics.
  Date: 2026-04-28

- Decision: Start the control server only after the daemon actor subject is available.
  Rationale: Mutating commands must be serialized through the daemon. The current read-only startup path cannot safely dispatch commands because it creates the server before the daemon subject exists.
  Date: 2026-04-28

- Decision: Operator abort and stop-after-current-turn park the issue instead of flowing through ordinary retry failure handling.
  Rationale: A safety stop should not immediately redispatch the same runaway issue. Operators can explicitly resume with `unpark` or `retry` after cleanup.
  Date: 2026-04-28

- Decision: Keep audit storage proportional to the current EventHub model.
  Rationale: EventHub is session-scoped. Session-targeted commands get session events; global scheduler commands use structured daemon logs until a future plan adds global audit history.
  Date: 2026-04-28

- Decision: Keep pending UI request state inside the worker command loop for this phase.
  Rationale: `scherzoctl ui respond` can route by session id and request id, and attach already shows UI request events. Avoiding a `SessionSummary` schema expansion keeps the phase smaller while still making pending UI observable through status and events.
  Date: 2026-04-28

- Decision: Land scheduler controls and destructive process fallback before the full command-aware worker loop.
  Rationale: The command model, authentication boundary, daemon routing, pause/reload/retry/park/unpark, and safe abort/stop fallback are independently useful and covered by deterministic tests. Prompt queueing and operator UI responses require a larger pi read-loop refactor and remain tracked as incomplete progress items instead of being hidden behind stub success.
  Date: 2026-04-29

- Decision: Treat `docs/plans/command-aware-worker-loop.md` as the completion vehicle for the deferred prompt and operator-UI milestones in this plan.
  Rationale: That follow-up plan implemented the worker-owned command loop this plan identified as necessary, without changing the shared command model or daemon serialization rules. Marking the stale progress items complete keeps this living plan aligned with the current tree and prevents duplicate implementation work.
  Date: 2026-04-30

## Outcomes & Retrospective

First implementation slice outcome, 2026-04-29: Scherzo now has a shared command model, mutating protocol shapes, authenticated server dispatch with timeout handling, `scherzoctl` parsing/execution for all planned local commands, and daemon-owned handlers for pause, resume, reload, retry-now, park, unpark, abort, and stop-after-current-turn. `PromptSession` and `RespondUi` were intentionally returned as command-level `not_allowed` results until workers exposed command subjects and owned a command-aware pi loop. The main lesson is that routing through the daemon and preserving command-result semantics was a separable milestone; worker prompt/UI controls belonged in a follow-up milestone rather than pretending to work without single-owner pi command ordering.

Follow-up completion outcome, 2026-04-30: The previously deferred prompt and UI controls are complete in the current tree through `docs/plans/command-aware-worker-loop.md`. Daemon-spawned workers register command subjects, prompt commands can be queued, applied, or rejected by the worker loop, operator UI waits can be answered or cancelled on timeout, and that follow-up plan recorded final validation with 175 tests. This mutating-controls plan has no remaining unchecked implementation items; durable Linear command receipts and other transport-specific reliability work remain tracked in separate plans.

## Context and Orientation

The read-only control API accepts authenticated local JSON requests and serves EventHub data. `scherzoctl` is the CLI client. The daemon actor owns `domain.RuntimeState`, worker handles, timers, workflow reload state, and cleanup effects. Worker processes own pi RPC sessions and communicate updates back to the daemon.

A mutating control is any request that changes daemon state, worker state, pi session state, or issue scheduling. Examples are pause, resume, abort, prompt, retry, park, unpark, and UI response. These controls are different from Linear handoff writes in the real-board-readiness plan; handoff reports work to Linear, while controls alter Scherzo operation.

A command transport is any way an operator request reaches Scherzo. The local TCP control API is one transport. `scherzoctl` is a local CLI client for that transport. The future Linear comment command plan is another transport. All transports must map requests into the same command types and must receive the same command result statuses.

A worker command subject is a process subject stored in the daemon's worker handle. The daemon sends worker-level commands to it. The worker receives commands between pi read polls and between turns.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/real-board-readiness.md` is complete.
- `docs/plans/session-eventhub.md` is complete.
- `docs/plans/local-control-api-and-scherzoctl.md` is complete.
- `docs/plans/terminal-attach-renderer.md` is complete.
- `scherzoctl ps`, `scherzoctl events`, and `scherzoctl attach` work read-only.
- The control protocol requires a token and the control server is local-only.
- The daemon worker handle type can be extended without breaking current tests.
- `direnv exec . gleam test` passes.

Current repository facts to normalize during implementation:

- `src/scherzo/orchestrator/daemon.gleam` currently defines placeholder `WorkerCommand` variants.
- `src/scherzo/orchestrator/daemon.gleam` currently stores `command_subject: Option(process.Subject(WorkerCommand))` in `WorkerHandle`.
- Worker handle construction currently sets `command_subject: None`, so worker commands are not yet deliverable.
- `src/scherzo/orchestrator/daemon.gleam` starts the control server before the daemon actor subject exists; this must change before mutating controls can route through the daemon.
- `src/scherzo/control/server.gleam` currently accepts only an EventHub `EventStore`; it needs a backend that also applies operator commands with a timeout.
- `src/scherzo/agent/runner.gleam` is synchronous and `src/scherzo/agent/pi_rpc.gleam` owns the full prompt read loop, so worker command polling requires a real loop refactor.
- `src/scherzo/domain.gleam` currently defines `UiRequestPolicy` as `Cancel`, `Fail`, and `Ignore`; add `Operator` and a UI request timeout field.
- `src/scherzo/config.gleam` currently parses `fail` and `ignore` but defaults unknown values to cancel; add explicit `operator` parsing and reject unknown policy strings instead of silently cancelling them.
- `src/scherzo/orchestrator/core.gleam` has private parking logic and no public manual unpark helper; expose or add small pure helpers for manual park/unpark/retry control paths.
- There is no `src/scherzo/control/command.gleam` module yet unless the local-control plan creates it first.

If the current worker implementation is still a synchronous function without a command mailbox, this plan's worker refactor is required before prompt and UI-response controls can work.

## Scope Boundaries

In scope: transport-independent mutating command and result types; mutating control protocol commands; local `scherzoctl` mutating subcommands; daemon command handling; runtime pause/resume overlay; workflow reload request; retry-now; park/unpark; abort worker; stop-after-current-turn; queued follow-up prompts; operator UI request policy and timeout; UI response command; audit events; tests for command authorization, serialization, scheduling safety, worker command behavior, and CLI confirmation.

Out of scope: Linear comment polling or parsing; remote multi-user authorization; role-based access control; web forms; persistent command audit database; direct Linear command comments; mid-turn prompt injection; editing files through the control API; attaching stdin interactively to a worker; distributed scheduler state; a global EventHub audit stream; expanding `SessionSummary` with pending UI request lists.

The later Linear command transport must reuse the command model introduced here, but it owns its own polling, authorization, idempotency, edited-comment behavior, and acknowledgement comments.

## Milestones

Milestone 1 defines the shared command model, extends the local protocol safely, and fixes the control-server bootstrap path. At the end, mutating command requests and responses can be encoded/decoded, `scherzoctl` parses the commands, daemon command tests can construct commands without TCP, and the control server can call a fake command backend only after token authentication. Temporary `not_implemented` command results may exist during this milestone, but final acceptance must remove them or confine them to unsupported commands.

Milestone 2 adds daemon-level scheduler controls. At the end, pause/resume, reload, retry-now, park, and unpark are serialized through the daemon, pause behavior around pending claims is defined, issue references resolve deterministically, and these paths are covered by tests without touching pi.

Milestone 3 adds worker command channels for abort and stop-after-current-turn. At the end, a running fake worker can be stopped through the daemon, the session records an audit event, the issue is parked with an operator reason, retry timers are cancelled, and no stale duplicate work is dispatched after abort.

Milestone 4 adds queued operator prompts. At the end, a prompt sent while a worker is idle between turns becomes the next prompt, a prompt sent during a turn is queued for the following turn with a `queued` response, prompt queue overflow is rejected, and dropped queued prompts are audited if the session exits first.

Milestone 5 adds operator UI responses. At the end, workflows can opt into `pi.ui_request_policy: operator`, existing cancel/fail/ignore policies still behave as before, pending UI requests are owned by the worker loop and visible through session status/events, `scherzoctl ui respond` can answer or cancel them, and timeout cancels unanswered requests.

Milestone 6 updates documentation and runs acceptance. At the end, README documents mutating commands, confirmation flags, safety constraints, transport-independent command semantics, result status semantics, and examples.

## Plan of Work

Create `src/scherzo/control/command.gleam`. Define command target types, command variants, command-name helpers, target extraction helpers, result statuses, result-to-string helpers, and result constructors. This module should not import the local TCP protocol, the CLI parser, or Linear. It may import domain types only if necessary for stable identifiers. It should be small enough for future transports to reuse directly.

Extend `src/scherzo/control/protocol.gleam` with request types for `Pause`, `Resume`, `ReloadWorkflow`, `RetryIssue(issue_id_or_identifier)`, `ParkIssue(issue_id_or_identifier, reason)`, `UnparkIssue(issue_id_or_identifier)`, `AbortSession(session_id)`, `StopAfterCurrentTurn(session_id)`, `PromptSession(session_id, message)`, and `RespondUi(session_id, request_id, response)`. Protocol decoding should return or contain `control/command.OperatorCommand` values rather than duplicating command semantics. Add `command_result_data` and `decode_command_result_response`; `rejected`, `not_found`, and `not_allowed` must encode inside `data`, not inside the top-level `error` object.

Extend `src/scherzo/control/client.gleam` with `apply_command` helpers that send the new protocol requests and decode `CommandResult`. Extend its request authentication helper for every mutating request so callers cannot accidentally send an empty token.

Refactor `src/scherzo/control/server.gleam` so `start` receives a backend containing both the existing EventHub read functions and an `apply_command` function. Keep token checking in `handle_connection` before the backend is called. Add `command_timeout_ms` to `Settings`; if the backend does not return in time, return top-level `ok: false` with code `command_timeout`.

Refactor `src/scherzo/orchestrator/daemon.gleam` startup so the control plane starts after the actor subject exists. One concrete implementation is to move `start_control_plane` into the `actor.new_with_initialiser` function, build a control backend whose `apply_command` sends `ApplyOperatorCommand(command, reply)` to the subject, and store the resulting control server handle and control file path in `State`. If control server startup or control file writing fails, the initialiser should return the existing `StartupError` and clean up any partially created server or file.

Extend `src/scherzo/ctl.gleam` with subcommands `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`. Destructive commands require `--yes`; otherwise the CLI returns `UsageError` so `src/scherzo/main.gleam` exits with code 2. `park` requires `--reason <text>`. `ui respond` requires exactly one of `--cancel` or `--value <text>`. CLI parsing maps to the same command values used by protocol tests.

Modify `src/scherzo/orchestrator/daemon.gleam` to accept an operator command message such as `ApplyOperatorCommand(command.OperatorCommand, process.Subject(command.CommandResult))`, or equivalent specific messages that delegate immediately to one shared handler. Add runtime state outside `domain.RuntimeState` for an operator pause flag. `pause` sets this flag and blocks new candidate fetch/claim/dispatch work even if workflow config allows dispatch. `resume` clears the flag. Reconciliation, retry bookkeeping, cleanup, and shutdown still run while paused. Pending claims already in flight are allowed to finish; include the pending count in the pause result message.

Implement `reload` by extracting the current `reload_if_changed`/`apply_new_contents` behavior into a helper that can be called immediately by the command handler without waiting for the next poll tick. It returns `applied` on valid reload and `rejected` with a config/workflow error code on invalid reload, while preserving the existing invalid-reload dispatch-blocking behavior. Do not move or rewrite the control file when reload changes `workspace.root`; the local control server identity remains stable until daemon restart.

Implement `retry-now`, `park`, and `unpark` with pure helper functions where possible. `retry-now` should resolve the issue id or identifier, reject if the issue is running, claimed, or pending claim, clear parked state when the target is parked, and dispatch through the normal dispatch helper only if `core.should_dispatch` and daemon slot reservation both allow it. `park` rejects running or pending-claim issues, inserts a parked entry with the current observed `updated_at` when known, cancels retry timers, and releases any claim. `unpark` removes parked state and counters and cancels stale retry timers; it does not dispatch by itself unless the operator also sends `retry`.

Create or refactor `src/scherzo/agent/worker.gleam`. It should own the pi RPC session and expose a command subject. If the existing runner remains useful for one-shot tests, keep `runner.run_attempt` for synchronous harness tests while daemon uses the worker actor. If `runner.run_attempt` wraps the worker loop, the wrapper must still be command-aware for daemon usage.

Refactor `src/scherzo/agent/pi_rpc.gleam` so the worker can interleave command handling with pi reads. Keep the existing high-level functions for compatibility where possible, but add stepwise helpers such as `send_prompt`, `read_turn_record`, `send_abort`, `send_extension_ui_response`, and `get_session_stats` variants that operate on `Session` and preserve `next_id`. Do not expose the raw port to the daemon or control server.

Add `Operator` to `domain.UiRequestPolicy` and `ui_request_timeout_ms` to `domain.PiConfig`. Update `src/scherzo/config.gleam` so `ui_request_policy` accepts exactly `cancel`, `fail`, `ignore`, or `operator`; unknown values should return `InvalidConfig` instead of silently becoming cancel. The default remains `cancel`, and the default UI timeout is 300000 ms.

Keep pending UI request tracking inside the worker loop. A pending UI request stores request id, method, prompt/message excerpt, created time, and deadline. The daemon routes `RespondUi(session_id, request_id, response)` to the worker by session id; the worker validates the request id and policy. Only `pi.ui_request_policy: operator` creates pending requests; `cancel`, `fail`, and `ignore` keep previous behavior.

Publish audit events to the EventHub for session-targeted control requests: command received, command applied/queued/rejected, and worker-level results such as `pi_abort_sent`, `operator_prompt_sent`, `operator_prompt_dropped`, or `ui_response_sent`. Publish scheduler-level commands such as pause/resume/reload through the structured daemon logger. Redact and truncate prompt text, UI values, and operator reasons in both places.

Update `README.md` with command examples and safety notes. Mention that this phase creates the command model reused by future transports and that Linear comments are intentionally deferred to `docs/plans/linear-command-transport.md`.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count. Stop if read-only control or attach tests fail.

2. Create `test/control_command_test.gleam`. Add tests that construct every `OperatorCommand` variant and assert `command_name`, target extraction, and JSON-safe status strings for `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`.

3. Implement `src/scherzo/control/command.gleam` until the command tests pass.

4. Add tests to `test/control_protocol_test.gleam` for encoding and decoding each mutating request type. Add tests that `command_result_data(CommandResult(... Rejected("busy") ...))` encodes as a top-level successful response with `ok: true` and `data.status == "rejected"`, while malformed requests still decode to request errors.

5. Extend `src/scherzo/control/protocol.gleam` until the protocol tests pass.

6. Add tests to `test/control_server_test.gleam` with a fake command backend. Assert an authenticated mutating request calls the backend once and returns the backend `CommandResult`; a wrong token returns top-level `ok: false` with code `unauthorized` and does not call the backend; a backend timeout returns top-level `ok: false` with code `command_timeout`.

7. Refactor `src/scherzo/control/server.gleam` to accept the combined read/event and command backend plus `command_timeout_ms`. Keep existing read-only server tests passing.

8. Add a daemon startup test in `test/orchestrator_daemon_control_test.gleam` that proves the control server can route an authenticated command to the daemon after the actor subject exists. The initial daemon handler may return `CommandResult(status: Rejected("not_implemented"))` for commands not yet implemented.

9. Refactor `src/scherzo/orchestrator/daemon.gleam` startup so `start_control_plane` runs inside the actor initialiser after `subject` is available. Add `ApplyOperatorCommand` and a shared handler stub. Run the control server and daemon control tests.

10. Add tests to `test/ctl_test.gleam` for parsing `pause`, `resume`, `reload`, `retry ABC-123`, `park ABC-123 --reason manual --yes`, `park ABC-123 --reason manual` without `--yes`, `unpark ABC-123`, `abort <session> --yes`, `abort <session>` without `--yes`, `stop-after-turn <session> --yes`, `prompt <session> "continue"`, `ui respond <session> ui-1 --cancel`, and `ui respond <session> ui-1 --value ok`. Assert successful parses produce the same command values as protocol decoding.

11. Extend `src/scherzo/control/client.gleam` and `src/scherzo/ctl.gleam` parser, execution, and usage text until CLI and client tests pass. Mutating command execution may still receive `not_implemented` command results from the daemon stub.

12. Commit point: run `direnv exec . gleam test`; if green, commit the command model, protocol, server backend, daemon bootstrap, and CLI parsing work.

13. Add `pause_blocks_new_dispatch_but_not_reconciliation_test`: start a daemon with one candidate, send pause, wait for a poll, assert no new claim/dispatch starts, then arrange a running issue reconciliation and assert reconciliation still occurs.

14. Add `pause_reports_pending_claims_without_cancelling_them_test`: arrange one pending claim, send pause, assert the command result is `applied` with a message mentioning one pending claim, and assert the pending claim either finishes or is cleaned up by existing claim-failure logic rather than being silently dropped.

15. Implement daemon `Pause` and `Resume` command handling. Run tests.

16. Add `reload_control_applies_valid_reload_and_rejects_invalid_test`: rewrite the workflow to valid and invalid versions, send reload, and assert the command response and dispatch-blocking state match expectations. Include a case where `workspace.root` changes and assert the existing control file/server remain usable until restart.

17. Implement reload control by extracting and reusing existing workflow reload code. Run tests.

18. Add issue-reference resolution tests in `test/orchestrator_daemon_control_test.gleam`: id resolution through `fetch_issue_states_by_ids`, identifier resolution through running/parked/completed/candidate state, `not_found` for zero matches, and `rejected` for ambiguous identifier matches.

19. Add `retry_now_does_not_duplicate_running_claimed_or_pending_issue_test`: with running, claimed, and pending-claim issues, send retry-now and assert response `rejected`; with a parked eligible issue, assert parked state is cleared and the normal dispatch helper is used once; with a non-running eligible issue, assert the normal dispatch helper is used once.

20. Implement retry-now through the daemon and normal dispatch path. Run tests.

21. Add `park_and_unpark_issue_control_test`: park a non-running issue with reason `manual`, assert it appears in runtime parked state and structured audit output, unpark it, and assert it becomes dispatchable on a later poll but is not dispatched immediately by the unpark command itself.

22. Implement manual park and unpark controls, including retry timer cancellation and public core helpers if needed. Run tests.

23. Commit point: run `direnv exec . gleam test`; if green, commit daemon scheduler controls.

24. Create `test/agent_worker_control_test.gleam`. Add `abort_command_stops_fake_pi_worker_test`: start a worker with fake pi that delays, send abort through the worker command subject, assert worker exits with an operator-abort classification and the fake pi transcript contains `abort` if graceful abort was possible.

25. Add `stop_after_current_turn_parks_after_turn_end_test`: send stop-after-current-turn while a fake turn is streaming, assert response `queued`, assert the current turn reaches `agent_end`, then assert the worker exits with operator-stop classification without starting another prompt.

26. Refactor or create `src/scherzo/agent/worker.gleam` and stepwise helpers in `src/scherzo/agent/pi_rpc.gleam` so worker commands are checked between pi read attempts and after pi events. Add explicit operator abort/stop error or result classifications in `src/scherzo/error.gleam` or the worker result type.

27. Modify daemon worker handles to store the worker command subject and route `AbortSession` and `StopAfterCurrentTurn` controls by session id. Add daemon tests for missing session `not_found`, worker command timeout `rejected`, audit events, claim release, retry cancellation, and operator parking.

28. Implement daemon worker command routing and fallback process kill for abort if the worker does not acknowledge in time. Run `direnv exec . gleam test`.

29. Commit point: run `direnv exec . gleam test`; if green, commit worker abort and stop controls.

30. Add `operator_prompt_sent_between_turns_test`: when a worker is between turns, send a prompt and assert response `applied` and the fake transcript contains that prompt as the next pi prompt.

31. Add `operator_prompt_queued_during_turn_and_sent_next_turn_test`: using a fake pi mode that delays within a turn, send a prompt while busy, assert command response `queued`, then assert the fake transcript contains the operator prompt as the next prompt after current turn end.

32. Add `operator_prompt_queue_full_and_dropped_tests`: fill the prompt queue to its cap, assert the next prompt returns `rejected` with reason `prompt_queue_full`, and assert queued prompts are audited as dropped if the session exits before they are sent.

33. Implement prompt queueing in the worker loop. Run tests.

34. Commit point: run `direnv exec . gleam test`; if green, commit queued operator prompts.

35. Extend config tests in `test/config_test.gleam` for default `cancel`, explicit `cancel`, `fail`, `ignore`, explicit `operator`, default `ui_request_timeout_ms` of 300000, custom positive timeout, and invalid policy/timeout rejection.

36. Add fake pi fixture support for UI requests that wait for response before continuing. Add worker tests for existing `cancel`, `fail`, and `ignore` policies so the refactor does not regress current behavior.

37. Add `operator_ui_request_response_test`: with `pi.ui_request_policy: operator`, worker receives a UI request, session status becomes `waiting_ui`, control response sends cancel or selected value, and worker continues or exits according to fake pi behavior.

38. Add `operator_ui_request_timeout_test`: set a tiny `pi.ui_request_timeout_ms`, do not answer the pending request, and assert Scherzo sends cancel, publishes a timeout event, and does not hang.

39. Implement operator UI request tracking and `RespondUi` command routing through the worker. Run tests.

40. Update control server integration tests to assert wrong-token mutating commands are rejected like read-only commands, and valid command-level rejections still return top-level `ok: true` with `data.status == "rejected"`.

41. Update `README.md` with mutating command examples and warnings. Include examples using `--yes` for destructive commands, `--cancel` and `--value` for UI responses, and note that prompts are queued between turns. Mention that Linear command comments are a separate future transport over the same command model.

42. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count in Progress.

43. Commit point: commit the UI controls, docs, and final acceptance fixes with a message such as `Add authenticated Scherzo operator controls`.

## Testing and Falsifiability

This phase is falsified if a mutating command can bypass token authentication, if a valid command-level rejection is returned as a top-level protocol failure, if a control command mutates state outside the daemon, if the control server cannot route to the daemon without a startup race, if `retry-now` can start a duplicate worker for a running, claimed, or pending-claim issue, if retrying a parked issue dispatches without clearing parked state, if pause drops pending claims silently, if an operator prompt corrupts pi command ids, if abort leaves a worker handle behind, if operator abort immediately redispatches the same issue, if UI operator mode hangs forever without timeout, if existing cancel/fail/ignore UI policies regress, if destructive CLI commands run without explicit `--yes`, or if protocol/CLI/daemon tests implement different command semantics for the same operation.

Add deterministic tests in `test/control_command_test.gleam`, `test/control_protocol_test.gleam`, `test/control_server_test.gleam`, `test/ctl_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/agent_worker_control_test.gleam`, `test/pi_rpc_test.gleam`, and `test/config_test.gleam`. Use fake tracker clients and `test/fixtures/fake_pi_rpc.sh`. No test may require real Linear or real pi.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests for mutating protocol, command model, and worker commands should fail to compile. After implementation, all tests should pass and no worker or socket test should hang.

## Validation and Acceptance

Manual acceptance after tests with a safe fake-pi workflow:

1. Start daemon mode and dispatch one fake worker.
2. Run `scripts/scherzoctl pause` and verify later polls do not dispatch new issues, while daemon logs contain a structured pause audit entry.
3. Run `scripts/scherzoctl resume` and verify dispatch resumes.
4. Run `scripts/scherzoctl retry <running-issue>` and verify the response is a command result with status `rejected`, not a transport/protocol failure.
5. Run `scripts/scherzoctl prompt <session-id> "summarize progress"` during a worker turn and verify attach shows the prompt queued and then sent.
6. Run `scripts/scherzoctl abort <session-id> --yes` and verify the worker exits, attach shows an audit event, the issue becomes parked with reason `operator_abort`, no retry timer redispatches it, and no orphan fake-pi process remains.
7. With `pi.ui_request_policy: operator`, run a fake UI request and answer it with `scripts/scherzoctl ui respond <session-id> ui-1 --cancel`; verify attach shows the request and response.
8. With the default `pi.ui_request_policy: cancel`, run the same fake UI request and verify it is auto-cancelled without accepting an operator response command.

Do not accept this phase if any mutating command is available without the control token, if a valid command rejection returns `ok: false`, if the control server writes directly to a pi port, or if a command works only through `scherzoctl` and cannot be invoked through the shared daemon command handler.

## Rollout, Recovery, and Idempotence

Mutating controls are additive but operationally sensitive. If a command handler fails after authentication and decoding, it should return `rejected` or `not_allowed` as a command result and publish an audit event rather than crashing the daemon. If a worker does not acknowledge a command before a timeout, the daemon should report `rejected` or fall back to existing stop/kill behavior for abort. Only transport, authentication, parse, and backend timeout failures should use top-level `ok: false`.

Pause/resume is runtime-only and resets on daemon restart unless a future plan persists it. Pause does not cancel already pending claims; operators who need a hard drain should abort or park specific sessions/issues. Queued prompts are runtime-only and are dropped if the worker exits. UI request pending state is runtime-only and resolves by response, timeout, or worker exit. Operator abort and stop-after-turn park the issue to contain redispatch; rollback is to `unpark` or `retry` after the operator confirms the issue is safe to run again.

Read-only operations remain available if operators avoid mutating commands. If this phase causes problems in production, operators can continue using `scherzoctl ps`, `scherzoctl events`, and `scherzoctl attach` while mutating command usage is disabled operationally. Because the control API is local and token-authenticated, operational disablement can be as simple as not distributing the token/control file path to operators until confidence is restored.

Command execution is not globally idempotent. Local `scherzoctl` commands are explicit synchronous operator actions. Future asynchronous transports such as Linear comments must add their own deduplication before submitting commands to the daemon.

## Artifacts and Notes

Example command responses:

    {"ok":true,"data":{"status":"applied","command":"pause"}}
    {"ok":true,"data":{"status":"queued","command":"prompt","target":"ABC-123-..."}}
    {"ok":true,"data":{"status":"rejected","command":"abort","target":"ABC-123-...","message":"session is not running"}}
    {"ok":false,"error":{"code":"unauthorized","message":"invalid control token"}}

A session-targeted command should produce an EventHub audit event similar to:

    kind=lifecycle name=operator_command message="command=abort status=applied target=ABC-123-..."

A global scheduler command should produce a structured daemon log entry similar to:

    event=operator_command command=pause status=applied target=global

Future Linear comment transport example, intentionally out of scope here:

    /scherzo prompt "Please continue after applying Brian's suggestion."

That future transport parses the comment, authorizes the Linear actor, deduplicates the comment id, maps the command into `control/command.OperatorCommand`, and submits it to the daemon handler introduced by this plan.

## Interfaces and Dependencies

In `src/scherzo/control/command.gleam`, define types equivalent to:

    pub type IssueRef {
      IssueId(String)
      IssueIdentifier(String)
    }

    pub type UiResponse {
      UiCancel
      UiValue(String)
    }

    pub type OperatorCommand {
      PauseDispatch
      ResumeDispatch
      ReloadWorkflow
      RetryIssue(IssueRef)
      ParkIssue(IssueRef, reason: String)
      UnparkIssue(IssueRef)
      AbortSession(session_id: String)
      StopAfterCurrentTurn(session_id: String)
      PromptSession(session_id: String, message: String)
      RespondUi(session_id: String, request_id: String, response: UiResponse)
    }

    pub type CommandStatus {
      Applied
      Queued
      Rejected(reason: String)
      NotFound
      NotAllowed(reason: String)
    }

    pub type CommandResult {
      CommandResult(
        command: String,
        status: CommandStatus,
        target: Option(String),
        message: Option(String),
      )
    }

Add worker command types equivalent to:

    pub type WorkerCommand {
      Abort(reply: process.Subject(command.CommandResult))
      StopAfterCurrentTurn(reply: process.Subject(command.CommandResult))
      QueuePrompt(message: String, reply: process.Subject(command.CommandResult))
      RespondUi(request_id: String, response: command.UiResponse, reply: process.Subject(command.CommandResult))
    }

If the daemon already has a placeholder `WorkerCommand`, replace it or adapt it to include reply subjects so callers receive stable `CommandResult` values.

In `src/scherzo/control/server.gleam`, replace the read-only start signature with a backend shape equivalent to:

    pub type Backend {
      Backend(
        list_sessions: fn(Int) -> Result(List(event.SessionSummary), hub.HubError),
        get_session: fn(String, Int) -> Result(Option(event.SessionSummary), hub.HubError),
        events_after: fn(String, Int, Int, Int) -> Result(event.EventPage, hub.HubError),
        apply_command: fn(command.OperatorCommand, Int) -> Result(command.CommandResult, Nil),
      )
    }

The exact type name may differ, but the server must call `apply_command` only after token authentication and must convert timeout/no-reply into top-level `command_timeout`.

In `src/scherzo/orchestrator/daemon.gleam`, add message and state fields equivalent to:

    pub type Message {
      // existing messages...
      ApplyOperatorCommand(command.OperatorCommand, process.Subject(command.CommandResult))
    }

    type State {
      State(
        // existing fields...
        operator_paused: Bool,
      )
    }

In `src/scherzo/domain.gleam`, update UI configuration types equivalent to:

    pub type UiRequestPolicy {
      Cancel
      Fail
      Ignore
      Operator
    }

    pub type PiConfig {
      PiConfig(
        // existing fields...
        ui_request_policy: UiRequestPolicy,
        ui_request_timeout_ms: Int,
      )
    }

Add explicit operator stop classifications either to `src/scherzo/error.gleam` or to the worker result type, for example `OperatorAborted` and `OperatorStoppedAfterTurn`, so daemon handling does not have to infer operator stops from generic `PiFailed` errors.

No new package dependency should be required. Use existing actor/process modules and extend the existing control TCP protocol.

## Revision Notes

2026-04-28 review update: tightened the plan around control-server daemon routing, command result semantics, issue-reference resolution, operator abort safety, UI policy behavior, audit scope, and commit points. These changes make the plan executable from the current tree and avoid leaving future implementers to decide high-risk behavior during coding.

2026-04-28 implementation update: the worker-owned half now lives in `docs/plans/command-aware-worker-loop.md` and the current tree implements command-aware workers. Worker command subjects are created by the worker process that receives them, then registered with the daemon, preserving Gleam subject ownership. Prompt queueing, graceful worker-level abort, stop-after-current-turn, and operator UI responses are handled in `src/scherzo/agent/runner.gleam`; public command results are still converted by `src/scherzo/orchestrator/daemon.gleam` from agent-level `worker_command.Reply` values.
