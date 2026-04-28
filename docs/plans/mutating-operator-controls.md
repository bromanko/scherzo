# Add authenticated mutating operator controls

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo has one transport-independent operator command model and an authenticated local implementation of that model through the control API and `scherzoctl`. A local operator can safely pause and resume dispatch, reload the workflow, abort or stop a specific worker, queue an operator follow-up prompt for a live session, respond to supported pi extension UI requests when the workflow opts into operator handling, retry an issue now, and park or unpark an issue. The visible proof is that `scherzoctl pause`, `scherzoctl resume`, `scherzoctl abort <session> --yes`, `scherzoctl prompt <session> "..."`, and `scherzoctl ui respond <session> <request-id> --cancel` work against a fake daemon/worker in deterministic tests and produce auditable session events.

This phase introduces the canonical command API that future transports must reuse. `scherzoctl` over the local control API is the first client. A later Linear command transport will parse Linear comments and submit the same command values to the same daemon handlers; it must not mutate scheduler state through a separate Linear-specific path.

This plan introduces destructive and state-changing operations, so it is intentionally sequenced after read-only visibility and pretty attach are already working.

## Problem Framing and Constraints

Read-only attach helps an operator understand what a worker is doing, but it does not let them intervene. Real operation needs a small set of control actions: pause new dispatch during an incident, stop a runaway worker, ask a worker to continue with extra context, answer a pi UI dialog, retry a fixed issue, or unpark an issue after manual cleanup.

The constraints are sharper than the read-only API. Mutating controls must be authenticated, auditable, serialized through the daemon, and safe when a worker is busy. They must not write directly to pi stdin from the control server, must not create duplicate dispatch, must not bypass workspace safety, and must not assume a worker can process a command while blocked on a long pi read unless the worker loop is designed for it.

There is also an architectural constraint: Scherzo should not grow one command implementation per client. The local control API, the pi operator skill, and a future Linear comment transport should all call the same daemon command handlers and receive the same stable command result statuses. Transport-specific work is limited to authentication, parsing, request/response encoding, confirmation, idempotency, and audit acknowledgements.

This plan assumes the prior phases are complete: a long-lived daemon exists, the EventHub records sessions and events, the local control API authenticates read-only requests, and `scherzoctl attach` renders sessions.

## Strategy Overview

Introduce a pure command module, `src/scherzo/control/command.gleam`, that defines Scherzo operator commands independently of any wire protocol. It should contain command target types, command variants, and command result statuses such as `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`. Local TCP requests, `scherzoctl`, future pi skills, and future Linear comments map into these values.

Extend the control protocol with mutating request shapes, but route every decoded request to the daemon through the shared command model. The daemon remains the only owner of scheduling state. Scheduler-level commands are handled directly by the daemon. Worker-level commands are sent by the daemon to a worker command subject stored in the worker handle. The control server never writes directly to pi RPC stdin and never mutates `domain.RuntimeState` itself.

Refactor worker execution so live workers can observe commands. The worker loop should poll pi stdout using the existing short read timeout and check its command mailbox between poll iterations and event handling. Immediate abort can still be implemented by killing the worker process as a fallback, but graceful abort should send pi's `abort` RPC command when possible. Follow-up prompts are queued and sent only when the worker is between turns; if a turn is actively streaming, the command returns `queued` rather than interrupting the current turn.

Add auditable session events for every operator command request, acceptance, rejection, and application. This makes terminal attach and future pi or Linear operator sessions self-explanatory.

## Alternatives Considered

One alternative is to implement mutating controls directly in `scherzoctl` by finding and killing OS processes or writing to workspace files. That is unsafe because it bypasses the daemon's claims, retry counters, and cleanup logic.

Another alternative is to allow prompt injection at any time by writing to pi stdin concurrently. That can corrupt JSONL ordering and race with the worker's command/response correlation. Queueing prompts through the worker loop is slower but protocol-safe.

A third alternative is to expose mutating controls only through Linear comments. That would be asynchronous and ambiguous, and it would require Scherzo to poll and interpret human text commands before the command model has proven itself. Local authenticated controls are clearer and easier to audit as the first transport.

A fourth alternative is to let each transport implement command semantics independently. That would drift quickly: `/scherzo retry` in Linear might not match `scherzoctl retry`, and safety fixes would have to be duplicated. This plan instead makes the command model and daemon handlers transport-independent.

## Risks and Countermeasures

The main safety risk is duplicate dispatch or corrupted scheduler state. Countermeasure: all scheduler mutations go through daemon command handling, and every command rechecks current runtime state before applying. `retry-now` must not dispatch if the issue is running or already claimed. `pause` must affect only new dispatch; reconciliation and shutdown continue.

The main worker risk is corrupting pi RPC command ordering. Countermeasure: worker commands are handled by the worker process that owns the pi session. It sends `abort`, `prompt`, or `extension_ui_response` using the same `pi_rpc.Session` next-id sequence as normal prompts. The control server never writes to the port.

The main UX risk is an operator thinking a prompt interrupted the current turn when it was only queued. Countermeasure: command responses distinguish `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`, and every accepted command publishes a session event with the same status.

The main security risk is a local process abusing the control token. Countermeasure: preserve token authentication, log every mutating command with command type and session/issue id, redact command content where necessary, and require `scherzoctl --yes` for destructive commands such as `abort`, `park`, `cleanup`, and `stop-worker`.

The main UI-request risk is hanging a worker forever while waiting for an operator response. Countermeasure: keep the default `pi.ui_request_policy: cancel`; add `operator` policy only when configured. Operator-handled UI requests have a timeout, after which Scherzo cancels the request and records an event.

The main architecture risk is coupling command behavior to the local TCP protocol. Countermeasure: tests must exercise daemon command handling through the pure command type without going through TCP, and protocol tests must only prove request decoding/encoding maps to the same command type. Future transports, including Linear comments, reuse the same command type and handlers.

Transport-level idempotency is not solved by the command model itself. `scherzoctl` is a synchronous local client and does not need comment-style deduplication. Future Linear comment commands must add their own idempotency layer before submitting commands to the daemon.

## Progress

- [x] (2026-04-28 18:40Z) Confirmed this phase should follow read-only control and terminal attach.
- [x] (2026-04-28 23:05Z) Revised the plan so mutating controls define the shared command API that future Linear and pi-skill transports reuse instead of treating `scherzoctl` as the only command surface.
- [ ] Normalize the tree after the prior visibility phases are complete.
- [ ] Add transport-independent command and command-result types.
- [ ] Extend protocol and `scherzoctl` parsing with mutating commands that initially return controlled not-implemented errors.
- [ ] Add daemon-level scheduler controls for pause, resume, reload, retry-now, park, and unpark.
- [ ] Refactor worker handles to include command subjects and implement abort/stop-after-current-turn.
- [ ] Implement queued operator prompts between turns.
- [ ] Implement operator-managed UI request responses behind an explicit workflow policy.
- [ ] Add audit/session events and update documentation.

## Surprises & Discoveries

- Observation: The current daemon already has placeholder `WorkerCommand` variants and `WorkerHandle.command_subject`, but spawned workers still store `command_subject: None`.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` defines `Abort`, `StopAfterCurrentTurn`, `QueuePrompt`, and `RespondToUi`; worker handle creation currently sets the command subject to `None`.

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

## Outcomes & Retrospective

(To be filled at completion. Include which commands were implemented, which remain intentionally unsupported, and any safety constraints discovered.)

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
- There is no `src/scherzo/control/command.gleam` module yet unless the local-control plan creates it first.

If the current worker implementation is still a synchronous function without a command mailbox, this plan's worker refactor is required before prompt and UI-response controls can work.

## Scope Boundaries

In scope: transport-independent mutating command and result types; mutating control protocol commands; local `scherzoctl` mutating subcommands; daemon command handling; runtime pause/resume overlay; workflow reload request; retry-now; park/unpark; abort worker; stop-after-current-turn; queued follow-up prompts; operator UI request policy and timeout; UI response command; audit events; tests for command authorization, serialization, scheduling safety, worker command behavior, and CLI confirmation.

Out of scope: Linear comment polling or parsing; remote multi-user authorization; role-based access control; web forms; persistent command audit database; direct Linear command comments; mid-turn prompt injection; editing files through the control API; attaching stdin interactively to a worker; distributed scheduler state.

The later Linear command transport must reuse the command model introduced here, but it owns its own polling, authorization, idempotency, edited-comment behavior, and acknowledgement comments.

## Milestones

Milestone 1 defines the shared command model and extends the local protocol safely. At the end, mutating command requests and responses can be encoded/decoded, `scherzoctl` parses the commands, daemon command tests can construct commands without TCP, and the server returns stable `not_implemented` errors until daemon handlers exist.

Milestone 2 adds daemon-level scheduler controls. At the end, pause/resume, reload, retry-now, park, and unpark are serialized through the daemon and covered by tests without touching pi.

Milestone 3 adds worker command channels for abort and stop-after-current-turn. At the end, a running fake worker can be stopped through the daemon, the session records an audit event, and no retry timer dispatches stale duplicate work after abort.

Milestone 4 adds queued operator prompts. At the end, a prompt sent while a worker is idle between turns becomes the next prompt, and a prompt sent during a turn is queued for the following turn with a `queued` response.

Milestone 5 adds operator UI responses. At the end, workflows can opt into `pi.ui_request_policy: operator`, UI requests are recorded as pending session state, `scherzoctl ui respond` can answer or cancel them, and timeout cancels unanswered requests.

Milestone 6 updates documentation and runs acceptance. At the end, README documents mutating commands, confirmation flags, safety constraints, transport-independent command semantics, and examples.

## Plan of Work

Create `src/scherzo/control/command.gleam`. Define command target types, command variants, and response statuses. This module should not import the local TCP protocol, the CLI parser, or Linear. It may import domain types only if necessary for stable identifiers. It should be small enough for future transports to reuse directly.

Extend `src/scherzo/control/protocol.gleam` with request types for `Pause`, `Resume`, `ReloadWorkflow`, `RetryIssue(issue_id_or_identifier)`, `ParkIssue(issue_id_or_identifier, reason)`, `UnparkIssue(issue_id_or_identifier)`, `AbortSession(session_id)`, `StopAfterCurrentTurn(session_id)`, `PromptSession(session_id, message)`, and `RespondUi(session_id, request_id, response)`. Protocol decoding should return or contain `control/command.OperatorCommand` values rather than duplicating command semantics.

Extend `src/scherzo/ctl.gleam` with subcommands `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`. Destructive commands require `--yes`; otherwise the CLI prints a confirmation error and exits with code 2 rather than prompting in tests. CLI parsing maps to the same command values used by protocol tests.

Modify `src/scherzo/orchestrator/daemon.gleam` to accept an operator command message such as `ApplyOperatorCommand(command.OperatorCommand, process.Subject(command.CommandResult))`, or equivalent specific messages that delegate immediately to one shared handler. Add runtime state outside `domain.RuntimeState` for an operator pause flag. `pause` sets this flag and blocks new dispatch even if workflow config allows dispatch. `resume` clears the flag. Reconciliation, retry bookkeeping, and shutdown still run while paused.

Implement `reload` by reusing the workflow fingerprint/reload path immediately rather than waiting for the next poll tick. It returns `applied` on valid reload and `rejected` with a config/workflow error code on invalid reload, while preserving the existing invalid-reload dispatch-blocking behavior.

Implement `retry-now`, `park`, and `unpark` with pure helper functions where possible. `retry-now` should fetch the current active candidate by id or identifier using the tracker; if running or claimed, return `rejected`. If eligible, dispatch through the normal dispatch helper. `park` inserts a parked entry and releases claim only if the issue is not running. `unpark` removes parked state and counters.

Create or refactor `src/scherzo/agent/worker.gleam`. It should own the pi RPC session and expose a command subject. If the existing runner remains useful for one-shot tests, keep `runner.run_attempt` as a wrapper around the worker loop or keep it for synchronous harness tests while daemon uses the worker actor.

Extend `src/scherzo/agent/pi_rpc.gleam` with explicit command helpers for `abort`, `send_prompt`, and `send_extension_ui_response` that operate on the `Session` and preserve `next_id`. Do not expose the raw port to the daemon or control server.

Add pending UI request tracking to session summaries or a new `session/control_state.gleam` type. A pending UI request stores request id, session id, method, prompt/message excerpt, created time, and deadline. Only `pi.ui_request_policy: operator` creates pending requests; `cancel`, `fail`, and `ignore` keep previous behavior.

Publish audit events to the EventHub for every control request: command received, command applied/queued/rejected, and any worker-level result such as `pi_abort_sent`, `operator_prompt_sent`, or `ui_response_sent`.

Update `README.md` with command examples and safety notes. Mention that this phase creates the command model reused by future transports and that Linear comments are intentionally deferred to `docs/plans/linear-command-transport.md`.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count. Stop if read-only control or attach tests fail.

2. Create `test/control_command_test.gleam`. Add tests that construct every `OperatorCommand` variant and assert `command_name`, target extraction, and JSON-safe status strings for `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`.

3. Implement `src/scherzo/control/command.gleam` until the command tests pass.

4. Add tests to `test/control_protocol_test.gleam` for encoding and decoding each mutating request type and for decoding response statuses `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`. Assert decoded requests map to `control/command.OperatorCommand` values.

5. Extend `src/scherzo/control/protocol.gleam` until the protocol tests pass.

6. Add tests to `test/ctl_test.gleam` for parsing `pause`, `resume`, `reload`, `retry ABC-123`, `park ABC-123 --reason manual`, `unpark ABC-123`, `abort <session> --yes`, `abort <session>` without `--yes`, `prompt <session> "continue"`, and `ui respond <session> ui-1 --cancel`. Assert successful parses produce the same command values as protocol decoding.

7. Extend `src/scherzo/ctl.gleam` parser and usage text until the CLI parser tests pass. Mutating command execution may still call protocol endpoints that return `not_implemented`.

8. Add `test/orchestrator_daemon_control_test.gleam` if it does not already exist. Add `pause_blocks_new_dispatch_but_not_reconciliation_test`: start a daemon with one candidate, send pause, wait for a poll, assert no dispatch, then arrange a running issue reconciliation and assert reconciliation still occurs.

9. Implement daemon `Pause` and `Resume` command handling and control server routing. Run tests.

10. Add `reload_control_applies_valid_reload_and_rejects_invalid_test`: rewrite the workflow to valid and invalid versions, send reload, and assert the command response and dispatch-blocking state match expectations.

11. Implement reload control by reusing existing workflow reload code. Run tests.

12. Add `retry_now_does_not_duplicate_running_issue_test`: with a running issue, send retry-now for that issue and assert response `rejected`; with a non-running eligible issue, assert the normal dispatch helper is used once.

13. Implement retry-now through the daemon and normal dispatch path. Run tests.

14. Add `park_and_unpark_issue_control_test`: park a non-running issue with reason `manual`, assert it appears in runtime parked state and session/audit event output, unpark it, and assert it can dispatch on a later poll.

15. Implement park and unpark controls. Run tests.

16. Create `test/agent_worker_control_test.gleam`. Add `abort_command_stops_fake_pi_worker_test`: start a worker with fake pi that delays, send abort through the worker command subject, assert worker exits with an abort classification and the fake pi transcript contains `abort` if graceful abort was possible.

17. Refactor or create `src/scherzo/agent/worker.gleam` with command subject support. Keep existing runner tests passing by adapting `runner.run_attempt` or preserving the old path for synchronous tests.

18. Modify daemon worker handles to store the worker command subject and route `AbortSession` and `StopAfterCurrentTurn` controls through it. Add daemon tests that control by session id and assert audit events are published.

19. Extend `src/scherzo/agent/pi_rpc.gleam` with `send_abort` if needed. Run `direnv exec . gleam test`.

20. Add `operator_prompt_queued_during_turn_and_sent_next_turn_test` using a fake pi mode that delays within a turn. Send a prompt while busy, assert command response `queued`, then assert the fake transcript contains the operator prompt as the next prompt after current turn end.

21. Implement prompt queueing in the worker loop. If the session exits before the queued prompt is sent, publish `operator_prompt_dropped` with reason `session_exited`.

22. Extend config parsing in `src/scherzo/config.gleam` and domain types in `src/scherzo/domain.gleam` to support `pi.ui_request_policy: operator` and `pi.ui_request_timeout_ms`, if not already represented. Add config tests for default cancel behavior and explicit operator policy.

23. Add fake pi fixture support for UI requests that wait for response before continuing. Add `operator_ui_request_response_test`: worker receives a UI request, session status becomes waiting UI, control response sends cancel or selected value, and worker continues or exits according to fake pi behavior.

24. Implement pending UI request tracking and `RespondUi` command routing through the worker. Add timeout behavior that cancels unanswered requests and publishes an event.

25. Update control server tests to assert wrong token rejects mutating commands just like read-only commands.

26. Update README with mutating command examples and warnings. Include examples using `--yes` for destructive commands and note that prompts are queued between turns. Mention that Linear command comments are a separate future transport over the same command model.

27. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count in Progress.

28. Commit the phase with a message such as `Add authenticated Scherzo operator controls`.

## Testing and Falsifiability

This phase is falsified if a mutating command can bypass token authentication, if a control command mutates state outside the daemon, if `retry-now` can start a duplicate worker for a running or claimed issue, if an operator prompt corrupts pi command ids, if abort leaves a worker handle behind, if UI operator mode hangs forever without timeout, if destructive CLI commands run without explicit `--yes`, or if protocol/CLI/daemon tests implement different command semantics for the same operation.

Add deterministic tests in `test/control_command_test.gleam`, `test/control_protocol_test.gleam`, `test/ctl_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/agent_worker_control_test.gleam`, `test/pi_rpc_test.gleam`, and `test/config_test.gleam`. Use fake tracker clients and `test/fixtures/fake_pi_rpc.sh`. No test may require real Linear or real pi.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests for mutating protocol, command model, and worker commands should fail to compile. After implementation, all tests should pass and no worker or socket test should hang.

## Validation and Acceptance

Manual acceptance after tests with a safe fake-pi workflow:

1. Start daemon mode and dispatch one fake worker.
2. Run `scripts/scherzoctl pause` and verify later polls do not dispatch new issues.
3. Run `scripts/scherzoctl resume` and verify dispatch resumes.
4. Run `scripts/scherzoctl prompt <session-id> "summarize progress"` during a worker turn and verify attach shows the prompt queued and then sent.
5. Run `scripts/scherzoctl abort <session-id> --yes` and verify the worker exits, attach shows an audit event, and no orphan fake-pi process remains.
6. With `pi.ui_request_policy: operator`, run a fake UI request and answer it with `scripts/scherzoctl ui respond <session-id> ui-1 --cancel`; verify attach shows the request and response.

Do not accept this phase if any mutating command is available without the control token, if the control server writes directly to a pi port, or if a command works only through `scherzoctl` and cannot be invoked through the shared daemon command handler.

## Rollout, Recovery, and Idempotence

Mutating controls are additive but operationally sensitive. If a command handler fails, it should return `rejected` or `not_allowed` and publish an audit event rather than crashing the daemon. If a worker does not acknowledge a command before a timeout, the daemon should report `rejected` or fall back to existing stop/kill behavior for abort.

Pause/resume is runtime-only and resets on daemon restart unless a future plan persists it. Queued prompts are runtime-only and are dropped if the worker exits. UI request pending state is runtime-only and resolves by response, timeout, or worker exit.

Read-only operations remain available if operators avoid mutating commands. If this phase causes problems in production, operators can continue using `scherzoctl ps`, `scherzoctl events`, and `scherzoctl attach` while mutating command usage is disabled operationally.

Command execution is not globally idempotent. Local `scherzoctl` commands are explicit synchronous operator actions. Future asynchronous transports such as Linear comments must add their own deduplication before submitting commands to the daemon.

## Artifacts and Notes

Example command responses:

    {"ok":true,"data":{"status":"applied","command":"pause"}}
    {"ok":true,"data":{"status":"queued","command":"prompt","session_id":"ABC-123-..."}}
    {"ok":false,"error":{"code":"rejected","message":"session is not running"}}

Every command should produce an audit event similar to:

    kind=lifecycle message=operator_command command=abort status=applied session_id=ABC-123-...

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

No new package dependency should be required. Use existing actor/process modules and extend the existing control TCP protocol.
