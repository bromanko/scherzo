# LIV-688 Remote Command Routing and Result Streaming

This review document frames the implementation ExecPlan for adding the smallest safe server-originated pause/resume control slice on top of Scherzo's outbound `ui_server` daemon connection. The mechanical implementation bundle is supplied separately through the structured implementation-pack submission.

## Purpose / Big Picture

After this work, a development harness or future UI/server can send `pause` and `resume` commands over the daemon's outbound `ui_server` socket, observe a receipt and a correlated result for each command id, and see live state snapshots prove dispatch changed from running to paused and back. The feature is intentionally limited to remote pause/resume so operators get a demonstrable central-control path while local `scherzoctl` remains the fallback.

## Problem Framing and Constraints

LIV-688 must not implement the stale LIV-496 bundle because that bundle assumed files and APIs before LIV-686 landed. I re-checked the current tree: `main` includes `Implement LIV-686: Create ExecPlan: daemon outbound remote client lifecycle (#343)`, `src/scherzo/control/remote/client.gleam` exists, `src/scherzo/orchestrator/daemon_remote_client.gleam` wires the outbound client when `ui_server.enabled` is true, and `test/orchestrator_daemon_remote_client_test.gleam` covers that lifecycle.

The current envelope layer in `src/scherzo/control/remote_envelope.gleam` already encodes and decodes hello, heartbeat, server command, command receipt, command result, and state snapshot envelopes. The snapshot currently carries `now_ms` and sessions only, so the implementation needs one small dispatch-state field to prove pause/resume behavior. The current client sends hello, heartbeat, and state snapshots but has no inbound receive loop; its extension seam is the client actor plus `Dependencies`, not a finished command router.

Local operator mutation remains canonical through `src/scherzo/orchestrator/daemon.gleam` `ApplyOperatorCommand` and `apply_operator_command`. Pause/resume semantics are present in both the older `src/scherzo/orchestrator/control_command_handler.gleam` helper and the current transition path in `src/scherzo/orchestrator/transitions/operator.gleam`. `remote_commands` and `linear_commands` config are removed in `src/scherzo/config.gleam` and tested in `test/linear_command_config_test.gleam`; this plan stays on the `ui_server` outbound path only.

## Strategy Overview

The implementation should add a focused in-memory `src/scherzo/control/remote_command_router.gleam` that accepts only `PauseDispatch` and `ResumeDispatch`, registers each command id before daemon mutation, and remembers in-flight and completed ids for idempotency. The outbound client should gain a non-blocking inbound reader and asynchronous command application so socket reads and daemon mutation cannot starve heartbeat or state streaming.

The daemon integration should pass a remote-control callback that calls the existing `daemon.apply_operator_command` path rather than reimplementing pause/resume. After each applied command result, the client should emit an immediate state snapshot containing the updated dispatch-paused value. The live harness should be extended to send real socket `server_command` envelopes and capture live `command_receipt`, `command_result`, and post-command state evidence with command ids, run nonce, bound endpoint, and redacted auth.

Review feedback is treated as part of the design contract. Automated acceptance evidence and the live loopback harness transcript are pre-publish requirements. Browser UI checks and broader human dogfood are deferred operator checks after implementation because browser UI is explicitly out of scope for this slice. Documentation work is limited to operator-facing guidance for local `scherzoctl` versus outbound `ui_server` control; workflow helper/provider migrations, provider-live/cache behavior, and token-accounting behavior are inventory-only non-goals.

## Alternatives Considered

One alternative is to route remote commands directly to `control_command_handler.apply`. That is rejected because it bypasses the daemon's canonical `ApplyOperatorCommand` message path and risks diverging from local `scherzoctl` behavior.

A second alternative is to add durable central command storage now. That is too large for this slice; exact duplicate handling can be safely in-memory for one daemon/client lifetime, while durable command history can be split into a later ticket.

A third alternative is to revive tracker/comment `remote_commands`. That path is explicitly removed and would reintroduce stale transport semantics; the correct path here is the disabled-by-default outbound `ui_server` connection.

## Risks and Countermeasures

The main correctness risk is applying the same command twice. The countermeasure is router-first registration, tests for completed duplicates, in-flight duplicates, and conflicting duplicate ids, and live harness evidence that a repeated command id yields one daemon mutation.

The main liveness risk is a blocking socket read or daemon command call stopping heartbeat/state emission. The countermeasure is a separate inbound reader and asynchronous apply worker, with tests proving heartbeat and state continue while no inbound line is available and while a command is in flight.

The main scope risk is accidentally touching provider-live/cache behavior, token accounting, workflow helpers, browser UI, docs/helper migration paths, or tracker/comment command behavior. The countermeasure is a pre-publish diff inventory and full validation gates: `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. The diff inventory must explicitly say whether `.scherzo/workflows/`, workflow helper/provider contracts, provider live/cache modules, token-accounting modules, browser UI files, and tracker/comment command config changed; the acceptable answer for this ticket is no, except for operator-facing docs that explain the new remote-control path.

## Scope Boundaries

In scope are the remote pause/resume router, inbound server-command receive loop, receipt/result streaming, command-id correlation, dispatch-paused state snapshots, daemon callback wiring through `ApplyOperatorCommand`, live command harness evidence, and operator-facing documentation for local versus outbound control.

Out of scope are browser UI, durable central command storage, commands other than pause/resume, tracker/comment `remote_commands`, provider-live/cache behavior, token accounting, workflow helper/provider contracts, workflow helper migration, and any change that makes `ui_server` enabled by default. The implementation may add or update an operator-facing runbook, but it must not migrate workflow helpers, alter structured-output provider contracts, or change provider-live/cache semantics as part of this ticket.

## Milestones

Milestone 0 is a stop/go verification milestone. The implementer verifies LIV-686 is present on current `main`, confirms the current remote client API, confirms envelope and harness capabilities, confirms `ApplyOperatorCommand` still exists, and stops for plan refresh if any fact differs.

Milestone 1 delivers the pure command router. The outcome is a tested router that accepts pause/resume only, records command ids before mutation, reuses completed duplicate results, suppresses in-flight duplicate mutation, and rejects conflicting duplicate ids before mutation. The targeted proof is `direnv exec . gleam test test/remote_command_router_test.gleam` after adding cases for accepted pause/resume, unsupported commands, completed duplicates, in-flight duplicates, and conflicting duplicate ids.

Milestone 2 delivers protocol and client streaming. The outcome is a bidirectional outbound client that receives `RemoteServerCommand`, emits receipt before result, correlates both by `command_id`, keeps heartbeat/state streaming while reads and command application are pending, and emits post-command state. The targeted proof is `direnv exec . gleam test test/control_remote_client_test.gleam test/control_remote_envelope_test.gleam` with red-phase tests for receipt/result ordering, command-id correlation, non-blocking inbound reads, apply-worker liveness, invalid envelope rejection, and dispatch-paused snapshot encoding.

Milestone 3 delivers daemon integration. The outcome is remote pause/resume wired through `daemon.apply_operator_command`, with state snapshots proving dispatch paused/resumed and local `scherzoctl` still available when remote control is unreachable or disabled. The targeted proof is `direnv exec . gleam test test/orchestrator_daemon_remote_client_test.gleam` with fake remote-control callbacks showing one canonical daemon mutation per accepted command id.

Milestone 4 delivers live harness and docs evidence. The outcome is a real loopback-socket transcript containing hello, heartbeat, initial state, pause/resume server commands, receipts, applied results, post-command state, matching command ids, run nonce, bound endpoint, and redacted auth, plus docs explaining the fallback and non-goals. This milestone also produces the required scope inventory showing no workflow helper/provider, provider-live/cache, token-accounting, browser UI, or tracker/comment command changes. The targeted proof is `direnv exec . gleam test test/remote_harness_test.gleam` and the pre-publish harness command named in Validation and Acceptance.

Milestone 5 delivers acceptance validation. The outcome is all targeted tests, live harness validation, scope inventory, format, glinter, custom lint, full `gleam test`, and review-doc validation passing before publish. Any manual browser or broader UI dogfood remains a deferred human/operator check after implementation, not a pre-publish blocker for this browser-free slice.

## Progress

- [x] (2026-05-28) Verified the prepared review document target is `docs/plans`.
- [x] (2026-05-28) Re-checked the current remote client, envelope, harness, daemon operator path, pause/resume handler semantics, and removed tracker/comment command config.
- [x] (2026-05-28) Authored this concise review document and prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-28) Incorporated review feedback by making acceptance evidence, targeted tests, milestone proofs, pre-publish live harness validation, deferred manual/browser dogfood, docs/helper boundaries, provider-live/cache non-goals, full validation gates, and lint obligations explicit.

## Decision Log

- Decision: Regenerate LIV-688 as a fresh plan instead of reusing LIV-496. Rationale: the stale bundle was written against pre-LIV-686 assumptions and failed because the current remote client API did not exist then. Date: 2026-05-28.
- Decision: Limit the remote command slice to pause/resume and in-memory command-id idempotency. Rationale: it is the smallest observable central-control behavior and avoids durable command storage or broad operator-command scope. Date: 2026-05-28.
- Decision: Require daemon mutation through `ApplyOperatorCommand`. Rationale: remote control must preserve local control semantics and avoid a parallel pause/resume implementation. Date: 2026-05-28.
- Decision: Treat real loopback harness evidence as a pre-publish requirement, while deferring browser/manual dogfood to post-implementation human or operator checks. Rationale: the accepted slice has no browser UI, but it still needs observable socket evidence before publication. Date: 2026-05-28.
- Decision: Keep docs work operator-facing and make workflow helper/provider, provider-live/cache, and token-accounting paths inventory-only non-goals. Rationale: review feedback requires explicit scope containment so this command-routing plan cannot become an accidental helper or provider migration. Date: 2026-05-28.

## Validation and Acceptance

The implementation is acceptable only when automated tests cover router idempotency, in-flight duplicate suppression, duplicate-conflict rejection, unsupported command rejection, receipt-before-result ordering, command-id correlation, daemon pause/resume application, non-blocking inbound receive behavior, and state snapshot proof. Evidence must include named passing tests under `test/remote_command_router_test.gleam`, `test/control_remote_client_test.gleam`, `test/control_remote_envelope_test.gleam`, `test/orchestrator_daemon_remote_client_test.gleam`, and `test/remote_harness_test.gleam`, with the red phase observed before the corresponding implementation where a test covers new behavior.

Pre-publish live evidence must come from real socket traffic, not fixtures: run the extended harness with `direnv exec . gleam run -m scherzo/control/remote_harness -- command-demo --token test-token --transcript test/tmp/remote-command-transcript.json` and inspect the transcript for hello, heartbeat, initial state, pause and resume server commands, command receipts, command results with `status=applied`, post-command state showing paused then resumed dispatch, matching command ids, run nonce, bound port, and `[REDACTED]` auth with no raw token. A repeated command id in the live or integration evidence must show one daemon mutation and either a cached completed result or an in-flight duplicate response, not a second application.

Full validation requires `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-688-remote-command-routing-result-streaming.md`. The implementation must also include a diff/scope inventory artifact showing no workflow helper/provider contracts, provider-live/cache behavior, token-accounting behavior, browser UI, or tracker/comment command behavior changed unless a separate ticket explicitly owns that split. Browser or full UI dogfood is not a pre-publish requirement for this plan; it is a deferred human/operator check after implementation if a future UI/server is available.

## Rollout, Recovery, and Idempotence

Rollout stays opt-in because `ui_server.enabled` remains false by default. Operators can continue using local `scherzoctl`; if the server path is unavailable, disabling `ui_server` returns the daemon to local-only behavior.

Recovery is to stop the remote harness or UI/server, disable `ui_server`, and rely on local control while backing out the remote client receive/router changes if needed. In-memory command-id state is safe to rebuild on reconnect or daemon restart: completed duplicates reuse cached results during the current lifetime, in-flight duplicates do not mutate twice, and conflicting duplicates are rejected before mutation.

## Open Questions and Clarifications Needed

No open questions. Durable command history, broader operator commands, and browser UI should be separate follow-up decisions after this pause/resume slice is validated.
