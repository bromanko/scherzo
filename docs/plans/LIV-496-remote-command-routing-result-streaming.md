# Remote command routing and result streaming

This ExecPlan review document is the human review surface for LIV-496. Scherzo will materialize the mechanical implementation pack from the structured submission; this Markdown file intentionally stays concise and review-focused.

## Purpose / Big Picture

This plan defines the smallest live remote-control slice that lets a server or development harness send a `pause` or `resume` command to a running daemon, receive a correlated result, and observe the daemon state change over the real outbound daemon-to-harness path. After implementation, a reviewer can inspect a live transcript showing hello, heartbeat, state snapshot, command dispatch, a `command_received` or command-receipt event, `command_result` with `status=applied`, a later state snapshot, and no enrollment-token leakage.

The operator value is safe central-control semantics before any browser UI: local `scherzoctl`, tracker/comment remote-command transports, and UI/server daemon commands can share `OperatorCommand` and `CommandResult` meanings without sharing transport credentials or delivery assumptions.

For LIV-496 itself, the deliverable is this review document plus the structured implementation-pack submission that Scherzo will materialize into follow-up implementation artifacts. Review feedback about acceptance evidence, exact test obligations, milestone proof anchors, manual/dogfood timing, docs/helper boundaries, provider-live/cache non-scope, full validation, and lint gates must therefore be mirrored in both places rather than living only in prose.

## Problem Framing and Constraints

This is the final small plan in the replacement chain for the aborted LIV-483 monolith. Implementation is blocked until the daemon outbound client lifecycle ticket, LIV-495, has landed a real supervised daemon client and live harness path; if that client is absent, the follow-up implementation must stop rather than rebuilding lifecycle in this slice.

The slice must route server-originated pause/resume through the existing canonical daemon command path, preserve command idempotency, stream only minimal state/result events, and prove behavior from live traffic rather than fixtures. Explicit non-goals are browser UI, production multi-daemon dogfood before publish, and durable central server storage beyond an in-memory command-id cache needed for this live control slice.

## Strategy Overview

Reuse the existing command model instead of inventing a remote-only mutation path. The live remote client should decode a server command id plus `OperatorCommand`, record the command id before mutation, emit the received/accepted command receipt event, invoke the daemon's existing `ApplyOperatorCommand`/command-handler path, and emit `command_result` with the same command id.

Add an in-memory per-daemon-client command-id registry. A first command id starts execution; an exact duplicate returns the cached completed result or joins the in-flight execution without applying the command twice; a duplicate id with a different command is rejected before daemon mutation. The remote state stream remains minimal: emit a snapshot after hello and after applied pause/resume changes, including enough dispatch state to prove the command changed the daemon.

## Alternatives Considered

One alternative is to leave remote pause/resume as a harness-only fake. That is rejected because acceptance requires the real daemon/harness path and the canonical daemon command handler.

A second alternative is to accept at-least-once command delivery and rely on pause/resume being mostly harmless. That is rejected because the same command-id rules will later protect less benign operator commands, and conflict behavior must be established before broader command support.

A third alternative is to add a durable server-side command store now. That is too large for this slice; an in-memory daemon/client cache is enough to prove duplicate and conflict semantics on one live connection.

A fourth alternative is to build browser UI or production multi-daemon dogfood first. That would repeat the LIV-483 scope problem and is explicitly deferred.

## Risks and Countermeasures

The main mutation risk is bypassing existing daemon semantics. The countermeasure is to route through `OperatorCommand` and `CommandResult` and to test pause/resume via the same daemon command-handler path used by local control.

The main delivery risk is double mutation on retries. The countermeasure is command-id registration before daemon mutation, tests for completed and in-flight exact duplicates, and a conflicting-duplicate test that verifies the daemon mutation counter is unchanged.

The main observability risk is accepting a canned transcript. The countermeasure is a pre-publish live transcript from the real daemon outbound client connected to the harness, with run nonce, nonzero bound port or live endpoint evidence, command id correlation, ordered events, and token redaction.

The main security risk is confusing local and remote credentials. The countermeasure is documentation that separates local `scherzoctl`, tracker/comment remote commands, and UI/server daemon commands, plus transcript and log assertions that enrollment tokens never appear in captured evidence.

A plan/pack drift risk is that reviewers accept this document while the structured implementation pack omits acceptance evidence, negative-path tests, the pre-publish live transcript, docs/helper inventory, provider-live/cache boundaries, or full validation and lint obligations. The countermeasure is to keep those obligations explicit here, mirror them in the pack concrete steps and testing notes, and re-run review-document validation after revisions.

## Scope Boundaries

In scope are server/harness-originated pause and resume, server command ids, a `command_received` or command-receipt event before `command_result`, correlated result streaming, exact duplicate idempotency, conflicting duplicate rejection before mutation, state snapshots after hello and after pause/resume, live transcript evidence, operator-facing docs for transport boundaries, an explicit docs/helper boundary inventory, and the standard Gleam validation gates.

In scope for the LIV-496 planning handoff is exactly this Markdown review document and one structured implementation-pack submission. This ticket should not manually write an `exec_plan_bundle`, canonical implementation-pack JSON, production source code, tests, or helper migrations outside this document.

Out of scope are browser UI, durable central command storage, production multi-daemon dogfood, broad operator-command coverage beyond pause/resume, new tracker/comment command delivery behavior, provider-live/cache changes, token-accounting changes, workflow helper/provider contract rewrites, and replacing local `scherzoctl`. The expected documentation work is limited to operator-facing wording such as `docs/ARCHITECTURE.md` and `docs/GETTING_STARTED.md`; if implementation unexpectedly needs `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache behavior changes, that work must be split or explicitly rolled back before publish.

## Milestones

Milestone 0 verifies blockers and current seams before command-routing code begins. Reviewers should see evidence that LIV-495 has landed a real `src/scherzo/control/remote/client.gleam` outbound client and live harness path, that `src/scherzo/control/remote_envelope.gleam` still exposes server command and command result envelopes, and that daemon pause/resume still flows through `src/scherzo/control/command.gleam` and `src/scherzo/orchestrator/daemon.gleam` using `OperatorCommand` and `CommandResult`. If the LIV-495 client is absent, this implementation stops and reports the blocker rather than rebuilding lifecycle here.

Milestone 1 delivers command-id semantics before daemon wiring in a focused router module, expected as `src/scherzo/control/remote_command_router.gleam` with tests in `test/control_remote_command_router_test.gleam`. Reviewers should see tests proving the received/accepted command receipt event is emitted before any result, a new pause/resume executes once, a completed exact duplicate returns the cached result without mutation, an in-flight exact duplicate joins the original execution without mutation, and a conflicting duplicate id is rejected with the mutation callback count still zero.

Milestone 2 wires the live daemon remote client to the canonical command handler. Reviewers should see `src/scherzo/control/remote/client.gleam` decode a `RemoteServerCommand`, call the router, apply only pause/resume through the existing daemon `ApplyOperatorCommand` path in `src/scherzo/orchestrator/daemon.gleam`, return a correlated `RemoteCommandResult` with `status=applied`, and emit state snapshots after hello and after applied pause and resume changes.

Milestone 3 proves behavior on the real daemon/harness path before publish. Reviewers should see a live transcript, not a fixture, generated from the real daemon outbound client connected to the live harness, showing hello, heartbeat, initial state snapshot, command dispatch, a `command_received` or command-receipt event, `command_result`, post-command state snapshot, matching command ids, live-run provenance such as a run nonce and nonzero bound endpoint, and redacted auth.

Milestone 4 completes docs, helper-boundary inventory, scope audit, and gates. Reviewers should see operator-facing updates to `docs/ARCHITECTURE.md` and `docs/GETTING_STARTED.md` or an explicit note if either file no longer contains the relevant wording; an inventory confirming no workflow helper scripts, provider-facing structured-output contracts, provider-live probes, cache behavior, or token accounting changed; passing test/format/glinter/Scherzo-lint gates; and a final review that confirms the slice did not add browser UI, durable server storage, production dogfood requirements, provider-live/cache changes, or token-accounting changes.

## Progress

- [x] (2026-05-26) Confirmed the prepared review document target is `docs/plans/`.
- [x] (2026-05-26) Reviewed the current command codec, remote envelope, live harness, daemon command handler, daemon command entry point, `ui_server` docs, and LIV-493 through LIV-495 planning context.
- [x] (2026-05-26) Authored this concise human-reviewable review document and prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-27) Incorporated review feedback by making acceptance evidence, exact test obligations, milestone proof anchors, pre-publish live transcript evidence, deferred production/browser dogfood status, docs/helper inventory, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack submission.
- [x] (2026-05-27) Revalidated the revised review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-496-remote-command-routing-result-streaming.md` and received `REVIEW_DOC_VALID=ok`.

## Decision Log

- Decision: Treat LIV-495 as a hard implementation blocker.
  Rationale: This slice depends on a real daemon outbound client lifecycle and must not hide lifecycle work inside command routing.
  Date: 2026-05-26

- Decision: Limit live remote mutation to pause/resume.
  Rationale: Pause/resume is enough to prove routing, idempotency, ordering, result correlation, and state streaming while keeping blast radius small.
  Date: 2026-05-26

- Decision: Use command-id registration before daemon mutation with cached/completed and in-flight duplicate handling.
  Rationale: Retried server delivery must not apply a command twice, and conflicting duplicate ids must be rejected before state changes.
  Date: 2026-05-26

- Decision: Require live transcript evidence before publish and defer browser or production multi-daemon dogfood.
  Rationale: The acceptance risk is false evidence on the real daemon/harness path, not browser polish or production fleet behavior.
  Date: 2026-05-26

- Decision: Treat acceptance evidence, test obligations, manual/dogfood timing, docs/helper inventory, provider-live/cache boundaries, full validation, and linting as obligations in both this review document and the structured implementation pack.
  Rationale: Scherzo materializes follow-up implementation work from the pack, so prose-only requirements would be easy for later implementers to miss.
  Date: 2026-05-27

## Validation and Acceptance

Planning acceptance for LIV-496 requires this file to remain at `docs/plans/LIV-496-remote-command-routing-result-streaming.md`, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-496-remote-command-routing-result-streaming.md` to exit zero with `REVIEW_DOC_VALID=ok`, and Scherzo to capture the updated structured implementation-pack submission. The planning handoff must not include source/test implementation changes or manually written canonical bundle JSON.

Follow-up implementation acceptance requires automated tests for all command-routing behaviors. Tests must prove successful remote pause and resume; exact completed-duplicate idempotency; exact in-flight duplicate idempotency; conflicting duplicate rejection before daemon mutation; a `command_received` or command-receipt event before `command_result`; result `command_id` correlation; an initial state snapshot after hello; a state snapshot after applied pause; and a state snapshot after applied resume. The duplicate tests must prove the daemon mutation callback or command-handler invocation count remains one for exact duplicates and zero for conflicting duplicates.

The pre-publish manual/dogfood requirement for the follow-up implementation is the live daemon-to-harness transcript, not a browser UI check or production multi-daemon dogfood. The transcript must be generated from the real daemon outbound client connected to the live harness, not a fixture, and must show hello, heartbeat, state snapshot, command dispatch or server send, a `command_received` or command-receipt event, `command_result` with `status=applied`, post-command state snapshot, matching command ids, live-run provenance such as a run nonce and bound endpoint, and no raw enrollment token. Browser UI and production multi-daemon dogfood checks are deferred human/operator checks after implementation and do not block publish.

Docs/helper evidence must include operator-facing text, expected in `docs/ARCHITECTURE.md` and `docs/GETTING_STARTED.md`, that distinguishes local `scherzoctl`, tracker/comment remote commands, and UI/server daemon commands as separate transports sharing command semantics. It must also include a helper-boundary inventory: if `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache behavior are unchanged, the acceptance note says so; if any of those surfaces change, the work must either split into a separate ticket or include the relevant helper/contract tests and, for provider-live/cache changes, stale-read, invalidation, and TTL-disabling evidence.

Full validation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If any gate fails for a pre-existing unrelated reason, the implementer must capture the exact output and explain why the new remote command-routing work is not the cause before handoff; otherwise all gates must pass.

## Rollout, Recovery, and Idempotence

Rollout is opt-in through the existing disabled-by-default `ui_server` path and development harness. Local `scherzoctl` remains the fallback operator control surface, and tracker/comment command behavior is documentation-only in this slice unless already provided by another ticket.

Recovery is to disable `ui_server`, stop the remote client/harness, and continue using local control. If command-routing changes regress daemon behavior, back out the remote client routing and command-id registry while preserving the existing local command handler.

The remote command-id registry is intentionally in-memory and idempotent for repeated delivery during a daemon/client lifetime: exact duplicates reuse the original execution or result, conflicts are rejected without mutation, and restarting the daemon clears only this transient cache without changing durable workflow state, provider-live/cache behavior, workflow helper contracts, or local control files.

If implementation discovers it must alter workflow helpers, provider-facing structured-output contracts, provider-live probes, cache behavior, or token accounting, that work should be split or explicitly rolled back before publishing the command-routing slice. The safe default is no helper migration beyond operator docs and no provider-live/cache behavior change.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this planning slice. Production authorization policy, durable server-side command history, browser UI, multi-daemon fleet behavior, and broad remote support for non-pause/resume commands remain deferred to later work.
