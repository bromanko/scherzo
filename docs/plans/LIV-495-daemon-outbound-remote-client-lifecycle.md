# Daemon outbound remote client lifecycle

## Purpose / Big Picture

This plan defines the first real daemon-initiated outbound remote lifecycle for Scherzo. When `ui_server.enabled` is true, the daemon should start a supervised remote client, connect to the live harness or server, send hello, heartbeat, and minimal state evidence using a stable `daemon_id` and fresh `boot_id`, retry unreachable servers with bounded backoff, and stop the client during daemon shutdown while local `scherzoctl` control continues to work.

For LIV-495 itself, the human-reviewable deliverable is this review document plus the structured implementation-pack submission that Scherzo will materialize into follow-up implementation artifacts. Review feedback about evidence, tests, docs/helper boundaries, provider-live/cache non-scope, and validation gates must therefore be mirrored in both places rather than living only in prose.

## Problem Framing and Constraints

LIV-483 failed because remote-client helpers were not integrated with daemon startup or shutdown. This work is blocked by the live harness and liveness registry from LIV-494, and implementation must first verify those artifacts are present and passing rather than accepting a stub transcript as evidence.

The exact daemon seam is `src/scherzo/orchestrator/daemon.gleam`: `start/2` loads the runtime bundle, starts the event hub, and the actor initializer starts the local control plane and effect runner before constructing `State`; `shutdown_runtime_shell` tears down daemon-owned runtime resources. The remote client should be started at that initializer seam after local control and the effect runner have succeeded, stored in `State`, monitored, and stopped from `shutdown_runtime_shell` before the event hub is stopped.

## Strategy Overview

Create or replace `src/scherzo/control/remote/client.gleam` with an actor-like outbound client that owns connection state, heartbeat/state emission, retry timers, and shutdown. Daemon integration should be thin: disabled config produces no client, enabled config loads `src/scherzo/daemon_identity.gleam`, builds settings from `EffectiveConfig.ui_server`, starts the client asynchronously, and logs lifecycle state through the existing secret-redaction path.

The client should use the existing remote foundation: authenticated hello from `src/scherzo/control/remote_harness_hello.gleam`, heartbeat and minimal state envelopes from `src/scherzo/control/remote_envelope.gleam`, current session summaries from the event hub, and live transcript/liveness evidence from `src/scherzo/control/remote_harness.gleam`. Reconnects must be bounded and timer-driven so an unreachable server cannot block polling, local control, or daemon startup.

## Alternatives Considered

Leaving the module as helpers only is rejected because it repeats the LIV-483 failure mode.

Failing daemon startup whenever the remote server is unreachable is rejected because central UI availability must not break local Scherzo operation.

Implementing server-originated pause/resume, idempotent command execution, browser UI, or full event fan-out now is rejected because lifecycle proof is the next risk and command mutation needs a separate safety slice.

Reusing local `control.json` or `scherzoctl` tokens is rejected because local loopback control and outbound UI/server enrollment are different trust boundaries.

## Risks and Countermeasures

A false-positive lifecycle proof is the main evidence risk. The countermeasure is a live harness transcript with run nonce, bound port, matching send/receive digests, redacted auth, and observed daemon hello/heartbeat/state messages.

A retry storm or blocked daemon is the main availability risk. The countermeasure is injected timers, bounded exponential backoff, tests that drive retries deterministically, and a local-control test proving `scherzoctl` still works while the server is unreachable.

Secret leakage is the main security risk. The countermeasure is to pass `config.resolved_secrets` to remote lifecycle logging and to test that enrollment tokens do not appear in logs or transcripts.

Flaky tests are a delivery risk. The countermeasure is to use injected clocks/timers, `test/test_async.gleam` barriers, and harness subjects instead of ad hoc sleeps wherever possible.

A plan/pack drift risk is that reviewers accept this document while the structured implementation pack omits the acceptance evidence, manual/dogfood timing, docs/helper inventory, provider-live/cache boundary, full validation, or lint obligations. The countermeasure is to keep those obligations explicit in this document, mirror them in the pack concrete steps and testing notes, and re-run review-document validation after revisions.

## Scope Boundaries

In scope for the follow-up implementation described by this plan are startup gating on disabled-by-default `ui_server` config, daemon-owned remote client start/monitor/stop, stable `daemon_id` and fresh `boot_id` use, hello/heartbeat/minimal state emission, bounded reconnect, secret-safe connection logging, live harness evidence, documentation that the previous inert config now has lifecycle behavior when enabled, and tests for disabled, enabled, unreachable, local-control, and shutdown behavior.

In scope for the LIV-495 planning handoff is exactly this Markdown review document and one structured implementation-pack submission. This ticket should not manually write an `exec_plan_bundle`, canonical implementation-pack JSON, production source code, tests, or helper migrations outside this document.

Out of scope are server-originated pause/resume mutation, idempotent server command execution, browser UI, durable server storage, full event fan-out, replacing local `scherzoctl`, pi provider-live/cache changes, token-accounting changes, and workflow helper/provider contract rewrites. The expected documentation work is limited to operator-facing wording such as `docs/ARCHITECTURE.md` and `docs/GETTING_STARTED.md`; if implementation unexpectedly needs `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, provider-live probes, or cache behavior changes, that work must be split or explicitly rolled back before publish.

## Milestones

Milestone 0 verifies blockers and current seams before any lifecycle code is designed. Reviewers should see evidence that `src/scherzo/control/remote_harness.gleam`, `src/scherzo/control/remote_liveness.gleam`, `src/scherzo/control/remote_harness_hello.gleam`, `src/scherzo/control/remote_envelope.gleam`, and `src/scherzo/daemon_identity.gleam` exist and that the daemon integration point is still `start/2` plus `shutdown_runtime_shell` in `src/scherzo/orchestrator/daemon.gleam`. This milestone is accepted only after the existing remote-harness, liveness, identity, config, and daemon tests still pass under `direnv exec . gleam test`.

Milestone 1 delivers the outbound client lifecycle module in `src/scherzo/control/remote/client.gleam` with focused tests in a new `test/control_remote_client_test.gleam`. Reviewers should see a fake transport and injected timer/clock seam proving authenticated hello, heartbeat, one minimal state snapshot, bounded retry after connection failure, shutdown timer cancellation, and secret-redacted lifecycle logs without ad hoc sleeps.

Milestone 2 wires the daemon lifecycle from `src/scherzo/orchestrator/daemon.gleam` with focused daemon tests in a new or existing daemon test module such as `test/orchestrator_daemon_remote_client_test.gleam`. Reviewers should see disabled config starting no client, enabled config loading `src/scherzo/daemon_identity.gleam` and starting exactly one monitored client, unreachable endpoints retrying without blocking daemon snapshots or local `scherzoctl` ping, and shutdown stopping the client before the event hub is stopped.

Milestone 3 proves the lifecycle against the live harness rather than a fixture. Reviewers should see `src/scherzo/control/remote_harness.gleam` or its CLI entry point extended only as needed to let the real daemon client connect to a live loopback harness, with transcript evidence showing a run nonce, nonzero bound port, matching send/receive digests for hello, heartbeat, and state, liveness observations, and auth redaction. A static fixture transcript is insufficient.

Milestone 4 completes docs, helper-boundary inventory, scope audit, and gates. Reviewers should see operator-facing updates to `docs/ARCHITECTURE.md` and `docs/GETTING_STARTED.md` or an explicit note if either file no longer contains the relevant wording; a docs/helper inventory confirming no workflow helper scripts, provider-facing structured-output contracts, provider-live probes, cache behavior, or token accounting changed; passing Gleam test/format/glinter/Scherzo-lint gates; and a scope audit proving command mutation, browser UI, provider-live/cache, and token-accounting behavior stayed out of this slice.

## Progress

- [x] (2026-05-26) Confirmed the prepared review document target is `docs/plans/`.
- [x] (2026-05-26) Reviewed daemon startup/shutdown, `ui_server` config, daemon identity, remote envelope, live harness, and async-test support.
- [x] (2026-05-26) Authored this concise human-reviewable ExecPlan review document and prepared the structured implementation pack for Scherzo capture.
- [x] (2026-05-26) Incorporated review feedback by strengthening acceptance evidence, test obligations, milestone proof anchors, pre-publish live harness evidence, deferred/manual dogfood status, docs/helper inventory, provider-live/cache non-scope, full validation, and linting in this document and the updated structured implementation-pack submission.
- [x] (2026-05-26) Revalidated the revised review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-495-daemon-outbound-remote-client-lifecycle.md` and received `REVIEW_DOC_VALID=ok`.

## Decision Log

- Decision: Use `src/scherzo/control/remote/client.gleam` as the daemon outbound client module and integrate it from `src/scherzo/orchestrator/daemon.gleam` rather than leaving helper-only code.
  Rationale: The task is specifically about daemon lifecycle integration, not another pure helper foundation.
  Date: 2026-05-26

- Decision: Start the remote client asynchronously and treat connection failure as retryable, not as daemon startup failure.
  Rationale: Local daemon operation and `scherzoctl` must remain available when the central server is down.
  Date: 2026-05-26

- Decision: Keep server-originated commands out of this slice.
  Rationale: Lifecycle, liveness, and shutdown can be proven without introducing mutation/idempotency risk.
  Date: 2026-05-26

- Decision: Require live harness transcript evidence before publish.
  Rationale: The previous failure mode included non-live evidence, so acceptance must prove real I/O.
  Date: 2026-05-26

- Decision: Treat acceptance evidence, test obligations, manual/dogfood timing, docs/helper inventory, provider-live/cache boundaries, full validation, and linting as obligations in both this review document and the structured implementation pack.
  Rationale: Scherzo materializes follow-up implementation work from the pack, so prose-only requirements would be easy for later implementers to miss.
  Date: 2026-05-26

## Validation and Acceptance

Planning acceptance for LIV-495 requires this file to remain at `docs/plans/LIV-495-daemon-outbound-remote-client-lifecycle.md`, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-495-daemon-outbound-remote-client-lifecycle.md` to exit zero with `REVIEW_DOC_VALID=ok`, and Scherzo to capture the updated structured implementation-pack submission. The planning handoff must not include source/test implementation changes or manually written canonical bundle JSON.

Follow-up implementation acceptance requires automated evidence for all lifecycle behaviors. Tests must prove disabled config starts no client; enabled config starts a client and sends hello with stable `daemon_id`, fresh `boot_id`, and configured auth; heartbeat and minimal state snapshot messages are emitted; unreachable servers schedule bounded retries without blocking daemon snapshot/polling or local `scherzoctl` ping; shutdown stops the client and cancels timers; and logs/transcripts do not contain enrollment tokens.

The pre-publish manual/dogfood requirement for the follow-up implementation is the live loopback harness proof, not a browser UI check. The transcript must be generated from live socket/process traffic by the real daemon client, include a run nonce and nonzero bound port, show matching client/server digests for hello, heartbeat, and state, include liveness observations, and redact auth material. A deferred human/operator dogfood check after implementation may connect a disposable daemon to a staging or development server endpoint when one exists, but that deferred check supplements rather than replaces the pre-publish live harness transcript and automated tests.

Docs/helper evidence must include updated operator-facing wording for the previously inert `ui_server` lifecycle behavior and an explicit helper-boundary inventory. If `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache behavior are unchanged, the acceptance note says so. If any of those surfaces change, the work must either split into a separate ticket or include the relevant helper/contract tests and, for provider-live/cache changes, stale-read, invalidation, and TTL-disabling evidence.

Full validation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If any gate fails for a pre-existing unrelated reason, the implementer must capture the exact output and explain why the new remote lifecycle work is not the cause before handoff; otherwise all gates must pass.

## Rollout, Recovery, and Idempotence

Rollout is opt-in because `ui_server.enabled` remains false by default. Removing or disabling the `ui_server` section returns the daemon to local-only behavior without changing local control files, workflow state, provider-live/cache behavior, token accounting, or workflow helper contracts.

Recovery for an unreachable server is automatic bounded retry. Recovery for malformed daemon identity is the existing explicit operator repair path for `workspace.root/.scherzo-state/daemon_identity.json`. Recovery from implementation regressions is to back out the remote client module and daemon wiring; local `scherzoctl` remains the fallback operator surface.

Repeated daemon starts are idempotent for valid identity state: the same `daemon_id` is reused, each process gets a fresh `boot_id`, retry/shutdown tests can be rerun without manual cleanup beyond generated harness transcripts, and no provider cache invalidation or helper migration cleanup is required when the implementation stays within this scope.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this lifecycle plan. Production server API shape, browser UI, server-originated command authorization, command idempotency, enrollment rotation, and full event fan-out remain deferred to later tickets.
