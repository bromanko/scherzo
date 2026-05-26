# Live remote harness and liveness registry

## Purpose / Big Picture

This ExecPlan slice gives Scherzo a trustworthy development harness for the future UI/server control plane before any daemon lifecycle or command routing is wired in. After implementation, a developer can run `scripts/scherzo-ui-control-harness demo --token test-token --transcript test/tmp/remote-harness/transcript.json`, watch a minimal client connect over a real line-delimited loopback socket, authenticate with a daemon hello message, send heartbeats, and inspect a redacted transcript proving the traffic came from live I/O rather than canned JSON.

The operator-visible value is confidence. Later daemon startup, outbound client, browser UI, and pause/resume work will build on a tested liveness registry and a harness that already catches stale boots, bad auth, malformed hello messages, and stale cached-online views.

## Problem Framing and Constraints

A previous live-control attempt failed partly because its harness evidence was a stubbed transcript. This plan frames the next slice around evidence quality, not production control features. The implementation is blocked until the command/envelope foundation ticket and the daemon identity/config foundation ticket are complete and present in the implementation branch; before coding, the implementer must verify the current tree contains `src/scherzo/control/remote_envelope.gleam`, its tests, `src/scherzo/daemon_identity.gleam`, and the disabled-by-default `ui_server` config work. If either foundation is absent, implementation stops rather than re-creating those layers in this ticket.

The repository already has local loopback control and a remote envelope foundation, but this slice must not start the real Scherzo daemon, supervise a daemon outbound client loop, mutate pause/resume state, change pi provider-live/cache behavior, change token accounting, or add a browser UI. The harness may use a development-only script, a minimal test client, and loopback sockets only. Time-sensitive liveness behavior must be tested with controlled timestamps instead of sleeps.

## Strategy Overview

Add a pure liveness registry keyed by stable `daemon_id` with one active `boot_id`, then use it from a development-only harness server. The registry computes online, stale, and offline status from caller-provided `now_ms`, so querying later cannot reuse a cached online answer. Registering a different boot for the same daemon replaces the active boot, and later heartbeats from the old boot are rejected or ignored without moving `last_seen_at_ms`.

Add `scripts/scherzo-ui-control-harness` and focused Gleam modules under `src/scherzo/control/` for harness-only hello validation, registry updates, transcript event collection, and demo execution. The script starts a loopback line-socket server, pairs it with a minimal client, exchanges remote protocol JSON lines, updates the registry from accepted hello and heartbeat messages, and writes a redacted transcript from observed send/receive events. The transcript must include run-unique evidence such as the actual bound port, a generated run nonce, connection sequence numbers, matching client-sent/server-received line digests, and liveness observations.

## Alternatives Considered

Keeping a canned transcript is rejected because it would repeat the failure mode this ticket exists to prevent.

Waiting for the real daemon outbound loop is rejected because it would combine transport, identity, lifecycle, and command routing before the harness and registry are independently testable.

Building a browser UI first is rejected because a scriptable harness can prove the live socket and liveness semantics with much lower blast radius.

Reusing local `control.json` discovery or the `scherzoctl` token as remote identity/auth is rejected because local loopback control is a different trust boundary from a daemon-to-server UI/control channel.

## Risks and Countermeasures

The main evidence risk is another false green transcript. The countermeasure is to generate transcripts only from live harness event collection and to test that two demo runs produce distinct run nonces while preserving matching sent/received line evidence and a nonzero bound port.

The main liveness risk is stale cached-online state. The countermeasure is a pure registry API whose view functions take `now_ms` and recompute status on every call, plus tests that query online first and then stale/offline without sending another heartbeat.

The main stale-boot risk is an older process reviving after a newer boot registered. The countermeasure is newer-boot-wins semantics and tests proving old-boot heartbeats are rejected or ignored even if they arrive later.

The main docs/helper migration risk is leaving a root helper or documentation that implies this is a production UI/server path. The countermeasure is to make the helper name, script help, and any touched docs say development-only loopback harness, while deferring browser, real daemon, and operator dogfood checks to later tickets.

The main scope risk is accidentally implementing real daemon control or changing provider behavior. The countermeasure is a pre-publish diff audit showing no daemon startup integration, no supervised outbound client loop, no real pause/resume mutation, no browser UI, no pi provider-live/cache behavior changes, and no token-accounting changes.

## Scope Boundaries

In scope are a controlled-time liveness registry, harness-only remote hello/auth validation, loopback line-socket harness behavior, a minimal test/client path, live transcript generation with redaction and anti-fixture evidence, negative invalid-auth and invalid-hello behavior, automated tests for registry and harness transitions, `scripts/scherzo-ui-control-harness`, and script help or narrowly scoped documentation that labels the helper development-only.

Out of scope are Scherzo daemon startup integration, a supervised daemon outbound client loop, production transport selection, real pause/resume or other operator command mutation, durable server storage, browser UI, enrollment administration, replacing local `scherzoctl`, pi provider-live/cache behavior, and token-accounting changes.

## Milestones

Milestone 0 verifies blockers. The implementer confirms that the remote command/envelope foundation and the daemon identity/config foundation are present by naming the files and tests that prove them. If either foundation is absent, no LIV-494 code is written.

Milestone 1 delivers the liveness registry. At the end, `src/scherzo/control/remote_liveness.gleam` and `test/remote_liveness_test.gleam` exist, and tests prove online, stale, offline, newer-boot-wins, old-boot heartbeat rejection, invalid id rejection, and no stale cached-online behavior using only controlled `now_ms` values.

Milestone 2 delivers live harness I/O. At the end, harness code under `src/scherzo/control/` and `scripts/scherzo-ui-control-harness` start a loopback listener and minimal client, exchange line-delimited remote protocol messages, accept valid auth/hello, register the daemon online, refresh `last_seen_at_ms` on heartbeat, and reject invalid auth or malformed hello without leaving an online registry entry. `test/remote_harness_test.gleam` proves the socket path by asserting a nonzero bound port and matching client/server line digests.

Milestone 3 delivers transcript evidence. At the end, running the harness demo writes a redacted transcript derived from live send/receive events, includes a run nonce, bound port, connection sequence, message digests, auth-redaction evidence, and liveness observations, and automated tests run the demo twice to prove the transcript is not a static fixture.

Milestone 4 completes helper/docs classification, gates, and scope audit. At the end, the helper help text and any touched docs describe a development-only harness, the standard Gleam test, format, glinter, and Scherzo lint gates pass, and manual pre-publish evidence confirms the slice stayed within the harness/registry boundary with no provider-live/cache, token-accounting, real daemon, browser, or command-mutation changes.

## Progress

- [x] (2026-05-26) Confirmed the prepared review-doc target is `docs/plans/`.
- [x] (2026-05-26) Reviewed current remote envelope, local line-socket FFI, existing control tests, and related foundation plans.
- [x] (2026-05-26) Authored this concise human-reviewable ExecPlan review document and separated mechanical implementation detail into the structured implementation pack.
- [x] (2026-05-26) Incorporated review feedback by making acceptance evidence, test obligations, milestone acceptance, manual-check timing, docs/helper classification, provider-live/cache non-goals, and full lint/validation gates explicit.

## Decision Log

- Decision: Make implementation explicitly blocked by the command/envelope and daemon identity/config foundation tickets.
  Rationale: This slice needs validated message shapes plus stable `daemon_id`/fresh `boot_id` semantics, but should not re-solve those foundations.
  Date: 2026-05-26

- Decision: Use a loopback line-socket harness with a minimal client instead of the real daemon.
  Rationale: It proves the live process/socket path while avoiding daemon lifecycle and command-routing scope.
  Date: 2026-05-26

- Decision: Make liveness status a controlled-time computation.
  Rationale: Deterministic tests can prove stale/offline transitions and prevent cached-online bugs without sleeps.
  Date: 2026-05-26

- Decision: Treat transcript provenance as an acceptance requirement.
  Rationale: The core failure mode to avoid is accepting a fixture transcript as evidence of live remote behavior.
  Date: 2026-05-26

- Decision: Classify the demo transcript and scope diff audit as pre-publish manual evidence, while deferring browser, real-daemon, and operator dogfood checks.
  Rationale: This ticket intentionally stops at a development-only harness; requiring production dogfood now would encourage scope creep into daemon lifecycle and UI work.
  Date: 2026-05-26

## Validation and Acceptance

Acceptance requires concrete evidence before implementation publish. Automated tests must cover registry transitions for online, stale, offline, newer-boot-wins, old-boot heartbeat ignored or rejected, invalid ids, and no stale cached-online query. Harness tests must start a real loopback listener, connect a minimal client, exchange line-delimited remote protocol messages, accept valid auth/hello, reject invalid auth and malformed hello, prove rejected attempts do not leave an online registry entry, and assert matching client-sent/server-received line digests on a nonzero bound port.

Transcript acceptance requires a pre-publish harness run such as `scripts/scherzo-ui-control-harness demo --token test-token --transcript test/tmp/remote-harness/transcript.json`. The transcript must show a nonzero bound port, a run nonce generated for that run, connection/send/receive events from the live socket path, redacted auth material, matching client-sent/server-received message evidence, and resulting liveness observations. Tests must also run the demo twice and assert the run nonce differs, so a static fixture cannot satisfy the check.

Full validation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If a gate fails for a pre-existing unrelated reason, the implementer must capture the exact command output and explain why the new harness/registry changes are not the cause before handoff; otherwise all gates must pass.

A manual pre-publish diff audit must show no daemon startup integration, no supervised daemon outbound client loop, no real pause/resume mutation, no browser UI, no pi provider-live/cache behavior changes, no token-accounting changes, and no docs claiming a production UI/server control plane. Browser checks, production remote-server checks, real-daemon dogfood checks, and operator dogfood procedure are deferred to later implementation tickets.

## Rollout, Recovery, and Idempotence

Rollout is development-only and additive. The harness script should run only when invoked directly, bind to loopback, close sockets on completion, and write transcripts only to an explicit output path. Existing daemon behavior, local `scherzoctl`, tracker commands, pi provider live/cache behavior, token accounting, and production documentation remain unchanged except for any narrow helper/docs text that labels this script development-only.

Recovery is to stop the harness process, remove any generated `test/tmp/remote-harness` artifacts, and rerun. The registry is in-memory and safe to recreate. If validation shows scope creep into daemon lifecycle, real command mutation, browser UI, provider-live/cache behavior, token accounting, or production UI/server docs, back out those changes and keep only the harness/registry slice.

The implementation should be idempotent: repeated test and demo runs create fresh run nonces and clean transcript outputs without requiring manual daemon state repair.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this planning slice. Production transport, daemon outbound supervision, real command routing, enrollment rotation, browser UI, and operator dogfood procedure remain deferred to later tickets.
