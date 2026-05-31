# Daemon read-query infrastructure for control and UI RPC

## Purpose / Big Picture

Scherzo needs a reusable way for operators, `scherzoctl`, and the future remote UI/server to ask the daemon read-only questions without turning those reads into operator mutations. This plan defines an additive read-query plane: one typed query model, one set of safe DTOs, one daemon-owned bounded execution service, and two transport adapters for the existing local control socket and the disabled-by-default outbound `ui_server` socket.

After the follow-up implementation, a reviewer should be able to prove that a small non-domain query such as query introspection/status works through both local `scherzoctl` and remote socket envelopes, that slow fake backends time out without blocking dispatch or polling, and that task list/show remains unimplemented for a later first-consumer ticket.

## Problem Framing and Constraints

The current local control path is a loopback JSON protocol discovered through `.scherzo-state/control.json`; `src/scherzo/control/protocol.gleam` mixes session read requests and mutating command requests in one `Request` type, `src/scherzo/control/server.gleam` routes reads to the session event hub and mutations to `OperatorCommand`, and `src/scherzo/ctl.gleam` exposes both inspection commands and operator controls. The current remote path is not a REST API: `src/scherzo/control/remote_envelope.gleam`, `src/scherzo/control/remote/client.gleam`, and `src/scherzo/orchestrator/daemon_remote_client.gleam` implement daemon-initiated socket envelopes for hello, heartbeat, state snapshots, and server-originated operator commands when `ui_server.enabled` is true.

The design must stay additive. It must not introduce `/api/work`, enable `ui_server` by default, replace `ps`, `session`, `events`, or `attach`, or implement task list/show. It must also avoid raw tracker payloads, credentials, local control-file tokens, enrollment tokens, and provider-specific workflow internals in DTOs.

## Strategy Overview

Create a shared `scherzo/control/query` boundary that owns query request/response types, JSON codecs, query error codes, cursor pagination tokens, and redacted DTO definitions. Local control and remote envelopes should wrap those same query values rather than defining parallel payloads. The first implementation should expose only an infrastructure-safe query such as catalog/status introspection so the transports, CLI parsing, and service behavior are testable without shipping task list/show.

Add a daemon-owned query service process. The daemon starts it beside the control plane, passes its handle to the local control server and remote client, and stops it during daemon shutdown before the event hub disappears. Query execution happens in worker processes with a small concurrency limit, queue limit, per-query timeout, timeout cleanup, and shutdown cancellation. Fast daemon snapshots may use short actor calls, but slow tracker or ledger reads run outside the orchestrator transition loop so dispatch and polling continue.

## Alternatives Considered

A public HTTP REST endpoint such as `/api/work` was rejected because it does not match Scherzo's current local control-file protocol or outbound remote socket architecture.

Adding reads to `OperatorCommand` was rejected because operator commands are mutations with command-result semantics, idempotency concerns, and audit expectations that do not fit ordinary bounded reads.

Starting one-shot Linear helper processes for every UI read was rejected because future queries need daemon-local context, shared DTOs, pagination, and consistent timeouts rather than provider-specific helper behavior.

Migrating existing session/event commands now was rejected because those commands are stable operator tools with streaming behavior; the query plane should prove itself additively before any later migration.

## Risks and Countermeasures

The largest runtime risk is blocking the orchestrator with slow tracker or ledger reads. The countermeasure is a separate query service with worker processes, small timeouts, bounded concurrency, and tests that keep polling/control responsive while a fake query sleeps.

The largest protocol risk is DTO drift between local and remote callers. The countermeasure is one shared query codec and shared DTO constructors, with local and remote round-trip tests decoding to the same typed values.

The largest safety risk is leaking raw provider data or secrets. The countermeasure is allowlist DTO construction, redaction tests for token-like and raw-payload fields, and a pre-publish scope audit proving no credentials, raw tracker payloads, or local control-file fields moved into remote envelopes.

A delivery risk is scope creep into the first real task/work query. The countermeasure is to ship only the reusable infrastructure and a harmless introspection/status query in this slice, while explicitly recording task list/show as the next consumer.

A review/pack drift risk is that this prose document could require acceptance evidence, exact tests, manual checks, helper boundaries, provider/cache guardrails, or lint gates that the structured implementation pack does not ask the follow-up implementer to collect. The countermeasure is to mirror those obligations in the pack's concrete steps and testing notes before Scherzo materializes follow-up artifacts.

## Scope Boundaries

In scope are typed query request/response modules, reusable cursor pagination conventions, query DTO/redaction helpers, a bounded daemon query service, local protocol/client/server/CLI adapters for generic read queries, remote envelope/client/harness adapters for query RPC, and tests proving timeout, overload, cleanup, codec, and redaction behavior.

Out of scope are task list/show, public HTTP REST, enabling `ui_server` by default, replacing session/event streaming commands, changing operator mutation semantics, browser UI work, provider-live/cache behavior changes, migrating `.scherzo/workflows/scripts` or provider-live helper scripts, and offline/destructive maintenance commands. Documentation/helper work in scope is limited to additive operator-facing help or docs for the new generic query surface and mechanical test helper additions needed by the new harnesses; if no operator docs need a new query mention, the implementation should record that no docs/helper migration was required. Later migration candidates are workstream list/show, schedule status/history/log lookup, retained artifact reads, state projection inspection, and eventually task/work list/show. Commands that should remain as-is for now include `ping`, `ps`, `session`, `events`, `attach`, operator mutations, cleanup/state destructive commands, and workstream decision/start commands.

## Milestones

Milestone 1 establishes the shared query model under `src/scherzo/control/query/`. At the end, the repository has typed query requests, responses, errors, cursor tokens, and allowlist DTO/redaction helpers with pure codec tests and no daemon wiring. Acceptance evidence is targeted output from `direnv exec . gleam test test/control_query_codec_test.gleam` showing status/introspection query round trips, cursor encode/decode, invalid-cursor errors, error-code JSON mapping, and redaction of token-like/raw-payload fields.

Milestone 2 adds the bounded query service under the same query boundary. At the end, fake fast, slow, failing, and overflowing queries prove concurrency limits, queue behavior, timeout mapping, ignored stale completions, and shutdown cleanup without touching the orchestrator loop. Acceptance evidence is targeted output from `direnv exec . gleam test test/control_query_service_test.gleam` plus a responsiveness check, in `test/orchestrator_daemon_control_test.gleam` or an equivalent daemon test, proving a sleeping fake query does not block local ping/control or daemon polling callbacks.

Milestone 3 wires local control and `scherzoctl`. At the end, a generic read query can be sent through the existing control file and loopback socket, unauthorized local requests are rejected before execution, existing local commands still pass, and CLI parsing/output tests cover the new query surface without changing task list/show. Acceptance evidence is targeted output from `direnv exec . gleam test test/control_protocol_test.gleam test/control_server_test.gleam test/control_client_test.gleam test/ctl_test.gleam`, or the closest existing focused files if names drift, proving authenticated query round trips, unauthenticated rejection before backend execution, pretty and JSON CLI output for the status query, unchanged `ping`/`ps`/`session`/`events` behavior, and no task list/show command.

Milestone 4 wires remote `ui_server` envelopes. At the end, remote query request/response envelopes share the same query codec, the outbound remote client can answer a query while heartbeat/state behavior continues, malformed query envelopes map to safe errors, and the loopback harness can produce redacted live query evidence. Acceptance evidence is targeted output from `direnv exec . gleam test test/control_remote_envelope_test.gleam test/control_remote_client_test.gleam test/orchestrator_daemon_remote_client_test.gleam test/remote_harness_test.gleam`, plus a retained loopback transcript showing a remote status query response, heartbeat/state traffic before and after it, and no enrollment token, local control token, raw tracker payload, or credential fields.

Milestone 5 completes rollout proof. At the end, validation gates pass, additive docs/help status is recorded, a diff/scope audit confirms no REST API, default `ui_server` enablement, first-consumer task query, provider-live/cache behavior change, workflow-helper migration, or session/event migration slipped in, and migration candidates are documented for follow-up work. Pre-publish manual/operator evidence is limited to a local daemon plus `scherzoctl query` smoke transcript and the loopback remote harness transcript; browser or staging UI dogfood remains deferred until a later UI/server consumer exists.

## Progress

- [x] (2026-05-31 03:50Z) Confirmed the prepared output target is `docs/plans/` and selected this review document path.
- [x] (2026-05-31 03:50Z) Surveyed the local control protocol, client, server, `scherzoctl`, daemon wiring, remote envelope, remote client runtime, and harness coverage named by LIV-769.
- [x] (2026-05-31 03:50Z) Authored this human-reviewable plan summary and prepared the mechanical implementation detail for the structured implementation-pack submission.
- [x] (2026-05-31 03:50Z) Validated this review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-769-daemon-read-query-infrastructure.md`, which reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-05-31 04:20Z) Incorporated review feedback by making acceptance evidence, test obligations, milestone proof anchors, pre-publish versus deferred manual/dogfood checks, docs/helper boundaries, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Keep read queries separate from `OperatorCommand` and `CommandResult`.
  Rationale: Reads need pagination, bounded execution, and DTO responses, while operator commands represent mutations with command-result semantics.
  Date: 2026-05-31

- Decision: Use one shared query codec and wrap it from local and remote transports.
  Rationale: This directly prevents local `scherzoctl` and remote `ui_server` from drifting into incompatible DTO shapes.
  Date: 2026-05-31

- Decision: Prove the first slice with introspection/status rather than task list/show.
  Rationale: LIV-769 asks for infrastructure; task list/show is explicitly a follow-up first consumer.
  Date: 2026-05-31

- Decision: Run query work in a daemon-owned service outside the orchestrator transition loop.
  Rationale: Slow tracker and ledger reads must not delay dispatch, polling, shutdown, or operator mutations.
  Date: 2026-05-31

- Decision: Treat review feedback about acceptance evidence, tests, manual/dogfood timing, docs/helper boundaries, provider-live/cache non-scope, full validation, and linting as implementation-pack obligations rather than prose-only notes.
  Rationale: Scherzo materializes follow-up implementation artifacts from the structured pack, so those obligations must be visible mechanically as well as in this review document.
  Date: 2026-05-31

## Validation and Acceptance

Planning acceptance for LIV-769 requires this file to exist at `docs/plans/LIV-769-daemon-read-query-infrastructure.md`, the review-doc validator to report `REVIEW_DOC_VALID=ok`, and Scherzo to capture exactly one structured implementation-pack submission. The planning handoff must not manually write the canonical bundle or implement production query code.

Follow-up implementation acceptance requires automated evidence for shared local/remote query DTOs, cursor encode/decode and invalid-cursor errors, query timeout and overload errors, worker cleanup after timeout/shutdown, local protocol authentication and round trips, CLI parse/output behavior, remote envelope round trips and negative decoding, remote client query handling without stopping heartbeat/state traffic, harness transcript redaction, and unchanged existing control commands. The expected targeted evidence is successful output from `direnv exec . gleam test test/control_query_codec_test.gleam`, `direnv exec . gleam test test/control_query_service_test.gleam`, `direnv exec . gleam test test/control_protocol_test.gleam test/control_server_test.gleam test/control_client_test.gleam test/ctl_test.gleam`, and `direnv exec . gleam test test/control_remote_envelope_test.gleam test/control_remote_client_test.gleam test/orchestrator_daemon_remote_client_test.gleam test/remote_harness_test.gleam`, with file names adjusted only if the implementation records the equivalent focused files it actually created or extended. Full validation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; lint warnings are not a reason to skip the gates, and new production policy errors are unacceptable.

Pre-publish manual/operator evidence for the follow-up implementation is a short local smoke transcript from a real daemon showing the new generic `scherzoctl query` status/introspection command returning redacted JSON or pretty output, a loopback remote harness transcript showing a remote status query response while heartbeat/state messages continue, and a diff/scope audit proving no public HTTP REST API, no default `ui_server` enablement, no task list/show implementation, no raw tracker payload or credential DTO leakage, no provider-live/cache changes, no `.scherzo/workflows/scripts` or provider-live helper migration, and no migration of existing session/event commands. Browser or staging UI dogfood is deferred until a later UI/server consumer exists.

## Rollout, Recovery, and Idempotence

Rollout is additive and opt-in. Existing local control commands continue to use the current control file, existing remote behavior remains disabled unless `ui_server.enabled` is true, and the first query surface exposes only infrastructure-safe read behavior.

Recovery is straightforward: disable or do not configure `ui_server` to avoid the remote path, stop using the new generic `scherzoctl query` surface for local reads, or revert the query modules and transport envelope additions while leaving existing command paths intact. Because docs/helper work is additive and limited, any query help or operator-doc snippet can be reverted independently without touching workflow helpers, provider-live helpers, or cache behavior. Re-running tests, query calls, and harness transcripts should be idempotent except for ordinary generated transcript files under test temp directories.

## Open Questions and Clarifications Needed

No blocking clarification is needed for the infrastructure plan. Follow-up consumer tickets should decide the exact task/work DTO fields, whether opaque cursors need signing before any multi-tenant server exists, and whether later high-volume tracker queries need cache/invalidation rules beyond the first bounded worker service.
