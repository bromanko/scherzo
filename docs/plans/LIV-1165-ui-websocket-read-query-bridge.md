# UI WebSocket read-query bridge for Work queries

This ExecPlan v2 review document is the human review surface for LIV-1165. It plans a later implementation that lets the Scherzo UI/API ask a connected daemon for read-query data over the existing UI WebSocket control plane; mechanical steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo's Work UI needs to ask a daemon for Work list and show data without learning Linear-specific fields, local tracker adapter details, or daemon-local control tokens. After this plan is implemented, the UI server can send a read-only query request to the daemon over the already-authenticated UI WebSocket connection and receive a bounded query response or a safe query error on the same connection.

The visible result is not a completed browser Work screen. The visible result is a tested protocol and daemon client bridge: daemon hello, heartbeat, state snapshots, revocation, and operator commands continue to work while `query_request` messages are decoded, executed through the daemon query service, and answered with `query_response` messages.

## Problem Framing and Constraints

The current UI WebSocket client path in `src/scherzo/control/remote/ui_websocket_client.gleam` supports daemon hello, heartbeat, daemon state snapshots, credential and identity revocation, and server-originated operator commands. The line-delimited remote control envelope in `src/scherzo/control/remote_envelope.gleam` already has query request and response concepts, and the daemon already has a query service in `src/scherzo/control/query/service.gleam`, but the UI WebSocket protocol in `src/scherzo/control/remote/ui_protocol.gleam` does not expose query messages.

The bridge must be read-only and must preserve existing liveness behavior. Heartbeats and state snapshots must still be scheduled and sent while queries run. A slow or failing query must not block the actor, prevent operator command results from being sent, or cause the daemon to leak credentials, local control tokens, raw tracker payloads, raw prompts, comments, descriptions, or full provider responses in protocol payloads or logs.

Work queries are the first intended UI consumer. In the current tree, the generic query family already includes `task_list` and `task_show` in `src/scherzo/control/query/types.gleam`, with tracker-backed execution in `src/scherzo/control/query/backend.gleam`; if a later WorkItem-named contract lands, it must ride the same generic bridge rather than adding a second UI WebSocket path. This plan must not introduce provider-live cache changes: the bridge transports query requests and responses only, and it relies on the existing daemon query service and tracker adapter behavior for provider access, timeout, overload, and cache semantics.

## Strategy Overview

Use a UI-protocol wrapper with camelCase top-level fields and reuse the existing control-query JSON codec unchanged for the nested query and result bodies. A server-to-daemon request should have type `query_request`, a server-generated `queryId`, target `daemonId` and `bootId`, and a nested `query` object encoded by `scherzo/control/query/codec.gleam`. A daemon-to-server reply should have type `query_response`, the same `queryId`, and a nested `result` object encoded by the same query codec, either as an `ok: true` response or an `ok: false` query error.

Extend `ui_websocket_client` with a read-query runtime that mirrors the existing command bridge shape but stays independent from `command_bridge_enabled`; a deployment may allow read-only UI queries while still refusing remote operator commands. The client should validate the target daemon and boot id, cap concurrent query workers at eight, enforce an outer query timeout, send `query_overloaded`, `query_timeout`, `query_shutdown`, `unsupported_query`, or `query_backend_failed` responses as appropriate, tag worker completions with the current connection generation, and drop stale completions after reconnects.

Wire query execution from the UI WebSocket client to the daemon query service through the existing daemon control dependency path rather than adding tracker-specific knowledge to the UI client. The server/API side should add a matching pending-query map, timeout handling, disconnect cleanup, and Work API adapter that translates UI requests into query codec payloads and translates `query_response` payloads back into API responses.

## Alternatives Considered

Sending the existing line-delimited remote envelope directly over the UI WebSocket was rejected because the UI protocol already uses camelCase top-level fields such as `daemonId`, `bootId`, and `serverCommandId`. Reusing the nested query codec preserves the stable query contract without forcing the UI WebSocket protocol to adopt snake_case envelopes at the top level.

Exposing tracker-specific Work APIs directly from the server was rejected because it would put Linear and future provider details in the UI/API layer and bypass daemon-side redaction, bounds, and query-service overload behavior.

Using only the local control socket was rejected because the UI server needs to query remote daemons it already manages over the WebSocket control plane. Adding a general RPC framework was rejected as too large; this task needs one bounded request/response bridge over an existing authenticated connection.

## Risks and Countermeasures

The main safety risk is leaking secrets or large raw data in nested query payloads or logs. The countermeasure is to reuse the existing query DTO codec, log only query ids, query type names, error codes, and sanitized messages, and add tests with obvious secret/raw-payload markers that must not appear in outbound frames or logs.

A liveness risk is that slow queries starve heartbeat, state, or command handling. The countermeasure is to run each query in a worker process, track it in the actor state, enforce a timeout with a timer, and keep heartbeat and state timers independent from query execution.

An overload risk is that the server sends too many queries at once. The daemon should accept at most eight in-flight UI WebSocket queries per connection, reject additional requests immediately with a `query_overloaded` response, and rely on the daemon query service's own queue and timeout as a second safety boundary.

A stale-connection risk is that a query result completes after reconnect or shutdown. The countermeasure is to tag every query worker with the connection generation, cancel and kill query workers when the connection is closed or the client shuts down, and ignore completions whose generation or worker identity no longer matches the running-query table.

A compatibility risk is breaking existing command bridge behavior. The countermeasure is to keep command message types and routing unchanged and add compatibility tests where a command and a query are both in flight.

## Scope Boundaries

In scope are the UI WebSocket protocol representation in `src/scherzo/control/remote/ui_protocol.gleam`, daemon-side UI WebSocket query handling in `src/scherzo/control/remote/ui_websocket_client.gleam`, orchestration wiring through `src/scherzo/orchestrator/remote_command_runtime.gleam` and `src/scherzo/orchestrator/daemon_remote_client.gleam`, and tests under `test/` that prove protocol, lifecycle, liveness, error, overload, timeout, stale-generation, shutdown, and command-compatibility behavior.

In scope for planning is identifying the server/API work: the UI server must advertise or detect query-bridge support, generate unique `queryId` values, send `query_request` frames to the currently connected daemon generation, await matching `query_response` frames with a server-side timeout, clean up pending requests on disconnect, and expose Work or WorkItem list/show API calls through this bridge.

Out of scope are implementing a new WorkItem DTO contract if the existing `task_list` and `task_show` query DTOs are not sufficient, rendering browser UI, adding tracker writes, changing provider-live or cache behavior, migrating unrelated documentation helpers, exposing credentials or raw provider data, changing local control query semantics, replacing the line-delimited remote envelope, and requiring live browser or Linear dogfood before the daemon bridge can be published.

## Milestones

Milestone 1 establishes the UI protocol contract. The implementer first adds failing tests in `test/control_remote_ui_protocol_test.gleam` for a valid camelCase `query_request`, an encoded successful `query_response`, an encoded error `query_response`, and malformed query request rejection when a `queryId` is present. Reviewers should then see `src/scherzo/control/remote/ui_protocol.gleam` decode and encode those frames by reusing `scherzo/control/query/codec.gleam` for nested bodies.

Milestone 2 adds the daemon UI WebSocket query runtime. The implementer first adds failing tests in `test/control_remote_ui_websocket_client_test.gleam` for valid execution, invalid-query rejection without executor invocation, target daemon/boot mismatch, backend error, timeout, ninth-query overload, heartbeat continuity while a query worker is blocked, stale-generation completion suppression, shutdown cleanup, and command bridge compatibility. Reviewers should then see in-flight query tracking, an eight-query limit, timeout timers, sanitized logging, target validation, stale-generation handling, worker cleanup, and outbound query responses that do not interfere with heartbeat, state snapshots, or command results.

Milestone 3 wires the bridge to the daemon query service. Reviewers should see the orchestrator dependency path pass an `execute_query` function through `src/scherzo/orchestrator/remote_command_runtime.gleam` and `src/scherzo/orchestrator/daemon_remote_client.gleam` into the UI WebSocket client, so the client remains tracker-agnostic. The proof is a daemon remote-client or remote-command-runtime test showing a UI WebSocket query reaches the same query service used by local control queries, while existing command, heartbeat, and state tests still pass.

Milestone 4 defines the server/API consumption contract without blocking the daemon bridge on browser work. Reviewers should see a checked-in contract fixture or server-side test, if the server/API code is present in the implementation workspace, proving Work or WorkItem list/show requests translate into query codec payloads and complete only from matching `query_response` frames. If that code is absent, the milestone output is explicit handoff text for the server/API owner that preserves the same timeout and disconnect-cleanup obligations.

Milestone 5 proves safety and compatibility. Reviewers should see the focused tests from the earlier milestones passing, plus a redaction assertion with obvious secret/raw-payload markers, no added provider-live/cache behavior, and full repository validation with `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` all exiting zero.

## Progress

- [x] (2026-06-16) Read the repository-local ExecPlan guidance in `workflows/dogfood/guidance/exec-plan.md` for this workflow.
- [x] (2026-06-16) Inspected the current UI WebSocket protocol/client, remote envelope query codec, query service, daemon query wiring, daemon remote client wiring, and existing UI WebSocket tests relevant to this plan.
- [x] (2026-06-16) Authored this concise review document and delegated mechanical implementation details to the structured implementation pack for LIV-1165.
- [x] (2026-06-16) Incorporated review feedback by making test obligations, acceptance evidence, full validation, lint gates, provider-live/cache non-goals, and deferred manual dogfood checks explicit.

## Surprises & Discoveries

The existing line-delimited remote envelope already models `RemoteQueryRequest` and `RemoteQueryResponse`, including nested query codec encoding and decoding. That lowers protocol risk because the UI WebSocket bridge can reuse the same nested query shape rather than inventing a second query DTO.

The current UI WebSocket client already has robust command bridge behavior, including target daemon/boot checks, in-flight command limiting, malformed command rejection, send-failure reconnects, and generation checks. The query bridge should follow those lifecycle patterns, but it needs separate worker tracking and timeout cleanup because read queries should remain available even when `command_bridge_enabled` is false.

The daemon query service already has concurrency, queue, timeout, overload, and shutdown error concepts. The UI WebSocket bridge still needs its own per-connection in-flight cap and stale-connection cleanup so an untrusted or buggy server cannot accumulate client-side worker processes.

The current query DTOs already include `TaskList` and `TaskShow`, serialized as `task_list` and `task_show`, so the server/API Work surface can be proved with those existing query variants unless a separate WorkItem naming migration lands before implementation.

## Decision Log

- Decision: Use a camelCase UI WebSocket wrapper with nested existing query codec JSON. Rationale: This preserves UI protocol style while avoiding a duplicate query schema. Date: 2026-06-16.
- Decision: Keep read-query handling independent from `command_bridge_enabled`. Rationale: Read-only Work UI data should be available to an authenticated UI server even when remote operator commands are disabled. Date: 2026-06-16.
- Decision: Set the daemon UI WebSocket in-flight query limit to eight per connection. Rationale: It matches the existing command bridge and remote query envelope precedent while bounding worker/process pressure. Date: 2026-06-16.
- Decision: Treat daemonId/bootId mismatch and stale generation as non-executing conditions. Rationale: A query addressed to the wrong daemon or old boot must not reach the query service; stale worker completions after reconnect should be dropped. Date: 2026-06-16.
- Decision: Defer browser/UI dogfood and live Linear evidence to the server/API or Work UI integration follow-up. Rationale: This daemon bridge can be proven with deterministic protocol, client, wiring, and redaction tests before a browser surface exists. Date: 2026-06-16.
- Decision: Do not change provider-live or query cache behavior as part of this bridge. Rationale: The bridge is a transport layer over `QueryRequest` and `QueryResponse`; provider access, cache decisions, timeout, and overload semantics belong to the existing daemon query service and tracker adapter. Date: 2026-06-16.

## Outcomes & Retrospective

This plan has not been implemented yet. The intended outcome is a generic, bounded, read-only query bridge over the UI WebSocket control plane that can carry Work list/show data through the existing `task_list` and `task_show` query DTOs, or through a later WorkItem-named DTO if that contract supersedes them. The main intentional gaps are browser rendering, live server/API deployment evidence, and any WorkItem DTO migration that belongs to separate Work query-contract work.

## Validation and Acceptance

Acceptance outcome 1 is a protocol contract. Evidence must include tests in `test/control_remote_ui_protocol_test.gleam` showing that a valid `query_request` with camelCase `queryId`, `daemonId`, `bootId`, and nested query codec JSON decodes to the expected query request, that `query_response` encodes successful and error results using the query codec, and that malformed JSON or malformed nested query bodies produce sanitized decode errors.

Acceptance outcome 2 is daemon client execution. Evidence must include tests in `test/control_remote_ui_websocket_client_test.gleam` for a valid query reaching the injected query executor and sending a matching `query_response`, invalid query handling without executor invocation, backend error propagation, timeout response, concurrent-query overload at the ninth in-flight query, heartbeat emission while a query worker is blocked, command bridge compatibility while a query is in flight, stale-generation completion suppression after reconnect, and shutdown cleanup of query workers and timers.

Acceptance outcome 3 is orchestration wiring. Evidence must include tests in the daemon remote-client or remote-command-runtime test surface proving `daemon_remote_client` passes query execution from the UI WebSocket dependency to the daemon query service and that existing command, heartbeat, and state behavior still passes unchanged.

Acceptance outcome 4 is server/API readiness. This is not a pre-publish manual browser or live Linear dogfood gate for the daemon bridge. Evidence before daemon-bridge publish may be a checked-in contract fixture or server-side test if the server/API code is available in the implementation workspace; otherwise it is explicit post-implementation handoff evidence for the server/API owner: send a `task_list`, `task_show`, WorkItem-named successor, or other existing safe query request through the daemon WebSocket, observe a matching `query_response`, verify server-side timeout cleanup on disconnect, and verify no credentials, local control tokens, raw tracker payloads, raw prompts, or full provider responses appear in server logs. The deferred human/operator dogfood check after server/API integration is to exercise the Work page or API against a real daemon and confirm the same behavior without changing provider-live cache semantics.

Acceptance outcome 5 is repository validation. From the repository root, run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; each command must exit zero. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands. These full validation and linting gates are required before the implementation is considered ready for review.

## Rollout, Recovery, and Idempotence

The rollout is additive. Existing UI WebSocket message types remain valid, and the server should only send `query_request` frames to daemon connections known to support the query bridge or be prepared to time out safely when an older daemon ignores the message.

Recovery is straightforward because the bridge is read-only and writes no tracker data, ledger entries, retained artifacts, or local state files. If the bridge misbehaves, the server/API can stop sending `query_request` frames, and the daemon can reject unsupported or overloaded queries while leaving heartbeat, state snapshots, revocation handling, local control queries, and operator command handling intact.

The implementation is idempotent because query ids are server-generated per request, query responses do not mutate daemon state, reconnect cleanup drops stale completions, and tests use fake dependencies and temporary files under `test/tmp/`. Provider-live behavior and caches are not migrated by this plan, so retries should not create duplicate provider writes or require cache cleanup. Re-running validation should not require manual cleanup beyond the existing test helper reset behavior.

## Open Questions and Clarifications Needed

No blocking clarification is required before implementing the daemon bridge. The only follow-up product choice is whether the server/API should expose Work reads over HTTP, server-sent events, or another UI-facing route after it consumes the daemon WebSocket bridge; that choice is outside this daemon protocol plan.
