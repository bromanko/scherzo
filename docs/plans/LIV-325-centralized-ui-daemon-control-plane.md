# Centralized UI Daemon/Server Control Plane First Slice

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

This plan prepares the first safe slice of a centralized Scherzo UI/server control plane. After implementation, a daemon can initiate an authenticated outbound connection to a server harness, register with a stable daemon identity, heartbeat for liveness, stream minimal state/result events, receive a server command such as pause or resume, and return the existing `CommandResult` semantics produced by the daemon command handler.

The operator-visible win is central control without making the local `scherzoctl` loopback API network-facing. A human should be able to start a minimal server harness, start a daemon configured for that harness, see `hello` and heartbeat evidence, send pause/resume from the harness, and see an `applied` or rejected result that matches local control semantics.

## Problem Framing and Constraints

Scherzo currently has local operator control through `scripts/scherzoctl` and `src/scherzo/ctl.gleam`, which talk to the daemon through `src/scherzo/control/client.gleam`, `src/scherzo/control/protocol.gleam`, and the loopback `src/scherzo/control/server.gleam`. That path uses `control.json` as local discovery/auth material and `instance.lock` as a local singleton guard. Those files are not durable central identity, central liveness, or a safe public network API.

The hard constraint is semantic reuse: `src/scherzo/control/command.gleam` remains the canonical command model. Remote UI/server commands must decode to `command.OperatorCommand`, enter the daemon through the same command-handling semantics used by local controls, and complete with `command.CommandResult`. The plan must also keep tracker/comment-originated remote commands separate from this daemon/server transport; both may share command semantics, but they have different trust boundaries and lifecycles.

## Strategy Overview

Use an additive remote control-plane subsystem with a new remote envelope and a shared command/result codec. The local control protocol remains loopback-only and keeps its control-file token framing. Only the reusable JSON mapping for `OperatorCommand` and `CommandResult` is factored into a small codec module so the remote envelope can carry the same semantic objects without exposing local request fields, control tokens, or filesystem discovery.

Each daemon persists a stable `daemon_id` in local Scherzo state and generates a fresh `boot_id` for each process start. The daemon initiates the connection, sends an authenticated `hello`, then periodic heartbeats. The server registry keys liveness by `daemon_id` plus the active `boot_id`, marks missed heartbeats stale/offline, and closes or ignores stale connections after a newer boot registers. The first slice uses a line-delimited JSON development harness rather than the full UI; production transport, browser UI, and enrollment administration remain separate follow-up work.

## Context and Current Repository Facts

The current local control stack is intentionally host-local. `src/scherzo/control/file.gleam` writes `.scherzo-state/control.json` with a loopback host, port, and generated token for `scherzoctl`; `src/scherzo/control/server.gleam` defaults to `127.0.0.1` and authenticates every local request with that token. `src/scherzo_control_ffi.erl` rejects non-loopback hosts in its current listen/connect helper, which is correct for local control and is a reason not to reuse that helper blindly as the central outbound transport.

The command semantics already live outside the wire protocol. `src/scherzo/control/command.gleam` defines `OperatorCommand`, `CommandResult`, and stable status strings such as `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`. `src/scherzo/control/protocol.gleam` currently has local JSON mapping code for command requests and results; that mapping should be extracted or mirrored into `src/scherzo/control/command_codec.gleam` so both local and remote transports use the same command/result semantics.

The daemon entry point for local controls is `daemon.apply_operator_command` in `src/scherzo/orchestrator/daemon.gleam`, reached from `control_backend`. Tracker comment commands already use `RemoteOperatorCommand` in `src/scherzo/orchestrator/effects/types.gleam` and `src/scherzo/orchestrator/transitions/linear_commands.gleam`. The UI/server control plane must not reuse that tracker source value; it should add a distinct server-control source while sharing the same command handler logic.

## Alternatives Considered

Reusing `src/scherzo/control/protocol.gleam` directly as the remote protocol is rejected because it bakes in local control tokens, request/response assumptions, and loopback discovery semantics. Exposing the existing local control server on a non-loopback interface is rejected because it turns a host-local convenience API into a public control surface.

Having the server discover daemons through host filesystem paths, `control.json`, `instance.lock`, or process scans is rejected because it couples the UI/server to daemon hosts and breaks remote deployment. Polling tracker comments through `remote_commands` is also insufficient: that transport is task/comment-scoped and tracker-authorized, while the UI/server control plane is daemon-scoped and server-authorized.

A full browser UI first is rejected for this slice because it would combine transport, identity, liveness, and presentation risk. The right first slice is a scriptable harness that can prove live daemon registration and command/result correlation before a human-facing UI is added.

## Risks and Countermeasures

The main security risk is accidentally treating local control credentials or local files as central credentials. The countermeasure is a new enrollment secret/config path for daemon-to-server auth, log redaction for secrets, and tests that prove remote hello does not read `control.json` tokens.

The main semantic risk is inventing duplicate command meanings. The countermeasure is to require every valid remote command to decode to `OperatorCommand` and to route through the same daemon command semantics; remote-only parse/auth errors may use remote error envelopes, but valid operator-command outcomes use `CommandResult`.

The main reliability risk is duplicate or stale commands across reconnects. The countermeasure is server-generated command ids, daemon-side in-memory idempotency per boot, duplicate-conflict rejection, and server-side stale boot handling. The first slice proves this with pause/resume because those commands are low blast radius.

The main evidence risk is a false green result from a fixture or cached server view rather than a live daemon. The countermeasure is a live harness transcript collected before publish and tests with a controlled clock proving online, stale, and offline transitions. A server registry must never keep reporting a daemon as online after the stale/offline deadline merely because a cached snapshot exists.

## Scope Boundaries

In scope for the first implementation slice are persisted `daemon_id`, per-process `boot_id`, authenticated outbound hello, heartbeats, server-side liveness state, minimal state/result event streaming, a remote command envelope for pause/resume and the general command codec shape, result correlation by server command id, reconnect/backoff behavior, stale boot handling, idempotency tests, a scriptable server harness, and documentation that distinguishes local control, tracker remote commands, and the new UI/server control plane.

The implementation must migrate or add helper/docs surfaces needed for the first slice: the harness command or script, example disabled-by-default configuration, and operator documentation in the getting-started/control documentation. Existing helper behavior for `scripts/scherzoctl`, local `control.json`, and tracker comment commands stays compatible.

Out of scope are the full browser UI, public exposure of the local control server, filesystem/process discovery from the central server, durable server storage beyond what the harness needs, production-grade multi-tenant enrollment administration, token rotation UI, full session-event fan-out, and replacing `scherzoctl`. Browser/manual UI dogfood is deferred until a later UI slice; the pre-publish manual requirement for this slice is the scriptable live harness transcript.

## Milestones

The first milestone establishes the remote contract. At the end of this milestone there is a command/result codec around `OperatorCommand` and `CommandResult`, a remote envelope module for hello, heartbeat, server command, command receipt, command result, and minimal state events, and tests proving the existing local protocol still round-trips. This milestone retires the highest semantic risk first.

The second milestone adds daemon identity and configuration. At the end of this milestone the daemon has a stable persisted `daemon_id`, a fresh `boot_id` per process, disabled-by-default server connection settings, and authentication material that does not reuse the local control token. Tests prove stable identity reuse, fresh boot ids, config validation, and secret redaction.

The third milestone implements the outbound client and server harness. At the end of this milestone a loopback development harness can accept a daemon-initiated connection, authenticate hello, observe heartbeats, maintain liveness by `daemon_id` and `boot_id`, reconnect with bounded backoff, and mark older boots stale. Tests prove online, stale, offline, newer-boot-wins, and no stale cached-online behavior.

The fourth milestone routes one server command through the real daemon command path. At the end of this milestone the harness can send pause/resume with a server command id, the daemon invokes the common operator-command handler, and the correlated `CommandResult` returns to the harness. Tests prove exact duplicate command ids do not double-apply and conflicting duplicate ids are rejected.

The fifth milestone adds minimal streaming, hardening evidence, docs, and final validation. At the end of this milestone the daemon emits initial state plus command receipt/result events, the docs explain how this differs from `scherzoctl` and tracker `remote_commands`, the harness helper is documented, all automated validation and lint gates pass, and the pre-publish live transcript shows hello, heartbeat, state/result event, pause or resume dispatch, and a correlated `status=applied` result.

## Progress

- [x] (2026-05-22T01:55Z) Created the human-reviewable ExecPlan review document for LIV-325 and prepared the structured implementation-pack handoff.
- [x] (2026-05-22T02:20Z) Incorporated review feedback by tightening milestone specificity, acceptance evidence, test obligations, pre-publish versus deferred manual checks, docs/helper scope, liveness/cache behavior, full validation, and linting requirements.

## Surprises & Discoveries

- Observation: The existing TCP FFI used by local control rejects non-loopback hosts, which is correct for `scherzoctl` but unsuitable as the eventual central outbound transport if reused unchanged.
  Evidence: `src/scherzo_control_ffi.erl` parses only `127.0.0.1` and `localhost` in `parse_loopback_host`.

## Decision Log

- Decision: Use a new remote control-plane envelope instead of the local loopback request/response protocol. Rationale: local protocol fields and control-file tokens are host-local assumptions and should not become the central network API. Date: 2026-05-22.
- Decision: Keep `src/scherzo/control/command.gleam` canonical and route valid remote commands through the common daemon operator-command semantics. Rationale: one command semantics API prevents local and remote operator controls from diverging. Date: 2026-05-22.
- Decision: Treat tracker-adapter `remote_commands` as a separate transport and add a distinct UI/server command source if source metadata is needed in daemon transitions. Rationale: tracker comments and UI/server daemon control use different authentication, authorization, correlation, and liveness boundaries. Date: 2026-05-22.
- Decision: Make the first slice a scriptable line-delimited JSON harness rather than a browser UI. Rationale: hello, heartbeat, liveness, and command/result correlation can be proven with lower blast radius; browser UI dogfood is deferred until the transport slice is reliable. Date: 2026-05-22.

## Outcomes & Retrospective

This document revision addresses review feedback before implementation. No production code has been changed by this revision; the expected outcome of the follow-up implementation remains a disabled-by-default, additive remote control-plane first slice with live harness evidence before publish.

## Validation and Acceptance

Acceptance is verifiable only when the follow-up implementation records evidence before publish. Required automated evidence is `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` passing from the repository root. These commands are mandatory publish gates, not optional cleanup.

Required behavior evidence is: tests show stable `daemon_id` reuse and fresh `boot_id`; protocol tests show remote command envelopes decode to `OperatorCommand` and results encode/decode as `CommandResult`; liveness tests show online, stale, and offline transitions with a controlled clock; duplicate command-id tests show no double application and conflict rejection; and a harness transcript, collected before implementation publish, shows live hello, heartbeat, state/result event, pause or resume dispatch, and a correlated `status=applied` result.

The implementation must also include negative evidence that remote auth does not rely on `control.json`, that the local loopback control server remains loopback-only, that a cached or fixture liveness view cannot mask a stale/offline daemon, and that tracker/comment `remote_commands` tests continue to pass unchanged. Full browser UI checks and multi-daemon production dogfood are deferred human/operator checks after this slice, not pre-publish requirements for this slice.

## Rollout, Recovery, and Idempotence

Roll out behind disabled-by-default configuration. If the remote server is absent, unauthenticated, or unreachable, the daemon continues normal local operation and retries with bounded backoff without blocking polling, dispatch, or `scherzoctl`. The first slice must log connection state without logging enrollment tokens or command payload secrets.

Recovery is additive: disabling the remote control-plane config stops connection attempts and leaves local control untouched. Repeated daemon starts reuse the same `daemon_id` and emit new `boot_id` values. Retried server commands use server command ids for idempotency; exact duplicates return the cached or in-flight result, while duplicate ids with conflicting payloads are rejected without applying a second command.

Rollback is straightforward because local `scherzoctl`, `control.json`, `instance.lock`, and tracker comment commands remain unchanged. If the remote slice misbehaves, turn off the new config section, restart the daemon, and continue operating through local control while retaining the harness transcript and logs for diagnosis.

## Open Questions and Clarifications Needed

The first slice can use a development TCP or line-delimited harness, but production transport still needs a final decision such as WebSocket over TLS, HTTP/2 streaming, or another authenticated channel. Enrollment token rotation, server-side daemon ownership, multi-tenant authorization, browser UI workflows, and UI data retention are also deferred. Full session-event streaming should be designed after the first slice proves liveness and command/result correlation.
