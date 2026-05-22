# Daemon identity and disabled UI/server configuration

## Purpose / Big Picture

This plan creates the small identity and configuration layer needed before any central UI/server control plane can be safely connected. After implementation, Scherzo will have a stable daemon identifier persisted under the workspace state directory, a fresh boot identifier for each process lifetime, and a disabled-by-default `ui_server` configuration section whose credentials are separate from local `scherzoctl` control tokens. The observable result is test evidence, documentation, and a pre-publish scope audit; this slice must not produce a working network connection, browser UI, or live remote-server dogfood transcript.

## Problem Framing and Constraints

The failed monolithic LIV-483 effort tried to deliver identity, configuration, transport, lifecycle, command routing, and harness behavior together. This slice deliberately avoids that failure mode. It is blocked by the preceding codec/envelope foundation ticket, LIV-492, and implementation should begin only after that foundation is merged or otherwise present in the implementation branch. Before coding, the implementer must verify that the LIV-492 shared command/result codec and remote envelope foundation exist; if they do not, stop rather than reimplementing or bypassing that prerequisite in this ticket.

The plan must specify exact persistence behavior. The daemon identity file is `workspace.root/.scherzo-state/daemon_identity.json`. On first boot or when the file is missing, the helper creates `.scherzo-state`, generates a new stable `daemon_id`, writes versioned JSON, and returns it with a separate fresh `boot_id`. On repeated boot, it reuses the persisted `daemon_id` and generates a different `boot_id`. If the identity file exists but is malformed, has an unsupported version, lacks a non-empty `daemon_id`, or contains an invalid id shape, the helper returns a validation error and must not silently overwrite the file; deleting or repairing the file is an explicit operator recovery action.

The implementation should use a concrete id shape so corruption is testable: `daemon_id` should be `daemon_` followed by 32 lowercase hexadecimal characters, and `boot_id` should be `boot_` followed by 32 lowercase hexadecimal characters. The exact random source may reuse the existing private-token generation boundary in `src/scherzo/control/file.gleam` plus hashing, but local control tokens themselves must never be persisted or interpreted as central daemon identity.

## Context and Orientation

Configuration is resolved in `src/scherzo/config.gleam`, with public config types in `src/scherzo/config/types.gleam`. `EffectiveConfig` currently contains tracker, polling, workspace, hooks, agent, pi, handoff, linear contract, and linear command sections; this work adds a `ui_server` section there and updates `resolved_secrets` so UI/server enrollment material follows the existing redaction path.

Local control is implemented under `src/scherzo/control/`. The local control file helper in `src/scherzo/control/file.gleam` writes `workspace.root/.scherzo-state/control.json`, binds to loopback, uses a per-process token, and supports `SCHERZO_CONTROL_FILE` discovery for `scherzoctl`. That file is not durable daemon identity and is not a central-server credential.

Durable workflow state already lives below `workspace.root/.scherzo-state/`, especially the ledger in `src/scherzo/state/ledger.gleam` and artifacts in `src/scherzo/state/artifact_store.gleam`. The new identity helper should be a small sibling state helper, not a ledger record migration and not a provider-live/cache behavior change.

## Strategy Overview

Keep the work as pure primitives. Add a focused daemon identity module for loading, validating, and creating `daemon_identity.json`; add a config model and resolver for `ui_server`; add safe debug/redaction evidence through the existing resolved-secret and log-redaction path; and document how this central identity/auth material differs from `control.json`. The implementation should not start a socket, open a WebSocket, run an outbound loop, create a server harness, route pause/resume commands from a server, alter pi provider live/retry/cache behavior, or change token accounting semantics.

The `ui_server` section should default to disabled with no required endpoint or credentials. When enabled, it should require an HTTPS endpoint and an enrollment/auth secret resolved from an explicit environment-variable field named `enrollment_token_env`; that secret must join `config.resolved_secrets` and appear only as `[REDACTED]` in any config/debug/log summary. It must never be read from `.scherzo-state/control.json`, `SCHERZO_CONTROL_FILE`, host/port loopback fields, or local control-token values.

## Alternatives Considered

One alternative is to carry forward the larger LIV-483 control-plane design. That is rejected because it would reintroduce live transport, liveness, harness, and command-routing risk before the identity/config boundary is proven.

Another alternative is to reuse the local `control.json` token as central auth material. That is rejected because `control.json` is local loopback discovery for `scherzoctl`, rotates per daemon process, contains host/port assumptions, and is not a stable daemon identity or central server credential.

A third alternative is to store enrollment credentials beside `daemon_identity.json`. That is rejected for this slice. The durable state file should contain only non-secret identity metadata; server auth secrets should come from configuration/environment and be redacted through the existing secret-redaction path.

## Risks and Countermeasures

The main safety risk is accidental identity churn. The countermeasure is explicit first-boot, repeated-boot, missing-file, invalid-shape, unsupported-version, and corrupt-file tests, with corrupt data failing closed rather than regenerating a new daemon id. Acceptance evidence must show that the corrupt file contents remain unchanged after the failed load.

The main security risk is credential confusion between local control and central server auth. The countermeasure is separate config fields, tests that local `control.json` tokens and `SCHERZO_CONTROL_FILE` are not used as server auth, redaction tests for resolved UI/server secrets, and documentation of the boundary.

The main docs/helper migration risk is leaving operators with examples that imply `scherzoctl` control tokens are central credentials or that a live UI/server is already available. The countermeasure is to update the relevant operator-facing docs, at minimum `docs/ARCHITECTURE.md` and any getting-started/config example touched by the new section, while preserving wording that `ui_server` is disabled by default and does not connect anywhere in this slice.

The main scope risk is accidentally implying a working live control plane or changing unrelated provider behavior. The countermeasure is a required pre-publish diff/scope audit proving there is no outbound connection loop, socket/WebSocket/TCP implementation, server harness, server-originated pause/resume routing, liveness registry, provider-live retry/cache behavior change, token accounting change, or product documentation claiming a live UI/server control plane.

## Scope Boundaries

In scope are the persisted `daemon_id` primitive at `workspace.root/.scherzo-state/daemon_identity.json`, fresh in-memory `boot_id` generation, disabled-by-default `ui_server` config, enabled-config validation, server-auth redaction, tests for local-control separation, and documentation of the local-control versus central-identity distinction.

Out of scope are outbound transport, sockets, WebSockets, TCP, server harnesses, command routing from a server, heartbeat/liveness registries, UI integration, pi provider live/cache behavior, token accounting behavior, and any documentation claiming that Scherzo can already connect to a central UI/server. The only allowed state helper beyond existing Scherzo state infrastructure is the identity file helper.

## Milestones

Milestone 0 is prerequisite verification. Its outcome is a short implementation note or commit message confirming that the branch contains the LIV-492 codec/envelope foundation and identifying the files that prove it. If the foundation is absent, no LIV-493 implementation work should proceed.

Milestone 1 establishes identity persistence. Its outcome is `src/scherzo/daemon_identity.gleam` and `test/daemon_identity_test.gleam`, with tests proving first boot writes `daemon_identity.json`, missing-file boot recreates it, repeated boot preserves `daemon_id` while changing `boot_id`, generated ids match the chosen shape, and malformed existing files fail without overwrite.

Milestone 2 establishes disabled-by-default configuration. Its outcome is an `EffectiveConfig.ui_server` value that defaults disabled, requires no credentials while disabled, rejects malformed enabled config with clear `InvalidConfig` messages, and accepts enabled config only when `endpoint` is HTTPS and `enrollment_token_env` names an environment variable that resolves to a non-empty secret.

Milestone 3 establishes secret separation and redaction. Its outcome is test evidence that enrollment/server auth comes only from explicit UI/server config/environment, is included in the resolved secret list, is redacted from config/debug/log summaries, and never comes from local `control.json` tokens, `SCHERZO_CONTROL_FILE`, host, port, or workspace-root fields.

Milestone 4 updates documentation and performs scope validation. Its outcome is documentation that distinguishes local `scherzoctl` control material from central daemon identity/auth material, plus gate evidence that targeted tests, full tests, formatting, glinter, Scherzo lint, and the no-live-transport/provider-cache scope audit pass. The diff/scope audit is a pre-publish manual check; browser checks, live-server checks, and dogfood operator checks are deferred to later tickets that add transport or UI behavior.

## Progress

- [x] (2026-05-22) Confirmed the prepared review-doc target is `docs/plans/`.
- [x] (2026-05-22) Reviewed current config, state, local control, architecture, and LIV-492 plan context needed to frame this slice.
- [x] (2026-05-22) Authored this concise human-reviewable plan and separated mechanical implementation instructions into the structured implementation pack.
- [x] (2026-05-22) Incorporated review feedback by making acceptance evidence, test obligations, milestone acceptance, docs/helper migration, provider-live/cache non-goals, manual-check timing, and full validation explicit.

## Decision Log

- Decision: Persist daemon identity in `workspace.root/.scherzo-state/daemon_identity.json` rather than in `control.json` or the append-only ledger.
  Rationale: `control.json` is local and per-process, while the ledger is for operational workflow records; a small versioned identity file is easier to validate, repair, and keep free of secrets.
  Date: 2026-05-22

- Decision: Treat malformed existing identity data as an error instead of silently regenerating.
  Rationale: Silent regeneration would create central identity churn and could make a real deployment look like a different daemon without operator intent.
  Date: 2026-05-22

- Decision: Keep UI/server credentials in config/environment and include them in the existing resolved-secret redaction path.
  Rationale: The identity file should not store secrets, and the future central auth path must remain distinct from local `scherzoctl` control tokens.
  Date: 2026-05-22

- Decision: Classify only the diff/scope audit as required manual pre-publish evidence for this slice.
  Rationale: This ticket deliberately adds pure identity/config primitives without live transport, so browser checks, live-server checks, and dogfood operator checks would be impossible or would encourage scope creep into later tickets.
  Date: 2026-05-22

## Validation and Acceptance

Implementation acceptance requires concrete evidence, not assertions. The repository test runner is `direnv exec . gleam test`; it runs the default unit suite through `test/scherzo_test.gleam`, so the identity and config assertions should be added to `test/daemon_identity_test.gleam` and `test/config_test.gleam` and verified through that command. The tests must prove stable `daemon_id`, fresh `boot_id`, missing-file creation, invalid-shape and corrupt identity rejection without overwrite, default-disabled `ui_server`, enabled-config validation errors, secret resolution from `enrollment_token_env`, distinct server-auth material, and redaction through the resolved-secret/log path.

Full validation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If a gate fails for a pre-existing unrelated reason, the implementer must capture the exact command output and explain why the new identity/config changes are not the cause before handoff; otherwise all gates must pass.

The final implementation evidence must also include a pre-publish manual diff/scope audit, such as `git diff --name-only` plus a focused review of changed files, showing that the change did not add an outbound connection loop, socket/WebSocket/TCP transport, server harness, server-originated pause/resume routing, liveness registry, pi provider-live/cache behavior changes, token accounting changes, or product docs claiming a live UI/server control plane. No browser, live server, or dogfood operator evidence is required before publish for this pure primitive slice; those checks are deferred to later transport/lifecycle tickets.

## Rollout, Recovery, and Idempotence

Runtime rollout is inert because this plan adds primitives and configuration only. With `ui_server.enabled` absent or false, existing daemon behavior, pi provider behavior, cache/token accounting, and local `scherzoctl` control continue unchanged. Repeated identity loads are idempotent for a valid state file: the same `daemon_id` is reused and only the in-memory `boot_id` changes.

If identity creation fails before writing a valid file, rerunning after fixing filesystem permissions is safe. If a malformed identity file exists, the helper must fail without overwriting it; recovery is to inspect, restore, repair, or explicitly delete `workspace.root/.scherzo-state/daemon_identity.json`, accepting that deletion creates a new central daemon identity on the next load. Disabling or removing the `ui_server` config section reverts the configuration change without touching the identity file.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this slice. Later tickets should decide the exact remote transport, enrollment exchange, server-side liveness semantics, command routing rules, browser/operator UI, dogfood procedure, and any operator UI for repairing or rotating central daemon identity/auth material.
