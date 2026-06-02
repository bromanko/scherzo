# LIV-801 Remote daemon registration client

This review document summarizes the ExecPlan direction for Scherzo Core pairing with the Scherzo UI server. Mechanical implementation detail is submitted separately through the structured implementation-pack workflow output.

## Purpose / Big Picture

After this change, an operator can create a pairing invite in the UI server, run `scherzo connect` in a Scherzo Core checkout, and have the daemon persist a durable per-daemon credential without putting that credential in project YAML. When daemon mode starts with remote control enabled, it uses the stored credential to maintain an outbound WebSocket to the UI server, send hello/heartbeat/state messages, retry safely when the server is unreachable, and keep local `scherzoctl` as the fallback.

The visible outcome is a safe registration client, not a broad command bridge. The follow-up implementation should prove pairing, credential storage, WebSocket liveness, local-versus-remote URL behavior, and the negative error paths before any UI-originated command/result bridge is enabled against the real UI server.

Review feedback for this revision is part of the delivery contract: acceptance evidence, exact test obligations, milestone proof anchors, pre-publish versus deferred dogfood checks, docs/helper boundaries, provider-live/cache non-scope, full validation, and lint gates must appear in both this review document and the structured implementation-pack submission.

## Problem Framing and Constraints

Scherzo Core currently has a disabled-by-default `ui_server` configuration and a loopback/demo remote client that uses an environment enrollment token and line-delimited TCP envelopes. The UI backend from LIV-788/LIV-792 now expects a different production boundary: one-time pairing tokens are exchanged over HTTP for durable `dcred_...` daemon credentials, and registered daemons authenticate `GET /api/daemons/ws` with `Authorization: Bearer <daemon credential>` before sending UI-specific WebSocket JSON messages.

The Core plan must bridge that gap without modifying the `scherzo-ui` repository. It must not write raw daemon credentials, pairing tokens, or generated connect commands into `.scherzo/scherzo.yaml`, repo-local `scherzo.yaml`, logs, checked-in fixtures, or command-result artifacts. Repository config may contain only non-secret remote-control settings such as the server base URL and a credential reference; the raw credential must live in an OS keychain backend or an owner-only token file outside the project tree.

Local control remains separate. `workspace.root/.scherzo-state/control.json` and `SCHERZO_CONTROL_FILE` are still loopback-only `scherzoctl` discovery material, not UI-server identity or credential storage. Loopback advertised UI URLs are valid only when Core runs on the same host as the UI server; using `localhost` or `127.0.0.1` from a different host must fail with an operator-facing explanation instead of a generic retry loop.

The implementation must also preserve unrelated runtime surfaces. It must not change provider-live probes, provider cache keys or TTLs, token accounting, workflow helper scripts, review-lane structured-output helpers, or hand-written canonical bundle generation; if implementation discoveries make any of those surfaces necessary, the work should be split or rolled back before LIV-801 is accepted.

## Strategy Overview

Keep the change additive and opt-in. Replace the current enrollment-token model with a remote-control config that names a normalized UI server base URL, a credential reference profile, reconnect/heartbeat timing, and a command-bridge flag that defaults to disabled. The existing stable daemon identity file remains the daemon id source; `scherzo connect` loads or creates that identity, exchanges a one-time pairing token with `/api/daemons/pairing-exchanges`, and persists the returned credential in an owner-only file or keychain item keyed by server URL and daemon id.

Add a UI-compatible protocol boundary for the WebSocket path instead of stretching the old line-socket harness protocol. The daemon should derive `wss://.../api/daemons/ws` from an HTTPS UI base URL and `ws://.../api/daemons/ws` only for loopback HTTP development URLs. On connection it authenticates with the durable credential, sends `daemon_hello`, follows the server heartbeat interval when provided, emits `heartbeat` and `daemon_state`, treats credential revocation as a stop-and-repair condition, and treats ordinary network failures as bounded reconnect conditions.

Command/result bridging remains out of scope for enablement. The implementation may preserve existing local harness tests, but the UI-server client must not advertise or execute UI-originated commands unless a later command-bridge plan enables that flag with dedicated idempotency, authorization, duplicate, revocation, and documentation evidence.

## Alternatives Considered

Continuing to use `ui_server.enrollment_token_env` was rejected because the UI backend now issues per-daemon durable credentials after one-time pairing; an enrollment token in environment would be a stale shared-secret model.

Writing the returned daemon credential directly into `.scherzo/scherzo.yaml` was rejected because project config is often checked in, copied to workspaces, or displayed in diagnostics. A non-secret credential reference plus keychain or owner-only file storage gives the daemon durable auth without turning config into a secret store.

Reusing the existing loopback line-socket protocol was rejected because the UI server accepts WebSockets at `/api/daemons/ws` and uses `daemon_hello`, `heartbeat`, `daemon_state`, `server_hello`, and `server_command` JSON shapes. Keeping the old protocol for harness tests is acceptable only if the production UI client is explicitly separated.

Enabling command/result bridge work in the same slice was rejected because pairing, credential persistence, WebSocket auth, revocation, and reconnect semantics are enough risk for one release step.

## Risks and Countermeasures

The main security risk is raw credential leakage. Counter it with file permissions or keychain storage, redaction tests, config tests proving raw credentials are rejected, and grep/diff evidence that raw `pair_...` or `dcred_...` values appear only in synthetic tests.

The main usability risk is a generated loopback URL being copied to another host. Counter it by normalizing advertised URLs, rejecting `0.0.0.0`, allowing HTTP only for loopback, classifying loopback connection failures as `loopback_url_wrong_host`, and documenting that remote daemons require a reachable HTTPS advertised URL from the UI server.

The main availability risk is a retry storm or a remote outage blocking local daemon work. Counter it with bounded exponential backoff, deterministic timer tests, clear unreachable-server status logs, and tests proving daemon startup, polling snapshots, and local `scherzoctl` still work while the UI server is down.

The main migration risk is accidentally leaving the old command bridge or enrollment-token path active. Counter it with config migration errors, capability tests showing the UI WebSocket does not advertise command execution by default, and a docs checklist of bridge-specific tests required before enabling server-originated commands later.

A review/pack drift risk is that the prose document can demand acceptance evidence that the structured implementation pack does not ask a later implementer to collect. Counter it by mirroring targeted tests, fake-server transcripts, manual/dogfood timing, docs/helper inventory, provider-live/cache non-scope, full validation, and lint gates in the implementation pack before Scherzo materializes follow-up artifacts.

## Scope Boundaries

In scope are Core-only changes for remote-control config resolution, `scherzo connect`, pairing exchange HTTP client behavior, durable credential persistence, UI WebSocket auth/liveness/reconnect, loopback-versus-remote URL diagnostics, redaction, operator-facing docs, tests, and validation evidence.

Docs work is limited to Scherzo Core operator-facing guidance such as `docs/GETTING_STARTED.md`, `docs/ARCHITECTURE.md`, and, if needed, the simplified YAML spec or a small runbook that explains pairing, secret storage, loopback caveats, and local `scherzoctl` fallback. Helper work is limited to mechanical test-helper additions required for fake HTTP/WebSocket integration tests. The implementation must record a scope inventory saying whether `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache code changed; the expected answer is no.

Out of scope are modifications to the `scherzo-ui` repository, React pairing UI changes, UI OpenAPI edits, durable command storage, enabling UI-originated command execution, replacing local `scherzoctl`, moving local `control.json`, provider-live/cache behavior, token-accounting behavior, workflow helper migrations, browser UI work, production provider dogfood, and hand-written canonical ExecPlan bundles.

## Milestones

Milestone 1 defines the Core config and secret-storage boundary. The outcome is a disabled-by-default remote-control config that accepts only non-secret server URL and credential-reference fields, rejects raw credential fields, removes the old enrollment-token requirement from the forward path, and resolves secrets through a keychain or owner-only credential-file abstraction. Acceptance evidence is focused config and storage test output, expected from `direnv exec . gleam test test/config_test.gleam test/daemon_identity_test.gleam` plus the new credential-store tests, proving default-disabled behavior, raw-secret rejection, redaction, owner-only file permissions or keychain calls, daemon-id/server-url scoping, and no dependence on `control.json`.

Milestone 2 delivers `scherzo connect`. The outcome is a CLI command that loads the local daemon id, validates a UI control URL, posts the one-time pairing token to the UI pairing-exchange endpoint, stores the returned durable credential once, prints a non-secret success summary, and emits clear errors for expired/invalid tokens, unreachable servers, invalid URLs, and loopback URLs used from the wrong host. Acceptance evidence is targeted CLI and HTTP-client test output, expected from a new `test/ctl_connect_test.gleam` and focused remote-registration client tests, covering success, redacted pretty and JSON output, expired/invalid/already-consumed tokens, unreachable server responses, `0.0.0.0` rejection, HTTPS-versus-loopback HTTP policy, and explicit replace/idempotency behavior.

Milestone 3 delivers the UI WebSocket client. The outcome is a daemon-owned client that authenticates with the persisted credential, sends UI-compatible hello/heartbeat/state frames, follows heartbeat and reconnect timing, stops retrying on revoked credentials until the operator reconnects, and never blocks local operation. Acceptance evidence is fake-transport and fake-server output, expected from new UI WebSocket protocol/client tests and updated daemon remote-client tests, proving the `Authorization: Bearer <credential>` handshake, `daemon_hello`, `heartbeat`, `daemon_state`, heartbeat interval handling, bounded reconnect, revocation stop-and-repair behavior, log redaction, and continued local `scherzoctl` fallback while the UI server is down.

Milestone 4 completes bridge-gating, docs, helper inventory, dogfood timing, and validation. The outcome is documentation for setup, secret storage, URL caveats, local fallback, and the tests/docs still required before command/result bridging can be enabled. Required pre-publish evidence is a fake UI-server pairing/WebSocket integration transcript, the scope inventory showing no workflow helper/provider-live/cache/token-accounting migration, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and review-doc validation for this file. A real `scherzo-ui` smoke is pre-publish only when the environment can run that service; otherwise it is deferred as explicit human/operator evidence after handoff and before command-bridge enablement.

## Progress

- [x] 2026-06-02: Confirmed the prepared review document target is `docs/plans/`.
- [x] 2026-06-02: Inspected the Core config, daemon identity, existing remote client, CLI, docs, and relevant UI backend plan/API files in the read-only sibling repository.
- [x] 2026-06-02: Authored this concise review document and prepared the mechanical implementation detail for structured submission.
- [x] 2026-06-02: Incorporated review feedback by making acceptance evidence, test obligations, milestone proof anchors, pre-publish fake UI integration, deferred real UI dogfood, docs/helper boundaries, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Keep `ui_server` disabled by default and make the new credential reference non-secret. Rationale: remote control must be opt-in and must not turn project YAML into a credential store. Date: 2026-06-02.
- Decision: Use the existing stable `daemon_id` from `workspace.root/.scherzo-state/daemon_identity.json` for pairing exchange. Rationale: the UI backend expects a durable daemon identity, while each process still gets a fresh `boot_id` for connection liveness. Date: 2026-06-02.
- Decision: Implement a UI-compatible WebSocket client instead of adapting the old line-socket protocol. Rationale: the merged UI backend authenticates `/api/daemons/ws` and uses different JSON message names. Date: 2026-06-02.
- Decision: Defer command/result bridge enablement. Rationale: pairing, credential persistence, auth, revocation, heartbeat, and reconnect must be proven before remote mutation is safe. Date: 2026-06-02.
- Decision: Treat real UI-server dogfood as post-implementation human/operator evidence when the Core implementation environment cannot run `scherzo-ui`. Rationale: Core can ship with a fake UI-server integration proof, while cross-repo dogfood depends on external service setup and must not require modifying the UI repo. Date: 2026-06-02.
- Decision: Treat review feedback about evidence, tests, dogfood timing, docs/helper inventory, provider-live/cache boundaries, full validation, and linting as structured implementation-pack obligations. Rationale: Scherzo materializes follow-up implementation instructions from the pack, so prose-only obligations would be easy for later implementers to miss. Date: 2026-06-02.

## Validation and Acceptance

Planning acceptance for LIV-801 requires this file to remain at `docs/plans/LIV-801-remote-daemon-registration-client.md`, every required level-2 review-doc section to be present and non-empty, review-doc validation to report success for this path, and Scherzo to capture exactly one structured implementation-pack submission. The planning handoff must not manually write a canonical bundle or implement production code.

Config and storage acceptance requires tests proving default-disabled behavior, valid non-secret credential references, rejection of raw credential fields in YAML, rejection of legacy enrollment-token-only config on the forward path, owner-only file permissions or keychain calls, daemon-id/server-url mismatch detection, and redaction of `pair_...` and `dcred_...` values from debug summaries and logs. Targeted evidence should include `direnv exec . gleam test test/config_test.gleam test/daemon_identity_test.gleam` plus the new credential-store test file names created by the implementation.

`scherzo connect` acceptance requires focused CLI and HTTP-client tests for successful pairing exchange, persistent credential writing, no raw credential in stdout by default, explicit `--json` redaction, expired or invalid pairing token responses, already consumed token responses, unreachable server responses, invalid advertised URLs, `0.0.0.0` rejection, HTTP allowed only for loopback, loopback wrong-host diagnostics, and explicit idempotent same-credential versus replace-required behavior. These tests must run before daemon WebSocket work is considered complete, and their output or test names must be retained in the handoff.

WebSocket acceptance requires fake-server or fake-transport tests proving `Authorization: Bearer <credential>` is sent only in the handshake, the first frame is `daemon_hello` with stable `daemonId` and fresh `bootId`, `heartbeat` and `daemon_state` frames use the UI JSON shape, server heartbeat intervals are honored, send/read/close failures schedule bounded reconnect without blocking local control, revoked credential or revoked identity closes stop reconnecting with a repair message, and raw credentials do not appear in logs or transcripts. The pre-publish fake UI-server integration transcript must show pairing, credential storage, daemon startup, hello, heartbeat, state, reconnect after a simulated outage, and local `scherzoctl` fallback while the fake server is unavailable.

Bridge-readiness acceptance requires documentation and tests showing command/result execution is not advertised or applied by default. Before any later bridge enablement, the docs must require tests for command idempotency, duplicate conflicts, authorization separation, active revocation during command execution, result correlation, reconnect replay behavior, and a real UI-server dogfood transcript. This LIV-801 implementation must not claim those bridge checks are complete.

Docs/helper and scope acceptance requires operator-facing docs for pairing, credential storage, URL caveats, local fallback, and bridge non-goals. It also requires a diff/scope inventory stating whether `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, cache behavior, and token accounting changed. If any such surface changes, the implementation must run the relevant helper or offline contract tests and either prove provider-live/cache semantics are preserved or split that work out; if they do not change, the evidence must explicitly say no helper migration and no provider-live/cache validation were applicable.

Full validation acceptance requires `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-801-remote-daemon-registration-client.md`. Lint warnings are not a reason to skip these gates, and new production policy errors are unacceptable. A real `scherzo-ui` pairing/WebSocket smoke is pre-publish only when the environment can run that service; otherwise it is deferred as explicit human/operator evidence after handoff and before command-bridge enablement.

## Rollout, Recovery, and Idempotence

Rollout is opt-in because `ui_server.enabled` remains false by default. Operators first run `scherzo connect`, add or confirm the non-secret config reference, and then restart daemon mode. Local `scherzoctl` remains available whether the UI server is connected, unreachable, or disabled.

Pre-publish rollout proof is automated and local to Core: targeted tests, a fake UI-server integration transcript, full validation/lint gates, review-doc validation, and the docs/helper/provider-live/cache scope inventory. A live `scherzo-ui` smoke may be recorded before publish only when that service is runnable in the implementation environment. If not, the live smoke is deferred human/operator evidence after handoff and must be completed before a later command/result bridge is enabled; it is not an excuse to skip the fake UI-server integration proof.

Recovery from an expired pairing token is to create a new invite and rerun `scherzo connect`. Recovery from a revoked credential or daemon identity is to remove or replace the stored credential and pair again. Recovery from an unreachable server is to fix networking or disable `ui_server` while continuing local-only operation. Recovery from a wrong-host loopback URL is to run the command on the UI server host or configure the UI server with a reachable advertised HTTPS URL.

Credential-file writes are idempotent only for the same server, daemon id, and credential id; replacing a different credential requires an explicit replace path so accidental reruns do not silently overwrite usable credentials. Pairing-token exchange remains one-time by design, and repeated use of the same token must fail visibly. Because provider-live/cache behavior and workflow helper migration are out of scope, rollback should not require cache invalidation, provider-live cleanup, or helper-contract migration; if implementation accidentally adds those dependencies, split or revert them before publishing LIV-801.

## Open Questions and Clarifications Needed

No blocking clarification is needed for the Core plan. A later UI-side ticket may update the generated `connectCommand` string to call `scherzo connect` directly, but this Core implementation must work from the pairing token and advertised URL without modifying the UI repository.
