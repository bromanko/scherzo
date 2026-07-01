# UI-managed daemon launch grants

This ExecPlan v2 review document is the human review surface for LIV-1335. It plans a later implementation that lets the Scherzo UI start a local Scherzo daemon with a short-lived launch grant; mechanical steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo. This issue is planning only and must not implement Core or UI/server changes.

## Purpose / Big Picture

The UI should be able to start a local Scherzo daemon and have that daemon connect back to the UI without asking an operator to paste a pairing code or mutate `ui_server` YAML. After the later implementation, a UI-owned launch attempt creates a short-lived grant file, starts `scherzo` with a grant-file reference, receives the daemon's normal `daemonId` and per-boot `bootId` in `daemon_hello`, and can show precise startup failures such as `instance_lock_held` instead of a generic offline state.

Durable enrollment remains available through `scherzo connect`. This plan adds a transient, UI-owned startup session for local launches; it does not replace operator-managed persistent pairing.

## Problem Framing and Constraints

Today, remote UI connectivity is configured through `ui_server` YAML plus `scherzo connect`, which stores a durable daemon credential outside project YAML. That path is correct for persistent operator enrollment but too heavy for a UI that just launched a local daemon and already owns the launch session.

The Core command-line shape for this later implementation is `scherzo --managed-launch-grant-file <grant.json> --managed-launch-status-file <status.json> [path-to-scherzo.yaml]`. Both managed-launch flags are daemon-mode only and must be supplied together; `--once`, `doctor`, `ctl`, `connect`, workflow, cleanup, schedule, artifact, workstream, and state commands reject them. `scripts/scherzo-launcher` must keep routing this managed-launch daemon invocation through `scripts/scherzo-start-runner` so terminal shutdown behavior remains unchanged.

The launch grant must carry the minimum authority needed for that session. The v1 JSON grant fields are `version: 1`, `launchId`, `endpoint`, `credential`, optional `daemonLabel`, `capabilities`, `commandBridgeEnabled`, and `expiresAt`. `expiresAt` is an RFC 3339 UTC timestamp, and the UI/server should create grants with a maximum five-minute lifetime unless product policy explicitly chooses a shorter value. The supported capability vocabulary for this plan is `state`, `query`, and `command`: `state` permits hello, heartbeat, daemon state, and work-item invalidation frames; `query` permits server query requests; `command` permits remote operator commands when `commandBridgeEnabled` is also true. The credential is a secret and must not appear in argv, environment variables, project YAML, logs, status files, durable credential stores, or argv-derived diagnostics. The daemon keeps owning stable `daemon_id` through `.scherzo-state/daemon_identity.json`; the launch grant must not override daemon identity. The UI/server updates any project binding only after it sees a valid hello from the daemon.

Capabilities are an authorization ceiling. Project YAML must never enable a command bridge, query bridge, or other managed-launch capability that the grant does not allow, and the UI/server must enforce the same ceiling before it sends privileged frames. Grant-file handling is security-sensitive: the file must live in a private directory where possible, be a same-owner regular file with private permissions, avoid symlink surprises, be read once, be unlinked or deleted immediately when possible, and keep the credential only in memory.

## Strategy Overview

Add an additive managed-startup mode beside durable pairing. The UI/server creates a short-lived grant record and writes a matching local grant JSON file under a private temporary directory. It starts `scherzo` with `--managed-launch-grant-file` and a non-secret `--managed-launch-status-file`. Core reads and validates the grant before normal daemon startup, reports redacted startup status, acquires the existing instance lock, loads or creates its stable daemon identity, and starts the existing outbound UI WebSocket client with an in-memory credential from the grant instead of the durable credential store.

The status-file schema is also v1 JSON and non-secret. It must contain `version: 1`, optional `launchId` after the grant is parsed, `phase`, `ok`, `code`, `message`, and `updatedAtMs`. The status writer must use atomic replacement, redact the credential from every field, and report startup failures such as `grant_invalid`, `grant_expired`, `grant_file_unsafe`, `instance_lock_held`, `daemon_identity_failed`, and `remote_client_start_failed` before the UI declares the child simply offline.

On the first authenticated WebSocket hello, the UI/server atomically binds `launchId` to the first valid `daemonId + bootId` pair. Reconnects from that same pair remain allowed until expiry or revocation; attempts from a different pair are rejected. Core includes enough non-secret launch context in hello for the server to verify the launch and update UI project binding after identity is known.

The approach is proportionate because it reuses the existing daemon identity, instance lock, WebSocket protocol, remote command/query handling, and redaction model. It adds a transient credential source and startup status reporting instead of creating a second daemon identity system or rewriting durable pairing.

## Alternatives Considered

Putting the credential directly in argv or an environment variable would be simpler, but it would expose the secret through process listings, shell history, crash diagnostics, logs, or child process environments. The grant-file reference keeps the secret out of those channels.

Mutating `ui_server` YAML from the UI would reuse durable pairing machinery, but it would dirty project configuration, blur local launch sessions with persistent enrollment, and make cleanup ambiguous. Durable YAML activation stays behind `scherzo connect --activate`.

Letting the grant override `daemon_id` would make server-side binding easier, but it would split daemon identity between Core and the UI and create migration risk for existing control/query state. Core remains the source of stable daemon identity.

Using the durable credential store for launch grants was rejected because launch grants are short-lived, single-use, and UI-owned. They should expire or revoke without leaving persistent credentials behind.

## Risks and Countermeasures

The largest risk is credential leakage. Counter this by forbidding secret argv/env/YAML/status fields, adding redaction tests for launch credentials, reading the grant once, retaining the credential only in memory, and making unlink/delete behavior part of acceptance.

A second risk is local privilege confusion through a loose or replaced grant file. Counter this with strict file checks for private parent directory permissions where the platform exposes them, same owner, regular file type, no symlinks, no group/world permissions, open-and-verify behavior, and negative tests for unsafe files.

A third risk is accidentally enabling privileged UI commands. Counter this by treating grant capabilities as the ceiling in Core and in the UI/server, clamping command bridge enablement to the grant, and testing both Core-side rejection and server-side non-issuance/rejection when the grant lacks command capability.

A fourth risk is stale launch records or duplicate daemon ownership. Counter this with single-use server binding of `launchId` to `daemonId + bootId`, expiry/revocation checks on every connection, and first-class status reporting for `instance_lock_held` when another daemon already owns the workspace lock.

A fifth risk is breaking durable pairing. Counter this by keeping `scherzo connect`, `ui_server.credential_ref`, the durable credential store, and existing WebSocket behavior unchanged unless managed-startup options are present.

## Scope Boundaries

In scope for the later implementation are the Core managed-startup CLI shape, grant parsing and validation, secure grant-file read/unlink behavior, transient in-memory credential source for the existing UI WebSocket client, capability clamping, launch-aware hello metadata, redacted status-file reporting, and UI/server behavior for grant creation, single-use binding, capability enforcement, status display, expiry, and revocation. Documentation and helper migration is limited to the surfaces that expose or route managed daemon launch: `src/scherzo/main.gleam`, `scripts/scherzo-launcher`, `scripts/scherzo-start-runner` only if the existing route helper requires adjustment, their tests, and operator-facing docs such as `docs/runbooks/ui-server-pairing.md` or a new managed-launch runbook.

Out of scope are browser UI polish, replacing `scherzo connect`, changing durable credential-store semantics, storing launch credentials durably, overriding daemon identity, changing tracker dispatch behavior, changing workstream/workflow schemas, changing ExecPlan helper materialization, changing provider-live/cache behavior, and implementing the feature in this planning issue.

## Milestones

Milestone 1 defines the managed launch contract in Core. Reviewers should see new Core contract modules, for example under `src/scherzo/managed_launch/`, plus tests under `test/` that lock the v1 grant schema, v1 status schema, `--managed-launch-grant-file` and `--managed-launch-status-file` parser behavior, capability vocabulary, expiry/revocation semantics, and the rule that Core owns `daemon_id` while the UI/server binds after hello. This milestone is independently verifiable by targeted grant/status/parser tests and by `test/main_test.gleam` proving the managed-launch command still routes as daemon mode.

Milestone 2 implements Core grant ingestion and status reporting before the normal daemon startup path. Reviewers should see strict grant-file security checks, one-time read and best-effort unlink/delete, redacted atomic status writes for grant and lock failures, no credential persistence, no secret argv/env use, and a deterministic `instance_lock_held` status-file test. This milestone is independently verifiable before remote-client wiring because malformed, expired, unsafe, and lock-held launches can fail with redacted status output.

Milestone 3 wires the transient credential into the existing daemon remote client. Reviewers should see managed-startup effective UI settings built from the grant, durable pairing left unchanged, hello metadata carrying `launchId`, effective capabilities, and optional label, and command/query capability clamping enforced before remote commands or queries run. This milestone is independently verifiable by WebSocket protocol/client tests that prove durable credentials still work and managed credentials remain in memory only.

Milestone 4 implements UI/server launch-grant lifecycle in the UI/server codebase that owns `/api/daemons/ws`. Reviewers should see secure grant-file creation, process launch without secret argv flags, atomic single-use binding of `launchId` to the first valid `daemonId + bootId`, reconnect acceptance for the same pair, rejection for different pairs, expiry/revocation behavior, server-side command/query ceiling enforcement, and project binding updates after hello. If that codebase is not available to the implementing agent, the agent must stop before claiming this milestone complete and record the missing repository as blocked evidence rather than silently omitting server work.

Milestone 5 proves failure and recovery behavior. Reviewers should see deterministic automated evidence for `instance_lock_held`, unsafe grant files, expired grants, duplicate credentials, capability denial, server rejection, process exit before hello, grant cleanup, redacted UI-facing status, and idempotent retry with a fresh `launchId`. The instance-lock startup check is a pre-publish requirement; browser rendering and full UI dogfood may be deferred only as explicitly recorded post-implementation operator evidence.

Milestone 6 completes docs/helper migration and full repository validation. Reviewers should see the runbook or docs updated for managed launch, launcher/helper routing tests updated if flags affect the route classifier, focused tests plus full Core validation commands passing before publish, and explicit evidence that workflow schemas, ExecPlan helper materialization, provider-live behavior, and cache behavior were not changed.

## Progress

- [x] (2026-07-01 08:00Z) Read the repository-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md` and used it as the authoritative planning guidance.
- [x] (2026-07-01 08:00Z) Inspected the current Core surfaces for durable pairing, UI WebSocket client behavior, daemon identity, instance locking, CLI routing, launcher scripts, `ui_server` config, and related tests.
- [x] (2026-07-01 08:00Z) Authored this concise review document and delegated mechanical implementation detail to the structured implementation pack for LIV-1335.
- [x] (2026-07-01 08:30Z) Incorporated review feedback by making the managed-launch CLI shape, grant/status schemas, milestone evidence, docs/helper migration boundary, provider-live/cache non-goals, manual dogfood timing, full validation, and linting obligations explicit in the review document and updated implementation pack.

## Surprises & Discoveries

The repository already has a clean split between durable daemon identity and per-boot identity: `src/scherzo/daemon_identity.gleam` persists only `daemon_id` and generates a fresh `boot_id` on each load. That matches the launch-grant binding model and avoids identity override work.

The existing outbound UI client already sends `daemon_hello`, `heartbeat`, `daemon_state`, command results, query responses, and work-item invalidations using a credential passed only in memory to the WebSocket FFI. The main Core gap is therefore credential sourcing and launch metadata, not a new transport.

The current daemon acquires the workspace instance lock before starting the long-running actor, and `instance_lock_held` is already the mapped startup code. Managed startup mainly needs to surface that code through a redacted status file before the UI declares the launched daemon simply offline.

## Decision Log

- Decision: Add managed startup as an additive transient path rather than changing `scherzo connect`. Rationale: Durable pairing remains the right operator-managed enrollment path, while UI launches need no YAML mutation or durable credential. Date: 2026-07-01.
- Decision: Pass a grant-file reference and a non-secret status-file reference, never the credential itself, on the command line. Rationale: argv and environment variables are easier to leak than a private, one-time file. Date: 2026-07-01.
- Decision: Keep Core as the sole owner of stable `daemon_id`; the UI/server binds after hello. Rationale: The current identity model already supports stable daemon identity plus fresh boot identity and should not be forked. Date: 2026-07-01.
- Decision: Treat grant capabilities as the authorization ceiling in both Core and UI/server. Rationale: Defense in depth prevents either YAML or server bugs from enabling commands that the launch grant did not authorize. Date: 2026-07-01.
- Decision: Use a non-secret status file for managed-startup failures. Rationale: The UI needs structured startup evidence, especially for `instance_lock_held`, before the daemon can send WebSocket hello. Date: 2026-07-01.
- Decision: Standardize the Core CLI as `--managed-launch-grant-file` plus `--managed-launch-status-file`, supplied together on daemon launches. Rationale: The flag names make the file indirection explicit, keep secrets out of argv, and give `scripts/scherzo-launcher` a deterministic daemon-mode route to test. Date: 2026-07-01.
- Decision: Treat docs/helper migration as a narrow obligation for managed-launch docs and daemon-route helpers only, while leaving ExecPlan materialization helpers and provider-live/cache behavior unchanged. Rationale: The launch feature needs operator guidance and safe launcher routing, but unrelated workflow/helper/cache changes would expand blast radius. Date: 2026-07-01.
- Decision: Require automated Core evidence for startup failure, redaction, capability, and full validation before publish, while allowing browser/UI dogfood evidence to be recorded as post-implementation operator evidence when the UI/server repository is unavailable in the implementing workspace. Rationale: Security-sensitive Core behavior must be proven before merging; visual dogfood depends on an external codebase and should not be falsely marked complete. Date: 2026-07-01.

## Outcomes & Retrospective

This plan has not been implemented yet. The expected outcome is a safe UI-owned launch session that can connect a local daemon back to the UI with a short-lived credential, preserve existing durable pairing behavior, and give operators clear redacted failure information when startup fails before hello.

The main intentional gaps are that browser polish, full dogfood flows, and any long-lived enrollment UX remain follow-up work after the Core and UI/server launch-grant contract is implemented and validated.

## Validation and Acceptance

Acceptance outcome 1 is a verifiable Core grant contract. Evidence must include automated tests for valid grants, missing required fields, invalid endpoint, empty credential, unsupported capability, expired grant, malformed `expiresAt`, and redaction proving the credential does not appear in logs, status JSON, YAML, durable credential stores, or argv-derived diagnostics.

Acceptance outcome 2 is secure grant-file handling. Evidence must include tests or platform-specific integration checks for same-owner regular files, `0600` file permission acceptance, loose permission rejection, symlink rejection, non-regular-file rejection, unsafe parent directory rejection where permission metadata is available, one-time read, and unlink/delete after read or a recorded safe platform-specific cleanup fallback.

Acceptance outcome 3 is correct identity and binding behavior. Evidence must include Core tests showing the grant cannot override `daemon_id`, hello carries the stable `daemonId`, fresh `bootId`, `launchId`, effective capabilities, and optional label, plus UI/server tests showing single-use binding to the first valid pair, reconnect acceptance for the same pair, rejection for a different pair, expiry rejection, and revocation rejection.

Acceptance outcome 4 is capability safety. Evidence must include Core tests showing YAML or defaults cannot enable command bridge when the grant lacks command capability or `commandBridgeEnabled` is false, and UI/server tests showing commands are not sent or are rejected when outside the launch grant ceiling.

Acceptance outcome 5 is managed-startup failure reporting. Evidence must include a pre-publish automated or manual integration check where an existing daemon holds the instance lock, a UI-managed launch exits before hello, the status file contains redacted `instance_lock_held`, and the UI/server renders a degraded/failure state rather than plain offline.

Acceptance outcome 6 is docs/helper migration without workflow-cache drift. Evidence must include an updated operator runbook such as `docs/runbooks/ui-server-pairing.md` or a new managed-launch runbook, `test/main_test.gleam` coverage showing managed-launch arguments parse as daemon mode and direct/offline commands reject those flags, and launcher/helper evidence showing `scripts/scherzo-launcher` still delegates daemon launches to `scripts/scherzo-start-runner`. The implementation must also record that `.scherzo/workflows`, ExecPlan helper materialization, provider-live behavior, and cache behavior were not changed.

Acceptance outcome 7 is full Core validation before publish: from the repository root, run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting zero exits. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands. Browser/UI dogfood may be deferred only if the implementation task records exact post-implementation manual evidence to collect; the `instance_lock_held` launch failure check is not deferred and must be completed before publish.

## Rollout, Recovery, and Idempotence

Rollout is additive and opt-in. Existing daemon mode, `scherzo connect`, `ui_server` YAML, durable credentials, local control, and remote query/command behavior continue unchanged unless managed-startup CLI options are present. A feature flag or hidden UI preference may gate UI launch while Core support is new.

Recovery is to revoke the launch grant, delete any remaining grant/status files, terminate the child process if needed, and start a fresh launch with a new `launchId` and credential. If the instance lock is held, the UI should offer to attach to or inspect the existing daemon rather than retrying with the same grant. If capability enforcement causes trouble, disable command bridge in grants while leaving read-only hello/state/query behavior available.

The implementation must be idempotent under retries. Creating a second launch creates a distinct grant; reusing a consumed credential for a different `daemonId + bootId` fails; reconnecting the same daemon boot before expiry succeeds; status writes are atomic replacements; cleanup can be rerun safely; and no launch secret is stored durably.

## Open Questions and Clarifications Needed

No blocking clarification is required before implementation. The UI/server repository location is not present in this Core checkout, so the implementation task should apply the server-side steps in the UI/server codebase that owns `/api/daemons/ws` and record the exact files changed there. A follow-up product decision can add capabilities beyond the v1 `state`, `query`, and `command` vocabulary after the first UI-managed launch dogfood run.
