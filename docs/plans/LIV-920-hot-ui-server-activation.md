# Activate UI server pairing without daemon restart

## Purpose / Big Picture

After this change, an operator who pairs an already-running Scherzo daemon with the UI server can see the daemon come online without restarting Core. The observable success path is: run `scherzo connect --pairing-token <pair_...> --server-url <https-url>` after `ui_server.enabled: true` is configured, then run `scherzoctl query metrics --json` and see `ui_server_enabled: true` with `remote_client_status` no longer `disabled`.

## Problem Framing and Constraints

Today `scherzo connect` stores the durable daemon credential, but the running daemon does not re-read credential/config state, so the remote UI client remains disabled or retrying until the daemon process restarts. This is operator-visible breakage because the UI-generated pairing command appears to succeed while the daemon stays offline.

The solution must preserve local-only operation, keep durable UI credentials out of config and logs, avoid reusing local `control.json` as a remote secret, and avoid hot retry loops for revoked or invalid credentials. The repository already has a local loopback control plane, `scherzoctl reload`, daemon remote-client lifecycle code, credential storage, and metrics fields; this task should reuse those seams rather than introduce a broad new remote-control architecture.

## Strategy Overview

Make daemon reload reconcile the UI remote client lifecycle, then have `scherzo connect` ask the running local daemon to reload after successful credential storage when the local control file is available. Reload reconciliation should update `ui_server_enabled` in the read model, start the client when config is now enabled and a credential exists, stop it when config is disabled, and restart it on explicit reload so credential replacement is picked up.

This is the right size because it turns the existing operator command and local authenticated control plane into the activation mechanism. A dedicated `ui-server reconnect` command can remain unnecessary unless implementation discovers that overloading reload would break existing semantics.

Review feedback is part of the implementation contract, not optional commentary. The structured implementation pack must mirror the same acceptance evidence, named test obligations, manual/operator-check timing, docs/helper boundary, provider-live/cache non-scope, full validation, and lint gates so the materialized follow-up work cannot omit them.

## Alternatives Considered

Restarting the whole daemon after pairing is rejected because it preserves the current confusing operator workflow and interrupts local work unnecessarily.

Only teaching `scherzo connect` to start a remote client directly is rejected because the connect process is short-lived and does not own daemon runtime state, event-hub access, monitors, or shutdown.

Adding a new dedicated `scherzoctl ui-server reconnect` command is viable but deferred unless reload becomes too broad. Reload already means “re-read operator configuration now,” and pairing activation is a config/credential reload concern.

## Risks and Countermeasures

Secret leakage is the main safety risk. The countermeasure is to keep pairing tokens, durable credentials, and local control tokens out of stdout, JSON output, log fields, and test transcripts, and to add explicit no-secret tests for success and fallback paths.

A stale or unavailable local control file could turn successful pairing into a perceived failure. The countermeasure is for `scherzo connect` to keep credential storage successful and print a clear non-secret fallback message telling the operator to run `scherzoctl reload` or restart the daemon.

Remote-client churn could disrupt a healthy connection. The countermeasure is to reconcile only on daemon startup, explicit reload, or changed config reload, and to stop an existing client before replacing it so monitors and timers do not leak.

Invalid or revoked credentials could create retry storms. The countermeasure is to preserve the existing bounded retry and revoked-credential stop-for-repair behavior, and to add regression tests proving no immediate reconnect loop after revocation.

A planning risk is that review feedback could live only in this prose document while Scherzo later materializes implementation work from the structured pack. The countermeasure is to make the pack carry the same concrete steps, test files, manual-check timing, helper/provider/cache boundary, full validation commands, and lint commands before publication.

## Scope Boundaries

In scope are Core-only implementation changes to `src/scherzo/connect.gleam`, local-control discovery/request helpers under `src/scherzo/control/`, daemon reload handling in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/workflow_reloader.gleam`, read-model metrics under `src/scherzo/orchestrator/read_model.gleam` and `src/scherzo/control/query/`, targeted tests in `test/ctl_connect_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/orchestrator_daemon_remote_client_test.gleam`, and `test/control_remote_ui_websocket_client_test.gleam`, and operator documentation in `docs/runbooks/ui-server-pairing.md`.

In scope evidence also includes a changed-file inventory proving the implementation did not migrate `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live behavior, provider-cache behavior, or token accounting. If implementation unexpectedly needs any of those surfaces, split or defer that work, or revise this plan and add the relevant helper/contract/provider-cache tests before accepting the broader slice.

Out of scope are browser UI changes, server API changes beyond existing pairing/WebSocket behavior, server-originated command mutation, provider-live/cache behavior, token-accounting changes, workflow schema/helper migrations, and manually writing the canonical ExecPlan bundle.

## Milestones

Milestone 0 verifies the current seams and failure mode. Reviewers should see evidence from `src/scherzo/connect.gleam`, `src/scherzo/control/file.gleam`, `src/scherzo/control/client.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/workflow_reloader.gleam`, `src/scherzo/orchestrator/read_model.gleam`, and `docs/runbooks/ui-server-pairing.md` that `scherzo connect` stores credentials without local daemon notification, `scherzoctl reload` currently reloads workflow config, startup already sends `StartRemoteClient`, and metrics already expose `ui_server_enabled` plus `remote_client_status`.

Milestone 1 adds failing, named acceptance tests before production changes. Reviewers should see new or updated tests in `test/orchestrator_daemon_control_test.gleam` for disabled-to-enabled reload start, enabled-to-disabled reload stop, explicit enabled reload restart for credential replacement, and metrics status visibility; `test/ctl_connect_test.gleam` for success notification, unavailable-control fallback, JSON/pretty redaction, and replacement notification; and `test/control_remote_ui_websocket_client_test.gleam` or `test/orchestrator_daemon_remote_client_test.gleam` coverage proving revocation still stops retrying instead of reconnecting immediately.

Milestone 2 delivers reload-driven remote-client reconciliation. At the end, explicit reload and changed-config reload update the read model, start a client when `ui_server.enabled` becomes true and no client is running, stop the client when `ui_server.enabled` becomes false, and restart the client on explicit reload when it is already enabled so a replaced credential is read without process restart. Test evidence must include start/stop call counts or fake handles proving no duplicate remote-client process or monitor leak.

Milestone 3 delivers connect-driven activation. At the end, successful credential storage discovers the local daemon control file for the configured workspace, sends the existing reload operator command when possible, and prints either a non-secret activation summary or a non-secret fallback instruction. This milestone must not expose pairing tokens, daemon credential secrets, or local control tokens in stdout, JSON, logs, or retained transcripts.

Milestone 4 proves behavior and failure paths end to end. At the end, automated tests cover hot activation after pairing, credential replacement, disabled-to-enabled reload, enabled-to-disabled stop, metrics visibility, unavailable control fallback, invalid reload/config rejection, no-secret output/logging, and revoked/invalid credential non-looping behavior. A live `scherzo-ui` or browser smoke is not a pre-publish requirement unless a runnable UI server and pairing token are available in the implementation workspace; otherwise it is explicitly deferred as a human/operator check after implementation.

Milestone 5 completes docs, helper/provider inventory, and gates. At the end, `docs/runbooks/ui-server-pairing.md` explains the no-restart flow and fallback, the changed-file inventory confirms no workflow helper/schema, provider-live/cache, or token-accounting migration occurred, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-920-hot-ui-server-activation.md` passes if this plan is touched, and the full Gleam test/format/lint gates pass.

## Progress

- [x] (2026-06-06) Confirmed the prepared review document target is `docs/plans/`.
- [x] (2026-06-06) Reviewed the current connect, credential-store, local control, daemon reload, remote-client, read-model metrics, and UI pairing docs seams.
- [x] (2026-06-06) Authored this concise review document and prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-06-06) Validated this review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-920-hot-ui-server-activation.md` and received `REVIEW_DOC_VALID=ok`.
- [x] (2026-06-06) Incorporated review feedback by making acceptance evidence, named test obligations, milestone proofs, manual/operator check timing, docs/helper and provider-live/cache boundaries, full validation, linting, and structured-pack mirroring explicit.

## Decision Log

- Decision: Use reload reconciliation as the primary activation mechanism, with `scherzo connect` notifying reload after successful credential storage.
  Rationale: The running daemon owns the remote client and already has a local authenticated control plane; connect should not own long-lived runtime state.
  Date: 2026-06-06

- Decision: Explicit reload should restart an existing enabled remote client.
  Rationale: Credential replacement can occur without config text changing, so the safest operator-visible way to pick up a new durable credential is to restart the daemon-owned UI client during explicit reload.
  Date: 2026-06-06

- Decision: Keep a dedicated `ui-server reconnect` command out of the initial scope.
  Rationale: Reload is sufficient for the acceptance criteria and avoids adding CLI surface unless implementation proves a need.
  Date: 2026-06-06

- Decision: Treat review feedback about evidence, tests, milestone specificity, manual/operator timing, helper/provider/cache boundaries, full validation, and linting as implementation-pack obligations.
  Rationale: Scherzo materializes follow-up work from the structured pack, so prose-only obligations would be easy for later implementers to miss.
  Date: 2026-06-06

- Decision: Defer live `scherzo-ui` browser/server dogfood to a human/operator after implementation unless the implementer already has a runnable UI server and pairing token.
  Rationale: Browser UI and server API changes are out of scope; deterministic daemon/control tests are the pre-publish proof for this Core slice.
  Date: 2026-06-06

- Decision: Keep workflow helper/schema migration, provider-live/cache behavior, and token accounting as inventory-only non-goals.
  Rationale: Pairing activation can be delivered through Core reload and local control; touching helper or provider/cache surfaces would expand the risk and requires a separate acceptance contract.
  Date: 2026-06-06

## Validation and Acceptance

Hot activation acceptance requires automated daemon/connect evidence before publish. Add or update tests in `test/orchestrator_daemon_control_test.gleam` showing a daemon started with `ui_server.enabled: true` and missing or stale credential state moves to a non-disabled remote-client status after credential storage and reload notification, and add or update tests in `test/ctl_connect_test.gleam` showing `scherzo connect` sends the reload command after successful credential storage. Metrics assertions must verify `ui_server_enabled: true` plus `remote_client_status` equal to `starting`, `connected`, or `retrying` with a bounded non-disabled reason.

Config reload acceptance requires tests that rewrite config from disabled to enabled and from enabled to disabled, run the reload operator command, and observe the read model and metrics change without process restart. The enabled case must start the remote client exactly once; the disabled case must stop it, clear the handle, and report `remote_client_status: "disabled"`. Explicit reload while already enabled must stop and restart the client so `scherzo connect --replace-credential` can be picked up from the credential store.

Failure-mode acceptance requires tests for unavailable local control after successful credential storage, invalid reload/config errors, duplicate or replacement credentials, no credential or pairing token in stdout/JSON/logs, no local control token leakage, and revoked credentials not causing immediate reconnect loops. Existing revocation coverage in `test/control_remote_ui_websocket_client_test.gleam` must stay green, and any daemon-level regression test added for this issue must assert no hot retry loop after a revoked or invalid credential. The fallback message must be clear and non-secret: run `scherzoctl reload` or restart the daemon.

Manual or dogfood acceptance is conditional. If a live `scherzo-ui` server and pairing token are available before publish, run `scherzo connect --pairing-token <pair_...> --server-url <https-url>` followed by `scherzoctl query metrics --json` and record redacted evidence that `ui_server_enabled` is true and `remote_client_status` is non-disabled without restarting the daemon. If the live server is not available, do not block publication; record this as a deferred human/operator check after implementation because browser UI and server API changes are out of scope.

Documentation and helper-boundary acceptance requires an updated `docs/runbooks/ui-server-pairing.md` explaining the no-restart flow, fallback reload/restart instruction, and redaction expectations. It also requires a changed-file inventory proving that provider-live/cache behavior, workflow helper scripts, workflow schemas, provider-facing structured-output helpers, review-lane contract files, and token accounting were not changed. If any of those surfaces are touched, acceptance requires the relevant helper/contract tests and provider-live/cache stale-read, invalidation, and TTL-disabling evidence, or the broader change must be split out.

Full validation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If this review document is edited during implementation, also run `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-920-hot-ui-server-activation.md` and a required-section check proving every mandated level-2 section remains present and non-empty.

## Rollout, Recovery, and Idempotence

Rollout is opt-in because `ui_server.enabled` remains false by default. Existing local-only daemons continue to report disabled remote-client status until operators configure `ui_server`, pair, and reload or let connect notify reload. The safe rollout evidence is automated Core behavior plus, when available, a redacted live operator check; browser UI dogfood remains a deferred human/operator check when no live server is available in the implementation workspace.

Recovery from unavailable local control is to run `scherzoctl reload` once the control file is available or restart the daemon. Recovery from revoked credentials is to pair again and use explicit replacement when the store already contains a different credential. Recovery from implementation regression is to revert the reload/connect notification changes; local `scherzoctl` operation and stored credentials remain separate trust boundaries.

The flow is idempotent for the same stored credential: repeated reloads may restart the UI client but must not create duplicate credentials or leak monitors, and repeated connect without `--replace-credential` must preserve the existing duplicate-conflict behavior. No provider cache invalidation, helper migration cleanup, workflow schema rollback, token-accounting recovery, or server-side migration is required when the implementation stays within scope. The implementer must record the inventory that proves those surfaces were untouched, or split any accidental expansion into a separate change.

## Open Questions and Clarifications Needed

No blocking clarification is needed. The only non-blocking question is whether a future operator UX should add `scherzoctl ui-server reconnect` as a clearer alias after reload-based activation ships and is observed in use.
