# Daemon read-query plane for local control and remote UI RPC

This ExecPlan review document is the human review surface for LIV-769. Scherzo will materialize the mechanical implementation pack from the structured submission; this Markdown file intentionally stays concise and review-focused.

## Purpose / Big Picture

This plan creates a reusable read-only query plane owned by the daemon, so local operators and the future remote UI/server can ask the same typed questions without turning reads into `OperatorCommand` mutations or one-off Linear helper processes. After implementation, a reviewer should see a small query-capabilities round trip over local `scherzoctl` control and the disabled-by-default outbound `ui_server` socket path, with both transports sharing one query model, one DTO/redaction layer, and one error/pagination convention.

The operator value is safer observability: slow tracker or ledger reads can run outside the orchestrator transition loop, while dispatch, polling, local session/event commands, and mutating operator controls keep their current semantics.

## Problem Framing and Constraints

The current local control path in `src/scherzo/control/protocol.gleam`, `client.gleam`, `server.gleam`, `src/scherzo/ctl.gleam`, and daemon wiring mixes read-only session inspection with mutating operator-command framing. `protocol.Request` has read variants such as `ListSessions`, `GetSession`, and `GetEvents`, but mutating controls are converted to and from `OperatorCommand`; the control server routes EventHub reads directly and sends mutations to the daemon through `ApplyOperatorCommand`.

The current remote UI path in `src/scherzo/control/remote_envelope.gleam`, `src/scherzo/control/remote/client.gleam`, and the remote harness supports hello, heartbeat, minimal state snapshots, and server-originated commands/results. It does not yet have a general query request/response envelope; inbound non-command envelopes are logged as unexpected.

The new plane must therefore be additive. It must not introduce a public HTTP REST API, enable `ui_server` by default, replace `ps`, `session`, `events`, or `attach`, or migrate destructive/offline maintenance commands. It must respect source guardrails around already-large modules, especially `protocol.gleam`, `ctl.gleam`, and `orchestrator/daemon.gleam`.

## Strategy Overview

Introduce a typed `ReadQuery` model under the control package, separate from `OperatorCommand` and `CommandResult`. The model owns query names, parameters, page requests, result DTOs, error codes, and JSON codecs. Local control and remote UI adapters wrap that same model: local control uses the existing token-authenticated loopback JSON protocol, while the remote path adds correlated socket envelopes such as query request and query response without carrying local control-file fields.

Add a daemon-started query service process with injected dependencies for EventHub reads, ledger/projection reads, artifact metadata reads, and future tracker adapter reads. The service owns timeouts, a small concurrency limit, cancellation on shutdown or remote connection loss, and safe error mapping. It runs each accepted query in a worker process, so a slow tracker read cannot block the daemon actor's transition loop, polling timers, or dispatch decisions.

Use reusable opaque cursor pagination for list-shaped query results. Clients receive `items` plus `page` metadata with `limit`, `has_more`, and `next_cursor`; they must treat cursor strings as opaque. DTOs are normalized and redacted before transport: no raw tracker payloads, credentials, provider API cursors, prompt bodies, raw Linear comments, or provider-specific workflow implementation details are exposed.

## Alternatives Considered

A REST-style `/api/work` surface was rejected because Scherzo already has a local control-file and line-delimited JSON protocol, while the remote UI path is an outbound socket client. Adding HTTP as the primary contract would split the control model and increase exposure before the UI/server contract is ready.

Extending `OperatorCommand` for reads was rejected because reads need pagination, redaction, cacheability, and safe timeout semantics rather than mutation status such as `Applied` or `Rejected`. Keeping reads separate makes it harder to accidentally route a slow list query through the transition kernel.

Letting each CLI or UI command call tracker helpers directly was rejected because it repeats one-shot Linear helper behavior, produces divergent DTOs, and bypasses daemon-owned policy, redaction, and concurrency limits.

Using provider cursors directly was rejected because it leaks backend details and makes local ledger/artifact queries look like tracker queries. Scherzo cursors should be opaque Scherzo cursors.

## Risks and Countermeasures

The main availability risk is blocking dispatch or polling with slow reads. The countermeasure is a query service process that spawns bounded workers, enforces per-query timeouts, rejects excess concurrency with a stable `query_busy` error, and never calls tracker reads from the daemon transition handler.

The main compatibility risk is divergent local and remote DTOs. The countermeasure is one query codec and one DTO/redaction module used by both adapters, plus tests that compare local and remote JSON for the same query result.

The main security risk is leaking raw provider data or credentials. The countermeasure is DTO allowlisting, secret-redacted logs/transcripts, explicit negative tests for token/control-file/provider-cursor leakage, and bounded error messages.

The main delivery risk is source-guardrail churn in large modules. The countermeasure is to put query logic in new focused modules, keep `daemon.gleam`, `protocol.gleam`, and `ctl.gleam` adapter-only, and treat any source-baseline update as pre-publish evidence requiring a rationale.

The main remote risk is query spam over the outbound socket. The countermeasure is request-id correlation, per-daemon concurrency limits, cancellation when the connection generation changes, and preserving `ui_server.enabled: false` by default.

A final handoff risk is letting review feedback live only in this prose document while the structured implementation pack omits acceptance evidence, test obligations, milestone specificity, manual/dogfood timing, docs/helper migration boundaries, provider-live/cache behavior, full validation, or lint gates. The countermeasure is to mirror those obligations in the pack's concrete steps and testing notes before Scherzo materializes follow-up implementation artifacts.

## Scope Boundaries

For this planning issue, in scope is exactly this Markdown review document and one structured implementation-pack submission. No production code, workflow helper, workflow schema, provider-facing structured-output helper, provider-live behavior, provider-cache behavior, browser UI, or canonical bundle JSON should change during review incorporation.

For the follow-up implementation, in scope are the shared read-query type/codec, query service lifecycle, timeout/concurrency/cancellation behavior, local protocol adapter, remote envelope adapter, query-capabilities proof query, opaque pagination helpers, DTO/redaction rules, tests, harness evidence, and documentation of migration candidates.

Existing session/event commands (`ps`, `session`, `events`, `attach`) remain as-is. Mutating commands (`pause`, `resume`, `reload`, `retry`, `retry-step`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`, `schedules run`, and recovery cleanup) remain `OperatorCommand` or explicit maintenance flows. Offline/destructive commands such as retention cleanup, state archive/discard/reinitialize, and provenance repair stay out of the query plane.

Later migration candidates include workstream list/show, schedule status/history/doctor reads, artifact metadata or bounded artifact preview reads, and the follow-up task list/show consumer. This plan must not implement task list/show itself.

The docs/helper boundary is explicit. The implementation may update architecture or runbook documentation that describes the new query plane, but it must not migrate unrelated workflow helpers. If it touches `.scherzo/workflows/scripts/*`, `workflows/dogfood/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files, it must run and retain the relevant helper or offline contract tests. If it does not touch those surfaces, acceptance evidence must explicitly say that no helper migration, provider-live validation, or cache validation was applicable.

Provider-live and cache behavior are not part of the initial proof query. The query-capabilities round trip must not call Linear or any other live tracker provider and must not add cache invalidation, TTL, or stale-read semantics. If a later consumer needs live provider reads or a provider-backed cache, split that work or add stale-read, invalidation, TTL-disabling, and live-provider tests before accepting it.

## Milestones

Milestone 0 verifies the existing seams and guardrails. Reviewers should see evidence that the local control protocol, control server, CLI adapter, remote envelope, remote client, harness tests, daemon control wiring, source guardrail, and lint gates were inspected before changing code.

Milestone 1 delivers the pure query contract. Reviewers should see focused modules for read-query requests/responses, pagination, DTOs, redaction, and JSON codecs, with tests for valid query-capabilities responses, invalid query names and params, cursor validation, error mapping, and no credential/provider-cursor leakage.

Milestone 2 delivers the daemon query service. Reviewers should see a daemon-owned service process with bounded worker execution, timeout, busy, cancellation, and shutdown behavior proven by deterministic tests using barriers or fake dependencies rather than sleeps.

Milestone 3 wires the local control adapter. Reviewers should see local protocol/client/server support for one query RPC shape and a minimal `scherzoctl` query-capabilities command or equivalent proof, while existing session/event and operator-command tests continue to pass unchanged.

Milestone 4 wires the remote socket adapter. Reviewers should see query request/response envelope round trips, remote client handling with request-id correlation and cancellation on connection loss, and live loopback harness evidence from the real outbound client path. `ui_server` remains disabled by default.

Milestone 5 completes migration notes, rollout evidence, scope audit, helper/cache inventory, and validation gates. Reviewers should see documented migration candidates and non-goals, source-guardrail evidence, full test/format/lint evidence, a docs/helper inventory, an explicit statement that no provider-live or cache behavior changed, and an explicit statement that task list/show, REST, browser UI, and default remote enablement stayed out of scope.

## Progress

- [x] (2026-05-30) Confirmed the prepared review document target is `docs/plans/` and selected `docs/plans/LIV-769-daemon-read-query-plane.md`.
- [x] (2026-05-30) Surveyed the local control protocol, client, server, CLI, daemon backend wiring, remote envelope, remote client, harness, and relevant guardrail/docs files.
- [x] (2026-05-30) Authored this concise human-reviewable review document and prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-30) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, pre-publish remote loopback evidence, deferred browser/operator dogfood checks, docs/helper migration boundaries, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack obligations.

## Decision Log

- Decision: Define read queries separately from `OperatorCommand` and `CommandResult`.
  Rationale: Reads need pagination, bounded execution, DTO redaction, and cache-safe semantics rather than mutation status semantics.
  Date: 2026-05-30

- Decision: Use one shared query codec and DTO layer for both local and remote transports.
  Rationale: The task's core requirement is preventing divergent local `scherzoctl` and remote UI/server shapes.
  Date: 2026-05-30

- Decision: Prove the infrastructure with a query-capabilities query, not task list/show.
  Rationale: A minimal non-tracker consumer validates local and remote round trips while preserving the follow-up task-list/show non-goal.
  Date: 2026-05-30

- Decision: Keep session/event commands and destructive/offline maintenance commands out of the migration.
  Rationale: Those commands already have specialized behavior or mutation/offline safety semantics that this read-query plane should not blur.
  Date: 2026-05-30

- Decision: Treat review feedback about evidence, tests, manual/dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations.
  Rationale: Scherzo materializes follow-up implementation instructions from the structured pack, so prose-only obligations would be easy for later implementers to miss.
  Date: 2026-05-30

## Validation and Acceptance

Planning acceptance requires this file to remain at `docs/plans/LIV-769-daemon-read-query-plane.md`, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-769-daemon-read-query-plane.md` to exit zero with `REVIEW_DOC_VALID=ok` in this checkout, the packaged equivalent `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-769-daemon-read-query-plane.md` to be valid in workflow execution, every required level-2 review-doc section to be present and non-empty, and Scherzo to capture the structured implementation-pack submission. The planning handoff must not manually write canonical bundle JSON or implement the query plane.

Follow-up implementation acceptance requires automated evidence for the shared model: tests must prove query request/response JSON round trips, invalid query and cursor errors, error-code mapping, DTO redaction, provider-cursor rejection, and byte/limit bounds. Tests must compare local and remote serialization for the same query-capabilities response so one query model is visibly shared. New or updated coverage should include focused query model/codec tests, local `test/control_protocol_test.gleam`, `test/control_server_test.gleam`, a CLI parsing/output test for the proof command, `test/control_remote_envelope_test.gleam`, `test/control_remote_client_test.gleam`, and `test/remote_harness_test.gleam` or equivalent files if names drift.

Follow-up service acceptance requires deterministic tests proving that slow query workers time out without blocking daemon snapshots or local control ping, excess concurrency returns `query_busy`, cancellation or shutdown kills or ignores outstanding workers safely, and no query path mutates ledger, tracker, EventHub session state, or daemon dispatch state.

Follow-up transport acceptance requires local control protocol/client/server/CLI tests and remote envelope/client/harness tests. The pre-publish manual evidence is a live loopback harness transcript from the real outbound remote client showing hello, query request, query response, request-id correlation, and auth redaction. Browser UI and production multi-daemon dogfood are deferred human/operator checks and do not block publish.

Docs/helper evidence must include a helper inventory. If `.scherzo/workflows/scripts/*`, `workflows/dogfood/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files changed, run the relevant helper or offline contract tests and preserve provider-live/cache semantics. If they did not change, record that no helper migration, provider-live validation, or cache validation was applicable. The implementation must also record that the query-capabilities proof query does not call Linear or any other live tracker provider and does not introduce cache invalidation, TTL, or stale-read behavior.

Full validation for the implementation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands. If any gate fails for a pre-existing unrelated reason, the implementer must capture exact output and explain why the query-plane change is not the cause before handoff.

## Rollout, Recovery, and Idempotence

Rollout is additive. Local query RPC is token-authenticated through the existing control file, and remote query RPC is available only when the existing disabled-by-default `ui_server` path is enabled. Existing `scherzoctl` session/event commands and operator mutations remain the fallback surfaces.

Recovery is to stop the query service, reject new query requests with `query_service_unavailable`, or disable `ui_server` for remote issues while local control continues. A stale or invalid cursor returns `invalid_cursor` without mutation. If a source-guardrail baseline had to be raised, the implementation review must state why and identify the extraction that would shrink it later.

The work is idempotent because query requests are read-only, repeated query-capabilities calls return equivalent data, failed or timed-out queries do not append ledger records or mutate tracker state, and repeated validation runs should not change tracked files beyond intentional docs or transcript artifacts.

Workflow-helper, provider-live, and provider-cache changes are not part of the safe rollout. If implementation discovers that one of those surfaces must change, the safe recovery path is to split or revert that surface before publishing the query-plane infrastructure, unless the same implementation adds the helper/contract, stale-read, invalidation, TTL-disabling, and live-provider evidence called out above.

## Open Questions and Clarifications Needed

No blocking clarification is needed for the infrastructure plan. Exact task list/show filters, task DTO fields, and operator-facing UI presentation remain deferred to the first consumer follow-up. Production server authorization policy and browser UI behavior also remain deferred.
