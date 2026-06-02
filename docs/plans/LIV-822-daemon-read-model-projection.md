# Build a daemon read-model projection for query and UI consumers

This ExecPlan v2 review document is the human review surface for LIV-822. It plans a later implementation that gives Scherzo control and UI queries a bounded, non-secret daemon read model; mechanical steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Operators and UI consumers need a consistent operational dashboard without forcing every query endpoint to reassemble daemon state, poll EventHub directly, call a live tracker provider, or replay retained ledgers. After implementation, `scherzoctl query status`, `scherzoctl query metrics`, and remote read queries should return a safe snapshot owned by one daemon read-model module, with worker, retry, park, scheduler, session-token, cache-token, and remote-client status reflected from a bounded projection.

The visible result is not a new dashboard page. The visible result is that the existing status and metrics query surfaces continue to work, but their data comes from a named read-model owner that can be tested for freshness, boundedness, and redaction before future UI consumers depend on it.

## Problem Framing and Constraints

Today operational query data is assembled from narrow helper functions in the control/query and daemon layers, while session lists still come from EventHub and task list/show still call the tracker adapter. That makes future dashboards prone to inconsistent counts, duplicated reads, accidental query-time ledger replay, and accidental coupling to internal daemon structures.

The implementation must not leak local control tokens, UI enrollment tokens, API keys, raw prompts, raw Pi event JSON, raw tracker payloads, or full tracker issue bodies. Status and metrics queries must not make provider-live calls to Linear, a remote UI provider, or any other tracker backend. Cache behavior for this issue is limited to preserving already-recorded Pi token totals, including `cache_read` and `cache_write`; the plan does not introduce a new provider cache, TTL cache, or cache invalidation policy. The query path must stay bounded when retained session, event, and ledger histories are large, preserve the existing query service backpressure and timeout behavior, and keep task list/show tracker-backed unless a separate issue asks to make tracker task data part of the daemon operational dashboard.

No docs/helper migration is required for this issue. The `scripts/scherzoctl` wrapper and the `query status`, `query metrics`, `task list`, and `task show` command names remain stable. If implementation changes human-readable metric labels in `src/scherzo/ctl.gleam`, matching CLI tests and user-facing help text must be updated in the same implementation slice; otherwise documentation changes are out of scope.

## Strategy Overview

Add `src/scherzo/orchestrator/read_model.gleam` as the owner of a safe `ReadModel` and immutable `Snapshot`. A read model is an in-memory projection: it stores small derived facts that query callers need, instead of exposing the rich mutable daemon state. The daemon actor will keep this projection in its `State` and refresh it at controlled boundaries from runtime dictionaries, bounded EventHub session summaries, scheduler runtime state, startup or append-time ledger projection facts where needed, and remote-client lifecycle status. Query workers will ask the daemon for the latest snapshot instead of recomputing operational metrics from ad hoc daemon fields.

Keep the first slice proportionate: use the existing status and metrics DTOs, move their source to the read-model snapshot, and leave task list/show queries on the tracker adapter. The read model should store counts, identifiers, timestamps, enum-like status strings, and token totals only; it should not retain prompts, command bodies, raw tracker descriptions, raw event payloads, provider-live response bodies, or secret-bearing config values.

The structured implementation pack must mirror this strategy with mechanical steps: introduce the module and tests first, wire the daemon snapshot second, route local and remote status/metrics queries third, prove bounded/redacted behavior fourth, and run full validation plus linting before publish.

## Alternatives Considered

The simplest alternative is to keep `metrics_snapshot_from_state` and add more fields as dashboards need them. That is rejected because it keeps operational query behavior scattered and encourages every new dashboard field to inspect daemon internals directly.

A second alternative is to answer queries by loading `state/ledger` projections on demand. That is rejected because retained histories can be large and queries should remain cheap and responsive under UI polling. Ledger-derived facts may be seeded at startup or append boundaries, but `status` and `metrics` query handling must not call `ledger.load_projection`.

A third alternative is a broad dashboard schema redesign. That is too large for this issue. Existing status and metrics responses are adequate for the first read-model-backed dashboard slice, and new UI-specific DTOs can be introduced later if a concrete consumer needs them.

A fourth alternative is to add a provider-live or tracker-backed cache for dashboard data. That is rejected for this issue because task list/show already own tracker-backed task data, while status/metrics are daemon operational data and should be answerable without live provider calls.

## Risks and Countermeasures

The main risk is stale counts if the read model is not refreshed after every relevant daemon transition. Counter this with unit tests for worker start/finish, retry scheduling/cancellation, park/unpark, scheduler failure and retry state, remote-client status, startup recovery, and fresh daemon state, plus daemon integration tests that query through the existing query service.

A second risk is leaking sensitive data by copying rich runtime, session, ledger, provider-live, or tracker structures into the projection. Counter this by giving the read model its own narrow types, writing redaction tests that inject obvious secret markers and raw payload text, and keeping DTO conversion functions one-way from safe sources.

A third risk is performance regression from EventHub or ledger replay during frequent UI polling. Counter this by using bounded EventHub summaries, maintaining counts incrementally or from bounded active dictionaries, seeding ledger-derived facts at startup or append boundaries only, and adding a large-history test plus code-review evidence that status/metrics query paths do not call `ledger.load_projection` or iterate raw event pages.

A fourth risk is remote-client status being misleading. Counter this by defining a small vocabulary such as `disabled`, `starting`, `connected`, `retrying`, and `stopped`, recording only status and non-secret error codes, and testing disabled, successful, failed/retrying, and shutdown paths.

A fifth risk is regressing the existing query service behavior while changing where status and metrics data come from. Counter this by preserving `src/scherzo/control/query/service.gleam` behavior, keeping existing overload/timeout/shutdown tests, and adding read-model tests without weakening those assertions.

## Scope Boundaries

In scope for LIV-822 is exactly one daemon-owned read-model projection, query wiring for operational status and metrics dashboards, safe DTO conversion, focused tests, and validation. The later implementation may touch `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/read_model.gleam`, `src/scherzo/control/query/metrics.gleam`, `src/scherzo/control/query/types.gleam`, `src/scherzo/control/query/dto.gleam`, `src/scherzo/ctl.gleam` only if printed metric labels need to stay aligned, and related tests under `test/`.

Out of scope are changing tracker-backed task list/show semantics, adding new UI pages, changing the control transport protocol version except for already-supported metrics/status payload fields, retaining raw session events in the read model, adding persistent database storage, adding provider-live calls or a new provider cache, migrating docs/helpers, and replaying the full ledger for each query.

## Milestones

Milestone 1 establishes the read-model owner and safety boundary. Reviewers should see `src/scherzo/orchestrator/read_model.gleam` with safe types, default empty state, snapshot conversion to existing status/metrics sources, and tests in a new read-model-focused test file proving a fresh daemon snapshot is non-secret and zero-valued where appropriate. This milestone also proves token totals preserve `input`, `output`, `cache_read`, `cache_write`, and `total` without storing raw Pi payload text.

Milestone 2 integrates the projection with daemon lifecycle updates. Reviewers should see the daemon state carry a read model and refresh it after worker start/finish, retry scheduling or cancellation, park/unpark, scheduler pending/failure/retry changes, startup recovery, and remote-client lifecycle changes without requiring query-time ledger replay. Tests should exercise these transitions through daemon-facing helpers or the daemon actor rather than only constructing DTOs.

Milestone 3 routes operational queries through the read model. Reviewers should see local control queries and remote read queries for status and metrics using the snapshot path while the existing query service concurrency, overload, timeout, and shutdown behavior remains intact. Task list/show tests should still prove those queries use the tracker adapter and Scherzo-owned cursors.

Milestone 4 proves bounded behavior, provider isolation, cache-token preservation, and redaction. Reviewers should see tests with large retained session/event and ledger-like histories, assertions that query output omits secret and raw-payload markers, assertions that `cache_read` and `cache_write` survive snapshot conversion, and evidence that status/metrics query paths inspect bounded projection data instead of replaying ledgers or calling tracker/provider live paths.

Milestone 5 completes pre-publish repository validation and records the post-implementation dogfood check boundary. Before publish, reviewers should see targeted tests, full Gleam tests, formatting, glinter, and Scherzo lint commands passing from the repository root under `direnv exec .`. A manual/operator dogfood check with a running daemon is useful evidence after implementation but is not a pre-publish gate for this plan; when available, run `scripts/scherzoctl query status --json` and `scripts/scherzoctl query metrics --json` and verify the responses contain safe status/metrics payloads with no secret markers.

## Progress

- [x] (2026-06-02) Read the LIV-822 task brief and relevant current repository modules for control queries, daemon metrics, EventHub summaries, runtime state, ledger projection, scheduler runtime, and remote client behavior.
- [x] (2026-06-02) Drafted this concise human-reviewable review document under `docs/plans/`.
- [x] (2026-06-02) Prepared the structured implementation-pack submission obligations for Scherzo capture.
- [x] (2026-06-02) Validated this review document with `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-822-daemon-read-model-projection.md`.
- [x] (2026-06-02) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, provider-live/cache behavior, docs/helper migration scope, manual dogfood timing, full validation, and linting obligations explicit.

## Decision Log

- Decision: Make `src/scherzo/orchestrator/read_model.gleam` the read-model owner.
  Rationale: The projection is daemon-owned operational state, so placing it under the orchestrator boundary keeps query modules from depending directly on daemon internals.
  Date: 2026-06-02.

- Decision: Reuse the existing status and operational metrics DTOs for the first implementation slice.
  Rationale: The problem is inconsistent data ownership, not a missing wire schema; reusing DTOs minimizes client churn.
  Date: 2026-06-02.

- Decision: Keep task list/show tracker-backed for this issue.
  Rationale: Those queries expose tracker task data rather than daemon operational dashboard state, and pulling tracker payloads into the read model would increase leak and freshness risk.
  Date: 2026-06-02.

- Decision: Forbid query-time ledger replay and provider-live calls for status and metrics.
  Rationale: UI polling must remain bounded and independent of tracker/provider availability even when retained histories are large.
  Date: 2026-06-02.

- Decision: Treat manual `scherzoctl query status/metrics` dogfood as deferred operator evidence, not a pre-publish gate.
  Rationale: The implementation can be proven by deterministic tests and full validation in disposable workspaces; a live daemon check is still useful after implementation but should not require a browser or live operator session before the plan can publish.
  Date: 2026-06-02.

- Decision: Do not migrate docs/helpers unless implementation changes CLI labels or help text.
  Rationale: Existing command names and wrapper behavior already expose the status/metrics surfaces, so docs/helper migration would add churn unrelated to the read-model ownership problem.
  Date: 2026-06-02.

## Validation and Acceptance

Acceptance outcome 1 is a clear, safe owner. Evidence must include `src/scherzo/orchestrator/read_model.gleam`, read-model tests for fresh/empty snapshots, token totals including `cache_read` and `cache_write`, and redaction of obvious secret markers. JSON assertions must show status/metrics responses do not contain local control tokens, enrollment tokens, API keys, raw prompts, raw tracker payload text, raw provider-live response text, or raw Pi payload text.

Acceptance outcome 2 is query infrastructure using the read model for operational dashboards. Evidence must include local query tests and remote query tests showing status and metrics responses are produced from the daemon read-model snapshot, plus unchanged query service overload, timeout, and shutdown tests.

Acceptance outcome 3 is projection update coverage. Evidence must include tests for worker start, worker finish success/failure, retry schedule/cancel, park/unpark, scheduler failure/retry/report-retry state, remote client disabled/starting/connected/retrying/stopped status, startup recovery, and empty/fresh daemon state.

Acceptance outcome 4 is bounded performance and provider isolation. Evidence must include a large-history test that creates many retained sessions/events or synthetic ledger projection facts and proves metrics query time and output size stay bounded, plus code-review evidence that status and metrics query paths do not call `ledger.load_projection`, iterate raw event pages, or call tracker/provider live functions while handling a query.

Acceptance outcome 5 is CLI compatibility. Evidence must include existing `scherzoctl query status`, `scherzoctl query metrics`, `task list`, and `task show` parser/output tests staying green. If human-readable status or metric labels in `src/scherzo/ctl.gleam` change, update the corresponding tests and help text in the same implementation; otherwise no docs/helper migration is expected.

Acceptance outcome 6 is repository validation. From the repository root, run `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; each command must exit zero. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Deferred operator evidence is optional after implementation: with a running daemon, run `scripts/scherzoctl query status --json` and `scripts/scherzoctl query metrics --json`; expect successful protocol responses containing safe status/metrics payloads, stable `daemon_id`/`boot_id` values for the daemon, `schema_version: 1` metrics, integer token totals including cache fields, and no secret marker strings. This manual dogfood check is not required before publishing the implementation pack.

## Rollout, Recovery, and Idempotence

Rollout is additive and internal to the daemon/query path. The implementation should keep existing control protocol request names, CLI command names, and status/metrics JSON field names stable, then switch status and metrics sources to the read model in one small slice after tests cover both local and remote query paths.

Recovery is to revert the read-model field, snapshot message, and query wiring while leaving existing tracker-backed task queries and EventHub session endpoints intact. Because no stored data migration, docs/helper migration, provider cache migration, or protocol version migration is required, rollback should not require deleting ledger files, session history, or local helper files.

Repeated daemon starts should rebuild the read model from current runtime startup recovery and bounded scheduler/session state without duplicating ledger records. Tests that write temporary state under `test/tmp/` must reset or overwrite their own directories so they are safe to rerun.

## Open Questions and Clarifications Needed

No open questions. The implementation should use the small remote-client status vocabulary named in this document, treat task list/show as out of scope for read-model migration, avoid provider-live/cache additions beyond preserving recorded token cache counters, and collect any richer UI dashboard schema needs in a separate follow-up issue.
