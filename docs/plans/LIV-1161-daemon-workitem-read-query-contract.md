# Daemon WorkItem read-query contract and task hierarchy projection

This ExecPlan v2 review document is the human review surface for LIV-1161. It plans a later implementation that gives Scherzo's Work screen a daemon-owned WorkItem query contract; mechanical steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

The Work screen needs one daemon-normalized shape for tracker tasks so the UI does not learn Linear-specific issue fields, Scherzo Core internals, or future tracker-provider details. After this plan is implemented, the daemon can answer `work_item_list` and `work_item_show` queries with a stable parent task summary plus bounded child/subtask summaries, exact tracker state name/id, normalized state category, labels, source metadata, and truncation markers.

The visible result is not a finished Work screen. The visible result is that local control queries and remote query envelopes can carry the same safe WorkItem JSON, and automated tests can prove that the payload is bounded, redacted, provider-neutral, and live enough for a later UI consumer.

## Problem Framing and Constraints

The current read-query surface has `task_list` and `task_show`, but those DTOs expose only a normalized state category and a flat task. They do not carry the exact tracker state name/id, a bounded child-workflow projection, or an explicit Work screen contract. The target model treats labels as the only tag/type concept, so names such as `BUG`, `FEAT`, or `workflow:execplan` remain labels and must not become a separate kind/type field.

WorkItem queries intentionally read provider-live task data in this first implementation. Provider-live means the query asks the tracker adapter for the current provider view when the query runs; it does not read from, update, or create a daemon cache, a TTL cache, the scheduler read model, retained workflow artifacts, or the task dispatch state. This keeps the contract fresh and avoids inventing cache invalidation before the UI has real usage data. If Linear or a future provider is unavailable, the query must fail with a normal query error such as `query_backend_failed` or `unsupported_query`, not with raw provider response text.

The contract must avoid raw provider payloads, credentials, control tokens, API keys, raw prompts, comments, descriptions, full tracker response bodies, and Scherzo workflow-run internals. No docs/helper migration is required for this issue: do not change `workflows/dogfood/`, `.scherzo/workflows/schemas/`, or `scripts/scherzo-execplan-v2` for the WorkItem contract. If implementation adds a user-facing CLI command or help text for WorkItems, update the matching CLI tests and help text in the same slice; otherwise documentation changes are out of scope.

## Strategy Overview

Add a new query family, `work_item_list` and `work_item_show`, rather than stretching the existing `task_list` and `task_show` DTOs. Define a narrow WorkItem projection in production code, then map it to query DTOs. Each WorkItem summary should include a daemon-scoped id, task key/display id, title, source provider/id/url, exact tracker state id/name plus normalized category, bounded labels, label truncation, created/updated timestamps, and optional URL. Each WorkItem detail should include the parent summary plus bounded subtask summaries and a subtask truncation flag; it must not include descriptions or comments.

Use a dedicated tracker WorkItem read capability rather than storing hierarchy directly in `task.Task`. The existing `task.Task` type stays the flat normalized task used by runtime dispatch, legacy task queries, and adapter seams. The WorkItem capability can reuse `task.TaskState`, `task.TaskLabel`, and `task.TaskRef` concepts, but hierarchy bounds and redaction rules remain close to the read-query boundary.

The Linear implementation should use fixtures and fake transports first. It should request only allowlisted Linear fields needed for the WorkItem contract, including child issue summaries, state `id`/`name`/`type`, labels `id`/`name`, timestamps, identifiers, ids, titles, and urls. It should not fetch comment connections, descriptions, body data, raw prompt text, or full workflow artifacts. The structured implementation pack must mirror this strategy with mechanical steps: define the domain/codec first, add fake and adapter capabilities second, implement Linear with fixture tests third, wire backend/local/remote query paths fourth, and run safety plus full repository validation before publish.

## Alternatives Considered

Extending `task_list` and `task_show` would reuse existing code, but it would mix a UI Work contract into a generic task DTO and make it harder to see which callers receive hierarchy and exact state data. Keeping the existing task queries stable also preserves current `scherzoctl task` behavior while the Work screen contract is still being proven.

Replacing existing task queries outright is possible because there are no broad external users yet, but it creates unnecessary churn in `scherzoctl task`, parser tests, and operator habits. A later cleanup can alias or retire the older task queries after the UI and operators have used WorkItems.

Storing children directly in `task.Task` was rejected for the first implementation because most runtime paths do not need hierarchy, and a dedicated projection keeps hierarchy bounds and redaction rules close to the read-query boundary.

Adding a daemon-side provider cache was also rejected. WorkItem queries are tracker data, so the first slice should either return a fresh provider projection or a clear backend error. Cache design, stale indicators, invalidation, and offline behavior should wait until the UI demonstrates a concrete need.

## Risks and Countermeasures

The largest risk is accidentally leaking provider details or large bodies while adding richer task data. The countermeasure is a DTO that serializes only allowlisted fields, plus codec and backend tests that assert credentials, control tokens, API keys, raw provider payloads, descriptions, comments, body data, raw prompts, and workflow internals are absent from WorkItem JSON.

A second risk is unbounded hierarchy or label fan-out. Counter this by clamping page, subtask, and label limits in the backend and provider projection. The defaults are page limit 50 clamped to 100, list subtask summaries clamped to 10 per parent, show subtask summaries clamped to 50 per parent, and labels clamped to 50 per task summary. Payloads must report `subtasks_truncated` and `labels_truncated` where a provider returned more data than the contract exposes.

A third risk is stale or misleading data if the implementation silently introduces a cache. Counter this by treating WorkItem as provider-live read data for LIV-1161, not a scheduler/read-model projection. Tests should use fake transports and fixtures, but production query handling must not write a provider cache, read retained workflow artifacts, or synthesize workflow-run state.

A fourth risk is provider mismatch, especially if Linear hierarchy fields differ from assumptions. Counter this by isolating Linear GraphQL construction/parsing in a tested module with fixtures for parent tasks with zero, one, many, and truncated children; labels with ids and names; exact state ids and names; GraphQL errors; missing pages; and unsupported provider requests. Backend errors must map to `unsupported_query`, `query_backend_failed`, or `not_found` without exposing raw provider bodies.

A fifth risk is local and remote query paths diverging. Counter this by adding codec tests for the base query codec, local control server tests for a `query` request carrying `work_item_list` and `work_item_show`, and remote envelope tests proving `RemoteQueryRequest` and `RemoteQueryResponse` carry the same WorkItem payload.

## Scope Boundaries

In scope is the daemon read-query contract, WorkItem DTO encoding/decoding, backend wiring, tracker WorkItem capability, Linear-backed hierarchy projection, fake adapter support for tests, local control server round-trips, remote envelope round-trips, and safety validation. The later implementation may touch `src/scherzo/work_item.gleam`, `src/scherzo/tracker/adapter.gleam`, `src/scherzo/tracker/linear_adapter.gleam`, `src/scherzo/linear/work_item_query.gleam`, `src/scherzo/control/query/types.gleam`, `src/scherzo/control/query/dto.gleam`, `src/scherzo/control/query/codec.gleam`, `src/scherzo/control/query/backend.gleam`, local/remote protocol tests, and fake test adapters under `test/support/`.

Out of scope are UI rendering, browser tests, mutating task operations, tracker writes, provider cache design, full recursive hierarchy traversal, Scherzo workflow-run state synthesis, new task kind/type concepts, comment or description exposure, changes to the ExecPlan workflow helper, and mandatory live Linear/browser dogfood before publishing the implementation. Existing `task_list` and `task_show` may remain as compatibility and operator surfaces until a later cleanup decides whether to alias or remove them.

## Milestones

Milestone 1 establishes the WorkItem domain and codec contract. Reviewers should see narrow production types for WorkItem summaries/details, query request/response variants for `work_item_list` and `work_item_show`, JSON encoders/decoders, supported query names, and tests proving request/response round-trips. The contract evidence must show exact state `id`/`name` plus normalized category, labels as labels only, deterministic daemon ids, label/subtask truncation fields, invalid cursor rejection, and absence of description/comment fields.

Milestone 2 adds the tracker capability and fake adapter path. Reviewers should see a dedicated WorkItem read capability on `TrackerAdapter` or an equally explicit adapter seam, fake adapter data with parent and child tasks, backend routing that clamps list limits and maps unsupported capability errors safely, and backend tests for pagination, display-id lookup, remote-id lookup, provider mismatch, not found, label bounds, subtask bounds, and no-cache provider-live behavior.

Milestone 3 implements the Linear projection with fixture-driven tests. Reviewers should see a Linear WorkItem query module that builds allowlisted GraphQL queries for list and show, decodes parent/child issue summaries, normalizes Linear state `type` to Scherzo categories while preserving exact state `id`/`name`, applies project scope filtering, detects truncated child/label connections, and maps GraphQL or malformed payload errors without including raw response bodies. Tests must use fake transports and fixtures; live Linear access is useful later but is not required before publish.

Milestone 4 wires all query transports. Reviewers should see `work_item_list` and `work_item_show` accepted by the base query codec, reported by `supported_queries`, executed by the query backend, returned through the local control server `query` request, and carried by `src/scherzo/control/remote_envelope.gleam` query request/response envelopes. This milestone should preserve existing `status`, `metrics`, `task_list`, `task_show`, `outbox_list`, and `outbox_show` behavior and tests.

Milestone 5 proves safety, bounds, and compatibility. Reviewers should see redaction tests with obvious secret markers, raw provider-body markers, prompt markers, comment text, and description text; large label and child fixtures that prove truncation; unsupported provider and backend-failure tests; unchanged legacy task-query tests; and any necessary CLI/help/doc tests if a human-facing WorkItem command is added.

Milestone 6 completes pre-publish validation and records the manual dogfood boundary. Before publish, reviewers should see focused tests plus `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` passing from the repository root. Manual browser/UI dogfood and live Linear checks are deferred operator evidence after implementation or during the UI integration follow-up, not pre-publish gates for this daemon contract.

## Progress

- [x] (2026-06-16 01:07Z) Read the repository-local ExecPlan guidance in `workflows/dogfood/guidance/exec-plan.md` and inspected the current task, tracker adapter, Linear adapter, query, control server, remote envelope, remote client, UI websocket protocol, `scherzoctl`, and test surfaces relevant to this plan.
- [x] (2026-06-16 01:07Z) Authored this review document and delegated mechanical implementation steps, tests, interfaces, and artifact notes to the structured implementation pack for LIV-1161.
- [x] (2026-06-16 01:38Z) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, manual/dogfood timing, docs/helper migration scope, provider-live/cache behavior, full validation, and linting obligations explicit.

## Surprises & Discoveries

The existing `TaskSourceCapability` already has `list_tasks` and `lookup_task_detail`, and the Linear adapter already normalizes state categories for flat task queries. The current task DTO intentionally omits exact tracker state names from JSON, which is why a distinct WorkItem contract is cleaner than overloading the existing `state` field.

The current Linear task query module already decodes state `id`, state `name`, Linear state `type`, label `id`, and label `name` for flat tasks. That reduces Linear risk, but hierarchy still needs a dedicated allowlisted query and fixture coverage because the existing flat query does not fetch child issues.

The UI websocket protocol currently focuses on daemon state snapshots and server commands, while `src/scherzo/control/remote_envelope.gleam` already carries generic remote query request/response envelopes through the query codec. This plan therefore validates remote query envelopes but defers browser/UI rendering until a Work screen integration issue.

## Decision Log

- Decision: Add `work_item_list` and `work_item_show` as a new query family instead of replacing `task_list` and `task_show` in the first implementation. Rationale: This gives the UI a purpose-built contract while keeping existing operator query behavior stable and removable later. Date: 2026-06-16.
- Decision: Keep parent/subtask hierarchy in a dedicated WorkItem projection and tracker capability, not directly in `task.Task`. Rationale: `task.Task` is already the flat runtime-normalized task used by dispatch and adapter seams; hierarchy has UI-specific bounds and redaction concerns. Date: 2026-06-16.
- Decision: Default WorkItem bounds should be finite: page limit defaults to 50 and clamps to 100, list subtask summaries default to 10 per parent, show subtask summaries default to 50 per parent, and labels default to 50 per task summary. Rationale: These defaults match existing query pagination scale while preventing large provider fan-out. Date: 2026-06-16.
- Decision: Treat WorkItem reads as provider-live and do not add a daemon provider cache in LIV-1161. Rationale: Fresh tracker state is more important than cache complexity for the first Work screen contract, and a cache would require stale indicators, invalidation, rollout, and recovery rules that are not justified yet. Date: 2026-06-16.
- Decision: Defer browser/UI and live Linear dogfood to follow-up operator evidence rather than making them pre-publish gates. Rationale: The daemon contract can be proven with deterministic codec, backend, transport, fake adapter, and Linear fixture tests; a real UI consumer is explicitly out of scope for this contract slice. Date: 2026-06-16.
- Decision: Do not migrate ExecPlan workflow helpers or schemas for this issue. Rationale: WorkItem is a daemon query contract, and Scherzo will materialize implementation-pack artifacts from the structured submission without repository helper changes. Date: 2026-06-16.

## Outcomes & Retrospective

This plan has not been implemented yet. The expected outcome is a daemon-owned WorkItem read model that the UI can consume without provider-specific branching and without exposing raw tracker, credential, prompt, comment, description, or workflow-run material. The main intentional gaps are that actual UI integration, browser checks, live Linear dogfood, cache design, and legacy task-query retirement are deferred to follow-up work after the daemon contract exists and is testable.

## Validation and Acceptance

Acceptance outcome 1 is a stable WorkItem wire contract. Evidence must include codec tests for `work_item_list` and `work_item_show` requests and responses, supported-query introspection showing both query names, and JSON assertions for deterministic ids, source provider/id/url, display ids, titles, exact tracker state id/name/category, bounded labels, `labels_truncated`, bounded subtasks, `subtasks_truncated`, page cursors, and invalid cursor errors.

Acceptance outcome 2 is safe DTO redaction. Evidence must include tests that inject obvious markers for local control tokens, enrollment tokens, API keys, raw provider bodies, raw prompts, descriptions, comments, body data, and workflow internals, serialize WorkItem responses, and assert those markers are absent while allowlisted labels and titles remain present.

Acceptance outcome 3 is provider-live hierarchy behavior without a daemon cache. Evidence must include backend or adapter tests proving each WorkItem list/show call invokes the WorkItem provider capability, applies the list/show bounds, and does not read or write a provider cache, retained workflow artifact, scheduler read model, or dispatch state. Fixture tests should prove Linear parent/child projection, zero-child parents, truncated child pages, truncated labels, GraphQL error mapping, malformed payload mapping, missing parent not-found behavior, and provider mismatch behavior.

Acceptance outcome 4 is query transport parity. Evidence must include local backend tests, local control server query round-trips, and remote envelope round-trips proving the same WorkItem payload survives `query` request decoding, backend execution, response encoding, and response decoding. Existing `status`, `metrics`, `task_list`, `task_show`, `outbox_list`, and `outbox_show` tests must stay green.

Acceptance outcome 5 is repository validation and linting. From the repository root, run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; each command must exit zero. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Deferred operator evidence is optional after implementation: with a running daemon and a Linear-backed configuration, manually exercise `work_item_list` and `work_item_show` through the local or remote query path available at that time, and expect safe WorkItem JSON with parent/child summaries, exact state metadata, truncation flags, and no secret or raw-body markers. Browser/UI dogfood is intentionally deferred until the Work screen consumes this contract.

## Rollout, Recovery, and Idempotence

The rollout is additive: publish new supported query names and leave existing task queries in place. If the WorkItem contract causes trouble, remove `work_item_list` and `work_item_show` from `supported_queries`, return `unsupported_query` from the WorkItem backend, or disable the adapter WorkItem capability while leaving existing daemon status, metrics, task, and outbox queries unaffected.

Recovery requires no data migration. The projection is read-only, performs provider-live requests, and writes no provider cache, ledger record, retained workflow artifact, or local state file. Provider failures should be contained to the WorkItem query response and must not affect dispatch, operator commands, outbox replay, or legacy task queries.

Re-running implementation steps is idempotent because generated ids are deterministic from provider and remote task ids, query cursors are Scherzo-owned offset cursors, tests use fake transports/fixtures, and no persistent data is written. Test fixtures that create temporary files must use their own `test/tmp/` paths and overwrite or clean them so validation can be rerun safely.

## Open Questions and Clarifications Needed

No blocking clarification is required before implementation. The chosen defaults may be tuned after UI dogfooding if 10 list subtasks, 50 show subtasks, or 50 labels are too small or too large. A follow-up product decision should decide whether `task_list`/`task_show` are eventually aliased to WorkItem queries, kept for operators, or removed once the UI and `scherzoctl` surfaces settle.
