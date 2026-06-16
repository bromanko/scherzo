# WorkItemList query controls for active, archive, search, and sort

This ExecPlan review document is the human review surface for LIV-1164. It keeps the checked-in plan concise; mechanical implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

The Work screen needs to ask the daemon for the list it wants instead of interpreting Linear-specific state names, labels, and ordering rules in the UI. After the follow-up implementation, a `work_item_list` query can request active work, archived terminal work, text search, and a documented stable sort while keeping provider-specific filtering details behind tracker adapters.

The visible outcome is a daemon-owned contract: UI and operator query clients can send the same provider-neutral request shape and receive deterministic WorkItem pages with safe cursors, search hits over keys, titles, and labels, and no Linear-only query assumptions in the caller.

## Problem Framing and Constraints

`work_item_list` currently carries `states`, `limit`, and `cursor`. That is enough for a narrow category-filtered list, but it does not express the Work screen controls for Active versus Archive, text search, or sort order. The Linear adapter also rejects an unfiltered WorkItem list today, so a default query does not yet map cleanly to the active non-terminal list the UI expects.

The solution must remain tracker-neutral at the daemon query boundary. `active`, `archive`, search text, and sort names should be Scherzo concepts, not raw Linear GraphQL filter fragments. Existing WorkItem redaction and bounding rules still apply: query results must expose only summary fields, bounded labels and subtasks, timestamps, source metadata, and page information, not descriptions, comments, credentials, raw provider bodies, prompts, workflow internals, or a new provider cache. This issue is not a docs/helper migration: implementation must not change `workflows/dogfood/`, `.scherzo/workflows/schemas/`, or ExecPlan helper scripts for these query controls; documentation work is limited to the checked-in WorkItem query contract or this review artifact.

## Strategy Overview

Extend the WorkItem query contract with an explicit state filter, optional search text, and a stable sort. The default state filter should be `active`, meaning non-terminal categories by default; `archive` should mean terminal categories such as done, canceled, and duplicate; and an explicit category filter should remain available for tests and future operator use. Search should be case-insensitive over display id or provider id, title, and label names. The initial default sort should be `updated_desc`, with deterministic tie-breaking by daemon WorkItem id so pagination remains stable when timestamps match.

Adapters should receive the normalized request and may optimize however their provider supports it. The initial Linear implementation should use bounded adapter-side scanning and post-filtering over the project/task scope, reusing the existing allowlisted WorkItem projection and documenting the scan bound as a limitation for large Linear projects. This is proportionate because it gives the UI correct semantics now without exposing raw provider filters or designing a cache; a later adapter optimization can replace the scan while preserving the same tests and wire contract.

## Alternatives Considered

Letting the UI derive active/archive/search/sort from provider details was rejected because it would hard-code Linear behavior into the Work screen and make future tracker adapters diverge. Only adding more `states` values was rejected because it still leaves Active versus Archive defaults, search, sort, and cursor compatibility underspecified. Implementing a full provider query optimizer and cache was rejected for this slice because provider-live WorkItem reads already exist, the first UI need is modest, and cache invalidation would create more rollout and recovery risk than the feature requires.

## Risks and Countermeasures

The main risk is ambiguous active/archive meaning. The countermeasure is to define active as non-terminal WorkItem state categories by default and archive as terminal categories, then cover ready, active, done, canceled, and duplicate examples in tests.

A second risk is cursor misuse when a caller changes filter, search, or sort between pages. The countermeasure is a WorkItem-specific cursor that includes a normalized query fingerprint and is rejected before adapter access when it is malformed or does not match the current query.

A third risk is incomplete or misleading search/sort on large Linear projects. The countermeasure is to keep the behavior deterministic inside a documented scan bound, return normal page metadata, and document that the initial Linear adapter performs bounded post-filtering until a provider-backed optimization replaces it.

A fourth risk is provider leakage or redaction regression while expanding query fields. The countermeasure is to keep the WorkItem response DTO unchanged except page contents, add codec/backend/adapter tests for the new request fields, and rerun the existing redaction and WorkItem fixture tests.

## Scope Boundaries

In scope for the follow-up implementation are the WorkItem list request types, query codec JSON fields, WorkItem cursor validation, backend request normalization, fake tracker adapter filtering/sorting for tests, Linear WorkItem adapter bounded scan/search/sort behavior, documentation of the Linear limitation, and automated coverage for active, archive, search hit and miss, label search, stable sort, cursor continuation, invalid cursor, and invalid filter handling.

Out of scope are browser UI rendering, mutating WorkItem actions, provider cache design, live Linear dogfood as a pre-publish gate, changing `task_list` semantics, exposing raw provider filter syntax, recursive hierarchy expansion, comments or descriptions in WorkItem responses, docs/helper migrations, changes under `workflows/dogfood/`, changes under `.scherzo/workflows/schemas/`, and ExecPlan helper-script migrations.

## Milestones

Milestone 1 establishes the contract. Reviewers should see new provider-neutral WorkItem state-filter and sort types, default active behavior, JSON encode/decode support for `state_filter`, `search`, and `sort`, invalid filter errors, and checked-in contract documentation that names the supported request values, cursor fingerprint behavior, provider-live/cache expectations, and the Linear scan limitation without migrating workflow helpers or schemas.

Milestone 2 proves backend determinism. Reviewers should see the query backend normalize old and new request forms, reject malformed or mismatched WorkItem cursors before adapter calls, include a query fingerprint in continuation cursors, clamp limits as before, and pass the normalized state filter, search, sort, offset, and bounds to the WorkItem adapter capability.

Milestone 3 implements provider-neutral filtering helpers and fake-adapter behavior. Reviewers should see reusable WorkItem helpers for active/archive category expansion, case-insensitive search, total-order sorting, and offset pagination, plus fake adapter tests proving active filter, archive filter, search hit and miss, label search, stable sort, and cursor continuation without Linear dependencies.

Milestone 4 implements the Linear adapter slice. Reviewers should see the Linear WorkItem list path allow scoped unfiltered scans when a state filter cannot be reduced to configured Linear state names, reuse the allowlisted summary query, apply the same helper semantics after recategorization, enforce a documented scan bound, and preserve existing detail, label truncation, error mapping, and redaction behavior.

Milestone 5 completes validation and handoff. Reviewers should see focused tests plus full Gleam test, format, `glinter`, and `scherzo_lint` evidence passing under `direnv exec .`, with explicit notes that browser, live Linear, provider-cache, and dogfood evidence are deferred after implementation or UI integration.

## Progress

- [x] (2026-06-16 17:44Z) Read `workflows/dogfood/guidance/exec-plan.md` and confirmed this workflow requires a concise review document plus a structured implementation pack.
- [x] (2026-06-16 17:44Z) Inspected the current WorkItem domain, query codec, backend, cursor, tracker adapter, Linear WorkItem query, Linear adapter, fake tracker adapter, and existing WorkItem tests to ground this review in the repository.
- [x] (2026-06-16 17:44Z) Authored this review document as a planning artifact only; no production WorkItem query behavior was implemented here.
- [x] (2026-06-16 18:00Z) Incorporated review feedback clarifying pre-publish documentation evidence, helper/schema migration boundaries, provider-live/no-cache behavior, full validation, and deferred manual dogfood checks.

## Surprises & Discoveries

The current `WorkItemListQuery` already has a `states` field, but the Linear WorkItem adapter rejects an empty state list for unfiltered reads. That matters because the desired UI default is an active list, not a provider-specific error for a request that omitted state names.

The existing Linear WorkItem projection already fetches the fields needed for search and stable sorting: identifier, provider id, title, labels, created timestamp, updated timestamp, and normalized state category. This reduces implementation risk because the new behavior can reuse the allowlisted projection without exposing descriptions, comments, or raw Linear payloads.

## Decision Log

- Decision: Add explicit WorkItem state-filter and sort concepts rather than asking the UI to send provider-specific state names or raw filters. Rationale: The query boundary must stay tracker-neutral and future adapters need one Scherzo-owned contract. Date: 2026-06-16.
- Decision: Make `active` the default WorkItem list filter and define it as non-terminal categories, while `archive` means terminal categories. Rationale: This matches the Work screen control model and avoids making an omitted filter mean unsupported provider-specific behavior. Date: 2026-06-16.
- Decision: Use `updated_desc` as the documented default sort with daemon id tie-breaking. Rationale: The UI needs recent work first, and a total ordering makes cursor continuation deterministic even when provider timestamps tie or are absent. Date: 2026-06-16.
- Decision: Implement the first Linear search/sort slice with bounded adapter-side scanning and post-filtering, not a cache or raw GraphQL passthrough. Rationale: This is the smallest provider-neutral implementation that can be tested deterministically and optimized later without changing the contract. Date: 2026-06-16.
- Decision: Treat docs/helper and workflow-schema migration as out of scope for this feature. Rationale: LIV-1164 is a daemon query-contract slice, and changing ExecPlan helper machinery would increase rollout risk without helping the Work screen list controls. Date: 2026-06-16.

## Outcomes & Retrospective

This review has not implemented the feature. The expected outcome of the follow-up is that `work_item_list` can power Active/Archive, search, and recent-first ordering from daemon queries, with deterministic cursors and invalid-filter failures proven by tests. The main known limitation is the initial Linear adapter's bounded scan for search/sort on large projects, which must be documented before publish.

## Validation and Acceptance

This planning artifact is accepted when this file exists under `docs/plans/`, Scherzo's review-doc validator reports it valid, every required section is present and non-empty, and the structured implementation-pack submission for LIV-1164 is captured.

The follow-up implementation is accepted only with pre-publish automated evidence for every requested behavior: active filter tests must show non-terminal WorkItems returned and terminal WorkItems excluded; archive filter tests must show done, canceled, or duplicate WorkItems returned and active WorkItems excluded; search tests must cover display id or provider id, title, miss, and label-name hits; stable-sort tests must prove `updated_desc` ordering and daemon-id tie-breaking; cursor tests must prove continuation under the same filter/search/sort and rejection when a cursor is malformed or reused with different query options; invalid filter tests must cover unknown state filters, invalid sort values, and invalid category values.

Documentation evidence is also a pre-publish requirement. The checked-in contract documentation must name the accepted `state_filter` values, the relationship between `active`, `archive`, and explicit category filters, the `search` matching fields, the supported `sort` values, the cursor fingerprint rejection rule, the Linear bounded-scan limitation, and the fact that WorkItem reads remain provider-live with no provider cache. It must also state that this feature does not migrate ExecPlan helper scripts or workflow schemas.

Repository validation is also a pre-publish requirement: from the repository root run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting each command to exit zero. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through `direnv exec .`.

Manual/browser/dogfood evidence is deferred after the daemon implementation or to the UI integration follow-up. If an operator performs it later, the observable result should be that a running daemon answers local or remote `work_item_list` queries for active, archive, search, and `updated_desc` sort with safe WorkItem JSON and documented scan-bound expectations. No provider cache evidence is required because this plan keeps WorkItem reads provider-live.

## Rollout, Recovery, and Idempotence

Rollout is additive: publish new optional WorkItem list request fields while preserving existing `work_item_show`, `task_list`, and `task_show` behavior. Old `work_item_list` callers that omit the new fields should receive the documented active/default-sort behavior instead of a provider-specific unfiltered error.

Recovery is straightforward because the change is read-only and writes no tracker data, daemon cache, ledger entry, retained artifact, or workflow state. If the new list semantics cause trouble, revert the query-type, codec, backend, adapter, and documentation changes or temporarily return `unsupported_query` for unsupported filter combinations while leaving existing daemon status, task, outbox, and WorkItem show queries intact.

Implementation steps are idempotent: tests use fake adapters or fixed Linear fixtures, query cursors are deterministic from normalized query options and offsets, and rerunning validation commands does not mutate provider state. Temporary test artifacts, if any, must live under `test/tmp/` and be overwrite-safe.

## Open Questions and Clarifications Needed

No open question blocks implementation. A later optimization may replace the Linear bounded scan with provider-backed search or ordering, but it must preserve the same daemon query contract, cursor validation semantics, redaction rules, and acceptance tests.
