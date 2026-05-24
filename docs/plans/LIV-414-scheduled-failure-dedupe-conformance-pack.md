# Draft scheduled-failure dedupe conformance pack

This ExecPlan v2 review document is the human review surface for LIV-414. It plans a future optional conformance pack only; exact implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo already has a black-box tracker conformance foundation and optional side-effect packs, but scheduled failure publication has a stricter promise: a failed scheduled job may be retried after recovery and must not create duplicate operator-visible failure tasks for the same `dedupe_key`.

After the later implementation tickets complete, an adapter author will be able to request a `scheduled_failures` conformance pack. The pack will publish scheduled-failure cases through the public driver, retry the same logical failure, verify dedupe-key recovery behavior, and produce reports that explain whether the adapter created one task, updated the existing task, leaked duplicates, lost run metadata, or failed cleanup.

## Problem Framing and Constraints

Scheduled failure publication is a side-effect capability, not part of the read-only `task_source` MVP and not a variant of simpler comment or state-transition packs. It creates or updates a backend task with operator-visible title, body, labels, target state, due time, run metadata, attempt counters, and recovery context.

The public adapter contract says `scheduled_failures.publish` must be idempotent by `dedupe_key`. Repeated publication for the same scheduled job and dedupe key must leave at most one visible open failure task, with `created` distinguishing first creation from later updates. The pack must therefore prove create, retry, remembered-task update through `previous_task_remote_id`, and dedupe-key recovery when the previous remote id is absent or stale.

This issue drafts the plan only. It must not implement the conformance pack, mutate real tracker data, materialize the canonical bundle by hand, or merge scheduled-failure coverage into the MVP or the comment/state-transition packs.

## Strategy Overview

Add `scheduled_failures` as a separate optional conformance pack selected only when `profile.requested_packs` names it and `profile.capabilities` claims `scheduled_failures`. The public operation is `scheduled_failures.publish`, with a request payload matching `ScheduledFailurePublication` and a receipt matching `ScheduledFailureReceipt`.

The pack should run deterministic create and retry sequences. The first request uses a unique run marker, `previous_task_remote_id: null`, target state, labels, due time, run id, attempt, and max attempts; it expects a newly created task receipt. A second request uses the same `dedupe_key`, the first receipt's remote id as `previous_task_remote_id`, and a later attempt; it expects an update or comment on the same task with `created: false`. A third recovery request repeats the same `dedupe_key` without a usable previous remote id to prove the adapter can recover by key rather than relying only on Scherzo's remembered task id.

A requested `scheduled_failures` pack must configure at least one backend visibility probe. Backend probes may count visible tasks and comments after public driver calls, but they must never be treated as adapter-under-test evidence. The public driver operation and normalized receipt are the conformance evidence; probes are support evidence that can falsify duplicate suppression. A probe that finds two open failure tasks for one dedupe key fails the case, and a broken probe is reported as support failure rather than silently passing the adapter.

## Alternatives Considered

One alternative is to fold scheduled failures into the comments pack because retries may update comments. That is rejected because the capability creates or updates tasks, applies labels and target state, and carries scheduled-run fields that comments do not cover.

A second alternative is to rely on existing scheduled failure unit tests around the Linear reporter. That is insufficient for black-box adapters because those tests exercise one in-repository implementation rather than an external driver protocol and cannot prove closed-box adapter behavior.

A third alternative is to accept adapter receipts without visibility checks. That would miss adapters that return plausible receipts while creating duplicate backend tasks. The chosen approach keeps probes outside the adapter evidence boundary but uses them to falsify duplicate and no-op claims.

## Risks and Countermeasures

The main risk is destructive or noisy backend mutation. Counter it with unique conformance run markers in titles, bodies, labels, and comments; manifests that require trusted cleanup hooks for real backends; and cleanup that enumerates all tasks/comments carrying the marker or dedupe key so defective duplicate runs can be removed.

A second risk is falsely passing an adapter by using privileged probes to discover or repair backend state. Counter it by rejecting fixture, hook, and probe namespaces inside `profile.adapter_operations`, invoking `scheduled_failures.publish` for every public case, and reporting probe output only as support evidence or falsification.

A third risk is under-testing recovery. Counter it with fake-driver cases where the adapter creates a duplicate on retry, ignores `previous_task_remote_id`, loses `target_state_name`, omits labels, drops due time or attempt metadata, returns `created: true` twice, returns `created: false` on first create, or reports success while leaving no visible task.

A fourth risk is leaking sensitive run context in reports. Counter it by applying existing redaction and transcript bounds to title/body excerpts, run roots, session ids, driver diagnostics, probe output, cleanup output, and CLI summaries.

A fifth risk is letting review feedback live only in this prose document while the structured implementation pack still omits acceptance evidence, test obligations, milestone-level proof, fake-driver dogfood timing, deferred live-backend checks, docs/helper inventory, provider-live/cache non-scope, or full validation and lint gates. Counter it by mirroring those obligations in the pack's concrete steps and testing notes before Scherzo materializes follow-up implementation artifacts.

## Scope Boundaries

In scope for LIV-414 is exactly this Markdown review document and one structured implementation-pack submission. No conformance runner code, fake-driver fixture, schema change, production Linear change, live backend run, or canonical bundle file belongs in this issue.

The first follow-up implementation ticket should add the shared protocol surface: pack name, capability name, operation name, payload and receipt JSON, schema fixtures, manifest validation, report summary fields, and documentation that `scheduled_failures` requires explicit request and capability claim.

The second follow-up implementation ticket should add the conformance cases and fake-driver fixtures for create, retry with `previous_task_remote_id`, dedupe-key recovery without a remembered task id, target state, labels, due time, run id, attempt, max attempts, receipt validation, and intentionally defective duplicate/no-op/metadata-loss drivers.

The third follow-up implementation ticket should harden probe, cleanup, report, redaction, fake-driver dogfood, documentation evidence, and the docs/helper migration inventory. That inventory must either prove no `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files need to change for the pack, or name the exact helper/offline contract tests that validate any intentional helper edit. It should prove duplicate suppression and cleanup are diagnosable without changing provider-live/cache behavior or requiring real tracker credentials before publish.

Out of scope are the MVP `task_source` pack, comment update semantics except where a scheduled failure update comment is observed, state-transition packs except for verifying requested target state in the scheduled failure task, remote-command and handoff retry packs, new transports, production tracker adapter behavior changes, unrelated workflow-helper rewrites, and provider-live/cache behavior changes. The conformance runner must continue issuing fresh black-box driver requests and must not introduce a cache; if a future ticket intentionally adds caching, split that work and require stale-read, invalidation, and TTL-disabling tests before acceptance.

## Milestones

Milestone 1 establishes the scheduled-failure protocol boundary. Reviewers should see `scheduled_failures` added as an optional pack and capability, `scheduled_failures.publish` accepted as a public adapter operation, payload and receipt JSON round-trip tests, manifest validation that requested-but-unclaimed packs fail before side effects, and claimed-but-unrequested capabilities selecting no scheduled-failure cases.

Milestone 2 delivers the create and remembered-retry cases. Reviewers should see fake-driver reports where the first publication includes `dedupe_key`, `previous_task_remote_id: null`, target state, labels, due time, run id, attempt, and max attempts and returns `created: true`; the second publication reuses the same key with the first remote id and returns `created: false` for the same task.

Milestone 3 proves dedupe-key recovery and falsifiability. Reviewers should see passing fake-driver evidence for retry without a usable `previous_task_remote_id`, plus intentionally defective fake-driver reports for duplicate task creation, wrong receipt identity, no visible task, incorrect `created` flags, missing target state, missing labels, and lost attempt/due-time metadata.

Milestone 4 completes cleanup, reporting, and redaction. Reviewers should see cleanup hooks that remove or close every task/comment carrying the conformance marker, cleanup-failure reports that retain created remote ids for manual recovery, duplicate-count fields, retry-classification fields, redacted request/response/probe/cleanup transcripts, and no raw secret markers in stdout or JSON reports.

Milestone 5 finishes documentation, helper inventory, and gates. Reviewers should see `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, adapter-author examples, fixture manifests, expected report fixtures, fake-driver dogfood commands that write sanitized reports under `test/tmp/tracker-conformance/`, schema fixtures, an explicit docs/helper migration inventory, `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` all passing before publish. If a follow-up implementation touches `.scherzo/workflows/scripts/*`, workflow schemas, or provider-facing helper contracts, reviewers should also see the relevant helper or offline contract tests; otherwise the evidence should explicitly state that provider-live/cache behavior and helper contracts were not changed.

## Progress

- [x] (2026-05-23) Read the LIV-414 brief, ExecPlan authoring guidance, prior tracker conformance review documents, the current tracker conformance protocol, and the scheduled failure adapter contract.
- [x] (2026-05-23) Drafted this concise review document under `docs/plans/`.
- [x] (2026-05-23) Prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-23) Validated this review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-414-scheduled-failure-dedupe-conformance-pack.md`.
- [x] (2026-05-23) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, pre-publish fake-driver dogfood, deferred live-backend checks, docs/helper inventory, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Scheduled failures are planned as a separate optional pack rather than an extension of MVP `task_source`, comments, or state transitions.
  Rationale: They combine task creation/update, retry recovery, dedupe-key idempotency, labels, target state, and scheduled-run metadata in one capability.
  Date: 2026-05-23.

- Decision: The pack requires visibility probes for falsification but does not use probes as adapter evidence.
  Rationale: Public driver receipts must prove adapter behavior, while probes only check whether the claimed visible backend outcome really happened and whether duplicates leaked.
  Date: 2026-05-23.

- Decision: Dedupe recovery must be tested without relying only on `previous_task_remote_id`.
  Rationale: Scherzo may retry after recovery with missing or stale remembered task ids; the adapter contract requires idempotency by `dedupe_key`.
  Date: 2026-05-23.

- Decision: Cleanup and report diagnostics are acceptance gates, not follow-up polish.
  Rationale: A defective adapter may create duplicate tasks during conformance; reviewers and operators need retained remote ids, duplicate counts, and cleanup status to recover safely.
  Date: 2026-05-23.

- Decision: Treat review feedback about evidence, tests, dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations.
  Rationale: The workflow materializes follow-up implementation instructions from the structured pack, so prose-only obligations would be easy for later implementers to miss.
  Date: 2026-05-23.

## Validation and Acceptance

This planning issue is accepted when this file exists at `docs/plans/LIV-414-scheduled-failure-dedupe-conformance-pack.md`, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-414-scheduled-failure-dedupe-conformance-pack.md` exits zero and prints `REVIEW_DOC_VALID=ok`, and Scherzo captures the structured implementation-pack submission for LIV-414. In the packaged workflow, the same validator is invoked as `scripts/scherzo-execplan validate-review-doc` from the workflow bundle.

The later implementation is accepted only with concrete fake-driver evidence for every scheduled-failure behavior. Protocol evidence must include schema or decoder tests for `scheduled_failures.publish` request and receipt JSON, manifest tests for requested-and-claimed, requested-but-unclaimed, and claimed-but-unrequested pack selection, and report fixtures showing scheduled-failure summaries. Case evidence must include passing reports for create, remembered retry with `previous_task_remote_id`, and dedupe-key recovery without a usable previous remote id.

Negative evidence is required before publish. Intentionally defective fake-driver runs must fail with diagnosable reports when the adapter creates duplicate visible tasks for one `dedupe_key`, changes receipt task identity across retries, returns the wrong `created` flag, ignores target state, omits labels, drops due time, run id, attempt, or max attempts, returns success with no visible task, or leaks unredacted secret markers.

Cleanup and dogfood evidence must complete before publish by running documented fake-driver manifests from the repository root, writing retained reports under `test/tmp/tracker-conformance/`, confirming duplicate-count and cleanup-status fields, and confirming stdout plus JSON reports contain `[REDACTED]` rather than raw secret markers. Optional real-backend manual evidence is deferred to a human/operator after implementation and must record only redacted report excerpts plus the cleanup command used.

Repository validation evidence must include `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Docs/helper evidence must include updates to `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, adapter-author examples, fixture manifests, and expected report fixtures that describe the scheduled-failure pack. It must also include a helper inventory: if `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files change, run the relevant helper or offline contract tests and preserve provider-live/cache semantics; if they are not changed, record that no helper migration or provider-live/cache validation was applicable. Provider-live/cache behavior is accepted as unchanged for this plan unless a future split ticket intentionally adds caching, in which case stale-read, invalidation, and TTL-disabling tests become mandatory before acceptance.

## Rollout, Recovery, and Idempotence

Rollout should be additive. Existing `task_source`, comments, state-transition, routing, remote-command, handoff, CLI, and HTTP conformance behavior remains unchanged, and the scheduled-failure pack runs only when requested and claimed.

If a follow-up implementation discovers it must alter workflow helpers, provider-facing structured-output contracts, provider-live checks, or cache behavior, that change should be split or explicitly rolled back before publishing the scheduled-failure pack. The safe default is no helper migration beyond docs and no provider-live/cache behavior change.

Recovery for this planning issue is to revise or remove only this Markdown file and resubmit the structured pack. Recovery for later implementation is to omit `scheduled_failures` from manifests or revert the scheduled-failure pack ticket while leaving the existing conformance runner and optional packs intact.

Idempotence is the core acceptance property. Every fake and real-backend run must use a unique conformance marker and a deterministic `dedupe_key`; repeated create/retry/recovery requests must leave at most one visible open failure task for that key. Cleanup must be safe to run after success, failure, or partial failure and must enumerate all duplicate tasks/comments so intentionally defective runs do not leave hidden residue.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this planning issue. During implementation, reviewers should confirm the final report field names for duplicate counts and retry classification, and whether `comment_id` is required on update receipts or only required when an update comment was actually created.
