# LIV-1024 startup tracker refresh scoping and degraded boot review

This is a focused ExecPlan review for LIV-1024. It is documentation-only; the follow-up implementation task owns code changes and validation evidence.

## Purpose / Big Picture

Scherzo daemon startup should no longer depend on refreshing every task ever recorded in the ledger. After implementation, old finished-only ledger history will not increase boot latency or outage probability, and a tracker outage during startup will degrade into safe parked or unrefreshed recovery state instead of an external-supervisor crash loop. Operators should be able to see that affected tasks were not silently resumed and should have evidence explaining whether a task was truly unavailable, archived or deleted, or merely not refreshed because the tracker was unavailable.

## Problem Framing and Constraints

The current startup path replays the ledger, calls `recovery.known_task_refs(replayed.projection)`, refreshes those refs from the tracker in chunks, and aborts startup if any chunk returns an error. The broad ref source includes finished runs, finished workflow runs, all issue counters, known workspaces, command history, outbox history, parked issues, and retries, so manual-only compaction is the only current bound on boot-time tracker reads. A transient Linear outage can therefore become a daemon crash loop, and the probability and latency of boot failure grow with historical ledger size.

Successful refreshes have a different failure mode: Linear may omit archived or deleted issues without returning an adapter error. Today that omission is mapped to `IssueUnavailable`; active workflow runs are parked and interrupted, but legacy interrupted runs are only warned about and can leave workspaces uncleaned. The implementation must preserve the park-don't-resume invariant, avoid changing public control APIs or ledger schema unless explicitly justified, keep multi-tracker task refs intact, and comply with the repository's Gleam format, test, `glinter`, and `scherzo_lint` gates.

## Strategy Overview

The right-sized approach is to separate "all refs the projection knows" from "refs startup recovery actually needs refreshed now." Add a recovery-scoped ref inventory based on active workflow candidates, unfinished legacy runs, currently parked entries, scheduled task retries, and counters that can still affect non-terminal recovery decisions; do not include tasks whose only remaining trace is a finished run, historical command, historical outbox, or known workspace. Keep the broad `known_task_refs` behavior for non-recovery callers that need historical task identity.

Then change startup refresh from all-or-nothing to a typed refresh outcome. Successful omissions should become deliberate `issue_unavailable` or `issue_archived_or_deleted` recovery decisions, while adapter errors should be retried with bounded backoff and then represented as `tracker_refresh_unavailable` for only the affected refs. Recovery planning must consume that distinction so unavailable issues are parked or cleaned intentionally, but unrefreshed tasks never resume, retry, or clean up as if the issue were confirmed missing.

## Alternatives Considered

Doing nothing was rejected because it leaves startup latency and failure probability proportional to ledger history and lets supervisors amplify a transient tracker outage into a crash loop. Relying on `scherzo ctl state compact` was rejected because compaction is manual, operationally optional, and does not solve transient tracker failure for genuinely active refs. Retrying forever was rejected because it still blocks boot and can look like a hang. Treating every refresh error as `IssueUnavailable` was rejected because it conflates tracker outage with archived/deleted issues and could cancel retries or clean workspaces based on missing evidence. Replacing startup refresh with a full background reconciliation service was rejected as larger than needed for this recovery slice.

## Risks and Countermeasures

The main safety risk is silent resumption after a failed refresh. Countermeasure: the implementation must thread a typed unrefreshed state into workflow finalization and retry recovery, and tests must prove no workflow resumption or retry timer is produced for an affected ref. Another risk is over-pruning refs and losing needed recovery decisions; countermeasure: tests must enumerate each current consumer of refreshed issues and assert that active workflow runs, unfinished legacy runs, parked entries, scheduled retries, and non-terminal counter cases remain included.

Archived or deleted issues can be mistaken for transient outages. Countermeasure: successful refresh omissions and adapter errors must be distinct in records, warnings, park reasons, and tests. Durable ledger changes are also risky: appending park, interrupt, retry-cancel, or cleanup records must remain idempotent on repeated degraded boots. Countermeasure: add duplicate/conflict tests that run recovery twice and prove the second run does not append conflicting records or schedule duplicate timers. Scope creep into provider-live behavior, cache behavior, public protocol, workflow schemas, or docs/helper migration should stop the implementation for rescoping unless it is only adding tests or documentation evidence.

## Scope Boundaries

In scope: `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/orchestrator/startup_recovery.gleam`, and targeted tests in `test/state_projection_test.gleam`, `test/state_recovery_test.gleam`, and `test/orchestrator_startup_recovery_test.gleam`. The implementation may add small internal types or helpers for recovery refresh scope and refresh outcomes, and may add deterministic startup retry dependencies such as injectable sleep/backoff for tests.

Out of scope: changing tracker provider APIs, Linear query semantics, public daemon/control commands, workflow YAML semantics, scheduled-job publication behavior, event hub shape, provider-live behavior, cache behavior, docs/helper migration, and broad ledger compaction. The existing broad `projection.known_task_refs` may remain for historical identity consumers; startup recovery should call the new scoped helper instead. Provider-live and cache paths may be inspected or covered only to prove behavior is unchanged; they must not be altered without rescoping. No browser validation is applicable, and no docs or root-helper migration is required for this implementation slice.

## Milestones

Milestone 1 establishes the ref-scope contract. At the end, targeted tests in `test/state_projection_test.gleam` document which recovery decisions consume refreshed issues. The acceptance evidence must prove finished-only tasks, known-workspace-only tasks, completed command history, and completed outbox history do not force startup tracker reads, while active workflow runs, unfinished legacy runs, parked entries, scheduled retries, and conservative non-terminal counter refs remain included.

Milestone 2 introduces typed refresh outcomes and bounded startup retry. At the end, targeted tests in `test/orchestrator_startup_recovery_test.gleam` prove tracker adapter errors are retried with deterministic backoff, persistent failure returns a degraded recovery outcome instead of `StartupError("recovery_issue_fetch_failed", ...)`, successful chunks remain usable, and failed refs are not silently treated as deleted issues.

Milestone 3 threads unavailable versus unrefreshed issue state through recovery planning. At the end, targeted tests in `test/state_recovery_test.gleam` and `test/orchestrator_startup_recovery_test.gleam` prove active workflow runs, scheduled retries, parked entries, and unfinished legacy runs make deliberate park, cancel, cleanup, or hold decisions based on whether the issue was confirmed omitted or merely not refreshed. The evidence must explicitly show no workflow resumption, retry timer, or cleanup is produced for a merely unrefreshed ref.

Milestone 4 hardens archived/deleted legacy handling and idempotence. At the end, the legacy interrupted-run missing-issue path appends deliberate evidence and produces cleanup or park work when a workspace is known, while duplicate/conflict tests prove repeated degraded recovery does not append conflicting records or schedule duplicate timers.

Milestone 5 completes pre-publish validation and operational evidence. At the end, targeted tests, the full Gleam test suite, format check, `glinter`, and `scherzo_lint` pass; the implementation records that no provider-live, cache, public protocol, workflow schema, scheduled-job publication, or docs/helper behavior changed. A live daemon dogfood restart is useful but deferred to a human/operator after implementation handoff, not a pre-publish gate.

## Progress

- [x] (2026-06-11T16:09Z) Read the prepared target file and confirmed the default review-doc directory is `docs/plans`.
- [x] (2026-06-11T16:09Z) Inspected the current startup recovery, projection, and state recovery code paths named by the issue.
- [x] (2026-06-11T16:09Z) Wrote this focused review document without implementing the code change.
- [x] (2026-06-11T16:09Z) Validated this review document with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1024-startup-tracker-refresh-scoping-degraded-boot-review.md`; it reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-06-11T16:20Z) Incorporated review feedback by making milestone acceptance evidence, pre-publish test and lint obligations, out-of-scope provider-live/cache/docs-helper behavior, and deferred human/operator dogfood timing explicit.
- [x] (2026-06-11T16:23Z) Revalidated the edited review document with `direnv exec . workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1024-startup-tracker-refresh-scoping-degraded-boot-review.md`; it reported `REVIEW_DOC_VALID=ok`.

## Decision Log

- Decision: Write a new LIV-1024 review document under `docs/plans` rather than editing an older startup recovery plan. Rationale: this task has a specific resilience and scoping outcome with its own follow-up implementation bundle. Date: 2026-06-11.
- Decision: Keep broad historical ref enumeration separate from startup recovery ref enumeration. Rationale: other callers may still need historical task identity, but startup recovery should only pay tracker-read cost for refs that affect boot safety. Date: 2026-06-11.
- Decision: Distinguish successful refresh omission from adapter refresh failure. Rationale: archived/deleted issues and tracker outages require different operator messages and different recovery safety behavior. Date: 2026-06-11.
- Decision: Require automated evidence before publish and defer any live operator dogfood to after handoff. Rationale: the behavior is startup-internal and can be covered deterministically with fake tracker adapters; a human restart check is useful but not required before implementation publish. Date: 2026-06-11.
- Decision: Keep provider-live behavior, cache behavior, docs/helper migration, scheduled-job publication, and browser validation out of this implementation pack unless the implementer stops for explicit rescoping. Rationale: review feedback requested these boundaries and evidence obligations be visible without expanding the startup recovery slice. Date: 2026-06-11.

## Validation and Acceptance

This review document is accepted when `test -f docs/plans/LIV-1024-startup-tracker-refresh-scoping-degraded-boot-review.md` succeeds and `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1024-startup-tracker-refresh-scoping-degraded-boot-review.md` reports `REVIEW_DOC_VALID=ok` with every required level-2 section present and non-empty.

The follow-up implementation is accepted only with concrete pre-publish evidence that targeted tests cover: startup refresh excludes finished-only history; active workflow, parked, scheduled retry, unfinished legacy, and non-terminal counter refs remain refresh-relevant; bounded retry/backoff occurs on transient refresh errors; persistent refresh failure boots degraded without workflow resumption, retry scheduling, or cleanup for unrefreshed refs; successful omission of an archived/deleted issue parks or cancels deliberately; the legacy interrupted-run missing-issue path records a deliberate outcome and cleanup/park evidence; repeated degraded recovery is idempotent and duplicate-safe. Required commands from the repository root are `direnv exec . gleam test test/state_projection_test.gleam test/state_recovery_test.gleam test/orchestrator_startup_recovery_test.gleam`, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, all expected to pass before publish. The implementation evidence must also state that provider-live behavior, cache behavior, public protocol, workflow schema, scheduled-job publication, browser behavior, and docs/helper migration behavior were not changed. No browser check is applicable. A post-implementation manual/operator check may be collected after handoff by restarting a daemon against a retained workspace with an interrupted workflow and a fake or unavailable tracker, then recording that the daemon stays up and affected tasks are visible as parked or unrefreshed rather than resumed.

## Rollout, Recovery, and Idempotence

Rollout should be additive: introduce the scoped helper and tests, add typed refresh outcomes behind startup recovery, then update recovery decisions and remove only obsolete all-or-nothing startup fetch behavior. No feature flag or data migration is required if existing record types are reused; if a new durable record type is proposed, it must be backwards-compatible and separately justified. Recovery is to revert the implementation commits; because the plan should not alter public provider APIs or workflow schemas, rollback should not require external cleanup. Startup recovery itself must be safe to run repeatedly: a second degraded boot must not append duplicate interrupts, duplicate parks, duplicate retry cancellations, duplicate cleanup requests, or duplicate timers for the same recovery condition.

## Open Questions and Clarifications Needed

The implementation should confirm the exact definition of "non-terminal counter" against current projection data before coding it. The intended boundary is that counters with no pending recovery decision and whose only remaining evidence is finished history must not force tracker refresh; if the current projection lacks enough timestamp or terminal evidence to prove that safely, the implementation should start with the conservative nonzero-counter subset and record the limitation for a follow-up.
