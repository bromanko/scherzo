# LIV-553 scheduled runtime extraction review

This review is a focused planning artifact for LIV-523 rank 1. It defines the behavior-preserving extraction boundary and acceptance evidence for a follow-up implementation task; it does not implement the extraction.

## Purpose / Big Picture

Scherzo's daemon should remain the stable public actor for daemon mode while scheduled-job runtime state stops accumulating inside `src/scherzo/orchestrator/daemon.gleam`. After the follow-up implementation, maintainers should be able to reason about scheduled due admission, pending starts, retries, failure reporting, and scheduled-worker completion in a dedicated scheduled runtime module, with unchanged daemon messages, EventHub events, and ledger records proving compatibility. This review also makes the implementation evidence explicit: characterization tests come before code movement, full validation and lint gates are pre-publish requirements, and any manual/operator smoke is deferred unless the implementation intentionally changes operator-visible behavior.

## Problem Framing and Constraints

The current daemon is a large compatibility shell that also owns scheduled runtime details: the prepared tree reports `7315` lines in `src/scherzo/orchestrator/daemon.gleam`, and the file contains scheduled state fields such as `scheduled_next_due`, `pending_scheduled_starts`, `scheduled_retries`, and `scheduled_report_retries`. Rank 1 must preserve behavior rather than redesign daemon mode: no public control protocol, Linear behavior, workflow YAML semantics, EventHub shape, ledger record shape, OTP/process architecture, docs/helper migration, provider-live behavior, or cache behavior should change in this slice. The implementation pack must therefore specify concrete acceptance evidence instead of treating a shorter daemon file as success.

## Strategy Overview

Extract scheduled runtime state and decisions into `src/scherzo/orchestrator/scheduled_runtime.gleam` while keeping `daemon.gleam` as the actor, mailbox owner, dependency holder, logger, and compatibility shell. Keep pure schedule math and backoff helpers in `src/scherzo/orchestrator/schedule_core.gleam`. Prefer a callback/action-returning boundary so the daemon can still perform process spawning, timer creation, ledger appends, EventHub publication, and side-effect enqueueing; add `src/scherzo/orchestrator/scheduled_worker_runner.gleam` only if scheduled worker execution cannot remain cleanly callback-based inside the daemon shell. Sequence the follow-up work as characterization first, state/runtime extraction second, worker-finish and failure-report extraction third, and full validation last.

## Alternatives Considered

Leaving scheduled code in the daemon and adding only line-count guardrails was rejected because it would not reduce the current ownership problem. A big-bang daemon rewrite was rejected because scheduled runtime is only one rank in LIV-523 and broad lifecycle changes would hide regressions. Moving `schedule_core.gleam` into the new runtime was rejected because its pure date and backoff helpers already have focused tests and should remain reusable and easy to verify. Requiring a pre-publish dogfood run was rejected for this internal behavior-preserving extraction because automated daemon parity evidence is a stronger gate; a human/operator smoke can still be requested after implementation handoff.

## Risks and Countermeasures

The highest risk is changing lifecycle ordering: due records, pending records, worker session events, retry timers, failure reports, and blocked intervals must still occur in the same observable order. The countermeasure is to add characterization tests before moving code, keep daemon-level parity tests through the extraction, and make stale timer ticks and failure-report retry paths explicit. A second risk is creating a new god module; the countermeasure is to keep `scheduled_runtime.gleam` limited to scheduled state and decisions, and to split scheduled process execution into `scheduled_worker_runner.gleam` only if review size or dependency shape demands it. A third risk is timer or generation drift; the countermeasure is negative coverage for stale `ScheduledRetryTick` and `ScheduledReportRetryTick` messages. A fourth risk is acceptance drift, where the extraction is considered done after code movement but before evidence exists; the countermeasure is to require targeted runtime tests, daemon parity tests, format, full tests, `glinter`, and `scherzo_lint` output in the implementation evidence.

## Scope Boundaries

In scope: scheduled job due-state, pending starts, scheduled retry timers, scheduled report retry timers, scheduled worker finish handling, scheduled failure report orchestration, and the daemon calls needed to delegate those responsibilities. Also in scope: keeping and extending tests that prove due job admission, overlap skip, max concurrency blocking, stale retry ticks, failure report success/failure/retry, report retry without rerunning a workflow, and unchanged EventHub/ledger outcomes.

Out of scope: LIV-523 ranks 2 through 6, startup recovery beyond the scheduled runtime state needed by rank 1, worker/YAML lifecycle extraction beyond the optional scheduled worker runner split, operator/remote command extraction, transition/effect bridge extraction, daemon boundary guardrail implementation, provider-live behavior, cache behavior, docs/helper migration, and any public behavior change. If implementation discovers it must touch provider-live, cache, workflow helper, schema, script, or documentation-helper paths to complete rank 1, it should stop and rescope rather than silently expanding this plan.

## Milestones

Milestone 1 captures the current scheduled behavior before code movement. The implementer should keep `test/orchestrator_schedule_core_test.gleam` green and add or extend characterization coverage for due job admission, overlap skip, max concurrency blocking, stale scheduled retry ticks, stale scheduled report retry ticks, failure report success/failure/retry, report retry without rerunning the workflow, and unchanged EventHub/ledger outcomes. The outcome is a green baseline that can fail if the extraction changes observable daemon behavior.

Milestone 2 introduces `src/scherzo/orchestrator/scheduled_runtime.gleam` as the owner of scheduled state, due admission decisions, pending-start state transitions, scheduled retry timer bookkeeping, and scheduled report retry timer bookkeeping. `test/orchestrator_scheduled_runtime_test.gleam` should exercise the new pure/action-returning boundary directly, including generation mismatches and blocked intervals. The outcome is that the daemon stores one scheduled runtime value and applies returned actions without changing public message constructors.

Milestone 3 delegates daemon scheduled admission and retry handling to the new runtime while preserving daemon responsibilities for mailbox ownership, dependencies, logging, ledger writes, EventHub publication, timer creation, and process spawning. The outcome is that existing daemon scheduled tests still observe the same command messages, ledger records, projection states, and log/event names after the state movement.

Milestone 4 moves scheduled worker finish handling and scheduled failure report orchestration behind the scheduled runtime boundary. Its outcome is parity for success, needs-human/failure, retry exhaustion, failure-report success, failure-report failure, report retry, and report retry without rerunning the workflow.

Milestone 5 keeps the extraction reviewable. If the rank 1 diff becomes too large, split it into scheduled-runtime state first and scheduled-worker process execution second; create `scheduled_worker_runner.gleam` only for the second slice if callbacks make the first module too process-heavy. The split must not defer required test coverage; each accepted slice must keep the daemon deployable and green.

Milestone 6 completes full validation and scope audit. Its outcome is passing targeted runtime/daemon tests, `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, plus evidence that no provider-live, cache, docs/helper, public protocol, Linear, workflow YAML, EventHub, ledger, or OTP/process behavior changed. No browser check is applicable; no pre-publish dogfood run is required unless implementation intentionally creates an operator-visible behavior change, and otherwise any manual daemon smoke is a deferred human/operator check after handoff.

## Progress

- [x] (2026-05-26) Read the prepared target metadata and confirmed the review document belongs directly under `docs/plans/`.
- [x] (2026-05-26) Reviewed `docs/plans/LIV-523-daemon-decomposition-v2.md` and narrowed this derivative artifact to rank 1 only.
- [x] (2026-05-26) Checked current scheduled runtime locations in `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/schedule_core.gleam`, `src/scherzo/orchestrator/worker_registry.gleam`, and existing scheduled daemon tests.
- [x] (2026-05-26) Wrote this review document without implementing scheduled runtime extraction code.
- [x] (2026-05-26) Validated this review document with the Scherzo review-doc validator.
- [x] (2026-05-26) Incorporated review feedback by tightening acceptance evidence, test obligations, milestone specificity, manual/dogfood timing, docs/helper migration scope, provider-live/cache non-goals, full validation, and linting requirements in this review document and the updated implementation pack.
- [x] (2026-05-26) Re-checked that every required level-2 review-doc section is present and non-empty, then re-ran the Scherzo review-doc validator after review incorporation.

## Decision Log

- Decision: Plan `src/scherzo/orchestrator/scheduled_runtime.gleam` as the default extraction target. Rationale: rank 1 is the largest coherent scheduled-state owner and the source plan explicitly recommends it as the first slice. Date: 2026-05-26.
- Decision: Keep `src/scherzo/orchestrator/schedule_core.gleam` as the pure helper module. Rationale: it already contains deterministic interval, run-id, and retry/backoff helpers with focused tests. Date: 2026-05-26.
- Decision: Treat `src/scherzo/orchestrator/scheduled_worker_runner.gleam` as optional. Rationale: process execution should stay callback-based if that keeps the daemon compatibility shell small and the scheduled runtime boundary testable. Date: 2026-05-26.
- Decision: Require no pre-publish dogfood run for this internal extraction if automated parity evidence is complete. Rationale: the slice is behavior-preserving and has no intended operator-visible feature change; any manual operator smoke is deferred to a human/operator after implementation handoff. Date: 2026-05-26.
- Decision: Make acceptance evidence and scope guards first-class in the implementation pack. Rationale: review feedback identified acceptance evidence, test obligations, docs/helper migration, provider-live/cache behavior, full validation, and linting as items that must be mechanically visible before handoff. Date: 2026-05-26.

## Validation and Acceptance

This planning artifact is accepted when `test -f docs/plans/LIV-553-scheduled-runtime-extraction-review.md` succeeds and `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-553-scheduled-runtime-extraction-review.md` reports `REVIEW_DOC_VALID=ok` with `REVIEW_DOC_PATH=docs/plans/LIV-553-scheduled-runtime-extraction-review.md`. That validator was run on 2026-05-26 before review incorporation and rerun after this update; every required level-2 section in this document is intentionally present and non-empty.

The follow-up implementation is accepted only with concrete evidence that `test/orchestrator_schedule_core_test.gleam` remains, `test/orchestrator_scheduled_runtime_test.gleam` exists, and daemon tests keep or extend coverage for due job admission, overlap skip, max concurrency blocking, stale `ScheduledRetryTick`, stale `ScheduledReportRetryTick`, failure report success/failure/retry, report retry without rerunning a workflow, and unchanged EventHub/ledger outcomes. Before publish, the implementation owner must run `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` and include passing output in the implementation evidence. No browser check is applicable; no pre-publish operator dogfood check is required if those automated gates pass, and any manual daemon smoke is explicitly deferred to a human/operator after the implementation workflow. Provider-live, cache, docs/helper, public control protocol, Linear behavior, workflow YAML semantics, EventHub shape, ledger record shape, and OTP/process architecture must remain unchanged; any implementation that needs to alter them must update the scope before proceeding.

## Rollout, Recovery, and Idempotence

Rollout is a normal code review for a behavior-preserving internal extraction. Recovery is to revert the extraction commit or, if split, revert the scheduled worker runner slice independently while leaving the characterized runtime tests in place when they still describe current behavior. The planned commands are idempotent, the new tests can be rerun safely, and the daemon must remain deployable after every accepted milestone. Because no data format, cache, provider-live, docs/helper, or operator protocol changes are planned, rollback should not require data cleanup, cache invalidation, helper migration, or operator retraining. If an implementation begins touching out-of-scope surfaces, recovery is to stop, keep any useful characterization tests, and re-scope before merging behavior changes.

## Open Questions and Clarifications Needed

No open question blocks the implementation handoff. The only sizing clarification is review-time: if the initial diff is too large, split rank 1 into scheduled-runtime state first and scheduled-worker process execution second rather than expanding scope into LIV-523 ranks 2 through 6.
