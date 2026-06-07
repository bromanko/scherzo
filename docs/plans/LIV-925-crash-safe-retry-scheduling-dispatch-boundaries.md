# LIV-925 crash-safe retry scheduling and dispatch boundaries

This is a focused ExecPlan review for LIV-925. It is documentation-only; the mechanical implementation details are supplied through the structured implementation pack for the follow-up implementation task.

## Purpose / Big Picture

Scherzo should never lose an issue at a retry boundary merely because the last durable retry record is `RetryCancelled`. After the follow-up implementation, a daemon crash or restart during retry rescheduling, retry dispatch, or retry append failure must recover each affected issue as a durable scheduled retry, a durable active workflow run, a durable terminal/parked/released state, or an operator-visible failed recovery state. Operators should be able to trust startup recovery rather than manually discovering that a retry vanished.

## Problem Framing and Constraints

The current retry path spans pure runtime state in `src/scherzo/orchestrator/core.gleam`, transition mapping in `src/scherzo/orchestrator/transition.gleam`, claim-start batching in `src/scherzo/orchestrator/transitions/claims.gleam`, timer side effects in `src/scherzo/orchestrator/daemon.gleam`, retry projection in `src/scherzo/state/projection.gleam`, and startup recovery in `src/scherzo/state/recovery.gleam` and `src/scherzo/orchestrator/startup_recovery.gleam`. Today `schedule_task_retry` mutates `RuntimeState.retry_attempts` and emits `CancelRetry` before `ScheduleRetry`; `dispatch_retry_claim` emits retry cancellation before the durable claim-start append; and `schedule_retry_effects_for_ref` and `cancel_retry_effects` use `ContinueRegardless`, so timer side effects can continue after a ledger append failure.

The plan must preserve existing successful retry scheduling, timer firing, refresh, deferral, terminal and policy-invalid cancellation, startup retry restoration, workflow claim-start behavior, ledger file format compatibility, and Linear/operator behavior. It may change retry projection semantics and internal transition/effect boundaries, but it should avoid a broad ledger migration or a new process boundary.

## Strategy Overview

Use a single strategy named schedule-first, append-gated retry boundaries. Retry rescheduling should stop writing a standalone replacement cancellation; a newer `RetryScheduled` generation supersedes the older scheduled generation from the projection's point of view. The projection should also become generation-aware so an old crash pattern with `RetryScheduled(generation: 1)` followed by `RetryCancelled(generation: 2)` keeps the generation-1 retry recoverable until a generation-2 schedule is durable.

Retry schedule and cancel side effects should be append-gated: timer creation, timer cancellation, and final runtime rollback/confirmation happen only after the relevant durable append reports success. Retry dispatch should no longer append `RetryCancelled` before claim start. Instead, `src/scherzo/orchestrator/transitions/claims.gleam` should carry a retry-cancellation attachment on the pending retry claim and append that `RetryCancelled` at the end of the same claim-start ledger batch, after the workflow-start record. A crash before that batch leaves the scheduled retry recoverable; any durable prefix that includes the workflow-start record makes the active run recoverable; a fully written batch has both active run and retry cancellation.

Startup recovery should treat active workflow runs as stronger evidence than stale scheduled retries for the same issue. If an active workflow run exists, recovery must not restore a retry timer for that issue, even if an older retry schedule is still visible.

## Alternatives Considered

A cancel-plus-schedule batch was rejected for replacement because a partially written batch can still expose ordering questions, and schedule-first superseding is smaller and more tolerant of historical partial cancellations. A new `RetryReplaced` ledger event was rejected because it would require a schema and projection migration when existing `RetryScheduled` generations are enough. Keeping `ContinueRegardless` with more logging was rejected for recovery-critical paths because it still permits timer side effects after failed durable writes. Moving all retry logic into a new process was rejected as too large for this bug; the necessary safety boundary is the ledger append and projection semantics, not a new actor.

## Risks and Countermeasures

The largest risk is preserving stale retries after a successful dispatch. The countermeasure is to put `RetryCancelled` in the same claim-start batch and make startup recovery skip and durably cancel retries when an active workflow run exists. Another risk is leaving live runtime state without a timer after append failure; append completion handling must restore the previous retry generation and defer its timer when a previous durable retry exists, or fail closed with an explicit `ledger_append_failed` log and no in-memory-only timer when no previous retry can be restored. Projection changes can regress normal cancellation; focused tests must prove terminal, policy-invalid, released, and operator cancellation still cancel the intended generation. Batching retry cancellation into claim start could affect claim-start validation, so claim batch validation must still require a workflow-start record and must order workflow-start before retry cancellation. A final review risk is accidentally broadening the work into helper or provider migrations; the countermeasure is to add only narrow helpers needed for ledger batch composition and retry completion policy, and to leave docs helpers, provider-live behavior, provider-cache behavior, and browser/UI flows unchanged except for existing tests proving no regression.

## Scope Boundaries

In scope: retry generation projection semantics, retry schedule and cancel append policies, retry timer side-effect ordering, retry dispatch claim-start coupling, pending-claim metadata needed for retry cancellation, startup recovery precedence between active runs and retries, and focused fault-injection tests. Out of scope: changing backoff math, changing public workflow YAML, changing tracker adapter APIs except for existing claim-start batch composition, adding a new ledger storage backend, adding a browser UI, changing provider-live behavior, changing provider-cache behavior, migrating docs or workflow helper scripts, or redesigning scheduled-job retry semantics. Native scheduled-job retry records are a comparison point but not part of this issue unless a shared helper can be reused without broadening behavior.

The implementation may add narrowly scoped production helpers such as a ledger-batch append function or a retry append-completion policy, but it must not move documentation helpers, workflow scripts, provider-live code, or provider-cache code. Manual daemon dogfood is not a pre-publish requirement for this internal durability fix; it is a deferred human/operator check after implementation handoff unless the implementer intentionally changes an operator-visible surface.

This should become one implementation ticket, not multiple child tickets, because projection semantics, dispatch batching, and append policy must land together to avoid a half-safe intermediate state. A follow-up child ticket is only warranted if reviewers want richer operator UI for retry append failures beyond the required logs and recovery behavior.

## Milestones

Milestone 1 establishes failing characterization tests for the two crash windows and the append-failure window. Add focused tests in `test/state_projection_test.gleam`, `test/orchestrator_transition_dispatch_test.gleam`, and the narrow transition or effect-interpreter test file that owns ledger append completion behavior. The red evidence should show that `RetryScheduled(generation: 1)` followed by a future `RetryCancelled(generation: 2)` currently drops the schedule, retry dispatch currently emits standalone cancellation before claim start, and retry scheduling currently creates a timer even when the schedule append reports failure.

Milestone 2 changes projection and reschedule semantics. At the end, `src/scherzo/orchestrator/core.gleam` no longer emits a replacement `CancelRetry` before `ScheduleRetry`, `src/scherzo/state/projection.gleam` ignores stale or future-generation cancellations while a different scheduled generation remains current, and successful replacement still leaves only the latest scheduled generation recoverable. Evidence is the new projection test plus existing retry projection tests passing.

Milestone 3 makes retry schedule and cancel side effects append-gated. At the end, `src/scherzo/orchestrator/effects/types.gleam`, `src/scherzo/orchestrator/effects/interpreter.gleam`, and `src/scherzo/orchestrator/transition.gleam` emit retry timer side effects only from ledger-append completion continuations. `RetryScheduled` append failure must not create a live timer; failed replacement or cancellation must restore or defer the previous retry when one exists; and failure with no previous retry must produce an explicit append-failure log and no in-memory-only timer. Evidence is a transition/effect test for success and failure completion paths.

Milestone 4 couples retry dispatch cancellation to durable claim start. At the end, `src/scherzo/orchestrator/transition_types.gleam`, `src/scherzo/orchestrator/transitions/claims.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/state/ledger_batch.gleam` carry retry-cancellation metadata on pending retry claims and append `RetryCancelled` after the workflow-start record in the same claim-start batch. A failed claim-start append must restore and defer the previous retry; a successful append must start the worker and cancel the matching retry timer. Evidence is a transition/claim test that inspects batch order and failure behavior.

Milestone 5 updates startup recovery and completes regression coverage. At the end, `src/scherzo/state/recovery.gleam` or `src/scherzo/orchestrator/startup_recovery.gleam` treats active workflow runs as stronger evidence than stale scheduled retries, appends a recovery cancellation such as `recovery_active_workflow_run` for the stale retry, and still preserves terminal/parked/released recovery behavior. Evidence is startup-recovery coverage plus full pre-publish validation: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Progress

- [x] (2026-06-07) Read the prepared review-doc target and confirmed the default output directory is `docs/plans`.
- [x] (2026-06-07) Inspected retry scheduling, transition mapping, claim batching, projection, startup recovery, timer, and existing retry test files in the current tree.
- [x] (2026-06-07) Wrote this focused review document for LIV-925 without implementing code changes.
- [x] (2026-06-07) Validated this review document with the Scherzo ExecPlan review-doc validator; it reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-06-07) Incorporated review feedback by tightening milestone evidence, test obligations, manual/dogfood deferral, docs/helper migration boundaries, provider-live/provider-cache scope, full validation, and linting requirements.

## Decision Log

- Decision: Use schedule-first, append-gated retry boundaries. Rationale: it closes both known crash windows without introducing a new ledger event or process boundary. Date: 2026-06-07.
- Decision: Put retry-dispatch cancellation in the claim-start batch and order it after workflow-start records. Rationale: any durable prefix then recovers either the retry or the active run, never only a cancellation. Date: 2026-06-07.
- Decision: Treat this as one implementation ticket. Rationale: splitting projection, dispatch, and append policy would create unsafe intermediate states. Date: 2026-06-07.
- Decision: Defer manual daemon dogfood and browser checks unless operator-visible surfaces are intentionally changed. Rationale: this is an internal durability fix whose pre-publish evidence should be automated fault-injection, startup-recovery, full-test, and lint output; human/operator daemon checks can happen after implementation handoff. Date: 2026-06-07.
- Decision: Do not migrate docs helpers, workflow helper scripts, provider-live code, or provider-cache code for LIV-925. Rationale: review feedback called out migration and provider-surface scope risk, and the retry invariant can be fixed with local retry, projection, claim, and recovery changes only. Date: 2026-06-07.

## Validation and Acceptance

This review document is valid when `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-925-crash-safe-retry-scheduling-dispatch-boundaries.md` reports `REVIEW_DOC_VALID=ok`, and when the structured implementation pack has been submitted for LIV-925.

The follow-up implementation is acceptable only with automated evidence for all required outcomes: a projection test for crash after replacement cancel before replacement schedule; a transition or claim-start test proving retry dispatch no longer emits a standalone durable cancellation before claim start; a claim-start batch test proving `WorkflowRunStarted` or `WorkflowRunStartedWithTask` appears before `RetryCancelled`; an append-completion failure test proving `RetryScheduled` append failure creates no in-memory-only timer and restores or defers a previous retry when available; a startup-recovery test proving active workflow runs suppress and durably cancel stale retry restoration; and regression tests for successful retry scheduling, timer firing, refresh, deferral, terminal/policy-invalid/operator cancellation, released or missing-issue cancellation, and startup retry restoration. The implementation handoff must capture the names of the new tests and the validation command results so acceptance evidence is reviewable rather than implied.

Full pre-publish validation must include `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. Existing provider-live and provider-cache tests, if touched indirectly by the full suite, must continue to pass, but no live provider/cache run is a separate pre-publish gate for this issue. No browser, manual daemon dogfood, or `scherzoctl` evidence is required before publish unless the implementation intentionally changes operator-visible surfaces. If richer manual operator evidence is desired, it is deferred to a human/operator after implementation handoff: restart a daemon against a retained workspace with injected retry-boundary records and confirm `scherzoctl` or logs show recovered retry, active run, or explicit failed recovery.

## Rollout, Recovery, and Idempotence

Roll out as an internal durability fix with no data migration and no provider-live, provider-cache, browser, docs-helper, or workflow-helper migration. The generation-aware projection must be backward compatible with existing retry records, including historical future-generation cancellations from the old reschedule path. If the implementation must be reverted, revert the projection, transition, claim-batching, recovery, and tests together rather than partially; partial revert can reopen the dispatch or replacement crash window. Test and validation commands are idempotent. Startup recovery must remain idempotent: repeated recovery over the same ledger should not create duplicate timers or duplicate terminal/cancel records beyond existing recovery append rules, and any `recovery_active_workflow_run` cancellation should be safe to see more than once.

## Open Questions and Clarifications Needed

No open question blocks implementation. The only follow-up decision after the main fix is whether the required operator-visible append-failure evidence is sufficiently covered by structured logs and existing state/status surfaces, or whether a separate operator UI enhancement ticket should be opened.
