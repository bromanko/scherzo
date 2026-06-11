# LIV-1019 batch-fatal recovery without sibling re-execution

This is a focused ExecPlan review for LIV-1019. It is documentation-only; mechanical implementation detail is supplied through the structured implementation pack for the follow-up implementation task.

## Purpose / Big Picture

Scherzo should recover a failed step inside a concurrent workflow batch without re-running sibling steps that already completed or were still safely running. After the follow-up implementation, a recoverable fatal step can be repaired and rechecked while successful siblings keep their recorded artifacts, interrupted command siblings are not silently retried, and workspace hooks do not re-enter a directory while the previous attempt's process group is still winding down.

## Problem Framing and Constraints

The current in-run batch recovery path loses facts. `src/scherzo/workflow_run/step_worker_pool.gleam` returns only the fatal result when one step fails, so completed sibling results in the accumulator are discarded. `src/scherzo/workflow_run.gleam` then resets every prepared step in the batch on recovery recheck, which can re-execute successful side-effecting commands and also auto-rerun siblings that were killed mid-flight. That conflicts with `docs/ARCHITECTURE.md`, which says interrupted command steps are unsafe to retry automatically. The fix must preserve concurrency for independent workspaces, the existing `on_failure: continue` semantics, recovery blocks, artifact checkpoints, and normal kill-fast behavior for non-recoverable fatal failures.

## Strategy Overview

Use the chosen drain-don't-kill strategy. When the first fatal result is eligible for step recovery, the worker pool should keep collecting all already-started sibling results instead of killing them. The workflow runner should checkpoint drained siblings in DAG order, run recovery for only the fatal step, clear the failed attempt from in-memory template locals before the recheck, and mark only that step pending. If the fatal result is not recoverable, kill-fast remains: retain and checkpoint any sibling results already collected, interrupt only still-active siblings, write the workflow-finished checkpoint, and only then clean up.

## Alternatives Considered

Leaving the kill-fast path in place and merely checkpointing interrupted siblings was rejected because it still kills command steps and can immediately re-enter their workspaces on the retry. Resetting all batch steps except those with completed checkpoints was rejected because it still relies on reconstructing in-memory facts after losing the accumulator and does not solve the asynchronous process-group overlap for killed siblings. Serializing all workflow execution was rejected as too broad; Scherzo's concurrency remains useful when independent workspaces can safely proceed. Adding restart-recovery planner policy to this in-memory path was rejected because the defect occurs before restart recovery can observe it.

## Risks and Countermeasures

The main tradeoff is latency: recovery waits for the slowest already-started sibling. This is acceptable because it prevents duplicate publication, duplicate push, and unsafe command reruns. A second risk is mishandling `on_failure: continue`; the countermeasure is to keep those failures non-fatal, checkpoint them as `failed_continued`, and never invoke step recovery for them. A third risk is exposing a failed retrying artifact to template rendering; the countermeasure is to write the failed artifact durably for audit and recovery input, then remove it from the live artifact map before rechecking the step. A fourth risk is terminal cleanup ordering; failure branches must write `workflow_finished` before workspace cleanup so a crash leaves either a recoverable run root or a terminal ledger record. A fifth risk is accidental scope creep into docs/helper migration, provider-live/cache behavior, browser UI, or dogfood workflow rewrites; the countermeasure is to keep those surfaces out of the pre-publish implementation unless a new review-doc revision explicitly expands scope.

## Scope Boundaries

In scope are `src/scherzo/workflow_run/step_worker_pool.gleam`, `src/scherzo/workflow_run.gleam`, the recovery prompt path in `src/scherzo/workflow_run/step_execution.gleam` only as needed for stale artifact prevention, terminal cleanup ordering, recovered-run cleanup eligibility, and focused tests in the workflow-run test suite or a new worker-pool test file. Out of scope are workflow YAML schema changes, new recovery policy syntax, restart-recovery planner redesign, workspace path layout migration, provider-live/provider-cache behavior, browser UI work, docs or runbook updates, workflow-helper or bundle-local/root-helper migration, and changes to the dogfood workflow definitions beyond relying on their existing `concurrency: 4` and recovery configuration as real-world coverage. If implementation discovers that any out-of-scope helper, docs, provider, cache, browser, or dogfood surface must change to make the fix safe, stop and revise this review document and the implementation pack before coding that surface.

## Milestones

Milestone 1 establishes executable red tests for the current hazards in `test/workflow_run_test.gleam` and, if a lower-level unit boundary is clearer, `test/workflow_step_worker_pool_test.gleam`. The tests should cover duplicate execution of a completed sibling after recoverable fatal recovery, automatic retry of an in-flight command sibling, workspace re-entry before an active sibling releases its marker, non-recoverable fatal handling that retains completed sibling checkpoints while interrupting only active siblings, `on_failure: continue` remaining non-recoverable, stale failed artifacts being hidden from recheck template locals, resumed-success cleanup eligibility, and `WorkflowFailed` ledger ordering.

Milestone 2 changes the batch worker outcome model in `src/scherzo/workflow_run/step_worker_pool.gleam` so the fatal path retains collected results and distinguishes drained recoverable siblings from killed active siblings. Recoverable fatals drain remaining workers to completion; non-recoverable fatals still kill active workers but return completed results and the step ids actually interrupted for checkpointing.

Milestone 3 rewrites workflow-run result application in `src/scherzo/workflow_run.gleam` around drained batches. At the end, drained siblings are finalized in DAG order, `on_failure: continue` remains a completed status, recovery runs only for the recoverable fatal step, and only that step is marked pending for recheck.

Milestone 4 fixes adjacent ordering defects in the same path. The failed artifact for a retrying step is not visible to concurrently scheduled template rendering after recovery asks for recheck, successful resumed runs are eligible for normal cleanup while preserving retention-marker semantics, and the `WorkflowFailed` branch writes `workflow_finished` before workspace cleanup.

Milestone 5 completes validation without expanding scope. The implementation is accepted only after the focused red-to-green tests, full Gleam test suite, formatting check, `glinter`, and `scherzo_lint` all pass from the repository root, and after the implementer confirms no docs/helper migration, provider-live/cache change, browser check, or dogfood workflow rewrite was introduced as a pre-publish requirement.

## Progress

- [x] (2026-06-11) Confirmed the prepared review-doc target is the default directory `docs/plans`.
- [x] (2026-06-11) Inspected the referenced batch collection, recovery retry, template rendering, cleanup, workspace path, command-port cleanup, architecture, and dogfood workflow configuration files.
- [x] (2026-06-11) Wrote this focused review document for LIV-1019 without implementing production code changes.
- [x] (2026-06-11) Validated this review document with the Scherzo ExecPlan review-doc validator; it reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-06-11) Incorporated review feedback by tightening milestone specificity, acceptance evidence, full validation and lint obligations, and explicit out-of-scope treatment for manual dogfood, docs/helper migration, provider-live/cache behavior, and browser checks.

## Decision Log

- Decision: Use drain-don't-kill for recoverable fatal batch failures. Rationale: it eliminates duplicate sibling execution, unsafe interrupted-command reruns, and workspace overlap with the smallest behavior change. Date: 2026-06-11.
- Decision: Preserve kill-fast for non-recoverable fatals but checkpoint completed siblings and interrupted active siblings separately. Rationale: terminal failures should still stop quickly without losing work that already completed. Date: 2026-06-11.
- Decision: Mark only the recovered fatal step pending after a recovery recheck request. Rationale: sibling status should reflect what actually happened in the run, not be reset as collateral damage. Date: 2026-06-11.
- Decision: Keep this as one implementation task. Rationale: the accumulator, scheduler, checkpoint, stale-artifact, and cleanup-order fixes must land together to avoid a half-safe recovery boundary. Date: 2026-06-11.
- Decision: Do not require pre-publish manual/browser/dogfood, docs/helper migration, or provider-live/cache validation for this fix. Rationale: those surfaces are out of scope unless the implementation deliberately changes them; any optional dogfood run remains a deferred human/operator check after implementation handoff. Date: 2026-06-11.

## Validation and Acceptance

This review document is valid when `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1019-batch-fatal-recovery.md` reports `REVIEW_DOC_VALID=ok`, and when the structured implementation pack for LIV-1019 has been submitted.

The follow-up implementation is acceptable only with concrete evidence for each required behavior: a test showing a completed sibling command runs exactly once across a recoverable fatal recheck; a test showing an in-flight command sibling is allowed to finish and is not marked interrupted or retried; a test showing retry preparation does not re-enter a sibling workspace while that sibling's active marker remains; a test showing non-recoverable fatal failure still interrupts active siblings but retains any completed sibling `step_attempt_finished` checkpoint; a test showing `on_failure: continue` failures in a drained batch are recorded as continued and do not invoke recovery; a test showing stale failed artifacts are not available to templates after recheck scheduling; a test showing a resumed-then-successful run performs eligible cleanup; and a test showing `workflow_finished` is appended before cleanup on `WorkflowFailed`.

Full pre-publish validation must include `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. No browser, provider-live, provider-cache, docs/helper migration, or manual daemon dogfood evidence is required before publish because those surfaces are out of scope for the intended code path; if the implementation intentionally changes one of those surfaces, the review document and implementation pack must be revised before publish. Any desired dogfood run against `.scherzo/workflows/execplan-implementation.yaml` or `.scherzo/workflows/implementation.yaml` is deferred to a human/operator after implementation handoff, not a pre-publish gate.

## Rollout, Recovery, and Idempotence

Roll out as an internal workflow-runner correctness fix with no data migration, no configuration migration, no helper migration, and no provider-live/cache behavior change. The change is backward-compatible with existing checkpoints because it changes in-memory scheduling, checkpoint ordering, and tests rather than durable record shape. If the implementation must be reverted, revert the worker-pool outcome, workflow-run recovery handling, stale-artifact cleanup, terminal cleanup ordering, and tests together. Test commands are idempotent. Recovery rechecks must also be idempotent at the workflow level: repeating a failed recovery should not duplicate sibling side effects, and interrupted-step checkpoints should name only attempts that were actually killed. Optional post-handoff dogfood evidence can be gathered by a human/operator without changing the rollout gate.

## Open Questions and Clarifications Needed

No open question blocks implementation. The only policy detail to confirm during implementation is how to report the rare case where more than one fatal recoverable step completes in the same drained batch; the safe default is to checkpoint all results and recover one fatal step at a time without retrying siblings.
