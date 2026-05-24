# Make retry-step recover terminal failed runs with stale active steps

## Purpose / Big Picture

This plan preserves a safe operator recovery boundary when a retained Scherzo workflow run fails while being resumed. After the change, a terminal failed workflow cannot be left with only stale `pending` or `running` step attempts and no repairable step. Operators should be able to run `scherzoctl retry-step run:<run-id> --step <step-id>` against the LIV-459-shaped failure and get a normal repair plan for the selected stale agent step instead of `step_not_repairable` or `no_failed_workflow_run`.

## Problem Framing and Constraints

The observed failure happened after a recovered run advanced successfully, then a later resume failed during jj workspace lifecycle preparation. The workflow was marked terminal failed, but review-lane step attempts still projected as active, so `retry-step` had no failed or interrupted boundary to select. The fix must be additive to the ledger model, preserve workflow and issue drift checks, and remain fail-closed when stale active attempts might represent unsafe command side effects or ambiguous operator intent.

## Strategy Overview

Make terminal failure paths preserve the invariant startup recovery already aims to preserve: unfinished active attempts must be interrupted before the workflow is considered terminal. Then harden `retry-step` so it can repair already-retained terminal failed runs that predate or escaped that invariant by treating one explicitly selected stale active agent attempt as a synthetic interrupted boundary and appending the missing `StepAttemptInterrupted` record before superseding it. Finally, make the jj workspace driver self-heal the specific stale working-copy condition by running `jj workspace update-stale` once and retrying `jj status` only when the original status output explicitly asks for that command.

## Alternatives Considered

Doing nothing leaves retained failed runs unrecoverable without hand-editing ledger state. Treating any stale active attempt as retryable would be too broad because command steps and ambiguous parallel lanes can hide side effects. Rewriting old ledger history would be riskier than appending normalization records. Adding a new record type is unnecessary because `StepAttemptInterrupted` already expresses the needed state transition and is understood by projection and recovery code.

## Risks and Countermeasures

The main risk is retrying a step whose prior execution may have produced side effects. The countermeasure is to normalize only terminal failed runs, keep command steps fail-closed, require the current workflow and issue identity to match retained provenance, and append an auditable interruption reason such as `terminal_failure_repair_normalized`. Another risk is selecting the wrong parallel lane; `retry-step` without `--step` must reject multiple stale candidates as `ambiguous_repair_step`. A third risk is masking jj errors; the driver should retry only when the failed status output explicitly points to `jj workspace update-stale`, then fail with the original class if the update or retried status fails.

## Scope Boundaries

In scope are `src/scherzo/workflow_run.gleam` terminal failure paths, `src/scherzo/workflow_repair.gleam` repair-boundary selection and normalization records, `src/scherzo/orchestrator/daemon.gleam` retry-step finalization only if the appended record ordering needs adjustment, regression tests under `test/`, and `scripts/scherzo-workspace-jj` stale-workspace verification. Out of scope are Linear state policy changes, workflow YAML changes, changing `failed_continued` semantics, editing retained historical ledgers in place, docs/helper migrations, provider-live or cache behavior, browser UI work, or inventing bundle identifiers in this review document.

## Milestones

Milestone 1 captures the stale-active terminal failure shape in focused red tests. The implementation should add or extend regression cases in `test/workflow_run_test.gleam`, `test/workflow_repair_test.gleam`, and `test/jj_workspace_driver_test.gleam`; before code changes, at least the new stale-active repair and stale jj status cases should fail with the current `step_not_repairable` or raw status failure behavior.

Milestone 2 changes workflow terminal failure handling in `src/scherzo/workflow_run.gleam` so every unfinished pending or running attempt known to a recovered or active run receives `StepAttemptInterrupted` before `WorkflowRunFinished`. The observable evidence is a workflow-run ledger assertion that interruption records precede the terminal failure record and cover every unfinished active attempt.

Milestone 3 changes `src/scherzo/workflow_repair.gleam` so `retry-step` can select exactly one stale active agent attempt from a terminal failed run, append a `StepAttemptInterrupted` normalization record with a distinct reason, and then append the existing repair request and supersede records. The milestone also preserves negative cases: multiple stale candidates without `--step` remain ambiguous, selected stale command steps remain unsafe, workflow and issue drift still reject, and workspace/artifact recovery failures still park or fail as they do now.

Milestone 4 changes `scripts/scherzo-workspace-jj` so `status --human` runs `jj status --color=never`, detects the explicit stale-working-copy instruction, runs `jj workspace update-stale` once, and retries the same status command once. The evidence is the fake-driver log `status --color=never`, `workspace update-stale`, `status --color=never`; non-stale failures and failed updates must still fail instead of looping.

Milestone 5 runs targeted and full validation gates and records the outputs for review. No manual browser check is relevant. A live Scherzo dogfood retry against an actual retained failed run is a deferred human/operator check after implementation, not a pre-publish gate, because the pre-publish acceptance is covered by deterministic ledger and driver tests.

## Progress

2026-05-24: Reviewed the current repair, workflow-run, recovery, transition, and jj workspace-driver surfaces; drafted this review document and prepared the mechanical implementation pack. No implementation code has been changed by this review document.

2026-05-24: Incorporated review feedback by making milestone outcomes, acceptance evidence, negative test obligations, full validation and linting gates, and manual/dogfood timing explicit. No required implementation or validation work is marked as completed here.

## Decision Log

2026-05-24: Stale active normalization is limited to terminal failed workflow runs, not successful, cancelled, or merely superseded runs, because the operator intent is clearest when the workflow has already failed.

2026-05-24: The normalization reason should be a distinct value such as `terminal_failure_repair_normalized` so operators can distinguish repair-created boundaries from `daemon_restart`.

2026-05-24: Multiple stale active candidates remain ambiguous unless the operator supplies `--step`, and stale command steps remain fail-closed because their side effects cannot be proven safe from the ledger alone.

2026-05-24: Automated tests are the pre-publish acceptance evidence; live dogfood on a retained daemon run is deferred to an operator after implementation lands, and no browser, docs/helper migration, provider-live, or cache validation is part of this change.

## Validation and Acceptance

Evidence for terminal failure interruption must come from a workflow-run regression test that records `StepAttemptInterrupted` for every unfinished pending or running attempt before `WorkflowRunFinished`, run with `direnv exec . gleam test test/workflow_run_test.gleam`. Evidence for repair normalization must come from `direnv exec . gleam test test/workflow_repair_test.gleam`, including one stale review-lane agent attempt that plans successfully, multiple stale attempts that return `ambiguous_repair_step` without `--step`, a selected stale command attempt that remains unsafe, and drift or workspace/artifact recovery cases that still fail closed. Evidence for the jj fix must come from `direnv exec . gleam test test/jj_workspace_driver_test.gleam`, with the fake-driver log showing `status --color=never`, `workspace update-stale`, then a retried `status --color=never`, plus failure cases proving non-stale status failures and failed update-stale attempts do not loop.

Final pre-publish acceptance requires `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` to pass. No browser validation is required. The deferred dogfood check, if an operator has a retained LIV-459-shaped run available after implementation, is to run `scherzoctl retry-step run:<retained-run-id> --step <stale-agent-step-id>` and expect the control response to say it is retrying that run and step at the next attempt while the ledger contains the normalization interruption followed by repair records.

## Rollout, Recovery, and Idempotence

The rollout is additive: new ledger facts are appended, existing projections continue to understand them, and no retained ledger is rewritten. If the implementation misbehaves, reverting the code restores the old behavior without data migration. Retrying `retry-step` after a successful repair request should remain idempotent in practice because the selected stale attempt is no longer active once normalized and superseded. The jj repair is bounded to one update-and-retry cycle so repeated lifecycle checks do not loop indefinitely.

## Open Questions and Clarifications Needed

No blocking clarification is needed. This plan answers the known questions by allowing stale active normalization only for terminal failed retained runs, keeping command steps fail-closed, and using a dedicated interruption reason such as `terminal_failure_repair_normalized`.
