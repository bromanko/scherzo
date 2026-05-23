# Make interrupted retained workflow runs retry-step viable

This ExecPlan v2 review document frames the design for LIV-515. Mechanical implementation details, file-by-file edits, commands, and test instructions are intentionally kept out of this human-reviewable document and supplied through the structured implementation pack.

## Purpose / Big Picture

After this change, an operator can repair a retained workflow run that stopped because the daemon shut down during a workflow step. For the motivating case, the visible outcome is that `scherzoctl retry-step run:LIV-509-1779481136143-33 --step apply_feedback` can reopen the retained run, preserve completed implementation and review artifacts, retry `apply_feedback` at the next attempt index, and then let downstream steps proceed only after that retry succeeds.

This gives operators a safe middle path between full redispatch and manual salvage. The command remains explicit, audited, and fail-closed: Scherzo reuses prior work only when the retained run root, recorded artifacts, source workspaces, workflow identity, and task identity still match the current world.

## Problem Framing and Constraints

The existing `retry-step` path is shaped around terminal failed runs and failed step outcomes. An interrupted retained run is different: it may have a durable `workflow_run_interrupted` record and a `step_attempt_interrupted` record, but no terminal failed step for the current planner to select. That leaves runs like LIV-509 parked even when the intended recovery boundary is obvious to a human.

The key constraint is safety. An interrupted attempt might have partial side effects, stale source workspaces, changed workflow YAML or prompts, or an updated issue. Repair must reject real drift with actionable diagnostics instead of silently continuing. Parked issues also need explicit handling so the operator understands whether to unpark first, retry the interrupted step, or abandon partial repair and run a full retry.

## Strategy Overview

Treat an interrupted step attempt as a first-class repair boundary for explicit `retry-step`, alongside existing fatal failed attempts. The planner should select a run whose durable status is either terminal failed or interrupted, identify a selected boundary whose step status is failed or interrupted, supersede that boundary and its downstream descendants, and then build the same recovered workflow candidate used by existing partial repair.

The resumed run should keep the original run id and append explicit repair, supersession, and run-reopen records before worker spawn. Completed upstream attempts, including `failed_continued` gate artifacts that satisfy dependencies, remain preserved. The selected interrupted attempt starts fresh at the next attempt index rather than trying to continue a dead pi process, unless existing continuation validation is deliberately invoked by another recovery path.

Selection stays conservative. If multiple interrupted or failed boundaries could explain the retained run, Scherzo requires `--step`. If workflow, issue, task, artifact, or workspace validation cannot prove the retained context is still safe, the command rejects with a stable reason such as `workflow_drift`, `issue_drift`, `workspace_recovery_failed`, `artifact_recovery_failed`, `ambiguous_repair_step`, or `issue_parked`.

## Alternatives Considered

Manual salvage is the lowest-code alternative, but it is slow, unaudited, and easy to get wrong because the operator must preserve upstream artifacts and restart the correct downstream step by hand.

A full `scherzoctl retry <task>` is safe but wasteful. It discards useful completed work and does not solve the operator need to continue from the interrupted boundary.

Automatic startup repair was rejected for this slice. Daemon restart can record and park interrupted work, but rerunning a step that may have side effects should require explicit operator intent through `retry-step`.

Resuming the old pi session was also rejected for this repair path. The motivating run was interrupted by daemon shutdown; retry-step should create a new attempt and rely on durable artifacts rather than pretending the old process survived.

## Risks and Countermeasures

The largest risk is reusing stale context after workflow or issue drift. The countermeasure is to preserve and compare workflow fingerprints, issue fingerprints, task references, run roots, artifact hashes, and source workspace paths before accepting repair; missing provenance rejects rather than falls back to blind trust.

A second risk is ambiguity in parallel workflows. The countermeasure is to require `--step` when more than one failed or interrupted boundary remains plausible, even if one boundary is newer.

A third risk is bypassing park semantics. The command should not quietly spawn a worker for an explicitly parked issue. It should return an operator-visible parked diagnostic that names the park reason and the required sequencing.

A fourth risk is duplicate repair attempts. The countermeasure is to treat previously superseded attempts as already consumed and to allocate a new attempt only after the latest selected boundary is still eligible.

A fifth risk is over-scoping the fix into unrelated workflow infrastructure. This work must not change provider live/cache behavior, structured-output helper materialization, or workflow bundle publishing. The countermeasure is to keep the code change in retained-run repair, daemon command handling, tests, CLI help, and operator runbook text, with full lint and validation evidence captured before handoff.

## Scope Boundaries

In scope is the explicit operator repair path for issue-dispatched retained workflow runs with `workflow_run_interrupted` and one or more `step_attempt_interrupted` records. The work includes planner selection, durable repair records, recovery validation, daemon command handling, CLI/help wording, runbook guidance, and deterministic fixture coverage for LIV-509-like histories.

Out of scope is changing full retry semantics, adding automatic startup retry for interrupted terminal runs, repairing scheduled workflows, adding drift override flags, inventing linked repair run ids, changing provider live/cache semantics, migrating structured-output helpers, or guaranteeing continuation of a killed pi session. Command-step interruption remains fail-closed unless an explicit existing safety rule says it can be retried.

## Milestones

First, stabilize the repair-boundary model so the planner can describe failed and interrupted boundaries with one vocabulary and preserve run/task provenance after terminal or interrupted status overwrites. The observable outcome is a pure planner test that can select an interrupted `apply_feedback` attempt from an interrupted run while still selecting existing terminal failed runs unchanged.

Second, make the pure retry-step planner accept an explicitly selected interrupted boundary, supersede only that boundary and downstream descendants, and reject ambiguous or non-eligible selections with stable reasons. The observable outcome is fixture evidence for next attempt indexing, preserved completed artifacts, downstream blocking until the retry succeeds, ambiguity rejection, drift rejection, and idempotent duplicate-command rejection.

Third, connect the daemon operator path to the expanded planner while respecting active-run and parked-issue safety checks. The observable outcome is command-result evidence that an active issue is rejected as `issue_already_active`, a parked issue is rejected as `issue_parked` with unpark/retry sequencing guidance, and an accepted repair appends records before spawning the recovered worker.

Fourth, add regression fixtures that resemble LIV-509 and prove preserved upstream artifacts, next attempt indexing, downstream ordering, ambiguity rejection, drift rejection, artifact/workspace recovery rejection, and idempotent supersession behavior. The observable outcome is a deterministic test transcript from `direnv exec . gleam test` that names the new retry-step repair cases.

Fifth, update operator help and runbook text so interrupted retained runs are clearly distinguished from full retry, manual salvage, unrecoverable drift, and deferred live dogfood. The observable outcome is a help/docs assertion plus a runbook diff; no structured-output helper migration or provider live/cache behavior change is part of this milestone.

## Progress

- [x] (2026-05-22 00:00Z) Drafted this human-reviewable ExecPlan review document for LIV-515.
- [x] (2026-05-22 23:55Z) Incorporated review feedback by tightening milestone evidence, validation obligations, out-of-scope helper/provider boundaries, lint expectations, and deferred dogfood wording.

## Decision Log

- Decision: Make interrupted retry-step repair explicit rather than automatic.
  Rationale: A killed step may have partial side effects, so rerun requires operator intent.
  Date: 2026-05-22

- Decision: Reuse the existing same-run repair and recovered workflow execution path.
  Rationale: It already preserves upstream artifacts and workspaces, appends audit records, and starts downstream work from scheduler state derived from recovered artifacts.
  Date: 2026-05-22

- Decision: Require `--step` for ambiguous interrupted boundaries.
  Rationale: Guessing in parallel or partially completed DAGs can rerun the wrong side-effecting work.
  Date: 2026-05-22

- Decision: Do not auto-unpark during retry-step.
  Rationale: Parked state is operator policy; the command should explain sequencing rather than silently overriding it.
  Date: 2026-05-22

- Decision: Treat the LIV-509 live dogfood retry as deferred operator evidence, not a pre-publish gate.
  Rationale: The deterministic fixture is the required safety proof; running the historical retained issue may depend on local daemon state, parked-issue sequencing, and operator timing outside the implementation workspace.
  Date: 2026-05-22

- Decision: Keep structured-output helper migration and provider live/cache behavior out of LIV-515.
  Rationale: The repair path should be additive and local to retained workflow run recovery; broad workflow-helper or provider-cache changes would increase risk without helping operators retry interrupted retained runs.
  Date: 2026-05-22

## Validation and Acceptance

Pre-publish evidence must include `direnv exec . gleam test` passing with named new tests for a LIV-509-like fixture: implementation and review steps are preserved, `validate_before_native_review` with `failed_continued` remains dependency-satisfying, `apply_feedback` attempt 1 is interrupted, retry-step selects `apply_feedback`, supersedes it, and starts attempt 2. The same evidence must show downstream steps are not ready until the retried `apply_feedback` succeeds, and the test output or final notes must identify the new planner/daemon/help tests that provide this evidence.

Negative acceptance must be proven by tests that omit `--step` with multiple failed or interrupted boundaries and expect an ambiguity reason; change the workflow fingerprint and expect `workflow_drift`; change the issue fingerprint or task identity and expect `issue_drift`; remove or corrupt an upstream artifact or source workspace and expect recovery rejection; repeat the same retry-step command after supersession and expect no duplicate attempt allocation; and leave the issue parked to expect an `issue_parked` diagnostic with unpark/retry sequencing guidance.

Documentation acceptance must be shown by a help or docs assertion that `retry-step` mentions failed or interrupted workflow steps and by a runbook diff explaining when interrupted retained runs are eligible for retry-step versus manual salvage or full retry. Full validation before handoff is `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. A live dogfood retry of LIV-509 is useful deferred post-implementation operator evidence; it is not required before publishing the code if the deterministic fixture passes, and the handoff should preserve that deferred status instead of converting it into a blocking gate.

## Rollout, Recovery, and Idempotence

The rollout is additive. Existing terminal failed-step repair should continue to behave the same, full issue retry remains a fresh dispatch path, structured-output helpers keep their current materialization flow, and provider live/cache behavior is unchanged. If the new interrupted-boundary validation fails in production, the safe fallback is no code-side mutation: the command rejects before appending repair records, and the operator can choose manual salvage or full retry.

Once repair is accepted, records are appended before worker spawn so daemon crash recovery sees an active reopened run with superseded old attempts. Repeating the same command should not create another attempt for an already superseded interrupted attempt; only a later newly interrupted or failed repair attempt can become a new boundary. If the deferred live dogfood check later fails on LIV-509, the recovery action is to keep or re-park the issue, inspect the stable rejection reason, and either unpark/retry after correcting drift or use full retry/manual salvage.

## Open Questions and Clarifications Needed

No open MVP questions remain. A future ticket can decide whether a specialized command should combine unpark and retry-step for parked interrupted runs, but LIV-515 should keep those operator actions separate and explicit.
