# LIV-1371 unified retry planner

This ExecPlan review document is paired with a structured implementation pack. The review document stays concise and human-reviewable; the implementation pack contains the mechanical file-by-file steps, interfaces, tests, and command details.

## Purpose / Big Picture

After this change, a Scherzo operator can ask for `retry` without first diagnosing whether the correct expert verb is step retry, output recollection, publication retry, orphan cleanup, unpark, or start fresh. For a retained failed or interrupted workflow run, Scherzo chooses the deepest safe point it can prove from the current workflow, current issue, retained ledger records, and retained artifact hashes. The visible result states whether the old run resumed, rewound to an earlier boundary, or was superseded by a fresh run, and it names what evidence was preserved and what was discarded.

## Problem Framing and Constraints

Today retry is a fan of fail-closed actions. `run retry-step` is safe only when the workflow, issue, upstream artifacts, and retained workspaces still match the failure moment; otherwise operators see stable rejections such as `workflow_drift`, `issue_drift`, or `artifact_recovery_failed` and must infer the next command. The new behavior must keep Scherzo's safety guarantees while making ordinary retry total: it may degrade to a shallower safe point or a fresh run, but it must not leave a retained failed or interrupted run in a state that requires daemon restart or manual ledger surgery.

The main constraints are that explicit operator holds and terminal tracker issue states remain hard stops, active duplicate work must not be created, and existing expert verbs remain available. Older retained runs may not have enough historical workflow-interface evidence to prove a changed-boundary resume; those runs should degrade to a fresh superseding run rather than guessing. No provider-live/cache behavior, browser UI, or workflow-specific recipe logic is required for this feature.

## Strategy Overview

Implement one durable retry planner that ranks safe points from deepest to shallowest. The planner first tries the exact current behavior: if the current workflow identity and fingerprint match and every upstream artifact needed for the failed or interrupted boundary verifies by ledger SHA and decodes to the expected step artifact, it produces a normal resume from that boundary. If the workflow fingerprint changed but the workflow id and recorded step-interface snapshot prove the current DAG is compatible, it preserves the verified unchanged prefix, rewinds to the first changed or unverified step, and reruns that step and its descendants. If artifacts are missing or corrupt for part of the prefix, it preserves only the verified prefix and rewinds before the first unverified artifact. If compatibility or artifact proof is not available, it supersedes the retained run and starts a fresh run from the current task payload.

New workflow-interface snapshots should be recorded for runs started after this feature so future changed-workflow retries can prove step ids, dependency edges, workspace source contracts, structured-output contracts, and per-step execution fingerprints. Existing runs without such snapshots still benefit from artifact-aware same-fingerprint retry and safe fresh degradation. The daemon should run this planner both for explicit retry-step-style operations and dispatch-time retry recovery so common operator paths share one decision lattice and one reporting format.

## Alternatives Considered

Leaving the existing expert verbs unchanged is safest locally but preserves the operator burden and the misleading impression that retry failed when a safe fresh run is available. Making every retry immediately start fresh is simpler but wastes completed work and loses Scherzo's retained-run value. Allowing operators to override drift manually is rejected for this slice because the feature's promise is provable safety, not best-effort continuation. Workflow-specific recovery recipes are also rejected; the planner must reason from generic DAG, artifact, and ledger contracts rather than dogfood workflow names or failure codes.

## Risks and Countermeasures

The largest risk is preserving an artifact produced under an incompatible workflow. The countermeasure is to preserve changed-workflow artifacts only when a recorded interface snapshot exists and the current step's dependency and output contracts match that snapshot; otherwise the planner degrades fresh. A second risk is corrupt evidence being discovered after repair records are appended. The countermeasure is to verify the artifact prefix before selecting the repair boundary and to keep finalization rejection non-mutating; if a race still invalidates evidence, the operation re-plans and degrades fresh rather than appending a partial repair.

A third risk is duplicate fresh runs or duplicate supersession records when operators retry repeatedly. The countermeasure is an idempotent operation identity and projection checks that treat an already queued, running, or already superseded retry as the current answer instead of appending another supersession. A fourth risk is broad command-surface churn. The countermeasure is to add the common aliases `retry step` and `--from-scratch` while keeping `run retry-step`, `run recollect-outputs`, `run finalize`, `publication retry`, `recovery cleanup-orphan-steps`, and `unpark` as explicit expert overrides.

## Scope Boundaries

In scope are the unified retry planner, artifact-prefix verification, workflow-interface snapshot recording for new runs, daemon execution of resume-or-fresh decisions, CLI/control aliases for common retry and forced-from-scratch retry, durable supersession provenance, focused tests, and a small runbook update explaining the new operator meaning. The expected code surface is `src/scherzo/workflow_repair.gleam`, `src/scherzo/workflow_recovery_planner.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/retry_step_operation.gleam`, `src/scherzo/orchestrator/retry_step_resumption.gleam`, `src/scherzo/orchestrator/transitions/operator.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/artifact_store.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/ctl.gleam`, `src/scherzo/ctl/command_registry.gleam`, and focused tests under `test/`. Existing artifact publication retry, output recollection, finalization, orphan cleanup, explicit unpark, and exact expert retry-step verbs remain available and should not be removed.

Documentation scope is limited to updating `docs/runbooks/workflow-step-recovery.md` or `docs/runbooks/workflow-recovery.md` if the operator retry meaning needs runbook coverage, plus recording a docs/helper inventory at implementation handoff. No `.scherzo/workflows/scripts/*` migration, workflow schema migration, provider-facing structured-output helper migration, or review-lane contract migration is planned. If implementation unexpectedly touches any of those helper surfaces, it must either split or roll back that helper work, or run the relevant helper and offline contract validation before publish and record the evidence.

Out of scope are semantic merge repair, provider-live/cache changes, browser UI changes, workflow-specific failure-code recipes, old-ledger migration, and manual dogfood/browser evidence as a pre-publish blocker. Existing retained runs without interface snapshots are supported by degradation to fresh runs, not by reconstructing historical workflow YAML from outside the ledger. A manual dogfood retry of a real retained run may be useful as a deferred human/operator check after implementation, but automated tests and lint gates are the pre-publish acceptance evidence.

## Milestones

Milestone 1 defines and tests the pure safe-point lattice before daemon or CLI wiring. Add a small pure module, for example `src/scherzo/workflow_retry_planner.gleam`, and tests in `test/workflow_retry_planner_test.gleam`. At the end, the planner returns explicit decision data for exact resume, changed-boundary rewind when an interface snapshot proves compatibility, artifact-prefix rewind when an upstream artifact is corrupt or missing, fresh degradation when proof is missing or the DAG is incompatible, and hard-stop rejection only for explicit holds, terminal issue state, dispatch pause, or active duplicate work. The acceptance evidence is a table-style test matrix that names the selected safe point, preserved step ids, discarded step ids, and degradation reason for each case.

Milestone 2 records durable workflow-interface evidence for new runs. Add a snapshot encoder/decoder, for example `src/scherzo/workflow_interface_snapshot.gleam`, plus artifact-store, checkpoint-writer, record, projection, and recovery plumbing in `src/scherzo/state/artifact_store.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and the workflow start path. At the end, fresh workflow starts write an interface snapshot artifact and ledger reference; projection exposes it; tests in `test/workflow_interface_snapshot_test.gleam`, `test/state_record_test.gleam`, and `test/state_projection_test.gleam` prove round-trip decoding, SHA preservation, and old-run absence. Old runs without the record must be explicitly reported as `interface_snapshot_missing` when workflow drift prevents a provable partial resume.

Milestone 3 wires daemon retry execution through the unified planner. Update `src/scherzo/workflow_repair.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/workflow_recovery_planner.gleam`, `src/scherzo/orchestrator/retry_step_operation.gleam`, `src/scherzo/orchestrator/retry_step_resumption.gleam`, `src/scherzo/orchestrator/transitions/operator.gleam`, and `src/scherzo/orchestrator/daemon.gleam` so queued retry-step-style operations, task retry, forced fresh retry, and dispatch-time retry recovery share the same decision lattice. At the end, operation completion messages name the selected safe point, preserved steps, discarded steps, and degradation reason; tests in `test/orchestrator_daemon_retry_step_test.gleam`, `test/recovery_workflow_checkpoint_test.gleam`, and a property-style retained-run matrix prove workflow drift and artifact corruption no longer end as `workflow_drift` or `artifact_recovery_failed` for common retry.

Milestone 4 collapses the operator surface while preserving expert overrides. Update `src/scherzo/ctl.gleam`, `src/scherzo/ctl/command_registry.gleam`, and `src/scherzo/control/command.gleam` so `retry <task>`, `task retry <task>`, `retry-step <target>`, and the new `retry step <target>` use the lattice; `retry all <task>` and `--from-scratch` force fresh supersession; and `run retry-step` remains the exact expert override for operators who intentionally want old fail-closed semantics. At the end, `test/ctl_test.gleam` and command/protocol tests prove aliases parse, forced-fresh commands carry an explicit reason, and deprecated/expert overrides still behave as documented.

Milestone 5 completes documentation, helper inventory, full validation, and linting. At the end, the runbook update explains the new common retry meaning, the implementation notes whether docs/helper surfaces were untouched or names their validation evidence, and targeted unit and daemon tests, command/protocol tests, the retained-run matrix, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and review-doc validation all pass or record any unrelated pre-existing failures with evidence.

## Progress

- [x] (2026-07-03) Read the repo-local ExecPlan guidance and prepared this concise review document with mechanical implementation detail reserved for the structured pack.
- [x] (2026-07-03) Inspected the current retry-step planner, startup/dispatch recovery planner, daemon queued operation path, command parser, artifact recovery path, and superseded-run projection support.
- [x] (2026-07-03) Incorporated review feedback by tightening milestone specificity, acceptance evidence, test obligations, docs/helper inventory, full validation and lint obligations, and explicit non-pre-publish treatment for provider-live/cache, browser, and manual dogfood checks.

## Surprises & Discoveries

`workflow_repair.plan` currently checks issue and workflow drift before selecting the repair boundary, so a workflow YAML change prevents even a safe fresh degradation from the retry-step operation path. Artifact corruption is discovered during recovery finalization after a retry-step operation has already been queued, which is why the current result becomes `artifact_recovery_failed` rather than rewinding before the corrupt artifact. The repository already has `workflow_run_superseded` records and projection support, so fresh degradation can be represented additively instead of inventing a destructive migration.

## Decision Log

- Decision: Make the common retry paths use one safe-point lattice and leave `run retry-step` as the exact expert override.
  Rationale: Operators get the total common behavior without losing the old diagnostic tool when they need fail-closed proof of a specific step.
  Date: 2026-07-03.

- Decision: Add durable workflow-interface snapshots for new runs and degrade old drifted runs fresh when snapshots are missing.
  Rationale: Changed-boundary rewind is only safe when Scherzo can prove the old and current interfaces match; older ledgers often cannot prove that.
  Date: 2026-07-03.

- Decision: Use `workflow_run_superseded` plus diagnostics and operation messages for fresh degradation provenance.
  Rationale: Projection already understands superseded workflow runs, and operation messages can carry the human-readable preserved/discarded summary.
  Date: 2026-07-03.

- Decision: Treat review feedback about acceptance evidence, test obligations, milestone specificity, docs/helper migration boundaries, provider-live/cache boundaries, full validation, linting, and manual dogfood timing as implementation-pack obligations.
  Rationale: The human review document and structured implementation pack must stay aligned so the follow-up implementation cannot satisfy one artifact while missing the other.
  Date: 2026-07-03.

## Outcomes & Retrospective

Implementation has not started. The expected outcome is that ordinary retry no longer strands operators on drift or artifact-recovery rejections: it either starts a safe retained-run resume or starts a fresh superseding run with explicit provenance. The retrospective should compare final evidence against the three acceptance outcomes: workflow YAML changed mid-run, corrupt upstream artifact, and a matrix of retained failed/interrupted states.

## Validation and Acceptance

Acceptance is pre-publish automated evidence. Add planner coverage in `test/workflow_retry_planner_test.gleam` for exact same-fingerprint resume, compatible workflow drift with a snapshot, workflow drift without a snapshot, incompatible DAG, corrupt artifact, missing artifact, explicit operator hold, terminal issue state, dispatch pause, and active duplicate work. Each test must assert the selected safe-point kind, selected step id when applicable, preserved step ids, discarded step ids, degradation reason, and whether the result is a hard stop or a fresh supersession.

Add snapshot coverage in `test/workflow_interface_snapshot_test.gleam`, `test/state_record_test.gleam`, and `test/state_projection_test.gleam` showing that a fresh run records a workflow-interface artifact and ledger reference, the artifact SHA and decoded content are stable, projection exposes the snapshot by run id, and an older run without the record is distinguishable from a corrupt snapshot. The snapshot tests must include step ids, dependency edges, workspace source contracts, structured-output contracts, workflow contract inputs/outputs, publication routes when present, and per-step execution fingerprints so changed-workflow retry is falsifiable rather than heuristic.

Add daemon coverage showing that after a workflow prompt or YAML change between failure and retry, `retry-step` or `retry step` no longer completes with `workflow_drift`; it either records a changed-boundary resume when a snapshot proves compatibility or records `workflow_run_superseded` and starts a fresh run, and the completed operation message names the selected safe point. Add daemon coverage showing that corrupting or deleting an upstream step artifact no longer completes with `artifact_recovery_failed`; the planner preserves only verified upstream evidence, supersedes the corrupt or missing step and descendants, and starts a recovered worker from the earlier boundary or degrades fresh if no verifiable prefix exists.

Add a property-style retained-run matrix test that enumerates failed and interrupted runs across exact match, workflow drift with compatible snapshot, workflow drift without snapshot, corrupt artifact, missing artifact, incompatible DAG, manual hold, terminal issue state, dispatch pause, and active duplicate work. For every non-held, non-terminal, non-paused, non-duplicate retained failed/interrupted case, the observable result must be a running recovered worker or a fresh superseding run, never an operation failure requiring daemon restart. Command tests in `test/ctl_test.gleam` and protocol/codec coverage must prove `retry all` and `--from-scratch` force fresh supersession, `retry step <target>` reaches the lattice, and `run retry-step <run-id> --step <step-id>` remains the exact fail-closed expert override.

Final validation must run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1371-unified-retry-planner.md`. The implementation handoff must record a docs/helper inventory: if `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contracts were untouched, state that no helper migration was applicable; if touched, include the relevant helper or offline contract validation evidence. No browser, provider-live/cache, or manual dogfood evidence is required before publish. A manual dogfood retry of a real retained run is deferred to a human/operator check after implementation if maintainers want live confidence beyond the automated gates.

## Rollout, Recovery, and Idempotence

Rollout is additive. New interface snapshot records help future retries but old ledgers continue to decode; when proof is absent, the planner degrades to fresh rather than mutating historical records. If the feature misbehaves, pause dispatch or place an explicit operator hold on affected issues, then revert the planner and command-surface changes; retained runs and new supersession records remain readable because they use existing workflow-run state concepts plus additive snapshot records.

Retry operations must be repeatable. Repeating the same retry after a resume is queued or running should report the current queued/running operation or active run without appending duplicate repair records. Repeating a fresh degradation after the source run is already superseded should report the superseding run rather than creating another one. Forced-from-scratch retries should be idempotent at the issue/run boundary and should still respect explicit holds, terminal states, dispatch pause, and active duplicate-work checks.

Provider-live behavior, provider cache behavior, browser UI behavior, and `.scherzo/workflows` helper contracts are not part of rollout. If implementation unexpectedly needs any of those surfaces, split that work or require explicit validation before publish; do not silently broaden this retry-planner rollout. After implementation, a human operator may optionally dogfood the new retry command against a retained failed run and inspect `query operation-status` or `scripts/scherzoctl session <session-ref>` output, but that live check is deferred and must not block the automated pre-publish acceptance gates.

## Open Questions and Clarifications Needed

No blocking open questions. After implementation, maintainers can decide whether the exact expert override should continue to be spelled only `run retry-step` or whether another explicitly named fail-closed command should be introduced for clarity.
