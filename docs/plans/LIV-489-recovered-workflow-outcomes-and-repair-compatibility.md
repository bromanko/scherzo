# Recovered workflow outcomes and repair compatibility

## Purpose / Big Picture

Scherzo needs stable terminal outcome names that tell operators and downstream automation when a workflow only finished after step-level recovery was attempted. In this plan, "recovered" means the LIV-488 runtime started or completed a workflow-step recovery attempt for the same workflow run; it does not mean Scherzo merely resumed an interrupted run during daemon startup. After LIV-489, clean workflow runs still record the existing clean outcomes, while step-recovered runs record `succeeded_after_recovery` or `failed_after_recovery` exactly. The same change must preserve repair and startup-recovery behavior so a recovered failure is still treated as a failed workflow that can be retried or repaired.

## Problem Framing and Constraints

The foundation recovery work already added durable step-recovery records and reserved the recovered outcome names, but the runtime and compatibility boundaries still need a precise implementation plan. The core constraint is compatibility: old `completed`, `failed_fatal`, and `cancelled` records must remain valid, clean runs must not be renamed just because the code now understands recovery, and startup-resumed runs with no step-recovery records must not be relabelled. LIV-489 depends on LIV-488 providing runtime recovery execution or an equivalent in-memory or durable marker that a workflow run had step-recovery activity before its terminal result.

The outcome strings are data-shape compatibility work, not operator timeline rendering. This plan intentionally keeps rich timeline display, browser/UI work, provider-live behavior, cache behavior, and broad negative-path hardening out of scope except where an existing reader must accept the new data shape safely.

## Strategy Overview

Keep the scheduler recovery-agnostic. The scheduler should continue to answer only whether the DAG is in progress, succeeded, or failed; it should not learn presentation strings. Add or use a small compatibility mapping layer for workflow outcome constants, exact string rendering, and success/failure predicates, then have the terminal workflow-run boundary choose between clean and recovered strings based on a same-run step-recovery marker from LIV-488. The checkpoint and ledger should persist the chosen string verbatim, and projection/history readers should treat it as stored data for new records rather than recomputing it.

Repair and recovery-planner compatibility should use predicates from that same mapping layer. `failed_after_recovery` must be classified as terminal failed anywhere the system currently treats `failed_fatal` as repairable, cleanup-eligible, or planner-terminal. Existing clean strings remain unchanged, and existing ledger/projection data requires no rewrite. If LIV-488 exposes only durable recovery records rather than an in-memory boolean, LIV-489 should derive the marker from same-run `workflow_step_recovery_started` or `workflow_step_recovery_finished` records rather than from the existing startup `RecoveredRun` path.

## Alternatives Considered

One alternative was to infer recovered outcomes only in history or projection by looking for step-recovery records next to a clean terminal outcome. That preserves the existing terminal strings, but it makes exact observable outcome strings dependent on each reader and leaves ledger consumers with ambiguous terminal records.

Another alternative was to add recovered states directly to `workflow_scheduler.gleam`. That would overfit the scheduler to audit semantics; recovery is about how the run reached a terminal state, while scheduling only needs dependency and failure status.

A third option was to rename every post-recovery step attempt outcome. That is unnecessary for the MVP and risks breaking dependency completion. The workflow terminal outcome should carry the recovered success/failure distinction; step attempts should continue to use existing clean step outcomes unless LIV-488 already produced a recovered step outcome that compatibility code must read.

## Risks and Countermeasures

The main risk is silently changing clean-run behavior. The countermeasure is to add tests that run clean success and clean failure paths and assert the exact stored outcomes remain `completed` and `failed_fatal`.

A second risk is scattering string comparisons so one boundary treats `failed_after_recovery` as failed while another does not. The countermeasure is a single outcome helper used by repair selection, planner parsing/emission, projection compatibility, and tests.

A third risk is old or partially upgraded ledgers. The countermeasure is additive parsing: existing records keep their stored outcomes, new records may use the recovered names, and compatibility predicates accept both clean and recovered failed terminal names without projection migration.

A fourth risk is conflating daemon startup recovery with step recovery. The countermeasure is an explicit test that a run resumed from checkpoint with no step-recovery record keeps `completed` or `failed_fatal`, plus recovered-path tests that only turn on the new strings after same-run step-recovery evidence exists.

A fifth risk is documentation or acceptance drift. The countermeasure is to update the step-recovery runbook when outcome emission lands, require exact pre-publish test/format/lint evidence, and explicitly record that browser, live-provider, cache, and dogfood checks are not pre-publish blockers for this data-shape ticket.

## Scope Boundaries

In scope are workflow terminal outcome constants and predicates, terminal outcome emission for recovered success and recovered failure, repair eligibility for `failed_after_recovery`, recovery-planner parsing and finish-record behavior for recovered outcomes, exact-string tests, projection/record compatibility tests, and a small runbook/data-shape note in `docs/runbooks/workflow-step-recovery.md` that moves the recovered outcome names from "deferred" to "emitted by LIV-489" once implementation lands.

Out of scope are operator-facing nested recovery timeline rendering, browser/UI changes, live provider or cache behavior changes, a broader failure-classification system, additional recovery negative-path hardening, helper migrations unrelated to terminal outcome classification, and changing clean terminal outcome names. Ledger migration is limited to backward-compatible readers and documented coexistence; no stored records should be rewritten.

## Milestones

Milestone 1 establishes a single compatibility vocabulary. At the end, code has one place that names `completed`, `failed_fatal`, `cancelled`, `succeeded_after_recovery`, and `failed_after_recovery`, plus predicates for terminal success and terminal failure, and targeted tests prove the exact strings and predicate classifications.

Milestone 2 validates the recovery marker boundary before runtime emission changes. At the end, the implementer has identified the LIV-488 same-run step-recovery signal, or has derived it from durable `workflow_step_recovery_started` and `workflow_step_recovery_finished` records; a startup-resumed run without that signal remains a clean outcome in tests.

Milestone 3 makes runtime terminal emission recovery-aware. At the end, a workflow run that reaches success after LIV-488 step-recovery activity persists `succeeded_after_recovery`, a workflow run that fails after step-recovery activity persists `failed_after_recovery`, and clean success/failure still persist the old strings.

Milestone 4 makes repair and planner readers accept recovered failures. At the end, `workflow_repair` can select a `failed_after_recovery` run by run id or issue target, failed-step repair still computes the next retry attempt, and `workflow_recovery_planner` maps stored recovered terminal strings to terminal planner outcomes while emitting recovered finish intents only for active runs with same-run recovery activity.

Milestone 5 validates stored-data compatibility and documentation. At the end, projection and record tests prove that old records replay unchanged, new recovered records retain their exact strings, no migration or snapshot rewrite is required, and the step-recovery runbook accurately documents the emitted outcomes and deferred UI timeline work.

Milestone 6 completes pre-publish acceptance evidence. At the end, targeted tests, full Gleam tests, formatting, glinter, and Scherzo lint have been run from the repository root, with any unrelated pre-existing inventory explicitly called out before handoff.

## Progress

- [x] (2026-05-22 00:00Z) Inspected relevant scheduler, workflow run, checkpoint, projection, repair, planner, and step-recovery record files.
- [x] (2026-05-22 00:00Z) Created this human-reviewable ExecPlan review document; mechanical implementation details are in the structured implementation-pack submission.
- [x] (2026-05-22 00:00Z) Incorporated review feedback by making milestones independently verifiable, adding exact acceptance evidence for runtime/projection/repair/planner behavior, spelling out the runbook documentation obligation, requiring full pre-publish test/format/lint validation, and preserving browser, provider-live, cache, and dogfood checks as non-blocking or deferred.

## Decision Log

- Decision: Persist recovered workflow outcomes at the workflow terminal checkpoint boundary, not only in history rendering.
  Rationale: The ledger should contain the stable observable outcome string, while readers can remain simple and deterministic.
  Date: 2026-05-22

- Decision: Keep `workflow_scheduler.gleam` unaware of recovered outcome names.
  Rationale: The scheduler decides terminal shape, not how the run reached that terminal shape.
  Date: 2026-05-22

- Decision: Use a compatibility mapping layer for exact strings and predicates.
  Rationale: Repair, planner, projection, and tests need consistent treatment of `failed_after_recovery` without duplicating literals.
  Date: 2026-05-22

- Decision: Do not migrate existing ledger or projection data.
  Rationale: The new outcomes are additive strings; old records are still meaningful and must replay without rewrite.
  Date: 2026-05-22

- Decision: Treat startup `RecoveredRun` execution and step-recovered terminal naming as separate concepts.
  Rationale: Startup recovery resumes durable state after daemon interruption, while this ticket's outcome names describe a nested step-recovery attempt inside a run; conflating them would rename clean resumed runs incorrectly.
  Date: 2026-05-22

- Decision: Do not require browser, provider-live, cache, or dogfood evidence before publish.
  Rationale: LIV-489 changes durable ledger/projection data and failed-terminal classification; manual/operator timeline confidence belongs to later UI/history work or optional post-implementation dogfood observation.
  Date: 2026-05-22

## Validation and Acceptance

Acceptance is verifiable only with exact-string evidence. LIV-489 must include tests that assert clean success records `completed`, clean failure records `failed_fatal`, recovered success records `succeeded_after_recovery`, and recovered failure records `failed_after_recovery`. Those tests must also assert that startup-resumed execution without same-run step-recovery evidence keeps the clean outcome names. The repair tests must prove a `failed_after_recovery` workflow run is selected as a failed run and remains retry-step eligible. The recovery-planner tests must prove stored `succeeded_after_recovery` maps to terminal success, stored `failed_after_recovery` maps to terminal failure, and active recovered runs emit recovered finish intents only when same-run recovery activity exists.

Compatibility evidence must include projection or record replay tests showing old clean records remain unchanged and new recovered records round-trip without schema migration. Documentation evidence must show `docs/runbooks/workflow-step-recovery.md` no longer lists recovered terminal outcome emission as deferred once LIV-489 implements it, while still deferring operator-facing nested timeline rendering. Pre-publish validation for the implementation is `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root.

No browser, live-provider, cache, or dogfood evidence is a pre-publish requirement for this ticket because the changed observable surface is durable ledger/projection data. Operator-facing manual timeline evidence is explicitly deferred to later UI/history work; if a human wants an additional dogfood check after implementation, it should be collected after publish as an operator check, not as a blocker for LIV-489.

## Rollout, Recovery, and Idempotence

Rollout is additive. If the change is deployed over old ledgers, replay keeps existing clean strings, and only future step-recovered terminal records use the new names. If the implementation must be backed out, records already written with recovered names should still parse because record and projection outcomes are strings; the safe fallback is to keep compatibility predicates even if runtime emission is disabled.

The work is idempotent because it does not rewrite ledgers or snapshots. Re-running tests or replaying the same ledger should produce the same projection. If LIV-488's recovered-run marker is missing during implementation, LIV-489 should stop at the dependency check rather than guessing from unrelated logs; if recovery records are present but no marker exists, derive the marker only from durable `workflow_step_recovery_*` records for the same run. If recovered names have already been written before a rollback, repair and planner compatibility predicates should remain in place so operators can still retry or inspect those runs.

## Open Questions and Clarifications Needed

No product decision is blocking. The implementer must verify the exact LIV-488 runtime marker name before coding; the required semantics are that the final workflow-run boundary can answer whether this run attempted step recovery before terminal success or failure. If LIV-488 exposes only durable recovery records, the implementation should derive that boolean from same-run recovery records and document that adapter choice in the implementation notes. If neither signal exists in the implementation branch, stop and request a dependency update instead of guessing from startup recovery state, logs, provider-live checks, cache entries, or UI history.
