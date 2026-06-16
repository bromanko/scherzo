# Extend dispatch recovery with active-state retry intent and failure-code repair recipes

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as implementation proceeds.

## Purpose / Big Picture

A Scherzo operator should be able to move a parked failed workflow issue back to an active dispatch state such as `Todo` and have Scherzo choose the safest retained-run recovery path automatically. After this change, Scherzo will treat that board move as retry intent for eligible auto-unparked failed runs, avoid starting a fresh full workflow when retained recovery evidence is safer, and use failure-code-specific repair recipes when a plain retry-step would repeat the same stale failure.

The first recipe targets `plan_completion_verdict_stale` in `workflow:execplan-implementation`: instead of asking an operator to edit JSON by hand, Scherzo will refresh the plan-completion verdict through audited workflow steps, checkpoint it through the existing helper command, and then resume downstream finalization and publication.

## Problem Framing and Constraints

Scherzo already has dispatch-time recovery classification, retry-step repair planning, publication retry, retained workspaces and artifacts, and auto-unpark-on-issue-content-change. The missing behavior is narrower: a state-only move from a failure state back to `Todo` is not an issue-content change, so the parked issue can remain locally blocked; unresolved repair-boundary selection can still count old failed attempts that were later repaired; and failure codes such as `plan_completion_verdict_stale` need a recipe that refreshes prerequisite artifacts before retrying the failing finalization step.

This plan must preserve Scherzo's safety properties. It must fail closed on workflow drift, issue drift, ambiguous repair boundaries, corrupt or missing artifacts, unsupported recipes, tracker transition failures, and explicit operator holds. It must append auditable records or diagnostics rather than rewriting ledger history, and it must use existing workflow commands to mutate retained artifacts.

Review feedback is part of the implementation contract. The Markdown plan and structured implementation pack must agree on acceptance evidence, exact test obligations, milestone-specific proof, manual dogfood timing, docs/helper migration boundaries, provider-live/cache non-scope, full validation, and lint gates. Those obligations are pre-publish requirements except where this document explicitly defers them to a human/operator check after implementation handoff.

## Strategy Overview

The implementation should extend the current dispatch recovery path rather than replace it. First, make active dispatch states count as retry intent for parked failed runs whose release policy is already automatic; explicit holds remain explicit. Second, teach retry-step planning to distinguish unresolved failures from failures that have already been superseded or successfully rechecked. Third, add a small failure-code recipe layer that loads the failed command-step artifact by retained reference and SHA, matches a safe recipe, and either rewrites the repair boundary to an earlier proof/checkpoint step or rejects with evidence.

For `plan_completion_verdict_stale`, the recipe should require the current `execplan-implementation` DAG shape and select the final verification/checkpoint chain as the recovery boundary, so the workflow reruns verifier, checkpoint, final validation, final gate, and finalization in order. Dispatch and explicit retry-step should share this planner so manual and automatic recovery stay consistent.

## Alternatives Considered

Keeping recovery fully manual is safe but preserves the LIV-1168 failure mode and requires operators to know retained-workspace internals. Automatically running a whole-workflow retry is simpler but can discard useful work and duplicate expensive validation or publication. Editing `tmp/` or canonical verdict JSON directly is rejected because it bypasses workflow artifacts, checksums, and helper validation. A general workflow-engine retry loop is also too broad for this incremental recovery gap.

## Risks and Countermeasures

The largest risk is resuming the wrong retained run. The countermeasure is to keep the existing provenance, workflow fingerprint, issue fingerprint, task identity, run-root containment, and artifact SHA checks before any recovery starts. Another risk is hiding a real failure behind a recipe. Recipes must be allowlisted by workflow id, failed step id, failure code, DAG shape, and artifact integrity, and unsupported cases must park with a clear reason instead of falling back to a fresh run.

Boundary ambiguity is a specific risk because old failed attempts may remain in projection after successful recovery. The planner must ignore resolved failures, keep true multiple-current-failure cases as `ambiguous_repair_step`, and test both sides. Tracker comment or state-transition failure must not cause a dispatch loop; Scherzo should log, park locally, and suppress immediate repeat dispatch as current dispatch recovery does.

A review-specific risk is accepting a smart-retry implementation whose prose promises evidence that the structured pack omits. The countermeasure is to mirror the same test names, full validation commands, lint gates, docs/helper inventory, provider-live/cache non-scope, and dogfood timing in the implementation pack before bundle materialization.

A scope-creep risk is accidentally turning this recovery fix into a helper migration, provider-live experiment, cache invalidation change, or browser/UI task. The countermeasure is a pre-publish diff inventory: if those surfaces are untouched, record that fact; if any must change, stop, split that work, and add helper/contract or provider-live/cache tests before accepting the broader scope.

## Scope Boundaries

In scope are `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/dispatch_recovery.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/workflow_repair.gleam`, `src/scherzo/state/projection.gleam`, ledger record/projection support for durable smart-retry evidence, the `workflows/dogfood/scripts/scherzo-implementation` helper only if an audited refresh command needs a narrow addition, `workflows/dogfood/execplan-implementation.yaml` only for documented step ids and DAG-shape tests, and focused tests under `test/`.

The expected helper/docs path is inventory-only: the implementation should reuse the existing `checkpoint-plan-completion-verdict`, `gate-plan-completion --final`, and plan-completion recovery commands. If a narrow helper change is unavoidable, update `test/execplan_implementation_helper_test.gleam` and keep the command audited; otherwise record that no helper migration or documentation migration was needed.

Out of scope are changing `scherzoctl retry` whole-run semantics, recovering arbitrary command failure codes, direct JSON artifact mutation, browser/UI work, provider-live behavior, provider cache behavior, cleanup ownership changes, or changing Linear workflow states beyond existing state-transition/comment capabilities. Provider-live and cache behavior require no pre-publish validation for this plan unless the implementation deliberately touches those surfaces, in which case the work must be split or accepted only with stale-read, invalidation, and TTL-disabling evidence.

## Milestones

Milestone 1 proves active-state retry intent. A parked failed issue whose release policy is `auto_unpark_on_issue_change` and whose current state is in the configured ready/dispatch set, such as `Todo`, becomes eligible for dispatch recovery even when its content fingerprint is unchanged. Explicit holds, terminal states, and non-ready states still block. The proof is a focused core or daemon dispatch test that fails before the change because the parked issue is skipped, and passes after the change by reaching the dispatch recovery planner rather than starting a fresh full run.

Milestone 2 fixes unresolved repair-boundary selection. Retry-step planning ignores failed attempts that were later successfully recovered or superseded, while true concurrent unresolved failures remain ambiguous. The proof is red-to-green coverage in `test/workflow_repair_test.gleam` for a repaired old failure, a superseded old failure, and a real multiple-unresolved-failure case that still returns `ambiguous_repair_step`.

Milestone 3 introduces safe failure-artifact inspection and recipe matching. Command-step artifacts are read only through retained artifact refs and expected SHA values, and corrupt, missing, or mismatched artifacts produce fail-closed evidence. The proof is targeted coverage that loads a valid failed `StepArtifact`, rejects a SHA mismatch, rejects missing/corrupt JSON, and rejects unsupported workflow/step/failure-code combinations without falling back to fresh dispatch.

Milestone 4 implements the `plan_completion_verdict_stale` recipe. The LIV-1168-shaped retained run selects `verify_plan_completion_before_final_validation` as the repair boundary when the failed `final_plan_completion_gate` command artifact carries `failure_code: plan_completion_verdict_stale` and the checked-in `workflows/dogfood/execplan-implementation.yaml` DAG still has the verified final chain `verify_plan_completion_before_final_validation -> checkpoint_final_plan_completion_verdict -> final_validate -> final_plan_completion_gate -> finalize_final_plan_completion_gate -> materialize_commit_stack`. The proof is a recovery-planner test showing the selected boundary, descendants, next attempt index, and no fresh full dispatch.

Milestone 5 records and reports smart-retry decisions. Operators can see in the ledger, logs, and Linear comment which run, failed step, failure code, recipe, selected repair boundary, and next attempt Scherzo chose. The proof is daemon-level coverage in `test/orchestrator_daemon_retry_step_test.gleam` that asserts durable records or diagnostics are appended once, repeated polling is idempotent, tracker transition/comment failure parks locally, and the recovered worker resumes from the recipe boundary.

Milestone 6 completes regression and rollout evidence. Targeted tests, full Gleam tests, format, glinter, Scherzo lint, and review-doc validation pass before publish. The implementation evidence must include a diff inventory proving no docs/helper migration, provider-live change, provider-cache change, or browser/UI check was introduced, unless such a change is explicitly split and separately validated. A live dogfood board exercise is deferred to a human/operator check after implementation handoff when credentials and a retained LIV-1168-shaped fixture or staging issue are available.

## Progress

- [x] (2026-06-16) Read Scherzo's repo-local ExecPlan authoring guidance and prepared this concise review document plus a structured implementation pack.
- [x] (2026-06-16) Inspected the current dispatch recovery, retry-step planner, daemon dispatch hook, projection records, plan-completion helper, workflow YAML, and existing retry-step/plan-completion tests.
- [x] (2026-06-16) Incorporated review feedback by making acceptance evidence, targeted tests, milestone proofs, manual dogfood timing, docs/helper inventory, provider-live/cache non-scope, full validation, linting, and implementation-pack mirroring explicit.

## Surprises & Discoveries

- Observation: `tracker_issue.content_fingerprint` intentionally omits issue state and labels, so moving a parked issue from `Triage` to `Todo` does not trigger the existing auto-unpark-on-issue-content-change path. Evidence: `src/scherzo/tracker/issue.gleam` fingerprints id, identifier, title, description, priority, branch, blockers, and blocker completeness.
- Observation: current `workflow_repair.repair_boundaries` selects failed/interrupted attempts by status and does not consult `projection.step_recoveries` when deciding whether an old failure was later repaired. Evidence: `src/scherzo/workflow_repair.gleam` builds boundaries from `StepAttemptFinishedStatus` terminal failures and `StepAttemptInterruptedStatus`.
- Observation: the plan-completion helper already has audited commands for the desired mutation path. Evidence: `workflows/dogfood/scripts/scherzo-implementation` defines `checkpoint-plan-completion-verdict`, `gate-plan-completion --final`, and canonical verdict restore from run-root state.

## Decision Log

- Decision: Treat active dispatch state as retry intent only for automatically releasable parked failed runs, not for explicit holds. Rationale: this matches normal board workflow while preserving operator holds. Date: 2026-06-16.
- Decision: Use allowlisted failure-code recipes instead of generic artifact mutation. Rationale: each safe repair has workflow-specific preconditions and evidence requirements. Date: 2026-06-16.
- Decision: For `plan_completion_verdict_stale`, rerun the final plan-completion verification/checkpoint chain through workflow steps rather than editing retained JSON. Rationale: this preserves structured-output capture, helper validation, checksums, and ledger auditability. Date: 2026-06-16.
- Decision: Treat review feedback about evidence, tests, milestone specificity, dogfood timing, docs/helper boundaries, provider-live/cache boundaries, full validation, and linting as obligations in both this document and the structured implementation pack. Rationale: Scherzo materializes downstream implementation artifacts from the pack, so prose-only obligations are easy to lose. Date: 2026-06-16.
- Decision: Keep provider-live behavior, provider cache behavior, browser/UI work, and broad helper/docs migration out of scope. Rationale: the retry intent and stale-verdict repair are dispatch-recovery concerns; touching those surfaces would expand blast radius and require separate acceptance tests. Date: 2026-06-16.

## Outcomes & Retrospective

This authoring task produces the implementation plan only. The intended outcome of implementation is that the LIV-1168-shaped recovery no longer requires manual unpark plus JSON surgery: the operator board move is enough to start a safe smart retry or to receive a clear fail-closed explanation.

## Validation and Acceptance

Pre-publish acceptance requires focused tests proving active-state retry intent, resolved-failure boundary filtering, corrupt-artifact rejection, the `plan_completion_verdict_stale` recipe, idempotent repeated polling, and no fresh dispatch on smart-retry paths. Concrete evidence should include new or updated tests in `test/orchestrator_core_test.gleam` or equivalent daemon coverage for the active-state parked issue precondition, `test/workflow_repair_test.gleam` for repair-boundary and recipe planning, `test/orchestrator_daemon_retry_step_test.gleam` for dispatch-time smart retry and duplicate-poll suppression, and `test/execplan_implementation_helper_test.gleam` if the helper is touched or to retain the existing stale-verdict failure-code evidence.

The acceptance evidence must name the new test functions and their assertions. Required scenarios are: same-fingerprint auto-parked issue moved to `Todo` reaches smart retry; explicit hold remains blocked; old repaired or superseded failures are ignored; two unresolved failures remain ambiguous; valid `plan_completion_verdict_stale` artifact selects `verify_plan_completion_before_final_validation`; SHA mismatch, corrupt artifact, missing artifact, unsupported workflow id, unsupported failed step id, unsupported failure code, and DAG drift all reject with clear reasons; repeated polls do not append duplicate repair records; and smart retry does not create a fresh full dispatch before appending repair records.

Run from the repository root: `direnv exec . gleam test test/orchestrator_core_test.gleam`, `direnv exec . gleam test test/workflow_repair_test.gleam`, `direnv exec . gleam test test/orchestrator_daemon_retry_step_test.gleam`, `direnv exec . gleam test test/execplan_implementation_helper_test.gleam`, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `direnv exec . python3 workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1171-smart-retry-dispatch-parked-failed-runs.md`. Expected result is success for all commands.

Pre-publish evidence also requires a scope inventory: report whether `workflows/dogfood/scripts/scherzo-implementation`, workflow schemas, provider-facing structured-output helpers, docs, provider-live code, provider cache code, and browser/UI files were unchanged. No provider-live, cache, browser, or human dogfood evidence is required before publish when those surfaces remain untouched. Post-implementation manual dogfood evidence is deferred until a retained fixture or staging issue is available: move a LIV-1168-shaped parked failed issue to `Todo` and capture the Linear comment, ledger smart-retry records, recovered worker or publication events, and absence of a fresh full dispatch.

## Rollout, Recovery, and Idempotence

Roll out as an additive dispatch-recovery behavior with no durable ledger rewrite and no migration. If it misbehaves, pause dispatch, move affected issues back to the failure state, and use existing explicit `scherzoctl retry-step`, publication retry, or whole-run retry while reverting the implementation. Recipes must be idempotent: once a repair request is appended and the recovered run is active, repeated polls must not append duplicate repair records; once a recipe rejects, the issue remains locally parked until state/content changes or explicit operator action.

Rollback does not require provider-live cleanup, cache invalidation, browser rollback, or helper migration rollback when the implementation stays within scope. If the implementation intentionally changes helper/provider/cache surfaces, that change must carry its own rollback and validation notes before publish.

## Open Questions and Clarifications Needed

No open questions. If implementation discovers that the final verification step is not safe to rerun after final validation, stop and update this plan before choosing a narrower checkpoint-only recipe.
