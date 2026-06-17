# Add active-state retry intent for parked failed dispatch recovery

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as implementation proceeds.

## Purpose / Big Picture

A Scherzo operator should be able to express retry intent with the workflow board action they already use: move an automatically releasable parked failed issue back to an active dispatch state such as `Todo`. Today that state-only move can leave the issue blocked because Scherzo's automatic unpark check compares the issue content fingerprint, and the fingerprint intentionally does not include state.

After this change, an auto-parked failed issue whose release policy is `auto_unpark_on_issue_change` can also be released when the current issue is in a configured dispatch state. The issue then flows through the existing dispatch validation and dispatch recovery classifier, which can choose retained step recovery, publication recovery, fresh dispatch, or a fail-closed rejection using the safety checks Scherzo already has. This plan is only the generic active-state retry-intent change; it does not add workflow-specific failure-code recipes or make Scherzo core understand one dogfood workflow's final gate semantics.

## Problem Framing and Constraints

The operator problem is specific and real: if a workflow fails and Scherzo parks the issue with `AutoUnparkOnIssueChange`, changing the issue title or description can release it, but moving it from a failed/non-ready state back to `Todo` does not. That board move usually means "please retry," yet the local parked entry is still present, so dispatch preconditions reject the candidate before the retained-run recovery path can classify the run.

The fix must preserve all existing safety boundaries. Explicit operator holds remain blocked until explicit operator action. Terminal states, non-ready states, missing required issue fields, dependency blockers, active runs, claims, slot limits, workflow-policy checks, retained-run provenance checks, issue/workflow drift checks, and publication retry validation remain authoritative. A same-fingerprint state move must not bypass those checks; it only removes the local auto-park barrier so the candidate can reach them.

This revision deliberately removes the previous recipe system from scope. Core must not inspect `plan_completion_verdict_stale`, hardcode `execplan-implementation` step ids, validate a dogfood workflow's DAG shape, read recipe-only artifacts, or append durable smart-retry recipe records/comments. If stale plan-completion recovery still needs automation, it should be designed separately as either generic workflow-declared recovery hints/contracts that core can validate, or as an explicit operator action such as `scherzoctl retry-step --step verify_plan_completion_before_final_validation`.

## Strategy Overview

The implementation should make the smallest dispatch-policy change that exposes the existing recovery machinery. Extend `src/scherzo/orchestrator/core.gleam` with a config-aware helper such as `unpark_if_issue_changed_or_retry_intent(state, config, issue)` while keeping `unpark_if_issue_changed` available for existing content-change callers. The helper keeps the current content-change auto-unpark behavior and adds one more release condition: when the parked entry is `AutoUnparkOnIssueChange(_)` and the current issue state is a configured dispatch state, delete the parked, claimed, retry-attempt, and issue-counter entries the same way the content-change path already does. `ExplicitUnparkOnly` returns the unchanged state.

`src/scherzo/orchestrator/transition.gleam` should call that helper in both dispatch-candidate locations after `core.is_dispatch_state(context.effective, issue.state)` succeeds and before dispatch preconditions run: the initial candidate path and the refreshed-issue claim-validation path. This placement is important: non-dispatch states never release by this path, while blocker checks, slot checks, workflow-policy checks, and daemon dispatch validation still happen afterward. If any legacy dispatch path in `src/scherzo/orchestrator/service.gleam` is still compiled against the helper signature, update it mechanically without changing its behavior. Once validation reaches the daemon claim callback, `src/scherzo/orchestrator/daemon.gleam` already calls `dispatch_recovery.classify(projected, issue, observation)`, so same-fingerprint retries can reuse the retained recovery classifier without a new recipe layer.

## Alternatives Considered

Leaving the behavior manual is safe but preserves the confusing operator experience: moving a failed issue to `Todo` appears to request work but Scherzo silently keeps the local park. Asking operators to use explicit retry-step every time is also too manual for the generic dispatch-state signal, although explicit retry-step remains the right escape hatch for step-specific recovery.

Starting a fresh full workflow whenever a parked issue is moved to `Todo` is rejected because Scherzo already has dispatch recovery that can reuse retained work and validate provenance before resuming. Treating every active state as retry intent is also rejected; the configured dispatch states are the policy boundary for work starts, and non-ready active states must remain non-dispatching.

The earlier failure-code recipe approach is rejected from this plan. It may be useful later, but hardcoding `plan_completion_verdict_stale`, `execplan-implementation` step ids, and DAG-shape knowledge in core is disproportionate to this generic board-move problem and couples Scherzo core to one dogfood workflow.

## Risks and Countermeasures

The main risk is releasing a parked issue when the operator meant to hold it. The countermeasure is to release only `AutoUnparkOnIssueChange` entries and to prove with tests that `ExplicitUnparkOnly` entries remain parked even when the issue is in `Todo` or the content changes.

Another risk is bypassing dispatch safety. The helper must run only inside the already filtered dispatch-state path, and only before the same preconditions that already guard fresh dispatch. Tests must prove that non-dispatch states do not release, dependency blockers still suppress dispatch, and slot limits still prevent a start after the local park is cleared.

A third risk is accidentally preferring a fresh workflow over retained recovery. The countermeasure is a daemon-level test with a same-fingerprint auto-parked issue in `Todo` and an interrupted retained run: the observable evidence must be the existing recovered-worker path and `workflow_repair_requested` records, not `dispatch_started` for a fresh run.

Repeated polling is a loop risk. Once recovery has been requested or a recovered worker is active, a second poll for the same candidate must not append duplicate repair records or start another worker. The plan relies on existing recovery records and active-run checks, and requires a same-fingerprint active-state retry test that sends a second poll and checks record counts.

## Scope Boundaries

In scope are the generic dispatch-policy surfaces: `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/transition.gleam`, any mechanical `src/scherzo/orchestrator/service.gleam` call-site update needed to keep existing compiled behavior, and focused tests in `test/orchestrator_core_test.gleam`, `test/orchestrator_transition_dispatch_test.gleam` if transition-level coverage is the clearest way to prove non-ready/blocker/slot behavior, and `test/orchestrator_daemon_retry_step_test.gleam` for the retained recovery and idempotence path. `src/scherzo/orchestrator/dispatch_recovery.gleam` and `src/scherzo/orchestrator/daemon.gleam` are in scope only for narrow adjustments needed to preserve or observe the existing recovery classification behavior; they should not grow workflow-specific recipe logic.

Out of scope are failure-code recipe planning, `plan_completion_verdict_stale` special handling, artifact-aware recipe reads, `execplan-implementation` DAG-shape checks, dogfood helper changes, workflow YAML changes for this purpose, provider-live behavior, provider cache behavior, browser/UI work, docs migration, Linear state changes beyond existing transition/comment behavior, and new durable smart-retry recipe records/comments. Existing `workflow_repair.plan` and publication retry behavior may be exercised by tests but should not be redesigned by this plan.

## Milestones

Milestone 1 characterizes the board-move gap with focused red tests. Add coverage showing that an auto-parked issue with the same content fingerprint and current state `Todo` remains blocked before the change, while an explicit hold remains blocked and a non-dispatch state such as `Backlog` remains blocked. The observable pre-change failure is that the same-fingerprint `Todo` issue still has a parked entry or never reaches a dispatch validation/recovery effect.

Milestone 2 implements the minimal unpark policy. Add the config-aware helper in `src/scherzo/orchestrator/core.gleam`, update the dispatch-candidate call site in `src/scherzo/orchestrator/transition.gleam`, and preserve the existing content-change behavior. At the end of the milestone, core and transition tests show that same-fingerprint `Todo` releases only automatic parks, explicit holds stay parked, and dispatch preconditions still enforce blockers, non-ready states, terminal states, and slot limits.

Milestone 3 proves that active-state retry intent reaches existing retained recovery rather than fresh dispatch. Add or adjust daemon coverage so a same-fingerprint auto-parked issue in `Todo` with an interrupted retained workflow run starts the existing step-recovery path. The evidence is a recovered worker or `workflow_repair_requested`/`step_attempt_superseded` ledger sequence, absence of a fresh `dispatch_started` signal before recovery, and one set of recovery records after a repeated poll.

Milestone 4 completes validation and scope inventory. Run the targeted tests, the full Gleam suite, formatting, glinter, Scherzo lint, and review-doc validation. Record that the recipe/artifact/helper/provider/cache/browser surfaces were not changed. If implementation discovers it must touch any out-of-scope surface, stop and revise or split the plan before accepting the broader work.

## Progress

- [x] (2026-06-16) Read Scherzo's repo-local ExecPlan authoring guidance and the retained LIV-1171 bundle and implementation pack.
- [x] (2026-06-16) Rechecked the current dispatch candidate path, local park release helper, dispatch recovery classifier, and retry-step daemon tests against the repository tree.
- [x] (2026-06-16) Revised this review document to Plan A only: generic active-state retry intent for automatically releasable parked failed issues.
- [x] (2026-06-16) Removed workflow-specific failure-code recipes, `plan_completion_verdict_stale` handling, hardcoded dogfood step ids, artifact recipe tests, and durable smart-retry recipe reporting from the planned scope.
- [x] (2026-06-16 23:35Z) Implemented `unpark_if_issue_changed_or_retry_intent`, routed dispatch candidates through it, and kept content-change-only callers on the legacy helper.
- [x] (2026-06-16 23:35Z) Added core coverage for same-fingerprint `Todo` retry intent, preserved explicit holds, preserved non-dispatch-state blocking, and blocker enforcement after local unpark.
- [x] (2026-06-16 23:35Z) Added daemon retry-step coverage for same-fingerprint auto-parked `Todo` recovery and duplicate-poll idempotence.
- [x] (2026-06-16 23:35Z) Ran focused core validation, formatting, glinter, Scherzo lint, and review-doc validation; recorded existing unrelated unit/contract-suite failures separately.

## Surprises & Discoveries

- Observation: `tracker_issue.content_fingerprint` omits issue state and labels, so moving an issue to `Todo` cannot trigger the existing content-change auto-unpark path. Evidence: `src/scherzo/tracker/issue.gleam` fingerprints id, identifier, title, description, priority, branch name, blocker completeness, and blocker refs.
- Observation: dispatch candidates are filtered to configured dispatch states before the local unpark helper runs. Evidence: `src/scherzo/orchestrator/transition.gleam` checks `core.is_dispatch_state(context.effective, issue.state)` before calling `core.unpark_if_issue_changed(state.runtime, issue)`.
- Observation: parked issues fail dispatch preconditions before the daemon can classify retained recovery. Evidence: `src/scherzo/orchestrator/core.gleam` includes `!is_parked_for_issue(state, issue)` in `dispatch_preconditions_satisfied_without_slot_capacity`.
- Observation: the daemon already has the retained recovery classification hook this plan needs. Evidence: `src/scherzo/orchestrator/daemon.gleam` calls `dispatch_recovery.classify(projected, issue, observation)` inside `dispatch_time_recovery_claim_issue`, and `src/scherzo/orchestrator/dispatch_recovery.gleam` can return `StepRecovery`, `PublicationRecovery`, `FreshDispatch`, or `RejectRecovery`.
- Observation: `src/scherzo/orchestrator/core.gleam` tripped the checked source-module line baseline when the new helper logic stayed inline. Evidence: `gleam test -- --suite unit` reported `src/scherzo/orchestrator/core.gleam grew beyond its line baseline: 1177 > 1155`, so the unpark implementation was moved into `src/scherzo/orchestrator/parked_issue.gleam` and `core.gleam` now delegates.
- Observation: full unit and contract suite runs currently have unrelated pre-existing failures in this workspace. Evidence: `gleam test -- --suite unit` still fails `runtime_bundle_test.loads_selected_driver_profile_after_capability_match_test`, and `gleam test -- --suite contract` still fails `orchestrator_daemon_retry_step_test.dispatch_recovery_tracker_transition_failure_parks_and_suppresses_repeat_poll_test`.

## Decision Log

- Decision: Narrow the plan to generic active-state retry intent only. Rationale: the actionable review feedback identified a valid core dispatch-policy problem and rejected the workflow-specific repair system as disproportionate. Date: 2026-06-16.
- Decision: Release only `AutoUnparkOnIssueChange` parked entries on configured dispatch-state retry intent. Rationale: this matches the existing automatic release model while preserving explicit operator holds. Date: 2026-06-16.
- Decision: Reuse existing dispatch recovery classification instead of adding a new smart-retry recipe planner. Rationale: `dispatch_recovery.classify` already centralizes retained step recovery, publication retry, fresh dispatch, and fail-closed rejection. Date: 2026-06-16.
- Decision: Defer stale plan-completion automation to a separate design. Rationale: core should validate generic recovery invariants, not know the failure code or DAG semantics of `workflow:execplan-implementation`. Date: 2026-06-16.
- Decision: Keep helper scripts, workflow YAML, artifact-store recipe reads, provider-live/cache behavior, browser/UI, and docs migration out of scope. Rationale: none of those surfaces are needed to make a same-fingerprint `Todo` board move reach existing recovery classification. Date: 2026-06-16.
- Decision: Extract the unpark implementation to `src/scherzo/orchestrator/parked_issue.gleam` while keeping the public helper entry points in `core.gleam`. Rationale: the behavior belongs to orchestrator dispatch policy, but moving the implementation out preserved the checked source-module baseline without widening scope. Date: 2026-06-16.

## Outcomes & Retrospective

Implementation is complete for the narrowed Plan A slice. Same-fingerprint moves of automatically releasable parked issues into configured dispatch states now clear only the local automatic park barrier, then flow through the existing blocker checks, slot checks, workflow-policy checks, and retained recovery classifier. Explicit holds still block, non-dispatch states still block, and blocker checks still suppress dispatch after local unpark.

The final code change touched only the generic dispatch-policy surfaces plus focused tests. To satisfy the repository source-module guardrail, the unpark logic lives in the new helper module `src/scherzo/orchestrator/parked_issue.gleam` while `src/scherzo/orchestrator/core.gleam` keeps the public API expected by the rest of the orchestrator. The broader stale plan-completion repair remains intentionally deferred.

## Validation and Acceptance

Focused acceptance requires named tests for the active-state retry-intent behavior and the existing recovery handoff. Add or update tests so `test/orchestrator_core_test.gleam` proves that a same-fingerprint `AutoUnparkOnIssueChange` parked issue in `Todo` is released, `ExplicitUnparkOnly` stays parked, and a non-dispatch state stays parked. Add transition coverage in `test/orchestrator_transition_dispatch_test.gleam` if needed to prove dependency blockers and slot checks still prevent dispatch after the helper change.

Daemon acceptance requires `test/orchestrator_daemon_retry_step_test.gleam` coverage where the ledger contains an interrupted retained workflow run and an `IssueParkedV2` record with release policy `auto_unpark_on_issue_change` and the same issue fingerprint. Polling with the issue in `Todo` must reach the existing step-recovery path, append exactly one recovery request sequence such as `workflow_repair_requested` and `step_attempt_superseded`, start the recovered worker, and not emit a fresh `dispatch_started` signal before recovery. A second poll while the recovered run is active must leave the recovery record counts at one and must not start another recovered worker.

Run from the repository root and expect every command to exit zero: `direnv exec . gleam test test/orchestrator_core_test.gleam`, `direnv exec . gleam test test/orchestrator_transition_dispatch_test.gleam`, `direnv exec . gleam test test/orchestrator_daemon_retry_step_test.gleam`, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `direnv exec . python3 workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1171-smart-retry-dispatch-parked-failed-runs.md`.

Acceptance also requires a scope inventory stating that no failure-code recipe layer, `plan_completion_verdict_stale` special case, hardcoded `execplan-implementation` DAG check, recipe-only artifact SHA/corrupt JSON test, helper-script change, provider-live/cache change, browser/UI change, or durable smart-retry recipe comment/record was added. Manual live dogfood is optional after implementation; if performed, the evidence is a parked failed issue moved to `Todo`, the recovered worker or recovery ledger sequence, and no fresh full dispatch before retained recovery.

## Rollout, Recovery, and Idempotence

Roll out as an additive dispatch-policy change with no ledger rewrite, schema migration, provider cache migration, helper migration, or browser rollout. The change is safe to back out by reverting the helper and call-site edits; parked issues remain in their prior durable ledger state, and operators can use existing explicit commands such as `scherzoctl retry-step` or whole-run retry while the patch is reverted.

If the change misbehaves, pause dispatch, move affected issues out of `Todo` or apply an explicit hold, and revert the implementation. Repeated polls must be idempotent: after retained recovery has been requested or a recovered run is active, another poll for the same issue must not append duplicate `workflow_repair_requested` records or start duplicate workers. If retained recovery rejects for provenance, issue drift, workflow drift, or publication safety reasons, the issue should remain parked with the existing fail-closed diagnostics until another state/content change or explicit operator action occurs.

## Open Questions and Clarifications Needed

No open questions for Plan A. Automation for `plan_completion_verdict_stale` or any other workflow-specific stale-artifact recovery is explicitly deferred to a separate design.

## Revision Note

This revision supersedes the earlier LIV-1171 plan by removing the workflow-specific smart-retry recipe system and retaining only the generic active-state retry-intent dispatch-policy change requested by review feedback.
