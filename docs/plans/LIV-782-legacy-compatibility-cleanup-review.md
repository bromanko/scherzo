# LIV-782 Legacy Compatibility and Duplicate Surface Cleanup

## Purpose / Big Picture

This plan turns the cleanup audit into a safe removal program for compatibility shims, duplicate old/new implementation paths, and stale review/workstream surfaces. The visible outcome is a smaller, clearer Scherzo tree where each remaining legacy surface has a written reason, a retirement gate, or a dedicated follow-up task instead of surviving by accident.

## Problem Framing and Constraints

The problem is not ordinary simplification; it is implementation debt that makes operators and future adapter authors unsure which surfaces are live, compatibility-only, or obsolete. Cleanup must preserve old run readability, daemon recovery, Linear dogfood safety, and the native review workflow. Production Gleam code must keep passing the repository lint policy, and durable-state readers must not be removed until an operator policy says whether old ledgers remain readable, are migrated, or require an archive/reinitialize flow.

Review feedback adds one more constraint: evidence obligations must not live only in prose. Acceptance evidence, test obligations, milestone-specific proof, manual or dogfood timing, docs/helper migration boundaries, provider-live and cache non-scope, full validation, and lint gates must be mirrored in the structured implementation pack that Scherzo materializes for the follow-up implementation.

## Strategy Overview

Proceed in small, evidence-backed slices. First publish an inventory that classifies every named candidate as removed, retained with reason, or split to follow-up. Then remove test-only production facades one at a time, migrate daemon/workflow execution toward task-native tracker adapter APIs before deleting bridges, reconcile the LIV-573 alias gates, isolate historical review helper backends from native review, and classify workstream/playbook code against the current roadmap. Durable-state compatibility remains policy-first: no reader deletion until migration and recovery consequences are explicit.

## Alternatives Considered

Deleting every legacy-named file immediately would make the diff look clean but risks breaking tests, recovery, old ledgers, and operator scripts. Leaving all compatibility code in place with comments would be safe but would not reduce duplicate surfaces. A single giant tracker/task rewrite was also rejected because it would mix public CLI behavior, durable schema policy, review helper cleanup, and daemon runtime migration in one hard-to-review change.

## Risks and Countermeasures

The main risk is breaking live dogfood dispatch or recovery while removing code that only appears stale. Countermeasure: remove one surface per validated slice and require import greps plus full tests. A second risk is corrupting retained-state readability. Countermeasure: inventory `legacy_runs`, `linear_command_*`, `issue_id`, and `issue_identifier` readers and keep them until an approved policy or migration exists. A third risk is confusing operators by retiring aliases or review commands without a replacement. Countermeasure: either keep aliases with documented gates or remove them with parser/docs tests, and move legacy review helpers behind an explicit historical-test boundary while preserving native `kind: agent` review lanes.

A fourth risk is accidentally expanding cleanup into helper rewrites, live provider behavior, or cache semantics that are harder to validate than deleting stale code. Countermeasure: require a docs/helper inventory, split unrelated helper migrations, state when live/provider dogfood is deferred to a human operator, and treat provider-live and preflight cache behavior as unchanged unless a separate follow-up adds stale-read, invalidation, TTL, and live-provider tests.

## Scope Boundaries

In scope are the candidate facades `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/state/workflow_checkpoint.gleam`, `src/scherzo/config/ui_server.gleam`, and `src/scherzo/tracker/conformance/linear_driver.gleam`; the tracker/task bridge around `src/scherzo/task.gleam`, `src/scherzo/tracker/adapter.gleam`, `src/scherzo/tracker.gleam`, `src/scherzo/tracker/issue.gleam`, and `src/scherzo/tracker/adapter_legacy.gleam`; Linear smoke/contract aliases; legacy `.scherzo/workflows/scripts/scherzo-review` lane backends; durable-state compatibility readers; and workstream/playbook classification.

Docs/helper migration is limited to artifacts directly affected by those removals, such as parser usage text, runbooks that mention aliases, workflow YAML that still calls legacy review lanes, and tests that prove native review remains intact. If the implementation touches `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, or provider-live preflight helpers, the affected helper tests must run and the inventory must explain why the helper edit is part of cleanup rather than an unrelated migration. Provider-live behavior and cache semantics are out of scope and should be recorded as unchanged; if the cleanup reveals that either must change, split that work to a follow-up with live-provider, stale-read, invalidation, and TTL-disabling tests.

Out of scope are adding a new tracker adapter, redesigning Linear GraphQL internals, deleting old ledger compatibility before policy approval, replacing the native staged-review workflow, introducing provider-live behavior, or changing cache behavior.

## Milestones

Milestone 1 produces the checked cleanup inventory and durable-state policy. It is complete when the repository contains a greppable inventory document with each candidate surface classified, durable-state compatibility consequences stated, and any unsafe removals split to explicit follow-up.

Milestone 2 removes safe test-only production facades. It is complete when each removed module has no production or test imports, tests use the real production module boundaries directly, and the candidate inventory records the before/after evidence.

Milestone 3 advances the task-native tracker migration. It is complete when daemon and workflow execution fetch, refresh, handoff, and command paths consume `task.Task` or `task.TaskRef` from `tracker/adapter.gleam` without `adapter_legacy` in production runtime paths, or when the remaining bridge is isolated with a named follow-up and reason.

Milestone 4 reconciles public compatibility aliases and review helper backends. It is complete when LIV-573 alias gates are either still documented as unmet or parser/docs tests prove removal, and when `scherzo-review run-lane`, `preflight`, and heuristic/fixture/external lane backends are removed from production helper entrypoints or moved to an explicit historical-test-only path. Reviewers should also see a docs/helper inventory naming every changed usage document, workflow YAML, helper script, provider-facing structured-output contract, and review-lane contract test, or explicitly stating that none changed.

Milestone 5 classifies workstream and playbook surfaces. It is complete when `src/scherzo/workstream/*`, `.scherzo/playbooks/*.yaml`, and v1/v2 artifact references are marked active, planned, or stale, with stale items removed or split to follow-up.

Milestone 6 validates the cleanup as an operator-safe change. It is complete when format, tests, production lints, and any changed helper contract suites pass; when acceptance evidence records import-grep output, inventory classifications, alias/review-helper decisions, durable-state policy, and workstream/playbook classification; when deterministic local dogfood or helper contract checks required by changed workflow helpers complete before publish; and when live Linear/provider dogfood is either not applicable or explicitly deferred to a human/operator after implementation with redacted evidence requirements. The validation note must also state that provider-live and cache behavior were unchanged, or name the follow-up that owns those tests.

## Progress

As of 2026-05-31T22:42Z, this review document and its paired implementation pack were drafted after inspecting the current candidate modules, tracker/task bridge call sites, LIV-573 review plan, native review workflow, durable-state readers, and workstream/playbook surfaces. No source cleanup has been applied in this execplan authoring run.

As of 2026-05-31T22:59Z, review feedback was incorporated by making acceptance evidence, test obligations, milestone specificity, docs/helper migration boundaries, manual/dogfood timing, provider-live/cache non-scope, full validation, and lint gates explicit in this review document and the updated structured implementation-pack submission. There are no unchecked Progress TODOs in this review document; the required implementation and validation work belongs to the follow-up implementation pack steps.

## Decision Log

Decision: Treat durable-state compatibility as policy-gated, not a code-delete task. Rationale: old ledgers and recovery flows use `legacy_runs`, `linear_command_*`, `issue_id`, `issue_identifier`, and optional task-ref fields, so deleting readers without migration would create operator recovery ambiguity. Date: 2026-05-31.

Decision: Prefer deleting true facades before changing behavior. Rationale: current exact import checks show the named facade modules are test-only, while tracker/task migration and review helper retirement affect production paths and require separate validation. Date: 2026-05-31.

Decision: Preserve native review commands while isolating legacy lane backends. Rationale: implementation workflows already use native Scherzo agent lanes with `submit_review_lane_draft`, but the same script still hosts native artifact commands that remain production workflow helpers. Date: 2026-05-31.

Decision: Reuse LIV-573's gate model for Linear aliases. Rationale: LIV-573 intentionally retained `linear-smoke` and `linear-contract` compatibility until tracker-neutral context and operator compatibility gates are satisfied; this cleanup should either prove those gates now pass or leave an explicit follow-up. Date: 2026-05-31.

Decision: Treat review feedback about acceptance evidence, tests, milestone specificity, manual/dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations. Rationale: Scherzo materializes follow-up implementation instructions from the structured pack, so prose-only obligations would be easy for later implementers to miss. Date: 2026-05-31.

## Validation and Acceptance

Acceptance requires concrete evidence for every outcome. The inventory is accepted only if the checked document lists each candidate as removed, retained with reason, or split to follow-up, and includes grep evidence for current imports. Removed modules are accepted only when `rg` finds no remaining imports and `direnv exec . gleam test` passes. Alias behavior is accepted only when parser, usage, doctor, and docs tests show either retained aliases with gates or removed aliases with replacement diagnostics.

Review helper cleanup is accepted only when native review workflow tests still prove `kind: agent` lanes and `submit_review_lane_draft` structured output, and any legacy helper path is absent from production workflow YAML or explicitly historical. Durable-state policy is accepted only with tests or documented manual evidence showing old ledgers remain readable, are migrated, or are intentionally archived. Workstream classification is accepted only with an inventory entry for each workstream module, playbook YAML file, and artifact version family.

Before publish, run `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If workflow helper scripts, review-lane contracts, workflow YAML, provider-facing structured-output contracts, or provider-live preflight helpers change, also run the relevant helper or offline contract suite such as `direnv exec . scherzo-test-contract`, preserve native review workflow tests, and retain the command output. If no helper contracts changed, the final evidence must explicitly say so.

Manual and dogfood checks are split by risk. Deterministic local checks that exercise changed helper commands or workflow YAML are pre-publish requirements. Live Linear/provider dogfood is deferred to a human/operator after implementation unless the implementation intentionally changes live dispatch, provider-live probes, or provider-facing cache behavior; in that case it becomes pre-publish and must include redacted evidence. Provider-live and cache behavior are otherwise accepted as unchanged only when the implementation records that no provider-live code, cache TTL, invalidation, stale-read behavior, or preflight cache path changed.

## Rollout, Recovery, and Idempotence

Roll out as a sequence of small green commits: inventory, facade removals, task migration slice, alias/review helper slice, workstream classification, and final validation. Each slice is idempotent because import greps, inventory updates, and validation commands can be rerun without mutating external state. Recovery for a bad removal is to restore the last deleted facade or alias parser arm and keep the inventory entry that explains why the gate failed. Recovery for durable-state uncertainty is to stop before deleting readers and convert the remaining work into a migration or archive-policy follow-up.

If helper, provider-live, or cache behavior appears to be required for cleanup, stop and split that work unless it is directly necessary to preserve the native review workflow. Recovery for accidental helper expansion is to revert the helper edit, keep the cleanup inventory, and file a focused migration with contract tests. Recovery for accidental provider-live or cache behavior changes is to revert to the previous behavior before publish, or block publish until stale-read, invalidation, TTL-disabling, and live-provider evidence exists.

## Open Questions and Clarifications Needed

The implementation task should confirm whether operators approve actual Linear alias removal after the task-native migration, or whether aliases remain until a compatibility window is announced. It should also confirm whether stale workstream/playbook pieces are roadmap placeholders or should become explicit follow-up tasks rather than being deleted.
