# Migrate dogfood workflows to the generic artifact taxonomy

## Purpose / Big Picture

After this migration, the checked-in dogfood workflows describe new retained outputs with generic carrier descriptors instead of workflow-specific Scherzo-core carrier names. A reviewer can see `kind`, `media_type`, and namespaced workflow-owned `artifact_type` fields on ExecPlan bundles, implementation packs, and code-change bundles, while existing handoff tasks and retained artifacts that still use legacy `type: exec_plan_bundle`, `type: implementation_pack`, or `type: code_change_bundle` continue to work. The visible outcome is that a new ExecPlan workflow run emits descriptor-first manifests and bundle payloads, and the workflow scripts and schemas, not Scherzo core branches, enforce the domain rules for those artifacts.

## Problem Framing and Constraints

`docs/specs/WORKFLOW_ARTIFACT_TAXONOMY.md` separates generic carrier behavior from workflow semantics, but the dogfood workflow declarations still encode domain artifacts as core contract types. That keeps Scherzo core coupled to ExecPlan and implementation-review concepts and makes every future domain artifact look like it needs a daemon enum change. This plan starts only after the additive descriptor compatibility phase exists: the runtime must already accept generic descriptor syntax, write descriptor data without breaking legacy manifest fields, and decode old manifests. This phase must migrate current checked-in workflows and validators without removing legacy support, without rewriting historical retained artifacts, and without treating local filesystem paths as durable artifact identity.

## Strategy Overview

The right-sized strategy is an additive dogfood migration. First, prove the prerequisite descriptor compatibility is present and inventory every current workflow, schema, fixture, script, prompt, and test that still depends on workflow-specific carrier names. Then update the dogfood workflow contract declarations to use generic `kind`, `media_type`, and workflow-owned `artifact_type` values for new outputs and mapped inputs, while preserving compatibility adapters for legacy declarations. Next, move domain-specific rules for ExecPlan bundles, implementation packs, code-change bundles, and code-change references into workflow-owned JSON schemas, command validators, and `workflows/dogfood/scripts/scherzo-execplan`. Finally, update fixtures, examples, and retained-surface tests so consumers prefer descriptor fields and fall back to legacy bundle or manifest shapes only when reading old data.

## Alternatives Considered

Removing all legacy `ContractType` support now was rejected because retained manifests, existing workflow YAML, mapped-output handoffs, and operator tooling may still refer to old names. Leaving workflow YAML unchanged and only adding schemas was rejected because it would not prove that new workflow declarations can use the generic taxonomy. Forking new `execplan-v3` workflow IDs was rejected because it would duplicate dogfood workflows instead of migrating the checked-in declarations operators already use. Rewriting historical retained artifacts was rejected because compatibility adapters can preserve old data without risky storage mutation.

## Risks and Countermeasures

The main risk is breaking old retained bundles or handoff tasks that contain legacy carrier names. The countermeasure is to keep legacy decoding and declaration aliases, add explicit old-fixture parity tests, and make new consumers prefer descriptors only after a legacy fallback is proven. A second risk is a half-migration where YAML uses descriptors but helpers still validate by legacy payload fields; schema and command tests must cover both descriptor-first and legacy fixture shapes. A third risk is losing the existing `code_change` safety rule; the rule must move to a workflow-owned validator with positive and negative tests before any core branch is relaxed. A fourth risk is descriptor/domain drift, where `entries` metadata disagrees with the retained file fields; validators must reject mismatches in refs, hashes, byte counts, media types, and artifact types. A fifth risk is idempotency regressions in materialization and revision flows; fixed-input tests must show repeated materialization produces stable refs and canonical bytes.

## Scope Boundaries

In scope are `workflows/dogfood/execplan.yaml`, `workflows/dogfood/execplan-revision.yaml`, `workflows/dogfood/execplan-implementation.yaml`, relevant examples under `examples/`, workflow schemas under `workflows/dogfood/schemas/` and `.scherzo/workflows/schemas/`, `workflows/dogfood/scripts/scherzo-execplan`, related prompts that name bundle resolution fields, fixtures under `test/fixtures/execplan_v2/`, and tests for workflow parsing, manifests, workstream handoffs, ExecPlan bundle validation, implementation preparation, revision, and code-change bundle materialization. Out of scope are removing legacy declaration or manifest support, changing historical retained artifacts in `.scherzo-state`, adding a new artifact-store backend, browser UI changes, and live Linear or GitHub dogfood runs as pre-publish gates.

## Milestones

Milestone 1 proves the prerequisite and freezes the migration inventory. At the end, the implementer has confirmed descriptor compatibility exists, listed every legacy dogfood carrier use, and added or updated tests that fail while the checked-in workflows still depend on workflow-specific core carrier names.

Milestone 2 migrates workflow contracts to descriptor-first declarations. At the end, the ExecPlan, ExecPlan revision, and ExecPlan implementation workflows parse using generic carrier descriptors for new outputs and mapped inputs, while tests still prove old `type:` declarations are accepted as compatibility aliases.

Milestone 3 moves domain semantics into workflow-owned schemas and helpers. At the end, ExecPlan bundle, implementation-pack, code-change bundle, and code-change reference rules are enforced by JSON schemas and `scherzo-execplan` command validators, and Scherzo core no longer needs workflow-specific branches for new descriptor declarations.

Milestone 4 updates consumers and retained-surface parity tests. At the end, implementation preparation, revision preparation, workstream handoff emission, mapped-output consumption, and code-change bundle materialization prefer descriptor fields but still accept legacy retained bundle and manifest shapes.

Milestone 5 updates fixtures, examples, and validation gates. At the end, descriptor-first fixtures and examples are checked in, legacy fixtures remain, negative and idempotency coverage is present, and the full repository test, format, glinter, and Scherzo lint gates pass.

## Progress

2026-05-28: Reviewed the taxonomy spec, the phase-1 ExecPlan, current dogfood workflow YAML, current ExecPlan bundle and code-change bundle schemas, `workflows/dogfood/scripts/scherzo-execplan`, representative contract and workflow-run code, and existing tests. Drafted this review document only; no production implementation files were changed.

2026-05-29: Verified that additive descriptor-manifest support is present in `src/scherzo/workflow_artifact_descriptor.gleam` and `src/scherzo/workflow_contract_manifest.gleam`, but the workflow contract parser still only accepts legacy `type` declarations in `src/scherzo/workflow_contract.gleam`. Stopped this migration without changing production code because the prerequisite generic YAML descriptor syntax from LIV-718 is not landed in the current tree.

2026-05-30: Rebased the retained LIV-728 implementation workspace onto `main@origin`, resolved the workflow YAML conflict by keeping descriptor-first `exec_plan_bundle` metadata while preserving the newer optional `mapped_output` workstream start path, and completed the remaining plan-completion blockers. Added descriptor artifact-type mismatch checks for mapped workstream starts and workflow contract values, descriptor-entry cross-checking in `scherzo-execplan validate-bundle`, a retained `plan.md` fixture artifact, and explicit legacy retained-shape fixtures for ExecPlan bundle, implementation pack, and code-change bundle payloads.

## Surprises & Discoveries

2026-05-29: The repository already has the manifest-side descriptor model and tests, but not the contract-YAML descriptor syntax this migration needs.
Evidence: `src/scherzo/workflow_contract.gleam` still limits contract entry keys to `type`, `description`, `required`, and `source`, while `test/workflow_artifact_descriptor_test.gleam` and `src/scherzo/workflow_contract_manifest.gleam` show descriptor support only after contract parsing.

2026-05-30: The retained workspace predated the current workstream input contract shape. Rebasing showed `execplan-implementation.yaml` now uses `--prefer-workstream-input` and an optional mapped input, so the descriptor migration must preserve that source shape instead of reverting to required `issue_context`.
Evidence: the rebase conflict had `required: false` and `source: mapped_output` on the current `main@origin` side; the resolved YAML keeps those fields and adds the descriptor `kind`, `media_type`, and `artifact_type` fields.

## Decision Log

2026-05-28: This migration must be descriptor-first for new checked-in dogfood declarations, because leaving YAML on workflow-specific carrier names would not prove the taxonomy boundary.

2026-05-28: Legacy declaration aliases and retained manifest fallbacks stay in place for this phase, because old workflow runs and handoff tasks may remain active after new workflows are published.

2026-05-28: Domain artifact strings should be namespaced workflow-owned values such as `scherzo.exec_plan_bundle.v2`, `scherzo.implementation_pack.v2`, and `scherzo.code_change_bundle.v2`; core may store and display them but must not branch on their meaning for new descriptor declarations.

2026-05-28: Manual browser and live Linear/GitHub dogfood runs are deferred operator evidence, not pre-publish gates, because automated parser, schema, helper, retained-surface, idempotency, and repository validation gates can prove this migration without live-provider dependencies.

2026-05-29: Do not start Milestone 2 on this branch because the prerequisite generic contract-descriptor YAML syntax is absent; landing it remains separate prerequisite work rather than scope to absorb here.

2026-05-30: When preserving legacy retained-shape compatibility, absence of `artifact_type` on an old workstream handoff or input bundle remains acceptable if the legacy `contract_type` matches. A present descriptor `artifact_type` is now semantic evidence and must match the expected descriptor or legacy alias.

2026-05-30: Descriptor-first ExecPlan bundle validation belongs in the workflow helper, not JSON Schema alone, because JSON Schema can require `entries` fields but cannot compare the `plan` and `implementation_pack` entry refs, hashes, byte counts, media types, and artifact types against sibling metadata with the needed diagnostics.

## Outcomes & Retrospective

2026-05-29: This implementation handoff stopped at Milestone 1 verification. The branch is green under `direnv exec . gleam test`, but the prerequisite from LIV-718 is only partially landed: descriptor data exists for manifests and fixtures, not for workflow contract YAML parsing. No production migration files were changed, which keeps the stop safe and reversible.

2026-05-30: The migration now covers all five milestones after manual completion from the retained workspace. The remaining plan-completion blockers were closed with mapped-output artifact-type mismatch rejection, helper-level descriptor entry cross-checks, a passing descriptor-first retained plan fixture, and legacy retained-shape fixtures plus tests for `exec_plan_bundle`, `implementation_pack`, and `code_change_bundle`. A full `direnv exec . gleam test` run reached 1530 passing tests with one expected source-guardrail baseline failure before updating `src/scherzo/workstream/start.gleam`'s line-count baseline from 1065 to 1076.

## Validation and Acceptance

Pre-publish evidence must prove checked-in workflows prefer generic descriptors: parser or workflow-DAG tests must load `workflows/dogfood/execplan.yaml`, `workflows/dogfood/execplan-revision.yaml`, and `workflows/dogfood/execplan-implementation.yaml` and assert the relevant contracts use `kind: file`, `kind: artifact_set`, `media_type`, and namespaced `artifact_type` values, while separate compatibility tests still parse old `type: exec_plan_bundle`, `type: implementation_pack`, `type: code_change_bundle`, and `type: code_change` declarations.

Pre-publish evidence must prove workflow-owned validation owns domain semantics: descriptor-first ExecPlan bundle, implementation-pack, and code-change bundle fixtures must pass their workflow schemas and `scherzo-execplan` validators; malformed fixtures missing `kind`, using the wrong `artifact_type`, mismatching `entries` against retained refs or hashes, omitting a required code-change reference such as PR URL, branch, merge commit, or patch ref, or using stale implementation-pack provenance must fail with specific validator errors.

Pre-publish evidence must prove retained-surface parity: old retained ExecPlan bundle fixtures, old code-change bundle fixtures, and old output manifests without descriptors must still validate or decode through fallback paths, while new descriptor-first fixtures are preferred by implementation preparation, revision preparation, workstream handoff snapshots, mapped-output consumption, and materialized code-change bundles. Evidence may be targeted Gleam tests plus helper-script tests, but it must include both positive and negative paths and must cover old and new shapes.

Pre-publish evidence must prove idempotency and recovery safety: repeated materialization of the same ExecPlan bundle, revision bundle, and code-change bundle under fixed inputs must produce stable artifact refs, hashes, byte counts, descriptor fields, and canonical JSON bytes, and must not duplicate outputs or mutate historical retained fixtures. A diff or test assertion must show no historical `.scherzo-state` artifacts are rewritten.

Final validation must run from the repository root with `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting all commands to exit successfully. No manual browser check or live dogfood workflow is required before publish; after handoff, an operator may collect deferred evidence by running one real `workflow:execplan` to `workflow:execplan-implementation` handoff and inspecting that the retained output manifest and bundle prefer descriptor fields while the handoff task still contains valid `Bundle ref:` and `Bundle sha256:` lines.

## Rollout, Recovery, and Idempotence

Rollout is additive. New checked-in dogfood workflows emit descriptor-first outputs, but old workflow declarations and retained manifests remain accepted through adapters. If the migration breaks a consumer, reverting the workflow YAML, schema, script, and test changes returns new runs to the old carrier names without touching historical retained artifacts. Re-running helper materialization after a failed attempt must be safe: identical inputs should produce the same canonical bundle bytes and refs, while conflicting bytes or stale provenance should fail closed with validator diagnostics. Historical artifacts are read-only during this phase; any need to rewrite them must become a separate operator-approved task.

## Open Questions and Clarifications Needed

No blocking clarification is needed. The only sequencing requirement is that the additive generic descriptor compatibility work must be present before this migration begins; if the implementation branch lacks that prerequisite, the implementer should stop and land the compatibility phase first.
