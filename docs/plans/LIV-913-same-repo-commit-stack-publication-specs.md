# Revise same-repo commit stack publication specs

This ExecPlan review document is the human review surface for LIV-913. It plans a docs-only revision before LIV-908 implementation begins; mechanical edit steps, validation commands, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo needs its publication specifications to describe the correct same-repo boundary before same-repo GitHub publication is redesigned. After this revision, an implementer of LIV-908 can read `docs/specs/ARTIFACT_PUBLICATION_PRD.md` and `docs/plans/commit-stack-artifact-publication.md` and see that same-repo repository-change publication is workspace-driver-backed, uses the retained workflow workspace plus a renamed `publish-commit-stack` driver operation, and never routes through hidden managed artifact-repository clones by default.

The revision also makes ExecPlan output semantics explicit: an ExecPlan workflow produces a checked-in `commit_stack` for repository changes and separately retains the singular Markdown plan document as a file artifact for future internal review surfaces. The review feedback is incorporated by making the acceptance evidence local, deterministic, and grep-checkable, while explicitly deferring browser, live GitHub, provider-live, provider-cache, and post-implementation dogfood checks until a later implementation actually changes those surfaces.

## Problem Framing and Constraints

The current PRD frames artifact publication as moving same-repo workspace changes away from the workspace-driver `publish-change` seam. The current commit-stack plan frames retained Git bundle import as the default publication route. Those directions conflict with LIV-913's requirement: same-repo repository-change publication must preserve the workflow workspace as the publication boundary, with `commit_stack` as a first-class workflow/artifact concept but not as a mandate to reconstruct same-repo changes from retained Git bundles.

The docs must distinguish same-repo routes from external or cross-repo managed publication. Same-repo means the target repository is the repository already represented by the workflow workspace driver. External or cross-repo means Scherzo is copying artifacts or changes into a separate configured target; that may later justify managed checkouts, Git bundle import, or `.scherzo-state/artifact-repositories/github/<hash>`, but it is deferred and out of scope for LIV-908.

LIV-913 remains docs-only. It does not migrate workflow YAML, helper scripts, provider-facing structured-output helpers, provider-live behavior, provider cache behavior, or runtime driver code. If implementation discovers that one of those surfaces must change to make the docs truthful, the implementer must update this review document and the structured pack before making that broader change; otherwise the acceptance evidence must explicitly record that those surfaces were untouched and that their live/cache/manual checks are not pre-publish requirements for this docs-only correction.

## Strategy Overview

Revise the PRD into a split model. File artifact publication to external repositories can remain an artifact-repository concept, but same-repo repository-change publication must be specified as a workspace-driver capability. The docs should say that `publish-change` is renamed to `publish-commit-stack` for this route, and that doctor/preflight fails before implementation when the selected workspace driver cannot publish commit stacks.

Revise `docs/plans/commit-stack-artifact-publication.md` by de-scoping retained Git bundles and import-based publication from the same-repo default. The retained workflow workspace is authoritative for same-repo `commit_stack` publication; the `commit_stack` artifact records the workflow-level change identity, ordered commit stack, base boundary, validation metadata, and publication intent that the driver publishes from that workspace. Retained Git bundles remain optional future, recovery, or external/cross-repo carriers only.

Make validation obligations mechanical. The implementation pack must direct the implementer to edit only the two target docs for LIV-913, run the review-doc validator for this plan, collect grep or scripted evidence over both revised docs, and record whether full repository validation or linting was applicable. For a pure docs-only implementation, new unit tests, live GitHub mutation, browser checks, provider-live checks, provider-cache checks, and dogfood publication runs are not required before publish; they are deferred to LIV-908 or later runtime implementation work.

## Alternatives Considered

Leaving the PRD as-is was rejected because it would direct LIV-908 toward an artifact-repository adapter for same-repo code changes, the opposite of the required workspace-driver boundary.

Keeping retained Git bundle reconstruction as the default was rejected because it introduces hidden managed clones, bundle verification, and import semantics before same-repo publication needs them. That path is appropriate as a later recovery or external/cross-repo capability, not as the default route for a retained same-repo workflow workspace.

Adding only a small note to the existing documents was rejected because the conflict appears in the core problem statement, migration direction, and commit-stack strategy. A reviewer should not have to infer which sections are obsolete.

Requiring live GitHub, browser, provider-live, provider-cache, or dogfood publication evidence for LIV-913 was rejected because this task changes only documentation. Those checks become pre-publish requirements only for a later implementation that touches the corresponding runtime, provider, helper, or remote-publication surfaces; otherwise they are deferred human/operator checks after LIV-908 is implemented.

## Risks and Countermeasures

The main risk is leaving contradictory language in place. Counter this by replacing or explicitly narrowing statements that say artifact publication should move same-repo changes away from workspace drivers, and by adding grep-checkable statements for `publish-commit-stack`, the retained workflow workspace, and the ban on `.scherzo-state/artifact-repositories/github/<hash>` for same-repo routes.

A second risk is erasing the useful future idea of retained Git bundles. Counter this by preserving bundle/import publication as optional future, recovery, or external/cross-repo capability, while saying it is not the same-repo default and must not be a fallback for same-repo routes.

A third risk is underspecifying failed or unpublished workspaces. Counter this by requiring docs to define retry from the retained workspace, abandonment marking, retention until explicit cleanup or policy expiry, and failure-closed behavior when the retained workspace is missing, stale, or no longer publishable.

A fourth risk is producing acceptance evidence that is too subjective. Counter this with exact local commands in the implementation pack: the review-doc validator, a required-section check, grep or scripted checks over both revised docs, and a changed-file inventory that proves no helper, provider, cache, or runtime files were changed for LIV-913.

A fifth risk is accidentally expanding a docs-only correction into a helper or runtime migration without the required testing. Counter this by treating any change outside the two target docs as a scope change that requires updating the review document and pack, then adding focused tests plus `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` before publish.

## Scope Boundaries

In scope are docs revisions to `docs/specs/ARTIFACT_PUBLICATION_PRD.md` and `docs/plans/commit-stack-artifact-publication.md`; same-repo versus external/cross-repo publication boundaries; same-repo `commit_stack` workflow/artifact semantics; the required `publish-commit-stack` workspace-driver capability; doctor/preflight behavior; ExecPlan's checked-in `commit_stack` plus separately retained plan-doc artifact; and retention, retry, and abandonment semantics for unpublished same-repo workspaces.

Also in scope is acceptance evidence for the docs revision: validating this review document, proving every required section is non-empty, collecting grep or scripted evidence from both target docs, and recording a changed-file inventory. That inventory must say that workflow YAML, helper scripts under `workflows/dogfood/scripts`, workflow schemas, provider-facing structured-output helpers, provider-live behavior, and provider-cache behavior were not changed for LIV-913. If that statement would be false, the task has exceeded this review document's scope.

Out of scope for LIV-913 are Gleam runtime changes, workflow YAML migrations, actual driver command implementation, live GitHub mutation, managed external publication implementation, helper script migration, provider-live/cache behavior changes, and any attempt to invent bundle refs or PR URLs in this review document. External/cross-repo managed publication, including default use of `.scherzo-state/artifact-repositories/github/<hash>`, is deferred and out of scope for LIV-908.

## Milestones

Milestone 1 revises `docs/specs/ARTIFACT_PUBLICATION_PRD.md` so its problem statement, concepts, GitHub behavior, and migration section preserve the workspace-driver publication boundary for same-repo repository changes. The outcome is a PRD that clearly separates external file artifact publication from same-repo `commit_stack` publication, names `publish-commit-stack`, describes doctor/preflight failure when the selected driver lacks that capability, documents the ExecPlan dual-output rule, and contains explicit negative language forbidding same-repo routes from using or falling back to `.scherzo-state/artifact-repositories/github/<hash>`.

Milestone 2 revises `docs/plans/commit-stack-artifact-publication.md` so it no longer presents retained Git bundles/import publication as the same-repo default. The outcome is a de-scoped plan that defines same-repo `commit_stack` as authoritative via the retained workflow workspace, keeps retained bundle carriers as future/recovery/external capability, names `publish-commit-stack` as the selected workspace-driver operation, and states retry, unchanged retry, failure-closed, retention, and abandonment semantics for unpublished same-repo workspaces.

Milestone 3 completes acceptance evidence before publish. The outcome is a short command transcript showing the review-doc validator passing for this planning document, a section check showing every required level-2 section remains non-empty, grep or scripted checks proving that both revised docs contain the same same-repo boundary and negative managed-clone rule, and a changed-file inventory confirming that LIV-913 touched only docs. For the pure docs-only path, this milestone records that no new unit tests, helper migration, browser check, live GitHub check, provider-live check, provider-cache check, or dogfood publication run is a pre-publish requirement; those checks are deferred to LIV-908 or become required only if implementation changes the corresponding non-doc surfaces.

## Progress

- [x] (2026-06-06) Read the LIV-913 task brief and prepared output target `tmp/execplan-review-doc-target.json`, which points to `docs/plans`.
- [x] (2026-06-06) Inspected `docs/specs/ARTIFACT_PUBLICATION_PRD.md`, `docs/plans/commit-stack-artifact-publication.md`, and `docs/specs/WORKFLOW_ARTIFACT_TAXONOMY.md` for the current publication and artifact-taxonomy direction.
- [x] (2026-06-06) Created this concise review document under `docs/plans/` and prepared the structured implementation pack for Scherzo capture.
- [x] (2026-06-06) Validated this review document with `direnv exec . workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-913-same-repo-commit-stack-publication-specs.md`, which reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-06-06) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, manual/dogfood scope, docs/helper/provider inventory, conditional full validation, and linting obligations explicit in this review document and the updated implementation-pack submission.
- [x] (2026-06-06) Revalidated the updated review document and required-section inventory after incorporating review feedback.

## Decision Log

- Decision: Preserve workspace-driver publication as the same-repo boundary and rename the operation from `publish-change` to `publish-commit-stack` in the revised docs. Rationale: LIV-913 explicitly requires same-repo repository-change publication to be workspace-driver-backed for LIV-908. Date: 2026-06-06.
- Decision: Define same-repo `commit_stack` authority through the retained workflow workspace, not through retained Git bundle reconstruction. Rationale: `commit_stack` is the workflow/artifact concept, while the same-repo carrier remains the retained workspace and selected driver operation. Date: 2026-06-06.
- Decision: Keep managed clones, `.scherzo-state/artifact-repositories/github/<hash>`, and Git bundle import as deferred external/cross-repo or recovery capability. Rationale: The specs should not discard that future capability, but LIV-908 must not implement it as the same-repo default or fallback. Date: 2026-06-06.
- Decision: Require ExecPlan dual outputs in the docs. Rationale: Future internal review surfaces need the singular plan document retained separately even when the repository-visible change is published as a commit stack. Date: 2026-06-06.
- Decision: Use deterministic local docs evidence for LIV-913 and defer browser, live GitHub, provider-live, provider-cache, and dogfood publication checks. Rationale: LIV-913 does not change runtime publication behavior, so remote/manual evidence would not validate the docs-only correction; those checks belong to LIV-908 or to any scope-expanded implementation that touches the relevant surfaces. Date: 2026-06-06.
- Decision: Treat helper, workflow-schema, provider, and cache changes as out of scope unless the review document and pack are revised again. Rationale: Review feedback requires docs/helper migration, provider-live/cache behavior, full validation, and linting obligations to be explicit rather than implicit TODOs. Date: 2026-06-06.

## Validation and Acceptance

Planning acceptance for LIV-913 requires this file to exist at `docs/plans/LIV-913-same-repo-commit-stack-publication-specs.md`, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-913-same-repo-commit-stack-publication-specs.md` to exit zero with `REVIEW_DOC_VALID=ok`, every required level-2 section to be non-empty, no required implementation or validation TODO to remain as an unchecked `Progress` item, and Scherzo to capture the structured implementation-pack submission for LIV-913.

Implementation acceptance for the docs revision requires evidence from the repository root that `docs/specs/ARTIFACT_PUBLICATION_PRD.md` and `docs/plans/commit-stack-artifact-publication.md` both preserve workspace-driver publication as the same-repo boundary. A reviewer can verify this with grep or scripted checks for `same-repo`, `workspace-driver-backed`, `publish-commit-stack`, `retained workflow workspace`, `commit_stack`, `external/cross-repo`, and `singular Markdown plan` or equivalent ExecPlan dual-output wording in both files.

The same evidence must prove the negative route: same-repo publication must not use or fall back to `.scherzo-state/artifact-repositories/github/<hash>`. A reviewer can verify this by checking that both revised docs contain that literal path only in text that forbids it for same-repo routes or defers it to external/cross-repo managed publication. The evidence must also show driver preflight and recovery semantics: missing `publish-commit-stack` capability fails doctor/preflight before remote mutation, retry uses the retained workflow workspace, unchanged retries are idempotent, missing or stale retained workspaces fail closed, and unpublished workspaces remain retained until explicit abandonment or configured cleanup.

The test obligation for a pure LIV-913 implementation is documentation validation, not new Gleam unit tests. Before publish, run the review-doc validator and the grep or scripted documentation checks above, then record `git diff --name-only` evidence showing that only `docs/specs/ARTIFACT_PUBLICATION_PRD.md`, `docs/plans/commit-stack-artifact-publication.md`, and this review document changed. If any `src/`, `test/`, `workflows/dogfood/scripts`, `workflows/dogfood/schemas`, `.scherzo/workflows`, or provider-facing helper file changes, the implementer must revise this review document and pack, add focused tests for the changed behavior, and run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` before publish. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

No browser, live GitHub, provider-live, provider-cache, or dogfood publication evidence is required before publishing this docs-only revision. A later LIV-908 implementation should dogfood same-repo publication from a retained workflow workspace before rollout; that dogfood is a deferred human/operator check unless LIV-913's scope expands into runtime or provider/cache behavior.

## Rollout, Recovery, and Idempotence

Rollout is a docs-only correction before LIV-908 implementation. The safe rollout is to merge the revised PRD and plan first, then implement LIV-908 against those clarified boundaries. If the docs change causes confusion, recovery is to revert only the docs commit or amend the relevant wording; no persisted daemon state, remote branch, provider cache, browser state, or workflow output needs migration for LIV-913 itself.

The later same-repo implementation should be idempotent by design: the same retained workflow workspace, base boundary, selected `commit_stack`, branch policy, and `publish-commit-stack` driver operation should produce the same publication result or `unchanged` status on retry. Abandonment must be explicit and auditable so Scherzo does not silently delete unpublished workspaces that are still the authoritative same-repo commit-stack carrier.

Manual and dogfood rollout checks are deferred to LIV-908 after runtime support exists. At that point, an operator should run a same-repo publication scenario with fake/local evidence first, then an explicitly authorized live GitHub staging check if provider/helper changes require it, keeping credentials out of logs. LIV-913 itself remains reversible by editing or reverting documentation only.

## Open Questions and Clarifications Needed

No blocking open questions. The revised docs should use `publish-commit-stack` as the target operation name and may mention `publish-change` only as the legacy name being replaced or migrated away from. If a later implementation decides that helper scripts, provider-live behavior, provider-cache behavior, or live dogfood must change before LIV-908, that is a new scope decision and should be captured in a follow-up plan or a revised review document before code changes begin.
