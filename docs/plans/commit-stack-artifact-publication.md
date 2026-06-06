# First-class commit stack artifact publication

This review document now describes the corrected same-repo default for `commit_stack` publication.

## Purpose / Big Picture

Scherzo implementation workflows need to publish reviewed same-repo code changes from the retained workflow workspace that already represents the target repository. After this revision, a reader should understand that same-repo `commit_stack` publication is workspace-driver-backed, uses `publish-commit-stack`, and keeps the retained workflow workspace as the authoritative carrier for retry, `unchanged`, and failure recovery.

The document also preserves room for future external/cross-repo publication. Managed checkouts, retained bundle import, and `.scherzo-state/artifact-repositories/github/<hash>` can still exist later as external/cross-repo or recovery tools, but they are not the same-repo default or fallback.

## Problem Framing and Constraints

The earlier direction treated retained Git bundle import as the primary publication path and said publication must not require the original workspace. That conflicts with the required same-repo boundary. For same-repo repository changes, the retained workflow workspace already captures the normalized repository state, selected workspace driver, and local publication context that runtime implementation needs.

This document is intentionally about specification boundaries, not runtime code. It must clearly separate:

- same-repo publication, which is workspace-driver-backed and publishes from the retained workflow workspace; and
- external/cross-repo publication, which may later justify managed clones, retained bundle carriers, or `.scherzo-state/artifact-repositories/github/<hash>`.

## Strategy Overview

Make `commit_stack` the first-class workflow output for repository changes, but keep same-repo publication anchored to the retained workflow workspace. The driver operation for this path is `publish-commit-stack`.

The retained `commit_stack` still matters. It records workflow-level change identity, ordered commits, base boundary, validation metadata, and publication intent. But for same-repo publication it does not replace the retained workflow workspace as the authoritative carrier. ExecPlan also keeps the singular Markdown plan document as a separately retained file artifact/internal review output when repository changes are published as a checked-in `commit_stack`.

For same-repo publication, doctor/preflight must fail before remote mutation when the selected workspace driver cannot publish commit stacks. Because this docs-only change precedes runtime migration, `publish-commit-stack` is the target operation name while existing runtime/workspace-driver contracts may still expose `publish-change` until LIV-908 implements an alias or replacement and updates those contracts together.

Retained Git bundles can remain in the long-term design as optional recovery or external/cross-repo carriers. They must not be described as the same-repo default path.

## Alternatives Considered

Using retained Git bundle import as the same-repo default was rejected because it introduces hidden managed clones and import semantics before same-repo publication needs them.

Moving same-repo publication fully into artifact-repository adapters was rejected because it would move publication away from the workspace-driver boundary that already owns repository normalization and same-repo safety checks.

Leaving the older wording in place was rejected because the core problem statement and milestone story would still point implementers toward the wrong default.

## Risks and Countermeasures

The main risk is leaving mixed signals about the publication boundary. Counter this by repeating the exact same-repo rule in the purpose, strategy, milestones, validation, and rollout sections: same-repo publication is workspace-driver-backed, uses `publish-commit-stack`, and publishes from the retained workflow workspace.

A second risk is erasing future recovery or external/cross-repo ideas. Counter this by preserving retained bundles and managed artifact repositories as deferred capabilities while explicitly stating they are not the same-repo default.

A third risk is underspecifying failure handling. Counter this by requiring same-repo publication to fail closed when the retained workflow workspace is missing, stale, no longer publishable, or no longer matches the selected `commit_stack` identity, while keeping unpublished workspaces retained until explicit abandonment or configured cleanup.

## Scope Boundaries

In scope are the same-repo default, the `publish-commit-stack` target operation name, the retained workflow workspace as the authoritative same-repo carrier, current `publish-change` compatibility vocabulary until runtime migration, `commit_stack` workflow/artifact semantics, ExecPlan's separately retained singular Markdown plan artifact, retry and `unchanged` rules, failure-closed behavior, and explicit abandonment/retention semantics.

Also in scope is the future external/cross-repo boundary: retained Git bundles, managed artifact repositories, and `.scherzo-state/artifact-repositories/github/<hash>` remain deferred capabilities only.

Out of scope are runtime code changes, helper migrations, provider-live behavior changes, provider-cache behavior changes, workflow YAML migrations, and live GitHub behavior changes.

## Milestones

Milestone 1 revises the publication boundary. Reviewers should see that same-repo publication is workspace-driver-backed, uses `publish-commit-stack` as the target operation, documents current `publish-change` compatibility until runtime migration, and publishes from the retained workflow workspace.

Milestone 2 revises `commit_stack` semantics. Reviewers should see that `commit_stack` remains the first-class workflow/artifact concept for repository changes, but not a mandate to reconstruct same-repo publication from retained bundle import.

Milestone 3 revises recovery semantics. Reviewers should see retry from the retained workflow workspace, idempotent `unchanged` behavior, failure-closed handling when the retained workflow workspace is missing, stale, or does not match the selected `commit_stack`, and retention until explicit abandonment or configured cleanup.

Milestone 4 preserves future optional carriers. Reviewers should see retained Git bundles, managed artifact repositories, and `.scherzo-state/artifact-repositories/github/<hash>` described only as external/cross-repo or recovery ideas.

## Progress

- [x] (2026-06-04) Drafted the original retained-commit-stack publication plan.
- [x] (2026-06-06) Revised the same-repo specification so publication is workspace-driver-backed and uses the retained workflow workspace plus `publish-commit-stack`.
- [x] (2026-06-06) De-scoped retained Git bundle import and managed artifact repositories from the same-repo default path.
- [ ] Runtime implementation and workflow/helper/provider migration remain future work.

## Decision Log

- Decision: same-repo publication is workspace-driver-backed. Rationale: the selected workspace driver already owns repository identity, normalization, and publication safety for the repository represented by the retained workflow workspace. Date: 2026-06-06.
- Decision: use `publish-commit-stack` as the same-repo publication operation name. Rationale: the docs need a precise operation name for doctor/preflight and runtime implementation. Date: 2026-06-06.
- Decision: keep the retained workflow workspace authoritative for same-repo publication. Rationale: same-repo retry and publication safety depend on the exact retained workspace context, not a reconstructed hidden clone by default. Date: 2026-06-06.
- Decision: defer retained Git bundle import and `.scherzo-state/artifact-repositories/github/<hash>` to external/cross-repo or recovery scope. Rationale: those ideas remain useful later but must not redefine the same-repo default. Date: 2026-06-06.

## Validation and Acceptance

This document is correct when a reviewer can grep it for `same-repo`, `workspace-driver-backed`, `publish-commit-stack`, `retained workflow workspace`, `commit_stack`, `external/cross-repo`, and `.scherzo-state/artifact-repositories/github/<hash>` and find language that matches the same revised boundary used in `docs/specs/ARTIFACT_PUBLICATION_PRD.md`.

A reviewer must also be able to verify the negative rule: `.scherzo-state/artifact-repositories/github/<hash>` and retained Git bundle import are not the same-repo default or fallback. They are only future external/cross-repo or recovery capabilities.

A reviewer must also be able to verify ExecPlan dual-output semantics: repository changes are represented by a checked-in `commit_stack`, while the singular Markdown plan remains a separately retained file artifact for internal review surfaces.

A reviewer must also be able to verify preflight and recovery semantics: a selected driver that lacks `publish-commit-stack` or a documented migration-compatible `publish-change` alias with the same semantics fails before remote mutation; retry uses the retained workflow workspace; unchanged retry is idempotent; the workspace must exactly match the selected `commit_stack` repository identity, base ref/commit, ordered commits, head commit/tree, and validation metadata; missing, stale, drifted, or policy-unsafe retained workspaces fail closed; and unpublished workspaces remain retained until explicit abandonment or configured cleanup.

## Rollout, Recovery, and Idempotence

Rollout is docs-first. Runtime implementation should follow this corrected boundary instead of the earlier import-first direction.

Recovery for same-repo publication uses the retained workflow workspace. If the retained workflow workspace is missing, stale, drifted away from the selected `commit_stack`, or no longer publishable under branch/ref policy, runtime behavior must fail closed instead of silently switching to a hidden managed clone. If the publication is intentionally given up, abandonment must be explicit and auditable.

Idempotence means the same retained workflow workspace, base boundary, selected `commit_stack`, verified head commit/tree, and `publish-commit-stack` invocation produce either the same publication result or `unchanged`.

## Open Questions and Clarifications Needed

No blocking open questions for the docs revision. Runtime implementation still needs to decide whether future external/cross-repo recovery uses retained Git bundles, managed artifact repositories, or both, but that choice is intentionally deferred and does not change the same-repo default.