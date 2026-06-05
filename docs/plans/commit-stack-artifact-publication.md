# First-class commit stack artifact publication

This ExecPlan v2 review document is the human review surface for LIV-871. It plans a later implementation of retained Git commit-stack publication; concrete edit steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo implementation workflows need to publish reviewed code changes from retained VCS objects, not from an ephemeral workflow workspace. After this change, a workflow can expose a first-class `commit_stack` output, retain a manifest plus a Git object carrier, and later publish or retry a GitHub PR from those retained artifacts while preserving the exact commit history, including multiple commits.

Operators should be able to delete or ignore the original step workspace, retry publication, and observe the same stable branch and PR being updated or reported `unchanged` from the retained stack alone.

## Problem Framing and Constraints

Current artifact publication is file-oriented: `artifacts.publications[].files[]` selects retained file bytes, the planner builds a file publication manifest, and the GitHub adapter materializes those bytes into a managed checkout before committing them. That works for review documents, but implementation workflows currently still rely on `workspace-driver publish-change`, which publishes a live workspace diff and can collapse, rebase, or otherwise reinterpret the commits produced during implementation.

The target is not a patch-only MVP. The first-class concept is `commit_stack`, not a generic `artifact_set` and not a `code_change_bundle` alias. `code_change_bundle` may remain as a reporting envelope for validation, review, and PR metadata, but it must not be the primary publication input. Publication must import and push retained Git objects, must not regenerate the change from a diff, must not require the original workspace, and must not silently rebase or rewrite the retained stack. Any base refresh or rebase happens before capture and must be validated before the commit stack is retained.

## Strategy Overview

Add `commit_stack` to Scherzo's workflow contract and artifact descriptor taxonomy, then route it through artifact publication with a new `mode: commit_stack`. The existing GitHub repository target shape remains reusable:

    artifacts:
      repositories:
        github:
          code:
            repo: living-systems/scherzo
            base: main
            checkout:
              strategy: managed_git
            branch:
              strategy: stable_per_work
              template: scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}
            pull_request:
              enabled: true
              strategy: update_existing
              draft: true

A future implementation workflow should expose a required commit-stack output whose canonical value is the JSON manifest:

    contract:
      version: 1
      outputs:
        commit_stack:
          kind: commit_stack
          media_type: application/vnd.scherzo.git-commit-stack+json
          artifact_type: scherzo.git_commit_stack.v1
          required: true
          source:
            step: export_commit_stack
            commit_stack: default

Publication selects that output directly rather than selecting files:

    artifacts:
      publications:
        - id: implementation_pr
          repository: github.code
          required: true
          mode: commit_stack
          commit_stack:
            select:
              output: commit_stack
          pull_request:
            title: "{{ work.identifier }}: {{ issue.title }}"
            body_template: prompts/implementation-pr-body.md

The retained manifest uses `application/vnd.scherzo.git-commit-stack+json` and records repository identity, base ref, base commit, head commit, head tree, ordered commits, parent relationships, author and committer metadata, subjects, full message hashes or bodies, changed files including modes and rename pairs, and an object carrier with `media_type: application/vnd.git.bundle`, ref, sha256, byte count, and bundle verification metadata. The Git bundle is the canonical object payload because it preserves commits, trees, blobs, binary files, file modes, symlinks, and exact tree reconstruction.

The workspace driver gets an export-only capability, for example `export-commit-stack`, that writes the manifest and bundle into the run artifact area and returns retained refs. The publication executor reads those refs, verifies the bundle and manifest agree, imports the bundle into a managed checkout or bare import repository, checks that the retained base boundary is safe, updates the stable branch to the retained head without rebasing, pushes with lease protection, and creates or updates the matching GitHub PR.

## Alternatives Considered

A patch-only publication path was rejected because it cannot preserve multi-commit history, author metadata, merge-sensitive parent relationships, binary modes, symlinks, or the exact tree without relying on reconstruction.

Using `artifact_set` or `code_change_bundle` as the primary publication input was rejected because those names describe generic grouping or reporting envelopes. `commit_stack` names the VCS object being published and gives planners, validators, and operators a precise policy boundary.

Continuing to call `workspace-driver publish-change` was rejected because it depends on a live workspace and performs publication as part of workspace-driver behavior. Commit-stack publication must be a Scherzo artifact-publication route that can retry from retained artifacts.

Rebasing during publication was rejected. If a base refresh is needed, the implementation workflow must refresh, revalidate, and then capture a new commit stack. Publication only imports and pushes what was retained.

## Risks and Countermeasures

A corrupted or mismatched bundle could publish code that differs from the manifest. Counter this with sha256 and byte verification, `git bundle verify`, checks that the imported head commit and head tree equal the manifest, and tests that deliberately mismatch bundle bytes, head trees, commit lists, and parent links.

A stale or unsafe base could make a retained stack misleading. Counter this by recording both base ref and base commit, allowing publication when the current remote base still contains the retained base commit, and failing closed when the configured base no longer contains that commit. Publication must report base drift instead of rebasing.

A retry could duplicate commits, force-push over unrelated work, or create PR spam. Counter this with stable branch templates, deterministic `version_id` values based on retained stack identity, remote lease checks, exact branch-head comparison, `update_existing` PR lookup rules, and ledger records for `published`, `unchanged`, and retryable `failed` attempts.

A Git object carrier can be hazardous if treated as a trusted workspace. Counter this by importing in Scherzo-managed checkouts or bare repositories, never executing hooks or workspace files during verification, validating ref names before push, and limiting remote mutation to the configured repository and stable branch.

## Scope Boundaries

In scope are the `commit_stack` contract and descriptor kind, the retained manifest schema, Git bundle retention, workspace-driver export without publication, `mode: commit_stack` publication planning and execution, GitHub managed-checkout import and push behavior, publication ledger and retry semantics, operator inspection, and migration guidance for `implementation` and `execplan-implementation` after support is proven.

Docs and helper migration are also in scope for the later implementation. The implementer must inventory and update the workflow YAML, `workflows/dogfood/scripts/scherzo-implementation`, bundled workspace drivers such as `scripts/scherzo-workspace-jj`, artifact-publication operator docs such as `docs/runbooks/artifact-publication.md`, and any schema examples affected by the new route. If provider-facing structured-output helper files, provider-live behavior, or provider cache/token-accounting behavior are not touched, the implementation must say so explicitly in its evidence; if they are touched, the matching helper smoke tests and cache/live validation must become pre-publish validation rather than deferred operator checks.

Out of scope are patch-only publication, Linear as an artifact repository backend, removing legacy `publish-change` before this path is implemented and dogfooded, live GitHub credentials in normal unit tests, generalized non-Git object carriers, and broad workflow migration before deterministic fakes prove export, import, push, PR, retry, and idempotency behavior.

## Milestones

Milestone 1 establishes the artifact taxonomy and manifest contract. Reviewers should see `commit_stack` parse as a first-class contract and descriptor kind, schema examples validate, retained manifests reject missing or inconsistent repository, base, head, tree, commit, changed-file, and carrier metadata, and `code_change_bundle` remain only as compatibility/reporting state.

Milestone 2 proves export without publication. A workspace driver can capture a validated commit stack into retained artifacts, including a manifest and Git bundle, without pushing or creating a PR. The export must happen after base normalization and validation, and tests should prove multi-commit stacks, binary files, executable modes, symlinks, and changed-file metadata survive capture.

Milestone 3 proves commit-stack publication planning. `artifacts.publications[].mode: commit_stack` selects exactly one commit-stack output, rejects file selectors in commit-stack mode, produces a retained publication manifest that links the stack manifest and carrier refs, and computes stable series and version ids without reading a step workspace.

Milestone 4 proves GitHub import and PR mutation. The adapter imports the bundle, verifies the retained graph, updates the stable branch to the retained head without rebasing, pushes with lease protection, creates or updates the configured draft PR, records status and remote refs, and handles malformed bundles, wrong repositories, base drift, push failures, and ambiguous PR lookup deterministically with fakes.

Milestone 5 proves retry, operator, and migration readiness. `scherzoctl artifact publication retry` can replay a failed or successful commit-stack publication from retained artifacts after the original workspace is gone; unchanged retries record `unchanged`; failed publications are retryable without rerunning agents; and `implementation` plus `execplan-implementation` have a documented migration path that keeps `code_change_bundle` as reporting until the new publication route is dogfooded. This milestone also produces a docs/helper/provider inventory: it documents which workflow YAML files, `workflows/dogfood/scripts/scherzo-implementation` helper commands, bundled workspace-driver commands, schemas, and operator docs were migrated, and it explicitly records provider-live/cache as unchanged unless implementation facts prove that extra live/cache validation is required.

## Progress

- [x] (2026-06-04) Reviewed the LIV-871 task brief and required output target.
- [x] (2026-06-04) Inspected current workflow contract parsing, artifact descriptors, contract-output manifests, file-oriented artifact publication config, planner, executor, GitHub adapter, publication retry command, `code_change_bundle` materialization, schemas, and dogfood implementation workflow YAML.
- [x] (2026-06-04) Drafted this human-reviewable review document under `docs/plans/` and prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-06-04) Incorporated review feedback by making docs/helper migration, provider-live/cache inventory, full validation, linting, deterministic manual evidence, and deferred live dogfood expectations explicit in the review document and structured implementation pack.

## Decision Log

- Decision: Name the artifact kind and publication concept `commit_stack`. Rationale: The deliverable is a retained Git commit graph and object payload, not a diff, file set, or reporting bundle. Date: 2026-06-04.
- Decision: Use a semantic manifest media type plus a Git-specific object payload media type. Rationale: `application/vnd.scherzo.git-commit-stack+json` identifies the Scherzo manifest contract, while `application/vnd.git.bundle` identifies the carrier that Git can verify and import. Date: 2026-06-04.
- Decision: Reuse the existing GitHub repository target config and put the distinction on publication routes with `mode: commit_stack`. Rationale: Repository identity, base branch, managed checkout, stable branch, and PR defaults apply to both file and commit-stack publication. Date: 2026-06-04.
- Decision: Forbid publication-time rebase. Rationale: Rebase changes the retained stack and invalidates the validation evidence associated with it; refresh must happen before capture and be validated before retention. Date: 2026-06-04.
- Decision: Require deterministic fake Git and fake GitHub evidence before live dogfood. Rationale: Export, import, retry, idempotency, and failure modes can be proven without credentials or network access. Date: 2026-06-04.
- Decision: Require a docs/helper/provider inventory in the implementation evidence. Rationale: The change necessarily crosses workflow YAML, helper scripts, bundled workspace-driver commands, schemas, docs, and publication retry behavior, while provider-live/cache behavior should remain unchanged unless implementation touches provider-facing helper code. Date: 2026-06-04.

## Validation and Acceptance

This planning issue is accepted when `docs/plans/commit-stack-artifact-publication.md` exists, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/commit-stack-artifact-publication.md` exits zero and prints `REVIEW_DOC_VALID=ok`, every required review-doc section is non-empty, and Scherzo captures the structured implementation-pack submission for LIV-871.

The later implementation is accepted only with automated evidence that workflow YAML accepts a `commit_stack` contract output and a `mode: commit_stack` publication route while the existing `artifacts.repositories.github` target shape remains valid for file and commit-stack routes. Tests must prove publication reads the retained manifest and Git bundle, not a workspace diff, and that retry still works after the original workspace directory is removed.

Git behavior evidence must include a multi-commit stack whose imported `git rev-list --reverse <base>..<head>` order equals the manifest, whose commit parent lists and head tree match the manifest, and whose authors, committers, subjects, messages, binary blobs, symlinks, executable modes, and rename metadata are preserved. Negative evidence must cover corrupt bundle bytes, manifest/bundle hash mismatch, head tree mismatch, parent mismatch, wrong repository, missing carrier ref, base ref whose current remote history no longer contains the retained base commit, push failure, ambiguous PR lookup, and PR edit/create failure.

Retry and idempotency evidence must show an unchanged retry recording `unchanged` with no new commit, no duplicate PR, and no agent rerun; a failed publication becoming successful when retried from retained artifacts; a repeated successful stack reusing the same stable branch and PR; and a changed retained stack updating the same branch to the new retained head without rebasing.

Docs/helper migration acceptance requires evidence that migrated workflow YAML, `workflows/dogfood/scripts/scherzo-implementation`, bundled workspace-driver commands, schema examples, and `docs/runbooks/artifact-publication.md` agree on the same `commit_stack` contract, `export_commit_stack` step, and `mode: commit_stack` publication route. If provider-facing structured-output helpers, provider-live behavior, or provider cache/token-accounting behavior remain unchanged, acceptance evidence must include that inventory statement; if any of those surfaces change, focused helper smoke tests and provider-live/cache validation become required pre-publish checks.

Full validation must run from the repository root and pass: focused commit-stack tests, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and the review-doc validation command above. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Manual evidence before publish is local and deterministic: collect fake-command or seeded-state `scherzoctl artifact publication list`, `show`, and `retry --run <run-id> --publication implementation_pr --json` transcripts showing `published`, `unchanged`, and retryable `failed` commit-stack states. Live GitHub PR evidence, browser evidence, provider-live checks, and provider-cache checks are deferred to a human/operator after implementation only when the implementation inventory proves provider-facing helpers and cache behavior were not changed; otherwise those checks are pre-publish requirements against an explicitly chosen staging repository with credentials kept out of logs.

## Rollout, Recovery, and Idempotence

Rollout is additive. Existing file publication routes and legacy `publish-change` workflows continue to work until commit-stack publication is implemented, tested, and migrated deliberately. New commit-stack publication is opt-in through `mode: commit_stack` and a `commit_stack` contract output. Helper and docs migration happens in two phases: first add export/import support and operator documentation while leaving current implementation workflows on `publish-change`; only after fake retry/idempotency evidence passes should `implementation` and `execplan-implementation` dogfood the retained `commit_stack` route.

Recovery from local publication failures is to inspect the retained stack and publication manifest, repair or delete the affected managed checkout if necessary, and rerun `scherzoctl artifact publication retry --run <run-id> --publication <publication-id> --json`. Recovery from remote failures is the same retained-artifact retry path; if the remote branch already points at the retained head, the retry records `unchanged` rather than pushing a duplicate change.

If the rollout must be backed out before migration, revert the new schema, parser, executor, adapter, driver, and workflow changes. If remote branches or PRs were created during dogfood, leave them as audit evidence or close/delete them manually. Idempotence is required: the same retained manifest, carrier hash, repository target, branch template, and PR template must map to the same publication version, stable branch, and PR outcome.

## Open Questions and Clarifications Needed

No blocking open questions. The implementation should explicitly document that this carrier preserves Git objects; external systems such as Git LFS are supported only to the extent their pointer files are present in the Git object database unless a later carrier extension is approved.
