# GitHub artifact repository adapter

This ExecPlan v2 review document is the human review surface for LIV-740. It is retained as historical context only; LIV-934 removed the managed-checkout artifact repository implementation described here. Mechanical implementation steps, tests, interfaces, dependencies, and artifact notes were supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo workflows need to publish selected retained artifacts to GitHub without treating a workflow step workspace as the publication source. After the later implementation, a workflow route such as `artifacts.publications[].files[]` can copy canonical artifact-store bytes into a repository-relative path, commit those bytes on a stable Scherzo-owned branch, push the branch, and create or update a GitHub pull request. Operators should be able to see that changed content produced a new published version and that an unchanged rerun produced no new version.

## Problem Framing and Constraints

The source PRD keeps Scherzo's internal artifact store canonical and treats GitHub as a derived publication target. The current tree already has typed publication config and a dry-run planner, while legacy ExecPlan publishing still goes through `workspace-driver publish-change`. The adapter must therefore sit behind Scherzo artifact publication runtime code, not behind a workspace driver, and it must read bytes by artifact-store ref rather than from a step workspace path.

The historical MVP was intentionally GitHub-only and planned to use local `git` plus `gh` as implementation tools. It assumed a managed checkout under Scherzo state outside workflow step workspaces, keyed by repository target, base branch, and publication series so independent publications to the same GitHub repository could not share a dirty branch. LIV-934 superseded that strategy: GitHub file publication is unsupported until a driver-owned lane/worktree exists, while same-repo publication uses `mode: commit_stack` and the workspace driver. Non-GitHub backends, generalized review state, GitHub approval tracking, and broad dogfood workflow migration were deferred.

## Strategy Overview

Build the adapter as a small Scherzo-owned publication execution layer around the existing planner. The planner already selects canonical file artifacts, verifies hashes and byte counts, renders branch names, PR text, and destination paths, and computes a stable `version_id`. The adapter should consume that planned manifest, prepare or reuse a managed Git checkout for the repository target, base branch, and publication series, materialize only the selected canonical bytes into rendered repository-relative destinations, detect whether the working tree changed, commit changed files, push the configured stable branch, and create or update the matching pull request. The same execution path should be callable from workflow completion and from an explicit local operator retry command that uses the retained manifest rather than a step workspace.

Treat unchanged selected content as a first-class terminal outcome. If the latest successful publication for the same series already has the same `version_id`, or if materialization leaves the managed checkout with no staged diff after synchronizing the branch, the adapter records `unchanged` and does not create a commit, push a new branch head, or create a duplicate PR. If content changed, it records `published` with commit SHA, branch, PR URL, selected files, and manifest ref. Required publication failure blocks workflow success; optional failure records a warning state.

## Alternatives Considered

Calling `workspace-driver publish-change` was rejected because it publishes workspace diffs and couples artifact publication to step workspace lifecycle. The PRD explicitly moves retained workflow artifacts to a Scherzo-owned artifact repository adapter.

Using the workflow step workspace as the Git checkout was rejected because workflow steps may be isolated, cleaned up, retried, or based on a different repository state. A managed checkout under Scherzo state gives the adapter its own lifecycle, makes retries independent of artifact-producing steps, and avoids accidental publication of unrelated workspace edits.

Using the GitHub API directly for every file write was deferred. Local `git` plus `gh` is sufficient for the MVP, is deterministic with fake binaries in tests, and still leaves the public Scherzo seam as an artifact repository adapter rather than a command-line publishing helper.

Creating a new PR for every run was rejected for the MVP. `stable_per_work` plus `update_existing` keeps one review thread for a work item, workflow, and publication id, while Git commit history preserves changed versions without PR spam.

## Risks and Countermeasures

The main correctness risk is publishing bytes that did not come from Scherzo's canonical artifact store. Counter it by making the adapter accept planned selected artifacts and explicit byte payloads read through the artifact-store API, with tests that use fake artifact refs and no step workspace file fallback.

The main side-effect risk is mutating the wrong Git repository or including unrelated files. Counter it with one managed checkout root per repository target, base branch, and publication series; explicit base branch synchronization; repository-relative destination validation before writes; staged-path checks limited to rendered destinations; and failure if `git status --porcelain` shows unexpected changes.

The main idempotency risk is duplicate commits or duplicate PRs after retries. Counter it with durable publication series state, deterministic `version_id`, no-op detection before commit, `gh pr list` lookup for the stable head branch, an explicit `scherzoctl artifact publication retry` path that reuses the retained manifest, and tests that run the same publication twice and assert one commit and one PR operation.

The main operational risk is relying on real GitHub during tests. Counter it with fake `git` and fake `gh` executables that record argv, simulate success and failure, and expose deterministic clone, fetch, checkout, status, add, diff, commit, push, PR-list, PR-create, and PR-edit behavior. Live GitHub dogfood should be deferred to a human/operator after implementation unless a repository-specific staging target is explicitly configured.

The main review-process risk is prose-only obligations drifting from the structured implementation pack. Counter it by requiring the implementation pack to include matching adapter steps, fake-command tests, idempotency checks, helper inventory, full validation, and manual evidence timing.

## Scope Boundaries

In scope: a GitHub artifact repository adapter owned by Scherzo; managed `git` checkout lifecycle outside workflow step workspaces with checkout roots keyed by repository target, base branch, and publication series; materialization of selected canonical artifact bytes into rendered repository-relative paths; changed-file detection; commits; branch pushes; PR create/update behavior through `gh`; `stable_per_work`, `update_existing`, and `pull_request.draft` support; unchanged-content handling; durable publication result recording; deterministic fake `git` and `gh` tests; local operator list/show evidence for publication status; and an observable local `scherzoctl artifact publication retry --run <run-id> --publication <publication-id> --json` path that replays a retained manifest idempotently.

Out of scope: invoking `workspace-driver publish-change`; publishing arbitrary workspace diffs; making GitHub canonical; selecting artifacts by `artifact_type`, media type, or metadata; non-GitHub repositories; generalized review approval state; real browser checks; provider-live/cache behavior; and broad migration of checked-in dogfood workflows from their existing publishing command. A later migration task may move ExecPlan workflows to `artifacts.publications` after this adapter and the state/inspection path are proven.

## Milestones

Milestone 1 proves the adapter seam and runtime placement. Reviewers should see a Scherzo-owned module that is called after workflow contract outputs are recorded and before a required workflow is marked successful, receives planned publication manifests and artifact-store bytes, records required versus optional outcomes, and never calls a workspace driver or reads from a step workspace.

Milestone 2 proves managed checkout safety. Reviewers should see deterministic checkout roots under Scherzo state, each root keyed by repository target, base branch, and publication series, base branch synchronization, branch creation or reset for `stable_per_work`, and failure on dirty unexpected files before materialization.

Milestone 3 proves materialization and version semantics. Reviewers should see selected canonical bytes written only to rendered repository-relative paths, duplicate and unsafe paths rejected, expected deletions or replacements handled explicitly, unchanged selected bytes producing `unchanged`, and changed bytes producing a staged diff eligible for commit.

Milestone 4 proves GitHub mutation behavior with fakes. Reviewers should see fake `git` evidence for clone, fetch, checkout, status, add, diff, commit, rev-parse, and push; fake `gh` evidence for PR lookup, PR creation, PR editing, and draft flag handling; and failure tests for early Git command exits, mutation command exits, malformed `gh` JSON, multiple matching PRs, PR edit failures after a previously found PR disappears, and push failures. When PR lookup returns no open PR for the stable head branch, the expected `update_existing` behavior is to create the PR, not fail.

Milestone 5 proves release readiness. Reviewers should see durable publication manifests and status entries with branch, commit SHA, PR URL, selected files, `version_id`, terminal status, and retryable diagnostics; targeted tests and full validation passing; local operator list/show/retry evidence using seeded or fake-command state; a helper/scope inventory; and explicit evidence that live GitHub/manual browser dogfood is deferred to a human/operator after implementation.

## Progress

- [x] (2026-05-31) Read the LIV-740 task brief, source PRD, workflow artifact taxonomy, existing artifact publication config and planner code, current workflow contract/output path, and related artifact publication review documents.
- [x] (2026-05-31) Drafted this concise human-reviewable review document under `docs/plans/`.
- [x] (2026-05-31) Prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-31) Validated this review document with `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/github-artifact-repository-adapter.md`.
- [x] (2026-05-31) Incorporated review feedback by making operator retry scope observable, aligning managed checkout identity to repository target/base/series, clarifying no-PR versus disappeared-PR behavior, and requiring fake-git negative evidence for early Git commands.
- [x] (2026-05-31) Implemented the Milestone 1 execution seam: workflow completion now routes artifact publications through a Scherzo-owned executor that plans publications, rereads canonical artifact-store bytes for GitHub routes, and records outcomes before workflow success is finalized.

## Decision Log

- Decision: Implement a Scherzo-owned GitHub artifact repository adapter instead of routing through `workspace-driver publish-change`.
  Rationale: Artifact publication consumes canonical workflow outputs, while workspace drivers own workspace diffs and lifecycle mechanics.
  Date: 2026-05-31.

- Decision: Use a managed Git checkout under Scherzo state, outside workflow step workspaces.
  Rationale: Publication retry and idempotency must not depend on an ephemeral or dirty step workspace.
  Date: 2026-05-31.

- Decision: Treat `stable_per_work` plus `update_existing` as the only MVP mutation strategy.
  Rationale: It satisfies the PRD's no-PR-spam requirement and leaves room for future strategies without overgeneralizing now.
  Date: 2026-05-31.

- Decision: Require fake `git` and `gh` tests before any live GitHub evidence.
  Rationale: The adapter's branch, commit, push, PR, idempotency, and failure behavior can be proven deterministically without credentials or network access.
  Date: 2026-05-31.

- Decision: Key managed checkout roots by repository target, base branch, and publication series.
  Rationale: A single checkout per GitHub repository could let one publication series leave dirty or branch-specific state that blocks or pollutes another; per-series roots are more storage-heavy but keep retry and safety boundaries clear.
  Date: 2026-05-31.

- Decision: Keep publication retry in scope as an explicit local operator command while deferring live GitHub retry dogfood.
  Rationale: Rollout and recovery need an observable way to replay a retained manifest, but deterministic fake-command and seeded-state evidence is enough before a staging repository and credentials are chosen.
  Date: 2026-05-31.

- Decision: Land Milestone 1 as an additive executor seam that records repository-backed publication outcomes while proving artifact-store-byte handoff to a Scherzo-owned GitHub adapter boundary.
  Rationale: This keeps workflow finalization observable while moving completion onto the non-workspace execution path required for checkout, materialization, and mutation retries.
  Date: 2026-05-31.

## Validation and Acceptance

This planning issue is accepted when `docs/plans/github-artifact-repository-adapter.md` exists, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/github-artifact-repository-adapter.md` exits zero and prints `REVIEW_DOC_VALID=ok`, every required review-doc section is non-empty, and Scherzo captures the structured implementation-pack submission for LIV-740.

The later implementation is accepted only with automated evidence that a configured GitHub publication route uses the Scherzo artifact publication adapter and not `workspace-driver publish-change`; reads selected bytes from canonical artifact-store refs; materializes those bytes into repository-relative destinations; commits changed files; pushes the stable branch; creates a draft PR when no PR exists; updates the existing PR when one exists; records branch, commit SHA, PR URL, status, `series_id`, `version_id`, retryable diagnostic status, and retained manifest ref; exposes local list/show/retry inspection through `scherzoctl artifact publication`; and treats unchanged bytes and mapping as `unchanged` with no new commit, push, or PR creation.

Negative and idempotency evidence is required. Tests must use fake `git` and fake `gh` or an equivalent deterministic command runner to assert exact argv and outcomes for clone success and failure, fetch success and failure, checkout or branch creation success and failure, status success, dirty status, and status command failure, add success and failure, diff success, no-diff, and diff command failure, unsafe path failure, missing artifact failure, unchanged no-op, commit failure, push failure, PR-create failure, PR-edit failure after an existing PR was found, malformed `gh` JSON, multiple matching PRs, duplicate destination conflict, explicit operator retry from a retained manifest, and retry after a prior successful version. The expected `update_existing` rule is: zero PRs from `gh pr list` creates a PR; exactly one edits it; more than one fails closed; an edit command that reports the previously found PR is gone records a retryable failure. The same fake-command evidence must prove `pull_request.draft: true` passes a draft flag on create and that `pull_request.draft: false` does not.

Full validation evidence must include `direnv exec . gleam test`, focused adapter tests, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Manual evidence timing is explicit. Before publish, local manual evidence should be limited to seeded or fake-command operator output showing `published`, `unchanged`, and `failed` publication states through the local inspection path, plus one retry transcript for `scherzoctl artifact publication retry --run <run-id> --publication <publication-id> --json` proving the retained manifest is replayed without duplicate commits or PRs. Live GitHub PR evidence, browser evidence, provider-live checks, and provider-cache checks are deferred to a human/operator after implementation and must be collected against a staging repository with credentials kept out of logs.

Docs/helper evidence must include a scope inventory. If workflow helper scripts, workflow schemas, provider-facing structured-output helpers, review-lane contracts, or operator docs are changed, the relevant helper or offline contract tests must run; if those surfaces are untouched, the implementation evidence must state that no helper migration, provider-live validation, or cache validation was applicable.

## Rollout, Recovery, and Idempotence

Rollout should be additive. Existing workflows that do not declare `artifacts.publications` keep their current behavior, and legacy `publish-change` workflows continue to run until a later migration task removes them. The adapter should be enabled only for configured repository targets and publication routes, and required publication failure should leave canonical artifacts retained for retry or inspection.

Recovery from local failures is to delete or repair the managed checkout for the affected repository target/base/series and rerun publication from the retained artifact manifest with `scherzoctl artifact publication retry --run <run-id> --publication <publication-id> --json`. Recovery from remote failures is to retry the same publication series through that command or an equivalent workflow-finalization replay; if the branch already contains the selected version, the retry records `unchanged` or `published` according to observed state rather than creating duplicates. Recovery from a bad rollout before remote mutations is a code revert; after remote mutations, keep the branch and PR as audit evidence or close/delete them manually from GitHub if the operator decides the publication was invalid.

Idempotence is an acceptance property. Repeating the same run or retrying a run with the same selected artifact bytes, destination mapping, repository target, branch template, and PR templates must not create a new version. Changing selected bytes or destination mapping must create a new commit on the same stable branch and update the same PR.

## Open Questions and Clarifications Needed

No blocking open questions. The implementation should keep real GitHub dogfood deferred until a staging repository and credentials are explicitly chosen, and dogfood workflow migration from `publish-change` should remain a later task after the adapter is proven with deterministic tests and local operator evidence.
