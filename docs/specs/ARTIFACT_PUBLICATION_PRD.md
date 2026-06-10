# Artifact Publication and GitHub Repository PRD

Status: Draft PRD

Related: [`WORKFLOW_ARTIFACT_TAXONOMY.md`](./WORKFLOW_ARTIFACT_TAXONOMY.md)

## 1. Problem

Scherzo workflows produce canonical retained artifacts. Publication must copy selected artifacts to useful review surfaces without changing which system is authoritative for the workflow output.

The publication model needs an explicit split:

- File artifacts for external/cross-repo review need a future driver-owned or external publication lane; the old managed-checkout implementation is removed.
- same-repo repository changes must remain workspace-driver-backed and must publish from the retained workflow workspace, not from a hidden managed clone.

Today the docs overstate the artifact-repository path and make it sound like same-repo repository changes should move away from workspace drivers. That is the wrong boundary for same-repo publication. For same-repo changes, the selected workspace driver already owns repository identity, baseline normalization, diff semantics, and publication safety. Scherzo should keep using that boundary through a named driver capability, `publish-commit-stack`, while making `commit_stack` a first-class workflow output and publication concept.

ExecPlan workflows also have dual outputs with different audiences. An ExecPlan workflow can publish a checked-in `commit_stack` for repository changes while separately retaining the singular Markdown plan document as a file artifact for internal review surfaces. The docs must make that split explicit.

## 2. Goals

- Keep Scherzo's internal artifact store as the canonical source of workflow artifacts.
- Keep same-repo repository-change publication workspace-driver-backed.
- Define `publish-commit-stack` as the same-repo publication capability.
- Define `commit_stack` as the workflow-level repository-change output for same-repo publication.
- Keep file artifact selection/configuration understandable while runtime publication is unsupported until a driver-owned or external replacement exists.
- Support deterministic doctor/preflight checks before remote mutation.
- Make same-repo publication retryable from the retained workflow workspace.
- Preserve unchanged retry idempotence and explicit abandonment semantics.
- Keep the model compatible with future external/cross-repo managed publication.

## 3. Non-goals for MVP

- Reconstructing same-repo repository changes from retained Git bundles by default.
- Reintroducing hidden Scherzo-owned GitHub checkout clones for same-repo publication.
- Generalized review-state tracking.
- Non-GitHub managed repository backends beyond the current artifact-repository framing.
- Step-level publication configuration.
- Runtime migration of existing workflows in this PRD alone.

## 4. Concepts

### Canonical artifact

A workflow output artifact retained by Scherzo's internal artifact store. Canonical retained artifacts are addressed by Scherzo refs such as `runs/<run-id>/outputs/<name>` and carry integrity metadata such as `sha256`, `bytes`, and `media_type`.

### Artifact repository

A named, operator-configured external target that can receive derived copies of file artifacts. For MVP, the repository backend is GitHub. This concept is for external/cross-repo publication and does not replace the same-repo publication boundary.

### same-repo publication

Publication of repository changes back into the repository already represented by the workflow workspace driver. same-repo publication is workspace-driver-backed, uses the retained workflow workspace as the authoritative carrier, and publishes through `publish-commit-stack`.

### external/cross-repo publication

Publication into a different configured repository or a future driver-owned/external lane. This path is not implemented by hidden Scherzo-owned GitHub checkout clones and is not the default or fallback for same-repo publication.

### Publication series

A stable logical publication target across runs, such as one workflow publication for one work item and workflow.

### Publication version

A concrete published version derived from selected canonical artifacts, publication mode, and destination mapping. If the selected identity is unchanged, the result is `unchanged`.

## 5. Publication modes

Scherzo needs two distinct publication modes.

### File publication

File publication selects retained file artifacts for an external/cross-repo review surface. The route shape remains part of the schema, but runtime GitHub file publication is currently unsupported until a driver-owned or external replacement exists. It is not the active same-repository GitHub publication model for checked-in dogfood workflows.

### `commit_stack` publication

`commit_stack` publication selects a workflow-owned repository-change output and publishes it through the workspace driver that owns the same repository.

Example:

```yaml
artifacts:
  publications:
    - id: implementation_commit_stack
      repository: github.code
      mode: commit_stack
      required: true
      commit_stack:
        select:
          output: commit_stack
      target:
        kind: existing_pr_branch
        source:
          output: merge_conflict_target
```

Until the runtime/schema migration for LIV-908 lands, `repository` and `target.kind: existing_pr_branch` are the current executable schema shape for `commit_stack` routes. The `target.source.output` value names a `code_change` output that identifies the existing same-repo PR branch target; it does not make a hidden managed clone authoritative. A later migration may replace or alias this shape with a cleaner driver-owned route, but examples in this PRD should stay explicit about current validator requirements.

`mode: commit_stack` routes select the repository-change output with `commit_stack.select` and must not declare `files` selectors or `pull_request` overrides. Retained plan-doc and review-doc artifacts may remain workflow outputs, but same-repository GitHub publication must not use separate single-file routes.

For same-repo publication, Scherzo must not redirect to `.scherzo-state/artifact-repositories/github/<hash>`, must not fall back to a hidden managed clone, and must not require retained Git bundle import as the default path.

## 6. Configuration model

### Orchestrator-level repository targets

Repository targets remain useful for same-repo `commit_stack` routing metadata and future external/cross-repo flows. The checked-in dogfood configuration no longer uses a `github.docs` single-file artifact-publication repository, and managed GitHub checkout configuration must not be treated as an active same-repository publication fallback. This configuration names repository metadata for planning and future external publication; it does not activate a Scherzo-owned checkout and does not redefine same-repo publication.

### Workflow-level publication routes

Workflow-level publication remains the selection point for both modes.

- File routes reference a named repository target.
- same-repo `commit_stack` routes reference the retained workflow workspace and selected workspace driver.
- In the current schema, same-repo `commit_stack` routes also declare `target.kind: existing_pr_branch` and its `source` output so validators know which existing PR branch is being updated until the LIV-908 migration changes or aliases that route shape.

A same-repo `commit_stack` route must fail doctor/preflight before remote mutation when the selected workspace driver does not expose `publish-commit-stack` or a documented migration-compatible `publish-change` alias that implements the same semantics before the rename is implemented.

## 7. GitHub MVP behavior

### File publication behavior

For each configured file publication, current Scherzo runtime should:

1. Resolve and validate the route enough to produce a targeted failure.
2. Record `file_publication_unsupported` without cloning, fetching, resetting, or cleaning a hidden GitHub checkout.
3. Direct workflow authors to publish from a workspace-driver-owned step or convert repository changes to `mode: commit_stack`.

A future external/cross-repo implementation must keep the active workflow workspace boundary explicit and must not recreate hidden same-repo clones.

### same-repo `commit_stack` behavior

For each configured same-repo `commit_stack` publication, Scherzo should:

1. Resolve the selected `commit_stack` output.
2. Resolve the retained workflow workspace that produced that output.
3. Run doctor/preflight checks and fail before remote mutation when the workspace is missing, stale, points at the wrong repository, or the selected driver lacks `publish-commit-stack` or a documented migration-compatible `publish-change` alias that implements the same semantics before the rename is implemented.
4. Verify the retained workflow workspace still exactly matches the selected `commit_stack`: repository identity, clean/no-drift status, base ref and base commit, ordered commits, head commit and head tree, and validation metadata. Fail closed if local Git config, hooks, workspace drift, or branch/ref policy could change what gets published.
5. Ask the selected workspace driver to run `publish-commit-stack` from the retained workflow workspace using branch/ref allowlisting and lease-protected remote updates.
6. Record whether the result was `published`, `unchanged`, or `failed`.
7. Retain the unpublished workspace until explicit abandonment or configured cleanup.

same-repo publication is workspace-driver-backed. It must not use hidden Scherzo-owned GitHub checkout clones as a default path or fallback path.

## 8. Publication states

Candidate states:

- `planned`
- `publishing`
- `published`
- `unchanged`
- `failed`
- `retry_scheduled`
- `abandoned`
- `superseded`

`abandoned` means the retained workflow workspace was intentionally retired without publication. It is not an implicit cleanup result.

## 9. Failure, retry, and retention behavior

Publication happens after canonical artifacts already exist.

If same-repo `commit_stack` publication fails:

1. Canonical artifacts remain available.
2. The retained workflow workspace remains the authoritative same-repo carrier.
3. Failure is recorded durably.
4. Retry reuses the retained workflow workspace.
5. Unchanged retry is idempotent and records `unchanged` instead of creating duplicate remote state.
6. Missing or stale retained workspaces fail closed.
7. Cleanup does not silently abandon publishable workspaces; abandonment must be explicit and auditable.

If file publication is requested, the retained file artifacts remain available, but GitHub file publication fails with `file_publication_unsupported` until a driver-owned or external replacement exists.

## 10. Migration from workspace `publish-change`

The same-repo capability is `publish-commit-stack`. Checked-in dogfood workflows now require `publish-commit-stack` for commit-stack GitHub publication; `publish-change` remains only a compatibility alias accepted by bundled runtime drivers and older custom drivers with equivalent semantics.

Migration direction:

1. Keep GitHub file publication disabled rather than recreating the removed managed-checkout path, and do not add new same-repository GitHub file-publication routes.
2. Preserve same-repo repository-change publication as workspace-driver-backed `commit_stack` publication.
3. Prefer `publish-commit-stack` in workflow requirements and operator docs; treat `publish-change` as a driver compatibility alias with equivalent semantics, not as a workflow-local direct PR creation path.
4. Update doctor/preflight to reject same-repo `commit_stack` publication when the chosen driver cannot publish commit stacks under either the target name or the documented compatibility alias.
5. Keep hidden Scherzo-owned GitHub checkout clones removed from active same-repo GitHub publication; any future external/cross-repo work must define a new driver-owned or explicit external boundary.

The key migration rule is that artifact publication does not move same-repo repository changes away from workspace drivers.

## 11. Resolved design decisions

### 11.1 ExecPlan dual outputs

An ExecPlan workflow may produce both:

- a checked-in `commit_stack` for repository changes; and
- a separately retained singular Markdown plan document for internal review surfaces.

The Markdown plan remains a file artifact even when the implementation result is published as a same-repo `commit_stack`.

### 11.2 same-repo boundary

same-repo publication is workspace-driver-backed because the driver already owns repository identity, workspace normalization, and safe publication behavior for that repository.

### 11.3 external/cross-repo boundary

External/cross-repo publication remains a future design space, but hidden Scherzo-owned GitHub checkout clones are not the same-repo default and must not be used as the same-repo fallback path.

### 11.4 Retry semantics

Retry must be idempotent. The same retained workflow workspace, base boundary, selected `commit_stack`, branch policy, and verified head commit/tree should produce the same publication result or `unchanged`. Every retry must repeat the workspace-to-`commit_stack` identity checks before remote mutation.

### 11.5 Scope notes

Workflow helper migration, provider-live behavior, provider-cache behavior, and runtime rollout are separate implementation concerns. This PRD defines the boundary and acceptance semantics they must follow.