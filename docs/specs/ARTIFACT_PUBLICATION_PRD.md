# Artifact Publication and GitHub Repository PRD

Status: Draft PRD

Related: [`WORKFLOW_ARTIFACT_TAXONOMY.md`](./WORKFLOW_ARTIFACT_TAXONOMY.md)

## 1. Problem

Scherzo workflows produce canonical retained artifacts. Publication must copy selected artifacts to useful review surfaces without changing which system is authoritative for the workflow output.

The publication model needs an explicit split:

- File artifacts for external/cross-repo review can be copied into a managed artifact repository.
- same-repo repository changes must remain workspace-driver-backed and must publish from the retained workflow workspace, not from a hidden managed clone.

Today the docs overstate the artifact-repository path and make it sound like same-repo repository changes should move away from workspace drivers. That is the wrong boundary for same-repo publication. For same-repo changes, the selected workspace driver already owns repository identity, baseline normalization, diff semantics, and publication safety. Scherzo should keep using that boundary through a named driver capability, `publish-commit-stack`, while making `commit_stack` a first-class workflow output and publication concept.

ExecPlan workflows also have dual outputs with different audiences. An ExecPlan workflow can publish a checked-in `commit_stack` for repository changes while separately retaining the singular Markdown plan document as a file artifact for internal review surfaces. The checked-in dogfood ExecPlan workflows no longer publish that Markdown file to GitHub as a single-file route.

## 2. Goals

- Keep Scherzo's internal artifact store as the canonical source of workflow artifacts.
- Keep same-repo repository-change publication workspace-driver-backed.
- Define `publish-commit-stack` as the same-repo publication capability.
- Define `commit_stack` as the workflow-level repository-change output for same-repo publication.
- Keep file artifacts retained for internal review and future external/cross-repo work without presenting GitHub single-file publication as an active dogfood path.
- Support deterministic doctor/preflight checks before remote mutation.
- Make same-repo publication retryable from the retained workflow workspace.
- Preserve unchanged retry idempotence and explicit abandonment semantics.
- Keep the model compatible with future external/cross-repo managed publication.

## 3. Non-goals for MVP

- Reconstructing same-repo repository changes from retained Git bundles by default.
- Making `.scherzo-state/artifact-repositories/github/<hash>` part of the same-repo default path.
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

Publication into a different configured repository or a managed copy owned by Scherzo. This path may use managed checkouts, retained bundle import, or `.scherzo-state/artifact-repositories/github/<hash>` in future implementations. It is not the default or fallback for same-repo publication.

### Publication series

A stable logical publication target across runs, such as one workflow publication for one work item and workflow.

### Publication version

A concrete published version derived from selected canonical artifacts, publication mode, and destination mapping. If the selected identity is unchanged, the result is `unchanged`.

## 5. Publication modes

Scherzo needs two distinct publication modes.

### File publication

File publication selects retained file artifacts and materializes them into an external/cross-repo artifact repository.

Legacy dogfood workflows used `files` routes for ExecPlan review documents. Do not add new GitHub `files` routes for source-tree publication; retain the file artifact in Scherzo state instead, or use a `commit_stack` route when publishing repository changes.

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

`mode: commit_stack` routes select the repository-change output with `commit_stack.select` and must not declare `files` selectors or `pull_request` overrides. Retained plan-doc and review-doc artifacts remain retained workflow outputs unless a future non-dogfood external/cross-repo publication model explicitly reintroduces file publication.

For same-repo publication, Scherzo must not redirect to `.scherzo-state/artifact-repositories/github/<hash>`, must not fall back to a hidden managed clone, and must not require retained Git bundle import as the default path.

## 6. Configuration model

### Orchestrator-level repository targets

Repository targets remain useful for file publication and future external/cross-repo flows.

```yaml
artifacts:
  repositories:
    github:
      docs:
        repo: scherzo-systems/scherzo
        base: main
        branch:
          strategy: stable_per_work
          template: scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}
        pull_request:
          enabled: true
          strategy: update_existing
          draft: false
```

This configuration describes repository identity and branch/PR defaults. It does not redefine same-repo publication, and dogfood GitHub publication should use `mode: commit_stack` rather than managed single-file routes.

### Workflow-level publication routes

Workflow-level publication remains the selection point for both modes.

- File routes reference a named repository target.
- same-repo `commit_stack` routes reference the retained workflow workspace and selected workspace driver.
- In the current schema, same-repo `commit_stack` routes also declare `target.kind: existing_pr_branch` and its `source` output so validators know which existing PR branch is being updated until the LIV-908 migration changes or aliases that route shape.

A same-repo `commit_stack` route must fail doctor/preflight before remote mutation when the selected workspace driver does not expose `publish-commit-stack`.

## 7. GitHub MVP behavior

### File publication behavior

For each configured file publication, Scherzo should:

1. Resolve the workflow output manifest.
2. Select the configured file descriptors.
3. Read selected canonical artifact bytes from the internal artifact store.
4. Materialize those bytes into configured repository-relative paths in a Scherzo-managed publication checkout.
5. Create or update a GitHub branch according to `branch.strategy`.
6. Create or update a pull request when `pull_request.enabled` is true.
7. Record publication metadata and final status in durable state.

### same-repo `commit_stack` behavior

For each configured same-repo `commit_stack` publication, Scherzo should:

1. Resolve the selected `commit_stack` output.
2. Resolve the retained workflow workspace that produced that output.
3. Run doctor/preflight checks and fail before remote mutation when the workspace is missing, stale, points at the wrong repository, or the selected driver lacks `publish-commit-stack`.
4. Verify the retained workflow workspace still exactly matches the selected `commit_stack`: repository identity, clean/no-drift status, base ref and base commit, ordered commits, head commit and head tree, and validation metadata. Fail closed if local Git config, hooks, workspace drift, or branch/ref policy could change what gets published.
5. Ask the selected workspace driver to run `publish-commit-stack` from the retained workflow workspace using branch/ref allowlisting and lease-protected remote updates.
6. Record whether the result was `published`, `unchanged`, or `failed`.
7. Retain the unpublished workspace until explicit abandonment or configured cleanup.

same-repo publication is workspace-driver-backed. It must not use `.scherzo-state/artifact-repositories/github/<hash>` as a default path or fallback path. Managed artifact repositories are not an active dogfood GitHub publication model.

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

If a retained file artifact needs review, inspect the Scherzo artifact directly or publish a repository `commit_stack`; do not add a GitHub single-file publication route.

## 10. Migration from workspace `publish-change`

The same-repo capability is `publish-commit-stack`. Scherzo core commit-stack publication requires that capability and no longer falls back to `publish-change`.

Migration direction:

1. Preserve same-repo repository-change publication as workspace-driver-backed.
2. Require `publish-commit-stack` for same-repo `commit_stack` publication preflight and execution.
3. Keep `.scherzo-state/artifact-repositories/github/<hash>` and retained bundle import out of active dogfood same-repo GitHub publication.

The key migration rule is that artifact publication does not move same-repo repository changes away from workspace drivers or into single-file GitHub routes.

## 11. Resolved design decisions

### 11.1 ExecPlan dual outputs

An ExecPlan workflow may produce both:

- a checked-in `commit_stack` for repository changes; and
- a separately retained singular Markdown plan document for internal review surfaces.

The Markdown plan remains a file artifact even when the implementation result is published as a same-repo `commit_stack`.

### 11.2 same-repo boundary

same-repo publication is workspace-driver-backed because the driver already owns repository identity, workspace normalization, and safe publication behavior for that repository.

### 11.3 external/cross-repo boundary

Managed artifact repositories, retained Git bundle import, and `.scherzo-state/artifact-repositories/github/<hash>` are not active dogfood GitHub publication paths. They are not the same-repo default and must not be used as the same-repo fallback path.

### 11.4 Retry semantics

Retry must be idempotent. The same retained workflow workspace, base boundary, selected `commit_stack`, branch policy, and verified head commit/tree should produce the same publication result or `unchanged`. Every retry must repeat the workspace-to-`commit_stack` identity checks before remote mutation.

### 11.5 Scope notes

Workflow helper migration, provider-live behavior, provider-cache behavior, and runtime rollout are separate implementation concerns. This PRD defines the boundary and acceptance semantics they must follow.