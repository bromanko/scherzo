# Artifact Publication and GitHub Repository PRD

Status: Draft PRD

Related: [`WORKFLOW_ARTIFACT_TAXONOMY.md`](./WORKFLOW_ARTIFACT_TAXONOMY.md)

## 1. Problem

Scherzo workflows can produce durable output artifacts described by the workflow artifact taxonomy. Today, publishing a reviewable change is tied to the workspace-driver `publish-change` capability. That is the wrong seam for workflow artifacts: a workspace driver should manage workspaces, diffs, baselines, and related VCS mechanics, while artifact publication should route canonical workflow outputs to configured external artifact repositories.

Workflows such as ExecPlan generation produce multiple artifacts with different audiences. For example, an ExecPlan bundle may contain a human-consumable Markdown plan and machine-consumable JSON files for later agents. Some teams may want the Markdown plan checked into a GitHub branch and surfaced through a pull request, while storing JSON artifacts only in Scherzo's internal artifact store or a different external repository in the future.

Scherzo needs an opt-in publication layer that can persist selected workflow artifacts to configured repositories, record publication state, and support retry after publication failures without rerunning artifact-producing steps.

## 2. Goals

- Keep Scherzo's internal artifact store as the canonical source of workflow artifacts.
- Treat all external artifact repositories as derived copies of canonical Scherzo artifacts.
- Allow workflows to opt in to artifact publication.
- Configure named artifact repositories at the orchestrator level.
- Configure workflow-level publication routes from output descriptors to repository targets.
- Support GitHub pull-request publication as the MVP repository backend.
- Have Scherzo create and update GitHub branches and PRs for published artifacts.
- Record publication state in durable Scherzo state, likely the existing daemon ledger or a related ledger.
- Make publication retryable by the operator after transient failures.
- Defer generalized review/approval state while leaving the model compatible with future review providers.

## 3. Non-goals for MVP

- Generalized human review state tracking.
- Tracking GitHub approval, requested changes, comments, or checks as normalized review state.
- Non-GitHub external repository backends such as S3, Artifactory, or custom services.
- Step-level publication configuration.
- Making external repositories canonical.
- Security policy beyond the target repository/provider's existing permissions and failures.

## 4. Concepts

### Canonical artifact

A workflow output artifact retained by Scherzo's internal artifact store. Canonical retained artifacts are addressed by backend-neutral Scherzo refs such as `runs/<run-id>/outputs/<name>` and carry integrity metadata such as `sha256`, `bytes`, and `media_type`.

### Artifact descriptor

The generic descriptor shape from the workflow artifact taxonomy. Publication consumes descriptors with carrier kinds such as `file`, `value`, `ref`, and `artifact_set`. Publication must not require Scherzo core to understand workflow-owned semantic `artifact_type` strings.

### Artifact set

An aggregate descriptor that contains child artifact descriptors. For publication, an artifact set is metadata/a retained manifest plus a collection of selectable entries. Repository backends do not need native artifact-set semantics.

### Artifact repository

A named, operator-configured external target that can receive derived artifact copies. The MVP repository backend is GitHub.

### Publication

A workflow-level route that selects one or more canonical artifacts and materializes them into a configured artifact repository.

### Publication series

A stable logical publication target across runs, such as one ExecPlan publication for one work item and workflow. A series may have multiple versions as reruns produce changed artifacts.

### Publication version

A concrete published version derived from a workflow run's selected artifacts and target paths. If the selected artifact bytes and target mapping are unchanged, no new external version should be created.

## 5. Artifact identity and addressing

The canonical source identity should be based on workflow run outputs rather than workspace paths.

Candidate URI shape:

```text
scherzo://runs/<run-id>/outputs/<output-name>
scherzo://runs/<run-id>/outputs/<output-name>/entries/<entry-name>
```

Output names are unique within a workflow contract. Artifact-set entries are addressed beneath the output that owns the set.

Publication series identity should be stable across reruns. Candidate logical shape:

```text
work/<work-id>/workflow/<workflow-id>/publication/<publication-id>
```

Publication version identity should include the selected canonical artifact digests, repository target, and destination paths. If that version identity matches the previous successful publication for the series, the publication result is `unchanged`.

## 6. Configuration model

### Orchestrator-level repository targets

Repository targets should live under the existing top-level `artifacts` configuration.

Proposed MVP shape:

```yaml
artifacts:
  repositories:
    github:
      docs:
        repo: scherzo-systems/scherzo
        base: main
        checkout:
          strategy: managed_git
        branch:
          strategy: stable_per_work
          template: scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}
        pull_request:
          enabled: true
          strategy: update_existing
          draft: false
```

Notes:

- `artifacts.repositories.github.<name>` defines a named GitHub repository target.
- `repo` is the GitHub `owner/repo`.
- `base` is the target branch for PRs and branch creation.
- `checkout.strategy: managed_git` means Scherzo owns a publication checkout separate from workflow step workspaces. The checkout can live under `.scherzo-state` and is an implementation detail of the repository adapter.
- `branch` config controls materialization/versioning behavior and provides defaults for publications that target this repository.
- `pull_request` config controls optional PR behavior and provides defaults for publications that target this repository.
- `pull_request.draft`, not repository-level `draft_pr`, controls draft PR creation.
- Content-specific PR titles and body templates belong on workflow publication routes. Repository targets may provide defaults, and workflow routes may override them.

### Workflow-level publication routes

Publication should initially be configured at the workflow level, not the step level. If a step artifact needs publication, the workflow should expose it as a contract output first.

Proposed shape:

```yaml
artifacts:
  publications:
    - id: execplan_review_doc
      repository: github.docs
      required: true
      pull_request:
        title: "{{ work.identifier }} ExecPlan"
        body_template: prompts/execplan-pr-body.md
      files:
        - select:
            output: exec_plan_bundle
            entry: plan
          path: docs/plans/{{ work.identifier }}.md
```

Notes:

- `id` identifies the publication within the workflow and participates in publication series identity.
- `repository` references a named orchestrator target.
- `required` defaults to `true`. A required publication failure prevents the workflow from completing successfully and moves the task/run into a triage/recovery state. `required: false` records failures as non-blocking publication warnings.
- `pull_request` fields on the publication route override repository-level PR defaults. Titles and body templates are content-specific, so they normally belong here.
- `files` selects retained `file` artifacts and maps them to repository-relative paths.
- For MVP, publication should select workflow outputs or artifact-set entries. Step-level selectors are deferred.

## 7. GitHub MVP behavior

For each configured GitHub publication, Scherzo should:

1. Resolve the workflow output manifest.
2. Select the configured artifact descriptors.
3. Read selected canonical artifact bytes from the internal artifact store.
4. Materialize those bytes into configured repository-relative paths in a Scherzo-managed publication checkout.
5. Create or update a GitHub branch according to `branch.strategy`.
6. Create or update a pull request when `pull_request.enabled` is true.
7. Record publication metadata and final status in durable state.

The GitHub backend may use local `git` plus `gh` internally for the MVP, but it should be exposed as an artifact repository adapter, not as a workspace-driver capability. It should not require publishing from the workflow step workspace.

The MVP default should be:

```yaml
branch:
  strategy: stable_per_work
pull_request:
  strategy: update_existing
```

This means a new run for the same work item, workflow, and publication id updates the same external branch/PR when artifacts changed. GitHub commit history provides version history without creating PR spam.

Future strategies should remain possible, such as:

- `new_pr_per_run`
- `new_pr_per_changed_version`
- `commit_only`
- repository-specific single-head publication

The implementation should avoid hard-coding the MVP strategy into durable state shape.

## 8. Publication states

MVP publication state should avoid review terminology.

Candidate states:

- `planned`: publication was selected and queued.
- `publishing`: publication is in progress.
- `published`: external repository received a new version.
- `unchanged`: selected artifacts matched the previous external version; no new external version was created.
- `failed`: publication did not complete.
- `retry_scheduled`: an operator or scheduler has queued retry after failure.
- `superseded`: a newer successful version exists for the same publication series.

`planned` and `publishing` may be represented as ledger events rather than durable terminal states.

## 9. Failure and retry behavior

Artifact publication happens after canonical artifacts have already been written. If publication fails:

1. Canonical artifacts and output manifests remain available.
2. The publication failure is recorded durably.
3. If the publication is required, the workflow/task enters a triage or recoverable failure state.
4. An operator can retry publication without rerunning artifact-producing steps.

The preferred operator command shape is:

```sh
scherzoctl artifact publication list --run <run-id>
scherzoctl artifact publication show --run <run-id> --publication <publication-id>
scherzoctl artifact publication retry --run <run-id> [--publication <publication-id>]
```

Retry should be idempotent. If a retry discovers that the target already contains the selected version, it should record `unchanged` or `published` according to backend-specific semantics rather than creating duplicate external state. A retry appends a new publication attempt record; it does not rerun producing workflow steps.

## 10. Migration from workspace `publish-change`

The existing workspace-driver `publish-change` capability should be treated as a legacy seam for publishing workspace changes. Artifact publication should move this responsibility out of workspace drivers.

Migration direction:

1. Keep current behavior until artifact publication is implemented.
2. Add artifact publication config and durable publication state alongside existing workflows.
3. Implement GitHub publication as an artifact repository backend.
4. Migrate ExecPlan-style workflows from explicit publish steps / `publish-change` requirements to workflow-level artifact publications.
5. Deprecate `WorkspacePublishChange` for artifact publication use cases.

Workspace drivers should continue to own workspace setup, status, diff, changed-files, baseline, and refresh-base behavior.

## 11. Resolved design decisions

### 11.1 Template variables

Template rendering should be deterministic, side-effect-free, and limited to a known context. Missing variables are configuration/runtime errors before remote mutation.

Publication-scoped templates such as branch names, PR titles, and PR bodies may use:

- `work.kind`: `task` or `scheduled`.
- `work.id`: backend-owned work id.
- `work.identifier`: human-readable work key, such as `LIV-123`; scheduled runs may use the scheduled job id.
- `work.slug`: branch/path-safe slug derived from `work.identifier`.
- `workflow.id`.
- `run.id`.
- `publication.id`.
- `publication.series_id`.
- `publication.version_id`.
- `repository.kind`, for example `github`.
- `repository.id`, for example `github.docs`.
- `github.repo` and `github.base` for GitHub repository targets.

File path templates also get artifact-scoped variables:

- `artifact.output`: output name that selected the artifact.
- `artifact.entry`: artifact-set entry name when applicable.
- `artifact.name`: descriptor name.
- `artifact.ref`: canonical Scherzo artifact-store ref.
- `artifact.media_type`.
- `artifact.artifact_type` when present.
- `artifact.sha256` and `artifact.sha256_short`.
- `artifact.default_extension`, derived from media type when known.

PR body templates should additionally receive a generated publication summary, such as `publication.files_markdown`, so simple templates do not need loops in the MVP.

Rendered branch names and paths must still pass backend-specific validation. Templates should provide slug variables for safe components rather than silently sanitizing arbitrary rendered output.

### 11.2 Selectors

MVP publication routes should select artifacts explicitly by output name and optional artifact-set entry name. Selecting by `artifact_type`, `media_type`, or metadata tags is deferred.

Reasoning:

- Output and entry names are stable contract keys.
- `artifact_type` and `media_type` are useful metadata but may not be unique within a bundle.
- The taxonomy's `metadata` field is intentionally not a domain-critical routing mechanism.

A future selector extension may add `match` filters over `artifact_type`, `media_type`, or explicit publication tags, but ambiguous matches must be rejected.

### 11.3 Durable state and ledger shape

Use the existing Scherzo state ledger for publication summary events and projections. Store full publication manifests as retained artifacts to avoid making ledger records large.

The publication manifest should include:

- schema version and artifact type, for example `scherzo.artifact_publication.v1`;
- run id, workflow id, publication id, series id, version id, and required flag;
- repository target id and backend kind;
- selected source artifact descriptors and canonical refs;
- destination paths;
- backend result fields such as branch, PR URL, commit SHA, and changed-file list;
- final publication status and bounded diagnostics.

The first implementation slice should emit a dry-run publication manifest only. That manifest should set `dry_run: true`, include repository metadata, branch, rendered pull-request title/body text, and selected file destinations, and deliberately omit remote mutation results such as PR URL, commit SHA, push status, durable ledger ids, or retry metadata until the GitHub publisher lands.

Ledger records should append attempt-level events with deterministic idempotency keys, including attempt start and attempt finish/failure. The projection can compute the latest status per run, publication id, and publication series. If publication volume later warrants a side ledger, the main ledger should still keep summary records and manifest refs.

### 11.4 Optional publication failures

Optional publications are allowed. `required` defaults to `true`; `required: false` means publication failure is recorded as a non-blocking warning and the workflow may still complete if all required work succeeded.

Operators may retry failed optional publications after completion. The publication status remains `failed` with `required: false` until a retry records `published` or `unchanged`.

### 11.5 Operator display

Operator-facing views should show latest publication status by default and expose attempt history on demand.

At minimum, Scherzo should expose:

- publication id;
- required/optional flag;
- status;
- repository target;
- series id and version id;
- selected source artifacts;
- external branch, PR URL, and commit SHA when available;
- failure diagnostics and retryability.

Successful task/result comments should include a compact publication table for externally useful refs. `scherzoctl artifact publication show` should point operators to the retained publication manifest for full details.

### 11.6 Migration and code sharing with `publish-change`

The GitHub artifact publisher may reuse or extract implementation techniques from the current `publish-change` path, especially branch naming, push, and PR creation behavior. It should not invoke `workspace-driver publish-change` as the artifact-publication API.

The target seam is a Scherzo-owned artifact repository adapter. During migration, `publish-change` can remain for legacy workspace-change workflows while artifact-producing workflows move to `artifacts.publications`. After those workflows migrate, `WorkspacePublishChange` can be deprecated for artifact publication use cases.

For the dry-run planner slice, workflow helper scripts under `workflows/dogfood/scripts`, workflow schemas under `workflows/dogfood/schemas`, provider-live/cache behavior, and installed `.scherzo/workflows` workflow migrations remain explicitly out of scope. If those surfaces are untouched, the implementation should record that no helper migration or provider/cache validation was applicable.
