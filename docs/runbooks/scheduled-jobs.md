# Scheduled jobs operator runbook

Scherzo scheduled jobs run configured workflow DAGs on fixed intervals without creating a tracker task for successful intervals. Use them for recurring maintenance workflows that should be quiet when healthy and visible when they need human attention. The production failure-reporting adapter is Linear today, so terminal failures can create or update a Linear issue through the tracker adapter.

## MVP configuration shape

Add `schedules` at the top level of `scherzo.yaml`. The MVP supports fixed intervals only, skips overlaps, does not catch up missed intervals, and does not support schedule-level `input`, `vars`, or payload blobs. Put schedule-specific details in workflow YAML, prompt files, scripts, environment, or repository config.

This current conflict-management example schedules the GitHub PR conflict scout. The scout discovers conflicted same-repository PRs and creates normal resolver issues labeled `workflow:merge-conflict-resolution`; the resolver remains an issue-dispatched workflow and is not itself scheduled.

```yaml
workflows:
  merge-conflict-resolution: workflows/merge-conflict-resolution.yaml
  github-pr-conflict-scout: workflows/github-pr-conflict-scout.yaml

schedules:
  - id: github-pr-conflict-scout
    workflow: github-pr-conflict-scout
    enabled: false
    every: 15m
    overlap: skip
    catch_up: false
    on_failure:
      task:
        enabled: true
        state: Triage
        labels:
          - job:github-pr-conflict-scout
        dedupe: open_task_per_schedule
```

Start public or copied configs with `enabled: false` until `SCHERZO_GITHUB_REPO`, the Linear project slug, GitHub credentials if needed, and the resolver workflow label are configured. If tracker workflow-label enforcement is enabled, keep `merge-conflict-resolution` in top-level `workflows` so Scherzo derives the resolver label, but keep `github-pr-conflict-scout` scheduled-only; scheduled workflows are started by `schedules`, not tracker labels. Only trusted operators or automation should be able to apply the resolver workflow label because that workflow can publish the validated conflict resolution. The checked-in example config uses the same shape in `examples/scherzo.yaml`.

The public example defaults to GitHub API conflict detection, caps each run at `SCHERZO_CONFLICT_MAX_OPEN_PRS` open PRs (`100` by default), and passes `--skip-local-preflight` so scheduled intervals do not perform per-PR git fetch/merge preflight by default. For known-small repositories where local merge preflight is acceptable, set `SCHERZO_CONFLICT_ENABLE_LOCAL_PREFLIGHT=true`; the command changes to `repo_root` first so the helper can read the repository origin before it performs temporary-directory git preflight work.

## Adopting the GitHub PR conflict package in another repository

The checked-in package is intentionally limited to Scherzo-managed, same-repository GitHub PR repair with Linear task discovery. It does not support fork PRs, non-GitHub forges, non-Linear resolver issue discovery, or arbitrary publish backends.

To adopt it in another Scherzo-managed GitHub repository:

1. Copy or package `examples/workflows/github-pr-conflict-scout.yaml`, `examples/workflows/merge-conflict-resolution.yaml`, and `examples/workflows/prompts/resolve-merge-conflicts.md` into that repository's Scherzo config.
2. Route both workflows in `scherzo.yaml`, include `merge-conflict-resolution` in the trusted workflow labels, and schedule only `github-pr-conflict-scout`.
3. Configure `SCHERZO_GITHUB_REPO=owner/repo` and either `SCHERZO_LINEAR_PROJECT_SLUG` or `LINEAR_PROJECT_SLUG`. Optionally set `SCHERZO_CONFLICT_CREATE_STATE`, `SCHERZO_CONFLICT_WORKFLOW_LABEL`, `SCHERZO_CONFLICT_MAX_OPEN_PRS`, and `SCHERZO_CONFLICT_ENABLE_LOCAL_PREFLIGHT`.
4. Use a workspace driver that can publish commit stacks (`publish-commit-stack`) back to the same repository branch. The resolver assumes it may fast-forward the PR head branch in the configured repository; fork PRs are rejected.
5. Replace the example command passed to `.scherzo/workflows/scripts/scherzo-merge-conflict run-project-validation -- ...` in `merge-conflict-resolution.yaml` with repo-local validation commands. Keep it after `.scherzo/workflows/scripts/scherzo-merge-conflict validate`: the helper performs generic guard checks, while the workflow YAML owns project-specific checks such as `npm test`, `cargo test`, or repository scripts. The `run-project-validation` wrapper scrubs `SCHERZO_*` workflow context before running those commands and records success so the publish step and PR comment know repo-local validation passed.
6. Start with the scheduled job disabled until GitHub credentials, Linear credentials/project, workflow labels, and validation commands have been verified.

The resolver's generic helper validates conflict-specific invariants only: unresolved conflict markers, allowed non-conflicted file drift, mechanical-edit manifests, and the required resolution summary. Project validation is deliberately outside the helper so each repository can define its own command step.

When `on_failure.task.enabled: true`, the scheduler also applies reserved Linear labels `scherzo:scheduled` and `scherzo:scheduled-job:<job-id>` and writes the marker `<!-- scherzo-dedupe: scheduled-job:<job-id> -->` into the failure task body/comments. Do not rely on configured labels for dedupe.

For Scherzo's checked-in workspace maintenance, `workspace-cleanup` now calls `scherzo cleanup --root <workspace-root> --json --yes`. Treat `scherzo cleanup --json` as the authoritative dry-run inventory for daemon-owned local state and delegated workspace cleanup. The legacy `scripts/scherzo-workspace-cleanup` helper remains only as a local diagnostic shim for empty noop/jj workspaces and is not the scheduled authority.

## Workflow and command shape

The scheduled workflow is a normal workflow DAG. Scheduled prompts and command templates may use scheduled context variables and must not reference `issue.*` because no tracker task exists for successful scheduled intervals. The conflict scout is command-only and invokes the checked-in bundle-local `.scherzo/workflows/scripts/scherzo-github-pr-conflict-scout` helper.

```yaml
version: 1
id: github-pr-conflict-scout
description: Scan open same-repository GitHub pull requests and enqueue merge-conflict resolver issues.
workspace:
  driver: noop
concurrency: 1
steps:
  - id: scan_open_prs
    run: |
      set -eu
      bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}
      if [ -z "$bundle_dir" ]; then
        bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"
      fi
      repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}
      : "${SCHERZO_GITHUB_REPO:?set SCHERZO_GITHUB_REPO to owner/repo}"
      linear_project_slug=${SCHERZO_LINEAR_PROJECT_SLUG:-${LINEAR_PROJECT_SLUG:-}}
      if test -z "$linear_project_slug"; then
        echo "set SCHERZO_LINEAR_PROJECT_SLUG or LINEAR_PROJECT_SLUG" >&2
        exit 64
      fi
      cd "$repo_root"
      max_open_prs=${SCHERZO_CONFLICT_MAX_OPEN_PRS:-100}
      set -- "$bundle_dir/scripts/scherzo-github-pr-conflict-scout" scan \
        --repo "$SCHERZO_GITHUB_REPO" \
        --linear-project-slug "$linear_project_slug" \
        --create-state "${SCHERZO_CONFLICT_CREATE_STATE:-Todo}" \
        --workflow-label "${SCHERZO_CONFLICT_WORKFLOW_LABEL:-workflow:merge-conflict-resolution}" \
        --max-open-prs "$max_open_prs"
      if test "${SCHERZO_CONFLICT_ENABLE_LOCAL_PREFLIGHT:-false}" != "true"; then
        set -- "$@" --skip-local-preflight
      fi
      "$@"
    timeout: 5m
    run_in: main
```

Useful scheduled variables include `{{ scheduled_job.id }}`, `{{ scheduled_job.workflow }}`, `{{ schedule.due_at }}`, `{{ schedule.started_at }}`, `{{ run.id }}`, and `{{ run.attempt }}`.

## Rollout

Start with `enabled: false` or `on_failure.task.enabled: false` while validating local behavior. The current production `scheduled_failures` capability is provided by the Linear tracker adapter. Use a conservative interval such as `15m`; the MVP rejects `catch_up: true`, `overlap: queue`, `overlap: cancel`, and intervals below one second. Make scripts idempotent because retries and daemon restarts can run the same due interval more than once.

After reload or daemon start, inspect local state:

```sh
scherzo schedules status github-pr-conflict-scout
scherzo schedules history github-pr-conflict-scout
scherzo schedules doctor github-pr-conflict-scout
```

`scherzoctl query metrics` distinguishes scheduled configuration from due work: `scheduled_job_count` is the configured job count, `scheduled_next_due_count` is the number of jobs with a remembered next due timestamp, and `scheduled_due_count` is only the number whose next due timestamp is currently due. During a healthy terminal-success interval, expect `scheduled_next_due_count` to remain non-zero while `scheduled_due_count` and `running_scheduled_workers` are zero.

Force a safe manual run only when dispatch is not paused and the same job is not already pending, active, or retrying:

```sh
scherzoctl run-schedule github-pr-conflict-scout --now
scherzo schedules logs github-pr-conflict-scout --last
```

## Failure triage

Successful scheduled intervals are silent in the tracker. A terminal scheduled failure after retries are exhausted, or a needs-human scheduled outcome, creates or updates one open failure task per scheduled job using the stable dedupe key `scheduled-job:<job-id>`. With the Linear adapter, that task is a Linear issue. Later failures for the same job update the same task when possible. Later successes remain local and do not comment on or close the prior failure task.

If tracker failure reporting fails, Scherzo records `scheduled_failure_report_failed` in the local ledger and retries only the report side effect. It does not rerun the completed workflow solely because the tracker side effect failed. See [Tracker adapters](tracker-adapters.md) for the current capability matrix.

## Rollback caution

Once scheduled records have been written, older Scherzo binaries may not understand the local ledger. Before rolling back, run:

```sh
scherzo state status --root <workspace-root>
```

If the status warns about scheduled records, keep the newer binary, archive old state with the existing state archive command, or reinitialize only after accepting loss of local scheduled history.
