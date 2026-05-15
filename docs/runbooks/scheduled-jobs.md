# Scheduled jobs operator runbook

Scherzo scheduled jobs run configured workflow DAGs on fixed intervals without creating a tracker task for successful intervals. Use them for recurring maintenance workflows that should be quiet when healthy and visible when they need human attention. The production failure-reporting adapter is Linear today, so terminal failures can create or update a Linear issue.

## MVP configuration shape

Add `scheduled_jobs` at the top level of `scherzo.yaml`. The MVP supports fixed intervals only, skips overlaps, does not catch up missed intervals, and does not support schedule-level `input`, `vars`, or payload blobs. Put job-specific details in workflow YAML, prompt files, scripts, environment, or repository config.

```yaml
routing:
  workflows:
    pr-conflict-repair: workflows/pr-conflict-repair.yaml

scheduled_jobs:
  - id: pr-conflict-repair
    workflow: pr-conflict-repair
    enabled: true
    every: 15m
    overlap: skip
    catch_up: false
    on_failure:
      linear:
        enabled: true
        state: Triage
        labels:
          - job:pr-conflict-repair
        dedupe: open_issue_per_job
```

When `on_failure.linear.enabled: true`, the scheduler also applies reserved Linear labels `scherzo:scheduled` and `scherzo:scheduled-job:<job-id>` and writes the marker `<!-- scherzo-dedupe: scheduled-job:<job-id> -->` into the failure task body/comments. Do not rely on configured labels for dedupe.

## Workflow and prompt shape

The scheduled workflow is a normal workflow DAG. Scheduled prompts and command templates may use scheduled context variables and must not reference `issue.*` because no tracker task exists for successful scheduled intervals.

```yaml
version: 1
id: pr-conflict-repair
steps:
  - id: inspect
    kind: command
    run: ./scripts/pr-conflict-repair-inspect.sh
    timeout_ms: 300000
    workspace: main
  - id: repair
    kind: agent
    depends_on: [inspect]
    prompt: prompts/pr-conflict-repair.md
    workspace: main
```

Useful scheduled variables include `{{ scheduled_job.id }}`, `{{ scheduled_job.workflow }}`, `{{ schedule.due_at }}`, `{{ schedule.started_at }}`, `{{ run.id }}`, and `{{ run.attempt }}`.

## Rollout

Start with `enabled: false` or `on_failure.linear.enabled: false` while validating local behavior. The current production `scheduled_failures` capability is provided by the Linear tracker adapter. Use a conservative interval such as `15m`; the MVP rejects `catch_up: true`, `overlap: queue`, `overlap: cancel`, and intervals below one second. Make scripts idempotent because retries and daemon restarts can run the same due interval more than once.

After reload or daemon start, inspect local state:

```sh
scherzoctl schedules status pr-conflict-repair
scherzoctl schedules history pr-conflict-repair
scherzoctl schedules doctor pr-conflict-repair
```

Force a safe manual run only when dispatch is not paused and the same job is not already pending, active, or retrying:

```sh
scherzoctl schedules run pr-conflict-repair --now
scherzoctl schedules logs pr-conflict-repair --last
```

## Failure triage

Successful scheduled intervals are silent in the tracker. A terminal scheduled failure after retries are exhausted, or a needs-human scheduled outcome, creates or updates one open failure task per scheduled job using the stable dedupe key `scheduled-job:<job-id>`. With the Linear adapter, that task is a Linear issue. Later failures for the same job update the same task when possible. Later successes remain local and do not comment on or close the prior failure task.

If tracker failure reporting fails, Scherzo records `scheduled_failure_report_failed` in the local ledger and retries only the report side effect. It does not rerun the completed workflow solely because the tracker side effect failed. See [Tracker adapters](tracker-adapters.md) for the current capability matrix.

## Rollback caution

Once scheduled records have been written, older Scherzo binaries may not understand the local ledger. Before rolling back, run:

```sh
scherzoctl state status --root <workspace-root>
```

If the status warns about scheduled records, keep the newer binary, archive old state with the existing state archive command, or reinitialize only after accepting loss of local scheduled history.
