# Linear completion states

Scherzo can choose a Linear state from the workflow outcome instead of treating every successful run as `Done`.

Use `task_updates.states` for artifact-producing workflows such as `workflow:execplan` or implementation workflows. The standard policy is:

- successful runs with reviewable artifacts move to `In Review`;
- successful no-review maintenance runs may move to `Done` through `no_review_success`;
- failures and missing expected artifacts move to `Needs Attention` or `Triage`;
- cancellations leave the issue unchanged unless a future cancellation policy is configured.

Example:

```yaml
tracker:
  linear:
    project: YOUR_LINEAR_PROJECT_SLUG
    check_setup: true

task_updates:
  enabled: true
  states:
    claim: In Progress
    success: In Review
    no_review_success: Done
    failure: Needs Attention
    partial_success: Needs Attention
  comment_on: [claim, failure]
  result:
    on_success: attachment
    max_chars: 20000
```

The failure state does not need to be listed in `tracker.states.ready`. Ready states gate only new initial issue pickup; automatic failure retries and explicit operator retries can resume the same issue from `tracker.states.active` or configured task-update states while still enforcing parked, active/pending, terminal, workflow drift, and recovery safety checks.

Use global `task_updates.states.success`/`no_review_success` when the board policy should apply to every workflow. Use `task_updates.workflows.<workflow-id>` only for workflow-specific exceptions, such as a publish-commit-stack workflow that should not wait for review after it resolves merge conflicts:

```yaml
task_updates:
  states:
    success: In Review
    no_review_success: Done
    failure: Triage
    partial_success: Triage
  workflows:
    merge-conflict-resolution:
      requires_review: false
      states:
        no_review_success: Done
```

A workflow can also override the success target unconditionally without changing other reviewable workflows:

```yaml
task_updates:
  workflows:
    merge-conflict-resolution:
      states:
        success: Done
```

Keep workflow-specific state overrides nested under `states:`. Flat keys such as `task_updates.workflows.merge-conflict-resolution.success` are intentionally rejected so the shape mirrors top-level `task_updates.states`. Each workflow id under `task_updates.workflows` must also match a configured workflow id; Scherzo rejects typoed workflow ids during runtime-bundle loading instead of silently ignoring them.

If your Linear team uses different names, configure those state names instead. The simplified YAML schema currently accepts names in `task_updates.states` and supported `task_updates.workflows.<workflow-id>.states` entries; it does not define id-specific task-update fields. If `doctor --check tracker-contract` reports an ambiguous state name, rename or disambiguate the Linear states, or pause task updates until the board shape is fixed.

Before enabling daemon task updates, run:

```sh
scherzo doctor --check tracker-contract .scherzo/scherzo.yaml
```

Doctor must pass. If it reports a missing or ambiguous state, fix the Linear board or config and rerun the check. Do not rely on best-effort fallback: unresolved configured states skip the Linear state mutation so Scherzo does not accidentally move artifact-producing work to `Done`.

To roll back, set `task_updates.enabled: false` or remove the specific state mappings while you inspect the board policy.
