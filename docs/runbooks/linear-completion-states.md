# Linear completion states

Scherzo can choose a Linear state from the workflow outcome instead of treating every successful run as `Done`.

Use `handoff.completion_states` for artifact-producing workflows such as `workflow:execplan` or implementation workflows. The standard policy is:

- successful runs with reviewable artifacts move to `In Review`;
- successful no-review maintenance runs may move to `Done` when `no_review_completion_state` or a workflow `success_state` is configured;
- failures and missing expected artifacts move to `Needs Attention`;
- cancellations leave the issue unchanged unless `cancellation_state` is configured.

Example:

```yaml
linear_contract:
  enabled: true

handoff:
  enabled: true
  completion_states:
    default_completion_state: In Review
    no_review_completion_state: Done
    failure_state: Needs Attention
    partial_success_state: Needs Attention
    workflows:
      execplan:
        produces_reviewable_artifacts: true
        requires_review: true
```

If your Linear team uses different names, configure those names instead. If names are ambiguous or unstable, use ids:

```yaml
handoff:
  completion_states:
    default_completion_state_id: <linear-in-review-state-id>
    failure_state_id: <linear-needs-attention-state-id>
    partial_success_state_id: <linear-needs-attention-state-id>
```

Before enabling daemon handoff, run:

```sh
scherzo doctor --check linear-contract
```

Doctor must pass. If it reports `missing_state`, `missing_completion_state_id`, or `ambiguous_completion_state_name`, fix the Linear board or config and rerun the check. Do not rely on best-effort fallback: unresolved configured states skip the Linear state mutation so Scherzo does not accidentally move artifact-producing work to `Done`.

To roll back, remove `handoff.completion_states`. Legacy `handoff.success_state_id` and `handoff.failure_state_id` remain supported for deployments that have not opted into outcome-based completion policy.
