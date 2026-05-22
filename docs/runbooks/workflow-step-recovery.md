# Workflow step recovery groundwork

This page documents the step-recovery foundation that Scherzo can parse, bundle, persist, and project. Runtime step recovery is not active yet: `workflow_run` does not start nested recovery workers, retry failed steps, or emit recovered terminal outcomes. Treat `recover` as reserved workflow metadata until the runtime follow-up lands.

## Current merge scope

Implemented in this slice:

- workflow-level and step-level `recover` parsing;
- shallow merge semantics and `recover.enabled: false` disablement;
- recovery prompt bundling in runtime bundles;
- recovery decision protocol parsing for `retry_requested` and `gave_up`;
- ledger record types for recovery start/finish events;
- projection fields for stored step-recovery history;
- retained artifact path helpers and default prompt/schema assets.

Intentionally deferred:

- starting a nested recovery worker from `workflow_run` after a failed step;
- enforcing the recovery attempt budget at runtime;
- retrying the original step unchanged after `retry_requested`;
- emitting `succeeded_after_recovery` or `failed_after_recovery` outcomes;
- operator-facing history/CLI rendering for the failed-attempt → recovery → retry timeline;
- crash, timeout, invalid-output, artifact-conflict, and interruption-safety runtime tests.

## Reserved YAML shape

The reserved configuration shape is:

```yaml
version: 1
id: implementation
recover:
  attempts: 1
  prompt: .scherzo/workflows/prompts/recover_failed_step.md
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    workspace: main
```

Step overrides shallow-merge onto workflow defaults. `recover.enabled: false` disables recovery for that step.

Supported fields:

- `enabled`: bool
- `attempts`: int, default `1`
- `model`: optional model override
- `prompt`: required when recovery is enabled

## Protocol and artifact names

Default protocol assets:

- Prompt: `.scherzo/workflows/prompts/recover_failed_step.md`
- Provider schema: `.scherzo/workflows/schemas/provider/workflow-step-recovery-result.v1.schema.json`
- Canonical schema: `.scherzo/workflows/schemas/workflow-step-recovery-result.v1.schema.json`

Planned retained recovery artifacts live under the failed attempt:

- `runs/<run>/<step>/attempt-<n>.json`
- `runs/<run>/<step>/attempt-<n>/recovery-<m>/workflow_step_recovery_result.json`

Durable history records are available for future runtime integration:

- `workflow_step_recovery_started`
- `workflow_step_recovery_finished`

These records link the failed attempt, recovery attempt number, recovery session id, decision (`retry_requested` or `gave_up`), and optional retry attempt index.
