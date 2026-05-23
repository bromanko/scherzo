# Workflow step recovery

This page documents the currently active runtime step-recovery path in Scherzo. When a step has effective `recover` configuration and its failure would otherwise be fatal, `workflow_run` records the failed original attempt, starts one bounded nested recovery worker in the same workspace, and retries the original step unchanged when the recovery result is `retry_requested`.

Recovery remains a no-op for steps without effective recovery, for `recover.enabled: false`, and for `on_failure: continue`. Scherzo also accepts stored step-recovery history and recovered workflow terminal outcomes for compatibility. A workflow run emits `succeeded_after_recovery` or `failed_after_recovery` only when the same run has durable `workflow_step_recovery_started` or `workflow_step_recovery_finished` evidence; daemon startup resume by itself does not relabel a clean run.

## Current merge scope

Implemented in this slice:

- workflow-level and step-level `recover` parsing;
- shallow merge semantics and `recover.enabled: false` disablement;
- recovery prompt bundling in runtime bundles;
- recovery decision protocol parsing for `retry_requested` and `gave_up`;
- recording the failed original attempt before recovery starts;
- starting a nested recovery worker from `workflow_run` after a recoverable fatal step failure;
- enforcing the configured recovery attempt budget at runtime;
- retrying the original step unchanged after `retry_requested`;
- ledger record types for recovery start/finish events and retained recovery result artifacts;
- projection fields for stored step-recovery history;
- runtime coverage for no-op paths plus invalid-output and artifact-conflict failure preservation.

Still intentionally deferred:

- operator-facing history/CLI rendering for the failed-attempt → recovery → retry timeline;
- deeper interruption and crash-resume hardening beyond preserving the original failure result.

## YAML shape

The recovery configuration shape is:

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

Retained recovery artifacts live under the failed attempt:

- `runs/<run>/<step>/attempt-<n>.json`
- `runs/<run>/<step>/attempt-<n>/recovery-<m>/workflow_step_recovery_result.json`

Durable history records written by the runtime are:

- `workflow_step_recovery_started`
- `workflow_step_recovery_finished`

These records link the failed attempt, recovery attempt number, recovery session id, decision (`retry_requested` or `gave_up`), and optional retry attempt index.

Workflow terminal outcomes remain `completed` and `failed_fatal` for clean runs. When same-run step-recovery evidence exists, terminal workflow outcomes may instead be `succeeded_after_recovery` or `failed_after_recovery`.
