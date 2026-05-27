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
- runtime hardening for malformed or invalid recovery output, worker failures and timeouts, artifact-write failures, immutable artifact conflicts, and interrupted start-without-finish recovery visibility.

Still intentionally deferred:

- automatic resumption of an interrupted nested recovery worker after daemon restart;
- richer browser/operator timeline presentation beyond the retained `scherzoctl session` history block.

## YAML shape

The recovery configuration shape is:

```yaml
version: 1
id: implementation
recovery:
  attempts: 1
  prompt: .scherzo/workflows/prompts/recover_failed_step.md
steps:
  - id: implement
    prompt: prompts/implement.md
    run_in: main
```

Step overrides shallow-merge onto workflow defaults. `recovery.enabled: false` disables recovery for that step.

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

- `runs/<run>/<step-ref>/attempt-<n>.json`
- `runs/<run>/<step-ref>/attempt-<n>/recovery-<m>/workflow_step_recovery_result.json`

`<step-ref>` is the collision-resistant sanitized step-artifact path component used for step-scoped retained artifacts.

Durable history records written by the runtime are:

- `workflow_step_recovery_started`
- `workflow_step_recovery_finished`

These records link the failed attempt, recovery attempt number, recovery session id, result (`retry_requested`, `gave_up`, or a non-retry diagnostic such as `worker_failed`, `invalid_output`, or `artifact_write_failed`), and optional retry attempt index.

Use the human session view to inspect that history quickly:

```sh
scripts/scherzoctl session <session-ref>
```

The output appends a `workflow_step_recovery_history` block for the original failed step session, the retry continuation session, or the nested recovery session. The block now links the original failed attempt artifact, the retained recovery result artifact when one was durably written, and the retry attempt artifact when a retry was scheduled. A successful recovered timeline looks like:

```text
workflow_step_recovery_history:
  - run_id: run-1
    workflow_id: implementation
    step_id: implement
    failed_attempt_index: 1
    recovery_attempt_number: 1
    recovery_session_id: workflow-run-1-implement-recovery-1
    status: finished
    failed_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-1.json
    recovery_result_artifact_ref: runs/run-1/<step-ref>/attempt-1/recovery-1/workflow_step_recovery_result.json
    decision: retry_requested
    summary: Fixed tests
    reason: The workspace is ready for a retry.
    retry_attempt_index: 2
    retry_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-2.json
    retry_result: succeeded
    final_workflow_outcome: succeeded_after_recovery
```

For deeper transcript inspection, replay the nested recovery session directly:

```sh
scripts/scherzoctl events --pretty <recovery-session-id>
```

## Protected retry checkpoints

Self-healing recovery may edit the normal retained workspace under `StepContext.workspace_path`, but it must not leave ledger-addressed retry checkpoints mutated. Scherzo protects these retained artifact paths for the current run:

- `.scherzo-state/artifacts/runs/<run>/<step>/attempt-<n>.json`
- `.scherzo-state/artifacts/runs/<run>/inputs.v1.json`
- `.scherzo-state/artifacts/runs/<run>/outputs.v1.json` when an output manifest was already recorded

If a recovery worker changes or deletes one of those protected files, Scherzo restores the original bytes before the recovery result is accepted and appends the `protected_checkpoint_restored` diagnostic to recovery history. If Scherzo cannot read, hash, or restore a protected checkpoint, recovery stops early with `recovery_artifact_restore_failed` instead of proceeding to a later retry-step validation failure.

Workflow terminal outcomes remain `completed` and `failed_fatal` for clean runs. When same-run step-recovery evidence exists, terminal workflow outcomes may instead be `succeeded_after_recovery` or `failed_after_recovery`.
