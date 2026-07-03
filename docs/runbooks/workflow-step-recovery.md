# Workflow step recovery

This page documents the active repair-and-recheck step-recovery path in Scherzo. When a step has effective `recover` configuration and its failure would otherwise be fatal, `workflow_run` records the failed attempt, writes a structured recovery-input artifact, starts one bounded nested recovery worker in the same workspace for that failed attempt, and rechecks the original step unchanged when the recovery result is `recheck`. If the recheck fails and the step still has remaining recovery budget, the next failed attempt starts a fresh nested recovery worker.

Recovery remains a no-op for steps without effective recovery, for `recover.enabled: false`, and for `on_failure: continue`. A workflow run emits `succeeded_after_recovery` or `failed_after_recovery` only when the same run has durable `workflow_step_recovery_started` or `workflow_step_recovery_finished` evidence; daemon startup resume by itself does not relabel a clean run.

## Agent-step recheck semantics

After a recovery worker returns `recheck`, Scherzo schedules the failed step as the next normal attempt. For agent steps, that recheck is a fresh pi session using the step's original prompt mode, not a continuation of the failed pi session and not a follow-up prompt sent into the recovery worker. Any recorded startup-recovery continuation for that step is discarded before the recheck.

The recheck renders the original step prompt/template again from the workflow step artifacts. The failed attempt artifact, recovery result, and previous pi transcript/session file are not added to the recheck prompt. Declared upstream interpolations such as `{{ steps.collect.stdout }}` therefore render the same on the original attempt and on the recheck while the recovery worker's workspace changes remain available on disk.

## Structured-output validation failures

Required structured-output validation is part of the agent step attempt. If the agent result is missing required structured output, fails the baseline object schema, fails a `json_schema` validator, or is rejected by a `command` validator, Scherzo first consumes the step's `structured_output.validation_retries` budget when the failure is retryable. That retry is an in-session `StructuredOutputRetryPrompt` to the same agent step; recovery has not started yet. Workflows that want a validator rejection to hand off immediately to step recovery should set `structured_output.validation_retries: 0`.

When there is no retry budget, the retry worker fails, the retry result still fails validation, or the validation error is non-retryable, the agent step attempt is recorded as failed with a `structured_output_*` failure code. From that point it follows the normal fatal-step path: if `on_failure` is fatal and the step has effective `recover` configuration, `workflow_run` records the failed attempt, starts the bounded step-recovery worker, and on a `recheck` decision reruns the original agent step unchanged in the next attempt. A recovery `gave_up` decision preserves the original structured-output failure and fails the workflow as `failed_after_recovery`.

Command validators used as same-step gates follow the same ordering. The validator receives the captured structured-output submission on stdin, runs in the configured working directory for that same step workspace, and can write run-local stamped artifacts under `SCHERZO_RUN_ROOT` before it exits. For a plan-completion verifier, set `validation_retries: 0` when `verdict: fail` should hand control directly to step recovery; otherwise command-validator exit status `1` first triggers the normal structured-output retry prompt. A recovery `recheck` reruns the original verifier step, captures a fresh submission, and reruns the validator so workspace fingerprints are recomputed rather than reused from the failed attempt.

## Current merge scope

Implemented in this slice:

- workflow-level and step-level `recover` parsing;
- shallow merge semantics and `recover.enabled: false` disablement;
- recovery prompt bundling in runtime bundles;
- recovery decision protocol parsing for `recheck` and `gave_up`;
- recording the failed original attempt before recovery starts;
- writing retained structured recovery-input artifacts before launching recovery workers;
- starting a nested recovery worker from `workflow_run` after a recoverable fatal step failure;
- enforcing the configured recovery attempt budget at runtime;
- rechecking the original step unchanged after `recheck`;
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
- `attempts`: int, default `1`; the budget is enforced per step, so `attempts: 2` permits recovery workers after failed step attempts 1 and 2, then a failed attempt 3 exhausts recovery and the run finishes as `failed_after_recovery`
- `model`: optional model override
- `prompt`: required when recovery is enabled

## Protocol and artifact names

Default protocol assets:

- Prompt: `.scherzo/workflows/prompts/recover_failed_step.md`
- Provider schema: `.scherzo/workflows/schemas/provider/workflow-step-recovery-result.v1.schema.json`
- Canonical schema: `.scherzo/workflows/schemas/workflow-step-recovery-result.v1.schema.json`

Retained recovery artifacts live under the failed attempt:

- `runs/<run>/<step-ref>/attempt-<n>.json`
- `runs/<run>/<step-ref>/attempt-<n>/recovery-<m>/workflow_step_recovery_input.json`
- `runs/<run>/<step-ref>/attempt-<n>/recovery-<m>/workflow_step_recovery_result.json`

`<step-ref>` is the collision-resistant sanitized step-artifact path component used for step-scoped retained artifacts.

The recovery input is evidence-oriented. It includes workflow/run/step identifiers, the failed attempt index, a failure summary, diagnostic refs when available, structured-output refs when available, and allowed/forbidden recovery actions. `reason_code` is included only when the failed step artifact already carries a specific failure code.

Durable history records written by the runtime are:

- `workflow_step_recovery_started`
- `workflow_step_recovery_finished`

These records link the failed attempt, recovery attempt number, recovery session id, result (`recheck`, `gave_up`, or a non-recheck diagnostic such as `worker_failed`, `invalid_output`, or `artifact_write_failed`), and optional recheck attempt index.

Use the human session view to inspect that history quickly:

```sh
scripts/scherzoctl session <session-ref>
```

The output appends a `workflow_step_recovery_history` block for the original failed step session, the recheck continuation session, or the nested recovery session. The block links the original failed attempt artifact, the retained recovery result artifact when one was durably written, and the recheck attempt artifact when a recheck was scheduled. A successful recovered timeline looks like:

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
    decision: recheck
    summary: Fixed tests
    reason: The workspace is ready for a recheck.
    recheck_attempt_index: 2
    recheck_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-2.json
    recheck_result: succeeded
    final_workflow_outcome: succeeded_after_recovery
```

With a multi-attempt budget, the same block includes one entry per recovery attempt in the connected step-attempt chain. In this example attempt 1 fails, recovery attempt 1 requests a recheck, attempt 2 fails, recovery attempt 2 requests another recheck, and attempt 3 succeeds:

```text
workflow_step_recovery_history:
  - run_id: run-1
    workflow_id: implementation
    step_id: verify
    failed_attempt_index: 1
    recovery_attempt_number: 1
    recovery_session_id: workflow-run-1-verify-recovery-1
    status: finished
    failed_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-1.json
    recovery_result_artifact_ref: runs/run-1/<step-ref>/attempt-1/recovery-1/workflow_step_recovery_result.json
    decision: recheck
    summary: Patched first failure
    reason: Ready for first recheck.
    recheck_attempt_index: 2
    recheck_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-2.json
    recheck_result: failed
    final_workflow_outcome: succeeded_after_recovery
  - run_id: run-1
    workflow_id: implementation
    step_id: verify
    failed_attempt_index: 2
    recovery_attempt_number: 2
    recovery_session_id: workflow-run-1-verify-recovery-2
    status: finished
    failed_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-2.json
    recovery_result_artifact_ref: runs/run-1/<step-ref>/attempt-2/recovery-2/workflow_step_recovery_result.json
    decision: recheck
    summary: Patched remaining failure
    reason: Ready for second recheck.
    recheck_attempt_index: 3
    recheck_attempt_artifact_ref: runs/run-1/<step-ref>/attempt-3.json
    recheck_result: succeeded
    final_workflow_outcome: succeeded_after_recovery
```

For deeper transcript inspection, replay the nested recovery session directly:

```sh
scripts/scherzoctl events --pretty <recovery-session-id>
```

## Protected recovery checkpoints

Self-healing recovery may edit the normal retained workspace under `StepContext.workspace_path`, but it must not leave ledger-addressed checkpoint artifacts mutated. Scherzo protects these retained artifact paths for the current run:

- `.scherzo-state/artifacts/runs/<run>/<step>/attempt-<n>.json`
- `.scherzo-state/artifacts/runs/<run>/inputs.v1.json`
- `.scherzo-state/artifacts/runs/<run>/outputs.v1.json` when an output manifest was already recorded

If a recovery worker changes or deletes one of those protected files, Scherzo restores the original bytes before the recovery result is accepted and appends the `protected_checkpoint_restored` diagnostic to recovery history. If Scherzo cannot read, hash, or restore a protected checkpoint, recovery stops early with `recovery_artifact_restore_failed` instead of proceeding to a later recheck failure.

Workflow terminal outcomes remain `completed` and `failed_fatal` for clean runs. When same-run step-recovery evidence exists, terminal workflow outcomes may instead be `succeeded_after_recovery` or `failed_after_recovery`.
