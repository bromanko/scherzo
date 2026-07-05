# Workflow recovery operator runbook

This runbook explains how to inspect Scherzo recovery meaning, cleanup eligibility, and unsupported local state. It keeps live worker state separate from recovery meaning. A worker `status` such as `running`, `waiting_ui`, or `exited` describes the process; a nullable `recovery` object describes backed durable recovery facts and safe operator actions.

## Status vocabulary

Backed statuses in this release are `recovered`, `interrupted`, `parked`, `cleanup`, and offline `old_state_reset_required`.

`recovered` means Scherzo replayed durable state and restored something such as retry timers, outbox entries, or warnings, but no more specific hold applies to the session being inspected. Safe actions are to inspect the session and view events. When outbox counters remain non-zero, use the [outbox drill-down](outbox-drilldown.md) runbook.

`interrupted` means a run was active without a durable finish record. Live Erlang ports and live pi processes do not survive daemon restart, even if a later live session has a current `pi_session_id`. Safe actions are to inspect, view events, retry if the task is safe to retry, or park the task.

`parked` means dispatch is suppressed for the task. Inspect the park reason, release policy, parked time, and task fingerprint when present. Safe actions are to inspect, view events, or unpark when the reason has been resolved.

`cleanup` is artifact lifecycle state. It means no new runtime work is expected for the artifact and retention classification is `retained`, `eligible`, `deleting`, or `deleted`. It is not proof that the workflow succeeded. When a workflow already succeeded but post-success workspace cleanup fails, Scherzo keeps the successful terminal outcome, appends a durable `workflow_run_diagnostic`, emits a live `recovery_cleanup` event, and retains the run-local managed-workspace manifest at `<run-root>/.scherzo/managed-workspaces.json` as the cleanup source of truth.

`old_state_reset_required` means local ledger or snapshot schema markers are unsupported by this tree. Backward compatibility is intentionally not provided for unsupported old local state. Safe choices are archive, discard, or reinitialize after old state is moved aside or removed.

Reserved strings are `resumed`, `inspection_needed`, `blocked`, and `drift_detected`. They are documented for stable vocabulary, but this release must not emit them from real projection. They require future durable facts for workflow checkpoints or previous pi sessions, inspection holds, unsafe side-effecting step holds, or drift rejection.

## Inspect recovered or interrupted sessions

Start with the session list:

```sh
scripts/scherzoctl ps
scripts/scherzoctl ps --json
```

The human table has a `RECOVERY` column. A dash means no recovery fact is attached to that summary. JSON includes `recovery: null` or a recovery object. For YAML workflow runs, the top-level run session reflects child-step last-event, turn, and token progress so operators can see that the run is moving; child step sessions still remain separate rows and JSON summaries for step-specific details.

For a session with `RECOVERY` set, inspect detail:

```sh
scripts/scherzoctl session <session-id>
scripts/scherzoctl session <session-id> --json
scripts/scherzoctl events --pretty <session-id>
```

Look for `recovery.status`, `recovery.source`, `recovery.message`, `recovery.safe_actions`, `workflow_run_id`, and pi session ids. When the session belongs to a workflow step run or nested recovery run, the human `session` output also appends `workflow_step_recovery_history`, which summarizes the failed attempt, recovery decision, retry attempt, and recovered terminal workflow outcome when known. Do not assume a current pi session id is a recovered old pi process. If `status` is `running` and `recovery.status` is `interrupted`, keep those meanings separate: the process status is current process state; the recovery status is durable history or operator guidance.

If retrying could repeat an unsafe external side effect, park the task first:

```sh
scripts/scherzoctl park <task> --reason "operator inspection after interrupted recovery" --yes
```

## Handle parked tasks

When a session or task shows `parked`, read the recovery details:

```sh
scripts/scherzoctl session <session-id>
```

Check `park_reason`, `park_release_policy`, and `parked_at_ms`. If the task was parked by operator policy, unpark only after the reason is resolved:

```sh
scripts/scherzoctl unpark <task>
```

If a parked task exists because of retry caps, inspect recent events and handoff comments before unpark or retry. Do not unpark solely because the live worker list is quiet.

## Repair interrupted retained workflow runs

Common retry now has three operator-facing entry points. Use `retry step <target>` for the common lattice path that picks the deepest safe retained boundary Scherzo can prove, use `retry all <task>` or `task retry <task> --from-scratch --reason <text>` when you want to force a fresh superseding run, and keep `run retry-step <run-id> --step <step-id>` as the exact fail-closed expert override.

Use `retry step` or `run retry-step` when a retained workflow run has durable completed upstream artifacts and a failed or interrupted repair boundary that you want to rerun in place instead of redispatching the full task:

```sh
scripts/scherzoctl retry step <task|run:run-id> --step <step-id> --json
scripts/scherzoctl run retry-step <run-id> --step <step-id> --json
scripts/scherzoctl query operation-status <operation-id> --json
```

A successful acknowledgement is now durable and asynchronous: `retry-step` returns `status: queued` plus `operation_id` after Scherzo records the control intent, and the slow repair runs afterward. Poll `query operation-status <operation-id>` for `queued`, `running`, `completed`, or `failed` status.

If you want Scherzo to wait for completion, add `--wait --timeout <duration>` to the operation-status read. When the wait budget expires but the accepted operation is still queued or running, `--json` stays machine-readable and returns `ok: true` with `wait.timed_out: true`, `wait.phase: "operation_wait"`, and a safe follow-up command instead of a blind retry. Use `SCHERZO_CTL_TIMEOUT` to change the default request budget and `SCHERZO_CTL_WAIT_TIMEOUT` when you want a longer default wait budget without changing the initial admission timeout.

Mutating controls now treat the admission boundary as the idempotency boundary. `pause`, `resume`, `reload`, `task retry`, `run retry-step`, `run recollect-outputs`, `run finalize`, `publication retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`, `schedules run`, and `recovery cleanup-orphan-steps` accept the same timeout options where they use the daemon control channel. If the client loses the response after sending one of those requests, JSON reports `phase: "operation_admission"`, `accepted: "unknown"`, and `retryable: false`; the suggested command is a safe read such as `query status`, `task show`, `events`, or `ps`, never a blind repeat of the mutation. Successful queued or applied command results include `accepted: true` and keep any durable `operation_id` so operators can poll `query operation-status`.

Timeout diagnostics intentionally reuse existing operator-visible surfaces rather than adding provider-live state. The new CLI-local timeout branches are observable through structured JSON/human output and through tests that cover `control_file_discovery`, `daemon_connect`, `request_round_trip`, `daemon_actor_query`, `operation_admission`, and `operation_wait`. No provider-live behavior, remote-provider cache behavior, cache TTL, or invalidation policy changes in this rollout; read commands return the policy timeout error when no safe stale projection is available instead of inventing stale data.

Common `retry step` is total for non-held, non-terminal, non-paused, non-duplicate retained runs: when exact repair is no longer provable, Scherzo rewinds to the deepest verified prefix or supersedes the retained run and starts fresh. The exact `run retry-step` expert override remains fail-closed and is accepted only when workflow identity, issue identity, task identity, run root, retained artifacts, and required source workspaces still match the current world. Stable exact-override rejection reasons include `workflow_drift`, `issue_drift`, `artifact_recovery_failed`, and `workspace_recovery_failed`.

When the exact override rejects with `artifact_recovery_failed`, the result message names the step, retained artifact ref, and failure reason. A hash mismatch also includes the ledger sha and current file sha, for example:

```json
{
  "status": "rejected",
  "command": "retry_step",
  "reason": "artifact_recovery_failed",
  "message": "retry-step repair was rejected by recovery validation: artifact_recovery_failed: step_id=incorporate_review artifact_ref=runs/run-1/incorporate_review-edd312798e29/attempt-1.json reason=sha_mismatch expected_sha256=<ledger-sha> current_sha256=<current-sha>"
}
```

Interpret `reason=missing` as an artifact ref that is absent under `.scherzo-state/artifacts`, `reason=unreadable` as a local permission or I/O failure, `reason=invalid_json` as retained artifact bytes that match the ledger hash but are not a valid step artifact, and `reason=sha_mismatch` as retained bytes that no longer match the ledger `artifact_sha256`. Fix or restore the named artifact from backup or from the retained workspace evidence, then rerun `retry-step`. The same bounded detail is retained as a projection-neutral `workflow_run_diagnostic` ledger record so later operators can inspect the failed repair without recomputing hashes manually.

If the issue is parked, unpark it first and then rerun `retry-step`. `retry-step` does not silently override operator park policy. `retry-step` now also fails closed when the refreshed issue state is non-active, with a message naming the run and current state; move the issue back to a configured active state before retrying. Terminal states are still rejected.

Use `run recollect-outputs` when every workflow step already succeeded but the workflow output manifest is missing or invalid and you need a fresh output record without rerunning steps:

```sh
scripts/scherzoctl run recollect-outputs <run-id> --json
```

This command is output-only. Once the daemon has validated the run target and durably recorded the operation intent, it returns `status: "queued"` with an `operation_id`; poll the durable status path to follow completion:

```sh
scripts/scherzoctl query operation-status <operation-id> --json
```

The asynchronous operation validates the current workflow and issue identity, reuses retained step artifacts and workspaces, writes fresh output blobs and the output manifest under `runs/<run-id>/recollections/<n>/...`, appends a fresh `workflow_run_outputs_recorded` record, and does not spawn a worker, append a new successful terminal `workflow_run_finished`, retry publication, or change provider-live/cache behavior.

Stable asynchronous failure reasons include `workflow_drift`, `issue_drift`, `artifact_recovery_failed`, `workspace_recovery_failed`, and `run_not_complete`; inspect `query operation-status`, the ledger, and session diagnostics for the retained reason/message. A successful no-op completes the operation with a message pointing at the already-valid latest manifest, for example:

```json
{
  "operation_id": "recollect-outputs:run-1:1234",
  "operation_kind": "recollect_outputs",
  "status": "completed",
  "message": "workflow outputs already valid for run-1: runs/run-1/outputs.v1.json"
}
```

When retained evidence is missing, expect source-specific failed operation reasons. For example, missing or mismatched step artifacts fail with `artifact_recovery_failed`, and missing step workspaces for `source.path` outputs fail with `workspace_recovery_failed`. Restore the named artifact or workspace first, then rerun `recollect-outputs`. If publication failed separately, run publication retry only after recollection has produced a valid output manifest. If retained evidence cannot be restored safely, fall back to a full task retry instead of forcing output recollection.

When a retained run already has orphaned YAML child step sessions, inspect `ps --json` or `session --json` for `workflow_run_id`, `workflow_step_id`, `workflow_attempt_index`, `orphan_status`, `issue_state`, and `recommended_action`, then run:

```sh
scripts/scherzoctl recovery cleanup-orphan-steps run:<run-id> --dry-run
scripts/scherzoctl recovery cleanup-orphan-steps run:<run-id> --yes
```

Dry run is the default. `--yes` is the only mutating mode, and rerunning either form is expected to be idempotent.

Use `retry all`, `task retry --from-scratch`, or the older explicit `--start-fresh` spelling when retained drift blocked the old run and you need Scherzo to start a fresh run from the current task payload and current workflow definition instead of repairing the retained run:

```sh
scripts/scherzoctl retry all <task> --json
scripts/scherzoctl task retry <task> --from-scratch --reason "workflow drift" --json
scripts/scherzoctl task retry <task> --start-fresh --reason "workflow drift" --json
```

The response message explicitly says that Scherzo `starts a fresh run`. This path clears only retained drift or recovery-blocked state; it does not silently override arbitrary operator holds.

Use `run finalize` when a retained run already has the evidence you need and the remaining work is validation adoption, outputs, publication, and tracker completion without starting a new worker:

```sh
scripts/scherzoctl run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason "operator salvage" --dry-run --json
scripts/scherzoctl run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason "operator salvage" --yes --json
```

`--dry-run` is immediate and read-only. `--yes` queues durable daemon-owned retained-run finalization work and returns an `operation_id` for `query operation-status` polling.

If drift or retained artifact recovery cannot be proven safe, fall back to manual salvage or a full task retry with `scripts/scherzoctl task retry <task>`.

The historical LIV-509 retry remains deferred operator evidence after publish, not a pre-publish gate for code changes.

## Inspect cleanup eligibility

Cleanup starts read-only:

```sh
scherzo cleanup
scherzo cleanup --dry-run
scherzo cleanup --json --dry-run
scherzo cleanup --json --dry-run --limit 100 --max-runtime-ms 240000
scherzo cleanup --json --yes --limit 100 --max-runtime-ms 240000 --cursor <opaque-cursor>
```

Dry run reports `would_delete`, `retained`, `warnings`, `roots`, and `transcript_root_status`. When bounded options are present it also reports `truncated`, `next_cursor`, `cursor`, `limit`, `max_runtime_ms`, `summary.scanned`, `summary.applied`, and `summary.truncated_reason`. It deletes nothing. Retained artifacts include active, interrupted, parked, old-state-reset-required, unknown, malformed, unsupported, missing-owner, missing-terminal-time, path-unsafe, and symlink-unsafe candidates. Pi transcript deletion is unavailable unless a concrete transcript root is verified.

The scheduled dogfood cleanup workflow runs `scherzo cleanup --provider workspaces --json --yes --limit 100 --max-runtime-ms 240000` under a 5 minute command timeout and persists its resume cursor at `<workspace-root>/.scherzo-state/cleanup/scheduled/workspace-cleanup.cursor`. When bounded output reports `truncated=true` with `next_cursor`, the workflow writes that opaque cursor atomically; the next scheduled run passes it with `--cursor` and continues cleanup. If a run reports truncation without a cursor, there is no safe resume point, so the next scheduled run starts another bounded pass from the beginning rather than extending the current command step. When no cursor remains, the workflow clears the file so later scheduled runs start a fresh pass. Operators may remove that file to restart scheduled cleanup from the beginning. Cursors are opaque and root-bound; malformed or wrong-root cursors fail closed for direct `scherzo cleanup` runs, and the scheduled workflow clears its saved cursor instead of deleting from an untrusted position.

Review every warning. Warnings are redacted and bounded, but paths and identifiers may still reveal local project names or Linear issue identifiers.

## Apply cleanup

Apply only when the dry run output is expected:

```sh
scherzo cleanup --yes
scherzo cleanup --json --yes
scherzo cleanup --json --yes --limit 100 --max-runtime-ms 240000
```

Apply classifies all candidates before deleting any file. It only deletes eligible artifacts under verified `<workspace-root>/.scherzo-state/` roots, rejects path escapes and symlink escapes, and writes redacted tombstones below `<workspace-root>/.scherzo-state/cleanup/tombstones/`. `--max-runtime-ms` stops only at item boundaries; once local tombstone/delete work or delegated workspace removal starts, that item is allowed to finish and the report explains partial progress with `truncated_reason`.

Workspace cleanup still delegates deletions through `workspace_run.cleanup_run` and the configured workspace driver lifecycle remove hook; generic cleanup must not delete run roots directly. Provider-live state and remote-provider-cache data remain explicit unavailable boundary providers in cleanup output and are not mutated by generic cleanup.

A `.scherzo-keep-workspace` file is a fail-closed hold. Legacy prose-only files remain indefinite manual holds. Schema-backed markers with `Schema: scherzo.retained-workspace.v1` use `Review state:` values `publication_guard`, `safe_to_delete`, `manual_hold`, or `abandoned`. `publication_guard` markers can age out only after their stale window elapses and active, parked, interrupted, or publication-required hard holds are gone.

Do not run shell commands such as `rm -rf <workspace-root>/.scherzo-state` as a substitute for cleanup. Manual deletion can remove the only evidence needed to diagnose interrupted or parked recovery.

Cleanup deletion is irreversible unless you have an external backup. Tombstones identify what was deleted, but they do not contain artifact content.

## Inspect unsupported old local state

If daemon startup fails with an unsupported schema message, use offline state status:

```sh
scherzo state status --root <workspace-root>
scherzo state status --root <workspace-root> --json
```

Status values are `current`, `unsupported`, `corrupt`, `missing`, or `archived`. Only `unsupported` is eligible for archive or discard controls. Corrupt or malformed state must be retained for manual inspection.

## Archive unsupported old state

Archive first when you may need evidence later:

```sh
scherzo state archive-old --root <workspace-root> --yes
scherzo state reinitialize --root <workspace-root> --yes
```

Archive moves the active ledger to `<workspace-root>/.scherzo-state/archive/old-state/<id>/ledger/`. Reinitialize then creates the current empty layout: `.scherzo-state/ledger/archive/` and `.scherzo-state/ledger/current.jsonl`.

To restore manually, stop Scherzo, move the archived `ledger/` directory back to `<workspace-root>/.scherzo-state/ledger/`, and run `state status` again.

## Discard unsupported old state

Discard only when the old state is known to be disposable:

```sh
scherzo state discard-old --root <workspace-root> --yes
scherzo state reinitialize --root <workspace-root> --yes
```

Discard is not reversible. It deletes unsupported active ledger state. It refuses current, missing, archived, corrupt, or malformed state.

## Execplan plan-completion recovery

`workflow:execplan-implementation` can stop with `plan_completion_recovery_exhausted` either after the bounded automatic late repair budget is spent at the pre-review gate or when the final pre-publish diagnostic gate still reports unmet promised plan behavior.

When that happens, inspect these retained-workspace artifacts first:

```sh
tmp/scherzo-plan-completion-recovery.md
tmp/scherzo-plan-completion-verdict.json
```

The recovery summary names the failure phase, blocking findings, retention status, and the recommended full-workflow retry command. Inspect or salvage the retained workspace manually if needed, then use the retry command shown in the artifact — normally `scherzoctl task retry <issue>` — when it is safe to rerun the full workflow.

Missing, malformed, or stale plan-completion verdict failures are not repairable through this path; they remain terminal verifier or workflow-state failures. Same-run resume from exhausted plan-completion recovery is intentionally out of scope for this MVP.

For the earlier ExecPlan structural checks that run before implementation starts, see `docs/runbooks/execplan-completion-preflight.md`.

## Sensitive-data handling

Treat pi transcripts, raw event payloads, prompts, tool inputs, tool outputs, and tracker excerpts as sensitive. Scherzo recovery output uses bounded redacted text for recovery messages, cleanup warnings, old-state reasons, and structured logs, but operators should still avoid pasting full prompts, API tokens, raw tool payloads, or full Linear comment bodies into task comments or public logs.

## Do not do this

Do not infer recovery meaning from live worker `status` alone.

Do not assume `resumed`, `inspection_needed`, `blocked`, or `drift_detected` are emitted statuses in this release.

Do not delete arbitrary worker workspace directories just because a cleanup request exists. The deletion-capable cleanup path is limited to verified `.scherzo-state` artifact roots.

Do not discard corrupt or malformed state through old-state controls. Preserve it for manual inspection.

Do not run destructive commands without reading the dry-run or status output first.
