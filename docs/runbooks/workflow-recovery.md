# Workflow recovery operator runbook

This runbook explains how to inspect Scherzo recovery meaning, cleanup eligibility, and unsupported local state. It keeps live worker state separate from recovery meaning. A worker `status` such as `running`, `waiting_ui`, or `exited` describes the process; a nullable `recovery` object describes backed durable recovery facts and safe operator actions.

## Status vocabulary

Backed statuses in this release are `recovered`, `interrupted`, `parked`, `cleanup`, and offline `old_state_reset_required`.

`recovered` means Scherzo replayed durable state and restored something such as retry timers, outbox entries, or warnings, but no more specific hold applies to the session being inspected. Safe actions are to inspect the session and view events.

`interrupted` means a run was active without a durable finish record. Live Erlang ports and live pi processes do not survive daemon restart, even if a later live session has a current `pi_session_id`. Safe actions are to inspect, view events, retry if the task is safe to retry, or park the task.

`parked` means dispatch is suppressed for the task. Inspect the park reason, release policy, parked time, and task fingerprint when present. Safe actions are to inspect, view events, or unpark when the reason has been resolved.

`cleanup` is artifact lifecycle state. It means no new runtime work is expected for the artifact and retention classification is `retained`, `eligible`, `deleting`, or `deleted`. It is not proof that the workflow succeeded.

`old_state_reset_required` means local ledger or snapshot schema markers are unsupported by this tree. Backward compatibility is intentionally not provided for unsupported old local state. Safe choices are archive, discard, or reinitialize after old state is moved aside or removed.

Reserved strings are `resumed`, `inspection_needed`, `blocked`, and `drift_detected`. They are documented for stable vocabulary, but this release must not emit them from real projection. They require future durable facts for workflow checkpoints or previous pi sessions, inspection holds, unsafe side-effecting step holds, or drift rejection.

## Inspect recovered or interrupted sessions

Start with the session list:

```sh
scripts/scherzoctl ps
scripts/scherzoctl ps --json
```

The human table has a `RECOVERY` column. A dash means no recovery fact is attached to that summary. JSON includes `recovery: null` or a recovery object.

For a session with `RECOVERY` set, inspect detail:

```sh
scripts/scherzoctl session <session-id>
scripts/scherzoctl session <session-id> --json
scripts/scherzoctl events --pretty <session-id>
```

Look for `recovery.status`, `recovery.source`, `recovery.message`, `recovery.safe_actions`, `workflow_run_id`, and pi session ids. Do not assume a current pi session id is a recovered old pi process. If `status` is `running` and `recovery.status` is `interrupted`, keep those meanings separate: the process status is current process state; the recovery status is durable history or operator guidance.

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

## Inspect cleanup eligibility

Cleanup starts read-only:

```sh
scripts/scherzoctl cleanup
scripts/scherzoctl cleanup --dry-run
scripts/scherzoctl cleanup --json --dry-run
```

Dry run reports `would_delete`, `retained`, `warnings`, `roots`, and `transcript_root_status`. It deletes nothing. Retained artifacts include active, interrupted, parked, old-state-reset-required, unknown, malformed, unsupported, missing-owner, missing-terminal-time, path-unsafe, and symlink-unsafe candidates. Pi transcript deletion is unavailable unless a concrete transcript root is verified.

Review every warning. Warnings are redacted and bounded, but paths and identifiers may still reveal local project names or Linear issue identifiers.

## Apply cleanup

Apply only when the dry run output is expected:

```sh
scripts/scherzoctl cleanup --yes
scripts/scherzoctl cleanup --json --yes
```

Apply classifies all candidates before deleting any file. It only deletes eligible artifacts under verified `<workspace-root>/.scherzo-state/` roots, rejects path escapes and symlink escapes, and writes redacted tombstones below `<workspace-root>/.scherzo-state/cleanup/tombstones/`.

Do not run shell commands such as `rm -rf <workspace-root>/.scherzo-state` as a substitute for cleanup. Manual deletion can remove the only evidence needed to diagnose interrupted or parked recovery.

Cleanup deletion is irreversible unless you have an external backup. Tombstones identify what was deleted, but they do not contain artifact content.

## Inspect unsupported old local state

If daemon startup fails with an unsupported schema message, use offline state status:

```sh
scripts/scherzoctl state status --root <workspace-root>
scripts/scherzoctl state status --root <workspace-root> --json
```

Status values are `current`, `unsupported`, `corrupt`, `missing`, or `archived`. Only `unsupported` is eligible for archive or discard controls. Corrupt or malformed state must be retained for manual inspection.

## Archive unsupported old state

Archive first when you may need evidence later:

```sh
scripts/scherzoctl state archive-old --root <workspace-root> --yes
scripts/scherzoctl state reinitialize --root <workspace-root> --yes
```

Archive moves the active ledger to `<workspace-root>/.scherzo-state/archive/old-state/<id>/ledger/`. Reinitialize then creates the current empty layout: `.scherzo-state/ledger/archive/` and `.scherzo-state/ledger/current.jsonl`.

To restore manually, stop Scherzo, move the archived `ledger/` directory back to `<workspace-root>/.scherzo-state/ledger/`, and run `state status` again.

## Discard unsupported old state

Discard only when the old state is known to be disposable:

```sh
scripts/scherzoctl state discard-old --root <workspace-root> --yes
scripts/scherzoctl state reinitialize --root <workspace-root> --yes
```

Discard is not reversible. It deletes unsupported active ledger state. It refuses current, missing, archived, corrupt, or malformed state.

## Sensitive-data handling

Treat pi transcripts, raw event payloads, prompts, tool inputs, tool outputs, and tracker excerpts as sensitive. Scherzo recovery output uses bounded redacted text for recovery messages, cleanup warnings, old-state reasons, and structured logs, but operators should still avoid pasting full prompts, API tokens, raw tool payloads, or full Linear comment bodies into task comments or public logs.

## Do not do this

Do not infer recovery meaning from live worker `status` alone.

Do not assume `resumed`, `inspection_needed`, `blocked`, or `drift_detected` are emitted statuses in this release.

Do not delete arbitrary worker workspace directories just because a cleanup request exists. The deletion-capable cleanup path is limited to verified `.scherzo-state` artifact roots.

Do not discard corrupt or malformed state through old-state controls. Preserve it for manual inspection.

Do not run destructive commands without reading the dry-run or status output first.
