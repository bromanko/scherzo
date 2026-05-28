# LIV-695 retry-step provenance repair

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Operators should be able to retry a failed or interrupted workflow step even when the retained local ledger is missing the original `workflow_run_started` provenance record. After this change, `scherzoctl retry-step run:<run-id> --step <step-id>` reconstructs missing run provenance from retained local evidence, validates that recovery can actually start, appends an auditable repair record, and then starts the normal recovered workflow. Operators also get an offline `scherzoctl state repair-run-provenance` dry-run and confirmed repair path for inspecting or repairing retained state without hand-editing JSONL.

## Problem Framing and Constraints

Today `workflow_repair.plan` rejects missing `workflow_run_provenance(run_id)` with `workspace_recovery_failed`, even when the ledger still has terminal workflow records, workflow input manifests, step attempts, workspace roots, and task refs. Manual repair by appending a fake historical `workflow_run_started` hides what actually happened and can accidentally change retained run status. The repair must be auditable, additive, safe to repeat, and must not report retry success when the current tracker issue state is non-active such as `Triage`.

## Strategy Overview

Add a new ledger body `WorkflowRunProvenanceRepaired` with the same provenance fields as `WorkflowRunStartedWithTask`, plus a `repair_mode` string and a list of source evidence identifiers. Projection applies this record only to `workflow_run_provenances` and `workflow_task_refs`; it does not change `workflow_runs`. `workflow_repair` first tries the existing provenance projection, then reconstructs a single candidate from terminal workflow status, input manifest, step attempts, task refs, and current issue evidence. `retry-step` appends the repair record only after recovery finalization accepts a resumption, before the normal retry `workflow_run_started` record. The CLI state repair path uses the same reconstruction and appends only the repair record when confirmed.

## Alternatives Considered

The smallest workaround is to keep appending synthetic `workflow_run_started` records. That is insufficient because it mutates historical meaning and makes offline repairs indistinguishable from real starts. A larger migration that rewrites old ledgers is unnecessary for v1; additive repair records let old records continue to decode and project safely.

## Risks and Countermeasures

The main risk is reconstructing the wrong run. The implementation rejects missing, conflicting, or drifted evidence with specific reasons and no mutation. The second risk is appending a repair record before discovering that artifacts or workspaces are unusable. `retry-step` avoids this by running recovery finalization before any retry repair records are appended. The third risk is a retry that immediately stops because the Linear issue is in a failure or non-active state. `retry-step` now rejects non-active states up front with an actionable message.

## Progress

- [x] (2026-05-28 00:00Z) Wrote the concise design note before coding.
- [x] (2026-05-28 23:35Z) Added the repair ledger record encoding, decoding, and projection semantics.
- [x] (2026-05-28 23:35Z) Added provenance reconstruction and retry-step append ordering.
- [x] (2026-05-28 23:35Z) Added explicit offline `state repair-run-provenance` dry-run and confirmed repair output.
- [x] (2026-05-28 23:35Z) Added focused tests for reconstruction, projection, CLI idempotency, finalization rejection, and issue-state rejection.

## Surprises & Discoveries

- Observation: `gleam test` in this repository does not accept individual test file paths; the deterministic unit suite is selected with `gleam test -- --suite unit`.
  Evidence: `direnv exec . gleam test --target erlang test/workflow_repair_test.gleam ...` returned the project test-runner usage text, while `direnv exec . gleam test -- --suite unit` passed.

## Decision Log

- Decision: Use `WorkflowRunProvenanceRepaired` instead of synthetic `WorkflowRunStartedWithTask` for offline provenance repair.
  Rationale: The repair is auditable and projection can restore provenance without changing the retained terminal or interrupted status.
  Date: 2026-05-28.

- Decision: Append automatic repair records only after recovery finalization returns exactly one resumption.
  Rationale: Missing artifacts, corrupt artifacts, missing workspaces, and invalid run roots must reject without mutating provenance.
  Date: 2026-05-28.

- Decision: Reject non-active current issue states during retry-step v1 rather than silently claiming or moving the issue.
  Rationale: Silent state transitions are out of scope and previously allowed doomed recoveries from `Triage`.
  Date: 2026-05-28.

## Outcomes & Retrospective

The implementation is complete for v1: missing workflow provenance can be repaired automatically after recovery finalization accepts a retry-step resumption, and explicit local repair supports dry-run, confirmed, and idempotent outcomes. Projection restores provenance and task refs from the repair record without changing retained terminal or interrupted run status. Retry-step now rejects non-active or terminal issue states before appending retry records or spawning recovery.

## Context and Orientation

The local ledger record model lives in `src/scherzo/state/record.gleam` and record-specific JSON entry helpers live in `src/scherzo/state/record/workflow_runs.gleam`. Projection is in `src/scherzo/state/projection.gleam`; it derives `workflow_run_provenances`, `workflow_task_refs`, workflow run status, manifests, and step attempts from ledger records. Step retry planning is in `src/scherzo/workflow_repair.gleam`. The daemon operator path for `retry-step` is in `src/scherzo/orchestrator/daemon.gleam`. The `scherzoctl` parser and offline state commands are in `src/scherzo/ctl.gleam`.

## Preconditions and Verified Facts

The current tree already records workflow starts, terminal records, input/output manifests, step attempts, step recovery records, known workspaces, and task refs. `workflow_repair.plan` builds a `recovery.WorkflowRecoveryCandidate` and the daemon calls `recovery.finalize_workflow_candidates_with_config` before appending retry records. Projection currently preserves provenance across terminal statuses only when an original start record exists.

## Scope Boundaries

In scope: retry-step repair for a retained failed or interrupted workflow run, explicit offline repair for one run id, record/projection support, issue-state rejection, and focused tests. Out of scope: whole-run `scherzoctl retry` repair, automatic Linear claiming or state transitions, and manual ledger surgery as a supported workflow.

## Milestones

First, add the record and projection behavior so repaired provenance can be represented without changing run status. Second, teach workflow repair to reconstruct and validate a single provenance candidate and to include the repair record in accepted retry-step append batches. Third, add the CLI state repair command with dry-run, confirmed, and idempotent results. Fourth, update daemon issue-state behavior and tests.

## Plan of Work

In `src/scherzo/state/record.gleam`, add `WorkflowRunProvenanceRepaired` with fields `run_id`, `workflow_id`, `workflow_fingerprint`, `issue_id`, `issue_identifier`, `task_ref`, `issue_fingerprint`, `observed_updated_at_ms`, `run_root`, `repair_mode`, and `source_evidence`. Encode it as kind `workflow_run_provenance_repaired` and decode the same fields. In `src/scherzo/state/projection.gleam`, add a fold case that inserts `WorkflowRunProvenance` and the task ref only.

In `src/scherzo/workflow_repair.gleam`, reconstruct provenance only when projection lookup is missing. Reject incomplete or ambiguous evidence. Add `retry_step_auto` repair records to the plan when reconstruction was needed. Keep the existing retry `WorkflowRunStartedWithTask` append in the accepted retry batch.

In `src/scherzo/orchestrator/daemon.gleam`, reject retry-step when the current issue state is terminal or not one of the configured active states, and remove the additional-active-state finalization shortcut. In `src/scherzo/ctl.gleam`, parse and run `state repair-run-provenance run:<run-id> --root <workspace-root> --dry-run|--yes --json` using the shared reconstruction path.

## Concrete Steps

From the repository root, run validation through direnv where available:

    direnv exec . gleam test -- --suite unit
    direnv exec . gleam test -- --suite contract
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. Do not commit from this workflow workspace; Scherzo publishes after validation.

## Testing and Falsifiability

Add record round-trip coverage for `workflow_run_provenance_repaired`. Add projection coverage proving a repaired interrupted run still projects as interrupted while provenance and task refs are restored. Add workflow repair coverage for successful missing-provenance reconstruction, ambiguous/conflicting fallback evidence, incomplete missing run root, drift against current issue/workflow, and that finalization rejection does not require appending the repair record. Add CLI coverage for dry-run, confirmed repair, and idempotent repeat. Update daemon retry-step coverage so `Triage` rejects with an actionable message and no ledger mutation.

## Validation and Acceptance

Acceptance is demonstrated when the targeted tests pass and operator output includes run id, repair status, rejection reason or next action, and no secret-bearing local paths beyond existing workspace-root arguments. A manual smoke shape is: `scherzoctl state repair-run-provenance run:<run-id> --root <workspace-root> --dry-run --json` reports `would_repair`, then `--yes --json` reports `repaired`, and a second confirmed run reports `already_repaired`.

## Rollout, Recovery, and Idempotence

The change is additive to schema version 2 and does not require rewriting old ledgers. If a repair is wrong or must be backed out, removing the single repair record returns projection to the prior missing-provenance state without altering terminal workflow status. Confirmed explicit repair is idempotent because it does not append when projection already has the repaired provenance.

## Artifacts and Notes

No external artifacts are required.

## Interfaces and Dependencies

No new dependencies are needed. The new public repair mode strings are `retry_step_auto` and `state_repair_explicit`. The explicit CLI result contract uses JSON fields `command`, `status`, `run_id`, `repair_status`, `repair_mode`, `source_evidence`, `reason`, and `message`.
