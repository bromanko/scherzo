# Hardening 03: Add single-instance crash recovery from the local ledger

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, one Scherzo daemon using one canonical workspace root can restart after an unexpected process exit and recover local scheduler facts from the durable ledger. On startup, Scherzo reloads durable retry counters, session counters, parked issues, retry timers, known workspace paths, interrupted runs, and pending Linear outbox work. Previously running workers are marked interrupted because live pi sessions cannot be recovered. Active interrupted issues are retried or parked according to the same caps as ordinary failures; terminal interrupted issues have their known workspaces cleaned up. Overdue retry timers are scheduled immediately or after a small deterministic startup delay.

The visible proof is a deterministic recovery test that starts a daemon, records a running issue and a parked issue in the ledger, simulates process death by starting a new daemon from the same workspace root, and observes that the new daemon preserves the parked issue, marks the old run interrupted, schedules/rejects retry according to caps, and does not dispatch duplicate work before recovery reconciliation completes.

This phase is single-instance recovery only. It does not recover live pi sessions, does not make EventHub history durable, does not process Linear command comments posted while Scherzo was down, and does not allow multiple hosts or workspace roots to process the same Linear project safely.

## Problem Framing and Constraints

Scherzo currently rebuilds itself from Linear and the filesystem after restart. That is safe enough for early operation, but it loses important runtime state: retry timers, retry/session counters, parked issues, processed command ids, pending acknowledgement status, and knowledge that a worker was in progress. After a crash, an active Linear issue can be dispatched again immediately even if it had been parked or had just failed repeatedly enough to hit a cap. A completed issue's workspace can remain on disk because Scherzo does not have a durable list of known terminal workspaces to clean on startup.

The previous hardening plan adds a local JSONL ledger and replay projection under `workspace.root/.scherzo-state/ledger/`. This plan wires that ledger into daemon startup and daemon transitions. The daemon remains the only process that mutates scheduler state. The ledger records facts before or at the same point as the corresponding in-memory transition so a later process can make conservative recovery decisions.

The recovery rule is conservative: never assume a live worker or pi session survived a BEAM restart. Every ledger run that was started but not finished is treated as interrupted. Recovery should either retry it through the normal dispatch path, park it if caps are exhausted, or clean its workspace if Linear says the issue is terminal.

## Strategy Overview

Add a recovery module under `src/scherzo/state/recovery.gleam`. It consumes the ledger projection and current workflow config, fetches current Linear state for durable issue ids, and produces a recovered `domain.RuntimeState` plus daemon effects to schedule retries, cleanup terminal workspaces, replay pending outbox items, and log interrupted runs. It does not spawn workers directly.

Wire ledger emission into the daemon at key transition points:

- after handoff claim succeeds and before spawning a worker, append `RunStarted`;
- when a worker finishes successfully or fails, append `RunFinished` with final classification or failure code before reporting handoff;
- when a worker is killed during daemon shutdown or disappears across restart, append or synthesize `RunInterrupted`;
- when retry timers are scheduled or cancelled, append retry records;
- when an issue is parked or unparked, append parking records;
- when issue counters change, append a counter record if counters cannot be derived exactly from run records;
- before a Linear side effect that must be replayable, append `OutboxPending`; after it succeeds, append `OutboxCompleted`; after a final non-retryable failure, append `OutboxFailed`.

On daemon startup, after config resolution and before scheduling the first poll tick, load the ledger. If replay fails because of a corrupt non-trailing record, startup fails clearly. If replay reports a truncated tail, startup logs a warning and continues from the valid prefix. Recovery then fetches current Linear state for issue ids present in durable running/retry/parked/counter records. It reconstructs runtime state conservatively and schedules due retries.

Keep startup recovery bounded. It should inspect only issue ids known from the local ledger, not scan all terminal issues in the Linear project.

## Alternatives Considered

One alternative is to keep relying on Linear state only. That is simple but loses parked state and retry counters, and it cannot distinguish an interrupted Scherzo worker from a never-started issue.

Another alternative is to persist the entire `domain.RuntimeState` snapshot and reload it directly. That couples durable format to current in-memory types and risks restoring stale process/session handles. A fact ledger plus recovery projection is safer because process-owned fields are intentionally not persisted.

A third alternative is to recover live pi sessions by reattaching to pi. That is out of scope. Current workers own Erlang ports and pi RPC sessions, and those disappear when the BEAM process exits.

A fourth alternative is to scan every terminal Linear issue on startup and cleanup matching workspaces. That can be expensive on real boards and is unnecessary for single-instance recovery. This plan cleans only workspaces known from the local ledger.

## Risks and Countermeasures

The main duplication risk is crashing after a worker completed but before Scherzo recorded completion. Recovery will see a started run without a finish record and may retry. Countermeasure: append `RunFinished` immediately when the daemon receives `WorkerFinished`, before handoff reporting. This reduces but cannot eliminate the tiny window before the append. The result is at-least-once work, not exactly-once.

The main outbox risk is duplicating Linear comments after a crash between Linear success and `OutboxCompleted`. Countermeasure: outbox records include a stable dedupe key containing run id, issue id, outbox kind, and source comment id when applicable. Replayed comments include the same run id/source id so duplicates are auditable. True Linear-side idempotency is deferred unless Linear exposes a stable client mutation id.

The main state risk is restoring stale running entries. Countermeasure: recovery never restores live workers. It marks incomplete runs as interrupted and schedules retry or parking through normal core logic.

The main startup latency risk is fetching too many issues. Countermeasure: fetch only ledger-known issue ids and chunk requests through `tracker.fetch_issue_states_by_ids`. Do not scan project history.

The main compatibility risk is missing facts needed to reconstruct counters exactly. Countermeasure: add explicit `IssueCounterUpdated` or equivalent ledger records in this plan if run records are insufficient. Tests must prove recovered counters enforce `max_retry_attempts` and `max_sessions_per_issue` after restart.

The main stale-lock risk remains manual after an abrupt kill. Countermeasure: document that if the old process left `instance.lock`, the operator must still verify no process is running and remove the stale lock before recovery can start. Automatic stale-lock takeover is intentionally not part of this plan.

## Progress

- [x] (2026-04-29 04:20Z) Drafted this plan to consume the durable ledger from `hardening-02-local-durable-state-ledger.md`.
- [ ] Add recovery projection tests for interrupted, parked, retrying, and completed issues.
- [ ] Append ledger records from daemon state transitions.
- [ ] Load the ledger during daemon startup and reconstruct recoverable runtime state.
- [ ] Replay pending Linear outbox items safely.
- [ ] Add bounded known-workspace cleanup for terminal recovered issues.
- [ ] Update README with restart recovery behavior and remaining limits.

## Surprises & Discoveries

(To be filled during implementation. Record any daemon transition that lacks enough data for a durable record and any Linear outbox duplication risk discovered.)

## Decision Log

- Decision: Treat all started-but-unfinished runs as interrupted on restart.
  Rationale: Scherzo cannot recover live Erlang ports or pi sessions. Retrying or parking is safer than pretending the worker is still running.
  Date: 2026-04-29

- Decision: Recover only issue ids known from the local ledger.
  Rationale: This avoids unbounded Linear scans and keeps recovery tied to one workspace root's durable facts.
  Date: 2026-04-29

- Decision: Preserve parked issues across restart.
  Rationale: Parking is a safety cap. Losing it on restart can immediately resume token-spending loops that Scherzo intentionally stopped.
  Date: 2026-04-29

- Decision: Keep stale instance-lock cleanup manual in this phase.
  Rationale: Automatically proving that a lock is stale across hosts/filesystems is a separate safety problem. Graceful lifecycle reduces normal stale locks; this plan handles recovery after the operator has safely restarted.
  Date: 2026-04-29

## Outcomes & Retrospective

(To be filled at completion. Include final recovery rules, final ledger records emitted by daemon, final test count, and any known at-least-once duplicate windows that remain.)

## Context and Orientation

The daemon actor in `src/scherzo/orchestrator/daemon.gleam` owns runtime state. It starts with `core.new_state(effective)`, schedules an immediate poll tick, fetches candidates, dispatches workers, handles `WorkerFinished` and `WorkerDown`, schedules retry timers, and publishes session events to the in-memory EventHub. The pure scheduler in `src/scherzo/orchestrator/core.gleam` updates `domain.RuntimeState` and emits effects.

The local durable ledger from `hardening-02-local-durable-state-ledger.md` lives under `workspace.root/.scherzo-state/ledger/`. It records versioned JSONL facts and can replay into a projection. This plan wires daemon transitions to that ledger and uses replay during daemon startup.

`domain.RuntimeState` contains process-local fields such as running workers and retry timers. Recovery must reconstruct only safe logical state: claims, counters, parked entries, completed issues, retry entries, and known workspaces. It must not try to restore Erlang pids, monitor refs, worker command subjects, EventHub subjects, or control server state.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/hardening-01-graceful-daemon-lifecycle.md` is complete or consciously deferred.
- `docs/plans/hardening-02-local-durable-state-ledger.md` is complete.
- `src/scherzo/state/ledger.gleam`, `src/scherzo/state/record.gleam`, and `src/scherzo/state/projection.gleam` exist and have passing tests.
- `direnv exec . gleam test` passes.
- The daemon still starts from one canonical workspace root guarded by `instance.lock`.

If the ledger cannot replay due to a corrupt non-trailing record, do not implement best-effort recovery on top of bad data. Fail startup clearly and instruct the operator to back up and inspect the ledger.

## Scope Boundaries

In scope: ledger emission from daemon transitions; startup ledger replay; reconstruction of retry counters, session counters, parked issues, retry entries, completed known issues, and aggregate token totals where feasible; interrupted-run classification; due retry rescheduling; known terminal workspace cleanup; Linear outbox pending/completed replay; deterministic recovery tests.

Out of scope: live pi session recovery; durable EventHub archive; Linear command comments posted while Scherzo was down; automatic stale-lock takeover; multi-host or multi-workspace-root exactly-once; global Linear terminal scans; schema migration beyond the current ledger version.

## Milestones

Milestone 1 adds pure recovery planning. At the end, tests can feed ledger projections and fake current Linear issue states into `state/recovery.gleam` and assert recovered runtime state and effects without starting a daemon.

Milestone 2 records daemon transitions. At the end, daemon tests can run dispatch, worker success, worker failure, retry scheduling, park/unpark, and handoff side effects, then inspect ledger records for the expected facts.

Milestone 3 wires startup recovery. At the end, a daemon started with an existing ledger uses recovered runtime state instead of always starting from `core.new_state`.

Milestone 4 adds outbox replay and known workspace cleanup. At the end, pending Linear side effects are retried or acknowledged according to durable status, and terminal known workspaces are cleaned on startup.

Milestone 5 documents and validates. At the end, README explains restart behavior and the deterministic suite passes.

## Plan of Work

Create `src/scherzo/state/recovery.gleam`. Define a `RecoveryPlan` containing a recovered `domain.RuntimeState`, retry timers to schedule, workspaces to cleanup, interrupted run records to append, outbox items to replay, and log fields. The pure recovery function takes the ledger projection, current effective config, current time, and refreshed Linear issues for ledger-known ids.

Extend the ledger schema if necessary with `IssueCounterUpdated(issue_id, failure_attempts, worker_sessions, observed_updated_at_ms)` and `KnownWorkspace(issue_id, issue_identifier, workspace_path)`. Add these in `hardening-03` rather than changing current runtime shape if the base ledger did not include them.

Modify daemon startup. After config resolution, create a ledger path from `effective.workspace.root`, replay it, fetch current Linear states for known issue ids, build a recovery plan, and initialize daemon state with the recovered runtime state and linear command state/outbox data. Schedule retry timers from the plan after actor initialization.

Modify daemon transition points to append records. Use helper functions such as `record_run_started`, `record_run_finished`, `record_retry_scheduled`, and `record_issue_parked` so tests can inject a fake ledger writer. Ledger write failure for critical records should fail the transition before spawning or side effects where practical; for non-critical observability records, log and continue only if the plan explicitly says the record is non-critical.

For dispatch, append `RunStarted` after handoff claim succeeds and before spawning the worker. Include run id, issue id, identifier, workspace path, and current attempt/session counters. If the append fails, do not spawn the worker; release or preserve claim according to existing safe behavior and log `ledger_append_failed`.

For worker completion, append `RunFinished` before reporting handoff success/failure. If this append fails, do not report success to Linear yet; return a failure path or park conservatively so the operator can inspect. The exact choice must be recorded in Decision Log during implementation.

For retries and parking, append records at the same time daemon applies core effects. On startup recovery, retry entries whose due time is in the past should schedule `RetryTick` with delay `0` or a small deterministic startup jitter such as `100 ms`; future due entries keep their remaining delay.

For outbox, wrap existing handoff comment/state update and Linear command ack side effects. Before executing the side effect, append `OutboxPending`. After success, append `OutboxCompleted`. On startup, replay pending outbox items whose completed record is absent. Use stable dedupe keys in comment bodies where possible: run id for handoff, source comment id for command acks.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress.

2. Create `test/state_recovery_test.gleam`. Add `unfinished_run_becomes_interrupted_retry_test`: build a projection with `RunStarted` for active issue `ABC-1` and no finish record; provide refreshed Linear issue state `Todo`; assert recovery marks it interrupted and schedules retry according to failure caps.

3. Add `unfinished_run_terminal_issue_cleans_known_workspace_test`: projection has started run and known workspace; refreshed issue state is `Done`; assert recovery emits cleanup for that workspace and does not schedule retry.

4. Add `parked_issue_survives_restart_test`: projection has `IssueParked`; refreshed issue has same `updated_at`; assert recovered runtime contains the parked entry and candidate dispatch remains blocked.

5. Add `parked_issue_with_new_update_unparks_test`: projection has `IssueParked`; refreshed issue has newer `updated_at`; assert recovery clears parking and counters as current core logic does.

6. Add `overdue_retry_is_scheduled_immediately_test`: projection has retry due before `now_ms`; assert recovery schedules a retry timer with zero or configured startup delay.

7. Implement `src/scherzo/state/recovery.gleam` until pure recovery tests pass.

8. Add ledger writer dependency seams to `daemon.RuntimeDependencies`, or add a `state_writer` field that production sets from the ledger and tests can fake. Keep existing tests passing with a no-op writer until specific ledger tests opt in.

9. Add `test/orchestrator_daemon_ledger_test.gleam`. Test that dispatch appends `RunStarted` before worker spawn by using a fake ledger writer and fake worker subject.

10. Add daemon ledger tests for worker success/failure, retry scheduled/cancelled, issue parked/unparked, and known workspace recorded.

11. Implement daemon ledger appends at transition points.

12. Add startup recovery tests: prewrite ledger records under a temporary workspace root, start daemon with fake dependencies, and assert `GetSnapshot` returns recovered parked/retry/counter state before the first candidate dispatch.

13. Add recovery ordering test: prewrite a candidate plus a `/scherzo park` or durable parked record and assert startup recovery completes before candidate dispatch can start.

14. Add outbox tests. Prewrite `OutboxPending` with no completed record, start daemon, and assert the fake Linear handoff or command ack client receives one replay attempt. Prewrite `OutboxCompleted` and assert no replay.

15. Add terminal known workspace cleanup test. Prewrite a known workspace for an issue that refreshes as terminal; start daemon and assert cleanup dependency receives that path.

16. Update README `Daemon behavior and shutdown`, `Safety posture`, and `Implemented coverage` to describe single-instance restart recovery, interrupted-run handling, preserved parked state, and remaining at-least-once limitations.

17. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count.

18. Commit the phase with a message such as `Recover daemon state from local ledger`.

## Testing and Falsifiability

This plan is falsified if restart loses parked issues, if retry counters reset after restart, if unfinished runs are restored as live workers, if due retries are never scheduled, if startup dispatches candidates before recovery completes, if known terminal workspaces are not cleaned, if pending outbox items are replayed more than once when completed, or if corrupt ledger data causes silent unsafe recovery.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

No deterministic test may require real Linear or real pi. Use fake tracker and fake handoff/command clients.

## Validation and Acceptance

Accept this phase when:

- Daemon startup replays the ledger before first poll dispatch.
- Started-without-finished runs are marked interrupted, not restored as live.
- Parked issues and counters survive restart.
- Retry timers are reconstructed from durable due times.
- Terminal known workspaces are cleaned without scanning all Linear terminal issues.
- Pending outbox items replay and completed outbox items do not.
- README documents at-least-once behavior and remaining stale-lock/manual recovery boundary.
- The full deterministic suite passes.

## Rollout, Recovery, and Idempotence

Roll out after the ledger format is stable. On first startup after this phase, an empty ledger produces the same behavior as current Scherzo. As daemon transitions occur, it begins recording durable facts.

If startup detects ledger corruption in the middle of the file, it should fail before dispatching. Operators should back up the ledger, inspect it, and either restore from a clean snapshot or deliberately move the ledger aside, accepting loss of local recovery state.

Recovery is at-least-once. A crash in the tiny window between a real-world side effect and its completed outbox record can still produce duplicate Linear comments on replay. Run IDs and source comment ids make duplicates auditable.

## Artifacts and Notes

Target recovery transcript shape:

    level=info service=scherzo event=ledger_replay_ok records=42 truncated_tail=false
    level=warn service=scherzo event=run_marked_interrupted run_id=LIV-9-... issue_id=...
    level=info service=scherzo event=recovered_retry_scheduled issue_id=... delay_ms=0
    level=info service=scherzo event=recovered_parked_issue issue_id=... reason=max_retry_attempts

This plan does not change distributed behavior. Two daemons with different workspace roots can still duplicate work.

## Interfaces and Dependencies

In `src/scherzo/state/recovery.gleam`, expose functions equivalent to:

    pub type RecoveryPlan {
      RecoveryPlan(
        runtime: domain.RuntimeState,
        retry_timers: List(domain.RetryEntry),
        interrupted_runs: List(record.RunRef),
        cleanup_workspaces: List(String),
        outbox_to_replay: List(record.OutboxRef),
        warnings: List(String),
      )
    }

    pub fn plan(
      projection: projection.Projection,
      config: domain.EffectiveConfig,
      refreshed_issues: List(domain.Issue),
      now_ms: Int,
    ) -> RecoveryPlan

In `daemon.RuntimeDependencies`, add a ledger writer/reader seam or state-store seam that production implements using `state/ledger.gleam` and tests can replace with in-memory fakes.

No new package dependency should be required beyond the ledger modules from `hardening-02`.
