# Hardening 03: Add single-instance crash recovery from the local ledger

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, one Scherzo daemon using one canonical workspace root can restart after an unexpected process exit and recover local scheduler facts from the durable ledger. On startup, Scherzo reloads durable retry counters, session counters, parked issues, retry timers, known workspace paths, interrupted runs, and replayable pending Linear outbox work. Previously running workers are marked interrupted because live pi sessions cannot be recovered. Active interrupted issues are retried or parked according to the same caps as ordinary failures; terminal interrupted issues have their known workspaces cleaned up. Overdue retry timers are scheduled immediately or after a small deterministic startup delay.

The first implementation milestone closes recovery-critical gaps in the ledger created by `docs/plans/hardening-02-local-durable-state-ledger.md`. The current ledger implementation records run, retry, parking, command, and outbox statuses, but it does not yet persist all facts needed to recover safely: exact issue counters, known workspaces independent of run status, parking release policy and issue fingerprint, and replay payloads for pending outbox items. This plan makes those facts explicit before wiring daemon recovery.

The visible proof is a deterministic recovery test that starts a daemon, records a running issue and a parked issue in the ledger, simulates process death by starting a new daemon from the same workspace root, and observes that the new daemon preserves the parked issue, marks the old run interrupted, schedules or rejects retry according to caps, and does not dispatch duplicate work before recovery reconciliation completes. A second restart over the same ledger must not double-count the interrupted run or duplicate completed outbox work.

This phase is single-instance recovery only. It does not recover live pi sessions, does not make EventHub history durable, does not process Linear command comments posted while Scherzo was down, and does not allow multiple hosts or workspace roots to process the same Linear project safely. Durable command receipts and processing commands posted while down remain the responsibility of `docs/plans/hardening-05-durable-linear-command-inbox.md`.

## Problem Framing and Constraints

Scherzo currently rebuilds itself from Linear and the filesystem after restart. That is safe enough for early operation, but it loses important runtime state: retry timers, retry/session counters, parked issues, processed command ids, pending acknowledgement status, and knowledge that a worker was in progress. After a crash, an active Linear issue can be dispatched again immediately even if it had been parked or had just failed repeatedly enough to hit a cap. A completed issue's workspace can remain on disk because Scherzo does not have a durable list of known terminal workspaces to clean on startup.

The previous hardening plan added a local JSONL ledger and replay projection under `workspace.root/.scherzo-state/ledger/`. In the current tree, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/ledger.gleam` exist and are tested. The current record schema is a storage foundation, not yet a recovery contract: `RetryScheduled` stores `delay_ms` and the record timestamp rather than an explicit due timestamp; `RunFinished` overwrites the projection's `RunStarted` details and therefore loses the workspace path unless a separate known-workspace fact exists; `IssueParked` stores an observed update time but not the release policy or issue fingerprint needed by `domain.ParkedEntry`; and `OutboxPending` stores only ids and a dedupe key, not enough payload to replay a Linear side effect. This plan must address those gaps before daemon transition wiring, otherwise a future implementer could build startup recovery that passes happy-path tests while losing counters, workspaces, or outbox work after compaction.

The daemon remains the only process that mutates scheduler state. The ledger records facts before or at the same point as the corresponding in-memory transition so a later process can make conservative recovery decisions. For facts that guard external side effects or worker spawning, ledger append failure is critical: the daemon must not proceed with the side effect as though recovery data were durable.

The recovery rule is conservative: never assume a live worker or pi session survived a BEAM restart. Every ledger run that was started but not finished is treated as interrupted. Recovery should either retry it through the normal dispatch path, park it if caps are exhausted, or clean its workspace if Linear says the issue is terminal.

## Strategy Overview

First extend the ledger schema and projection with the recovery facts this plan needs. Add additive record kinds under the existing schema version unless implementation discovers a hard compatibility reason to introduce `schema_version = 2`; if a version bump is used, the decoder must still read all existing version 1 records from hardening 02. The new durable facts are:

- `IssueCounterUpdated(issue_id, issue_identifier, failure_attempts, worker_sessions, observed_updated_at_ms, source_run_id)` where `source_run_id` is optional and is set when a particular run caused the counter change. Recovery uses this source id to avoid double-counting interrupted runs across repeated startups.
- `KnownWorkspace(issue_id, issue_identifier, workspace_path)` so workspace cleanup survives run-status projection changes and ledger compaction.
- `IssueParkedV2(issue_id, issue_identifier, reason, release_policy, issue_fingerprint, observed_updated_at_ms)` where `release_policy` is either `explicit_unpark_only` or `auto_unpark_on_issue_change`. The daemon should emit this new form for all future parks; the projection may continue decoding older `IssueParked` records for tests and manual ledgers, but recovery should prefer the v2 facts.
- `OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json)` where `payload_json` is a bounded, redacted, replayable payload for the specific Linear side effect. Payloads must not contain API keys, raw pi JSON, prompt text, or unbounded result transcripts.

For existing `RetryScheduled` records, compute the retry due time as `scheduled_at_ms + delay_ms`, where `scheduled_at_ms` is the ledger record timestamp already retained in `projection.RetryScheduled`. New daemon code must construct retry records with `record.at_ms` equal to the same `now_ms` value used for scheduling the timer. Do not treat `delay_ms` alone as an absolute due time.

Add a recovery module under `src/scherzo/state/recovery.gleam`. It consumes the ledger projection and current workflow config, fetches current Linear state for durable issue ids, and produces a recovered `domain.RuntimeState` plus daemon work to schedule retries, append recovery records, cleanup terminal workspaces, replay pending outbox items, and log interrupted runs. It does not spawn workers directly.

Wire ledger emission into the daemon at key transition points:

- after handoff claim succeeds and before spawning a worker, append `KnownWorkspace`, `RunStarted`, and any needed counter snapshot;
- when a worker finishes successfully or fails, append `RunFinished` and `IssueCounterUpdated` before reporting handoff success or failure;
- when a worker is killed during daemon shutdown or disappears across restart, append or synthesize `RunInterrupted` and, for recovery-caused counter changes, an `IssueCounterUpdated` with `source_run_id` set to the interrupted run id;
- when retry timers are scheduled or cancelled, append retry records;
- when an issue is parked or unparked, append parking records that include the release policy and reset counters where current core logic resets them;
- before a Linear side effect that must be replayable, append `OutboxPendingV2`; after it succeeds, append `OutboxCompleted`; after a final non-retryable failure, append `OutboxFailed`.

On daemon startup, after config resolution and before scheduling the first poll tick, load the ledger. If replay fails because of a corrupt non-trailing record or corrupt snapshot, startup fails clearly before dispatching. If replay reports a truncated tail, startup logs a warning and continues from the valid prefix. Recovery then fetches current Linear state for issue ids present in durable running, interrupted, retry, parked, counter, known-workspace, and pending-outbox records. It reconstructs runtime state conservatively, appends any required recovery records with fsync, initializes the daemon with the recovered runtime state, schedules due retry timers, and only then starts the poll scheduler.

Keep startup recovery bounded. It should inspect only issue ids known from the local ledger, not scan all terminal issues in the Linear project. Fetch known ids through `tracker.fetch_issue_states_by_ids` in deterministic chunks of 50 ids or fewer.

## Alternatives Considered

One alternative is to keep relying on Linear state only. That is simple but loses parked state and retry counters, and it cannot distinguish an interrupted Scherzo worker from a never-started issue.

Another alternative is to persist the entire `domain.RuntimeState` snapshot and reload it directly. That couples durable format to current in-memory types and risks restoring stale process/session handles. A fact ledger plus recovery projection is safer because process-owned fields are intentionally not persisted.

A third alternative is to recover live pi sessions by reattaching to pi. That is out of scope. Current workers own Erlang ports and pi RPC sessions, and those disappear when the BEAM process exits.

A fourth alternative is to scan every terminal Linear issue on startup and cleanup matching workspaces. That can be expensive on real boards and is unnecessary for single-instance recovery. This plan cleans only workspaces known from the local ledger.

A fifth alternative is to implement startup recovery against the hardening-02 schema exactly as it exists today. That would be under-specified and unsafe: counters would not enforce caps after restart, compaction could lose workspace paths, operator parks could auto-unpark incorrectly, and outbox replay would have no payload to send. This plan rejects that shortcut and closes the schema gaps first.

## Risks and Countermeasures

The main duplication risk is crashing after a worker completed but before Scherzo recorded completion. Recovery will see a started run without a finish record and may retry. Countermeasure: append `RunFinished` immediately when the daemon receives `WorkerFinished`, before handoff reporting. This reduces but cannot eliminate the tiny window before the append. The result is at-least-once work, not exactly-once.

The main double-counting risk is crashing during startup recovery after marking a run interrupted but before completing all retry or parking records. Countermeasure: counter updates caused by recovery include `source_run_id`; recovery checks whether that run id has already contributed to counters before incrementing again. Add a deterministic test that starts twice from the same interrupted ledger and asserts the failure counter increases only once.

The main outbox risk is duplicating Linear comments after a crash between Linear success and `OutboxCompleted`. Countermeasure: outbox records include a stable dedupe key containing run id, issue id, outbox kind, and source comment id when applicable. Replayed comments include the same run id/source id so duplicates are auditable. True Linear-side idempotency is deferred unless Linear exposes a stable client mutation id.

The main outbox-data risk is recording pending items that cannot be replayed. Countermeasure: this phase must emit `OutboxPendingV2` with bounded replay payloads before claiming outbox replay works. If startup encounters a payload-less pending `OutboxPending` without a completed or failed record, it must fail clearly with an `outbox_payload_missing` startup error rather than silently dropping the work.

The main state risk is restoring stale running entries. Countermeasure: recovery never restores live workers. It marks incomplete runs as interrupted and schedules retry or parking through normal core logic.

The main startup latency risk is fetching too many issues. Countermeasure: fetch only ledger-known issue ids and chunk requests through `tracker.fetch_issue_states_by_ids`. Do not scan project history.

The main startup availability risk is Linear being unavailable during recovery. Countermeasure: if current issue-state fetch fails, startup fails before dispatching any work. Operators can retry startup when Linear is available. Starting from stale local state and dispatching without knowing terminal or updated issue state is less safe than refusing to start.

The main compatibility risk is missing facts needed to reconstruct counters exactly. Countermeasure: add explicit `IssueCounterUpdated` records in this plan and require tests proving recovered counters enforce `max_retry_attempts` and `max_sessions_per_issue` after restart.

The main stale-lock risk remains manual after an abrupt kill. Countermeasure: document that if the old process left `instance.lock`, the operator must still verify no process is running and remove the stale lock before recovery can start. Automatic stale-lock takeover is intentionally not part of this plan.

## Progress

- [x] (2026-04-29 04:20Z) Drafted this plan to consume the durable ledger from `hardening-02-local-durable-state-ledger.md`.
- [x] (2026-05-01 22:41Z) Reviewed the plan against the current `src/scherzo/state/*` and daemon code; tightened schema, startup ordering, idempotence, and outbox-payload requirements before implementation.
- [x] (2026-05-01 23:20Z) Ran baseline `direnv exec . gleam test`; observed 507 passed, no failures.
- [x] (2026-05-01 23:31Z) Added ledger schema/projection tests for counters, known workspaces, parking release policy, retry due-time calculation, replayable outbox payloads, and payload-less pending outbox errors.
- [x] (2026-05-01 23:34Z) Added pure recovery tests for interrupted, parked, retrying, terminal, auto-unpark, repeated-startup, and payload-less outbox cases.
- [x] (2026-05-01 23:40Z) Appended ledger records from daemon dispatch, worker finish, retry, park, unpark, and Linear command acknowledgement transitions.
- [x] (2026-05-01 23:41Z) Loaded the ledger during daemon startup, fetched ledger-known issue ids in 50-id chunks, appended recovery records, and installed recoverable runtime state before actor polling.
- [x] (2026-05-01 23:42Z) Replayed pending Linear command/comment outbox items with `OutboxPendingV2` payloads through the existing Linear command acknowledgement side-effect path and append `OutboxCompleted` on success.
- [x] (2026-05-01 23:42Z) Added bounded known-workspace cleanup for terminal recovered issues.
- [x] (2026-05-01 23:43Z) Updated README with restart recovery behavior, coverage, outbox payload limits, and remaining at-least-once/stale-lock limits.
- [x] (2026-05-01 23:43Z) Ran `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`; observed 520 passed, no failures before the stale jj workspace update.
- [x] (2026-05-01 23:44Z) Skipped the final commit step because this workspace initially appeared to have no Git or Jujutsu metadata while the working copy was stale.
- [x] (2026-05-01 23:56Z) Ran `jj workspace update-stale`, resolved conflicts only in the `liv-20-crash-recovery` workspace history, adapted the new recovery test config to the rebased `HandoffConfig` shape, and reran validation; observed 493 passed, no failures.

## Surprises & Discoveries

- Observation: The hardening-02 ledger is present in the current tree, but its current version 1 records are not sufficient for safe startup recovery by themselves.
  Evidence: `src/scherzo/state/record.gleam` has `RetryScheduled(delay_ms, generation, reason)`, `IssueParked(observed_updated_at_ms)`, and `OutboxPending(dedupe_key)` but no counter update, known-workspace, park release-policy/fingerprint, or outbox payload record.

- Observation: `src/scherzo/state/projection.gleam` stores one final `RunStatus` per run id, so a later `RunFinished` or `RunInterrupted` replaces the `RunStarted` value that contained the workspace path.
  Evidence: projection `apply` inserts `RunFinished` and `RunInterrupted` into `projection.runs` under the same run id. Recovery cleanup must therefore use a separate known-workspace projection instead of assuming run status still contains the original workspace path.

- Observation: Existing daemon control tests consume the first startup log event as the control-file path. Emitting an unconditional `ledger_replay_ok` log before the control server starts broke those tests even though recovery was otherwise empty.
  Evidence: `direnv exec . gleam test` failed in `orchestrator_daemon_control_test` with attempts to read a control file named `ledger_replay_ok`. The implementation now logs only actionable recovery warnings/work items during ordinary empty-ledger startup.

- Observation: While the working copy was stale, ordinary VCS discovery was misleading.
  Evidence: `git status --short` returned `fatal: not a git repository`, and no `.jj` directory was visible under the workspace, but `jj workspace update-stale` later succeeded and showed the active workspace as `liv-20-crash-recovery`.

- Observation: The stale workspace update rebased this workspace onto a history that no longer includes Linear attachment tests or the newer 11-field `HandoffConfig` shape.
  Evidence: The conflict resolution deleted `test/linear_attachment_graphql_test.gleam` and `test/linear_attachment_test.gleam`, removed the stray `scherzo/linear_attachment` import from `test/handoff_test.gleam`, and `direnv exec . gleam test` then required `test/state_recovery_test.gleam` to use the 9-field `domain.HandoffConfig`; the final suite reported 493 passed.

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

- Decision: Add explicit counter, known-workspace, park-policy, and outbox-payload records before daemon recovery wiring.
  Rationale: The current ledger stores useful facts but not enough information to reconstruct `domain.RuntimeState` safely after compaction or to replay pending side effects.
  Date: 2026-05-01

- Decision: Compute existing retry due times as `scheduled_at_ms + delay_ms` and require new retry records to use the scheduling `now_ms` as their ledger timestamp.
  Rationale: The current record body has `delay_ms`, not `due_at_ms`; using the record timestamp keeps recovery compatible with the existing schema while making overdue retry tests deterministic.
  Date: 2026-05-01

- Decision: Recovery must complete before `poll_scheduler.start` schedules the immediate `PollTick(1)`.
  Rationale: The current daemon startup schedules a zero-delay poll during actor initialization. If recovery runs after that, candidates can dispatch before parked/retry/interrupted state is restored.
  Date: 2026-05-01

- Decision: Counter updates caused by recovery include the interrupted `source_run_id`.
  Rationale: A crash during recovery must not increment retry counters repeatedly on each restart.
  Date: 2026-05-01

- Decision: Keep `schema_version = 1` and add record kinds additively.
  Rationale: The existing decoder can remain compatible with hardening-02 ledgers while new binaries understand the new recovery facts. Older binaries may still reject unknown kinds after rollback, as documented.
  Date: 2026-05-01

- Decision: Reuse the existing Linear command acknowledgement effect for replayable v2 outbox comment payloads in this phase.
  Rationale: It provides a deterministic, tested side-effect path without adding a new effect runner abstraction. The behavior remains at-least-once and records `OutboxCompleted` after success.
  Date: 2026-05-01

## Outcomes & Retrospective

Implemented single-instance startup recovery from the local ledger. The final recovery rules are conservative: live workers are never restored; started-but-unfinished or already interrupted runs are classified from refreshed Linear state; active interrupted runs increment failure counters once per `source_run_id`; terminal interrupted runs enqueue cleanup for known workspaces; explicit parks survive until explicit unpark; auto parks survive only while the refreshed issue fingerprint is unchanged; retry due times are computed from `scheduled_at_ms + delay_ms`; payload-less old outbox pending records fail recovery.

The daemon now emits durable facts for successful claims before worker spawn, worker finish before handoff reporting, retry schedule/cancel effects, park/unpark transitions, and Linear command acknowledgement outbox pending/completed status. Startup replays the ledger, refreshes ledger-known issue ids in chunks of 50, appends recovery records with fsync, initializes runtime state from the recovery plan, schedules recovered retry timers, enqueues terminal workspace cleanup, and then polls.

Final validation passed with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`; after the stale workspace update and conflict resolution, the suite reported 493 passed, no failures. The main remaining duplicate window is still at-least-once Linear side effects: a crash after the external side effect succeeds but before `OutboxCompleted` is appended can replay the side effect on restart.

## Context and Orientation

The daemon actor in `src/scherzo/orchestrator/daemon.gleam` owns runtime state. It starts with `core.new_state(effective)`, currently schedules an immediate poll tick through `poll_scheduler.start`, fetches candidates, dispatches workers, handles `WorkerFinished` and `WorkerDown`, schedules retry timers, and publishes session events to the in-memory EventHub. The pure scheduler in `src/scherzo/orchestrator/core.gleam` updates `domain.RuntimeState` and emits effects such as `Dispatch`, `ScheduleRetry`, `CancelRetry`, `CleanupWorkspace`, `ReleaseClaim`, `StopWorker`, and `ParkIssue`.

`domain.RuntimeState` contains process-local fields such as running workers and retry timers. Recovery must reconstruct only safe logical state: claims that block duplicate normal dispatch while a retry is pending, counters, parked entries, completed terminal issues where known, retry entries, known workspaces, aggregate token totals where durable totals exist, and pending replayable outbox items. It must not try to restore Erlang pids, monitor refs, worker command subjects, EventHub subjects, or control server state.

The local durable ledger from `docs/plans/hardening-02-local-durable-state-ledger.md` lives under `workspace.root/.scherzo-state/ledger/`. It records versioned JSONL facts and can replay into a projection. In the current tree, `src/scherzo/state/ledger.gleam` provides `path_for_workspace_root`, `append`, `append_many`, `read_records`, `replay`, `load_projection`, and `compact`; `src/scherzo/state/record.gleam` defines record variants; and `src/scherzo/state/projection.gleam` folds records into status maps. This plan wires daemon transitions to that ledger and uses replay during daemon startup.

Linear side effects currently run through `src/scherzo/orchestrator/effect_runner.gleam` and high-level clients in `src/scherzo/handoff.gleam`, `src/scherzo/linear.gleam`, and `src/scherzo/linear_triage.gleam`. Outbox replay in this phase should be bounded to side effects whose payload can be stored safely in the ledger. Durable command inbox semantics are deferred to hardening 05.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/hardening-01-graceful-daemon-lifecycle.md` is complete or consciously deferred.
- `docs/plans/hardening-02-local-durable-state-ledger.md` is complete.
- `src/scherzo/state/ledger.gleam`, `src/scherzo/state/record.gleam`, and `src/scherzo/state/projection.gleam` exist and have passing tests.
- The current hardening-02 implementation uses `schema_version = 1`, append/fsync support through `src/scherzo_state_ffi.erl`, and projection snapshots under `workspace.root/.scherzo-state/ledger/snapshot.json`.
- `direnv exec . gleam test` passes.
- The daemon still starts from one canonical workspace root guarded by `instance.lock`.

If the ledger cannot replay due to a corrupt non-trailing record or corrupt snapshot, do not implement best-effort recovery on top of bad data. Fail startup clearly and instruct the operator to back up and inspect the ledger.

If the repository has already added any of the record variants this plan calls for, normalize the plan during implementation by reusing the existing names only if they carry the same semantics. Do not silently reuse a similarly named record that lacks counters, source run id, park release policy, workspace path, or outbox payload.

## Scope Boundaries

In scope: additive ledger schema/projection updates required for recovery; ledger emission from daemon transitions; startup ledger replay; reconstruction of retry counters, session counters, parked issues, retry entries, completed known terminal issues, known workspaces, and aggregate token totals where feasible; interrupted-run classification; due retry rescheduling; known terminal workspace cleanup; replay of pending Linear outbox items that have bounded `OutboxPendingV2` payloads; deterministic recovery tests.

Out of scope: live pi session recovery; durable EventHub archive; Linear command comments posted while Scherzo was down; generic durable command receipts beyond any payload needed to replay an already-created acknowledgement; automatic stale-lock takeover; multi-host or multi-workspace-root exactly-once; global Linear terminal scans; storing raw pi JSON, prompt text, API keys, or unbounded Linear comment bodies; schema migration beyond compatibility needed for the current ledger version.

## Milestones

Milestone 1 closes ledger schema and projection gaps. At the end, tests can encode/decode and project counter updates, known workspaces, v2 parking records, retry due times computed from existing retry records, and v2 outbox payloads. No daemon startup behavior changes yet.

Milestone 2 adds pure recovery planning. At the end, tests can feed ledger projections and fake current Linear issue states into `src/scherzo/state/recovery.gleam` and assert recovered runtime state, records to append, retry timers, cleanup requests, and outbox replay requests without starting a daemon.

Milestone 3 records daemon transitions. At the end, daemon tests can run dispatch, worker success, worker failure, retry scheduling, park/unpark, and handoff side effects, then inspect ledger records for the expected facts and ordering.

Milestone 4 wires startup recovery. At the end, a daemon started with an existing ledger uses recovered runtime state instead of always starting from `core.new_state`, and the first poll tick cannot run until recovery has completed.

Milestone 5 adds replayable outbox handling and known workspace cleanup. At the end, pending Linear side effects with payloads are retried or acknowledged according to durable status, payload-less pending records fail startup clearly, and terminal known workspaces are cleaned on startup.

Milestone 6 documents and validates. At the end, README explains restart behavior and the deterministic suite passes.

## Plan of Work

Extend `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam` first. Add additive record variants and projection fields for issue counters, known workspaces, v2 parked issues, and v2 outbox pending payloads. Keep all existing hardening-02 record tests passing. Add tests proving old records still decode and new records roundtrip. Projection should expose helper views for `known_issue_ids`, `known_workspaces`, latest counters by issue id, counter source run ids by issue id, parked entries by issue id, retry due timestamps, and replayable pending outbox entries.

For parking, store enough data to rebuild `domain.ParkedEntry` precisely. Operator parks created through `park_issue_state` use `domain.ExplicitUnparkOnly`. Core safety parks created by `core.apply_worker_failure` or `core.apply_worker_success_with_workspace_path` use `domain.AutoUnparkOnIssueChange(core.issue_fingerprint(issue))`. Recovery keeps an explicit park until an explicit unpark record exists. Recovery keeps an auto-unpark park only if the refreshed issue still has the same fingerprint; if the issue changed, recovery clears parking, retry, and counters just like `core.unpark_if_issue_changed`.

For counters, write `IssueCounterUpdated` whenever `state.runtime.issue_counters` changes or is reset. Worker failure writes a new failure-attempt count with `source_run_id` set to the worker run id. Successful non-terminal continuation writes a new worker-session count with `source_run_id` set to the worker run id. Operator retry, operator unpark, and auto-unpark-on-issue-change write a reset record with both counts `0` and no `source_run_id`. During startup recovery of interrupted runs, if a counter update with that interrupted run id is already present, do not increment again.

For known workspaces, append `KnownWorkspace` whenever the daemon has resolved the stored workspace path for an issue and before or with `RunStarted`. Do not rely on `RunStarted` to preserve workspace paths after compaction, because the current projection replaces run status under the run id.

For retry records, use existing `RetryScheduled` records by computing due time from the record timestamp plus `delay_ms`. When daemon code creates a retry scheduled record, pass the same `now_ms` used by `core.schedule_retry` or `apply_effect(core.ScheduleRetry(...))` into `record.new`. Recovery should set the restored `domain.RetryEntry.delay_ms` to the remaining startup delay, not the original delay, and preserve the durable generation.

For outbox, introduce a small payload module or helper type under `src/scherzo/state/outbox.gleam` if that keeps `record.gleam` readable. Payloads should be JSON objects tagged by kind, for example a Linear comment payload with a redacted bounded `body`, a Linear state-update payload with `state_id`, or a Linear command acknowledgement payload with `source_comment_id` and redacted bounded `body`. Store only data needed to replay the side effect, not raw GraphQL requests or credentials. Tests must inject a fake secret and an overlong body and assert the persisted JSON is redacted and bounded.

Create `src/scherzo/state/recovery.gleam`. Define a pure `RecoveryPlan` containing a recovered `domain.RuntimeState`, retry timers to schedule, ledger records to append before the daemon starts polling, workspaces to cleanup, outbox items to replay, and warnings. The pure recovery function takes the ledger projection, current effective config, current time, and refreshed Linear issues for ledger-known ids.

Recovery should start from `core.new_state(config)` and fold durable facts into it. It should restore counters from latest `IssueCounterUpdated` records. It should restore parked entries from v2 parked facts after applying release-policy checks against refreshed issues. It should restore retry entries for scheduled retries that have not been cancelled and whose issue is not terminal or unparked. It should preserve claims only when doing so prevents duplicate normal dispatch while a retry is pending; do not restore claims for terminal or missing issues. It should add refreshed terminal issues to `runtime.completed` when useful for operator lookup and cleanup, but must not invent full `domain.Issue` values for ids that Linear did not return.

For ledger-known issue ids that `fetch_issue_states_by_ids` does not return, recovery should not dispatch work. Keep explicit parked entries, drop or defer retry timers for that id, log a warning, and do not cleanup workspaces because terminal state is unproven. This keeps the system conservative without blocking the whole daemon for an issue that may have been deleted or become inaccessible after the ledger was written.

For started-but-unfinished runs, recovery should append `RunInterrupted` if the run is still projected as running, then classify by refreshed issue state. If the issue is terminal, schedule known-workspace cleanup and release local claim state. If the issue is active and the interrupted run has not already contributed to counters, increment failure attempts once, append `IssueCounterUpdated(source_run_id: run_id)`, and either append `IssueParkedV2` when the max retry cap is reached or append `RetryScheduled` for an immediate or startup-jittered retry. If the issue is non-active or missing, do not dispatch it; log a warning and leave it for future operator action.

Modify daemon startup in `src/scherzo/orchestrator/daemon.gleam`. After `runtime_bundle.load` and client construction, build `ledger.path_for_workspace_root(effective.workspace.root)`, replay the ledger, compute known issue ids, fetch current issue states in chunks of 50 using `tracker_client.fetch_issue_states_by_ids`, build the recovery plan, append recovery records with fsync, and initialize `State.runtime` from the plan. Only after this succeeds should startup call `poll_scheduler.start` or otherwise schedule the immediate poll tick. Schedule recovered retry timers after actor initialization using the delay in each recovered retry. Enqueue cleanup and outbox replay work after the actor and effect runner exist, before starting the first poll if practical; if they must run through the effect runner after initialization, keep candidate dispatch blocked until they have been enqueued.

Add a ledger/state-store dependency seam to `daemon.RuntimeDependencies`. Production should use `state/ledger.gleam`; tests should be able to inject an in-memory ledger that records appends, returns replay projections, and can fail specific appends. Keep existing tests passing with a no-op state store until tests opt into recovery behavior.

Modify daemon transition points to append records. Use helper functions such as `append_run_started`, `append_run_finished`, `append_retry_scheduled`, `append_issue_parked`, and `append_outbox_pending` so tests can inject a fake ledger writer. Ledger write failure for records that guard worker spawning, counter changes, retry scheduling, parking, or external side effects is critical: log an error and stop or reject the transition rather than silently continuing.

For dispatch, append `KnownWorkspace` and `RunStarted` after handoff claim succeeds and before spawning the worker. Include run id, issue id, identifier, workspace path, and current attempt/session counters. If the append fails after Linear claim succeeded, do not spawn the worker. Stop the daemon abnormally with a clear `ledger_append_failed` log so recovery does not proceed from an unrecorded worker start; the operator may need to inspect the Linear claim.

For worker completion, append `RunFinished` and the relevant `IssueCounterUpdated` before reporting handoff success/failure. If this append fails, do not report success or failure to Linear yet. Log `ledger_append_failed` with the run id and stop abnormally; on restart, the run will be treated as interrupted and may be retried, which is safer than reporting a result that cannot be recovered.

For retries and parking, append records at the same time daemon applies core effects. Append `RetryScheduled` before scheduling the timer and append `RetryCancelled` before cancelling a durable retry. Append `IssueParkedV2` before exposing the parked state as durable. On startup recovery, retry entries whose due time is in the past should schedule `RetryTick` with delay `0` or a small deterministic startup jitter such as `100 ms`; future due entries keep their remaining delay.

For outbox, wrap replayable handoff comments/state updates, Linear command acknowledgements that already have a payload, and invalid-workflow reports if their payload can be stored safely. Before executing the side effect, append `OutboxPendingV2`. After success, append `OutboxCompleted`. On startup, replay pending v2 outbox items whose completed or failed record is absent. Use stable dedupe keys in comment bodies where possible: run id for handoff, source comment id for command acknowledgements, and issue id plus violation fingerprint for invalid-workflow reports.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

2. Update `test/state_record_test.gleam`. Add encode/decode tests for `IssueCounterUpdated`, `KnownWorkspace`, `IssueParkedV2`, and `OutboxPendingV2`. Assert old hardening-02 records still decode.

3. Update `test/state_projection_test.gleam`. Add tests proving projection exposes latest counters, known workspaces after a run is finished or interrupted, v2 parked release policy and fingerprint, retry due time as `scheduled_at_ms + delay_ms`, and pending v2 outbox entries.

4. Implement the record and projection extensions in `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam` until the state tests pass.

5. Add a redaction/bounding test for outbox payloads. Construct an `OutboxPendingV2` payload containing `secret-value` and a body longer than the configured maximum; assert the persisted JSON contains neither the secret nor an overlong body.

6. Create `test/state_recovery_test.gleam`. Add `unfinished_run_becomes_interrupted_retry_test`: build a projection with `RunStarted` for active issue `ABC-1` and no finish record; provide refreshed Linear issue state `Todo`; assert recovery emits `RunInterrupted`, increments the failure counter once, and schedules retry according to failure caps.

7. Add `interrupted_run_recovery_is_idempotent_test`: build a projection with `RunStarted`, `RunInterrupted`, and `IssueCounterUpdated(source_run_id: run_id)`; run recovery twice and assert the failure counter remains unchanged on the second recovery.

8. Add `unfinished_run_terminal_issue_cleans_known_workspace_test`: projection has started run and `KnownWorkspace`; refreshed issue state is `Done`; assert recovery emits cleanup for that workspace and does not schedule retry.

9. Add `parked_issue_survives_restart_test`: projection has `IssueParkedV2` with `explicit_unpark_only`; refreshed issue changed; assert recovered runtime still contains the parked entry.

10. Add `auto_parked_issue_with_same_fingerprint_survives_restart_test`: projection has `IssueParkedV2` with `auto_unpark_on_issue_change` and stored fingerprint; refreshed issue has the same fingerprint; assert recovered runtime contains the parked entry.

11. Add `auto_parked_issue_with_new_fingerprint_unparks_test`: projection has an auto-unpark parked entry; refreshed issue has a different fingerprint; assert recovery clears parking, retry, and counters as current core logic does.

12. Add `overdue_retry_is_scheduled_immediately_test`: projection has retry scheduled at `1000 ms` with `delay_ms = 5000`; run recovery at `7000 ms`; assert recovery schedules a retry timer with zero or configured startup delay.

13. Add `future_retry_keeps_remaining_delay_test`: projection has retry due in the future; assert recovered runtime and retry timer use the remaining delay and durable generation.

14. Add `payload_less_pending_outbox_fails_recovery_test`: projection has old `OutboxPending` with no completed or failed record; assert startup recovery returns a clear error rather than silently ignoring it.

15. Implement `src/scherzo/state/recovery.gleam` until pure recovery tests pass.

16. Add ledger/state-store dependency seams to `daemon.RuntimeDependencies`, or add a `state_store` field that production sets from the ledger and tests can fake. Keep existing tests passing with a no-op store until specific ledger tests opt in.

17. Add `test/orchestrator_daemon_ledger_test.gleam`. Test that dispatch appends `KnownWorkspace` and `RunStarted` after handoff claim success and before worker spawn by using a fake ledger writer and fake worker subject.

18. Add `dispatch_does_not_spawn_when_run_started_append_fails_test`: make the fake writer fail on `RunStarted`; assert no worker is spawned, the failure is logged, and the daemon stops or rejects the transition according to the implemented failure policy.

19. Add daemon ledger tests for worker success/failure. Assert `RunFinished` and `IssueCounterUpdated` are appended before `ReportSuccess` or `ReportFailure` is enqueued.

20. Add daemon ledger tests for retry scheduled/cancelled, issue parked/unparked, counter resets, and known workspace recorded.

21. Implement daemon ledger appends at transition points.

22. Add startup recovery tests: prewrite ledger records under a temporary workspace root, start daemon with fake dependencies, and assert `GetSnapshot` returns recovered parked/retry/counter state before the first candidate dispatch.

23. Add recovery ordering test: prewrite a candidate plus a durable parked record and assert startup recovery completes before candidate dispatch can start. Use the fake tracker/effect runner to fail the test if `fetch_candidate_issues` is called before recovery state is installed.

24. Add startup fetch failure test: make `fetch_issue_states_by_ids` fail during recovery and assert daemon startup fails before scheduling the first poll.

25. Add terminal known workspace cleanup test. Prewrite a known workspace for an issue that refreshes as terminal; start daemon and assert cleanup dependency receives that path.

26. Add outbox tests. Prewrite `OutboxPendingV2` with no completed record, start daemon, and assert the fake Linear handoff or command ack client receives one replay attempt with the stored payload. Prewrite `OutboxCompleted` and assert no replay.

27. Add outbox duplicate-window test: prewrite pending v2, start daemon, let the fake client succeed, assert `OutboxCompleted` is appended. Restart and assert the side effect is not replayed again.

28. Update README `Daemon behavior and shutdown`, `Safety posture`, and `Implemented coverage` to describe single-instance restart recovery, interrupted-run handling, preserved parked state, retry due-time recovery, outbox replay payload limits, and remaining at-least-once limitations.

29. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count in Progress.

30. Commit the phase with a message such as `Recover daemon state from local ledger`.

## Testing and Falsifiability

This plan is falsified if restart loses parked issues, if retry counters reset after restart, if an interrupted run can be counted twice by restarting twice, if unfinished runs are restored as live workers, if due retries are never scheduled, if startup dispatches candidates before recovery completes, if known terminal workspaces are not cleaned, if pending v2 outbox items are not replayed, if completed outbox items replay again, if payload-less pending outbox records are silently ignored, or if corrupt ledger data causes silent unsafe recovery.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

No deterministic test may require real Linear or real pi. Use fake tracker, fake handoff/command clients, fake cleanup dependencies, and fake ledger writers. The new recovery tests should prove both happy paths and negative paths: corrupt ledger fails startup, Linear fetch failure fails startup, missing issue ids do not dispatch, and repeated startup is idempotent for interrupted-run counters.

## Validation and Acceptance

Accept this phase when:

- Daemon startup replays the ledger before first poll dispatch.
- Started-without-finished runs are marked interrupted, not restored as live.
- Interrupted-run recovery is idempotent across repeated restarts.
- Parked issues and counters survive restart with the correct release policy.
- Retry timers are reconstructed from durable due times computed from ledger timestamps plus delays.
- Terminal known workspaces are cleaned without scanning all Linear terminal issues.
- Pending v2 outbox items replay and completed outbox items do not.
- Payload-less pending outbox items fail clearly rather than being dropped.
- README documents at-least-once behavior, outbox payload limits, and remaining stale-lock/manual recovery boundary.
- The full deterministic suite passes.

## Rollout, Recovery, and Idempotence

Roll out after the ledger format is stable. On first startup after this phase, an empty ledger produces the same behavior as current Scherzo. As daemon transitions occur, it begins recording durable facts.

The record extensions are additive for the current hardening-02 schema. After a hardening-03 daemon writes new record kinds, older binaries that reject unknown kinds may not be able to replay the ledger. Rollback therefore requires either using a binary that understands the new records or backing up and deliberately moving aside the ledger, accepting loss of local recovery state.

If startup detects ledger corruption in the middle of the file or in `snapshot.json`, it should fail before dispatching. Operators should back up the ledger, inspect it, and either restore from a clean snapshot or deliberately move the ledger aside, accepting loss of local recovery state.

If startup detects a truncated trailing JSONL record, it should log the warning returned by replay and continue from the valid prefix. If a recovery append was partially written, the idempotence rules around `source_run_id` and retry/park records must keep the next startup from double-counting interrupted runs.

Recovery is at-least-once. A crash in the tiny window between a real-world side effect and its completed outbox record can still produce duplicate Linear comments on replay. Run IDs and source comment ids make duplicates auditable.

If the old process left `workspace.root/.scherzo-state/instance.lock`, this phase does not take over automatically. The operator must verify no old daemon is running and remove the stale lock before starting recovery.

## Artifacts and Notes

Target recovery transcript shape:

    level=info service=scherzo event=ledger_replay_ok records=42 truncated_tail=false
    level=warn service=scherzo event=run_marked_interrupted run_id=LIV-9-... issue_id=...
    level=info service=scherzo event=recovered_retry_scheduled issue_id=... delay_ms=0 generation=...
    level=info service=scherzo event=recovered_parked_issue issue_id=... reason=max_retry_attempts release_policy=auto_unpark_on_issue_change
    level=info service=scherzo event=recovered_workspace_cleanup issue_id=... workspace_path=...
    level=info service=scherzo event=outbox_replay_enqueued outbox_id=... kind=...

Example additive record shapes:

    IssueCounterUpdated(issue_id="issue-1", issue_identifier="LIV-9", failure_attempts=2, worker_sessions=0, observed_updated_at_ms=1714320000000, source_run_id=Some("LIV-9-1714320000000-1"))
    KnownWorkspace(issue_id="issue-1", issue_identifier="LIV-9", workspace_path=".scherzo/workspaces/LIV-9")
    IssueParkedV2(issue_id="issue-1", issue_identifier="LIV-9", reason="max_retry_attempts", release_policy="auto_unpark_on_issue_change", issue_fingerprint="...", observed_updated_at_ms=1714320000000)
    OutboxPendingV2(outbox_id="handoff-success-LIV-9-...", issue_id="issue-1", outbox_kind="linear_comment", dedupe_key="run:LIV-9-...:success", payload_json="{...bounded redacted payload...}")

This plan does not change distributed behavior. Two daemons with different workspace roots can still duplicate work.

## Interfaces and Dependencies

In `src/scherzo/state/recovery.gleam`, expose functions equivalent to:

    pub type RecoveredRetry {
      RecoveredRetry(
        issue_id: String,
        issue_identifier: String,
        delay_ms: Int,
        generation: Int,
        reason: String,
      )
    }

    pub type CleanupRequest {
      CleanupRequest(issue_id: String, issue_identifier: String, workspace_path: String)
    }

    pub type OutboxReplay {
      OutboxReplay(
        outbox_id: String,
        issue_id: String,
        outbox_kind: String,
        dedupe_key: String,
        payload_json: String,
      )
    }

    pub type RecoveryPlan {
      RecoveryPlan(
        runtime: domain.RuntimeState,
        retry_timers: List(RecoveredRetry),
        records_to_append: List(record.LedgerRecord),
        cleanup_workspaces: List(CleanupRequest),
        outbox_to_replay: List(OutboxReplay),
        warnings: List(String),
      )
    }

    pub type RecoveryError {
      MissingOutboxPayload(outbox_id: String)
      InvalidRecordSemantics(reason: String)
    }

    pub fn known_issue_ids(projection: projection.Projection) -> List(String)

    pub fn plan(
      projection: projection.Projection,
      config: domain.EffectiveConfig,
      refreshed_issues: List(domain.Issue),
      now_ms: Int,
    ) -> Result(RecoveryPlan, RecoveryError)

In `daemon.RuntimeDependencies`, add a ledger reader/writer seam or state-store seam that production implements using `src/scherzo/state/ledger.gleam` and tests can replace with in-memory fakes. The seam should support replaying the current projection, appending one or more records with an fsync flag, and constructing the ledger path from `effective.workspace.root`.

In `src/scherzo/state/projection.gleam`, expose helper views equivalent to:

    pub fn known_issue_ids(projection: Projection) -> List(String)
    pub fn known_workspace_for_issue(projection: Projection, issue_id: String) -> Result(String, Nil)
    pub fn latest_counter(projection: Projection, issue_id: String) -> domain.IssueCounter
    pub fn counter_has_source_run(projection: Projection, issue_id: String, run_id: String) -> Bool
    pub fn retry_due_at_ms(status: RetryStatus) -> Result(Int, Nil)
    pub fn pending_outbox_replays(projection: Projection) -> Result(List(OutboxReplay), RecoveryError)

No new package dependency should be required beyond the ledger modules from hardening 02. Add helper modules under `src/scherzo/state/` if needed to keep record encoding, projection, recovery, and outbox payload logic small and testable.
