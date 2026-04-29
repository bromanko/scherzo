# Hardening 02: Add a local durable state ledger

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo has a local, versioned, append-only durable state ledger under `workspace.root/.scherzo-state/` that can record scheduler and command facts without changing daemon recovery behavior yet. The visible proof is that deterministic tests can append run, retry, parking, command, and outbox records; replay them into an in-memory projection; tolerate a truncated trailing JSONL record; reject unsupported schema versions clearly; and compact records into a snapshot plus a new log segment.

This phase builds the storage foundation for later single-instance crash recovery and durable Linear command receipts. It does not yet restore retry timers, parked issues, command receipts, or interrupted runs on daemon startup. Later hardening plans consume this ledger.

## Problem Framing and Constraints

Scherzo currently keeps scheduler state in memory. If the daemon exits unexpectedly, it loses retry timers, parked issues, retry/session counters, processed Linear command ids, pending acknowledgement state, and knowledge of which worker runs were in progress. Some external facts survive in Linear comments/state and in workspaces, but there is no local durable record that can distinguish a completed run from an interrupted one or a processed command from a new command after restart.

Before changing recovery behavior, Scherzo needs a small, reliable storage primitive. The repository is a single-daemon design today, protected by a local instance lock for one canonical workspace root. That means a simple append-only JSON Lines ledger is proportionate: one writer, easy tests, human-inspectable files, no database dependency, and straightforward corruption recovery for trailing partial writes.

The ledger must not store secrets or unredacted raw pi payloads. It is operational state, not a transcript archive. EventHub data remains in memory until a separate event archive plan intentionally persists redacted events.

## Strategy Overview

Create a new package under `src/scherzo/state/`. The core module, `src/scherzo/state/ledger.gleam`, defines versioned record types, JSON encoders/decoders, append operations, replay, and compaction. Store ledger files under `workspace.root/.scherzo-state/ledger/`:

    workspace.root/.scherzo-state/ledger/current.jsonl
    workspace.root/.scherzo-state/ledger/snapshot.json
    workspace.root/.scherzo-state/ledger/archive/<timestamp-or-generation>.jsonl

Each JSONL line is one object with at least `schema_version`, `record_id`, `kind`, and `at_ms`. The record id is deterministic enough for tests and unique enough for production, for example `<at_ms>-<sequence>-<kind>`. The ledger append function owns the sequence within one process.

Add records for the facts future plans need, but do not wire every daemon transition yet. The first schema should include records for run start/finish/interruption, retry scheduled/cancelled, issue parked/unparked, Linear command seen/started/completed/acked, and generic outbox pending/completed/failed. The later recovery plans can choose which records to emit from the daemon.

Use a small Erlang FFI only if needed for atomic append and fsync. Gleam `simplifile` is enough for reading and writing whole files, but append durability should avoid read-modify-write races and should flush records when requested. Because the instance lock gives one writer, the FFI can be minimal.

## Alternatives Considered

One alternative is SQLite. SQLite is robust and gives transactions, but it adds a new runtime dependency and schema migration surface. For one local writer and append/replay semantics, JSONL is smaller and easier to inspect.

Another alternative is to serialize the whole runtime state as one JSON snapshot after every transition. That is simple to read but risks losing the file on partial writes, makes audit history harder, and couples durable format too tightly to current in-memory `domain.RuntimeState` shape.

A third alternative is to write durable state directly inside the recovery plan. That would make the recovery plan too large and would mix storage correctness with scheduler semantics. This plan proves storage separately.

A fourth alternative is to persist EventHub events and derive scheduler state from them. That is wrong because EventHub is observability-only; scheduler facts need explicit, stable records.

## Risks and Countermeasures

The main durability risk is corrupting the ledger on crash during write. Countermeasure: each append writes one full JSON object followed by `\n`. Replay ignores at most one malformed trailing line and returns an error for malformed non-trailing lines. Optional fsync can be enabled for records that future plans require before applying side effects.

The main schema risk is making future migrations impossible. Countermeasure: every record has `schema_version: 1`, and the decoder rejects unsupported versions with `UnsupportedVersion`. Add tests for unknown future versions.

The main secrecy risk is writing API keys, prompt text, raw pi JSON, or full Linear comment bodies. Countermeasure: ledger records store identifiers, statuses, result codes, bounded excerpts, and redacted strings only. Tests should inject a fake secret into command/result excerpts and assert JSONL output does not contain it.

The main scope risk is accidentally changing runtime behavior while adding storage. Countermeasure: this plan adds modules and tests only, plus optional no-op wiring behind test helpers if needed. The daemon should not depend on ledger replay until the next plan.

The main file-growth risk is unbounded JSONL files. Countermeasure: include compaction in this storage phase. Compaction writes a `snapshot.json` projection and archives or truncates old segments. The first production wiring may not call compaction automatically, but the API and tests must exist.

## Progress

- [x] (2026-04-29 04:20Z) Drafted this plan as the storage prerequisite for single-instance crash recovery and durable Linear command inbox work.
- [ ] Add ledger record types, encoders, decoders, and replay projection.
- [ ] Add append and fsync-safe file helpers.
- [ ] Add corrupt trailing record and unsupported-version tests.
- [ ] Add compaction/snapshot tests.
- [ ] Document the ledger format and its non-goals.

## Surprises & Discoveries

(To be filled during implementation. Record whether `simplifile` append support was sufficient or an Erlang FFI was required.)

## Decision Log

- Decision: Use append-only JSONL plus snapshots instead of SQLite for the first durable state store.
  Rationale: Scherzo has one local writer protected by the instance lock. JSONL keeps the first durable layer inspectable, small, and dependency-free.
  Date: 2026-04-29

- Decision: Do not derive durable scheduler state from EventHub events.
  Rationale: EventHub is observability-only and intentionally lossy/bounded. Recovery needs explicit scheduler facts.
  Date: 2026-04-29

- Decision: Implement storage without changing daemon recovery behavior in this phase.
  Rationale: Separating storage correctness from recovery semantics keeps each plan reviewable and gives later plans a tested primitive.
  Date: 2026-04-29

## Outcomes & Retrospective

(To be filled at completion. Include final file layout, whether fsync is used, final test count, and any schema changes made during implementation.)

## Context and Orientation

Scherzo stores runtime files under `workspace.root/.scherzo-state/`. The instance lock lives at `.scherzo-state/instance.lock`; workspace population markers live at `.scherzo-state/<workspace-key>.populating`; the local control API writes a control file under the state directory. This plan adds a `ledger/` subdirectory under the same state root.

The daemon actor in `src/scherzo/orchestrator/daemon.gleam` owns scheduler state. The pure scheduler in `src/scherzo/orchestrator/core.gleam` produces effects such as `Dispatch`, `ScheduleRetry`, `CancelRetry`, `CleanupWorkspace`, `ReleaseClaim`, `StopWorker`, and `ParkIssue`. The Linear command transport in `src/scherzo/control/linear_transport.gleam` currently keeps processed comment ids in memory. Later plans will record selected daemon transitions and command facts in the ledger.

The ledger is not a lock and not a distributed database. It is local durable state for one canonical workspace root.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/hardening-01-graceful-daemon-lifecycle.md` should be complete or deliberately deferred. The ledger can be implemented before graceful signals, but recovery behavior is easier to validate after graceful lifecycle is stable.
- `direnv exec . gleam test` passes. On 2026-04-29 the suite reported `200 passed, no failures`.
- `workspace.root/.scherzo-state/` is already used by Scherzo and is safe as the parent for durable local files.
- Only one daemon process should write the ledger because the local instance lock is already required for daemon mode.

## Scope Boundaries

In scope: ledger file layout; record schema versioning; JSON encoders/decoders; append API; replay API; projection type; corruption handling for trailing records; unsupported-version errors; compaction/snapshot API; tests; README format note.

Out of scope: daemon startup recovery; restoring retry timers; preserving Linear command receipts in production; durable EventHub archive; distributed claims; multi-writer locking beyond the existing instance lock; storing full prompt text, raw pi JSON, or full Linear comment bodies.

## Milestones

Milestone 1 defines record schema and pure JSON behavior. At the end, tests can encode/decode every record type and reject unsupported versions.

Milestone 2 adds file append and replay. At the end, tests can append records to `current.jsonl`, replay them into a projection, and ignore one truncated trailing record.

Milestone 3 adds snapshots and compaction. At the end, tests can write a snapshot, compact a ledger, and replay from snapshot plus current segment.

Milestone 4 documents the format and validates. At the end, README or a dedicated doc describes the ledger's file layout, non-goals, and manual inspection notes.

## Plan of Work

Create `src/scherzo/state/record.gleam`. Define stable record variants. Use only strings, ints, booleans, and lists that can be encoded predictably. Avoid embedding current `domain.RuntimeState` directly.

Initial record variants should be equivalent to:

    RunStarted(run_id, issue_id, issue_identifier, workspace_path)
    RunFinished(run_id, issue_id, classification, token_total, turns)
    RunInterrupted(run_id, issue_id, reason)
    RetryScheduled(issue_id, issue_identifier, due_at_ms, generation, reason)
    RetryCancelled(issue_id, generation, reason)
    IssueParked(issue_id, issue_identifier, reason, observed_updated_at_ms)
    IssueUnparked(issue_id, issue_identifier, reason)
    LinearCommandSeen(comment_id, issue_id, author_id, command_name, excerpt)
    LinearCommandStarted(comment_id, issue_id, command_name)
    LinearCommandCompleted(comment_id, issue_id, status, message_excerpt)
    LinearCommandAcked(comment_id, issue_id)
    OutboxPending(outbox_id, issue_id, kind, dedupe_key)
    OutboxCompleted(outbox_id, issue_id, kind)
    OutboxFailed(outbox_id, issue_id, kind, error_code)

Create `src/scherzo/state/ledger.gleam`. Define `LedgerPath`, `AppendOptions(fsync: Bool)`, `LedgerError`, `append`, `append_many`, `read_records`, `replay`, `compact`, and `load_projection`. The projection should be a simple type that future plans can extend, not the daemon runtime state itself.

Create `src/scherzo/state/projection.gleam`. It should fold records into maps of run status, retry status, parked issues, command receipts, and outbox statuses. This projection is for replay tests and future recovery planning; daemon code does not use it yet.

Implement file helpers. Prefer `simplifile` for directory creation and reading. If append/fsync is not available, add `src/scherzo_state_ffi.erl` with `append_line(path, line, fsync) -> Result(Nil, String)` using Erlang file APIs. Ensure each record line ends with exactly one newline.

Replay behavior must distinguish malformed trailing line from malformed middle line. If the last line is non-empty and invalid JSON, return records up to that line with a `truncated_tail: True` flag or warning value. If any earlier line is malformed, return an error because the ledger's integrity is uncertain.

Compaction should write a snapshot file through a temp path and rename it into place. After snapshot write succeeds, move the old `current.jsonl` into `archive/` or truncate it. Tests can accept either archive or truncate as long as replay remains correct and no records are lost.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress.

2. Create `test/state_record_test.gleam`. Add `encodes_and_decodes_run_records_test`, `encodes_and_decodes_retry_and_park_records_test`, `encodes_and_decodes_linear_command_records_test`, and `unsupported_schema_version_is_rejected_test`.

3. Implement `src/scherzo/state/record.gleam` with JSON encoders/decoders until the record tests pass.

4. Add `redacts_record_excerpts_test`: encode a command/result record with excerpt containing `secret-value` after passing a secret list and assert the JSON does not contain the secret. If redaction is handled before record construction instead, add a constructor helper that performs redaction and test that helper.

5. Create `test/state_ledger_test.gleam`. Add `append_and_replay_records_test`: append run started, retry scheduled, issue parked, and command completed records to a temporary workspace root, replay them, and assert the projection contains those facts.

6. Implement `src/scherzo/state/ledger.gleam` and file append helpers. Add an Erlang FFI only if needed.

7. Add `replay_ignores_truncated_trailing_line_test`: write two valid JSONL records and one partial trailing line; assert replay returns the two valid records plus a warning/truncated flag.

8. Add `replay_rejects_malformed_middle_line_test`: write valid, invalid, valid lines; assert replay returns a ledger corruption error.

9. Create `src/scherzo/state/projection.gleam`. Add tests that folding records produces expected run statuses, retry entries, parked entries, command receipt statuses, and outbox statuses.

10. Add `test/state_compaction_test.gleam`. Append enough records to produce a non-empty projection, call `ledger.compact`, and assert `load_projection` after compaction matches the pre-compaction projection.

11. Add `compaction_uses_temp_then_rename_test` if practical by injecting file operations; otherwise document the atomicity limitations in Outcomes.

12. Update README with a short `Local durable ledger` note saying the ledger exists as a foundation and is not yet used for startup recovery until the next plan lands.

13. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count in Progress.

14. Commit the phase with a message such as `Add local durable state ledger`.

## Testing and Falsifiability

This plan is falsified if records cannot be decoded after encoding, if unsupported schema versions are accepted silently, if replay crashes on a truncated trailing line, if malformed middle records are ignored, if secrets appear in persisted JSONL, if compaction changes the projection, or if the ledger writes outside `workspace.root/.scherzo-state/ledger`.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

No test may require real Linear, real pi, a running daemon, or OS crash simulation.

## Validation and Acceptance

Accept this phase when:

- All record types have deterministic JSON encoders/decoders.
- `current.jsonl` append and replay are tested.
- One truncated trailing line is tolerated and surfaced as a warning.
- Malformed non-trailing lines fail replay.
- Snapshots/compaction preserve projection state.
- Ledger files do not contain configured secret values in tests.
- README documents the ledger as a foundation, not active recovery.
- The full deterministic suite passes.

## Rollout, Recovery, and Idempotence

This phase should not change production daemon behavior unless a developer explicitly calls ledger APIs. It is safe to merge before recovery wiring because no startup path depends on the ledger yet.

If ledger files are manually deleted before later recovery plans use them, current Scherzo behavior is unchanged. After later plans depend on the ledger, deletion will mean losing local durable recovery state.

Append operations should be idempotent only when future callers use stable dedupe keys; this storage phase does not enforce global uniqueness except where projection folding naturally keeps the latest status for an id.

## Artifacts and Notes

Example JSONL line shape:

    {"schema_version":1,"record_id":"1714320000000-1-run_started","at_ms":1714320000000,"kind":"run_started","run_id":"LIV-9-1714320000000-1","issue_id":"issue-id","issue_identifier":"LIV-9","workspace_path":".scherzo/workspaces/LIV-9"}

Example directory shape:

    .scherzo-state/
      instance.lock
      control.json
      ledger/
        current.jsonl
        snapshot.json
        archive/

## Interfaces and Dependencies

In `src/scherzo/state/ledger.gleam`, expose functions equivalent to:

    pub type LedgerError {
      Io(String)
      UnsupportedVersion(Int)
      CorruptRecord(line: Int, reason: String)
    }

    pub type ReplayResult {
      ReplayResult(
        records: List(record.LedgerRecord),
        projection: projection.Projection,
        truncated_tail: Bool,
      )
    }

    pub fn path_for_workspace_root(workspace_root: String) -> Result(LedgerPath, LedgerError)
    pub fn append(path: LedgerPath, record: record.LedgerRecord, fsync: Bool) -> Result(Nil, LedgerError)
    pub fn append_many(path: LedgerPath, records: List(record.LedgerRecord), fsync: Bool) -> Result(Nil, LedgerError)
    pub fn replay(path: LedgerPath) -> Result(ReplayResult, LedgerError)
    pub fn compact(path: LedgerPath) -> Result(Nil, LedgerError)

No new package dependency should be required. Add a small Erlang FFI for append/fsync only if existing file helpers are insufficient.
