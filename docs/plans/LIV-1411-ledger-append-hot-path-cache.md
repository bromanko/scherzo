# Remove full-ledger scans from state ledger appends

This ExecPlan review is a concise human-facing companion to the structured implementation pack. The implementation pack carries the mechanical file-by-file steps.

## Purpose / Big Picture

Scherzo operators should be able to leave a daemon running with large local state without watching every ledger write burn seconds of CPU and block control queries. After this change, normal state appends validate against a resident projection and a resident record-id index, so scheduled jobs, workflow checkpoints, publication records, and workstream starts no longer re-decode the full snapshot, current segment, and archives on every write. The JSONL ledger and snapshot files remain the durable source of truth; the cache only removes repeated parsing from the hot path.

## Problem Framing and Constraints

The observed failure is real in current `src/scherzo/state/ledger.gleam`: `append` and `append_many` call `validate_append_batch_unlocked`, which calls `load_projection_unlocked`, which decodes `snapshot.json` and folds all of `current.jsonl` for every append. `append_idempotent` and `append_workstream_start_records` call `find_record_by_id_unlocked`, which line-scans the current segment and then every archived segment on a miss, even though misses are the common case. On large ledgers this serialized appends behind repeated JSON decoding and off-heap binary garbage collection. The solution must preserve aggregate validation, exact idempotency outcomes (`AlreadyRecorded` versus `RecordIdConflict`), crash consistency, and the reality that offline commands or manual restores can mutate files from another VM while the existing `global:trans` lock is only VM-local.

## Strategy Overview

Introduce a VM-local ledger cache for each ledger directory and route the existing public ledger append APIs through it. The cache keeps the folded `projection.Projection`, a compact record-id index, and a file fingerprint for `snapshot.json`, `current.jsonl`, and archive segment listings. Before a cached operation writes or serves a projection, it checks the fingerprint; if files changed outside the VM, it reloads from disk and only then proceeds. Successful appends write JSONL first and update the projection and record-id index only after the disk write succeeds. Compaction writes snapshot metadata for record ids so future startup can hydrate the index from snapshot metadata plus only uncovered segments. This is proportionate because it keeps the existing ledger API and disk format as the authority while removing the repeated full-ledger parse from normal appends.

## Alternatives Considered

Only compacting more often was rejected because it still leaves every append decoding the whole snapshot and lets the current segment regrow into the same failure. Adding a process-wide timeout or more scheduler capacity was rejected because the bottleneck is repeated JSON parsing under the write path, not waiting policy. Moving every offline mutation through the daemon was rejected as too large for this fix; instead, the plan combines cache invalidation with an explicit daemon-stopped policy and live-control-file rejection for offline state mutations. Storing full decoded records in the idempotency index was rejected because it gives exact duplicate bodies at high memory cost; a compact body hash plus targeted disk confirmation on duplicate hits preserves semantics with bounded memory.

## Risks and Countermeasures

A stale cache could validate against old state after `state compact`, `state repair-*`, manual edits, or `.bak` restores. The countermeasure is a file fingerprint check before cached operations, fail-closed reload behavior when the fingerprint changed but reload fails, and offline command guards that reject mutation while a live daemon control file exists. Idempotency could regress if the index treats a duplicate id with different body as already recorded; the countermeasure is to store body hashes, treat different hashes as conflicts, and perform a targeted disk probe on same-hash hits before returning `AlreadyRecorded`. Same-VM concurrent appends could race if cache updates bypass the existing ledger lock; the countermeasure is to keep every cache read-modify-write sequence inside `with_ledger_lock` and add concurrent append/idempotency tests that prove only one writer wins each record id. Crash consistency could regress if memory is updated before disk; the countermeasure is to update cache state only after append or compaction succeeds on disk. Memory could grow with record count; the countermeasure is to store only record id, body hash, timestamp, and locator metadata, expose index-size diagnostics, and warn rather than dropping entries until retention/pruning work lands separately.

## Scope Boundaries

In scope are `src/scherzo/state/ledger.gleam`, small supporting ledger cache/index/fingerprint modules or FFI helpers, snapshot metadata needed to hydrate the record-id index, tests for append validation, idempotency, same-VM concurrency under the ledger lock, compaction, external-change reloads, and offline mutation guards. In scope are documentation updates that state offline ledger repair, compaction, manual edits, and `.bak` restores require the daemon to be stopped. Out of scope are changing ledger record schema semantics, pruning retained archive segments, replacing JSONL durability with a database, broad daemon decomposition, UI changes, provider-live behavior, and any attempt to make simultaneous cross-VM writes safe beyond documented rejection and reload detection.

## Milestones

Milestone 1 builds and tests the cache data model without changing callers. It defines the record-id index, body hashing, file fingerprint, snapshot metadata decoder/encoder, and deterministic hydration from existing snapshots and segments. The proof is unit coverage for old snapshots with no metadata, new snapshots with metadata, corrupt records, truncated final lines, duplicate record ids, and compact memory-shaped entries.

Milestone 2 routes `load_projection`, `append`, `append_many`, `append_idempotent`, and `append_workstream_start_records` through the cache while preserving existing public results. The proof is that existing ledger and workflow checkpoint tests still pass, new tests show repeated appends do not trigger per-append projection reloads, idempotent miss, duplicate, and conflict paths match current behavior, and same-VM concurrent attempts for the same record id serialize to one append plus either `AlreadyRecorded` or `RecordIdConflict`.

Milestone 3 makes compaction and external mutation safe. Compaction writes record-id metadata with the projection snapshot, refreshes or invalidates the in-memory cache after success, and external fingerprint changes force reload before the next cached operation. The proof is tests for external current-segment edits, external compaction or restore-like snapshot changes, corrupt reload fail-closed behavior, and old/new snapshot compatibility.

Milestone 4 hardens operator safety, observability, and performance evidence. Offline state mutation commands reject when a daemon control file for the workspace is present, docs explain the daemon-stopped policy for manual repairs, cache diagnostics expose hydration, reload, fingerprint miss, and index-size information, and a synthetic large-ledger scenario demonstrates that appending after hydration no longer scales with total ledger bytes.

## Progress

- [x] (2026-07-08 00:00Z) Read the repo-local ExecPlan guidance and current ledger, FFI, projection, startup recovery, workflow checkpoint, compaction, and offline state command code paths.
- [x] (2026-07-08 00:00Z) Authored this concise review document and prepared the structured implementation pack for Scherzo handoff.
- [x] (2026-07-08 00:00Z) Incorporated review feedback by making concurrency/idempotency acceptance explicit in the review document and implementation pack.
- [ ] Implementation follow-up task has not started; update this section as milestones land.

## Surprises & Discoveries

The daemon already keeps `ledger_projection` in `src/scherzo/orchestrator/daemon.gleam` after successful daemon appends, but the lower-level `ledger.append_many` still reloads the projection from disk before validating, so the existing daemon state does not protect the hot path. Workflow checkpoint writers call `ledger.append_idempotent`, `ledger.append_workstream_start_records`, `ledger.append`, and `ledger.append_many` directly, so fixing only the daemon append wrapper would leave important append paths scanning disk.

## Decision Log

- Decision: Keep the existing public ledger API and make caching an internal implementation detail. Rationale: daemon, workflow checkpoint, CLI, and tests already share `src/scherzo/state/ledger.gleam`; preserving that boundary reduces migration risk. Date: 2026-07-08.
- Decision: Treat disk as authoritative and cache as advisory. Rationale: JSONL append plus snapshot remains the crash-consistency mechanism, while a stale or lost cache can always be rebuilt. Date: 2026-07-08.
- Decision: Use fingerprint reloads plus daemon-stopped offline mutation policy instead of promising cross-VM write locking. Rationale: the current `global:trans` lock is VM-local, and adding robust filesystem locking across all platforms is larger than the hot-path fix. Date: 2026-07-08.
- Decision: Store compact idempotency entries and confirm duplicate hits from disk when necessary. Rationale: this preserves exact duplicate/conflict semantics without storing every decoded record body in memory. Date: 2026-07-08.
- Decision: Treat same-VM concurrent append/idempotency coverage as blocking acceptance. Rationale: the cache is safe only if the existing ledger lock still serializes cache reads, disk writes, and in-memory updates as one critical section. Date: 2026-07-08.

## Outcomes & Retrospective

Not yet implemented. The expected outcome is that the first cache hydration may still parse existing ledger bytes, but subsequent normal appends in the same VM validate and update state incrementally, control queries stop timing out behind append-time scans, and operators retain a clear recovery path by stopping the daemon and letting the next operation reload from disk.

## Validation and Acceptance

Implementation is acceptable only with deterministic test and lint evidence. Run `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting formatting, all tests, and production lint gates to pass. New tests must prove projection validation still rejects orphan workflow-step records, same-batch parent/child appends still pass, idempotent append miss/duplicate/conflict results are unchanged, workstream start duplicate/conflict behavior is unchanged, same-VM concurrent idempotent appends for the same record id serialize to one successful append plus duplicate/conflict outcomes, compaction preserves projection and record-id metadata, old snapshots without metadata still load, external file mutations cause reload before cached writes, corrupt external mutations fail closed rather than writing with stale state, cache diagnostics report hydration, reload, fingerprint miss, and index-size counts, and repeated appends after one hydration do not perform per-append full snapshot/current/archive reloads. Performance evidence must include a synthetic ledger large enough to make the old O(ledger) behavior visible and must show append validation after cache warm-up scales with appended batch size rather than total ledger bytes.

## Rollout, Recovery, and Idempotence

Rollout is internal and additive. Existing ledger files remain readable, old snapshots without record-id metadata hydrate by scanning segments once, and new snapshots add optional metadata that older projection decoders ignore. If the cache is missing, stale, or invalidated, the next operation reloads from disk. If reload fails after an external change, writes fail rather than using old state. If a deployment must be backed out, the JSONL records and projection snapshot remain the source of truth; optional snapshot metadata can be ignored, and restarting the old code will replay from disk as before. Offline repair, compaction, manual edits, and `.bak` restores must be performed with the daemon stopped, then the daemon or next CLI invocation reloads from disk.

## Open Questions and Clarifications Needed

No blocking open questions. The exact warning threshold for record-id index size can be tuned during implementation, while archive pruning and long-term retention policy remain separate follow-up work.
