# Establish one ledger projection authority

This ExecPlan review is the concise human-facing companion to the structured implementation pack. Together they are the living plan; Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must remain current during implementation.

## Purpose / Big Picture

Scherzo operators should be able to trust that a committed workflow checkpoint is visible to the next retry, recovery, scheduling, outbox, metrics, or query decision. After this change, `scherzo/state/ledger` is the sole VM-local authority for the current online projection. Every decision receives one immutable, versioned projection snapshot and uses it for the whole command, so the LIV-1526 sequence cannot resolve a run from fresh state and then plan from an older daemon copy.

JSONL segments, archives, and `snapshot.json` remain the durable source of truth. “Online projection” means the bounded state available to normal commands after retention pruning; it does not claim to contain all archived history.

## Problem Framing and Constraints

The repository currently has two independently advanced current projections. The cache in `src/scherzo/state/ledger.gleam` is updated by every public ledger append API, while `src/scherzo/orchestrator/query_projection.gleam` stores a second projection that advances only through the daemon append wrapper or manual pruning. Workflow execution, scheduled execution, publication recording, recovery checkpoints, and workstream operations can append through `src/scherzo/workflow_checkpoint.gleam` without advancing the daemon copy. Operator paths also replay or fold disk records ad hoc and then return to the resident copy later in one request. Published query snapshots inherit that copy.

The implementation must preserve single-VM writer serialization, JSONL durability, exact duplicate/conflict behavior, online retention and archive reconstruction, and the warm-cache performance established by LIV-1457. It must not introduce a full-ledger scan on ordinary appends, actor messages, control commands, or queries. Cross-VM concurrent writers and a database replacement remain outside this work.

## Strategy Overview

Promote the ledger module’s existing VM-local cache into an explicit current-projection service. It will return an immutable `ProjectionSnapshot` containing the online `projection.Projection`, current-segment statistics, truncated-tail state, and a `ProjectionCursor`. The cursor consists of a durable snapshot generation, current-segment byte offset, and a fingerprint digest. Generation is optional metadata in old snapshots, starts at zero when absent, and increments only when compaction or archive reconstruction commits a new fold boundary. Cursor equality proves derivative freshness; generation and offset order duplicate or delayed notifications; a fingerprint disagreement at the same logical position forces invalidation rather than guessing.

All ledger append variants will commit under the existing ledger lock and return the exact committed snapshot and cursor. Workflow checkpoints, schedules, publications, workstreams, daemon effects, startup recovery, and guarded offline tools therefore communicate with the authority by calling it directly. A daemon notification is only a best-effort wake-up for compaction and query publication, never part of correctness. Daemon-owned derivatives and query-cache snapshots carry the cursor that produced them and may be used only after matching the authority.

Every operator command, query, scheduled evaluation, and queued control-operation execution acquires one command-scoped snapshot. Target resolution, admission, planning, dry-run rendering, and append preconditions receive that same value explicitly; helpers must not reload or replay behind the caller. Projection-dependent mutations use a cursor precondition and recompute from a new snapshot on a stale decision before external effects. Query execution reads one whole cache snapshot rather than separate projection, outbox, and dispatch fields, then reconciles it with one authoritative ledger snapshot. Stale cached ledger fields are rebuilt from that authoritative value or the query fails closed. No public query JSON change is required because cursor metadata remains internal.

Compaction and retain-all reconstruction are authority commits. They publish a new generation and online projection only after snapshot, archive, coverage, marker, and cache work succeeds. The daemon consumes the returned snapshot instead of pruning its own copy. Archived replay remains an explicitly historical, offline-only type and can never be passed where a current online snapshot is required.

## Alternatives Considered

Fixing only retry-step dry-run to reuse its replayed projection is the smallest symptom repair, but every other direct checkpoint append and resident consumer could still diverge. Making the daemon actor the authority and requiring every writer to notify it was rejected because workflow workers already write concurrently, a durable append could succeed while the actor message is lost, and offline/startup paths would need a second protocol. Keeping both projections with periodic parity checks was rejected because detection after a wrong decision is not an ownership contract. Replaying the ledger at each command boundary was rejected because it reverses LIV-1025 and LIV-1457. The selected ledger authority reuses the lock, cache, fingerprint, record-id index, compaction transaction, and direct writer boundary already shared by production code.

## Risks and Countermeasures

The most dangerous partial failure is a durable append followed by cache-update failure. Append records must therefore be retry-safe by record id, and the authority must distinguish “not committed” from “committed but projection temporarily unavailable.” It invalidates and synchronously rehydrates under the lock; if that fails, dependent daemon effects, query publication, and control results stop rather than using an older cursor. A later authority read self-heals from disk. Ambiguous sync or close failures probe the exact record ids and retry file synchronization before returning a disposition.

A lost or delayed checkpoint notification could leave compaction counters or a published derivative behind. Notifications carry cursors, are idempotent, and may arrive out of order; the daemon ignores older cursors and refreshes from the authority at the next decision. Query serving compares cursors and never labels an unmatched cached snapshot current. Cache-process publication becomes acknowledged; failure makes the query path unavailable until refresh or restart instead of serving stale ledger fields.

A broad migration could leave hidden replay calls or resident reads. The countermeasure is an additive sequence with a parity phase: introduce snapshots and cursors first, migrate writers, make command contexts explicit, migrate each consumer family, then remove `query_projection.State.ledger_projection`, manual folds, and field-by-field query getters. Search-based architecture tests block reintroduction.

Compaction could publish a cursor for a partly rotated ledger. Generation advances only at the final successful transaction boundary; rollback restores snapshot, current segment, metadata, cache, and prior cursor. Pruned online state remains visibly distinct from archive history, and malformed coverage or pruned-run metadata continues to fail closed.

## Scope Boundaries

In scope are the ledger cache, fingerprint and snapshot metadata, every production append entry point, workflow checkpoint writers, startup recovery, daemon effects, retry and recovery controls, scheduling, publication and workstream paths, outbox and metrics derivation, query publication and serving, online pruning, compaction, archive reconstruction, and deterministic concurrency/failure tests. Internal function signatures and snapshot metadata may change without compatibility shims because Scherzo is pre-release.

The durable record schema, tracker authority, public query response schema, provider behavior, and workflow YAML contracts stay unchanged. Replacing JSONL, deleting raw archives, cross-VM locking, targeted reconstruction of one pruned run, and UI/browser work are out of scope. Offline mutation remains guarded by the daemon instance lock.

## Milestones

Milestone 1 adds the versioned authority beside existing APIs. Ledger hydration, append, duplicate delivery, compaction, rollback, and restart produce exact snapshots and cursors, while compatibility accessors delegate to the new service. Existing daemon state remains temporarily, and parity assertions prove whether the authority model is viable before callers move.

Milestone 2 migrates every writer and startup path. Daemon appends and workflow checkpoint observers receive commit results, startup recovery uses the post-commit authoritative snapshot instead of a manual fold, notification cursors update compaction accounting without scanning the current segment, and direct publication/workstream/offline writers are classified as authority calls rather than daemon publications.

Milestone 3 establishes command-scoped reads, beginning with the LIV-1526 retry regression. Operator resolution, dry-run, queued control operations, recovery, finalization, publication retry, work-item action derivation, and transition retry policy all accept one explicit snapshot. Stale mutation preconditions retry before side effects. Ad hoc current-state replays are removed or renamed as historical offline operations.

Milestone 4 makes query, scheduling, outbox, metrics, compaction, and pruning cursor-aware. Queries consume one whole snapshot and fail closed on authority errors; compaction returns the new authoritative projection; query publication is acknowledged; duplicate and out-of-order deliveries are harmless. The daemon remains usable after each slice because the cursor-bearing derivative coexists with the old accessor until its consumer family is migrated.

Milestone 5 removes the second owner and proves resilience and cost. `query_projection.State.ledger_projection`, daemon-side `projection.fold_from` after append, manual prune updates, and independent projection/outbox query getters disappear. Deterministic tests cover restart, concurrency, all partial failures, and online-versus-historical state; instrumentation proves warm appends and normal control/query reads perform no full-ledger fold.

## Progress

- [x] (2026-07-19 18:14Z) Read the repository-local ExecPlan guidance and prepared target metadata.
- [x] (2026-07-19 18:14Z) Inventoried production append entry points, current/replay readers, daemon projection consumers, query-cache getters, checkpoint notifications, compaction, pruning, and archive reconstruction.
- [x] (2026-07-19 18:14Z) Chose the ledger module’s VM-local projection service as the sole current-projection authority and defined the cursor, command-snapshot, failure, and migration contracts.
- [x] (2026-07-19 18:14Z) Authored this review document and the accompanying structured implementation pack.
- [ ] Implement Milestones 1–5, updating this section and the decision/discovery/retrospective sections after every stopping point.

## Surprises & Discoveries

The existing `LedgerCheckpointAppended` message does not refresh the daemon projection; it refreshes current-segment compaction statistics only. The checkpoint writer’s append observer therefore creates the appearance of synchronization without closing the freshness gap.

The retry path is more mixed than a replay-versus-resident pair: `replay_projection_for_operator` folds only `read_records`, which reads `current.jsonl`, while `ledger.load_projection` includes `snapshot.json` and current state. After compaction, those are not equivalent sources. Query work-item detail also reads dispatch-paused state and projection through separate cache requests, so an update can land between them.

The authority cache already detects external current, snapshot, and archive changes using size, timestamps, inode, and archive listing, and compaction already refreshes or invalidates it. The missing architecture is versioned ownership and disciplined consumption, not another projection engine.

## Decision Log

- Decision: `src/scherzo/state/ledger.gleam` owns the sole current online projection service; the daemon does not own a peer projection. Rationale: every production writer already crosses the ledger module, including worker checkpoints and offline/startup paths. Date: 2026-07-19.
- Decision: identify snapshots with durable generation, current byte offset, and fingerprint digest, stored as optional snapshot metadata and internal query metadata. Rationale: cursor equality detects stale derivatives across append, compaction, restart, and same-size file replacement without changing public query schemas. Date: 2026-07-19.
- Decision: acquire and pass one immutable snapshot per decision, with cursor-conditional commits for projection-dependent mutations. Rationale: explicit values prevent helper reloads and make concurrent checkpoint writes observable before side effects. Date: 2026-07-19.
- Decision: make notifications advisory and self-healing. Rationale: a process message cannot be atomic with an fsynced JSONL append, while the ledger authority can always recover from its durable files. Date: 2026-07-19.
- Decision: represent archived replay as historical state that is type-distinct from the online projection. Rationale: retention-pruned state must not masquerade as complete durable history, and reconstructed history must not leak into hot actor paths. Date: 2026-07-19.
- Decision: keep cursor metadata internal. Rationale: stale detection and parity can be enforced inside the daemon and query cache; no external client currently needs the value, so a public protocol migration would add cost without solving the ownership bug. Date: 2026-07-19.

## Outcomes & Retrospective

Planning is complete. The plan resolves ownership in favor of the ledger cache service, defines how every writer reaches that owner, prevents mixed-view commands, gives query and compaction derivatives a freshness identity, and provides an incremental removal path for the daemon copy. Implementation outcomes, measured cache diagnostics, and any deviations must be recorded here as milestones complete.

## Validation and Acceptance

Acceptance requires the LIV-1526 regression to show that a run advanced by direct workflow checkpoint writes after daemon startup is resolved and dry-run planned from the same failed snapshot. Tests must also cover direct scheduled and publication/workstream writes, concurrent daemon and checkpoint commits, cursor-conditional stale decisions, query projection/outbox/dispatch/metrics parity, successful and failed pruning, retain-all reconstruction, restart from legacy and cursor-bearing snapshots, duplicate records and notifications, out-of-order delivery, cache-update failure, ambiguous append failure, lost notification, and failed query publication.

The performance claim is false if any warm ordinary append, daemon decision, control query, or query-snapshot reconciliation folds all of `current.jsonl` or archives. Deterministic diagnostics must show one initial hydration, no reload or segment-fold increment across a large warm append/query scenario, and batch-size-only projection work. Full publish-blocking validation is `direnv exec . gleam format --check src test`, the unit and contract suites described in `test/README.md`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; all must exit successfully with no new warning inventory. No manual browser or provider-live evidence is required because public UI and provider protocols do not change.

## Rollout, Recovery, and Idempotence

Roll out through the parity migration: ship cursor-bearing authority snapshots first, then migrated writers and command contexts, then cursor-aware query/compaction derivatives, and remove the old resident projection last. Each commit must leave the daemon usable and tests green. Cursor mismatch or authority reload failure is a containment signal: reject the command or query, suppress dependent effects, invalidate the derivative, and retry hydration; never fall back to the old projection.

Existing ledgers require no rewrite. A legacy snapshot decodes as generation zero and gains cursor metadata on its next successful compaction. Reverting code remains possible while the new metadata is optional and ignored by the projection decoder. If compaction or reconstruction fails, restore the exact prior transaction state; if deployment must be backed out after successful pruning, stop the daemon, disable retention, and use the existing verified retain-all archive reconstruction or restore a copied ledger.

Append retries, duplicate checkpoint deliveries, repeated query publications, and repeated notifications are idempotent by record id and cursor. An older notification cannot replace a newer derivative. Offline repair, backup restore, and manual ledger edits continue to require a stopped daemon; the next process start hydrates the authority from disk.

## Open Questions and Clarifications Needed

No open questions block implementation. The plan deliberately chooses ledger ownership, internal cursor metadata, cursor-conditional decision commits, and no public query schema change rather than leaving those choices to the implementer.
