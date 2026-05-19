# Add the minimal workstream ledger and artifact snapshot store

This ExecPlan v2 review document is the human review surface for LIV-393. It covers Ticket 2 from the composable workstreams UberPlan: adding the durable ledger and immutable artifact snapshot foundation that later handoff, start-from-handoff, manual import, decision, playbook, and inspection work will use. Mechanical implementation steps, exact tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Ticket 2 makes workstream state durable before any operator command or workflow emitter depends on it. After the later implementation issue completes, Scherzo can create a local workstream, record assignment and artifact facts, persist exact artifact bytes in a content-addressed store, replay the state after restart, and safely coalesce repeated writes with the same idempotency key.

This enables future handoffs and input bundles to point at immutable snapshot refs instead of mutable repository paths or Linear comments. Operators still use current workflows unchanged until later tickets opt in.

## Problem Framing and Constraints

The parent UberPlan in `docs/plans/LIV-241-composable-workstreams-uberplan.html` requires local Scherzo state, not Linear comments, to be the MVP source of truth for cross-run artifacts and approvals. Ticket 0 and Ticket 1 are already merged: `src/scherzo/workstream/foundation.gleam` reuses existing validators, and `src/scherzo/workstream/types.gleam`, `src/scherzo/workstream/artifacts.gleam`, fixtures, schemas, and optional `workstream_phase` parsing define the artifact vocabulary.

Ticket 2 must stay foundational. It must add durable records and byte snapshots, but it must not add a handoff emitter, start-from-handoff command, manual import command, read-only workstream CLI, decision commands, playbook automation, Linear state changes, remote storage, backup/export/import, or dogfood workflow conversion. Existing ledger records, projection behavior, workflows, and top-level `contract` blocks must continue to parse and run unchanged.

## Strategy Overview

The strategy is to extend the existing append-only state style instead of creating a separate database. Workstream records should become additive `RecordBody` variants in `src/scherzo/state/record.gleam`, replay through `src/scherzo/state/projection.gleam`, and be appended through `src/scherzo/state/ledger.gleam` with a generic idempotent append helper.

The artifact store should be a small workstream module that stores exact bytes under the same `.scherzo-state` root using refs shaped like the Ticket 1 fixtures: `workstream-artifacts/sha256/<sha256>.json`. The hash and byte count are computed over the original bytes. The original repository-relative path or run-local ref is display metadata only; future handoffs, input bundles, and decisions target the immutable snapshot ref plus hash.

This is proportionate because it adds only the durable substrate that later tickets need. It deliberately avoids operator UX and automation until the append, replay, hash, path, and idempotency invariants are proven.

## Alternatives Considered

One alternative is to delay the ledger until start-from-handoff tooling exists. That is rejected because those commands would have to create temporary state or infer history from comments, then migrate later.

A second alternative is to store only repository paths plus hashes. That is rejected because paths drift; the approved bytes can be overwritten or deleted even if a hash mismatch later detects the drift.

A third alternative is to build a separate workstream database. That is too large for the MVP and would duplicate the existing JSONL ledger, lock, compaction, replay, and projection style.

A fourth alternative is to implement the full workstream projection and inspection CLI in this ticket. That is deferred because Ticket 2 only needs enough projection to prove replay, idempotency, and compatibility; human-facing inspection belongs to the later projection ticket.

## Risks and Countermeasures

The main risk is weakening append-only state with duplicate or conflicting records. The countermeasure is deterministic record IDs, explicit idempotency keys, and a locked append helper that returns an already-recorded result for identical records and rejects same-ID different-body conflicts.

A second risk is path escape or byte drift. The countermeasure is strict repository-relative path validation, realpath containment checks for existing files, tests for absolute and `..` paths, and hash verification after every write.

A third risk is breaking existing ledger and projection consumers. The countermeasure is additive record variants, optional projection snapshot fields for old snapshots, golden-fixture updates, and tests that current workflow-run, contract-manifest, parked-issue, Linear-command, scheduled-job, and outbox records still round-trip and replay.

A fourth risk is scope creep into handoff or command behavior. The countermeasure is a stop rule: once the ledger, snapshot, replay, and idempotency tests pass, the later implementation stops before adding any operator command or workflow emitter.

## Scope Boundaries

In scope for this planning issue is this single review document and one structured implementation-pack submission. No source implementation belongs in LIV-393.

In scope for the later Ticket 2 implementation issue are minimal workstream record bodies, deterministic record IDs, an idempotent ledger append path, a small workstream projection slice, and a content-addressed snapshot store for repository-relative files and existing contract-output or run-local artifact refs.

Out of scope are handoff emission, start-from-handoff tooling, manual import tooling, read-only inspection CLI, decision and gate commands, playbooks, auto-enqueue policy, Linear state changes, remote storage, backup/export/import, and dogfood workflow conversion. Existing workflows and existing top-level `contract` blocks remain unchanged.

## Milestones

The first milestone adds failing tests and a precise record model for the five minimal workstream events: created, assigned, artifact recorded, handoff recorded, and phase run queued.

The second milestone implements stable encoding and decoding for those records while keeping all existing ledger constructors and schema guardrails intact.

The third milestone adds immutable snapshot writing and reading under the Scherzo state root for both repository-relative files and existing contract-output or run-local artifact refs. It covers repository path validation, missing-file errors, missing or unresolvable artifact-ref errors, hash checks, byte counts, idempotent duplicate writes, and the rule that failed ref resolution or validation writes no partial snapshot.

The fourth milestone folds workstream records into a minimal projection and proves replay after restart without disturbing current projection consumers.

The final milestone runs the standard test, format, and lint gates, validates this review document, and stops before later workstream-aware commands or workflow emitters are added.

## Progress

- [x] (2026-05-19 00:00Z) Read the LIV-393 task, ExecPlan authoring guidance, parent UberPlan, Ticket 0 plan, Ticket 1 plan, and current workstream spec modules.
- [x] (2026-05-19 00:00Z) Inspected current state ledger, record, projection, artifact-store, contract-manifest, and workstream fixture surfaces relevant to Ticket 2.
- [x] (2026-05-19 00:00Z) Drafted this concise review document for human review.
- [x] (2026-05-19 00:00Z) Prepared the structured implementation-pack content for Scherzo capture.
- [x] (2026-05-19 04:03Z) Incorporated review feedback requiring existing contract-output and run-local artifact refs to be snapshot-tested for exact bytes, idempotency, and fail-closed missing or mismatch behavior.

## Decision Log

- Decision: Build Ticket 2 on the existing JSONL ledger and projection instead of a separate workstream store.
  Rationale: Scherzo already has local append-only state, locking, compaction, replay, and projection behavior that match the MVP durability requirement.
  Date: 2026-05-19.

- Decision: Keep snapshot refs content-addressed by SHA-256 of the original bytes and retain paths only for display.
  Rationale: Future approvals and starts must target immutable bytes, not mutable repository paths.
  Date: 2026-05-19.

- Decision: Add a generic idempotent append helper rather than making each future command solve duplicate handling itself.
  Rationale: The ledger lock is the correct place to distinguish identical retries from same-ID conflicts before handoff and start tooling exists.
  Date: 2026-05-19.

- Decision: Stop before operator commands, handoff emitters, decisions, playbooks, or Linear behavior.
  Rationale: Ticket 2 is the durable substrate; higher-level work needs separate acceptance tests after this foundation is proven.
  Date: 2026-05-19.

- Decision: Treat existing contract-output and run-local artifact refs as first-class snapshot sources alongside repository-relative files.
  Rationale: Future handoffs and approvals may start from artifacts already retained in `.scherzo-state/artifacts`, so Ticket 2 must prove those refs preserve exact bytes and fail without partial writes when refs are missing, malformed, unresolvable, or mismatched.
  Date: 2026-05-19.

## Validation and Acceptance

This planning issue is accepted when this Markdown review document exists under `docs/plans/`, `scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-393-composable-workstreams-ticket-2-minimal-ledger-snapshot-store.md` accepts it, and Scherzo captures the structured implementation-pack submission.

The later implementation issue is accepted only when tests prove repository-relative path handling, rejection of absolute and escaping paths, missing-file errors, exact hash and byte-count consistency, stable ledger encoding and decoding, replay after restart, idempotent snapshot writes, idempotent or explicitly rejected duplicate appends, and compatibility with existing state projection behavior. Acceptance also requires tests for snapshotting existing contract-output or run-local artifact refs: valid refs must preserve exact bytes, SHA-256, byte count, media type, and display ref metadata; duplicate writes of the same bytes must coalesce to the same workstream snapshot ref; and missing, malformed, unresolvable, hash-mismatched, or byte-count-mismatched refs must fail visibly without writing a partial workstream snapshot. Existing workflows and existing top-level `contract` blocks must continue to parse and run unchanged.

## Rollout, Recovery, and Idempotence

The planning change is additive. If review rejects this document, revise or remove only `docs/plans/LIV-393-composable-workstreams-ticket-2-minimal-ledger-snapshot-store.md` and resubmit the structured pack.

The later implementation should also be additive. It introduces new record kinds and snapshot files but does not change existing workflow dispatch. If a repository-file or existing-artifact-ref snapshot write is retried with the same bytes, it should return the same ref. If an existing artifact ref cannot be resolved or does not match its expected hash and byte count, it should fail visibly and leave no partial workstream snapshot. If a ledger append is retried with the same stable record ID and body, it should return an already-recorded result. If the same stable ID has different content, it should fail visibly and write nothing. Rollback before production use is removing the new modules and record variants; retained local snapshots can remain as inert audit files.

## Open Questions and Clarifications Needed

No blocking clarification is needed for Ticket 2. Later tickets still need to decide operator command syntax, handoff emission timing, read-only inspection UX, decision and gate semantics, playbook policy, and any export/import or remote-storage story.
