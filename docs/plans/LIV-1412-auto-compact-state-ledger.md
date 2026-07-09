# Auto-compact the state ledger in the daemon

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, a running Scherzo daemon will keep `.scherzo-state/ledger/current.jsonl` bounded without an operator remembering to run `state compact` offline. Operators should be able to tune `state_ledger.auto_compaction`, observe structured `ledger_compacted` and `ledger_compaction_failed` log events, and see the current segment return below the configured threshold while normal scheduled-job operation continues.

## Problem Framing and Constraints

Today ledger compaction exists only as an offline maintenance command, so active workspaces can accumulate tens of thousands of records and tens of megabytes in `current.jsonl`. Each append validates against the ledger projection, so an unbounded current segment turns every append into progressively more CPU work. Compaction must be serialized with appends because folding the current segment and then archiving it has a data-loss window if a record is appended between those phases. The daemon owns the relevant in-process append stream, while the existing `with_ledger_lock` serialization is VM-local and therefore suitable for daemon-owned compaction but not for an external process racing a live daemon. The public YAML schema in `schemas/scherzo.config.v1.schema.json` rejects unknown root keys, so the new thresholds must be modeled in config types, parsing, tests, and schema rather than accepted only by internal defaults.

## Strategy Overview

Add daemon-owned auto-compaction with configurable thresholds for current-segment record count, byte size, and minimum interval between compactions. The public YAML shape is `state_ledger.auto_compaction.enabled`, `state_ledger.auto_compaction.max_current_records`, `state_ledger.auto_compaction.max_current_bytes`, and `state_ledger.auto_compaction.min_interval`; the corresponding effective config type should use milliseconds for the interval. The daemon should initialize current-segment counters from `ledger.current_segment_stats`, evaluate the threshold once during startup so an already-oversized segment is compacted without waiting for another append, update counters after successful appends, enqueue compaction work onto a worker/effect process when a threshold is crossed, refresh current-segment counters from disk when that worker completes so appends that landed after the worker's post-measurement are not lost from in-memory accounting, and let the ledger module perform the actual fold, snapshot write, archive, and before/after measurement under the same ledger lock already used by appends. Keep the offline `state compact` command for stopped-daemon maintenance, but make it acquire the workspace instance lock before inspecting or compacting so it refuses to run when a live daemon owns the workspace.

## Alternatives Considered

Leaving compaction manual is the smallest change, but the observed production ledgers show that operators do not run it often enough and the resulting CPU cost is severe. Shelling out from the daemon to the existing CLI would reuse code but would reintroduce the unsafe external-process race and would make error handling and structured logging poorer. Compacting synchronously inside the daemon actor is simpler than a worker, but it can block the message loop for many seconds on large ledgers, which is exactly the operational failure this plan is meant to avoid.

## Risks and Countermeasures

The highest-risk failure is silent loss from archiving a segment that contains records not folded into the snapshot. The countermeasure is to keep folding, snapshot writing, archiving, and before/after measurement inside the ledger lock and to add concurrent-append tests that prove all records remain visible through `ledger.load_projection`. A second risk is repeatedly compacting an already-small segment or starting duplicate workers during a burst; the countermeasure is a configurable minimum interval, an in-flight guard, and tests that cross the threshold repeatedly while only one compaction is in flight. A third risk is miscounting bytes and failing to trigger compaction; the countermeasure is to add exact byte-size reporting to the ledger stats and assert it against filesystem `file_info` or UTF-8 byte counts. A fourth risk is snapshot growth becoming the next bottleneck; this work documents that retention/pruning remains out of scope and keeps the implementation focused on bounding `current.jsonl`.

## Scope Boundaries

In scope are daemon-triggered current-segment compaction, startup compaction for an already-oversized current segment, ledger-level reporting needed for structured logs, config parsing and schema updates for `state_ledger.auto_compaction`, the offline command guard using `instance.lock`, targeted tests, and a runbook update under `docs/runbooks/`. Out of scope are pruning or changing what the projection retains, cross-VM or cross-host distributed locking, changing archive format, changing scheduler semantics, and removing the offline maintenance command.

## Milestones

Milestone 1 adds the ledger primitives needed for safe reporting in `src/scherzo/state/ledger.gleam`: current-segment byte stats, exact JSONL append byte accounting, and a compaction report gathered while the ledger lock is held. Milestone 2 adds config defaults and validation in `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/config/duration_config.gleam`, and `schemas/scherzo.config.v1.schema.json` so operators can tune thresholds without code changes. Milestone 3 wires `src/scherzo/orchestrator/daemon.gleam` to initialize and track current-segment size cheaply, trigger once at startup when needed, launch compaction on a worker/effect process, suppress duplicate in-flight compactions, and log `ledger_compacted` or `ledger_compaction_failed` events. Milestone 4 hardens `src/scherzo/ctl/state_handlers.gleam` so both dry-run and confirmed offline compaction acquire `instance.lock`, then documents operator procedures in a runbook. Milestone 5 validates threshold-triggered, startup-triggered, failure, duplicate-suppression, lock-conflict, and concurrent-append scenarios.

## Progress

- [x] (2026-07-09) Reviewed the repo-local ExecPlan guidance and the current ledger, daemon, config, instance-lock, and `state compact` code paths.
- [x] (2026-07-09) Chose daemon-owned worker/effect compaction under the existing ledger lock as the plan direction.
- [x] (2026-07-09) Incorporated review feedback by making the config namespace, startup trigger, duplicate-suppression, failure logging, byte accounting, lock-conflict, and validation obligations explicit.
- [ ] Implement and validate the plan in a follow-up implementation workflow.

## Surprises & Discoveries

- Observation: `src/scherzo/state/ledger.gleam` already wraps `compact` and appends with `with_ledger_lock`, but `CurrentSegmentStats` currently reports record count only.
  Evidence: `CurrentSegmentStats(record_count, truncated_tail)` and `compact_locked` are in `src/scherzo/state/ledger.gleam`.
- Observation: the daemon updates its in-memory projection after successful appends in one central path.
  Evidence: `append_ledger_records` in `src/scherzo/orchestrator/daemon.gleam` calls `ledger.append_many` and then `projection.fold_from`.
- Observation: the offline command already reports before/after compaction details, which can be extended rather than replaced.
  Evidence: `StateCompactDetails` and `run_compact` are in `src/scherzo/ctl/state_handlers.gleam`.
- Observation: the daemon holds the workspace instance lock for its process lifetime, so the offline guard can reuse the existing lock mechanism instead of inventing a new sentinel.
  Evidence: `start_daemon_with_lifecycle` in `src/scherzo/orchestrator/service.gleam` calls `acquire_lock_for_workflow` before starting the daemon and releases it during shutdown.
- Observation: the public config schema is closed to unknown root keys, so a new YAML namespace must be added to both parser and schema.
  Evidence: `RootConfig` in `schemas/scherzo.config.v1.schema.json` has `additionalProperties: false`.

## Decision Log

- Decision: daemon auto-compaction will be additive and config-tunable, with conservative defaults of enabled, 10,000 records, 8 MiB, and a 5-minute minimum interval unless implementation evidence requires adjustment.
  Rationale: these defaults bound the segment well below the observed 45-50 MB ledgers while avoiding frequent compaction during bursts.
  Date: 2026-07-09.
- Decision: the offline command will acquire and release `instance.lock` around dry-run inspection and actual compaction.
  Rationale: refusing when the lock is held is the clearest operator signal and prevents external maintenance from racing a live daemon.
  Date: 2026-07-09.
- Decision: the public config namespace will be `state_ledger.auto_compaction`, with fields `enabled`, `max_current_records`, `max_current_bytes`, and `min_interval`.
  Rationale: the name matches the existing `state compact` operator vocabulary, groups the thresholds with the persisted ledger rather than scheduler or tracker config, and leaves room for future state-ledger settings without overloading the daemon section.
  Date: 2026-07-09.
- Decision: the daemon will check thresholds immediately after startup stats are initialized and after successful appends.
  Rationale: an already-large current segment is the common production failure mode, and waiting for another append would leave idle scheduled-job deployments unbounded until the next write.
  Date: 2026-07-09.

## Outcomes & Retrospective

No implementation outcome yet. At completion, record whether scheduled-job steady state kept `current.jsonl` under threshold, whether concurrent-append tests passed, and whether logs and runbook instructions were sufficient for an operator to diagnose compaction.

## Validation and Acceptance

Acceptance requires a ledger test where `current_segment_stats` reports exact byte size as well as record count; a compaction-report test where before and after stats are gathered under the ledger lock; a daemon test where appends cross the configured record threshold and the current segment is compacted automatically; a daemon startup test where an already-oversized current segment compacts without another append; a daemon in-flight test where repeated threshold crossings start only one compaction worker; a failure-path test where compaction failure logs `ledger_compaction_failed`, leaves `current.jsonl` intact, and does not spin in a tight loop; a safety test where records appended around compaction remain present in the loaded projection; config parser and schema tests for defaults, custom thresholds, disabled compaction, and invalid non-positive thresholds; a CLI test where `state compact --dry-run` and `state compact --yes` reject or clearly report an existing live instance lock; a structured-log assertion for `ledger_compacted` with before/after record and byte counts, archive segment count, and duration; and documentation that explains thresholds, safe offline maintenance, and recovery from failed compaction. Full validation must include targeted tests such as `direnv exec . gleam test test/state_ledger_test.gleam test/config_test.gleam test/ctl_state_compact_test.gleam test/orchestrator_daemon_test.gleam`, then `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Rollout, Recovery, and Idempotence

Rollout is safe because auto-compaction is additive, the default thresholds are conservative, operators can disable it with `state_ledger.auto_compaction.enabled: false`, and the offline command remains available when the daemon is stopped. No stored-data migration is required: existing `snapshot.json`, archive segments, and `current.jsonl` keep their formats. If compaction fails, the daemon should log `ledger_compaction_failed`, keep running, leave the current segment intact, record the attempt time so the minimum interval prevents a tight failure loop, and try again only after another threshold check outside that interval. Re-running compaction is idempotent for an empty current segment and safe for a non-empty current segment because it is performed under the ledger lock. Duplicate threshold checks while a worker is in flight are ignored until the worker reports completion.

## Open Questions and Clarifications Needed

No blocking open questions. Threshold defaults should be revisited after live dogfood evidence, especially because snapshot bloat is intentionally deferred to projection retention/pruning work; that dogfood observation is deferred to operators after the implementation workflow and is not a pre-publish blocking acceptance gate for this ExecPlan.
