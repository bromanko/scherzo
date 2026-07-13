# State ledger compaction and projection retention

Scherzo stores daemon state under `.scherzo-state/ledger`. `current.jsonl` is the append-only current segment, `snapshot.json` is the online projection, and `archive/segment-*.jsonl` is the durable audit tier. Compaction always preserves the raw numbered segments. Projection retention can remove old terminal workflow-run details from the snapshot without deleting those segments.

## Defaults and configuration

Auto-compaction remains enabled by default with 10,000 current records, 8 MiB, and a five-minute minimum interval. Projection retention is independent and disabled by default. An upgrade therefore does not prune existing state.

```yaml
state_ledger:
  auto_compaction:
    enabled: true
    max_current_records: 10000
    max_current_bytes: 8388608
    min_interval: 5m
  projection_retention:
    enabled: false
    terminal_grace: 24h
    scheduled_max_age: 7d
    scheduled_last_per_job: 25
```

When enabled, issue-driven terminal runs become eligible after the grace period. Scheduled runs in grace are retained; outside grace, a run is retained only when it is both no older than `scheduled_max_age` and among the newest `scheduled_last_per_job` safe runs for that job. Active, parked, recovering, retained-workspace, in-flight-control, unsettled-publication, and unsettled-outbox runs are held regardless of the ordinary limits.

## Preview and enablement

Stop the daemon and back up `.scherzo-state/ledger` before first enablement. Configure the exact intended values, then run:

```sh
direnv exec . gleam run -- state compact --root <workspace> --dry-run
```

The preview loads `<workspace>/scherzo.yaml`, uses the same resolved policy as confirmed compaction, and does not mutate ledger files. It reports the exact enabled flag, grace, scheduled age/count limits, policy fingerprint, coverage status, candidate and blocker counts, every run-owned family count, before/projected bytes, and the retain-all reconstruction estimate. The estimate uses `estimated_output_bytes = max(current_snapshot_bytes, raw_input_bytes)`, requires memory of `4 * raw_input_bytes + estimated_output_bytes`, and requires disk of `raw_input_bytes + 2 * estimated_output_bytes + 1 GiB`. A first retention-enabled preview also proves that numbered archives plus `current.jsonl` reconstruct the current unpruned projection. Do not enable retention if coverage cannot be established, candidates are surprising, or recovery resources and backup are not ready.

After approving the preview, compact while the daemon remains stopped:

```sh
direnv exec . gleam run -- state compact --root <workspace> --yes
```

Confirmed compaction reports the same resolved policy values and stable fingerprint as preview, together with candidate, pruned, and coverage counts; compare them with the approved preview before continuing. Repeating preview is safe. Repeating compaction at the same clock and policy is idempotent: already-pruned runs have no online families to remove, and exact marker writes merge sorted unique ids.

## Archive coverage and pruned-run index

Retention creates `archive/coverage.json`, which records each numeric segment name, size, and SHA-256 hash. Later retention-enabled compactions verify it before pruning and extend it after archiving a new current segment. Missing, reordered, renamed, or hash-mismatched coverage fails closed.

Each pruned id is recorded under `archive/pruned-runs/v1/<hash[0:2]>/<hash[2:4]>/<sha256>`. Marker contents store sorted exact original ids, so hash collisions are compared rather than assumed. Missing-run classification computes one marker path and never scans archived JSONL. A malformed or unreadable marker is an error; it is not treated as unknown. Online projection presence wins over a stale marker left by a failed transaction.

Late run-scoped appends to a marked run are rejected as `pruned_workflow_run`. A never-known id retains its existing unknown/orphan error. Raw history remains archived.

## Logs

Every successful daemon compaction emits `ledger_compacted` with current-record, byte, archive-segment, and duration fields. A compaction that removes runs additionally emits `projection_pruned` with all effective policy values and the fingerprint, removed run count, every family and blocker count, before/after projection bytes, and coverage status. `ledger_compaction_failed` includes the ledger error and refreshed current-segment statistics when available.

## Failure handling and rollback

1. Stop the daemon and verify the instance lock is no longer live before offline maintenance.
2. Preserve the copied pre-enable ledger as the primary rollback artifact.
3. On coverage, marker, snapshot, archive, or manifest failure, do not force pruning or edit derived files by hand. Inspect `ledger_compaction_failed` and rerun dry-run only after correcting the underlying filesystem problem.
4. To disable additional pruning, set `projection_retention.enabled: false`. This does not recreate already-pruned online history.
5. To roll back immediately, restore the copied ledger while the daemon is stopped. Keep the failed copy for diagnosis.

For retain-all reconstruction, keep retention disabled, stop the daemon, and run:

```sh
direnv exec . gleam run -- state compact --root <workspace> --rebuild-from-archives --yes
```

The command acquires the instance lock, verifies the stored coverage manifest against numerically ordered segment names, sizes, and hashes, and runs the exact memory and disk formulas printed by preview. Unavailable probes and insufficient-memory or insufficient-disk failures include `raw_input_bytes`, `estimated_output_bytes`, `required_memory_bytes`, and `required_disk_bytes` in their output so the rejection can be reviewed without mutation. Enabled retention, incomplete coverage, or a held instance lock also fail before mutation. After preflight, Scherzo folds verified archives and `current.jsonl` from an empty projection, atomically writes a retain-all snapshot, rotates a nonempty current segment once, and updates coverage while preserving archive and pruned-index files. Online restored state takes precedence over stale pruned markers. The copied ledger remains the primary rollback; do not attempt a partial manual merge into `snapshot.json`.

## Deferred operator evidence

Publication of the implementation does not enable retention on a real workspace. The operator owns backup, intended-policy preview, approval, and enablement. After enablement, observe later compactions for `projection_pruned`, snapshot plateau, archive-index latency, and first-enable coverage cost. Those real-workspace observations are post-implementation evidence, not automated publish gates.
