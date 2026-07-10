# State ledger compaction

Scherzo stores daemon state under `.scherzo-state/ledger`. `current.jsonl` is the append-only current segment, `snapshot.json` is the folded projection snapshot, and `archive/segment-*.jsonl` holds older compacted current segments.

## Auto-compaction defaults

Daemon auto-compaction is enabled by default with these thresholds:

- `max_current_records: 10000`
- `max_current_bytes: 8388608`
- `min_interval: 5m`

Configure it in project YAML under `state_ledger.auto_compaction`:

```yaml
state_ledger:
  auto_compaction:
    enabled: true
    max_current_records: 10000
    max_current_bytes: 8388608
    min_interval: 5m
```

Disable it with:

```yaml
state_ledger:
  auto_compaction:
    enabled: false
```

## Logs

Successful daemon compaction logs `ledger_compacted` with:

- `before_current_records`
- `before_current_bytes`
- `after_current_records`
- `after_current_bytes`
- `after_archive_segment_count`
- `duration_ms`

Failed daemon compaction logs `ledger_compaction_failed` with the ledger error and, when refresh succeeds, `latest_current_records` and `latest_current_bytes`.

## Offline maintenance

`state compact` remains an offline maintenance command. It now acquires `.scherzo-state/instance.lock` first and rejects when a live daemon still owns the workspace.

Use dry-run inspection first:

```sh
direnv exec . gleam run -- state compact --root <workspace> --dry-run
```

Then compact only after the daemon is stopped:

```sh
direnv exec . gleam run -- state compact --root <workspace> --yes
```

## Recovery from failed compaction

1. Inspect daemon logs for `ledger_compaction_failed`.
2. Verify whether the daemon is still running before touching `.scherzo-state/instance.lock`.
3. If the daemon is stopped and the lock is stale, remove the stale `instance.lock` only after that verification.
4. Run `state compact --dry-run` to inspect current ledger size.
5. Run `state compact --yes` while the daemon is stopped if manual compaction is still needed.

Snapshot or projection retention/pruning is out of scope for LIV-1412. This work only bounds `current.jsonl`; it does not prune `snapshot.json` contents.