# State ledger maintenance

Offline state maintenance changes files under `.scherzo-state/ledger/`. Stop the Scherzo daemon before running mutating maintenance commands or editing ledger files by hand.

The daemon-stopped requirement applies to:

- `state compact --yes`
- `state repair-run-provenance --yes`
- manual edits to `current.jsonl`, `snapshot.json`, or archived `segment-*.jsonl` files
- restore operations that replace those files from `.bak` or other backups

Dry-run inspection remains safe while the daemon is running because it does not mutate ledger files.

If a workspace still has `.scherzo-state/control.json`, treat that as a live-daemon marker. Only remove a stale control file after you have confirmed the daemon is stopped.
