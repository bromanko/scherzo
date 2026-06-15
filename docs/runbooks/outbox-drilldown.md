# Outbox drill-down

Use the outbox drill-down when `scherzoctl query metrics` reports non-zero `pending_outbox_count`, `in_flight_outbox_count`, `retryable_outbox_count`, or `permanent_outbox_count`.

## List records

```sh
scripts/scherzoctl outbox
scripts/scherzoctl outbox --status retryable --json
scripts/scherzoctl outbox --status permanent --limit 25 --json
```

The list view shows operator-safe metadata only: outbox id, kind, status, task ref, dedupe key where retained, attempt count, next retry time, last error code, and timestamps where available. Use `--kind <kind>` and repeat `--status` or `--kind` to narrow the view. Use `--cursor` with the returned `next_cursor` for additional pages.

## Inspect one record

```sh
scripts/scherzoctl outbox <outbox-id>
scripts/scherzoctl outbox <outbox-id> --json
```

For retryable records, check `last_error_code`, `attempt_count`, and `next_attempt_at_ms` to decide whether the item is waiting for its next automatic retry or needs operator investigation. For permanent records, use the task ref and dedupe key to correlate with the relevant tracker task and retained workflow logs.

Default text and JSON output intentionally do not include raw payload bodies, prompt/result content, or secrets. Records that still have a retained payload only expose `has_payload: true` or `payload: redacted`.
