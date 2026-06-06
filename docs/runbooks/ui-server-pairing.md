# UI server pairing

Use `scherzo connect --pairing-token <pair_...> --server-url <https-url>` to exchange a one-time pairing token for a durable daemon credential. Add `--name <friendly-name>` to send a non-secret UI display label during pairing.

The credential is stored outside project YAML in the owner-only daemon credential store selected by `ui_server.credential_ref`. Keep `ui_server.enabled: false` until pairing succeeds, then configure:

```yaml
ui_server:
  enabled: true
  endpoint: https://ui.example.test
  credential_ref: work-laptop
  daemon_label: Project Foo
```

Notes:
- `--name` overrides `ui_server.daemon_label` for the pairing exchange; omit it to use the config label.
- Friendly names are trimmed 1-80 character display text. Spaces and punctuation are allowed; newlines and control characters are rejected.
- The friendly name is not a secret and may appear in success output or UI metadata. Pairing tokens and durable credentials must still be redacted.
- To rename an already paired daemon, update `ui_server.daemon_label` and restart/reload the daemon connection so the next heartbeat/state metadata can update the UI; re-pairing is not required.
- HTTPS is required for non-loopback URLs.
- Loopback HTTP URLs are development-only and should not be copied to another host.
- Local `scherzoctl` remains the fallback when the UI server is unavailable.
- Command/result bridge work is intentionally out of scope here and remains disabled by default.
- Scope inventory: no `.scherzo/workflows/scripts/*`, workflow schema, provider-live, cache, or token-accounting changes were required for this pairing slice.
