# UI server pairing

Use `scherzo connect --pairing-token <pair_...> --server-url <https-url>` to exchange a one-time pairing token for a durable daemon credential.

The credential is stored outside project YAML in the owner-only daemon credential store selected by `ui_server.credential_ref`. Keep `ui_server.enabled: false` until pairing succeeds, then configure:

```yaml
ui_server:
  enabled: true
  endpoint: https://ui.example.test
  credential_ref: work-laptop
```

Notes:
- HTTPS is required for non-loopback URLs.
- Loopback HTTP URLs are development-only and should not be copied to another host.
- Local `scherzoctl` remains the fallback when the UI server is unavailable.
- Command/result bridge work is intentionally out of scope here and remains disabled by default.
- Scope inventory: no `.scherzo/workflows/scripts/*`, workflow schema, provider-live, cache, or token-accounting changes were required for this pairing slice.
