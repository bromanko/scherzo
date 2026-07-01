# UI server pairing

Use `scherzo connect --pairing-token <pair_...> --server-url <https-url>` to exchange a one-time pairing token for a durable daemon credential. Add `--name <friendly-name>` to send a non-secret UI display label during pairing.

By default, `scherzo connect` does not mutate project YAML. This keeps version-controlled configs and loopback development URLs explicit. To pair and update the selected config (`--config` or the default config path) in one step, add `--activate`:

```sh
scherzo connect --pairing-token <pair_...> --server-url <https-url> --credential-ref work-laptop --name "Project Foo" --activate
```

The credential is stored outside project YAML in the owner-only daemon credential store selected by `ui_server.credential_ref`. `--activate` writes only non-secret UI fields, equivalent to:

```yaml
ui_server:
  enabled: true
  endpoint: https://ui.example.test
  credential_ref: work-laptop
  daemon_label: Project Foo
```

After credential storage, `scherzo connect` tries to notify the local daemon to reload immediately. With `--activate`, a reachable local control file should hot-reload the updated UI client config without a Core restart. Verify the activation path with `scherzoctl query metrics --json`: `ui_server_enabled` should be `true` and `remote_client_status` should move away from `disabled`.

If the local daemon cannot be reached, `scherzo connect --activate` still stores the credential, updates non-secret config when safe, and prints a non-secret fallback instruction. In that case run `scherzoctl reload` or restart the daemon manually.

Notes:
- `--activate` is idempotent when the existing `ui_server` settings already match; if endpoint, credential_ref, or an explicitly supplied `--name` would replace a different existing value, the command rejects the activation instead of overwriting silently.
- `--name` overrides `ui_server.daemon_label` for the pairing exchange; omit it to use the config label.
- Friendly names are trimmed 1-80 character display text. Spaces and punctuation are allowed; newlines and control characters are rejected.
- The friendly name is not a secret and may appear in success output or UI metadata. Pairing tokens, durable credentials, and local control tokens must still be redacted.
- To rename an already paired daemon, update `ui_server.daemon_label` and restart/reload the daemon connection so the next heartbeat/state metadata can update the UI; re-pairing is not required.
- HTTPS is required for non-loopback URLs.
- Loopback HTTP URLs are development-only and should not be copied to another host.
- Local `scherzoctl` remains the fallback when the UI server is unavailable.
- Command/result bridge work is intentionally out of scope here and remains disabled by default.
- Scope inventory: no `.scherzo/workflows/scripts/*`, workflow schema, provider-live, cache, browser UI, server API, or token-accounting changes were required for this pairing slice.

## Transient UI-managed launch grants

UI-managed launch grants are separate from durable `scherzo connect` enrollment. The UI starts a local daemon with:

```sh
scherzo --managed-launch-grant-file <grant.json> --managed-launch-status-file <status.json> [path-to-scherzo.yaml]
```

Both flags are required together and are valid only for daemon mode. The UI must not use them with `--once`, `doctor`, `ctl`, workflow commands, cleanup commands, schedules, artifact commands, workstream commands, state commands, or other direct/offline modes.

Secret-handling rules:
- The grant file contains the short-lived launch credential and is secret.
- Do not place the credential in argv beyond the grant-file path, environment variables, project YAML, logs, status files, durable credential stores, or copied diagnostics.
- The status file is intentionally non-secret. It may contain `launchId`, `phase`, `ok`, `code`, `message`, and `updatedAtMs`, but it must redact the credential from every field.
- Core reads the grant once, keeps the credential only in memory, and deletes the grant file after reading when possible.

Operator recovery and retry guidance:
- If startup fails before hello with `instance_lock_held`, another daemon already owns the workspace lock. Inspect or attach to the existing daemon instead of retrying the same launch blindly.
- Recovery is to revoke the abandoned launch, delete any leftover grant/status files, stop the child process if it is still alive, and retry with a fresh `launchId` and credential.
- Retry is idempotent only with a new launch grant. Reusing a consumed grant for a different daemon boot must fail.
- Cleanup is safe to rerun: status writes are atomic replacements, grant deletion is best-effort, and leftover non-secret status files may be removed after the UI has collected failure evidence.

Deferred manual evidence:
- Browser/UI dogfood for a full UI-managed local launch, including startup-failure display and end-to-end `/api/daemons/ws` behavior, is deferred to the UI/server environment when that repository is available.
- Core-side automated evidence for redacted startup status, including `instance_lock_held`, is still required before publish and is not deferred.
