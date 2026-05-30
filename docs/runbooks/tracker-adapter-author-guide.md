# Tracker adapter author guide

This guide is for an external adapter author who wants one reliable path from a local driver to a conformance report. The normative protocol remains [docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md](../specs/TRACKER_CONFORMANCE_PROTOCOL.md). The operator-focused context and capability matrix remain in [docs/runbooks/tracker-adapters.md](tracker-adapters.md).

## What is stable today

The stable local author path today is the CLI runner:

```sh
direnv exec . gleam run -- tracker-conformance run <manifest.json> --report <report.json>
```

The stable transport today is a **CLI driver**. Scherzo writes one JSON request to the driver's stdin, reads one JSON response line from stdout, and captures stderr as private diagnostics.

The checked-in author example lives under `examples/tracker-conformance/adapter-author/`:

- `driver.sh` is the portable shell driver wrapper.
- `manifest.pass.json` is the minimal passing task-source example.
- `manifest.invalid-shape.json` fails manifest decoding.
- `manifest.missing-capability.json` fails requested-pack validation.
- `manifest.namespace-misuse.json` fails reserved-namespace validation.
- `manifest.malformed-response.json` shows malformed stdout diagnostics.
- `manifest.stale-response.json` shows stale `request_id` envelope diagnostics.
- `manifest.redaction.json` shows report redaction.
- `request.fetch_candidates.json`, `response.fetch_candidates.success.json`, and `response.fetch_candidates.stale.json` are documented envelope snippets.

The repository also has an HTTP manifest shape in schemas and decoders, but the runner does not execute HTTP transports yet. Do not advertise HTTP as a supported adapter transport until the runner implements it.

## Step 1: start from the passing example

Make the driver executable, then run the passing example from the repository root:

```sh
chmod +x examples/tracker-conformance/adapter-author/driver.sh
direnv exec . gleam run -- tracker-conformance run \
  examples/tracker-conformance/adapter-author/manifest.pass.json \
  --report test/tmp/tracker-conformance/adapter-author-pass.report.json
```

A passing run exits `0`, prints a `tracker-conformance` summary, and writes a report whose top-level counters keep `failed`, `setup_failed`, `probe_failed`, and `cleanup_failed` at zero.

## Step 2: shape the manifest

A manifest must keep `schema_version: 1`, declare the backend `adapter_kind`, configure a `driver`, choose `profile.name: "task_source"`, and point `fixtures.task_file` at a repository-relative fixture file.

Use `profile.capabilities` to declare what the adapter can do. Use `profile.requested_packs` to choose which conformance packs actually run. Requested packs are validated before setup hooks, probes, cleanup hooks, or adapter operations run.

The passing example keeps the smallest stable author surface:

- transport: CLI
- profile: `task_source`
- requested packs: `task_source`
- required adapter operations: `task_source.fetch_candidates`, `task_source.refresh_by_refs`, `task_source.lookup_by_operator_ref`

Optional packs currently covered by checked-in schemas, fixtures, and tests are `comments`, `remote_commands`, `state_transitions`, `routing_metadata`, `handoff`, and `scheduled_failures`. `remote_commands` remains a historical compatibility pack, not a production operator-control path.

## Driver environment and PATH

CLI driver commands run with the manifest `driver.command.env` entries plus the runner's current `PATH` when the manifest does not set `PATH` explicitly. This keeps local and CI runs on the same Nix/devenv toolchain and prevents portable shell wrappers from losing access to tools such as `dirname`, `pwd`, `sh`, or adapter-specific CLIs.

If you need a hermetic adapter environment, set `PATH` explicitly in the manifest and include every directory the driver and its helper scripts need. Avoid assuming `/bin` or `/usr/bin` exist in CI; NixOS runners may not have those paths.

## Step 3: implement the driver envelope

The driver must read one request from stdin and print exactly one response line to stdout.

The checked-in envelope snippet files are valid JSON and decode through the same repository codecs as the runner:

- Request: `examples/tracker-conformance/adapter-author/request.fetch_candidates.json`
- Success response: `examples/tracker-conformance/adapter-author/response.fetch_candidates.success.json`
- Stale-envelope response: `examples/tracker-conformance/adapter-author/response.fetch_candidates.stale.json`

Use the snippet files directly when you want copyable request and response examples.

## Fixtures, probes, and hooks

`fixtures.task_file` is the adapter-under-test evidence source. Keep it repository-relative and inside `test/fixtures/tracker_conformance/`.

`fixtures.tasks` is optional. Use it when you want explicit durable task identities and operator refs instead of inferring from the full task file.

`hooks.setup` and `hooks.cleanup` are privileged support commands. `probes` are backend-visibility support commands. None of them count as adapter-under-test operations. Keep `fixture.*`, `probe.*`, and `hook.*` names out of `profile.adapter_operations`; the runner rejects those namespaces.

## Reports, diagnostics, and redaction

Use `report.redact` for secrets that must not appear in summaries or JSON reports. The `manifest.redaction.json` example intentionally sends `SECRET_TOKEN` through fixture declarations, hooks, probes, stderr diagnostics, and driver payloads; the runner must replace those values with `[REDACTED]` in retained output.

Use these failure examples when diagnosing your own adapter:

- `manifest.invalid-shape.json` should fail with `invalid_manifest_json`.
- `manifest.missing-capability.json` should fail with `missing_requested_pack_capability` because it requests `comments` without claiming `comments.create`.
- `manifest.namespace-misuse.json` should fail with `fixture_operation_disallowed` because `fixture.setup` is not an adapter operation.
- `manifest.malformed-response.json` should exit `1` and record `driver stdout was not valid conformance JSON`.
- `manifest.stale-response.json` should exit `1` and record `driver response envelope did not match request schema_version or request_id`.

## Packaging and CI

When you ship an adapter package:

1. Ship a driver executable or wrapper that can run non-interactively from the repository root or a known working directory.
2. Pin the manifest to the current schema version and keep fixture paths repository-relative.
3. Bundle any fake fixtures needed for local dogfood.
4. Run the conformance command in CI and retain only sanitized JSON reports.
5. Treat fake-driver dogfood as the required pre-publish proof. Treat live-backend runs as deferred operator checks with unique markers, idempotent cleanup, and redacted excerpts only.

A minimal CI command is:

```sh
chmod +x examples/tracker-conformance/adapter-author/driver.sh
direnv exec . gleam run -- tracker-conformance run \
  examples/tracker-conformance/adapter-author/manifest.pass.json \
  --report test/tmp/tracker-conformance/adapter-author-pass.report.json
```

If you change checked-in docs examples in this repository, also run:

```sh
direnv exec . gleam test
direnv exec . gleam format --check src test
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint
```
