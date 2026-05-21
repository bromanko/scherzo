# Tracker conformance protocol

Status: Draft v1 MVP

This document defines the black-box tracker adapter conformance MVP implemented for LIV-406.

LIV-410 baseline note: the current tree already supports the CLI `task_source` transport, `fixtures.task_file`, optional setup and cleanup hooks, optional probes, rejection of fixture/probe/hook names inside `profile.adapter_operations`, separate `setup_failed`/`probe_failed`/`cleanup_failed` counters, configured report redaction, and truncation of captured external diagnostics. The suite treats the adapter under test as an external driver process. Scherzo invokes a CLI command, writes one JSON request to stdin, reads one JSON response line from stdout, and records private diagnostics from stderr without mixing them into the JSON transport.

## Manifest

A conformance manifest is a JSON document with `schema_version: 1` and these top-level fields:

- `adapter_kind`: stable backend kind under test.
- `driver`: CLI transport declaration.
- `profile`: currently only `task_source`.
- `fixtures`: repository-relative fixture metadata.
- `probes`: optional backend visibility probes.
- `hooks`: optional setup and cleanup commands.
- `report`: report redaction configuration.

The MVP accepts only `driver.transport = "cli"`, `driver.timeout_ms` from 1 through 60,000 milliseconds, `profile.name = "task_source"`, and `profile.capabilities = ["task_source"]`. `profile.adapter_operations` must declare all three required task-source operations. Adapter operation names must stay inside the public adapter namespace. Fixture, probe, and hook namespaces are reserved for non-conformance support paths and are rejected inside `profile.adapter_operations`.

`fixtures.task_file` points at a repository-relative JSON fixture that decodes as a successful non-empty task-list driver response. The MVP rejects absolute paths, parent-directory traversal, and Windows-style drive or backslash escape forms, then confirms at runtime that the resolved fixture stays inside the repository root. The MVP uses that fixture both as the expected task inventory and as the stable-identity source for refresh and lookup cases.

`fixtures.tasks` is an optional array of explicit pre-provisioned fixture declarations. Each declaration has `name`, `ref`, `operator_refs`, and `purpose`. Names must be non-empty and unique, every `operator_ref` must stay non-blank after trimming, each declared `ref.backend_kind` must match the manifest `adapter_kind`, and every declared ref must already exist in `fixtures.task_file`. When declarations are present, refresh and known-lookup cases use those explicit refs and operator refs instead of inferring them from the full task file.

## Driver request envelope

Every adapter operation uses one JSON request envelope:

- `schema_version`
- `request_id`
- `operation`
- `payload`

The supported MVP operations are:

- `task_source.fetch_candidates`
- `task_source.refresh_by_refs`
- `task_source.lookup_by_operator_ref`

## Driver response envelope

Every driver response echoes `schema_version` and `request_id` and uses one of two shapes:

- success: `{ "ok": true, "result": ... }`
- failure: `{ "ok": false, "error": ... }`

Failure responses carry a normalized error object with `kind`, `message`, optional `ref`, and optional `capability`.

Driver transport failures are not reported as tracker errors. Scherzo classifies spawn failure, timeout, missing stdout, non-zero exit, malformed JSON, and response-envelope schema/request-id mismatches as driver-level conformance failures and reports them against the public case id that triggered the request. Captured external-process diagnostics are truncated to 4,096 characters before reporting.

Every case report now also carries bounded request and response transcript evidence. A transcript record contains `body`, `truncated`, and `original_chars`. The `body` is the bounded retained text after truncation, `truncated` tells reviewers whether the original text exceeded the safety limit, and `original_chars` records the pre-truncation length. Request transcripts retain the exact JSON stdin Scherzo wrote to the driver. Response transcripts retain the raw stdout line when one existed, even when the line later failed JSON decoding or envelope validation.

## `task_source` MVP cases

The MVP runner executes five public cases:

- `task_source.fetch.backend_kind`
- `task_source.refresh.stable_identity`
- `task_source.refresh.wrong_backend_ref`
- `task_source.lookup.empty_operator_ref`
- `task_source.lookup.known_operator_ref`

These prove that candidate reads stay on the declared backend kind, refresh uses stable `(backend_kind, remote_id)` identity, wrong-backend refs fail or are omitted, empty operator refs return no match, and known operator refs resolve to the same durable task identity.

## Fixtures, hooks, and probes

Setup and cleanup hooks are optional CLI commands used for privileged fixture preparation and teardown. Probes are optional CLI commands used for backend visibility checks. None of these paths count as adapter-under-test operations. A manifest executes local commands with the operator's privileges, so operators should run only trusted manifests or place untrusted adapters in a sandboxed environment.

Reports distinguish these failure classes explicitly:

- `failed`: adapter-under-test case failures.
- `setup_failed`: setup hook failures.
- `probe_failed`: probe failures.
- `cleanup_failed`: cleanup hook failures.
- `passed` and `skipped`: case-level outcomes.

JSON reports keep those top-level counters and also add a grouped `counts` object with the same values. Each case result retains `id`, `operation`, `status`, `request_id`, `message`, and `diagnostics`, and adds `expected_summary`, `actual_summary`, `request_transcript`, `response_transcript`, and `recovery_guidance`. Hook and probe results add `recovery_guidance` so operators can tell whether to fix setup, visibility checks, cleanup, or the adapter implementation itself.

Configured `report.redact` strings are replaced with `[REDACTED]` in JSON reports and CLI summaries. Redaction applies to messages, diagnostics, summaries, hook and probe evidence, and both request and response transcripts before Scherzo writes report files or prints recovery guidance.

## Local runner command

Run the MVP locally from the repository root with:

    direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/task-source-pass.manifest.json --report test/tmp/tracker-conformance/task-source-pass.report.json

A passing run exits `0`, writes the requested report, and prints a summary naming the adapter kind, selected profile, total case count, and aggregate failure counters. When any adapter, setup, probe, or cleanup failure occurs, the CLI summary appends failure-specific recovery guidance without printing raw transcripts to stdout.
