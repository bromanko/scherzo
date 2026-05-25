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

The runner still uses `profile.name = "task_source"`, `driver.transport = "cli"`, and `driver.timeout_ms` from 1 through 60,000 milliseconds. `profile.capabilities` must include `task_source` and may additionally claim optional-pack capabilities such as `comments.create`, `comments.update`, `comments.allow_create_fallback`, `remote_commands`, `state_transitions.transition`, `state_transitions.reason`, `routing_metadata.workflow_labels`, `routing_metadata.blocker_refs`, `handoff`, and `scheduled_failures`.

`profile.requested_packs` is optional and defaults to `["task_source"]`. It must always include `task_source`. Claimed-but-unrequested optional capabilities do not select extra cases. Requested optional packs are validated against their required claimed capabilities before setup hooks, probes, cleanup hooks, or adapter driver operations run. For example, requesting `comments` without `comments.create` is a manifest validation error. Requesting `remote_commands` also requires `comments.create` because acknowledgement receipts are visible comments, and requesting `remote_commands` or `handoff` requires `profile.retry_behavior` to declare `remote_command_ack` or `handoff_report` as either `idempotent_update_or_dedupe` or `duplicate_visible`. Because `handoff` retry classification is probe-backed rather than receipt-backed, manifests that request `handoff` must also configure at least one backend-visibility probe. `scheduled_failures` is also probe-backed: manifests that request `scheduled_failures` must claim `scheduled_failures`, include `scheduled_failures.publish` in `profile.adapter_operations`, and configure at least one backend-visibility probe named with the `scheduled-failures` prefix.

`profile.adapter_operations` must declare all three required task-source operations. It may also list future optional-pack adapter operations such as `comments.post_or_update` and `state_transitions.transition`. Adapter operation names must stay inside the public adapter namespace. Fixture, probe, and hook namespaces are reserved for non-conformance support paths and are rejected inside `profile.adapter_operations`.

`fixtures.task_file` points at a repository-relative JSON fixture that decodes as a successful non-empty task-list driver response. The MVP rejects absolute paths, parent-directory traversal, and Windows-style drive or backslash escape forms, then confirms at runtime that the resolved fixture stays inside the repository root. The MVP uses that fixture both as the expected task inventory and as the stable-identity source for refresh and lookup cases.

`fixtures.tasks` is an optional array of explicit pre-provisioned fixture declarations. Each declaration has `name`, `ref`, `operator_refs`, and `purpose`. Names must be non-empty and unique, every `operator_ref` must stay non-blank after trimming, each declared `ref.backend_kind` must match the manifest `adapter_kind`, and every declared ref must already exist in `fixtures.task_file`. When declarations are present, refresh and known-lookup cases use those explicit refs and operator refs instead of inferring them from the full task file.

## Driver request envelope

Every adapter operation uses one JSON request envelope:

- `schema_version`
- `request_id`
- `operation`
- `payload`

The supported operations are:

- `task_source.fetch_candidates`
- `task_source.refresh_by_refs`
- `task_source.lookup_by_operator_ref`
- `comments.post_or_update`
- `remote_commands.fetch_events`
- `remote_commands.post_ack`
- `state_transitions.transition`
- `handoff.report`
- `scheduled_failures.publish`

## Driver response envelope

Every driver response echoes `schema_version` and `request_id` and uses one of two shapes:

- success: `{ "ok": true, "result": ... }`
- failure: `{ "ok": false, "error": ... }`

Failure responses carry a normalized error object with `kind`, `message`, optional `ref`, and optional `capability`.

Driver transport failures are not reported as tracker errors. Scherzo classifies spawn failure, timeout, missing stdout, non-zero exit, malformed JSON, and response-envelope schema/request-id mismatches as driver-level conformance failures and reports them against the public case id that triggered the request. Captured external-process diagnostics are truncated to 4,096 characters before reporting.

Every case report now also carries bounded request and response transcript evidence. A transcript record contains `body`, `truncated`, and `original_chars`. The `body` is the bounded retained text after truncation, `truncated` tells reviewers whether the original text exceeded the safety limit, and `original_chars` records the pre-truncation length. Request transcripts retain the exact JSON stdin Scherzo wrote to the driver. Response transcripts retain the raw stdout line when one existed, even when the line later failed JSON decoding or envelope validation.

## Optional pack cases

When `profile.requested_packs` selects optional packs and the manifest claims the matching capabilities, Scherzo appends these extra public cases after the required `task_source` cases.

`comments` adds `comments.post_or_update.create_only`, `comments.post_or_update.update_existing`, `comments.post_or_update.update_missing_no_fallback`, and `comments.post_or_update.update_missing_allow_create_fallback`. Successful comment receipts must carry a non-empty `id`, the same fixture task identity, and the expected `created` flag. The stale-update-without-fallback case passes only on a normalized `not_found` error.

`remote_commands` adds `remote_commands.fetch.normalized_events`, `remote_commands.fetch.since_event_ids`, `remote_commands.fetch.limit_per_task`, `remote_commands.post_ack.receipt`, `remote_commands.post_ack.same_event_retry`, and `remote_commands.post_ack.failure_visibility`. Fetch cases prove normalized event ids, fixture-task coverage, bounded fields, `since_event_ids`, and `limit_per_task`. Bounded event fields currently cap `event_id`, `author_id`, and `command_name` at 128 characters and cap `body` and `excerpt` at 128 characters; oversized fields fail conformance instead of being retained in reports. Acknowledgement cases prove normalized receipts and retain the declared retry classification in case summaries while probes verify backend-visible duplicate handling.

`state_transitions` adds `state_transitions.transition.target_id_precedence`, `state_transitions.transition.target_name_only`, `state_transitions.transition.unknown_target`, and `state_transitions.transition.reason_propagation`. Successful transition receipts must carry the same fixture task identity and the expected normalized target state. The unknown-target case passes only on a normalized `permanent` error.

`routing_metadata` adds `routing_metadata.fetch.workflow_labels` and `routing_metadata.refresh.blocker_refs`. These cases reuse public task-source reads to prove workflow labels and blocker refs from adapter-returned normalized tasks. Probe commands remain support evidence only and are still rejected inside `profile.adapter_operations`.

`handoff` adds generic `handoff.report.claim`, `handoff.report.success`, `handoff.report.failure`, `handoff.report.park`, legacy `handoff.report.legacy_*` cases, and per-event retry cases under `handoff.report.retry.*`. Successful handoff receipts must return `reported=true`; retry summaries retain the declared classification while probes verify backend-visible duplicate handling for each generic and legacy event class, so manifests without backend-visibility probes are rejected before execution.

`scheduled_failures` adds `scheduled_failures.publish.create`, `scheduled_failures.publish.remembered_retry`, and `scheduled_failures.publish.dedupe_recovery`. The request payload matches `ScheduledFailurePublication` from `docs/specs/TRACKER_ADAPTER_SPEC.md`: `job_id`, `workflow_id`, `due_at_ms`, `run_id`, `attempt`, `max_attempts`, `reason`, `run_root`, `session_id`, `dedupe_key`, `title`, `body`, `labels`, `target_state_name`, and `previous_task_remote_id`. Successful receipts return `scheduled_failure: { task, created, comment_id }`. The create case expects `created=true` for the first visible failure task. The remembered-retry case reuses the same `dedupe_key` and a usable `previous_task_remote_id` and expects `created=false` for the same visible task. The dedupe-recovery case reuses the same `dedupe_key` without a usable remembered task id and still expects `created=false` for that same visible task. Backend probes remain support evidence only, but scheduled-failure reports now retain `scheduled_failures.created_remote_ids`, `scheduled_failures.retry_classifications`, `scheduled_failures.duplicate_count`, `scheduled_failures.visible_task_count`, `scheduled_failures.cleanup_status`, and `scheduled_failures.probe_status` so duplicate suppression, metadata retention, and cleanup recovery stay diagnosable.

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

Adapter authors should request optional packs explicitly, keep fake-driver manifests deterministic, reserve probes for support evidence rather than public adapter operations, and make cleanup hooks idempotent so reruns do not leave duplicate marker data behind.

## Local runner command

Run the MVP locally from the repository root with:

    direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/task-source-pass.manifest.json --report test/tmp/tracker-conformance/task-source-pass.report.json

A passing run exits `0`, writes the requested report, and prints a summary naming the adapter kind, selected profile, total case count, and aggregate failure counters. When any adapter, setup, probe, or cleanup failure occurs, the CLI summary appends failure-specific recovery guidance without printing raw transcripts to stdout.
