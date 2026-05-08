# Typed-error strategy for durable and domain APIs

Scherzo keeps `stringly_typed_error` as warning inventory until durable and domain APIs have explicit error boundaries. The policy for new or touched durable/domain code is:

1. Public durable/domain functions should return a module-owned error type instead of `Result(_, String)`.
2. Private FFI declarations may still return raw `String` details, but the nearest Gleam wrapper should translate those strings into a typed FFI or subsystem error before the value crosses a public API boundary.
3. Durable records, logs, CLI output, and operator messages should not pattern match on arbitrary strings. Convert typed errors at those edges with explicit renderers:
   - stable machine/durable codes with `*_code` functions, and
   - human-readable descriptions with `describe_*` or `*_to_string` functions.
4. Keep conversions one-way at the edge. Do not parse durable error-code strings back into domain control flow unless a migration explicitly requires compatibility with older records.
5. Prefer cohesive slices over mechanical churn. A migration should improve caller handling, observability, or durable semantics, not merely silence the lint.

## Candidate inventory

| Area | Current shape | Typed-error boundary to prefer | Notes |
| --- | --- | --- | --- |
| `scherzo/state/outbox` replay validation | Migrated from string codes to `outbox.ReplayError`. | `outbox.replay_error_code` for `record.OutboxFailed.error_code`; `outbox.describe_replay_error` for operator logs. | Initial LIV-130 slice. Preserves existing durable codes: `outbox_payload_missing`, `invalid_outbox_payload`, `unsupported_outbox_kind:*`, and `unsupported_outbox_payload_kind:*`. |
| `scherzo/state/projection.decode_string` and snapshot helpers | `Result(_, String)` decode failures. | A projection decode error that distinguishes malformed JSON, unsupported schema version, and invalid snapshot shape. | Ledger code already maps unsupported versions specially; make that structured before broad migration. |
| `scherzo/state/artifact_store` stored-artifact decoding | Private decode helpers return strings that are later wrapped in `ArtifactError`. | Keep `ArtifactError` as the public boundary; make decode failures typed before wrapping. | Good follow-up because artifact corruption is already domain-specific. |
| `scherzo/state/local_artifacts` schema parsing | Private JSON schema helpers return strings. | Local artifact decode/schema error. | Preserve artifact paths and schema versions in the typed error. |
| `scherzo/step_artifact` | Step artifact JSON/status parsing returns strings. | Step artifact decode/status error with a renderer for persisted/user messages. | A small domain parser slice, but it affects many recovery callers. |
| `scherzo/workflow_scheduler` | Scheduler operations return string reasons. | Scheduler error covering invalid state, duplicate jobs, and persistence/ledger causes. | Migrate with scheduler tests because callers may branch on retryability later. |
| `scherzo/workflow_run` step-batch helpers | Internal workflow execution helpers return strings. | Workflow-run domain error or narrower batch error. | Avoid a broad refactor until the caller boundary is selected. |
| `scherzo/linear_body_data` | Linear document parsing/appending returns strings. | Linear body/document error with renderers at tracker/user edges. | Not durable state, but domain-specific enough to avoid losing document-shape context. |
| FFI/control/process modules (`port`, `signal`, `control/*`, `instance_lock`) | Raw `Result(_, String)` from external functions and socket/process boundaries. | FFI-specific error wrappers at the first Gleam boundary; public APIs expose subsystem errors. | Do not start with private `@external` declarations alone; wrap them as part of a public boundary migration. |

## Initial migrated slice: durable outbox replay

The outbox replay path is the first typed-error slice because it is small, durable, and operator-visible:

- Replay validation now returns `outbox.ReplayError` instead of string codes.
- Recovery planning branches on typed errors and renders them to stable codes only when appending `OutboxFailed` durable records or warning strings.
- Startup recovery logs keep the stable `error` code and add a human-readable `reason` rendered from the same typed error.
- Existing durable codes are intentionally unchanged so older dashboards, tests, and retained ledgers remain understandable.

Follow-up migrations should follow the same pattern: introduce the narrow module-owned error, update callers to branch on that type, then render stable codes or descriptions only at durable/operator/user edges.
