# Tracker Adapter Specification

Status: Draft v1

Purpose: Define the normative contract between Scherzo and tracker adapters, the backend boundary that reads tasks from an external task system and performs optional task-system side effects.

## Normative language

The key words `MUST`, `MUST NOT`, `REQUIRED`, `SHOULD`, `SHOULD NOT`, `RECOMMENDED`, `MAY`, and `OPTIONAL` in this document are to be interpreted as described in RFC 2119.

`Implementation-defined` means the behavior is part of an adapter's contract, but this specification does not prescribe one universal policy. Adapters MUST document their selected behavior when they rely on implementation-defined semantics.

## 1. Purpose and scope

A Scherzo tracker adapter is the runtime boundary between the daemon and one external task system. The adapter normalizes backend tasks into the `scherzo/task.gleam` data model and exposes the capability records defined in `scherzo/tracker/adapter.gleam`.

This specification covers:

- the tracker adapter vocabulary and identity model,
- the backend-neutral task data model,
- required and optional adapter capabilities,
- per-capability operation inputs, receipts, error behavior, idempotency expectations, and startup requirements,
- `TrackerError` semantics,
- startup capability validation and operator diagnostics,
- durable recovery and legacy Linear issue compatibility,
- security and secret-handling expectations,
- extension/versioning rules, and
- conformance profiles for adapter implementers.

This specification documents the contract that exists today. Linear is the only production adapter in this repository. Jira, Trello, and other production adapters are future work; this document does not claim production support for them.

## 2. Glossary

**Task**: a backend-neutral unit of work selected or updated by Scherzo. A Linear issue is the production task type today.

**Task system**: the external system that owns tasks, such as Linear today or a future tracker backend.

**Tracker adapter**: the Scherzo component that talks to a task system. A tracker adapter reads tasks through `task_source` and MAY perform optional side effects through capabilities such as `comments`, `state_transitions`, or `scheduled_failures`.

**Task provider**: an informal synonym sometimes used in tickets or stakeholder discussion for a tracker adapter. It is not a second runtime abstraction.

**Backend kind**: the stable adapter/backend identifier used in `TrackerAdapter.kind` and `TaskRef.backend_kind`, such as `linear` or the test-only `test-memory` kind. Backend kind values MUST be stable across daemon restarts and durable recovery.

**Remote id**: the backend-owned opaque identifier for a task, stored as `TaskRef.remote_id`. Scherzo treats it as the durable identity inside a backend kind.

**Task reference**: a `TaskRef` containing `backend_kind`, backend `remote_id`, optional human key, and optional URL. The durable identity of a task reference is `(backend_kind, remote_id)`.

**Capability**: a named adapter field that exposes a family of operations. `task_source` is REQUIRED. All other capabilities are optional `Option(...)` fields and MUST be `None` when unsupported.

**legacy issue compatibility**: the preserved compatibility surface for existing Linear-shaped runtime state, prompt variables, environment variables, command records, and ledger fields. Compatibility includes `issue.*`, `SCHERZO_ISSUE_*`, `issue_id`, `issue_identifier`, `linear_command_*`, Linear CLI aliases, and Linear-specific helper scripts. Compatibility names MUST NOT be removed merely because the backend-neutral task model exists.

## 3. Adapter identity and shape

A `TrackerAdapter` has this public shape:

| Field | Required | Contract |
| --- | --- | --- |
| `kind` | yes | Stable backend kind. For task references created by the adapter, `TaskRef.backend_kind` MUST equal this value. |
| `display_name` | yes | Human-readable backend name for diagnostics. It MUST NOT contain secrets. |
| `task_source` | yes | Required task-read capability. Every adapter MUST provide it. |
| `comments` | no | Optional comment creation/update capability. |
| `remote_commands` | no | Historical/conformance-only remote command capability; production runtime does not consume it. |
| `state_transitions` | no | Optional task state transition capability. |
| `routing_metadata` | no | Optional workflow-label and blocker metadata extraction capability. |
| `links` | no | Optional external-link upsert capability. |
| `handoff` | no | Optional handoff reporting capability. |
| `scheduled_failures` | no | Optional scheduled-job failure publication capability. |
| `readiness` | no | Optional tracker contract/readiness check capability. |
| `smoke` | no | Optional tracker smoke check capability. |
| `attachments` | no | Optional generic task attachment upload capability. |

Unsupported optional capabilities MUST be represented as `None`, not as a function that always fails startup. A capability that is generally present MAY still return `UnsupportedCapability("suboperation")` for a narrower operation that the capability type can express but the backend cannot perform, such as `comments.update`.

## 4. Task data model

### 4.1 `TaskRef`

`TaskRef(backend_kind, remote_id, key, url)` identifies one backend task.

- `backend_kind` MUST name the adapter/backend kind that owns `remote_id`.
- `remote_id` MUST be the stable backend identifier used for durable recovery and follow-up operations. It SHOULD be non-empty after backend normalization.
- `key` is an optional human-facing key, such as a Linear issue identifier. It MAY be absent and MAY change independently from `remote_id`.
- `url` is an optional human-facing task URL.
- Display code uses `display_key`: a non-empty trimmed `key` is preferred; otherwise the display value is `remote_id`.
- Equality for durable identity is `(backend_kind, remote_id)`. `key` and `url` are display/compatibility metadata.

### 4.2 Task states and labels

`TaskStateCategory` is the backend-neutral state bucket used by Scherzo:

| Category | Meaning |
| --- | --- |
| `Backlog` | Not ready for dispatch. |
| `Ready` | Ready or dispatchable. |
| `Active` | In progress. |
| `Done` | Successfully complete. |
| `Canceled` | Closed without completion. |
| `Duplicate` | Closed as a duplicate. |
| `Unknown` | Backend state has not been classified. |

`TaskState(id, name, category)` preserves the backend state id when known, the display name, and the normalized category. Adapters MUST preserve the backend state `name`; they SHOULD set `id` when the backend exposes a stable state id. `Unknown` is valid when the adapter cannot safely classify the state.

`TaskLabel(id, name)` preserves a backend label id when known and a display name. Routing code treats label names as the compatibility surface.

### 4.3 Comments, attachments, and links

`TaskComment(id, task, author_id, body, created_at, updated_at)` represents a comment read from or written to a task. `author_id`, `created_at`, and `updated_at` are optional and MAY be absent when the backend does not expose them. The comment `id` MUST be stable enough for update/acknowledgement workflows when the backend supports updates.

`TaskAttachment(id, task, name, url)` represents a generic attachment. The `url` MUST be a backend-visible URL or file handle that operators can use according to backend policy.

`TaskLink(id, task, title, url)` represents an external link attached to a task. `id` MAY be absent when the backend identifies links by URL/title rather than by a stable id.

### 4.4 `Task`

`Task(ref, title, description, priority, state, branch_hint, labels, blockers, blockers_complete, created_at, updated_at)` is the normalized task snapshot used by Scherzo.

- `ref` MUST identify the backend task.
- `title` SHOULD be suitable for operator display.
- `description`, `priority`, `branch_hint`, `created_at`, and `updated_at` MAY be absent.
- `state` MUST contain the backend state name and best-known category.
- `labels` SHOULD include workflow-routing labels when available.
- `blockers` SHOULD include blocking task references when available.
- `blockers_complete` MUST be `True` only when the adapter knows the blocker list is complete or when the backend has no blocker concept. It MUST be `False` when blockers are partial or indicate that dispatch should treat dependency data cautiously.

### 4.5 Linear compatibility mapping

`task.from_legacy_issue` maps a Linear `tracker/issue.gleam` value to a backend-neutral task:

| Legacy Linear issue field | Backend-neutral task field |
| --- | --- |
| `id` | `TaskRef.remote_id` with `backend_kind = "linear"` |
| `identifier` | `TaskRef.key = Some(identifier)` |
| `url` | `TaskRef.url` |
| `title` | `Task.title` |
| `description` | `Task.description` |
| `priority` | `Task.priority` |
| `state` | `TaskState.name`; category is currently `Unknown` in this compatibility conversion |
| `branch_name` | `Task.branch_hint` |
| `labels` | `TaskLabel(name: label)` values |
| `blocked_by` | `Task.blockers` as Linear `TaskRef` values |
| `blocked_by_complete` | `Task.blockers_complete` |
| `created_at`, `updated_at` | matching task timestamps |

`task.to_legacy_issue` is a compatibility conversion only. It MUST succeed only for `backend_kind = "linear"` tasks with a non-empty `key`; otherwise it returns `RequiresLinearTask` or `MissingTaskKey`.

## 5. Capability matrix

Capability names in this table are canonical and MUST match the public fields in `src/scherzo/tracker/adapter.gleam`.

| Capability | Required by adapter construction | Feature that requires it at startup | Notes |
| --- | --- | --- | --- |
| `task_source` | Yes | Always; it is a non-optional field | Reads candidate tasks, refreshes task refs, and resolves operator refs. |
| `comments` | No | Handoff, invalid-workflow, and other outbound comment reporting when configured | Linear comment command acknowledgements are removed. Invalid-workflow comment reporting also uses this capability at runtime when configured. |
| `remote_commands` | No | No production runtime feature today | `remote_commands` and the legacy `linear_commands` section are removed command-transport settings; leaving either section in config is a validation error. |
| `state_transitions` | No | Handoff state moves when configured | Moves tasks to configured states. Invalid-workflow state reporting also uses this capability at runtime when configured. |
| `routing_metadata` | No | Workflow label routing when workflow label paths are configured | Extracts labels and blocker refs from normalized tasks. |
| `links` | No | No required startup feature today | Generic link upsert seam for future use. |
| `handoff` | No | Handoff comments/reporting when handoff comments are enabled | Current Linear path still uses legacy handoff event variants. |
| `scheduled_failures` | No | Scheduled job failure publication for enabled `scheduled_jobs.<id>.on_failure` | Publication MUST be idempotent by `dedupe_key`. |
| `readiness` | No | Tracker contract/readiness checks when a caller enables `readiness_checks_enabled` | Current Linear contract doctor uses a compatibility path rather than this adapter capability. |
| `smoke` | No | Tracker smoke checks when a caller enables `smoke_checks_enabled` | Linear exposes this capability; Linear CLI aliases remain. |
| `attachments` | No | No required startup feature today | Linear attachment upload is still a Linear-only compatibility helper, not this generic capability. |

## 6. Capability operation contracts

All operations return `Result(..., TrackerError)`. Adapter operations MUST NOT panic for ordinary backend failures. They MUST map backend failures into the error categories in [section 7](#7-trackererror-semantics).

### 6.1 `task_source`

`TaskSourceCapability` exposes:

| Operation | Input | Result |
| --- | --- | --- |
| `fetch_candidates` | `TaskSearchRequest(active_states, dispatch_states, terminal_states, workflow_labels, limit)` | `List(Task)` candidate snapshots. |
| `refresh_by_refs` | `List(TaskRef)` | Current `List(Task)` snapshots for known refs. |
| `lookup_by_operator_ref` | operator-supplied string | `Option(Task)` for a matching task. Empty or whitespace-only input SHOULD return `Ok(None)`. |

`fetch_candidates` MUST accept every request field. It SHOULD apply dispatch-state, workflow-label, and limit filters when the backend supports them. It MUST NOT mutate backend tasks. Returned tasks MUST have `Task.ref.backend_kind` equal to the adapter kind.

`refresh_by_refs` MUST treat each input `TaskRef` as a durable identity lookup by `(backend_kind, remote_id)`. Refs for another backend kind SHOULD return `NotFound(ref)` or be omitted only when the caller has explicitly documented partial-refresh semantics. The current Linear adapter returns `NotFound` for non-Linear or empty Linear refs.

`lookup_by_operator_ref` SHOULD match the backend `remote_id`, the human `key`, or another documented operator reference. It MUST return `Ok(None)` when no task is found.

Task-source operations are read-only and SHOULD be safe to retry after `Transient` failures.

### 6.2 `comments`

`CommentCapability.post_or_update` accepts `CommentRequest(task, body, mode)`.

`mode` is either:

- `CreateOnly`, which requests a new visible comment; or
- `UpdateExisting(comment_id, allow_create_fallback)`, which requests an update of an existing comment and optionally allows the adapter to create a replacement comment when update is unavailable.

The result is `CommentReceipt(id, task, url, created)`.

- `id` MUST identify the backend comment that exists after the operation.
- `task` MUST echo the task reference that was written.
- `url` SHOULD point to the created/updated comment or task when the backend exposes a useful URL.
- `created` MUST be `True` for a newly-created comment and `False` for an update.

When `UpdateExisting(..., allow_create_fallback: False)` cannot be implemented, the adapter SHOULD return `UnsupportedCapability("comments.update")`. When fallback is allowed, the adapter MAY create a new comment and return `created: True`.

Create-only comment operations are not guaranteed exactly-once under retry unless the adapter documents a backend-specific idempotency key. Update operations SHOULD be idempotent for the same `comment_id` and body.

### 6.3 `remote_commands`

`RemoteCommandCapability` remains in the adapter API for historical conformance fixtures and possible future adapter work, but Scherzo's production runtime no longer consumes it. Linear does not expose this capability, the daemon does not fetch command events, and remote command acknowledgements are not posted. Operator control is local through `scherzoctl`.

`RemoteCommandFetch(task_refs, since_event_ids, limit_per_task)` describes command-bearing events for tasks. `RemoteCommandEvent(event_id, task, author_id, body, command_name, excerpt, observed_at_ms)` is the normalized event shape for adapters that still exercise the optional conformance pack.

`RemoteCommandAck(event, body)` describes acknowledgement posting for that optional conformance pack only. It is not a production Linear command-comment transport.

### 6.4 `state_transitions`

`StateTransitionCapability.transition` accepts `StateTransitionRequest(task, target_state_id, target_state_name, reason)` and returns `StateTransitionReceipt(task, state)`.

- `target_state_id` SHOULD be preferred when present and non-empty.
- `target_state_name` MUST be accepted for configuration-driven state moves and SHOULD be resolved against backend state names when no id is present.
- `reason` is an operator/audit string and SHOULD be included in backend audit metadata when the backend supports it.
- The returned `TaskState` MUST describe the state after the operation.

Transitioning a task that is already in the target state SHOULD be treated as success. Missing, ambiguous, or unsupported target states SHOULD return `Permanent`. Refs for another backend kind SHOULD return `NotFound(ref)`.

Startup validation requires `state_transitions` when handoff state moves are configured. Invalid-workflow state reporting also depends on this capability when that reporting path is enabled.

### 6.5 `routing_metadata`

`RoutingMetadataCapability` exposes pure metadata extractors:

| Operation | Input | Result |
| --- | --- | --- |
| `workflow_labels` | `Task` | `List(String)` label names used for workflow routing. |
| `blocker_refs` | `Task` | `List(TaskRef)` blockers used for dispatch/dependency checks. |

These functions MUST be deterministic for a given task snapshot and MUST NOT perform remote writes. They SHOULD use the normalized task fields (`labels` and `blockers`) unless the adapter has documented backend-specific routing metadata. Startup validation requires `routing_metadata` for configured workflow-label routing paths.

### 6.6 `links`

`LinkCapability.upsert_link(task_ref, link)` creates or updates a visible external link and returns the resulting `TaskLink`.

The operation SHOULD be idempotent for the same task, title, and URL. If the backend cannot create links but the capability is exposed for other link suboperations, unsupported upsert SHOULD return `UnsupportedCapability("links.upsert")`. No current daemon startup feature requires `links`.

### 6.7 `handoff`

`HandoffCapability.report` accepts a `HandoffEvent` and returns `Nil` on success. Event variants are:

- `HandoffClaim(task, workspace_path, run_id)`
- `HandoffSuccess(task, run_id, summary)`
- `HandoffFailure(task, run_id, reason)`
- `HandoffPark(task, reason, release_policy)`
- `LegacyHandoffClaim(issue, workspace_path, run_id)`
- `LegacyHandoffSuccess(issue, success, run_id, workflow_id)`
- `LegacyHandoffFailure(issue, failure, run_id, workflow_id)`
- `LegacyHandoffPark(report)`

Adapters MUST accept every variant. For a configured feature that promises visible handoff reporting, the adapter MUST produce the corresponding backend-visible effect for the event variants Scherzo sends on that runtime path. Current Linear compatibility uses the legacy variants for issue-shaped handoff reports; the Linear adapter accepts the generic variants as no-ops for forward compatibility.

Handoff reports SHOULD be idempotent for the same task/run/event class where the backend can update or de-duplicate comments. Startup validation requires `handoff` when handoff comments/reporting are enabled.

### 6.8 `scheduled_failures`

`ScheduledFailureCapability.publish` accepts `ScheduledFailurePublication`:

| Field | Contract |
| --- | --- |
| `job_id`, `workflow_id` | Identify the scheduled job/workflow. |
| `due_at_ms`, `run_id`, `attempt`, `max_attempts` | Identify the failing scheduled run attempt. |
| `reason`, `title`, `body` | Operator-visible failure content. |
| `run_root`, `session_id` | Optional local recovery/session context. |
| `dedupe_key` | REQUIRED idempotency key for the visible failure task. |
| `labels` | Labels/tags the adapter SHOULD apply when supported. |
| `target_state_name` | Optional desired state for created/updated failure tasks; Linear currently requires a non-empty value. |
| `previous_task_remote_id` | Optional previously published failure task remote id to update. |

The result is `ScheduledFailureReceipt(task, created, comment_id)`.

The operation MUST be idempotent by `dedupe_key`: repeated publication for the same scheduled job/dedupe key MUST result in at most one visible open failure task for that key, updating or commenting on the existing task when appropriate. `created` MUST distinguish newly-created failure tasks from updates. `comment_id` SHOULD identify the update comment when one was created.

If the adapter cannot guarantee dedupe-key idempotency, it MUST NOT expose `scheduled_failures`. If a publication is well-formed but the backend reports a no-op that leaves no visible task/update, the adapter MAY return `UnsupportedCapability("scheduled_failures.publish")`.

Startup validation requires `scheduled_failures` for enabled scheduled failure publication paths.

### 6.9 `readiness`

`ReadinessCapability.check_contract` returns `List(ReadinessFinding)`, where each finding has `severity`, `code`, `message`, and optional `config_path`.

Readiness checks SHOULD validate adapter/backend configuration without mutating tasks. Findings MUST be actionable and MUST NOT include secrets. The capability is required only when the caller enables `readiness_checks_enabled`; current Linear contract checks still run through the Linear compatibility path.

### 6.10 `smoke`

`SmokeCapability.run_smoke_check` returns `SmokeReport(candidate_count, refreshed_count, terminal_sample_count, messages)`.

Smoke checks SHOULD perform low-risk read operations that prove candidate fetch and refresh behavior. They MUST NOT mutate task state. The capability is required only when the caller enables `smoke_checks_enabled`. Linear exposes `smoke`; `tracker-smoke` is the preferred operator name and `linear-smoke` remains a compatibility alias.

### 6.11 `attachments`

`AttachmentCapability.upload(task_ref, attachment)` uploads a generic task attachment and returns the resulting `TaskAttachment`.

The operation SHOULD be idempotent when the backend supports content hashes or stable attachment ids. It MUST reject unsafe or unsupported attachment URLs/files with `Permanent` or `UnsupportedCapability("attachments.upload")`. No current startup feature requires `attachments`; Linear attachment upload remains a Linear-only comment-file helper rather than this generic adapter capability.

## 7. `TrackerError` semantics

Adapters MUST map backend failures into these constructors:

| Error | Meaning | Retry guidance |
| --- | --- | --- |
| `Unauthorized(message)` | Credentials, scopes, or authorization are missing/invalid. | Operator action required; do not retry indefinitely without config change. |
| `NotFound(ref)` | The task ref does not identify a task in this backend or is for the wrong backend kind. | Do not retry unless task identity/config changes. |
| `Transient(message)` | Temporary backend/network/rate-limit/server failure. | Safe for Scherzo to retry according to runtime policy. |
| `Permanent(message)` | Request/config/backend state is invalid for this operation. | Operator/config/code action required. |
| `UnsupportedCapability(capability)` | The adapter or suboperation cannot perform the requested capability. | Disable the feature or use an adapter that supports it. Startup validation should catch configured missing capabilities. |
| `DecodeFailed(message)` | Backend response could not be decoded or did not match expected schema. | Treat as backend/API contract drift; retry only after considering payload stability. |

Error messages MUST be bounded and MUST NOT include API keys, bearer tokens, raw secret values, or unredacted request bodies containing secrets.

## 8. Startup capability validation

Before starting work that depends on optional tracker side effects, Scherzo validates configured feature requirements with `validate_required_capabilities`. Validation returns one `CapabilityValidationError` per missing feature/capability/config path.

Operator-facing diagnostics are rendered as:

```text
tracker_capability_missing feature=<feature> capability=<capability> path=<config_path> backend=<backend_kind> message="<actionable message>"
```

The current validation rules are:

| Feature | Required capability | Config path | Message |
| --- | --- | --- | --- |
| `remote_commands` | n/a | n/a | Production startup no longer enables remote command ingestion. |
| `remote_command_ack` | n/a | n/a | Production startup no longer posts remote command acknowledgements. |
| `handoff_comments` | `handoff` | `handoff.comments` or caller-supplied handoff path | `handoff comments require handoff capability` |
| `handoff_state_moves` | `state_transitions` | `handoff.states` or caller-supplied handoff path | `handoff state moves require state_transitions capability` |
| `workflow_label_routing` | `routing_metadata` | each configured workflow label path | `workflow label routing requires routing_metadata capability` |
| `scheduled_failures` | `scheduled_failures` | each enabled scheduled failure path | `scheduled failure publication requires scheduled_failures capability` |
| `tracker_contract` | `readiness` | `doctor.checks.tracker-contract` | `tracker contract checks require readiness capability` |
| `tracker_smoke` | `smoke` | `doctor.checks.tracker-smoke` | `tracker smoke checks require smoke capability` |

`task_source` is a required field in the `TrackerAdapter` type, so missing task-source support is a construction-time violation rather than an optional-capability diagnostic.

If an adapter cannot perform a configured feature, Scherzo MUST fail startup validation with an actionable missing-capability diagnostic before dispatching work. Normal daemon startup currently validates handoff, workflow label routing, and scheduled failure publication. Readiness and smoke validation is used by callers that enable those requirement flags.

## 9. Durable and recovery compatibility

Tracker adapters participate in durable recovery through stable task identity. Implementers MUST preserve these invariants:

- `TaskRef.backend_kind` and `TaskRef.remote_id` MUST be stable across daemon restarts and long-lived enough to replay outbox effects.
- Durable task fields are `task_backend_kind`, `task_remote_id`, `task_key`, and `task_url`.
- Records that include task fields MUST keep legacy `issue_id` and `issue_identifier` fields when the record shape still contains them. Those fields are compatibility aliases, not the backend-neutral identity.
- Decoding old workflow records without task fields MUST continue to synthesize a Linear task ref with `backend_kind = "linear"`, `remote_id = issue_id`, `key = Some(issue_identifier)`, and `url = None`.
- Existing `issue.*` prompt variables and `SCHERZO_ISSUE_*` environment variables MUST remain compatibility aliases until explicitly migrated by a separate compatibility plan.
- Existing Linear command ledger records (`linear_command_seen`, `linear_command_started`, `linear_command_completed`, and `linear_command_acked`) MUST remain decodable.
- Backend-neutral remote command records (`remote_command_seen`, `remote_command_started`, `remote_command_completed`, and `remote_command_acked`) MUST use `backend_kind`, `event_id`, and `task_remote_id` as the durable remote-command identity. `task_key` is optional display metadata.
- Pending Linear/remote command acknowledgement outbox records from older versions MUST remain decodable and recoverable as failed/ignored local state; they MUST NOT post new Linear comments after this transport removal.
- Scheduled failure retry/update flows MAY pass `previous_task_remote_id`; adapters MUST interpret it in the adapter's own backend kind.

Adapters MUST NOT require operators to delete old Linear-shaped ledgers or checkpoints when upgrading to the backend-neutral task model.

## 10. Backend-neutral contract versus Linear compatibility surfaces

The backend-neutral contract consists of `src/scherzo/task.gleam`, `src/scherzo/tracker/adapter.gleam`, this specification, and tests that use the fake non-Linear adapter seam.

Linear compatibility surfaces are intentionally preserved and are not generic adapter requirements:

- `backend_kind = "linear"` and Linear `remote_id` values are Linear-specific.
- `tracker/issue.gleam` and `task.from_legacy_issue`/`task.to_legacy_issue` preserve existing Linear issue behavior.
- `issue.*` prompt variables, `SCHERZO_ISSUE_*`, `issue_id`, and `issue_identifier` remain compatibility names.
- `linear_contract`, `linear-smoke`, `linear-contract`, `--linear-smoke`, and `--linear-contract-check` remain compatibility aliases or Linear-specific config/CLI surfaces. `linear_commands` is a removed config surface; leaving the section in config is rejected.
- Linear-only helper scripts and options that create, update, or inspect Linear tasks directly remain Linear-specific until replaced by generic adapter capabilities.

Future Jira/Trello work MUST add production adapters and tests before docs or examples claim support. A fake or historical plan is not evidence of production conformance.

## 11. Security and secrets

Adapters are responsible for protecting task-system credentials and operator data.

- Credentials SHOULD come from config/env resolution, not from task fields.
- `kind`, `display_name`, task refs, receipts, readiness findings, smoke messages, and error messages MUST NOT contain secrets.
- Backend request/response bodies SHOULD be logged only through existing Scherzo redaction policy.
- Comments, acknowledgements, scheduled failure bodies, handoff summaries, links, and attachments are operator-visible backend content. Callers and adapters SHOULD avoid embedding local secret values in them.
- `Unauthorized` errors SHOULD identify the missing capability/scope at a high level without including credential material.

## 12. Conformance profiles

### 12.1 Minimal task-source adapter

A minimal adapter provides `kind`, `display_name`, and `task_source`, with all optional capabilities set to `None`. It can support read-only candidate discovery, refresh, and operator lookup. It cannot run configurations that require workflow-label routing, handoff, scheduled failures, readiness, or smoke checks unless those features are disabled or validated elsewhere.

### 12.2 Dispatch/routing adapter

A dispatch/routing adapter provides `task_source` and `routing_metadata`. It supports workflow selection by labels and blocker metadata extraction. It SHOULD populate `Task.labels`, `Task.blockers`, and `Task.blockers_complete` accurately enough for dispatch policy.

### 12.3 Historical remote-command conformance adapter

A historical remote-command conformance adapter may provide `task_source`, `remote_commands`, and `comments` for the optional tracker-conformance pack. This profile is not a production operator-control path: daemon startup no longer enables remote-command ingestion, and no runtime config requires `remote_commands`.

### 12.4 Handoff/state adapter

A handoff/state adapter provides `handoff` for configured handoff reporting and `state_transitions` for configured handoff state moves. It MAY also provide `comments` when invalid-workflow comment reporting is enabled through generic comment paths.

### 12.5 Scheduled-failure-capable adapter

A scheduled-failure-capable adapter provides `scheduled_failures` and MUST publish at most one visible open failure task per `dedupe_key`/scheduled job, updating or commenting on that task for later attempts. It SHOULD apply configured labels and target state when the backend supports them.

### 12.6 Linear-equivalent adapter

A Linear-equivalent adapter for today's production behavior provides:

- `task_source`
- `comments`
- `state_transitions`
- `routing_metadata`
- `handoff`
- `scheduled_failures`
- `smoke`

The current Linear adapter does not expose `remote_commands`, generic `links`, `readiness`, or `attachments`. Linear readiness/contract checks still run through the `linear_contract` compatibility path, and attachment upload still uses Linear-only comment-file helpers.

## 13. Current adapter conformance status

| Adapter | Status | Conformance summary |
| --- | --- | --- |
| Linear | Production | Provides `task_source`, `comments`, `state_transitions`, `routing_metadata`, `handoff`, `scheduled_failures`, and `smoke`. Does not expose `remote_commands`, generic `links`, `readiness`, or `attachments`. Contract/readiness and attachments remain Linear compatibility surfaces. |
| `test-memory` | Test fixture | Provides the fake non-Linear seam used by tests. It is not a production backend. |
| Jira | Future work | No production adapter is supported today. |
| Trello | Future work | No production adapter is supported today. |

Known Linear adapter gaps:

- Inbound command comments are removed: the Linear adapter intentionally does not expose `remote_commands`, and Scherzo posts no command acknowledgements.
- Generic `links`, `readiness`, and `attachments` are not exposed through the adapter today; corresponding Linear behavior remains on compatibility paths where it exists.

## 14. Extension and versioning rules

Changes to the tracker adapter contract SHOULD be additive:

- New optional capabilities MAY be added as `Option(...)` fields when the default unsupported value is `None`.
- New fields in task/receipt records SHOULD be optional or have compatibility defaults.
- Existing capability names MUST NOT be renamed without a migration plan and docs/test updates.
- Existing legacy Linear decoding and aliases MUST remain until a separate compatibility change explicitly removes them.
- New production adapters MUST document their conformance profile, unsupported capabilities, backend-specific idempotency behavior, and any implementation-defined matching rules for operator refs or routing metadata.
