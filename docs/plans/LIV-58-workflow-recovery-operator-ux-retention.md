# Make workflow recovery visible and safe for operators

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Operators need to understand what Scherzo did after a daemon restart or workflow interruption before they decide whether to wait, inspect, resume, park, clean up, or reset state. After this change, `scripts/scherzoctl ps`, `scripts/scherzoctl session <session-id> --json`, daemon logs, README guidance, and an operator runbook expose recovery meaning separately from live worker status. A worker may be `running`, `waiting_ui`, or `exited`; recovery meaning answers a different question: whether the run was restored from durable state, interrupted by a daemon stop, parked, eligible only for artifact cleanup, or blocked by unsupported local state.

The observable result is intentionally modest. This plan does not create new runtime recovery semantics, decide whether a workflow step can be replayed, or recover live pi processes. It adds a verified projection from current durable facts into operator-facing status, reserves vocabulary for future workflow-resumption facts without emitting unsupported states, and defines a retention policy whose destructive controls are gated behind verified artifact roots, schema markers, dry-run inventory, and path-safety checks.

## Problem Framing and Constraints

Today Scherzo has daemon control commands, session summaries, session event JSON, logs, and a durable ledger under `.scherzo-state/ledger/`. Those surfaces expose low-level process status such as a worker running, waiting for UI, stopped, or exited. That is not enough for recovery. An operator seeing an exited worker still has to infer whether the run finished normally, was interrupted by a daemon crash, was restored as a retry, remains parked, has terminal workspace cleanup pending, or cannot start because local durable state uses an unsupported schema.

The concrete operator pain is uncertainty during recovery. If Scherzo hides the difference between “interrupted by daemon restart” and “parked by policy,” the operator can accidentally retry unsafe work or wait for work that will never continue. If Scherzo hides old local durable state incompatibility behind a generic startup failure, the operator will not know whether to archive, discard, or reinitialize local state. If cleanup deletes ambiguous local artifacts, the operator can lose the only evidence needed to debug recovery.

The main constraint is that runtime workflow-resumption behavior is out of scope. This plan must not invent previous pi session linkage, workflow-step checkpoint replay, unsafe command-step classification, or drift detection if those facts are not present in the current tree. Statuses that lack source facts are kept as documented reserved vocabulary and must not be emitted by implementation tests or acceptance criteria until a later plan adds the corresponding durable facts.

## Strategy Overview

Use a small, additive operator status model instead of overloading the existing live worker status. `src/scherzo/session/event.gleam` currently defines live worker statuses `Preparing`, `Probing`, `Running`, `WaitingUi`, `Stopping`, and `Exited(reason)`. Those describe the worker process. Add a separate `recovery` object to session summaries for recovery meaning. Existing JSON consumers can ignore the new field, and new operator tooling can distinguish process state from recovery state.

The first implementation tier emits only statuses backed by verified current-tree facts:

- `recovered` is emitted when startup recovery in `src/scherzo/state/recovery.gleam` restores durable state but no more specific status applies to the session summary being reported.
- `interrupted` is emitted from `src/scherzo/state/projection.gleam` run facts `RunRunning` discovered during startup recovery and `RunInterrupted` records produced from `src/scherzo/state/record.gleam` `RunInterrupted`.
- `parked` is emitted from `projection.ParkedIssue`, which is created from ledger records `IssueParked` and `IssueParkedV2` and restored by `restore_parked` in `src/scherzo/state/recovery.gleam`.
- `cleanup` is emitted for terminal or abandoned local artifacts after a read-only retention classifier determines they have no recovery hold. The current tree already has `recovery.CleanupRequest` for terminal interrupted runs with known workspace paths; this plan may surface that as cleanup inventory, but must not delete arbitrary workspace directories.
- `old_state_reset_required` is emitted primarily by offline `scripts/scherzoctl state status` when local ledger or snapshot schema markers are unsupported. It appears on a session summary only if a real session exists and a source fact is available.

The second tier is reserved vocabulary. The implementation may define string conversions and documentation for these statuses, but it must not emit them from real recovery projection until source facts exist:

- `resumed` requires a durable workflow checkpoint or previous pi session identifier that the current inspected tree does not expose.
- `inspection_needed` requires a runtime hold fact saying automatic continuation intentionally stopped for operator inspection.
- `blocked` requires a durable unsafe command-step or side-effecting step classification.
- `drift_detected` requires a durable drift rejection fact. The current tree restores parked issues and auto-unparks on issue fingerprint change; that is not the same as workflow drift rejection.

Retention is split into read-only inventory first and deletion second. Cleanup dry run is safe to implement once it treats unknown, malformed, unsupported, missing-owner, missing-terminal-time, and path-unsafe artifacts as retained with warnings. Cleanup apply is allowed only after the same inventory verifies exact roots under `<workspace-root>/.scherzo-state/`, ownership markers or schema markers, containment, and symlink safety. Pi transcript deletion is dry-run-only unless a concrete transcript root exists in the current tree and passes the same ownership and containment rules.

## Alternatives Considered

The simplest alternative is to update README text only and tell operators to infer recovery state from existing worker statuses and logs. That is insufficient because machine-readable `--json` control output would remain ambiguous, and pi-based operator automation would have to parse prose logs or guess from worker exit reasons.

Another option is to replace the existing `SessionStatus` type with recovery-specific states. That is too risky and semantically muddy. A worker can be `running` while recovery meaning is `interrupted` or `recovered`, and a worker can be `exited` while artifact lifecycle state is `cleanup`. Keeping process status and recovery status separate avoids breaking the current mental model and avoids unnecessary protocol churn.

A broader option is to implement workflow-step replay, previous pi session resumption, unsafe command-step detection, drift rejection, cleanup deletion, and old-state reset all at once. That is larger than this operator UX ticket can safely own. This plan reserves names for future states, emits only states backed by current durable facts, and requires read-only proof before destructive local maintenance.

## Risks and Countermeasures

One risk is that the new recovery status model becomes a second source of truth for runtime decisions. The countermeasure is to make `RecoveryInfo` a projection of durable facts. The scheduler must not decide whether to resume, park, retry, or skip based on a CLI label introduced here. Tests must build real `record.RecordBody` values, fold them through `src/scherzo/state/projection.gleam`, and assert that summaries and JSON expose the facts; tests must not assert new scheduling semantics.

A second risk is emitting statuses that current code cannot actually know. The countermeasure is the verified source inventory in this plan. `resumed`, `inspection_needed`, `blocked`, and `drift_detected` remain reserved unless implementation first finds or adds a current-tree source fact in runtime recovery work. If a future implementer believes one of those facts exists, they must update the source inventory, tests, acceptance, and Decision Log before emitting it.

A third risk is cleanup deleting information needed to diagnose an interrupted or blocked run. The retention classifier must retain active, recovered-without-terminal-time, interrupted, parked, old-state-reset-required, unknown, malformed, unsupported, path-unsafe, and missing-owner artifacts. Cleanup apply must classify all candidates before deleting any candidate. A root-level safety error aborts the deletion phase. A per-file deletion error records a warning and may continue with other already-classified eligible files, but it must never touch retained candidates.

A fourth risk is deleting or moving the wrong directory during offline state maintenance. The countermeasure is to operate only below `<workspace-root>/.scherzo-state/`, derive paths through `ledger.path_for_workspace_root` and a new local-state helper, reject empty or filesystem-root workspace roots, reject paths that escape the state root after normalization, reject symlink escapes, and require an unsupported schema marker before `archive-old` or `discard-old` changes anything. Corrupt or malformed state is not treated as unsupported old state; it is retained for manual inspection.

A fifth risk is leaking sensitive data. Pi transcripts and raw event payloads can contain prompts, tool inputs, tool outputs, local file names, and Linear excerpts. New logs and summaries must include identifiers, bounded redacted messages, status strings, counts, and artifact classes only. Add one reusable helper, `recovery_safe_text`, that calls `log.redact` and `log.truncate` with a maximum of 200 characters for recovery messages, cleanup warnings, state-status reasons, and log fields. Do not log raw transcript text, full prompts, full Linear comment bodies, API tokens, or unredacted tool payloads.

A sixth risk is breaking existing JSON consumers. The countermeasure is to leave `status` as the live worker status and add `recovery` as a new nullable object. Missing `recovery` must decode as `None`. Additive response fields should not require a control protocol version bump unless implementation discovers an existing decoder cannot tolerate them.

## Progress

- [x] (2026-05-03) Drafted this ExecPlan from the Linear issue and a bounded inspection of the current repository surfaces.
- [x] (2026-05-03) Incorporated adversarial review by narrowing emitted statuses to current durable facts, deferring unsupported statuses, gating destructive cleanup, removing outside-plan dependencies, and adding source inventory, path-safety, redaction, and edge-case testing requirements.
- [ ] Verify source-fact inventory in the implementation workspace before adding new code; update this plan if the current tree has drifted.
- [ ] Add the canonical recovery status model and serialize nullable recovery metadata without changing live worker `status` semantics.
- [ ] Project currently backed recovery facts into session summaries, control JSON, CLI output, and structured logs.
- [ ] Add read-only retention and old-state inventory with path, schema, metadata, and symlink safety checks.
- [ ] Add deletion-capable cleanup and old-state mutation only after read-only inventory tests pass.
- [ ] Update README and add the operator runbook for workflow recovery and retention.
- [ ] Run formatting, tests, plan validation, and documentation path checks; update this plan with outcomes.

## Surprises & Discoveries

- Observation: README already warns that Scherzo is moving quickly and that local development state such as `.scherzo-state/ledger` may be deleted or regenerated after breaking internal changes.
  Evidence: `README.md` contains that guidance near the top-level development notes.
- Observation: The existing control surface is already centered on `scripts/scherzoctl` commands including `ps`, `session`, `events`, `attach`, `prompt`, `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, and `ui respond`.
  Evidence: `README.md` documents these commands in the daemon control section.
- Observation: The current session summary JSON includes `session_id`, `display_name`, issue fields, `workspace_path`, `pi_session_id`, `status`, `exit_reason`, turn counters, timestamps, and token totals, but no recovery-specific field.
  Evidence: `src/scherzo/session/event.gleam` defines `SessionSummary`; `src/scherzo/session/json.gleam` serializes it.
- Observation: The current durable recovery code exposes interrupted runs, restored parked issues, restored retry timers, terminal workspace cleanup requests, outbox replay, and warnings, but does not expose previous pi session linkage, workflow-step checkpoint identifiers, unsafe command-step classification, or workflow drift rejection.
  Evidence: `src/scherzo/state/recovery.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/record.gleam` define the current recovery plan and record vocabulary.
- Observation: The current ledger path helper knows `.scherzo-state/ledger/current.jsonl`, `.scherzo-state/ledger/snapshot.json`, and `.scherzo-state/ledger/archive/`, but the bounded review did not find a concrete pi transcript root in the current source tree.
  Evidence: `src/scherzo/state/ledger.gleam` defines `LedgerPath`; `src/scherzo/session/event.gleam` and `src/scherzo/session/json.gleam` expose only `pi_session_id` metadata.

## Decision Log

- Decision: Represent recovery meaning as a new nullable `recovery` object on session summaries instead of adding recovery labels to the existing live worker `status` field.
  Rationale: Process state and recovery meaning are independent; keeping them separate avoids misleading output and reduces breakage for existing control clients.
  Date: 2026-05-03
- Decision: Emit only recovery statuses backed by current durable facts in this ticket: `recovered`, `interrupted`, `parked`, `cleanup`, and offline `old_state_reset_required`.
  Rationale: The review found that several originally promised states lacked source facts. Emitting labels without durable facts would create false confidence and contradict the scope boundary.
  Date: 2026-05-03
- Decision: Keep `resumed`, `inspection_needed`, `blocked`, and `drift_detected` as reserved vocabulary, serialization values, and documentation entries, but do not accept them as operationally observable until later runtime work provides source facts.
  Rationale: Operators benefit from stable vocabulary, but implementation must not invent runtime semantics in the UX layer.
  Date: 2026-05-03
- Decision: Make cleanup read-only first and deletion-capable only after artifact roots, ownership/schema markers, path containment, and symlink behavior are verified by tests.
  Rationale: Recovery artifacts and transcripts may be the only debugging evidence, and guessed roots or broad deletion can destroy unrelated local data.
  Date: 2026-05-03
- Decision: Treat old-state compatibility primarily as an offline `scripts/scherzoctl state status` outcome, not as a per-session recovery state.
  Rationale: Unsupported local state may prevent daemon startup and session creation, so the operator needs a path that does not depend on a running control server.
  Date: 2026-05-03
- Decision: Use one reusable redaction/truncation helper for recovery UX strings with a 200-character bound.
  Rationale: Repeated ad-hoc truncation makes leaks more likely. A single helper lets tests assert consistent behavior across JSON, CLI output, cleanup output, state output, and logs.
  Date: 2026-05-03
- Decision: Remove references to outside child plans and inline the old-state command, schema-detection, and safety rules here.
  Rationale: A later implementer must be able to execute this plan from the current working tree and this file alone.
  Date: 2026-05-03

## Outcomes & Retrospective

Implementation has not started. Fill this section after each milestone with what changed, what was validated, what remains risky, and whether operators can distinguish the recovery states backed by current source facts.

## Context and Orientation

Scherzo is a Gleam application that supervises issue-driven agent work. In daemon mode it starts a local control server and writes a control file. Operators and pi operator skills use `scripts/scherzoctl` to inspect active sessions and send controls. A session is the operator-visible record of an agent run. EventHub is the in-memory session event stream and summary surface used by `scherzoctl ps`, `scherzoctl session`, `scherzoctl events`, and attach-style views. A pi session is the conversation or run identifier assigned by pi; live pi processes do not survive a BEAM restart, and the current inspected tree only stores the current `pi_session_id` in session summaries.

The local durable ledger is under `<workspace-root>/.scherzo-state/ledger/`. README describes `current.jsonl`, `snapshot.json`, and archived JSONL segments. The ledger is operational state, not a transcript. It should contain identifiers, statuses, bounded excerpts, result codes, and redacted strings only. Pi session transcripts are different: they may include raw or semi-raw conversation content and must be treated as sensitive local artifacts. Because this plan has not verified a concrete pi transcript root in the current tree, transcript retention is documented and inventoried only when a verified root exists; transcript deletion must otherwise report “transcript root unavailable” and delete nothing.

The files most relevant to this plan are:

- `src/scherzo/session/event.gleam`, which defines `SessionStatus`, `LifecycleEventName`, `EventPayload`, `SessionSummary`, `SessionList`, `SessionEvent`, and helpers that convert status and event names to strings.
- `src/scherzo/session/reason.gleam`, which defines worker exit reasons such as normal, failed, operator abort, worker down, and stopped.
- `src/scherzo/session/json.gleam`, which serializes session summaries, events, pages, tokens, raw redacted JSON, and exit reasons.
- `src/scherzo/control/protocol.gleam`, which defines the local control protocol, request and response shapes, list/session/event response helpers, command result JSON, and decoders for session summaries.
- `src/scherzo/control/command.gleam`, which defines operator commands and command result status values used by control protocol responses.
- `src/scherzo/ctl.gleam`, which is the Gleam side of the `scripts/scherzoctl` CLI and formats human and JSON output.
- `src/scherzo/orchestrator/event_publisher.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/orchestrator/control_command_handler.gleam`, which connect daemon state, session events, and operator commands.
- `src/scherzo/state/ledger.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam`, which are the verified sources for durable recovery facts and unsupported schema errors.
- `src/scherzo/log.gleam`, which provides structured log formatting plus `redact` and `truncate` helpers.
- `README.md`, which documents daemon operation, `scripts/scherzoctl`, Linear command fallback, the local durable ledger, and current recovery limitations.
- Existing tests under `test/session_event_test.gleam`, `test/session_hub_test.gleam`, `test/control_protocol_test.gleam`, `test/ctl_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/state_ledger_test.gleam`, `test/state_projection_test.gleam`, and `test/state_recovery_test.gleam`.

## Preconditions and Verified Facts

This plan assumes the repository is a Gleam project with `gleam.toml` and tests under `test/`. From the repository root, validation should normally run through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test

If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the direnv-backed command. Treat that as environment setup, not a code failure.

The inspected tree contains `scripts/scherzoctl`, `README.md`, the `src/scherzo/session/`, `src/scherzo/control/`, `src/scherzo/orchestrator/`, and `src/scherzo/state/` modules named above. `src/scherzo/session/event.gleam` currently has live worker statuses `Preparing`, `Probing`, `Running`, `WaitingUi`, `Stopping`, and `Exited(reason)`. `src/scherzo/session/json.gleam` currently serializes `status` and `exit_reason` but no recovery metadata. `src/scherzo/control/protocol.gleam` currently serializes list/session responses through `session_json.summary_to_json`, so adding recovery metadata to that serializer is the narrowest path to consistent JSON output.

The current `SessionSummary` fields are, in order: `session_id`, `display_name`, `issue_id`, `issue_identifier`, `issue_title`, `workspace_path`, `pi_session_id`, `status`, `current_turn`, `started_at_ms`, `last_event_at_ms`, and `token_totals`. Add `recovery: Option(RecoveryInfo)` after `status` unless implementation discovers a lower-churn placement; do not remove or reorder the existing fields without updating all constructor call sites and tests deliberately.

The current durable source facts are:

- `record.RunStarted`, `record.RunFinished`, and `record.RunInterrupted` fold into `projection.RunRunning`, `projection.RunFinished`, and `projection.RunInterrupted`.
- `recovery.plan` marks `projection.RunRunning` as interrupted on startup by appending `record.RunInterrupted(run_id, issue_id, "daemon_restart")`.
- `record.IssueParked` and `record.IssueParkedV2` fold into `projection.ParkedIssue`, including reason, observed issue update time, parked time, release policy, and issue fingerprint.
- `recovery.plan` restores parked issues and retry timers and returns `RecoveredRetry`, `CleanupRequest`, `OutboxReplay`, and warning strings.
- `ledger.path_for_workspace_root` derives `<workspace-root>/.scherzo-state/ledger/current.jsonl`, `<workspace-root>/.scherzo-state/ledger/snapshot.json`, and `<workspace-root>/.scherzo-state/ledger/archive/`.
- `record.decode_string` returns `UnsupportedVersion(version)` when a ledger JSONL record has an unsupported `schema_version`; `ledger.read_records` and `ledger.replay` surface this as `ledger.UnsupportedVersion(version)`.
- `src/scherzo/log.gleam` has `redact(key, value, secrets)` and `truncate(value, max)` helpers that new recovery text must reuse.

The current tree does not expose durable facts for previous pi session identifiers, workflow step identifiers, unsafe command-step replay classification, operator inspection holds, or workflow drift rejection. Fields depending on those facts must serialize as `null` and reserved statuses depending on those facts must not be emitted.

README already documents that local durable ledger state is stored below `<workspace-root>/.scherzo-state/ledger/` and that the ledger must not contain API keys, raw pi JSON, full prompts, or full Linear comment bodies. Preserve that invariant and extend it to the new recovery and retention policy.

## Scope Boundaries

In scope:

- Define the canonical recovery status strings `recovered`, `interrupted`, `resumed`, `inspection_needed`, `blocked`, `parked`, `cleanup`, `drift_detected`, and `old_state_reset_required`, while emitting only the currently backed subset described in this plan.
- Add a nullable recovery metadata object to session summaries and EventHub lifecycle events.
- Expose currently backed recovery metadata in local control JSON and `scripts/scherzoctl` human output.
- Add read-only retention inventory and cleanup dry-run output for local artifacts under verified roots.
- Add deletion-capable cleanup only after read-only inventory proves exact roots, ownership or schema markers, metadata completeness, containment, symlink safety, and all-read-before-delete behavior.
- Add offline old-state status, archive, discard, and reinitialize controls with `--root <workspace-root>` and `--yes` for mutations.
- Add structured, redacted logs for recovery status changes, cleanup dry runs, cleanup application, old-state detection, old-state archive, old-state discard, and reinitialization.
- Define local retention rules for workflow artifacts, ledger archives, cleanup tombstones, and pi session transcripts when a transcript root is verified.
- Update README and add an operator runbook under `docs/runbooks/workflow-recovery.md`.
- Add tests for real durable inputs, JSON/control output, CLI formatting/parsing, retention classification, path safety, schema detection, redaction, and documentation examples.

Out of scope:

- Deciding whether a workflow step is safe to replay.
- Resuming a previous live pi process or inventing previous pi session linkage.
- Adding workflow-step checkpoint schema, drift detection schema, or unsafe command-step schema.
- Deleting arbitrary worker workspace directories named by `recovery.CleanupRequest`; this plan may report them as cleanup candidates or warnings, but deletion is limited to verified `.scherzo-state` artifact roots.
- Adding backward compatibility shims for pre-workflow-resumption local durable state.
- Changing Linear command transport semantics.
- Changing pi RPC behavior or pi transcript capture semantics beyond retention metadata and operator-visible identifiers.
- Building a new web UI or external dashboard.

If implementation discovers a durable fact for a reserved status in the current tree, update `## Preconditions and Verified Facts`, `## Operator-Visible Status Contract`, tests, acceptance, and the Decision Log before emitting that status. If no source fact exists, do not add placeholders that can pass tests while delivering no operator value.

## Operator-Visible Status Contract

The implementation must publish the following contract in README, in the runbook, in code comments near the status type, and in tests. The JSON status strings are part of the local control API.

`recovered` is informational. It says Scherzo replayed durable state and knows this run or issue came from recovery, but no more specific operator hold is known. In this ticket it is derived from `recovery.plan` effects such as restored retry timers, replayable outbox entries, or recovery warnings associated with an issue. The safe operator action is to observe with `scripts/scherzoctl session <session-id>` or `scripts/scherzoctl events <session-id>`.

`interrupted` requires attention unless existing recovery logic has already scheduled a retry or parked the issue. It says a run was active without a recorded terminal result. In this ticket it is derived from `projection.RunRunning` at startup or `projection.RunInterrupted`. Human output must say that live Erlang ports and live pi processes do not survive a daemon restart, even if a current pi session identifier is visible for a later live session.

`parked` means dispatch is suppressed for the issue. In this ticket it is derived from `projection.ParkedIssue`. Human output must show the park reason and release policy when available, using source fields `reason`, `release_policy`, `issue_fingerprint`, `parked_at_ms`, and `observed_updated_at_ms`.

`cleanup` means no new runtime work is expected for the artifact being reported and local artifacts are in a retention phase: `retained`, `eligible`, `deleting`, or `deleted`. In this ticket it is derived from the retention classifier, terminal run facts, cleanup tombstones, and `recovery.CleanupRequest` inventory. Human output must never make cleanup look like a successful workflow result by itself; it is artifact lifecycle state.

`old_state_reset_required` means local durable state is from an unsupported schema family. It is primarily an offline `scripts/scherzoctl state status` result derived from ledger or snapshot schema checks. Human output must say that backward compatibility is intentionally not provided and list the safe choices: archive, discard, or reinitialize. The daemon and offline state commands must not silently delete or ignore this state.

`resumed` is reserved in this ticket. It must not be emitted until a durable workflow checkpoint or previous pi session identifier source exists. Documentation may say the reserved meaning is “continuing from a durable workflow checkpoint or previous pi session identifier.”

`inspection_needed` is reserved in this ticket. It must not be emitted until runtime recovery writes a durable hold fact saying automatic continuation stopped for operator inspection.

`blocked` is reserved in this ticket. It must not be emitted until runtime recovery writes a durable unsafe side-effecting step fact. Until then, cleanup treats unknown or command-like metadata as retained with warnings rather than labeling it blocked.

`drift_detected` is reserved in this ticket. It must not be emitted until runtime recovery writes a durable drift rejection fact, such as workflow fingerprint drift or issue fingerprint drift. Existing auto-unpark behavior on issue change is not enough to emit this status.

## Recovery Field Source Inventory

`RecoveryInfo.status` comes from the status contract above. For this ticket, only backed statuses may appear in operator output from real recovery projection.

`RecoveryInfo.message` comes from bounded source strings such as `projection.RunInterrupted.reason`, `projection.ParkedIssue.reason`, `recovery.RecoveredRetry.reason`, `recovery.RecoveryPlan.warnings`, cleanup classifier reasons, or old-state schema diagnostics. Before storage, JSON serialization, CLI rendering, or logging, pass the text through `recovery_safe_text`, which must call `log.redact` and `log.truncate` with a 200-character maximum.

`RecoveryInfo.safe_actions` is derived from status, not from ad-hoc call sites. Use stable action strings such as `inspect`, `view_events`, `retry`, `park`, `unpark`, `cleanup_dry_run`, `archive_old_state`, `discard_old_state`, and `reinitialize_state`. Reserved statuses may have documented action lists but must not appear in emitted real summaries.

`workflow_run_id` comes from the dictionary key of `projection.runs` for `RunRunning`, `RunFinished`, or `RunInterrupted`. It is `null` when the source fact is issue-level only, such as `projection.ParkedIssue`.

`workflow_step_id` is always `null` in this ticket because no current source fact exists.

`current_pi_session_id` comes from `SessionSummary.pi_session_id` when a live session summary exists. It is not recovered durable state.

`previous_pi_session_id` is always `null` in this ticket because no current source fact exists.

`park_reason`, `park_release_policy`, and `parked_at_ms` come from `projection.ParkedIssue` and are `null` for non-parked statuses.

`drift_kind` is always `null` in this ticket because no current source fact exists.

`retention_until_ms`, `cleanup_eligible_at_ms`, and `cleanup_phase` come only from the retention classifier. Missing terminal time, missing owner, malformed metadata, unknown status, unsupported schema, and path-unsafe artifacts must produce `cleanup_phase: retained`, null timestamps where unknown, and a warning.

`source` is a short string naming where the fact came from, such as `projection.run_interrupted`, `projection.parked_issue`, `recovery.cleanup_request`, `ledger.unsupported_version`, or `retention.classifier`. This helps operators and tests verify that the UX is a projection, not a new source of truth.

## Milestones

Milestone 1 verifies and codifies the source inventory. At the end of this milestone, tests construct real ledger records with `record.RecordBody`, fold them through `projection.fold`, and prove which recovery statuses can be produced from current facts. Reserved statuses have string conversion tests only and no end-to-end emission tests. This milestone comes first because it prevents the implementation from creating labels disconnected from durable input.

Milestone 2 adds the typed recovery metadata model, JSON shape, control decoder compatibility, EventHub summary updates, CLI display, and structured logs for currently backed statuses. At the end of this milestone, `ps --json`, `session --json`, human `ps`, and human `session` show recovery state without changing the meaning of the existing live worker `status`. This is independently verifiable through protocol and CLI tests.

Milestone 3 adds read-only retention inventory and offline state status. At the end of this milestone, operators can ask what Scherzo knows about local ledger state and artifact candidates, why each candidate is retained or eligible, and whether pi transcript roots are unavailable. No deletion happens in this milestone. This de-risks artifact paths, schema detection, unknown metadata, and symlink behavior before destructive controls exist.

Milestone 4 adds confirmed cleanup apply and old-state archive, discard, and reinitialize controls only after Milestone 3 tests pass. Cleanup apply deletes only eligible candidates under verified `.scherzo-state` roots and writes redacted tombstones. Old-state archive and discard require unsupported schema markers and `--yes`. This milestone is separate because local file mutation has a different blast radius than read-only recovery visibility.

Milestone 5 updates operator documentation and validates examples. At the end of this milestone, README and `docs/runbooks/workflow-recovery.md` explain emitted and reserved statuses, controls, retention policy, sensitive-data handling, old-state reset path, and irreversible cleanup consequences. Documentation examples match CLI behavior and avoid absolute local path examples.

## Plan of Work

In `src/scherzo/session/event.gleam`, add `RecoveryStatus`, `CleanupPhase`, `RecoveryAction`, and `RecoveryInfo` types. Add string conversion helpers for all canonical statuses, cleanup phases, and safe actions. Add `recovery: Option(RecoveryInfo)` to `SessionSummary` without removing existing fields. Add lifecycle event names for backed recovery and cleanup events: `recovery_detected`, `recovery_interrupted`, `recovery_parked`, `recovery_cleanup`, `old_state_reset_required`, `cleanup_dry_run`, `cleanup_started`, and `cleanup_completed`. Reserved statuses do not need lifecycle event names unless source facts are later added.

Create `src/scherzo/session/recovery.gleam` for pure recovery helpers: `recovery_safe_text`, `safe_actions_for_status`, and mappers that convert verified facts into `RecoveryInfo`. This module must not decide whether to resume, park, retry, skip, or delete work. It only converts inputs such as a projection run, a parked issue, a cleanup request, or an old-state diagnostic into operator metadata.

In `src/scherzo/session/json.gleam`, add `recovery_to_json`, status/action/phase serialization helpers, and the nullable `recovery` field in `summary_to_json`. When recovery is present, include every field documented in `## Interfaces and Dependencies`; use JSON null for absent optional facts. When recovery is absent, serialize `recovery` as JSON null so automation receives a stable shape.

In `src/scherzo/control/protocol.gleam`, update session summary decoding to accept missing `recovery` as `None` and present `recovery` as a typed object. Keep the existing live worker `status` decoder unchanged. Do not bump the protocol version for an additive nullable field unless implementation finds an existing compatibility failure.

In `src/scherzo/orchestrator/event_publisher.gleam` and the daemon recovery integration point in `src/scherzo/orchestrator/daemon.gleam`, publish only currently backed recovery statuses. Build projection tests from real `record.RecordBody` values. If a reserved status still lacks source facts, keep its end-to-end emission test absent and document it as reserved.

In `src/scherzo/ctl.gleam`, update human `ps` output with a compact `RECOVERY` column. Values are `-` when no recovery metadata exists, otherwise the canonical status string. Update human `session` output with a recovery section that shows status, source, bounded reason, safe actions, current pi session id, workflow run id, park reason, park release policy, cleanup phase, and retention deadline when present. JSON mode must pass through the control protocol JSON without dropping `recovery`.

Create `src/scherzo/state/local_artifacts.gleam` for retention and state-maintenance data. It should define artifact metadata, schema status, path safety results, retention decisions, cleanup results, and old-state actions as pure types. The retention classifier accepts metadata, current time, terminal time, recovery status, artifact type, owner id, and configured durations. It returns keep/delete decisions with reasons and warnings. Unknown or malformed metadata is retained.

Add read-only cleanup controls to `src/scherzo/control/command.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/orchestrator/control_command_handler.gleam`, and `src/scherzo/ctl.gleam` first. The default command is dry-run:

    scripts/scherzoctl cleanup
    scripts/scherzoctl cleanup --dry-run
    scripts/scherzoctl cleanup --json --dry-run

Dry run returns `dry_run`, `now_ms`, `would_delete`, `retained`, `warnings`, `roots`, and `transcript_root_status`. It must not delete anything. If no verified pi transcript root exists, report that status and include no transcript deletion candidates.

Only after read-only inventory tests pass, add confirmed cleanup apply:

    scripts/scherzoctl cleanup --yes
    scripts/scherzoctl cleanup --json --yes

Apply must classify all candidates before deleting any file, operate only below `<workspace-root>/.scherzo-state/`, reject path escapes and symlink escapes, write redacted tombstone records below `<workspace-root>/.scherzo-state/cleanup/tombstones/`, delete only eligible candidates, and report per-file warnings. A root containment or schema-marker failure aborts before deletion. A per-file IO failure is best-effort: report the warning and continue only with other already-classified eligible candidates.

Add offline state maintenance to `src/scherzo/ctl.gleam` because unsupported old state may prevent the daemon control server from starting:

    scripts/scherzoctl state status --root <workspace-root> --json
    scripts/scherzoctl state archive-old --root <workspace-root> --yes
    scripts/scherzoctl state discard-old --root <workspace-root> --yes
    scripts/scherzoctl state reinitialize --root <workspace-root> --yes

`state status` is read-only. It derives the ledger paths through `ledger.path_for_workspace_root`, inspects `current.jsonl`, `snapshot.json`, and `ledger/archive/`, and reports `current`, `unsupported`, `corrupt`, `missing`, or `archived`. `archive-old` moves only state classified as `unsupported` into `<workspace-root>/.scherzo-state/archive/old-state/<unique-id>/ledger/`. `discard-old` deletes only unsupported active ledger state after `--yes`; if state is corrupt or malformed rather than unsupported, refuse and tell the operator to inspect manually. `reinitialize` creates the current empty layout expected by this tree: `.scherzo-state/ledger/archive/` and an empty `.scherzo-state/ledger/current.jsonl`. It must not synthesize recovered runs or write fake snapshots.

Add structured logs through existing `src/scherzo/log.gleam` helpers. Required event names are `workflow_recovery_status`, `workflow_cleanup_dry_run`, `workflow_cleanup_completed`, `workflow_state_status`, `workflow_state_archived`, `workflow_state_discarded`, and `workflow_state_reinitialized`. Each log includes issue identifier when available, workflow run id when available, status, source, artifact counts, and bounded redacted reason. No log may include raw transcript text, full prompt text, full Linear comment bodies, tokens, or unredacted tool input/output.

Update `README.md` and create `docs/runbooks/workflow-recovery.md`. The README should introduce the status vocabulary, emitted versus reserved statuses, JSON examples, cleanup dry-run/apply behavior, sensitive-data warning, and old-state controls. The runbook should be task oriented: inspect a recovered or interrupted run, handle parked issues, inspect cleanup eligibility, archive unsupported old state, discard unsupported old state, and reinitialize. Use placeholders such as `<workspace-root>` and `<session-id>`.

## Concrete Steps

1. From the repository root, inspect source control state without changing workspaces:

       jj status --color=never

   Expect only the intended implementation files once work begins. Do not create, switch, forget, finish, push, or otherwise manage jj workspaces as part of this plan.

2. In `test/session_event_test.gleam`, add red tests for every recovery status string, cleanup phase string, and safe action string. Assert exact lower snake case output and parse-back behavior. Include an unknown string case that returns `None` or an error, matching existing style.

3. Run `direnv exec . gleam test`. The new tests should fail to compile because the types and helpers do not exist.

4. In `src/scherzo/session/event.gleam`, add `RecoveryStatus`, `CleanupPhase`, `RecoveryAction`, `RecoveryInfo`, string conversion helpers, and `recovery: Option(RecoveryInfo)` on `SessionSummary`. Update constructor call sites in `src/scherzo/session/hub.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and tests by adding `recovery: None` where no recovery fact exists.

5. Run `direnv exec . gleam test` and expect the status/helper tests to pass, with any remaining failures limited to callers that still need `recovery: None`.

6. In `test/session_event_test.gleam` or `test/state_recovery_test.gleam`, add source-inventory tests that build `record.RunStarted`, `record.RunInterrupted`, `record.IssueParkedV2`, and `record.RunFinished` records, fold them through `projection.fold`, and assert which facts are available for `interrupted`, `parked`, and terminal cleanup inventory. Add assertions that no previous pi session id, workflow step id, blocked fact, inspection hold, or drift fact exists in those sources.

7. In `src/scherzo/session/recovery.gleam`, implement `recovery_safe_text` using `log.redact` and `log.truncate(value, 200)`. Implement pure mappers for `projection.RunInterrupted`, startup `projection.RunRunning` interruption, `projection.ParkedIssue`, `recovery.CleanupRequest`, and old-state diagnostics. Reserved statuses must have string helpers but no mapper from current facts.

8. Run `direnv exec . gleam test` and expect the source-inventory tests to pass. If a reserved source fact is discovered, update this plan before mapping it.

9. In `test/control_protocol_test.gleam`, add JSON serialization and decoding tests. A running session with `recovery.status: interrupted` must still have live `status: running`. A parked issue summary must include `recovery.status: parked`, `park_reason`, and `park_release_policy`. A summary without recovery must include `recovery: null` or decode missing recovery as `None`.

10. In `src/scherzo/session/json.gleam` and `src/scherzo/control/protocol.gleam`, implement recovery JSON serialization and tolerant decoding. Keep `status` and `exit_reason` behavior unchanged.

11. In `test/session_event_test.gleam`, add event-name tests for backed lifecycle events. Then add the event names in `src/scherzo/session/event.gleam`.

12. In `test/orchestrator_daemon_session_event_test.gleam`, add tests that feed real projection/recovery inputs far enough through existing recovery or publisher code to expose `interrupted` and `parked` metadata. These tests must not assert new scheduling behavior.

13. In `src/scherzo/orchestrator/event_publisher.gleam` and `src/scherzo/orchestrator/daemon.gleam`, wire the pure recovery mappers so EventHub summaries receive backed recovery metadata and lifecycle events.

14. In `test/ctl_test.gleam`, add human-output tests for `ps` showing `RECOVERY`, `interrupted`, `parked`, `cleanup`, and `-`. Add `session` tests for status, source, bounded reason, safe actions, current pi session id, workflow run id, park reason, cleanup phase, and retention deadline.

15. In `src/scherzo/ctl.gleam`, implement the human output changes. Preserve JSON passthrough. If row width is tight, truncate display name or issue title before dropping recovery information.

16. Commit after steps 2 through 15 once `direnv exec . gleam test` and `direnv exec . gleam format --check src test` pass. Suggested commit message: `Expose backed workflow recovery status`.

17. In `test/state_local_artifacts_test.gleam`, add retention classifier tests before implementation. Cases must include terminal workflow artifact older than 30 days eligible, terminal pi transcript older than 14 days eligible when a verified transcript root exists, cleanup tombstone older than 30 days eligible, active artifact retained, interrupted retained, parked retained, old-state-reset-required retained, unknown status retained, missing terminal time retained, missing owner retained, malformed metadata retained, and unsupported schema retained.

18. In `src/scherzo/state/local_artifacts.gleam`, implement pure artifact metadata and classifier types. Default durations are 30 days for terminal workflow artifacts, 14 days for pi transcripts, and 30 days for cleanup tombstones. Hard-code these defaults in this module unless an existing config surface is discovered; do not add a large config refactor.

19. In the same test file, add path-safety tests with temporary fixture roots. Include decoy files outside `.scherzo-state`, a path containing parent traversal, a symlink escape, a missing root, and a root without an ownership or schema marker. Assert all are rejected or retained with warnings and no deletion decision.

20. In `src/scherzo/state/local_artifacts.gleam`, implement path normalization and safety helpers. Resolve `<workspace-root>`, derive state roots by joining `.scherzo-state`, require candidates to remain under that state root, refuse empty or filesystem-root workspace roots, and refuse symlink escapes. Add a small FFI helper only if current Gleam file APIs cannot distinguish symlinks safely.

21. In `test/ctl_test.gleam`, `test/control_command_test.gleam`, and `test/control_protocol_test.gleam`, add cleanup dry-run parser and JSON tests. Assert plain `cleanup` is a dry run, `cleanup --dry-run` is a dry run, JSON output includes roots and warnings, and no deletion request is produced without `--yes`.

22. In `src/scherzo/control/command.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/orchestrator/control_command_handler.gleam`, and `src/scherzo/ctl.gleam`, implement cleanup dry run using the inventory and classifier. If pi transcript root is unavailable, include a warning and no transcript candidates.

23. In `test/orchestrator_daemon_control_test.gleam`, add cleanup dry-run integration tests with fake artifact metadata. Assert dry run returns candidates, retains ambiguous artifacts, and deletes nothing.

24. Commit after steps 17 through 23 once formatting and tests pass. Suggested commit message: `Add read-only recovery artifact inventory`.

25. Add red tests for cleanup apply edge cases: unknown metadata retained, malformed metadata retained, missing terminal time retained, symlink/path escape rejected before deletion, retained artifacts never removed beside eligible artifacts, already-deleted eligible artifact reported as warning, and partial deletion failure reported without touching retained artifacts.

26. Implement `cleanup --yes` apply. It must classify all candidates first, abort on root safety errors, write a tombstone record before each successful deletion or immediately after if the filesystem API requires that ordering, delete only eligible candidates, and report `deleted`, `retained`, and `warnings`.

27. Add red tests for offline `state status`, `archive-old`, `discard-old`, and `reinitialize` parser behavior and local fixture behavior. Use dummy files under test-created temporary directories. Assert mutations refuse to run without `--yes`.

28. Implement offline state commands in `src/scherzo/ctl.gleam` and helpers in `src/scherzo/state/local_artifacts.gleam`. Unsupported means a readable schema marker has a version unsupported by this tree. Corrupt or malformed means unreadable or invalid shape and must be retained for manual inspection. Archive and discard operate only on unsupported active ledger state. Reinitialize creates the empty current layout.

29. Add log tests in `test/log_test.gleam` or existing orchestrator tests. Use sensitive sample strings and assert log events include status and counts but do not include raw prompt text, raw tool input, raw Linear comment body, token-like keys, or unredacted secret values.

30. Implement the structured logs at recovery projection, cleanup dry run/apply, and state maintenance call sites.

31. Commit after steps 25 through 30 once formatting and tests pass. Suggested commit message: `Add safe cleanup and old-state maintenance controls`.

32. Update `README.md` with sections named “Workflow recovery operator status,” “Local retention and cleanup,” and “Unsupported old local state.” Create `docs/runbooks/workflow-recovery.md` with procedures and “do not do this” guidance.

33. Run the documentation path check from the repository root:

       python3 - <<'PY'
       from pathlib import Path
       parts = ["Users/", "home/", "private/", "var/folders/"]
       forbidden = [chr(47) + part for part in parts]
       files = [Path("README.md"), Path("docs/runbooks/workflow-recovery.md")]
       bad = []
       for file in files:
           text = file.read_text()
           for marker in forbidden:
               if marker in text:
                   bad.append((str(file), marker))
       if bad:
           raise SystemExit(f"forbidden local path examples: {bad}")
       PY

   Expect no output and exit status zero.

34. Run final validation from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       scripts/scherzo-execplan validate docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md

   Expect all commands to exit zero. The exact test count may change, but the final test summary must report all tests passing and the plan validator must report `VALIDATION=ok`.

35. Update this ExecPlan’s Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections with implementation results and any deviations. Commit the documentation and validation work. Suggested commit message: `Document workflow recovery operator runbook`.

## Testing and Falsifiability

The feature is falsified if an operator cannot distinguish live worker status from currently backed recovery status in both human and JSON output, if a reserved status is emitted without a source fact, if cleanup can delete ambiguous recovery artifacts by default, if old unsupported state is silently ignored, or if sensitive transcript or prompt content appears in summaries or logs.

Status vocabulary tests live in `test/session_event_test.gleam`. They cover every canonical status string: `recovered`, `interrupted`, `resumed`, `inspection_needed`, `blocked`, `parked`, `cleanup`, `drift_detected`, and `old_state_reset_required`. They also cover cleanup phases and safe action strings. Unknown strings must be rejected rather than mapped to a misleading default.

Source projection tests live in `test/state_recovery_test.gleam` or `test/orchestrator_daemon_session_event_test.gleam`. They build real `record.RecordBody` values, fold them through `projection.fold`, run the existing recovery path where practical, and assert that `interrupted` and `parked` come from real durable facts. Tests for `resumed`, `inspection_needed`, `blocked`, and `drift_detected` should assert only string conversion unless source facts are added in a later decision.

JSON tests live in `test/control_protocol_test.gleam`. Construct a `SessionSummary` with live worker status `Running` and recovery status `Interrupted`; assert JSON contains `status: running` and `recovery.status: interrupted`. Construct a parked summary; assert `recovery.status: parked`, `park_reason`, and `park_release_policy`. Decode fixtures with missing recovery and present recovery.

CLI tests live in `test/ctl_test.gleam`. For `ps`, assert the human table includes a `RECOVERY` header and shows backed statuses plus `-` for no recovery metadata. For `session`, assert detailed output includes status, source, reason, safe actions, workflow run id, park details, cleanup phase, and retention deadline when present. Existing truncation expectations must still hold.

Cleanup tests live in `test/state_local_artifacts_test.gleam`, `test/control_command_test.gleam`, `test/control_protocol_test.gleam`, and `test/orchestrator_daemon_control_test.gleam`. They must cover the safe cases and dangerous cases: unknown metadata, malformed metadata, unknown recovery status, missing terminal time, missing owner, missing files, symlinked paths, path escapes, artifacts outside `.scherzo-state`, already-deleted artifacts, partial deletion failure, and deletion failure ordering. Unknown or unsafe artifacts are retained with warnings.

Offline state tests use test-created temporary directories containing dummy ledger and snapshot files. They assert `state status` reports current, unsupported, corrupt, missing, and archived state correctly; `archive-old --yes` moves unsupported state out of active paths; `discard-old --yes` removes only unsupported active state; and `reinitialize --yes` creates the current empty layout without recovered runs.

Redaction tests feed the same sensitive sample through session recovery JSON, CLI output, cleanup JSON, state-status output, and logs. They assert the shared 200-character redacted value is used and that raw prompt text, tool input, Linear comment body, token-like fields, and secret values do not appear.

Documentation validation is part of the test plan. README and the runbook must include all canonical statuses, explain emitted versus reserved status behavior, document cleanup dry-run/apply commands, document old-state archive/discard/reinitialize commands, and warn about sensitive data. The path check in the concrete steps must pass.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    scripts/scherzo-execplan validate docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md

All commands must exit zero. If `.envrc` is blocked, inspect it, run `direnv allow .`, and retry. Do not treat an unapproved `.envrc` as a failing test.

Acceptance is met when the following behavior is observable:

- A session summary with recovery metadata serializes a `recovery` object while preserving existing live worker `status` and `exit_reason` fields.
- `scripts/scherzoctl ps` human output shows a `RECOVERY` column, and `scripts/scherzoctl ps --json` includes `recovery.status` for backed statuses.
- `scripts/scherzoctl session <session-id>` explains `recovered`, `interrupted`, `parked`, and `cleanup` when their source facts exist, with safe actions and bounded reasons.
- `resumed`, `inspection_needed`, `blocked`, and `drift_detected` are documented as reserved and are not emitted from real recovery projection unless the source inventory is updated with concrete facts and tests.
- `scripts/scherzoctl cleanup` dry run lists eligible terminal artifacts and retained recovery-hold or ambiguous artifacts without deleting anything.
- `scripts/scherzoctl cleanup --yes` deletes only eligible artifacts under verified `.scherzo-state` roots, writes redacted tombstones, and reports warnings for failures.
- Offline state controls let an operator inspect, archive, discard, and reinitialize unsupported old local state without a running daemon.
- Daemon logs include structured recovery and cleanup events without raw transcript, prompt, tool, token, or full Linear comment content.
- README and the runbook explain statuses, controls, retention durations, sensitive-data handling, irreversible deletion consequences, and the old-state reset path.

## Rollout, Recovery, and Idempotence

The rollout is additive. Adding `recovery` to summaries does not change the existing `status` field. If a local control client does not know the new field, it can continue using existing status and exit reason. If a new client talks to an older daemon, missing `recovery` decodes as `None` and displays as `-`.

Cleanup dry run is idempotent and read-only. Cleanup apply is best-effort only for already-classified eligible artifacts. Repeating apply should report already-deleted artifacts as warnings or already cleaned, not as permission to touch retained artifacts. Deletion is not generally reversible; the rollback artifact is the tombstone, which records identifiers, display paths, reasons, and deletion result but not content. Operators must treat `cleanup --yes` as irreversible.

Old-state archive is reversible by design. The archive lives below `<workspace-root>/.scherzo-state/archive/old-state/<unique-id>/ledger/`. To restore manually, stop Scherzo, inspect `state status`, move the archived ledger directory back to `<workspace-root>/.scherzo-state/ledger/`, and run `state status` again. Old-state discard is not reversible unless the operator archived the state separately first; human output must say this before accepting `--yes`. Reinitialize is idempotent after archive or discard because it creates an empty current layout.

If the code change must be backed out, leave operator-created archives and tombstones untouched. Reverting display fields and CLI commands should not mutate local state. If cleanup deleted files incorrectly, restore from backups if available and use tombstone records to identify what was deleted; if no backup exists, the content is irrecoverable.

## Artifacts and Notes

Example `ps --json` shape for an interrupted run:

    {
      "session_id": "workflow:LIV-58:attempt-2",
      "display_name": "LIV-58 workflow:execplan",
      "issue_identifier": "LIV-58",
      "status": "running",
      "exit_reason": null,
      "pi_session_id": "pi-current",
      "recovery": {
        "status": "interrupted",
        "source": "projection.run_interrupted",
        "message": "daemon_restart",
        "safe_actions": ["inspect", "view_events", "retry", "park"],
        "workflow_run_id": "run-2",
        "workflow_step_id": null,
        "current_pi_session_id": "pi-current",
        "previous_pi_session_id": null,
        "park_reason": null,
        "park_release_policy": null,
        "parked_at_ms": null,
        "drift_kind": null,
        "retention_until_ms": null,
        "cleanup_eligible_at_ms": null,
        "cleanup_phase": null
      }
    }

Example cleanup dry-run JSON shape:

    {
      "dry_run": true,
      "now_ms": 1770000000000,
      "roots": ["<workspace-root>/.scherzo-state/ledger/archive"],
      "transcript_root_status": "unavailable",
      "would_delete": [],
      "deleted": [],
      "retained": [
        {
          "artifact_type": "workflow_artifact",
          "id": "run-interrupted",
          "recovery_status": "interrupted",
          "cleanup_phase": "retained",
          "reason": "interrupted recovery state requires operator inspection",
          "retention_until_ms": null,
          "path": "<workspace-root>/.scherzo-state/ledger/current.jsonl"
        }
      ],
      "warnings": ["pi transcript root is not available in this tree"]
    }

Example human guidance for unsupported old state:

    old local state requires reset
    reason: unsupported ledger schema version 0
    safe actions:
      1. stop Scherzo and verify no daemon is using <workspace-root>
      2. run scripts/scherzoctl state status --root <workspace-root> --json
      3. archive with scripts/scherzoctl state archive-old --root <workspace-root> --yes
      4. reinitialize with scripts/scherzoctl state reinitialize --root <workspace-root> --yes

## Interfaces and Dependencies

Use existing Gleam modules and tests. Do not add external package dependencies unless implementation discovers a hard requirement. Prefer internal modules such as `src/scherzo/session/recovery.gleam` and `src/scherzo/state/local_artifacts.gleam` over spreading rules across CLI, daemon, and ledger code.

The final session summary interface should keep every existing field and add only recovery metadata:

    pub type SessionSummary {
      SessionSummary(
        session_id: String,
        display_name: String,
        issue_id: String,
        issue_identifier: String,
        issue_title: String,
        workspace_path: String,
        pi_session_id: Option(String),
        status: SessionStatus,
        recovery: Option(RecoveryInfo),
        current_turn: Int,
        started_at_ms: Int,
        last_event_at_ms: Int,
        token_totals: session_tokens.TokenTotals,
      )
    }

The recovery metadata should be equivalent to:

    pub type RecoveryInfo {
      RecoveryInfo(
        status: RecoveryStatus,
        source: String,
        message: Option(String),
        safe_actions: List(RecoveryAction),
        workflow_run_id: Option(String),
        workflow_step_id: Option(String),
        current_pi_session_id: Option(String),
        previous_pi_session_id: Option(String),
        park_reason: Option(String),
        park_release_policy: Option(String),
        parked_at_ms: Option(Int),
        drift_kind: Option(String),
        retention_until_ms: Option(Int),
        cleanup_eligible_at_ms: Option(Int),
        cleanup_phase: Option(CleanupPhase),
      )
    }

The cleanup response should be equivalent to:

    pub type CleanupResult {
      CleanupResult(
        dry_run: Bool,
        now_ms: Int,
        roots: List(String),
        transcript_root_status: String,
        would_delete: List(LocalArtifactDecision),
        deleted: List(LocalArtifactDecision),
        retained: List(LocalArtifactDecision),
        warnings: List(String),
      )
    }

The local artifact decision should be equivalent to:

    pub type LocalArtifactDecision {
      LocalArtifactDecision(
        artifact_type: String,
        id: String,
        recovery_status: Option(RecoveryStatus),
        cleanup_phase: CleanupPhase,
        reason: String,
        retention_until_ms: Option(Int),
        display_path: String,
      )
    }

The old-state maintenance commands depend only on local file operations, the workspace root argument, `ledger.path_for_workspace_root`, `record.schema_version`, and snapshot schema helpers added in this plan. They must not require Linear, pi, or a running daemon.

## Open Questions and Clarifications Needed

None.
