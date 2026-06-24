# Scherzo architecture and change guide

Audience: agents and reviewers making focused changes to Scherzo. This is a
present-tense map of the checked-in implementation, not a replacement for source
or tests.

Use this file to answer:

- Where do I change X?
- Which invariants must I preserve?
- Which tests or validation commands should I run?

Historical ExecPlans under `docs/plans/` explain why major pieces exist. For
simple changes, start here and then inspect only the linked plan/runbook when a
section points you there.

## System overview

Runtime flow:

```text
Tracker tasks (Linear issues in the production adapter today)
  -> src/scherzo/tracker/linear_adapter.gleam and legacy Linear tracker client
  -> src/scherzo/runtime_bundle.gleam routing by workflow label
  -> YAML workflow DAG in .scherzo/workflows/ or examples/workflows/
  -> src/scherzo/orchestrator/daemon.gleam poll/retry/control actor
  -> src/scherzo/workflow_run.gleam step scheduler/executor
  -> command steps via src/scherzo/command_step.gleam
     or agent steps via src/scherzo/agent/* + src/scherzo/pi/*
  -> session events in src/scherzo/session/hub.gleam
  -> task updates/tracker comments, currently Linear comments
  -> durable state under workspace.root/.scherzo-state/
```

Startup/recovery flow:

```text
scherzo main -> orchestrator service -> runtime_bundle.load
  -> ledger.replay + projection fold
  -> refresh ledger-known tasks, currently Linear issues
  -> state/recovery.plan + workflow candidate finalization
  -> append recovery records with fsync
  -> start EventHub, control server, effect runner, poll scheduler
```

Operator-control flow:

```text
scripts/scherzoctl / scherzo ctl
  -> src/scherzo/ctl.gleam
  -> src/scherzo/control/client.gleam
  -> loopback JSON protocol + control token
  -> src/scherzo/control/server.gleam
  -> EventHub queries or daemon ApplyOperatorCommand
  -> worker command subjects when targeting active step sessions
```

## Current module map

| Area | Main files | Notes |
| --- | --- | --- |
| CLI/service startup | `src/scherzo/main.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/runtime_bundle.gleam` | CLI modes, doctor, once/daemon startup, config/workflow bundle loading. |
| Config types/resolution | `src/scherzo/config.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/model_config.gleam` | YAML orchestrator config is resolved to typed `EffectiveConfig`/`OrchestratorConfig`; runtime `.md` workflows are no longer supported. |
| Task/tracker adapter/task updates | `src/scherzo/task.gleam`, `src/scherzo/tracker/adapter.gleam`, `src/scherzo/tracker/linear_adapter.gleam`, `src/scherzo/tracker.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/linear_*` | Backend-neutral task model and capability contract; see the normative [Tracker Adapter Specification](specs/TRACKER_ADAPTER_SPEC.md). Linear GraphQL transport, issue compatibility normalization, board setup checks, comments/state changes, attachments. |
| Workflow DAGs | `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_fingerprint.gleam`, `src/scherzo/workspace_run.gleam` | Parse/validate YAML DAGs, schedule ready steps, prepare step workspaces, execute agent/command steps, checkpoint durable facts. |
| Orchestrator | `src/scherzo/orchestrator/daemon.gleam`, `core.gleam`, `state.gleam`, `effect_runner.gleam`, `worker_registry.gleam`, `workflow_reloader.gleam`, `control_command_handler.gleam` | Daemon actor owns polling, retry timers, claims, running sessions, reload, side-effect queue, and local controls. `core.gleam` is the pure policy layer. |
| Agent/pi execution | `src/scherzo/agent/run_attempt.gleam`, `turn_loop.gleam`, `operator_control.gleam`, `worker_command.gleam`, `src/scherzo/pi/client.gleam`, `protocol.gleam`, `command.gleam` | Launch pi RPC, send prompts/abort/UI responses, stream turn records, record token/session observations. |
| Command execution | `src/scherzo/command_step.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/template.gleam` | Run shell command steps in prepared workspaces, cap/redact artifacts, expose `steps.*` fields to downstream prompts. |
| Durable state/recovery | `src/scherzo/state/record.gleam`, `ledger.gleam`, `projection.gleam`, `recovery.gleam`, `artifact_store.gleam`, `local_artifacts.gleam`, `src/scherzo/workflow_checkpoint.gleam` | Append-only ledger, projection snapshots, port-backed artifact storage with a default filesystem implementation, startup recovery, cleanup/retention. |
| Control API/CLI | `src/scherzo/control/*`, `src/scherzo/ctl.gleam`, `scripts/scherzoctl`, `src/scherzo/session/*`, `src/scherzo/terminal/*` | Local loopback control protocol, EventHub queries, attach rendering, mutating operator commands, offline state commands. |
| FFI | `src/*_ffi.erl` plus `@external` declarations in Gleam files | Erlang boundaries for ports, TCP, filesystem sync/locking, signals, terminal, hashing, paths/env, shutdown, time. See `docs/ffi.md` for the contract every FFI export and wrapper must preserve. |
| Examples/docs | `.scherzo/scherzo.yaml`, `.scherzo/workflows/*.yaml`, `examples/`, `README.md`, `docs/specs/`, `docs/runbooks/`, `docs/plans/` | Dogfood workflows and reusable examples must track user-visible config/schema changes. |

## Import-boundary guardrails

The deterministic test suite includes `test/architecture_guardrail_test.gleam`,
which scans `src/**/*.gleam` imports and fails when dependencies cross selected
architecture boundaries in the wrong direction. Failure messages name the
importing file, forbidden import, rule id, and remediation guidance.

Current guarded boundaries:

- `src/scherzo/state/**` must not import `scherzo/orchestrator/**`.
- `src/scherzo/tracker/**` must not import `scherzo/linear*` unless the file is
  an explicit Linear adapter seam.
- `src/scherzo/orchestrator/**` subsystem modules must not import
  `scherzo/orchestrator/daemon`; startup edges that launch the daemon must be
  allowlisted.
- Workflow execution/core modules must not import `scherzo/control/server`,
  `scherzo/control/client`, or `scherzo/orchestrator/daemon`.
- Imports of `scherzo/tracker/adapter_legacy` are compatibility seams and must
  have an explicit allowlist entry.

When a new exception is intentional, prefer moving shared types or pure helpers
to a neutral module first. If that is out of scope, add the narrowest possible
entry to `boundary_allowlist()` in `test/architecture_guardrail_test.gleam` with
the exact importing file, exact imported module, matching rule id, and a rationale
that explains the migration or compatibility seam. The guardrail also fails stale
or rationale-free allowlist entries, so remove entries as soon as the dependency
is eliminated.

## Core invariants

### Runtime config and routing

- Runtime entrypoints load YAML orchestrator config (`.yaml`/`.yml`) through
  `runtime_bundle.load`; Markdown is prompt-template content only.
- Default config discovery order is `.scherzo/scherzo.yaml`,
  `.scherzo/scherzo.yml`, `scherzo.yaml`, `scherzo.yml`.
- Relative config, workflow, prompt, workspace, and driver-related paths are resolved from
  the config/workflow file directory as implemented by `config.gleam`,
  `runtime_bundle.gleam`, and `path.gleam`.
- Workflow routing uses top-level `workflows` plus normalized labels with
  `task_routing.labels.prefix` (default `workflow:`). With
  `task_routing.labels.require_exactly_one: true`, missing/multiple labels are
  route errors. Contract enforcement is separate in `workflow_policy.gleam`.
- Top-level `workflows` keys must match the loaded DAG `id`.

### Workflow DAGs and execution

- DAG files are `version: 1` and parse in `workflow_dag.gleam`.
- Step ids are unique, lowercase/underscore style, and dependencies must exist.
- Cycles are rejected.
- More than one terminal sink is rejected; real workflows should have one final
  sink step.
- `run_in: { name, from }` may only derive from a workspace lane produced by a
  transitive dependency.
- Prompt file paths are relative to the workflow YAML file and may not escape
  that directory.
- Command steps cannot set pi model/thinking fields.
- `workflow_scheduler.ready_steps` respects workflow `concurrency`, dependency
  completion, fatal failures/cancellation, and same-workspace serialization.
- A failed step with `on_failure: continue` records a terminal
  `failed_continued` artifact and can unblock downstream dependencies.
- Workflow runner executes a ready batch concurrently, then applies artifacts in
  DAG order so downstream template rendering is deterministic.
- Workspaces are prepared by orchestrator-defined workspace drivers. `workspace.driver`
  selects either a built-in driver (`noop` or `jj`) or a named entry under
  `workspace.drivers`; custom entries name a trusted command, and driver capability
  names are discovered from `describe --json` when they are not built in. A workflow may
  select one trusted driver with `workspace.driver` and may require capabilities with
  `workspace.requires`; omitted selectors use the orchestrator default driver.
  Scherzo validates required capabilities against the selected driver before dispatch.
  `docs/specs/WORKSPACE_DRIVER_SPEC.md` is the normative driver contract. Direct
  `workspace.hooks`, `workspace.profiles`, and driver-local lifecycle selection are
  unsupported legacy shapes rejected during config loading, not current architecture
  invariants.

### Orchestrator dispatch

- `orchestrator/daemon.gleam` is the side-effecting actor. Keep pure decisions
  in `orchestrator/core.gleam` where possible.
- Candidate dispatch preconditions include required task fields, active and
  non-terminal state, no running/claimed worker for the task, not parked,
  blockers satisfied, workflow-policy satisfied, global concurrency, and
  per-state concurrency.
- `agents.concurrency: 0` pauses new dispatch while daemon reload and
  reconciliation remain alive.
- Dispatch validates the task with a fresh tracker read before claim/task update.
- Claims, task-update comments, invalid-workflow reports, tracker refreshes, and
  orchestrator cleanup effects run through `effect_runner.gleam`; workflow-run
  cleanup is part of
  `workflow_run.gleam` dependencies. Some compatibility paths still use Linear
  names; see `docs/runbooks/tracker-adapters.md` for the current coupling map.
- The daemon owns session registration in the EventHub and command-subject
  routing for active workflow step sessions.
- Retry counters and max-session parking must remain durable through the ledger;
  do not introduce in-memory-only retry/session facts.

### Ledger, projection, artifacts, and recovery

- The current ledger record schema is `record.schema_version = 2`.
- Ledger layout is:

  ```text
  workspace.root/.scherzo-state/ledger/current.jsonl
  workspace.root/.scherzo-state/ledger/snapshot.json
  workspace.root/.scherzo-state/ledger/archive/segment-<n>.jsonl
  ```

- `current.jsonl` is append-only JSON Lines. Replay tolerates at most one
  truncated trailing line and rejects malformed middle records or unsupported
  schema versions.
- Projection snapshots use the same schema version as ledger records.
- `state/artifact_store.gleam` is the artifact-store abstraction seam. The
  default implementation is a filesystem store, so durable step artifacts still
  live under `.scherzo-state/artifacts/runs/...`; step-finished ledger records
  reference the artifact ref and sha256.
- Artifact locations now distinguish durable `ref` from operator-facing `uri`,
  `display_path`, and optional `local_path`. Core reads and inline structured
  output access must work by ref even when `local_path` is absent.
- Ledger data is operational state only: identifiers, statuses, counters,
  bounded excerpts, bounded/redacted outbox payloads, artifact refs, and
  recovery facts. Do not store API keys, raw pi JSON, full prompts, or full
  Linear comment bodies.
- Startup recovery replays ledger state, refreshes known task ids in chunks of
  50, plans retry/park/outbox/cleanup recovery, finalizes active workflow
  candidates, and appends recovery records with fsync before polling. Current
  durable records keep issue-shaped fields for Linear compatibility.
- Live Erlang ports and live pi streams do not survive restart. A current pi
  session id after restart is not proof that the previous process resumed.
- Interrupted command steps are unsafe to retry automatically. Interrupted agent
  steps may be rerun, or continued only when step-scoped session persistence is
  enabled and the exact workspace/session facts validate.
- Workflow recovery parks rather than silently resumes when the task is
  unavailable, the selected workflow is unavailable, workflow/task fingerprint
  drift is detected, artifact recovery fails, or workspace safety checks fail.

### Control protocol and operator commands

- Local control protocol version is `1` in `control/protocol.gleam`.
- The daemon binds the control server to loopback only, generates a token, and
  writes `workspace.root/.scherzo-state/control.json` with private permissions.
  Never log the token.
- Local `control.json` is only for loopback `scherzoctl` discovery and
  per-process local auth. It is not the durable daemon identity and it is not a
  remote UI/server credential.
- The durable daemon identity primitive stores identity in
  `workspace.root/.scherzo-state/daemon_identity.json`. It persists only a
  stable `daemon_id`; each helper load still generates a fresh in-memory
  `boot_id`.
- `ui_server` config is disabled by default. Operators pair a daemon with
  `scherzo connect`, which exchanges a one-time pairing token for a durable
  daemon credential and stores that credential outside project YAML. When
  enabled the daemon loads the durable daemon identity plus stored credential,
  starts one outbound UI client, sends `daemon_hello`, `heartbeat`, and
  `daemon_state` snapshots, retries temporary outages without blocking local
  control, and stops retrying when the credential or daemon identity is revoked
  until the operator pairs again. Pairing and later heartbeat/state frames may
  include the non-secret `daemonLabel` from `scherzo connect --name` or
  `ui_server.daemon_label`, with the CLI flag taking precedence during pairing.
  Each `daemon_hello`, `heartbeat`, and `daemon_state` frame includes a stable
  `state` object:

  ```json
  {
    "schemaVersion": 1,
    "host": "worker-hostname-or-unknown",
    "version": "scherzo revision=... date=... dirty=...",
    "daemonLabel": "optional friendly label",
    "agentSlots": { "capacity": 2, "active": 1, "used": 1, "known": true }
  }
  ```

  `agentSlots.capacity` mirrors `agents.concurrency`; `active` and `used` are
  the latest successful occupied agent/worker slot count when `known` is true,
  derived from the daemon operational metrics that also describe concurrency
  pressure. Session snapshots are published separately in `daemon_state` frames
  and are not counted as occupied slots, so a workflow parent session plus one
  active child step still reports one occupied slot. If the occupancy lookup is
  temporarily unavailable while building hello or state frames, `known` is
  false and the daemon leaves `active`/`used` at `0` rather than presenting
  those counts as authoritative. Heartbeats reuse the cached slot snapshot from
  connect/state frames so heartbeat delivery does not block on another
  occupancy lookup. Heartbeats also include an `event` object
  with lifecycle kind, heartbeat type, and `daemon heartbeat` message, which
  lets the UI sidecar maintain `lastEvent` without inventing daemon activity.
  Project config keeps only the non-secret UI
  base URL, `credential_ref`, and optional `daemon_label`; it does not read or
  reuse `control.json` or `SCHERZO_CONTROL_FILE` for remote auth.
- When `ui_server.command_bridge_enabled` is true, UI `server_command` frames
  are correlated by `serverCommandId` and answered with `command_result` frames.
  The bridge rejects malformed commands, daemon/boot mismatches, disabled bridge
  use, and overloads with explicit command-result statuses. Supported operator
  commands use the shared `control/command.gleam` codec; pause, resume, and
  reload produce applied/not-allowed/rejected results and are followed by a fresh
  `daemon_state` snapshot when applied.
- This lifecycle slice does not add browser UI, workflow-helper/schema changes,
  provider-live or provider-cache behavior changes, or token-accounting changes.
- `scherzoctl` discovers the control file from `--control-file`,
  `SCHERZO_CONTROL_FILE`, or the repository default path.
- The local control server waits `control.command_timeout` for mutating
  operator command results before returning `command_timeout`; the default is
  `60s`.
- Non-streaming control commands support JSON output. `attach`/`events` can
  render compact, pretty, verbose, or JSON stream output.
- Mutating local commands (`pause`, `resume`, `reload`, `retry`, `park`,
  `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`) enter the daemon
  as `OperatorCommand` values and must return explicit `CommandResult` statuses.
- Remote task comments are not an operator command transport. The former Linear
  comment command path has been removed; `remote_commands` and legacy
  `linear_commands` config sections are rejected. Use `scherzoctl` for operator
  commands.

### FFI boundary

FFI modules are small and purpose-specific:

| FFI file | Used by | Responsibility |
| --- | --- | --- |
| `scherzo_port_ffi.erl` | `port.gleam`, pi/command steps | Launch shell/argv processes, send/read lines, collect stderr diagnostics, terminate process trees. |
| `scherzo_control_ffi.erl` | `control/*`, `ctl.gleam` | Loopback TCP, line protocol, token generation, chmod, dynamic JSON helper, env lookup. |
| `scherzo_state_ffi.erl` | `state/ledger.gleam`, `state/local_artifacts.gleam` | Append/fsync JSONL, fold lines, ledger lock, system time. |
| `scherzo_artifact_store_ffi.erl` | `state/artifact_store.gleam` | Atomic artifact writes. |
| `scherzo_lock_ffi.erl` | `instance_lock.gleam` | Single-instance lock acquire/release. |
| `scherzo_signal_ffi.erl`, `scherzo_lifecycle_ffi.erl` | `signal.gleam`, `lifecycle.gleam` | SIGTERM integration and safe VM shutdown. |
| `scherzo_config_ffi.erl` | `config.gleam`, `path.gleam` | Env, home/tmp, dirname, abs path helpers. |
| `scherzo_time_ffi.erl` | command/pi/daemon timing | Monotonic milliseconds for durations; Unix epoch milliseconds for persisted/display timestamps. |
| `scherzo_hash_ffi.erl` | `hash.gleam` | SHA-256 hex. |
| `scherzo_terminal_ffi.erl` | `terminal/style.gleam` | Color support and terminal width. |
| `scherzo_redaction_ffi.erl` | `session/redaction.gleam` | Fail-closed raw JSON redaction. |
| `scherzo_main_ffi.erl`, `scherzo_process_ext_ffi.erl` | `main.gleam`, `process_ext.gleam` | argv/halt and trap-exit support. |

When changing FFI, preserve loopback/path/process safety at the Gleam boundary
and add tests that exercise the public Gleam wrapper, not just the Erlang helper.

## Schema-change checklist

Use this checklist for ledger, projection snapshot, artifact, control protocol,
config, workflow YAML, tracker task shape, or Linear payload shape changes. Breaking
changes must also follow the upgrade policy in [docs/runbooks/upgrades.md](runbooks/upgrades.md): detect old shapes at safe boundaries, fail fast with stable diagnostics, add `doctor` coverage when static, and require explicit operator actions for unsupported durable state.

1. Identify the owning module:
   - Ledger records: `src/scherzo/state/record.gleam`.
   - Projection/snapshot: `src/scherzo/state/projection.gleam`.
   - Recovery planner: `src/scherzo/state/recovery.gleam`.
   - Workflow checkpoints: `src/scherzo/workflow_checkpoint.gleam`.
   - Step artifacts: `src/scherzo/step_artifact.gleam` and
     `src/scherzo/state/artifact_store.gleam`.
   - Control protocol: `src/scherzo/control/protocol.gleam` and
     `src/scherzo/control/client.gleam`/`server.gleam`/`ctl.gleam`.
   - Config/workflow YAML: `src/scherzo/config.gleam`,
     `src/scherzo/config/types.gleam`, `src/scherzo/workflow_dag.gleam`,
     `src/scherzo/runtime_bundle.gleam`.
2. Update encoders, decoders, pure projection/folding logic, and all writers in
   the same change. Do not add a record that is never projected or a projection
   field that is never written.
3. Decide compatibility explicitly. Unsupported local state should fail clearly
   and be handled by `scherzoctl state ...` or documented reset steps; do not
   silently reinterpret old records. Link diagnostics to the relevant upgrade
   runbook and include affected paths/fields when safe.
4. Keep persisted strings bounded/redacted. Add or adjust redaction tests when a
   new persisted field may contain user, Linear, command, or pi text.
5. Update recovery behavior and operator surface if the new schema affects
   retry, park, outbox, command receipt, workflow resumption, cleanup, or
   control output.
6. Update README/examples/runbooks when users or operators see the schema.
7. Run the targeted tests listed below and consider SelfCI for broad schema
   changes.

## Validation commands

Use direnv-backed commands from the repository root.

| Command | When to run |
| --- | --- |
| `direnv exec . gleam test` | Default deterministic unit suite; run for normal source changes and before review when cheap. |
| `direnv exec . gleam format --check src test` | Source/test formatting check. Docs-only changes normally do not need it, but SelfCI runs it. |
| `direnv exec . scherzo-test-unit` | Explicit alias for the default unit suite. |
| `direnv exec . scherzo-test-contract` | Shell-heavy helper-script, workflow, renderer, daemon/service, port/process, pi-client, and workspace-driver contract coverage excluded from the default unit loop. For CI timeout control, run the `contract-runtime`, `contract-orchestrator`, `contract-tracker`, `contract-workflow`, and `contract-repository` shards separately with `gleam test -- --suite <name>`. |
| `direnv exec . scherzo-test-local-integration` | Workspace drivers, jj workspace behavior, local integration paths. |
| `direnv exec . scherzo-test-real-pi-validation` | Real pi/session-persistence changes only; uses the devenv-provided pi and requires credentials, network, and time. |
| `LINEAR_API_KEY=... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml` | Real-board readiness after config, workflow, tracker/Linear contract, workspace lifecycle, or pi launch changes. |
| `direnv exec . selfci check --base main@origin --candidate @ --print-output` | Canonical final gate for Scherzo dogfood implementation workflows and broad changes. |

SelfCI vs direct checks:

- Use direct Gleam tests for tight implementation feedback and focused repairs.
- Use local integration or real-pi suites only when the changed surface needs
  those dependencies.
- Use SelfCI when you need the same final gate as the workflow publish step, or
  after changes that span docs/config/source/Nix/formatting.

## If changing workflow DAG parsing or routing

Touch:

- `src/scherzo/workflow_dag.gleam`
- `src/scherzo/runtime_bundle.gleam`
- `src/scherzo/workflow_fingerprint.gleam` if execution identity changes
- `src/scherzo/config.gleam` and `src/scherzo/config/types.gleam` for config
  keys that affect DAG loading or model defaults
- `.scherzo/workflows/*.yaml`, `examples/workflows/*.yaml`, prompt templates if
  the file format changes
- `README.md` and this file for user-visible shape changes

Must preserve:

- YAML orchestrator config remains the runtime entrypoint.
- Routing key equals DAG id.
- Prompt paths stay inside the workflow directory.
- Unique step ids, existing dependencies, no dependency cycles, valid workspace
  source references, and single terminal sink for real workflows.
- Command steps remain non-pi steps and reject model/thinking fields.
- Workflow fingerprints include execution-affecting DAG, prompt, selected workspace
  profile, workspace capability requirements, workspace driver metadata, artifact,
  and model settings.

Run tests:

- `test/workflow_dag_test.gleam`
- `test/runtime_bundle_test.gleam`
- `test/workflow_fingerprint_test.gleam`
- `test/config_test.gleam`
- `test/orchestrator_config_test.gleam`
- `test/orchestrator_service_test.gleam` when routing/startup behavior changes

## If changing workflow scheduling, steps, or workspaces

Touch:

- `src/scherzo/workflow_scheduler.gleam`
- `src/scherzo/workflow_run.gleam`
- `src/scherzo/workspace_run.gleam`
- `src/scherzo/workspace.gleam`
- `src/scherzo/command_step.gleam`
- `src/scherzo/step_artifact.gleam`
- `src/scherzo/template.gleam`
- `src/scherzo/workflow_checkpoint.gleam` for durable step/run facts

Must preserve:

- Same logical workspace never runs two steps concurrently.
- Derived workspaces are prepared from an already-produced transitive source.
- Ready batch execution may be concurrent, but artifact application is
  deterministic.
- `on_failure: continue` produces a dependency-satisfying failed artifact;
  `on_failure: fail` stops scheduling new work and cancels active siblings.
- Workspace paths and cleanup remain contained under `workspace.root`/run root.
- Step artifacts are bounded/redacted before being persisted or templated.
- Driver `after-step` lifecycle operations and legacy `after_step` hooks are
  best-effort; checkpoint append/artifact failures are fatal to recovery correctness.

Run tests:

- `test/workflow_scheduler_test.gleam`
- `test/workflow_run_test.gleam`
- `test/workspace_run_test.gleam`
- `test/workspace_test.gleam`
- `test/command_step_test.gleam`
- `test/step_artifact_test.gleam`
- `test/template_test.gleam`
- `test/state_workflow_checkpoint_test.gleam`
- `test/recovery_workflow_checkpoint_test.gleam` for checkpoint/recovery changes

## If changing orchestrator dispatch, retry, or task updates

Touch:

- `src/scherzo/orchestrator/core.gleam` for pure policy/state transitions
- `src/scherzo/orchestrator/state.gleam` for runtime state shape
- `src/scherzo/orchestrator/daemon.gleam` for actor messages, timers, worker
  lifecycle, EventHub publication, and effect handling
- `src/scherzo/orchestrator/effect_runner.gleam` for async side effects
- `src/scherzo/orchestrator/worker_registry.gleam` for session/worker routing
- `src/scherzo/orchestrator/workflow_reloader.gleam` for reload behavior
- `src/scherzo/tracker/adapter.gleam`, `src/scherzo/tracker/linear_adapter.gleam`, `src/scherzo/handoff.gleam`, and `src/scherzo/linear*.gleam` for tracker/Linear writes
- Ledger/checkpoint modules if the transition must survive restart

Must preserve:

- Pure dispatch logic remains testable in `core.gleam`.
- A claim/dispatch cannot start for inactive, terminal, parked, blocked,
  duplicate running/claimed, over-capacity, or workflow-invalid tasks.
- Refresh/claim validation happens before dispatch side effects.
- Retry/session counters are durable and do not double-count the same recovered
  interrupted run.
- Parking release policy is explicit: operator parks require unpark; auto-unpark
  compares task fingerprints.
- Handoff and tracker side effects are auditable but not exactly-once; preserve
  dedupe/source ids when adding new outbox-like behavior.
- Shutdown removes the control file, stops workers, stops effect runner, and
  releases the instance lock on graceful path.

Run tests:

- `test/orchestrator_core_test.gleam`
- `test/orchestrator_daemon_test.gleam`
- `test/orchestrator_daemon_session_event_test.gleam`
- `test/orchestrator_daemon_control_test.gleam`
- `test/orchestrator_service_test.gleam`
- `test/orchestrator_service_lifecycle_test.gleam`
- `test/orchestrator_effect_runner_test.gleam`
- `test/orchestrator_worker_registry_test.gleam`
- `test/orchestrator_poll_scheduler_test.gleam`
- `test/orchestrator_retry_scheduler_test.gleam`
- `test/handoff_test.gleam`, `test/handoff_format_test.gleam`

## If changing ledger, projection, recovery, or cleanup schemas

Touch:

- `src/scherzo/state/record.gleam`
- `src/scherzo/state/projection.gleam`
- `src/scherzo/state/ledger.gleam`
- `src/scherzo/state/recovery.gleam`
- `src/scherzo/state/outbox.gleam`
- `src/scherzo/state/artifact_store.gleam`
- `src/scherzo/state/local_artifacts.gleam`
- `src/scherzo/workflow_checkpoint.gleam`
- `src/scherzo/orchestrator/daemon.gleam` startup recovery wiring
- `src/scherzo/ctl.gleam` for offline state/cleanup operator output
- `docs/runbooks/workflow-recovery.md` when operator behavior changes

Must preserve:

- Append-only JSONL semantics and one-truncated-tail tolerance.
- Unsupported versions and malformed middle records fail clearly.
- Projection snapshots encode/decode the full projected state.
- Artifact writes happen before step-finished records reference them.
- Pending outbox replay requires bounded v2 payloads.
- Recovery refreshes tracker task state before dispatching from local facts.
- Unsafe interrupted command steps and drifted workflows park instead of silently
  rerunning/resuming.
- Cleanup is conservative, path-safe, symlink-safe, dry-run-first, and writes
  redacted tombstones only for eligible artifacts.

Run tests:

- `test/state_record_test.gleam`
- `test/state_projection_test.gleam`
- `test/state_ledger_test.gleam`
- `test/state_compaction_test.gleam`
- `test/state_recovery_test.gleam`
- `test/workflow_recovery_planner_test.gleam`
- `test/workflow_checkpoint_state_test.gleam`
- `test/recovery_workflow_checkpoint_test.gleam`
- `test/artifact_store_test.gleam`
- `test/state_local_artifacts_test.gleam`
- `test/ctl_test.gleam` for offline state/cleanup CLI changes

## If changing tracker adapters, Linear integration, task updates, or control protocol

Touch:

- `src/scherzo/task.gleam`, `src/scherzo/tracker/adapter.gleam`, and `src/scherzo/tracker/linear_adapter.gleam`
- `src/scherzo/linear.gleam`, `src/scherzo/linear_*`
- `src/scherzo/handoff.gleam`, `src/scherzo/handoff_format.gleam`
- `src/scherzo/linear_contract.gleam`, `src/scherzo/linear_triage.gleam`
- `src/scherzo/control/command.gleam`
- `src/scherzo/control/protocol.gleam`
- `src/scherzo/control/server.gleam`
- `src/scherzo/control/client.gleam`
- `src/scherzo/control/file.gleam`
- `src/scherzo/ctl.gleam`, `scripts/scherzoctl`
- `src/scherzo/session/event.gleam`, `json.gleam`, `hub.gleam` if session data
  shape changes

Must preserve:

- Tracker reads normalize task data through the adapter boundary defined in
  `docs/specs/TRACKER_ADAPTER_SPEC.md` and preserve Linear
  `tracker/issue.gleam` compatibility without leaking raw GraphQL details into
  orchestrator policy.
- Handoff can be disabled and must honor configured comment/state booleans.
- Linear attachment fallback remains safe for non-HTTPS or changed bodyData
  shapes.
- Local control remains loopback-only, token-authenticated, line-delimited, and
  versioned.
- Mutating commands return stable status strings and bounded/redacted messages.
- Linear comments are outbound reporting only; they must not become an inbound
  operator command transport or acknowledgement channel. Future remote-command
  adapter experiments must stay outside production daemon polling unless a new
  design explicitly reopens that boundary.

Run tests:

- `test/linear_test.gleam`, `test/linear_http_test.gleam`
- `test/linear_attachment_test.gleam`,
  `test/linear_attachment_graphql_test.gleam`,
  `test/linear_body_data_test.gleam`
- `test/linear_contract_test.gleam`, `test/linear_triage_test.gleam`
- `test/handoff_test.gleam`, `test/handoff_format_test.gleam`
- `test/control_protocol_test.gleam`, `test/control_server_test.gleam`,
  `test/control_file_test.gleam`, `test/control_command_test.gleam`,
  `test/ctl_test.gleam`, `test/ctl_attach_render_test.gleam`
- `test/linear_command_config_test.gleam`,
  `test/tracker_linear_adapter_test.gleam`,
  `test/orchestrator_daemon_test.gleam`

## If changing FFI, process, pi, or terminal behavior

Touch:

- Relevant `src/*_ffi.erl`
- Public Gleam wrapper (`port.gleam`, `control/*`, `state/ledger.gleam`,
  `instance_lock.gleam`, `signal.gleam`, `lifecycle.gleam`, `terminal/*`,
  `path.gleam`, `hash.gleam`)
- `src/scherzo/agent/*` and `src/scherzo/pi/*` for pi protocol/session changes
- `src/scherzo/command_step.gleam` for shell command behavior

Must preserve:

- No public-network control binding; reject non-loopback hosts.
- Process cwd is the prepared workspace; path containment checks stay in Gleam.
- Bounded line sizes and timeouts fail cleanly.
- Termination paths best-effort kill child process trees and collect diagnostics.
- Pi persistent `argv` launch owns `--session` construction; do not append
  session flags to arbitrary shell command strings.
- Real pi session continuation validates session file and cwd before reuse.
- Terminal rendering must be optional/controlled by color mode and not required
  for JSON automation.

Run tests:

- `test/port_test.gleam`
- `test/command_step_test.gleam`
- `test/pi_protocol_test.gleam`, `test/pi_client_test.gleam`,
  `test/pi_rpc_test.gleam`
- `test/agent_runner_test.gleam`, `test/agent_worker_command_test.gleam`,
  `test/agent_worker_control_test.gleam`, `test/agent_operator_control_test.gleam`
- `test/control_server_test.gleam`, `test/control_file_test.gleam`
- `test/instance_lock_test.gleam`, `test/signal_test.gleam`,
  `test/lifecycle_test.gleam`
- `test/terminal_render_test.gleam`
- `direnv exec . scherzo-test-real-pi-validation` for real pi persistence or
  session continuation changes

## If changing tests or validation infrastructure

Touch:

- `gleam.toml`, `test/scherzo_test.gleam`, `test/test_suite_contract_test.gleam`
- `scripts/scherzo-test-unit`, `scripts/scherzo-test-contract`,
  `scripts/scherzo-test-local-integration`,
  `scripts/scherzo-test-real-pi-validation`
- `.config/selfci/ci.sh` if present in the target tree
- `README.md` test-suite and SelfCI sections
- `devenv.*`, `flake.*`, `nix/*` for toolchain changes

Must preserve:

- `direnv exec . gleam test` remains deterministic, unit-scoped, and free of real
  Linear/pi/network dependencies.
- Shell-heavy daemon/service, port/process, pi-client, workflow, helper-script,
  renderer, and workspace-driver contract coverage stays explicit via
  `scherzo-test-contract`, with CI-friendly contract shards included in SelfCI
  rather than one timeout-prone monolithic contract command in the default unit
  loop.
- Local integration and real-pi suites stay opt-in explicit suites.
- SelfCI remains the final gate used by Scherzo implementation workflows.
- Test fixtures do not require secrets.

Run tests:

- `direnv exec . gleam test`
- The specific suite wrapper or shard being changed, for example `direnv exec . scherzo-test-contract` or `direnv exec . gleam test -- --suite contract-runtime`
- `direnv exec . selfci check --base main@origin --candidate @ --print-output`
  when SelfCI or Nix/devenv behavior changes

## If changing docs, examples, or checked-in workflows

Touch:

- `README.md`
- `docs/ARCHITECTURE.md`
- `docs/runbooks/workflow-recovery.md` for operator recovery/cleanup changes
- `docs/SYMPHONY_SPEC.md` for language-agnostic contract changes
- `.scherzo/scherzo.yaml`, `.scherzo/workflows/*.yaml`, `.scherzo/workflows/prompts/*.md`
- `examples/scherzo.yaml`, `examples/workflows/*.yaml`,
  `examples/workflows/prompts/*.md`
- Relevant helper scripts under `scripts/`

Must preserve:

- Docs describe current implementation, not planned behavior.
- Example config uses YAML orchestrator + YAML DAG + Markdown prompts.
- Checked-in dogfood workflows remain valid under `runtime_bundle.load`.
- Any user-visible command in docs exists in `main.gleam`, `ctl.gleam`, or a
  script.
- Architecture docs link plans/runbooks only as background; agents should not
  have to read full plan history for simple edits.

Run checks:

- For docs-only changes: targeted inspection is usually enough; note validation
  as deferred if the workflow will run final checks.
- For workflow/config examples: `direnv exec . gleam test` plus focused
  `runtime_bundle`, `config`, and `workflow_dag` tests when shape changes.
- For dogfood workflow command changes: consider
  `direnv exec . scherzo-test-local-integration`.

## Existing docs and plans worth knowing

- `README.md`: operator-facing quick start, test suites, config examples,
  control commands, recovery/cleanup summaries, safety posture.
- `docs/runbooks/workflow-recovery.md`: current recovery, cleanup, and old-state
  operator runbook.
- `docs/SYMPHONY_SPEC.md`: broader service contract and vocabulary.
- `docs/plans/simple-dag-workflows.md`: history for YAML orchestrator config and
  DAG workflows.
- `docs/plans/local-control-api-and-scherzoctl.md` and
  `docs/plans/mutating-operator-controls.md`: history for local control.
- `docs/plans/linear-command-transport.md`: history for Linear comment commands.
- `docs/specs/TRACKER_ADAPTER_SPEC.md`: normative tracker adapter contract, data model, capabilities, startup validation, and compatibility rules.
- `docs/runbooks/tracker-adapters.md`: operator guidance for tracker adapters, current capability matrix, and remaining Linear compatibility surfaces.
- `docs/plans/hardening-02-local-durable-state-ledger.md` and
  `docs/plans/hardening-03-single-instance-crash-recovery.md`: ledger/recovery
  history.
- `docs/plans/workflow-resumption-umbrella.md` plus `docs/plans/LIV-54-*` through
  `docs/plans/LIV-58-*`: workflow-step recovery and pi session continuation
  history.

Plans are historical context. The source, tests, README, runbooks, and this file
are the current guide for changes.
