# Daemon boundary guardrails

`src/scherzo/orchestrator/daemon.gleam` remains Scherzo's public daemon actor shell. It may own public actor startup, public message receipt, compatibility types, dependency injection, control-plane/process/timer edges, top-level logging/redaction context, and handoff between subsystem outcomes. It must not regrow extracted subsystem helpers without an explicit update to this document and the matching source guardrail.

The daemon line-count ratchet is `max_daemon_lines: 6376`. Lower the ratchet whenever `src/scherzo/orchestrator/daemon.gleam` shrinks. Never raise it to let extracted code move back into the daemon. Raise it only when a review shows the added code is daemon-owned according to this document.

`src/scherzo/orchestrator/service.gleam` is the only documented startup-edge import exception for `scherzo/orchestrator/daemon`. It may import the daemon because it is the process edge that starts the public actor. Extracted orchestrator subsystem modules must not import `scherzo/orchestrator/daemon`.

## Extracted owners and forbidden prefixes

### `src/scherzo/orchestrator/scheduled_runtime.gleam`

Forbidden daemon prefix: `scheduled_`.

Exact daemon shell exceptions:

- `scheduled_failure_paths`: the daemon shell still maps configured scheduled-failure routes into scheduled-runtime input lists.
- `scheduled_projection_for_root`: the daemon shell still reads projected scheduled state before calling scheduled-runtime helpers.
- `scheduled_job_by_id`: the daemon shell still resolves a scheduled job from the loaded workflow before handing work off.
- `scheduled_worker_down_context`: the daemon shell still shapes daemon-owned crash logging for scheduled worker exits.
- `scheduled_worker_active_for_job`: the daemon shell still checks actor occupancy before asking scheduled-runtime whether another run may start.
- `scheduled_slot_available_for_start`: the daemon shell still applies daemon-owned headroom and active-worker policy at the process edge.
- `scheduled_worker_spawn_context`: the daemon shell still builds top-level logging fields for scheduled worker launches.
- `scheduled_worker_finished_context`: the daemon shell still shapes top-level completion logging for scheduled workers.
- `scheduled_worker_success_context`: the daemon shell still formats daemon-owned success log context after scheduled-runtime decisions.
- `scheduled_worker_needs_human_context`: the daemon shell still formats daemon-owned follow-up log context after scheduled-runtime decisions.
- `scheduled_worker_failure_context`: the daemon shell still formats daemon-owned failure log context after scheduled-runtime decisions.
- `scheduled_worker_failure_follow_up`: the daemon shell still translates scheduled-runtime failure output into daemon-owned transition messages.
- `scheduled_failure_ledger_append`: the daemon shell still appends daemon-owned ledger records after scheduled failure reporting.
- `scheduled_failure_dedupe_key`: the daemon shell still computes daemon-owned report dedupe keys at the orchestration edge.
- `scheduled_failure_issue_id_for_state`: the daemon shell still resolves daemon-owned issue identity while reporting scheduled failures.

### `src/scherzo/orchestrator/startup_recovery.gleam`

Forbidden daemon prefix: `recovered_`.

Exact daemon shell exceptions:

- `recovered_contract_manifest`: the daemon shell still reads workflow contract metadata while rebuilding startup recovery inputs.
- `recovered_workflow_identity_matches`: the daemon shell still compares daemon-owned recovery records with loaded workflow identities.
- `recovered_workspaces_to_prepared`: the daemon shell still converts daemon-owned workspace recovery snapshots into worker inputs.

### `src/scherzo/orchestrator/worker_lifecycle.gleam`

Forbidden daemon prefix: `worker_`.

Exact daemon shell exceptions:

- `worker_issue_state_name`: the daemon shell still reads daemon runtime state when shaping operator and retry decisions.
- `worker_run_id_from_resolution`: the daemon shell still extracts daemon-owned resolution metadata after worker lookups.
- `worker_issue_state_name_from_resolution`: the daemon shell still derives daemon-owned issue-state labels from lookup results.
- `worker_for_session`: the daemon shell still resolves live worker handles from daemon-owned session maps.
- `worker_spawn_context`: the daemon shell still formats top-level logging context for worker launches.
- `worker_command_ready_context`: the daemon shell still formats top-level logging context when a worker command subject arrives.
- `worker_update_context`: the daemon shell still formats top-level logging context for worker progress updates.
- `worker_finished_context`: the daemon shell still formats top-level logging context for worker completion.
- `worker_down_context`: the daemon shell still formats top-level logging context for worker crashes.

### `src/scherzo/orchestrator/yaml_workflow_lifecycle.gleam`

Forbidden daemon prefixes: `yaml_`, `handle_yaml_`, and `log_yaml_`.

Exact daemon shell exceptions:

- `handle_yaml_step_command_ready`: the daemon shell still routes actor-owned command subjects into daemon state.
- `handle_yaml_step_started`: the daemon shell still records daemon-owned session-start side effects for YAML steps.
- `handle_yaml_step_finished`: the daemon shell still applies daemon-owned cleanup after YAML step completion.
- `yaml_child_recovery_info`: the daemon shell still derives daemon-owned recovery metadata for orphan YAML children.
- `yaml_step_callbacks`: the daemon shell still assembles daemon-owned actor callbacks before delegating step execution.
- `yaml_scheduled_workflow_dependencies`: the daemon shell still wires daemon-owned scheduled workflow dependencies.
- `yaml_workflow_dependencies`: the daemon shell still wires daemon-owned ad hoc workflow dependencies.
- `yaml_worker_failure`: the daemon shell still translates workflow-lifecycle failures into daemon transition messages.
- `yaml_workflow_failure`: the daemon shell still translates workflow-level failures into daemon transition messages.
- `log_yaml_step_update`: the daemon shell still formats top-level logging for YAML step updates.

### `src/scherzo/orchestrator/operator_runtime.gleam`

Forbidden daemon prefixes: `operator_` and `parked_`.

Exact daemon shell exceptions:

- `operator_command_reply`: the daemon shell still owns the public actor reply contract for operator commands.
- `operator_issue_resolution`: the daemon shell still exposes a compatibility wrapper while operator-runtime remains an internal helper.
- `parked_issue_resolution`: the daemon shell still exposes a compatibility wrapper while operator-runtime remains an internal helper.
- `parked_issue_id_for_ref`: the daemon shell still resolves parked issue identity from daemon-owned state.
- `parked_issue_id_for_identifier`: the daemon shell still resolves parked issue identity from daemon-owned state.

### `src/scherzo/orchestrator/remote_command_runtime.gleam`

Forbidden daemon prefixes: `start_remote_client` and `restart_remote_client`.

Exact daemon shell exceptions:

- `start_remote_client_now`: the daemon shell still owns the actor-side wiring that starts the remote client process.
- `restart_remote_client_if_enabled`: the daemon shell still owns the actor-side policy that decides whether a remote client restart should happen.

## Update process

When a future change adds a daemon-owned helper that would otherwise match a forbidden prefix, add the exact function name here with a one-sentence daemon-shell rationale in the same review. Do not add broad prefix exceptions. If a helper belongs to an extracted subsystem instead of the daemon shell, move the helper out of `src/scherzo/orchestrator/daemon.gleam` and lower the line-count ratchet in the same change.
