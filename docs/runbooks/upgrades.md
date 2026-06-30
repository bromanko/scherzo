# Breaking-change upgrade policy

Scherzo prefers explicit upgrades over hidden compatibility. When a change breaks an old config, schema, workflow, driver contract, runtime assumption, or durable state shape, detect the old shape at the safest boundary and fail fast with actionable diagnostics instead of silently reinterpreting it. Where the old shape is statically discoverable, `doctor` should report it before operators dispatch work.

This runbook is contributor policy and operator guidance. It is not a generic migration framework.

## What counts as a breaking change

Treat a change as breaking when an existing supported deployment could keep the same files or local state but get a different result, fail later in a less obvious place, or lose the ability to inspect/recover safely. Common Scherzo examples include:

- orchestrator config keys, defaults, meanings, or required fields;
- workflow YAML schema, prompt/template variables, structured-output declarations, or validator contracts;
- workspace driver, lifecycle, capability, or environment-variable contracts;
- tracker adapter task shape, capability names, task-update behavior, or Linear compatibility aliases;
- persisted ledger records, projection snapshots, workflow checkpoints, artifacts, control protocol payloads, or offline state layout;
- `scherzoctl` command behavior where scripts or operators rely on stable output; and
- runtime behavior that changes retry, recovery, parking, cleanup, publication, or destructive action semantics.

Internal refactors are not breaking when they preserve the same checked examples, operator commands, durable semantics, and public diagnostics.

## Contributor policy

For every breaking change, make the compatibility decision explicit in the same change as the implementation.

1. **Name the old shape.** Identify the exact old key, field, record version, workflow field, driver operation, command output, or runtime condition. Prefer path-like names such as `workspace.hooks`, `workspace.default_profile`, `workspace.profiles`, `tracker.api_key`, or `ledger.schema_version`.
2. **Choose the boundary.** Detect stale shapes at parse/load boundaries for config and workflow YAML, at discovery boundaries for drivers and tracker capabilities, at replay/decode boundaries for durable state, and at command/runtime boundaries when the shape only appears during execution.
3. **Fail fast instead of emulating.** Do not silently reinterpret old input as new semantics. A temporary compatibility window may warn and continue only when a specific migration plan says so; the warning must still name the old shape and target shape.
4. **Use stable diagnostic codes.** Diagnostics should have stable warning/error codes suitable for tests and operator search. Include the affected path/field and replacement path when safe. Keep text bounded and redacted.
5. **Link the runbook.** Operator-facing diagnostics should point to this runbook or a more specific runbook, for example `docs/runbooks/simplified-yaml-migration.md`, `docs/runbooks/workspace-driver-migration.md`, `docs/runbooks/tracker-adapters.md`, or `docs/runbooks/workflow-recovery.md`.
6. **Add `doctor` coverage when static.** If Scherzo can discover the old shape without dispatching work, add or update a `doctor` check. `doctor` should report the same stable code and path details that runtime errors use.
7. **Prefer explicit operator actions for durable state.** For unsupported local state, require deliberate actions such as `archive-old`, `discard-old`, or `reinitialize`. Do not mutate, delete, or partially replay old durable data automatically.
8. **Update examples and docs.** Checked examples, README documentation-map links, architecture checklists, and focused runbooks must describe the current shape and the upgrade path.
9. **Guard the policy with tests.** Add parser/doctor/runtime tests for the diagnostic and a lightweight docs test when the change adds a runbook or required link.

## Diagnostic expectations

A good breaking-change diagnostic is short, searchable, and actionable. It should include:

- a stable `code` or event name, such as `invalid_config`, `legacy_tracker_field_ignored`, or `old_state_reset_required`;
- the old path or shape, for example `workspace.hooks`;
- the replacement or required action, for example `workspace.driver` / `workspace.drivers.<name>`;
- the boundary where it was detected, such as config load, `doctor`, driver discovery, ledger replay, or `scherzo state status`;
- a link to upgrade guidance; and
- bounded context that avoids secrets, full prompts, raw tracker comments, and raw tool payloads.

Prefer one diagnostic that names all affected paths over a cascade of generic parse failures. If a parser cannot safely continue after the first stale field, make the first failure point at the upgrade path.

## Doctor behavior

Add a `doctor` check when the old shape is statically discoverable from local files or read-only external metadata. Good candidates include old config keys, workflow YAML fields, driver `describe --json` versions, missing tracker capabilities, and board/label contract mismatches.

`doctor` should:

- keep read-only checks read-only;
- use backend-neutral check names where available, while preserving existing compatibility aliases;
- return `Warn` for a temporary compatibility window and `Fail` when the old shape is unsupported;
- include fields such as `legacy_key`, `replacement`, `config_path`, or `schema_version` when safe; and
- point to the relevant runbook in human output and structured output.

Do not rely only on `doctor`. Runtime and decode boundaries must still reject unsupported shapes because operators may skip readiness checks or change files after running them.

## Durable state policy

Durable state is evidence and recovery input, not a place to guess. If a new Scherzo tree cannot safely replay old ledger, snapshot, artifact, checkpoint, or control-protocol data, the runtime should reject that state clearly. Operators then choose an explicit action.

Recommended flow for unsupported local state:

```sh
scherzo state status --root <workspace-root>
scherzo state archive-old --root <workspace-root> --yes
scherzo state reinitialize --root <workspace-root> --yes
```

Use `archive-old` first when the old state may be needed for forensics. Use `discard-old` only when the old state is known to be disposable:

```sh
scherzo state discard-old --root <workspace-root> --yes
scherzo state reinitialize --root <workspace-root> --yes
```

Do not silently replay unsupported records as partial current state. Do not delete old state automatically during daemon startup. See [workflow recovery](workflow-recovery.md) for recovery vocabulary, cleanup, and old-state handling.

## Concrete Scherzo examples

### Legacy workspace hooks/profiles to workspace drivers

Old shapes:

- `workspace.hooks`
- `workspace.default_profile`
- `workspace.profiles`
- `workspace.drivers.<name>.hooks`
- `workspace.drivers.<name>.lifecycle`
- `workspace.drivers.<name>.timeout_ms`

Current target shape:

- `workspace.driver`
- `workspace.drivers.<name>` with `type: noop`, `type: jj`, or `type: custom`

Current behavior rejects those old shapes during workflow-config loading with `invalid_config`. The diagnostic names the old key, names the driver key, and links to [workspace driver migration](workspace-driver-migration.md) or the simplified YAML spec. Checked configs must use workspace drivers; no compatibility window remains for hook-backed workspace profiles.

Recommended operator command:

```sh
scherzo doctor --check workflow-config .scherzo/scherzo.yaml
```

### Obsolete in-process WorkflowDag tuple shims

Old shapes:

- BEAM terms tagged `workflow_dag` whose tuple arity predates `recover` or `workstream_phase` fields.
- BEAM terms tagged `workflow_step` whose tuple arity predates the step `recover` field.

Current behavior rejects silent in-process compatibility for those tuple shapes by not loading or normalizing serialized DAG terms. Runtime bundles load workflow definitions from YAML through the current `workflow_dag.parse` path on startup and reload, and recovery observations are rebuilt from the current runtime bundle rather than decoded from retained DAG terms. Workflow YAML that omits optional `recover` fields remains supported; obsolete pre-current BEAM tuple shapes from mixed-version hot code upgrades are unsupported.

Recommended operator action is to stop and restart Scherzo after upgrading instead of hot-swapping modules. If durable local state cannot be replayed by the current tree, follow the unsupported local state flow below (`archive-old` or `discard-old`, then `reinitialize`) rather than expecting DAG tuple normalization.

### Older orchestrator config to simplified YAML

Old shapes include:

- `tracker.kind`
- `tracker.credentials.api_key_env`
- `tracker.linear.project_slug`
- `tracker.active_states`, `tracker.dispatch_states`, and `tracker.terminal_states`
- `polling.interval_ms`
- `routing.workflows`
- `handoff`
- top-level `agent`
- top-level `pi`

Current target fields include:

- `tracker.linear.api_key_env`
- `tracker.linear.tasks_from.project` (or `tracker.linear.tasks_from.projects` for an explicit list); `tracker.linear.project` remains compatibility syntax for existing single-project configs
- `tracker.states.active`, `tracker.states.ready`, and `tracker.states.terminal`
- `tracker.polling.every`
- top-level `workflows`
- `task_routing`
- `task_updates`
- `agents` and `agents.runtime`

Current behavior rejects removed simplified-YAML keys during config loading with an `invalid_config` diagnostic that names the old key, names the replacement or removal, and links to [simplified YAML migration](simplified-yaml-migration.md) or the [simplified schema](../specs/SCHERZO_YAML_SIMPLIFIED_V1.md). Do not keep old and new sections side by side.

### Unsupported local state

Old ledger or snapshot schema markers are durable runtime shapes. If the current tree cannot support them, Scherzo reports unsupported state instead of partially replaying it. Operator-facing recovery uses `old_state_reset_required` vocabulary and the offline state commands:

```sh
scherzo state status --root <workspace-root>
scherzo state archive-old --root <workspace-root> --yes
scherzo state discard-old --root <workspace-root> --yes
scherzo state reinitialize --root <workspace-root> --yes
```

Prefer archive plus reinitialize when evidence may matter; discard only after accepting loss of old local history.

## Review checklist

Before merging a breaking change, reviewers should be able to answer yes to these questions:

- Does the change define the old and new shapes?
- Is old input detected at the earliest safe boundary?
- Is the diagnostic stable, tested, and linked to a runbook?
- Does the diagnostic include the affected path/field when safe?
- Does `doctor` catch the old shape when it is statically discoverable?
- Are checked examples and docs updated to the new shape?
- Are durable-state changes handled by explicit archive/discard/reinitialize-style actions rather than automatic mutation?
- Is any temporary compatibility behavior intentionally documented and covered by tests?

## Non-goals

- Do not build a generic migration framework for every future schema change.
- Do not add silent automatic compatibility behavior unless a specific future issue calls for it.
- Do not perform destructive state changes during detection.
- Do not keep old docs/examples as equal current guidance after a replacement shape is available.
