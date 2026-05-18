# Breaking-change upgrade policy

Scherzo prefers explicit upgrades over hidden compatibility. When a change breaks an old config, schema, workflow, driver contract, runtime assumption, or durable state shape, detect the old shape at the safest boundary and fail fast with actionable diagnostics instead of silently reinterpreting it. Where the old shape is statically discoverable, `doctor` should report it before operators dispatch work.

This runbook is contributor policy and operator guidance. It is not a generic migration framework.

## What counts as a breaking change

Treat a change as breaking when an existing supported deployment could keep the same files or local state but get a different result, fail later in a less obvious place, or lose the ability to inspect/recover safely. Common Scherzo examples include:

- orchestrator config keys, defaults, meanings, or required fields;
- workflow YAML schema, prompt/template variables, structured-output declarations, or validator contracts;
- workspace profile, workspace driver, lifecycle, capability, or environment-variable contracts;
- tracker adapter task shape, capability names, handoff behavior, or Linear compatibility aliases;
- persisted ledger records, projection snapshots, workflow checkpoints, artifacts, control protocol payloads, or offline state layout;
- `scherzoctl` command behavior where scripts or operators rely on stable output; and
- runtime behavior that changes retry, recovery, parking, cleanup, publication, or destructive action semantics.

Internal refactors are not breaking when they preserve the same checked examples, operator commands, durable semantics, and public diagnostics.

## Contributor policy

For every breaking change, make the compatibility decision explicit in the same change as the implementation.

1. **Name the old shape.** Identify the exact old key, field, record version, workflow field, driver operation, command output, or runtime condition. Prefer path-like names such as `workspace.hooks`, `workspace.profiles.<name>.hooks`, `tracker.api_key`, or `ledger.schema_version`.
2. **Choose the boundary.** Detect stale shapes at parse/load boundaries for config and workflow YAML, at discovery boundaries for drivers and tracker capabilities, at replay/decode boundaries for durable state, and at command/runtime boundaries when the shape only appears during execution.
3. **Fail fast instead of emulating.** Do not silently reinterpret old input as new semantics. A temporary compatibility window may warn and continue only when a specific migration plan says so; the warning must still name the old shape and target shape.
4. **Use stable diagnostic codes.** Diagnostics should have stable warning/error codes suitable for tests and operator search. Include the affected path/field and replacement path when safe. Keep text bounded and redacted.
5. **Link the runbook.** Operator-facing diagnostics should point to this runbook or a more specific runbook, for example `docs/runbooks/workspace-driver-migration.md`, `docs/runbooks/tracker-adapters.md`, or `docs/runbooks/workflow-recovery.md`.
6. **Add `doctor` coverage when static.** If Scherzo can discover the old shape without dispatching work, add or update a `doctor` check. `doctor` should report the same stable code and path details that runtime errors use.
7. **Prefer explicit operator actions for durable state.** For unsupported local state, require deliberate actions such as `archive-old`, `discard-old`, or `reinitialize`. Do not mutate, delete, or partially replay old durable data automatically.
8. **Update examples and docs.** Checked examples, README documentation-map links, architecture checklists, and focused runbooks must describe the current shape and the upgrade path.
9. **Guard the policy with tests.** Add parser/doctor/runtime tests for the diagnostic and a lightweight docs test when the change adds a runbook or required link.

## Diagnostic expectations

A good breaking-change diagnostic is short, searchable, and actionable. It should include:

- a stable `code` or event name, such as `legacy_workspace_hooks`, `legacy_tracker_field_ignored`, or `old_state_reset_required`;
- the old path or shape, for example `workspace.hooks`;
- the replacement or required action, for example `workspace.profiles.<name>.driver`;
- the boundary where it was detected, such as config load, `doctor`, driver discovery, ledger replay, or `scherzoctl state status`;
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
scherzoctl state status --root <workspace-root>
scherzoctl state archive-old --root <workspace-root> --yes
scherzoctl state reinitialize --root <workspace-root> --yes
```

Use `archive-old` first when the old state may be needed for forensics. Use `discard-old` only when the old state is known to be disposable:

```sh
scherzoctl state discard-old --root <workspace-root> --yes
scherzoctl state reinitialize --root <workspace-root> --yes
```

Do not silently replay unsupported records as partial current state. Do not delete old state automatically during daemon startup. See [workflow recovery](workflow-recovery.md) for recovery vocabulary, cleanup, and old-state handling.

## Concrete Scherzo examples

### Legacy workspace hooks to workspace drivers

Old shapes:

- `workspace.hooks`
- `workspace.profiles.<name>.hooks`

Current target shape:

- `workspace.profiles.<name>.driver`

Current behavior uses a statically discoverable `doctor` warning with code `legacy_workspace_hooks`. The warning names the old key, names the driver key, and links to [workspace driver migration](workspace-driver-migration.md). During the compatibility window, checked configs may still load, but new examples should use driver-backed profiles. If a future change removes hook support, config loading should reject the hook shape at the config boundary with the same stable code/path vocabulary and the same migration link.

Recommended operator command:

```sh
scherzo doctor --check workspace-hooks .scherzo/scherzo.yaml
```

### Flat tracker fields to nested tracker config

Old flat fields:

- `tracker.api_key`
- `tracker.endpoint`
- `tracker.project_slug`

Current target fields:

- `tracker.credentials.api_key_env`
- `tracker.linear.endpoint`
- `tracker.linear.project_slug`

When both flat and nested fields are present, the nested value wins and Scherzo reports `legacy_tracker_field_ignored` with `path` and `replacement`. New examples should use the nested tracker shape. A future hard removal should fail during config load or `doctor --check workflow-config`, not silently choose one meaning. See [tracker adapters](tracker-adapters.md) for the current tracker configuration guidance.

### Unsupported local state

Old ledger or snapshot schema markers are durable runtime shapes. If the current tree cannot support them, Scherzo reports unsupported state instead of partially replaying it. Operator-facing recovery uses `old_state_reset_required` vocabulary and the offline state commands:

```sh
scherzoctl state status --root <workspace-root>
scherzoctl state archive-old --root <workspace-root> --yes
scherzoctl state discard-old --root <workspace-root> --yes
scherzoctl state reinitialize --root <workspace-root> --yes
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
