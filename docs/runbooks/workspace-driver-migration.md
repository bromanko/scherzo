# Workspace driver migration

Scherzo is adding typed workspace driver profiles so workflows can declare the workspace operations they require without embedding trusted shell in workflow YAML. This release is a transition point: legacy hook-backed workspace profiles still run, hook-backed profiles may advertise workflow-facing driver context, driver-only profiles parse as schema, and workflows that select driver-only profiles are rejected before dispatch until driver lifecycle invocation support lands.

## What changed

Legacy hook configuration is still supported, but doctor now warns that it is legacy. The warning points here so operators can plan the move from hook snippets to named driver profiles.

Driver commands are operator config only. A workflow may select a profile with `workspace_profile` and declare required metadata with `workspace_capabilities`, but it may not define a driver command. Scherzo validates the selected profile's declared driver capabilities during runtime bundle loading. If the selected profile has hooks and a driver, the hooks still prepare and clean up the workspace while command steps receive `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`, and agent prompts can render `workspace.profile`, `workspace.driver`, and `workspace.capabilities`. If the selected profile is driver-only and the declared capabilities match, this release still fails safely with `workspace_driver_invocation_unavailable` because the runtime does not yet invoke driver lifecycle operations.

This means a hook-backed profile with driver metadata can run production workflows and expose workflow-facing driver context. A driver-only profile is useful for schema validation and review, but not for running production workflows yet.

## Doctor warnings to expect

A top-level legacy hook config produces warning content like this:

    workspace.hooks is legacy workspace configuration; migrate to workspace.profiles.<name>.driver and read docs/runbooks/workspace-driver-migration.md

A profile-local hook config produces warning content like this:

    workspace.profiles.noop.hooks is legacy workspace configuration; migrate to workspace.profiles.noop.driver and read docs/runbooks/workspace-driver-migration.md

The doctor check name remains `workspace-hooks` during this transition for CLI compatibility.

## Old top-level hook shape

Top-level `workspace.hooks` created an implicit `default` workspace profile:

    workspace:
      root: workspaces
      hooks:
        create: scripts/scherzo-jj-workspace after-create
        before_step: scripts/scherzo-jj-workspace before-run
        after_step: true
        remove: scripts/scherzo-jj-workspace before-remove
        timeout_ms: 60000

This remains valid and runnable in this release, but it is legacy.

## Old profile-local hook shape

Named profiles can still be hook-backed:

    workspace:
      root: workspaces
      default_profile: isolated
      profiles:
        isolated:
          hooks:
            create: scripts/scherzo-jj-workspace after-create
            before_step: scripts/scherzo-jj-workspace before-run
            after_step: true
            remove: scripts/scherzo-jj-workspace before-remove
            timeout_ms: 60000

This remains valid and runnable in this release, but it is also legacy.

## Hook-backed profile with workflow-facing driver context

During the transition, the production-safe shape is a hook-backed profile that also declares a driver command and capabilities. Hooks still own workspace lifecycle; the driver command is exposed to workflow command steps and prompt templates as workflow-facing context.

    workspace:
      root: workspaces
      default_profile: isolated
      profiles:
        isolated:
          hooks:
            create: scripts/scherzo-jj-workspace after-create
            before_step: scripts/scherzo-jj-workspace before-run
            after_step: true
            remove: scripts/scherzo-jj-workspace before-remove
            timeout_ms: 60000
          driver:
            command: scripts/scherzo-workspace-jj
            capabilities: [status, diff, changed-files, assert-only]

A command step run under this profile receives `SCHERZO_WORKSPACE_PROFILE=isolated`, `SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj`, and `SCHERZO_WORKSPACE_CAPABILITIES="status diff changed-files assert-only"`. An original agent prompt can render `{{ workspace.driver }}` or loop over `{% for capability in workspace.capabilities %}`. Pi subprocess environment inheritance is intentionally not part of this transition.

## Driver-only schema shape

The driver-only schema uses `driver` under a named profile:

    workspace:
      root: workspaces
      default_profile: isolated
      profiles:
        isolated:
          driver:
            command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [status, diff, changed-files, assert-only, baseline, refresh-base, publish-change]
            timeout_ms: 60000

The accepted lifecycle names are `create`, `before-step`, `after-step`, and `remove`. The accepted workflow capability names are `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`.

Do not switch an active workflow to this driver-only profile in this release. Runtime bundle loading rejects selected driver-only profiles with `workspace_driver_invocation_unavailable` until the driver invocation child plan defines and tests the lifecycle command contract.

## Workflow capability declarations

A workflow can declare required capabilities at the top level:

    version: 1
    id: research
    workspace_profile: isolated
    workspace_capabilities: [assert-only, changed-files]
    steps:
      - id: research
        kind: agent
        prompt: prompts/research.md

Scherzo compares this list to the selected profile's declared `driver.capabilities`. Hook-backed profiles without a `driver` block and the synthetic default profile provide no driver capabilities. This is declared metadata validation only; it does not prove that a driver command actually implements the operation.

## Recommended migration sequence

1. Keep existing hook-backed profiles in production.
2. Add a `driver` block with a non-secret wrapper command and capabilities to hook-backed profiles that need portable workflow commands or prompt text.
3. Add or review candidate driver-only profile YAML in a non-dispatching environment to validate schema and capability names.
4. Wait for the driver lifecycle invocation release and repository-specific adapter scripts before migrating workspace preparation from `hooks` to driver lifecycle operations.
5. Migrate dogfood and example configs from hook lifecycle to driver-only profiles in the same coordinated release that proves driver lifecycle behavior.
6. Only after driver invocation and dogfood migration are complete, plan a separate hard-rejection release for legacy hooks.

## Rollback and recovery

There is no stored data migration in this transition. If doctor warnings are disruptive, operators can temporarily ignore the `workspace-hooks` warning or pin to the previous Scherzo version. Do not try to work around `workspace_driver_invocation_unavailable` by selecting a driver-only profile for active dispatch; use a hook-backed profile, optionally with driver metadata, until the invocation plan lands.

If a workflow requiring `workspace_capabilities` fails with `workspace_capabilities_unavailable`, either remove the requirement or select a profile that declares the missing capabilities in `driver.capabilities`. If it then fails with `workspace_driver_invocation_unavailable`, the schema is compatible but driver-only lifecycle runtime support is intentionally not available yet.
