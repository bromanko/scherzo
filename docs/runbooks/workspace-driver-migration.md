# Workspace driver migration

Scherzo has typed workspace driver profiles so workflows can declare the workspace operations they require without embedding trusted shell in workflow YAML. Legacy hook-backed workspace profiles still run during migration, hook-backed profiles may advertise workflow-facing driver context, and driver-only profiles can now own workspace lifecycle through the configured driver command.

## What changed

Legacy hook configuration is still supported, but doctor now warns that it is legacy. The warning points here so operators can plan the move from hook snippets to named driver profiles.

Driver commands are operator config only. A workflow may select a profile with `workspace_profile` and declare required metadata with `workspace_capabilities`, but it may not define a driver command. Scherzo validates the selected profile's declared driver capabilities during runtime bundle loading and again before direct workflow execution. If the selected profile has hooks and a driver, the hooks still prepare and clean up the workspace while command steps, agent subprocesses, and prompt templates receive the selected driver context. If the selected profile is driver-only, Scherzo invokes the configured driver for supported lifecycle operations.

This means a hook-backed profile with driver metadata remains a production-safe bridge, and a driver-only profile is the target shape for profiles whose driver implements the lifecycle contract in `docs/runbooks/workspace-driver-contract.md`.

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

Command steps and agent subprocesses run under this profile receive `SCHERZO_WORKSPACE_PROFILE=isolated`, `SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj`, and `SCHERZO_WORKSPACE_CAPABILITIES="status diff changed-files assert-only"`. An original agent prompt can render `{{ workspace.driver }}` or loop over `{% for capability in workspace.capabilities %}`.

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

The accepted lifecycle names are `create`, `before-step`, `after-step`, and `remove`. The accepted workflow capability names are `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. The `driver.command` value must be one executable token without whitespace or shell metacharacters. It may use `$SCHERZO_REPO_ROOT` as a leading placeholder, which Scherzo resolves before exposing `SCHERZO_WORKSPACE_DRIVER` to workflow steps.

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

1. Keep existing hook-backed profiles in production until the target driver has contract tests for lifecycle and required capabilities.
2. Add a `driver` block with a non-secret wrapper command and capabilities to hook-backed profiles that need portable workflow commands or prompt text.
3. Add or review candidate driver-only profile YAML in a non-dispatching environment to validate schema, capability names, and lifecycle behavior.
4. Migrate selected profiles from hook lifecycle to driver-only lifecycle once the repository-specific adapter script passes the contract in `docs/runbooks/workspace-driver-contract.md`.
5. After dogfood and examples use driver-only profiles successfully, plan a separate hard-rejection release for legacy hooks.

## Rollback and recovery

There is no stored data migration in this transition. If doctor warnings are disruptive, operators can temporarily ignore the `workspace-hooks` warning or pin to the previous Scherzo version. To roll back a driver-only profile, restore the previous hook-backed profile or select a hook-backed profile with equivalent driver metadata.

If a workflow requiring `workspace_capabilities` fails with `workspace_capabilities_unavailable`, either remove the requirement or select a profile that declares the missing capabilities in `driver.capabilities`. If lifecycle execution fails, inspect the hook-style failure diagnostics for `driver_lifecycle_create`, `driver_lifecycle_before_step`, `driver_lifecycle_after_step`, or `driver_lifecycle_remove` and run the same driver command manually with the logged environment values.
