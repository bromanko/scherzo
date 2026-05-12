# Migrating from workspace hooks to workspace drivers

Scherzo workspace configuration has moved from inline hook snippets to named workspace profiles with trusted driver commands. A workspace profile is operator-owned policy for creating and preparing step workspaces. A workspace driver is the command configured under that profile. A workspace capability is a named operation, such as `assert-only` or `changed-files`, that a workflow can require before dispatch.

## Who needs this

Use this guide if your orchestrator config contains legacy `workspace.hooks`, `workspace.profiles.<name>.hooks`, or examples copied from an older Scherzo README. Current Scherzo still warns about those legacy shapes during doctor checks so operators can migrate safely, but new checked configs should use `workspace.profiles.<name>.driver`.

Workflow authors also need this guide when a workflow previously assumed a particular VCS command. The workflow should declare `workspace_capabilities` and call the driver command exposed in `SCHERZO_WORKSPACE_DRIVER` for capability operations instead of defining trusted shell in workflow YAML. Command steps run inside the prepared workspace, so workflows that call a relative driver command should resolve it against `SCHERZO_CONFIG_DIR` before invoking it.

## What changed

The old model put trusted shell snippets directly in YAML as hooks. The new model keeps named profiles but moves trusted shell into a driver command configured by the operator. Scherzo calls the driver for lifecycle operations and exposes the selected driver command verbatim to command steps through `SCHERZO_WORKSPACE_DRIVER`.

Driver commands are trusted operator config, not workflow-defined shell. A workflow can select `workspace_profile: isolated` or require `workspace_capabilities: [assert-only]`, but it cannot set `workspace.profiles.<name>.driver.command` and cannot override the configured command at runtime. Scherzo discovers the selected profile's driver capabilities by running `<driver> describe --json`, then validates workflow requirements during runtime bundle loading and again before direct workflow execution.

The accepted lifecycle names are `create`, `before-step`, `after-step`, and `remove`. The accepted public capability names are `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. The `driver.command` value must be one executable token without whitespace or shell metacharacters. It may use `$SCHERZO_REPO_ROOT` as a leading placeholder, which Scherzo resolves before exposing `SCHERZO_WORKSPACE_DRIVER` to workflow steps. Profiles configure command, lifecycle, and timeout; drivers self-describe capabilities with `describe --json`.

If the selected profile has hooks and a driver, the hooks still prepare and clean up the workspace while command steps, agent subprocesses, and prompt templates receive the selected driver context. If the selected profile is driver-only, Scherzo invokes the configured driver for supported lifecycle operations. A hook-backed profile with a driver command remains a production-safe bridge, and a driver-only profile is the target shape for profiles whose driver implements the lifecycle contract in `docs/runbooks/workspace-driver-contract.md`.

`examples/scherzo.yaml` is the canonical runnable checked example for reusable configuration. Because that file lives under `examples/`, its driver commands use `../scripts/...` to reach the checked driver scripts. A config copied to a repository root would normally use `scripts/...`, while a packaged installation can use a PATH command or an absolute trusted wrapper. Keep snippets in this guide aligned with the checked example when driver field names, command paths, lifecycle operations, or metadata behavior change.

## Doctor warnings to expect

A top-level legacy hook config produces warning content like this:

    workspace.hooks is legacy workspace configuration; migrate to workspace.profiles.<name>.driver and read docs/runbooks/workspace-driver-migration.md

A profile-local hook config produces warning content like this:

    workspace.profiles.noop.hooks is legacy workspace configuration; migrate to workspace.profiles.noop.driver and read docs/runbooks/workspace-driver-migration.md

The doctor check name remains `workspace-hooks` during this transition for CLI compatibility.

## Before and after: direct hooks

Before, a small config could put hooks directly under `workspace.hooks`:

```yaml
workspace:
  root: .scherzo/workspaces
  hooks:
    create: |
      mkdir -p "$SCHERZO_WORKSPACE_PATH"
    before_step: |
      test -d "$SCHERZO_WORKSPACE_PATH"
    after_step: |
      true
    remove: |
      rm -rf "$SCHERZO_WORKSPACE_PATH"
    timeout_ms: 60000
```

After, choose a named default profile and configure a driver command. This generic snippet assumes the config file sits at the repository root next to `scripts/`; adjust the command path for the actual config location. The checked no-op driver is useful for artifact-only workflows that do not need a VCS checkout:

```yaml
workspace:
  root: .scherzo/workspaces
  default_profile: noop
  profiles:
    noop:
      driver:
        command: scripts/scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

Then make workflows choose the profile explicitly when they rely on it:

```yaml
version: 1
id: research
workspace_profile: noop
workspace_capabilities: [assert-only]
steps:
  - id: collect_findings
    kind: command
    run: |
      set -eu
      driver_command=${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required}
      : "${SCHERZO_CONFIG_DIR:?SCHERZO_CONFIG_DIR is required for relative workspace drivers}"
      case "$driver_command" in
        /*)
          driver=$driver_command
          ;;
        */*)
          if test -x "$SCHERZO_CONFIG_DIR/$driver_command"; then
            driver=$SCHERZO_CONFIG_DIR/$driver_command
          elif test -x "$SCHERZO_CONFIG_DIR/../$driver_command"; then
            driver=$SCHERZO_CONFIG_DIR/../$driver_command
          else
            driver=$driver_command
          fi
          ;;
        *)
          driver=$driver_command
          ;;
      esac
      "$driver" assert-only --path research-findings.md
      cat research-findings.md
    workspace: main
```

## Before and after: named hook profiles

Before, named profiles could still embed hook snippets:

```yaml
workspace:
  root: .scherzo/workspaces
  default_profile: isolated
  profiles:
    isolated:
      hooks:
        create: |
          mkdir -p "$SCHERZO_WORKSPACE_PATH"
          git clone "$REPO_URL" "$SCHERZO_WORKSPACE_PATH"
        before_step: |
          test -d "$SCHERZO_WORKSPACE_PATH/.git"
        after_step: |
          true
        remove: |
          rm -rf "$SCHERZO_WORKSPACE_PATH"
        timeout_ms: 60000
```

After, keep the profile name and replace `hooks:` with `driver:`. For a jj-backed repository, use the checked driver adapter and let it self-describe supported capabilities:

```yaml
workspace:
  root: .scherzo/workspaces
  default_profile: isolated
  profiles:
    isolated:
      driver:
        command: scripts/scherzo-workspace-jj
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

The profile name can stay stable, so existing workflows that already say `workspace_profile: isolated` do not need to change unless they also require a new capability.

## Hook-backed profile with workflow-facing driver context

During the transition, a hook-backed profile can also declare a driver command. Hooks still own workspace lifecycle; the driver command is discovered with `describe --json` and exposed to workflow command steps, agent subprocesses, and prompt templates as workflow-facing context.

```yaml
workspace:
  root: .scherzo/workspaces
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
```

Command steps and agent subprocesses run under this profile receive `SCHERZO_WORKSPACE_PROFILE=isolated`, `SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj`, and `SCHERZO_WORKSPACE_CAPABILITIES="status diff changed-files assert-only baseline refresh-base publish-change"`. An original agent prompt can render `{{ workspace.driver }}` or loop over `{% for capability in workspace.capabilities %}`.

## Choosing capabilities

Declare only workflow requirements that the workflow actually needs. Driver profiles no longer declare provided capabilities in YAML. The current checked drivers self-describe these public capability names from `describe --json`:

- `scripts/scherzo-workspace-noop`: `status`, `changed-files`, and `assert-only`.
- `scripts/scherzo-workspace-jj`: `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`.

A workflow that invokes the selected driver for `assert-only --path research-findings.md` should declare `workspace_capabilities: [assert-only]`. A workflow that asks the driver for changed files should declare `workspace_capabilities: [changed-files]`. If a workflow declares a capability missing from the selected driver's `describe --json` response, Scherzo fails workflow-config loading before dispatch.

## No-op or artifact-only workflows

Use the no-op driver when a workflow only needs an empty workspace for generated artifacts. The checked research example uses this shape from `examples/scherzo.yaml`, where `../scripts/...` is relative to the `examples/` config directory:

```yaml
workspace:
  default_profile: noop
  profiles:
    noop:
      driver:
        command: ../scripts/scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

The no-op driver's `assert-only` capability verifies that the workspace contains exactly the expected artifact and no other regular files. This is a good fit for research workflows that should produce one Markdown findings file and then stream it as the workflow result.

## Dogfood jj profile

This repository's dogfood config already uses a driver-backed profile named `dogfood-jj` in `.scherzo/scherzo.yaml`. The profile selects `scripts/scherzo-workspace-jj` through `SCHERZO_REPO_ROOT` so Scherzo can locate the checked script even when a runtime workspace has a nested current directory:

```yaml
workspace:
  root: workspaces
  default_profile: dogfood-jj
  profiles:
    dogfood-jj:
      driver:
        command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

Older dogfood configs called `scripts/scherzo-jj-workspace` from hook snippets. That helper still exists behind the driver adapter, but new docs and workflows should point at `scripts/scherzo-workspace-jj` as the driver command.

## Validation

Run validation from the repository root after changing config or workflows:

```sh
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
direnv exec . gleam test
```

For local script sanity, also check that the driver commands named by docs and examples exist and are executable:

```sh
test -x scripts/scherzo-workspace-jj
test -x scripts/scherzo-workspace-noop
```

A passing workflow-config check means Scherzo can parse the orchestrator config, resolve routed workflow DAGs, run each configured driver's `describe --json`, and validate capability requirements against the selected workspace profiles. It does not contact Linear when `LINEAR_API_KEY=dummy` is used with only the workflow-config check.

## Troubleshooting

If doctor reports `legacy_workspace_hooks`, find the `workspace.hooks` or `workspace.profiles.<name>.hooks` block named in the message and migrate that block to a named `driver:` profile. Keep the old profile name when possible so existing `workspace_profile` selectors continue to work.

If workflow-config fails with `workspace_capabilities_unavailable`, the workflow requires a capability that the selected driver's `describe --json` response does not include. Either remove the unnecessary workflow capability, select a profile whose driver reports it, or teach the trusted driver to support it in a separate runtime change.

If a command step says `SCHERZO_WORKSPACE_DRIVER` is empty, the selected profile does not have a driver. Move the workflow to a driver-backed profile before calling driver capabilities.

If lifecycle execution fails, inspect the hook-style failure diagnostics for `driver_lifecycle_create`, `driver_lifecycle_before_step`, `driver_lifecycle_after_step`, or `driver_lifecycle_remove` and run the same driver command manually with the logged environment values.

If a lifecycle command cannot find `scripts/scherzo-workspace-jj`, `scripts/scherzo-workspace-noop`, `../scripts/scherzo-workspace-jj`, or `../scripts/scherzo-workspace-noop`, check where the orchestrator config lives. Public reusable examples use config-relative command paths for checked examples. A copied config in another repository should either place its driver script at the same relative path from the config file, install the driver on `PATH`, or update `driver.command` to an absolute trusted script path.

## Rollback

There is no stored data migration in this transition. If doctor warnings are disruptive, operators can temporarily ignore the `workspace-hooks` warning or pin to the previous Scherzo version. To roll back a driver-only profile, restore the previous hook-backed profile and a Scherzo version whose config schema matches that profile.

Keep config and Scherzo binary versions together. If you must roll back after migrating to drivers, restore the previous config at the same time as the previous Scherzo version. Do not mix a future driver-only Scherzo binary with old direct-hook config; either finish the migration or roll back both the binary and config.

If the migrated profile fails validation or lifecycle execution, switch back to the old config, keep dispatch paused with `agent.max_concurrent_agents: 0` if needed, and rerun the workflow-config doctor check before resuming real dispatch.
