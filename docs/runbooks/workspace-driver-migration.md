# Migrating from workspace hooks to workspace drivers

Scherzo workspace configuration has moved from inline hook snippets to named workspace profiles with trusted driver commands. A workspace profile is operator-owned policy for creating and preparing step workspaces. A workspace driver is the command configured under that profile. A workspace capability is a named operation, such as `assert-only` or `changed-files`, that a workflow can require before dispatch. The normative driver contract is [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md); this runbook stays focused on migration and troubleshooting.

## Who needs this

Use this guide if your orchestrator config contains legacy `workspace.hooks`, `workspace.profiles.<name>.hooks`, or examples copied from an older Scherzo README. Current Scherzo still warns about those legacy shapes during doctor checks so operators can migrate safely, but new checked configs should use `workspace.profiles.<name>.driver`.

Workflow authors also need this guide when a workflow previously assumed a particular VCS command. The workflow should declare `workspace_capabilities` and call the driver command exposed in `SCHERZO_WORKSPACE_DRIVER` for capability operations instead of defining trusted shell in workflow YAML. Command steps run inside the prepared workspace, so workflows that call a relative driver command should resolve it against `SCHERZO_CONFIG_DIR` before invoking it.

## What changed

The old model put trusted shell snippets directly in YAML as hooks. The new model keeps named profiles but moves trusted shell into a driver command configured by the operator. Scherzo calls the driver for lifecycle operations and exposes the selected driver command verbatim to command steps through `SCHERZO_WORKSPACE_DRIVER`.

Driver commands are trusted operator config, not workflow-defined shell. A workflow can select `workspace_profile: isolated` or require `workspace_capabilities: [assert-only]`, but it cannot set `workspace.profiles.<name>.driver.command` and cannot override the configured command at runtime. Scherzo discovers the selected profile's driver capabilities by running `<driver> describe --json`, then validates workflow requirements during runtime bundle loading and again before direct workflow execution.

The accepted lifecycle names are `create`, `before-step`, `after-step`, and `remove`. The accepted public capability names are `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. The `driver.command` value must be one executable token without whitespace or shell metacharacters. It may use `$SCHERZO_REPO_ROOT` as a leading placeholder, which Scherzo resolves before exposing `SCHERZO_WORKSPACE_DRIVER` to workflow steps. Profiles configure command, lifecycle, and timeout; drivers self-describe capabilities with `describe --json`.

If the selected profile has hooks and a driver, the hooks still prepare and clean up the workspace while command steps, agent subprocesses, and prompt templates receive the selected driver context. If the selected profile is driver-only, Scherzo invokes the configured driver for supported lifecycle operations. A hook-backed profile with a driver command remains a production-safe bridge, and a driver-only profile is the target shape for profiles whose driver implements the lifecycle contract in [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md).

`examples/scherzo.yaml` is the canonical runnable checked source-tree example for reusable configuration. Because that file lives under `examples/`, its driver commands use `../scripts/...` to reach the checked driver scripts. A config copied to a repository root would normally use `scripts/...`. A packaged installation should use `command: scherzo-workspace-noop` for the bundled no-op driver, as shown by `examples/scherzo-packaged-noop.yaml`, or another PATH command/absolute trusted wrapper. Keep snippets in this guide aligned with the checked examples when driver field names, command paths, lifecycle operations, or metadata behavior change.

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

If Scherzo is installed as a package, prefer the packaged executable instead of a source-tree script path:

```yaml
workspace:
  root: .scherzo/workspaces
  default_profile: noop
  profiles:
    noop:
      driver:
        command: scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

Packaged jj deployments can use the stable installed command name without copying `scripts/` from this repository. The checked example is `examples/scherzo-packaged-jj.yaml`:

```yaml
workspace:
  root: .scherzo/workspaces
  default_profile: isolated
  profiles:
    isolated:
      driver:
        command: scherzo-workspace-jj
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
        env:
          SCHERZO_JJ_WORKSPACE_REMOTE: upstream
          SCHERZO_JJ_WORKSPACE_BASE_BRANCH: trunk
          SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE: origin
```

This fork/upstream recipe selects the base from `trunk@upstream` and publishes through `origin`. For local-only work, set `SCHERZO_JJ_WORKSPACE_BASE: "@"`; for an offline repository that already has the selected base locally, set `SCHERZO_JJ_WORKSPACE_FETCH_BASE: "false"`.

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

## Legacy hook-backed profile with workflow-facing driver context

During the transition, a hook-backed profile can also declare a driver command. Prefer the direct `driver:` profile shape above for new config. If a legacy hook-backed profile must remain temporarily, call the same driver lifecycle operations directly from hooks; the driver command is discovered with `describe --json` and exposed to workflow command steps, agent subprocesses, and prompt templates as workflow-facing context.

```yaml
workspace:
  root: .scherzo/workspaces
  default_profile: isolated
  profiles:
    isolated:
      hooks:
        create: scripts/scherzo-workspace-jj lifecycle create
        before_step: scripts/scherzo-workspace-jj lifecycle before-step
        after_step: scripts/scherzo-workspace-jj lifecycle after-step
        remove: scripts/scherzo-workspace-jj lifecycle remove
        timeout_ms: 60000
      driver:
        command: scripts/scherzo-workspace-jj
```

Command steps and agent subprocesses run under this profile receive `SCHERZO_WORKSPACE_PROFILE=isolated`, `SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj`, and `SCHERZO_WORKSPACE_CAPABILITIES="status diff changed-files assert-only baseline refresh-base publish-change"`. An original agent prompt can render `{{ workspace.driver }}` or loop over `{% for capability in workspace.capabilities %}`.

## Choosing capabilities

Declare only workflow requirements that the workflow actually needs. Driver profiles no longer declare provided capabilities in YAML. The current checked and packaged drivers self-describe these public capability names from `describe --json`:

- `scripts/scherzo-workspace-noop` and `scherzo-workspace-noop`: `status`, `changed-files`, and `assert-only`.
- `scripts/scherzo-workspace-jj` and `scherzo-workspace-jj`: `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`.

A workflow that invokes the selected driver for `assert-only --path research-findings.md` should declare `workspace_capabilities: [assert-only]`. A workflow that asks the driver for changed files should declare `workspace_capabilities: [changed-files]`. If a workflow declares a capability missing from the selected driver's `describe --json` response, Scherzo fails workflow-config loading before dispatch.

## No-op or artifact-only workflows

Use the no-op driver when a workflow only needs an empty workspace for generated artifacts. Packaged deployments should use the installed command name:

```yaml
workspace:
  default_profile: noop
  profiles:
    noop:
      driver:
        command: scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

The checked research source-tree example uses this shape from `examples/scherzo.yaml`, where `../scripts/...` is relative to the `examples/` config directory:

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

The jj driver is self-contained. Configure `scripts/scherzo-workspace-jj` directly for source-tree dogfood workflows, or use the packaged `scherzo-workspace-jj` command in installed deployments.

Legacy hook snippets that previously used the removed `scripts/scherzo-jj-workspace` compatibility command should map its verbs onto the driver lifecycle operations instead: `after-create` becomes `scripts/scherzo-workspace-jj lifecycle create`, `before-run` becomes `scripts/scherzo-workspace-jj lifecycle before-step`, and `before-remove` becomes `scripts/scherzo-workspace-jj lifecycle remove`.

## Replacing simple environment wrapper scripts

Some old profiles used a wrapper only to export driver policy variables before execing the real driver:

```sh
#!/bin/sh
export SCHERZO_JJ_WORKSPACE_BASE="@"
export SCHERZO_JJ_WORKSPACE_REMOTE="upstream"
export SCHERZO_JJ_WORKSPACE_BASE_BRANCH="trunk"
exec scripts/scherzo-workspace-jj "$@"
```

That wrapper can now move to `driver.env` under the same trusted profile:

```yaml
workspace:
  profiles:
    isolated:
      driver:
        command: scripts/scherzo-workspace-jj
        lifecycle: [create, before-step, after-step, remove]
        env:
          SCHERZO_JJ_WORKSPACE_BASE: "@"
          SCHERZO_JJ_WORKSPACE_REMOTE: upstream
          SCHERZO_JJ_WORKSPACE_BASE_BRANCH: trunk
          SCHERZO_JJ_WORKSPACE_FETCH_BASE: "false"
          SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE: origin
```

`SCHERZO_JJ_WORKSPACE_BASE` is the strongest base override and `SCHERZO_JJ_WORKSPACE_BASE=@` skips fetch for local/offline work. Without that explicit base, `SCHERZO_JJ_WORKSPACE_REMOTE` and `SCHERZO_JJ_WORKSPACE_BASE_BRANCH` choose the root-workspace base and default `refresh-base` target; `SCHERZO_PR_REMOTE` and `SCHERZO_PR_BASE` are legacy compatibility names used only when the jj-specific names are absent. `SCHERZO_JJ_WORKSPACE_FETCH_BASE=false` disables driver-initiated base fetches. Derived workspaces created with `from:` use the source workspace's `@` and skip root-base fetches because the source workspace already embodies the selected base policy.

Publication is separate from base selection. In a fork/upstream setup, set `SCHERZO_JJ_WORKSPACE_REMOTE=upstream` and `SCHERZO_JJ_WORKSPACE_BASE_BRANCH=trunk` for the base, then set `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE=origin` or another fork remote for `publish-change`. If `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE` is absent, publication falls back to `SCHERZO_PR_REMOTE` and then `origin`. The `publish-change` capability requires GitHub CLI `gh`; packaged `scherzo-workspace-jj` includes `gh` on its wrapper path, while source-tree usage requires `gh` on the process `PATH` before publishing.

`driver.env` values are literal strings. Scherzo does not expand `$LINEAR_API_KEY` or append `$PATH`; a configured `PATH` is a full replacement for discovery, lifecycle, command-step, and agent-step subprocesses under that profile. `driver.env` is not a secret store. Scherzo performs limited redaction for likely-sensitive keys in Scherzo-owned diagnostics, but operators should not put durable secrets there. Keep a wrapper when it also supplies arguments, performs authentication, discovers tools dynamically, or enforces safety checks that are more than fixed environment variables.

## Validation

Run validation from the repository root after changing config or workflows:

```sh
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
direnv exec . env PATH="$PWD/result/bin:$PATH" LINEAR_API_KEY=dummy gleam run -- doctor --check workflow-config examples/scherzo-packaged-noop.yaml
direnv exec . env PATH="$PWD/result/bin:$PATH" LINEAR_API_KEY=dummy gleam run -- doctor --check workflow-config examples/scherzo-packaged-jj.yaml
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
