# Migrating from workspace hooks to workspace drivers

Scherzo workspace configuration has moved from inline hook snippets and public workspace profiles to workspace drivers. `workspace.driver` selects built-in `noop`/`jj` or a named entry under `workspace.drivers`; a `type: custom` driver supplies the trusted command. A workspace capability is a named operation, such as `assert-only` or `changed-files`, that a workflow can require before dispatch. The normative driver contract is [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md); this runbook stays focused on migration and troubleshooting.

## Who needs this

Use this guide if your orchestrator config contains legacy `workspace.hooks`, `workspace.default_profile`, `workspace.profiles`, driver-local `hooks`, driver-local `lifecycle`, `timeout_ms`, or examples copied from an older Scherzo README. Current Scherzo rejects those legacy shapes during config loading; update or reset the config to use `workspace.driver` and `workspace.drivers.<name>` before dispatching or running doctor checks that load workflow config.

Workflow authors also need this guide when a workflow previously assumed a particular VCS command. The workflow should declare `workspace.requires` and call the driver command exposed in `SCHERZO_WORKSPACE_DRIVER` for capability operations instead of defining trusted shell in workflow YAML. Command steps run inside the prepared workspace, so workflows that call a relative driver command should resolve it against `SCHERZO_CONFIG_DIR` before invoking it.

## What changed

The old model put trusted shell snippets directly in YAML as hooks, and later exposed them through `workspace.profiles`. The current model keeps trusted workspace policy in `workspace.driver` and `workspace.drivers`. Scherzo calls the selected driver for lifecycle operations and exposes the resolved driver command to command steps through `SCHERZO_WORKSPACE_DRIVER`.

Driver commands are trusted operator config, not workflow-defined shell. A workflow can select `workspace.driver: isolated` or require `workspace.requires: [assert-only]`, but it cannot set `workspace.drivers.<name>.command` and cannot override the configured command at runtime. Built-in `noop` and `jj` drivers have known capabilities; custom drivers self-describe by running `<driver> describe --json`. Scherzo validates workflow requirements during runtime bundle loading and again before direct workflow execution.

The accepted lifecycle command names remain `create`, `before-step`, `after-step`, and `remove`, but lifecycle selection is no longer public config. The accepted public capability names are `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. `type: custom` `command` values must be one executable token without whitespace or shell metacharacters. They may use `$SCHERZO_REPO_ROOT` as a leading placeholder, which Scherzo resolves before exposing `SCHERZO_WORKSPACE_DRIVER` to workflow steps. Drivers configure `type`, optional `timeout`, optional `env`, and for `type: jj` friendly fields such as `remote`, `base_branch`, `base`, `fetch_base`, `publish_remote`, and `github_repo`.

Legacy hook/profile blocks are no longer runtime configuration. Top-level `workspace.hooks`, `workspace.default_profile`, `workspace.profiles`, driver-local `hooks`, driver-local `lifecycle`, and `timeout_ms` fail config loading with an `invalid_config` diagnostic that names the unsupported key and links back to the simplified schema. Use `workspace.driver` plus a `workspace.drivers.<name>` entry whose selected driver implements the lifecycle contract in [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md).

`examples/scherzo.yaml` is the canonical runnable checked source-tree example for reusable configuration. Because that file lives under `examples/`, custom driver commands use `../scripts/...` to reach checked driver scripts. A config copied to a repository root would normally use `scripts/...` for a custom driver, or select the built-in `noop`/`jj` driver by type. A packaged installation can select `driver: noop`/`driver: jj`, use a named `type: noop`/`type: jj` entry, or use `type: custom` with a PATH command/absolute trusted wrapper. Keep snippets in this guide aligned with the checked examples when driver field names, command paths, lifecycle behavior, or metadata behavior change.

## Diagnostics to expect

A top-level legacy hook config fails workflow-config loading with an `invalid_config` message like this:

    workspace.hooks was removed. Use workspace.drivers.<name>.type: custom. Top-level workspace lifecycle hooks were removed; use a custom workspace driver command instead. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.

Driver-local legacy config fails with the same diagnostic shape and names the driver key:

    workspace.drivers.noop.hooks was removed. Use workspace.drivers.noop.type: custom. Workspace lifecycle hook config was removed; use a custom workspace driver command instead. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.

The doctor check name remains `workspace-hooks` for CLI compatibility, but legacy hook config is rejected by the earlier workflow-config load.

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

After, select a workspace driver. The built-in no-op driver is useful for artifact-only workflows that do not need a VCS checkout:

```yaml
workspace:
  root: .scherzo/workspaces
  driver: noop
```

If you need a named no-op driver with an explicit timeout or env, define it under `workspace.drivers`:

```yaml
workspace:
  root: .scherzo/workspaces
  driver: artifact-only
  drivers:
    artifact-only:
      type: noop
      timeout: 60s
```

Packaged jj deployments can use the built-in jj driver and friendly fields without copying `scripts/` from this repository:

```yaml
workspace:
  root: .scherzo/workspaces
  driver: isolated
  drivers:
    isolated:
      type: jj
      remote: upstream
      base_branch: trunk
      publish_remote: origin
      timeout: 60s
```

This fork/upstream recipe selects the base from `trunk@upstream` and publishes through `origin`. For local-only work, set `base: "@"`; for an offline repository that already has the selected base locally, set `fetch_base: false`.

Then make workflows choose the driver name explicitly when they rely on it:

```yaml
version: 1
id: research
workspace:
  driver: noop
  requires: [assert-only]
steps:
  - id: collect_findings
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
    run_in: main
```

## Before and after: old named hook profiles

Before, old named profiles could embed hook snippets:

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

After, keep the selector name and move policy to `workspace.driver` / `workspace.drivers`. For a jj-backed repository, use `type: jj` and let Scherzo map friendly fields to the bundled driver environment:

```yaml
workspace:
  root: .scherzo/workspaces
  driver: isolated
  drivers:
    isolated:
      type: jj
      timeout: 60s
```

The name can stay stable, but workflows must now select it with `workspace.driver: isolated` and move any capability declarations to `workspace.requires`.

Command steps and agent subprocesses run under a selected driver receive `SCHERZO_WORKSPACE_PROFILE=isolated`, `SCHERZO_WORKSPACE_DRIVER=<resolved-driver-command>`, and `SCHERZO_WORKSPACE_CAPABILITIES="status diff changed-files assert-only baseline refresh-base publish-change"`. An original agent prompt can render `{{ workspace.driver }}` or loop over `{% for capability in workspace.capabilities %}`.

## Choosing capabilities

Declare only workflow requirements that the workflow actually needs. Workspace driver config no longer declares provided capabilities in YAML. The current checked and packaged drivers provide these public capability names:

- `scripts/scherzo-workspace-noop` and `scherzo-workspace-noop`: `status`, `changed-files`, and `assert-only`.
- `scripts/scherzo-workspace-jj` and `scherzo-workspace-jj`: `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`.

A workflow that invokes the selected driver for `assert-only --path research-findings.md` should declare `workspace.requires: [assert-only]`. A workflow that asks the driver for changed files should declare `workspace.requires: [changed-files]`. If a workflow declares a capability missing from the selected driver's `describe --json` response, Scherzo fails workflow-config loading before dispatch.

## No-op or artifact-only workflows

Use the no-op driver when a workflow only needs an empty workspace for generated artifacts. Packaged and source-tree deployments can select the built-in directly:

```yaml
workspace:
  driver: noop
```

If you need to configure timeout or env for a named no-op driver, use `type: noop`:

```yaml
workspace:
  driver: artifact-only
  drivers:
    artifact-only:
      type: noop
      timeout: 60s
```

The no-op driver's `assert-only` capability verifies that the workspace contains exactly the expected artifact and no other regular files. This is a good fit for research workflows that should produce one Markdown findings file and then stream it as the workflow result.

## Dogfood jj driver

This repository's dogfood config uses a named driver `dogfood-jj` in `.scherzo/scherzo.yaml`. The driver uses `type: jj`, so Scherzo selects the bundled jj command and maps friendly fields to the driver environment:

```yaml
workspace:
  root: workspaces
  driver: dogfood-jj
  drivers:
    dogfood-jj:
      type: jj
      remote: scherzo-agent
      base_branch: main
      fetch_base: true
      publish_remote: scherzo-agent
      github_repo: scherzo-systems/scherzo
      timeout: 60s
```

The jj driver is self-contained. Use `type: jj` for the bundled source-tree/packaged driver, or `type: custom` with an explicit command when you intentionally wrap or replace it.

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

That wrapper can now move to `type: jj` friendly fields under the same trusted driver name:

```yaml
workspace:
  driver: isolated
  drivers:
    isolated:
      type: jj
      base: "@"
      remote: upstream
      base_branch: trunk
      fetch_base: false
      publish_remote: origin
```

`base` maps to `SCHERZO_JJ_WORKSPACE_BASE`, the strongest base override, and `base: "@"` skips fetch for local/offline work. Without that explicit base, `remote` and `base_branch` choose the root-workspace base and default `refresh-base` target. `fetch_base: false` disables driver-initiated base fetches. Derived workspaces created with `from:` use the source workspace's `@` and skip root-base fetches because the source workspace already embodies the selected base policy.

Publication is separate from base selection. In a fork/upstream setup, set `remote: upstream` and `base_branch: trunk` for the base, then set `publish_remote: origin` or another fork remote for `publish-change`. If `publish_remote` / `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE` is absent, publication uses `origin` unless legacy `SCHERZO_PR_REMOTE` is present; legacy-only `SCHERZO_PR_REMOTE` makes `publish-change` fail closed with guidance to set `publish_remote`. Legacy `SCHERZO_PR_REMOTE` and `SCHERZO_PR_BASE` do not affect jj driver base, fetch, or publication remote selection. Set `github_repo: owner/repo` when publication should not infer the GitHub repository from the selected remote URL, such as SSH host-alias remotes. Put `SCHERZO_PR_DRAFT` in `env` when you need to force draft or ready-for-review PRs; unset preserves the driver's current default. The `publish-change` capability requires GitHub CLI `gh`; packaged `scherzo-workspace-jj` includes `gh` on its wrapper path, while source-tree usage requires `gh` on the process `PATH` before publishing.

Driver `env` values are literal strings. Scherzo does not expand `$LINEAR_API_KEY` or append `$PATH`; a configured `PATH` is a full replacement for discovery, lifecycle, command-step, and agent-step subprocesses under that driver. Driver env is not a secret store. Scherzo performs limited redaction for likely-sensitive keys in Scherzo-owned diagnostics, but operators should not put durable secrets there. Keep a wrapper when it also supplies arguments, performs authentication, discovers tools dynamically, or enforces safety checks that are more than fixed environment variables.

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

A passing workflow-config check means Scherzo can parse the orchestrator config, resolve routed workflow DAGs, run discovery for custom drivers that need `describe --json`, and validate capability requirements against the selected workspace drivers. It does not contact Linear when `LINEAR_API_KEY=dummy` is used with only the workflow-config check.

## Troubleshooting

If workflow-config reports `invalid_config` for `workspace.hooks`, `workspace.default_profile`, `workspace.profiles`, driver-local `hooks`, driver-local `lifecycle`, or `timeout_ms`, migrate that block to `workspace.driver` and `workspace.drivers.<name>`. Keep the old selector name when possible, but update workflow files to select it through `workspace.driver`.

If workflow-config fails with `workspace_capabilities_unavailable`, the workflow requires a capability that the selected driver does not provide. Either remove the unnecessary workflow capability, select a driver that reports it, or teach the trusted driver to support it in a separate runtime change.

If a command step says `SCHERZO_WORKSPACE_DRIVER` is empty, the selected driver did not resolve to a command. Move the workflow to `noop`, `jj`, or a valid named `workspace.drivers.<name>` entry before calling driver capabilities.

If lifecycle execution fails, inspect the hook-style failure diagnostics for `driver_lifecycle_create`, `driver_lifecycle_before_step`, `driver_lifecycle_after_step`, or `driver_lifecycle_remove` and run the same driver command manually with the logged environment values.

If a lifecycle command cannot find `scripts/scherzo-workspace-jj`, `scripts/scherzo-workspace-noop`, `../scripts/scherzo-workspace-jj`, or `../scripts/scherzo-workspace-noop`, check whether you intentionally configured `type: custom` with a source-tree path. A copied config in another repository should either place its custom driver script at the same relative path from the config file, install the driver on `PATH`, update `workspace.drivers.<name>.command` to an absolute trusted script path, or use `type: noop` / `type: jj` instead.

## Rollback

There is no stored data migration in this transition. Legacy hook/profile config now requires an older Scherzo version whose config schema still accepts it. To roll back, restore the previous hook-backed profile config and the matching older Scherzo version together.

Keep config and Scherzo binary versions together. Do not mix a workspace-driver Scherzo binary with old direct-hook/profile config; either finish the migration or roll back both the binary and config.

If the migrated driver fails validation or lifecycle execution, keep dispatch paused with `agents.concurrency: 0` if needed, and rerun the workflow-config doctor check before resuming real dispatch.
