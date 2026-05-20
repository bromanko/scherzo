# Workspace driver capability discovery

Scherzo no longer accepts `workspace.profiles.<name>.driver.capabilities` in orchestrator YAML. Driver capability metadata now comes from the trusted driver command itself. The normative discovery and capability contract is [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md).

Drivers self-describe through:

```sh
<driver> describe --json
```

The command must print one JSON object with version `1` and the supported capability names, for example:

```json
{"version":1,"capabilities":["status","changed-files","assert-only"]}
```

## Fixing the removed-key error

If config loading reports a message like:

```text
workspace.profiles.noop.driver.capabilities was removed; remove this key and ensure the configured driver implements describe --json. See docs/runbooks/workspace-driver-capabilities.md
```

apply these steps:

1. Remove the `capabilities:` key from the profile's `driver:` block. Keep `command`, `lifecycle`, and `timeout_ms`.
2. Ensure the configured driver implements `describe --json` and exits 0 with valid metadata.
3. Rerun workflow-config validation or the test suite.

A migrated source-tree profile should look like this when the config is at the repository root:

```yaml
workspace:
  profiles:
    noop:
      driver:
        command: scripts/scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

A packaged deployment should use the installed command name instead:

```yaml
workspace:
  profiles:
    noop:
      driver:
        command: scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

The packaged jj driver uses the same installed-command shape and can carry base policy in trusted `driver.env`:

```yaml
workspace:
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

For local/offline work use `SCHERZO_JJ_WORKSPACE_BASE=@`; for a repository that already has the selected base locally use `SCHERZO_JJ_WORKSPACE_FETCH_BASE=false`. `SCHERZO_PR_REMOTE` and `SCHERZO_PR_BASE` remain legacy compatibility names. In a fork/upstream recipe, `upstream` selects the base remote while `origin` selects the publication remote. Set `SCHERZO_PR_DRAFT=true` to create draft GitHub PRs or `SCHERZO_PR_DRAFT=false` to force ready-for-review PRs; when it is unset, `publish-change` keeps the current default PR draft behavior. `publish-change` requires `gh`; packaged `scherzo-workspace-jj` includes it on the wrapper path, while source-tree users must provide `gh` on `PATH`.

Workflow requirements still belong in workflow YAML:

```yaml
workspace_profile: noop
workspace_capabilities: [assert-only]
```

## Manual checks

From the repository root, check the bundled source-tree drivers directly:

```sh
scripts/scherzo-workspace-noop describe --json
scripts/scherzo-workspace-jj describe --json
```

After building or installing the package, check the packaged drivers by command name or from the build result:

```sh
scherzo-workspace-noop describe --json
scherzo-workspace-jj describe --json
result/bin/scherzo-workspace-noop describe --json
result/bin/scherzo-workspace-jj describe --json
```

The no-op driver should report `status`, `changed-files`, and `assert-only`. The jj driver should report `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. Neither command should require `SCHERZO_WORKSPACE_PATH` or a prepared workflow workspace.

Then rerun the relevant validation:

```sh
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
direnv exec . env PATH="$PWD/result/bin:$PATH" LINEAR_API_KEY=dummy gleam run -- doctor --check workflow-config examples/scherzo-packaged-noop.yaml
direnv exec . env PATH="$PWD/result/bin:$PATH" LINEAR_API_KEY=dummy gleam run -- doctor --check workflow-config examples/scherzo-packaged-jj.yaml
direnv exec . gleam test
```

If discovery fails with `workspace_driver_discovery_failed`, inspect the profile name and command in the diagnostic. The usual causes are an old driver without `describe --json`, malformed JSON, an unknown or duplicate capability name, a nonzero exit, or a timeout.
