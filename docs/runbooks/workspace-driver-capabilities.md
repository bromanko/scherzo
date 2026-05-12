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

A migrated profile should look like this:

```yaml
workspace:
  profiles:
    noop:
      driver:
        command: scripts/scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

Workflow requirements still belong in workflow YAML:

```yaml
workspace_profile: noop
workspace_capabilities: [assert-only]
```

## Manual checks

From the repository root, check the bundled drivers directly:

```sh
scripts/scherzo-workspace-noop describe --json
scripts/scherzo-workspace-jj describe --json
```

The no-op driver should report `status`, `changed-files`, and `assert-only`. The jj driver should report `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. Neither command should require `SCHERZO_WORKSPACE_PATH` or a prepared workflow workspace.

Then rerun the relevant validation:

```sh
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
direnv exec . gleam test
```

If discovery fails with `workspace_driver_discovery_failed`, inspect the profile name and command in the diagnostic. The usual causes are an old driver without `describe --json`, malformed JSON, an unknown or duplicate capability name, a nonzero exit, or a timeout.
