# Workspace driver capability discovery

Scherzo no longer accepts capability lists in orchestrator workspace-driver YAML. Built-in drivers provide known capabilities, and custom driver capability metadata comes from the trusted driver command itself. The normative discovery and capability contract is [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md).

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
workspace.drivers.noop.capabilities was removed. Use driver describe --json. Capabilities are discovered from the workspace driver at runtime. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.
```

apply these steps:

1. Remove the `capabilities:` key from `workspace.drivers.<name>`.
2. For `type: custom`, ensure the configured driver implements `describe --json` and exits 0 with valid metadata. Built-in `type: noop` and `type: jj` do not need discovery config.
3. Rerun workflow-config validation or the test suite.

A migrated artifact-only config can select the built-in no-op driver directly:

```yaml
workspace:
  driver: noop
```

If you need a named no-op driver with explicit timeout or env, use `type: noop`:

```yaml
workspace:
  driver: artifact-only
  drivers:
    artifact-only:
      type: noop
      timeout: 60s
```

The jj driver can carry base policy with friendly fields:

```yaml
workspace:
  driver: isolated
  drivers:
    isolated:
      type: jj
      remote: upstream
      base_branch: trunk
      publish_remote: origin
      timeout: 60s
```

For local/offline work use `base: "@"`; for a repository that already has the selected base locally use `fetch_base: false`. In a fork/upstream recipe, `remote` selects the base remote while `publish_remote` selects the publication remote. Legacy `SCHERZO_PR_REMOTE` and `SCHERZO_PR_BASE` do not configure jj workspace base, fetch, or publication remote behavior; a legacy-only `SCHERZO_PR_REMOTE` makes commit-stack publication fail closed instead of silently publishing to `origin`. Set `github_repo: owner/repo` when `publish-commit-stack` should use an explicit GitHub repository instead of remote URL inference, for example with SSH host aliases. Put `SCHERZO_PR_DRAFT` in `env` when you need to force draft or ready-for-review PRs; when it is unset, `publish-commit-stack` keeps the current default PR draft behavior. `publish-commit-stack` requires `gh`; packaged `scherzo-workspace-jj` includes it on the wrapper path, while source-tree users must provide `gh` on `PATH`.

Workflow requirements still belong in workflow YAML:

```yaml
workspace:
  driver: noop
  requires: [assert-only]
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

The no-op driver should report `status`, `changed-files`, and `assert-only`. The current jj driver should report `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-commit-stack`. It must not report the removed `publish-change` capability; custom drivers that still report it need the migration in [`workspace-driver-migration.md#migrating-from-publish-change-to-publish-commit-stack`](workspace-driver-migration.md#migrating-from-publish-change-to-publish-commit-stack). Neither command should require `SCHERZO_WORKSPACE_PATH` or a prepared workflow workspace.

Then rerun the relevant validation:

```sh
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
direnv exec . env PATH="$PWD/result/bin:$PATH" LINEAR_API_KEY=dummy gleam run -- doctor --check workflow-config examples/scherzo-packaged-noop.yaml
direnv exec . env PATH="$PWD/result/bin:$PATH" LINEAR_API_KEY=dummy gleam run -- doctor --check workflow-config examples/scherzo-packaged-jj.yaml
direnv exec . gleam test
```

If discovery fails with `workspace_driver_discovery_failed`, inspect the driver name and command in the diagnostic. The usual causes are an old custom driver without `describe --json`, malformed JSON, the removed `publish-change` capability, another unknown or duplicate capability name, a nonzero exit, or a timeout.
