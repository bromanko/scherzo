# Portable research workflow

The portable research workflow lets an operator run a Scherzo research task in any repository that can provide one workspace-driver capability: `assert-only`. The workflow asks the agent to write exactly one file, `research-findings.md`, then a command step asks the configured workspace driver to prove that this file is the only produced artifact before Scherzo streams the file as the workflow result.

## Terms

A workspace is the directory where Scherzo runs a workflow step. A workspace driver is operator configuration that decides how that directory is created, checked, and removed. `workspace.driver` selects a built-in driver or a named `workspace.drivers.<name>` entry. Workflow YAML may require a capability, but it must not supply the trusted driver command itself. The full normative driver contract is [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../specs/WORKSPACE_DRIVER_SPEC.md).

The `assert-only --path research-findings.md` capability must exit 0 only when `research-findings.md` is the only produced file or change according to the selected driver's baseline. It must exit nonzero with a bounded diagnostic when the file is missing or when any unexpected artifact is present.

## Files to copy

Copy these files into the target repository's Scherzo workflow directory:

- `examples/workflows/research.yaml`
- `examples/workflows/prompts/research.md`
- `examples/workflows/prompts/research-recover-failed-step.md`
- `examples/workflows/schemas/provider/workflow-step-recovery-result.v1.schema.json`
- `examples/workflows/schemas/workflow-step-recovery-result.v1.schema.json`

The workflow selects `workspace_profile: noop` and declares `workspace_capabilities: [assert-only]`. You may rename the driver in your config, but if you do, update the workflow's `workspace_profile` to match.

## Minimal driver config

Use a workspace driver so Scherzo can create, check, and remove the artifact workspace through the same trusted command that provides `assert-only`.

A minimal artifact-only config selects the built-in no-op driver:

    workspace:
      root: .scherzo/workspaces
      driver: noop

If you need to configure timeout or env, use a named `type: noop` driver:

    workspace:
      root: .scherzo/workspaces
      driver: research
      drivers:
        research:
          type: noop
          timeout: 60s

For a custom source-tree driver, configure the trusted command explicitly:

    workspace:
      root: .scherzo/workspaces
      driver: research
      drivers:
        research:
          type: custom
          command: scripts/scherzo-workspace-noop
          timeout: 60s

A custom `command` should name the trusted executable configured by the operator. In another repository, place the wrapper at the configured relative path from the config file, install it on `PATH`, or configure an absolute trusted script path. Custom drivers must self-describe `assert-only` from `describe --json`; do not add capability lists to YAML. Command steps receive `SCHERZO_WORKSPACE_DRIVER` and run from the prepared workspace, so workflows that call driver capabilities should resolve simple relative custom driver paths against `SCHERZO_CONFIG_DIR`, as `examples/workflows/research.yaml` does. Do not put secret material in driver config.

For packaged jj research drivers, prefer `type: jj` friendly fields to make base policy explicit. `base: "@"` is the local/offline recipe, `fetch_base: false` skips driver fetches, `remote: upstream` plus `base_branch: trunk` selects `trunk@upstream`, and `publish_remote: origin` keeps publication pointed at a fork remote. Legacy `SCHERZO_PR_REMOTE` and `SCHERZO_PR_BASE` do not affect jj driver base, fetch, or publication remote selection; legacy-only `SCHERZO_PR_REMOTE` fails closed for `publish-change`, which requires `gh` when a workflow uses publication. Set `github_repo: owner/repo` when publication needs an explicit GitHub repository, such as with SSH host aliases.

## Driver behavior by workspace style

For an empty artifact workspace, `scherzo-workspace-noop` in a package or `scripts/scherzo-workspace-noop` in a source-tree checkout treats every regular file under the workspace root as a produced artifact, except its private marker and Scherzo's `.scherzo` diagnostics directory. Ignored files, build caches, generated metadata, downloaded indexes, snapshots, and lockfile changes count as unexpected artifacts when they are regular files in the workspace. A successful research run should therefore leave only `research-findings.md`.

For a copied repository workspace, use an adapter that records or knows the prepared baseline before the agent step. Its `assert-only` operation should compare the current workspace against that baseline and succeed only when the relative changed-file set is exactly `research-findings.md`. The adapter documentation should say whether ignored files and generated caches are counted. If it cannot state that rule clearly, do not use it for this workflow.

For a worktree or clone backed by a version-control system, the adapter should use that system's changed-file view from the prepared baseline. The workflow itself must not call the version-control tool directly. If generated files, build outputs, or lockfile updates appear in the changed-file view, `assert-only` should fail before the report is streamed.

For a jj workspace, `scripts/scherzo-workspace-jj` implements `assert-only` from the current jj change with `jj diff --name-only --color=never`. In ordinary single-parent workflow workspaces this is the prepared parent-to-current diff; in merge-resolution workspaces it avoids ambiguous `@-` revsets. Files reported by that diff count as produced changes. Files ignored by jj's own ignore rules are not reported by that diff, but tracked or unignored generated files are counted and will make the assertion fail.

## Agent side effects and cleanup

Research commands can write files even when the agent only meant to inspect behavior. Common examples include language build caches, dependency downloads, generated indexes, snapshots, coverage output, temporary metadata, and lockfile updates. The prompt tells agents to avoid commands likely to write those files unless they are necessary.

If `assert-only` fails, inspect the driver's diagnostic. Remove only artifacts you recognize as generated side effects. Do not remove source files or repository metadata to force the assertion to pass. The example workflow wires one bounded recovery attempt only on the final `collect_findings` step. That recovery worker may clean obvious generated side effects or repair `research-findings.md` from already-retained local evidence, then ask Scherzo to retry the unchanged collection step. It must give up on driver/configuration errors, ambiguous source changes, missing evidence, or unsafe cleanup. If recovery is unavailable or gives up, rerun the task from a clean workspace.

If a command would be useful but would create artifacts that cannot be cleaned safely, the agent should skip it and record the skipped command under `Issues encountered` in `research-findings.md`.

## Validation before adoption

Before routing real tasks to the workflow, run a small manual check in a disposable workspace. Resolve the configured command the same way the example workflow does, then call the resolved path:

    driver_command=${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required}
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

1. Run `"$driver" describe --json`; expect version `1` metadata that includes `assert-only`.
2. Create only `research-findings.md` and run `"$driver" assert-only --path research-findings.md`; expect exit code 0.
3. Add `unexpected-artifact.txt` and run the same command; expect a nonzero exit and a diagnostic naming the unexpected artifact or changed-file set.
4. Remove the unexpected file and run the workflow against a low-risk task; expect the terminal result to be the contents of `research-findings.md`.

This workflow is safe to roll back by reverting the workflow YAML, prompt, and driver config changes. It does not change stored tracker state or repository data beyond the temporary workflow workspace.
