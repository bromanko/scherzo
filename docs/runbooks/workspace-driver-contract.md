# Workspace driver command contract

Scherzo workspace drivers are trusted operator-configured commands. They prepare a workspace for a workflow run, check it around step execution, and provide a small set of workflow-facing capabilities that command steps can call without knowing whether the workspace is backed by jj, git, a copy, or an empty artifact directory.

This document defines the initial command contract implemented by `scripts/scherzo-workspace-jj` and `scripts/scherzo-workspace-noop`. It is a contract reference, not a migration guide. Legacy hook-backed profiles are still documented in `docs/runbooks/workspace-driver-migration.md` until runtime driver invocation is enabled.

## Glossary

A workspace is the directory where Scherzo runs one logical workflow step or a sequence of steps that share the same named workspace. `SCHERZO_WORKSPACE_PATH` points at this directory when Scherzo knows it.

A workspace driver is one local command configured by the operator. Workflow YAML does not provide arbitrary driver commands. Scherzo and workflow command steps invoke the configured driver with the subcommands in this document.

A lifecycle operation is a driver command that Scherzo runs while creating, checking, or removing a workspace. Lifecycle operations may create or delete directories, so destructive operations must require an explicit target.

A capability is a driver command that a workflow step may run to inspect or assert workspace output. Capability paths and adapter-authored diagnostics are relative to the workspace root.

## Invocation root and environment

Drivers must interpret `SCHERZO_WORKSPACE_PATH` as the workspace root when it is set. Read-only capability commands may fall back to the current directory when `SCHERZO_WORKSPACE_PATH` is unset. Destructive lifecycle commands must not fall back to the current directory; they must require an explicit, non-empty `SCHERZO_WORKSPACE_PATH`.

Drivers may also receive these Scherzo environment variables:

- `SCHERZO_RUN_ROOT`, the run directory that contains logical workspaces. Destructive removal should verify that the workspace path is contained by this run root when the variable is set.
- `SCHERZO_CONFIG_DIR`, the directory containing the orchestrator config.
- `SCHERZO_REPO_ROOT`, the coordinating source repository for VCS-backed drivers.
- `SCHERZO_SOURCE_WORKSPACE_PATH`, the source workspace for derived workspace creation.
- `SCHERZO_WORKFLOW_ID`, `SCHERZO_ISSUE_IDENTIFIER`, `SCHERZO_RUN_ID`, and `SCHERZO_WORKSPACE_NAME`, identity values used to derive stable workspace names.

Driver-authored output must avoid local absolute workspace roots unless it is relaying bounded output from an underlying tool failure. Capability output and assertion diagnostics should name workspace-root-relative paths.

## Lifecycle command forms

The initial lifecycle command forms are exactly:

    <driver> lifecycle create
    <driver> lifecycle before-step
    <driver> lifecycle after-step
    <driver> lifecycle remove

`lifecycle create` prepares the workspace directory. `lifecycle before-step` checks that the workspace is usable before a step runs. `lifecycle after-step` performs any post-step check or cleanup; adapters that have no post-step work may make it a successful no-op. `lifecycle remove` performs driver-specific cleanup before the run directory is removed.

The no-op artifact driver requires an explicit `SCHERZO_WORKSPACE_PATH` for every lifecycle command. It creates the directory and a private marker file during `lifecycle create`, verifies that the directory exists for `before-step` and `after-step`, and refuses `remove` unless the marker is present. When `SCHERZO_RUN_ROOT` is set, `remove` also refuses a workspace outside that run root.

The jj driver maps lifecycle operations to the existing legacy helper: `create` delegates to `scripts/scherzo-jj-workspace after-create`, `before-step` delegates to `scripts/scherzo-jj-workspace before-run`, `remove` delegates to `scripts/scherzo-jj-workspace before-remove`, and `after-step` is a successful no-op in the initial contract.

## Capability command forms

The initial capability command forms are exactly:

    <driver> status --human
    <driver> diff --human
    <driver> changed-files --json
    <driver> assert-only --path <relative-file>

`status --human` prints a short human-oriented summary. Tests may assert basic text, but workflow automation should not parse it.

`diff --human` prints a human-oriented diff. The initial jj driver supports it by invoking jj. The no-op artifact driver does not support `diff` because an empty artifact workspace has no baseline.

`changed-files --json` prints one JSON array of workspace-root-relative path strings followed by a newline. The array is sorted lexicographically and deduplicated. For the no-op artifact driver, the changed-file set is every regular file under the workspace root except driver or Scherzo scratch files such as the private marker and `.scherzo` diagnostics. For the jj driver, the changed-file set is derived from `jj diff --from @- --to @ --name-only --color=never`.

`assert-only --path <relative-file>` succeeds only when the changed-file set is exactly the given relative file path. The path argument must be non-empty, must not be `.`, must not be absolute, and must not contain `..` as a complete path segment. Unsafe path arguments fail before filesystem or VCS inspection.

## Exit codes

Exit code 0 means the requested operation succeeded.

Exit code 1 means the request was valid but the operation failed or an assertion did not hold. For example, `assert-only --path research-findings.md` exits 1 when extra files are present.

Exit code 2 means the caller used the contract incorrectly, requested an unsupported operation, supplied an unsafe path argument, or asked a destructive lifecycle operation to operate on a refused target.

## Adapter support matrix

`scripts/scherzo-workspace-noop` supports these lifecycle operations and capabilities:

- lifecycle: `create`, `before-step`, `after-step`, `remove`
- capabilities: `status`, `changed-files`, `assert-only`

`scripts/scherzo-workspace-jj` supports these lifecycle operations and capabilities:

- lifecycle: `create`, `before-step`, `after-step`, `remove`
- capabilities: `status`, `diff`, `changed-files`, `assert-only`

The capability names `baseline`, `refresh-base`, and `publish-change` are reserved for later plans. Initial adapters must not silently treat these names as successful operations.

## Safety rules for adapter authors

Keep path semantics rooted at the workspace. Emit relative paths in machine output and adapter-authored assertion failures. Reject unsafe assertion paths before inspecting the filesystem or invoking a VCS. Keep usage text bounded and return exit code 2 for unsupported commands.

For destructive commands, require an explicit target. A driver may use the current directory for read-only inspection, but it must not use the current directory as the target for `lifecycle remove` when `SCHERZO_WORKSPACE_PATH` is missing or empty.
