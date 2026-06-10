# Workspace Driver Specification

Status: Draft v1

Purpose: Define the normative contract between Scherzo, operator-configured workspace drivers, and workflow code that calls workspace-driver capabilities.

## Normative language

The key words `MUST`, `MUST NOT`, `REQUIRED`, `SHOULD`, `SHOULD NOT`, `RECOMMENDED`, `MAY`, and `OPTIONAL` in this document are to be interpreted as described in RFC 2119.

`Implementation-defined` means the behavior is part of a driver's contract, but this specification does not prescribe one universal policy. Drivers MUST document their selected behavior when they rely on implementation-defined semantics.

## 1. Purpose and scope

A Scherzo workspace driver is one trusted, operator-configured local command. Scherzo invokes that command to prepare and check workflow workspaces, and workflow command steps or agent prompts invoke the same command for portable workspace capabilities such as `changed-files` or `assert-only`.

This specification covers:

- the workspace driver fields that select and configure a driver,
- the driver command validation contract,
- driver discovery through `describe --json`,
- lifecycle command forms,
- workflow-facing capability command forms,
- machine-readable output schemas,
- exit-code and diagnostic expectations,
- safety invariants for destructive operations and path handling,
- conformance expectations for artifact/no-op and VCS-backed drivers, and
- compatibility rules for extending the contract.

This specification does not prescribe one version-control backend, one publication host, or one sandboxing model. The bundled source-tree drivers currently covered by this contract are `scripts/scherzo-workspace-noop` and `scripts/scherzo-workspace-jj`; packaged Scherzo installs expose the same bundled drivers as `scherzo-workspace-noop` and `scherzo-workspace-jj` on `PATH`.

## 2. Glossary

**Workspace**: the directory where Scherzo runs one workflow step or a sequence of steps that share the same logical workspace name. `SCHERZO_WORKSPACE_PATH` points at this directory when Scherzo has prepared one.

**Workspace root**: the filesystem path of the current workspace. Capability paths and driver-authored diagnostics are relative to this root unless this specification explicitly says otherwise.

**Run root**: the per-run directory containing one or more logical workspaces, artifacts, and run-local state. `SCHERZO_RUN_ROOT` points at this directory when it is available.

**Workspace driver config**: operator-owned policy under `workspace.driver` and `workspace.drivers.<name>` that decides how Scherzo prepares, checks, and removes workspaces.

**Workspace driver**: the trusted local executable selected by `workspace.driver`. Built-in `noop` and `jj` drivers use bundled commands; `type: custom` entries provide a `command`.

**Lifecycle operation**: a driver operation Scherzo invokes while creating, checking, or removing a workspace. Lifecycle operations may create or delete directories and therefore have stricter target-safety requirements than read-only capabilities.

**Capability**: a named operation that workflow code may require with `workspace.requires` and invoke through the selected driver, such as `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, or `publish-commit-stack`. `publish-change` is retained only as a migration-compatible alias.

**Driver reference**: an opaque backend-specific reference string accepted by a VCS-backed driver, such as a base revision, remote branch, change id, bookmark, or hosted-review target. Scherzo treats driver references as strings unless a command in this specification narrows the accepted shape.

## 3. Workspace driver configuration schema

Workspace drivers are configured under the orchestrator `workspace` block. `workspace.driver` selects a built-in driver name (`noop` or `jj`) or a named entry under `workspace.drivers`. If `workspace.driver` is omitted, Scherzo selects the built-in `noop` driver.

```yaml
workspace:
  root: .scherzo/workspaces
  driver: isolated
  drivers:
    isolated:
      type: jj
      base: "@"
      remote: upstream
      base_branch: trunk
      fetch_base: false
      publish_remote: origin
      github_repo: example/repo
      timeout: 60s
```

Named driver entries have these fields:

| Field | Required | Contract |
| --- | --- | --- |
| `type` | yes | `noop`, `jj`, or `custom`. Built-in `noop`/`jj` names can also be selected directly with `workspace.driver`. |
| `command` | custom only | One executable token naming the trusted driver command for `type: custom`. See [command validation](#4-driver-command-validation). `command` MUST NOT be configured for `type: noop` or `type: jj`. |
| `timeout` | no | Positive duration used by Scherzo for driver lifecycle and discovery invocations. Missing defaults to `60s`. |
| `env` | no | A map of driver environment variable names to literal strings. Missing means no driver-local environment. Keys MUST match `[A-Za-z_][A-Za-z0-9_]*`, MUST be unique, and MUST NOT be Scherzo-generated variables such as `SCHERZO_WORKSPACE_DRIVER`, `SCHERZO_WORKSPACE_PATH`, or `SCHERZO_RUN_ID`. `PATH` is allowed. Values MUST be strings, including empty strings. |

`type: jj` entries MAY also use friendly fields that Scherzo maps to driver environment variables: `remote`, `base_branch`, `base`, `fetch_base`, `publish_remote`, and `github_repo`. A friendly field MUST NOT duplicate the same environment variable in `env`.

Public config MUST NOT set driver capabilities or lifecycle operations. Capabilities are discovered by invoking `<driver> describe --json` for custom drivers and are known for built-in drivers. Scherzo invokes every supported lifecycle operation (`create`, `before-step`, `after-step`, and `remove`) through the selected driver protocol.

Top-level `workspace.hooks`, `workspace.default_profile`, `workspace.profiles`, driver-local `hooks`, driver-local `lifecycle`, and `timeout_ms` are unsupported migration input; Scherzo rejects them during config loading with migration hints.

Workflow DAGs select and require workspace policy with:

```yaml
workspace:
  driver: isolated
  requires: [changed-files, assert-only]
```

A workflow MAY select a driver with `workspace.driver`. If omitted, Scherzo uses the orchestrator default. A workflow MAY require capability names with `workspace.requires`; Scherzo MUST reject loading or dispatch when the selected driver's capabilities do not include all required names.

## 4. Driver command validation

A `type: custom` `command` MUST be a non-empty string after trimming. It MUST be one executable token with no whitespace. It MUST NOT contain shell metacharacters `;`, `&`, `|`, `<`, `>`, backtick, single quote, or double quote.

A custom `command` MAY be:

- an absolute path,
- a relative path resolved according to the invocation current working directory,
- a PATH-resolved executable name, or
- `$SCHERZO_REPO_ROOT` exactly or `$SCHERZO_REPO_ROOT/<path>`.

`$SCHERZO_REPO_ROOT` is the only supported environment placeholder in a custom `command`. A command string containing any other `$` placeholder MUST be rejected by Scherzo config validation. Drivers and workflows MUST NOT rely on shell interpolation of the command; Scherzo invokes the command as an executable plus arguments, not by evaluating it as shell.

Operators SHOULD prefer absolute paths, PATH-installed wrappers, or `$SCHERZO_REPO_ROOT/...` in reusable configs. Example configs under `examples/` use `../scripts/...` because the config file lives under `examples/`; configs copied to a repository root normally use `scripts/...`.

## 5. Invocation roots, current directory, and environment

### 5.1 Discovery invocation

Before Scherzo can validate workflow capability requirements, it MUST discover each configured driver by invoking:

```text
<driver> describe --json
```

Discovery runs without a prepared workflow workspace. Scherzo runs discovery from the inferred repository root: if the config directory ends in `/.scherzo`, the inferred repository root is its parent; otherwise the inferred repository root is the config directory.

Discovery MUST use a minimal environment containing:

- `PATH` when available,
- driver-local `env` entries,
- `SCHERZO_CONFIG_DIR`,
- `SCHERZO_REPO_ROOT`, and
- `SCHERZO_WORKSPACE_DRIVER` set to the resolved driver command.

Driver `env` values are trusted operator config and are literal strings. Scherzo MUST NOT expand `$NAME` or append inherited `PATH`; `PATH: "$PATH:/tool/bin"` is passed as the literal text `$PATH:/tool/bin`. If driver `env` includes `PATH`, it replaces inherited `PATH` for discovery. Generated Scherzo variables override driver entries for the exact same key, and config validation rejects those generated keys before dispatch.

Discovery MUST NOT pass `SCHERZO_WORKSPACE_PATH` or `SCHERZO_WORKSPACE_CAPABILITIES`. A driver MUST NOT require a prepared workspace, credentials, network access, VCS access, or mutable filesystem state for `describe --json`.

### 5.2 Lifecycle invocation

For selected workspace drivers, Scherzo invokes lifecycle operations as argv commands, not shell snippets:

```text
<driver> lifecycle create
<driver> lifecycle before-step
<driver> lifecycle after-step
<driver> lifecycle remove
```

Lifecycle invocations run from the inferred repository root described above. Scherzo passes driver-local `env`, then the selected driver's resolved context plus the workflow/run/workspace environment described in [environment variables](#54-environment-variables). Drivers MUST treat `SCHERZO_WORKSPACE_PATH` as the lifecycle target. Destructive lifecycle operations MUST NOT fall back to the current directory when `SCHERZO_WORKSPACE_PATH` is missing or empty.

Public config no longer selects a lifecycle subset. Drivers SHOULD implement all four lifecycle operations; if an implementation intentionally treats an operation as a no-op, it SHOULD document that behavior and still exit successfully for the command form.

### 5.3 Capability invocation

Workflow code invokes capabilities through `SCHERZO_WORKSPACE_DRIVER`. Command steps and agent-step shells run from the prepared workspace root and receive driver-local `env` in addition to Scherzo-generated variables. A driver SHOULD interpret `SCHERZO_WORKSPACE_PATH` as the workspace root when it is set. Read-only or assertion capability commands MAY fall back to the current directory when `SCHERZO_WORKSPACE_PATH` is unset, because manually running a driver from a prepared workspace is useful for debugging.

Relative driver commands are exposed as configured or resolved by Scherzo. Workflow shell code that needs to call a simple relative driver command SHOULD resolve it against `SCHERZO_CONFIG_DIR` before invoking it, as the checked example workflows do.

### 5.4 Environment variables

Scherzo-provided driver environments may include:

| Variable | Meaning |
| --- | --- |
| `SCHERZO_CONFIG_DIR` | Directory containing the orchestrator config file. |
| `SCHERZO_REPO_ROOT` | Inferred or configured source repository root used to resolve `$SCHERZO_REPO_ROOT/...` commands. |
| `SCHERZO_WORKSPACE_DRIVER` | Resolved driver command exposed to lifecycle calls and workflow steps. |
| `SCHERZO_WORKSPACE_CAPABILITIES` | Space-separated discovered capability names in canonical order. |
| `SCHERZO_RUN_KIND` | `issue` for issue workflows or `scheduled` for scheduled workflows. |
| `SCHERZO_WORKFLOW_ID` | Workflow DAG id for issue workflow runs. |
| `SCHERZO_RUN_ID` | Scherzo run id. |
| `SCHERZO_RUN_ROOT` | Per-run directory containing logical workspaces. |
| `SCHERZO_ISSUE_ID` | Tracker-internal issue id for issue workflows. |
| `SCHERZO_ISSUE_IDENTIFIER` | Human-readable issue key, such as `LIV-231`, for issue workflows. |
| `SCHERZO_STEP_ID` | Current workflow step id. |
| `SCHERZO_ATTEMPT_INDEX` | Numeric attempt index for the current step. |
| `SCHERZO_ATTEMPT_KEY` | Stable step-attempt idempotency key. |
| `SCHERZO_HOOK_IDEMPOTENCY_KEY` | Stable lifecycle idempotency key for the run/step. |
| `SCHERZO_WORKSPACE_ROOT` | Configured workspace root from the orchestrator config. |
| `SCHERZO_WORKSPACE_PROFILE` | Selected workspace driver name; the legacy environment variable name is retained for workflow compatibility. |
| `SCHERZO_WORKSPACE_NAME` | Logical workspace name for the current step. |
| `SCHERZO_WORKSPACE_PATH` | Prepared workspace path. |
| `SCHERZO_SOURCE_WORKSPACE_NAME` | Source logical workspace name when deriving a workspace, otherwise empty. |
| `SCHERZO_SOURCE_WORKSPACE_PATH` | Source workspace path when deriving a workspace, otherwise empty. |
| `SCHERZO_SCHEDULED_JOB_ID` | Scheduled job id for scheduled runs. |
| `SCHERZO_SCHEDULE_DUE_AT` | Scheduled due timestamp for scheduled runs. |
| `SCHERZO_SCHEDULE_STARTED_AT` | Scheduled start timestamp for scheduled runs. |
| `SCHERZO_RUN_ATTEMPT` | Scheduled-run attempt number. |

Drivers MUST tolerate absent optional variables. Driver `env` is not a secret store. Its values are visible to discovery, lifecycle, command-step, and agent-step subprocesses, may be printed by those subprocesses, and are included in execution fingerprints only as key names plus SHA-256 value digests under `value_sha256`. Prompt templates do not receive a `workspace.env` map. Scherzo applies limited redaction to Scherzo-owned diagnostics and artifacts only for likely-sensitive driver env keys such as `SECRET`, `TOKEN`, `PASSWORD`, `API_KEY`, `ACCESS_KEY`, `PRIVATE_KEY`, or `SESSION_KEY`, and only for non-empty values of at least eight characters. Drivers MUST NOT print secrets. Driver-authored output SHOULD avoid local absolute workspace roots unless it is relaying bounded output from an underlying tool failure.

## 6. Metadata command: `describe --json`

Every workspace driver MUST implement:

```text
<driver> describe --json
```

The command MUST be side-effect-free. It MUST print exactly one JSON object followed by a newline to stdout and exit 0. It MUST NOT inspect or mutate a workflow workspace, invoke a VCS, require credentials, contact a network service, or depend on `SCHERZO_WORKSPACE_CAPABILITIES`.

Version 1 has this schema:

```json
{"version":1,"capabilities":["status","changed-files","assert-only"]}
```

Required fields:

| Field | Contract |
| --- | --- |
| `version` | Integer `1`. |
| `capabilities` | List of unique strings from the fixed capability vocabulary: `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, `publish-change`, and `publish-commit-stack`. |

The capability list MAY be in any order. Scherzo canonicalizes it before validation, prompt rendering, environment exposure, and fingerprinting. Malformed JSON, missing fields, unsupported versions, unknown capability names, duplicate capability names, non-string entries, nonzero exit, empty stdout, and timeout are workflow-config load failures. `publish-commit-stack` is the same-repo `commit_stack` publication successor capability; drivers may continue to advertise `publish-change` for the migration-compatible path until the command contract is fully renamed.

## 7. Lifecycle commands

### 7.1 `lifecycle create`

`lifecycle create` prepares `SCHERZO_WORKSPACE_PATH` for the upcoming step or shared logical workspace. A driver that owns creation MUST ensure the directory exists on success. It MAY populate the workspace from a source checkout, derive from `SCHERZO_SOURCE_WORKSPACE_PATH`, initialize VCS metadata, or create marker files needed for later safety checks.

The operation MUST be idempotent for the same run/workspace target when possible. If the target is unsafe or cannot be prepared, the driver MUST exit nonzero with a bounded diagnostic.

### 7.2 `lifecycle before-step`

`lifecycle before-step` checks that the prepared workspace is usable before a step runs. It SHOULD verify backend-specific invariants such as directory existence, expected VCS metadata, marker files, or source-workspace linkage. If no check is needed, the driver MAY implement it as a successful no-op.

### 7.3 `lifecycle after-step`

`lifecycle after-step` performs post-step validation or cleanup. It MAY be a successful no-op. A driver SHOULD avoid destructive cleanup here unless that cleanup is deterministic, scoped to the workspace, and cannot remove workflow outputs needed by downstream steps.

### 7.4 `lifecycle remove`

`lifecycle remove` performs driver-specific cleanup before or during run cleanup. It is destructive. A driver implementing it MUST require a non-empty explicit `SCHERZO_WORKSPACE_PATH`, MUST NOT use the current directory as a fallback target, MUST reject filesystem root as a target, and SHOULD verify that the target is contained by `SCHERZO_RUN_ROOT` when that variable is set.

Artifact/no-op drivers SHOULD use a private marker written during `lifecycle create` and MUST refuse to remove a directory that does not contain the expected marker or otherwise fails the driver's ownership check.

## 8. Workflow-facing capability commands

A driver MUST implement every command form associated with each capability it reports from `describe --json`. Unsupported operations or unsupported flags MUST exit 2.

### 8.1 `status`

A driver that advertises `status` MUST implement:

```text
<driver> status --human
```

The command prints a bounded human-oriented summary of workspace state. Workflow automation MUST NOT parse this output. It is for operator and agent orientation.

### 8.2 `diff`

A driver that advertises `diff` MUST implement:

```text
<driver> diff --human
<driver> diff --json
```

`diff --human` prints a bounded human-oriented diff. Workflow automation SHOULD NOT parse this output.

`diff --json` prints exactly one JSON object followed by a newline:

```json
{"version":1,"format":"unified","text":"diff --git ...\n","truncated":false}
```

Required fields:

| Field | Contract |
| --- | --- |
| `version` | Integer `1`. |
| `format` | Diff format string. Current bundled VCS-backed drivers use `unified`. |
| `text` | Diff text as a string. |
| `truncated` | Boolean indicating whether `text` was truncated by the driver. |

### 8.3 `changed-files`

A driver that advertises `changed-files` MUST implement:

```text
<driver> changed-files --json
```

The command prints exactly one JSON object followed by a newline:

```json
{"version":1,"files":[{"path":"research-findings.md","status":"modified"}]}
```

Required fields:

| Field | Contract |
| --- | --- |
| `version` | Integer `1`. |
| `files` | List of changed-file records sorted lexicographically by `path` and deduplicated by path. |

Each file record MUST contain:

| Field | Contract |
| --- | --- |
| `path` | Non-empty workspace-root-relative path. It MUST NOT be absolute and MUST NOT contain `..` as a complete path segment. |
| `status` | One of `added`, `modified`, `deleted`, `renamed`, or `conflicted`. |

A renamed record SHOULD include `old_path` with the previous workspace-root-relative path. Drivers MAY include additional adapter-specific fields; consumers MUST ignore unknown fields unless a workflow-specific policy says otherwise.

For artifact/no-op workspaces, the changed-file set is every regular file under the workspace root except private driver markers and Scherzo scratch/diagnostic directories such as `.scherzo`. Because there is no VCS baseline, such drivers report present files with `status: "modified"`.

For VCS-backed workspaces, the changed-file set is the backend's view of changes from the prepared baseline to the current workspace revision.

### 8.4 `assert-only`

A driver that advertises `assert-only` MUST implement:

```text
<driver> assert-only --path <relative-file>
```

The command succeeds only when the changed-file set is exactly the provided relative path. It MUST fail when the file is absent, when any additional file is changed or present according to the selected driver's baseline semantics, or when the path argument is unsafe.

The path argument MUST be non-empty, MUST NOT be `.`, MUST NOT be absolute, MUST NOT use a Windows drive absolute form such as `C:\path`, and MUST NOT contain `..` as a complete path segment. A driver MUST reject unsafe path arguments before filesystem or VCS inspection and exit 2.

### 8.5 `baseline`

A VCS-backed driver that advertises `baseline` MUST implement:

```text
<driver> baseline --json
```

The command prints exactly one JSON object followed by a newline:

```json
{
  "version": 1,
  "baseline_id": "base-change-or-revision",
  "workspace_revision_id": "current-revision",
  "change_id": "current-change",
  "dirty": true
}
```

Required fields:

| Field | Contract |
| --- | --- |
| `version` | Integer `1`. |
| `baseline_id` | Non-empty backend-specific id for the prepared baseline. |
| `workspace_revision_id` | Non-empty backend-specific id for the current workspace revision. |
| `change_id` | Non-empty backend-specific id for the mutable workspace change. |
| `dirty` | Boolean indicating whether `changed-files --json` would report any records. |

### 8.6 `refresh-base`

A VCS-backed driver that advertises `refresh-base` MUST implement:

```text
<driver> refresh-base --stage <stage> [--target <driver-ref>] --json
```

`--stage` is a workflow-provided label used for diagnostics and artifact names. It MUST match `^[A-Za-z0-9][A-Za-z0-9._-]*$`. `--target` is OPTIONAL and MUST be non-empty when present. Drivers SHOULD treat `--target` as an opaque driver reference.

On success, the command prints exactly one JSON object followed by a newline and exits 0:

```json
{
  "version": 1,
  "status": "fresh",
  "stage": "pre-validation",
  "base_ref": "main",
  "base_revision": "main@origin",
  "before_revision": "old-head",
  "after_revision": "new-head",
  "conflict_files": []
}
```

Successful `status` values are:

- `fresh`: the workspace was already based on the selected base.
- `rebased_clean`: the driver refreshed the workspace base without unresolved conflicts.

On a valid request that cannot be completed, the driver SHOULD print a version 1 JSON failure object and exit nonzero. Current failure status values are:

- `fetch_failed`,
- `base_not_found`,
- `rebase_failed`, and
- `conflicts`.

Failure JSON MUST include `version`, `status`, `failure_code`, and `message`. When available, it SHOULD also include `stage`, `base_ref`, `base_revision`, `before_revision`, `after_revision`, and `conflict_files`. A driver MAY use a command-specific nonzero exit code for operator classification; the bundled jj driver exits `20` for `status: "conflicts"`.

### 8.7 `publish-commit-stack`

A VCS-backed driver that advertises `publish-commit-stack` MUST implement the commit-stack publication command. A driver MAY also advertise `publish-change` as a migration-compatible alias with the same hosted-review semantics, but checked-in workflows should require `publish-commit-stack` for same-repository GitHub publication.

```text
<driver> publish-commit-stack --kind <kind> --title-file <path> --body-file <path> --branch-prefix <prefix> --base <driver-ref> [--target-branch <branch>] [--target-pr <number>] [--expected-head <sha>] [--allow-no-changes <true|false>] --json
```

Required arguments:

| Argument | Contract |
| --- | --- |
| `--kind` | Workflow publication kind. The bundled jj driver accepts lowercase workflow kind tokens made from letters, numbers, and hyphens, for example `implementation`, `execplan`, `execplan-revision`, `execplan-implementation`, and `merge-conflict`. |
| `--title-file` | Workspace-root-relative file path containing the hosted-review title. |
| `--body-file` | Workspace-root-relative file path containing the hosted-review body. |
| `--branch-prefix` | Prefix the driver may use when creating a publication branch/bookmark. For stable same-repository `publish-commit-stack` requests without `--target-branch`, the bundled jj driver treats this value as the stable publication branch so retries reconcile with the same PR/branch instead of creating a new suffixed branch. The stable branch must be distinct from `--base` (before any `@remote` suffix) and must not be `HEAD`. |
| `--base` | Base driver reference used for hosted-review base metadata. |
| `--json` | Required output selector. |

Optional arguments:

| Argument | Contract |
| --- | --- |
| `--target-branch` | Existing branch/bookmark to update instead of creating a new one. |
| `--target-pr` | Existing hosted-review identifier to update or inspect. |
| `--expected-head` | Optional expected head Git SHA for commit-stack publication safety checks. |
| `--allow-no-changes` | Literal `true` or `false`; missing defaults to `false`. |

`--title-file` and `--body-file` MUST be validated as safe workspace-root-relative paths before reading. Drivers MUST NOT read arbitrary absolute paths or paths containing `..` segments for these arguments. Drivers that create GitHub PRs MUST honor `SCHERZO_PR_DRAFT=true|false` when present: `true` creates a draft PR, `false` creates a ready-for-review PR, and unset preserves the driver's current default PR draft behavior. Any other non-empty value MUST fail before publication with a clear configuration diagnostic.

On success, the command prints exactly one JSON object followed by a newline:

```json
{
  "version": 1,
  "status": "published",
  "url": "https://example.invalid/review/123",
  "branch": "scherzo/implementation/example",
  "base_ref": "main@origin",
  "base_revision": "main@origin",
  "head_revision": "new-head",
  "change_id": "change",
  "created": true,
  "updated": false
}
```

Successful `status` values are:

- `published`: a new hosted review was created.
- `updated`: an existing hosted review or branch/bookmark was updated.
- `unchanged`: no publishable changes were present and `--allow-no-changes true` permitted that outcome.

`version`, `status`, `url`, `branch`, `base_ref`, `base_revision`, `head_revision`, `change_id`, `created`, and `updated` are required fields. `url` MUST be non-empty for `published` and `updated`. For `unchanged`, `url` SHOULD name the existing hosted review when known and MAY be empty only when no hosted review target was supplied or found. `created` and `updated` MUST be booleans.

On a valid request that cannot be completed, the driver SHOULD print a version 1 JSON failure object with `status`, `failure_code`, and `message`, then exit nonzero. The legacy `publish-change` alias, when advertised, uses the same required fields and output contract but omits commit-stack-only safety arguments such as `--expected-head`.

## 9. Machine-readable output rules

Every command with a `--json` flag MUST write one JSON object to stdout followed by a newline. It MUST NOT write non-JSON preambles, progress text, or trailing records to stdout. Diagnostics and progress for failures SHOULD go to stderr unless represented inside the JSON object.

All version 1 JSON outputs MUST include `"version": 1`. Consumers MUST reject missing or unsupported versions for commands they parse. Consumers SHOULD ignore unknown fields in otherwise valid version 1 objects so drivers can add non-breaking metadata.

Machine-readable paths MUST be workspace-root-relative. They MUST NOT include local absolute workspace roots. They MUST use `/` as the path separator in JSON outputs.

## 10. Exit codes and diagnostics

Drivers SHOULD use these exit-code categories unless a command-specific status above documents a different nonzero code:

| Exit code | Meaning |
| --- | --- |
| `0` | The requested operation succeeded. |
| `1` | The request was syntactically valid, but the operation failed or an assertion did not hold. Examples: `assert-only` found extra files, the workspace directory did not exist, a backend command failed, or there was nothing to publish when `--allow-no-changes false`. |
| `2` | Usage error, unsupported operation, unsupported flag, malformed argument, unsafe path argument, or refused destructive target. |

Diagnostics MUST be bounded. Driver-authored diagnostics SHOULD name workspace-root-relative paths. They SHOULD NOT include local absolute workspace roots, run roots, token values, credential material, or unbounded backend output. It is acceptable to relay a bounded excerpt of an underlying tool failure when that excerpt is necessary to diagnose the failure.

Usage text MUST be bounded and SHOULD list the supported command forms. Unsupported commands and unsupported flags MUST exit 2.

## 11. Safety invariants

Drivers are trusted operator commands, but they are still part of Scherzo's safety boundary. A conforming driver MUST preserve these invariants:

1. Destructive operations MUST require an explicit target from `SCHERZO_WORKSPACE_PATH` or an equally explicit command argument defined by this specification.
2. `lifecycle remove` MUST NOT fall back to the current directory.
3. Destructive operations MUST reject empty targets, filesystem root targets, and targets outside `SCHERZO_RUN_ROOT` when containment can be verified.
4. Drivers SHOULD maintain ownership evidence, such as marker files for artifact/no-op workspaces or backend metadata for VCS workspaces, before deleting a directory.
5. Path arguments from workflow code MUST be validated before filesystem or backend inspection.
6. Path arguments MUST reject empty paths, `.`, absolute paths, Windows drive absolute paths, and `..` path segments.
7. Machine-readable output MUST use workspace-root-relative paths.
8. `describe --json` MUST be side-effect-free and independent of workspace state, network state, and credentials.
9. Drivers MUST NOT evaluate workflow-provided strings as shell. When a driver invokes backend tools, it SHOULD pass arguments as argv arrays rather than shell snippets.
10. Drivers MUST keep diagnostics and relayed backend output bounded.

## 12. Driver conformance classes

### 12.1 Artifact/no-op driver

An artifact/no-op driver is suitable for workflows that only need an empty workspace and final artifact assertions. A conforming artifact/no-op driver SHOULD:

- implement lifecycle `create`, `before-step`, `after-step`, and `remove`,
- create the workspace directory during `lifecycle create`,
- write a private ownership marker during `lifecycle create`,
- verify directory existence during `before-step` and `after-step`,
- refuse `remove` unless the ownership marker is present,
- report capabilities `status`, `changed-files`, and `assert-only`,
- report every regular file under the workspace root as a changed file except private driver markers and Scherzo scratch/diagnostic directories, and
- reject `diff`, `baseline`, `refresh-base`, `publish-change`, and `publish-commit-stack` with exit code 2.

`scripts/scherzo-workspace-noop` is the bundled artifact/no-op driver, and packaged installs expose the same command as `scherzo-workspace-noop`. It reports present files with `status: "modified"` because it has no VCS baseline.

### 12.2 VCS-backed driver

A VCS-backed driver is suitable for workflows that run in a source workspace and need changed-file inventory, diffs, base refresh, or publication. A conforming VCS-backed driver SHOULD:

- implement lifecycle `create`, `before-step`, `after-step`, and `remove` directly, or document any lifecycle operations intentionally unsupported,
- preserve a clear baseline for each workspace,
- report capabilities matching the operations it actually implements,
- implement `status`, `diff`, `changed-files`, `assert-only`, and `baseline` against the prepared baseline,
- implement `refresh-base`, `publish-change`, and `publish-commit-stack` only when the backend and operator credentials support those operations,
- keep changed-file and diff semantics stable enough for workflow validation, and
- document which ignored files, generated files, caches, and backend metadata appear in `changed-files`.

`scripts/scherzo-workspace-jj` is the bundled VCS-backed driver for Scherzo dogfood workspaces, and packaged installs expose the same command as `scherzo-workspace-jj`. It implements lifecycle `create`, `before-step`, `after-step`, and `remove` directly for jj workspaces, including workspace creation, verification, optional direnv trust, and workspace-forget cleanup. It reports capabilities `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, `publish-commit-stack`, and the compatibility alias `publish-change` from `describe --json`. Its changed-file view is the jj current-change diff (`jj diff` without explicit revision arguments), enriched from `jj diff --summary` when status details are available; this preserves the normal prepared-parent baseline for single-parent changes while avoiding ambiguous `@-` revsets for merge-resolution workspaces. Root workspace base selection is operator policy supplied through driver environment such as `SCHERZO_JJ_WORKSPACE_BASE`, `SCHERZO_JJ_WORKSPACE_REMOTE`, `SCHERZO_JJ_WORKSPACE_BASE_BRANCH`, and `SCHERZO_JJ_WORKSPACE_FETCH_BASE`; publication remote selection is separate through `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE`. GitHub repository publication can be pinned with `SCHERZO_GITHUB_REPO=owner/repo` when remote URL inference is not enough. Legacy `SCHERZO_PR_REMOTE` and `SCHERZO_PR_BASE` are not jj driver configuration inputs; commit-stack publication fails closed when only the legacy publication remote is present.

## 13. Compatibility and versioning

The `describe --json` response version and all versioned command JSON objects currently use integer `1`.

Adding optional fields to a version 1 JSON object is backward compatible. Consumers MUST ignore unknown fields in a recognized version 1 object unless a workflow-specific policy requires stricter validation. Removing required fields, changing field types, changing existing status meanings, or changing path-root semantics is not backward compatible and requires a new version or a coordinated Scherzo release.

The capability vocabulary is fixed for a Scherzo release. Drivers MUST NOT advertise capability names unknown to the Scherzo version that will load them; current Scherzo versions reject unknown names during discovery. To add a future capability, Scherzo MUST first add the capability name to the vocabulary and validation logic, then drivers MAY advertise it and document its command forms.

A driver MAY implement experimental commands that it does not advertise, but workflows MUST NOT require or depend on unadvertised capabilities. Once a capability is advertised, the driver MUST preserve the command forms and output contracts in this specification for that capability until a documented version transition supersedes them.
