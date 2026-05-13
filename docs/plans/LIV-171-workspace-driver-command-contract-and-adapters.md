# Define the initial workspace driver command contract and adapters

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this plan is implemented, Scherzo will have a small, tested workspace driver command contract plus two concrete adapters that later workflow-profile work can wire into real runs. Operators will be able to point a driver-backed workspace profile at a jj adapter for Scherzo dogfood workspaces or at a no-op artifact adapter for workflows that only need an empty directory and a final file assertion. Workflow authors will be able to rely on a stable command shape for lifecycle operations and the first workflow-facing capabilities instead of hardcoding `jj` in every portable workflow.

The visible proof is deliberately modest and concrete. A developer can run the new adapter tests from the repository root and see that `scripts/scherzo-workspace-jj` prepares and inspects a jj workspace through the same lifecycle behavior currently provided by `scripts/scherzo-jj-workspace`, while `scripts/scherzo-workspace-noop` creates an empty workspace and enforces an `assert-only` artifact contract without needing jj. This plan does not migrate `.scherzo/scherzo.yaml`, does not change any checked-in workflow YAML, and does not change the runtime profile parser unless the prerequisite schema plan has already provided an explicit extension point that this plan must use.

## Problem Framing and Constraints

Scherzo currently has workflow DAG files under `.scherzo/workflows/` and examples under `examples/workflows/`. The dogfood configuration in `.scherzo/scherzo.yaml` still uses legacy direct `workspace.hooks` that shell out to `scripts/scherzo-jj-workspace`. That helper is useful, but it is a hook helper rather than a workflow-facing command contract: it accepts `after-create`, `before-run`, and `before-remove`, and it assumes jj-specific behavior. Portable workflows need a way to say, "the selected workspace profile can assert that only `research-findings.md` changed," without knowing whether the workspace is jj-backed, git-backed, copied, or simply empty.

The source umbrella design defines a workspace driver as a trusted operator-configured command. Scherzo invokes the driver for lifecycle operations such as creating and removing workspaces, and workflow command steps may invoke the same driver for declared capabilities such as listing changed files or asserting that only one artifact exists. This child plan is the first adapter slice of that umbrella: it makes the command contract executable and testable, but it avoids the broader parser, runtime exposure, dogfood migration, and portable research workflow changes that belong to separate child plans.

The main constraints are portability, safety, and small scope. All capability paths and adapter-authored diagnostics must use workspace-root-relative paths rather than absolute local paths. For a VCS-backed workspace, the workspace root may also be the repository root reported by the VCS; for an artifact-only workspace, it is simply the workspace directory. Driver commands are trusted local shell configured by the operator, not commands supplied by workflow YAML. The adapters must not print secrets or unbounded command output. The jj adapter should reuse existing behavior where safe instead of reimplementing jj workspace creation from scratch. The no-op adapter must be genuinely no-op with respect to version control: it may create, inspect, and remove a directory, but it must not require `jj`, `git`, network access, or a source repository.

## Strategy Overview

Implement the contract as executable scripts plus a short reference document, and test those scripts directly. Add `scripts/scherzo-workspace-jj` as the jj-backed driver endpoint. Its lifecycle subcommands translate to the existing `scripts/scherzo-jj-workspace` helper, preserving the current dogfood workspace behavior. Its workflow-facing subcommands use jj read-only commands for `status`, `diff`, and `changed-files`, and use the changed-file list for `assert-only`.

Add `scripts/scherzo-workspace-noop` as the no-op or artifact-only driver endpoint. It creates an empty workspace directory, treats the set of regular files under that directory as the output file set, and supports `status`, `changed-files`, and `assert-only`. It intentionally does not support `diff` because there is no baseline to diff against in an empty artifact workspace.

Document the initial contract in `docs/runbooks/workspace-driver-contract.md`. The document is not a migration guide; it is the adapter contract reference that later schema and workflow plans can link to. The contract fixes the command shapes, exit-code categories, JSON output for `changed-files`, path-safety rules for `assert-only`, and the lifecycle environment variables the adapters consume.

This approach is proportionate because it creates the smallest real driver surface that can support the portable research workflow and the dogfood jj lifecycle migration. It avoids prematurely abstracting publish or base-refresh behavior, which is tied to heavier implementation workflows and has more policy choices than this adapter slice needs to settle.

## Alternatives Considered

The simplest alternative is to only document the contract and wait to write adapters until the runtime schema lands. That is insufficient because the contract would remain aspirational. The hard parts of this slice are command arguments, path normalization, exit codes, and adapter semantics; those can be tested before the runtime parser is ready.

Another alternative is to modify `scripts/scherzo-jj-workspace` in place so it accepts both old hook verbs and new driver verbs. That would reduce the number of files, but it would blur the current hook helper with the new contract. Keeping `scripts/scherzo-workspace-jj` as a thin wrapper lets existing legacy hooks continue unchanged while the driver command grows independently.

A third alternative is to implement `baseline`, `refresh-base`, and `publish-change` now because the umbrella lists them as future capability names. This plan rejects that for the first adapter slice. `refresh-base` and `publish-change` involve remote fetches, rebases, branch or bookmark names, external review systems, and recovery policy. Those choices are better handled in the later implementation-workflow conversion plan. This plan may reserve those names in documentation, but the initial adapters should not advertise or silently implement partial versions of them.

A fourth alternative is to make the no-op adapter support `diff` by printing file contents. That creates misleading semantics because there is no stable baseline. A no-op artifact workflow needs `changed-files` and `assert-only`, not a fake diff.

## Risks and Countermeasures

The first risk is contract drift between scripts, docs, and future runtime capability validation. Countermeasure: write the contract document and tests in the same implementation milestone, use the exact same command names in both, and keep unsupported operations returning the same usage error rather than silently succeeding.

The second risk is that the jj adapter changes dogfood workspace lifecycle behavior accidentally. Countermeasure: delegate lifecycle operations to `scripts/scherzo-jj-workspace` and add a local integration smoke test that calls the new driver through legacy hook slots. Existing `test/jj_workspace_hook_test.gleam` remains the detailed coverage for the old helper.

The third risk is leaking absolute local workspace paths in user-facing diagnostics. Countermeasure: contract tests must assert that `assert-only` diagnostics name workspace-root-relative changed files and do not include the workspace root. When an underlying tool fails, the adapter may include bounded stderr, but adapter-authored messages should not print literal local roots.

The fourth risk is destructive lifecycle removal outside the intended workspace. Countermeasure: the no-op adapter must require an explicit, non-empty `SCHERZO_WORKSPACE_PATH` for lifecycle `create` and `remove`; `lifecycle create` must place a private marker file inside the workspace; and `lifecycle remove` must refuse to delete unless that marker is present. When `SCHERZO_RUN_ROOT` is set, `lifecycle remove` must also verify that the workspace path is contained by that run root before deleting anything. Capability commands may still use the current directory fallback for read-only inspection, but destructive lifecycle verbs must not.

The fifth risk is path traversal or accidental assertions outside the workspace. Countermeasure: both adapters must reject empty paths, absolute paths, `.` as a target file, and any path with `..` segments before inspecting the filesystem or running jj path filters. Tests must construct an absolute path dynamically and ensure it is rejected without putting a literal absolute path in the repository.

The sixth risk is overfitting the no-op adapter to an empty workspace and later using it with copied source trees. Countermeasure: document that the no-op adapter is an artifact workspace driver. It assumes lifecycle `create` starts from an empty directory and `changed-files` means "regular files present under the workspace," not VCS changes. A copy-based or git-backed driver should be added later if a workflow needs source-tree semantics without jj.

## Progress

- [x] (2026-05-09 00:00Z) Read the repository-local ExecPlan authoring guidance and drafted this plan from the umbrella design plus current tree inspection.
- [x] (2026-05-10 16:50Z) Implemented `docs/runbooks/workspace-driver-contract.md` with the initial command contract.
- [x] (2026-05-10 16:50Z) Added `scripts/scherzo-workspace-noop` and direct contract tests for lifecycle, `changed-files`, `status`, and `assert-only`.
- [x] (2026-05-10 16:50Z) Added `scripts/scherzo-workspace-jj` and direct contract tests for lifecycle delegation, `status`, `diff`, `changed-files`, and `assert-only`.
- [x] (2026-05-10 16:50Z) Added a jj driver local integration smoke test that exercises the new lifecycle verbs through existing hook execution.
- [x] (2026-05-10 16:50Z) Ran formatting, unit tests, glinter, and Scherzo lint from the repository root.
- [x] (2026-05-10 16:50Z) Updated Outcomes & Retrospective with the final command shapes, deviations from this plan, and validation evidence.
- [x] (2026-05-10 23:55Z) Review hardening made `scripts/scherzo-workspace-jj lifecycle remove` reject missing or empty `SCHERZO_WORKSPACE_PATH` before delegating to the legacy helper, added a regression test, and reran the unit and local-integration suites.

## Surprises & Discoveries

- Observation: The implementation base already includes the prerequisite workspace driver schema types, so this adapter slice did not need production Gleam changes for profile parsing.
  Evidence: `src/scherzo/config/types.gleam` defines `WorkspaceDriverConfig`, `WorkspaceLifecycleOperation`, `WorkspaceCapability`, and `WorkspaceHookProfile(name, hooks, driver, source)`.

- Observation: The checked-in dogfood configuration still uses direct legacy `workspace.hooks` and calls the jj hook helper directly.
  Evidence: `.scherzo/scherzo.yaml` has `workspace.hooks.create`, `workspace.hooks.before_step`, `workspace.hooks.after_step`, and `workspace.hooks.remove` entries that invoke `scripts/scherzo-jj-workspace`.

- Observation: The current jj hook helper already implements the lifecycle behavior that the jj driver should preserve.
  Evidence: `scripts/scherzo-jj-workspace` accepts `after-create`, `before-run`, and `before-remove`; it creates jj workspaces, verifies `jj status --color=never`, allows trusted `.envrc`, and forgets run workspaces during removal.

- Observation: Tests already use `command_step.run` and fake scripts on `PATH` to validate shell helpers without relying on a real external service.
  Evidence: `test/jj_workspace_hook_test.gleam` writes a fake `jj`, runs `scripts/scherzo-jj-workspace`, and asserts exact command-line behavior.

- Observation: The current command-step environment does not yet expose a workspace driver command.
  Evidence: `src/scherzo/workflow_run.gleam` builds `step_command_env` with workflow, run, issue, step, attempt, workspace name, and workspace path fields, but not `SCHERZO_WORKSPACE_DRIVER`.

- Observation: Command-step stdout diagnostics create transient files under `.scherzo/command-step-diagnostics`; the no-op adapter must exclude `.scherzo` scratch files from artifact changed-file output.
  Evidence: `src/scherzo/command_step.gleam` prepares `.scherzo/command-step-diagnostics/<step>.stdout.raw`, and `scripts/scherzo-workspace-noop changed-files --json` now ignores `.scherzo` while tests assert only user artifact files.

- Observation: An earlier full test run printed one `erl_child_setup: failed with error 32 on line 284` line during an existing broad suite path, but the runner completed successfully and a later full run did not repeat the line.
  Evidence: The final `direnv exec . gleam test` run ended with `1069 passed, no failures`.

- Observation: The first review pass found that `scripts/scherzo-workspace-jj lifecycle remove` delegated to the legacy helper before checking whether `SCHERZO_WORKSPACE_PATH` was explicit.
  Evidence: The workspace driver contract forbids current-directory fallback for destructive lifecycle removal; `jj_driver_lifecycle_remove_requires_explicit_workspace_path_test` now covers unset and empty workspace paths without invoking fake `jj`.

## Decision Log

- Decision: The initial implemented capability set is `status`, `diff`, `changed-files`, and `assert-only` for the jj driver, and `status`, `changed-files`, and `assert-only` for the no-op driver.
  Rationale: These capabilities are enough to test the command contract and support the portable research artifact use case. Base refresh and publication are higher-risk workflow lifecycle operations that should wait for the implementation-workflow conversion plan.
  Date: 2026-05-09

- Decision: Add new driver scripts instead of changing `scripts/scherzo-jj-workspace` to be the public driver command.
  Rationale: The existing script is a lifecycle hook helper with legacy verbs. A wrapper keeps existing behavior stable and gives the new contract a clear endpoint.
  Date: 2026-05-09

- Decision: The no-op adapter treats regular files under the workspace as its changed-file set.
  Rationale: A no-op artifact workspace has no VCS baseline. For research-style workflows that start in an empty directory, the observable output set is exactly the files present at collection time.
  Date: 2026-05-09

- Decision: `changed-files --json` returns a JSON array of workspace-root-relative path strings, sorted lexicographically and deduplicated.
  Rationale: A bare array is the smallest machine-readable shape for command steps such as `assert-only`. Sorting and deduplication make tests deterministic and avoid leaking adapter-specific ordering. Defining the root as the workspace root gives the no-op artifact adapter and the jj adapter one shared interpretation.
  Date: 2026-05-09

- Decision: The no-op adapter protects destructive `lifecycle remove` with an explicit workspace path, an optional run-root containment check, and a private marker file created by `lifecycle create`.
  Rationale: A current-directory fallback is convenient for read-only capabilities but unsafe for deletion. Marker-based protection keeps the adapter no-op with respect to version control while preventing a missing environment variable or wrong working directory from deleting the caller's checkout.
  Date: 2026-05-09

- Decision: Unsupported operations return exit code 2 with usage text; contract or workspace failures return exit code 1; success returns 0.
  Rationale: This mirrors common shell conventions and lets command steps distinguish caller misuse from a valid assertion failure.
  Date: 2026-05-09

- Decision: Implement the new adapter commands as shell-launchable Python standard-library programs, with shell shebang wrappers that work both as executable commands and when invoked as `sh <script>` by legacy hooks.
  Rationale: Python provides correct JSON encoding and filesystem path handling for names containing spaces, quotes, and backslashes without adding a package dependency. The shell wrapper preserves the hook invocation style used by existing tests and smoke workflows.
  Date: 2026-05-10

- Decision: Locate the legacy jj helper as a sibling of `scripts/scherzo-workspace-jj`, not under `SCHERZO_REPO_ROOT`.
  Rationale: `SCHERZO_REPO_ROOT` names the coordinating jj repository for lifecycle operations. The local integration smoke deliberately sets it to a temporary jj repository that does not contain Scherzo scripts, so the adapter must use its own script location to find `scripts/scherzo-jj-workspace` while passing `SCHERZO_REPO_ROOT` through for jj operations.
  Date: 2026-05-10

- Decision: Exclude `.scherzo` scratch files from the no-op adapter's changed-file set in addition to the private marker.
  Rationale: Scherzo command-step diagnostics can create transient `.scherzo/command-step-diagnostics` files while a command runs. A no-op artifact driver should report workflow artifacts, not Scherzo's own diagnostics.
  Date: 2026-05-10

- Decision: Do not create a manual jj commit from this implementation workflow.
  Rationale: The Scherzo `workflow:execplan-implementation` contract for LIV-183 says not to create commits; the publish step creates the final logical jj commit after review and validation.
  Date: 2026-05-10

- Decision: Require an explicit, non-empty `SCHERZO_WORKSPACE_PATH` for `scripts/scherzo-workspace-jj lifecycle remove` before delegating to `scripts/scherzo-jj-workspace before-remove`.
  Rationale: The legacy helper falls back to the current directory when the workspace path is missing, but the new driver contract requires destructive lifecycle removal to have an explicit target.
  Date: 2026-05-10

## Outcomes & Retrospective

Implementation completed the initial adapter contract without changing runtime configuration or production Gleam code. `docs/runbooks/workspace-driver-contract.md` now defines lifecycle and capability command shapes, exit codes, path-safety rules, output formats, supported adapter capability sets, and reserved future capability names.

`scripts/scherzo-workspace-noop` now implements an artifact-only driver. It requires an explicit `SCHERZO_WORKSPACE_PATH` for lifecycle commands, writes `.scherzo-workspace-driver-noop` during create, refuses unsafe remove targets, reports sorted JSON regular-file artifacts for `changed-files --json`, prints deterministic human status, and enforces `assert-only --path <relative-file>` with relative diagnostics.

`scripts/scherzo-workspace-jj` now implements the jj-backed driver. It delegates `lifecycle create`, `lifecycle before-step`, and `lifecycle remove` to `scripts/scherzo-jj-workspace`, treats `lifecycle after-step` as a successful no-op, delegates human `status` and `diff` to jj, and implements sorted JSON `changed-files --json` plus `assert-only` from `jj diff --from @- --to @ --name-only --color=never`.

The test suite now has direct adapter tests in `test/workspace_driver_contract_test.gleam` and `test/jj_workspace_driver_test.gleam`, plus a driver lifecycle smoke test added to `test/local_integration/workflow_jj_workspace_smoke_test.gleam`. Validation from the repository root passed with `direnv exec . gleam test` reporting `1069 passed, no failures`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter` reporting `0 errors` with the existing warning inventory, and `direnv exec . gleam run -m scherzo_lint` exiting 0 with the same existing warning inventory.

Review hardening added an explicit-target guard for `scripts/scherzo-workspace-jj lifecycle remove` and a regression test for unset or empty `SCHERZO_WORKSPACE_PATH`. Targeted post-review validation passed with `direnv exec . gleam format --check test/jj_workspace_driver_test.gleam`, `direnv exec . gleam test --target erlang -- --suite unit` reporting `1070 passed, no failures`, and `direnv exec . gleam test --target erlang -- --suite local-integration` reporting `2 passed, no failures`.

## Context and Orientation

Scherzo is a Gleam application that dispatches issues into workflow DAGs. A workflow DAG is a YAML file, such as `.scherzo/workflows/research.yaml`, that defines agent and command steps. Each step runs in a workspace directory prepared by Scherzo. A workspace profile is operator configuration that says how those workspaces are created, checked before steps, and removed afterward.

The current repository has two related but separate workspace concepts. Runtime code now has named workspace profiles and typed driver profile schema, but runtime driver invocation and command-step exposure are still separate follow-up work. The dogfood configuration uses the legacy direct `workspace.hooks` shape rather than named driver profiles. The current jj lifecycle helper is `scripts/scherzo-jj-workspace`, and it uses environment variables such as `SCHERZO_WORKSPACE_PATH`, `SCHERZO_RUN_ROOT`, `SCHERZO_REPO_ROOT`, `SCHERZO_SOURCE_WORKSPACE_PATH`, `SCHERZO_WORKFLOW_ID`, `SCHERZO_ISSUE_IDENTIFIER`, `SCHERZO_RUN_ID`, and `SCHERZO_WORKSPACE_NAME`.

A workspace driver is the next abstraction from the umbrella design. It is one trusted command configured by the operator. Scherzo will call it for lifecycle operations, and workflow command steps will call it for workflow-facing capabilities. This plan concerns the command contract and adapter scripts only. The runtime parser and validation changes that understand `workspace.profiles.<name>.driver` and workflow `workspace_capabilities` have landed before this adapter slice; runtime invocation, dogfood config migration, and portable research workflow updates remain separate child plans.

The important current files are:

- `scripts/scherzo-jj-workspace`, the existing jj lifecycle hook helper that the new jj driver should delegate to for lifecycle operations.
- `.scherzo/scherzo.yaml`, the current dogfood orchestrator config, which still uses direct `workspace.hooks` and must not be changed by this plan.
- `examples/scherzo.yaml`, which demonstrates named hook profiles today and includes a `noop` profile, but not the new driver contract.
- `src/scherzo/config/types.gleam`, which currently defines hook-profile and driver-profile types.
- `src/scherzo/config.gleam`, which currently parses legacy hooks and driver profile schema.
- `src/scherzo/workflow_dag.gleam`, which currently parses `workspace_profile` and `workspace_capabilities`.
- `src/scherzo/workflow_run.gleam`, which currently prepares command-step environments without `SCHERZO_WORKSPACE_DRIVER`.
- `test/jj_workspace_hook_test.gleam`, which shows the existing pattern for testing shell helpers with a fake `jj` executable.
- `test/local_integration/workflow_jj_workspace_smoke_test.gleam`, which shows a real jj workspace smoke test through current hook execution.

## Preconditions and Verified Facts

This plan exists at `docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md`. The working copy was clean before the plan was originally written and before implementation began.

Before this implementation, the repository did not contain `docs/runbooks/workspace-driver-contract.md`, `scripts/scherzo-workspace-jj`, or `scripts/scherzo-workspace-noop`. This implementation adds those files, and both scripts are executable.

The current `scripts/scherzo-jj-workspace` usage is `scherzo-jj-workspace <after-create|before-run|before-remove> <workflow-name>`. It infers or uses `SCHERZO_REPO_ROOT`, creates jj workspaces with `jj workspace add`, verifies with `jj status --color=never`, and forgets workspaces before removal.

The current `test/jj_workspace_hook_test.gleam` uses `command_step.run`, `simplifile`, and a fake `jj` on `PATH` to validate helper behavior. New adapter tests should follow this style for fast deterministic coverage.

The current `test/local_integration/workflow_jj_workspace_smoke_test.gleam` creates a colocated jj repository with `jj git init --colocate .`, executes a workflow with hooks that call `scripts/scherzo-jj-workspace`, and asserts that the jj workspace is forgotten after cleanup. A new local integration smoke can reuse that structure while swapping the hook bodies to call `scripts/scherzo-workspace-jj lifecycle ...`.

The repository validation commands should run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. Treat that as environment setup, not as a code failure.

## Scope Boundaries

In scope for this implementation plan:

- Add `docs/runbooks/workspace-driver-contract.md` as the initial adapter contract reference.
- Add `scripts/scherzo-workspace-jj` as the jj driver command.
- Add `scripts/scherzo-workspace-noop` as the no-op artifact driver command.
- Add direct Gleam tests for both driver scripts.
- Add a jj lifecycle smoke test that proves the new jj driver can be called from current hook execution.
- Preserve `scripts/scherzo-jj-workspace` behavior and keep the old direct hook configuration working until the migration plan deliberately changes it.

Out of scope for this implementation plan:

- Do not modify `.scherzo/scherzo.yaml`, `examples/scherzo.yaml`, or any workflow YAML to use the new drivers.
- Do not add or change `workspace.profiles.<name>.driver` parsing unless the prerequisite core schema plan has already created a required integration point for adapter paths.
- Do not expose `SCHERZO_WORKSPACE_DRIVER` to command steps or prompt templates; that belongs to the runtime exposure child plan.
- Do not implement `baseline`, `refresh-base`, or `publish-change` as advertised capabilities in the adapter scripts.
- Do not remove legacy direct `workspace.hooks`; legacy detection and migration guidance belong to the core schema and migration plans.

## Milestones

The first milestone writes down the contract before code depends on it. At the end, `docs/runbooks/workspace-driver-contract.md` explains the lifecycle verbs, supported capabilities, exit codes, output formats, path-safety rules, and environment variables. This comes first because tests and scripts should be judged against one concrete contract rather than against implicit expectations.

The second milestone implements and tests the no-op artifact adapter. At the end, `scripts/scherzo-workspace-noop` can create an empty workspace, list regular files as changed files, report a human status, assert that exactly one relative path is present, and remove the workspace. This comes before the jj adapter because it proves the VCS-neutral artifact semantics with no external tool dependency.

The third milestone implements and tests the jj adapter. At the end, `scripts/scherzo-workspace-jj` delegates lifecycle commands to `scripts/scherzo-jj-workspace`, supports human `status` and `diff`, emits deterministic JSON changed-file lists, and implements `assert-only` from that list. Fast tests use a fake `jj`; a local integration smoke uses a real jj repository.

The fourth milestone runs the repository gates and records any deviations. At the end, all tests and lint gates pass, the adapter scripts are executable, and the plan's Outcomes & Retrospective section records the final behavior.

## Plan of Work

Create `docs/runbooks/workspace-driver-contract.md`. Write it as a reference for adapter authors and workflow authors. It must define the driver invocation root, the lifecycle forms `lifecycle create`, `lifecycle before-step`, `lifecycle after-step`, and `lifecycle remove`, and the initial capability forms `status --human`, `diff --human`, `changed-files --json`, and `assert-only --path <relative-file>`. It must state that capability path outputs and adapter-authored diagnostics are relative to the workspace root, not to the orchestrator config directory or an arbitrary source checkout. It must also state that destructive lifecycle commands must not fall back to the current directory. `baseline`, `refresh-base`, and `publish-change` are reserved future capability names unless the operator clarifies that they must be included in this first adapter slice.

Create `scripts/scherzo-workspace-noop`. The script should accept `lifecycle` and capability subcommands. For lifecycle subcommands, it must resolve the workspace root from a non-empty `SCHERZO_WORKSPACE_PATH`; lifecycle commands must not use the current directory fallback. `lifecycle create` should create the workspace directory and a private marker file such as `.scherzo-workspace-driver-noop`. `lifecycle before-step` and `lifecycle after-step` should verify that the workspace directory exists. `lifecycle remove` should refuse to delete unless `SCHERZO_WORKSPACE_PATH` is explicit, the marker is present, and, when `SCHERZO_RUN_ROOT` is set, the workspace path is contained by that run root. Read-only capability commands may resolve the workspace root as `SCHERZO_WORKSPACE_PATH` when set and as the current directory otherwise. `changed-files --json` should list all regular files under the workspace root, excluding the private marker and any other driver scratch files, as sorted workspace-root-relative path strings. `status --human` should print a short deterministic summary such as `No files` or a `Files:` list. `assert-only --path <relative-file>` should succeed only when the changed-file list is exactly that one path and the file exists.

Create `scripts/scherzo-workspace-jj`. The script should accept the same top-level contract. For `lifecycle create`, `lifecycle before-step`, and `lifecycle remove`, it should call `scripts/scherzo-jj-workspace after-create`, `scripts/scherzo-jj-workspace before-run`, and `scripts/scherzo-jj-workspace before-remove`, passing `${SCHERZO_WORKFLOW_ID:-workflow}` as the workflow name. `lifecycle after-step` should be a successful no-op for now. For capability commands, it should run from the workspace root resolved from `SCHERZO_WORKSPACE_PATH` or current directory. `status --human` should run `jj status --color=never`. `diff --human` should run `jj diff --from @- --to @ --color=never`. `changed-files --json` should run `jj diff --from @- --to @ --name-only --color=never`, normalize path separators if needed, remove blank lines, sort and deduplicate, and JSON-encode the result as workspace-root-relative paths. `assert-only --path <relative-file>` should use the same changed-file list and path-safety validation as the no-op adapter.

Keep shared behavior either duplicated in small script functions or placed in a tiny shared shell/Python helper only if duplication becomes error-prone. Do not introduce a new package dependency. If using Python for JSON encoding, rely only on the standard library. If using shell, make JSON escaping correct for spaces, quotes, and backslashes in file names; do not assume path names are alphanumeric.

Make both scripts executable in the implementation commit. Tests may call them through `sh` or through their executable bit, but the real configured driver command should be executable directly.

Add `test/workspace_driver_contract_test.gleam` for no-op and shared contract behavior. Follow the helper style in `test/jj_workspace_hook_test.gleam`: create directories under `test/tmp/`, use `simplifile` for setup and assertions, and use `command_step.run` with small artifact limits. Add tests named clearly enough that a failure identifies the contract clause that broke.

Add `test/jj_workspace_driver_test.gleam` for the jj adapter. Write a fake `jj` executable into a test-local `bin` directory and prepend that directory to `PATH` for fast tests. The fake `jj` should record invocations to a log and return controlled output for `status`, `diff`, `diff --name-only`, `workspace add`, `root`, `log`, and `git fetch`, following the existing fake pattern in `test/jj_workspace_hook_test.gleam`.

Add `test/local_integration/workflow_jj_workspace_driver_smoke_test.gleam` or extend `test/local_integration/workflow_jj_workspace_smoke_test.gleam` with a separate test. Prefer a new file if the existing smoke would become too broad. The smoke should configure legacy hook snippets that call the new driver lifecycle commands and then execute a two-command-step workflow in one logical workspace. This proves the driver lifecycle can be used before the runtime schema migration exists.

Do not update dogfood configs or public examples in this plan. Later child plans will change `.scherzo/scherzo.yaml`, expose `SCHERZO_WORKSPACE_DRIVER`, and update portable workflows after the contract and adapters are stable.

## Concrete Steps

1. From the repository root, re-read `scripts/scherzo-jj-workspace`, `test/jj_workspace_hook_test.gleam`, and `test/local_integration/workflow_jj_workspace_smoke_test.gleam` to ensure the implementation still matches the current helper behavior. If any file has drifted, update this ExecPlan before coding.

2. Create `docs/runbooks/workspace-driver-contract.md`. Include a short glossary for workspace, workspace driver, lifecycle operation, and capability. Define these command forms exactly:

    <driver> lifecycle create
    <driver> lifecycle before-step
    <driver> lifecycle after-step
    <driver> lifecycle remove
    <driver> status --human
    <driver> diff --human
    <driver> changed-files --json
    <driver> assert-only --path <relative-file>

3. In `docs/runbooks/workspace-driver-contract.md`, state the exit-code rules: exit 0 means success, exit 1 means a valid operation failed or an assertion did not hold, and exit 2 means usage error, unsupported operation, unsafe path argument, or refused destructive target. State that adapter-authored diagnostics must name workspace-root-relative paths and must not include local workspace roots.

4. In `docs/runbooks/workspace-driver-contract.md`, document that the no-op driver supports `status`, `changed-files`, and `assert-only`; the jj driver supports `status`, `diff`, `changed-files`, and `assert-only`; and `baseline`, `refresh-base`, and `publish-change` are reserved for later plans.

5. Create `scripts/scherzo-workspace-noop`. Implement argument parsing for the lifecycle verbs and supported capability verbs. Keep the usage text bounded and include the supported commands.

6. In `scripts/scherzo-workspace-noop`, implement workspace-root resolution. For lifecycle commands, require `SCHERZO_WORKSPACE_PATH` to be set to a non-empty value and return exit 2 when it is missing or unsafe. For read-only capability commands, use `SCHERZO_WORKSPACE_PATH` when it is set and otherwise use the current directory. Do not print the resolved root in normal diagnostics.

7. In `scripts/scherzo-workspace-noop`, implement safe relative path validation for `assert-only`. Reject an empty string, `.`, any path with an absolute shape, and any path containing `..` as a full path segment. Return exit 2 for these cases.

8. In `scripts/scherzo-workspace-noop`, implement `changed-files --json` by walking regular files below the workspace root, excluding the private marker file, producing sorted workspace-root-relative paths, and JSON-encoding the array. Directory entries are not changed files. If the workspace is empty or only contains the marker, print `[]` followed by a newline.

9. In `scripts/scherzo-workspace-noop`, implement `status --human` as a deterministic summary of the same file list. The exact text may be simple, but tests should lock it down enough to prevent accidental absolute path output.

10. In `scripts/scherzo-workspace-noop`, implement `assert-only --path <relative-file>` using the same file list. On success, exit 0 and keep stdout empty unless a `--json` option is added during implementation. On failure, exit 1 and print a bounded stderr message that includes the expected relative path and the actual relative file list.

11. Create `test/workspace_driver_contract_test.gleam`. Add helper functions for artifact limits, directory reset, command execution, script paths, and JSON/string assertions.

12. In `test/workspace_driver_contract_test.gleam`, add `noop_driver_lifecycle_create_before_after_and_remove_test`. It should set `SCHERZO_RUN_ROOT` to `test/tmp/workspace-driver-noop-lifecycle/run` and `SCHERZO_WORKSPACE_PATH` to `test/tmp/workspace-driver-noop-lifecycle/run/workspaces/workspace`, run `scripts/scherzo-workspace-noop lifecycle create`, assert the directory and private marker exist, run `lifecycle before-step` and `lifecycle after-step`, assert `changed-files --json` does not report the marker, run `lifecycle remove`, and assert the directory no longer exists. Add a companion `noop_driver_lifecycle_remove_rejects_unset_empty_unmarked_and_outside_run_root_test` that runs `lifecycle remove` with `SCHERZO_WORKSPACE_PATH` unset, with `SCHERZO_WORKSPACE_PATH` set to an empty value, with an unmarked workspace directory, and with a marked target dynamically constructed outside `SCHERZO_RUN_ROOT`; assert exit code 2 and assert the target directory still exists. Do not write a literal absolute path into the test source.

13. In `test/workspace_driver_contract_test.gleam`, add `noop_driver_changed_files_json_is_sorted_relative_and_empty_safe_test`. It should create files such as `zeta.md` and `nested/alpha.md`, run `changed-files --json`, and assert stdout is exactly a JSON array in sorted order with workspace-root-relative paths. It should also check an empty workspace prints `[]`. Add a second JSON case named `noop_driver_changed_files_json_escapes_special_path_names_test` that creates files with a space, a quote, and a backslash in their names and asserts the output is valid JSON with the expected escaped strings.

14. In `test/workspace_driver_contract_test.gleam`, add `noop_driver_assert_only_accepts_exact_single_file_test`. It should create only `research-findings.md`, run `assert-only --path research-findings.md`, and assert exit code 0.

15. In `test/workspace_driver_contract_test.gleam`, add `noop_driver_assert_only_rejects_extra_file_test`. It should create `research-findings.md` and `notes.md`, run the same assertion, and assert exit code 1 with stderr containing both relative file names and no workspace root string.

16. In `test/workspace_driver_contract_test.gleam`, add `driver_assert_only_rejects_unsafe_paths_test`. It should test an empty value, `../outside.md`, `.`, and a dynamically constructed absolute path using the repository's path helper. Assert exit code 2. Do not write a literal absolute path into the test source.

17. Run `direnv exec . gleam test` from the repository root. Expect the new no-op driver tests to compile and pass. If they fail because `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry.

18. Create `scripts/scherzo-workspace-jj`. Implement usage text and top-level dispatch for `lifecycle`, `status`, `diff`, `changed-files`, and `assert-only`. Unsupported operations must exit 2.

19. In `scripts/scherzo-workspace-jj`, implement lifecycle delegation to `scripts/scherzo-jj-workspace`. Resolve the repository root from `SCHERZO_REPO_ROOT` when set; otherwise use the script location to find the repository root. Call the old helper with `after-create`, `before-run`, or `before-remove` and the workflow name from `SCHERZO_WORKFLOW_ID`, defaulting to `workflow`.

20. In `scripts/scherzo-workspace-jj`, implement workspace-root resolution for capability commands. Use `SCHERZO_WORKSPACE_PATH` when set; otherwise use the current directory. Run jj commands from that root.

21. In `scripts/scherzo-workspace-jj`, implement `changed-files --json` from `jj diff --from @- --to @ --name-only --color=never`. Normalize blank output to an empty array. Sort, deduplicate, and JSON-encode paths.

22. In `scripts/scherzo-workspace-jj`, implement `status --human`, `diff --human`, and `assert-only --path <relative-file>`. Reuse the same safe path validation and changed-file list semantics as the no-op adapter.

23. Create `test/jj_workspace_driver_test.gleam`. Copy only the helper ideas needed from `test/jj_workspace_hook_test.gleam`; do not import test helpers from another test module unless the repository already has a shared test-helper convention for this pattern.

24. In `test/jj_workspace_driver_test.gleam`, add `jj_driver_lifecycle_create_delegates_to_existing_helper_test`. Use a fake `jj` and a fake repo with `.jj`, run `scripts/scherzo-workspace-jj lifecycle create`, and assert the fake log shows the same fetch, revision resolution, workspace add, root, and status shape that `scripts/scherzo-jj-workspace` currently uses.

25. In `test/jj_workspace_driver_test.gleam`, add `jj_driver_changed_files_json_is_sorted_and_deduplicated_test`. Make fake `jj diff --name-only` output duplicate and unsorted file names. Assert stdout is one sorted JSON array. Add `jj_driver_changed_files_json_escapes_special_path_names_test` with fake changed-file output containing a space, a quote, and a backslash, and assert the adapter emits valid escaped JSON.

26. In `test/jj_workspace_driver_test.gleam`, add `jj_driver_status_and_diff_use_human_jj_commands_test`. Make fake `jj status --color=never` and `jj diff --from @- --to @ --color=never` print known text. Assert the adapter returns that text on stdout with exit code 0.

27. In `test/jj_workspace_driver_test.gleam`, add `jj_driver_assert_only_accepts_exact_file_and_rejects_extra_file_test`. Use fake changed-file output for one case with only `docs/plans/example.md` and another with `docs/plans/example.md` plus `notes.md`. Assert exit 0 for the first and exit 1 for the second. Add `jj_driver_assert_only_rejects_unsafe_paths_without_invoking_jj_test`; it should pass unsafe values including an empty value, `../outside.md`, `.`, and a dynamically constructed absolute path, assert exit code 2, and assert the fake `jj` invocation log remains empty. Add `jj_driver_lifecycle_after_step_is_successful_noop_test`; it should run `scripts/scherzo-workspace-jj lifecycle after-step`, assert exit code 0, and assert neither the old helper path nor fake `jj` was invoked.

28. Add a jj lifecycle smoke test in `test/local_integration/workflow_jj_workspace_driver_smoke_test.gleam` or as a new test in `test/local_integration/workflow_jj_workspace_smoke_test.gleam`. Follow the existing smoke-test pattern instead of relying on the hook working directory: compute a runtime script path for `scripts/scherzo-workspace-jj`, interpolate that script path into the hook snippets, and set `SCHERZO_REPO_ROOT` to the temporary jj repository created by the test. For the create hook, also set `SCHERZO_JJ_WORKSPACE_BASE=@` so the helper does not depend on a remote base. The hook snippets should conceptually run:

    SCHERZO_REPO_ROOT="$temporary_jj_repo" SCHERZO_JJ_WORKSPACE_BASE=@ sh "$script" lifecycle create
    SCHERZO_REPO_ROOT="$temporary_jj_repo" sh "$script" lifecycle before-step
    SCHERZO_REPO_ROOT="$temporary_jj_repo" sh "$script" lifecycle after-step
    SCHERZO_REPO_ROOT="$temporary_jj_repo" sh "$script" lifecycle remove

    Do not use `sh scripts/scherzo-workspace-jj ...` in this smoke because hooks execute from the temporary config directory, not from the repository root. The smoke should assert that two command steps use the same workspace path and that the jj workspace name is gone after cleanup, matching the existing smoke's acceptance.

29. Make the new scripts executable:

    chmod +x scripts/scherzo-workspace-jj scripts/scherzo-workspace-noop

30. Run the full test suite from the repository root:

    direnv exec . gleam test

    Expect all tests to pass, including the new `workspace_driver_contract`, `jj_workspace_driver`, and jj local integration smoke tests.

31. Run formatting from the repository root:

    direnv exec . gleam format --check src test

    Expect no formatting changes needed. If formatting fails for new test files, run `direnv exec . gleam format src test`, inspect the diff, and rerun the check.

32. Run production lint gates from the repository root:

    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

    Expect both commands to complete successfully. New scripts are not Gleam production files, but any incidental production Gleam changes must satisfy these gates.

33. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections with the final validation output and any contract deviations discovered during implementation.

34. When implementing outside Scherzo's managed implementation workflow, commit the implementation as one logical adapter-contract commit after validation passes. Suggested commit message: `feat(workspace): add initial driver contract adapters`. When implementing under `workflow:execplan-implementation`, do not create a manual commit; the publish step creates the final jj commit after review and validation.

## Testing and Falsifiability

This plan is falsified if a workflow author still has to know whether a workspace is jj-backed or no-op-backed to call `changed-files --json` or `assert-only --path research-findings.md`. It is also falsified if adapter-authored failures expose local workspace roots, if unsupported commands silently succeed, or if the jj lifecycle smoke shows behavior different from the existing `scripts/scherzo-jj-workspace` lifecycle.

The no-op adapter tests in `test/workspace_driver_contract_test.gleam` must cover these exact cases. `noop_driver_lifecycle_create_before_after_and_remove_test` proves lifecycle commands are idempotent enough for an empty artifact workspace and that the private marker is excluded from capability output. `noop_driver_lifecycle_remove_rejects_unset_empty_unmarked_and_outside_run_root_test` proves destructive removal cannot fall back to the caller's current directory, accept an empty target, delete an unmarked target, or delete a marked target outside the configured run root. `noop_driver_changed_files_json_is_sorted_relative_and_empty_safe_test` proves deterministic JSON output for both empty and populated workspaces. `noop_driver_changed_files_json_escapes_special_path_names_test` proves JSON output remains valid for file names with spaces, quotes, and backslashes. `noop_driver_assert_only_accepts_exact_single_file_test` proves the portable research success path. `noop_driver_assert_only_rejects_extra_file_test` proves extra artifacts fail with actionable relative diagnostics. `driver_assert_only_rejects_unsafe_paths_test` proves unsafe path inputs are rejected before filesystem inspection.

The jj adapter tests in `test/jj_workspace_driver_test.gleam` must cover these exact cases. `jj_driver_lifecycle_create_delegates_to_existing_helper_test` proves the new driver preserves the old lifecycle behavior. `jj_driver_lifecycle_after_step_is_successful_noop_test` proves the advertised after-step lifecycle command succeeds without invoking the legacy helper or jj. `jj_driver_changed_files_json_is_sorted_and_deduplicated_test` proves machine output is deterministic. `jj_driver_changed_files_json_escapes_special_path_names_test` proves JSON output remains valid for non-simple file names. `jj_driver_status_and_diff_use_human_jj_commands_test` proves human commands delegate to jj with the expected flags. `jj_driver_assert_only_accepts_exact_file_and_rejects_extra_file_test` proves the jj driver enforces the same artifact contract as the no-op driver, using jj's changed-file list. `jj_driver_assert_only_rejects_unsafe_paths_without_invoking_jj_test` proves unsafe assertion arguments are rejected before running any jj command.

The local integration smoke must prove the new lifecycle verbs can be used through Scherzo's current hook execution while the hooks run from the temporary config directory. It should fail before `scripts/scherzo-workspace-jj` exists with a missing script or unsupported command error, and pass after the jj adapter is implemented. It must use a runtime-computed script path plus temporary `SCHERZO_REPO_ROOT` and `SCHERZO_JJ_WORKSPACE_BASE=@` values so it never exercises the developer's real checkout or remotes.

Manual contract checks are optional but useful when debugging. From the repository root, an implementer can create a no-op workspace under `test/tmp/manual-workspace-driver`, set `SCHERZO_WORKSPACE_PATH` to that directory, run `scripts/scherzo-workspace-noop lifecycle create`, write `research-findings.md`, and run `scripts/scherzo-workspace-noop assert-only --path research-findings.md`. The expected result is exit code 0 and no stdout. Adding `notes.md` and rerunning should produce exit code 1 with both relative paths named on stderr.

## Validation and Acceptance

Validation is complete when these commands pass from the repository root:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Acceptance requires these observable behaviors:

A no-op workspace can be created and removed without jj or git when `SCHERZO_WORKSPACE_PATH` is explicit. `scripts/scherzo-workspace-noop lifecycle remove` refuses unset, empty, unmarked, or run-root-outside targets with exit code 2. When the workspace contains only `research-findings.md`, `scripts/scherzo-workspace-noop assert-only --path research-findings.md` exits 0. When it contains `research-findings.md` and `notes.md`, the same command exits 1 and names the unexpected workspace-root-relative file.

A jj workspace can be created through `scripts/scherzo-workspace-jj lifecycle create` while preserving the existing behavior of `scripts/scherzo-jj-workspace after-create`. In a jj workspace with changes, `scripts/scherzo-workspace-jj changed-files --json` prints a sorted JSON array of changed workspace-root-relative paths, and `scripts/scherzo-workspace-jj assert-only --path <relative-file>` succeeds only when that one file is the complete changed-file set.

The contract reference at `docs/runbooks/workspace-driver-contract.md` describes exactly the command shapes that the tests execute. No dogfood workflow or public example is migrated as part of this plan.

## Rollout, Recovery, and Idempotence

Rollout for this plan is additive. Adding contract documentation, two new scripts, and new tests does not change runtime behavior until later child plans configure a workspace profile to use the scripts. The existing `scripts/scherzo-jj-workspace` and legacy `.scherzo/scherzo.yaml` hooks remain intact.

Rollback is straightforward: remove `docs/runbooks/workspace-driver-contract.md`, `scripts/scherzo-workspace-jj`, `scripts/scherzo-workspace-noop`, and the new tests. Because no runtime config or state schema changes are part of this plan, rollback does not require data migration or recovery handling.

The implementation should be safe to retry. Recreating the scripts should overwrite the same files deterministically. Tests should clean their own `test/tmp/` directories before use. The no-op adapter's private marker may be recreated by `lifecycle create` and must be excluded from `changed-files --json`. If a local integration jj workspace is left behind by an interrupted test, rerunning the test should either forget it during setup or create a distinct test workspace name and clean it during teardown.

Later migration plans must not assume these adapters are enabled merely because the files exist. A driver-backed profile must explicitly point to the chosen adapter and advertise only the capabilities that adapter actually supports.

## Artifacts and Notes

The current working copy was clean before the plan file was created:

    jj status --color=never
    The working copy has no changes.

The current dogfood config invokes the old hook helper directly from `workspace.hooks`:

    .scherzo/scherzo.yaml
    workspace:
      root: workspaces
      hooks:
        create: ... sh "$repo_root/scripts/scherzo-jj-workspace" after-create "$SCHERZO_WORKFLOW_ID"
        before_step: ... sh "$repo_root/scripts/scherzo-jj-workspace" before-run "$SCHERZO_WORKFLOW_ID"
        remove: ... sh "$repo_root/scripts/scherzo-jj-workspace" before-remove "$workflow_name"

The current example config has named hook profiles, including `noop`, but still uses `hooks` rather than `driver`:

    examples/scherzo.yaml
    workspace:
      default_profile: isolated
      profiles:
        isolated:
          hooks: ...
        noop:
          hooks: ...

The existing jj helper verbs are:

    scripts/scherzo-jj-workspace after-create <workflow-name>
    scripts/scherzo-jj-workspace before-run <workflow-name>
    scripts/scherzo-jj-workspace before-remove <workflow-name>

The new jj driver lifecycle verbs should map to those old verbs without changing old behavior.

## Interfaces and Dependencies

The driver command contract for this plan is:

    <driver> lifecycle create
    <driver> lifecycle before-step
    <driver> lifecycle after-step
    <driver> lifecycle remove
    <driver> status --human
    <driver> diff --human
    <driver> changed-files --json
    <driver> assert-only --path <relative-file>

All commands run with the normal Scherzo hook or step environment. The adapters must use these environment variables when present:

- `SCHERZO_WORKSPACE_PATH` is the workspace root. No-op lifecycle `create` and `remove` require this variable to be explicit and non-empty; capability commands may use the current directory if this variable is unset.
- `SCHERZO_RUN_ROOT` is the run directory containing one or more logical workspaces. When it is set, the no-op adapter's destructive `lifecycle remove` must verify that `SCHERZO_WORKSPACE_PATH` is contained by this run root.
- `SCHERZO_CONFIG_DIR` is the directory containing the orchestrator config.
- `SCHERZO_REPO_ROOT` is the coordinating repository root for jj lifecycle operations when provided.
- `SCHERZO_SOURCE_WORKSPACE_PATH` is the source workspace for derived workspaces during lifecycle creation.
- `SCHERZO_WORKFLOW_ID`, `SCHERZO_ISSUE_IDENTIFIER`, `SCHERZO_RUN_ID`, and `SCHERZO_WORKSPACE_NAME` are identity fields used by the existing jj lifecycle helper.
- `SCHERZO_JJ_WORKSPACE_BASE`, `SCHERZO_PR_BASE`, and `SCHERZO_PR_REMOTE` remain jj-specific configuration used by `scripts/scherzo-jj-workspace`.

The jj adapter depends on `scripts/scherzo-jj-workspace` and the `jj` executable for jj-backed lifecycle and diff behavior. It must not depend on Linear or GitHub. The no-op adapter must not depend on `jj`, `git`, Linear, GitHub, or network access.

`changed-files --json` must print a JSON array of workspace-root-relative path strings followed by a newline. The array must be sorted and deduplicated. The no-op adapter's changed-file set is all regular files under the workspace root except its private marker and any other driver scratch files. The jj adapter's changed-file set is the output of `jj diff --from @- --to @ --name-only --color=never` after normalization.

`assert-only --path <relative-file>` must validate the path argument first. The target must be a non-empty relative file path, must not be `.`, must not contain `..` as a path segment, and must not be an absolute path. On success it exits 0. On mismatch it exits 1 and prints a bounded diagnostic naming the expected path and actual workspace-root-relative changed-file list. On unsafe input it exits 2.

`status --human` is for people and tests should not parse it beyond basic expected text. `diff --human` is only advertised by the jj driver in this plan. Unsupported operations must exit 2 and print usage.

## Open Questions and Clarifications Needed

- [CLARIFY] The umbrella listed `baseline`, `refresh-base`, and `publish-change` as likely future capabilities. This plan recommends reserving those names but not implementing them in the initial jj/no-op adapters because they require policy decisions about rebasing, remotes, branch names, review systems, and recovery. If the operator wants any of those capabilities in this first adapter slice, revise Scope Boundaries, Interfaces and Dependencies, and Testing before implementation begins.
