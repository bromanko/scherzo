# Add repo-root workspace mode and a safe sync-main workflow

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo is good at implementation work because each workflow run normally receives an isolated jj workspace under `.scherzo/workspaces`. That isolation is the wrong tool for local maintenance tasks that are intentionally about the operator's real checkout. After this plan is implemented, an operator can label or route a `sync-main` workflow and have Scherzo fetch `origin/main`, leave dirty local work untouched, and only move a clean repository-root checkout onto the fetched main revision.

The visible behavior is a checked-in workflow at `.scherzo/workflows/sync-main.yaml` whose single command step runs in the repository root, not in `.scherzo/workspaces/<workflow>/<issue>/<run>/workspaces/<name>`. A successful clean run prints a status such as `SCHERZO_SYNC_MAIN_STATUS=updated` or `SCHERZO_SYNC_MAIN_STATUS=already_current`. A dirty checkout run prints `SCHERZO_SYNC_MAIN_STATUS=skipped_dirty`, exits successfully, and does not reset, discard, or rebase local work.

## Problem Framing and Constraints

Today all checked-in Scherzo workflows are effectively isolated by the workspace hooks in `.scherzo/scherzo.yaml`. The `workspace.hooks.create` hook creates a jj workspace at `SCHERZO_WORKSPACE_PATH`, and the `workspace.hooks.remove` hook forgets per-run jj workspaces before the run root is deleted. That default prevents implementation agents from mutating the operator's checkout, which is desirable for normal implementation and review workflows.

A local maintenance workflow has the opposite need. It must operate on the checkout that the operator uses as the repository root. The first required workflow fetches GitHub main, checks whether the checkout is safe to update, and moves only a clean checkout to the fetched main revision. The implementation must not make repo-root execution the default, must not let agent steps run in repo-root mode, and must not delete or move the repository root during cleanup. A normal Scherzo run root is still required because checkpoints, command artifacts, cleanup retention markers, and run identity remain part of the workflow system even when the command's current directory is the repository root.

## Strategy Overview

Add a first-class workspace mode to workflow DAG step workspace references. The default mode remains the current isolated behavior. A new `repo_root` mode is available only on command steps and means: create the normal run root, set `SCHERZO_WORKSPACE_PATH` and the command current directory to the repository root, do not create a per-run jj workspace, and ensure cleanup deletes only the run root.

This is the right size because it keeps the default isolation path intact and avoids special-casing one workflow ID in the runner. The schema explicitly records that a step is intentionally operating at repo root, so validation, fingerprinting, hooks, command environment, and cleanup can all reason about it. The first user-visible consumer is a small shell helper, `scripts/scherzo-sync-main`, and a single command-only workflow, `.scherzo/workflows/sync-main.yaml`.

The runner should also avoid creating command diagnostics under the repository root for repo-root command steps. Current command diagnostics are written under `workspace_path/.scherzo/command-step-diagnostics`; if `workspace_path` is the repository root, the diagnostic file itself can make the checkout dirty and defeat the sync helper. For repo-root command steps, command diagnostics must live under the run root.

## Alternatives Considered

The simplest possible alternative is a standalone script that an operator runs manually from the repository root. That would solve the first sync use case, but it would not exercise or validate Scherzo workflows, would not produce Scherzo artifacts, and would not support scheduled or label-routed maintenance work.

Another alternative is to keep the workflow schema unchanged and teach the checked-in hooks to treat a workspace named `repo` specially. That hides dangerous behavior behind a name convention, makes fingerprinting blind to the execution-mode change, and increases the risk that cleanup or a hook treats the repository root as a disposable workspace.

A third alternative is to allow both command and agent steps in repo-root mode. That is intentionally rejected for this first implementation. Agent steps have many more ways to modify files, run tools, and recover sessions. Command-only support is enough for `sync-main` and provides a smaller safety envelope.

## Risks and Countermeasures

The largest risk is deleting or moving the repository root during cleanup. The countermeasure is that `repo_root` mode still computes a run root under the configured workspace root, but the prepared workspace path is the repository root. Cleanup APIs must continue to accept only the run root, and tests must prove that cleanup removes the run root while leaving a sentinel file in the repository root intact. No cleanup function may call delete on `PreparedStepWorkspace.path` for repo-root mode.

A second risk is that command diagnostics, lock files, or hooks dirty the repository root before `scripts/scherzo-sync-main` checks cleanliness. Command diagnostics for repo-root command steps must be written under `SCHERZO_RUN_ROOT`, not under `SCHERZO_WORKSPACE_PATH`. Lock files for sync-main must be placed under the configured Scherzo workspace state directory, not in the repository root. The existing checked-in `before_step` hook may run in repo root because it only verifies the jj workspace before execution; the `create` hook must not run for repo-root mode because its purpose is to create isolated jj workspaces.

A third risk is concurrent mutation of the operator checkout. The checked-in `sync-main` workflow must have `max_parallel_steps: 1`, but that only serializes steps inside one workflow run. The helper must also acquire an atomic lock under the Scherzo workspace state directory before checking or updating the checkout. If the lock is held, it should print `SCHERZO_SYNC_MAIN_STATUS=skipped_locked`, exit zero, and leave the checkout untouched. This makes repeated scheduled runs safe and non-noisy.

A fourth risk is accidentally allowing an agent step to run in repo-root mode. The workflow parser must reject any `AgentStep` whose workspace mode is `repo_root` with a deterministic error code such as `repo_root_agent_step`. Tests must cover this parser rejection.

A fifth risk is changing fingerprints for all existing isolated workflows and making active run recovery think every workflow changed. The fingerprint should record the non-default `repo_root` mode, but it should preserve canonical input for existing isolated workflow YAML. The plan therefore encodes `mode` in `src/scherzo/workflow_fingerprint.gleam` only when the mode is `repo_root`, while explicit `mode: isolated` and omitted mode are semantically identical.

## Progress

- [x] (2026-05-06 00:00Z) Drafted this ExecPlan from Linear issue LIV-105 and inspected the current workflow schema, workspace preparation, cleanup, command-step, hooks, and checked-in workflow configuration.
- [ ] Implement workspace-mode parsing, validation, and fingerprint behavior.
- [ ] Implement repo-root workspace preparation, hook environment, command diagnostics, and cleanup safety.
- [ ] Implement the sync-main helper and checked-in workflow.
- [ ] Add focused parser, execution, cleanup, script, and happy-path sync tests.
- [ ] Run validation commands and update this plan with results, surprises, and retrospective notes.

## Surprises & Discoveries

- Observation: Existing command diagnostics are currently rooted at the command workspace path.
  Evidence: `src/scherzo/command_step.gleam` builds `.scherzo/command-step-diagnostics` under the `workspace_path` passed to `command_step.run_with_env`. Repo-root mode must redirect those diagnostics to the run root to avoid making the repository root dirty.
- Observation: Current workflow-run cleanup already deletes a run root rather than an individual workspace path.
  Evidence: `src/scherzo/workspace_run.gleam` exposes `cleanup_run(run_root, orchestrator)` and checks that the target is inside `orchestrator.effective.workspace.root` and is not the workspace root itself before deleting it.
- Observation: The checked-in `.scherzo/scherzo.yaml` has explicit `linear_contract.workflow_labels` while `routing.require_exactly_one_workflow_label` is true.
  Evidence: `src/scherzo/config.gleam` validates that `linear_contract.workflow_labels` matches `routing.workflows` when exact workflow-label routing is required, so adding `sync-main` to routing also requires adding it to the contract label list.

## Decision Log

- Decision: Represent repo-root execution as a `mode` on `workflow_dag.WorkspaceRef`, not as a workflow ID or workspace name convention.
  Rationale: The execution mode changes safety, cleanup, hooks, environment variables, and fingerprinting. A schema field is explicit and testable.
  Date: 2026-05-06
- Decision: Restrict `mode: repo_root` to command steps in the workflow parser.
  Rationale: The first use case is command-only maintenance. Agent steps in the operator checkout have a larger blast radius and are a non-goal for this implementation.
  Date: 2026-05-06
- Decision: Skip the `workspace.hooks.create` hook for repo-root mode, but still run `before_step` and `after_step` with repo-root environment variables.
  Rationale: The create hook exists to create or populate disposable isolated workspaces. The before and after step hooks are step lifecycle hooks and can safely verify or observe the actual command workspace.
  Date: 2026-05-06
- Decision: The sync helper rebases only an empty current jj change onto the fetched remote bookmark and does not reset local files.
  Rationale: In jj, a clean operator checkout is commonly an empty working-copy change on top of a base revision. Rebasing that empty change onto `main@origin` updates the checkout without discarding dirty files or non-empty local changes.
  Date: 2026-05-06

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam application. Workflow DAG YAML files live under `.scherzo/workflows`. The checked-in daemon configuration is `.scherzo/scherzo.yaml`. The current workflow schema is implemented in `src/scherzo/workflow_dag.gleam`. It parses each step into a `WorkflowStep` with a `StepKind` and a `WorkspaceRef`. Today `WorkspaceRef` has only `name` and `from`; `from` means that a step derives a new logical workspace from a workspace produced by one of its transitive dependencies.

The current fingerprint code is `src/scherzo/workflow_fingerprint.gleam`. It serializes workflow steps, step kinds, workspace names, workspace sources, failure policy, hooks, artifact limits, and model settings to canonical JSON before hashing. Repo-root mode changes execution semantics and must be represented in this canonical input for repo-root workflows.

The current run-root and step-workspace preparation code is `src/scherzo/workspace_run.gleam`. It computes run roots as `.scherzo/workspaces/<workflow>/<issue>/<run>` relative to the checked-in config and isolated workspace paths as `<run-root>/workspaces/<workspace-name>`. It runs create and before-step hooks with environment variables such as `SCHERZO_RUN_ROOT`, `SCHERZO_WORKSPACE_NAME`, `SCHERZO_WORKSPACE_PATH`, and source workspace variables.

The current command runner is `src/scherzo/command_step.gleam`. It runs a shell command with a working directory and environment variables. The workflow runner in `src/scherzo/workflow_run.gleam` builds a `StepContext`, dispatches command steps through `dependencies.command_step`, and dispatches agent steps through the agent runner.

The current checked-in workspace hooks in `.scherzo/scherzo.yaml` call `scripts/scherzo-jj-workspace`. The create hook makes per-run jj workspaces, the before-step hook verifies the current jj workspace, and the remove hook forgets run workspaces before deleting the run root. The new repo-root mode must not invoke the create hook because the repository root already exists and must not become a disposable per-run jj workspace.

A jj workspace is a checkout managed by Jujutsu. In this repository, ordinary implementation workflows get separate jj workspaces under `.scherzo/workspaces`. The repository root is the operator checkout that contains `.scherzo/scherzo.yaml`, `src`, `test`, `scripts`, and `docs`.

## Preconditions and Verified Facts

The repository already contains `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_fingerprint.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/command_step.gleam`, `.scherzo/scherzo.yaml`, and `scripts/scherzo-jj-workspace`.

The workflow parser currently accepts both `workspace: main` and a map form with `name` and `from`. Tests for this behavior are in `test/workflow_dag_test.gleam`.

The current workspace-run tests are in `test/workspace_run_test.gleam`. They already verify logical workspace paths under a run root, hook environment variables, recovered workspace validation, cleanup rejection for paths outside the workspace root, and cleanup retention-marker behavior.

The checked-in routing map in `.scherzo/scherzo.yaml` currently includes `research`, `implementation`, `execplan`, `execplan-revision`, `execplan-implementation`, and `merge-conflict-resolution`. Because exact workflow-label routing and the Linear contract are enabled, adding a checked-in workflow also requires updating `linear_contract.workflow_labels`.

The current source-control status at plan-authoring time had no working-copy changes when checked with `jj status --color=never`. Implementation agents must still inspect their own working copy before starting and must not assume this authoring-time status remains true.

## Scope Boundaries

In scope: add `repo_root` as an explicit workflow workspace mode; keep isolated workspaces as the default; make parser, validation, fingerprinting, workspace preparation, hook environment, command environment, command diagnostics, cleanup, and tests aware of the new mode; add `scripts/scherzo-sync-main`; add `.scherzo/workflows/sync-main.yaml`; update `.scherzo/scherzo.yaml` routing and Linear contract labels for `sync-main`.

Out of scope: allowing agent steps in repo-root mode; changing the default isolated behavior for implementation, research, review, execplan, or merge-conflict workflows; resetting or discarding dirty local work; replacing the existing jj workspace hook system; implementing a general scheduler for every possible operator-maintenance workflow beyond the safety needed here.

Existing files that must retain their current behavior for isolated workflows are `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/research.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, and `.scherzo/workflows/merge-conflict-resolution.yaml`. They should not need schema changes unless tests require explicit expected constructors to include the default isolated mode.

## Milestones

Milestone 1 adds the schema and validation. At the end, workflow YAML can say `workspace: main`, `workspace: { name: main, mode: isolated }`, or `workspace: { name: repo, mode: repo_root }`. Existing isolated workflows parse the same way as before from an operator perspective. Agent steps with `mode: repo_root` fail during parsing.

Milestone 2 adds execution support for repo-root command workspaces. At the end, preparing a repo-root command step creates the run root, returns `PreparedStepWorkspace.path` equal to the repository root, skips isolated-workspace creation, exposes clear environment variables, and leaves cleanup pointed at the run root only. Command diagnostics for repo-root steps are written below the run root.

Milestone 3 adds the sync helper. At the end, `scripts/scherzo-sync-main` can be run from the repository root. It fetches `origin/main`, skips safely when the checkout is dirty or the current jj change is not empty, and rebases an empty current change onto the fetched remote bookmark.

Milestone 4 checks in the workflow and routing changes. At the end, `.scherzo/workflows/sync-main.yaml` is routeable with a `workflow:sync-main` label and uses the new repo-root workspace mode.

Milestone 5 adds and runs validation. At the end, parser, fingerprint, workspace preparation, workflow execution, cleanup safety, dirty no-op, and happy-path sync behavior are covered by tests, and the full test suite and formatter pass.

## Plan of Work

In `src/scherzo/workflow_dag.gleam`, add a public `WorkspaceMode` type with constructors `IsolatedWorkspace` and `RepoRootWorkspace`. Extend `WorkspaceRef` to include `mode: WorkspaceMode`. Add helpers if helpful, for example `isolated_workspace(name: String, from: Option(String))` and `repo_root_workspace(name: String)`, but keep the public constructor usable by tests and existing modules.

Update every `WorkspaceRef` construction in source and tests to pass `mode: IsolatedWorkspace` unless the specific test or workflow is about repo-root mode. This includes inline legacy workflow construction in `src/scherzo/workflow_dag.gleam`, tests in `test/workflow_dag_test.gleam`, workspace-run tests in `test/workspace_run_test.gleam`, workflow-run tests in `test/workflow_run_test.gleam`, and any other compile errors found by `gleam test`.

In `src/scherzo/workflow_dag.gleam`, update `read_workspace` so map-form workspaces accept an optional `mode`. Missing mode means `isolated`. Accepted string values are `isolated` and `repo_root` after trimming and lowercasing. Any other value returns `DagError("invalid_workspace_mode", ...)`. String-form workspaces remain isolated and cannot express repo-root mode. Map-form repo-root workspaces still require a valid `name` because the name appears in environment variables and artifacts.

In `src/scherzo/workflow_dag.gleam`, update validation so `mode: repo_root` rejects `from` with `DagError("repo_root_workspace_from", ...)`, and rejects agent steps with `DagError("repo_root_agent_step", ...)`. Keep the existing `invalid_workspace_from` validation for isolated derived workspaces.

In `src/scherzo/workflow_fingerprint.gleam`, update `workspace_to_json`. Preserve existing isolated workflow fingerprints by keeping the current `name` and `from` fields unchanged when the mode is isolated. Add a `mode` field with value `repo_root` only when `workspace.mode == RepoRootWorkspace`. Add tests in `test/workflow_fingerprint_test.gleam` that prove an omitted isolated mode and explicit `mode: isolated` have the same canonical input, while `mode: repo_root` changes the canonical input.

In `src/scherzo/workspace_run.gleam`, extend `PreparedStepWorkspace` with `mode: workflow_dag.WorkspaceMode`. Add a helper to compute the repository root from `orchestrator.config_dir` by taking the parent directory of the config directory and canonicalizing it with `scherzo/path.absolute`. For checked-in configuration this turns `.scherzo` into the repository root. Tests should create a config path like `test/tmp/<case>/.scherzo/scherzo.yaml` so the expected repo root is `test/tmp/<case>`.

In `src/scherzo/workspace_run.gleam`, split preparation by workspace mode. For `IsolatedWorkspace`, keep the existing `workspace_paths`, source-workspace validation, create hook, before-step hook, reuse, recovered validation, and cleanup behavior. For `RepoRootWorkspace`, compute the normal run root with `run_root_for`, create that run root, reject any source workspace, build a `PreparedStepWorkspace` whose `path` is the repository root and whose source fields are `None`, skip `run_create_hook`, run `run_before_step_hook`, and return it. Reusing a repo-root workspace inside the same run should be allowed by name and mode and should re-run only `before_step`, just as isolated workspace reuse does.

In `src/scherzo/workspace_run.gleam`, update recovered workspace validation. An isolated recovered workspace must still be inside the expected run root and inside the configured workspace root. A repo-root recovered workspace must have the same workflow ID, run ID, expected run root, workspace name, and mode, and its `path` must equal the computed repository root. This exception must not loosen isolated validation.

In `src/scherzo/workspace_run.gleam`, update `hook_env` to include `SCHERZO_WORKSPACE_MODE` with values `isolated` or `repo_root`. Keep `SCHERZO_RUN_ROOT` as the run root for both modes. For repo-root mode, `SCHERZO_WORKSPACE_PATH` is the repository root, `SCHERZO_WORKSPACE_NAME` is the YAML workspace name, and source workspace variables are empty strings. Keep `SCHERZO_WORKSPACE_ROOT` as the configured workspace root.

In `src/scherzo/workflow_run.gleam`, update `StepContext` and `step_command_env` to include `workspace_mode` and `workspace_root`. Command steps should receive `SCHERZO_WORKSPACE_MODE`, `SCHERZO_WORKSPACE_ROOT`, `SCHERZO_RUN_ROOT`, `SCHERZO_WORKSPACE_NAME`, and `SCHERZO_WORKSPACE_PATH`. This gives scripts enough information to place locks and temporary files outside the repository root while still running in the repository root.

In `src/scherzo/command_step.gleam`, separate the command current directory from the diagnostics directory. One concrete implementation is to add a new public function `run_with_env_and_diagnostics(step_id, command, workspace_path, diagnostics_root, timeout_ms, env, secrets, limits)` and have existing callers delegate to it with `diagnostics_root == workspace_path`. Update the workflow-run command dependency to pass `context.run_root` as the diagnostics root when `workspace_mode == RepoRootWorkspace`, and `context.workspace_path` for isolated mode. Keep artifact content and truncation behavior unchanged.

In `src/scherzo/workflow_run.gleam`, make sure ready-batch selection continues to serialize steps by resolved workspace path. Since repo-root steps resolve to the same repository-root path, multiple repo-root steps inside one workflow run cannot execute in the same ready batch. Do not rely on this for cross-run safety; the sync helper lock covers cross-run safety.

Create `scripts/scherzo-sync-main` as a POSIX shell script. It should require `jj` on PATH and should be intended to run with current directory equal to `SCHERZO_WORKSPACE_PATH`, which is the repository root in the checked-in workflow. It should use `SCHERZO_SYNC_MAIN_REMOTE` defaulting to `origin` and `SCHERZO_SYNC_MAIN_BRANCH` defaulting to `main`. It should create a state directory under `${SCHERZO_WORKSPACE_ROOT}/.scherzo-state`, acquire an atomic lock directory such as `sync-main.lock`, and remove that lock in a trap. If the lock already exists, print `SCHERZO_SYNC_MAIN_STATUS=skipped_locked` and exit zero.

In `scripts/scherzo-sync-main`, implement the command sequence as follows. First validate that `jj root --color=never` equals the current physical directory. Then run `jj git fetch --remote "$remote" --branch "$branch" --color=never`. Then capture `jj status --color=never`; if it does not contain the clean status text for the installed jj version, print the status output, print `SCHERZO_SYNC_MAIN_STATUS=skipped_dirty`, and exit zero. Then verify the current change is empty with a jj template such as `jj log -r @ --no-graph -T 'empty' --color=never`; if that template is not supported, replace it during implementation with the equivalent current jj command and record the discovery in this plan. If the current change is not empty, print `SCHERZO_SYNC_MAIN_STATUS=skipped_non_empty_change` and exit zero. Resolve the fetched target as `${branch}@${remote}` and verify it exists with `jj log -r "$target" --no-graph -T 'commit_id' --color=never`. Finally run `jj rebase -r @ -d "$target" --color=never`, run `jj status --color=never` again, and print `SCHERZO_SYNC_MAIN_STATUS=updated`. If the target is already the current parent and the current change is empty, it is acceptable to print `SCHERZO_SYNC_MAIN_STATUS=already_current` instead of rebasing.

Create `.scherzo/workflows/sync-main.yaml` with `version: 1`, `id: sync-main`, `max_parallel_steps: 1`, and one command step. The step ID should be `sync_main`, the command should be `scripts/scherzo-sync-main`, the timeout should be `300000`, and the workspace should be map-form with `name: repo` and `mode: repo_root`.

Update `.scherzo/scherzo.yaml` routing by adding `sync-main: workflows/sync-main.yaml` under `routing.workflows`. Update `linear_contract.workflow_labels` to include `sync-main` because exact workflow-label routing is enabled. Do not change existing workflow labels or routes.

## Concrete Steps

1. From the repository root, inspect source-control status:

       jj status --color=never

   Expect either `The working copy has no changes.` or a clear list of local changes. If local changes exist, understand them before editing; do not overwrite unrelated work.

2. In `src/scherzo/workflow_dag.gleam`, add `WorkspaceMode`, extend `WorkspaceRef`, and update the default inline and legacy constructors to use `IsolatedWorkspace`.

3. In `test/workflow_dag_test.gleam`, update existing expected `WorkspaceRef` values to include `mode: workflow_dag.IsolatedWorkspace`. Add a parser test that accepts:

       version: 1
       id: sync-main
       steps:
         - id: sync_main
           kind: command
           run: scripts/scherzo-sync-main
           workspace:
             name: repo
             mode: repo_root

   The test should assert the parsed workspace is `WorkspaceRef(name: "repo", from: None, mode: workflow_dag.RepoRootWorkspace)`.

4. In `test/workflow_dag_test.gleam`, add negative parser tests. One test should use an agent step with `mode: repo_root` and assert error code `repo_root_agent_step`. One test should use `mode: repo_root` with `from: main` and assert error code `repo_root_workspace_from`. One test should use `mode: banana` and assert error code `invalid_workspace_mode`.

5. Run the targeted parser tests:

       direnv exec . gleam test -- --suite unit --target erlang

   If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same command. Expect compile errors until all constructor call sites are updated; fix only the compile errors related to the new `WorkspaceRef` field.

6. In `src/scherzo/workflow_fingerprint.gleam`, update workspace canonical JSON as described above. In `test/workflow_fingerprint_test.gleam`, add tests for isolated fingerprint stability and repo-root fingerprint difference.

7. Commit milestone 1 after parser and fingerprint tests pass. Suggested commit message: `Add workflow workspace mode schema`.

8. In `src/scherzo/workspace_run.gleam`, add mode to `PreparedStepWorkspace`, compute repository root from `orchestrator.config_dir`, split isolated and repo-root preparation, and update recovered validation.

9. In `test/workspace_run_test.gleam`, add a repo-root preparation test. The fixture should create `test/tmp/workspace-run-repo-root/.scherzo`, resolve an orchestrator with config path `test/tmp/workspace-run-repo-root/.scherzo/scherzo.yaml`, and call `prepare_step` with `WorkspaceRef(name: "repo", from: None, mode: workflow_dag.RepoRootWorkspace)`. The create hook should write a marker if it runs; the test should assert that marker does not exist. The before-step hook should write `$SCHERZO_WORKSPACE_MODE|$SCHERZO_WORKSPACE_PATH|$SCHERZO_RUN_ROOT` to a file under the run root; the test should assert mode is `repo_root`, workspace path ends with `test/tmp/workspace-run-repo-root`, and run root ends with `/workspaces/<workflow>/<issue>/<run>`.

10. In `test/workspace_run_test.gleam`, add a cleanup safety test. Create a sentinel file such as `test/tmp/workspace-run-repo-root/sentinel.txt`, prepare a repo-root step, call `workspace_run.cleanup_run(prepared.run_root, orchestrator)`, and assert the run root no longer exists while the sentinel file still exists. Also assert no `workspaces/repo` directory was created under the run root.

11. In `src/scherzo/workflow_run.gleam`, add workspace mode and workspace root to `StepContext` and command environment. In `src/scherzo/command_step.gleam`, add the diagnostics-root variant and route repo-root command diagnostics to the run root.

12. In `test/command_step_test.gleam` or `test/workflow_run_test.gleam`, add a command-step test that runs a repo-root command which prints `$SCHERZO_WORKSPACE_MODE` and creates no diagnostics under the repository root. Assert command diagnostics are under the run root for repo-root mode.

13. Run workspace and command focused tests, then commit milestone 2. Suggested commit message: `Prepare repo-root command workspaces safely`.

14. Create `scripts/scherzo-sync-main` with the helper contract described in this plan and make it executable.

15. Add script tests. Prefer a new `test/sync_main_script_test.gleam` if existing project test helpers can run scripts with custom environments. Include at least these cases: lock already held returns exit zero and prints `SCHERZO_SYNC_MAIN_STATUS=skipped_locked`; dirty status after fetch returns exit zero and prints `SCHERZO_SYNC_MAIN_STATUS=skipped_dirty`; a clean empty-change repository with a newer `origin/main` exits zero, prints `SCHERZO_SYNC_MAIN_STATUS=updated` or `already_current`, and leaves `jj status --color=never` clean. If a full jj remote fixture is too slow for the unit suite, keep the lock and dirty tests in the unit suite with a fake `jj` executable on PATH and add the real jj happy-path scenario to `scripts/scherzo-test-local-integration`.

16. Commit milestone 3 after helper tests pass. Suggested commit message: `Add safe sync-main helper`.

17. Create `.scherzo/workflows/sync-main.yaml` with one repo-root command step. Update `.scherzo/scherzo.yaml` routing and `linear_contract.workflow_labels` to include `sync-main`.

18. Add a config or runtime bundle test, likely in `test/config_test.gleam` or the existing runtime-bundle tests, that loads `.scherzo/scherzo.yaml` and confirms the `sync-main` route resolves and the Linear contract labels still match routing.

19. Commit milestone 4 after config tests pass. Suggested commit message: `Check in sync-main workflow`.

20. Run full validation from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expect formatting to pass and all tests to pass. If the project uses the helper script suite in this checkout, also run:

       direnv exec . scripts/scherzo-test-unit

   Record notable output in this plan's Artifacts and Notes section and update Outcomes & Retrospective.

## Testing and Falsifiability

Parser tests in `test/workflow_dag_test.gleam` must prove three positive facts: existing string-form workspaces still parse as isolated, map-form `mode: isolated` parses as isolated, and map-form `mode: repo_root` parses as repo root for command steps. They must also prove three negative facts: repo-root agent steps are rejected, repo-root workspaces cannot derive from another workspace, and unknown workspace modes are rejected.

Fingerprint tests in `test/workflow_fingerprint_test.gleam` must prove that isolated workflow fingerprints remain stable when `mode` is omitted versus explicitly set to `isolated`, and that changing the same step to `mode: repo_root` changes canonical input and hash. This falsifies the claim that recovery and scheduling can distinguish repo-root workflows while avoiding unnecessary fingerprint churn for existing workflows.

Workspace-run tests in `test/workspace_run_test.gleam` must prove that repo-root mode creates a run root, returns the repository root as `PreparedStepWorkspace.path`, skips the create hook, runs the before-step hook with `SCHERZO_WORKSPACE_MODE=repo_root`, does not create a per-run workspace directory, and cleans up only the run root. The cleanup test must write a sentinel in the repository root and assert it survives cleanup.

Command execution tests in `test/command_step_test.gleam` or `test/workflow_run_test.gleam` must prove that repo-root command steps run with current directory equal to the repository root and receive `SCHERZO_WORKSPACE_PATH` equal to that directory. They must also assert command diagnostics are not written under repository-root `.scherzo/command-step-diagnostics`; they belong under the run root for repo-root steps.

Sync helper tests must prove dirty no-op behavior. A dirty scenario should arrange for `jj status --color=never` to report a modified file after fetch. The helper must exit zero, print `SCHERZO_SYNC_MAIN_STATUS=skipped_dirty`, and leave the file content unchanged. A non-empty current-change scenario should report no uncommitted working-copy changes but a non-empty `@` change; the helper must exit zero with `SCHERZO_SYNC_MAIN_STATUS=skipped_non_empty_change`.

The happy-path sync test must prove that a clean empty current jj change moves to the fetched main revision. The observable assertion is that after running `scripts/scherzo-sync-main`, `jj status --color=never` reports no changes and the parent or base revision of `@` is the fetched `${branch}@${remote}` revision. If the implementation uses a different jj command than `jj rebase -r @ -d "$target"`, the test must still assert this observable outcome.

The concurrency test must prove that if the sync-main lock already exists, the helper does not fetch or update. With a fake `jj` executable, assert the fake `jj` log file remains empty. With a real fixture, assert the checkout revision is unchanged. The helper must exit zero so a scheduled duplicate run is a safe no-op rather than a noisy failure.

## Validation and Acceptance

After implementation, run these commands from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . scripts/scherzo-test-unit

The expected result is that formatting passes and all tests pass. If `.envrc` is blocked, inspect it, run `direnv allow .`, and rerun the commands through direnv.

Acceptance is behavioral. A workflow file containing `workspace: main` still creates and uses an isolated per-run jj workspace. A workflow file containing a command step with `workspace: { name: repo, mode: repo_root }` creates a normal run root, runs the command in the repository root, and deletes only the run root during cleanup. A workflow file containing an agent step with `mode: repo_root` is rejected before execution.

The checked-in `.scherzo/workflows/sync-main.yaml` is routeable by `workflow:sync-main`. When run on a dirty checkout, it fetches first, prints `SCHERZO_SYNC_MAIN_STATUS=skipped_dirty`, exits zero, and leaves local file changes untouched. When run on a clean empty jj working-copy change, it fetches first, moves that empty change to the fetched `main@origin` revision, prints `SCHERZO_SYNC_MAIN_STATUS=updated` or `already_current`, exits zero, and leaves `jj status --color=never` clean.

Cleanup acceptance is explicit: no implementation may delete, move, or recreate the repository root. The proof is the cleanup safety test that writes a sentinel file in the repository root, prepares a repo-root step, runs cleanup for the run root, and then observes that the run root is gone while the sentinel remains.

## Rollout, Recovery, and Idempotence

The schema change is additive. Existing workflows omit `mode` and continue to use isolated workspaces. Existing `.scherzo/workflows/*.yaml` files do not need modification except for adding the new `sync-main` workflow and routing entry.

If parser or execution changes cause failures, rollback is straightforward: remove the `sync-main` route and workflow, revert the workspace-mode code, and existing isolated workflows return to the old path. Because the plan preserves isolated fingerprint canonical input, active isolated workflow recovery should not be invalidated solely by this schema addition.

The sync helper is idempotent. Running it repeatedly on a clean checkout that is already based on the fetched main revision prints `already_current` or performs a no-op rebase and remains clean. Running it on a dirty checkout is a no-op after fetch. Running it concurrently is a no-op for all but the lock holder.

If `scripts/scherzo-sync-main` fails after fetch but before rebase, the checkout remains at its old revision. If it fails during rebase, jj should leave a visible error and the workflow command should fail nonzero; the operator can inspect `jj status --color=never` and recover using normal jj commands. The helper must not run a reset or abandon command against non-empty local work.

## Artifacts and Notes

Expected repo-root workflow YAML shape:

    version: 1
    id: sync-main
    description: Fetch origin/main and update a clean operator checkout.
    max_parallel_steps: 1
    steps:
      - id: sync_main
        kind: command
        run: scripts/scherzo-sync-main
        timeout_ms: 300000
        workspace:
          name: repo
          mode: repo_root

Expected dirty helper transcript excerpt:

    Fetching origin main...
    Working copy is not clean; leaving checkout unchanged.
    SCHERZO_SYNC_MAIN_STATUS=skipped_dirty

Expected clean helper transcript excerpt:

    Fetching origin main...
    Updating empty working-copy change to main@origin...
    SCHERZO_SYNC_MAIN_STATUS=updated

Expected parser rejection for an unsafe workflow:

    repo_root_agent_step: agent steps may not use workspace mode repo_root

## Interfaces and Dependencies

In `src/scherzo/workflow_dag.gleam`, the public types should end in this shape:

    pub type WorkspaceMode {
      IsolatedWorkspace
      RepoRootWorkspace
    }

    pub type WorkspaceRef {
      WorkspaceRef(
        name: String,
        from: Option(String),
        mode: WorkspaceMode,
      )
    }

In `src/scherzo/workspace_run.gleam`, `PreparedStepWorkspace` should include mode while preserving existing fields:

    pub type PreparedStepWorkspace {
      PreparedStepWorkspace(
        workflow_id: String,
        run_id: String,
        run_root: String,
        attempt_index: Int,
        workspace_name: String,
        path: String,
        source_workspace_name: Option(String),
        source_workspace_path: Option(String),
        mode: workflow_dag.WorkspaceMode,
      )
    }

In `src/scherzo/workflow_run.gleam`, `StepContext` should include `workspace_mode` and `workspace_root`, and command environments should include:

    SCHERZO_CONFIG_DIR
    SCHERZO_WORKFLOW_ID
    SCHERZO_RUN_ID
    SCHERZO_RUN_ROOT
    SCHERZO_ISSUE_ID
    SCHERZO_ISSUE_IDENTIFIER
    SCHERZO_STEP_ID
    SCHERZO_ATTEMPT_INDEX
    SCHERZO_ATTEMPT_KEY
    SCHERZO_HOOK_IDEMPOTENCY_KEY
    SCHERZO_WORKSPACE_ROOT
    SCHERZO_WORKSPACE_NAME
    SCHERZO_WORKSPACE_PATH
    SCHERZO_WORKSPACE_MODE

`scripts/scherzo-sync-main` depends on `jj` and the checked-in Scherzo environment variables. It must not require pi, Linear credentials, or network access beyond the `jj git fetch` call to the configured remote. Tests may use `git` to construct a local remote fixture if that is the simplest reliable way to exercise the happy path.

## Open Questions and Clarifications Needed

None.
