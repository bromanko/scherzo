# Add workflow-level workspace hook profiles

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators need one orchestrator to run different kinds of workflows without hiding workspace lifecycle policy inside shell conditionals. After this change, the orchestrator config can define trusted named workspace hook profiles such as `isolated` and `noop`, and each workflow DAG can select one profile with a top-level `workspace_profile` field. Existing workflows that do not select a profile continue to use the default profile and behave like they do today with `workspace.hooks`.

The observable outcome is that a workflow like `examples/workflows/research.yaml` can say `workspace_profile: noop`, the daemon prepares its steps using only the `noop` hooks, and every hook sees `SCHERZO_WORKSPACE_PROFILE=noop`. A workflow that omits `workspace_profile` uses the configured default profile. Unknown profile names fail during workflow loading with a clear error before any issue is dispatched.

## Problem Framing and Constraints

Today Scherzo has one workspace lifecycle hook policy per orchestrator config. The same `workspace.hooks.create`, `before_step`, `after_step`, and `remove` snippets apply to every routed workflow. That forces operators to put workflow-specific branching into shell scripts, usually by checking `SCHERZO_WORKFLOW_ID`, when one orchestrator hosts workflows with different workspace needs. Implementation workflows often need isolated mutable source workspaces. Research, maintenance, or command-only workflows may need only a scratch directory, or a minimal workspace whose command manages its own current directory or external resource.

The native abstraction should describe that operator policy directly. It must stay VCS-agnostic: Scherzo core should not know about `git`, `jj`, repository roots, repo-root cleanup exceptions, or a native `repo_root` workspace mode. Existing step workspace semantics remain unchanged: `workspace: main` and `workspace: { name, from }` still name logical per-run step workspaces. The first version is workflow-level only. Step-level profile overrides are explicitly deferred because they would make one run mix lifecycle policies and would complicate fingerprinting, recovery, and cleanup before the workflow-level need is proven insufficient.

## Strategy Overview

Add a named profile layer around the existing `DagHooksConfig` shape. The orchestrator config owns profile definitions, including hook bodies and timeout. Workflow YAML owns only the profile selector. Runtime code resolves exactly one profile per workflow run and passes that resolved profile to workspace preparation, `before_step`, `after_step`, and cleanup. Hook environment gains `SCHERZO_WORKSPACE_PROFILE`.

Backward compatibility is additive. The existing direct `workspace.hooks` shape remains valid and is treated as a synthetic default profile named `default`. Existing workflow DAG files omit `workspace_profile`, so they select the default profile. Existing config tests that assert `orchestrator.dag_hooks` can keep passing initially because `dag_hooks` remains the resolved default hooks while new code uses the richer profile set for workflow-specific selection.

DAG-only fingerprints include a workflow's explicit `workspace_profile` selector when one is present, because it is part of the workflow YAML and changes execution behavior. They deliberately omit the field when a workflow omits `workspace_profile` so existing workflow files keep their previous canonical DAG shape. Execution fingerprints include the selected profile name and the selected resolved hook bodies for profile-based configs, but they must not include unselected profiles. For legacy direct `workspace.hooks` configs and workflows that omit `workspace_profile`, preserve the current canonical `dag_hooks` JSON and omit configured-profile metadata so existing workflows do not churn merely because the implementation learned about synthetic profiles.

## Alternatives Considered

The smallest possible alternative is to document the current workaround: branch inside `workspace.hooks` using `SCHERZO_WORKFLOW_ID`. That is insufficient because the operator-visible lifecycle policy stays hidden in shell dispatch code, cannot be validated against workflow names at load time, and is invisible in workflow fingerprints except as one large global hook script.

A native `mode: repo_root` or VCS-specific workspace mode was rejected. It would solve one dogfood case by baking repository-root or VCS behavior into Scherzo core, but the real gap is more general: different workflows need different hook policies. Scherzo should continue computing safe paths, lifecycle order, and environment variables while leaving repository-specific behavior to operator-controlled scripts.

Workflow-defined hook bodies were also rejected. They would make arbitrary lifecycle shell policy travel with workflow YAML, which is less safe for operators and harder to review. The orchestrator config is the right trust boundary for hooks; workflow files should only select from operator-approved profiles.

Step-level profile overrides were rejected for version 1. They can be considered later if a concrete workflow cannot be expressed with one profile per workflow, but adding them now would increase scheduler, fingerprint, and cleanup complexity without evidence that the workflow-level abstraction is too small.

## Risks and Countermeasures

The main compatibility risk is changing behavior for configs that already use direct `workspace.hooks`. Countermeasure: keep parsing direct hooks, synthesize the default profile from them, keep `orchestrator.dag_hooks` as the resolved default hooks, and add tests that direct-hook config produces the same hook bodies and no required workflow YAML changes.

A second risk is fingerprint churn. Countermeasure: include only the selected profile and selected hook bodies in execution fingerprints. Do not hash unselected profiles. Keep the legacy direct-hook canonical input compatible with the current `dag_hooks` object so existing deployments do not park all recovered runs after upgrade solely because the internal config model changed.

A third risk is unsafe cleanup or reuse with the wrong hook profile during recovery. Countermeasure: do not persist a profile field in the v1 ledger schema; instead, after the recovered workflow has passed execution-fingerprint validation, resolve the selected profile from the recovered workflow's current DAG and orchestrator config, fill `PreparedStepWorkspace.workspace_profile` from that resolved name, and pass that same profile to cleanup. If the workflow, selected profile, or execution fingerprint is unavailable or drifted, fail safe by retaining or parking the run according to the existing recovery path rather than reconstructing prepared workspaces or running a different profile's `remove` hook. This preserves legacy `StepAttemptPrepared` record compatibility because old records recover as the synthetic `default` profile only when their execution fingerprint still matches.

A fourth risk is startup or doctor probes accidentally running an arbitrary workflow profile or bypassing profile environment setup. Countermeasure: non-workflow workspace callers such as `doctor` and `probe` use the orchestrator default profile resolved from `orchestrator.workspace_profiles.default_profile`. If the default profile cannot resolve, startup or doctor fails with a clear workspace-profile error; these callers must not fall back to raw `orchestrator.dag_hooks` or invent a synthetic probe profile.

A fifth risk is unclear errors when a workflow selects a misspelled profile. Countermeasure: validate profile names strictly in workflow parsing and validate profile existence in runtime bundle loading. Unknown names fail with `unknown_workspace_profile` and a message that names the workflow id, requested profile, and available profile names.

A sixth risk is accidentally creating a VCS mode under a different name. Countermeasure: keep profiles as hook bodies only. Do not add enums such as `repo_root`, VCS names, or cleanup exceptions to `workflow_dag.gleam`, `workspace_run.gleam`, or config types.

## Progress

- [x] (2026-05-06 00:00Z) Drafted this ExecPlan from the Linear issue and current repository inspection.
- [x] (2026-05-06 00:30Z) Incorporated adversarial review findings for recovery, non-workflow callers, signature-threading detail, and fingerprint scope.
- [x] (2026-05-08 00:47Z) Implemented config profile types and backward-compatible parsing.
- [x] (2026-05-08 00:47Z) Implemented workflow-level `workspace_profile` parsing and validation.
- [x] (2026-05-08 00:47Z) Resolved profiles during bundle loading, runtime execution, cleanup, and fingerprinting.
- [x] (2026-05-08 00:47Z) Added focused tests for config, workflow parsing, runtime hooks, fingerprinting, recovery implications, and workflow loading failures.
- [x] (2026-05-08 00:47Z) Updated README, architecture notes, examples, and dogfood hook script guidance.
- [x] (2026-05-08 00:47Z) Applied plan-completion feedback by preserving the literal `workspace.hooks` in CLI usage text and updating this living ExecPlan completion state.
- [x] (2026-05-08 01:10Z) Applied post-plan Gleam review feedback by making command-step default timeouts use the selected workspace profile and adding focused regression coverage.

## Surprises & Discoveries

- Observation: The current orchestrator-specific workspace hooks live in `config_types.DagHooksConfig`, separate from the older global `HooksConfig` used by other lifecycle hooks.
  Evidence: `src/scherzo/config/types.gleam` defines both `HooksConfig` and `DagHooksConfig`; `src/scherzo/config.gleam` resolves `DagHooksConfig` from `workspace.hooks`.
- Observation: Workflow execution fingerprints already hash hook bodies and hook timeout.
  Evidence: `src/scherzo/workflow_fingerprint.gleam` has `dag_hooks_to_json` with `create`, `before_step`, `after_step`, `remove`, and `timeout_ms`.
- Observation: Hook execution currently reads directly from `orchestrator.dag_hooks` in all workspace lifecycle phases.
  Evidence: `src/scherzo/workspace_run.gleam` uses `orchestrator.dag_hooks` in `run_create_hook`, `run_before_step_hook`, `after_step`, and `cleanup_run`.
- Observation: Plan-completion validation caught that the CLI usage test intentionally requires the exact literal `workspace.hooks`, not only the looser phrase `workspace hooks/profiles`.
  Evidence: The verifier reported `main_test.usage_mentions_required_operational_constraints_test` failed because `main.usage()` did not contain `workspace.hooks`; after the usage text was changed to say `workspace.hooks or workspace profiles`, `direnv exec . gleam test` reported `831 passed, no failures`.
- Observation: The post-plan Gleam review found command-step default timeout selection still needed to follow the selected workspace profile, not the orchestrator default profile.
  Evidence: `src/scherzo/workflow_run.gleam` now unwraps command-step `timeout_ms` with `profile.hooks.timeout_ms`, and `test/workflow_run_test.gleam` has `command_default_timeout_uses_selected_workspace_profile_test` asserting `workspace_profile: noop` uses the `noop` timeout.

## Decision Log

- Decision: Profiles are defined only in orchestrator config and workflow YAML contains only a top-level `workspace_profile` selector.
  Rationale: The orchestrator config is the trust boundary for shell hooks; workflow YAML should not carry arbitrary lifecycle scripts.
  Date: 2026-05-06
- Decision: Direct `workspace.hooks` remains valid and is treated as a synthetic default profile named `default`.
  Rationale: This preserves existing config behavior while letting the runtime use one profile-resolution path.
  Date: 2026-05-06
- Decision: The selected profile is workflow-level for v1; step-level selection is rejected explicitly if encountered.
  Rationale: Workflow-level profiles solve the identified operator problem with less scheduler, cleanup, and fingerprint complexity.
  Date: 2026-05-06
- Decision: Execution fingerprints include only the selected profile and selected resolved hooks, not every configured profile.
  Rationale: Unselected profiles should not park or invalidate runs for workflows that do not use them.
  Date: 2026-05-06
- Decision: DAG-only fingerprints include an explicit workflow `workspace_profile` selector, but omit the field when the selector is absent.
  Rationale: An explicit selector is part of workflow YAML semantics and should not be invisible to callers of `workflow_fingerprint.fingerprint`; omitting the absent field preserves canonical input for existing workflow files.
  Date: 2026-05-06
- Decision: `PreparedStepWorkspace.workspace_profile` is reconstructed after fingerprint validation instead of persisted in the v1 ledger schema.
  Rationale: The selected profile is already covered by the execution fingerprint; leaving `record.StepAttemptPrepared` unchanged preserves old ledger compatibility and avoids a migration for an in-memory safety guard.
  Date: 2026-05-06
- Decision: Non-workflow service, doctor, and probe workspace callers use the orchestrator default workspace profile.
  Rationale: These callers have no workflow DAG selector, but they still need one trusted profile and consistent `SCHERZO_WORKSPACE_PROFILE` hook environment; the default profile is the only operator-configured policy that applies globally.
  Date: 2026-05-06
- Decision: CLI usage must keep the literal spelling `workspace.hooks` while also mentioning workspace profiles.
  Rationale: Direct `workspace.hooks` remains a supported compatibility path, and the existing usage test encodes that operators should continue to see that exact configuration surface in help text.
  Date: 2026-05-08
- Decision: Command-step default timeouts use the selected workspace profile's hook timeout.
  Rationale: Operators expect one profile selection to govern both lifecycle hooks and the command timeout default derived from those hooks; falling back to the orchestrator default profile would make workflows selecting `noop` or another configured profile execute with the wrong timeout unless every command step repeated an explicit timeout.
  Date: 2026-05-08

## Outcomes & Retrospective

Implementation completed the required workflow-level workspace hook profile behavior. The orchestrator config now supports backward-compatible direct `workspace.hooks` plus named `workspace.profiles`, workflows can select a top-level `workspace_profile`, runtime bundle loading rejects unknown selectors, execution and DAG fingerprints account for the selected profile without hashing unselected profiles, workspace lifecycle hooks receive and use the resolved profile, doctor/probe paths use the default profile, recovery reconstructs in-memory prepared workspaces with the selected profile after fingerprint validation, and documentation/examples describe the native profile abstraction.

The plan-completion repair closed the two verifier blockers: CLI usage once again contains the exact literal `workspace.hooks`, and this living ExecPlan now records the completed required implementation milestones. The full test suite passed with `831 passed, no failures`. Deferred items remain the original non-goals: no native `repo_root` mode, no VCS-specific behavior in core, no workflow-defined hook scripts, no step-level profile overrides, and no persisted profile field in v1 ledger records.

Post-review feedback closed the remaining timeout consistency gap: command steps that omit an explicit timeout now inherit the selected workspace profile's `timeout_ms`, and a focused workflow-run regression test protects that behavior. Targeted formatting validation passed after this review repair; the workflow's final validation should rerun the complete validation gate before publish.

## Context and Orientation

Scherzo is a Gleam service that routes tracker issues into YAML workflow DAGs. A DAG is a directed acyclic graph: steps can depend on earlier steps, and Scherzo runs ready steps when their dependencies have completed. Workspace lifecycle hooks are trusted shell snippets that Scherzo runs while preparing and cleaning per-run workspaces. The hook snippets are trusted because they execute local shell commands.

The current config path is `src/scherzo/config.gleam`. It parses the orchestrator YAML into types from `src/scherzo/config/types.gleam`. The current workflow parser is `src/scherzo/workflow_dag.gleam`. Runtime bundle loading in `src/scherzo/runtime_bundle.gleam` loads the orchestrator config, then loads each workflow path from `routing.workflows`, verifies the routing key matches the DAG `id`, resolves prompt files, and validates model settings. Workspace lifecycle execution is in `src/scherzo/workspace_run.gleam`. Workflow execution fingerprints are in `src/scherzo/workflow_fingerprint.gleam`. Recovery and daemon startup compare execution fingerprints in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/workflow_attempt.gleam`.

Not every workspace lifecycle caller is a routed workflow. `src/scherzo/orchestrator/service.gleam` prepares and cleans synthetic doctor/probe workspaces through `workspace_run.prepare_step` and `workspace_run.cleanup_run`, using workflow ids such as `doctor` and `probe` only as hook environment labels. These non-workflow callers do not have a workflow DAG selector, so they must use the orchestrator default profile.

Prepared workspace information is recorded in the append-only ledger as `record.StepAttemptPrepared`, written from `src/scherzo/workflow_checkpoint.gleam`, decoded in `src/scherzo/state/record.gleam`, summarized by `src/scherzo/state/recovery.gleam`, and reconstructed as `workspace_run.PreparedStepWorkspace` in `src/scherzo/orchestrator/daemon.gleam`. A ledger is an append-only file of workflow events; recovery replays it after a restart to resume safe work. The v1 profile design does not add a persisted profile field to that ledger record.

The existing config shape is:

    workspace:
      root: workspaces
      hooks:
        create: |
          mkdir -p "$SCHERZO_WORKSPACE_PATH"
        before_step: |
          test -d "$SCHERZO_WORKSPACE_PATH"
        after_step: |
          true
        remove: |
          true
        timeout_ms: 60000

The new profile shape should be:

    workspace:
      root: workspaces
      default_profile: isolated
      profiles:
        isolated:
          hooks:
            create: |
              mkdir -p "$SCHERZO_WORKSPACE_PATH"
            before_step: |
              test -d "$SCHERZO_WORKSPACE_PATH"
            after_step: |
              true
            remove: |
              true
            timeout_ms: 60000
        noop:
          hooks:
            create: |
              mkdir -p "$SCHERZO_WORKSPACE_PATH"
            before_step: |
              true
            after_step: |
              true
            remove: |
              true
            timeout_ms: 60000

A workflow selects a profile like this:

    version: 1
    id: maintenance-example
    workspace_profile: noop
    steps:
      - id: run_maintenance
        kind: command
        run: ./scripts/some-maintenance-task
        workspace: main

This plan intentionally does not define a native `repo_root` mode. A profile may run repository-specific scripts, but Scherzo core only knows the selected profile name, hook bodies, timeout, safe workspace paths, and lifecycle order.

## Preconditions and Verified Facts

The working copy was clean before drafting. `jj status --color=never` reported no changes.

`src/scherzo/config/types.gleam` currently defines `WorkspaceConfig(root: String)`, `DagHooksConfig(create, before_step, after_step, remove, timeout_ms)`, and `OrchestratorConfig(... dag_hooks, artifact_limits, model_settings)`. `src/scherzo/config.gleam` has `resolve_orchestrator_root`, which calls `resolve_dag_hooks`; `resolve_dag_hooks` reads `workspace.hooks` and defaults `timeout_ms` to `60_000`.

`src/scherzo/workflow_dag.gleam` currently defines `WorkflowDag(id, description, max_parallel_steps, steps)` with no profile selector. `parse_root` requires `version: 1`, reads `id`, reads `max_parallel_steps`, reads `steps`, and validates dependencies, acyclicity, workspace sources, and terminal sinks. `WorkspaceRef(name, from)` is the existing per-step workspace reference and must not be repurposed for profiles.

`src/scherzo/runtime_bundle.gleam` loads orchestrator config first, then loads workflow DAGs from `orchestrator.routing.workflows`, rejects routing key and DAG id mismatches, resolves prompt files, and validates model settings. This is the right place to reject a workflow that selects an unknown workspace profile, because both the loaded workflow and config profile map are available there.

`src/scherzo/workspace_run.gleam` currently calls `run_create_hook`, `run_before_step_hook`, `after_step`, and `cleanup_run` using `orchestrator.dag_hooks`. Its `hook_env` already provides `SCHERZO_CONFIG_DIR`, `SCHERZO_WORKFLOW_ID`, `SCHERZO_RUN_ID`, `SCHERZO_RUN_ROOT`, `SCHERZO_ISSUE_ID`, `SCHERZO_ISSUE_IDENTIFIER`, `SCHERZO_STEP_ID`, `SCHERZO_ATTEMPT_INDEX`, `SCHERZO_ATTEMPT_KEY`, `SCHERZO_HOOK_IDEMPOTENCY_KEY`, `SCHERZO_WORKSPACE_ROOT`, `SCHERZO_WORKSPACE_NAME`, `SCHERZO_WORKSPACE_PATH`, `SCHERZO_SOURCE_WORKSPACE_NAME`, and `SCHERZO_SOURCE_WORKSPACE_PATH`. Add `SCHERZO_WORKSPACE_PROFILE` here.

`src/scherzo/orchestrator/service.gleam` currently wires `default_doctor_dependencies().prepare_step` to `workspace_run.prepare_step` and `cleanup_run` to `workspace_run.cleanup_run`. `prepare_doctor_workspace` calls the dependency with workflow id, run id, and step id all set to `doctor`; `append_cleanup_warning_if_needed` cleans the doctor run root; `run_pi_probe_orchestrator` directly calls `workspace_run.prepare_step` and `workspace_run.cleanup_run` with synthetic `probe` ids. These call sites must resolve and pass the orchestrator default profile.

`src/scherzo/workflow_checkpoint.gleam` writes `record.StepAttemptPrepared` with run id, workflow id, step id, attempt index, workspace name, workspace path, run root, and optional source workspace fields. `src/scherzo/state/record.gleam` encodes and decodes the same fields. `src/scherzo/state/recovery.gleam` builds `RecoveredWorkspaceSummary` without a profile field. `src/scherzo/orchestrator/daemon.gleam` reconstructs prepared workspaces in `recovered_workspaces_to_prepared`. Do not change the persisted record shape for v1; instead, set the in-memory `workspace_profile` field during reconstruction after the selected profile has been resolved and the execution fingerprint has matched.

`src/scherzo/workflow_fingerprint.gleam` currently hashes canonical DAG fields, `dag_hooks`, artifact limits, and global model settings. `test/workflow_fingerprint_test.gleam` already has a test proving execution fingerprints change when hook bodies or artifact limits change.

The repository uses Gleam and gleeunit. The normal validation commands from the repo root are `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same direnv-backed commands.

## Scope Boundaries

In scope:

- Add named workspace lifecycle profiles to orchestrator config under `workspace.profiles`.
- Add optional workflow-level `workspace_profile` parsing to `src/scherzo/workflow_dag.gleam`.
- Validate profile names, profile shape, default profile existence, and workflow selectors.
- Resolve exactly one workspace profile per workflow run and use its hooks for create, before-step validation, after-step best-effort hook, and remove cleanup.
- Resolve the orchestrator default profile for non-workflow doctor/probe workspace preparation and cleanup.
- Add `SCHERZO_WORKSPACE_PROFILE` to hook environment.
- Update DAG and execution fingerprinting to account for explicit selectors, the selected profile, and selected hook bodies without hashing unselected profiles.
- Reconstruct the in-memory `PreparedStepWorkspace.workspace_profile` for recovered prepared workspaces after execution-fingerprint validation.
- Update tests, README, architecture notes, examples, and local dogfood guidance to explain native profiles.

Out of scope:

- Do not implement native `mode: repo_root`.
- Do not encode `git`, `jj`, worktree, or workspace cleanup exceptions in Scherzo core.
- Do not let workflow YAML define raw hook scripts.
- Do not add step-level profile overrides in v1.
- Do not add `workspace_profile` to persisted `StepAttemptPrepared` ledger records in v1.
- Do not redesign the sync-main workflow. It can be mentioned only as a motivating example.
- Do not change existing `workspace: main` or `workspace: { name, from }` step semantics.

## Milestones

Milestone 1 adds the config model and parser support while preserving direct `workspace.hooks`. At the end, orchestrator config can resolve profile maps, legacy direct hooks still resolve as before, and invalid profile config fails with clear `InvalidConfig` messages.

Milestone 2 adds workflow parser support. At the end, `workflow_dag.parse` returns a `WorkflowDag` with `workspace_profile: Option(String)`, valid top-level selectors parse, invalid selector names fail, and step-level selectors fail as unsupported.

Milestone 3 validates profile selectors during runtime bundle loading and updates fingerprinting. At the end, unknown profile names fail before dispatch, DAG-only fingerprints distinguish workflows that explicitly select different profiles, execution fingerprints change when the selected profile name or selected hook bodies change, and unselected profile changes do not affect a workflow fingerprint.

Milestone 4 updates workspace runtime execution and non-workflow workspace callers. At the end, create, before-step, after-step, and remove use the selected profile's hooks for workflow runs; doctor and probe workspaces use the orchestrator default profile; and every hook script receives `SCHERZO_WORKSPACE_PROFILE`.

Milestone 5 updates recovery paths, tests, and documentation. At the end, recovery reconstructs in-memory prepared workspaces with the selected profile only after execution-fingerprint validation, legacy `StepAttemptPrepared` records remain readable, and docs describe profiles as the native abstraction while distinguishing them from repo-local dogfood scripts.

## Plan of Work

In `src/scherzo/config/types.gleam`, add profile types without removing `DagHooksConfig`:

    pub type WorkspaceProfileSource {
      LegacyWorkspaceHooks
      ConfiguredWorkspaceProfile
    }

    pub type WorkspaceHookProfile {
      WorkspaceHookProfile(
        name: String,
        hooks: DagHooksConfig,
        source: WorkspaceProfileSource,
      )
    }

    pub type WorkspaceHookProfiles {
      WorkspaceHookProfiles(
        default_profile: String,
        profiles: Dict(String, WorkspaceHookProfile),
      )
    }

Then extend `OrchestratorConfig` with `workspace_profiles: WorkspaceHookProfiles` while keeping `dag_hooks: DagHooksConfig`. Keep `dag_hooks` equal to the default profile hooks for compatibility with tests and callers that are converted later in the implementation.

In `src/scherzo/config.gleam`, replace `resolve_dag_hooks(root)` with a two-layer resolver. First, keep a helper that reads a hook map into `DagHooksConfig`; it should be usable for both `workspace.hooks` and `workspace.profiles.<name>.hooks`. Second, add `resolve_workspace_profiles(root)`. The resolver must implement these rules:

- Profile names must use the same lowercase/digit/underscore/hyphen style as workflow and workspace ids: non-empty, first character lowercase letter or digit, remaining characters lowercase letters, digits, `_`, or `-`.
- A profile entry must be a map with a `hooks` map. Non-map profile entries fail with `workspace.profiles.<name> must be a map`. A non-map `hooks` value fails with `workspace.profiles.<name>.hooks must be a map`.
- `timeout_ms` defaults to `60_000` per profile and must be positive. Direct legacy `workspace.hooks.timeout_ms` keeps the existing positive validation message.
- If direct `workspace.hooks` exists or if no `workspace.profiles` exist, synthesize a profile named `default` with source `LegacyWorkspaceHooks` from the direct hooks. This exactly preserves configs with only direct hooks and configs with no workspace hooks.
- If `workspace.profiles` exists, parse each configured profile with source `ConfiguredWorkspaceProfile`.
- If direct hooks and configured profiles both exist, allow both unless a configured profile is named `default`. If a configured `default` collides with the synthetic legacy `default`, fail with `workspace.profiles.default conflicts with legacy workspace.hooks; move the legacy hooks into profiles.default or rename the profile`.
- If `workspace.default_profile` is present, validate that it is a valid profile name and exists after synthesis and profile parsing.
- If `workspace.default_profile` is absent and the synthetic legacy `default` exists, use `default`.
- If `workspace.default_profile` is absent, no direct hooks exist, and configured profiles exist, fail with `workspace.default_profile is required when workspace.profiles is set without workspace.hooks`.

`resolve_orchestrator_root` should call `resolve_workspace_profiles(root)`, set `dag_hooks` to the selected default profile hooks, and store the full `workspace_profiles` on `OrchestratorConfig`.

In `src/scherzo/workflow_dag.gleam`, add `workspace_profile: Option(String)` to `WorkflowDag`. In `parse_root`, read optional top-level `workspace_profile` after the id is validated and before constructing the DAG. Add a private `read_workspace_profile` helper that returns `Ok(None)` when the key is absent, `Ok(Some(profile))` when the key is a valid string profile name, `DagError("workspace_profile_not_string", "workspace_profile must be a string")` for non-strings, and `DagError("invalid_workspace_profile", "invalid workspace_profile: " <> profile)` for invalid names. Duplicate the small profile-name validator in this module or extract a shared identifier helper if that is simpler; do not export private config parser internals just for this.

Still in `src/scherzo/workflow_dag.gleam`, reject `workspace_profile` inside a step map. Add a check at the start of `read_step` after confirming the node is a map. If the key exists on a step, return `DagError("step_workspace_profile_not_supported", "workspace_profile is only valid at workflow top level")`. This makes the v1 non-goal explicit instead of silently ignoring a step-level typo.

Add a small helper module, `src/scherzo/workspace_profile.gleam`, to avoid scattering lookup rules. It should expose at least:

    pub type ProfileResolutionError {
      UnknownWorkspaceProfile(workflow_id: String, profile_name: String, available: List(String))
    }

    pub fn selected_name(dag: workflow_dag.WorkflowDag, orchestrator: config_types.OrchestratorConfig) -> String

    pub fn resolve(dag: workflow_dag.WorkflowDag, orchestrator: config_types.OrchestratorConfig) -> Result(config_types.WorkspaceHookProfile, ProfileResolutionError)

`selected_name` returns `dag.workspace_profile` when present, otherwise `orchestrator.workspace_profiles.default_profile`. `resolve` looks up the selected name in `orchestrator.workspace_profiles.profiles`, sorts available profile names in error messages, and does not fall back to another profile on miss.

In `src/scherzo/runtime_bundle.gleam`, call the new resolver for every loaded workflow after `load_workflow_map` and before returning `RuntimeBundle`. Convert misses to `BundleError("unknown_workspace_profile", message)`. The message should be clear enough for an operator, for example: `workflow maintenance-example selects unknown workspace_profile noop; available profiles: default, isolated`. If no profiles are available, say `available profiles: none`.

In `src/scherzo/workflow_fingerprint.gleam`, update DAG and execution fingerprint APIs for the selector. In `dag_to_json`, include `#("workspace_profile", json.string(profile))` only when `dag.workspace_profile` is `Some(profile)`. Do not emit `workspace_profile: null` when the field is absent; that preserves the canonical DAG input for existing workflow files. Change `for_execution` and `fingerprint_for_execution` to resolve the selected profile from the DAG and orchestrator; if lookup fails, return an error from `fingerprint_for_execution` and use the bundle-level validation to prevent normal runtime misses. Add a new test-friendly function that accepts a resolved profile directly, for example:

    pub fn for_execution_profile_options(
      workflow_id: String,
      dag: workflow_dag.WorkflowDag,
      profile: config_types.WorkspaceHookProfile,
      artifact_limits: config_types.ArtifactLimits,
      model_settings: model_config.Settings,
    ) -> String

The canonical execution JSON should keep the existing `dag_hooks` object for hook bodies and timeout. For configured profiles, add a `workspace_profile` object containing at least `name` and `source: "configured"`. For the synthetic legacy direct-hook profile, omit the `workspace_profile` object and emit exactly the existing `dag_hooks` object shape so legacy direct-hook configs avoid unnecessary fingerprint churn. In every case, never include unselected profiles.

In `src/scherzo/workspace_run.gleam`, stop reading hook bodies from `orchestrator.dag_hooks` inside lifecycle functions. Pass the whole `config_types.WorkspaceHookProfile` so `hook_env` can set `SCHERZO_WORKSPACE_PROFILE` from `profile.name`. Change the public signatures exactly as follows: `prepare_step`, `prepare_step_attempt`, `prepare_recovered_step`, and `prepare_recovered_step_attempt` receive `profile: config_types.WorkspaceHookProfile` after the `orchestrator` argument and before `known_workspaces`; `after_step` receives `profile` after `orchestrator`; and `cleanup_run` receives `profile` after `orchestrator`. Use `profile.hooks.create`, `profile.hooks.before_step`, `profile.hooks.after_step`, `profile.hooks.remove`, and `profile.hooks.timeout_ms` for lifecycle execution. Keep the existing fallback where a missing `create` hook creates the workspace directory with `simplifile.create_directory_all`.

Thread `profile` through the private functions `prepare_step_attempt_with_cleanup`, `finish_prepare_step`, `reuse_prepared_workspace`, `run_create_hook`, `run_before_step_hook`, and `hook_env`. In `prepare_step_attempt_with_cleanup`, the cleanup-on-create-or-hook-failure path must call `cleanup_run(run_root, orchestrator, profile)` so cleanup uses the same profile that attempted creation. Do not keep any lifecycle path that reads `orchestrator.dag_hooks` directly.

Update `PreparedStepWorkspace` with a `workspace_profile: String` field. Set it to `profile.name` when a workspace is first prepared, preserve it when reusing a prepared workspace, and include it when constructing the dummy prepared workspace used by `cleanup_run` for the `remove` hook. When `reusable_workspace` returns an existing workspace, and when `source_workspace` returns a source workspace, validate that the prepared workspace profile equals the current run's selected profile. If it does not, return `WorkspaceFailure(error.WorkspaceIo("prepared workspace profile mismatch"))`; this should never happen within one workflow-level profile run, but the guard prevents accidental cross-run reuse if future code changes.

In `hook_env`, add:

    #("SCHERZO_WORKSPACE_PROFILE", profile.name)

The value must be present for create, before_step, after_step, and remove hooks. If `hook_env` reads from `prepared.workspace_profile` instead of receiving `profile`, assert by construction that `prepared.workspace_profile` was set from the resolved profile before calling it.

In `src/scherzo/workflow_run.gleam`, update the `Dependencies` type so `prepare_step`, `prepare_recovered_step`, `after_step`, and `cleanup_run` all take a `config_types.WorkspaceHookProfile` argument in the same positions used by `workspace_run.gleam`. Update `default_dependencies` to pass the new `workspace_run` functions. Resolve the profile once at the beginning of `execute_with_context` for normal runs and once at the beginning of `execute_with_resume` before recovered execution resumes, using `workspace_profile.resolve(dag, orchestrator)`. Thread the resolved profile through `run_workflow_context`, `prepare_step_for_mode`, `run_after_step`, `cleanup_if_allowed`, and `cleanup_if_needed`. Do not resolve per step. If resolution fails despite bundle validation, fail the workflow before starting steps with a clear fatal diagnostic such as `workspace_profile_resolution_failed:<profile>`.

In `src/scherzo/orchestrator/daemon.gleam`, make recovery reconstruction explicit. In `run_recovered_workflow_worker`, keep the existing order: select the workflow, compare the execution fingerprint with `recovered_workflow_identity_matches`, and only inside the `True` branch resolve `workspace_profile.resolve(dag, bundle.orchestrator)`. If profile resolution fails, return a recovery failure such as `workflow_recovery_invalid:workspace_profile_unavailable` and leave the run root untouched. Change `recovered_workspaces_to_prepared` to accept `profile_name: String`; set `workspace_profile: profile_name` on every reconstructed `workspace_run.PreparedStepWorkspace`. Do not call `recovered_workspaces_to_prepared` when workflow selection fails, fingerprint comparison fails, or profile resolution fails.

Do not change `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/record.gleam`, or `src/scherzo/state/recovery.gleam` to persist a profile field for v1. `record.StepAttemptPrepared` and `RecoveredWorkspaceSummary` remain backward-compatible. Legacy direct-hook ledgers recover because `fingerprint_for_execution` preserves the old direct-hook canonical input and the reconstructed profile name is the synthetic `default` only after the fingerprint matches.

In `src/scherzo/orchestrator/service.gleam`, update non-workflow callers to use the orchestrator default profile. Update `DoctorDependencies.prepare_step` and `DoctorDependencies.cleanup_run` signatures to include `config_types.WorkspaceHookProfile`, update `default_doctor_dependencies`, and add a private helper such as `default_workspace_profile(orchestrator)` that looks up `orchestrator.workspace_profiles.default_profile` in `orchestrator.workspace_profiles.profiles`. In `run_workspace_and_pi_checks`, resolve the default profile once before calling `prepare_doctor_workspace`. If the default profile is unavailable, append a `doctor.WorkspaceHooks` failure with code `workspace_profile_unavailable`, skip `doctor.PiProbe` when it was selected because there is no workspace to probe, and do not run cleanup. Pass the resolved profile into `prepare_doctor_workspace` and `append_cleanup_warning_if_needed`, and have those helpers pass it to `dependencies.prepare_step` and `dependencies.cleanup_run`. In `run_pi_probe_orchestrator`, resolve the default profile before calling `workspace_run.prepare_step`, pass it to `workspace_run.cleanup_run`, and map an unavailable default to `StartupError("workspace_profile_unavailable", "default workspace profile unavailable")`. Do not use `orchestrator.dag_hooks` as a fallback and do not invent a `probe` profile.

In `src/scherzo/workflow_attempt.gleam`, keep using `workflow_fingerprint.fingerprint_for_execution(dag, orchestrator)`. After the fingerprint function changes, it will include the selected profile. No call site should pass global `orchestrator.dag_hooks` directly for workflow execution identity.

Update documentation in `README.md` and `docs/ARCHITECTURE.md`. The README should show both legacy direct hooks and the preferred profile shape, document top-level workflow `workspace_profile`, list `SCHERZO_WORKSPACE_PROFILE` in the hook environment, and say workflow YAML cannot define hook scripts. The architecture doc should say workspaces are hook-owned and hook profiles are orchestrator-defined, workflow-selected lifecycle policies. Update `examples/scherzo.yaml` to demonstrate `workspace.default_profile` and `workspace.profiles` while preserving an easy legacy migration story. Update `examples/workflows/research.yaml` to select `workspace_profile: noop`; if that file has drifted out of the tree by implementation time, add a minimal routed example workflow under `examples/workflows/research.yaml` and route it from `examples/scherzo.yaml`. If dogfood scripts under `scripts/` continue to implement repository-specific behavior, document them as repo-local profile hook bodies rather than native workspace modes.

## Concrete Steps

1. From the repo root, run `jj status --color=never` and confirm the working copy is clean or contains only intentional plan/test changes from this implementation.
2. Edit `src/scherzo/config/types.gleam` to add `WorkspaceProfileSource`, `WorkspaceHookProfile`, and `WorkspaceHookProfiles`; extend `OrchestratorConfig` with `workspace_profiles` while keeping `dag_hooks`.
3. Edit `src/scherzo/config.gleam` to extract a reusable hook-map reader from `resolve_dag_hooks`.
4. In `src/scherzo/config.gleam`, add profile-name validation and `resolve_workspace_profiles` with the exact rules from the Plan of Work.
5. Update `resolve_orchestrator_root` in `src/scherzo/config.gleam` to call `resolve_workspace_profiles`, populate `orchestrator.workspace_profiles`, and set `orchestrator.dag_hooks` to the default profile's hooks.
6. Add or update tests in `test/orchestrator_config_test.gleam` for legacy direct hooks, profile config, direct-hooks-plus-extra-profile config, invalid profile names, profile entry type errors, missing `default_profile`, unknown `default_profile`, and direct/profile `default` collision.
7. Run `direnv exec . gleam test`. Expect the new config tests to pass before moving on.
8. Commit milestone 1 with a message like `Add orchestrator workspace hook profile config` after tests pass.
9. Edit `src/scherzo/workflow_dag.gleam` to add `workspace_profile: Option(String)` to `WorkflowDag`, parse top-level `workspace_profile`, and reject step-level `workspace_profile`.
10. Update all existing `WorkflowDag(...)` construction sites in `src/scherzo/workflow_dag.gleam`, `src/scherzo/runtime_bundle.gleam`, and tests to include or preserve `workspace_profile`.
11. Add tests in `test/workflow_dag_test.gleam` proving a valid top-level selector parses to `Some("noop")`, omitted selector parses to `None`, non-string selector returns `workspace_profile_not_string`, invalid selector returns `invalid_workspace_profile`, and step-level selector returns `step_workspace_profile_not_supported`.
12. Run `direnv exec . gleam test`. Expect workflow parser tests to pass.
13. Commit milestone 2 with a message like `Add workflow-level workspace profile selector`.
14. Create `src/scherzo/workspace_profile.gleam` with `selected_name` and `resolve` helpers.
15. Edit `src/scherzo/runtime_bundle.gleam` to validate every loaded workflow selector against `orchestrator.workspace_profiles` and return `BundleError("unknown_workspace_profile", ...)` on miss.
16. Add tests in `test/runtime_bundle_test.gleam` with a temporary config and workflow that selects an unknown profile; assert the bundle error code is `unknown_workspace_profile` and the message contains the workflow id and profile name. Add a passing bundle test where one workflow uses `workspace_profile: noop` and another omits the field and gets the default.
17. Edit `src/scherzo/workflow_fingerprint.gleam` so `dag_to_json` includes explicit `workspace_profile` selectors, execution fingerprints resolve and hash the selected profile, configured-profile metadata is present only for configured profiles, legacy direct-hook canonical input remains unchanged, and unselected profiles are excluded.
18. Update `test/workflow_fingerprint_test.gleam` so DAG-only fingerprints differ when two workflow files explicitly select different profiles, omitted `workspace_profile` does not add a `workspace_profile: null` field to canonical DAG JSON, execution fingerprints differ when selected profile names differ, execution fingerprints differ when selected hook bodies differ, unselected profile changes do not affect a workflow execution fingerprint, and legacy direct-hook canonical execution JSON has no configured `workspace_profile` object.
19. Run `direnv exec . gleam test`. Expect bundle and fingerprint tests to pass.
20. Commit milestone 3 with a message like `Validate and fingerprint selected workspace profiles`.
21. Edit `src/scherzo/workspace_run.gleam` to add `workspace_profile: String` to `PreparedStepWorkspace` and add `profile: config_types.WorkspaceHookProfile` to `prepare_step`, `prepare_step_attempt`, `prepare_recovered_step`, `prepare_recovered_step_attempt`, `after_step`, and `cleanup_run` in the positions described in the Plan of Work.
22. Still in `src/scherzo/workspace_run.gleam`, thread `profile` through `prepare_step_attempt_with_cleanup`, `finish_prepare_step`, `reuse_prepared_workspace`, `run_create_hook`, `run_before_step_hook`, and `hook_env`; replace all lifecycle uses of `orchestrator.dag_hooks` with `profile.hooks`.
23. Still in `src/scherzo/workspace_run.gleam`, set `PreparedStepWorkspace.workspace_profile` from `profile.name`, validate reusable and source workspaces against `profile.name`, make cleanup-on-prepare-failure call `cleanup_run(run_root, orchestrator, profile)`, and make the remove-hook dummy workspace carry the selected profile name.
24. Update `test/workspace_run_test.gleam` helpers to build orchestrator configs with profiles and pass the resolved profile into workspace preparation. Add assertions that create, before-step, after-step, and remove hooks see `SCHERZO_WORKSPACE_PROFILE`, that selecting `noop` uses the `noop` hook body rather than the default hook body, that mismatched prepared workspace reuse returns `prepared workspace profile mismatch`, and that cleanup after a create/before-step failure uses the same profile.
25. Edit `src/scherzo/workflow_run.gleam` to update `Dependencies`, `default_dependencies`, `execute_with_context`, `execute_with_resume`, `run_workflow_context`, `prepare_step_for_mode`, `run_after_step`, `cleanup_if_allowed`, and `cleanup_if_needed` so the selected profile is resolved once and passed explicitly to prepare, after-step, and cleanup calls.
26. Update workflow-run tests and stubs that construct `workflow_run.Dependencies`. Add a focused test, if one exists near workflow-run failure handling, that a profile resolution error fails before any step starts; otherwise rely on the bundle validation test and the compile-time dependency signature updates.
27. Edit `src/scherzo/orchestrator/service.gleam` to update `DoctorDependencies.prepare_step` and `DoctorDependencies.cleanup_run` signatures, `default_doctor_dependencies`, `run_workspace_and_pi_checks`, `prepare_doctor_workspace`, `append_cleanup_warning_if_needed`, and `run_pi_probe_orchestrator` so doctor/probe workspaces resolve and pass the orchestrator default profile.
28. Add tests in `test/orchestrator_service_doctor_test.gleam` and `test/orchestrator_service_test.gleam` proving doctor workspace preparation, doctor cleanup, and startup pi probe use the default profile name and report `workspace_profile_unavailable` when the default profile cannot resolve.
29. Edit `src/scherzo/orchestrator/daemon.gleam` so `run_recovered_workflow_worker` resolves the selected profile only after `recovered_workflow_identity_matches` succeeds and passes `profile.name` into `recovered_workspaces_to_prepared`; update `recovered_workspaces_to_prepared` to set `PreparedStepWorkspace.workspace_profile`.
30. Confirm that `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/record.gleam`, and `src/scherzo/state/recovery.gleam` do not add a persisted profile field. Only update them if a constructor arity change is mechanically required elsewhere; do not change the encoded `StepAttemptPrepared` payload.
31. Add recovery tests near the existing recovery suite proving recovered prepared workspaces for a workflow selecting `noop` reconstruct with `workspace_profile == "noop"`, profile removal or fingerprint drift prevents reconstruction/reuse, and legacy direct-hook `StepAttemptPrepared` payloads with no profile field remain recoverable as synthetic `default` when the fingerprint matches.
32. Update remaining compile errors caused by explicit profile arguments in test helpers and dependency records. Do not introduce global mutable selected-profile state and do not reintroduce direct `orchestrator.dag_hooks` lifecycle reads.
33. Run `direnv exec . gleam test`. Expect all tests to pass.
34. Commit milestone 4 with a message like `Run workspace hooks through selected profiles`.
35. Update `README.md`, `docs/ARCHITECTURE.md`, `examples/scherzo.yaml`, and `examples/workflows/research.yaml` to explain native profiles, legacy direct hooks, `workspace_profile`, and `SCHERZO_WORKSPACE_PROFILE`.
36. Run `direnv exec . gleam format --check src test` from the repo root. Expect exit code 0 and no formatting diff.
37. Run `direnv exec . gleam test` from the repo root. Expect exit code 0 and all gleeunit tests passing.
38. Commit milestone 5 with a message like `Document workflow workspace hook profiles`.

## Testing and Falsifiability

Add focused config tests in `test/orchestrator_config_test.gleam`:

- `legacy_workspace_hooks_synthesize_default_profile_test`: parse a config with only `workspace.hooks`; assert `orchestrator.workspace_profiles.default_profile == "default"`, `profiles` contains `default`, the profile source is `LegacyWorkspaceHooks`, the hooks match the direct hook strings, and `orchestrator.dag_hooks` still matches those hooks.
- `workspace_profiles_resolve_default_and_named_hooks_test`: parse `workspace.default_profile: isolated` with `isolated` and `noop` profiles; assert both profiles exist, sources are `ConfiguredWorkspaceProfile`, default is `isolated`, and hook bodies/timeouts are profile-specific.
- `workspace_hooks_can_coexist_with_extra_profiles_test`: parse direct legacy hooks plus a `noop` configured profile and no `default_profile`; assert default is synthetic `default` and `noop` is selectable.
- Negative tests for invalid profile names, non-map profile entries, non-map `hooks`, non-positive profile timeout, missing default profile when only configured profiles exist, unknown default profile, and configured `default` collision with direct hooks. Assert the `InvalidConfig` message contains the relevant YAML path such as `workspace.profiles.bad name` or `workspace.default_profile`.

Add workflow parser tests in `test/workflow_dag_test.gleam`:

- A workflow with `workspace_profile: noop` parses and the DAG has `workspace_profile == Some("noop")`.
- A workflow without the field parses and has `workspace_profile == None`.
- `workspace_profile: 123` returns error code `workspace_profile_not_string`.
- `workspace_profile: ../noop` and `workspace_profile: Noop` return `invalid_workspace_profile`.
- A step containing `workspace_profile: noop` returns `step_workspace_profile_not_supported`.

Add runtime bundle tests in `test/runtime_bundle_test.gleam`:

- A config with `workspace.profiles.noop` and a routed workflow selecting `noop` loads successfully.
- A routed workflow selecting `missing` fails `runtime_bundle.load_with_env` with `BundleError("unknown_workspace_profile", message)`, and the message contains the workflow id, `missing`, and the available profile names.
- A workflow omitting `workspace_profile` loads and selects the config default during validation.

Add workspace runtime tests in `test/workspace_run_test.gleam`:

- Extend the existing hook environment test so the create hook appends `$SCHERZO_WORKSPACE_PROFILE` to `hook.log`, and assert it contains the selected profile name.
- Add a test with two profiles whose create hooks write different marker files. Prepare a step with the `noop` profile and assert only the `noop` marker exists. Prepare with the default profile and assert only the default marker exists.
- Add a reuse test that a prepared workspace retains `workspace_profile` and that a mismatched profile reuse returns `prepared workspace profile mismatch`.
- Add a source-workspace reuse test where a step uses `workspace: { name: derived, from: main }` and the source workspace has a different `workspace_profile`; assert preparation fails with `prepared workspace profile mismatch`.
- Add a remove-hook test if one does not already exist: configure a `remove` hook that writes the profile name to a file under the config dir, call cleanup with the `noop` profile, and assert the file contains `noop`.
- Add a cleanup-on-prepare-failure test where the selected profile's create or before-step hook fails after creating a marker, and assert the remove hook from the same selected profile runs.

Add service, doctor, and probe tests:

- In `test/orchestrator_service_doctor_test.gleam`, update fake `DoctorDependencies.prepare_step` and `cleanup_run` functions to receive a profile and assert `profile.name` equals the orchestrator default profile. Cover both `prepare_doctor_workspace` and `append_cleanup_warning_if_needed` through the public doctor report helpers.
- In `test/orchestrator_service_test.gleam`, add or extend a pi probe test so the workspace hook sees `SCHERZO_WORKSPACE_PROFILE` equal to the default profile name, and a negative test where the default profile is missing from `orchestrator.workspace_profiles.profiles` returns `StartupError("workspace_profile_unavailable", ...)`.

Add fingerprint tests in `test/workflow_fingerprint_test.gleam`:

- Two DAGs that differ only by explicit top-level `workspace_profile` selectors produce different DAG-only fingerprints.
- A DAG that omits `workspace_profile` has canonical DAG JSON with no `workspace_profile` key, not a null value.
- Two resolved profiles with identical hook bodies but different configured names produce different execution fingerprints when selected.
- Changing a selected profile's `create` hook changes the execution fingerprint.
- Changing an unselected profile does not change the execution fingerprint for a workflow selecting another profile.
- A legacy synthetic `default` profile produces the same canonical execution input shape as the current direct `dag_hooks` representation; this can assert the canonical JSON does not contain a configured `workspace_profile` object for `LegacyWorkspaceHooks`.

Add recovery-oriented tests where the repository already tests recovery fingerprint drift and recovered workspace summaries:

- Extend the drift test so changing the selected profile name or selected hook body changes the stored execution fingerprint and causes the same drift handling as changing a hook body today.
- Add a focused recovery reconstruction test that simulates or replays a ledger with a completed workspace for a workflow selecting `noop`, recovers it, and asserts the reconstructed `PreparedStepWorkspace.workspace_profile == "noop"` before a resumed dependent step reuses it.
- Add a legacy compatibility test using a direct `workspace.hooks` config and an old-style `StepAttemptPrepared` payload with no profile field; assert decoding succeeds, fingerprint comparison matches, and the reconstructed prepared workspace uses `workspace_profile == "default"`.
- Add a negative recovery test where the workflow still exists but the selected profile has been removed; assert the run is treated as unavailable or drifted and no recovered prepared workspace is reconstructed.

This feature is falsified if any of these are true after implementation:

- A workflow can select an unknown profile and still dispatch.
- A workflow can define raw hook scripts and have Scherzo run them.
- A step-level profile override is silently accepted.
- A changed unselected profile causes an unrelated workflow execution fingerprint to change.
- A legacy direct `workspace.hooks` config changes hook bodies, default profile behavior, or workspace paths for workflows that omit `workspace_profile`.
- Hook scripts do not receive `SCHERZO_WORKSPACE_PROFILE` consistently across create, before_step, after_step, and remove.
- Doctor or probe workspaces use anything other than the orchestrator default profile.
- A recovered prepared workspace lacks the selected profile name after fingerprint validation, or an old direct-hook ledger record becomes undecodable.
- Two workflow files with different explicit top-level `workspace_profile` selectors produce the same DAG-only fingerprint.

## Validation and Acceptance

From the repo root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expected result: `direnv exec . gleam format --check src test` exits 0 with no formatting diff, and `direnv exec . gleam test` exits 0 with all gleeunit tests passing. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Manual acceptance can use temporary test config files rather than a real tracker. A minimal profile config should load through `runtime_bundle.load_with_env` when the workflow selects an existing profile. A workflow selecting `workspace_profile: does-not-exist` should fail with `unknown_workspace_profile` before dispatch. Workspace hook tests should demonstrate that a selected `noop` profile writes `SCHERZO_WORKSPACE_PROFILE=noop` and uses `noop` hook bodies while a workflow omitting `workspace_profile` uses the default profile.

The implementation meets the Linear issue acceptance criteria when:

- Existing direct `workspace.hooks` configs still parse and are the default for existing workflows.
- Profile configs parse and validate named profiles and default profile selection.
- Workflow DAGs accept top-level `workspace_profile` and reject invalid or step-level selectors.
- Runtime hook execution uses the selected profile for create, before_step, after_step, and remove.
- Doctor and probe workspace hooks use the orchestrator default profile and receive `SCHERZO_WORKSPACE_PROFILE`.
- Execution fingerprints include the selected profile and selected hook bodies, while avoiding unselected-profile churn; DAG-only fingerprints include explicit selectors without changing omitted-selector canonical input.
- Recovered prepared workspaces reconstruct the selected profile after fingerprint validation, and old direct-hook ledger records remain compatible.
- Unknown profile names fail with clear load-time errors.
- README, architecture docs, and examples explain that profiles are Scherzo's native abstraction and that repo-local scripts can implement repo-specific behavior outside core.

## Rollout, Recovery, and Idempotence

The rollout is additive. Operators can upgrade without changing config because direct `workspace.hooks` continues to synthesize the `default` profile. Existing workflow files do not need `workspace_profile`; they select the default. Operators can then add a `workspace.profiles.noop` entry and opt one workflow into it by adding top-level `workspace_profile: noop`.

Recovery remains conservative. The recorded workflow execution fingerprint already gates recovery decisions. Because the fingerprint will include the selected profile name and selected hook bodies, changing a selected profile while a run is interrupted should cause the same safe drift behavior as changing hook bodies today. Changing an unselected profile should not disturb recovery for workflows that do not select it. The v1 ledger schema remains unchanged; after a recovered run's fingerprint matches, daemon reconstruction fills `PreparedStepWorkspace.workspace_profile` from the resolved selected profile. Legacy direct-hook records therefore remain readable and recover as `default` only when their existing fingerprint still matches.

Cleanup should never substitute a different profile if the selected one cannot be resolved. If a run was using `noop` and `noop` is removed from config before recovery cleanup, Scherzo should retain or park rather than running `default` cleanup. This is safer than guessing because `remove` hooks may clean external resources. Non-workflow doctor and probe workspaces are the exception only in the sense that they have no workflow selector; they always use the orchestrator default profile, and if that default is unavailable they fail the doctor/probe path rather than guessing.

The change is reversible. Removing profile config and workflow selectors returns deployments to direct `workspace.hooks`. Because v1 does not alter workspace path layout or step workspace semantics, partially prepared workspaces remain under the configured workspace root and existing path safety checks still apply.

## Artifacts and Notes

Current direct hook resolution is implemented in `src/scherzo/config.gleam` as `resolve_dag_hooks`, which reads `workspace.hooks` and returns `config_types.DagHooksConfig`. Workspace hook execution is currently centralized in `src/scherzo/workspace_run.gleam`; the functions to inspect during implementation are `run_create_hook`, `run_before_step_hook`, `after_step`, `cleanup_run`, and `hook_env`.

The existing hook environment test in `test/workspace_run_test.gleam` is a good starting point because it already proves hooks run with the config directory as current working directory and receive workspace/source workspace variables. Extend that test rather than adding a broad integration test.

The existing fingerprint test `workflow_execution_fingerprint_changes_for_hooks_and_artifact_limits_test` in `test/workflow_fingerprint_test.gleam` is a good starting point for selected profile fingerprint coverage.

## Interfaces and Dependencies

No new package dependencies are required. Use existing Gleam stdlib modules, `gleam/dict`, `gleam/list`, `gleam/option`, `gleam/result`, `gleam/string`, and existing `yay` YAML parsing helpers.

At the end of the implementation, `src/scherzo/config/types.gleam` should expose these additional types:

    pub type WorkspaceProfileSource {
      LegacyWorkspaceHooks
      ConfiguredWorkspaceProfile
    }

    pub type WorkspaceHookProfile {
      WorkspaceHookProfile(
        name: String,
        hooks: DagHooksConfig,
        source: WorkspaceProfileSource,
      )
    }

    pub type WorkspaceHookProfiles {
      WorkspaceHookProfiles(
        default_profile: String,
        profiles: Dict(String, WorkspaceHookProfile),
      )
    }

`OrchestratorConfig` should include both:

    dag_hooks: DagHooksConfig,
    workspace_profiles: WorkspaceHookProfiles,

`dag_hooks` is the default profile's hooks for compatibility. New runtime code should prefer `workspace_profiles` plus a resolved `WorkspaceHookProfile`.

At the end of the implementation, `src/scherzo/workflow_dag.gleam` should define:

    pub type WorkflowDag {
      WorkflowDag(
        id: String,
        description: Option(String),
        workspace_profile: Option(String),
        max_parallel_steps: Int,
        steps: List(WorkflowStep),
      )
    }

If field order causes excessive call-site churn, keep the existing field order and append `workspace_profile` at the end, but update every construction site explicitly rather than relying on positional guesses.

At the end of the implementation, `src/scherzo/workspace_profile.gleam` should expose:

    pub fn selected_name(
      dag: workflow_dag.WorkflowDag,
      orchestrator: config_types.OrchestratorConfig,
    ) -> String

    pub fn resolve(
      dag: workflow_dag.WorkflowDag,
      orchestrator: config_types.OrchestratorConfig,
    ) -> Result(config_types.WorkspaceHookProfile, ProfileResolutionError)

At the end of the implementation, `src/scherzo/workspace_run.gleam` public lifecycle functions should all accept a resolved `WorkspaceHookProfile`:

    pub fn prepare_step(..., orchestrator: config_types.OrchestratorConfig, profile: config_types.WorkspaceHookProfile, known_workspaces: Dict(String, PreparedStepWorkspace)) -> Result(PreparedStepWorkspace, PrepareError)
    pub fn prepare_step_attempt(..., orchestrator: config_types.OrchestratorConfig, profile: config_types.WorkspaceHookProfile, known_workspaces: Dict(String, PreparedStepWorkspace)) -> Result(PreparedStepWorkspace, PrepareError)
    pub fn prepare_recovered_step(..., orchestrator: config_types.OrchestratorConfig, profile: config_types.WorkspaceHookProfile, known_workspaces: Dict(String, PreparedStepWorkspace)) -> Result(PreparedStepWorkspace, PrepareError)
    pub fn prepare_recovered_step_attempt(..., orchestrator: config_types.OrchestratorConfig, profile: config_types.WorkspaceHookProfile, known_workspaces: Dict(String, PreparedStepWorkspace)) -> Result(PreparedStepWorkspace, PrepareError)
    pub fn after_step(issue: tracker_issue.Issue, step_id: String, prepared: PreparedStepWorkspace, orchestrator: config_types.OrchestratorConfig, profile: config_types.WorkspaceHookProfile) -> Nil
    pub fn cleanup_run(run_root: String, orchestrator: config_types.OrchestratorConfig, profile: config_types.WorkspaceHookProfile) -> Result(Nil, error.WorkspaceError)

The exact preceding arguments in the `...` portions remain the existing arguments; only the resolved `profile` argument is inserted after `orchestrator` and before any prepared-workspace dictionary. `workflow_run.Dependencies` and `orchestrator/service.gleam` doctor dependencies should mirror these profile arguments instead of wrapping them in hidden global state.

At the end of the implementation, every workspace hook invocation should use a resolved `WorkspaceHookProfile`. No core module should branch on `git`, `jj`, repository root names, or a `repo_root` mode. Repository-specific behavior belongs in configured hook scripts selected through profiles.

## Open Questions and Clarifications Needed

None.
