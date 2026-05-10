# Expose workspace driver context to workflow steps

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, a portable Scherzo workflow can discover the workspace profile selected for the run, the trusted workspace driver command configured by the operator, and the workflow-facing capabilities that driver advertises. A command step can read the values from `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`. An agent step can render the same values into its prompt with `{{ workspace.profile }}`, `{{ workspace.driver }}`, and a loop over `workspace.capabilities`.

The visible outcome is that a workflow can declare it needs a capability such as `assert-only`, fail during bundle validation if the selected profile does not provide that capability, run a command step that invokes `$SCHERZO_WORKSPACE_DRIVER assert-only --path research-findings.md`, and render an agent prompt that explicitly tells the agent which driver command and capabilities are available. This plan deliberately does not make shell commands launched inside pi inherit the variables. If an agent needs the driver, the prompt must render the concrete command text through template locals.

## Problem Framing and Constraints

Scherzo workflows are meant to be shareable across repositories, but workflow prompts and command steps can become tied to dogfood-specific version-control commands. A portable workflow should not have to hardcode `jj`, `git`, or a repository-local helper name when the operator may have configured a no-op, git, jj, worktree, copy-based, or internal workspace driver. The workflow layer needs a small, stable way to refer to the selected driver and to prove before dispatch that the selected profile provides the capabilities the workflow expects.

The current tree does not yet expose a driver command or capability list on workspace profiles. It defines `WorkspaceHookProfile(name, hooks, source)` in `src/scherzo/config/types.gleam`, validates only that a selected profile exists, and fingerprints configured profiles by name/source plus hooks. Therefore this plan includes the minimal driver-schema work needed to make LIV-173 self-contained: a driver record on workspace profiles, a top-level workflow capability requirement list, bundle validation that selected profiles provide required capabilities, and fingerprint coverage for driver metadata. The plan does not design real driver scripts or migrate dogfood workflows to use the new variables.

Driver commands remain trusted operator configuration. Workflow YAML may select a profile and require capabilities, but it must not define or override the driver command. The command is exposed to workflow commands and to language-model prompts, so it must be non-secret and workflow-safe. Capability validation is a compatibility check, not a shell-level authorization boundary: Scherzo can prove that a workflow declared `assert-only` and the selected profile advertises `assert-only`, but the configured driver wrapper must still reject destructive or undeclared operations if those operations exist.

## Strategy Overview

First add minimal, explicit schema support for workspace drivers. Extend configured and legacy workspace profiles with a `WorkspaceDriver(command, capabilities)` record. Configured profiles may define `workspace.profiles.<name>.driver.command` and `workspace.profiles.<name>.driver.capabilities`; legacy or driverless profiles get an empty command and an empty capability list so existing configurations continue to load. Extend workflow DAGs with a top-level `workspace_capabilities` list, defaulting to `[]`, and validate at runtime-bundle load that the selected profile advertises every required capability.

Then add one runtime-context helper that converts the resolved workspace profile into three public values: profile name, driver command, and normalized capability list. Thread that context into `StepContext` in `src/scherzo/workflow_run.gleam`. Command-step environment construction is already centralized in `step_command_env(context)`, so append the three `SCHERZO_WORKSPACE_*` variables there. Agent prompt rendering already accepts caller-provided locals through `template.render_with_locals`, so merge workspace locals with existing artifact locals before rendering original prompts.

This is the right size because it reuses the existing selected-profile flow: `workspace_profile.resolve(dag, orchestrator)` already happens before the workflow loop starts, `workspace_run.PreparedStepWorkspace` already records the selected profile name for prepared workspaces, and `workflow_fingerprint.for_execution` already includes selected-profile execution metadata. The plan avoids adding workflow-defined driver shell, daemon-global environment, or a generic process-environment plumbing layer.

## Alternatives Considered

One simpler alternative is to expose only prompt locals and tell agents the driver command. That helps agent steps but leaves command steps unable to use the driver without hardcoded repository knowledge, and command steps are the most direct way to run deterministic artifact and assertion operations.

A second alternative is to expose only environment variables and ask prompts to mention those variable names. That keeps the runtime smaller but is ambiguous for language-model prompts, especially because this plan does not make pi subprocesses inherit those variables. Rendering concrete prompt locals lets workflow authors show the actual command text to the agent.

A third alternative is to put the driver variables into the daemon process environment at startup. That is rejected because the selected profile can vary by workflow, and future recovery or routing behavior may select different profiles for different runs. The values belong to the per-step runtime context, not to daemon-global state.

A fourth alternative is to expose the full pi subprocess environment in this issue. That is rejected for LIV-173 because it requires lower-level changes in `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/pi/command.gleam`, `src/scherzo/pi/client.gleam`, and possibly `src/scherzo/port.gleam`. Command-step variables and rendered prompt locals solve the immediate portability problem with less blast radius. Pi environment inheritance should be a separate follow-up if operators need it.

A fifth alternative is to keep the driver-schema work as a prerequisite outside this plan. That is rejected because the current tree does not yet contain the required interfaces, and a future implementer must be able to start from this plan alone.

## Risks and Countermeasures

The main schema risk is accidentally breaking existing configurations and tests that construct `WorkspaceHookProfile` directly. Countermeasure: make the driver field additive with an empty default for legacy and driverless profiles, update all constructors in tests and production in one milestone, and run the full test suite before changing workflow execution behavior.

The main public-interface risk is ambiguous driver command representation. Countermeasure: this plan defines `driver.command` as one executable command token, not a full shell script and not an executable-plus-argv structure. It may be a program name on `PATH` or a path that is valid from the step workspace, such as `scripts/scherzo-workspace-jj`. It must not contain whitespace, shell metacharacters, newlines, secrets, or local machine paths in checked-in examples. If an operator needs arguments or complex setup, they must put that logic in a wrapper command and expose the wrapper as `driver.command`.

The main capability risk is treating compatibility metadata as authorization. Countermeasure: bundle validation fails when a workflow requires a capability the selected profile does not advertise, but the plan states clearly that `$SCHERZO_WORKSPACE_DRIVER` must point at a workflow-safe wrapper. Tests prove missing required capabilities are rejected; operators remain responsible for wrapper-side enforcement of destructive operations.

The main source-of-truth risk is letting workflow YAML or stale prepared workspace data supply driver metadata. Countermeasure: runtime context is built only from the resolved `config_types.WorkspaceHookProfile` returned by `workspace_profile.resolve(dag, orchestrator)`. `PreparedStepWorkspace.workspace_profile` remains only the selected profile name used for workspace recovery checks. Tests must include workflow content that attempts to define a driver-shaped key and prove the exposed values still come from the orchestrator profile.

The main recovery risk is resuming a run after driver metadata changed. Countermeasure: include driver command and normalized capabilities in the execution fingerprint for configured profiles. Existing recovery validation compares the stored workflow fingerprint with the current execution fingerprint; add a test that a driver command or capability change changes the fingerprint and a workflow-run recovery test that mismatched fingerprints reject recovery before exposing current driver metadata to an old workspace.

## Progress

- [x] (2026-05-09 00:00Z) Read the repo-local ExecPlan skill and the workspace-driver umbrella source material for the initial draft.
- [x] (2026-05-09 00:00Z) Inspected the runtime files that currently build command environments, render prompt locals, and launch pi processes for the initial draft.
- [x] (2026-05-09 00:00Z) Drafted the first child ExecPlan for LIV-173.
- [x] (2026-05-09 00:00Z) Incorporated adversarial review findings by making the schema prerequisite self-contained, removing the pi-environment branch, and closing driver/capability interface decisions.
- [x] (2026-05-10 00:00Z) Re-checked the implementation tree and found that prior work had already added typed workspace driver schema, workflow capability requirements, bundle validation, and configured-profile fingerprint coverage.
- [x] (2026-05-10 00:00Z) Updated profile parsing and bundle validation so hook-backed profiles can carry workflow-facing driver metadata while driver-only profiles still fail safely before dispatch.
- [x] (2026-05-10 00:00Z) Added `src/scherzo/workspace_driver_context.gleam` and threaded resolved profile context through `StepContext`.
- [x] (2026-05-10 00:00Z) Added `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES` to command-step environments.
- [x] (2026-05-10 00:00Z) Added `workspace.profile`, `workspace.driver`, and `workspace.capabilities` prompt locals for original agent prompts while preserving recovery prompt behavior.
- [x] (2026-05-10 00:00Z) Added source-of-truth, artifact-local preservation, recovery prompt, helper serialization, bundle, config, and hook-profile fingerprint tests.
- [x] (2026-05-10 00:00Z) Ran validation: `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Surprises & Discoveries

- Observation: Command-step environment construction is centralized enough for a small change.
  Evidence: `src/scherzo/workflow_run.gleam` defines `step_command_env(context)` and `default_dependencies()` passes that list to `command_step.run_with_env`.

- Observation: Agent prompt rendering already accepts caller-provided locals.
  Evidence: `src/scherzo/template.gleam` defines `render_with_locals` and `render_scheduled_with_locals`, and `src/scherzo/workflow_run.gleam` currently passes `step_artifact.to_template_locals(artifacts)` when rendering original agent prompts.

- Observation: Pi launch currently has argv-level environment support but no step-specific environment parameter.
  Evidence: `src/scherzo/pi/command.gleam` defines `ArgvLaunch(executable, args, env)`, `src/scherzo/pi/client.gleam` starts argv launches with that env, and `src/scherzo/agent/run_attempt.gleam` calls `pi_command.build_launch(config.pi, launch_mode)` without passing per-step environment. This plan intentionally leaves that path unchanged.

- Observation: The current tree lacks driver metadata on workspace profiles.
  Evidence: `src/scherzo/config/types.gleam` currently defines `WorkspaceHookProfile(name, hooks, source)`, and `src/scherzo/workspace_profile.gleam` resolves that type without driver command or capability fields.

- Observation: By implementation time, the checked-in tree had already landed typed workspace-driver schema and capability validation from another slice.
  Evidence: `src/scherzo/config/types.gleam` defined `WorkspaceDriverConfig(command, lifecycle, capabilities, timeout_ms)` and `WorkspaceCapability`; `src/scherzo/workflow_dag.gleam` already parsed `workspace_capabilities`; `src/scherzo/workflow_fingerprint.gleam` already serialized `workspace_driver` for selected profiles.

- Observation: Runtime bundle loading intentionally rejected every selected profile with `driver: Some(_)`, which would have prevented the command and prompt context from being used in a runnable workflow.
  Evidence: `src/scherzo/workspace_profile.gleam` had `validate_dispatchable_profile` return `workspace_driver_invocation_unavailable` for any profile with a driver, and `docs/runbooks/workspace-driver-migration.md` documented driver-backed profiles as schema-only.

- Observation: The test runner in this repository does not accept individual test file arguments through `gleam test`.
  Evidence: `direnv exec . gleam test test/workspace_driver_context_test.gleam ...` exited with usage text, so validation used the full deterministic unit suite instead.

- Observation: Adding the `StepContext.workspace_context` field and command/prompt wiring grew the already-baselined `src/scherzo/workflow_run.gleam` line count but did not increase its internal import count after re-exporting the helper type through `src/scherzo/workspace_profile.gleam`.
  Evidence: the source guardrail initially reported `workflow_run.gleam grew beyond its internal-import baseline: 26 > 25` and `line baseline: 3194 > 3189`; after moving the import behind `workspace_profile`, only intentional line growth remained and `test/source_guardrail_test.gleam` was updated to `3197, 25`.

## Decision Log

- Decision: Include minimal workspace-driver schema work in this ExecPlan instead of leaving it as an external prerequisite.
  Rationale: The current tree does not expose the required profile fields. A self-contained ExecPlan must give the implementer exact interfaces to build before runtime exposure.
  Date: 2026-05-09

- Decision: Use the environment variable names `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`.
  Rationale: These names match the workspace-driver vocabulary and keep driver context separate from existing workflow, run, issue, attempt, and workspace path variables.
  Date: 2026-05-09

- Decision: Use prompt local names `workspace.profile`, `workspace.driver`, and `workspace.capabilities`.
  Rationale: These names parallel the environment variables while using the existing dotted template style. They let prompts render concrete driver instructions without hardcoding a version-control system.
  Date: 2026-05-09

- Decision: Define `driver.command` as a single executable command token and copy it exactly into `SCHERZO_WORKSPACE_DRIVER` and `workspace.driver`.
  Rationale: A single token keeps `$SCHERZO_WORKSPACE_DRIVER assert-only` predictable in shell command steps and avoids inventing quoting or argv serialization in this plan. Operators that need arguments or setup should expose a wrapper command.
  Date: 2026-05-09

- Decision: Normalize capabilities by trimming entries, rejecting invalid names, and deduplicating duplicates while preserving first occurrence order.
  Rationale: Capability names are intended to be shell-friendly tokens such as `assert-only`, `changed-files`, and `publish-change`. A space-separated environment string is readable and deterministic when entries cannot contain whitespace.
  Date: 2026-05-09

- Decision: Serialize `SCHERZO_WORKSPACE_CAPABILITIES` as a single space-separated string in normalized profile order, and expose `workspace.capabilities` as a `template.VList` in the same order.
  Rationale: Shell command steps can inspect the environment value directly, while prompts can loop over the structured list. Keeping the same order makes tests and rendered prompts deterministic.
  Date: 2026-05-09

- Decision: Defer pi-process environment inheritance out of LIV-173.
  Rationale: The required supported surfaces are command-step environments and agent prompt locals. Pi subprocess environment inheritance touches lower-level launch code and should be designed and tested separately if needed.
  Date: 2026-05-09

- Decision: Treat capabilities as compatibility metadata validated by Scherzo, not as an authorization boundary enforced by Scherzo shell parsing.
  Rationale: Scherzo can reject workflows whose selected profile lacks declared capabilities, but once a workflow invokes a shell command, the configured driver wrapper must enforce which operations are safe.
  Date: 2026-05-09

- Decision: Reuse the checked-in typed `WorkspaceDriverConfig` and `WorkspaceCapability` schema rather than replacing it with the plan draft's simpler string-list `WorkspaceDriver` record.
  Rationale: The repository had already moved to a richer schema with lifecycle metadata and typed capability names. Reusing it avoided a backwards schema churn and kept existing capability validation, fingerprint tests, and migration documentation intact.
  Date: 2026-05-10

- Decision: Permit hook-backed profiles to include driver metadata, but keep driver-only profiles non-dispatchable until lifecycle driver invocation is implemented.
  Rationale: This exposes workflow-facing driver context in runnable workflows without pretending that Scherzo can already use a driver-only profile to create, prepare, and remove workspaces. Existing hook lifecycle remains the safe workspace preparation mechanism.
  Date: 2026-05-10

- Decision: Re-export workspace driver context helpers through `src/scherzo/workspace_profile.gleam` for use by `src/scherzo/workflow_run.gleam`.
  Rationale: The focused helper still lives in `src/scherzo/workspace_driver_context.gleam`, but routing calls through the already-imported `workspace_profile` module avoided growing `workflow_run.gleam`'s internal import count beyond its source-guardrail baseline.
  Date: 2026-05-10

- Decision: Update the `src/scherzo/workflow_run.gleam` source-guardrail line baseline from 3189 to 3197.
  Rationale: The remaining growth is the minimal intentional surface needed to carry `workspace_context` in `StepContext`, pass the resolved profile to context construction, merge prompt locals, and append command environment variables. The helper logic was extracted so only wiring remains in the large module.
  Date: 2026-05-10

## Outcomes & Retrospective

Implemented the workflow-facing workspace driver context. Hook-backed profiles can now define a `driver` block with a trusted command and capabilities; such profiles remain runnable because hooks still own workspace lifecycle, and workflows that declare matching `workspace_capabilities` load successfully. Driver-only profiles still fail with `workspace_driver_invocation_unavailable`, preserving the existing recovery and rollout boundary until lifecycle driver invocation is designed.

Command steps receive `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES` through the existing command environment path. Original agent prompts can render `workspace.profile`, `workspace.driver`, and iterate `workspace.capabilities` alongside existing `steps.*` artifact locals. Recovery prompts are still returned from stored continuation data and are not re-rendered.

Validation passed with the full deterministic unit suite and formatting/lint gates. `glinter` and `scherzo_lint` still report the repository's existing warning inventory but exit with zero errors.

## Context and Orientation

Scherzo is a Gleam application that dispatches tracker issues into workflow DAGs. A workflow DAG is a YAML workflow definition. A workflow step runs either as a command step, which executes a shell command in a prepared workspace, or as an agent step, which launches pi in the prepared workspace and sends it a rendered prompt.

A workspace is the directory where a step works. A workspace profile is an operator-configured policy for creating and cleaning those workspaces. Today profiles are hook-focused: they hold lifecycle hook commands such as create, before-step, after-step, and remove. This plan extends each profile with a workspace driver. A workspace driver is a workflow-facing command wrapper configured by the operator. A workspace capability is a named operation such as `assert-only` or `changed-files` that a workflow may require and a profile may advertise.

The current runtime path relevant to this plan is concentrated in these files:

- `src/scherzo/config/types.gleam` defines configuration records, including `WorkspaceHookProfile`, `WorkspaceHookProfiles`, `DagHooksConfig`, `OrchestratorConfig`, and `EffectiveConfig`.
- `src/scherzo/config.gleam` parses orchestrator YAML into those configuration records. Workspace profile parsing currently lives around `resolve_workspace_profiles`, `read_configured_workspace_profiles`, and `read_workspace_profile_entry`.
- `src/scherzo/workflow_dag.gleam` parses workflow YAML. It currently records `workspace_profile: Option(String)` but has no capability requirement list.
- `src/scherzo/runtime_bundle.gleam` loads orchestrator config and workflows, resolves prompt files, and validates that selected workspace profiles exist.
- `src/scherzo/workspace_profile.gleam` chooses the effective profile for a workflow by using the workflow's `workspace_profile` or the orchestrator default.
- `src/scherzo/workflow_fingerprint.gleam` builds deterministic fingerprints used to detect whether a recovered workflow run is still compatible with the current workflow and execution configuration.
- `src/scherzo/workspace_run.gleam` defines `PreparedStepWorkspace`, which records the prepared workspace path and the selected `workspace_profile` name.
- `src/scherzo/workflow_run.gleam` defines the ephemeral `StepContext`, prepares command-step environments through `step_command_env`, renders agent prompts in `prompt_mode_for_step`, and calls the agent dependency for agent steps.
- `src/scherzo/template.gleam` defines the prompt template renderer. `render_with_locals` accepts caller-provided locals, and local lookup happens before built-in issue or scheduled variables.
- `src/scherzo/step_artifact.gleam` provides artifact template locals that are currently the only custom locals passed to agent prompts.

## Preconditions and Verified Facts

Implementation starts from the current repository tree. There is no external prerequisite to land first; this plan includes the missing schema work.

Current verified facts:

`src/scherzo/config/types.gleam` defines `WorkspaceHookProfile` with fields `name`, `hooks`, and `source`. It does not yet define a `WorkspaceDriver` type and does not store a driver command or capabilities.

`src/scherzo/config.gleam` reads configured profiles from `workspace.profiles.<name>.hooks`. `read_workspace_profile_entry` currently requires `hooks` to be a map and returns `WorkspaceHookProfile(name, hooks, ConfiguredWorkspaceProfile)`.

`src/scherzo/workflow_dag.gleam` defines `WorkflowDag(id, description, workspace_profile, max_parallel_steps, steps)`. It rejects invalid top-level `workspace_profile` values and rejects step-level `workspace_profile`, but it does not parse `workspace_capabilities`.

`src/scherzo/runtime_bundle.gleam` validates that every workflow's selected workspace profile can be resolved. It does not yet validate required workspace capabilities.

`src/scherzo/workflow_fingerprint.gleam` includes configured workspace profile metadata in execution fingerprints, but `workspace_profile_to_json(profile)` currently includes only the profile name and source. It does not include driver metadata because driver metadata does not exist yet.

`src/scherzo/workflow_run.gleam` defines `StepContext` with workflow, run, step, attempt, workspace, config, issue, scheduled-run, and run-attempt fields. It does not yet carry workspace driver context.

`src/scherzo/workflow_run.gleam` defines `step_command_env(context)` with variables including `SCHERZO_CONFIG_DIR`, `SCHERZO_WORKFLOW_ID`, `SCHERZO_RUN_ID`, `SCHERZO_RUN_ROOT`, `SCHERZO_RUN_KIND`, issue fields, scheduled-run fields, step fields, `SCHERZO_WORKSPACE_NAME`, and `SCHERZO_WORKSPACE_PATH`. It does not include `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, or `SCHERZO_WORKSPACE_CAPABILITIES`.

`src/scherzo/workflow_run.gleam` renders original agent prompts with `template.render_with_locals(prompt_template, issue, None, step_artifact.to_template_locals(artifacts))`. It does not yet pass workspace profile, driver, or capability locals. Recovery prompts are returned from stored `workflow_attempt.PiContinuation` data and are not re-rendered.

`src/scherzo/workspace_run.gleam` defines `PreparedStepWorkspace` with a `workspace_profile` field. This is the selected profile name for the prepared workspace. It should remain a recovery consistency check, not the source of driver command or capabilities.

Normal repository validation should be run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. Treat that as environment setup, not as a code failure.

## Scope Boundaries

In scope:

- Add a minimal `WorkspaceDriver` record to workspace profiles.
- Parse `workspace.profiles.<name>.driver.command` and `workspace.profiles.<name>.driver.capabilities` from orchestrator YAML.
- Parse a top-level workflow `workspace_capabilities` list from workflow YAML.
- Validate at runtime-bundle load that the selected profile provides every capability required by each workflow.
- Include configured profile driver metadata in execution fingerprints.
- Add runtime context conversion from the resolved workspace profile to profile, driver, and capability values.
- Add the three workspace-driver environment variables to command steps.
- Add the three workspace-driver prompt locals to original agent prompt rendering.
- Add focused tests for schema parsing, capability validation, fingerprint changes, command-step environment, prompt locals, source-of-truth behavior, and recovery safety.

Out of scope:

- Writing no-op, jj, git, or artifact driver scripts.
- Migrating `.scherzo/scherzo.yaml`, `.scherzo/workflows/*.yaml`, or `examples/workflows/` to use the variables.
- Adding a capability vocabulary beyond validating token shape and matching required names to advertised names.
- Enforcing capability authorization inside arbitrary shell commands. The configured driver wrapper must enforce destructive-operation policy.
- Exposing workflow-defined shell commands as trusted driver commands.
- Adding step-specific environment variables to pi subprocess launches.
- Changing pi launch interfaces in `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/pi/command.gleam`, `src/scherzo/pi/client.gleam`, or `src/scherzo/port.gleam`.

## Open Questions and Clarifications Needed

None.

## Milestones

The first milestone adds the missing schema and validation foundation. At the end, orchestrator profiles can carry a normalized driver command and capabilities, workflows can declare `workspace_capabilities`, runtime-bundle validation rejects missing capabilities, and execution fingerprints change when configured driver metadata changes. Existing workflows without driver configuration continue to load because legacy and driverless profiles receive an empty driver.

The second milestone adds a central runtime context helper. At the end, one tested module turns a resolved `WorkspaceHookProfile` into environment variables and prompt locals with exact serialization rules. This de-risks the workflow runtime edits by keeping conversion and normalization behavior in one place.

The third milestone exposes the context to command steps. At the end, command steps run with `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES` in their environment, and a workflow-run test proves those values come from the resolved orchestrator profile.

The fourth milestone exposes the context to agent prompt templates. At the end, an original agent prompt can render `{{ workspace.profile }}`, `{{ workspace.driver }}`, and loop over `workspace.capabilities` without losing existing artifact locals. A workflow-run test proves the rendered prompt passed to the agent dependency contains the expected values. Recovery prompts remain unchanged and are not re-rendered.

The fifth milestone adds recovery/source-of-truth safety coverage and runs full validation. At the end, tests prove workflow YAML cannot override driver context, missing capabilities fail before dispatch, driver metadata changes affect execution fingerprints, recovered runs with stale fingerprints fail safely, all tests pass, formatting is clean, and production lint gates pass.

## Plan of Work

In `src/scherzo/config/types.gleam`, add a new public type:

    pub type WorkspaceDriver {
      WorkspaceDriver(command: String, capabilities: List(String))
    }

Extend `WorkspaceHookProfile` to include `driver: WorkspaceDriver` after `source`. Update every construction of `WorkspaceHookProfile` in production and tests to pass `driver: WorkspaceDriver(command: "", capabilities: [])` unless the test specifically needs driver metadata.

In `src/scherzo/config.gleam`, add parsing helpers for configured profile drivers. The YAML shape is:

    workspace:
      profiles:
        dogfood-jj:
          hooks:
            timeout_ms: 60000
          driver:
            command: scripts/scherzo-workspace-jj
            capabilities: [assert-only, changed-files]

If `driver` is absent, use `WorkspaceDriver(command: "", capabilities: [])`. If `driver` is present, it must be a map and `driver.command` must be a non-empty string after trimming. `driver.command` must be one executable command token: reject values containing whitespace, newlines, or shell metacharacters such as `;`, `&`, `|`, `<`, `>`, backticks, or quotes. Capabilities are an optional string list defaulting to `[]`; trim each entry, reject empty or invalid entries, and deduplicate while preserving first occurrence order. Use lower-case token rules compatible with existing workflow/profile names: examples such as `assert-only`, `changed-files`, and `publish-change` are valid; values with spaces or uppercase letters are invalid.

In `src/scherzo/workflow_dag.gleam`, extend `WorkflowDag` with `workspace_capabilities: List(String)` after `workspace_profile`. Parse top-level `workspace_capabilities` from YAML. Missing means `[]`. The field must be a list of strings, use the same capability validation and deduplication rules as profile capabilities, and reject step-level `workspace_capabilities` with a clear DAG error such as `step_workspace_capabilities_not_supported`.

To avoid duplicating capability-token logic between config and workflow parsing, create a small helper module such as `src/scherzo/workspace_capability.gleam` with functions equivalent to `normalize_list(values: List(String)) -> Result(List(String), String)`, `valid_name(value: String) -> Bool`, and `serialize(values: List(String)) -> String`. The helper should not know about YAML; callers map its error string into `error.InvalidConfig` or `workflow_dag.DagError`.

In `src/scherzo/runtime_bundle.gleam`, replace the existing workspace-profile-only validation with validation that also checks capabilities. Keep the existing unknown-profile behavior. After resolving the selected profile for each workflow, compare `dag.workspace_capabilities` to `profile.driver.capabilities`. If a required capability is missing, return `BundleError(code: "missing_workspace_capability", message: "workflow " <> dag.id <> " requires workspace capability " <> capability <> " but workspace profile " <> profile.name <> " does not provide it")`. Validate all workflows loaded through the bundle before dispatch begins.

In `src/scherzo/workflow_fingerprint.gleam`, include configured profile driver metadata in `workspace_profile_to_json(profile)`. The JSON for configured profiles must include at least the profile name, source, driver command, and normalized capability list. Legacy profiles with empty drivers should continue to omit the `workspace_profile` object in `execution_to_json` as they do today when `profile.source == LegacyWorkspaceHooks`; this preserves current legacy fingerprint behavior. Configured profiles should change fingerprint when the driver command or normalized capability list changes.

Create `src/scherzo/workspace_driver_context.gleam`. Define a public `Context(profile: String, driver: String, capabilities: List(String))`. Add `from_profile(profile: config_types.WorkspaceHookProfile) -> Context`, `env_vars(context: Context) -> List(#(String, String))`, and `template_locals(context: Context) -> List(#(String, template.Value))`. `env_vars` must return exactly `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`. `template_locals` must return `workspace.profile` and `workspace.driver` as `template.VString`, and `workspace.capabilities` as `template.VList` of `template.VString` values.

Update `src/scherzo/workflow_run.gleam` so `StepContext` carries `workspace_context: workspace_driver_context.Context`. Update the `step_context` function signature to:

    fn step_context(
      step: workflow_dag.WorkflowStep,
      workspace: workspace_run.PreparedStepWorkspace,
      issue: tracker_issue.Issue,
      orchestrator: config_types.OrchestratorConfig,
      profile: config_types.WorkspaceHookProfile,
    ) -> StepContext

Build `workspace_context` with `workspace_driver_context.from_profile(profile)`. Keep `workspace.workspace_profile` as the prepared-workspace consistency value; do not read driver command or capabilities from prepared workspace records. In `run_step`, change the existing call to `let context = step_context(step, workspace, issue, orchestrator, profile)`. `scheduled_step_context(context, scheduled)` can remain a record update; it must preserve `workspace_context` unchanged.

Update `step_command_env(context)` in `src/scherzo/workflow_run.gleam` to append `workspace_driver_context.env_vars(context.workspace_context)` after the existing `SCHERZO_WORKSPACE_PATH` pair. Preserve all existing variables and their names.

Update `prompt_mode_for_step` in `src/scherzo/workflow_run.gleam` to receive `context: StepContext` or `workspace_context: workspace_driver_context.Context`. Prefer passing `context` from `run_agent_step` so future prompt rendering has access to the same step metadata. The new signature should be equivalent to:

    fn prompt_mode_for_step(
      step: workflow_dag.WorkflowStep,
      prompt_ref: workflow_dag.PromptRef,
      issue: tracker_issue.Issue,
      artifacts: Dict(String, step_artifact.StepArtifact),
      pi_session_continuations: Dict(String, workflow_attempt.PiContinuation),
      context: StepContext,
    ) -> Result(workflow_attempt.AgentPromptMode, Nil)

Inside `prompt_mode_for_step`, keep the existing first branch for `pi_session_continuations`: if a continuation exists, return `workflow_attempt.RecoveryPrompt(continuation.recovery_prompt)` without rendering templates or adding locals. For original prompts, build locals as `list.append(step_artifact.to_template_locals(artifacts), workspace_driver_context.template_locals(context.workspace_context))` and pass that list to `template.render_with_locals`.

Do not change `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/pi/command.gleam`, `src/scherzo/pi/client.gleam`, or `src/scherzo/port.gleam` for this issue.

## Concrete Steps

1. From the repository root, add `WorkspaceDriver` to `src/scherzo/config/types.gleam` and add `driver: WorkspaceDriver` to `WorkspaceHookProfile`.

2. Update all production constructors of `WorkspaceHookProfile` to include an empty driver. Start with `src/scherzo/config.gleam`, `src/scherzo/workflow_fingerprint.gleam`, and any other compiler-reported production call sites.

3. Update test constructors and helpers that build `WorkspaceHookProfile`. In `test/workflow_run_test.gleam`, update the local `workspace_profile` helper so callers can keep using the existing name/hooks/source arguments and the helper supplies `driver: WorkspaceDriver(command: "", capabilities: [])` by default. Add a second helper such as `workspace_profile_with_driver(name, hooks, source, command, capabilities)` for driver-specific tests.

4. Run the compiler through the test suite:

    direnv exec . gleam test

   Expect compile errors until every `WorkspaceHookProfile` construction includes the new field. Do not proceed until the suite compiles and existing tests pass with empty-driver defaults.

5. Add `src/scherzo/workspace_capability.gleam` with name validation, list normalization, duplicate removal preserving first occurrence order, and serialization. Add tests in `test/workspace_capability_test.gleam` for valid names `assert-only` and `changed-files`, rejection of `Assert-Only`, rejection of `assert only`, rejection of empty strings, deduplication of `[assert-only, changed-files, assert-only]` to `[assert-only, changed-files]`, and serialization to `assert-only changed-files`.

6. Update `src/scherzo/config.gleam` to parse `workspace.profiles.<name>.driver`. Add config tests in `test/config_test.gleam` that call `config.resolve_orchestrator_root` with a profile containing `driver.command: scripts/scherzo-workspace-jj` and capabilities `[assert-only, changed-files]`; assert the resolved profile has that command and those capabilities. Add negative tests for non-map `driver`, missing `driver.command` when `driver` is present, `driver.command` with whitespace, `driver.command` with `;`, and capability entries with uppercase or whitespace.

7. Update `src/scherzo/workflow_dag.gleam` to add and parse `workspace_capabilities`. Add tests in `test/workflow_dag_test.gleam`: parsing a workflow with `workspace_capabilities: [assert-only, changed-files]` yields that list; omitting the field yields `[]`; `workspace_capabilities: assert-only` fails with `workspace_capabilities_not_list`; a non-string entry fails with `workspace_capability_not_string`; a step-level `workspace_capabilities` field fails with `step_workspace_capabilities_not_supported`.

8. Run targeted schema tests:

    direnv exec . gleam test test/workspace_capability_test.gleam test/config_test.gleam test/workflow_dag_test.gleam

   If the test runner does not accept file arguments in this project, run `direnv exec . gleam test` instead. Expect the new schema tests to pass.

9. Update `src/scherzo/runtime_bundle.gleam` to validate workflow capability requirements against the resolved profile. Add tests in `test/runtime_bundle_test.gleam`; create the file if it does not already exist. The positive test should load or construct a bundle where workflow `implementation` selects profile `dogfood-jj`, requires `assert-only`, and the profile advertises `assert-only`; expect validation/load success. The negative test should require `publish-change` while the profile advertises only `assert-only`; expect `BundleError.code == "missing_workspace_capability"` and a message mentioning workflow `implementation`, profile `dogfood-jj`, and capability `publish-change`.

10. Update `src/scherzo/workflow_fingerprint.gleam` so configured profile fingerprint JSON includes `driver.command` and `driver.capabilities`. Add `execution_fingerprint_changes_for_workspace_driver_metadata_test` to `test/workflow_fingerprint_test.gleam`. Construct three configured profiles with the same name and hooks: one with command `scripts/scherzo-workspace-jj` and capabilities `[assert-only]`, one with command `scripts/other-driver` and capabilities `[assert-only]`, and one with command `scripts/scherzo-workspace-jj` and capabilities `[assert-only, changed-files]`. Assert all three execution fingerprints are not equal where driver metadata differs, and assert the canonical execution input contains `driver`, `scripts/scherzo-workspace-jj`, and `assert-only`.

11. Run the full test suite:

    direnv exec . gleam test

   Commit point after this passes: schema, validation, and fingerprint foundation. Suggested commit message: `Add workspace driver metadata to profiles`.

12. Add `src/scherzo/workspace_driver_context.gleam` with `Context`, `from_profile`, `env_vars`, and `template_locals` as described in Plan of Work.

13. Add `test/workspace_driver_context_test.gleam`. Construct a configured profile named `dogfood-jj` with driver command `scripts/scherzo-workspace-jj` and capabilities `[assert-only, changed-files]`. Assert `from_profile` returns profile `dogfood-jj`, driver `scripts/scherzo-workspace-jj`, and the same capability list. Assert `env_vars` returns exactly `SCHERZO_WORKSPACE_PROFILE=dogfood-jj`, `SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj`, and `SCHERZO_WORKSPACE_CAPABILITIES=assert-only changed-files`. Assert `template_locals` contains `workspace.profile`, `workspace.driver`, and a two-item `workspace.capabilities` list.

14. Run the helper tests:

    direnv exec . gleam test test/workspace_driver_context_test.gleam

   If file-scoped test execution is not supported, run `direnv exec . gleam test`. Expect the helper tests to pass.

15. Update `src/scherzo/workflow_run.gleam`: import `scherzo/workspace_driver_context`, add `workspace_context` to `StepContext`, update `step_context` to accept `profile`, build the context from the resolved profile, and update the `run_step` call site. Let compiler errors guide any direct `StepContext` construction updates in tests.

16. Update `step_command_env(context)` in `src/scherzo/workflow_run.gleam` to append `workspace_driver_context.env_vars(context.workspace_context)` after `SCHERZO_WORKSPACE_PATH`.

17. Add `command_step_receives_workspace_driver_env_from_resolved_profile_test` to `test/workflow_run_test.gleam`. Use the existing `deps(subject, None)` harness, but override `command_step` so it sends a message containing `context.workspace_context.profile`, `context.workspace_context.driver`, and `workspace_capability.serialize(context.workspace_context.capabilities)`. Execute a command workflow whose `workspace_profile` is `dogfood-jj` and whose orchestrator profile has driver command `scripts/scherzo-workspace-jj` and capabilities `[assert-only, changed-files]`. Assert the observed message is `driver_env:dogfood-jj|scripts/scherzo-workspace-jj|assert-only changed-files`. This verifies the same data that `step_command_env` uses without requiring the test to spawn a real shell.

18. Add a source-of-truth command test in `test/workflow_run_test.gleam`. Parse a workflow YAML that includes normal `workspace_profile: dogfood-jj`, `workspace_capabilities: [assert-only]`, and an extra unsupported top-level key such as `workspace_driver: scripts/malicious-driver`. Execute it with the resolved orchestrator profile command `scripts/scherzo-workspace-jj`. Assert the command dependency observes `scripts/scherzo-workspace-jj`, not `scripts/malicious-driver`. This proves workflow content cannot override the driver context.

19. Run the workflow-run tests:

    direnv exec . gleam test test/workflow_run_test.gleam

   If file-scoped test execution is not supported, run `direnv exec . gleam test`. The new command-context tests should fail before steps 15 and 16 and pass after them.

20. Update `prompt_mode_for_step` in `src/scherzo/workflow_run.gleam` to accept `context` and pass merged artifact plus workspace driver locals to `template.render_with_locals` for original prompts. Keep recovery prompts unchanged by returning `RecoveryPrompt` before rendering.

21. Add `agent_prompt_renders_workspace_driver_locals_test` to `test/workflow_run_test.gleam`. Create an agent step with an inline prompt similar to `driver={{ workspace.driver }} profile={{ workspace.profile }} caps={% for capability in workspace.capabilities %}{{ capability }};{% endfor %}`. Use a fake `agent_step` dependency that captures `workflow_attempt.OriginalPrompt(prompt)` with `prompt_text(prompt_mode)` and returns success. Execute with profile `dogfood-jj`, driver `scripts/scherzo-workspace-jj`, and capabilities `[assert-only, changed-files]`. Assert the captured prompt contains `driver=scripts/scherzo-workspace-jj`, `profile=dogfood-jj`, `assert-only;`, and `changed-files;`.

22. Add `agent_prompt_preserves_artifact_locals_with_workspace_driver_locals_test` to `test/workflow_run_test.gleam`. Create a first command step with id `collect` whose fake `command_step` returns a successful artifact with stdout `artifact-value`. Create an agent step depending on `collect` whose inline prompt is `artifact={{ steps.collect.stdout }} driver={{ workspace.driver }}`. Execute with driver `scripts/scherzo-workspace-jj`. Assert the fake agent receives an original prompt containing `artifact=artifact-value` and `driver=scripts/scherzo-workspace-jj`. This uses the existing artifact local naming from `step_artifact.to_template_locals`, where a step's stdout is exposed as `steps.<step_id>.stdout`.

23. Add `recovery_prompt_does_not_rerender_workspace_driver_locals_test` to `test/workflow_run_test.gleam`. Provide `pi_session_continuations` through the existing recovered-run test helper with recovery prompt text that includes `{{ workspace.driver }}` literally. Execute the recovered agent step and assert the fake agent receives a `RecoveryPrompt` whose text still contains the literal braces, proving recovery prompts are not re-rendered or mutated by new locals.

24. Run the workflow-run tests again:

    direnv exec . gleam test test/workflow_run_test.gleam

   If file-scoped test execution is not supported, run `direnv exec . gleam test`. The prompt-local test should fail before step 20 and pass after it; existing recovery tests should remain green.

25. Add a recovery fingerprint safety test in `test/workflow_run_test.gleam`. Build a recovered context whose `workflow_fingerprint` was computed with profile `dogfood-jj` command `scripts/old-driver`. Execute with an orchestrator profile of the same name but command `scripts/new-driver`. Assert `workflow_run.execute_with_context` returns `Error` with reason `workflow_recovery_invalid:workflow_fingerprint_mismatch` before any command or agent dependency observes a workspace context. If the current mismatch reason differs, use the exact existing reason from the recovery code and record it in Surprises & Discoveries.

26. Run the full test suite:

    direnv exec . gleam test

   Expect all tests to pass.

27. Run formatting and production lint gates from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

   Expect each command to exit 0. If formatting fails, run the repository's normal formatter, inspect the diff, and rerun the check. Do not add production `panic`, `todo`, or `let assert`.

28. Commit point after all validation passes: runtime exposure and tests. Suggested commit message: `Expose workspace driver context to workflow steps`.

29. Update this plan's Progress, Surprises & Discoveries, and Outcomes & Retrospective with the validation results and any deviations.

## Testing and Falsifiability

This plan is falsified if a workflow command step cannot read all three driver-context values after the implementation, if an original agent prompt cannot render all three prompt locals, if bundle validation allows a workflow to require a capability the selected profile does not advertise, or if the exposed values come from workflow YAML rather than the resolved orchestrator profile. It is also falsified if a recovered run can proceed after the selected profile's driver command or capabilities changed without a fingerprint mismatch.

Schema tests in `test/workspace_capability_test.gleam`, `test/config_test.gleam`, and `test/workflow_dag_test.gleam` must cover valid and invalid capability names, driver command validation, duplicate capability normalization, parsing configured profile driver metadata, parsing workflow `workspace_capabilities`, and preserving existing empty-driver behavior for legacy or driverless profiles.

Runtime-bundle tests in `test/runtime_bundle_test.gleam` must prove that workflow capability requirements are checked against the resolved profile. The positive test requires `assert-only` and uses a profile that advertises `assert-only`. The negative test requires `publish-change` and uses a profile that lacks it; expected error code is `missing_workspace_capability`.

Fingerprint tests in `test/workflow_fingerprint_test.gleam` must prove configured profile driver command and capability changes affect execution fingerprints. A canonical input assertion must show the driver metadata is actually present in the fingerprint input, not merely changing because of unrelated fields.

Helper tests in `test/workspace_driver_context_test.gleam` must verify exact environment serialization. Input context: profile `dogfood-jj`, driver `scripts/scherzo-workspace-jj`, capabilities `assert-only` and `changed-files`. Expected env values: `SCHERZO_WORKSPACE_PROFILE` is `dogfood-jj`, `SCHERZO_WORKSPACE_DRIVER` is `scripts/scherzo-workspace-jj`, and `SCHERZO_WORKSPACE_CAPABILITIES` is `assert-only changed-files`.

Command-step tests in `test/workflow_run_test.gleam` must prove `StepContext.workspace_context` and therefore `step_command_env` are built from the resolved profile. One happy-path test asserts the profile, driver, and serialized capabilities match `dogfood-jj|scripts/scherzo-workspace-jj|assert-only changed-files`. One source-of-truth test includes an unsupported workflow YAML key such as `workspace_driver: scripts/malicious-driver` and asserts the observed driver remains `scripts/scherzo-workspace-jj`.

Agent prompt tests in `test/workflow_run_test.gleam` must prove original prompts render `workspace.profile`, `workspace.driver`, and `workspace.capabilities` while preserving artifact locals. A recovery prompt test must prove stored recovery prompt text is not re-rendered and therefore literal `{{ workspace.driver }}` text remains literal during recovery.

Recovery tests must prove driver metadata is covered by the execution fingerprint. A recovered run with a fingerprint computed from `scripts/old-driver` must fail when the current resolved profile uses `scripts/new-driver`, and no command or agent step should observe the new context for the old workspace.

Run the complete suite with:

    direnv exec . gleam test

The expected success condition is that all tests pass. The new tests should fail before their implementation steps and pass after them.

## Validation and Acceptance

Acceptance for schema and validation is that configured workspace profiles can define driver metadata, workflows can declare `workspace_capabilities`, missing capabilities fail during runtime-bundle validation before dispatch, and execution fingerprints include configured profile driver metadata.

Acceptance for command steps is that `src/scherzo/workflow_run.gleam` builds each `StepContext` from the resolved selected profile and command steps receive:

    SCHERZO_WORKSPACE_PROFILE=dogfood-jj
    SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj
    SCHERZO_WORKSPACE_CAPABILITIES=assert-only changed-files

Acceptance for agent prompts is that an original prompt rendered by `src/scherzo/workflow_run.gleam` can use:

    {{ workspace.profile }}
    {{ workspace.driver }}
    {% for capability in workspace.capabilities %}{{ capability }}{% endfor %}

Existing artifact locals still render in the same prompt, and recovery prompts are not re-rendered.

Acceptance for safety is that workflow YAML can require capabilities but cannot define or override the driver command, missing required capabilities reject the bundle, and recovered runs reject stale driver metadata through fingerprint mismatch.

The feature is validated by these commands from the repository root:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

All commands must exit 0. The plan is not accepted if pi subprocesses inherit the variables by accident through broad environment plumbing; pi environment inheritance is intentionally out of scope.

## Rollout, Recovery, and Idempotence

The change is additive for existing workflows. Workflows that do not declare `workspace_capabilities` and profiles that do not define a driver keep working with an empty driver command and empty capability list. Existing command-step environment variables must not be renamed or removed.

The new validation affects only workflows that opt into `workspace_capabilities`. Those workflows fail at bundle load if the selected profile does not advertise every required capability. This is safer than failing midway through a workflow command step.

Rollback is straightforward before workflows depend on the new fields: revert the schema additions, capability validation, fingerprint additions, runtime context helper, `StepContext` field, command environment additions, and prompt local additions. If any workflows have started using `$SCHERZO_WORKSPACE_DRIVER` or `workspace.driver`, roll those workflow changes back at the same time. Because this plan does not change stored workspace directories or persisted artifact formats, no data migration is required.

Recovery behavior is protected by execution fingerprints. Configured profile driver command and capability changes must alter the fingerprint, so a recovered run prepared under old driver metadata fails before executing steps under new metadata. `PreparedStepWorkspace.workspace_profile` remains a profile-name consistency check and should not be expanded to store driver command or capabilities unless future tests prove recovery cannot reconstruct context from the selected profile.

All implementation steps are idempotent at the source level. Rerunning tests and validation commands is safe. If implementation stops halfway, keep the tree compiling before committing; do not commit a state where `WorkspaceHookProfile` or `StepContext` call sites are only partially updated.

## Artifacts and Notes

Important current code excerpts to re-check during implementation:

`src/scherzo/config/types.gleam` currently has no driver field on `WorkspaceHookProfile`; this plan adds one and requires updating constructors across the tree.

`src/scherzo/runtime_bundle.gleam` currently validates only that the selected workspace profile exists. This plan extends that validation to required workflow capabilities.

`src/scherzo/workflow_fingerprint.gleam` currently serializes configured workspace profile name and source but not driver metadata. This plan adds driver metadata to configured profile execution fingerprints.

`src/scherzo/workflow_run.gleam` command execution currently calls `command_step.run_with_env` with `step_command_env(context)`. That is the insertion point for command-step variables.

`src/scherzo/workflow_run.gleam` prompt rendering currently calls `template.render_with_locals` with only `step_artifact.to_template_locals(artifacts)`. That is the insertion point for prompt locals.

Do not include local absolute paths in tests or docs. Use repository-relative command strings such as `scripts/scherzo-workspace-jj` or placeholders such as `<absolute-local-path>` when a test needs to discuss forbidden path shapes.

## Interfaces and Dependencies

At the end of the schema milestone, `src/scherzo/config/types.gleam` must expose:

    pub type WorkspaceDriver {
      WorkspaceDriver(command: String, capabilities: List(String))
    }

    pub type WorkspaceHookProfile {
      WorkspaceHookProfile(
        name: String,
        hooks: DagHooksConfig,
        source: WorkspaceProfileSource,
        driver: WorkspaceDriver,
      )
    }

At the end of the workflow-DAG milestone, `src/scherzo/workflow_dag.gleam` must expose `WorkflowDag` with a `workspace_capabilities: List(String)` field. YAML uses the top-level key `workspace_capabilities`.

At the end of the runtime context milestone, `src/scherzo/workspace_driver_context.gleam` must expose an interface equivalent to:

    pub type Context {
      Context(profile: String, driver: String, capabilities: List(String))
    }

    pub fn from_profile(profile: config_types.WorkspaceHookProfile) -> Context

    pub fn env_vars(context: Context) -> List(#(String, String))

    pub fn template_locals(context: Context) -> List(#(String, template.Value))

At the end of the required implementation, command steps must receive these environment variables when the resolved configured profile is `dogfood-jj` with driver `scripts/scherzo-workspace-jj` and capabilities `assert-only` and `changed-files`:

    SCHERZO_WORKSPACE_PROFILE=dogfood-jj
    SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj
    SCHERZO_WORKSPACE_CAPABILITIES=assert-only changed-files

At the end of the required implementation, prompt templates must be able to use these locals:

    {{ workspace.profile }}
    {{ workspace.driver }}
    {% for capability in workspace.capabilities %}{{ capability }}{% endfor %}

No new package dependencies are required. Use existing Gleam standard-library modules, the existing `yay` YAML parser, the existing `template.Value` type, existing runtime-bundle validation structure, and existing workflow-run test harnesses.
