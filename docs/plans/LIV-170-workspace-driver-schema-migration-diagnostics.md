# Add workspace driver schema and safe migration diagnostics

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo workflows are meant to be reusable across repositories, but today a workflow can only rely on repository-specific workspace hook commands. Operators currently configure old `workspace.hooks` or profile-local `hooks` snippets, while workflows can select a `workspace_profile` but cannot declare the workspace operations they need. After this plan is implemented, operators can begin writing named workspace profiles with a typed `driver` schema, workflow authors can declare required workspace capabilities, Scherzo can reject declared metadata mismatches before dispatch, and operators who still have legacy hook config will see actionable doctor diagnostics that point to `docs/runbooks/workspace-driver-migration.md`.

This child plan intentionally validates declared metadata, not real driver behavior. A profile that advertises `assert-only` is promising that a later driver adapter will implement that operation; this plan does not prove the command actually performs `assert-only`, `changed-files`, or any other capability. To avoid silently running workflows without workspace lifecycle setup, driver-backed profiles are schema-valid but not dispatchable in this child plan. When a workflow selects a driver-backed profile, runtime bundle loading first checks the declared capability metadata and then fails safely with a `workspace_driver_invocation_unavailable` diagnostic until the later driver invocation child plan lands. Existing hook-backed profiles remain valid and continue to drive the current runtime.

The observable result is a safe transition point: driver-backed profile YAML parses, required capability YAML parses, missing declared capabilities fail during runtime bundle loading before issue dispatch, selected driver metadata is present in execution fingerprints with canonical ordering, legacy hook shapes remain runnable but produce migration doctor guidance, and selecting a driver-backed profile cannot silently dispatch without driver lifecycle invocation support.

## Problem Framing and Constraints

The operator pain is that Scherzo cannot tell a workflow is incompatible with a repository until too late. A workflow might need an operation such as `assert-only`, but the only current configuration surface is shell hook snippets used for workspace lifecycle setup. That forces workflow prompts and command steps to know whether a repository uses jj, git, a copy-based workspace, or no isolation at all. It also leaves legacy `workspace.hooks` as a hidden default that third-party operators cannot reason about.

This plan is the first child plan under the workspace profile driver umbrella. It handles the core schema, declared compatibility validation, migration diagnostics, fingerprinting, and doctor guidance. It does not implement driver command invocation, define the complete command-line contract for every driver operation, migrate dogfood workflows, expose driver variables to command steps or agent prompts, or make the research workflow portable. Those are later child plans from the umbrella sequence.

The main constraints are safety and reviewability. Driver commands are trusted operator configuration and must stay in orchestrator config, not workflow YAML. Workflow YAML may select a profile and declare required capabilities, but it must not define arbitrary driver shell. Because the current runtime in `src/scherzo/workspace_run.gleam` still prepares workspaces by running `profile.hooks.*`, this plan must not hard-reject hooks or allow driver-only profiles to dispatch as if lifecycle setup happened. The transition is therefore additive: hooks remain valid and operational, driver profiles are accepted as schema and fingerprint data, and actual dispatch through driver profiles is blocked with an explicit diagnostic until the invocation plan converts the runtime.

## Strategy Overview

Add a typed workspace driver model beside the existing workspace profile selection mechanism. A workspace profile remains the operator-selected policy bundle under `workspace.profiles.<name>`. During this transition a configured profile must contain exactly one of `hooks` or `driver`. A hook-backed profile uses the existing `DagHooksConfig` and remains fully operational. A driver-backed profile stores one trusted command string, the lifecycle operations that command claims to support, a fixed set of workflow-facing capabilities, and a timeout, but it is not allowed to dispatch until a later plan teaches `workspace_run.gleam` how to invoke drivers.

Workflows gain a top-level `workspace_capabilities` list. Runtime bundle loading resolves the workflow's selected profile, compares the required capabilities with the selected driver profile's advertised capabilities when a driver is present, and reports `workspace_capabilities_unavailable` for missing declared metadata. If the selected profile is driver-backed and the declared capabilities are satisfied, runtime bundle loading then reports `workspace_driver_invocation_unavailable` instead of dispatching. If the selected profile is hook-backed, it advertises no driver capabilities and continues through the existing hook runtime only when the workflow requires no capabilities.

The implementation should be intentionally conservative. It should introduce typed enums for lifecycle operations and capabilities, with conversion functions to and from YAML strings. It should keep the capability vocabulary fixed for now: `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. It should reject unknown capability names instead of silently accepting them, because early validation is the main value of this child plan.

For migration, do not fail config loading for top-level `workspace.hooks` or profile-local `hooks` in this child plan. Instead, update the `workspace-hooks` doctor check to warn when either legacy shape is present. The warning must name the offending key, say that hook-based workspace configuration is legacy but still supported during the transition, show the new profile driver key shape, and point to `docs/runbooks/workspace-driver-migration.md`. Hard rejection is deferred to the coordinated driver invocation and dogfood/example migration release.

This is proportionate because it reuses existing profile resolution, runtime bundle validation, workflow DAG parsing, fingerprinting, and doctor surfaces. It avoids building driver scripts, command-step environment variables, prompt locals, or full runtime driver invocation before the schema and compatibility rules are stable, while also avoiding a broken main-branch dogfood configuration.

## Alternatives Considered

The smallest alternative is to keep accepting `workspace.hooks` and only add prose documentation. That is insufficient because Scherzo would still have no typed driver schema, workflows could not declare required capabilities, and fingerprints would not capture selected driver metadata once drivers are introduced.

Another alternative is to hard-reject `workspace.hooks` and profile-local `hooks` immediately. That is rejected for this child plan because the current checked-in dogfood config at `.scherzo/scherzo.yaml` uses top-level `workspace.hooks`, `examples/scherzo.yaml` uses profile-local `hooks`, and the runtime still invokes `profile.hooks.*` in `src/scherzo/workspace_run.gleam`. Hard rejection before driver invocation and dogfood/example migration would knowingly make the repository's current operator config invalid.

Another alternative is to implement a minimal driver lifecycle adapter in this child plan by mapping `driver.command` and lifecycle names into the existing hook runner. That would make driver-backed profiles dispatchable sooner, but it would also define part of the driver command contract and require adapter tests that belong in the later invocation child plan. This plan chooses the smaller schema-and-diagnostics step and blocks driver dispatch explicitly instead of silently downgrading lifecycle behavior.

Another alternative is to let workflows declare the driver command directly. That is rejected because driver commands are trusted shell selected by the operator. Workflow files can come from third parties, so they should only say which named profile they need and which capabilities they require.

Another alternative is to allow arbitrary custom capability strings immediately. That is rejected for this child plan because the first validation behavior must be falsifiable. A fixed vocabulary catches misspellings and lets doctor and migration docs explain concrete operations. Namespaced custom capabilities such as `custom:company-review` can be added later when there is a real workflow that needs them.

Another alternative is to rename every internal `WorkspaceHookProfile` reference and remove hook-related helper types in the same change. That is attractive long term, but too broad for this child plan. The implementation should introduce the new driver fields and source variants, but it may leave transitional helper names or old fingerprint helper functions in place if that avoids mixing schema validation with runtime driver invocation.

## Risks and Countermeasures

A hard breaking migration can make an operator's current config fail at startup. Countermeasure: this child plan does not hard-reject hooks. Legacy hook config remains valid and operational, and doctor emits a warning with migration guidance instead. The later hard-rejection plan must be gated on driver invocation support and dogfood/example migration.

Driver-backed profiles can be dangerous if they parse successfully and then dispatch without lifecycle commands running. Countermeasure: runtime bundle loading must fail selected driver-backed profiles with `workspace_driver_invocation_unavailable` after declared capability validation. Add tests proving a workflow that selects a driver-backed profile cannot dispatch silently, even when the profile advertises all required capabilities.

Capability validation can overclaim if it is treated as proof of real driver behavior. Countermeasure: the plan, diagnostics, and runbook must say this is declared metadata validation only. A later driver invocation plan must add smoke tests or contract tests proving configured commands actually implement lifecycle and capability operations.

Capability validation can be too weak if it happens only inside step execution. Countermeasure: put validation in `src/scherzo/runtime_bundle.gleam`, where workflows are already loaded and selected workspace profiles are already checked. Add tests that fail before dispatch when a workflow requires `assert-only` and the selected driver profile does not advertise it.

Capability validation can be too strong if it breaks workflows that do not need workspace operations. Countermeasure: treat omitted `workspace_capabilities` as an empty list. Hook-backed profiles and the synthetic default profile remain compatible only with workflows that require no driver capabilities.

Fingerprints can become unsafe if two runs with different driver commands or capability sets look identical. Countermeasure: include selected driver command, lifecycle list, capability list, and timeout in the execution fingerprint when the selected profile is driver-backed. Keep unselected profiles out of the fingerprint, matching the current selected-profile behavior. Canonicalize driver lifecycle and capability sets so reordering YAML does not create spurious fingerprint changes.

Doctor output can become misleading if it still says only "workspace hooks" after hooks become legacy. Countermeasure: keep the existing `workspace-hooks` check string for CLI stability in this plan, but update human titles, impact text, and remediation text to refer to workspace driver/profile migration and the migration guide.

The implementation can accidentally start defining the full driver command contract. Countermeasure: this plan stores and validates schema only. Driver invocation semantics, adapter scripts, command-step environment variables, and prompt locals are deferred to later child plans. The only runtime behavior for selected driver-backed profiles in this plan is an explicit pre-dispatch block.

## Progress

- [x] (2026-05-09 00:00Z) Read the repo-local ExecPlan authoring guidance.
- [x] (2026-05-09 00:00Z) Read the workspace profile driver umbrella and extracted the child-plan scope.
- [x] (2026-05-09 00:00Z) Inspected current workspace profile, workflow DAG, runtime bundle, fingerprint, doctor, and test surfaces needed for this plan.
- [x] (2026-05-09 00:00Z) Incorporated adversarial review feedback by switching from hard rejection to an additive schema transition, adding a driver dispatch safety gate, making step-level capability rejection mandatory, and specifying canonical fingerprint ordering.
- [ ] Implement Milestone 1: add typed driver and capability schema parsing while preserving hook-backed profiles.
- [ ] Implement Milestone 2: add workflow capability requirements, declared metadata validation, and the driver dispatch safety gate.
- [ ] Implement Milestone 3: add fingerprint coverage for selected driver metadata and workflow capabilities with canonical ordering.
- [ ] Implement Milestone 4: add legacy hook migration doctor warnings and checked-in config/example validation.
- [ ] Implement Milestone 5: add the migration runbook and run full validation.

## Surprises & Discoveries

- Observation: Scherzo already has top-level workflow profile selection but no workflow capability declaration.
  Evidence: `src/scherzo/workflow_dag.gleam` defines `WorkflowDag(id, description, workspace_profile, max_parallel_steps, steps)` and parses top-level `workspace_profile`; `test/workflow_dag_test.gleam` already covers top-level profile parsing and rejects step-level `workspace_profile`.

- Observation: Current orchestrator config turns legacy direct `workspace.hooks` into a synthetic default profile.
  Evidence: `src/scherzo/config.gleam` has `add_legacy_default_profile`, which reads `workspace.hooks` into a `WorkspaceHookProfile` with source `LegacyWorkspaceHooks`; `test/orchestrator_config_test.gleam` has `legacy_workspace_hooks_synthesize_default_profile_test`.

- Observation: Runtime bundle loading is already the right place to reject unknown or incompatible workspace profiles before dispatch.
  Evidence: `src/scherzo/runtime_bundle.gleam` calls `validate_workspace_profiles`, which uses `workspace_profile.resolve` and returns code `unknown_workspace_profile` for missing selected profiles.

- Observation: Execution fingerprints already include the selected configured profile's hook data and exclude unselected profiles.
  Evidence: `src/scherzo/workflow_fingerprint.gleam` resolves the selected profile in `for_execution` and serializes `workspace_profile` plus `dag_hooks` for configured profiles; `test/workflow_fingerprint_test.gleam` checks selected profile behavior.

- Observation: Doctor has a workspace hook check name and remediation text that must be updated carefully rather than replaced blindly.
  Evidence: `src/scherzo/doctor.gleam` defines `WorkspaceHooks` with external string `workspace-hooks`; `src/scherzo/orchestrator/service.gleam` builds the workspace hook doctor result; `test/orchestrator_service_doctor_test.gleam` has doctor workflow config and workspace hook tests.

- Observation: The current runtime still depends on hook fields for workspace lifecycle behavior.
  Evidence: `src/scherzo/workspace_run.gleam` accepts `config_types.WorkspaceHookProfile` and calls `run_create_hook`, `run_before_step_hook`, `after_step`, and `cleanup_run`; `src/scherzo/config.gleam` derives `orchestrator.dag_hooks` from `default_workspace_profile.hooks`.

- Observation: The repository's own operator config and public example still use legacy hooks.
  Evidence: `.scherzo/scherzo.yaml` contains top-level `workspace.hooks`; `examples/scherzo.yaml` contains `workspace.profiles.isolated.hooks` and `workspace.profiles.noop.hooks`.

## Decision Log

- Decision: Keep workspace profiles as the top-level operator abstraction and add `driver` underneath configured profiles.
  Rationale: Profiles already exist in config, workflow DAGs already select them with `workspace_profile`, and keeping trusted shell in operator config preserves the security boundary.
  Date: 2026-05-09

- Decision: Use a fixed core capability vocabulary in this plan.
  Rationale: Fixed strings make parser errors, migration guidance, and runtime compatibility tests concrete. Custom names can be added later with a namespacing rule once a real workflow needs them.
  Date: 2026-05-09

- Decision: Treat omitted `workspace_capabilities` as an empty list.
  Rationale: Existing workflows that do not require driver operations should remain valid once their selected profile resolves. Requiring every workflow to add an empty list would be noisy and would not improve safety.
  Date: 2026-05-09

- Decision: Defer hard rejection of legacy `workspace.hooks` and profile-local `hooks`.
  Rationale: The checked-in dogfood config and example still use hooks, and the runtime still invokes `profile.hooks.*`. Warning through doctor keeps operators informed without breaking the current daemon before driver invocation and dogfood/example migration land.
  Date: 2026-05-09

- Decision: Block selected driver-backed profiles from dispatch in this child plan.
  Rationale: Driver profiles are schema data until `workspace_run.gleam` knows how to invoke driver lifecycle operations. Failing runtime bundle loading with `workspace_driver_invocation_unavailable` is safer than silently preparing a bare directory while `driver.lifecycle` claims `create` or `remove` support.
  Date: 2026-05-09

- Decision: Keep the doctor check string `workspace-hooks` during this child plan, but update its human-facing text to talk about workspace driver migration.
  Rationale: Renaming the CLI check is unrelated to schema validation and would create an extra operator-visible breaking change. A later documentation or cleanup plan can add `workspace-driver` as a new check name or alias.
  Date: 2026-05-09

- Decision: Canonicalize workflow capabilities, driver capabilities, and driver lifecycle names in fingerprints.
  Rationale: These lists are advertised sets in this child plan. YAML ordering should not change fingerprints unless the membership changes.
  Date: 2026-05-09

- Decision: Defer driver invocation, driver adapter scripts, command-step environment variables, and prompt locals.
  Rationale: This child plan proves the schema, metadata validation, diagnostics, and safety gate first. Runtime command contracts and adapters need their own tests and are covered by later umbrella child plans.
  Date: 2026-05-09

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam application that loads operator config, loads workflow DAG YAML files, dispatches issues into workflow steps, prepares per-step workspace directories, and runs command or agent steps. A workflow DAG is a YAML file with fields such as `id`, `workspace_profile`, and `steps`. A workspace profile is an operator-defined policy for how a workflow workspace should be prepared. A workspace hook profile is the current operational profile shape; it stores shell snippets such as `create`, `before_step`, `after_step`, and `remove`, and `src/scherzo/workspace_run.gleam` runs those snippets during workspace preparation and cleanup. A workspace driver is the new trusted command configured by an operator inside a workspace profile. A workspace capability is a named operation a workflow may require from the selected profile's driver.

The current config model lives mostly in `src/scherzo/config/types.gleam` and `src/scherzo/config.gleam`. `src/scherzo/config/types.gleam` currently defines `DagHooksConfig`, `WorkspaceProfileSource`, `WorkspaceHookProfile`, and `WorkspaceHookProfiles`. `DagHooksConfig` has lifecycle hook command fields named `create`, `before_step`, `after_step`, and `remove`, plus `timeout_ms`. `WorkspaceProfileSource` currently distinguishes `LegacyWorkspaceHooks` from `ConfiguredWorkspaceProfile`.

`src/scherzo/config.gleam` currently reads `workspace.hooks` and `workspace.profiles`. Top-level `workspace.hooks` is treated as a legacy default profile. Configured profiles under `workspace.profiles.<name>` currently require a `hooks` map. When `workspace.profiles` is set without `workspace.hooks`, `workspace.default_profile` is required. Profile names are validated by `validate_workspace_profile_name`.

`src/scherzo/workflow_dag.gleam` currently parses workflow YAML. It supports an optional top-level `workspace_profile` string and rejects `workspace_profile` on individual steps. It does not parse `workspace_capabilities` yet.

`src/scherzo/workspace_profile.gleam` chooses the selected profile. If the workflow has `workspace_profile`, that name is selected. Otherwise, `orchestrator.workspace_profiles.default_profile` is selected. Unknown selected profiles produce `UnknownWorkspaceProfile` and a message listing available profile names.

`src/scherzo/runtime_bundle.gleam` loads workflow files and validates their selected profiles before Scherzo dispatches issues. This is where the new declared capability compatibility check and the driver dispatch safety gate belong.

`src/scherzo/workspace_run.gleam` prepares and cleans workspaces. It currently accepts a selected `WorkspaceHookProfile` and runs hook commands through functions such as `run_create_hook`, `run_before_step_hook`, `after_step`, and `cleanup_run`. This plan does not convert those functions to driver invocation. It must therefore prevent driver-backed profiles from reaching dispatch as runnable profiles.

`src/scherzo/workflow_fingerprint.gleam` builds stable hashes from workflow DAGs and selected execution settings. Recovery and rerun logic rely on fingerprints changing when execution-affecting configuration changes. This plan must update fingerprint input so selected driver metadata and workflow-required capabilities are included.

`src/scherzo/doctor.gleam` defines doctor check names and human/logfmt output. `src/scherzo/orchestrator/service.gleam` builds doctor reports and currently contains workspace hook check logic. Doctor should surface migration guidance for legacy hook shapes while treating them as warnings during this transition.

The relevant tests are `test/orchestrator_config_test.gleam`, `test/workflow_dag_test.gleam`, `test/runtime_bundle_test.gleam`, `test/workflow_fingerprint_test.gleam`, and `test/orchestrator_service_doctor_test.gleam`. Existing tests in these files cover legacy profile synthesis, top-level `workspace_profile`, unknown selected profiles, execution fingerprint changes for hook profiles, and doctor workflow-config output.

## Preconditions and Verified Facts

The repository currently uses Gleam. The standard validation commands should be run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. That is an environment setup issue, not an implementation failure.

The current working tree was clean before this plan was drafted, as shown by `jj status --color=never`. Future implementers should start by running that command and ensuring they understand any existing changes before editing.

The current `src/scherzo/config/types.gleam` does not define driver command types or workspace capability types. The current `src/scherzo/workflow_dag.gleam` does not define or parse `workspace_capabilities`. The current `src/scherzo/runtime_bundle.gleam` validates unknown profiles but not missing capabilities or selected driver dispatch safety. The current `src/scherzo/workflow_fingerprint.gleam` serializes selected profile hooks, not driver metadata. The current doctor check is still named `WorkspaceHooks` internally and `workspace-hooks` externally.

This plan depends on the umbrella vocabulary of workspace profile, workspace driver, and workspace capability. It restates that vocabulary here, and an implementer does not need to read the umbrella to carry out this child plan.

## Scope Boundaries

In scope for this plan:

- Add driver schema types and parsers for `workspace.profiles.<name>.driver`.
- Preserve top-level `workspace.hooks` and profile-local `hooks` as valid, operational legacy configuration.
- Require configured profiles to contain exactly one of `hooks` or `driver`; reject profiles that contain both or neither.
- Add fixed lifecycle operation parsing for `create`, `before-step`, `after-step`, and `remove`.
- Add fixed capability parsing for `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`.
- Add workflow-level `workspace_capabilities` parsing.
- Reject step-level `workspace_capabilities` with a pinned workflow DAG error.
- Validate selected profile declared capabilities during runtime bundle loading.
- Block selected driver-backed profiles from dispatch with `workspace_driver_invocation_unavailable` until driver invocation support exists.
- Include workflow capabilities and selected driver metadata in fingerprints with canonical set ordering.
- Update doctor text and tests so legacy hook migration guidance is visible through the existing `workspace-hooks` check.
- Add tests proving `.scherzo/scherzo.yaml` and `examples/scherzo.yaml` still load or are diagnosed according to this additive transition.
- Add `docs/runbooks/workspace-driver-migration.md` as the migration guide.

Out of scope for this plan:

- Implementing `scripts/scherzo-workspace-jj`, a no-op driver, or any other driver script.
- Invoking `driver.command` for lifecycle or capability operations.
- Defining complete driver subcommand behavior such as `assert-only --path` or `changed-files --json` beyond the names in config schema.
- Hard-rejecting top-level `workspace.hooks` or profile-local `hooks`.
- Changing `.scherzo/scherzo.yaml`, `.scherzo/workflows/*.yaml`, `examples/scherzo.yaml`, or `examples/workflows/` to migrate dogfood or example workflows.
- Adding `SCHERZO_WORKSPACE_DRIVER`, `SCHERZO_WORKSPACE_PROFILE`, or `SCHERZO_WORKSPACE_CAPABILITIES` to command-step environments.
- Adding workspace driver locals to prompt templates.
- Renaming the public doctor check from `workspace-hooks` to `workspace-driver`.

## Milestones

Milestone 1 adds the typed schema without breaking existing hooks. At the end of this milestone, `src/scherzo/config/types.gleam` has driver, lifecycle, and capability types, and `src/scherzo/config.gleam` can parse either a legacy hook-backed profile or a configured profile with `driver.command`, `driver.lifecycle`, `driver.capabilities`, and `driver.timeout_ms`. Tests prove valid hook config still parses, a valid driver profile parses, invalid driver fields produce actionable config errors, and mixed `hooks` plus `driver` profiles are rejected.

Milestone 2 adds workflow capability requirements and safe runtime validation. At the end, `src/scherzo/workflow_dag.gleam` parses top-level `workspace_capabilities` and rejects step-level `workspace_capabilities`. `src/scherzo/runtime_bundle.gleam` rejects workflows whose selected profile lacks any required declared capability and rejects selected driver-backed profiles with `workspace_driver_invocation_unavailable` after metadata checks pass. Tests prove hook-backed workflows with no requirements still load, missing capabilities fail before dispatch, and matching driver metadata still cannot dispatch silently.

Milestone 3 updates fingerprints. At the end, a workflow's own fingerprint changes when `workspace_capabilities` membership changes, and an execution fingerprint changes when the selected profile's driver command, lifecycle set, capability set, or timeout changes. Tests also prove reordering workflow capabilities, driver lifecycle names, or driver capability names does not change the fingerprint, and changing an unselected profile does not change the selected execution fingerprint.

Milestone 4 adds migration diagnostics and checked-in config validation. At the end, legacy `workspace.hooks` and profile-local `hooks` remain valid, but doctor warns with a message naming the old key, naming the new `driver` key, and pointing to `docs/runbooks/workspace-driver-migration.md`. Tests prove `.scherzo/scherzo.yaml` and `examples/scherzo.yaml` remain parseable under the additive transition and produce migration guidance through doctor or focused legacy-detection helpers.

Milestone 5 adds the migration runbook and validates the whole change. At the end, `docs/runbooks/workspace-driver-migration.md` contains before-and-after examples, warning messages, future hard-rejection notes, rollback notes, and sequencing guidance for the later adapter and dogfood migration plans. Full tests, format, glinter, and Scherzo lint pass.

## Plan of Work

Start in `src/scherzo/config/types.gleam`. Add a `WorkspaceLifecycleOperation` type with variants for create, before-step, after-step, and remove. Add a `WorkspaceCapability` type with variants for status, diff, changed-files, assert-only, baseline, refresh-base, and publish-change. Add a `WorkspaceDriverConfig` type with fields `command: String`, `lifecycle: List(WorkspaceLifecycleOperation)`, `capabilities: List(WorkspaceCapability)`, and `timeout_ms: Int`.

Update the workspace profile type to carry either hook metadata or driver metadata. The preferred end state is conceptually shaped like this:

    pub type WorkspaceProfileSource {
      LegacyWorkspaceHooks
      ConfiguredWorkspaceHooks
      ConfiguredWorkspaceDriver
      SyntheticDefaultWorkspace
    }

    pub type WorkspaceProfile {
      WorkspaceProfile(
        name: String,
        hooks: Option(DagHooksConfig),
        driver: Option(WorkspaceDriverConfig),
        source: WorkspaceProfileSource,
      )
    }

The implementation may keep the old `WorkspaceHookProfile` type name temporarily if renaming every call site would obscure the schema change, but the stored data must include `hooks: Option(DagHooksConfig)` and `driver: Option(WorkspaceDriverConfig)` or an equivalent tagged shape that prevents mixed semantics. A configured profile under `workspace.profiles.<name>` must contain exactly one of `hooks` or `driver`. A synthetic default profile created when there is no workspace profile config may have `hooks: None`, `driver: None`, and no capabilities. Legacy hook-derived profiles must continue to be created from config in this child plan.

Keep `OrchestratorConfig.dag_hooks` as a transitional compatibility field because existing call sites and helper APIs use it. When the default profile has hooks, set `dag_hooks` to those hooks exactly as today. When the default profile has no hooks, set `dag_hooks` to an empty `DagHooksConfig` with all command fields `None` and `timeout_ms: 60_000`; runtime bundle validation must block selected driver-backed profiles before `workspace_run.gleam` depends on this empty value.

In `src/scherzo/config.gleam`, add parser helpers that convert lifecycle and capability strings to typed values. Use these YAML spellings exactly: lifecycle operations are `create`, `before-step`, `after-step`, and `remove`; capabilities are `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. Reject non-list lifecycle or capability fields, non-string entries, unknown names, duplicates, empty `driver.command`, and non-positive `driver.timeout_ms`. Default `driver.timeout_ms` to `60_000` when omitted.

Still in `src/scherzo/config.gleam`, replace `read_workspace_profile_entry` so a configured profile accepts either a `hooks` map or a `driver` map, but not both. Keep `read_dag_hooks` for legacy hook profiles. If neither key is present, return an invalid config error saying the profile must define `hooks` for legacy runtime behavior or `driver` for the new schema. If both keys are present, return an invalid config error saying `workspace.profiles.<name>` must not mix `hooks` and `driver`. Do not remove `add_legacy_default_profile` in this child plan.

In `src/scherzo/workflow_dag.gleam`, extend `WorkflowDag` with `workspace_capabilities: List(config_types.WorkspaceCapability)` or a small workflow-local capability type if importing config types would create an undesirable dependency. Prefer reusing the shared capability type so runtime validation does not compare raw strings. Parse top-level `workspace_capabilities` as an optional list. Omitted means empty. Reject non-list values, non-string entries, unknown names, and duplicates. Add mandatory step-level rejection parallel to `reject_step_workspace_profile`: if an individual step contains `workspace_capabilities`, return code `step_workspace_capabilities_not_supported` and message `workspace_capabilities is only valid at workflow top level`.

Inventory constructor and helper updates before editing production code. Search for `WorkflowDag(` in `src/` and `test/`; each direct constructor must add `workspace_capabilities: []` unless the test is specifically exercising capabilities. Update parsing helpers in `test/workflow_dag_test.gleam` and `test/workflow_fingerprint_test.gleam` through parsed YAML when possible. Update runtime bundle or fingerprint test builders that construct `WorkspaceHookProfile` or the renamed profile type so legacy hook profiles use `hooks: Some(...)`, `driver: None`, driver profiles use `hooks: None`, `driver: Some(...)`, and synthetic defaults use both `None`.

In `src/scherzo/workspace_profile.gleam`, add a validation function that compares a workflow's required capabilities with the selected profile's provided capabilities. A profile with `driver: None` provides an empty capability list. Return an error that includes the workflow id, selected profile name, required capability names, provided capability names, and missing capability names. Keep the existing unknown-profile error behavior.

In `src/scherzo/workspace_profile.gleam` or `src/scherzo/runtime_bundle.gleam`, add a second safety check for selected driver-backed profiles. After unknown-profile and missing-capability validation, if the selected profile has `driver: Some(_)`, return a new runtime bundle error code such as `workspace_driver_invocation_unavailable` with a message like `workflow research selects workspace_profile noop, but workspace driver invocation is not implemented in this Scherzo version; use a hook-backed profile or wait for the driver invocation migration`. This check is mandatory and is the transition behavior that prevents silent lifecycle downgrade.

In `src/scherzo/runtime_bundle.gleam`, update `validate_workspace_profiles` to call the new compatibility validation and the driver dispatch safety gate. Preserve the existing `unknown_workspace_profile` code for unknown profiles. Use `workspace_capabilities_unavailable` for missing capabilities. The missing-capability message should be concrete enough for doctor and startup logs, for example: `workflow research requires workspace capabilities assert-only but workspace_profile noop does not provide them; missing: assert-only`. The driver-unavailable message must mention the workflow id, selected profile name, and `docs/runbooks/workspace-driver-migration.md`.

In `src/scherzo/workflow_fingerprint.gleam`, serialize `workspace_capabilities` into the workflow DAG canonical input when the list is non-empty. Serialize capability names in canonical vocabulary order so YAML order does not create a spurious fingerprint change. In execution fingerprints, serialize selected driver metadata under a `workspace_driver` or `workspace_profile.driver` object containing command, lifecycle names, capability names, and timeout. Serialize driver lifecycle names in canonical order `create`, `before-step`, `after-step`, `remove`. Serialize driver capability names in canonical order `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, `publish-change`. Do not serialize unselected profiles.

In `src/scherzo/doctor.gleam`, update the human title, impact, and remediation for `WorkspaceHooks` so the output talks about workspace driver migration and references `docs/runbooks/workspace-driver-migration.md`. Keep `check_name_to_string(WorkspaceHooks)` returning `workspace-hooks` in this plan. In `src/scherzo/orchestrator/service.gleam`, update the workspace hook doctor check so it returns `doctor.Warn` when the effective config contains legacy top-level hooks or profile-local hooks. Include fields that identify the legacy key where feasible, such as `legacy_key=workspace.hooks` or `legacy_key=workspace.profiles.isolated.hooks`. Do not make workflow-config doctor fail for legacy hooks in this child plan because config loading remains valid.

Add `docs/runbooks/workspace-driver-migration.md`. Keep it focused on migration, not on the full driver command contract. It should show the old top-level `workspace.hooks` shape, the old profile-local `hooks` shape, and the new profile-local `driver` shape. It should explain that legacy hooks are still supported in this transition, that selecting driver-backed profiles is blocked until driver invocation lands, and that driver adapters plus dogfood workflow migration are separate rollout steps. It should include the exact doctor warning text an operator should expect.

Update tests in `test/orchestrator_config_test.gleam`, `test/workflow_dag_test.gleam`, `test/runtime_bundle_test.gleam`, `test/workflow_fingerprint_test.gleam`, and `test/orchestrator_service_doctor_test.gleam`. Preserve tests that assert legacy hook synthesis is valid, and add new assertions that legacy hooks are discoverable for doctor migration warnings. Add targeted tests before changing production code for each new behavior.

## Concrete Steps

1. From the repository root, inspect source-control state:

       jj status --color=never

   Expect either a clean working copy or only changes you already know about. Do not proceed until unrelated changes are understood.

2. Add failing config parser tests in `test/orchestrator_config_test.gleam`. Add one test named `driver_workspace_profile_parses_schema_test` using config with `workspace.default_profile: noop` and `workspace.profiles.noop.driver.command`, `lifecycle: [create, remove]`, `capabilities: [assert-only]`, and `timeout_ms: 1234`. Assert the default profile is `noop`, the driver command string is preserved, lifecycle values are create and remove, capability value is assert-only, timeout is `1234`, hooks are absent for that profile, and the profile source records driver configuration.

3. In the same test file, preserve or update the existing legacy tests so `workspace.hooks.create: legacy-create` still synthesizes the default profile and profile-local `workspace.profiles.noop.hooks: {}` still parses as a hook-backed profile. Add assertions that legacy hook profiles have `hooks: Some(...)`, `driver: None`, and a legacy or configured-hook source.

4. In the same test file, add failure tests for invalid profile shapes and driver fields: missing both `hooks` and `driver`, both `hooks` and `driver` present in one profile, empty `driver.command`, non-list `driver.lifecycle`, unknown lifecycle name such as `publish`, duplicate lifecycle name such as `[create, create]`, non-list `driver.capabilities`, unknown capability name such as `pull-request`, duplicate capability name such as `[assert-only, assert-only]`, and `timeout_ms: 0`. Keep assertions focused on the key path and reason rather than exact full messages.

5. Run the tests to confirm they fail before implementation:

       direnv exec . gleam test

   Expect failures in the new config tests because the current parser only accepts `hooks` and has no driver schema.

6. Implement the config types and parser changes in `src/scherzo/config/types.gleam` and `src/scherzo/config.gleam`. Keep each parser helper small: one helper for lifecycle names, one for capability names, one for unique list parsing, one for empty hooks, and one for profile-shape validation. Preserve legacy `workspace.hooks` synthesis.

7. Run the tests again:

       direnv exec . gleam test

   Expect the config parser tests to pass or reveal compile errors in call sites that still expect the old profile shape. Fix those call sites by following the constructor inventory in the Plan of Work: hook-backed profiles get hooks and no driver; driver-backed profiles get driver and no hooks; synthetic defaults get neither.

8. Add failing workflow DAG tests in `test/workflow_dag_test.gleam`. Add `parses_workspace_capabilities_test` with YAML containing `workspace_capabilities: [assert-only, changed-files]`, and assert the parsed capability values match those names. Add tests for omitted capabilities defaulting to an empty list, a non-list value, a non-string entry, an unknown capability, a duplicate capability, and step-level `workspace_capabilities` returning code `step_workspace_capabilities_not_supported`.

9. Implement `workspace_capabilities` parsing and mandatory step-level rejection in `src/scherzo/workflow_dag.gleam`. Update direct `WorkflowDag` constructors in production and tests to include `workspace_capabilities: []` unless the test is explicitly about capabilities.

10. Add failing runtime bundle tests in `test/runtime_bundle_test.gleam`. One test should configure a hook-backed profile and load a workflow with omitted `workspace_capabilities`; assert bundle load succeeds. One test should configure a driver-backed profile with no capabilities or only `status`, load a workflow selecting that profile and requiring `[assert-only]`, and assert bundle load fails with code `workspace_capabilities_unavailable` and a message containing the workflow id, profile name, and missing capability. One test should configure a driver-backed profile advertising `assert-only`, load a workflow selecting that profile and requiring `[assert-only]`, and assert bundle load fails with code `workspace_driver_invocation_unavailable` and a message containing the workflow id, profile name, and `docs/runbooks/workspace-driver-migration.md`.

11. Implement declared capability validation and the driver dispatch safety gate in `src/scherzo/workspace_profile.gleam` and `src/scherzo/runtime_bundle.gleam`. Keep unknown-profile validation first, missing-capability validation second, and driver-invocation-unavailable validation third so the diagnostics are specific.

12. Add failing fingerprint tests in `test/workflow_fingerprint_test.gleam`. Add one test proving `workspace_capabilities: [assert-only]` changes the workflow DAG fingerprint compared with no capabilities. Add one test proving two workflow capability lists with the same names in different YAML order have the same canonical fingerprint. Add one test proving an execution fingerprint changes when the selected profile's driver command or capability membership changes. Add one test proving reordering the selected profile's driver lifecycle list and capability list does not change the execution fingerprint. Add one test proving changing an unselected profile does not change the selected execution fingerprint.

13. Implement fingerprint serialization in `src/scherzo/workflow_fingerprint.gleam` with canonical vocabulary ordering for workflow capabilities, driver lifecycle names, and driver capability names.

14. Add failing doctor tests in `test/orchestrator_service_doctor_test.gleam`. Add a workspace-hooks doctor test with a config containing legacy top-level `workspace.hooks`; assert the check status is `doctor.Warn`, the message contains `workspace.hooks` and `docs/runbooks/workspace-driver-migration.md`, and human output contains the same guide path. Add a second test with profile-local `workspace.profiles.noop.hooks` and assert the warning names that key. Preserve workflow-config doctor success for valid legacy config.

15. Add tests that cover checked-in config and example compatibility. In the most appropriate existing test file, load `.scherzo/scherzo.yaml` and `examples/scherzo.yaml` with a stub environment that provides required secrets or avoids secret resolution in the same way existing config tests do. Assert both still parse successfully under this child plan. If direct file loading would make tests brittle because of environment-specific values, add focused fixture strings copied from the relevant workspace sections and record the limitation in Surprises & Discoveries during implementation.

16. Update `src/scherzo/doctor.gleam` and, if needed, `src/scherzo/orchestrator/service.gleam` so doctor output preserves migration guidance, uses workspace driver migration wording, and warns rather than fails for legacy hooks.

17. Add `docs/runbooks/workspace-driver-migration.md`. Include before-and-after YAML examples for top-level hooks and profile hooks. Include a short section explaining that this plan only adds schema, declared metadata validation, dispatch blocking for selected driver profiles, and diagnostics; driver adapters and dogfood migration follow in later child plans.

18. Run formatting:

       direnv exec . gleam format --check src test

   If it fails, run the repository's normal formatter command for the changed Gleam files, then rerun the check. The final check should pass.

19. Run full tests:

       direnv exec . gleam test

   Expect all tests to pass. The exact test count may drift; the important result is zero failures.

20. Run production lint gates:

       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect no lint errors. Existing warnings may be reported as part of the repository's ratchet policy, but do not add new warnings.

21. Inspect source-control state:

       jj status --color=never

   Expect changes only in the files named by this plan. Commit the schema, validation, fingerprint, doctor, tests, and runbook as one logical change after validation is green. A suitable commit message is `Add workspace driver schema diagnostics`.

## Testing and Falsifiability

This plan is falsified if a workflow requiring a capability can still be loaded with a selected profile that lacks that declared capability. The runtime bundle test must prove this failure happens before dispatch with code `workspace_capabilities_unavailable`.

This plan is falsified if a driver-backed profile can be selected and dispatched before driver invocation support exists. The runtime bundle test must prove that a workflow selecting a driver-backed profile whose metadata satisfies all requirements still fails before dispatch with code `workspace_driver_invocation_unavailable`. This is the safety invariant that prevents silent lifecycle downgrade.

This plan is falsified if legacy hook config becomes invalid during this child plan. The config and checked-in compatibility tests must prove top-level `workspace.hooks`, profile-local `hooks`, `.scherzo/scherzo.yaml`, and `examples/scherzo.yaml` still parse or are covered by equivalent focused fixtures if direct loading is not practical.

This plan is falsified if legacy hook config lacks migration guidance. The doctor tests must assert the warning message contains the old key, the new `driver` concept, and `docs/runbooks/workspace-driver-migration.md`.

This plan is falsified if fingerprinting ignores execution-affecting selected driver metadata. The fingerprint tests must prove changing the selected driver command or selected profile capability membership changes the execution fingerprint. The tests must also prove changing an unselected profile does not change the fingerprint, because unselected profiles should not invalidate recoverable runs.

This plan is falsified if ordering alone changes fingerprints for set-like metadata. Tests must prove workflow `workspace_capabilities`, selected driver `lifecycle`, and selected driver `capabilities` produce stable fingerprints when the same values are listed in a different YAML order.

Add or update these exact test groups:

In `test/orchestrator_config_test.gleam`, add tests for valid driver parsing, invalid driver schema fields, mixed hook/driver rejection, legacy top-level `workspace.hooks` preservation, and profile-local `hooks` preservation.

In `test/workflow_dag_test.gleam`, add tests for successful `workspace_capabilities` parsing, omitted capabilities defaulting to an empty list, invalid capability values, duplicate capabilities, and mandatory step-level rejection with code `step_workspace_capabilities_not_supported`.

In `test/runtime_bundle_test.gleam`, add tests for hook-backed selected profile success with no required capabilities, selected driver profile capability failure, selected driver profile dispatch blocking after capabilities match, default profile capability failure when `workspace_capabilities` is required and the default has no driver, and preservation of the existing unknown-profile error code.

In `test/workflow_fingerprint_test.gleam`, add tests for workflow capabilities in DAG fingerprints, selected driver metadata in execution fingerprints, stable canonical ordering of workflow capability lists, stable canonical ordering of selected driver lifecycle and capability lists, and unselected profile exclusion.

In `test/orchestrator_service_doctor_test.gleam`, add tests for doctor workspace-hooks warnings caused by legacy hook config and for human output containing the migration guide path.

Run these validation commands from the repository root:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

## Validation and Acceptance

The implementation is accepted when a reviewer can observe all of the following behavior in tests and, where useful, by reading the new runbook.

A config with a driver-backed profile parses successfully but is not yet dispatchable:

    workspace:
      root: workspaces
      default_profile: noop
      profiles:
        noop:
          driver:
            command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-noop"
            lifecycle: [create, remove]
            capabilities: [assert-only]
            timeout_ms: 60000

A workflow can declare a required capability:

    version: 1
    id: research
    workspace_profile: noop
    workspace_capabilities: [assert-only]
    steps:
      - id: research
        kind: agent
        prompt: prompts/research.md

If profile `noop` advertises `assert-only`, declared capability validation succeeds, but runtime bundle loading still fails before dispatch because driver invocation is not implemented in this child plan. Expected diagnostic content:

    workspace_driver_invocation_unavailable
    workflow research
    workspace_profile noop
    docs/runbooks/workspace-driver-migration.md

If profile `noop` omits `assert-only`, runtime bundle loading fails before the driver-unavailable gate with a message naming `research`, `noop`, and `assert-only`, and with code `workspace_capabilities_unavailable`.

A legacy top-level config remains valid and operational:

    workspace:
      root: workspaces
      hooks:
        create: scripts/legacy-create

Doctor output for the `workspace-hooks` check warns with migration guidance. Expected diagnostic content:

    workspace.hooks is legacy workspace configuration
    workspace.profiles.<name>.driver
    docs/runbooks/workspace-driver-migration.md

A profile-local legacy config also remains valid and produces migration guidance:

    workspace:
      root: workspaces
      default_profile: noop
      profiles:
        noop:
          hooks: {}

Expected diagnostic content:

    workspace.profiles.noop.hooks is legacy workspace configuration
    workspace.profiles.noop.driver
    docs/runbooks/workspace-driver-migration.md

A step-level capability declaration is rejected:

    steps:
      - id: research
        kind: agent
        prompt: prompts/research.md
        workspace_capabilities: [assert-only]

Expected workflow DAG error code and message:

    step_workspace_capabilities_not_supported
    workspace_capabilities is only valid at workflow top level

Full validation commands pass from the repository root. The checked-in dogfood config and example config either parse in tests or are represented by focused workspace-section fixtures with a note explaining why direct loading was not used.

## Rollout, Recovery, and Idempotence

This child plan is additive and should be safe to merge before the driver invocation and dogfood migration child plans. Legacy hook config remains valid, so the current daemon can continue using `.scherzo/scherzo.yaml` after this plan lands. Operators should expect a doctor warning telling them hooks are legacy and pointing to `docs/runbooks/workspace-driver-migration.md`, but startup should not fail solely because hooks are present.

Do not switch an active daemon to a driver-backed profile after this child plan alone. Runtime bundle loading will reject workflows that select driver-backed profiles with `workspace_driver_invocation_unavailable`. That failure is intentional containment, not a rollout bug. To run driver-backed profiles, wait for the driver invocation child plan and the adapter/dogfood migration plan.

There is no stored data migration. The main recovery concern is workflow execution fingerprints. Because selected driver metadata and required capabilities become part of the fingerprint, a recovered run whose selected profile changed may be treated as incompatible. That is desired once driver profiles become dispatchable: it prevents a workflow from silently resuming under a different workspace driver contract. During this child plan, hook-backed profiles continue to fingerprint through their hook metadata as before.

The migration runbook should include manual rollback instructions: if doctor warnings are disruptive, temporarily pin to the previous Scherzo version or ignore the warning until the coordinated driver invocation release. Do not promise that a schema-only Scherzo can run driver-backed profiles.

The implementation steps are idempotent at the source level. Re-running tests and lint is safe. Re-running parser changes should not generate files. The new runbook is deterministic text and can be edited normally.

The later hard-rejection rollout must be a separate gated plan. That future plan must require driver invocation support, update `.scherzo/scherzo.yaml`, update `examples/scherzo.yaml`, validate checked-in dogfood and example configs under the new driver shape, and only then convert legacy hook warnings into startup errors.

## Artifacts and Notes

The old top-level shape to preserve for now and warn about in doctor is:

    workspace:
      root: workspaces
      hooks:
        create: scripts/scherzo-jj-workspace create
        before_step: scripts/scherzo-jj-workspace before-step
        remove: scripts/scherzo-jj-workspace remove

The old profile-local shape to preserve for now and warn about in doctor is:

    workspace:
      root: workspaces
      default_profile: isolated
      profiles:
        isolated:
          hooks:
            create: scripts/scherzo-jj-workspace create

The new schema shape to parse is:

    workspace:
      root: workspaces
      default_profile: isolated
      profiles:
        isolated:
          driver:
            command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [status, diff, changed-files, assert-only, baseline, refresh-base, publish-change]
            timeout_ms: 60000

The exact driver command behavior behind those names is not implemented by this child plan. Later driver adapter plans will define and test commands such as:

    $SCHERZO_WORKSPACE_DRIVER lifecycle create
    $SCHERZO_WORKSPACE_DRIVER assert-only --path research-findings.md

The migration guide should not claim those commands work until the adapter child plan has landed. It should say that driver-backed profiles are schema-valid but rejected for dispatch in this child plan.

## Interfaces and Dependencies

Add these concepts to `src/scherzo/config/types.gleam` or an adjacent shared module if the implementation needs to avoid import cycles:

    pub type WorkspaceLifecycleOperation {
      LifecycleCreate
      LifecycleBeforeStep
      LifecycleAfterStep
      LifecycleRemove
    }

    pub type WorkspaceCapability {
      WorkspaceStatus
      WorkspaceDiff
      WorkspaceChangedFiles
      WorkspaceAssertOnly
      WorkspaceBaseline
      WorkspaceRefreshBase
      WorkspacePublishChange
    }

    pub type WorkspaceDriverConfig {
      WorkspaceDriverConfig(
        command: String,
        lifecycle: List(WorkspaceLifecycleOperation),
        capabilities: List(WorkspaceCapability),
        timeout_ms: Int,
      )
    }

The workspace profile model should expose both optional hooks and optional driver metadata during this transition. Configured profiles must have exactly one of the two. The synthetic default may have neither. Hook-backed profiles remain the only dispatchable profiles in this child plan.

The workflow DAG model should gain a field equivalent to:

    workspace_capabilities: List(WorkspaceCapability)

The runtime compatibility functions should expose behavior equivalent to:

    pub fn validate_capabilities(
      dag: workflow_dag.WorkflowDag,
      profile: WorkspaceProfile,
    ) -> Result(Nil, ProfileResolutionError)

    pub fn validate_dispatchable_profile(
      dag: workflow_dag.WorkflowDag,
      profile: WorkspaceProfile,
    ) -> Result(Nil, ProfileResolutionError)

The final internal error names do not need to match this sketch exactly, but tests must pin the external runtime bundle codes `workspace_capabilities_unavailable` and `workspace_driver_invocation_unavailable` and the human message content.

The files expected to change during implementation are:

- `src/scherzo/config/types.gleam`
- `src/scherzo/config.gleam`
- `src/scherzo/workflow_dag.gleam`
- `src/scherzo/workspace_profile.gleam`
- `src/scherzo/runtime_bundle.gleam`
- `src/scherzo/workflow_fingerprint.gleam`
- `src/scherzo/doctor.gleam`
- `src/scherzo/orchestrator/service.gleam`
- `test/orchestrator_config_test.gleam`
- `test/workflow_dag_test.gleam`
- `test/runtime_bundle_test.gleam`
- `test/workflow_fingerprint_test.gleam`
- `test/orchestrator_service_doctor_test.gleam`
- `docs/runbooks/workspace-driver-migration.md`

The files to inspect but not change during this child plan are:

- `src/scherzo/workspace_run.gleam`, to confirm this plan does not accidentally promise driver lifecycle invocation and to understand why selected driver profiles must be blocked before dispatch.
- `src/scherzo/workflow_run.gleam`, to confirm command-step driver environment variables remain out of scope.
- `.scherzo/scherzo.yaml`, to confirm current dogfood legacy-hook usage remains valid and warned about.
- `examples/scherzo.yaml`, to confirm current example profile hooks remain valid and warned about.

## Open Questions and Clarifications Needed

None.
