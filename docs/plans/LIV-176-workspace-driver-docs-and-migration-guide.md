# Document workspace drivers, examples, and the migration guide

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this plan is implemented, an operator who has never worked on Scherzo can read the public documentation, choose a workspace profile backed by a workspace driver, migrate an old `workspace.hooks` configuration, and verify the result before dispatching workflow runs. A workflow author can read the examples and understand the difference between a workspace profile, a workspace driver, and a workspace capability without learning this repository's dogfood jj setup first. A reviewer can prove the documentation is not stale by running the repository's doc-focused tests and workflow-config checks.

This is the documentation and migration-cleanup child of the workspace-driver umbrella. The umbrella's core idea is that workflows should ask for named workspace capabilities, while trusted operator config supplies a named workspace profile whose driver command implements those capabilities. This plan does not design or implement runtime driver behavior. It documents the runtime surface only after the schema, adapter scripts, capability validation, migration diagnostics, and driver-environment exposure have landed and passed the hard prerequisite gate below.

## Problem Framing and Constraints

The current documentation still teaches the old mental model. The top-level `README.md` shows `workspace.profiles` entries that contain inline `hooks`, has a separate legacy `workspace.hooks` example, and has a section titled `Workspace hooks`. The dogfood `.scherzo/README.md` tells maintainers to call `scripts/scherzo-jj-workspace` from YAML `workspace.hooks`. `examples/scherzo.yaml` demonstrates profiles named `isolated` and `noop`, but both profiles still contain `hooks`. `docs/ARCHITECTURE.md` describes "workspace hook profiles" rather than drivers and capabilities. The command-line usage string in `src/scherzo/main.gleam` also names `workspace.hooks` as a required runtime input.

Those facts make the new driver model hard to adopt and make a breaking migration risky. If operators see a runtime error telling them that direct `workspace.hooks` is no longer accepted, the repository must have a clear migration guide at `docs/runbooks/workspace-driver-migration.md`. If reusable examples still use hook-shaped config, third-party repositories will copy the wrong shape and fail. If the docs mention drivers but the examples cannot pass Scherzo's own workflow-config checks, the migration guidance is not trustworthy.

The constraints are intentionally narrow. This plan may update prose documentation, example YAML, doc tests, and user-visible help text. It must not change config parsing, workflow execution, workspace preparation, driver scripts, pi launch behavior, or fingerprint semantics. Those runtime changes belong to earlier workspace-driver child plans. If those earlier plans have not landed, this plan is blocked; the implementer must update the living-document sections and stop before editing user-facing docs or examples.

## Hard Prerequisite Gate

This is a hard entry condition, not a suggestion. Do not add the doc tests, migrate examples, change README prose, change `.scherzo/README.md`, or edit help text until every item in this gate is true in the checked-out repository. If any item fails, update Progress with the failed item, add a Surprises & Discoveries note with the command output, add or update `[CLARIFY]` entries in Open Questions, and stop. There is no successful merge path for migrated docs or examples with a failed prerequisite gate.

The runtime/schema gate requires checked-in code and tests that prove these exact external names are implemented, or a prior Decision Log entry in this ExecPlan that replaces all of them with the final checked-in names before documentation edits begin:

- `src/scherzo/config.gleam`, or the final config parser module if it was renamed, parses `workspace.profiles.<name>.driver.command`, `workspace.profiles.<name>.driver.lifecycle`, `workspace.profiles.<name>.driver.capabilities`, and `workspace.profiles.<name>.driver.timeout_ms`.
- The same config/parser and doctor path surfaces legacy direct `workspace.hooks` as migration material and includes `docs/runbooks/workspace-driver-migration.md` in the diagnostic or doctor guidance. In the checked 2026-05-11 runtime this is a doctor warning rather than an immediate parser rejection.
- `src/scherzo/workflow_dag.gleam`, or the final workflow DAG parser module if it was renamed, parses top-level `workspace_capabilities` as a list of required driver capabilities.
- `src/scherzo/workspace_profile.gleam`, or the final workspace-profile module if it was renamed, has a driver-backed profile representation equivalent to `WorkspaceDriver` and validates workflow-required capabilities against the selected profile before dispatch.
- `src/scherzo/workspace_run.gleam` and `src/scherzo/workflow_run.gleam`, or their final runtime equivalents if renamed, invoke the selected driver's lifecycle operations and expose these command-step environment variables when documentation mentions them: `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`.
- Runtime tests exist and pass for driver-profile parsing, legacy-hook migration guidance, workflow-capability parsing, capability validation failure, driver lifecycle invocation, and driver environment exposure. If the earlier runtime plans used different test names, record the actual file and test names in the Decision Log before proceeding.

The driver-command gate requires checked-in executable scripts for the examples. This docs plan pins the public reusable example strategy to checked, portable driver command strings: config-relative paths for checked examples, PATH commands, or absolute trusted wrappers in operator-owned configs, not local developer paths and not undocumented dogfood-only environment variables. The required checked-in commands are:

- `scripts/scherzo-workspace-jj`, executable, for the general isolated profile and dogfood jj profile.
- `scripts/scherzo-workspace-noop`, executable, for the public no-op or artifact-only profile that supports `assert-only`.

If the runtime adapter lands different script names, do not choose replacements during docs editing. First update this gate, the Context and Orientation examples, the planned tests, and the Decision Log with the final checked-in script paths.

The dogfood-alignment gate requires `.scherzo/scherzo.yaml` to have already been migrated by its owning runtime or dogfood migration work before this docs plan changes `.scherzo/README.md` to describe the final dogfood driver profile. This plan does not own edits to `.scherzo/scherzo.yaml`. If `.scherzo/scherzo.yaml` still contains direct `workspace.hooks`, this plan is blocked; do not publish dogfood README instructions that disagree with checked-in dogfood config.

The validation gate requires these commands, run from the repository root, to exit 0 before documentation edits begin:

    test -x scripts/scherzo-workspace-jj
    test -x scripts/scherzo-workspace-noop
    grep -R "workspace_capabilities" -n src test
    grep -R "SCHERZO_WORKSPACE_DRIVER" -n src test
    grep -R "docs/runbooks/workspace-driver-migration.md" -n src test
    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
    direnv exec . gleam test

Expected output is command-specific, but the important observable result is zero exit status for every command, grep matches in runtime code and tests rather than only in docs, and `gleam test` reporting all tests passed. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. That is environment setup, not a test failure.

## Strategy Overview

Treat the work as a post-runtime documentation migration with a binary go/no-go preflight. First prove the runtime surface, adapter scripts, dogfood config, and migration diagnostics exist. If they do not exist, stop with the plan updated; do not write future-looking public docs that the current binary cannot parse.

After the hard gate passes, add tests that describe the desired documentation surface: the README explains profiles, drivers, capabilities, environment variables, and migration; the migration guide contains before-and-after examples and validation commands; reusable examples use driver-backed profiles instead of hooks; the dogfood README points at the checked-in dogfood driver profile; architecture docs use driver vocabulary; and referenced driver scripts exist. Those tests should fail before the documentation edits for documentation reasons only, not because runtime prerequisites are missing.

Then update documentation from broadest to most specific. Start with `README.md` because it is the primary public entry point. Update `docs/ARCHITECTURE.md` so maintainers see the same model. Add the migration runbook because it is the recovery path for breaking configs. Update `examples/scherzo.yaml` and any example workflow comments so copied examples use the new shape. Update `.scherzo/README.md` only after the dogfood-alignment gate proves `.scherzo/scherzo.yaml` already uses the same driver profile. Finally update the CLI usage string in `src/scherzo/main.gleam` and its test so `scherzo --help` does not keep teaching the old required inputs.

This is proportionate because it avoids a second runtime migration. The only code-like changes are tests and help text; all runtime driver behavior is assumed to exist already and is guarded by the prerequisite gate. The plan is falsifiable because stale docs cause explicit test failures, workflow-config checks fail if examples or dogfood config are invalid, and a bounded search finds any remaining accidental old-hook language outside migration context.

## Alternatives Considered

One alternative is to update only `README.md` and leave examples and dogfood docs for later. That is insufficient because operators copy examples more often than prose, and `.scherzo/README.md` is the guide for the repository's own workflows. Leaving those stale would make the migration guide look optional or theoretical.

Another alternative is to keep both hooks and drivers as equal public models in the docs. That is rejected because the umbrella explicitly chooses a clean breaking migration over a long-lived compatibility story. The docs should still mention `workspace.hooks`, but only as a legacy shape that now produces migration guidance.

A third alternative is to implement more runtime diagnostics as part of this docs plan. That is too broad for this ticket. The core schema and migration-diagnostic child plans own parser errors, doctor checks, driver command contracts, and environment exposure. This plan may document those surfaces and add tests that protect documentation, but it must not add new runtime behavior.

A fourth alternative is to allow an "upcoming docs" branch that describes driver config before the runtime can parse it. That is rejected for the checked-in public docs and examples because it would create an operator failure mode: users could copy a driver-backed config into a hook-only binary. If stakeholders need future-looking release notes, they should create a separate document explicitly labeled as upcoming and keep runnable examples unchanged until the runtime gate passes.

## Risks and Countermeasures

The largest risk is documenting names that differ from the final runtime implementation. Countermeasure: the hard prerequisite gate validates actual names before any docs edit. The pinned names are `workspace.profiles.<name>.driver.command`, `driver.lifecycle`, `driver.capabilities`, `driver.timeout_ms`, workflow-level `workspace_capabilities`, `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, `SCHERZO_WORKSPACE_CAPABILITIES`, `scripts/scherzo-workspace-jj`, and `scripts/scherzo-workspace-noop`. If the code uses different final names, update this ExecPlan's Decision Log, Context, planned tests, and examples before editing docs.

A second risk is making `examples/scherzo.yaml` look nice but not runnable. Countermeasure: checked public examples use config-relative command paths to scripts that must exist and be executable before implementation starts, and example command steps resolve simple relative driver paths against `SCHERZO_CONFIG_DIR` before invoking driver capabilities from inside a prepared workspace. After editing, both `examples/scherzo.yaml` and `.scherzo/scherzo.yaml` must pass `doctor --check workflow-config` with a dummy Linear key. Placeholders such as `<driver-command>` are allowed only in explanatory prose, not in checked configs that are expected to pass doctor validation.

A third risk is removing all mentions of `workspace.hooks` and leaving operators with no migration path. Countermeasure: keep `workspace.hooks` in the migration guide and in brief legacy warnings that link to that guide. The cleanup target is not zero mentions; it is no accidental endorsement of direct hooks as a current configuration model.

A fourth risk is accidentally broadening into runtime implementation. Countermeasure: do not edit `src/scherzo/config.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workspace_profile.gleam`, `src/scherzo/runtime_bundle.gleam`, or driver scripts under `scripts/` as part of this plan. If a doc edit reveals runtime behavior is missing, stop and record the failed prerequisite instead of implementing it here.

A fifth risk is dogfood docs disagreeing with dogfood config. Countermeasure: this plan does not own `.scherzo/scherzo.yaml`; it requires that file to be migrated before `.scherzo/README.md` is changed to driver guidance. Acceptance requires both dogfood docs and dogfood config to describe the same profile and for the dogfood workflow-config doctor check to pass. There is no skip path for dogfood validation.

## Progress

- [x] (2026-05-09 00:00Z) Read the repository-local ExecPlan authoring guidance.
- [x] (2026-05-09 00:00Z) Read the workspace-driver umbrella source material and identified this as the documentation child plan.
- [x] (2026-05-09 00:00Z) Verified that the current working copy was clean before creating this plan.
- [x] (2026-05-09 00:00Z) Inspected the current public docs, dogfood docs, examples, architecture notes, and relevant tests with the smallest useful scope.
- [x] (2026-05-09 00:00Z) Incorporated adversarial review feedback by adding a hard prerequisite gate, pinning example command strategy, removing validation escape hatches, and aligning dogfood docs with dogfood config ownership.
- [x] (2026-05-11 17:40Z) Ran the hard prerequisite gate. `test -x` passed for `scripts/scherzo-workspace-jj` and `scripts/scherzo-workspace-noop`; grep found runtime and test references for `workspace_capabilities`, `SCHERZO_WORKSPACE_DRIVER`, and `docs/runbooks/workspace-driver-migration.md`; the dogfood workflow-config doctor check passed; and `direnv exec . gleam test` reported 1100 passed before doc-test additions.
- [x] (2026-05-11 17:40Z) The hard prerequisite gate passed, so the failed-gate stop path was not used.
- [x] (2026-05-11 17:40Z) Recorded final runtime evidence and adjusted the documentation target to the checked warning-based legacy-hook transition, driver scripts, environment variables, lifecycle names, and currently implemented capabilities.
- [x] (2026-05-11 17:40Z) Added `test/workspace_driver_docs_test.gleam` for README, migration guide, examples, driver script presence, dogfood README, and architecture vocabulary.
- [x] (2026-05-11 17:40Z) Confirmed the new doc tests failed for documentation reasons before edits: the migration guide was absent, examples used hooks, README and architecture used hook vocabulary, and dogfood README lacked the exact `workspace driver` wording.
- [x] (2026-05-11 17:40Z) Updated `README.md` to teach workspace profiles, drivers, lifecycle operations, capabilities, migration, and driver environment variables.
- [x] (2026-05-11 17:40Z) Updated `docs/ARCHITECTURE.md` to use workspace driver and workspace capability vocabulary and to describe direct hooks as legacy migration shapes.
- [x] (2026-05-11 17:40Z) Added `docs/runbooks/workspace-driver-migration.md` with before-and-after direct-hook, named-profile, no-op, dogfood jj, validation, troubleshooting, and rollback guidance.
- [x] (2026-05-11 17:40Z) Updated `examples/scherzo.yaml` to use driver-backed `isolated` and `noop` profiles, and verified `examples/workflows/research.yaml` already declared `workspace_capabilities: [assert-only]` and used the selected workspace driver for `assert-only`.
- [x] (2026-05-11 17:40Z) Updated `.scherzo/README.md` to use exact dogfood workspace driver wording after `.scherzo/scherzo.yaml` passed workflow-config validation.
- [x] (2026-05-11 17:40Z) Updated `src/scherzo/main.gleam` usage text and `test/main_test.gleam` so help no longer names `workspace.hooks` as a current required input.
- [x] (2026-05-11 17:40Z) Updated stale transition-release wording in `docs/runbooks/portable-research-workflow.md` and `docs/runbooks/workspace-driver-contract.md` because the stale-term review found public runbook guidance that still described driver lifecycle invocation as not yet enabled.
- [x] (2026-05-11 17:45Z) Ran final validation. `test -x scripts/scherzo-workspace-jj`, `test -x scripts/scherzo-workspace-noop`, `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml`, `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml`, `direnv exec . gleam test`, and `direnv exec . gleam format --check src test` all exited 0. `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint` also exited 0 with the existing warning inventory and no errors.
- [x] (2026-05-11 17:40Z) No commit was created during implementation because the Scherzo workflow contract says the publish step creates the final logical jj commit.
- [x] (2026-05-11 18:10Z) Applied post-review feedback for the checked reusable research example: `examples/scherzo.yaml` now uses config-relative `../scripts/...` driver commands from the `examples/` directory, `examples/workflows/research.yaml` resolves relative `SCHERZO_WORKSPACE_DRIVER` values against `SCHERZO_CONFIG_DIR` before invoking `assert-only`, and the README, migration guide, portable research runbook, and doc tests now document that command-step behavior.
- [x] (2026-05-11 18:15Z) Ran post-review targeted and full validation. `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml` passed, `direnv exec . gleam format --check test/workspace_driver_docs_test.gleam test/portable_research_workflow_test.gleam` passed, and `direnv exec . gleam test` reported 1107 passed with no failures.

## Surprises & Discoveries

- Observation: The current top-level documentation teaches both named profiles with `hooks` and legacy direct `workspace.hooks`.
  Evidence: `README.md` contains a minimal config where `workspace.profiles.isolated.hooks` and `workspace.profiles.noop.hooks` are shown, and a later section titled `Workspace hooks` says legacy direct `workspace.hooks` is still treated as a synthetic `default` profile.

- Observation: The dogfood README is tightly coupled to the old jj hook helper.
  Evidence: `.scherzo/README.md` says to use `scripts/scherzo-jj-workspace` from YAML `workspace.hooks` and describes jj workspaces as the dogfood workspace population mechanism.

- Observation: The reusable example config already has named profiles, but not driver-backed profiles.
  Evidence: `examples/scherzo.yaml` defines `workspace.default_profile: isolated` and profiles named `isolated` and `noop`; both profiles contain `hooks` blocks rather than `driver` blocks.

- Observation: The current architecture doc still describes hook-owned workspaces.
  Evidence: `docs/ARCHITECTURE.md` says orchestrator-defined workspace hook profiles create, copy, validate, and remove content, and says workflows may select a profile with `workspace_profile`.

- Observation: The repository already uses tests to protect documentation expectations.
  Evidence: `test/test_suite_contract_test.gleam` reads `README.md`; `test/workflow_portability_test.gleam` reads `.scherzo/README.md` and asserts specific portability guidance; `test/main_test.gleam` checks `src/scherzo/main.gleam` usage text.

- Observation: The current jj workspace helper is a lifecycle helper, not the final driver contract.
  Evidence: `scripts/scherzo-jj-workspace` accepts `after-create`, `before-run`, and `before-remove`; it does not expose the umbrella's capability subcommands such as `assert-only`, `changed-files`, or `publish-change`.

- Observation: The review found the plan's original assumed driver names and scripts were not yet verified as checked-in runtime facts.
  Evidence: The review noted that relevant public surfaces still showed the hook model and that proposed `scripts/scherzo-workspace-*` commands were not present in the current checked tree.

- Observation: The checked runtime uses a warning-based legacy-hook transition rather than rejecting all legacy hook config at parse time.
  Evidence: `test/orchestrator_service_doctor_test.gleam` contains `doctor_workspace_hooks_warns_for_top_level_legacy_hooks_test` and `doctor_workspace_hooks_warns_for_profile_local_legacy_hooks_test`; `test/orchestrator_config_test.gleam` still contains `legacy_workspace_hooks_synthesize_default_profile_test` and `workspace_hooks_can_coexist_with_extra_profiles_test`.

- Observation: The final checked capability set is smaller than one early example in this plan.
  Evidence: `docs/runbooks/workspace-driver-contract.md` and `scripts/scherzo-workspace-jj` support `status`, `diff`, `changed-files`, and `assert-only`; `scripts/scherzo-workspace-noop` supports `status`, `changed-files`, and `assert-only`, while `baseline`, `refresh-base`, and `publish-change` are reserved for later plans.

- Observation: Public runbooks outside the initial file list had stale transition-release wording.
  Evidence: The stale-term review found `docs/runbooks/portable-research-workflow.md` saying driver-only workspace lifecycle invocation was not enabled and `docs/runbooks/workspace-driver-contract.md` saying legacy hook-backed profiles remained until runtime driver invocation was enabled.

- Observation: The checked reusable research example needed to account for command-step working directories, not just workflow-config parsing.
  Evidence: The post-review finding noted that `examples/workflows/research.yaml` invoked `"$SCHERZO_WORKSPACE_DRIVER"` from inside the prepared workspace while `examples/scherzo.yaml` exposed a relative command. `src/scherzo/workspace_driver_context.gleam` exposes the configured command verbatim, so the example workflow now resolves simple relative driver paths against `SCHERZO_CONFIG_DIR` before invoking `assert-only`.

## Decision Log

- Decision: Treat this as a docs, examples, migration-guide, doc-test, and help-text plan only.
  Rationale: The umbrella has separate child plans for schema, adapter, runtime exposure, research portability, and heavier workflow conversion. Mixing runtime implementation into this documentation cleanup would make review and rollback harder.
  Date: 2026-05-09

- Decision: Create a dedicated migration guide at `docs/runbooks/workspace-driver-migration.md`.
  Rationale: Legacy direct `workspace.hooks` is a breaking migration path, not merely a README footnote. Operators need a stable path that parser errors, doctor output, and public docs can all reference.
  Date: 2026-05-09

- Decision: Use tests to lock the new documentation model.
  Rationale: The existing docs drifted because hook terminology remained valid in prose after the driver design changed. Focused doc tests make stale examples, missing migration guidance, missing script references, and stale help text visible in normal `gleam test` runs.
  Date: 2026-05-09

- Decision: Keep `workspace.hooks` mentions only in legacy and migration contexts.
  Rationale: Operators must be able to search for their old config key and find help, but examples and primary docs should not present direct hooks as a current model.
  Date: 2026-05-09

- Decision: Include `src/scherzo/main.gleam` help text in scope only as user-facing documentation.
  Rationale: `scherzo --help` is documentation that operators see during migration. Updating a string and its test is acceptable here; changing parsing, doctor behavior, or runtime semantics is not.
  Date: 2026-05-09

- Decision: Add a hard prerequisite gate and make failed prerequisites a stop condition.
  Rationale: The review showed the original plan could be followed into future-looking docs even when runtime schema, scripts, diagnostics, and dogfood config were absent. The revised plan must not merge migrated docs or examples unless the current tree can parse and validate them.
  Date: 2026-05-09

- Decision: This plan does not own `.scherzo/scherzo.yaml` migration.
  Rationale: Dogfood runtime config migration changes the repository's own workflow execution behavior, while this plan is a documentation and example cleanup. To avoid disagreement, `.scherzo/scherzo.yaml` must be migrated by prerequisite work before this plan changes `.scherzo/README.md` to driver-profile guidance.
  Date: 2026-05-09

- Decision: Checked public examples will use concrete checked driver command paths once `scripts/scherzo-workspace-jj` and `scripts/scherzo-workspace-noop` exist.
  Rationale: Checked configs must pass workflow-config validation and must not contain absolute local paths, placeholder commands, or dogfood-only environment assumptions. Prose may use placeholders for third-party examples, but runnable checked examples need concrete commands that match the config file location.
  Date: 2026-05-09

- Decision: Document the final checked runtime as a warning-based migration from hooks to drivers, not as an immediate parser rejection of every legacy hook shape.
  Rationale: The hard validation commands passed, but deeper test inspection showed legacy top-level and profile-local hooks are still parseable and doctor emits `legacy_workspace_hooks` warnings with `docs/runbooks/workspace-driver-migration.md` guidance. Public docs should still prefer driver-backed profiles while accurately describing the current runtime and rollback path.
  Date: 2026-05-11

- Decision: Use the checked runtime test names as the prerequisite evidence for this docs migration.
  Rationale: `test/orchestrator_config_test.gleam` has `driver_workspace_profile_parses_schema_test` and `workspace_driver_profiles_resolve_dogfood_jj_shape_test` for driver profile parsing. `test/workflow_dag_test.gleam` has `parses_workspace_capabilities_test` and `rejects_invalid_workspace_capabilities_test` for workflow capability parsing. `test/runtime_bundle_test.gleam` has `rejects_missing_selected_workspace_capabilities_test`, `loads_selected_driver_profile_after_capability_match_test`, `rejects_default_profile_missing_workspace_capabilities_test`, and `dogfood_workflows_select_existing_driver_profile_test` for capability validation and dogfood alignment. `test/workspace_run_test.gleam` has `driver_profile_invokes_lifecycle_create_before_after_and_remove_test` for lifecycle invocation and environment exposure during lifecycle calls. `test/workspace_driver_context_test.gleam` has `env_vars_serialize_workspace_driver_context_test` and `template_locals_expose_workspace_driver_context_test` for command-step environment and template exposure. `test/orchestrator_service_doctor_test.gleam` has `doctor_workspace_hooks_warns_for_top_level_legacy_hooks_test` and `doctor_workspace_hooks_warns_for_profile_local_legacy_hooks_test` for legacy hook migration guidance.
  Date: 2026-05-11

- Decision: Limit public reusable examples to capabilities implemented by the checked driver scripts: `status`, `diff`, `changed-files`, and `assert-only` for `scripts/scherzo-workspace-jj`, and `assert-only` for the public no-op example profile.
  Rationale: The plan's earlier example mentioned future capabilities such as `baseline`, `refresh-base`, and `publish-change`, but workflow-config validation would reject unknown capability names. The contract runbook reserves those names for later plans, so this docs migration must not teach them as available today.
  Date: 2026-05-11

- Decision: Update `docs/runbooks/portable-research-workflow.md` and `docs/runbooks/workspace-driver-contract.md` even though they were not in the original concrete file list.
  Rationale: The stale-term review found public runbook text that still said runtime driver lifecycle invocation was not enabled. Leaving that text would directly contradict the migration guide and README, so the smallest safe fix was to update only those stale transition paragraphs.
  Date: 2026-05-11

- Decision: Fix the relative-driver review finding in documentation and checked examples rather than changing runtime driver exposure.
  Rationale: `SCHERZO_WORKSPACE_DRIVER` currently exposes the trusted configured command verbatim, and changing runtime resolution would broaden this docs plan into production semantics and fingerprint-sensitive behavior. The safer scoped fix is for `examples/scherzo.yaml` to use config-relative `../scripts/...` commands because the checked config lives in `examples/`, and for `examples/workflows/research.yaml` to resolve simple relative driver commands against `SCHERZO_CONFIG_DIR` before invoking driver capabilities from a prepared workspace. Public docs now tell operators to use PATH, absolute, or config-relative driver commands when workflows call capabilities.
  Date: 2026-05-11

- Decision: Do not create the commit requested by the original ExecPlan step sequence.
  Rationale: The active Scherzo workflow contract for LIV-188 explicitly says not to create jj or git commits; the publish step creates the final logical jj commit after validation and review.
  Date: 2026-05-11

## Outcomes & Retrospective

Implementation is complete as of 2026-05-11 17:45Z. The hard prerequisite gate passed with checked executable scripts, runtime/test references for `workspace_capabilities`, `SCHERZO_WORKSPACE_DRIVER`, and migration-guide diagnostics, a passing dogfood workflow-config doctor check, and a passing baseline `gleam test`. The documented final driver names are `scripts/scherzo-workspace-jj`, `scripts/scherzo-workspace-noop`, and the dogfood command `$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj`; the documented workflow-facing environment variables are `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`.

The implementation adjusted the original clean-break wording to match the checked runtime's warning-based transition: legacy hook shapes remain searchable and are intentionally retained in migration guidance, doctor warning tests, and legacy compatibility tests, but primary README, examples, architecture, and help text now teach driver-backed profiles. The new doc tests pass after the docs edits, and `examples/scherzo.yaml` plus `.scherzo/scherzo.yaml` both pass `doctor --check workflow-config` with a dummy Linear key. Final validation passed: `gleam test` reported 1106 passed, format check exited 0, and both lint gates exited 0 with no errors and only the existing warning inventory.

Post-review feedback tightened the reusable example so it is not merely parseable. The checked `examples/scherzo.yaml` now uses driver command paths that are relative to the `examples/` config file, and the checked research workflow resolves those relative commands before calling `assert-only` from inside the prepared workspace. This keeps runtime behavior unchanged while documenting the operator rule: workflows that invoke driver capabilities should use PATH, absolute, or config-relative driver commands that can be resolved from command steps. Post-review validation passed for the example workflow-config doctor check, the targeted format check for touched Gleam tests, and the full Gleam test suite with 1107 passing tests.

## Context and Orientation

Scherzo is a Gleam application that reads an orchestrator YAML config, routes tracker issues to workflow DAGs, prepares per-run workspaces, runs command or pi-agent steps, and reports results. A workflow DAG is a YAML file that declares ordered steps. A workspace is the directory where a step runs. A workspace profile is trusted operator policy that says how those directories are prepared. A workspace driver is a trusted command configured by the operator; Scherzo invokes it for lifecycle operations, and workflows or steps can invoke it for capability operations. A workspace capability is a named operation, such as `assert-only` or `changed-files`, that a workflow can require and a driver can promise to provide.

The current public documentation entry point is `README.md`. Dogfood-specific workflow guidance lives in `.scherzo/README.md`. Reusable example configs and workflows live under `examples/`. Architecture notes live in `docs/ARCHITECTURE.md`. Runbooks live under `docs/runbooks/`. The migration guide to create in this plan is `docs/runbooks/workspace-driver-migration.md`.

The old model used trusted shell snippets called hooks. Named profiles already exist in the current docs and examples, but those profiles still contain inline `hooks`. The new model keeps named profiles but replaces inline hooks with a `driver` block. The workflow still selects a profile with top-level `workspace_profile`; workflows that need operations from the driver declare top-level `workspace_capabilities`.

The expected generic root-level public YAML shape is:

    workspace:
      root: .scherzo/workspaces
      default_profile: isolated
      profiles:
        isolated:
          driver:
            command: scripts/scherzo-workspace-jj
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [status, diff, changed-files, assert-only]
            timeout_ms: 60000
        noop:
          driver:
            command: scripts/scherzo-workspace-noop
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [assert-only]
            timeout_ms: 60000

The checked `examples/scherzo.yaml` file lives under `examples/`, so its final commands are `../scripts/scherzo-workspace-jj` and `../scripts/scherzo-workspace-noop`. A copied config at repository root can use the `scripts/...` commands shown above, while an installed deployment can use PATH or absolute trusted driver commands.

The expected final workflow YAML shape for a portable artifact-only workflow is:

    version: 1
    id: research
    description: Investigate an issue and return a Markdown findings artifact.
    workspace_profile: noop
    workspace_capabilities: [assert-only]
    max_parallel_steps: 1
    steps:
      - id: research
        kind: agent
        prompt: prompts/research.md
        workspace: main
      - id: collect_findings
        kind: command
        depends_on: [research]
        run: |
          set -eu
          driver_command=${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required}
          : "${SCHERZO_CONFIG_DIR:?SCHERZO_CONFIG_DIR is required for relative workspace drivers}"
          case "$driver_command" in
            /*) driver=$driver_command ;;
            */*) driver=$SCHERZO_CONFIG_DIR/$driver_command ;;
            *) driver=$driver_command ;;
          esac
          "$driver" assert-only --path research-findings.md
          cat research-findings.md
        workspace: main

The exact script paths in these examples are now part of the prerequisite gate. Do not replace them with fake commands or local absolute paths. If the adapter plan uses different checked-in script names, update this ExecPlan before continuing.

## Preconditions and Verified Facts

This plan assumes the earlier workspace-driver runtime work has landed before implementation begins. The hard prerequisite gate is the source of truth for what "landed" means: driver-backed profile parsing, workflow-capability parsing, required-capability validation, driver lifecycle invocation, driver environment exposure, legacy-hook migration guidance, checked executable driver scripts, migrated dogfood config, and passing runtime tests. Implementation on 2026-05-11 verified the checked runtime uses doctor warnings for legacy hooks rather than immediate parser rejection, and the documentation now reflects that warning-based transition.

At plan-authoring and review-incorporation time, the current checked tree does not yet present drivers as the primary docs model. `README.md` contains hook-based config examples and a `Workspace hooks` section. `.scherzo/README.md` instructs maintainers to use `scripts/scherzo-jj-workspace` from YAML `workspace.hooks`. `examples/scherzo.yaml` defines named profiles with `hooks`, not `driver`. `.scherzo/scherzo.yaml` uses direct `workspace.hooks` and calls `scripts/scherzo-jj-workspace`. `docs/ARCHITECTURE.md` describes workspace hook profiles. `scripts/scherzo-jj-workspace` exists and supports the old lifecycle helper commands `after-create`, `before-run`, and `before-remove`.

The current repository has doc-oriented tests that can be extended. `test/test_suite_contract_test.gleam` reads `README.md`. `test/workflow_portability_test.gleam` reads `.scherzo/README.md`. `test/main_test.gleam` checks the `usage` string returned by `src/scherzo/main.gleam`. There is no `docs/runbooks/workspace-driver-migration.md` in the current runbook listing, so this plan creates it after the prerequisite gate passes.

The standard validation commands for production-quality changes are run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. Treat that as environment setup, not a code or test failure.

## Scope Boundaries

In scope after the hard prerequisite gate passes: update `README.md`; update `.scherzo/README.md`; update `docs/ARCHITECTURE.md`; create `docs/runbooks/workspace-driver-migration.md`; update `examples/scherzo.yaml`; update comments or capability declarations in `examples/workflows/research.yaml`, `examples/workflows/implementation.yaml`, and `examples/workflows/pr-conflict-repair.yaml` only if the runtime schema requires those examples to name `workspace_profile` or `workspace_capabilities`; update `src/scherzo/main.gleam` usage text and `test/main_test.gleam` only to remove stale help text; add or update documentation tests under `test/`.

Out of scope: changing config parsing, changing workflow DAG parsing, changing workspace lifecycle execution, adding or editing driver scripts, changing pi launch environment behavior, changing execution fingerprints, changing Linear transport, changing dogfood workflow runtime semantics, or editing `.scherzo/scherzo.yaml`. If `.scherzo/scherzo.yaml` has not already been migrated by the time this plan is implemented, stop at the hard prerequisite gate. Do not make `.scherzo/README.md` describe a driver profile that the checked-in dogfood config does not contain.

The reusable example migration in `examples/scherzo.yaml` is in scope because examples are part of the documentation surface and are validated by `doctor --check workflow-config`. The dogfood runtime config migration is out of scope because it changes the repository's own workflow execution setup and must be delivered by prerequisite runtime or dogfood migration work.

The migration guide must be portable. Do not include absolute local paths. Use config-relative or repository-relative paths where appropriate, or placeholders such as `<repo-root>` and `<absolute-local-path>` when warning about path shapes.

## Milestones

Milestone 0 is the hard prerequisite gate. At the end of this milestone, the implementer has either recorded that runtime schema, scripts, dogfood config, and runtime tests are present and passing, or has updated the living-document sections and stopped. This comes first because no docs edit is safe until the current binary can parse and validate the driver examples.

Milestone 1 adds failing doc tests. At the end, the implementer has verified the actual driver schema names in the current tree and has tests that fail because the docs still use old hook-oriented wording or because the migration guide is not yet present. These failures must be documentation failures only; missing runtime scripts or runtime parsing failures mean Milestone 0 was not satisfied.

Milestone 2 updates the primary public docs. At the end, `README.md` and `docs/ARCHITECTURE.md` explain workspace profiles, drivers, capabilities, selected-profile validation, trusted driver configuration, and the legacy migration path with the same vocabulary.

Milestone 3 creates the migration guide. At the end, `docs/runbooks/workspace-driver-migration.md` contains before-and-after examples for direct hooks, named hook profiles, no-op or artifact-only workflows, and dogfood jj profiles; it also contains validation, troubleshooting, rollback, release-order guidance, and a note that `examples/scherzo.yaml` is the canonical runnable sample.

Milestone 4 updates examples and dogfood docs. At the end, `examples/scherzo.yaml` and relevant example workflow files use the current driver-backed profile shape, while `.scherzo/README.md` explains the already-migrated dogfood profile and the commands maintainers should run to validate it.

Milestone 5 updates help text and finishes validation. At the end, `scherzo --help` no longer presents hooks as a current required input, doc tests pass, workflow-config checks pass for both checked-in dogfood and example configs, and a stale-term search shows that old hook terms remain only in migration or legacy-warning contexts.

## Plan of Work

Start with Milestone 0. Run the hard prerequisite commands exactly as written. Then inspect the runtime tests that satisfied the gate and record their actual file names and test names in the Decision Log. If any expected runtime artifact uses a different accepted name than this plan, update the Hard Prerequisite Gate, Context and Orientation, planned tests, examples, and Open Questions before writing docs. No later step should require the implementer to choose schema names, environment variable names, script paths, or dogfood config ownership.

Add a new test module `test/workspace_driver_docs_test.gleam`. It should define small helpers to read text files, assert that a string contains an expected substring, assert that a string does not contain an unexpected substring, and assert that an expected script file exists. If the Gleam standard or project dependencies provide an executable-bit check, use it for the script tests; otherwise use the shell `test -x` command in Concrete Steps as the executable-bit validation and make the Gleam test check file presence only. Add these tests:

- `readme_documents_workspace_driver_model_test`: read `README.md` and assert that it contains `Workspace profiles and drivers`, `workspace.profiles.<name>.driver.command`, `workspace_capabilities`, `SCHERZO_WORKSPACE_DRIVER`, `SCHERZO_WORKSPACE_CAPABILITIES`, `legacy workspace.hooks`, and `docs/runbooks/workspace-driver-migration.md`.
- `migration_guide_is_actionable_test`: read `docs/runbooks/workspace-driver-migration.md` and assert that it contains `Before`, `After`, `workspace.hooks`, `driver:`, `capabilities:`, `direnv exec . gleam run -- doctor --check workflow-config`, `direnv exec . gleam test`, `Rollback`, and `Troubleshooting`.
- `examples_use_driver_profiles_test`: read `examples/scherzo.yaml` and assert that it contains `driver:`, `command: ../scripts/scherzo-workspace-jj`, `command: ../scripts/scherzo-workspace-noop`, `lifecycle:`, and `capabilities:`; assert that it does not contain a profile-level `hooks:` block such as an indented `hooks:` immediately under a profile.
- `research_workflow_resolves_relative_driver_test`: read `examples/workflows/research.yaml` and assert that the command step resolves `SCHERZO_WORKSPACE_DRIVER` through `SCHERZO_CONFIG_DIR` before invoking `assert-only`, rather than directly executing `"$SCHERZO_WORKSPACE_DRIVER"` from the prepared workspace.
- `driver_scripts_are_present_test`: assert that `scripts/scherzo-workspace-jj` and `scripts/scherzo-workspace-noop` exist in the checked tree.
- `dogfood_readme_documents_driver_profile_test`: read `.scherzo/README.md` and assert that it contains `workspace.profiles`, `workspace driver`, `scripts/scherzo-workspace-jj`, and `doctor --check workflow-config`.
- `architecture_uses_driver_vocabulary_test`: read `docs/ARCHITECTURE.md` and assert that it contains `workspace driver` and `workspace capability`; assert that it does not contain `workspace hook profiles`.

If the final runtime implementation uses names different from the strings above, update these test strings first and record the reason in the Decision Log. Run `direnv exec . gleam test` and confirm the new tests fail for documentation reasons before making the docs edits.

Update `README.md`. In the orchestrator config section, change the description from "workspace hooks" to "workspace profiles, drivers, and capabilities". Replace hook-based YAML examples with driver-backed profile examples that use config-appropriate checked driver commands such as `scripts/scherzo-workspace-jj`, `scripts/scherzo-workspace-noop`, or the `../scripts/...` paths needed by `examples/scherzo.yaml` because it lives under `examples/`. Keep one short paragraph that says legacy direct `workspace.hooks` is no longer the current config shape and points to `docs/runbooks/workspace-driver-migration.md`. Rename the `Workspace hooks` section to `Workspace profiles and drivers`. Define profile, driver, lifecycle operation, and capability in plain language. Explain that workflows may select `workspace_profile` and declare `workspace_capabilities`, but they may not define trusted driver commands. Document the relevant environment variables exposed to command steps: `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES`. Show a small command-step example that resolves a relative `SCHERZO_WORKSPACE_DRIVER` value against `SCHERZO_CONFIG_DIR` before invoking `assert-only`. Do not include any machine-specific absolute path.

Update `docs/ARCHITECTURE.md`. In the workflow DAGs and execution invariants, replace the hook-owned workspace wording with driver-owned wording. State that the runtime loads trusted workspace profiles from the orchestrator config, validates workflow-required capabilities before dispatch, and prepares workspaces through driver lifecycle operations. Mention that direct `workspace.hooks` is a legacy config shape handled by migration diagnostics, not a current architecture invariant. Do not expand this into a full design document; keep it as an architecture invariant summary.

Create `docs/runbooks/workspace-driver-migration.md`. Use these headings: `# Migrating from workspace hooks to workspace drivers`, `## Who needs this`, `## What changed`, `## Before and after: direct hooks`, `## Before and after: named hook profiles`, `## Choosing capabilities`, `## No-op or artifact-only workflows`, `## Dogfood jj profile`, `## Validation`, `## Troubleshooting`, and `## Rollback`. The guide must include a before example with `workspace.hooks`, an after example with `workspace.default_profile` and `workspace.profiles.<name>.driver`, and a no-op or artifact-only profile that provides only `assert-only`. It must explain that driver commands are trusted operator config, not workflow-defined shell. It must state that `examples/scherzo.yaml` is the canonical runnable checked example and that snippets in the migration guide should stay aligned with it. It must tell operators to run:

    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
    direnv exec . gleam test

The guide must include rollback advice: keep the old config and previous Scherzo version together; if a future version rejects `workspace.hooks`, either finish the migration or roll back both the binary and config; do not mix a future driver-only binary with old direct-hook config.

Update `examples/scherzo.yaml`. Replace profile `hooks` blocks with driver-backed profiles using `../scripts/scherzo-workspace-jj` for the isolated profile and `../scripts/scherzo-workspace-noop` for the `noop` artifact-only profile because the checked config lives under `examples/`. The `noop` profile must provide `assert-only` because the portable research workflow uses that capability. Do not use `<driver-command>` or `$SCHERZO_REPO_ROOT` in this checked file. If these scripts are missing or doctor cannot validate the file, stop because the hard prerequisite gate was not satisfied.

Update `examples/workflows/research.yaml` only if needed to reflect the final portable research workflow contract. At minimum, if the workflow uses a driver capability, it must declare `workspace_capabilities: [assert-only]` and use the trusted command from `SCHERZO_WORKSPACE_DRIVER` rather than hardcoded VCS commands for capability operations. Because command steps run inside the prepared workspace and Scherzo exposes the driver command verbatim, resolve simple relative driver paths against `SCHERZO_CONFIG_DIR` before invoking them.

Review `examples/workflows/implementation.yaml` and `examples/workflows/pr-conflict-repair.yaml`. Update only stale comments or profile/capability metadata required by the final schema. Do not redesign these workflows here.

Update `.scherzo/README.md` only after `.scherzo/scherzo.yaml` has already passed the dogfood workflow-config check with a driver-backed profile. Replace the instruction to call `scripts/scherzo-jj-workspace` from `workspace.hooks` with the final dogfood driver profile instructions. Name the actual dogfood driver script, expected to be `scripts/scherzo-workspace-jj` after the prerequisite gate. Explain which environment variables remain relevant to dogfood operation, such as `SCHERZO_REPO_ROOT`, `SCHERZO_PR_REMOTE`, and `SCHERZO_PR_BASE`, only if the migrated dogfood config or script actually uses them. Keep the existing vendored pi skill guidance. Update the validation snippet so it includes the dogfood workflow-config doctor check and `direnv exec . gleam test`.

Update `src/scherzo/main.gleam` only if its `usage` string still describes `workspace.hooks` as a current required input. Change that sentence to say that Scherzo requires a YAML orchestrator config, YAML workflow DAG files, and workspace profiles with drivers that can prepare each step workspace. Update `test/main_test.gleam` to assert the new wording. Do not change command-line parsing, doctor check names, or behavior.

After editing, run a stale-term search over current public docs and examples. Review every remaining occurrence of `workspace.hooks`, `Workspace hooks`, `workspace hook profiles`, `scripts/scherzo-jj-workspace`, and `hooks:`. Remaining occurrences are acceptable only in `docs/runbooks/workspace-driver-migration.md`, in explicit legacy warnings that link to that runbook, or in source code/tests that still exercise migration diagnostics. If a remaining occurrence presents hooks as current guidance, update it before continuing.

## Concrete Steps

1. From the repository root, confirm source-control state:

       jj status --color=never

   Expected result before implementation is either a clean working copy or only intentional changes from earlier workspace-driver runtime plans. Do not create, switch, finish, forget, or otherwise manage workspaces.

2. From the repository root, run the hard prerequisite gate:

       test -x scripts/scherzo-workspace-jj
       test -x scripts/scherzo-workspace-noop
       grep -R "workspace_capabilities" -n src test
       grep -R "SCHERZO_WORKSPACE_DRIVER" -n src test
       grep -R "docs/runbooks/workspace-driver-migration.md" -n src test
       LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
       direnv exec . gleam test

   Expected result: every command exits 0; grep output includes runtime code and tests, not only documentation; the dogfood workflow-config check succeeds; and `gleam test` reports all tests passed. If any command fails because runtime driver work, scripts, migration diagnostics, or dogfood config are missing, update this ExecPlan and stop. Do not proceed to doc edits.

3. Inspect the runtime tests that satisfied the gate and record the actual evidence in the Decision Log. Include the files and test names that prove driver-profile parsing, legacy-hook migration guidance, workflow-capability parsing, capability validation, driver lifecycle invocation, and driver environment exposure. If the final runtime names differ from this plan, update all planned doc/test strings before continuing.

4. Create `test/workspace_driver_docs_test.gleam` with the tests named in the Plan of Work. Use `simplifile.read` to read files. Make assertion failures include the missing substring and file path. Include the script-presence test for `scripts/scherzo-workspace-jj` and `scripts/scherzo-workspace-noop`.

5. Run:

       direnv exec . gleam test

   Expected result before docs edits: the new `workspace_driver_docs_test` tests fail because the migration guide does not exist and current docs still contain hook-oriented examples. No failure should mention missing runtime schema, missing driver scripts, or a failing dogfood workflow-config check; those belong to the prerequisite gate.

6. Edit `README.md` as described in the Plan of Work. Keep existing repository conventions and workflow sections, but replace hook-shaped config examples with driver-shaped examples and add the migration-guide link.

7. Edit `docs/ARCHITECTURE.md` so the workflow DAG and execution invariants use workspace driver and workspace capability vocabulary.

8. Create `docs/runbooks/workspace-driver-migration.md` with the required migration sections, examples, validation commands, troubleshooting, rollback notes, and canonical-example guidance.

9. Edit `examples/scherzo.yaml` to use driver-backed profiles with config-relative `../scripts/scherzo-workspace-jj` and `../scripts/scherzo-workspace-noop` commands because the checked example config lives under `examples/`.

10. Edit `examples/workflows/research.yaml` only if needed to declare `workspace_capabilities: [assert-only]` or to use the trusted `SCHERZO_WORKSPACE_DRIVER` command for capability operations. If the example uses a relative driver command, resolve it against `SCHERZO_CONFIG_DIR` before invoking it from the prepared workspace.

11. Review `examples/workflows/implementation.yaml` and `examples/workflows/pr-conflict-repair.yaml`. Update only stale comments or profile/capability metadata required by the final schema.

12. Edit `.scherzo/README.md` to describe the already-migrated dogfood driver profile and validation commands. If `.scherzo/scherzo.yaml` is not already migrated, this step must not run; stop and update the plan instead.

13. If needed, edit `src/scherzo/main.gleam` usage text and `test/main_test.gleam` to replace current-hook wording with driver-profile wording.

14. Run the documentation stale-term review:

       grep -R "workspace.hooks\|Workspace hooks\|workspace hook profiles\|scripts/scherzo-jj-workspace\|hooks:" -n README.md .scherzo/README.md docs/ARCHITECTURE.md docs/runbooks examples src/scherzo/main.gleam test

   Expected result: occurrences are limited to the migration guide, explicit legacy warnings that point to the migration guide, migration-diagnostic tests, or old-helper references deliberately marked as legacy. Any current-guidance occurrence must be edited before continuing.

15. Run workflow-config checks:

       LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
       LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml

   Expected result: both commands exit 0 and report the workflow-config check as successful. There is no skip path. If either command fails, fix the docs/examples if they are wrong; if the runtime or dogfood prerequisite is missing, stop and do not merge the docs commit.

16. Run full validation:

       test -x scripts/scherzo-workspace-jj
       test -x scripts/scherzo-workspace-noop
       direnv exec . gleam test
       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expected result: all commands exit 0. If glinter or `scherzo_lint` reports pre-existing warnings, do not do unrelated cleanup; fix only warnings introduced by help-text or test changes.

17. Update the Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections of this ExecPlan with what changed, the prerequisite evidence, and the validation output summary.

18. Commit the completed docs migration as one logical commit after validation passes. A suitable commit message is `docs: document workspace driver migration`.

## Testing and Falsifiability

The hard prerequisite gate is the first falsifiability mechanism. The plan is false, or at least not ready to execute, if the runtime cannot parse driver-backed profiles, cannot parse `workspace_capabilities`, lacks driver environment exposure, lacks migration guidance for `workspace.hooks`, lacks the checked driver scripts, or has an unmigrated `.scherzo/scherzo.yaml`. A prerequisite failure is not a warning that can be accepted before merge; it is a stop condition.

The new doc tests are the primary documentation falsifiability mechanism. `readme_documents_workspace_driver_model_test` proves the public README contains the new model, the legacy warning, and the migration link. It fails before implementation if `README.md` still has the old `Workspace hooks` section and no driver environment documentation. It passes after the README teaches profiles, drivers, capabilities, and migration.

`migration_guide_is_actionable_test` proves the migration guide exists and contains the minimum content an operator needs: before-and-after examples, capability guidance, validation commands, rollback, troubleshooting, and alignment with the canonical runnable example. It fails before implementation because `docs/runbooks/workspace-driver-migration.md` does not exist.

`examples_use_driver_profiles_test` proves the reusable example config no longer teaches profile-level hooks and uses the pinned checked driver scripts. It fails before implementation because `examples/scherzo.yaml` currently contains `hooks` blocks under named profiles. It passes only when the example uses `driver`, `command`, `lifecycle`, and `capabilities` fields and no longer has a profile-level `hooks:` block.

`driver_scripts_are_present_test`, plus the shell `test -x` validation, protects code snippets and command paths from drifting toward nonexistent or non-executable commands. This prevents the docs from naming `scripts/scherzo-workspace-jj` or `scripts/scherzo-workspace-noop` unless those paths are actually checked in.

`dogfood_readme_documents_driver_profile_test` proves maintainers have dogfood-specific instructions for the final driver profile and validation commands. It should run only after the dogfood-alignment gate passes. It fails before implementation if `.scherzo/README.md` still tells maintainers to use the old jj hook helper from `workspace.hooks`.

`architecture_uses_driver_vocabulary_test` proves the architecture summary no longer describes hook profiles as the current model. It fails before implementation because `docs/ARCHITECTURE.md` currently contains `workspace hook profiles`.

The workflow-config checks prove the examples are not just prose. `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml` must succeed after `examples/scherzo.yaml` is migrated. `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml` must also succeed because dogfood README guidance must match checked dogfood config. If either check fails because the driver command path is missing or the schema differs from the docs, the plan is wrong and must be corrected before merging.

The stale-term search is a manual falsifiability check. The plan is false if a public doc or example still presents `workspace.hooks`, `Workspace hooks`, or `hooks:` as current configuration rather than migration context. It is acceptable for the migration guide and migration tests to mention old terms because operators need searchable old-key guidance.

The migration-guide snippets should stay aligned with `examples/scherzo.yaml`. If the repository later gains a snippet-extraction test, add this guide to it. Until then, the checked `examples/scherzo.yaml` is the canonical runnable sample, and prose snippets are illustrative examples that must use the same field names and command strategy.

## Validation and Acceptance

Acceptance requires these observable outcomes with no prerequisite-gap exceptions:

- The hard prerequisite gate passes before documentation edits begin, and the Decision Log records the final runtime files, symbols, scripts, and tests used as evidence.
- `README.md` explains workspace profiles, workspace drivers, lifecycle operations, workspace capabilities, `workspace_profile`, `workspace_capabilities`, `SCHERZO_WORKSPACE_DRIVER`, and the migration guide.
- `docs/runbooks/workspace-driver-migration.md` exists and gives direct-hook, named-hook-profile, no-op or artifact-only, and dogfood jj migration examples with validation and rollback notes.
- `examples/scherzo.yaml` uses driver-backed profiles with checked config-relative driver commands and passes the workflow-config doctor check.
- `.scherzo/README.md` describes the already-migrated dogfood driver profile, and `.scherzo/scherzo.yaml` passes the workflow-config doctor check.
- `docs/ARCHITECTURE.md` uses driver and capability vocabulary in the workflow execution invariants.
- `scherzo --help`, as represented by `src/scherzo/main.gleam` and `test/main_test.gleam`, no longer teaches `workspace.hooks` as a current required input.
- The new doc tests fail before the docs are edited for documentation reasons and pass after the docs are edited.
- `test -x scripts/scherzo-workspace-jj` and `test -x scripts/scherzo-workspace-noop` pass.
- The stale-term search leaves old hook terms only in migration or explicitly legacy contexts.
- `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` exit 0.

## Rollout, Recovery, and Idempotence

Roll this docs change with the runtime driver migration, not before it. The enforceable rollout gate is the Hard Prerequisite Gate plus the two workflow-config doctor checks. If the runtime release that accepts driver-backed profiles and surfaces legacy-hook migration guidance has not landed, keep this branch unmerged. Do not publish public docs or checked examples that tell users to configure driver fields that the current binary cannot parse.

Recovery is simple because this is a documentation and example cleanup after runtime migration. If the docs are wrong after merge, revert the docs commit or apply a follow-up docs correction. If `examples/scherzo.yaml` was migrated too early and breaks example validation, revert `examples/scherzo.yaml` and any matching example workflow metadata until the runtime implementation is present.

Operator rollback must be described in the migration guide. A user who has not migrated should keep the old Scherzo version and old config together. A user who has migrated can roll back by restoring the old config and old binary together. The guide must explicitly warn that a future driver-only binary should not be mixed with old direct `workspace.hooks` config.

The work is idempotent. Re-running the prerequisite gate, doc tests, workflow-config checks, stale-term search, and full validation should produce the same results. Re-applying the migration guide edits should not create duplicate sections. The only generated or runtime state touched by validation should be normal test and build artifacts.

## Open Questions and Clarifications Needed

- Resolved 2026-05-11: The Hard Prerequisite Gate and final validation passed in this workspace. Evidence is recorded in Progress, Decision Log, and Artifacts and Notes.
- Resolved 2026-05-11: The accepted runtime implementation uses the planned field names and environment variable names. The meaningful behavioral difference is that legacy hook shapes produce doctor migration warnings instead of immediate parser rejection, and the currently implemented public capabilities exclude future names such as `baseline`, `refresh-base`, and `publish-change`.
- Resolved 2026-05-11: Checked reusable examples use config-relative driver command strings `../scripts/scherzo-workspace-jj` and `../scripts/scherzo-workspace-noop` because `examples/scherzo.yaml` lives under `examples/`, and `doctor --check workflow-config examples/scherzo.yaml` passes. The migration guide notes that copied configs in another repository must place the trusted script at the configured relative path, install it on `PATH`, or update `driver.command` to that repository's trusted script path.

## Artifacts and Notes

Final validation evidence from 2026-05-11:

    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
    Summary: 1 passed, 0 warnings, 0 failed, 0 skipped

    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
    Summary: 1 passed, 0 warnings, 0 failed, 0 skipped

    direnv exec . gleam test
    1106 passed, no failures

    direnv exec . gleam format --check src test
    exited 0

    direnv exec . gleam run -m glinter
    Found 363 issues (0 errors, 277 warnings)

    direnv exec . gleam run -m scherzo_lint
    Found 363 issues (0 errors, 277 warnings)

Post-review validation evidence from 2026-05-11:

    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config examples/scherzo.yaml
    Summary: 1 passed, 0 warnings, 0 failed, 0 skipped

    direnv exec . gleam format --check test/workspace_driver_docs_test.gleam test/portable_research_workflow_test.gleam
    exited 0

    direnv exec . gleam test
    1107 passed, no failures

The current old dogfood lifecycle helper is `scripts/scherzo-jj-workspace`. It accepts lifecycle-style commands named `after-create`, `before-run`, and `before-remove`. The migration guide should mention it only when explaining how old dogfood hook config maps to the new driver-backed profile. It should not present that script as the new driver command unless the adapter plan deliberately kept it as the final command and this ExecPlan has been updated accordingly.

The documentation examples should avoid absolute local paths. Checked examples use config-relative command paths such as `../scripts/scherzo-workspace-noop` only after the prerequisite gate proves the target scripts are executable and valid for config loading. Prose for third-party repositories may use `<driver-command>` or `<repo-root>` placeholders when explaining that operators must supply their own trusted command. Do not write examples containing local prefixes such as a developer home directory.

The stale-term search is intentionally not a hard zero-match rule. These old terms should remain searchable in migration contexts:

    workspace.hooks
    hooks:
    scripts/scherzo-jj-workspace

But these old terms should not be used as current headings or architecture labels after implementation:

    Workspace hooks
    workspace hook profiles
    hook-owned workspaces

## Interfaces and Dependencies

This plan depends on the final workspace-driver runtime surface. The documentation should describe these external config interfaces if they are the implemented names recorded after the hard prerequisite gate:

    workspace:
      default_profile: <profile-name>
      profiles:
        <profile-name>:
          driver:
            command: scripts/scherzo-workspace-noop
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [assert-only]
            timeout_ms: 60000

    workspace_profile: <profile-name>
    workspace_capabilities: [assert-only]

The documentation should describe these command-step environment variables if they are the implemented names recorded after the hard prerequisite gate:

    SCHERZO_WORKSPACE_PROFILE
    SCHERZO_WORKSPACE_DRIVER
    SCHERZO_WORKSPACE_CAPABILITIES

The concrete files expected to change during implementation are:

- `README.md` for public setup, config examples, workflow YAML examples, and workspace driver concepts.
- `.scherzo/README.md` for repository dogfood conventions and validation, only after `.scherzo/scherzo.yaml` is already migrated and validated.
- `docs/ARCHITECTURE.md` for architecture vocabulary.
- `docs/runbooks/workspace-driver-migration.md` as a new migration guide.
- `examples/scherzo.yaml` for reusable driver-backed config examples.
- `examples/workflows/research.yaml` if the example needs `workspace_capabilities` or `$SCHERZO_WORKSPACE_DRIVER` collection behavior.
- `examples/workflows/implementation.yaml` and `examples/workflows/pr-conflict-repair.yaml` only for stale comments or required profile/capability metadata.
- `src/scherzo/main.gleam` for `scherzo --help` wording only.
- `test/main_test.gleam` for the help-text assertion.
- `test/workspace_driver_docs_test.gleam` as the new documentation test module.

The concrete files that must not be changed by this plan are runtime implementation modules and runtime config such as `src/scherzo/config.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/workspace_profile.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/workflow_run.gleam`, driver scripts under `scripts/`, and `.scherzo/scherzo.yaml`, unless the operator explicitly re-scopes this plan and the Decision Log is updated before work continues.
