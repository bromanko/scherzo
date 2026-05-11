# Migrate dogfood workflows to driver-backed workspace profiles

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

Implementation status: this plan may be merged as a **blocked planning artifact**, but it is not implementation-ready. The current target branch already has the core schema plan at `docs/plans/LIV-170-workspace-driver-schema-migration-diagnostics.md` and the adapter-contract plan at `docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md`; those are no longer open clarifications. As of the 2026-05-11 LIV-184 implementation attempt, the jj workspace driver adapter exists, but execution remains blocked until selected driver-backed profiles are dispatchable through real lifecycle invocation and the final runtime symbols/tests are verified in the implementation tree. Do not dispatch implementation from this plan while selected driver-backed profiles still fail with `workspace_driver_invocation_unavailable`.

## Purpose / Big Picture

After this change, the checked-in Scherzo dogfood configuration will keep the existing explicit workspace profile name `dogfood-jj`, but the profile implementation will move from profile-local `hooks` to a typed `driver`. The dogfood workflows already select `workspace_profile: dogfood-jj`; after this migration, that selector will resolve to a driver-backed profile whose lifecycle behavior is equivalent to the current jj hook helper.

The observable result is that `.scherzo/scherzo.yaml` contains `workspace.default_profile: dogfood-jj` and `workspace.profiles.dogfood-jj.driver`, does not contain `workspace.profiles.dogfood-jj.hooks` or top-level `workspace.hooks`, and `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml` exits successfully without legacy-hook or driver-invocation diagnostics. The seven checked-in dogfood workflow YAML files under `.scherzo/workflows/` continue to contain top-level `workspace_profile: dogfood-jj`, and the local jj workspace smoke coverage proves that driver lifecycle setup still reuses and cleans up the same workspace shape.

## Problem Framing and Constraints

Operators dogfood Scherzo with workflows stored in `.scherzo/workflows/` and runtime config stored in `.scherzo/scherzo.yaml`. The current target-branch dogfood config no longer uses top-level direct `workspace.hooks`; LIV-168 already completed the interim migration to an explicit named profile. The remaining legacy shape is profile-local: `.scherzo/scherzo.yaml` defines `workspace.profiles.dogfood-jj.hooks`, and those hook snippets call `scripts/scherzo-jj-workspace` to create, verify, and remove jj workspaces.

That interim state is better than the old synthetic `default` profile, but it still teaches maintainers to copy hook snippets instead of the driver-backed profile model defined by `docs/plans/workspace-profile-helper-capabilities-umbrella.md` and implemented in stages by the child plans. The operator pain is that dogfood config is the repository's most visible real configuration. If it stays hook-backed after driver invocation is available, future Scherzo contributors will keep exercising the legacy path and miss driver-profile validation problems.

This plan is intentionally narrow. It migrates the existing named dogfood profile from `hooks` to `driver` and keeps the workflow/profile boundary explicit. It does not implement the core schema, the driver command adapters, runtime driver lifecycle invocation, command-step driver environment variables, prompt locals, portable research wording, or generic publish abstractions. Those are covered by the prerequisite and sibling plans named in this document.

## Strategy Overview

Use the existing profile name `dogfood-jj`. The current checked-in workflows already select that name, so the implementation should not remove or rename workflow selectors. The migration should replace only the profile implementation in `.scherzo/scherzo.yaml`, update `.scherzo/README.md` to describe the new convention, and add tests that prove the dogfood bundle loads through the driver-backed profile path.

The target driver command is `scripts/scherzo-workspace-jj`, from `docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md`. The driver profile should declare lifecycle support for `create`, `before-step`, `after-step`, and `remove`. For capabilities, this plan should advertise only operations implemented by the initial jj adapter slice: `status`, `diff`, `changed-files`, and `assert-only`. The larger `baseline`, `refresh-base`, and `publish-change` operations are reserved by the umbrella and are handled by the later implementation-workflow conversion work in `docs/plans/LIV-175-convert-dogfood-implementation-workflows-to-driver-operations.md`; do not advertise them in the dogfood profile until that driver support exists.

This is the right size because it removes the remaining legacy hook implementation from dogfood without rewriting workflow semantics. The prompts and helper scripts may still mention jj after this plan, because they remain repository-specific dogfood workflows. Capability requirements belong in workflow YAML only when the workflow steps actually call `$SCHERZO_WORKSPACE_DRIVER`; this profile migration does not add `workspace_capabilities` to dogfood workflows.

## Alternatives Considered

The smallest possible alternative is to leave `.scherzo/scherzo.yaml` on `workspace.profiles.dogfood-jj.hooks` until all prompts and command scripts are made VCS-neutral. That is insufficient once runtime driver invocation exists, because dogfood would continue exercising the legacy hook path and would not validate the repository's real workflows against the driver-profile loader.

Another alternative is to define the driver command directly in each workflow YAML file. That is rejected because workflow YAML may be copied from third parties and should not contain trusted shell commands. Workflows should select a named profile and optionally declare required capabilities; operators should configure driver commands in `.scherzo/scherzo.yaml`.

A third alternative is to rely only on `workspace.default_profile: dogfood-jj` and remove `workspace_profile` from workflow files. That would work for ad-hoc workflows, but it would make workflow intent implicit and make future default-profile changes a hidden behavior change. This plan preserves explicit selectors in every checked-in dogfood workflow.

A fourth alternative is to advertise the full umbrella capability set immediately. That is rejected. `baseline`, `refresh-base`, and `publish-change` involve base identity, rebasing, remote publication, and hosted review policy. The initial adapter plan deliberately defers them, so advertising them here would overclaim profile support.

A fifth alternative is to proceed before driver lifecycle invocation is available and adapt the runtime during this implementation. That is rejected. This child plan is a config migration, not the runtime driver-invocation plan. If selected driver-backed profiles still fail with `workspace_driver_invocation_unavailable`, complete the missing runtime prerequisite first.

## Risks and Countermeasures

The main sequencing risk is applying dogfood config before runtime driver invocation exists. Countermeasure: the first implementation milestone runs explicit prerequisite checks. If `scripts/scherzo-workspace-jj` is missing, if selected driver profiles still fail with `workspace_driver_invocation_unavailable`, or if the jj driver lifecycle smoke does not exist, stop and complete the prerequisite adapter or runtime invocation work before editing `.scherzo/scherzo.yaml`.

The main runtime risk is breaking all dogfood workflows by pointing them at a driver command whose lifecycle behavior differs from the current profile-local hooks. Countermeasure: keep lifecycle parity with the current hooks: create the jj workspace, run the old after-create behavior, verify before each step, no-op after each step, and forget run workspaces during cleanup. Update or extend the existing jj workspace smoke test so it exercises the driver-backed profile path.

The main validation risk is declaring workflow capabilities that workflows do not actually invoke. Countermeasure: do not add `workspace_capabilities` in this plan. The profile may advertise driver capabilities, but workflows declare requirements only when their command steps or prompts actually use driver operations. The portable research and heavier dogfood conversion plans own those declarations.

The main operational rollout risk is applying `.scherzo/scherzo.yaml` while a dogfood daemon is actively dispatching or recovering runs created under the hook-backed profile. Countermeasure: pause dispatch with `scripts/scherzoctl pause --json`, inspect active sessions with `scripts/scherzoctl ps --json`, wait for or explicitly stop active runs before switching config, run the workflow-config doctor check before daemon reload, then resume only after reload accepts the new config.

The main documentation risk is letting `.scherzo/README.md` disagree with `.scherzo/scherzo.yaml`. Countermeasure: update `.scherzo/README.md` in the same implementation commit to describe `workspace.profiles.dogfood-jj.driver`, `scripts/scherzo-workspace-jj`, and the fact that hook-backed dogfood profiles are legacy.

Rollback is straightforward only while the runtime still accepts hook-backed profiles: restore the previous `.scherzo/scherzo.yaml` `workspace.profiles.dogfood-jj.hooks` block and restore the previous `.scherzo/README.md` wording. If a later release hard-rejects hooks, rollback requires pinning the previous Scherzo version or applying `docs/runbooks/workspace-driver-migration.md` rather than expecting the new binary to load old hooks.

## Progress

- [x] (2026-05-09 00:00Z) Drafted this child ExecPlan from the workspace-driver umbrella and the then-current dogfood repository state.
- [x] (2026-05-09 00:00Z) Incorporated adversarial review feedback by marking the plan blocked until prerequisite driver contracts are frozen, adding concrete prerequisite and rollout checks, and preserving material clarification items.
- [x] (2026-05-10 00:00Z) Revised after follow-up review: replaced open prerequisite-plan clarifications with the checked-in LIV-170 and LIV-171 paths, corrected the current dogfood state from top-level direct hooks to profile-local `dogfood-jj.hooks`, treated LIV-168 as completed interim history, aligned capabilities with the initial LIV-171 adapter scope, and included the scheduled `github-pr-conflict-scout` workflow in the dogfood selector inventory.
- [x] (2026-05-11 02:39Z) Ran the prerequisite gate in the LIV-184 implementation workspace. `scripts/scherzo-workspace-jj` exists and `scripts/scherzo-workspace-jj lifecycle after-step` exits 0, but selected driver-only profiles still fail with `workspace_driver_invocation_unavailable`, so implementation stopped before dogfood config edits.
- [ ] Wait for runtime driver lifecycle invocation to land, then rerun the prerequisite gate before editing dogfood config.
- [ ] Replace `workspace.profiles.dogfood-jj.hooks` with `workspace.profiles.dogfood-jj.driver` in `.scherzo/scherzo.yaml`.
- [ ] Verify all seven checked-in dogfood workflow YAML files still select `workspace_profile: dogfood-jj`.
- [ ] Update dogfood README guidance to describe driver-backed profiles instead of hook-backed profiles.
- [ ] Add or update tests that prove dogfood config and workflows load through the driver-backed profile path.
- [ ] Run validation commands and record the results in Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: The dogfood config has already completed the interim named-profile migration.
  Evidence: On the target branch, `.scherzo/scherzo.yaml` defines `workspace.default_profile: dogfood-jj` and `workspace.profiles.dogfood-jj.hooks`, not top-level `workspace.hooks`.

- Observation: The checked-in dogfood workflow files already select the `dogfood-jj` profile.
  Evidence: `.scherzo/workflows/research.yaml`, `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, `.scherzo/workflows/merge-conflict-resolution.yaml`, and `.scherzo/workflows/github-pr-conflict-scout.yaml` contain top-level `workspace_profile: dogfood-jj`.

- Observation: The core driver schema plan is already checked in and implemented as an additive transition.
  Evidence: `docs/plans/LIV-170-workspace-driver-schema-migration-diagnostics.md` records `WorkspaceDriverConfig`, driver lifecycle names, `workspace_capabilities`, runtime capability validation, fingerprint coverage, and the `workspace_driver_invocation_unavailable` safety gate.

- Observation: The initial driver command contract plan was checked in before its adapter work was complete, but the LIV-184 implementation workspace now has the jj driver script.
  Evidence: `docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md` names `scripts/scherzo-workspace-jj` and `scripts/scherzo-workspace-noop`. In the 2026-05-11 implementation workspace, `scripts/scherzo-workspace-jj` exists and the non-mutating `lifecycle after-step` preflight succeeds.

- Observation: The existing jj workspace smoke test is the right place to catch lifecycle parity regressions.
  Evidence: `test/local_integration/workflow_jj_workspace_smoke_test.gleam` creates a temporary jj repository, runs a two-step workflow in one logical workspace, and asserts that the workspace is reused and forgotten after cleanup.

- Observation: The umbrella's LIV-168 note is no longer ambiguous for this plan.
  Evidence: `docs/plans/workspace-profile-helper-capabilities-umbrella.md` records LIV-168 as the completed temporary migration from legacy direct hooks to an explicit named `dogfood-jj` hook profile. LIV-172 is therefore the later driver-backed migration.

- Observation: The LIV-171 adapter script is present, but the runtime invocation prerequisite is still missing in the LIV-184 implementation workspace.
  Evidence: `test -x scripts/scherzo-workspace-jj && scripts/scherzo-workspace-jj lifecycle after-step` exits 0. A temporary driver-only workflow-config doctor check exits 1 with `workspace_driver_invocation_unavailable` and the message "workspace driver invocation is not implemented in this Scherzo version".

## Decision Log

- Decision: Keep the dogfood profile name `dogfood-jj`.
  Rationale: The name is already checked in, every dogfood workflow selects it, and it accurately describes this repository's jj-backed dogfood profile.
  Date: 2026-05-09

- Decision: Preserve explicit `workspace_profile: dogfood-jj` selectors in every dogfood workflow.
  Rationale: Explicit selectors make the workflow/profile relationship visible in review and prevent future default-profile changes from silently changing these workflows.
  Date: 2026-05-09

- Decision: Treat LIV-168 as completed interim history and LIV-172 as the driver-backed follow-up.
  Rationale: The umbrella says LIV-168 completed the migration to a named hook profile. This plan should not redo that work; it should replace the hook-backed implementation under the existing profile.
  Date: 2026-05-10

- Decision: Advertise only the initial jj adapter capabilities in the migrated dogfood profile.
  Rationale: LIV-171 implements or plans `status`, `diff`, `changed-files`, and `assert-only` for the initial jj adapter. `baseline`, `refresh-base`, and `publish-change` belong to later dogfood helper conversion work and must not be advertised prematurely.
  Date: 2026-05-10

- Decision: Do not add `workspace_capabilities` fields in this plan.
  Rationale: The dogfood workflows do not yet invoke driver capabilities through `$SCHERZO_WORKSPACE_DRIVER`. Capability requirements should be added when workflow steps actually consume driver operations.
  Date: 2026-05-09

- Decision: Leave prompt wording and repo-local workflow helper scripts unchanged except for documentation that points to the new profile.
  Rationale: This plan is the dogfood profile migration child plan, not the heavier workflow-conversion child plan. Prompt and helper-script neutralization requires separate design around status, diff, refresh, and publish semantics.
  Date: 2026-05-09

- Decision: Keep this ExecPlan blocked until runtime driver lifecycle invocation is available.
  Rationale: LIV-170 intentionally blocks selected driver-backed profiles with `workspace_driver_invocation_unavailable`. Migrating dogfood config before that safety gate is retired would knowingly make the dogfood daemon unable to dispatch workflows.
  Date: 2026-05-10

- Decision: Stop the LIV-184 implementation attempt before changing `.scherzo/scherzo.yaml`, `.scherzo/README.md`, workflows, or tests.
  Rationale: The prerequisite gate proved selected driver-only profiles still fail with `workspace_driver_invocation_unavailable`; proceeding would replace the currently runnable hook-backed dogfood profile with a profile the runtime rejects before dispatch.
  Date: 2026-05-11

## Outcomes & Retrospective

The 2026-05-11 LIV-184 implementation attempt retired only part of the prerequisite risk. The jj driver adapter exists and accepts the non-mutating lifecycle preflight, but runtime lifecycle invocation for selected driver-backed profiles has not landed. No dogfood config, workflow, README, or test migration was performed. The remaining next step is to complete or merge the runtime invocation prerequisite, then rerun this plan from Milestone 0 before changing `.scherzo/scherzo.yaml`.

## Context and Orientation

Scherzo is a Gleam application that loads an operator config, routes tracker issues to workflow DAG files, prepares a workspace for each workflow step, runs command and agent steps, and reports results. A workflow DAG is a YAML file such as `.scherzo/workflows/execplan.yaml`. A workspace is the directory where a step runs. A workspace profile is named operator policy for preparing and cleaning those directories. A workspace driver is a trusted command configured by the operator; Scherzo invokes it for lifecycle operations and workflow steps may invoke it for declared capabilities in later child plans.

The current dogfood config is `.scherzo/scherzo.yaml`. Its workspace section uses `workspace.root: workspaces`, `workspace.default_profile: dogfood-jj`, and `workspace.profiles.dogfood-jj.hooks`. The current dogfood workspace helper is `scripts/scherzo-jj-workspace`. It supports the legacy helper verbs `after-create`, `before-run`, and `before-remove`.

The current dogfood routing and scheduled-job config reference seven workflow YAML files:

- `research` uses `.scherzo/workflows/research.yaml`.
- `implementation` uses `.scherzo/workflows/implementation.yaml`.
- `execplan` uses `.scherzo/workflows/execplan.yaml`.
- `execplan-revision` uses `.scherzo/workflows/execplan-revision.yaml`.
- `execplan-implementation` uses `.scherzo/workflows/execplan-implementation.yaml`.
- `merge-conflict-resolution` uses `.scherzo/workflows/merge-conflict-resolution.yaml`.
- `github-pr-conflict-scout` uses `.scherzo/workflows/github-pr-conflict-scout.yaml` and is also configured as a scheduled job.

`docs/plans/workspace-profile-helper-capabilities-umbrella.md` describes the larger program. `docs/plans/LIV-170-workspace-driver-schema-migration-diagnostics.md` is the core driver schema and migration-diagnostics child plan. `docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md` is the initial driver command contract and jj/no-op adapter plan. `docs/plans/LIV-173-expose-workspace-driver-context.md`, `docs/plans/LIV-174-portable-research-workflow-with-workspace-drivers.md`, `docs/plans/LIV-175-convert-dogfood-implementation-workflows-to-driver-operations.md`, and `docs/plans/LIV-176-workspace-driver-docs-and-migration-guide.md` are sibling or later plans that should not be folded into this migration.

## Preconditions and Verified Facts

Before implementation begins, run `jj status --color=never` from the repository root and confirm the working copy is clean or only contains intentional implementation changes for this plan. A dirty tree makes it hard to distinguish workflow migration edits from unrelated dogfood changes.

The prerequisite plan files are now known and must exist in the implementation tree:

    test -f docs/plans/LIV-170-workspace-driver-schema-migration-diagnostics.md
    test -f docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md
    test -f docs/runbooks/workspace-driver-migration.md

The core schema prerequisite from LIV-170 must be present. In the target implementation tree, `src/scherzo/config/types.gleam` should define `WorkspaceDriverConfig`, `WorkspaceLifecycleOperation`, `WorkspaceCapability`, and a workspace profile representation that can hold either `hooks` or `driver`. `src/scherzo/config.gleam` should parse `workspace.profiles.<name>.driver.command`, `driver.lifecycle`, `driver.capabilities`, and `driver.timeout_ms`. `src/scherzo/workflow_dag.gleam` should parse top-level `workspace_capabilities`. `src/scherzo/workspace_profile.gleam` and `src/scherzo/runtime_bundle.gleam` should validate selected profile capabilities before dispatch. `src/scherzo/workflow_fingerprint.gleam` should include selected driver-backed profile metadata in execution fingerprints.

The adapter prerequisite from LIV-171 must be implemented, not merely planned. The implementation tree must contain an executable jj driver command at `scripts/scherzo-workspace-jj`. It must support lifecycle subcommands equivalent to:

    scripts/scherzo-workspace-jj lifecycle create
    scripts/scherzo-workspace-jj lifecycle before-step
    scripts/scherzo-workspace-jj lifecycle after-step
    scripts/scherzo-workspace-jj lifecycle remove

It must also implement the initial jj adapter capability commands from LIV-171:

    scripts/scherzo-workspace-jj status --human
    scripts/scherzo-workspace-jj diff --human
    scripts/scherzo-workspace-jj changed-files --json
    scripts/scherzo-workspace-jj assert-only --path <relative-file>

Runtime driver lifecycle invocation must also have landed before this plan edits dogfood config. LIV-170 intentionally made selected driver-backed profiles fail with `workspace_driver_invocation_unavailable` until runtime invocation exists. If a selected driver-backed profile still produces that diagnostic in bundle loading or doctor output, stop and complete the missing runtime invocation prerequisite first.

The current profile migration should use this driver profile shape unless the runtime invocation prerequisite deliberately finalizes a different command resolution rule:

    driver:
      command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
      lifecycle: [create, before-step, after-step, remove]
      capabilities: [status, diff, changed-files, assert-only]
      timeout_ms: 60000

If a later runtime or documentation plan settles on a different repository-relative command spelling, such as `scripts/scherzo-workspace-jj`, revise this plan before implementing so `.scherzo/scherzo.yaml`, `.scherzo/README.md`, and tests all use the same final spelling. Do not improvise command expansion or shell quoting during implementation.

The normal validation commands are run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If `direnv exec . <command>` reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. That is an environment setup issue, not a code failure.

## Scope Boundaries

In scope after prerequisites are complete:

- Replace `workspace.profiles.dogfood-jj.hooks` in `.scherzo/scherzo.yaml` with `workspace.profiles.dogfood-jj.driver`.
- Preserve `workspace.root: workspaces` and `workspace.default_profile: dogfood-jj`.
- Verify all seven checked-in dogfood workflow YAML files under `.scherzo/workflows/` still contain top-level `workspace_profile: dogfood-jj`.
- Update `.scherzo/README.md` so the dogfood convention describes driver-backed profiles and no longer presents `scripts/scherzo-jj-workspace` hook snippets as the current dogfood profile implementation.
- Add or update tests proving driver-backed dogfood config loads, all dogfood workflows select the configured profile, and jj workspace lifecycle behavior is preserved through the driver-backed profile path.

Out of scope:

- Implementing runtime driver lifecycle invocation if it has not already landed.
- Creating `scripts/scherzo-workspace-jj` or `scripts/scherzo-workspace-noop` if they do not already exist.
- Adding `baseline`, `refresh-base`, or `publish-change` support to the driver.
- Adding `workspace_capabilities` to dogfood workflows.
- Rewriting prompts to remove direct jj language.
- Rewriting `scripts/scherzo-implementation`, `scripts/scherzo-execplan`, `scripts/scherzo-execplan-revision`, `scripts/scherzo-merge-conflict`, or `scripts/scherzo-review` to use driver capabilities.
- Changing `examples/` public workflows; that belongs to portable research and documentation plans.
- Changing Linear routing, handoff behavior, pi configuration, scheduled-job behavior, artifact limits, or model settings.
- Operating a live daemon without using the rollout checks in this plan.

## Milestones

Milestone 0 verifies prerequisites. At the end, the implementation tree contains the LIV-170 schema, the implemented LIV-171 jj driver adapter, and runtime lifecycle invocation for selected driver-backed profiles. A selected driver-backed profile no longer fails with `workspace_driver_invocation_unavailable`. No dogfood config edits happen before this milestone is complete.

Milestone 1 freezes the dogfood profile contract. At the end, the implementer has confirmed the exact driver command spelling, lifecycle names, capability list, config parser fields, and runtime test helpers to use. If any of those differ from this plan, revise this plan first.

Milestone 2 migrates `.scherzo/scherzo.yaml`. At the end, the config no longer contains `workspace.profiles.dogfood-jj.hooks`; it contains one default driver-backed profile named `dogfood-jj`; all non-workspace sections of the file remain unchanged. A config parse or doctor workflow-config check can load the file without a legacy-hook or driver-invocation diagnostic.

Milestone 3 verifies checked-in dogfood workflow DAGs. At the end, each of the seven dogfood workflow YAML files contains `workspace_profile: dogfood-jj` at workflow top level. Their step lists, command strings, prompts, timeouts, and `max_parallel_steps` values remain unchanged.

Milestone 4 updates dogfood documentation and tests. At the end, `.scherzo/README.md` tells maintainers to use the `dogfood-jj` driver profile, the config parser tests cover the intended driver profile shape, a runtime-bundle or workflow-config test proves all dogfood workflows select a known driver-backed profile, and the local jj workspace smoke test uses the driver-backed profile path.

Milestone 5 rolls the config into a live dogfood daemon safely. At the end, dispatch has been paused or confirmed inactive, active sessions have either finished or been explicitly handled, the workflow-config doctor check has passed, the daemon has reloaded the config, and dispatch has resumed only after the new config is accepted.

## Plan of Work

First run the prerequisite checks in Concrete Steps. If the driver adapter, runtime invocation support, or exact command spelling is missing, update Progress and Surprises & Discoveries and stop. Do not edit dogfood config while selected driver-backed profiles are still a schema-only transition state.

Then edit `.scherzo/scherzo.yaml`. Keep `workspace.root: workspaces`, the existing workspace comments, and `default_profile: dogfood-jj`. Remove only the `hooks:` map under `workspace.profiles.dogfood-jj`. Add a `driver:` map under `dogfood-jj`. Unless the prerequisite runtime finalized a different command spelling, the new workspace section should be:

    workspace:
      # Paths are resolved relative to .scherzo/scherzo.yaml. Runtime workspaces
      # land under repo-root/.scherzo/workspaces/<workflow>/<issue>/<run>/.
      root: workspaces
      # All checked-in dogfood workflows select dogfood-jj explicitly. Keep the
      # same named profile as the documented default for ad-hoc routed workflows.
      default_profile: dogfood-jj
      profiles:
        dogfood-jj:
          driver:
            command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [status, diff, changed-files, assert-only]
            timeout_ms: 60000

Do not change `tracker`, `polling`, `agent`, `pi`, `handoff`, `routing`, `scheduled_jobs`, `artifact_limits`, `linear_contract`, or `linear_commands` in this milestone.

Next inspect each dogfood workflow YAML file. The expected selector is already present. If any file lacks `workspace_profile: dogfood-jj`, add it at top level after `description:` and before `max_parallel_steps:`. The files are:

- `.scherzo/workflows/research.yaml`
- `.scherzo/workflows/implementation.yaml`
- `.scherzo/workflows/execplan.yaml`
- `.scherzo/workflows/execplan-revision.yaml`
- `.scherzo/workflows/execplan-implementation.yaml`
- `.scherzo/workflows/merge-conflict-resolution.yaml`
- `.scherzo/workflows/github-pr-conflict-scout.yaml`

Do not edit step `run:` strings. Do not edit prompt files under `.scherzo/workflows/prompts/` in this plan.

Then edit `.scherzo/README.md`. Replace wording that describes `dogfood-jj` as a hook-backed profile invoking `scripts/scherzo-jj-workspace`. The new wording should say that dogfood workflows use `.scherzo/scherzo.yaml` `workspace.profiles.dogfood-jj.driver`, that the driver command is the final `scripts/scherzo-workspace-jj` spelling selected by the runtime prerequisite, that workflows select it with top-level `workspace_profile: dogfood-jj`, and that hook-backed profile configuration is legacy migration material covered by `docs/runbooks/workspace-driver-migration.md`.

Finally update tests. Keep test edits focused on this migration rather than re-testing every driver internal. If exact test helpers or constructors differ from the names in this plan, stop and revise this plan before writing tests.

## Concrete Steps

1. From the repository root, inspect source-control state:

       jj status --color=never

   Expect either `The working copy has no changes.` or only intentional changes for this implementation.

2. Confirm the prerequisite plan and runbook files exist:

       test -f docs/plans/LIV-170-workspace-driver-schema-migration-diagnostics.md
       test -f docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md
       test -f docs/runbooks/workspace-driver-migration.md

   Expect all commands to exit 0.

3. Verify core schema support exists in source and tests:

       grep -R "WorkspaceDriverConfig" -n src test
       grep -R "workspace_capabilities" -n src test
       grep -R "workspace-driver-migration" -n src test docs/runbooks

   Expect each command to print relevant source or test lines and exit 0.

4. Verify the jj workspace driver exists and supports a non-mutating lifecycle preflight:

       test -x scripts/scherzo-workspace-jj
       scripts/scherzo-workspace-jj lifecycle after-step

   Expect both commands to exit 0. If the implemented driver uses a different non-mutating preflight, revise this plan to name the actual command before continuing.

5. Verify the runtime no longer blocks selected driver-backed profiles. Use the exact runtime-bundle or doctor test added by the driver-invocation prerequisite. If no such test exists, add or locate it before this migration. The important observable result is that selected driver-backed profiles are runnable and no longer fail with `workspace_driver_invocation_unavailable`.

6. Run the prerequisite test suite once before editing dogfood config:

       direnv exec . gleam test

   Expect all tests to pass. If tests fail because driver work is incomplete, fix or finish the prerequisite work before continuing.

7. Edit `.scherzo/scherzo.yaml` to replace only `workspace.profiles.dogfood-jj.hooks` with the `dogfood-jj` driver-backed profile shown in Plan of Work. Preserve non-workspace sections unchanged.

8. Run focused text checks:

       grep -n "^[[:space:]]*hooks:" .scherzo/scherzo.yaml
       grep -n "^[[:space:]]*driver:" .scherzo/scherzo.yaml
       grep -n "capabilities: \[status, diff, changed-files, assert-only\]" .scherzo/scherzo.yaml

   Expect the first command to print no output and exit status 1. Expect the second and third commands to print the dogfood driver profile lines.

9. Verify all dogfood workflow selectors:

       grep -n "^workspace_profile: dogfood-jj$" .scherzo/workflows/*.yaml

   Expect seven matching lines, one in each checked-in dogfood workflow YAML file. If any of the seven files is missing the selector, add it at top level after `description:`.

10. Edit `.scherzo/README.md` to describe `workspace.profiles.dogfood-jj.driver`, `scripts/scherzo-workspace-jj`, and top-level workflow `workspace_profile: dogfood-jj`. Remove wording that presents hook-backed `dogfood-jj` as the current dogfood convention.

11. Add or update `test/orchestrator_config_test.gleam` with a test named `workspace_driver_profiles_resolve_dogfood_jj_shape_test`. Use the exact config parser helper and exact `WorkspaceDriverConfig` data accessors in the implementation tree. The test must build a minimal config using the dogfood workspace snippet from this plan and assert that the resolved orchestrator default profile is `dogfood-jj`, that `dogfood-jj` exists, that its driver command matches the final configured command string, that its lifecycle list is `create`, `before-step`, `after-step`, `remove`, that its capabilities list is `status`, `diff`, `changed-files`, `assert-only`, and that its timeout is `60000`.

12. Add or update a runtime-bundle or workflow-config test, preferably in `test/runtime_bundle_test.gleam` if that file contains bundle-loading tests. The test name should be `dogfood_workflows_select_existing_driver_profile_test`. It should load `.scherzo/scherzo.yaml` with `LINEAR_API_KEY` set to `dummy`, load the seven routed dogfood workflow YAML files, and assert that bundle loading succeeds. Then assert that each loaded DAG's `workspace_profile` field is `Some("dogfood-jj")` and that none declares `workspace_capabilities` in this migration.

13. Update or add local integration coverage for the jj workspace driver profile. If `test/local_integration/workflow_jj_workspace_smoke_test.gleam` remains the right file, replace its synthetic hook profile setup with the exact driver-backed profile type and runtime invocation path from the prerequisite implementation. Name the profile `dogfood-jj`, make the test DAG include `workspace_profile: dogfood-jj`, and use `scripts/scherzo-workspace-jj` instead of `scripts/scherzo-jj-workspace`. Preserve the existing assertions: both command steps run in the same workspace path, the run root matches the success result, cleanup removes the run root, and the underlying jj repository no longer lists the temporary Scherzo workspace after cleanup.

14. Run the workflow-config doctor check from the repository root:

       LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml

   Expect exit status 0. The output must not contain `workspace.hooks`, `.hooks is legacy`, `unknown_workspace_profile`, `workspace_driver_invocation_unavailable`, or a missing-driver diagnostic.

15. Run the full test suite:

       direnv exec . gleam test

   Expect all tests to pass with no failures. The exact pass count may increase because this plan adds or updates tests.

16. Run formatting and production lint gates:

       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect all commands to exit 0. Do not add production `let assert`, `panic`, or `todo` while making this migration.

17. If this repository's dogfood daemon is running, perform the live rollout sequence before relying on the migrated config. Start with read-only inspection:

       scripts/scherzoctl ping --json
       scripts/scherzoctl ps --json

   If no control file exists and the command reports that it cannot reach a daemon, record that there is no live daemon to roll out and skip to the commit point after validation.

18. Pause dispatch before applying the migrated config to a live daemon:

       scripts/scherzoctl pause --json

   Expect a JSON response with `ok: true` and a command status of `applied` or an equivalent accepted status. If pause is rejected, do not reload or resume; investigate before continuing.

19. Confirm no active runs will be crossed over to the new workspace profile:

       scripts/scherzoctl ps --json

   It is safe to continue only when the `sessions` list is empty or every session has a terminal status. If any session is `preparing`, `probing`, `running`, `waiting_ui`, or `stopping`, either wait for it to finish, use `scripts/scherzoctl stop-after-turn <session-id> --yes --json` for a cooperative stop, or use `scripts/scherzoctl abort <session-id> --yes --json` only after an operator explicitly accepts losing in-flight work.

20. Check local state before daemon reload:

       scripts/scherzoctl state status --root .scherzo/workspaces --json

   Expect read-only state output whose status is `current` or another non-corrupt status explicitly accepted by the operator. If it reports `corrupt`, `unsupported`, or ambiguous retained runs, pause the rollout and inspect before reloading.

21. Reload and resume the daemon only after the doctor check and active-run check have passed:

       scripts/scherzoctl reload --json
       scripts/scherzoctl resume --json

   Expect each command to return `ok: true` and command status `applied` or an equivalent accepted status. If reload fails, keep dispatch paused, restore the previous config or fix the prerequisite issue, rerun the doctor check, then retry reload.

22. Commit point: create one logical commit containing `.scherzo/scherzo.yaml`, `.scherzo/README.md`, any selector fixes to `.scherzo/workflows/*.yaml`, and the test updates. A suitable message is `Migrate dogfood workflows to driver profiles`.

## Testing and Falsifiability

The plan is falsified if dogfood config can still load only through `workspace.profiles.dogfood-jj.hooks`, if any checked-in dogfood workflow stops selecting `workspace_profile: dogfood-jj`, if runtime bundle loading cannot resolve the selected driver-backed profile before dispatch, if selected driver-backed profiles still fail with `workspace_driver_invocation_unavailable`, if the jj workspace lifecycle smoke test fails after switching to the driver-backed profile, or if a live daemon cannot safely pause, validate, reload, and resume around the config migration.

Add or update these tests after the prerequisite driver-adapter and runtime-invocation code has landed:

- `test/orchestrator_config_test.gleam`, `workspace_driver_profiles_resolve_dogfood_jj_shape_test`: parse a minimal config with `workspace.default_profile: dogfood-jj` and `workspace.profiles.dogfood-jj.driver`; assert the resolved profile name, command, lifecycle list, capability list, and timeout exactly match this plan.
- `test/runtime_bundle_test.gleam`, `dogfood_workflows_select_existing_driver_profile_test`: load the real dogfood config and all seven real dogfood workflow files with a dummy Linear API key; assert the bundle succeeds and each DAG explicitly has `workspace_profile == Some("dogfood-jj")`.
- `test/local_integration/workflow_jj_workspace_smoke_test.gleam` or a new adjacent local integration file: construct the orchestrator with the driver-backed profile rather than a hook-backed profile; assert workspace reuse and cleanup behavior stay the same.

The config-shape and smoke tests should fail before this migration because the current dogfood config is hook-backed and the driver-backed lifecycle path is not yet being used. The selector inventory may already pass before migration, and that is expected because LIV-168 already added the selectors. This plan does not claim that workflows are VCS-neutral or portable after migration. Direct jj prompt wording is still allowed in this child plan.

## Validation and Acceptance

Acceptance requires all of the following observable outcomes:

- The prerequisite plan files `docs/plans/LIV-170-workspace-driver-schema-migration-diagnostics.md` and `docs/plans/LIV-171-workspace-driver-command-contract-and-adapters.md` exist.
- The prerequisite implementation includes `docs/runbooks/workspace-driver-migration.md`, `workspace_capabilities` parsing, driver-backed profile parsing, runtime capability validation, execution fingerprint coverage for selected driver metadata, executable `scripts/scherzo-workspace-jj`, and runtime lifecycle invocation for selected driver-backed profiles.
- `.scherzo/scherzo.yaml` has no `workspace.profiles.dogfood-jj.hooks` block and contains `workspace.profiles.dogfood-jj.driver`.
- The dogfood driver profile advertises only the initial implemented jj adapter capabilities unless a later prerequisite has implemented more: `status`, `diff`, `changed-files`, and `assert-only`.
- The seven files under `.scherzo/workflows/` named in this plan each contain top-level `workspace_profile: dogfood-jj`.
- `.scherzo/README.md` describes the driver-backed dogfood profile convention.
- `LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml` exits 0 without a legacy-hook or driver-invocation diagnostic.
- `direnv exec . gleam test` exits 0.
- `direnv exec . gleam format --check src test` exits 0.
- `direnv exec . gleam run -m glinter` exits 0.
- `direnv exec . gleam run -m scherzo_lint` exits 0.
- For a live daemon rollout, `scripts/scherzoctl pause --json`, `scripts/scherzoctl ps --json`, `scripts/scherzoctl reload --json`, and `scripts/scherzoctl resume --json` either succeed with accepted statuses or the operator records that no live daemon control file exists and the migration was validated offline only.

A reviewer should also be able to inspect the diff and see that this child plan did not implement core driver runtime features, did not add workflow capability declarations, did not rewrite prompt semantics, and did not change workflow step behavior beyond the selected workspace profile implementation.

## Rollout, Recovery, and Idempotence

Roll this out only after the core driver-schema, jj-driver adapter, and runtime driver-invocation prerequisites are merged. Because `.scherzo/scherzo.yaml` is checked-in operator config, the rollout surface is the dogfood daemon and any local developer who runs this repository's Scherzo config.

For a live daemon, use `scripts/scherzoctl ping --json` and `scripts/scherzoctl ps --json` first. If the daemon is reachable, run `scripts/scherzoctl pause --json` before switching the checked-in config that the daemon will reload. Continue only when `ps --json` shows no active sessions, or after active sessions have been allowed to finish, stopped after turn, or explicitly aborted by an operator. Then run the workflow-config doctor check. Only after the doctor check exits 0 should the operator run `scripts/scherzoctl reload --json` and `scripts/scherzoctl resume --json`.

If no daemon is reachable because there is no control file, this is a local offline config migration. In that case, the rollout proof is the doctor command, test suite, formatting, and lint gates. The next daemon start should be treated as the first deployment of the migrated config.

If a migrated daemon fails at startup or reload with an unknown profile, missing driver, capability-schema error, or migration-runbook diagnostic, keep dispatch paused and restore the previous hook-backed dogfood profile or fix the prerequisite driver installation. Do not allow the daemon to dispatch issues with a partially migrated config.

If an in-flight run was created under the old hook-backed `dogfood-jj` config, treat it conservatively. If the driver migration changes execution fingerprints, recovery should reject incompatible old runs rather than continuing them under a different workspace implementation. Operators can let old runs finish before deploying the config migration, park affected issues, or abort and redispatch them after the migrated config is active.

The migration edits are idempotent. Reapplying them should leave the same `workspace` section, the same seven workflow selectors, and the same README wording. The focused `grep` checks in Concrete Steps catch accidental duplicate or missing selectors.

Rollback for the configuration part is to restore the old `workspace.profiles.dogfood-jj.hooks` block in `.scherzo/scherzo.yaml` and restore the old `.scherzo/README.md` wording. Rollback is only valid on a Scherzo version that still accepts hook-backed profiles.

## Artifacts and Notes

The current dogfood workspace shape is a named hook profile under `.scherzo/scherzo.yaml`:

    workspace:
      root: workspaces
      default_profile: dogfood-jj
      profiles:
        dogfood-jj:
          hooks:
            create: ... scripts/scherzo-jj-workspace after-create ...
            before_step: ... scripts/scherzo-jj-workspace before-run ...
            after_step: true
            remove: ... scripts/scherzo-jj-workspace before-remove ...
            timeout_ms: 60000

The target dogfood workspace shape is the same named profile backed by the jj workspace driver:

    workspace:
      root: workspaces
      default_profile: dogfood-jj
      profiles:
        dogfood-jj:
          driver:
            command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [status, diff, changed-files, assert-only]
            timeout_ms: 60000

The workflow-file state should remain explicit and visible. Each dogfood workflow should contain:

    version: 1
    id: <workflow-id>
    description: <existing description>
    workspace_profile: dogfood-jj
    max_parallel_steps: <existing value>

Do not add local machine paths to docs, tests, config, or fixtures while implementing this plan. If a test needs to discuss an invalid absolute path, use a placeholder such as `<absolute-local-path>`.

The live-daemon rollout command sequence is:

    scripts/scherzoctl ping --json
    scripts/scherzoctl pause --json
    scripts/scherzoctl ps --json
    LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
    scripts/scherzoctl reload --json
    scripts/scherzoctl resume --json

## Interfaces and Dependencies

The config interface from LIV-170 is:

    workspace.profiles.<profile-name>.driver.command: String
    workspace.profiles.<profile-name>.driver.lifecycle: List(String)
    workspace.profiles.<profile-name>.driver.capabilities: List(String)
    workspace.profiles.<profile-name>.driver.timeout_ms: Int

The workflow interface that should already be present in all dogfood workflows is:

    workspace_profile: dogfood-jj

The expected driver command from LIV-171 is `scripts/scherzo-workspace-jj`. It must preserve the current dogfood jj lifecycle behavior, must exit 0 on success, and must print bounded diagnostics on stderr on failure. It must not print secrets. Its lifecycle operation command shapes are `lifecycle create`, `lifecycle before-step`, `lifecycle after-step`, and `lifecycle remove`. Its initial capability command shapes are `status --human`, `diff --human`, `changed-files --json`, and `assert-only --path <relative-file>`.

The exact Gleam symbols known from LIV-170 are `WorkspaceDriverConfig`, `WorkspaceLifecycleOperation`, `WorkspaceCapability`, `WorkspaceHookProfile` with optional `hooks` and `driver` fields, and `WorkflowDag.workspace_capabilities`. If those names have been renamed by the runtime invocation prerequisite, revise this plan before writing tests.

The files this plan expects the implementer to inspect or change are:

- Inspect prerequisite runtime code after it lands: `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workspace_profile.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/workflow_fingerprint.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/workflow_run.gleam`, and `scripts/scherzo-workspace-jj`.
- Change dogfood config: `.scherzo/scherzo.yaml`.
- Verify dogfood workflows: `.scherzo/workflows/research.yaml`, `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, `.scherzo/workflows/merge-conflict-resolution.yaml`, and `.scherzo/workflows/github-pr-conflict-scout.yaml`.
- Change dogfood documentation: `.scherzo/README.md`.
- Add or update tests: `test/orchestrator_config_test.gleam`, `test/runtime_bundle_test.gleam`, and `test/local_integration/workflow_jj_workspace_smoke_test.gleam` or a new adjacent local integration file.

## Open Questions and Clarifications Needed

None for merging this plan as a blocked planning artifact. The remaining blockers are implementation prerequisites, not stakeholder clarifications: the LIV-171 adapter work must be implemented, and selected driver-backed profiles must be dispatchable through runtime lifecycle invocation before this plan is executed.
