# Workspace profile drivers and capabilities for shareable Scherzo workflows

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this umbrella plan is complete, Scherzo will have a coherent design for making workflows portable across repositories without baking this repository's jj workspace behavior into every workflow prompt or driver script. Operators will be able to define a named workspace profile backed by one workspace driver command that says how a workflow workspace is created and what workflow-facing operations it supports. Workflow authors will be able to declare the capabilities their workflow needs, such as reading status, listing changed files, asserting that only one artifact changed, refreshing onto a base branch, or publishing a completed change to an external review or integration target. Scherzo will validate those declarations before dispatch so a workflow fails early with a clear configuration error instead of failing halfway through an agent run because `jj`, `git`, or a repo-local script is missing.

This umbrella deliberately does not implement the whole feature in one change. It records the design direction, vocabulary, invariants, and child ExecPlans needed to evolve the current `workspace.hooks` and `workspace_profile` support into a shareable workflow platform. The visible proof for this umbrella itself is a checked-in design document plus a concrete list of child ExecPlans. The visible proof for the full future program is that a third-party repository can copy the portable research workflow, provide either a no-op or git-based workspace profile with the required `assert-only` capability, and run the workflow without editing Scherzo's dogfood jj scripts.

## Problem Framing and Constraints

Scherzo already has workflow DAG files under `.scherzo/workflows/` and examples under `examples/workflows/`. The dogfood workflows in this repository are useful, but they are not all shareable. The current checked-in dogfood config in `.scherzo/scherzo.yaml` uses legacy direct `workspace.hooks` that call `scripts/scherzo-jj-workspace`, and the checked-in workflow YAML files omit `workspace_profile`, so they all implicitly use the synthetic legacy `default` profile. Many prompts also say things such as "You are already inside a dedicated jj workspace" and tell the agent to use `jj status --color=never`. Implementation-style helper scripts such as `scripts/scherzo-implementation`, `scripts/scherzo-execplan`, `scripts/scherzo-execplan-revision`, and `scripts/scherzo-merge-conflict` use jj not only for workspace setup but also for diffing, base refresh, bookmarking, pushing, and external review/change publication.

The immediate operator pain is that workflows are hard to package for others. A simple research workflow only needs a workspace and a way to collect or assert its `research-findings.md` artifact, but today the dogfood prompt and collection step mention Linear and jj. Larger workflows need more than a workspace; they need a standard way to ask for change lifecycle operations without hardcoding jj. If Scherzo exposes only raw hooks, every workflow must either know which VCS the repository uses or call repo-local scripts that third parties cannot reuse.

The design must preserve several constraints. Workspace lifecycle commands are trusted operator configuration, not untrusted workflow content. Workflow YAML may select a named profile and declare required capabilities, but it must not define arbitrary shell lifecycle commands. Named `workspace.profiles` and top-level workflow `workspace_profile` already exist and should be reused rather than replaced. Because the current user base is very small, the design should prefer a clean breaking migration over carrying a long-lived compatibility shim for legacy direct `workspace.hooks`. Old config should be detected and rejected with an actionable message pointing to a migration document, and `doctor` should make that migration path easy to find. The initial driver contract should be small and falsifiable; it should support the research workflow and the first dogfood migration before trying to abstract every publish workflow in the repository. The design must stay VCS-neutral at the workflow layer even though the first real dogfood driver will be jj-backed.

## Strategy Overview

Treat a workspace profile as the top-level operator policy bundle. A profile is backed by one workspace driver command. The driver has lifecycle operations that Scherzo invokes automatically, such as `lifecycle create`, `lifecycle before-step`, `lifecycle after-step`, and `lifecycle remove`, plus workflow-facing operations that command steps and agents invoke directly, such as `status`, `changed-files`, `diff`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. A profile declares the capabilities supported by its driver. A workflow declares the workspace capabilities it requires. During runtime bundle loading, Scherzo resolves the selected profile and fails before dispatch if the profile does not provide all required capabilities.

The preferred YAML shape should extend the existing profile model rather than introduce a separate VCS model. The profile remains under `workspace.profiles.<name>`, but new configs use `driver` instead of direct lifecycle shell snippets. For example:

    workspace:
      default_profile: dogfood-jj
      profiles:
        dogfood-jj:
          driver:
            command: "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj"
            lifecycle: [create, before-step, after-step, remove]
            capabilities: [status, diff, changed-files, assert-only, baseline, refresh-base, publish-change]
            timeout_ms: 60000

When Scherzo prepares a workspace for this profile, it invokes the same driver command with lifecycle subcommands. The exact arguments should be finalized by the child plan, but the intended shape is:

    $SCHERZO_WORKSPACE_DRIVER lifecycle create
    $SCHERZO_WORKSPACE_DRIVER lifecycle before-step
    $SCHERZO_WORKSPACE_DRIVER lifecycle after-step
    $SCHERZO_WORKSPACE_DRIVER lifecycle remove

Workflow command steps and agents use the same command endpoint for declared capabilities:

    $SCHERZO_WORKSPACE_DRIVER status --human
    $SCHERZO_WORKSPACE_DRIVER changed-files --json
    $SCHERZO_WORKSPACE_DRIVER assert-only --path research-findings.md

A portable research workflow can then declare only what it needs:

    version: 1
    id: research
    description: Investigate an issue, write Markdown findings, verify the findings file, and use it as the result artifact.
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
        run: '$SCHERZO_WORKSPACE_DRIVER assert-only --path research-findings.md && cat research-findings.md'
        workspace: main

Legacy direct `workspace.hooks` should become an explicitly detected old configuration shape, not a long-lived compatibility path. The config loader and `doctor` should point operators to a migration document such as `docs/runbooks/workspace-driver-migration.md`. The workflow layer talks about workspace operations, not `jj`, `git`, worktrees, or any other VCS implementation.

## Alternatives Considered

One alternative is to call the abstraction `vcs` or `vcs_helper`. That is rejected for the workflow-facing surface because not all workflows need version control. Research and artifact-only workflows may run in an empty directory, a copied tree, a container snapshot, or a no-op workspace. Even implementation workflows need change lifecycle operations more than they need a specific VCS. A jj or git adapter can exist internally, but workflows should require workspace capabilities.

Another alternative is to keep using raw `workspace.hooks` and ask every shareable workflow to document the commands it expects. That is insufficient because Scherzo cannot validate compatibility before dispatch and agents remain exposed to repository-specific instructions. Raw hooks also conflate lifecycle setup with operational commands.

A third alternative is to keep direct `workspace.hooks` as a long-lived compatibility path. That is now rejected because Scherzo has a very small user base and the driver model is clearer. A deliberate breaking migration with a precise error and a migration guide is less costly than carrying two profile systems and explaining both in public docs.

A fourth alternative is to rename `hooks` to `lifecycle` while keeping inline lifecycle shell. `lifecycle` is clearer than `hooks`, but it still leaves lifecycle commands separate from workflow-facing operations. A single driver command is more coherent because it can own both sets of commands.

A fifth alternative is to let workflow YAML define the driver command directly. That is rejected for the same reason workflows cannot define lifecycle hook scripts: driver commands are trusted local shell configured by the operator. Workflow files may be copied from third parties and should only select profiles and require capabilities.

A sixth alternative is to build one universal driver script that all repositories must use. That is too prescriptive. Scherzo should define the command contract and validation rules; repositories can provide jj, git, worktree, copy-based, no-op, or company-internal implementations behind the same capability names.

## Risks and Countermeasures

The main abstraction risk is creating a generic workspace driver junk drawer. Countermeasure: define a small fixed capability vocabulary first, require tests for each built-in capability, and avoid adding operations until a real workflow needs them. For custom future operations, prefer namespaced capabilities such as `custom:company-review` rather than overloading the core names.

The main migration risk is breaking the two current users of legacy `workspace.hooks` without a clear path forward. Countermeasure: detect direct `workspace.hooks` during config loading and return a targeted error that names the unsupported key and points to `docs/runbooks/workspace-driver-migration.md`. Update `doctor` so an operator can run one command and see the same migration guidance without digging through a stack trace. Include before/after examples for the current dogfood jj hooks and for a no-op profile.

The main prompt/runtime risk is exposing driver information to command steps but not to agents. Countermeasure: add driver/profile/capability information both to command-step environment variables and to agent prompt template locals. If Scherzo also wants agents to see the variables in shell commands launched through pi, a child plan must explicitly thread a step environment into pi launch for both shell and argv launch modes; otherwise prompts must include the driver command text directly.

The main recovery/fingerprint risk is changing workspace profile semantics without invalidating incompatible recovered runs. Countermeasure: include selected driver command and capabilities in execution fingerprints once they affect workflow execution. Keep unselected profiles out of the fingerprint, matching the existing selected-profile behavior.

The main security risk is that driver commands are shell configured by operators and can perform destructive actions. Countermeasure: keep drivers in orchestrator config, never allow workflow YAML to define driver shell, and document that drivers are trusted like lifecycle hooks. Workflow prompts should tell agents to use the configured driver, but Scherzo should not execute arbitrary driver capability subcommands automatically beyond deliberate validation/probe steps.

The main scope risk is trying to convert all jj-heavy dogfood scripts in one pass. Countermeasure: start with driver-backed named profiles and the portable research workflow. Convert implementation, ExecPlan, ExecPlan revision, and merge-conflict workflows through later child plans after the driver contract has passed real use in a small workflow.

## Progress

- [x] (2026-05-09 00:00Z) Identified that `workflow:research` is mostly portable but currently contains Linear and jj wording.
- [x] (2026-05-09 00:00Z) Identified that jj is a shared dogfood substrate across global workspace hooks, many prompts, and helper scripts.
- [x] (2026-05-09 00:00Z) Filed LIV-168 for migrating dogfood off legacy direct `workspace.hooks` and onto named workspace profiles.
- [x] (2026-05-09 00:00Z) Wrote this umbrella design plan with the initial profile/helper/capability architecture and child ExecPlan breakdown.
- [x] (2026-05-09 00:00Z) Revised the umbrella toward a single workspace driver command that owns both lifecycle operations and workflow-facing capabilities.
- [x] (2026-05-09 00:00Z) Revised the migration stance to prefer detecting legacy direct `workspace.hooks` and pointing operators to a migration guide instead of preserving a long-lived compatibility path.
- [ ] Review this umbrella with the operator and adjust vocabulary, capability names, and child plan sequencing.
- [ ] File or update Linear planning tickets for the child ExecPlans once the umbrella vocabulary is accepted.
- [ ] Write the first child ExecPlan for LIV-168 dogfood named profiles or driver profiles.
- [ ] Write the child ExecPlan for core driver and capability schema support.
- [ ] Write the child ExecPlan for the initial driver contract and built-in jj/noop adapters.
- [ ] Write the child ExecPlan for a portable research workflow example.

## Surprises & Discoveries

- Observation: Scherzo already has most of the profile-selection foundation needed for this design.
  Evidence: `src/scherzo/workflow_dag.gleam` defines `WorkflowDag.workspace_profile`; `src/scherzo/config.gleam` parses `workspace.profiles`; `src/scherzo/runtime_bundle.gleam` validates selected profiles during bundle loading; `src/scherzo/workspace_profile.gleam` resolves the selected profile and reports unknown-profile errors.

- Observation: The checked-in dogfood config still uses the legacy compatibility path even though named profiles exist.
  Evidence: `.scherzo/scherzo.yaml` contains direct `workspace.hooks` and none of `.scherzo/workflows/*.yaml` currently contains a `workspace_profile` field.

- Observation: The reusable examples already demonstrate named profiles, including a no-op profile.
  Evidence: `examples/scherzo.yaml` defines `workspace.default_profile: isolated` and profiles named `isolated` and `noop`; `examples/workflows/research.yaml` selects `workspace_profile: noop`.

- Observation: Hook environment already includes the selected workspace profile, but command-step environment does not.
  Evidence: `src/scherzo/workspace_run.gleam` adds `SCHERZO_WORKSPACE_PROFILE` in `base_hook_env`; `src/scherzo/workflow_run.gleam` builds `step_command_env` with workflow, run, issue, attempt, and workspace path fields but not `SCHERZO_WORKSPACE_PROFILE`, a driver command, or capabilities.

- Observation: Prompt rendering can carry additional top-level local variables without changing the template language.
  Evidence: `src/scherzo/template.gleam` checks `context.locals` before built-in issue and scheduled variables, and `src/scherzo/workflow_run.gleam` currently passes only `step_artifact.to_template_locals(artifacts)` for workflow agent prompts.

## Decision Log

- Decision: Use workspace profiles as the top-level abstraction, with one workspace driver command as the preferred profile implementation.
  Rationale: Profiles are already the Scherzo mechanism for choosing workspace policy, and they keep trusted shell in operator config rather than workflow YAML. A single driver command can own both lifecycle operations and workflow-facing capabilities.
  Date: 2026-05-09

- Decision: Use `workspace driver` as the concept instead of `vcs` or `workspace helper`.
  Rationale: Workflows need workspace operations and change lifecycle operations, not a hard dependency on a version-control system. A no-op or artifact-only profile should be first-class, and the driver concept covers both Scherzo-invoked lifecycle commands and workflow-invoked capability commands.
  Date: 2026-05-09

- Decision: Use the capability name `publish-change` instead of `publish-pr`.
  Rationale: Pull requests are a GitHub- and GitLab-adjacent review model, while Scherzo should also fit merge requests, Gerrit changes, Perforce shelves, direct branch publication, artifact publication, or internal review systems. The driver result can still report a provider-specific kind such as pull request when that is what happened.
  Date: 2026-05-09

- Decision: Prefer a breaking migration from legacy direct `workspace.hooks` to profile drivers instead of a long-lived compatibility shim.
  Rationale: Scherzo currently has a very small user base, and maintaining both legacy hooks and driver profiles would complicate docs, fingerprints, recovery semantics, and public examples. A clear error plus migration guide is safer and easier to support.
  Date: 2026-05-09

- Decision: Use explicit config-version or legacy-shape detection to point operators at the migration guide.
  Rationale: If the orchestrator config `version` is bumped for driver profiles, `version: 1` configs can fail with a migration-specific diagnostic. If the version is not bumped, the parser must still detect direct `workspace.hooks` before producing generic parse errors. In either case, `doctor` should surface the same `docs/runbooks/workspace-driver-migration.md` pointer.
  Date: 2026-05-09

- Decision: Prefer a single workspace driver command over separate helper and hook command concepts for new profiles.
  Rationale: Lifecycle hooks and helper operations are both commands operating on the same prepared workspace. A driver can expose lifecycle subcommands for Scherzo and capability subcommands for workflows. This reduces duplicated configuration and makes packaged profile adapters easier to share.
  Date: 2026-05-09

- Decision: Do not use a plural `helpers` map for the first design.
  Rationale: A profile should expose one command endpoint, analogous to one tool with subcommands. If a future profile needs several underlying implementations, the first choice should be one dispatcher driver command with a larger capability set rather than a plural helper map.
  Date: 2026-05-09

- Decision: Let workflow YAML declare `workspace_capabilities`, but not a driver command.
  Rationale: Required capabilities are safe declarative metadata. Driver commands are trusted shell and belong in orchestrator config.
  Date: 2026-05-09

- Decision: Start with research and dogfood profile migration before converting implementation/publish workflows.
  Rationale: Research needs only a small `assert-only` capability and will validate the architecture with less risk than refactoring jj-heavy publish scripts.
  Date: 2026-05-09

## Outcomes & Retrospective

This umbrella has not been implemented yet. It establishes the proposed vocabulary and divides the work into child ExecPlans. LIV-168 already exists for the prerequisite dogfood migration from legacy direct hooks to named workspace profiles; that ticket may be implemented as a temporary named-profile migration or refined to target final driver-backed profiles once the driver schema is planned. The next outcome should be an operator-approved vocabulary and a set of Linear tickets for the remaining child ExecPlans.

## Context and Orientation

Scherzo is a Gleam application that polls a tracker, dispatches issues into workflow DAGs, runs command and agent steps in prepared workspaces, and reports results back through handoff. A workflow DAG is a YAML file under `.scherzo/workflows/` or `examples/workflows/`. A workspace is a filesystem directory where a workflow step runs. A workspace profile is an operator-defined policy for preparing and cleaning those directories. A workspace driver is a trusted command configured in the orchestrator config; Scherzo invokes it for lifecycle operations, and workflow steps can invoke it for declared capabilities inside the prepared workspace. A workspace capability is a named operation that a driver promises to support and a workflow may require.

The current config and parsing code is spread across several files. `src/scherzo/config/types.gleam` defines `DagHooksConfig`, `WorkspaceHookProfile`, and `WorkspaceHookProfiles`. `src/scherzo/config.gleam` parses `workspace.hooks`, `workspace.profiles`, `workspace.default_profile`, and validates profile names. `src/scherzo/workflow_dag.gleam` parses workflow files, including the optional top-level `workspace_profile` selector. `src/scherzo/workspace_profile.gleam` resolves the selected profile for a workflow. `src/scherzo/runtime_bundle.gleam` loads workflows and rejects unknown selected profiles. `src/scherzo/workflow_fingerprint.gleam` hashes workflow definitions and selected profile hook bodies into execution fingerprints. `src/scherzo/workspace_run.gleam` prepares workspaces and runs lifecycle hooks. `src/scherzo/workflow_run.gleam` executes workflow steps, prepares command-step environments, and renders agent prompts with step artifact locals.

The checked-in dogfood workflow config lives in `.scherzo/scherzo.yaml`. It still uses direct `workspace.hooks` and calls `scripts/scherzo-jj-workspace` for create, before-step, and remove behavior. The dogfood workflows are `.scherzo/workflows/research.yaml`, `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, and `.scherzo/workflows/merge-conflict-resolution.yaml`. Their prompts live under `.scherzo/workflows/prompts/`. Most prompts mention jj directly. The reusable examples live under `examples/`, and `examples/scherzo.yaml` already demonstrates named profiles.

## Preconditions and Verified Facts

The current repository has named workspace profile support, but the type names still call the profile a hook profile. `src/scherzo/config/types.gleam` defines `WorkspaceHookProfile(name, hooks, source)` and `WorkspaceHookProfiles(default_profile, profiles)`. It does not yet contain driver command or capability fields.

The current workflow DAG parser supports `workspace_profile` but not `workspace_capabilities`. `src/scherzo/workflow_dag.gleam` defines `WorkflowDag(id, description, workspace_profile, max_parallel_steps, steps)`. Tests in `test/workflow_dag_test.gleam` cover parsing top-level `workspace_profile`, rejecting invalid profile names, and rejecting step-level `workspace_profile`.

The current runtime bundle validates unknown selected profiles. `src/scherzo/runtime_bundle.gleam` calls `validate_workspace_profiles`, which uses `workspace_profile.resolve` and reports `unknown_workspace_profile` if the selected profile does not exist.

The current execution fingerprint includes the selected configured profile and hook bodies. `src/scherzo/workflow_fingerprint.gleam` includes explicit `workspace_profile` in the DAG canonical input when present, and includes selected profile metadata and `dag_hooks` for configured profiles. Unselected profiles are not part of the execution fingerprint.

The current hook environment contains `SCHERZO_WORKSPACE_PROFILE`. The current command-step environment in `src/scherzo/workflow_run.gleam` does not include `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, or a capability list. Agent prompt rendering does not include workspace driver locals.

The current examples already contain a no-op profile and a research workflow selecting it. `examples/scherzo.yaml` defines profiles named `isolated` and `noop`; `examples/workflows/research.yaml` uses `workspace_profile: noop`, but that example is much thinner than the dogfood research workflow and does not enforce a `research-findings.md` artifact contract.

The repository's normal validation commands should be run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. This is an environment setup issue, not a code failure.

## Scope Boundaries

This umbrella is in scope for the architecture, vocabulary, sequencing, and child-plan decomposition. It is not in scope for implementing the parser, runtime, scripts, docs, or workflow changes directly.

The first implementation wave should include profile driver schema support, legacy direct-hook detection with migration guidance, a small driver command contract, jj-backed and no-op drivers, dogfood migration to driver-backed profiles, driver and capability exposure to steps, and a portable research workflow. It should not attempt to convert every implementation, ExecPlan, change-publication, or merge-conflict workflow in the same change.

The existing direct `workspace.hooks` compatibility path should not remain valid after the driver migration lands. Scherzo should detect it as an old config shape and point to `docs/runbooks/workspace-driver-migration.md`. Adding step-level profiles or step-level capability overrides is deferred. Adding a fully generic publishing abstraction for every hosted code review system is deferred.

## Child ExecPlans to Produce

Child Plan 1 is the core schema, migration, and validation plan for workspace drivers and capabilities. It should rename or wrap the internal `WorkspaceHookProfile` concept into a broader workspace profile model, finalize the `driver` YAML spelling, parse `workspace.profiles.<name>.driver.command`, parse driver lifecycle support and capabilities, parse workflow-level `workspace_capabilities`, validate that a workflow's selected profile provides all required capabilities, and update execution fingerprints to include the selected driver command and capabilities when configured. It should also detect legacy direct `workspace.hooks`, return an actionable error, update `doctor` to surface the migration, and add `docs/runbooks/workspace-driver-migration.md`. This plan should include config parser tests, workflow parser tests, runtime bundle tests, doctor tests, and fingerprint tests.

Child Plan 2 is the initial workspace driver command contract and adapters plan. It should define the first fixed lifecycle command shapes and capability command shapes, probably lifecycle operations `create`, `before-step`, `after-step`, and `remove`, plus capabilities `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-change`. It should implement a dogfood jj driver script, likely `scripts/scherzo-workspace-jj`, that delegates to existing jj operations where safe. It should implement a no-op or artifact-only driver for workflows that only need `assert-only`. It should include contract tests showing consistent exit codes, JSON output where applicable, human output where applicable, and secret-safe diagnostics.

Child Plan 3 is the dogfood migration plan. It is already represented by Linear issue LIV-168, titled "Switch dogfood workflows to named workspace profiles". It should be refined to target driver-backed profiles once Child Plans 1 and 2 exist. This plan should replace direct `.scherzo/scherzo.yaml` `workspace.hooks` with named `workspace.profiles` that use the jj driver, and make checked-in workflows either select that profile explicitly or intentionally rely on a documented named default.

Child Plan 4 is the runtime exposure plan. It should add `SCHERZO_WORKSPACE_PROFILE`, `SCHERZO_WORKSPACE_DRIVER`, and `SCHERZO_WORKSPACE_CAPABILITIES` to command-step environments. It should also expose prompt locals such as `workspace.profile`, `workspace.driver`, and `workspace.capabilities` so agent prompts can instruct agents without hardcoding jj. If the design chooses to put these variables into the actual pi process environment, this child plan must thread step-specific environment into pi launch for both shell-command and argv-command pi launch paths and test both persistent and non-persistent launch modes.

Child Plan 5 is the portable research workflow plan. It should produce a shareable research workflow and prompt that use "issue" instead of "Linear issue", say "dedicated workflow workspace" instead of "dedicated jj workspace", require only `assert-only`, write `research-findings.md`, collect the artifact through `$SCHERZO_WORKSPACE_DRIVER assert-only --path research-findings.md`, and document how a third-party Scherzo config can run it with a no-op, clone, worktree, or jj profile.

Child Plan 6 is the dogfood workflow conversion plan for implementation-like workflows. It should examine `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, and `.scherzo/workflows/merge-conflict-resolution.yaml`, then decide which jj-heavy operations stay in repo-local scripts and which move behind driver capabilities. This should be a later plan because these workflows depend on diffing, base refresh, publication, review acknowledgement, and conflict semantics.

Child Plan 7 is the documentation plan. It should update `README.md`, `.scherzo/README.md`, `examples/scherzo.yaml`, `docs/runbooks/workspace-driver-migration.md`, and any relevant runbooks so the primary public model is workspace profiles with drivers and capabilities. It should clearly label direct `workspace.hooks` as an old config shape that now produces migration guidance.

## Milestones

The first milestone is to add driver schema, validation, and migration diagnostics. At the end, Scherzo can parse driver-backed profiles, parse workflow-required capabilities, reject incompatible workflows before dispatch, include selected driver metadata in execution fingerprints, and reject legacy direct `workspace.hooks` with a clear pointer to `docs/runbooks/workspace-driver-migration.md`.

The second milestone is to implement the first driver adapters. At the end, a no-op/artifact driver supports `assert-only`, and a jj driver supports lifecycle operations plus the capabilities needed by current dogfood or the next migration. Contract tests prove driver behavior outside a full daemon run.

The third milestone is to normalize dogfood onto driver-backed named profiles. At the end, `.scherzo/scherzo.yaml` no longer uses direct legacy `workspace.hooks`, and every checked-in dogfood workflow either names its profile or relies on a documented named default.

The fourth milestone is to expose driver information to steps. At the end, command steps can use `$SCHERZO_WORKSPACE_DRIVER`, and prompts can render the selected driver and capability list. A test workflow can print those values from a command step and include them in a rendered prompt without depending on jj.

The fifth milestone is to ship a portable research workflow. At the end, a third-party operator can copy the workflow and prompt, provide a profile with `assert-only`, and get a validated `research-findings.md` result without editing any jj-specific prompt text.

The sixth milestone is to plan the heavier workflow conversions. At the end, Scherzo has separate ExecPlans for the implementation and publish workflow family rather than a vague intent to make everything generic.

## Plan of Work

The umbrella work itself is to keep this plan accurate, review the vocabulary with the operator, and file child ExecPlan tickets. No production code should change as part of this umbrella unless a child plan is being executed.

When Child Plan 1 is written, it should touch `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workspace_profile.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/workflow_fingerprint.gleam`, the doctor command path, and `docs/runbooks/workspace-driver-migration.md`. It should add tests under the existing relevant test files: `test/workflow_dag_test.gleam`, config parser tests if present, runtime bundle tests, doctor tests, and `test/workflow_fingerprint_test.gleam`.

When Child Plan 2 is written, it should add scripts under `scripts/` and tests around driver behavior. The plan should decide whether tests live as shell tests, Gleam tests invoking driver scripts, or local integration tests. It should keep the driver protocol small and avoid adding operations that no current child workflow uses.

When Child Plan 3 is written, it should focus on `.scherzo/scherzo.yaml`, `.scherzo/workflows/*.yaml`, `.scherzo/README.md`, and config loading validation. It should not broaden into implementation workflow semantics beyond selecting the new driver-backed profile.

When Child Plan 4 is written, it should touch `src/scherzo/workflow_run.gleam` for command-step environment and prompt locals. If pi process environment support is included, it should also touch `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/pi/client.gleam`, `src/scherzo/pi/command.gleam`, and tests for shell and argv launch modes.

When Child Plan 5 is written, it should update or add files under `examples/workflows/`, likely using the richer dogfood research artifact contract as the source material. If the dogfood research workflow is changed too, the plan must account for `.scherzo/workflows/research.yaml` and `.scherzo/workflows/prompts/research.md` separately from the public example.

## Concrete Steps

From the repository root, first review this umbrella:

    read docs/plans/workspace-profile-helper-capabilities-umbrella.md

Check the current dogfood and example workspace profile state:

    grep -R "workspace_profile\|workspace:" -n .scherzo/scherzo.yaml .scherzo/workflows examples/scherzo.yaml examples/workflows

The expected current result before Child Plan 1 is that `.scherzo/scherzo.yaml` has direct `workspace.hooks`, examples use named profiles, and dogfood workflow files do not yet select profiles.

After the operator accepts the vocabulary, create or update Linear tickets for Child Plans 2 through 7. Use `direnv exec . linear issue create` from the repository root, choose the LIV team, the Scherzo project, Backlog state, and `workflow:execplan` label for tickets that should produce child ExecPlans. LIV-168 already exists for Child Plan 1.

Write Child Plan 1 at a new `docs/plans/` path for core driver schema, legacy detection, doctor guidance, and migration documentation. Validate that plan with the ExecPlan review skill before implementation.

Refine LIV-168 after Child Plan 1 is reviewed. If the issue remains the dogfood migration ticket, write its child plan at a path such as `docs/plans/LIV-168-dogfood-workspace-driver-profiles.md` and make it depend on the driver schema and adapter facts that actually exist.

Repeat for the remaining child plans only after the preceding plan's vocabulary and interfaces are stable. Each child plan must be self-contained and must not rely on this umbrella alone for implementation details.

Commit points for this umbrella are simple. Commit this umbrella file by itself. Later child ExecPlans should each be committed separately after their own review and validation.

## Testing and Falsifiability

This umbrella is falsified if, after reading the current code, a child plan cannot be written without inventing missing core concepts, or if a supposed portable workflow still has to mention jj or Linear to complete a research artifact. It is also falsified if driver/capability compatibility cannot be validated before dispatch with the current runtime bundle architecture; Child Plan 1 must test that assumption early.

Child Plan 1 should add parser and validation tests with these scenarios: a profile with driver command and capabilities parses successfully; `workspace_capabilities` parses on a workflow; an unknown capability name fails if the design uses a fixed vocabulary; a workflow requiring `assert-only` fails when the selected profile lacks it; a workflow requiring `assert-only` succeeds when the selected profile provides it; changing the selected driver command or capability list changes the execution fingerprint; changing an unselected profile does not change the fingerprint; direct legacy `workspace.hooks` fails with an error that points to `docs/runbooks/workspace-driver-migration.md`; doctor surfaces the same guidance.

Child Plan 2 should add driver contract tests. For `assert-only`, create a temporary workspace with only the allowed file changed and expect exit 0. Create another temporary workspace with an extra changed file and expect nonzero exit plus a clear diagnostic naming the unexpected path. For `changed-files`, assert stable JSON output. For `status` and `diff`, assert nonempty human-readable output in a workspace with changes. For lifecycle operations, assert that the no-op driver creates and removes a workspace directory and that the jj driver can prepare and verify a local test workspace.

Child Plan 3 should include tests or validation commands proving that `.scherzo/scherzo.yaml` loads with driver-backed named profiles and checked-in workflows resolve their selected profile. The expected validation includes `direnv exec . gleam test` and any existing config or runtime bundle tests relevant to loading workflows.

Child Plan 4 should add runtime tests showing that command steps receive the driver variables and that prompt templates can render workspace driver locals. If pi process environment support is included, tests must prove both shell and argv pi launches receive the intended step environment without leaking variables between unrelated steps.

Child Plan 5 should validate a portable research run with a no-op or artifact driver. The test should prove that `research-findings.md` is emitted and that an unexpected second file makes the collection step fail.

Full validation for implementation child plans should use the repository's standard commands:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

## Validation and Acceptance

This umbrella is accepted when it is checked in under `docs/plans/`, the operator agrees on the vocabulary of workspace profile, workspace driver, and workspace capabilities, and there is a clear child ExecPlan sequence that proceeds through core schema and migration diagnostics, adapters, dogfood migration, runtime exposure, portable research, heavier workflow conversion, and documentation.

The full program is accepted when a new repository can configure a named workspace profile with a driver that provides `assert-only`, copy a portable research workflow and prompt from this repository, label an issue for that workflow, and complete a run that writes and reports `research-findings.md` without jj-specific or Linear-specific workflow text. A later acceptance target is that Scherzo's implementation-style workflows no longer instruct agents to use `jj status` directly and instead rely on workspace driver operations where appropriate.

## Rollout, Recovery, and Idempotence

Roll this out as a deliberate breaking migration with guardrails. First add driver schema support, legacy direct-hook detection, doctor guidance, and the migration runbook. Then add driver adapters. Then migrate dogfood to driver-backed named profiles. Then expose driver variables and prompt locals. Then update one low-risk workflow, research, to require and use `assert-only`. Only after that should heavier implementation and publish workflows be converted.

Rollback for the dogfood migration is to restore the previous Scherzo version and previous `.scherzo/scherzo.yaml`; once the breaking migration lands, direct `workspace.hooks` should produce migration guidance rather than continuing to load. Rollback for portable research is to restore the previous dogfood research workflow and prompt. The migration guide should include a small manual rollback note for the two current users.

All child plans must be safe for interrupted implementation. If a child plan changes config schema or fingerprint behavior, it must document how running workflow recovery behaves when the config changes. If a child plan adds scripts, rerunning the plan should either overwrite the same scripts deterministically or report that the expected files already exist.

## Artifacts and Notes

The first child ticket already filed is:

    LIV-168: Switch dogfood workflows to named workspace profiles

The initial capability names proposed by this umbrella are:

    status
    diff
    changed-files
    assert-only
    baseline
    refresh-base
    publish-change

These names are intentionally workspace-operation names, not VCS names. `publish-change` means making the completed workspace change visible outside the local workflow workspace and returning a structured result that can describe a pull request, merge request, Gerrit change, pushed branch, uploaded artifact, internal review item, or other provider-specific publication. A jj driver, git driver, no-op driver, or internal company driver can implement the same capability names differently.

The first driver environment and template names proposed by this umbrella are:

    SCHERZO_WORKSPACE_PROFILE
    SCHERZO_WORKSPACE_DRIVER
    SCHERZO_WORKSPACE_CAPABILITIES
    workspace.profile
    workspace.driver
    workspace.capabilities

The exact names can still change during review, but child plans should keep one clear mapping between config, environment, and prompt locals.

## Interfaces and Dependencies

The future profile model should conceptually look like this, even if the first implementation keeps some current type names temporarily:

    pub type WorkspaceProfile {
      WorkspaceProfile(
        name: String,
        driver: WorkspaceDriverConfig,
        source: WorkspaceProfileSource,
      )
    }

    pub type WorkspaceDriverConfig {
      WorkspaceDriverConfig(
        command: String,
        lifecycle: List(String),
        capabilities: List(String),
        timeout_ms: Int,
      )
    }

The external YAML should prefer one source of truth for capabilities, likely `workspace.profiles.<name>.driver.capabilities`. Lifecycle support should live beside it as `workspace.profiles.<name>.driver.lifecycle`.

The workflow DAG model should gain a field equivalent to:

    workspace_capabilities: List(String)

A workflow requiring no driver operations should omit the field or use an empty list. A workflow selecting a profile that lacks any required capability should fail during runtime bundle loading, before issue dispatch.

The driver command contract should be executable from the workspace root. It should receive the normal step environment. It should use exit code 0 for success and nonzero for failure. It should print machine-readable JSON for commands that advertise `--json`. It should print bounded, human-readable diagnostics on stderr when failing. It must not print secrets. The contract must be documented before implementation workflows depend on it.

The following repository files and commands are dependencies for child plans:

- `src/scherzo/config/types.gleam` for profile and driver config types.
- `src/scherzo/config.gleam` for YAML parsing and validation.
- `src/scherzo/workflow_dag.gleam` for workflow-level `workspace_capabilities` parsing.
- `src/scherzo/workspace_profile.gleam` for selected profile resolution and compatibility checks.
- `src/scherzo/runtime_bundle.gleam` for pre-dispatch workflow/profile validation.
- `src/scherzo/workflow_fingerprint.gleam` for execution fingerprint changes.
- `src/scherzo/workspace_run.gleam` for lifecycle hook environment and prepared workspace profile fields.
- `src/scherzo/workflow_run.gleam` for command-step environment and prompt locals.
- `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/pi/client.gleam`, and `src/scherzo/pi/command.gleam` if pi process environment support is included.
- `.scherzo/scherzo.yaml` and `.scherzo/workflows/*.yaml` for dogfood migration.
- `examples/scherzo.yaml` and `examples/workflows/` for portable examples.
- `scripts/scherzo-jj-workspace` as the current jj lifecycle helper that a future `scripts/scherzo-workspace-jj` driver can reuse or wrap.
