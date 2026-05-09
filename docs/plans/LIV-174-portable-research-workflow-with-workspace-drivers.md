# Ship a portable research workflow with workspace driver capabilities

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, a Scherzo operator can copy the research workflow and prompt from this repository into another repository, provide a workspace profile whose driver supports one capability named `assert-only`, and run research issues without editing jj-specific or Linear-specific text. The workflow will ask an agent to write exactly one Markdown report named `research-findings.md`; a follow-up command step will ask the configured workspace driver to prove that `research-findings.md` is the only produced artifact, then stream that file as the workflow result.

This plan is the portable research child of the workspace driver umbrella. It does not define the whole workspace driver system. It assumes the core driver schema, driver adapters, and runtime exposure described in this plan's Preconditions already exist, and it uses them to make one low-risk workflow portable.

## Problem Framing and Constraints

The current checked-in dogfood research workflow is useful but not portable. Its YAML description says the result is a Linear artifact, its prompt says the agent is in a dedicated jj workspace, and its collection command shells out to `jj diff` and `jj abandon`. The current public example research workflow is portable in one narrow sense because it uses `workspace_profile: noop`, but it is too thin to be a reusable workflow package: it has only one agent step, does not require `research-findings.md`, does not collect a terminal artifact, and its prompt still says "Linear issue".

The operator pain is concrete: a third-party repository that does not use jj should be able to adopt the research workflow without rewriting the workflow contract. The workflow should talk about Scherzo concepts, not about one repository's version-control implementation. It should still protect operators from stray edits by requiring the workspace driver to support `assert-only` and by validating that requirement before dispatch.

The main constraint is sequencing. This plan must not be implemented before the driver foundation exists. The implementation depends on these repository capabilities being present: workflow DAGs can declare `workspace_capabilities`, workspace profiles can declare a `driver` with `capabilities`, runtime bundle loading rejects workflows whose selected profile lacks a required capability, command steps receive `SCHERZO_WORKSPACE_DRIVER` as a single executable path or runtime-provided wrapper shim with no embedded shell arguments, and an artifact-only or no-op driver supports `assert-only --path <relative-path>`. The collection command must be able to invoke the driver safely as `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"`. If the landed platform exposes drivers as arbitrary shell command strings, argv arrays without a shell-safe shim, or any shape that cannot be quoted this way, stop this plan and finish the prerequisite driver plan first.

This plan intentionally does not convert the heavier implementation, ExecPlan, merge-conflict, or publish workflows. Those workflows need status, diff, refresh, baseline, and publish semantics, which are larger than the single-artifact research use case.

## Strategy Overview

Use the public example workflow as the portable package and use the dogfood research workflow as a parity proof once dogfood has a driver-backed profile with `assert-only`. The workflow shape is deliberately small: one agent step writes `research-findings.md`; one command step verifies and emits the file. The command step does not know whether the workspace is a jj workspace, a git worktree, a copied directory, an empty artifact workspace, or a container snapshot. It invokes only the configured driver endpoint from `SCHERZO_WORKSPACE_DRIVER`.

The workflow will declare exactly one required capability:

    workspace_capabilities: [assert-only]

The collection command will have this form after Milestone 1 records that `SCHERZO_WORKSPACE_DRIVER` is a single executable path or wrapper shim with no embedded shell arguments:

    set -eu
    findings=research-findings.md
    : "${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required for the research workflow}"
    test -f "$findings"
    "$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"
    cat "$findings"

The quoted driver invocation is intentional. Workflow YAML must not reconstruct argv arrays, eval configured command strings, or depend on shell word-splitting of a driver command. If the runtime cannot provide a safely quoteable driver executable or wrapper, the driver platform is not ready for this workflow.

The prompt will use tracker-neutral language such as "issue" rather than "Linear issue" and workspace-neutral language such as "dedicated workflow workspace" rather than "dedicated jj workspace". The agent will be told not to commit, publish, switch workspaces, or edit files other than `research-findings.md`. The prompt and runbook will also warn that build, test, or analysis commands can create caches or generated files; the agent must avoid such commands, clean up their artifacts before finishing, or explicitly report that a command was skipped because it would violate the one-artifact contract. The driver capability, not prompt discipline alone, is the enforcement mechanism for the one-artifact contract.

This approach is proportionate because it changes only the workflow package, prompt text, example configuration, documentation, and narrowly targeted tests. It does not introduce new source abstractions after the driver platform already exists.

## Alternatives Considered

The simplest alternative is to change only the prompt wording and leave the current `jj diff` fallback in the dogfood collection command. That is insufficient because a workflow with a jj collection step is still not shareable with repositories that do not use jj, and Scherzo cannot validate compatibility before dispatch.

Another alternative is to keep the public example as a one-step research prompt and document that operators should add their own artifact collection. That is insufficient because the artifact contract is the value of this workflow. Without an explicit `research-findings.md` file and a collection step, each adopter must rediscover the same safety checks.

A third alternative is to add a Scherzo built-in `research` command that knows how to collect findings. That is too large for the problem. The workspace driver already provides the needed seam, and a normal workflow command step keeps the behavior visible and editable.

A fourth alternative is to require broader capabilities such as `status` or `changed-files`. That would make adoption harder for no-op and artifact-only profiles. The research workflow only needs to prove that one path is the artifact, so `assert-only` is the smallest useful capability.

## Risks and Countermeasures

The main sequencing risk is implementing this workflow before the driver platform lands. Countermeasure: begin with a prerequisite check against `src/scherzo/workflow_dag.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/workflow_run.gleam`, and the driver scripts. Before editing workflow files, record in the Decision Log the exact landed field name for workflow capabilities, the exact command-step environment variable, the exact capability name and arguments, the exact profile schema fields for driver command and capabilities, the exact repository-relative driver script path used by `examples/scherzo.yaml`, and the guarantee that `SCHERZO_WORKSPACE_DRIVER` is a single quoteable executable path or wrapper shim. If any item is absent, stop and update this ExecPlan rather than adding temporary jj, git, or shell fallbacks.

The main shell-boundary risk is invoking a driver through unsafe shell word-splitting. Countermeasure: require the runtime to expose `SCHERZO_WORKSPACE_DRIVER` as one executable path or wrapper shim with no embedded arguments, and always invoke it as `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"`. If the landed driver runtime uses argv arrays, the prerequisite platform must provide a stable command-step shim; this workflow must not attempt to serialize or parse argv itself.

The main package-compatibility risk is shipping `examples/workflows/research.yaml` and `examples/scherzo.yaml` that look correct independently but cannot be loaded together. Countermeasure: add a package-level test that parses or loads `examples/scherzo.yaml` together with `examples/workflows/research.yaml`, asserts the workflow-selected profile exists, asserts that profile has the landed driver command field, and asserts its capabilities include `assert-only`. If the runtime bundle validator is available, use it to prove the example package is accepted and that a copy with `assert-only` removed is rejected before dispatch.

The main portability risk is accidentally leaving tracker- or VCS-specific wording in prompts, docs, or tests. Countermeasure: add tests that read the workflow and prompt files and assert that the portable example does not contain `Linear`, `jj`, or direct `git` commands, while still containing `issue`, `dedicated workflow workspace`, `research-findings.md`, and `assert-only`.

The main safety risk is a driver whose `assert-only` semantics are weaker than the workflow assumes. Countermeasure: document the required semantics in this plan and in the runbook: `assert-only --path research-findings.md` must exit 0 only when the named relative path exists and is the only workflow-produced change or artifact according to the selected profile's baseline. If a no-op driver runs in an empty workspace, its baseline is empty; if a clone, worktree, or jj driver runs over repository content, its baseline is the prepared repository state before the agent step. The runbook must state whether each adapter counts ignored files, generated caches, and tool metadata, and how an operator or agent should remove unexpected artifacts safely.

The main ordinary-use risk is that read-like research commands can still write files, such as language caches, build directories, downloaded indexes, tool metadata, generated snapshots, or lockfile updates. Countermeasure: the prompt must warn the agent that commands likely to write files should be avoided unless necessary, and any side effects must be cleaned up before the final report is written. If a useful command is skipped because it would produce artifacts, the agent records that in `research-findings.md` under `Issues encountered`. The executable collection-command tests must include a fake driver failure path for an unexpected artifact.

The main dogfood risk is breaking in-progress research runs by changing the workflow fingerprint. Countermeasure: before editing `.scherzo/workflows/research.yaml`, run `scripts/scherzoctl ps --json` from the repository root when a dogfood daemon is available and confirm there are no active sessions for the `research` workflow. If the command is unavailable, the output is ambiguous, or a research run is active, do not migrate dogfood in this change unless the operator explicitly confirms that no dogfood research run is in progress or asks for the migration anyway.

## Progress

- [x] (2026-05-09 00:00Z) Drafted this ExecPlan from LIV-174 and the workspace driver umbrella, after inspecting the current dogfood research workflow, public example workflow, example config, workflow parser, runtime profile validation, command-step environment, and workspace preparation code.
- [x] (2026-05-09 00:15Z) Incorporated adversarial review feedback by closing the driver invocation prerequisite, adding package-level config validation, adding executable collection-command tests, documenting strict `assert-only` side effects, and making dogfood migration depend on an active-run check or operator confirmation.
- [ ] Confirm the driver platform prerequisites exist in the current tree before implementation begins, and record the exact landed driver invocation and profile schema in the Decision Log.
- [ ] Update the public example research workflow and prompt to use `workspace_capabilities: [assert-only]`, a two-step artifact collection flow, side-effect-aware instructions, and tracker/VCS-neutral wording.
- [ ] Update the example Scherzo config and portable research runbook so third-party operators can configure a profile with `assert-only`.
- [ ] Update the dogfood research workflow and prompt to the same portable contract if the dogfood config has a driver-backed profile that provides `assert-only` and no active dogfood research run is in progress.
- [ ] Add tests that parse the portable workflow, validate the example config/workflow package, inspect prompt wording, and execute the collection command with a fake driver.
- [ ] Run the targeted tests and standard repository validation commands.
- [ ] Fill in Outcomes & Retrospective with the observed behavior and any remaining gaps.

## Surprises & Discoveries

- Observation: The dogfood research workflow already has the right high-level two-step shape, but its collection step is jj-specific.
  Evidence: `.scherzo/workflows/research.yaml` has an agent step followed by `collect_findings`; that command tests and cats `research-findings.md`, then uses `jj diff --name-only -r @ --color=never` and `jj abandon @ --color=never`.

- Observation: The public example research workflow selects a no-op profile but does not enforce an artifact contract.
  Evidence: `examples/workflows/research.yaml` has `workspace_profile: noop` and one agent step; it has no `collect_findings` command step and no `workspace_capabilities` field in the inspected tree.

- Observation: The current prompt text is not portable yet.
  Evidence: `.scherzo/workflows/prompts/research.md` says "Linear issue", "dedicated jj workspace", and "Use `jj status --color=never`". `examples/workflows/prompts/research.md` also says "Linear issue" and does not mention `research-findings.md`.

- Observation: The current example config demonstrates named profiles but still uses hook bodies, not the future driver-backed profile shape.
  Evidence: `examples/scherzo.yaml` defines `workspace.default_profile: isolated` and profiles named `isolated` and `noop` under `hooks`.

- Observation: The inspected workflow model in the current tree does not yet contain `workspace_capabilities`.
  Evidence: `src/scherzo/workflow_dag.gleam` defines `WorkflowDag(id, description, workspace_profile, max_parallel_steps, steps)` in the inspected tree.

- Observation: String-inspecting the collection command is not enough to prove the terminal artifact behavior.
  Evidence: The artifact handoff depends on shell ordering, `SCHERZO_WORKSPACE_DRIVER` handling, driver arguments, and failing before `cat` when validation fails; those can only be caught by executing the command with a fake driver.

- Observation: A strict one-artifact workflow can fail even when the agent only intended to research.
  Evidence: Common repository commands may write build caches, downloaded indexes, generated metadata, or lockfile changes, so the prompt and runbook must tell agents and operators how to avoid, clean up, or report those side effects.

## Decision Log

- Decision: Make `examples/workflows/research.yaml` and `examples/workflows/prompts/research.md` the canonical portable workflow package.
  Rationale: Files under `examples/` are intended for reuse outside dogfood, and the existing example already uses a no-op profile name. Making the example complete gives third-party operators a concrete workflow to copy.
  Date: 2026-05-09

- Decision: Use `assert-only` as the only required workspace capability for the research workflow.
  Rationale: The workflow needs to verify one terminal artifact, not inspect status, show diffs, refresh a base, or publish a change. A single required capability minimizes the driver burden for third-party repositories.
  Date: 2026-05-09

- Decision: Require `SCHERZO_WORKSPACE_DRIVER` to be a single executable path or runtime-provided wrapper shim that can be invoked as `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"`.
  Rationale: The collection command is the central integration point. Quoting a single executable path avoids unsafe shell word-splitting and prevents workflow YAML from having to reconstruct argv arrays or eval configured command strings.
  Date: 2026-05-09

- Decision: Promote the exact driver invocation names, profile schema, and reusable driver script path from open questions to pass/fail prerequisites.
  Rationale: A novice implementer should not choose command syntax or invent a driver path while editing workflow files. If the prerequisite platform has not closed those details, this plan must stop rather than ship a fake portable example.
  Date: 2026-05-09

- Decision: Keep artifact collection as an ordinary workflow command step.
  Rationale: A command step is visible in YAML, works with the existing workflow model, and exercises the same driver capability mechanism other workflows will use. A built-in Scherzo artifact collector would be larger and less transparent.
  Date: 2026-05-09

- Decision: Validate `examples/scherzo.yaml` and `examples/workflows/research.yaml` as a package.
  Rationale: The reusable deliverable is not only a YAML workflow or a prompt; it is a copyable example package whose selected profile must exist and provide `assert-only` before dispatch.
  Date: 2026-05-09

- Decision: Execute the `collect_findings` command in tests with a fake driver.
  Rationale: The terminal result can break through shell syntax errors, missing environment handling, wrong driver arguments, or validation running after `cat`; string tests cannot catch those failures.
  Date: 2026-05-09

- Decision: Remove tracker-specific and VCS-specific language from portable prompts.
  Rationale: The workflow should run for any tracker that provides Scherzo issue fields and any workspace driver that satisfies `assert-only`.
  Date: 2026-05-09

- Decision: Make strict `assert-only` side effects part of the prompt and runbook contract.
  Rationale: Research commands often create caches or generated files. Operators need the workflow to fail safely, while agents need instructions to avoid or clean up side effects and to report skipped commands.
  Date: 2026-05-09

- Decision: Treat dogfood research migration as a parity milestone, not as the first proof.
  Rationale: The public example is the reusable artifact. Dogfood should follow once its config has a driver-backed profile and no active research run would be disrupted, but this plan should not block the public package on unrelated heavier dogfood workflow conversions.
  Date: 2026-05-09

- Decision: Use `scripts/scherzoctl ps --json` or explicit operator confirmation as the dogfood active-run gate.
  Rationale: The workflow file fingerprint can affect running sessions. A bounded read-only daemon inspection, or a human operator confirmation when inspection is unavailable, keeps migration safe and avoids guessing.
  Date: 2026-05-09

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam application that loads workflow DAG files, prepares per-run workspaces, runs agent and command steps, and sends workflow results back through a tracker handoff. A workflow DAG is a YAML file. In this repository, dogfood workflows live under `.scherzo/workflows/`, while reusable examples live under `examples/workflows/`.

A workspace is the directory where a workflow step reads and writes files. A workspace profile is operator configuration that says how Scherzo creates, prepares, and removes that workspace. A workspace driver is a trusted local command configured by the operator inside a workspace profile. Scherzo invokes the driver for lifecycle operations, and workflow command steps invoke the same driver for named capabilities. A capability is a driver operation such as `assert-only` that a workflow can require before dispatch.

The portable research workflow has two pieces. The workflow YAML defines the step graph and the artifact collection command. The prompt Markdown tells the agent what to investigate and what file to write. The example Scherzo config shows an operator how to select a workspace profile whose driver supports the capability required by the workflow. Tests should verify the files as a package because a prompt-only or YAML-only change can silently break portability.

The relevant current files are:

- `.scherzo/workflows/research.yaml`, the dogfood research workflow. It currently has a two-step shape but jj-specific collection behavior.
- `.scherzo/workflows/prompts/research.md`, the dogfood research prompt. It currently mentions Linear and jj.
- `examples/workflows/research.yaml`, the public example research workflow. It currently selects `workspace_profile: noop` but lacks artifact collection.
- `examples/workflows/prompts/research.md`, the public example research prompt. It currently mentions Linear and lacks a `research-findings.md` contract.
- `examples/scherzo.yaml`, the public example orchestrator config. It currently demonstrates named hook profiles and will need to demonstrate a driver-backed profile after the driver platform exists.
- `src/scherzo/workflow_dag.gleam`, which defines the parsed workflow fields and must include `workspace_capabilities` before this workflow can require `assert-only`.
- `src/scherzo/runtime_bundle.gleam`, which validates workflow/profile compatibility before dispatch.
- `src/scherzo/workflow_run.gleam`, which builds command-step environments and must provide `SCHERZO_WORKSPACE_DRIVER` before the collection step can run.
- `test/workflow_dag_test.gleam`, `test/runtime_bundle_test.gleam`, and a new `test/portable_research_workflow_test.gleam`, which together should cover parsing, compatibility validation, and the checked-in workflow package.

## Preconditions and Verified Facts

Before implementing this plan, verify the following facts from the current tree. If any fact is false, do not paper over it inside the research workflow; update this ExecPlan and complete the prerequisite driver work first.

First, `src/scherzo/workflow_dag.gleam` must have a `workspace_capabilities: List(String)` field on `WorkflowDag`, and `test/workflow_dag_test.gleam` must already cover parsing `workspace_capabilities: [assert-only]`, rejecting malformed capability values, and preserving an empty list when the field is omitted.

Second, `src/scherzo/config/types.gleam` and `src/scherzo/config.gleam` must support driver-backed workspace profiles. The profile config must include a driver command field and a list of capabilities, and the example config syntax must not use legacy direct `workspace.hooks` for new driver-backed profiles.

Third, `src/scherzo/runtime_bundle.gleam` must reject a workflow requiring `assert-only` when its selected profile does not provide `assert-only`, before any issue is dispatched. The rejection should be an actionable configuration or bundle error, not a failure inside the agent step.

Fourth, `src/scherzo/workflow_run.gleam` must add `SCHERZO_WORKSPACE_DRIVER` to command-step environments. The value must be a single executable path or runtime-provided wrapper shim with no embedded shell arguments, so workflow command steps can safely invoke `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"`. The environment should also include `SCHERZO_WORKSPACE_PROFILE` and `SCHERZO_WORKSPACE_CAPABILITIES` if the runtime exposure plan landed those names, but this workflow's collection step only requires `SCHERZO_WORKSPACE_DRIVER`.

Fifth, a driver adapter must exist for an artifact-only or no-op workspace profile and must implement `assert-only --path <relative-path>`. The adapter may live under `scripts/` or another repository-relative path chosen by the driver adapter plan. This research workflow must not implement its own file-diff logic.

Sixth, `examples/scherzo.yaml` must be able to select a profile compatible with `examples/workflows/research.yaml`. The selected profile, expected to remain `noop` unless the driver platform deliberately renames it, must have the landed driver command field and capabilities including `assert-only`.

Before editing workflow files, add a dated Decision Log entry with the exact landed values for all of these names and surfaces: the workflow capability field, the command-step driver environment variable, the capability operation and arguments, the profile schema fields for driver command and capabilities, the repository-relative reusable driver script path, and the safe invocation guarantee for the driver endpoint. If the runtime exposes argv arrays rather than a shell-safe executable path or wrapper, the driver platform must provide a shim before this plan proceeds.

The current inspected tree has not yet met all of those prerequisites. In the inspected tree, `src/scherzo/workflow_dag.gleam` defines `WorkflowDag(id, description, workspace_profile, max_parallel_steps, steps)`, `src/scherzo/workflow_run.gleam` builds command-step environments without `SCHERZO_WORKSPACE_DRIVER`, and `examples/scherzo.yaml` still shows `hooks` profiles. That is expected for a planning ticket. An implementer should re-check these facts when this plan is picked up later.

The repository's normal validation commands should be run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the direnv-backed command. Treat that as environment setup, not as a product failure.

## Scope Boundaries

In scope:

- Update `examples/workflows/research.yaml` into the canonical portable two-step workflow.
- Update `examples/workflows/prompts/research.md` into the canonical portable prompt and `research-findings.md` report contract.
- Update `examples/scherzo.yaml` only as needed to show a driver-backed profile that provides `assert-only` for the example research workflow.
- Add `docs/runbooks/portable-research-workflow.md` explaining how to adopt the workflow with a no-op, clone/worktree, or jj-backed profile.
- Add tests that read the checked-in workflow and prompt files, validate the example config/workflow package, and execute the `collect_findings` command with a fake driver.
- Update `.scherzo/workflows/research.yaml` and `.scherzo/workflows/prompts/research.md` to the same portable contract if, and only if, `.scherzo/scherzo.yaml` already has a driver-backed selected profile that provides `assert-only` and the active-run gate is clear or the operator explicitly approves.

Out of scope:

- Implementing the driver schema, runtime capability validation, command-step environment exposure, or driver scripts.
- Migrating implementation, ExecPlan, ExecPlan revision, ExecPlan implementation, merge-conflict, or publish workflows.
- Adding status, diff, changed-files, baseline, refresh-base, or publish-change behavior to the research workflow.
- Allowing workflow YAML to define driver commands directly. Driver commands remain trusted operator configuration in Scherzo config, not workflow content.

The boundary for dogfood is intentionally conservative. If dogfood still uses legacy direct `workspace.hooks` when this plan is implemented, leave `.scherzo/workflows/research.yaml` and `.scherzo/workflows/prompts/research.md` unchanged except for a note in Outcomes & Retrospective, and implement only the public example plus docs. If dogfood has a driver-backed default or explicit research profile with `assert-only`, and no active research run would be disrupted, migrate dogfood research in the same PR so the repository's own low-risk workflow exercises the portable contract.

## Milestones

Milestone 1 verifies prerequisites and closes the driver invocation contract. At the end of this milestone, the implementer has confirmed that the driver platform exists, that `SCHERZO_WORKSPACE_DRIVER` can be safely quoted as one executable path or wrapper shim, that the example config can name a real repository-relative driver script, and that this plan's Decision Log records the exact landed names. No workflow files are edited until this milestone passes.

Milestone 2 makes the public example portable. At the end, `examples/workflows/research.yaml` declares `workspace_capabilities: [assert-only]`, has `research` and `collect_findings` steps, uses only `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"` for artifact validation, and its prompt tells the agent to write `research-findings.md` without mentioning Linear, jj, or git. The prompt also tells the agent how to avoid or clean up command side effects that would violate `assert-only`.

Milestone 3 documents adoption. At the end, `examples/scherzo.yaml` shows a compatible driver-backed profile, and `docs/runbooks/portable-research-workflow.md` tells an operator how to copy the workflow and configure a no-op, clone/worktree, or jj-backed driver profile. The runbook explains whether `assert-only` counts ignored files, caches, generated metadata, and how to inspect or remove unexpected artifacts safely. This milestone makes the workflow usable outside this repository.

Milestone 4 adds tests. At the end, tests fail if the portable example loses its `assert-only` requirement, loses the collection step, reintroduces tracker/VCS-specific wording, stops parsing as a valid workflow, becomes incompatible with `examples/scherzo.yaml`, or has a collection command that cannot actually call a driver and stream `research-findings.md`. These tests make portability falsifiable.

Milestone 5 migrates dogfood research when safe. At the end, the checked-in dogfood research workflow follows the same driver-backed artifact contract and no longer contains jj-specific collection logic, provided the dogfood config has an `assert-only` profile and the active-run gate is clear or the operator explicitly approves. If either prerequisite is absent, this milestone is explicitly skipped and recorded.

## Plan of Work

Start by inspecting the landed driver platform. In `src/scherzo/workflow_dag.gleam`, confirm the parsed DAG includes `workspace_capabilities`. In `src/scherzo/config/types.gleam`, confirm workspace profiles can carry a driver command and capability list. In `src/scherzo/runtime_bundle.gleam`, confirm workflow capability requirements are validated against the selected profile. In `src/scherzo/workflow_run.gleam`, confirm command steps receive `SCHERZO_WORKSPACE_DRIVER` as a single executable path or wrapper shim that can be safely quoted. In the driver scripts or adapter docs, confirm the artifact/no-op driver supports `assert-only --path research-findings.md` and identify the real repository-relative script path to use in `examples/scherzo.yaml`. Record all exact landed names and paths in the Decision Log before editing workflow files.

Then update `examples/workflows/research.yaml`. Keep `id: research`, `workspace_profile: noop`, and `max_parallel_steps: 1` unless the driver migration renamed the example profile. If the selected profile name changes, update `examples/scherzo.yaml` and the package-level test consistently and record the reason in the Decision Log. Add `workspace_capabilities: [assert-only]`. Keep the first step as an agent step using `prompts/research.md` in workspace `main`. Add a second terminal command step named `collect_findings`, depending on `research`, in the same workspace. Its command must set `findings=research-findings.md`, require `SCHERZO_WORKSPACE_DRIVER`, test that the file exists, run `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"`, and then `cat "$findings"`. Validation must happen before `cat`.

Update `examples/workflows/prompts/research.md` into the full portable prompt. It should say "You are running Scherzo's portable research workflow for issue {{ issue.identifier }}: {{ issue.title }}." It should include the issue description and labels using the same template fields available to current prompts. It should say the agent is inside a dedicated workflow workspace created by Scherzo. It should forbid creating, switching, publishing, committing, abandoning, or integrating workspaces or changes. It should say the agent may inspect repository files and run commands with the smallest useful scope but must not edit files other than `research-findings.md`. It should warn that commands likely to write caches, build outputs, generated metadata, snapshots, indexes, or lockfile changes should be avoided unless necessary; if such a command is necessary, the agent must clean up all side effects before the final report, or record in `Issues encountered` that it skipped the command because it would violate the one-artifact contract. It should require `research-findings.md` in the workspace root with sections `Brief summary`, `Findings`, `Evidence`, `Issues encountered`, and `Recommendation`. It should tell the agent to write `None` for `Issues encountered` when there are no problems. It should avoid the strings `Linear`, `jj`, and direct `git` instructions.

Update `examples/scherzo.yaml` after checking the driver config shape that landed. The example should have a profile selected by `examples/workflows/research.yaml`, preferably still named `noop`, whose driver capabilities include `assert-only`. Point the example profile's driver command at the real repository-relative script path supplied by the prerequisite driver adapter plan. If no reusable artifact driver exists, record that as a blocker and stop; do not add a placeholder that looks runnable. Do not add legacy direct `workspace.hooks` examples for the portable research workflow.

Add `docs/runbooks/portable-research-workflow.md`. The runbook should define workspace profile, workspace driver, and `assert-only` in plain language. It should show the two files an adopter copies from `examples/workflows/`. It should show a minimal profile whose driver capabilities include `assert-only` and whose driver command is trusted operator configuration. It should explain how `assert-only` behaves for an empty artifact workspace, a copied repository workspace, a git worktree, and a jj workspace without requiring the research workflow to mention those backends. It should explicitly say whether the relevant driver adapters count ignored files, generated caches, build directories, and tool metadata, and it should give safe cleanup guidance for unexpected artifacts. It should state that driver commands are trusted operator config and must not be supplied by third-party workflow YAML.

Add `test/portable_research_workflow_test.gleam`. Use file reads and `workflow_dag.parse` to test the checked-in example workflow and prompt as a package. Add helper functions in that test file to read repository-relative files and to assert that content contains or omits required strings. Keep these tests about workflow package behavior; do not duplicate lower-level driver parser tests already owned by the driver platform plan.

In the same test file, add a package-level test for `examples/scherzo.yaml`. Use the landed runtime bundle loader or config parser, expected to be in `src/scherzo/runtime_bundle.gleam` and `src/scherzo/config.gleam`, to load the example config and workflow together. Assert that the workflow-selected profile exists, that it has the landed driver command field, and that its capabilities include `assert-only`. If the runtime bundle validator exposes capability validation, also build a temporary copy of the example config or workflow with `assert-only` removed from the selected profile and assert that loading fails with the landed missing-capability error before dispatch.

Add executable tests for the `collect_findings` command. In `test/portable_research_workflow_test.gleam`, extract the `run` body from the parsed `collect_findings` step or keep one helper that returns that command from the parsed DAG. Create a temporary workspace under `test/tmp/portable-research-workflow`, write `research-findings.md`, write an executable fake driver script, and run the command with `SCHERZO_WORKSPACE_DRIVER` set to the fake driver path. Use existing test patterns from `test/jj_workspace_hook_test.gleam` for `simplifile`, `command_step.run`, and `chmod +x`. The fake driver should assert it receives exactly `assert-only --path research-findings.md`, record its arguments, exit 0 in the happy path, and exit nonzero when an unexpected artifact file such as `unexpected-artifact.txt` is present. Add negative tests for missing `SCHERZO_WORKSPACE_DRIVER` and missing `research-findings.md`.

If dogfood has a driver-backed profile with `assert-only`, check active runs before changing dogfood files. Run `scripts/scherzoctl ps --json` from the repository root when a daemon is available and confirm there is no active session for workflow `research`. If the command fails because no daemon is running, the control file is unavailable, or the output does not clearly identify active workflows, ask the operator for explicit confirmation before migrating dogfood; otherwise skip dogfood and record the reason. If clear, update `.scherzo/workflows/research.yaml` to the same two-step contract and update `.scherzo/workflows/prompts/research.md` to the same tracker/VCS-neutral, side-effect-aware language. Dogfood may keep dogfood-specific budget text such as a one-turn preference, but it must not say `Linear`, `jj`, or `jj status`, and it must not use `jj diff` or `jj abandon` in the collection step.

Update this ExecPlan's Progress after each milestone. If implementation discovers that the landed driver names differ from `workspace_capabilities`, `SCHERZO_WORKSPACE_DRIVER`, or `assert-only`, stop and update the Decision Log before changing workflow files.

## Concrete Steps

1. From the repository root, inspect source-control state:

       jj status --color=never

   Expect either a clean working copy or only changes made by this implementation. Do not create, switch, finish, or forget workspaces.

2. Verify prerequisites by reading the relevant files:

       src/scherzo/workflow_dag.gleam
       src/scherzo/config/types.gleam
       src/scherzo/config.gleam
       src/scherzo/runtime_bundle.gleam
       src/scherzo/workflow_run.gleam
       examples/scherzo.yaml

   Confirm the preconditions in this plan. Before making workflow edits, add a Decision Log entry recording the exact landed workflow capability field, command-step driver environment variable, capability operation, driver profile schema, reusable driver script path, and the guarantee that the driver endpoint is safely invoked as one quoted executable path or wrapper shim. If `workspace_capabilities`, `SCHERZO_WORKSPACE_DRIVER`, a real `assert-only` driver, or the quoteable invocation guarantee is absent, stop implementation and record the blocker in Surprises & Discoveries.

3. Edit `examples/workflows/research.yaml` so it has this behavior: `workspace_profile: noop`, `workspace_capabilities: [assert-only]`, an agent step named `research`, and a terminal command step named `collect_findings` that depends on `research` and runs the driver-backed artifact check. Preserve repository-relative prompt paths. The command body must be:

       set -eu
       findings=research-findings.md
       : "${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required for the research workflow}"
       test -f "$findings"
       "$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"
       cat "$findings"

4. Edit `examples/workflows/prompts/research.md` to the portable prompt contract described in Plan of Work. Confirm manually that it contains `research-findings.md`, `dedicated workflow workspace`, the required report sections, and instructions to avoid, clean up, or report command side effects. Confirm that it does not contain `Linear`, `jj`, or direct `git` instructions.

5. Edit `examples/scherzo.yaml` to show the landed driver-backed profile shape for the `noop` profile or the selected research profile. Ensure the profile capabilities include `assert-only` and the driver command points at a real repository-relative script path from the prerequisite driver work. Keep other example config unrelated to research unchanged.

6. Create `docs/runbooks/portable-research-workflow.md` with adoption instructions for third-party repositories. Use repository-relative paths only. When discussing local checkout-specific paths, use placeholders such as `<workspace-root>` or `<absolute-local-path>` rather than real machine paths. Include side-effect guidance for ignored files, caches, generated outputs, and cleanup after a failed `assert-only` check.

7. Create `test/portable_research_workflow_test.gleam`. Add `example_research_workflow_is_driver_portable_test` that reads `examples/workflows/research.yaml`, parses it with `workflow_dag.parse`, and asserts:

       dag.id == "research"
       dag.workspace_profile == Some("noop")
       dag.workspace_capabilities == ["assert-only"]
       the step ids are ["research", "collect_findings"] in dependency order
       collect_findings depends on ["research"]
       collect_findings.workspace is WorkspaceRef(name: "main", from: None)
       collect_findings.run contains "SCHERZO_WORKSPACE_DRIVER"
       collect_findings.run contains "assert-only --path"
       collect_findings.run contains "research-findings.md"
       collect_findings.run contains "cat"
       collect_findings.run contains the substring "\"$SCHERZO_WORKSPACE_DRIVER\""
       collect_findings.run does not contain "jj"
       collect_findings.run does not contain "git diff"
       collect_findings.run does not contain "Linear"

8. In the same test file, add `example_research_prompt_is_tracker_and_vcs_neutral_test` that reads `examples/workflows/prompts/research.md` and asserts it contains:

       "issue {{ issue.identifier }}"
       "dedicated workflow workspace"
       "research-findings.md"
       "## Brief summary"
       "## Findings"
       "## Evidence"
       "## Issues encountered"
       "## Recommendation"
       "avoid"
       "clean up"

   Assert the prompt does not contain `Linear`, `jj`, `jj status`, `git status`, `git diff`, or `pull request`.

9. In the same test file, add `example_research_package_profile_supports_assert_only_test`. Load or parse `examples/scherzo.yaml` and `examples/workflows/research.yaml` with the landed runtime bundle or config parser. Assert that the workflow-selected profile exists, has the landed driver command field populated, and advertises `assert-only`. If a bundle-level capability validator is available, add a negative case that removes `assert-only` from a temporary copy of the selected profile and asserts loading fails with the missing-capability error before any workflow run begins.

10. In the same test file, add `collect_findings_command_executes_driver_and_streams_findings_test`. Create `test/tmp/portable-research-workflow/happy`, write `research-findings.md`, write `bin/fake-driver`, mark it executable using the existing `chmod +x` test helper pattern, and run the parsed `collect_findings` command with `SCHERZO_WORKSPACE_DRIVER` set to the fake driver path. The fake driver must fail unless its arguments are exactly `assert-only --path research-findings.md`. Expect the command artifact status to be succeeded, exit code 0, stdout to contain the report body, and the fake driver's argument log to contain the exact received arguments.

11. Add `collect_findings_command_fails_when_driver_rejects_extra_artifact_test`. In a second temporary workspace, write `research-findings.md` and `unexpected-artifact.txt`, run the same command with the same fake driver, and make the fake driver exit nonzero when `unexpected-artifact.txt` exists. Expect the command artifact to fail, the exit code to be nonzero, stderr or diagnostics to mention `unexpected artifact`, and stdout not to contain the findings body after the failed driver check.

12. Add `collect_findings_command_requires_driver_and_findings_file_test`. Run the command once without `SCHERZO_WORKSPACE_DRIVER` and expect failure containing `SCHERZO_WORKSPACE_DRIVER is required for the research workflow`. Run it once with the driver set but without `research-findings.md` and expect failure before the fake driver argument log is written.

13. If dogfood has a driver-backed `assert-only` profile and the active-run gate is clear or the operator approves, edit `.scherzo/workflows/research.yaml` and `.scherzo/workflows/prompts/research.md` to match the portable contract. Then extend `test/portable_research_workflow_test.gleam` with `dogfood_research_workflow_uses_driver_artifact_contract_test`, `dogfood_research_prompt_is_tracker_and_vcs_neutral_test`, and an executable `collect_findings` test against the dogfood command if the command differs. If dogfood is not ready, do not edit dogfood files; instead add a note to Outcomes & Retrospective explaining which prerequisite is missing.

14. Run the targeted test command from the repository root:

        direnv exec . gleam test

    Before the implementation, the new tests should fail because the portable example lacks `workspace_capabilities`, lacks `collect_findings`, the example config lacks a driver-backed `assert-only` profile, the collection command cannot execute with a fake driver, and prompt text still says `Linear`. After the implementation, expect the test suite to pass.

15. Run formatting and lint gates from the repository root:

        direnv exec . gleam format --check src test
        direnv exec . gleam run -m glinter
        direnv exec . gleam run -m scherzo_lint

    Expect all commands to complete successfully. If an existing warning inventory is printed, do not broaden this plan to unrelated cleanup; only fix warnings caused by this change.

16. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective with the actual result. Commit after the full validation is green. Suggested commit content: public portable research workflow, runbook, package and executable command tests, and dogfood parity if included.

## Testing and Falsifiability

The plan is falsified if the workflow still needs jj, git, Linear, or any repository-specific shell to produce its artifact. It is also falsified if Scherzo can dispatch the workflow with a selected profile that lacks `assert-only`, because the operator would then see a runtime failure rather than an early configuration error. It is falsified if the collection command cannot run with the landed driver invocation contract, if it streams `research-findings.md` before driver validation, or if the example config and workflow cannot be loaded together as a package.

Add `test/portable_research_workflow_test.gleam` with file-level tests that make the portability contract explicit. These tests should parse `examples/workflows/research.yaml` with `workflow_dag.parse` and inspect the resulting `WorkflowDag`. The exact assertions are listed in Concrete Steps. The tests should also read prompt Markdown as text and assert required portable language is present while forbidden backend-specific language is absent.

Add a package-level example test. It should load or parse `examples/scherzo.yaml` with the same runtime/config path that Scherzo uses for normal operation, load `examples/workflows/research.yaml`, and assert the selected profile exists, has a driver command, and advertises `assert-only`. If the landed runtime bundle validator exposes a direct validation path, the test should also remove `assert-only` from a temporary profile copy and assert that validation fails with the missing-capability error before dispatch.

Add executable collection-command tests. The happy-path test creates a temporary workspace containing `research-findings.md` and an executable fake driver, sets `SCHERZO_WORKSPACE_DRIVER` to that fake driver, runs the parsed `collect_findings` shell body through `command_step.run`, and asserts success, exact driver arguments `assert-only --path research-findings.md`, and stdout containing the findings content. The failure-path tests prove that an unexpected artifact makes the fake driver reject the command, that an unset driver environment variable fails with the required diagnostic, and that a missing findings file fails before the driver is invoked.

If dogfood research is migrated, add the same assertions for `.scherzo/workflows/research.yaml` and `.scherzo/workflows/prompts/research.md`. If dogfood is not migrated because its profile prerequisites are absent or the active-run gate is not clear, do not add dogfood tests yet; record the skipped milestone in Outcomes & Retrospective.

Do not rely only on negative string tests. Positive assertions must prove the portable contract is present: `workspace_capabilities == ["assert-only"]`, a `collect_findings` terminal step exists, the command invokes `"$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"`, the prompt requires `research-findings.md` with the named sections, the example config's selected profile provides `assert-only`, and the collection command works when executed with a fake driver.

The lower-level driver platform should already have tests for these behaviors before this plan starts: parsing `workspace_capabilities`, parsing driver capabilities in config, rejecting missing capabilities in `runtime_bundle`, injecting `SCHERZO_WORKSPACE_DRIVER`, guaranteeing the driver endpoint is a quoteable executable path or shim, and driver adapter semantics for `assert-only`. If those tests are absent, add or complete them under the prerequisite driver plan, not as hidden setup inside this workflow plan.

Run from the repository root:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Expected result after implementation is that all tests and gates pass. Expected result before implementation, after adding only the new tests, is failure naming the missing `workspace_capabilities`, missing `collect_findings`, incompatible example profile, failed fake-driver invocation, or forbidden prompt strings.

## Validation and Acceptance

Acceptance is observable from the repository files, package validation, and executable tests.

A reviewer can open `examples/workflows/research.yaml` and see a portable two-step workflow requiring only `assert-only`. The reviewer can open `examples/workflows/prompts/research.md` and see instructions for a generic issue and a dedicated workflow workspace, with no dependency on a tracker brand or VCS tool and with explicit warning about command side effects. The reviewer can open `examples/scherzo.yaml` and see that the selected research profile has a driver command and `assert-only` capability. The reviewer can open `docs/runbooks/portable-research-workflow.md` and see how to configure a driver-backed profile for this workflow in another repository, including how strict artifact validation handles caches and generated files.

A test run from the repository root with `direnv exec . gleam test` should pass and include the new portable research workflow tests. The tests should fail if someone later removes `workspace_capabilities: [assert-only]`, removes the collection step, changes the command to call `jj` or `git diff` directly, changes the command to use unquoted `$SCHERZO_WORKSPACE_DRIVER`, reintroduces `Linear` wording in the portable prompt, breaks compatibility between `examples/scherzo.yaml` and `examples/workflows/research.yaml`, or changes the collection command so it no longer executes correctly with a fake driver.

If dogfood research is migrated, `.scherzo/workflows/research.yaml` should also use the driver-backed collection command, and `.scherzo/workflows/prompts/research.md` should say "issue" and "dedicated workflow workspace" rather than "Linear issue" and "dedicated jj workspace". Dogfood migration is accepted only if `scripts/scherzoctl ps --json` showed no active research sessions or the operator explicitly approved migration despite unavailable or ambiguous inspection. If dogfood is not migrated, acceptance requires an explicit Outcomes & Retrospective note explaining that the public portable example shipped while dogfood waits for a driver-backed `assert-only` profile or a safe migration window.

The practical adoption acceptance target is this: in a repository with a Scherzo profile whose driver supports `assert-only`, an operator can copy `examples/workflows/research.yaml` and `examples/workflows/prompts/research.md`, route `workflow:research` issues to the workflow, and receive the contents of `research-findings.md` as the terminal result without editing the workflow to mention that repository's VCS. If the agent or a command produces any additional artifact, the collection step fails before streaming the report and the operator receives a bounded diagnostic from the driver.

## Rollout, Recovery, and Idempotence

This change is additive for public examples and documentation. It does not change stored data, database schema, or tracker state. Rollback for the public package is a normal source revert of `examples/workflows/research.yaml`, `examples/workflows/prompts/research.md`, `examples/scherzo.yaml`, `docs/runbooks/portable-research-workflow.md`, and `test/portable_research_workflow_test.gleam`.

Dogfood rollout is more sensitive because changing `.scherzo/workflows/research.yaml` changes the workflow definition used by running daemons. Before merging dogfood migration, check whether any research workflow run is in progress from the repository root:

    scripts/scherzoctl ps --json

The expected safe result is parseable JSON with no active session whose workflow id or workflow label is `research` or `workflow:research`. If the command fails because no daemon is running, a control file is unavailable, or the output does not clearly show workflow ids, ask the operator to confirm whether dogfood research is idle. If a research run is active, let it finish, abort and retry it after the new profile is available, or skip dogfood migration in this change and record the skip. Do not try to resume an old research run across a workflow fingerprint change.

The collection command is idempotent. Running it repeatedly in the same workspace should keep returning the same `research-findings.md` content as long as the driver still reports that this file is the only produced artifact. If the driver reports extra files, the command fails without modifying the workspace and without streaming the findings body after validation failure.

The runbook should warn adopters that driver commands are trusted operator configuration. A copied third-party workflow may require capabilities, but it must not define the driver command itself. The runbook should also explain safe cleanup after a failed `assert-only` check: inspect the driver's bounded diagnostic, remove only known generated artifacts, rerun the collection step if the workflow system supports retry, or rerun the issue from a clean workspace.

## Artifacts and Notes

The current dogfood collection command shape to remove is jj-specific. It checks the findings file, cats it, then uses jj to inspect and abandon the current change when only `research-findings.md` changed. The portable replacement validates through the driver before streaming the report:

    set -eu
    findings=research-findings.md
    : "${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required for the research workflow}"
    test -f "$findings"
    "$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"
    cat "$findings"

The executable test fake driver can have this shape, adjusted to the repository's test helper style:

    #!/bin/sh
    set -eu
    printf '%s\\n' "$@" > driver-argv.log
    if [ "$#" -ne 3 ] || [ "$1" != "assert-only" ] || [ "$2" != "--path" ] || [ "$3" != "research-findings.md" ]; then
      echo "unexpected driver arguments" >&2
      exit 64
    fi
    if [ -e unexpected-artifact.txt ]; then
      echo "unexpected artifact: unexpected-artifact.txt" >&2
      exit 65
    fi
    exit 0

The future prompt should retain the useful report structure from the dogfood prompt while removing tracker and VCS assumptions and adding the side-effect warning. The report skeleton is:

    # Research findings for {{ issue.identifier }}: {{ issue.title }}

    ## Brief summary

    ## Findings

    ## Evidence

    ## Issues encountered

    ## Recommendation

The exact profile driver command path in `examples/scherzo.yaml` depends on the driver adapter plan that lands before this one. Use the landed repository-relative script path. If no reusable artifact driver exists, record that as a blocker rather than adding a fake working example.

## Interfaces and Dependencies

This plan depends on the following interfaces existing before implementation:

In `src/scherzo/workflow_dag.gleam`, the workflow type must conceptually include:

    pub type WorkflowDag {
      WorkflowDag(
        id: String,
        description: Option(String),
        workspace_profile: Option(String),
        workspace_capabilities: List(String),
        max_parallel_steps: Int,
        steps: List(WorkflowStep),
      )
    }

In workflow YAML, the portable research workflow must use:

    workspace_profile: noop
    workspace_capabilities: [assert-only]

In command-step environments, `src/scherzo/workflow_run.gleam` must provide:

    SCHERZO_WORKSPACE_DRIVER

The value of `SCHERZO_WORKSPACE_DRIVER` must be one executable path or runtime-provided wrapper shim with no embedded shell arguments. Workflow command steps must invoke it with quotes:

    "$SCHERZO_WORKSPACE_DRIVER" assert-only --path "$findings"

The driver command must accept this operation from the workspace root:

    assert-only --path research-findings.md

The operation must exit 0 only when `research-findings.md` is the only produced artifact or change according to the selected profile's baseline. It must exit nonzero with a bounded diagnostic when the file is missing or when any unexpected file is present. It must not print secrets. The driver adapter documentation or runbook must state whether ignored files, build caches, generated metadata, and tool indexes count as unexpected artifacts.

The selected profile in `examples/scherzo.yaml`, expected to be `noop`, must have a driver command field and capabilities list in the landed profile schema. Its capabilities must include `assert-only`, and its driver command must point at a real repository-relative script path supplied by the prerequisite driver work. The package-level test must prove that `examples/scherzo.yaml` and `examples/workflows/research.yaml` load together.

The files to change for the workflow package are:

- `examples/workflows/research.yaml`
- `examples/workflows/prompts/research.md`
- `examples/scherzo.yaml`
- `docs/runbooks/portable-research-workflow.md`
- `test/portable_research_workflow_test.gleam`

The conditional dogfood files are:

- `.scherzo/workflows/research.yaml`
- `.scherzo/workflows/prompts/research.md`

The validation commands are:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

## Open Questions and Clarifications Needed

None.
