# Convert dogfood implementation workflows to workspace driver operations

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo's checked-in implementation-like workflows will stop teaching agents and helper scripts to speak directly to Jujutsu, which this repository calls `jj`, or to GitHub publication commands for ordinary workspace operations. Operators will still be able to dogfood Scherzo in this repository, but workflow definitions and prompts will describe a dedicated workflow workspace and a configured workspace driver rather than a dedicated jj workspace. A workspace driver is a trusted command selected by a workspace profile; it owns common operations such as status, diff, changed-file inventory, baseline identity, base refresh, and publishing the completed workspace change.

The visible result is that the dogfood workflows under `.scherzo/workflows/` declare the workspace capabilities they require, command helpers invoke `$SCHERZO_WORKSPACE_DRIVER` for source-control-shaped operations, prompts tell agents to use the driver for status and diff orientation, and tests prove the converted workflow paths do not depend on raw `jj status`, `jj diff`, `@-`, bookmark, push, or PR-publication commands in the workflow layer. Repository-specific work such as fetching Linear issue context, validating an ExecPlan, interpreting GitHub review comments, running project validation, acknowledging review comments, creating follow-up Linear issues, and writing final handoff text stays in repo-local helper scripts.

## Problem Framing and Constraints

The operator problem is portability and maintainability. Today the implementation, ExecPlan, ExecPlan revision, ExecPlan implementation, and merge-conflict dogfood workflows are useful in this repository, but they leak this repository's jj workflow model into prompts and command arguments. Agents are repeatedly told that they are in a dedicated jj workspace and are told to run `jj status --color=never` or `jj diff --color=never`. Command helpers also do jj-specific changed-file discovery, base refresh, diffing, describing, bookmarking, pushing, and PR publication. This makes the workflows harder to reuse in repositories that use git worktrees, copied directories, container workspaces, or a different publication mechanism.

This plan has an implementation gate, not a fallback implementation. It is runnable only after the workspace-driver foundation exists in the current tree. The required repository facts are listed in the Preconditions section: workflow YAML can declare `workspace_capabilities`; a driver-backed workspace profile can be selected; capability validation happens before dispatch; command steps and agent-step shells receive `SCHERZO_WORKSPACE_DRIVER`; prompts can render workspace driver locals; and the selected dogfood profile uses a driver-backed workspace setup. If any required fact is false, stop this plan and mark it blocked rather than partially converting helper scripts. The plan is self-contained because the gate is expressed as file-level repository facts and exact driver command contracts, not as a dependency on an external document.

The scope is intentionally narrow. The plan does not make every helper script generic, does not remove all uses of `jj` or `gh` from the repository, and does not redesign GitHub or Linear integration. The first goal is to move common workspace operations behind the driver at the workflow boundary while preserving existing dogfood behavior. The checked-in jj driver may still implement those operations with jj and GitHub CLI internally; the important boundary is that workflow YAML, prompts, and converted helper paths call the driver rather than hard-coding those tools.

## Strategy Overview

Use the workspace driver as the boundary between reusable workflow logic and repository-specific source-control mechanics. Add explicit `workspace_profile` and `workspace_capabilities` declarations to the five implementation-like workflow YAML files. Keep the current repo-local scripts as orchestration wrappers, but change the wrappers so they call `$SCHERZO_WORKSPACE_DRIVER` for driver-owned operations instead of invoking `jj` or GitHub publication commands directly. Rewrite prompts to say "dedicated workflow workspace" and to instruct agents to use the configured driver for status and diff orientation.

The division of responsibility is concrete. The driver owns status, diff, changed-file inventory, baseline identity, refreshing the workspace onto the configured base or an explicit target, and publishing the completed workspace change to a hosted review URL. For the dogfood jj driver, that publication may internally create bookmarks, push, and create or update a GitHub PR, but those mechanics stay inside the driver command. The existing scripts keep workflow-specific policy: fetching Linear or GitHub context, extracting a plan path, validating a plan, deciding whether a plan-completion verdict is fresh, generating review prompts and artifacts, deciding whether conflict fallout is mechanical, running project validation, acknowledging GitHub review comments after deterministic helper steps, creating follow-up Linear issues, and formatting final stdout for Scherzo handoff.

This is proportionate because it does not demand a universal workflow platform in one step. The dogfood YAML and prompts become driver-oriented, while the scripts retain specialized business rules. The jj-backed driver remains free to use jj and hosting tools internally, so the operator-visible behavior can remain stable while the workflow interface becomes portable.

## Alternatives Considered

The simplest alternative is to leave these workflows alone and rely on the portable research workflow as the only shareable example. That is insufficient because implementation and ExecPlan workflows are the dogfood workflows most likely to be copied by users who want Scherzo to do code changes. Leaving them jj-specific keeps the most important examples tied to this repository's VCS.

Another alternative is to replace all helper scripts with direct calls to `$SCHERZO_WORKSPACE_DRIVER`. That is too large and would put workflow-specific policy into a generic driver. For example, creating a follow-up Linear issue after an ExecPlan PR and deciding whether a merge-conflict resolution changed a non-conflicted file are not generic workspace operations.

A third alternative is to add git branches or pull-request commands as first-class workflow syntax. That is rejected because the umbrella design deliberately uses workspace operations rather than VCS-specific workflow syntax. Workflows should ask for capabilities such as `publish-change`, not assume GitHub pull requests or jj bookmarks.

A fourth alternative is to leave prompts with direct `jj status` examples until every script is converted. That weakens the outcome. Agents follow prompt text, so prompt migration is part of the user-visible change. The plan instead changes prompts and provides tests that prevent reintroducing raw jj instructions in implementation-like agent prompts.

## Risks and Countermeasures

The main behavioral risk is changing publish or base-refresh semantics while hiding the difference behind a driver call. Countermeasure: this plan closes the command contract before helper conversion. `refresh-base --json` and `publish-change --json` have exact inputs, outputs, success statuses, failure statuses, and ownership boundaries in the Driver Operation Contract section. Helper wrappers preserve current stdout keys such as `PR_URL=...`, preserve existing JSON artifact paths under `tmp/`, and add fake-driver tests that assert wrappers pass the expected arguments and fail closed on driver errors.

The main abstraction risk is moving too much into the driver. Countermeasure: only operations named in the workspace driver capability vocabulary move behind `$SCHERZO_WORKSPACE_DRIVER`. Linear, GitHub feedback normalization, review synthesis, plan validation, conflict policy, project validation, and follow-up issue creation stay in scripts. Publication itself is driver-owned, but workflow-specific publication metadata, such as title and body text, is still assembled by the helper and passed to the driver through repository-relative files.

The main compatibility risk is weakening helper validation when replacing jj-specific changed-file logic. Countermeasure: this plan requires `changed-files --json` to return records with path and status information, not only paths. The contract must include at least `added`, `modified`, `deleted`, `renamed`, and `conflicted` statuses before plan validation or conflict validation is converted.

The main prompt risk is giving agents a driver command that is not actually available in their interactive shell. Countermeasure: the implementation gate requires agent-step shells to inherit `SCHERZO_WORKSPACE_DRIVER`, not merely to render a prompt-local value. The tests include an agent-step or prompt-rendering smoke test that proves the literal `$SCHERZO_WORKSPACE_DRIVER status --human` and `$SCHERZO_WORKSPACE_DRIVER diff --human` examples are usable as written.

The main merge-conflict risk is pretending conflict preparation is a generic workspace operation too early. Countermeasure: keep merge-conflict target discovery, conflict materialization, conflict marker policy, and conflict-specific validation in `scripts/scherzo-merge-conflict`. Convert its ordinary changed-file, status, diff, and publication calls to the driver using the exact capabilities declared by `.scherzo/workflows/merge-conflict-resolution.yaml`. Do not invent a generic conflict-preparation driver capability in this plan.

## Progress

- [x] (2026-05-09 00:00Z) Read the repo-local ExecPlan skill and confirmed this is an authoring task, not an implementation task.
- [x] (2026-05-09 00:00Z) Read the workspace profile driver umbrella and identified this as the umbrella's child plan for heavier dogfood workflow conversion.
- [x] (2026-05-09 00:00Z) Inspected the current workflow YAML files and helper-script surfaces relevant to implementation-like workflows.
- [x] (2026-05-09 00:00Z) Wrote this draft ExecPlan under `docs/plans/`.
- [x] (2026-05-09 00:30Z) Incorporated adversarial review findings by closing driver operation contracts, aligning capability declarations with helper behavior, strengthening prompt and fake-driver validation, and adding a workflow-level smoke test.
- [ ] Verify prerequisites from the workspace-driver foundation in the implementation workspace before making code changes.
- [ ] Add or adjust driver contract support needed by the dogfood helpers, especially changed-file status JSON, diff JSON, baseline JSON, refresh-base JSON, and publish-change JSON.
- [ ] Convert helper scripts incrementally to call `$SCHERZO_WORKSPACE_DRIVER` for driver-owned operations.
- [ ] Update dogfood workflow YAML capability declarations and command arguments.
- [ ] Rewrite implementation-like prompts to use workspace-driver language instead of jj language.
- [ ] Add tests and run the validation commands listed in this plan.

## Surprises & Discoveries

- Observation: The current tree inspected while drafting this plan has no `workspace_capabilities`, `WorkspaceDriver`, `SCHERZO_WORKSPACE_DRIVER`, or `assert-only` matches under `src`, `test`, `.scherzo`, `examples`, or `scripts`.
  Evidence: Repository search for `workspace_capabilities|WorkspaceDriver|SCHERZO_WORKSPACE_DRIVER|workspace_driver|driver:|assert-only` returned no matches.

- Observation: The checked-in dogfood config still uses legacy direct workspace hooks.
  Evidence: `.scherzo/scherzo.yaml` contains `workspace.hooks.create`, `workspace.hooks.before_step`, `workspace.hooks.after_step`, and `workspace.hooks.remove`, each wrapping `scripts/scherzo-jj-workspace` except `after_step`.

- Observation: The five implementation-like workflow YAML files all run repo-local helper scripts rather than embedding most jj commands directly, which makes this conversion feasible as wrapper changes.
  Evidence: `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, and `.scherzo/workflows/merge-conflict-resolution.yaml` call scripts such as `scripts/scherzo-implementation`, `scripts/scherzo-execplan`, `scripts/scherzo-execplan-revision`, `scripts/scherzo-merge-conflict`, and `scripts/scherzo-review`.

- Observation: The implementation-like prompts contain direct jj workspace and status instructions.
  Evidence: Prompt search found wording such as "dedicated jj workspace", `jj status --color=never`, `jj diff --color=never`, and `jj diff --from @- --to @ --name-only --color=never` in files under `.scherzo/workflows/prompts/`.

## Decision Log

- Decision: Treat this plan as gated on concrete workspace-driver repository facts rather than on an external document.
  Rationale: The current tree does not yet expose the driver fields and environment variables required for a safe conversion. A file-level gate lets a future implementer know exactly when to stop without needing prior context.
  Date: 2026-05-09

- Decision: Keep repo-local orchestration scripts, but move common source-control operations inside those scripts to `$SCHERZO_WORKSPACE_DRIVER`.
  Rationale: This preserves existing workflow command names and artifacts while changing the boundary between workflow logic and workspace mechanics.
  Date: 2026-05-09

- Decision: Make `publish-change` driver-owned for source-control and hosted-review publication.
  Rationale: If helpers still call `jj`, `gh pr create`, or `gh pr view` during converted publication paths, the workflow boundary remains tied to this repository's tools. Helpers will assemble title, body, issue metadata, and policy inputs, while the driver creates or updates the branch and hosted review URL and returns structured JSON.
  Date: 2026-05-09

- Decision: Keep Linear issue creation, GitHub review-feedback normalization, deterministic review-comment acknowledgement, staged review synthesis, project validation, and merge-conflict policy out of the driver.
  Rationale: Those behaviors are specific to Scherzo's dogfood workflow and are not generic workspace capabilities. GitHub comment acknowledgement after feedback incorporation remains helper-owned because it is review-feedback policy rather than publication of the workspace change.
  Date: 2026-05-09

- Decision: Require changed-file status information from `changed-files --json` before converting ExecPlan and merge-conflict validation.
  Rationale: `scripts/scherzo-execplan validate` currently distinguishes a newly added plan file from any changed plan file, and conflict validation must identify conflicted and non-conflicted paths. A path-only changed-file list would weaken validation.
  Date: 2026-05-09

- Decision: Require agent-step shells to inherit `SCHERZO_WORKSPACE_DRIVER` and keep prompt examples in environment-variable form.
  Rationale: Prompt text that says `$SCHERZO_WORKSPACE_DRIVER status --human` is only safe if an agent can run that literal command. Rendering a local value without exposing the shell environment would be misleading.
  Date: 2026-05-09

- Decision: Convert merge-conflict validation and publication to ordinary driver operations, but leave conflict target discovery and conflict materialization in `scripts/scherzo-merge-conflict`.
  Rationale: Changed-file inspection and publishing are generic workspace operations. Preparing a conflicted target and enforcing conflict-specific manifests are workflow policy and should not force a generic conflict driver capability into this plan.
  Date: 2026-05-09

- Decision: Convert prompts in the same implementation as helper scripts.
  Rationale: The workflow interface is not portable if agents still receive raw jj instructions, even when command helpers use a driver.
  Date: 2026-05-09

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam application that dispatches tracker issues into workflow DAGs. A workflow DAG is a YAML file under `.scherzo/workflows/`. Each step runs either a command or an agent prompt in a prepared workspace. A workspace is the directory where the step reads and writes files. A workspace profile is operator configuration that says how those workspaces are prepared. A workspace driver is the command configured by a workspace profile; workflows may require named driver capabilities, command steps may call the configured driver through `$SCHERZO_WORKSPACE_DRIVER`, and agent prompts may tell agents to run the same command when the agent-step shell exposes that environment variable.

The relevant dogfood workflow files are:

- `.scherzo/workflows/implementation.yaml`, which implements an issue directly from Linear ticket context, reviews it, validates it, and publishes a PR or equivalent hosted review URL.
- `.scherzo/workflows/execplan.yaml`, which writes a new ExecPlan, validates and reviews it, opens a PR or equivalent hosted review URL, and creates a follow-up implementation issue.
- `.scherzo/workflows/execplan-revision.yaml`, which revises an existing ExecPlan PR from GitHub review feedback and acknowledges the feedback.
- `.scherzo/workflows/execplan-implementation.yaml`, which implements an existing ExecPlan, verifies completion, reviews the change, validates, and publishes a PR or equivalent hosted review URL.
- `.scherzo/workflows/merge-conflict-resolution.yaml`, which prepares a conflicted target, lets an agent resolve conflicts, validates the resolution, and publishes it.

The relevant prompts live under `.scherzo/workflows/prompts/`. The implementation-like prompts that must be audited include `implement.md`, `code-review.md`, `apply-feedback.md`, `repair-base-drift.md`, `execplan-draft.md`, `execplan-repair-validation.md`, `execplan-review.md`, `execplan-incorporate-review.md`, `execplan-revision.md`, `execplan-implementation-implement.md`, `execplan-implementation-verify-completion.md`, `execplan-implementation-apply-plan-completion-feedback.md`, `execplan-implementation-verify-completion-after-feedback.md`, `execplan-implementation-review.md`, `execplan-implementation-apply-feedback.md`, `execplan-implementation-verify-completion-before-final-validation.md`, and `resolve-merge-conflicts.md`. The research prompt is not part of this plan except as a portability style reference.

The relevant scripts are:

- `scripts/scherzo-jj-workspace`, the current lifecycle hook helper used by `.scherzo/scherzo.yaml` to create, verify, and forget jj workspaces.
- `scripts/scherzo-implementation`, a Python helper for direct implementation and ExecPlan implementation workflows. It prepares issue or plan context, records the starting workspace baseline, analyzes changed files, refreshes onto the publish base, runs validation, checks plan-completion verdicts, and publishes a review URL.
- `scripts/scherzo-execplan`, a shell helper for the ExecPlan authoring workflow. It validates that exactly one new plan under `docs/plans/` changed, validates plan content, publishes the plan review URL, and creates or finds the follow-up Linear issue.
- `scripts/scherzo-execplan-revision`, a Python helper for revising an existing ExecPlan PR. It fetches PR metadata and feedback, updates the workspace to the PR head, validates the plan-only change, publishes the PR update, and acknowledges feedback.
- `scripts/scherzo-merge-conflict`, a Python helper for preparing, validating, and publishing conflict resolutions.
- `scripts/scherzo-review`, a review-artifact helper used by the implementation and ExecPlan implementation workflows to produce staged review briefs and lane outputs.

## Preconditions and Verified Facts

Before implementing this plan, verify the workspace-driver foundation in the current implementation workspace. The required facts are:

- `src/scherzo/workflow_dag.gleam` parses a top-level `workspace_capabilities` list on workflow DAGs.
- `src/scherzo/config/types.gleam` and `src/scherzo/config.gleam` model driver-backed workspace profiles.
- `src/scherzo/runtime_bundle.gleam` rejects a workflow when its selected profile lacks a required capability.
- `src/scherzo/workflow_run.gleam` exposes `SCHERZO_WORKSPACE_DRIVER`, `SCHERZO_WORKSPACE_PROFILE`, and `SCHERZO_WORKSPACE_CAPABILITIES` to command steps.
- Agent-step shells also inherit `SCHERZO_WORKSPACE_DRIVER`, and prompt rendering can include workspace driver locals.
- `.scherzo/scherzo.yaml` has a driver-backed dogfood profile named `dogfood-jj`.
- The selected driver command implements the operation contract in this plan or can be amended to that exact contract before helper conversion begins.

Use this drift checklist from the repository root before editing helper scripts:

    direnv exec . gleam test
    grep -R "workspace_capabilities" -n src .scherzo test
    grep -R "SCHERZO_WORKSPACE_DRIVER" -n src test .scherzo scripts
    grep -n "dogfood-jj" .scherzo/scherzo.yaml

The first command should pass. The grep commands should show the driver foundation in source, tests, dogfood workflow configuration, and helper or prompt surfaces. If `grep` is unavailable in the execution environment, use the repository search tool or an equivalent literal search. If any required fact is absent, stop this plan and mark it blocked; do not partially convert these workflows while the runtime cannot validate or expose their declared driver operations.

The current tree observed while drafting this plan does not yet have the driver foundation. `src/scherzo/config/types.gleam` still defines `WorkspaceHookProfile` and `WorkspaceHookProfiles`; `src/scherzo/workflow_dag.gleam` currently defines `WorkflowDag(id, description, workspace_profile, max_parallel_steps, steps)` with no `workspace_capabilities` field; and `.scherzo/scherzo.yaml` uses direct `workspace.hooks`. These facts are expected to change before this plan is implemented.

The repository's standard validation commands should be run from the repository root through direnv:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If direnv reports that `.envrc` is blocked in a fresh workspace, inspect `.envrc`, run `direnv allow .`, and retry the direnv-backed command. Treat that as environment setup, not as a test failure.

## Scope Boundaries

In scope: update `.scherzo/workflows/implementation.yaml`, `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, `.scherzo/workflows/execplan-implementation.yaml`, and `.scherzo/workflows/merge-conflict-resolution.yaml`; update the implementation-like prompts under `.scherzo/workflows/prompts/`; update `scripts/scherzo-implementation`, `scripts/scherzo-execplan`, `scripts/scherzo-execplan-revision`, `scripts/scherzo-merge-conflict`, and `scripts/scherzo-review`; update or add tests under `test/` that exercise those helpers and workflow portability; and normalize the workspace-driver command contract to the exact signatures and JSON shapes below.

Out of scope: changing the issue tracker model, replacing Linear integration, replacing GitHub review feedback logic, creating a general merge-conflict preparation capability, changing the public ExecPlan format, changing project validation policy, or converting the research workflow. Also out of scope is deleting all jj or GitHub references from implementation files. The jj-backed driver may still mention and execute jj and GitHub CLI internally, and helper subcommands that acknowledge review feedback may still call GitHub APIs because those calls are not workspace publication operations.

The workflow-specific helper subcommands should remain visible to workflows. For example, `.scherzo/workflows/implementation.yaml` should still call `scripts/scherzo-implementation prepare --source ticket`, `scripts/scherzo-implementation analyze`, `scripts/scherzo-implementation validate`, and `scripts/scherzo-implementation publish`. The difference is that these helpers should use `$SCHERZO_WORKSPACE_DRIVER` internally for driver-owned operations.

## Driver Operation Contract

All driver command paths and file arguments are repository-relative or environment-provided. All revision, branch, and change identifiers returned by the driver are opaque strings; helpers must copy them and compare them for equality but must not parse jj-specific or git-specific structure out of them. Every JSON response uses `version: 1`. On success, the driver exits 0. On failure, the driver exits nonzero and still writes a bounded JSON object with `version`, `status`, `failure_code`, and `message` whenever it can; wrappers must fail closed if stdout is missing, malformed, or missing required fields.

The status capability is:

    $SCHERZO_WORKSPACE_DRIVER status --human

It prints bounded human-readable status for the current workspace and exits 0 when the workspace is usable. Prompts use this command for orientation. Helpers do not parse this output.

The diff capability is:

    $SCHERZO_WORKSPACE_DRIVER diff --human
    $SCHERZO_WORKSPACE_DRIVER diff --json

The human form prints bounded colorless diff text for agents. The JSON form is used by `scripts/scherzo-review` and returns this shape:

    {
      "version": 1,
      "format": "unified",
      "text": "diff --git ...",
      "truncated": false
    }

If the diff is truncated, `truncated` is `true` and `text` still contains the bounded diff prefix. A missing `text` field or a non-boolean `truncated` field is a wrapper error.

The changed-file capability is:

    $SCHERZO_WORKSPACE_DRIVER changed-files --json

It returns stable JSON records sorted by `path`:

    {
      "version": 1,
      "files": [
        { "path": "docs/plans/example.md", "status": "added" },
        { "path": "src/example.gleam", "status": "modified" }
      ]
    }

`status` must be one of `added`, `modified`, `deleted`, `renamed`, or `conflicted`. A renamed record also includes `old_path`. Paths must be repository-relative, must not be empty, and must not contain absolute local path prefixes. Helpers use this output to enforce plan-only changes and merge-conflict fallout policy.

The baseline capability is:

    $SCHERZO_WORKSPACE_DRIVER baseline --json

It returns the opaque identities needed for freshness checks:

    {
      "version": 1,
      "baseline_id": "opaque-baseline",
      "workspace_revision_id": "opaque-workspace-revision",
      "change_id": "opaque-change",
      "dirty": true
    }

`scripts/scherzo-implementation plan-completion-context` writes these values into the plan-completion context, and `gate-plan-completion` requires the agent verdict to echo the same values. A changed value means the verdict is stale.

The base-refresh capability is:

    $SCHERZO_WORKSPACE_DRIVER refresh-base --stage <stage> --json
    $SCHERZO_WORKSPACE_DRIVER refresh-base --stage <stage> --target <driver-ref> --json

Without `--target`, the driver refreshes the workspace against the configured publication base from the selected profile. With `--target`, it refreshes against an explicit driver reference such as an existing PR head discovered by `scripts/scherzo-execplan-revision prepare`. The driver owns fetching, rebasing or merging, and reporting conflicts. The wrapper owns deciding whether to continue the workflow, running project validation, and writing workflow-specific artifacts.

Successful refresh output has `status` `fresh` or `rebased_clean` and exit 0:

    {
      "version": 1,
      "status": "rebased_clean",
      "stage": "before-implementation",
      "base_ref": "main",
      "base_revision": "opaque-base-after-refresh",
      "before_revision": "opaque-before",
      "after_revision": "opaque-after",
      "conflict_files": []
    }

Failure output uses `status` `conflicts`, `fetch_failed`, `base_not_found`, or `rebase_failed`, exits nonzero, and includes `failure_code` and `message`. The wrapper must store the JSON at the existing workflow artifact path, such as `tmp/scherzo-implementation-refresh-base-before-implementation.json`, even on failure when parseable JSON is available.

The publication capability is:

    $SCHERZO_WORKSPACE_DRIVER publish-change --kind <kind> --title-file <path> --body-file <path> --branch-prefix <prefix> --base <driver-ref> --json
    $SCHERZO_WORKSPACE_DRIVER publish-change --kind <kind> --title-file <path> --body-file <path> --branch-prefix <prefix> --base <driver-ref> --target-branch <branch> --target-pr <number> --allow-no-changes <true|false> --json

`kind` is one of `implementation`, `execplan`, `execplan-revision`, or `merge-conflict`. The helper assembles the title and body in repository-relative files under `tmp/`, chooses the branch prefix from workflow metadata, and passes the intended base reference. For revision and merge-conflict publication, the helper also passes the existing target branch and PR number. The driver owns describing or finalizing the workspace change, creating or updating the branch or equivalent remote change, pushing, and creating, finding, or updating the hosted review URL. For the dogfood jj driver this may involve jj and GitHub CLI internally, but helper tests must prove the helpers themselves do not invoke `jj` or `gh` on converted publication paths.

Successful publication output has `status` `published`, `updated`, or `unchanged` and exit 0:

    {
      "version": 1,
      "status": "published",
      "url": "https://example.invalid/review/123",
      "branch": "scherzo/liv-175-example",
      "base_ref": "main",
      "base_revision": "opaque-base",
      "head_revision": "opaque-head",
      "change_id": "opaque-change",
      "created": true,
      "updated": false
    }

Failure output uses `status` `nothing_to_publish`, `base_drift`, `conflicts`, `auth_failed`, `remote_rejected`, `publication_failed`, or `invalid_request`, exits nonzero, and includes `failure_code` and `message`. Helpers preserve their current final stdout keys, such as `PR_URL=...`, by reading `url` from successful driver output. Existing retention-marker cleanup remains wrapper-owned and happens only after a successful `published`, `updated`, or accepted `unchanged` result; failures leave retention markers intact for operator inspection.

## Milestones

The first milestone is prerequisite verification and driver-contract normalization. At the end, the implementation workspace has a driver-backed dogfood profile named `dogfood-jj`, command steps and agent-step shells can see `$SCHERZO_WORKSPACE_DRIVER`, prompts can render workspace driver locals, and the driver command satisfies the exact contract in this plan. This milestone comes first because the workflow conversion cannot be made safe without runtime validation and a usable driver command.

The second milestone converts changed-file, diff, baseline, and review analysis operations. At the end, `scripts/scherzo-implementation analyze`, plan-completion context generation, `scripts/scherzo-execplan validate`, `scripts/scherzo-execplan-revision validate`, the safe parts of `scripts/scherzo-merge-conflict validate`, and `scripts/scherzo-review` obtain file lists, baseline identities, and diffs through the driver instead of raw jj commands. Fake-driver tests prove the wrappers call the driver, preserve existing stdout and JSON artifacts, and fail if sentinel `jj` or `gh` commands are invoked on converted paths.

The third milestone converts base refresh and publication wrappers. At the end, implementation and ExecPlan workflows still run their existing helper subcommands, but the helper subcommands delegate base refresh and change publication to `$SCHERZO_WORKSPACE_DRIVER refresh-base` and `$SCHERZO_WORKSPACE_DRIVER publish-change`. The wrappers still write the existing `tmp/` JSON files, still run workflow-specific validation and Linear or feedback policy, and still print expected handoff lines such as `PR_URL=...`.

The fourth milestone updates workflow YAML and prompts. At the end, the five implementation-like workflow YAML files declare explicit workspace capabilities that match the driver calls their helpers and prompts use, review commands no longer pass jj revsets such as `@-` and `@`, and prompts tell agents they are in a dedicated workflow workspace controlled by a workspace driver. Agents are told not to manage workspace lifecycle or publication themselves, and status or diff orientation examples use `$SCHERZO_WORKSPACE_DRIVER`.

The fifth milestone validates portability and dogfood parity. At the end, tests prove that implementation-like prompts and YAML do not reintroduce raw jj workflow instructions, helper tests pass with fake drivers and sentinel `jj` or `gh` commands, a workflow-level smoke test loads a converted workflow through the runtime with a fake driver-backed profile, and the standard Gleam, formatting, glinter, and custom lint commands pass.

## Plan of Work

Start by normalizing the driver contract. Inspect the driver contract tests introduced by the workspace-driver foundation. If the existing driver command already implements the exact contract in this plan, keep the implementation and add any missing tests. If it differs, amend the contract implementation and tests before touching workflow helpers. The required contract includes changed-file status JSON, diff JSON, baseline JSON, base-refresh JSON, and publication JSON with the success and failure statuses listed above.

In `scripts/scherzo-implementation`, add a small internal function that requires `SCHERZO_WORKSPACE_DRIVER`, runs the driver with explicit arguments, captures stdout and stderr, and maps nonzero exits or malformed JSON to stable Scherzo failure codes. Name it plainly, for example `run_workspace_driver`. Add focused wrappers for `changed-files --json`, `diff --json`, `baseline --json`, `refresh-base --json`, and `publish-change --json`. Use these wrappers from `analyze`, `plan-completion-context`, `gate-plan-completion`, `refresh-base`, and `publish`. Keep existing JSON artifact names such as `tmp/scherzo-implementation-refresh-base-<stage>.json`, `tmp/scherzo-implementation-validation.json`, and `tmp/scherzo-implementation-publish.json`. Keep existing stdout keys consumed by prompts and later steps.

In `scripts/scherzo-execplan`, replace the internal `changed_files` and plan status checks that currently shell out to `jj diff` with driver `changed-files --json`. Preserve the existing validation policy: there must be exactly one changed file, it must be under `docs/plans/`, it must be a newly added Markdown file, it must be portable, and it must include `## Open Questions and Clarifications Needed`. In `create-pr`, delegate source-control and hosted-review publication to `publish-change --kind execplan`; keep plan validation, PR title/body construction, and follow-up implementation issue creation in this script. Print the same `PR_URL=...` line by reading the driver's `url` field.

In `scripts/scherzo-execplan-revision`, keep GitHub feedback discovery and acknowledgement in the helper, but use the driver for workspace movement and publication. `prepare` finds the referenced PR and feedback through GitHub, translates the PR head into the opaque driver reference expected by the selected profile, and calls `refresh-base --stage execplan-revision-prepare --target <driver-ref> --json`. `validate` uses driver changed-file status. `publish` calls `publish-change --kind execplan-revision --target-branch <branch> --target-pr <number> --allow-no-changes true --json`, stores `tmp/execplan-revision-publish.json`, and preserves final stdout keys such as `PR_URL=...`, `BRANCH=...`, `REVISION=...`, and `PUSHED=...`.

In `scripts/scherzo-merge-conflict`, keep PR or branch target discovery, conflict-specific preparation, conflict marker policy, non-conflicted file manifest validation, and project validation in the script. Replace ordinary changed-file inventory and status or diff orientation calls with driver calls. Change `publish` to call `publish-change --kind merge-conflict --target-branch <branch> --target-pr <number> --allow-no-changes false --json`, preserving existing conflict-resolution stdout and artifacts. Conflict target preparation remains script-owned because this plan does not define a generic conflict-preparation driver capability.

In `scripts/scherzo-review`, stop requiring workflow YAML callers to pass jj revsets such as `--from @- --to @`. Add a driver-backed mode that gets the changed-file list from `changed-files --json` and diff content from `diff --json`. Make that mode the default when `SCHERZO_WORKSPACE_DRIVER` exists. Preserve the existing explicit `--from` and `--to` arguments as a backwards-compatible manual fallback for local use, but remove those arguments from dogfood workflow YAML. The review artifacts and lane outputs must keep their current filenames and schemas.

Update workflow YAML with capability lists that match the final driver calls. In `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml`, add top-level `workspace_profile: dogfood-jj` and `workspace_capabilities: [status, diff, changed-files, baseline, refresh-base, publish-change]`. These workflows use prompts for status and diff orientation, changed-file and baseline helpers for analysis and plan-completion freshness, base refresh, and publication.

In `.scherzo/workflows/execplan.yaml`, add `workspace_profile: dogfood-jj` and `workspace_capabilities: [status, diff, changed-files, publish-change]`. It uses prompts for status and diff orientation, plan validation through changed-file status, and publication through the driver. It does not declare `baseline` or `refresh-base` because its helpers do not call those operations.

In `.scherzo/workflows/execplan-revision.yaml`, add `workspace_profile: dogfood-jj` and `workspace_capabilities: [status, diff, changed-files, refresh-base, publish-change]`. It uses prompts for status and diff orientation, target refresh during prepare, changed-file validation, and publication to the existing PR.

In `.scherzo/workflows/merge-conflict-resolution.yaml`, add `workspace_profile: dogfood-jj` and `workspace_capabilities: [status, diff, changed-files, publish-change]`. It uses prompts for status and diff orientation, driver changed-file validation, and driver publication. It does not declare `refresh-base` because conflict target preparation remains script-owned in this plan.

Update prompts under `.scherzo/workflows/prompts/`. Replace "dedicated jj workspace" with "dedicated workflow workspace prepared by Scherzo" or equivalent wording. Replace direct `jj status --color=never` examples with `$SCHERZO_WORKSPACE_DRIVER status --human` and direct `jj diff` examples with `$SCHERZO_WORKSPACE_DRIVER diff --human`. Replace instructions not to manage jj workspaces, branches, bookmarks, pushes, or pull requests with driver-neutral language: agents must not create, remove, switch, publish, push, or otherwise manage workflow workspaces or change publication; later deterministic command steps do that through the configured workspace driver. Keep prohibitions on direct GitHub PR posting where deterministic helper steps still handle comments, acknowledgements, or final handoff.

Update tests. Prefer extending existing test modules rather than creating broad new test suites. Use `test/workflow_portability_test.gleam` for prompt and workflow text assertions plus rendered-prompt checks. Use `test/execplan_implementation_helper_test.gleam` for `scripts/scherzo-implementation`, `scripts/scherzo-execplan`, `scripts/scherzo-execplan-revision`, and `scripts/scherzo-review` helper behavior because that file already contains fake command fixtures and publish/base-refresh tests for these helpers. Use `test/merge_conflict_helper_test.gleam` for `scripts/scherzo-merge-conflict`. Use the driver contract test file from the workspace-driver foundation, or create `test/workspace_driver_contract_test.gleam`, for exact JSON shape, missing-field, malformed-JSON, success-status, and failure-status coverage. Add a runtime smoke test in the existing workflow-run or runtime-bundle test module, or create `test/workflow_driver_smoke_test.gleam`, to load one converted workflow with a fake driver-backed profile, verify capability validation, run a representative command step with `SCHERZO_WORKSPACE_DRIVER`, and render a representative prompt.

## Concrete Steps

1. From the repository root, verify the prerequisite driver foundation. Inspect `src/scherzo/workflow_dag.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/workflow_run.gleam`, `.scherzo/scherzo.yaml`, and the selected dogfood driver command. Confirm that `workspace_capabilities` parses, driver-backed profiles load, capability validation happens before dispatch, command steps receive `SCHERZO_WORKSPACE_DRIVER`, agent-step shells receive `SCHERZO_WORKSPACE_DRIVER`, prompt rendering has workspace driver locals, and the dogfood profile is named `dogfood-jj`. If these facts are absent, stop this plan and mark it blocked.

2. Run the existing tests once before editing implementation files:

       direnv exec . gleam test

   Expect all tests to pass. If they fail before this plan's changes, record the failure in Surprises & Discoveries and either fix the unrelated breakage separately or pause this plan.

3. Add or update driver contract tests before converting helpers. In the existing driver contract test file, or in new `test/workspace_driver_contract_test.gleam` if no such file exists, add tests for `changed-files --json`, `diff --json`, `baseline --json`, `refresh-base --json`, and `publish-change --json`. Use fake or temporary workspaces that produce one added file, one modified file, a non-empty diff, a baseline identity, a clean refresh, a conflict refresh, a successful publication, and one publication failure. Assert the exact required fields and statuses from the Driver Operation Contract section. Add malformed-JSON and missing-field cases at the wrapper level so helpers fail closed with stable diagnostics.

4. Implement the minimal driver changes needed to satisfy the contract tests. Keep JSON stable, sorted by path where lists are returned, and secret-free. Re-run `direnv exec . gleam test` and expect the contract tests to pass. Commit this slice only after the full test command passes.

5. In `scripts/scherzo-implementation`, add the internal driver helper and change `analyze` to obtain the changed-file list from `changed-files --json`. Add a fake driver fixture in `test/execplan_implementation_helper_test.gleam` that writes its received arguments to a log and returns two changed files. Add sentinel `jj` and `gh` executables earlier on `PATH` that fail if invoked. Add a test named `implementation_analyze_uses_workspace_driver_changed_files_test` that runs `scripts/scherzo-implementation analyze` with `SCHERZO_WORKSPACE_DRIVER` pointing to the fake driver, then asserts the log contains `changed-files --json`, sentinel commands were not invoked, and the existing analysis stdout still reports the expected language or changed-file metadata.

6. In `scripts/scherzo-implementation`, change `plan-completion-context` and `gate-plan-completion` to use driver baseline and changed-file information for the context values they ask agents to copy into `tmp/scherzo-plan-completion-verdict.json`. Add or update tests in `test/execplan_implementation_helper_test.gleam` so `plan_completion_gate_passes_fresh_pass_verdict_test` and stale-verdict tests use fake driver output instead of fake jj output. The expected behavior remains the same: a fresh pass verdict succeeds; fail, malformed, missing, or stale verdict fails with the existing failure codes.

7. In `scripts/scherzo-implementation`, change `refresh-base --stage <stage>` to call `$SCHERZO_WORKSPACE_DRIVER refresh-base --stage <stage> --json`. Preserve `tmp/scherzo-implementation-refresh-base-<stage>.json` and existing status values such as `fresh`, `rebased_clean`, `conflicts`, `fetch_failed`, `base_not_found`, and `rebase_failed`. Update `test/execplan_implementation_helper_test.gleam` with fake-driver refresh tests for a clean refresh, conflict output, malformed JSON, and missing `SCHERZO_WORKSPACE_DRIVER`. Assert the wrapper writes the same JSON path, prints the same operator-facing summary, emits stable failure codes on errors, and does not invoke sentinel `jj` or `gh` commands.

8. In `scripts/scherzo-implementation`, change `publish` to call `$SCHERZO_WORKSPACE_DRIVER publish-change --kind implementation ... --json` for source-control and hosted-review publication. Keep PR title and body construction, validation-before-publish, revalidation-after-base-normalization when required by project policy, retention marker cleanup after successful publication, and final stdout in the wrapper. Update existing publish tests in `test/execplan_implementation_helper_test.gleam`, including `publish_rebases_to_remote_base_and_revalidates_test`, `execplan_implementation_publish_mentions_linear_issue_in_pr_metadata_test`, `publish_rebase_conflict_emits_stable_failure_code_test`, and `publish_revalidation_failure_emits_stable_failure_code_test`, so the fake driver supplies publish JSON and the assertions check that `PR_URL=...` and `tmp/scherzo-implementation-publish.json` still exist. In the driver-backed publish tests, sentinel `jj` and `gh` commands must fail the test if invoked by the helper.

9. In `scripts/scherzo-execplan`, replace jj-based changed-file and status checks with driver `changed-files --json`. Add tests in `test/execplan_implementation_helper_test.gleam` for three cases: exactly one added `docs/plans/example.md` passes validation and prints `PLAN_PATH=docs/plans/example.md`; one modified existing plan fails because the plan must be newly added; and an extra changed file fails with a diagnostic naming the unexpected path. Update create-pr tests so source-control and hosted-review publication is provided by a fake `publish-change` driver result while Linear follow-up issue creation remains in the script. Sentinel `jj` and `gh` commands must not be invoked by `validate` or `create-pr` in driver-backed mode.

10. In `scripts/scherzo-execplan-revision`, convert prepare refresh, validation changed-file inventory, and publication to the driver. `prepare` should call `refresh-base --stage execplan-revision-prepare --target <driver-ref> --json` after it discovers the PR head. `validate` should call `changed-files --json`. `publish` should call `publish-change --kind execplan-revision --target-branch <branch> --target-pr <number> --allow-no-changes true --json`. Add tests in `test/execplan_implementation_helper_test.gleam` with fake driver output for no-op revision, valid plan-only revision, extra-file rejection, refresh conflict, and publish branch-advanced failure. Preserve `tmp/execplan-revision-publish.json` and final stdout keys such as `PR_URL=...`, `BRANCH=...`, `REVISION=...`, and `PUSHED=...`. Sentinel `jj` and `gh` commands must not be invoked by the converted prepare-refresh, validate, or publish paths.

11. In `scripts/scherzo-merge-conflict`, replace ordinary changed-file inventory in validation with driver `changed-files --json` and change publication to `publish-change --kind merge-conflict --target-branch <branch> --target-pr <number> --allow-no-changes false --json`. Keep conflict marker checks, non-conflicted fingerprint checks, `tmp/scherzo-merge-conflict-mechanical-edits.json`, and project validation unchanged. Add tests in `test/merge_conflict_helper_test.gleam` that use a fake driver to prove `validate` rejects an unmanifested non-conflicted file change and accepts a manifested mechanical non-conflicted file change with the same stdout as before. Add publish tests that preserve existing handoff stdout and fail if sentinel `jj` or `gh` commands are invoked by the helper's converted publish path.

12. In `scripts/scherzo-review`, add driver-backed diff and changed-file loading. Update workflow callers so `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml` no longer pass `--from @- --to @`. Add tests in `test/execplan_implementation_helper_test.gleam` or `test/review_artifacts_test.gleam` that run `scripts/scherzo-review dry-run` with a fake driver and sentinel `jj` or `gh` commands. Assert the review brief still contains the changed files and diff text, the fake driver log contains `changed-files --json` and `diff --json`, and no sentinel command is invoked.

13. Update workflow YAML capability declarations exactly as described in Plan of Work. Preserve step IDs, dependencies, `on_failure`, `timeout_ms`, and workspace names unless the prerequisite driver migration already changed them. Run `direnv exec . gleam test` and expect workflow parsing and fingerprint tests to pass or to fail only where assertions need updating for the new capability fields.

14. Update prompts under `.scherzo/workflows/prompts/` to use workspace-driver wording. For every implementation-like prompt, remove direct `jj status` and `jj diff` commands unless the prompt is explicitly explaining forbidden legacy behavior. Add driver-neutral orientation language and examples using `$SCHERZO_WORKSPACE_DRIVER status --human` and `$SCHERZO_WORKSPACE_DRIVER diff --human`. Keep direct references to `scripts/scherzo-implementation`, `scripts/scherzo-execplan`, and `scripts/scherzo-merge-conflict` where agents must read helper artifacts.

15. Update `test/workflow_portability_test.gleam`. Add a test named `implementation_like_workflows_use_workspace_driver_language_test`. It should read the five workflow YAML files and the implementation-like prompts listed in this plan. Assert that the YAML files contain the exact capability lists described in Plan of Work and do not contain `--from @- --to @`. Assert that the prompts do not contain `dedicated jj workspace`, ``jj status --color=never``, ``jj diff --color=never``, `jj diff --from @-`, or "manage jj workspaces". Assert that the prompts contain `SCHERZO_WORKSPACE_DRIVER` or a rendered concrete workspace driver command.

16. Add missing-driver failure tests for each converted helper family. Use fake environments in `test/execplan_implementation_helper_test.gleam` and `test/merge_conflict_helper_test.gleam` to run a converted subcommand without `SCHERZO_WORKSPACE_DRIVER`. Assert a nonzero exit, a bounded diagnostic mentioning `SCHERZO_WORKSPACE_DRIVER`, and a stable failure code such as `workspace_driver_unavailable`. If the codebase has a different failure-code naming convention by the time this plan is implemented, use that convention but keep one stable code per family.

17. Add a workflow-level smoke test. In the existing runtime workflow test module, or in new `test/workflow_driver_smoke_test.gleam`, load one converted workflow such as `.scherzo/workflows/implementation.yaml` with a fake driver-backed `dogfood-jj` profile. Assert capability validation succeeds when the fake profile advertises the required capabilities and fails with a missing-capability diagnostic when one required capability is removed. Run a representative command step through the runtime test harness and assert `SCHERZO_WORKSPACE_DRIVER` is present. Render a representative agent prompt and assert it includes workspace-driver wording; if the test harness can execute an agent-shell fixture, assert that shell sees `SCHERZO_WORKSPACE_DRIVER` too.

18. Run the focused full test command from the repository root:

       direnv exec . gleam test

   Expect all tests to pass. The output should end with a successful Gleam test summary and no failing test names.

19. Run the formatting and lint gates from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect each command to exit 0. Do not add production `let assert`, `panic`, or `todo`; glinter enforces this policy for `src/`.

20. Commit at logical green points. Suggested commit map: one commit for driver contract normalization; one commit for helper script driver delegation and tests; one commit for workflow YAML and prompt wording; one commit for portability and runtime smoke tests plus final cleanup. Each commit should be made only after `direnv exec . gleam test` passes for that slice.

## Testing and Falsifiability

This plan is falsified if implementation-like workflows still require raw jj commands in prompts or workflow YAML after the change, if helper scripts cannot preserve their current artifacts and stdout while using the driver, if converted helper paths invoke raw `jj` or `gh` commands while `SCHERZO_WORKSPACE_DRIVER` is set, or if a non-jj fake driver cannot pass the helper tests for changed-file, diff, baseline, refresh, and publish operations.

Add or update the following tests exactly:

- The driver contract test file from the workspace-driver foundation, or new `test/workspace_driver_contract_test.gleam`: add exact JSON-shape tests for `changed-files --json`, `diff --json`, `baseline --json`, `refresh-base --json`, and `publish-change --json`. Cover success statuses, failure statuses, missing required fields, malformed JSON, and sorted repository-relative changed-file paths.
- `test/workflow_portability_test.gleam`: add `implementation_like_workflows_use_workspace_driver_language_test`. It reads the five workflow YAML files and implementation-like prompts, asserts the YAML declares the exact capability lists from Plan of Work, asserts review commands no longer include `--from @- --to @`, asserts prompts contain workspace-driver language, and asserts prompts no longer contain raw jj status/diff/workspace-management instructions.
- The existing runtime workflow tests, or new `test/workflow_driver_smoke_test.gleam`: add a workflow-level smoke test that loads a converted workflow with a fake driver-backed `dogfood-jj` profile, verifies capability validation, runs a representative command step with `SCHERZO_WORKSPACE_DRIVER` available, and renders or executes a representative agent prompt with driver wording and agent-shell driver environment.
- `test/execplan_implementation_helper_test.gleam`: add fake driver fixtures and tests for `scripts/scherzo-implementation analyze`, `plan-completion-context`, `gate-plan-completion`, `refresh-base`, and `publish`. Update existing plan-completion and publish tests so they exercise driver-backed behavior while preserving current failure codes and artifact paths.
- `test/execplan_implementation_helper_test.gleam`: add tests for `scripts/scherzo-execplan validate` and `create-pr` using driver changed-file JSON and publish JSON. Cover one added plan success, modified plan rejection, extra changed file rejection, missing `SCHERZO_WORKSPACE_DRIVER` rejection, malformed driver JSON rejection, and preservation of `PLAN_PATH=...` and `PR_URL=...` stdout.
- `test/execplan_implementation_helper_test.gleam`: add tests for `scripts/scherzo-execplan-revision prepare`, `validate`, and `publish` behavior with fake driver output. Cover no-op revision, valid plan-only revision, extra-file rejection, refresh conflict, branch-advanced or stale publish failure, and preservation of `tmp/execplan-revision-publish.json`.
- `test/merge_conflict_helper_test.gleam`: update validation and publish tests so changed-file inventory and publication come from a fake workspace driver. Keep assertions for non-conflicted drift, manifested mechanical edits, resolved conflicts, stable diagnostics, existing publish handoff stdout, and stable artifact paths.
- `test/review_artifacts_test.gleam` or `test/execplan_implementation_helper_test.gleam`: add a driver-backed `scripts/scherzo-review dry-run` test that proves review brief generation reads changed files and diffs from the driver and does not invoke `jj` when `SCHERZO_WORKSPACE_DRIVER` is present.

For every converted helper family, build the fake environment so `PATH` contains only required shell tools plus sentinel `jj` and `gh` executables that fail the test if invoked. Cover `scripts/scherzo-implementation analyze`, `refresh-base`, and `publish`; `scripts/scherzo-execplan validate` and `create-pr`; `scripts/scherzo-execplan-revision prepare`, `validate`, and `publish`; `scripts/scherzo-review dry-run`; and `scripts/scherzo-merge-conflict validate` and `publish`. Manual fallback modes that intentionally keep old `--from` and `--to` arguments in `scripts/scherzo-review` must be tested separately as legacy exceptions and must not be used by dogfood workflow YAML.

Expected red phase: before implementation, the new portability test fails because prompts mention `dedicated jj workspace` and `jj status --color=never`, workflow YAML lacks `workspace_capabilities`, review commands pass `--from @- --to @`, helper tests fail because helpers still invoke jj or GitHub publication commands directly, and the workflow-level smoke test fails if agent shells or command steps do not receive `SCHERZO_WORKSPACE_DRIVER`.

Expected green phase: after implementation, fake-driver logs show converted helpers invoking `changed-files --json`, `diff --json`, `baseline --json`, `refresh-base`, and `publish-change`; sentinel `jj` and `gh` commands are not invoked on converted paths; existing helper stdout keys and `tmp/` artifact paths still exist; rendered prompts and agent shells can use the driver command; portability tests find no direct jj instructions in implementation-like prompts; and the standard validation commands pass.

## Validation and Acceptance

Run all commands from the repository root. The acceptance validation is:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Accept the implementation when all four commands exit 0 and the following behavior is observable in tests and code review:

- The five implementation-like workflows select the driver-backed `dogfood-jj` profile explicitly and declare the exact workspace capabilities listed in Plan of Work.
- Runtime capability validation rejects a workflow when the selected profile is missing one of those capabilities.
- Command steps and agent-step shells receive `SCHERZO_WORKSPACE_DRIVER`; rendered prompts tell agents to use the workspace driver for status and diff orientation.
- Workflow YAML no longer passes raw jj revsets such as `@-` and `@` to `scripts/scherzo-review`.
- Implementation-like prompts no longer tell agents they are in a dedicated jj workspace and no longer provide raw `jj status` or `jj diff` orientation commands.
- Converted helpers fail closed with a bounded diagnostic when `SCHERZO_WORKSPACE_DRIVER` is missing or returns malformed JSON.
- Converted helpers preserve existing command names, step IDs, JSON artifact paths, and final handoff keys such as `PLAN_PATH=...` and `PR_URL=...`.
- Fake-driver tests with sentinel `jj` and `gh` commands prove converted helper paths do not invoke raw source-control or hosted-review publication commands outside the driver.
- The jj-backed driver may still contain jj and GitHub commands internally; the workflow boundary, prompt boundary, and converted helper boundary no longer require agents or workflow YAML to know those commands.

## Rollout, Recovery, and Idempotence

Roll this out after the driver schema, adapter, dogfood profile migration, and runtime-exposure work has landed and the implementation gate in Preconditions passes. This plan should be implemented in small commits so the repository is green after each milestone. The safest order is driver contract normalization first, helper wrappers second, workflow YAML third, prompts fourth, and portability plus runtime smoke assertions last.

Recovery is straightforward because the conversion is mostly wrapper and prompt work. If helper delegation breaks dogfood runs, revert the helper-delegation commit while leaving prerequisite driver support intact. If prompt wording causes agent confusion but helper behavior is correct, revert or amend the prompt commit only. If a driver contract normalization proves too broad, revert that extension and keep the implementation gate blocked until a smaller exact contract is designed; do not silently fall back to partial helper conversion in dogfood workflows.

The changes should be idempotent. Re-running a converted helper should read the same workspace driver environment and overwrite the same `tmp/` artifacts deterministically. Re-running the workflow YAML and prompt edits should not create duplicate capability entries or repeated instructions.

## Open Questions and Clarifications Needed

None.

## Artifacts and Notes

Current workflow command surfaces observed while drafting:

    .scherzo/workflows/implementation.yaml
      scripts/scherzo-implementation prepare --source ticket
      scripts/scherzo-implementation refresh-base --stage before-implementation
      scripts/scherzo-implementation analyze
      scripts/scherzo-review dry-run --from @- --to @
      scripts/scherzo-review run-lane --from @- --to @
      scripts/scherzo-implementation validate
      scripts/scherzo-implementation publish

    .scherzo/workflows/execplan.yaml
      scripts/scherzo-execplan validate
      scripts/scherzo-execplan create-pr
      scripts/scherzo-execplan create-implementation-issue

    .scherzo/workflows/execplan-revision.yaml
      scripts/scherzo-execplan-revision prepare
      scripts/scherzo-execplan-revision validate
      scripts/scherzo-execplan-revision publish
      scripts/scherzo-execplan-revision acknowledge

    .scherzo/workflows/execplan-implementation.yaml
      scripts/scherzo-implementation prepare --source execplan
      scripts/scherzo-implementation refresh-base --stage before-implementation
      scripts/scherzo-implementation analyze
      scripts/scherzo-implementation gate-plan-completion
      scripts/scherzo-review dry-run --from @- --to @
      scripts/scherzo-review run-lane --from @- --to @
      scripts/scherzo-implementation validate
      scripts/scherzo-implementation publish

    .scherzo/workflows/merge-conflict-resolution.yaml
      scripts/scherzo-merge-conflict prepare
      scripts/scherzo-merge-conflict validate
      scripts/scherzo-merge-conflict publish

Prompt text observed while drafting includes raw jj language in implementation-like prompts. The implementation should remove these examples from workflow-facing prompts and replace them with workspace-driver examples:

    $SCHERZO_WORKSPACE_DRIVER status --human
    $SCHERZO_WORKSPACE_DRIVER diff --human
    $SCHERZO_WORKSPACE_DRIVER changed-files --json

The driver command itself is trusted operator configuration. Workflow YAML must select a profile and declare capabilities, but it must not define a shell driver command.

Review incorporation note, 2026-05-09: this revision addresses the adversarial review by closing the `refresh-base` and `publish-change` contracts, removing conditional capability branches, requiring agent-shell driver exposure, adding sentinel tests against raw `jj` and `gh` calls on converted paths, and adding a workflow-level smoke test.

## Interfaces and Dependencies

This plan depends on these driver capabilities being available to the selected `dogfood-jj` profile:

    status
    diff
    changed-files
    baseline
    refresh-base
    publish-change

The command signatures, JSON schemas, success statuses, failure statuses, and ownership boundaries are defined in the Driver Operation Contract section and are part of this plan's acceptance criteria. The profile-level capability lists used by the workflows are intentionally narrower than the complete capability vocabulary when a workflow does not use an operation. Implementation and ExecPlan implementation workflows require all six capabilities. ExecPlan authoring requires `status`, `diff`, `changed-files`, and `publish-change`. ExecPlan revision requires `status`, `diff`, `changed-files`, `refresh-base`, and `publish-change`. Merge-conflict resolution requires `status`, `diff`, `changed-files`, and `publish-change`.

The production files most likely to change are:

- `.scherzo/workflows/implementation.yaml`
- `.scherzo/workflows/execplan.yaml`
- `.scherzo/workflows/execplan-revision.yaml`
- `.scherzo/workflows/execplan-implementation.yaml`
- `.scherzo/workflows/merge-conflict-resolution.yaml`
- `.scherzo/workflows/prompts/implement.md`
- `.scherzo/workflows/prompts/code-review.md`
- `.scherzo/workflows/prompts/apply-feedback.md`
- `.scherzo/workflows/prompts/repair-base-drift.md`
- `.scherzo/workflows/prompts/execplan-draft.md`
- `.scherzo/workflows/prompts/execplan-repair-validation.md`
- `.scherzo/workflows/prompts/execplan-review.md`
- `.scherzo/workflows/prompts/execplan-incorporate-review.md`
- `.scherzo/workflows/prompts/execplan-revision.md`
- `.scherzo/workflows/prompts/execplan-implementation-implement.md`
- `.scherzo/workflows/prompts/execplan-implementation-verify-completion.md`
- `.scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md`
- `.scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md`
- `.scherzo/workflows/prompts/execplan-implementation-review.md`
- `.scherzo/workflows/prompts/execplan-implementation-apply-feedback.md`
- `.scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md`
- `.scherzo/workflows/prompts/resolve-merge-conflicts.md`
- `scripts/scherzo-implementation`
- `scripts/scherzo-execplan`
- `scripts/scherzo-execplan-revision`
- `scripts/scherzo-merge-conflict`
- `scripts/scherzo-review`

The test files most likely to change are:

- `test/workflow_portability_test.gleam`
- `test/execplan_implementation_helper_test.gleam`
- `test/merge_conflict_helper_test.gleam`
- `test/review_artifacts_test.gleam`
- `test/workflow_driver_smoke_test.gleam`, if no existing runtime workflow test module is a better fit
- the driver contract test file introduced by the workspace-driver foundation, or new `test/workspace_driver_contract_test.gleam`
