# Harden implementation workflows against base drift

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo implementation workflows will refresh their pull request base before work begins and again before final validation, then route repairable base-drift failures through an agent instead of discovering them only during PR publication. Operators should see implementation runs fail earlier when the selected base branch cannot be fetched or rebased, repair ordinary rebase conflicts before publication, and repair clean-rebase validation failures when those failures are caused by the base changing underneath the workflow.

The observable outcome is that both `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml` gain explicit base-refresh and base-drift repair steps. The shared `scripts/scherzo-implementation` helper gains a structured `refresh-base` subcommand that writes JSON under `tmp/`, prints stable status lines, and can be reused by workflow commands and tests. A later implementer can prove the behavior with Gleam tests that fake `jj`, `direnv`, and `gh`, plus workflow-ordering assertions over the checked-in YAML files. This plan intentionally does not require reconstructing historical `LIV-59` or `LIV-65` workspaces for acceptance; the required evidence is the automated fake-command test matrix and the stable status lines described below.

## Problem Framing and Constraints

Scherzo runs implementation work in isolated jj workspaces. A jj workspace is a local working copy whose current change can be rebased independently of other work. This isolation allows multiple implementation workflows to run in parallel, but it also means each run can start from a base branch that becomes stale while other work lands on the real pull request base, usually `main` on the `origin` remote.

Today, `scripts/scherzo-implementation publish` fetches the configured PR base, rebases the final implementation change onto that base, and revalidates if it had to rebase. That is better than publishing from an old parent, but it is too late in the workflow. A rebase conflict at publish time fails after implementation, review, feedback, and validation have already happened. A clean rebase that breaks tests also fails inside the publish command, after there is no agent step left to make the mechanical repair. Operators then have to keep the retained workspace and repair by hand.

The plan must preserve concurrency. It must not require a global lock, a serialized implementation queue, or a rule that no two implementation workflows may run at once. Each run should refresh and repair only its own jj workspace. The plan must also preserve the existing final safety check at publish, because the base can still move in the short race window between final validation and PR creation.

The Linear issue names `LIV-59` and `LIV-65` as motivating incidents. [CLARIFY] The exact logs and branch histories for those incidents were not included in the ticket. This plan treats them as the two incident classes described by the acceptance criteria: one where stale base drift would have been exposed as a rebase conflict, and one where the rebase was clean but validation failed after the base update. Under this plan, the first class fails earlier or enters an explicit conflict repair step before publish, and the second class enters an explicit validation repair step before publish.

## Strategy Overview

Add one reusable helper command, `scripts/scherzo-implementation refresh-base`, and make both implementation workflows call it in two places. The first call happens immediately after `prepare` and before any implementation agent work. It fetches the configured PR base and ensures the empty or nearly-empty workflow change is based on the newest base. If that start-of-run refresh rebases cleanly, the helper updates the stored workflow baseline so later analysis does not accidentally include upstream changes. If it cannot fetch or rebase at this point, the workflow fails before spending agent time.

The second call happens after review feedback has been applied and before final validation. It fetches the base again and attempts to rebase the implementation change. This command step uses `on_failure: continue` so that an unresolved rebase conflict becomes agent-repairable instead of ending the workflow immediately. The workflow then runs validation with `on_failure: continue` and invokes a new `repair_base_drift` agent step. That agent is deliberately narrow: it resolves rebase conflicts caused by the refresh, or fixes validation failures only when the refresh JSON says the change was rebased cleanly onto a newer base. Because `repair_base_drift` runs unconditionally after the continue-on-failure validation step, the prompt must include an explicit state table: if validation succeeded and the refresh status is `fresh` or `rebased_clean`, the agent must not edit tracked files and should leave only an optional no-op summary. A strict final validation step then proves the workspace is clean before publish.

This is proportionate because it reuses the existing helper, jj workflow, validation commands, and prompt style. It does not introduce a service, queue, lock manager, global scheduler, or second implementation queue. The design intentionally does not add a second automated code-review agent after `repair_base_drift`; adding another review cycle would make every clean base refresh more expensive and could recurse into another feedback step. Instead, post-review base-drift edits are contained by a required repair summary, PR-body visibility, publish JSON, narrow prompt rules, and strict final validation. The new persistent interfaces are a helper subcommand, a prompt file, refresh JSON files, repair summary/failure marker files, and workflow step ordering.

## Alternatives Considered

The simplest alternative is to leave the workflows unchanged and rely on `publish` to rebase and revalidate. That already catches stale-base problems before a PR is opened, but it catches them after the last agent step. It provides no automated repair path for conflicts or validation failures, which is the current operator pain.

Another alternative is to serialize all implementation workflows so only one run can be active against the repository at a time. That would reduce base drift, but it would also remove Scherzo's useful parallelism and still would not eliminate drift from human merges or external automation. The Linear issue explicitly rejects repo-wide serialization, so this plan does not use it.

A third alternative is to add only a pre-implementation fetch check. That would prevent some stale starts, but it would not help when the base changes during implementation or review. The final refresh is necessary because long-running agent workflows can drift after they begin.

A fourth alternative is to make the repair agent rerun the entire implementation prompt whenever validation fails. That is too broad and risks changing behavior unrelated to base drift. This plan constrains repair to materialized conflicts and validation failures that occur after a recorded clean rebase.

## Risks and Countermeasures

The main risk is that rebasing before implementation changes the baseline used by `analyze`. The existing helper records `base_change_id` during `prepare`, and `analyze` diffs from that change id. If a start-of-run refresh rebases `@` onto a newer base without updating this metadata, later analysis could include upstream changes as if the agent made them. The countermeasure is to make `refresh-base --stage before-implementation` update `tmp/scherzo-implementation.json` so `base_change_id` points at the current parent after the successful start refresh. The helper should keep the original value in an audit field such as `initial_base_change_id` if it was changed.

Another risk is over-repair: an agent might treat any failing validation as base drift and modify implementation logic. The countermeasure is a strict prompt contract, an explicit state table, and a failure marker. The repair prompt may edit only when the refresh JSON reports `status: conflicts`, or reports `status: rebased_clean` and the validation step failed. If validation succeeded and the refresh status is `fresh` or `rebased_clean`, the agent must not edit tracked files and may only write a no-op summary. If the refresh status is `fresh` and validation failed, the agent must write `tmp/scherzo-implementation-base-drift-failure.md` explaining that the failure is not base drift and leave source files unchanged. The strict final validation step must fail when that marker exists.

A third risk is that `repair_base_drift` edits files after the existing automated code-review gate. The countermeasure is not to add a second broad review loop inside this plan. Instead, every non-no-op repair must write `tmp/scherzo-implementation-base-drift-repair.md` with the refresh status, validation status, exact files changed, and why each edit is mechanical. `publish` must include that summary in `tmp/scherzo-implementation-publish.json` and append a short `Base drift repair` section to the PR body so human reviewers can see that post-review edits occurred. Prompt smoke tests and publish tests must protect this visibility contract.

A fourth risk is that a repair agent resolves semantic conflicts by guessing product behavior. The countermeasure is to borrow the existing merge-conflict workflow policy: preserve the implementation's intended behavior while incorporating mechanical base-side changes; if a behavior choice is needed, write the failure marker and stop.

A fifth risk is a race after final validation. The base can advance between the final refresh and `publish`. The countermeasure is to keep a publish-time base normalization guard and treat it as a safety-critical test surface. If publish has to rebase and revalidation passes, it may proceed. If publish sees conflicts or validation failure after this last-moment rebase, it must fail before `jj describe`, `jj bookmark set`, `jj git push`, `gh pr create`, or retention-marker clearing. It must keep the workspace retained, write structured refresh status for `stage: publish`, and tell the operator to rerun or manually repair. Fake-command publish tests must assert those side effects do not happen in failing cases.

A sixth risk is that new status output becomes hard for prompts and tests to parse. The countermeasure is a stable JSON schema plus simple stdout key-value lines. Tests should assert both, including the latest-refresh JSON copy, stage-name rejection, and every declared status branch.

## Progress

- [x] (2026-05-04 00:00Z) Drafted the ExecPlan proposal for review.
- [x] (2026-05-04 00:00Z) Incorporated adversarial review findings about clean-rebase no-op repair, post-repair containment, publish-time safety tests, expanded helper branches, and manual dry-run scope.
- [x] (2026-05-05 16:05Z) Implemented and tested `scripts/scherzo-implementation refresh-base`, including stable stdout, stage/latest JSON, start metadata updates, conflict/fetch/base-missing/rebase-failed statuses, and publish-time reuse.
- [x] (2026-05-05 16:05Z) Added the `repair_base_drift` prompt, validation guards for base-drift failure markers and unresolved jj conflicts, and PR/publish JSON visibility for repair summaries.
- [x] (2026-05-05 16:05Z) Updated both implementation workflow YAML files with start refresh, pre-validation refresh, continue-on-failure validation, repair, strict final validation, and publish ordering.
- [x] (2026-05-05 16:05Z) Validated helper, workflow ordering, fallback behavior, formatting, and the full Gleam test suite locally.
- [x] (2026-05-05 16:18Z) Applied the review-feedback handoff. No remaining review findings required code changes; confirmed the source-preparation alias coverage includes both `prepare_plan` and `prepare_context`, and reran format plus full Gleam validation successfully.

## Surprises & Discoveries

- Observation: The current implementation helper already fetches and normalizes the publish base in `publish`, and revalidates after a successful publish-time rebase.
  Evidence: `scripts/scherzo-implementation` defines `fetch_publish_base`, `publish_base_revision`, `normalize_publish_base`, and calls `validate_command()` again from `publish_command()` when `normalize_publish_base` returns true.
- Observation: The existing workflow engine supports `on_failure: continue`, which is already used to route failed ExecPlan validation through a repair prompt.
  Evidence: `.scherzo/workflows/execplan.yaml` runs `validate_draft` with `on_failure: continue`, then invokes `repair_validation`.
- Observation: There is already a narrow merge-conflict repair prompt and helper that record conflicted files, constrain edits, and require a summary or failure file.
  Evidence: `.scherzo/workflows/prompts/resolve-merge-conflicts.md` and `scripts/scherzo-merge-conflict` provide the policy model for base-drift conflict repair.
- Observation: Workflow prompt templates do not support optional missing step variables; referencing both `steps.prepare_context.stdout` and `steps.prepare_plan.stdout` in one shared prompt would fail in one workflow or the other.
  Evidence: `src/scherzo/template.gleam` returns `TemplateRenderError("unknown variable ...")` for missing locals, and `test/template_test.gleam` asserts that unknown artifact variables still fail.
- Observation: The checked-in implementation workflows currently use `SCHERZO_CONFIG_DIR` for repository-root discovery, not the older `pwd | sed` prefix shown in the plan's interface example.
  Evidence: `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml` used `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}` before this implementation.

## Decision Log

- Decision: Build base refresh into `scripts/scherzo-implementation` rather than adding separate shell snippets to workflow YAML.
  Rationale: The current workflows already centralize prepare, analyze, validate, and publish in this helper. A helper subcommand is testable with fake `jj` commands and gives prompts structured JSON instead of brittle shell output.
  Date: 2026-05-04
- Decision: Refresh once before implementation and once before final validation.
  Rationale: The first refresh catches stale workspaces before agent time is spent. The second refresh catches drift that happens during implementation, review, and feedback, while an agent can still repair the change before publish.
  Date: 2026-05-04
- Decision: Use `on_failure: continue` only around the pre-validation refresh and validation window, not around the start-of-run refresh.
  Rationale: Before implementation starts there should be no meaningful user changes to repair. A start-of-run fetch or rebase failure should fail early. After implementation, conflicts and clean-rebase validation failures are repairable and should flow to the `repair_base_drift` agent.
  Date: 2026-05-04
- Decision: Make `fresh` plus successful validation and `rebased_clean` plus successful validation explicit no-op states for `repair_base_drift`.
  Rationale: The repair agent runs unconditionally after a continue-on-failure validation step. Without an explicit no-op rule, a clean base refresh followed by passing tests could invite unnecessary edits after code review.
  Date: 2026-05-04
- Decision: Do not add a second automated code-review step after base-drift repair.
  Rationale: The repair scope is intentionally narrow and should not restart the implementation-review loop. The safety tradeoff is accepted only with compensating containment: required repair summaries for non-no-op edits, PR-body visibility, publish JSON visibility, strict final validation, and tests that protect those contracts.
  Date: 2026-05-04
- Decision: Treat publish-time normalization failures as a first-class safety surface in tests.
  Rationale: Publish is the last chance to catch a base move before a PR is created. Tests must prove conflicts or revalidation failures after this last-moment rebase cannot push, bookmark, create a PR, or clear `.scherzo-keep-workspace`.
  Date: 2026-05-04
- Decision: Do not serialize implementation workflows across the repository.
  Rationale: Per-workspace refresh and repair directly addresses stale bases while preserving parallelism.
  Date: 2026-05-04
- Decision: Add a `steps.source_preparation.*` template alias for implementation prepare artifacts and have the shared repair prompt use that alias.
  Rationale: The direct implementation workflow names its prepare step `prepare_context`, while the ExecPlan implementation workflow names it `prepare_plan`. The existing template engine intentionally fails on unknown artifact variables, so an alias preserves the prompt's source-preparation context without weakening unknown-variable validation globally.
  Date: 2026-05-05
- Decision: Preserve the current `SCHERZO_CONFIG_DIR` repository-root discovery convention while adding new refresh commands to workflow YAML.
  Rationale: The plan's `pwd | sed` prefix was stale relative to the current checked-in workflows. Keeping the current convention minimizes unrelated YAML churn and matches repository-local behavior.
  Date: 2026-05-05

## Outcomes & Retrospective

Completed on 2026-05-05. The implementation workflows now refresh their PR base before agent work and before final validation, route repairable pre-validation base drift through a constrained agent prompt, and keep a strict validation gate before publish. The shared helper now produces machine-readable refresh JSON and stable `REFRESH_BASE_*` stdout lines, publish reuses the same refresh path as a last-moment safety guard, and repair summaries are exposed in both PR body content and publish JSON. The main plan deviation was adding a template alias for source preparation output so one shared prompt can render in both workflows without changing the template engine's intentional unknown-variable failures.

Post-review feedback application on 2026-05-05 found no remaining findings to address. The expanded source-preparation alias coverage includes both implementation prepare step names, and the full validation suite remained green.

## Context and Orientation

The repository uses Scherzo workflow YAML files under `.scherzo/workflows/` to define step ordering. A command step runs a shell command. An agent step runs pi with a prompt from `.scherzo/workflows/prompts/`. The `workspace: main` setting means the step runs inside the workflow's dedicated jj workspace.

The direct implementation workflow is `.scherzo/workflows/implementation.yaml`. It currently runs `prepare_context`, an `implement` agent, `analyze_changes`, `code_review`, `apply_feedback`, `final_validate`, and `publish_pr`. The prepare, analyze, validate, and publish command steps call `scripts/scherzo-implementation`.

The ExecPlan implementation workflow is `.scherzo/workflows/execplan-implementation.yaml`. It currently runs `prepare_plan`, an `implement_plan` agent, `analyze_changes`, `review_changes`, `apply_review_feedback`, `final_validate`, and `publish_pr`. It uses the same helper script, with `prepare --source execplan` instead of `prepare --source ticket`.

The shared helper is `scripts/scherzo-implementation`, a Python script. It writes workflow metadata to `tmp/scherzo-implementation.json`, analysis output to `tmp/scherzo-implementation-analysis.json`, validation output to `tmp/scherzo-implementation-validation.json`, and publish output to `tmp/scherzo-implementation-publish.json`. Its `prepare` command records `base_change_id`, which is the current parent change id at prepare time. Its `analyze` command diffs from that recorded base to `@`. Its `validate` command runs `direnv allow .`, removes `test/tmp`, checks Gleam formatting, and runs the Gleam test suite. Its `publish` command fetches the configured base branch, rebases `@` onto that base if needed, revalidates after a rebase, creates or finds a GitHub PR, and clears the retention marker.

The merge-conflict workflow under `.scherzo/workflows/merge-conflict-resolution.yaml` is a useful pattern but is not the target of this plan. Its prompt `.scherzo/workflows/prompts/resolve-merge-conflicts.md` shows how to constrain a repair agent to resolving only conflicts without choosing new product behavior.

The tests for helper behavior live in `test/execplan_implementation_helper_test.gleam`. They use `command_step.run` to run scripts through fake executables in `test/tmp`, then assert on stdout, stderr, JSON files, and fake command logs. New helper and workflow tests should follow that style.

## Preconditions and Verified Facts

The following facts were checked in the current tree before writing this plan.

`jj status --color=never` reported a clean working copy before drafting this plan.

`docs/plans/` did not already contain a file matching `LIV-69-*.md`.

`.scherzo/workflows/implementation.yaml` currently has no base-refresh or repair-base-drift steps. Its final validation step runs:

    repo_root=${SCHERZO_REPO_ROOT:-$(pwd -P | sed "s#/.scherzo/workspaces/.*##")}; "$repo_root/scripts/scherzo-implementation" validate

and its publish step runs:

    repo_root=${SCHERZO_REPO_ROOT:-$(pwd -P | sed "s#/.scherzo/workspaces/.*##")}; "$repo_root/scripts/scherzo-implementation" publish

`.scherzo/workflows/execplan-implementation.yaml` has the same final validation and publish shape, with different agent prompt names.

`scripts/scherzo-implementation` currently accepts `prepare`, `analyze`, `validate`, `publish`, `extract-plan`, `languages`, and `ticket-brief`. It does not accept `refresh-base` yet.

`scripts/scherzo-implementation publish` already reads `SCHERZO_PR_BASE`, defaulting to `main`, and `SCHERZO_PR_REMOTE`, defaulting to `origin`. The new helper must reuse those environment variables to avoid adding a second base-selection mechanism.

The existing workflow engine supports `on_failure: continue`, as shown in `.scherzo/workflows/execplan.yaml`.

## Scope Boundaries

In scope:

- Add `refresh-base` to `scripts/scherzo-implementation`.
- Add structured JSON files under `tmp/` for base refresh status.
- Reuse or refactor existing publish-base functions in `scripts/scherzo-implementation` so publish and refresh agree on base selection and conflict detection.
- Add a new repair prompt at `.scherzo/workflows/prompts/repair-base-drift.md`.
- Update `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml` step ordering.
- Add or update tests in `test/execplan_implementation_helper_test.gleam` for helper behavior, workflow ordering, prompt presence, publish-time safety, and repair-summary publication.
- Update `scripts/scherzo-implementation validate` to fail clearly when the repair prompt writes a base-drift failure marker or when `@` still has unresolved jj conflicts.
- Update `scripts/scherzo-implementation publish` to include a base-drift repair summary in publish JSON and the PR body when that summary exists.

Out of scope:

- Do not add a repository-wide implementation queue or lock.
- Do not change the merge-conflict-resolution workflow, except by reusing its policy ideas in the new prompt.
- Do not change the ExecPlan drafting workflow except indirectly through tests if existing shared helper tests need naming adjustments.
- Do not alter project validation commands beyond adding preflight guards for unresolved base-drift failure markers or unresolved jj conflicts.
- Do not change how Linear issues are fetched for implementation or ExecPlan implementation.
- Do not change GitHub PR creation semantics except for reusing the new refresh-base internals at publish time, blocking unsafe publish-time base-drift failures, and appending the base-drift repair summary when one exists.

## Milestones

Milestone 1 adds the reusable helper and tests it without changing workflow behavior. At the end, `scripts/scherzo-implementation refresh-base --stage before-implementation`, `scripts/scherzo-implementation refresh-base --stage before-validation`, and `scripts/scherzo-implementation refresh-base --stage publish` exist, print stable status lines, write stage-specific JSON, and update `tmp/scherzo-implementation-refresh-base-latest.json`. This milestone comes first because every workflow and prompt change depends on trustworthy machine-readable refresh status.

Milestone 2 adds validation guards, publish-time safety containment, and the `repair_base_drift` prompt. At the end, a workflow can route a failed refresh or failed validation to an agent that knows exactly when to edit, when to no-op, and when to write a failure marker; publish can include any repair summary in both JSON and the PR body; and publish-time conflicts or revalidation failures stop before bookmark, push, PR creation, or retention-marker clearing. This milestone comes before YAML wiring so the workflow has a safe repair contract before it invokes the prompt.

Milestone 3 wires both implementation workflows. At the end, each workflow fetches and normalizes the base before implementation, refreshes again before final validation, runs validation with `on_failure: continue`, invokes `repair_base_drift`, runs strict final validation, and only then publishes.

Milestone 4 validates rollout and fallback behavior. At the end, tests cover fresh base, stale clean rebase, successful clean-rebase no-op repair, unresolved conflicts, clean rebase followed by validation failure, `fetch_failed`, `base_not_found`, `rebase_failed`, latest JSON creation, stage-name sanitization, unresolved-conflict validation guards, publish-time safety, repair-summary visibility, and YAML step ordering. The implementer documents how to roll back by reverting the workflow YAML and helper changes if dogfood runs expose a false positive.

## Plan of Work

In `scripts/scherzo-implementation`, extend `usage()` to list `refresh-base`. Add constants for `BASE_REFRESH_LATEST_PATH`, `BASE_DRIFT_REPAIR_SUMMARY_PATH`, and `BASE_DRIFT_REPAIR_FAILURE_PATH`, and keep using the existing `RETENTION_MARKER = ".scherzo-keep-workspace"`. Define a helper that turns a stage name into a safe JSON path, for example `tmp/scherzo-implementation-refresh-base-before-validation.json`. Accept only simple stage names that match `^[A-Za-z0-9][A-Za-z0-9._-]*$`; reject missing stages, unknown flags, path separators, and `.` or `..` path-like names.

Add `refresh_base_command(args)` with the CLI shape:

    scripts/scherzo-implementation refresh-base --stage before-implementation
    scripts/scherzo-implementation refresh-base --stage before-validation
    scripts/scherzo-implementation refresh-base --stage publish

The command should require `jj`, read `SCHERZO_PR_BASE` and `SCHERZO_PR_REMOTE` with the same defaults as publish, fetch the branch from the remote, resolve the base revision in the same way publish does, compare the current parent commit ids with the base revision commit id, and rebase `@` onto the base revision when they differ. Split the implementation into small helpers: one helper reads the configured base and remote, one fetches and returns structured fetch output instead of exiting immediately, one resolves the remote or local fallback base revision, one lists current parent commit ids, one lists unresolved conflicts in `@`, one lists conflicted files, one runs `jj rebase`, and one builds the refresh result dictionary.

The helper must write a JSON object with this schema to both the stage-specific path and `tmp/scherzo-implementation-refresh-base-latest.json`:

    {
      "schema_version": 1,
      "stage": "before-validation",
      "status": "fresh",
      "repairable": false,
      "remote": "origin",
      "base": "main",
      "base_revision": "main@origin",
      "base_commit_before_fetch": null,
      "base_commit_after_fetch": "<commit-id>",
      "parent_commits_before": ["<commit-id>"],
      "parent_commits_after": ["<commit-id>"],
      "current_change_id": "<change-id>",
      "rebased": false,
      "has_unresolved_conflicts": false,
      "conflicted_files": [],
      "metadata_base_change_id_updated": false,
      "message": "current parent already matches publish base",
      "commands": [
        "jj git fetch --remote origin --branch main"
      ]
    }

The `status` field must be one of these exact values:

- `fresh`: the current parent already matches the fetched base revision.
- `rebased_clean`: the helper rebased `@` onto the fetched base and no unresolved conflicts remain.
- `conflicts`: the helper attempted the rebase and `@` now has unresolved conflicts.
- `fetch_failed`: fetching the configured base failed.
- `base_not_found`: neither the remote base revision nor the fallback local base revision exists after fetch.
- `rebase_failed`: the `jj rebase` command itself returned a nonzero exit code for a reason other than materialized conflicts.

The command must print these stable stdout lines in addition to any useful human text:

    REFRESH_BASE_STATUS=fresh
    REFRESH_BASE_REPAIRABLE=false
    REFRESH_BASE_REBASED=false
    REFRESH_BASE_BASE_REVISION=main@origin
    REFRESH_BASE_JSON=tmp/scherzo-implementation-refresh-base-before-validation.json
    REFRESH_BASE_CONFLICTED_FILES:
    - None

For `conflicts`, print `REFRESH_BASE_REPAIRABLE=true` and list conflicted paths under `REFRESH_BASE_CONFLICTED_FILES`. The helper should get conflicted paths from `jj resolve --list --color=never`, using the same cleanup rule as `scripts/scherzo-merge-conflict` for lines ending in `N-sided conflict`. If `jj resolve --list` reports no conflicts but the revset `conflicts() & (@)` still matches, report an empty list with `has_unresolved_conflicts: true` and a message saying the conflicted file list could not be determined.

Exit code rules are part of the interface. Exit `0` for `fresh` and `rebased_clean`. Exit `20` for `conflicts`, because this is repairable when the workflow step has `on_failure: continue`. Exit `1` for `fetch_failed`, `base_not_found`, and `rebase_failed`. The start-of-run workflow step will not use `on_failure: continue`, so any nonzero result there fails early. The pre-validation workflow step will use `on_failure: continue`, so the repair prompt can inspect the JSON and decide whether the failure is repairable.

When the stage is `before-implementation` and the status is `rebased_clean` or `fresh`, update `tmp/scherzo-implementation.json` so `base_change_id` equals the current parent change id after refresh. If the previous value differed, preserve it in `initial_base_change_id` and set `metadata_base_change_id_updated` to true in the refresh JSON. Do not update `base_change_id` in later stages, because after implementation the existing analysis baseline should remain an audit record of the workflow's implementation diff.

Refactor `publish_command()` so it uses the same low-level refresh internals for `stage: publish`. Publish should still perform a last-moment base check. If publish must rebase and validation passes, it may proceed as today. If publish sees conflicts, `base_not_found`, `fetch_failed`, `rebase_failed`, or validation failure after a last-moment rebase, it must fail before `jj describe`, `jj bookmark set`, `jj git push`, `gh pr create`, or `clear_retention_marker()`. The failure path must leave `.scherzo-keep-workspace` in place, write the publish-stage refresh JSON and latest-refresh JSON, and print a message explaining that the base moved after the repair window.

Still in `publish_command()`, include any repair summary in the publication artifacts. If `tmp/scherzo-implementation-base-drift-repair.md` exists, append a `Base drift repair` section to `tmp/scherzo-implementation-pr-body.md` before `gh pr create`, and write `base_drift_repair_summary_path` and `base_drift_repair_summary_included: true` into `tmp/scherzo-implementation-publish.json`. If the summary file is absent, write `base_drift_repair_summary_included: false`.

In `scripts/scherzo-implementation validate`, add preflight guards before running `direnv`. If `tmp/scherzo-implementation-base-drift-failure.md` exists, fail and print its contents. If `jj` is available and `@` has unresolved conflicts, fail with a message naming the conflicted files when possible. These guards keep final validation from hiding an explicit repair failure behind a generic formatter or compiler error.

Create `.scherzo/workflows/prompts/repair-base-drift.md`. The prompt must include the Linear issue metadata, source preparation stdout, refresh stdout, refresh stderr, refresh exit code, validation stdout, validation stderr, and validation exit code. In this prompt, "validation succeeded" means the `validate_after_refresh` command exited `0`; "validation failed" means it exited nonzero. The prompt contract must say:

- Do not manage jj workspaces, branches, bookmarks, pushes, or PRs.
- Read `tmp/scherzo-implementation-refresh-base-before-validation.json` when it exists, and otherwise read `tmp/scherzo-implementation-refresh-base-latest.json`.
- If refresh status is `fresh` or `rebased_clean` and validation succeeded, do not edit tracked files and do not write the failure marker. Optionally write `tmp/scherzo-implementation-base-drift-repair.md` as a no-op summary.
- If refresh status is `fresh` and validation failed, do not repair; write `tmp/scherzo-implementation-base-drift-failure.md` saying validation failed without recorded base drift.
- If refresh status is `conflicts`, inspect only the conflicted files and the smallest nearby context needed to resolve mechanical conflicts. Resolve conflict markers, preserve intended implementation behavior, and write `tmp/scherzo-implementation-base-drift-repair.md` summarizing the resolution. If a behavior decision is needed, write the failure marker and stop.
- If refresh status is `rebased_clean` and validation failed, inspect the validation output and the changed files. Make only the smallest mechanical edits needed to adapt the implementation to the new base. Examples include renamed functions, moved modules, changed imports, formatting expectations, and test fixture updates that preserve intended behavior. Do not add new features or change requirements.
- If refresh status is `fetch_failed`, `base_not_found`, or `rebase_failed`, do not edit source files; write the failure marker with the nonrepairable reason.
- If the agent edits any tracked source, test, workflow, or documentation file, it must write `tmp/scherzo-implementation-base-drift-repair.md` with the refresh status, validation exit code, exact files changed, and why each edit is mechanical rather than a product decision.
- Run targeted checks only if cheap. The strict final validation command is responsible for the full suite.

Update `.scherzo/workflows/implementation.yaml` to this order:

    prepare_context
    refresh_base_before_implementation
    implement
    analyze_changes
    code_review
    apply_feedback
    refresh_base_before_validation
    validate_after_refresh
    repair_base_drift
    final_validate
    publish_pr

The new `refresh_base_before_implementation` command depends on `prepare_context`, runs `scripts/scherzo-implementation refresh-base --stage before-implementation`, has a timeout similar to prepare, and does not set `on_failure: continue`.

The existing `implement` step depends on `refresh_base_before_implementation` instead of `prepare_context`.

The new `refresh_base_before_validation` command depends on `apply_feedback`, runs `scripts/scherzo-implementation refresh-base --stage before-validation`, has a timeout similar to publish or prepare, and sets `on_failure: continue`.

Rename the old strict `final_validate` position to `validate_after_refresh`, depend it on `refresh_base_before_validation`, run `scripts/scherzo-implementation validate`, and set `on_failure: continue`.

Add the `repair_base_drift` agent step depending on `validate_after_refresh`, using `prompts/repair-base-drift.md`.

Add a new strict `final_validate` command depending on `repair_base_drift`, running `scripts/scherzo-implementation validate` without `on_failure: continue`.

Keep `publish_pr` depending on strict `final_validate`.

Update `.scherzo/workflows/execplan-implementation.yaml` with the same ordering, replacing `prepare_context` with `prepare_plan`, `implement` with `implement_plan`, `code_review` with `review_changes`, and `apply_feedback` with `apply_review_feedback`.

## Concrete Steps

1. From the repository root, run `jj status --color=never` and confirm whether the working copy is clean or contains only intentional implementation changes for this plan.

2. Edit `scripts/scherzo-implementation` to add `BASE_REFRESH_LATEST_PATH`, `BASE_DRIFT_REPAIR_SUMMARY_PATH`, and `BASE_DRIFT_REPAIR_FAILURE_PATH` near the existing `TMP_DIR` constants.

3. Edit `scripts/scherzo-implementation` to update `usage()` and `main()` so `refresh-base` is documented and dispatches to `refresh_base_command(args)`.

4. Add `parse_refresh_base_args(args)` in `scripts/scherzo-implementation`. It must accept exactly `--stage <stage>`, reject missing `--stage`, reject unknown flags, reject path separators, reject `.` and `..`, and reject stage names that do not match `^[A-Za-z0-9][A-Za-z0-9._-]*$`.

5. Add `refresh_base_json_path(stage)` in `scripts/scherzo-implementation`. It must return `tmp/scherzo-implementation-refresh-base-<stage>.json` for safe stages and must never accept a value that can escape `tmp/`.

6. Add a small helper in `scripts/scherzo-implementation` that returns the configured publish base and remote from `SCHERZO_PR_BASE` and `SCHERZO_PR_REMOTE`, defaulting to `main` and `origin`. Use this helper from both `refresh-base` and `publish`.

7. Refactor the existing fetch logic into a helper that runs `jj git fetch --remote <remote> --branch <base>` and returns a structured result containing the command, exit code, stdout, and stderr. Keep the existing publish behavior unchanged until `publish_command()` is explicitly adapted later.

8. Refactor base-revision resolution into a helper that can return `main@origin`, a local fallback revision, or a `base_not_found` result without immediately exiting the process.

9. Add helpers for `base_commit_ids(revision)`, `current_parent_commit_ids()`, and `current_change_id_short()` if the existing helpers cannot already return the exact values needed by refresh JSON.

10. Add `has_unresolved_conflicts_at_current_change()` in `scripts/scherzo-implementation`. It should use the jj conflict revset for `@` and return a boolean without printing user-facing output.

11. Add `conflicted_files()` in `scripts/scherzo-implementation`. It should parse `jj resolve --list --color=never`, strip the same `N-sided conflict` suffix style used by `scripts/scherzo-merge-conflict`, and return repository-relative paths.

12. Add a low-level `refresh_base(stage)` helper that fetches the configured base, resolves the base revision, compares current parent commits to base commits, and returns a result dictionary with `status: fresh`, `fetch_failed`, or `base_not_found` before any rebase logic is added.

13. Extend `refresh_base(stage)` to run `jj rebase -r @ -d <base_revision> --color=never` when the current parent commits differ from the base commits.

14. Extend `refresh_base(stage)` to distinguish `rebased_clean`, `conflicts`, and `rebase_failed` after the rebase attempt. A nonzero `jj rebase` followed by unresolved conflicts in `@` should become `status: conflicts`; a nonzero rebase without materialized conflicts should become `status: rebase_failed`.

15. Add a writer helper that writes the refresh result to the stage-specific path and to `tmp/scherzo-implementation-refresh-base-latest.json`, then prints the stable `REFRESH_BASE_*` stdout lines.

16. Add `refresh_base_command(args)` using the parser, `refresh_base(stage)`, JSON writer, stdout lines, and exit code rules from the Plan of Work.

17. Add metadata update logic for `--stage before-implementation`. After a successful start refresh, update `tmp/scherzo-implementation.json` so `base_change_id` matches the current parent change id, preserving the previous value in `initial_base_change_id` when it changes.

18. Update `publish_command()` so it calls the shared refresh internals with `stage: publish` after validation has passed and before any publish side effects. Preserve the existing behavior that a clean publish-time rebase triggers `validate_command()` and `ensure_validation_passed()` again.

19. In `publish_command()`, make every publish-time refresh failure and every publish-time revalidation failure return before `jj describe`, `jj bookmark set`, `jj git push`, `gh pr create`, and `clear_retention_marker()`.

20. In `publish_command()`, append `tmp/scherzo-implementation-base-drift-repair.md` to the PR body under a `Base drift repair` heading when the summary exists, and record whether the summary was included in `tmp/scherzo-implementation-publish.json`.

21. Update `validate_command()` to fail early when `tmp/scherzo-implementation-base-drift-failure.md` exists. The failure output should include the marker contents and fake validation tests should prove `direnv` was not invoked.

22. Update `validate_command()` to fail early when `jj` is available and `@` has unresolved conflicts. The failure output should name conflicted files when `jj resolve --list --color=never` reports them.

23. In `test/execplan_implementation_helper_test.gleam`, add `refresh_base_reports_fresh_base_test`. The fixture fake `jj` should make `@-` and `main@origin` resolve to the same commit id. Assert exit code `0`, stdout contains `REFRESH_BASE_STATUS=fresh`, stdout contains `REFRESH_BASE_REPAIRABLE=false`, no `jj rebase` entry appears in the fake log, and the JSON file contains `"status": "fresh"`.

24. Add `refresh_base_rebases_stale_base_and_updates_start_metadata_test`. The fixture should include `tmp/scherzo-implementation.json` with `base_change_id: "old-base"`, make `main@origin` resolve to a different commit, and make `jj rebase -r @ -d main@origin --color=never` succeed. Run `refresh-base --stage before-implementation`. Assert stdout contains `REFRESH_BASE_STATUS=rebased_clean`, the fake `jj` log contains `git fetch --remote origin --branch main` and `rebase -r @ -d main@origin --color=never`, and metadata now contains the refreshed base change id plus `initial_base_change_id`.

25. Add `refresh_base_reports_repairable_conflicts_test`. The fake `jj` should make rebase materialize a conflict and make `jj resolve --list --color=never` print one conflicted file. Assert exit code `20`, stdout contains `REFRESH_BASE_STATUS=conflicts`, stdout contains `REFRESH_BASE_REPAIRABLE=true`, stdout lists the conflicted file, and JSON contains `"has_unresolved_conflicts": true`.

26. Add `refresh_base_fetch_failure_is_nonrepairable_test`. The fake `jj git fetch` should fail. Assert exit code `1`, stderr or stdout explains the fetch failure, JSON contains `"status": "fetch_failed"`, and JSON contains `"repairable": false`.

27. Add `refresh_base_base_not_found_is_nonrepairable_test`. The fake fetch should succeed, but fake revision resolution should fail for both `main@origin` and the local fallback. Assert exit code `1`, JSON contains `"status": "base_not_found"`, and no `jj rebase` entry appears in the fake log.

28. Add `refresh_base_rebase_failed_without_conflicts_is_nonrepairable_test`. The fake `jj rebase` should return nonzero, the conflict revset should report no conflict at `@`, and `jj resolve --list --color=never` should be empty. Assert exit code `1`, JSON contains `"status": "rebase_failed"`, and JSON contains `"repairable": false`.

29. Add `refresh_base_rejects_unsafe_stage_and_writes_latest_json_test`. First run `refresh-base --stage ../bad` and assert exit code `1`, no JSON is written outside `tmp/`, and stderr explains the invalid stage. Then run a valid stage and assert both the stage-specific JSON file and `tmp/scherzo-implementation-refresh-base-latest.json` contain the same `stage` and `status` values.

30. Add `validate_fails_on_base_drift_failure_marker_test`. Create `tmp/scherzo-implementation-base-drift-failure.md`, use fake `direnv` that would otherwise pass, run `scripts/scherzo-implementation validate`, and assert it fails before invoking fake `direnv`.

31. Add `validate_fails_on_unresolved_jj_conflicts_test`. Fake `jj` should report a conflict in `@` and `jj resolve --list --color=never` should name one file. Assert validation fails before fake `direnv`, and stdout or stderr names the conflicted file.

32. Add `publish_time_conflicts_do_not_publish_test`. Fake the publish-time refresh so it writes `status: conflicts`. Assert publish exits nonzero, `jj bookmark set`, `jj git push`, and `gh pr create` are absent from fake logs, `.scherzo-keep-workspace` still exists, and `tmp/scherzo-implementation-refresh-base-publish.json` records `"status": "conflicts"`.

33. Add `publish_time_revalidation_failure_does_not_publish_test`. Fake a publish-time clean rebase, then make fake `direnv` or `gleam` fail during the required revalidation. Assert publish exits nonzero, no bookmark, push, PR creation, or retention-marker clearing occurs, and the publish-stage refresh JSON records `"status": "rebased_clean"`.

34. Add `publish_time_revalidation_success_may_publish_test`. Fake a publish-time clean rebase followed by passing validation. Assert publish proceeds to `jj bookmark set`, `jj git push`, and PR view or creation, and assert `.scherzo-keep-workspace` is removed only after PR publication succeeds.

35. Add `publish_includes_base_drift_repair_summary_test`. Create `tmp/scherzo-implementation-base-drift-repair.md` before a successful publish. Assert `tmp/scherzo-implementation-pr-body.md` contains `Base drift repair`, `tmp/scherzo-implementation-publish.json` contains `"base_drift_repair_summary_included": true`, and a publish without the summary records `false`.

36. Create `.scherzo/workflows/prompts/repair-base-drift.md` using the prompt contract in the Plan of Work.

37. In `test/execplan_implementation_helper_test.gleam`, add a prompt smoke test that reads `.scherzo/workflows/prompts/repair-base-drift.md` and asserts it contains `tmp/scherzo-implementation-refresh-base`, `rebased_clean`, `conflicts`, the `rebased_clean` plus validation-success no-op rule, `tmp/scherzo-implementation-base-drift-repair.md`, `tmp/scherzo-implementation-base-drift-failure.md`, and the instruction not to manage jj workspaces, branches, bookmarks, pushes, or PRs.

38. Edit `.scherzo/workflows/implementation.yaml` to add `refresh_base_before_implementation`, `refresh_base_before_validation`, `validate_after_refresh`, and `repair_base_drift` with the ordering described above.

39. Edit `.scherzo/workflows/execplan-implementation.yaml` with the analogous steps and dependencies.

40. Add workflow ordering tests in `test/execplan_implementation_helper_test.gleam`. Read both YAML files as strings and assert each contains `refresh_base_before_implementation`, `refresh_base_before_validation`, `validate_after_refresh`, `repair_base_drift`, `on_failure: continue` on the pre-validation refresh and validation steps, and `depends_on` lines that put strict `final_validate` after `repair_base_drift` and `publish_pr` after strict `final_validate`.

41. Run the targeted test file from the repository root:

    direnv allow .
    direnv exec . gleam test --target erlang test/execplan_implementation_helper_test.gleam

    If the project test runner does not support passing a single file, run:

    direnv exec . gleam test

    Expected result after implementation is that all tests pass. During red-phase development, the new tests should fail before the helper, prompt, and workflow changes are made.

42. Run formatting from the repository root:

    direnv exec . gleam format --check src test

    Expected result is successful format validation with exit code `0`.

43. Run the full test suite from the repository root:

    rm -rf test/tmp
    direnv exec . gleam test

    Expected result is exit code `0` and no failing Gleam tests.

44. Commit after Milestone 1 once helper tests through `refresh_base_rejects_unsafe_stage_and_writes_latest_json_test` pass. Suggested commit message: `feat: add implementation base refresh helper`.

45. Commit after Milestone 2 once validation-guard, publish-safety, repair-summary, and prompt tests pass. Suggested commit message: `feat: add base drift repair safeguards`.

46. Commit after Milestone 3 once workflow ordering tests pass. Suggested commit message: `feat: refresh implementation bases before validation`.

47. Before opening a PR, run `jj status --color=never` and confirm that only intended files changed.

## Testing and Falsifiability

The helper tests should prove that the claimed refresh behavior is real without depending on a live remote. Use fake executables in `test/tmp`, following the existing style in `test/execplan_implementation_helper_test.gleam`. Each fake-command test should assert both human-readable stdout and machine-readable JSON so that prompts and operators do not depend on undocumented behavior.

Test `refresh_base_reports_fresh_base_test` falsifies the claim that the helper can no-op safely. It should set up fake `jj` output so the current parent and `main@origin` have the same commit id. The expected assertions are exit code `0`, `REFRESH_BASE_STATUS=fresh`, no `jj rebase` entry in the fake log, and JSON with `rebased: false`.

Test `refresh_base_rebases_stale_base_and_updates_start_metadata_test` falsifies the claim that stale starts are corrected before implementation. It should set up a stale parent, run `refresh-base --stage before-implementation`, and assert that `jj rebase -r @ -d main@origin --color=never` was invoked. It must also assert that `tmp/scherzo-implementation.json` no longer uses the old base as `base_change_id`; otherwise `analyze` could include upstream changes.

Test `refresh_base_reports_repairable_conflicts_test` falsifies the claim that unresolved rebase conflicts are visible to the workflow. It should make conflict detection report a conflicted file. The expected result is exit code `20`, JSON `status: conflicts`, `repairable: true`, and stdout listing the conflicted file.

Test `refresh_base_fetch_failure_is_nonrepairable_test` falsifies the claim that fetch failures are distinguishable. It should make `jj git fetch` fail and assert JSON `status: fetch_failed`, `repairable: false`, and exit code `1`.

Test `refresh_base_base_not_found_is_nonrepairable_test` falsifies the claim that a missing configured base cannot be mistaken for a repairable conflict. It should make fetch succeed but base revision resolution fail for both the remote and fallback local names. The expected result is exit code `1`, JSON `status: base_not_found`, `repairable: false`, and no rebase invocation.

Test `refresh_base_rebase_failed_without_conflicts_is_nonrepairable_test` falsifies the claim that rebase infrastructure failures are separated from materialized conflicts. It should make `jj rebase` fail while `@` has no unresolved conflicts. The expected result is exit code `1`, JSON `status: rebase_failed`, `repairable: false`, and a message preserving the rebase failure details.

Test `refresh_base_rejects_unsafe_stage_and_writes_latest_json_test` falsifies the path-safety and latest-status contracts. The unsafe stage half should call `refresh-base --stage ../bad` and assert no file appears outside `tmp/`. The latest JSON half should call a valid stage and assert `tmp/scherzo-implementation-refresh-base-latest.json` matches the stage-specific status and stage fields.

Test `validate_fails_on_base_drift_failure_marker_test` falsifies the claim that the repair agent can intentionally stop the workflow. It should create `tmp/scherzo-implementation-base-drift-failure.md`, run `validate`, and assert validation fails before fake `direnv` is invoked.

Test `validate_fails_on_unresolved_jj_conflicts_test` falsifies the claim that unresolved conflicts cannot be hidden behind later formatter or compiler failures. It should make `@` match the conflict revset, make `jj resolve --list --color=never` print one path, and assert validation fails before fake `direnv` while naming that path.

Publish safety tests falsify the highest-blast-radius claim: failed last-moment normalization must not publish. `publish_time_conflicts_do_not_publish_test` should assert that publish-time `status: conflicts` exits nonzero without `jj bookmark set`, `jj git push`, `gh pr create`, or retention-marker clearing. `publish_time_revalidation_failure_does_not_publish_test` should assert the same side-effect absence when publish-time rebase is clean but revalidation fails. `publish_time_revalidation_success_may_publish_test` should assert the positive path still bookmarks, pushes, creates or finds a PR, and clears `.scherzo-keep-workspace` only after successful PR publication.

Test `publish_includes_base_drift_repair_summary_test` falsifies the post-review containment story. It should create `tmp/scherzo-implementation-base-drift-repair.md`, run a successful fake publish, and assert the PR body contains `Base drift repair` and the publish JSON records `base_drift_repair_summary_included: true`. It should also run a successful publish without the summary and assert the JSON records `false`.

Workflow ordering tests falsify the claim that repair happens before publish. They should read `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml` and assert that `publish_pr` depends on strict `final_validate`, strict `final_validate` depends on `repair_base_drift`, `repair_base_drift` depends on `validate_after_refresh`, and both `refresh_base_before_validation` and `validate_after_refresh` use `on_failure: continue`.

Prompt smoke tests falsify the claim that the repair agent is constrained. They should assert that `.scherzo/workflows/prompts/repair-base-drift.md` mentions the refresh JSON path, `conflicts`, `rebased_clean`, the `rebased_clean` plus validation-success no-op rule, the repair summary, the failure marker, and the prohibition on jj workspace, branch, bookmark, push, or PR management.

The plan no longer requires a manual dry run or reconstruction of the historical `LIV-59` and `LIV-65` workspaces for acceptance. The `LIV-59`-style conflict drift is falsified by the conflict refresh test plus workflow ordering and publish safety tests. The `LIV-65`-style clean-rebase validation drift is falsified by the clean-rebase tests, prompt no-op/repair rules, validation guard tests, and strict final validation ordering. An operator may still dogfood the workflow in a disposable Scherzo workspace, but that dogfood is optional evidence rather than a required acceptance step.

## Validation and Acceptance

From the repository root, run:

    direnv allow .
    direnv exec . gleam format --check src test
    rm -rf test/tmp
    direnv exec . gleam test

Acceptance for the helper is met when the new helper tests pass and the stdout/JSON interface remains stable. A fresh-base run must print `REFRESH_BASE_STATUS=fresh`. A stale clean rebase must print `REFRESH_BASE_STATUS=rebased_clean`. A conflict must print `REFRESH_BASE_STATUS=conflicts`, exit `20`, set `repairable` to true in JSON, and list conflicted files when jj reports them. Fetch failure, missing base, and non-conflict rebase failure must print or record `fetch_failed`, `base_not_found`, and `rebase_failed` respectively, exit `1`, and set `repairable` to false. Every successful or failed refresh must write the stage-specific JSON path and `tmp/scherzo-implementation-refresh-base-latest.json`.

Acceptance for the repair prompt is met when the prompt smoke test proves the state table is present. In particular, `fresh` plus validation success and `rebased_clean` plus validation success must be explicit no-op states, while `fresh` plus validation failure and all nonrepairable refresh statuses must write `tmp/scherzo-implementation-base-drift-failure.md` instead of editing source files.

Acceptance for post-review repair containment is met when any non-no-op base-drift repair must write `tmp/scherzo-implementation-base-drift-repair.md`, and publish tests prove that summary is copied into `tmp/scherzo-implementation-publish.json` and appended to `tmp/scherzo-implementation-pr-body.md` under a `Base drift repair` heading. This is the compensating control for not running a second automated code-review step after `repair_base_drift`.

Acceptance for publish-time safety is met when fake-command tests prove that publish-time conflicts and publish-time revalidation failures do not run `jj bookmark set`, `jj git push`, or `gh pr create`, do not clear `.scherzo-keep-workspace`, and do write refresh JSON for `stage: publish`. A publish-time clean rebase followed by passing validation must still publish successfully and clear `.scherzo-keep-workspace` only after PR publication succeeds.

Acceptance for workflow ordering is met when both implementation workflow YAML files show the early start refresh before any implementation agent work and the pre-validation repair window before publish. The strict final validation step must occur after `repair_base_drift`, and `publish_pr` must depend on that strict final validation.

Acceptance for `LIV-59` and `LIV-65` is behavioral rather than tied to those exact historical workspaces. For a stale-base conflict like the motivating `LIV-59` class, the workflow must expose the conflict in `refresh_base_before_validation` and either repair it with `repair_base_drift` or fail with a base-drift failure marker before publish. For a clean-rebase validation failure like the motivating `LIV-65` class, the workflow must run validation after the clean rebase with `on_failure: continue`, invoke `repair_base_drift`, and require strict final validation before publish. [CLARIFY] If the exact incident mapping between `LIV-59` and `LIV-65` is different, update this paragraph with the exact postmortem while keeping both covered behaviors.

Acceptance for concurrency is met when no workflow step acquires a repo-wide lock or requires other implementation workflows to pause. Each workflow run fetches the latest base and rebases only its own `@` change.

No manual dry run is required for acceptance. Optional dogfood in a disposable Scherzo workflow workspace is allowed, but if it finds a false positive or false negative, update this ExecPlan before continuing implementation.

## Rollout, Recovery, and Idempotence

The change is additive and reversible. The first safe rollout point is after landing the helper and tests without relying on it from workflow YAML. The second rollout point is after wiring the workflows. If the workflow wiring causes false positives in dogfood runs, revert the YAML ordering to the previous prepare, implement, analyze, review, feedback, validate, publish sequence while leaving the helper in place for further testing.

`refresh-base` is intended to be idempotent. Re-running it when the current parent already equals the fetched base should produce `status: fresh` and make no changes. Re-running it after a clean rebase should also produce `fresh` unless the remote base advanced again. Re-running it while conflicts are unresolved should continue to report `conflicts` and should not hide the conflict state.

If `refresh-base --stage before-implementation` fails, the workflow should stop early. There is no implementation work to preserve, and the operator can retry after fixing remote access or base configuration.

If `refresh-base --stage before-validation` reports conflicts, the workflow keeps going to `repair_base_drift`. If the repair agent cannot resolve safely, it writes `tmp/scherzo-implementation-base-drift-failure.md`; strict validation then fails and the retention marker keeps the workspace available for manual inspection.

If refresh reports `fresh` or `rebased_clean` and validation succeeded, `repair_base_drift` should no-op. This path is safe to repeat, and it must not create source changes after review.

If clean rebase succeeds but validation fails, `repair_base_drift` may make only mechanical compatibility fixes. If it edits any tracked file, it must write `tmp/scherzo-implementation-base-drift-repair.md` so publish can expose the post-review edit in the PR body and publish JSON. If it cannot prove the failure is base drift, it writes the failure marker and leaves source behavior unchanged.

If publish detects a new base drift after strict final validation, publish may rebase and revalidate as a last-moment guard. If that last-moment rebase fails or breaks validation, publish must not describe the change, set a bookmark, push a bookmark, create a PR, or clear `.scherzo-keep-workspace`. It should write a refresh JSON for `stage: publish`, keep the retention marker, and print a message explaining that the base moved after the repair window. The safe recovery is to rerun the implementation workflow or perform a manual repair in the retained workspace.

## Artifacts and Notes

Expected successful fresh refresh output should look like:

    REFRESH_BASE_STATUS=fresh
    REFRESH_BASE_REPAIRABLE=false
    REFRESH_BASE_REBASED=false
    REFRESH_BASE_BASE_REVISION=main@origin
    REFRESH_BASE_JSON=tmp/scherzo-implementation-refresh-base-before-validation.json
    REFRESH_BASE_CONFLICTED_FILES:
    - None

Expected repairable conflict output should look like:

    REFRESH_BASE_STATUS=conflicts
    REFRESH_BASE_REPAIRABLE=true
    REFRESH_BASE_REBASED=true
    REFRESH_BASE_BASE_REVISION=main@origin
    REFRESH_BASE_JSON=tmp/scherzo-implementation-refresh-base-before-validation.json
    REFRESH_BASE_CONFLICTED_FILES:
    - src/example.gleam

Expected no-op repair summary content for `rebased_clean` plus successful validation should look like:

    # Base drift repair summary

    ## Outcome
    No base-drift repair was needed.

    ## Refresh status
    `rebased_clean`

    ## Validation status
    `validate_after_refresh` succeeded, so no tracked files were edited.

Expected publish-time safety failure output should include stable lines like:

    REFRESH_BASE_STATUS=conflicts
    REFRESH_BASE_JSON=tmp/scherzo-implementation-refresh-base-publish.json
    PUBLISH_BLOCKED=true
    PUBLISH_BLOCKED_REASON=base drift after final validation

Expected failure marker content from the repair prompt should look like:

    # Base drift repair failure

    ## Reason
    Validation failed, but the latest refresh status was `fresh`, so this is not classified as repairable base drift.

    ## Required human decision
    Inspect the validation failure and decide whether it is an implementation bug or an unrecorded base-drift case.

Expected repair summary content from the repair prompt should look like:

    # Base drift repair summary

    ## Outcome
    Resolved repairable base drift before final validation.

    ## Refresh status
    `conflicts`

    ## Files changed
    - `src/example.gleam`: reconciled a base-side function rename with the implementation's existing call site.

    ## Validation run by agent
    Not run; strict final validation is handled by the workflow.

    ## Remaining ambiguity
    None.

## Interfaces and Dependencies

No new package dependency is required. The implementation should continue using Python standard library modules already used by `scripts/scherzo-implementation`, including `json`, `os`, `re`, `subprocess`, `sys`, and `pathlib.Path`.

The helper depends on `jj`. Existing validation depends on `direnv`, `gleam`, and the repository's `.envrc`. Existing publish depends on `gh`. The new `refresh-base` command must not depend on `gh` because refreshing the base should work before PR creation.

`scripts/scherzo-implementation` should expose these new or refactored functions. The exact Python names can vary, but the behavior and parameters should be equivalent:

    BASE_REFRESH_LATEST_PATH = TMP_DIR / "scherzo-implementation-refresh-base-latest.json"
    BASE_DRIFT_REPAIR_SUMMARY_PATH = TMP_DIR / "scherzo-implementation-base-drift-repair.md"
    BASE_DRIFT_REPAIR_FAILURE_PATH = TMP_DIR / "scherzo-implementation-base-drift-failure.md"
    RETENTION_MARKER = ".scherzo-keep-workspace"

    def refresh_base_command(args: list[str]) -> None: ...
    def parse_refresh_base_args(args: list[str]) -> str: ...
    def refresh_base_json_path(stage: str) -> Path: ...
    def configured_publish_base() -> tuple[str, str]: ...
    def conflicted_files() -> list[str]: ...
    def refresh_base(stage: str) -> dict[str, Any]: ...
    def append_base_drift_repair_summary(body: str) -> tuple[str, bool]: ...

The JSON files are the stable interface between command steps, agent prompts, and tests. The latest refresh should always be copied or written to `tmp/scherzo-implementation-refresh-base-latest.json`, while each stage also gets its own file. The repair prompt should prefer the stage-specific `before-validation` file when present and fall back to the latest file only if the stage-specific file is missing. `tmp/scherzo-implementation-publish.json` should include `base_drift_repair_summary_included` on every successful publish, and should include `base_drift_repair_summary_path` when the summary exists.

The workflow files must continue using the existing repository-root discovery shell prefix:

    repo_root=${SCHERZO_REPO_ROOT:-$(pwd -P | sed "s#/.scherzo/workspaces/.*##")}; "$repo_root/scripts/scherzo-implementation" refresh-base --stage before-validation

The new prompt file path is `.scherzo/workflows/prompts/repair-base-drift.md`. The implementation workflow and ExecPlan implementation workflow should both point to that same prompt.

## Open Questions and Clarifications Needed

- [CLARIFY] The ticket references `LIV-59` and `LIV-65`, but the exact incident logs, branch states, and failure outputs were not included. This plan covers the two required classes, stale-base rebase conflict and clean-rebase validation failure, but the final implementation PR should update the incident examples if stakeholders provide exact details.
