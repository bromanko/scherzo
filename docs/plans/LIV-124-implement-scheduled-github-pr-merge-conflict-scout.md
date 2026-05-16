# Implement scheduled GitHub PR merge-conflict scout

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators should not have to notice that an open GitHub pull request is blocked by merge conflicts, manually create a Linear issue, and then wait for the existing merge-conflict workflow to pick it up. After this change, the running Scherzo daemon has one scheduled job that periodically scans open pull requests in `scherzo-systems/scherzo`, skips unsafe targets such as forks and drafts, detects likely merge conflicts, and creates or refreshes exactly one dispatchable Linear issue for each conflicted same-repository pull request.

The visible behavior is simple. A same-repository pull request such as `https://github.com/scherzo-systems/scherzo/pull/123` that needs conflict repair causes one Linear issue in `Todo` whose title or description names PR #123, whose body contains the stable marker `github-pr-conflict:scherzo-systems/scherzo#123`, and whose only workflow label is `workflow:merge-conflict-resolution`. The existing `.scherzo/workflows/merge-conflict-resolution.yaml` workflow remains the resolver. The new scheduled scout only discovers conflicted PRs and enqueues resolver-shaped Linear issues.

When no pull request needs conflict repair, the scheduled run exits successfully without creating or updating Linear issues and without printing noisy output. When the scout itself fails, Scherzo's native scheduled-job failure reporting creates or updates the normal scheduled-failure triage issue for the scout job, separate from any resolver issues.

## Problem Framing and Constraints

Scherzo already has an issue-driven merge-conflict resolver. The resolver is intentionally narrow: it reads one Linear issue, extracts one PR or branch target, prepares a local merge, lets an agent resolve the materialized conflicts, validates the result, and publishes the repair. That workflow should not be changed into a repository-wide discovery process, because it is safer and easier to reason about when each resolver run has exactly one target.

The missing operator capability is discovery and enqueueing. GitHub can have many open PRs, some may be forks, some may be drafts, and some may be temporarily blocked by checks rather than true merge conflicts. The scout must therefore be conservative. It only scans `scherzo-systems/scherzo`, only considers open non-draft pull requests whose head repository equals their base repository, and only creates Linear work when GitHub metadata or a local merge preflight gives enough evidence of a merge conflict. It must never push branches, modify PRs, invoke pi, or run `scripts/scherzo-merge-conflict` itself.

This plan refreshes the earlier stale plan for LIV-124. The native scheduler work that was previously uncertain is now present in the current tree, so this plan treats scheduled-job runtime as existing infrastructure. The implementation scope is the product-specific scout script, the command-only scheduled workflow, dogfood configuration, and tests that prove the scout is safe and idempotent.

## Strategy Overview

The smallest useful design is a scheduled command workflow that delegates GitHub and Linear integration to a Python script under `scripts/`. Scherzo's scheduler already knows how to run scheduled workflows, render scheduled template variables, record local scheduled-run history, skip overlapping runs, retry failures, and report terminal scheduled failures to Linear. Reusing that runtime avoids adding product-specific GitHub polling into the Gleam daemon.

The new script, `scripts/scherzo-github-pr-conflict-scout`, has one production subcommand named `scan` and one fixture-only test subcommand named `scan-fixture`. The production path uses GitHub's REST API to list open pull requests and fetch PR details, uses a temporary local Git repository for merge preflight when GitHub metadata is inconclusive, and uses Linear GraphQL only after at least one conflicted PR has been found. The fixture subcommand exercises the same decision code without network or real Git operations so the test suite remains deterministic and safe.

The new workflow `.scherzo/workflows/github-pr-conflict-scout.yaml` contains one command step. The checked-in `.scherzo/scherzo.yaml` adds a routing entry for the workflow and a top-level `scheduled_jobs` entry with `every: 15m`, `overlap: skip`, and `catch_up: false`. The workflow ID is not added to `linear_contract.workflow_labels`, because scheduled workflows are started by `scheduled_jobs`; Linear workflow labels remain reserved for issue-dispatched workflows.

## Alternatives Considered

The simplest apparent alternative is to run `.scherzo/workflows/merge-conflict-resolution.yaml` directly on a schedule. That is rejected because a scheduled run has no Linear issue describing one target PR. The resolver would either fail target extraction or need to grow repository-wide discovery behavior, which would make the safety boundary less clear.

Another alternative is to create a broad Gleam GitHub integration inside the daemon. That is rejected as too large for the problem. The daemon already has a generic scheduled workflow runner; the GitHub PR scout is repository-specific maintenance logic and fits better as a command step with offline tests.

A third alternative is to trust only GitHub's `mergeable` and `mergeable_state` fields. That is too weak because GitHub can temporarily report `mergeable: null`, and non-dirty states such as `blocked` can mean branch protection rather than conflicts. This plan uses GitHub metadata for clear cases and performs local merge preflight for inconclusive same-repository PRs.

A fourth alternative is to dedupe against any Linear issue that has the PR marker, regardless of state. That would be quiet but unsafe: a previous failed resolver issue in `Triage`, `Backlog`, `Done`, or `Canceled` could suppress new dispatchable work. This plan dedupes only against `Todo` and `In Progress`, the active states configured in `.scherzo/scherzo.yaml`, and treats non-dispatchable marker issues as historical evidence.

## Risks and Countermeasures

The first risk is accidentally creating duplicate Linear issues every 15 minutes. The scheduled job uses `overlap: skip`, the script searches for the stable marker before creating, and the script updates or no-ops an existing `Todo` or `In Progress` marker issue instead of creating another. Tests must prove that a repeated scan for PR #123 produces one create on the first run and either one update or a no-op on later runs.

The second risk is unsafe work on forks, deleted-head PRs, or cross-repository branches. The script must skip any PR whose base repo is not `scherzo-systems/scherzo`, whose head repo is missing, or whose head repo differs from the base repo. Fixture tests must cover draft PRs, fork PRs, cross-repository PRs, and `head.repo: null` deleted-head cases.

The third risk is false-positive conflict detection. The script may create expensive resolver work if it treats a merely blocked PR as conflicted. The script must treat `mergeable_state: dirty` as conflicted, must treat clearly mergeable states as clean when `mergeable` is true, and must run local preflight for unknown or non-dirty negative states. If preflight cannot run, the script must skip that PR with `preflight_unavailable` rather than create uncertain work.

The fourth risk is noisy or secret-bearing logs. The script must never print GitHub or Linear token values. In default production mode it should print nothing for no-op scans. `--json-summary` is for tests and manual diagnostics only. Error messages should identify the failing service and response shape, not credentials or raw authorization headers.

The fifth risk is breaking existing issue-dispatched workflows by adding a scheduled route or a new workflow label incorrectly. The dogfood config must add the route under `routing.workflows` but must not add `github-pr-conflict-scout` to `linear_contract.workflow_labels`. A runtime-bundle test must load the checked-in config and prove the scheduled workflow is accepted without `issue.*` template variables.

The sixth risk is a partially deployed scheduled job that starts failing every interval. Rollback is simple: set the `github-pr-conflict-scout` scheduled job to `enabled: false` or remove the `scheduled_jobs` entry from `.scherzo/scherzo.yaml`, reload or restart the daemon, and leave the script/workflow file in place until a later cleanup. The scheduled failure issue for the scout helps detect this condition early.

## Progress

- [x] (2026-05-09 00:00Z) Verified the current tree has native scheduled workflow execution, scheduled failure reporting, and `scherzoctl schedules` diagnostics.
- [x] (2026-05-09 00:00Z) Verified the current dogfood config has the merge-conflict resolver route but no scheduled scout job, scout workflow, or scout script.
- [x] (2026-05-09 00:00Z) Refreshed the plan to remove stale scheduler-runtime implementation scope and focus on the GitHub/Linear scout.
- [x] (2026-05-09 22:30Z) Added deterministic fixture tests for the scout CLI and decision behavior in `test/github_pr_conflict_scout_test.gleam`.
- [x] (2026-05-09 22:30Z) Implemented `scripts/scherzo-github-pr-conflict-scout` with production `scan` and fixture-only `scan-fixture` subcommands.
- [x] (2026-05-09 22:30Z) Added `.scherzo/workflows/github-pr-conflict-scout.yaml`.
- [x] (2026-05-09 22:30Z) Added the `github-pr-conflict-scout` route and `scheduled_jobs` entry to `.scherzo/scherzo.yaml` without adding a Linear workflow label for the scout.
- [x] (2026-05-09 22:30Z) Added runtime-bundle/config tests proving the checked-in scheduled workflow loads and has no issue context.
- [x] (2026-05-09 22:30Z) Ran full validation: tests, formatting, glinter, Scherzo custom lint, Python compile check, and a fixture smoke command.

## Surprises & Discoveries

- Observation: Native scheduled runtime is now present in the daemon rather than missing. The daemon evaluates scheduled jobs from the loaded runtime bundle during poll ticks, starts scheduled workflows asynchronously through `workflow_run.execute_scheduled`, records scheduled lifecycle ledger records, retries scheduled failures, and reports terminal scheduled failures.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` imports `scherzo/orchestrator/schedule_core`, defines scheduled pending/retry state, calls `evaluate_scheduled_jobs`, starts `spawn_scheduled_worker_for_pending`, and handles `ScheduledWorkerFinished`, `ScheduledRetryTick`, and `ScheduledReportRetryTick`.

- Observation: The current tests already prove the scheduler can run command-only scheduled workflows, skip overlap, recover active scheduled runs, and report terminal scheduled failures.
  Evidence: `test/orchestrator_daemon_test.gleam` contains `daemon_scheduled_due_tick_runs_command_workflow_test`, `daemon_scheduled_overlap_records_skip_without_second_start_test`, `daemon_scheduled_startup_recovers_active_run_with_retry_test`, `daemon_scheduled_failure_reports_after_retry_exhaustion_test`, and scheduled report retry tests.

- Observation: Scheduled workflow template context and command environment are already implemented.
  Evidence: `src/scherzo/workflow_run.gleam` exposes `execute_scheduled`, renders scheduled command templates with `template.render_scheduled`, sets `SCHERZO_RUN_KIND=scheduled`, `SCHERZO_SCHEDULED_JOB_ID`, `SCHERZO_SCHEDULE_DUE_AT`, `SCHERZO_SCHEDULE_STARTED_AT`, and `SCHERZO_RUN_ATTEMPT`, and uses scheduled workspace preparation.

- Observation: Scheduled workflows are rejected if they reference `issue.*` variables.
  Evidence: `src/scherzo/runtime_bundle.gleam` validates enabled scheduled jobs and returns `scheduled_workflow_requires_issue_context` if a scheduled workflow step references `issue` or `issue.*`.

- Observation: The checked-in dogfood config currently routes `merge-conflict-resolution` but has no scheduled jobs.
  Evidence: `.scherzo/scherzo.yaml` has `merge-conflict-resolution: workflows/merge-conflict-resolution.yaml` under `routing.workflows`, includes `merge-conflict-resolution` in `linear_contract.workflow_labels`, and contains no `scheduled_jobs` key.

- Observation: There is no current scout implementation.
  Evidence: repository searches found no `scripts/scherzo-github-pr-conflict-scout`, no `.scherzo/workflows/github-pr-conflict-scout.yaml`, and no `github-pr-conflict-scout` route.

- Observation: The checked-in config loader originally required explicit `linear_contract.workflow_labels` to match every `routing.workflows` key, which made a scheduled-only route fail to load unless it also became a Linear issue workflow label.
  Evidence: the first full `direnv exec . gleam test --target erlang` after adding the scout route failed with `invalid_config` and the message `linear_contract.workflow_labels must match routing.workflows when routing requires exactly one workflow label`.

- Observation: The source guardrail prevents growing the already-large `src/scherzo/config.gleam` module, including both line count and internal-import count growth.
  Evidence: after a first implementation of the scheduled-only route exemption in `src/scherzo/config.gleam`, `source_guardrail_test.source_guardrail_matches_checked_in_baseline_test` failed first on line growth and then on internal-import growth. The final implementation keeps the config module below its line baseline and avoids increasing its internal imports by placing pure workflow-label resolution helpers in `src/scherzo/config/types.gleam`.

## Decision Log

- Decision: Treat native scheduled workflow runtime as an existing prerequisite, not as part of this implementation.
  Rationale: The current tree already has scheduler execution, scheduled ledger records, overlap handling, retries, failure reporting, and operator diagnostics. Reimplementing that work would broaden the change unnecessarily and repeat LIV-127.
  Date: 2026-05-09

- Decision: Implement the PR scout as a Python script invoked by a one-step scheduled command workflow.
  Rationale: The existing resolver already uses a Python helper under `scripts/`, and the scout needs HTTP requests, JSON validation, subprocess-based Git preflight, and fixture-driven tests. Keeping this logic outside the daemon keeps the product-specific scan easy to test and easy to roll back.
  Date: 2026-05-09

- Decision: Create resolver issues in `Todo` and dedupe against `Todo` and `In Progress` only.
  Rationale: `.scherzo/scherzo.yaml` configures `dispatch_states: [Todo]` and `active_states: [Todo, In Progress]`. Issues in other states are not guaranteed to dispatch and must not suppress new actionable repair work.
  Date: 2026-05-09

- Decision: Do not add `github-pr-conflict-scout` to `linear_contract.workflow_labels`.
  Rationale: The scout is not issue-dispatched. Adding a Linear workflow label for it would let users accidentally create scout issues and would violate the separation between scheduled discovery and issue-based resolution.
  Date: 2026-05-09

- Decision: Use the body marker `github-pr-conflict:<owner>/<repo>#<number>` for resolver issue dedupe.
  Rationale: The marker is stable across title edits, independent of Linear identifiers, easy to search in descriptions, and readable to operators.
  Date: 2026-05-09

- Decision: Treat scheduled-only workflow routes as outside the required Linear issue workflow label set unless an operator explicitly includes them in `linear_contract.workflow_labels`.
  Rationale: Scheduled jobs still need a workflow route so the daemon can load their DAG, but they are not dispatched from Linear issues. This preserves the plan's safety boundary while keeping the existing exact-label contract for issue-dispatched workflows.
  Date: 2026-05-09

- Decision: Put the pure linear-contract route/label normalization helper in `src/scherzo/config/types.gleam` instead of growing `src/scherzo/config.gleam`.
  Rationale: The source guardrail already baselines `src/scherzo/config.gleam` as a large module. Keeping the new helper in the smaller config types module avoids increasing the large module's line or internal-import baseline while keeping the behavior close to the config data types it normalizes.
  Date: 2026-05-09

## Outcomes & Retrospective

The scheduled GitHub PR merge-conflict scout is implemented. The repository now has a deterministic fixture-driven scout test suite, an executable Python scout script with production GitHub, local preflight, and Linear GraphQL paths, a one-step scheduled command workflow, dogfood scheduling configuration, and runtime-bundle tests proving that the scheduled route loads without a `workflow:github-pr-conflict-scout` Linear label.

The main implementation gap discovered during execution was that the existing Linear contract resolver treated all workflow routes as issue-dispatched routes. The final code updates that config behavior so scheduled-only routes can coexist with strict Linear issue workflow labels. Full validation passed with the existing warning inventory unchanged in kind for the touched production code, and the fixture smoke command demonstrated the expected resolver issue summary for PR #123.

## Context and Orientation

Scherzo is a Gleam daemon that polls Linear for issues and runs workflow DAGs declared in YAML. In this repository, the main configuration file is `.scherzo/scherzo.yaml`, and workflow files live under `.scherzo/workflows/`. A workflow label is a Linear issue label whose name begins with `workflow:`. Issue-dispatched workflows require exactly one such label so the daemon can choose the workflow.

The current merge-conflict resolver is `.scherzo/workflows/merge-conflict-resolution.yaml`. It is an issue-dispatched workflow with four steps: `prepare_target`, `resolve_conflicts`, `validate_resolution`, and `publish_resolution`. Its command steps call `scripts/scherzo-merge-conflict prepare`, `scripts/scherzo-merge-conflict validate`, and `scripts/scherzo-merge-conflict publish`. The resolver script extracts one PR or branch target from the Linear issue title, description, or comments. The new scout must create Linear issues whose title or description contains the PR URL or PR number so this existing target extraction continues to work unchanged.

Scheduled jobs are configured by a top-level `scheduled_jobs` list in `scherzo.yaml`. Each scheduled job points at a normal workflow by ID and has interval and failure-reporting policy. The MVP scheduler supports fixed intervals such as `15m`, `overlap: skip`, and `catch_up: false`. It intentionally does not support schedule-level `input`, `vars`, or payload blobs; job-specific behavior belongs in the workflow, prompt, script, environment, or repository config.

The scheduler is daemon-local and poll-driven. When the daemon receives a valid poll tick, it reloads workflow configuration if needed, evaluates scheduled jobs, records due and pending records, starts any due scheduled workflow using the normal workflow machinery, and then performs the Linear poll. Successful scheduled runs remain local. Terminal scheduled failures can create or update a Linear triage issue through `src/scherzo/scheduled_failure_reporter.gleam`.

## Preconditions and Verified Facts

The repository uses Gleam on the Erlang target and has a direnv/devenv environment. Commands should be run from the repository root with `direnv exec .` unless direnv is unavailable. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command.

The file `.scherzo/scherzo.yaml` exists. It configures Linear with `project_slug: scherzo-f6f4bc92d6d7`, `active_states: [Todo, In Progress]`, `dispatch_states: [Todo]`, and `terminal_states: [Canceled, Duplicate, Done]`. It routes `merge-conflict-resolution` to `workflows/merge-conflict-resolution.yaml` and `github-pr-conflict-scout` to `workflows/github-pr-conflict-scout.yaml`. It requires exactly one issue workflow label and lists `merge-conflict-resolution`, but not `github-pr-conflict-scout`, in `linear_contract.workflow_labels`. It now contains an enabled `scheduled_jobs` entry for `github-pr-conflict-scout` with `every: 15m`, `overlap: skip`, `catch_up: false`, and Linear scheduled-failure reporting to `Triage`.

The file `.scherzo/workflows/merge-conflict-resolution.yaml` exists and uses `workspace_profile: dogfood-jj`. Its command steps use `SCHERZO_CONFIG_DIR` to infer the repository root and invoke `scripts/scherzo-merge-conflict`. This is the workflow that scout-created Linear issues must trigger.

The file `scripts/scherzo-merge-conflict` exists and is an executable Python 3 script. It already knows how to extract PR targets from Linear issue text and how to prepare, validate, and publish a conflict repair. This plan did not change that script. The file `scripts/scherzo-github-pr-conflict-scout` now also exists and is an executable Python 3 script with `scan` and `scan-fixture` subcommands.

The native scheduler files already exist. `src/scherzo/orchestrator/schedule_core.gleam` defines fixed-interval due-time math and run IDs such as `schedule-scheduled-job-19700101T000001Z`. `src/scherzo/orchestrator/daemon.gleam` evaluates scheduled jobs and starts scheduled workers. `src/scherzo/workflow_run.gleam` implements scheduled workflow execution, scheduled template rendering, and scheduled command environment variables. `src/scherzo/runtime_bundle.gleam` rejects scheduled workflows that reference `issue.*`.

The local operator commands already exist. `src/scherzo/ctl.gleam` supports `schedules status`, `schedules history`, `schedules logs <job> --last`, `schedules doctor <job>`, and `schedules run <job> --now`.

## Scope Boundaries

In scope: add the scout script, fixture-based script tests, a one-step command workflow, the dogfood scheduled job configuration, and config/runtime tests that prove the checked-in workflow loads through Scherzo's real runtime bundle.

In scope: create Linear issues for conflicted same-repository PRs in `Todo` with exactly one workflow label, `workflow:merge-conflict-resolution`, and a description containing the PR URL and dedupe marker.

In scope: update or no-op existing dispatchable marker issues in `Todo` or `In Progress`; ignore marker issues in non-dispatchable states for dedupe, while recording that they were ignored in `--json-summary`.

Out of scope: do not modify `scripts/scherzo-merge-conflict`, do not modify `.scherzo/workflows/merge-conflict-resolution.yaml`, do not add a GitHub webhook, do not add a GitHub App, do not add a daemon-native GitHub client, do not run pi from the scout, and do not change scheduler runtime behavior.

Out of scope: do not scan forks, draft PRs, closed PRs, cross-repository PRs, other repositories, arbitrary branches, or PRs whose head repository has been deleted.

Out of scope: do not add schedule-level `input`, `vars`, `variables`, or payload fields. The current config parser rejects those fields intentionally.

Out of scope: do not add `workflow:github-pr-conflict-scout` as a Linear workflow label. The scheduled job should be started only by `scheduled_jobs`.

## Milestones

Milestone 1 adds deterministic tests for the scout before production network code is trusted. At the end, `gleam test` exercises a fixture-backed `scan-fixture` command and proves no-op scans, conflict creation, dedupe updates, historical-state ignores, unsafe PR skips, preflight behavior, and malformed payload failures without touching GitHub or Linear. This milestone comes first because it fixes the safety contract before any real API mutation exists.

Milestone 2 implements the production scout command. At the end, `scripts/scherzo-github-pr-conflict-scout scan` can list GitHub pull requests, classify conflicts, run local preflight when needed, and create or update Linear issues. The production code reuses the same decision functions tested by `scan-fixture`.

Milestone 3 wires the scheduled workflow and dogfood configuration. At the end, `.scherzo/workflows/github-pr-conflict-scout.yaml` exists, `.scherzo/scherzo.yaml` routes it and schedules it every 15 minutes, and the checked-in runtime bundle loads successfully. The route exists for scheduled dispatch, but there is no corresponding Linear workflow label.

Milestone 4 validates the feature and rollback path. At the end, all tests and lint gates pass, `scherzoctl schedules doctor github-pr-conflict-scout` can diagnose the job after a daemon reload, and an operator can disable the job by setting `enabled: false` if the scout misbehaves.

## Plan of Work

Add `test/github_pr_conflict_scout_test.gleam`. Use the existing `command_step.run` pattern from `test/merge_conflict_helper_test.gleam` rather than adding a new Python test framework. Each test should write a JSON fixture under `test/tmp/github-pr-conflict-scout-*`, run `scripts/scherzo-github-pr-conflict-scout scan-fixture <fixture> --json-summary`, and assert on stdout, stderr, exit code, and important JSON substrings. The tests will fail at first because the script does not exist.

Implement `scripts/scherzo-github-pr-conflict-scout` as an executable Python 3 script using only the standard library. Use `argparse`, `dataclasses`, `json`, `os`, `re`, `subprocess`, `sys`, `tempfile`, `time`, `urllib.error`, `urllib.parse`, `urllib.request`, and typing helpers. Keep network clients thin and keep decision logic in functions that the fixture subcommand can call.

The script must expose these CLI shapes:

    scripts/scherzo-github-pr-conflict-scout scan \
      --repo scherzo-systems/scherzo \
      --linear-project-slug scherzo-f6f4bc92d6d7 \
      --create-state Todo \
      --workflow-label workflow:merge-conflict-resolution

    scripts/scherzo-github-pr-conflict-scout scan-fixture <fixture.json> --json-summary

`scan` supports optional `--json-summary` for manual diagnostics and tests. It also supports `--skip-local-preflight` for tests and emergency manual inspection only; the checked-in scheduled workflow must not pass that flag. Default production mode prints nothing when there are no conflicted PRs and no mutations.

Model the core data explicitly. Define `SafePullRequest` with repository, number, URL, base branch, head branch, base SHA if available, and head SHA if available. Define a conflict observation with statuses `conflicted`, `clean`, `skipped`, and `malformed`. Define Linear issue summaries with id, identifier, title, description, URL, state name, state type, label names, and created/updated timestamps. Define a summary dictionary containing `created`, `updated`, `noop`, `skipped_prs`, `conflicted_prs`, `warnings`, and `historical_marker_issues_ignored`.

Implement PR filtering in the script. `list_open_pulls` must require the GitHub list response to be a JSON list. For each PR, require integer `number`, string `html_url`, object `base`, object `head`, object `base.repo`, string `base.repo.full_name`, string `base.ref`, object `head.repo`, string `head.repo.full_name`, string `head.ref`, and boolean-compatible `draft`. If `head.repo` is null, skip with reason `deleted_head_repo`. If a required non-null field is missing or has the wrong type, treat the response as malformed and exit nonzero. Skip drafts, PRs whose base repo does not match `--repo`, and PRs whose head repo differs from the base repo.

Implement conflict classification in the script. For each safe PR, fetch PR detail from `/repos/<owner>/<repo>/pulls/<number>` because GitHub may only populate `mergeable` on the detail endpoint. If `mergeable` is null, retry the detail request up to three times with a one-second delay. If `mergeable_state` is `dirty`, classify the PR as conflicted. If `mergeable` is true, classify it as clean even when checks are blocked or unstable. If metadata is null, unknown, false-but-not-dirty, or otherwise inconclusive, run local merge preflight unless `--skip-local-preflight` was provided. If preflight returns conflicted, create Linear work. If preflight returns clean, do not create work. If preflight returns unavailable, skip with reason `preflight_unavailable` and exit zero.

Implement local merge preflight without modifying the working tree. Create a temporary directory, initialize a Git repository there, determine the fetch URL by first trying `git remote get-url origin` and then falling back to parsing `jj git remote list --color=never` like `scripts/scherzo-merge-conflict`, fetch the base branch and head branch by ref, check out the fetched base, and run `git merge --no-commit --no-ff` against the fetched head. Return conflicted only if the merge exits nonzero and `git diff --name-only --diff-filter=U` reports at least one path. Return clean if the merge exits zero. Return unavailable for missing commands, missing origin URL, fetch failures, checkout failures, or merge failures without unmerged paths. Delete the temporary directory before returning.

Implement Linear mutation planning. Do not read `LINEAR_API_KEY` or contact Linear until at least one PR has been classified as conflicted. Read `LINEAR_API_KEY`, falling back to `SCHERZO_AGENT_LINEAR_API_KEY` if present, and fail with `Linear API key is required to create merge-conflict issues` if neither is set. Fetch the Linear project by slug using the same contract-style query pattern as `src/scherzo/linear.gleam`: project id, teams, team states, team labels, and workspace labels. Use the first project team. Find the state named by `--create-state`, which is `Todo` in the scheduled workflow. Find an existing label named exactly `workflow:merge-conflict-resolution`; do not create this workflow label automatically.

Search candidate Linear issues in the project with the workflow label and state type not in `completed`, `canceled`, or `duplicate`, paginating until `pageInfo.hasNextPage` is false. Filter client-side for descriptions or titles containing the marker `github-pr-conflict:scherzo-systems/scherzo#<number>`. A dispatchable dedupe match has state name `Todo` or `In Progress` and the workflow label. If any dispatchable matches exist, choose an `In Progress` match first when present, otherwise choose the oldest `Todo` match by `createdAt`. Update its description only if the generated description differs; otherwise record a no-op. If only non-dispatchable marker issues exist, add their identifiers to `historical_marker_issues_ignored` and create a new `Todo` issue.

Generated resolver issue titles must be `Resolve merge conflicts for PR #<number>`. Generated descriptions must include the marker on its own line, the GitHub PR URL, repository, base branch, head branch, base SHA when known, head SHA when known, detection source (`github:dirty` or `local-preflight`), observation time in UTC ISO-8601, and a sentence saying that Scherzo's `workflow:merge-conflict-resolution` resolver should handle the repair. Created issues must have `labelIds` containing exactly the id for `workflow:merge-conflict-resolution`.

Add `.scherzo/workflows/github-pr-conflict-scout.yaml`. Use workflow id `github-pr-conflict-scout`, `workspace_profile: dogfood-jj`, `max_parallel_steps: 1`, and one command step named `scan_open_prs`. The command should infer `repo_root` from `SCHERZO_REPO_ROOT` or `SCHERZO_CONFIG_DIR` and invoke the script with the production `scan` arguments. Use `workspace: main` and `timeout_ms: 300000`.

Update `.scherzo/scherzo.yaml`. Under `routing.workflows`, add `github-pr-conflict-scout: workflows/github-pr-conflict-scout.yaml`. Do not add `github-pr-conflict-scout` to `linear_contract.workflow_labels`. Add a top-level scheduled job after the routing block and before `artifact_limits`:

    scheduled_jobs:
      - id: github-pr-conflict-scout
        workflow: github-pr-conflict-scout
        enabled: true
        every: 15m
        overlap: skip
        catch_up: false
        on_failure:
          linear:
            enabled: true
            state: Triage
            labels:
              - job:github-pr-conflict-scout
            dedupe: open_issue_per_job

Add `test/github_pr_conflict_scout_config_test.gleam`. Load `.scherzo/scherzo.yaml` through `runtime_bundle.load(Some(".scherzo/scherzo.yaml"))` and assert that the `github-pr-conflict-scout` route exists, the workflow DAG exists, the scheduled job exists, `job.enabled` is true, `job.every_ms` is `900000`, `job.catch_up` is false, the workflow has exactly one command step, the command mentions `scripts/scherzo-github-pr-conflict-scout` and does not mention `--skip-local-preflight`, and `linear_contract.workflow_labels` does not contain `github-pr-conflict-scout`.

## Concrete Steps

1. From the repository root, confirm the current tree state:

       jj status --color=never

   Expected output before implementation is a clean working copy or only this plan file if the plan has already been committed.

2. Create `test/github_pr_conflict_scout_test.gleam`. Copy the helper style from `test/merge_conflict_helper_test.gleam`: define `limits`, `reset_dir`, `run_scout`, and `run_scout_in` helpers that use `command_step.run` with a 10 second timeout.

3. Add a test named `scout_noop_open_prs_succeeds_without_linear_test`. It writes a fixture with an empty `github.pulls` list and a `linear.fail_if_called: true` marker, runs `scan-fixture <fixture> --json-summary`, and asserts exit code 0, stdout contains `"created": []`, stdout contains `"updated": []`, and stderr is empty.

4. Add a test named `scout_default_noop_is_silent_test`. It runs the same fixture without `--json-summary` and asserts exit code 0, empty stdout, and empty stderr.

5. Add a test named `scout_conflicted_same_repo_pr_creates_resolver_issue_test`. The fixture has one open non-draft PR #123 with `base.repo.full_name` and `head.repo.full_name` both `scherzo-systems/scherzo`; PR detail has `mergeable_state: dirty`; Linear has the project, `Todo` state, and workflow label id but no existing marker issue. Assert stdout contains one create action, `Resolve merge conflicts for PR #123`, `https://github.com/scherzo-systems/scherzo/pull/123`, `github-pr-conflict:scherzo-systems/scherzo#123`, `workflow-label-id`, and does not contain any support label id.

6. Add a test named `scout_existing_dispatchable_marker_updates_or_noops_test`. The fixture has the same conflicted PR and an existing `Todo` issue with the workflow label and the marker in its description. Use a stale description so the expected action is update. Assert there is one update for that issue id and no create action.

7. Add a test named `scout_identical_existing_marker_is_noop_test`. The fixture has the same conflicted PR and an existing `Todo` issue whose generated description already matches the current observation. Assert the summary records a no-op and no create or update action.

8. Add a test named `scout_ignores_triage_marker_and_creates_dispatchable_issue_test`. The fixture has a marker issue in state `Triage` and no `Todo` or `In Progress` marker issue. Assert stdout contains `historical_marker_issues_ignored`, includes the Triage issue identifier, and contains one create action in `Todo`.

9. Add a test named `scout_skips_unsafe_prs_test`. The fixture includes a draft PR, a fork PR, a cross-repository PR, and a PR with `head.repo: null`. Assert all are skipped with reasons `draft`, `cross_repository`, or `deleted_head_repo`, and no Linear mutation is planned.

10. Add a test named `scout_inconclusive_metadata_uses_preflight_test`. The fixture has a safe PR with `mergeable: false` and `mergeable_state: blocked`; fixture preflight for that PR returns `conflicted`. Assert one create action and that the generated description contains `local-preflight`.

11. Add a test named `scout_preflight_unavailable_skips_without_linear_test`. The fixture has inconclusive metadata and preflight result `unavailable`. Assert no create action, no update action, and skipped reason `preflight_unavailable`.

12. Add a test named `scout_malformed_github_payload_fails_test`. The fixture has a PR missing `number` or with `number` as a string. Assert exit code 1 and stderr contains `malformed GitHub PR payload`.

13. Run the test suite and observe the new tests fail because the script does not exist:

       direnv exec . gleam test --target erlang

   Expected failure before implementation is a command failure from `scripts/scherzo-github-pr-conflict-scout` not being found.

14. Create `scripts/scherzo-github-pr-conflict-scout` with a shebang `#!/usr/bin/env python3`, the production and fixture CLI parsers, the data validation helpers, and fixture-backed scan logic. At this point, implement enough fixture logic to pass the tests without real network calls.

15. Mark the script executable:

       chmod +x scripts/scherzo-github-pr-conflict-scout

16. Run `direnv exec . gleam test --target erlang` and make the fixture tests pass. Do not add production network mutations until the deterministic fixture path is green.

17. Add production GitHub client code to `scripts/scherzo-github-pr-conflict-scout`. Implement paginated `GET /repos/<repo>/pulls?state=open&per_page=100`, PR detail retries, optional `GITHUB_TOKEN` or `GH_TOKEN` authorization, and sanitized API error messages.

18. Add production local merge preflight code to the script. Ensure it uses a temporary Git repository and deletes it even when a fetch or merge fails.

19. Add production Linear GraphQL client code to the script. Implement project/team/state/label lookup, candidate issue pagination, issue creation, and issue description update. Do not create workflow labels automatically.

20. Add `.scherzo/workflows/github-pr-conflict-scout.yaml` with the one command step described in the Plan of Work.

21. Edit `.scherzo/scherzo.yaml` to add the route and scheduled job. Preserve all existing tracker, workspace, agent, handoff, routing, artifact, contract, and linear command settings.

22. Create `test/github_pr_conflict_scout_config_test.gleam` and add the runtime-bundle loading assertions described in the Plan of Work.

23. Run the full Gleam test suite:

       direnv exec . gleam test --target erlang

   Expected output is all tests passing.

24. Run formatting check:

       direnv exec . gleam format --check src test

   Expected output is no formatting changes required.

25. Run production lint gates:

       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expected output is no new production lint errors. Do not add `let assert`, `panic`, or `todo` in `src/`.

26. Run a fixture smoke command manually:

       direnv exec . scripts/scherzo-github-pr-conflict-scout scan-fixture test/tmp/github-pr-conflict-scout-create/fixture.json --json-summary

   Expected output is the JSON summary used by the create test. If the test writes a different fixture path, use that path.

27. Commit after tests and lint pass. Suggested commit message: `Add scheduled GitHub PR conflict scout`.

## Testing and Falsifiability

The scout is falsified if an empty PR list contacts Linear, if default no-op mode prints output, if a fork or draft PR can create Linear work, if a `Triage` marker suppresses a new `Todo` issue, if repeated scans create duplicate `Todo` issues, or if a scheduled workflow can reference `issue.*` and still load. The tests above explicitly cover those failure modes.

`test/github_pr_conflict_scout_test.gleam` must exercise the script through its CLI so executable permissions, argument parsing, fixture parsing, stdout/stderr behavior, and exit codes are covered. It must not make network calls and must not require `LINEAR_API_KEY`, `GITHUB_TOKEN`, or `GH_TOKEN`.

`test/github_pr_conflict_scout_config_test.gleam` must load the checked-in `.scherzo/scherzo.yaml` through `runtime_bundle.load`, not by substring checks alone. This proves the same path used by the daemon accepts the scheduled workflow and rejects issue-context mistakes.

Existing scheduler tests in `test/orchestrator_daemon_test.gleam` are the regression guard for native scheduled runtime. Do not duplicate them unless implementation changes scheduler behavior, which this plan should not do.

Run these commands from the repository root for final validation:

    direnv exec . gleam test --target erlang
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If any test fails because a fixture expected string changed, inspect whether the behavior changed or only the JSON formatting changed. Prefer stable keys and string assertions over exact whole-JSON comparisons.

## Validation and Acceptance

The implementation is accepted when the full validation commands pass and these behaviors are observable:

A fixture with no PRs exits zero, prints nothing by default, and does not contact the fixture Linear client.

A fixture with one safe PR #123 whose detail has `mergeable_state: dirty` plans one Linear create in `Todo`, with title `Resolve merge conflicts for PR #123`, description containing `https://github.com/scherzo-systems/scherzo/pull/123` and `github-pr-conflict:scherzo-systems/scherzo#123`, and label ids exactly `[workflow-label-id]` in the fixture.

A fixture with an existing `Todo` marker issue plans an update or no-op for that issue and no create.

A fixture with only a `Triage` marker issue plans a new `Todo` issue and reports the historical marker as ignored.

The checked-in `.scherzo/scherzo.yaml` loads through `runtime_bundle.load(Some(".scherzo/scherzo.yaml"))`, contains an enabled scheduled job `github-pr-conflict-scout` with a 15 minute interval, and does not contain `github-pr-conflict-scout` in `linear_contract.workflow_labels`.

After the daemon has been reloaded or restarted with the new config, an operator can run:

    scripts/scherzoctl schedules doctor github-pr-conflict-scout
    scripts/scherzoctl schedules status github-pr-conflict-scout

The doctor command should report that the job is configured and its workflow loads. The status command may show no local history until the first due interval or manual run.

## Rollout, Recovery, and Idempotence

Rollout is additive. The existing merge-conflict resolver remains unchanged, and existing issue workflows keep their current labels and routing. The new scheduled job is the only new autonomous behavior.

The scheduled job is idempotent by marker. Running it repeatedly for the same conflicted PR must produce at most one dispatchable Linear issue. If the generated body has not changed, the script should no-op rather than update Linear every interval. If the body has changed, updating the existing dispatchable issue is acceptable.

If the scout misbehaves, set the scheduled job to `enabled: false` in `.scherzo/scherzo.yaml`, reload or restart the daemon, and leave existing resolver issues alone. Resolver issues already created by the scout can be canceled or marked duplicate manually if they were false positives.

If Linear mutation fails after a conflict is found, the scheduled command should fail. The scheduler will retry according to the existing scheduled retry policy and then create or update the scheduled failure triage issue for `github-pr-conflict-scout`. This failure issue is not a resolver issue and should not carry `workflow:merge-conflict-resolution`.

If GitHub metadata is inconclusive and local preflight is unavailable, the script skips that PR and exits zero. This is a safe false negative. The JSON summary exposes the skip reason for manual diagnostics.

The script must clean up temporary Git directories even when a subprocess fails. It must not modify the current jj workspace, current Git repository, or any PR branch.

## Artifacts and Notes

The old unmerged ExecPlan PR for this work was stale because it included scheduler runtime as implementation scope. This refreshed plan intentionally relies on the scheduler runtime now present in the repository and narrows the remaining work to the scout.

The scheduled job should use this workflow command shape:

    repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}
    "$repo_root/scripts/scherzo-github-pr-conflict-scout" scan \
      --repo scherzo-systems/scherzo \
      --linear-project-slug scherzo-f6f4bc92d6d7 \
      --create-state Todo \
      --workflow-label workflow:merge-conflict-resolution

A generated resolver issue description should have this shape, with actual SHAs and timestamps filled in when available:

    github-pr-conflict:scherzo-systems/scherzo#123

    GitHub PR: https://github.com/scherzo-systems/scherzo/pull/123
    Repository: scherzo-systems/scherzo
    Base branch: main
    Head branch: feature/conflicted-change
    Base SHA: <sha or ->
    Head SHA: <sha or ->
    Detection: github:dirty
    Observed at: 2026-05-09T20:00:00Z

    Scherzo's workflow:merge-conflict-resolution resolver should repair this same-repository PR.

## Interfaces and Dependencies

No new package dependencies are required. The script must use Python 3 and the standard library only. Production validation remains Gleam-based through `gleam test`, `gleam format`, `glinter`, and `scherzo_lint`.

`scripts/scherzo-github-pr-conflict-scout` must expose:

    main(argv: list[str]) -> int
    marker(repo: str, number: int) -> str
    scan_with_clients(options, github_client, linear_client_factory, preflight) -> dict
    local_merge_preflight(repo: str, base_ref: str, head_ref: str) -> dict

The exact Python type annotations can use dataclasses or dictionaries, but the returned summary dictionary must contain stable keys `created`, `updated`, `noop`, `skipped_prs`, `conflicted_prs`, `warnings`, and `historical_marker_issues_ignored` because tests assert on those names.

The production GitHub client depends on `https://api.github.com`. It may use `GITHUB_TOKEN` or `GH_TOKEN` as an optional bearer token. It must work without a token until GitHub rate-limits the process.

The production Linear client depends on `https://api.linear.app/graphql`, `LINEAR_API_KEY` or `SCHERZO_AGENT_LINEAR_API_KEY`, and project slug `scherzo-f6f4bc92d6d7`. It must require an existing workflow label named `workflow:merge-conflict-resolution`, create issues in state `Todo`, and mutate issue descriptions through GraphQL `issueCreate` and `issueUpdate`.

The scheduled workflow depends on the current dogfood jj workspace profile named `dogfood-jj` in `.scherzo/scherzo.yaml`. The command may run inside a Scherzo jj workspace, so the script's local preflight must use a separate temporary Git repository rather than attempting merges in the current working directory.
