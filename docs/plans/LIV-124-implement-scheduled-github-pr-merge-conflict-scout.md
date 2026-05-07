# Implement scheduled GitHub PR merge-conflict scout

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators should not have to notice GitHub pull requests that are blocked by merge conflicts and manually create Linear work for each one. After this change, Scherzo has a native scheduled workflow runtime and a scheduled, command-only scout workflow. The runtime periodically starts configured workflows, and the scout scans open pull requests for the `bromanko/scherzo` GitHub repository, ignores unsafe targets, and creates or updates exactly one dispatchable Linear issue per conflicted same-repository pull request. The existing `workflow:merge-conflict-resolution` workflow remains the resolver; the new scout only enqueues resolver-shaped Linear issues that name the target PR.

The visible result is that a conflicted same-repository PR such as `https://github.com/bromanko/scherzo/pull/123` causes one active Linear issue whose body contains that URL and the stable marker `github-pr-conflict:bromanko/scherzo#123`, and that issue has the single workflow label `workflow:merge-conflict-resolution`. When no open same-repository non-draft PR appears conflicted, the scheduled command exits successfully without creating or updating any Linear issue. A local scheduler smoke test proves that a `scheduled_jobs` entry really starts a command-only workflow instead of merely parsing configuration.

## Problem Framing and Constraints

Today Scherzo can resolve merge conflicts through `.scherzo/workflows/merge-conflict-resolution.yaml` and `scripts/scherzo-merge-conflict`, but that workflow is Linear-issue-shaped: it expects a Linear issue that names one PR or branch, and its helper script reads `{{ issue.* }}`-derived context rather than discovering all PRs by itself. Running that resolver directly on a schedule would be unsafe because a scheduled run has no single target issue and could not know which branch to repair.

This plan therefore adds a scout, not a resolver. The scout is conservative. It only considers open, same-repository, non-draft GitHub pull requests for `bromanko/scherzo`; it skips forks and cross-repository PRs; it treats malformed GitHub data as an error rather than guessing; and it only opens Linear work for PRs that GitHub metadata or a local merge preflight identifies as likely conflicted. It does not push branches, modify PRs, run pi, or call `scripts/scherzo-merge-conflict`.

Source inspection for this plan found that the repository already parses `scheduled_jobs`, validates that scheduled workflows do not reference `issue.*`, defines pure schedule cadence logic, and persists scheduled-run records. The daemon runtime did not yet import or use `src/scherzo/orchestrator/schedule_core.gleam`, so scheduled jobs could be loaded without actually firing. LIV-124 therefore includes completing the native daemon-side scheduled workflow runtime before enabling the production scout. If a future implementer finds that equivalent runtime has already landed, they must keep the scheduler tests in this plan, record the discovery in `## Surprises & Discoveries`, and treat the runtime milestone as a verification milestone rather than reimplementing it.

## Strategy Overview

The first increment completes the smallest native scheduler that can run already-configured, command-only scheduled workflows. It reuses existing config and validation in `src/scherzo/config.gleam`, `src/scherzo/config/types.gleam`, and `src/scherzo/runtime_bundle.gleam`; existing cadence helpers in `src/scherzo/orchestrator/schedule_core.gleam`; existing scheduled state records in `src/scherzo/state/record.gleam`; and existing projection support in `src/scherzo/state/projection.gleam`. The daemon should use its existing timer dependency shape, worker registry, workflow runner, ledger writer, logger, and recovery patterns instead of introducing a second background-process framework. A scheduled workflow run must not block the daemon actor; it must start through the same asynchronous workflow execution path that normal issue dispatch uses, with a synthetic issue object only for internal identifiers and log context. Scheduled workflow validation already forbids `issue.*` references, so the synthetic issue must not carry business data.

The second increment adds one small Python command script, one workflow YAML file, one routing entry, and one scheduled job entry. Python is the right size because the existing merge-conflict helper is already a Python script at `scripts/scherzo-merge-conflict`, and the scout needs HTTP calls, JSON validation, subprocess calls to Git, and deterministic offline unit tests. Keeping the GitHub/Linear integration in a command script avoids adding product-specific PR scanning into the long-running Gleam daemon.

The script, named `scripts/scherzo-github-pr-conflict-scout`, exposes a production `scan` command and test-friendly pure functions. In production it lists GitHub PRs, filters unsafe PRs, classifies conflicts, and then calls Linear only if at least one conflicted PR is found. It dedupes only against Linear issues in states Scherzo actually dispatches from the checked-in tracker config: `Todo` and `In Progress`. If a matching marker issue exists only in `Triage` or another non-dispatchable state, the scout treats that issue as historical failure evidence and creates a new dispatchable issue rather than silently updating work the daemon will not poll.

The new workflow `.scherzo/workflows/github-pr-conflict-scout.yaml` contains a single command step that runs the script from the repository root. `.scherzo/scherzo.yaml` routes `github-pr-conflict-scout` to that workflow and schedules it with `overlap: skip` and `catch_up: false`, because repeated scouts should not queue up after downtime and overlapping scouts could race to create duplicate Linear issues.

## Alternatives Considered

The simplest alternative is to run `workflow:merge-conflict-resolution` directly on a schedule. That is rejected because the resolver is intentionally shaped around one Linear issue and one target PR or branch. Without a Linear issue body containing one target, the resolver's target extraction would either fail or need to grow a discovery feature that belongs outside the resolver.

Another alternative is to make completed native scheduled execution a hard external precondition and land only the scout script/config in LIV-124. That would keep this issue smaller, but current source inspection found no daemon-side use of the schedule cadence logic. Landing an enabled `scheduled_jobs` entry without runtime support would fail the operator-visible promise that the scout runs periodically, so this plan includes the missing runtime work and requires a scheduler smoke test as an acceptance gate.

A third alternative is to rely only on GitHub's `mergeable` and `mergeable_state` fields. That is attractive because it avoids local Git work, but GitHub can briefly return `mergeable: null`, and metadata may be inconclusive. This plan still uses GitHub metadata as the first hint, but it makes local merge preflight mandatory for safe same-repository PRs whose metadata is not clearly clean or dirty.

A fourth alternative is to implement the scout itself in Gleam inside the daemon. That would make scheduling and Linear/GitHub calls more uniform, but it increases the blast radius by adding product-specific PR scanning into the long-running orchestrator. A command script is easier to test offline, easier to roll back by removing the scheduled entry, and consistent with the existing `scripts/scherzo-merge-conflict` helper.

## Risks and Countermeasures

The first risk is that scheduled configuration loads but never fires. The plan counters this by putting daemon scheduler runtime before scout wiring, adding in-process daemon tests that force a scheduled tick with fake timers, adding checked-in runtime-bundle load tests for the real config and workflow, and making a local scheduler smoke validation an acceptance gate. Do not commit the production schedule if these scheduler tests fail.

The second risk is duplicate Linear issue creation if two scheduled runs overlap or if a retry happens after a partial failure. The scheduled job will use `overlap: skip`; the daemon scheduler will skip a due boundary while the same job is active; the script will search for the stable marker before creating; created descriptions will include the marker; and update/create will be performed per PR after rechecking existing dispatchable issues. If a duplicate still appears, the operator can mark one duplicate issue terminal and leave the marker-bearing dispatchable one.

The third risk is a previous failed resolver issue in `Triage` suppressing new actionable work. The scout will not dedupe against `Triage`, because `.scherzo/scherzo.yaml` configures `Todo` and `In Progress` as `tracker.active_states` and `Triage` as a failure state. A marker issue in `Triage` is historical evidence, not dispatchable work. Tests must prove that a `Triage` marker causes a new `Todo` issue to be created or planned rather than an update-only outcome.

The fourth risk is unsafe work on fork or cross-repository PRs. The script will reject any PR whose `head.repo.full_name` is absent, whose `base.repo.full_name` is absent, whose head repo differs from the base repo, or whose base repo differs from the configured repo. Tests will include fork, cross-repo, draft, deleted-head, and malformed cases.

The fifth risk is false-positive conflict detection from GitHub metadata. The script will treat `mergeable_state: dirty` as conflicted, but for other non-mergeable or unknown states it will use local merge preflight before enqueueing. If the preflight cannot run for a safe PR, the script will skip that PR instead of creating work from an uncertain signal and will expose the skipped reason `preflight_unavailable` in `--json-summary` for tests and manual debugging.

The sixth risk is leaking credentials or writing noisy logs. The script will read `GITHUB_TOKEN` or `GH_TOKEN` when present and `LINEAR_API_KEY` only when Linear work is needed. It will never print token values. The scheduled workflow will not pass `--json-summary` or `--verbose`, so no-conflict runs are silent. Failures will write concise stderr messages that identify the failed service and response shape, not secrets.

## Progress

- [x] (2026-05-07 00:00Z) Drafted the initial ExecPlan from the Linear issue and current repository inspection.
- [x] (2026-05-07 00:00Z) Incorporated adversarial review findings: scheduler runtime is now in scope, dedupe excludes non-dispatchable `Triage`, local preflight tests are mandatory, and config validation uses runtime loaders instead of substring checks alone.
- [ ] Add daemon scheduler runtime tests that prove a configured scheduled job starts a command-only workflow and skips overlap.
- [ ] Complete or verify daemon-side scheduled workflow runtime.
- [ ] Add offline tests for PR filtering, conflict classification, local merge preflight, Linear dedupe, and malformed responses.
- [ ] Add the GitHub/Linear scout command script.
- [ ] Add the command-only workflow and schedule configuration.
- [ ] Validate scheduler smoke, tests, formatting, and production glinter gate.

## Surprises & Discoveries

- Observation: The existing merge-conflict resolver is already a command/agent/command/publish workflow that delegates target extraction, validation, and publishing to `scripts/scherzo-merge-conflict`.
  Evidence: `.scherzo/workflows/merge-conflict-resolution.yaml` runs `scripts/scherzo-merge-conflict` for `prepare`, `validate`, and `publish`.
- Observation: Scheduled job configuration is parsed from a top-level `scheduled_jobs` list and rejects issue-context variables in scheduled workflows.
  Evidence: `src/scherzo/config/types.gleam` defines `ScheduledJobConfig`; `src/scherzo/config.gleam` parses `id`, `workflow`, `every`, `overlap`, `catch_up`, and `on_failure`; `src/scherzo/runtime_bundle.gleam` rejects scheduled workflow steps that reference `issue.*` variables.
- Observation: The checked-in `.scherzo/scherzo.yaml` currently routes `merge-conflict-resolution` but has no `scheduled_jobs` entry for a PR scout.
  Evidence: `.scherzo/scherzo.yaml` contains the merge-conflict routing entry and Linear workflow label list but no scheduled scout entry.
- Observation: Native scheduled runtime support is incomplete in the current daemon surface inspected for this plan.
  Evidence: `src/scherzo/orchestrator/schedule_core.gleam` contains cadence decisions and `src/scherzo/state/record.gleam` contains scheduled-run records, but `src/scherzo/orchestrator/daemon.gleam` imports `poll_scheduler` and `retry_scheduler` rather than `schedule_core`, and no existing source reference to `ScheduledDue` outside `schedule_core` and projection folds starts a workflow.

## Decision Log

- Decision: Implement LIV-124 as a native scheduled runtime plus a command-only scout workflow that creates Linear issues for the existing resolver, not as changes to the resolver itself.
  Rationale: The Linear issue promises periodic execution, and current source inspection found scheduled config support without daemon execution. The resolver intentionally handles one Linear issue and one target; discovery and dedupe are separate concerns and are safer as a scheduled command.
  Date: 2026-05-07
- Decision: Use existing scheduled config, cadence, record, projection, timer, and workflow-run infrastructure for daemon scheduling.
  Rationale: Reusing these pieces keeps the runtime proportionate and avoids inventing a second scheduler model. The new daemon code should only bridge configured due jobs into the existing workflow execution path.
  Date: 2026-05-07
- Decision: Use a Python script under `scripts/` for GitHub/Linear scanning.
  Rationale: The repository already uses a Python helper for merge-conflict work, and Python's standard library is sufficient for HTTP, JSON, subprocess, and offline unit tests without adding dependencies.
  Date: 2026-05-07
- Decision: Deduplicate only against dispatchable Linear states `Todo` and `In Progress`; treat marker issues in `Triage` as historical failures and create new dispatchable work.
  Rationale: `.scherzo/scherzo.yaml` configures `Triage` as the failure state rather than an active polling state. Updating a `Triage` marker would make the scout appear successful while the resolver never runs.
  Date: 2026-05-07
- Decision: Deduplicate with a body marker of the exact form `github-pr-conflict:bromanko/scherzo#<number>`.
  Rationale: The marker is stable across schedule intervals, independent of Linear issue title edits, and easy for both code and operators to find.
  Date: 2026-05-07
- Decision: Created Linear issues should attach only the workflow label `workflow:merge-conflict-resolution`.
  Rationale: `.scherzo/scherzo.yaml` requires exactly one workflow label for dispatch; support labels are unnecessary for this automated path and would complicate acceptance.
  Date: 2026-05-07

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam service that polls Linear and runs workflows defined in YAML files under `.scherzo/workflows/`. The main project configuration is `.scherzo/scherzo.yaml`. It declares the Linear tracker, workspace hooks, agent limits, workflow routing, artifact limits, and Linear workflow-label contract. Workflow labels are Linear issue labels whose names start with `workflow:`; this repository requires exactly one such label per issue so the daemon can choose one workflow.

The existing merge-conflict resolver is configured in `.scherzo/workflows/merge-conflict-resolution.yaml`. It has four steps: a command step that runs `scripts/scherzo-merge-conflict prepare`, an agent step that resolves materialized conflicts, a command step that runs `scripts/scherzo-merge-conflict validate`, and a command step that runs `scripts/scherzo-merge-conflict publish`. The helper script extracts one target PR or branch from the Linear issue title, description, or comments. Tests for this helper live in `test/merge_conflict_helper_test.gleam`.

Scheduled jobs are represented by `ScheduledJobConfig` in `src/scherzo/config/types.gleam` and parsed in `src/scherzo/config.gleam` from a top-level YAML list named `scheduled_jobs`. The current parser supports `id`, `workflow`, `enabled`, `every`, `overlap`, `catch_up`, and `on_failure`. The accepted overlap mode is `skip`, and `catch_up: true` is rejected. `src/scherzo/runtime_bundle.gleam` validates scheduled workflows and rejects steps that reference `issue.*`, because a scheduled workflow has no Linear issue context. Existing scheduled parser tests are in `test/config_test.gleam`, and pure cadence tests are in `test/orchestrator_schedule_core_test.gleam`.

The daemon lives primarily in `src/scherzo/orchestrator/daemon.gleam`. It is an actor: a long-running process that receives messages such as poll ticks, retry ticks, worker completions, and side-effect completions. Its `RuntimeDependencies` type already supplies `now_ms`, `send_after`, and `cancel_timer`, which tests can fake. Polling uses `src/scherzo/orchestrator/poll_scheduler.gleam`, retry timers use `src/scherzo/orchestrator/retry_scheduler.gleam`, and normal workflow execution ultimately reuses `src/scherzo/workflow_run.gleam`. Scheduled runtime should follow these patterns rather than running workflows synchronously inside the daemon.

The new scout fits these conventions by adding a workflow file with one command step and no `issue.*` template variables. It will run in a Scherzo workflow workspace, but it should treat that workspace as read-only except for temporary directories and test artifacts. It must not create commits, change PR branches, or invoke pi.

## Preconditions and Verified Facts

The repository root contains `.scherzo/scherzo.yaml`. That file currently routes `merge-conflict-resolution: workflows/merge-conflict-resolution.yaml`, lists `merge-conflict-resolution` in `linear_contract.workflow_labels`, and requires exactly one workflow label with prefix `workflow:`. Its tracker active states are `Todo` and `In Progress`; `Triage` is configured as a failure state and must not be treated as dispatchable dedupe state by the scout.

The file `.scherzo/workflows/merge-conflict-resolution.yaml` exists and has workflow id `merge-conflict-resolution`. Its command steps invoke `scripts/scherzo-merge-conflict` with `prepare`, `validate`, and `publish`. The scout must not remove or replace this workflow.

The file `scripts/scherzo-merge-conflict` exists and is a Python 3 executable. Its docstring states that the resolver locates one same-repository GitHub PR or branch named by a manually-created Linear issue, prepares a local merge commit, lets pi resolve conflicts, validates non-conflicted files, and publishes a fast-forward branch update. The new scout must create Linear issues whose title or description includes the PR URL or number so this existing extractor can run unchanged.

The file `test/merge_conflict_helper_test.gleam` exists and already uses `command_step.run` to run Python helper subcommands and assert stdout, stderr, status, and exit code. New tests for the scout should follow this pattern where CLI behavior is important.

The file `gleam.toml` uses Gleam on the Erlang target and lists `gleeunit` as the test framework and `glinter` as a dev dependency. Production glinter policy applies to `src/`, not `scripts/`, but full validation must still run the production glinter command.

## Scope Boundaries

In scope:

- Complete or verify daemon-side execution for enabled `scheduled_jobs` entries that point to validated workflows.
- Add tests that prove a scheduled workflow really starts, emits scheduled state records, respects `overlap: skip`, and does not use issue context.
- Add `scripts/scherzo-github-pr-conflict-scout` as the scout command.
- Add `.scherzo/workflows/github-pr-conflict-scout.yaml` as a one-step command-only workflow.
- Add a `github-pr-conflict-scout` routing entry and a top-level `scheduled_jobs` entry to `.scherzo/scherzo.yaml`.
- Add tests for offline scout logic, checked-in workflow/config loading, no-op behavior, conflicted PR behavior, dispatchable-state dedupe behavior, unsafe PR skipping, local preflight behavior, and malformed GitHub/Linear responses.
- Add any small helper code inside the new Python script needed to make the behavior testable without network access.

Out of scope:

- Do not change `.scherzo/workflows/merge-conflict-resolution.yaml` except tests may assert it remains routed.
- Do not change `scripts/scherzo-merge-conflict`; the resolver remains the only path that prepares, validates, and publishes conflict repairs.
- Do not add a GitHub App, webhook receiver, UI, operator command, or pi agent step for the scout.
- Do not scan fork PRs, draft PRs, closed PRs, other repositories, or arbitrary branches.
- Do not implement scheduled payload variables, `catch_up: true`, overlap modes other than `skip`, or a general-purpose GitHub workflow engine.
- Do not add Linear workflow label `workflow:github-pr-conflict-scout`; scheduled workflows are started by `scheduled_jobs`, not by Linear issue labels.

## Milestones

Milestone 1 completes native scheduled workflow execution. At the end, an in-process daemon test with fake timers demonstrates that an enabled `scheduled_jobs` entry starts a command-only workflow, emits due/pending/started/succeeded records, and skips an overlapping due boundary for the same job. This milestone comes first because a scout script and YAML entry are insufficient if no daemon can actually start scheduled workflows.

Milestone 2 locks down the checked-in scheduled-workflow loading surface. At the end, tests load `.scherzo/scherzo.yaml` and the new workflow through Scherzo's config/runtime loader, not by substring matching, and assert that the route, job interval, overlap policy, catch-up policy, workflow shape, and absence of issue-context references are all accepted by the same path the daemon uses.

Milestone 3 builds the scout's pure decision logic with offline tests. At the end, the repository will have tests that prove safe filtering, conflict classification, mandatory local merge preflight behavior, dedupe marker generation, dispatchable-state dedupe, and malformed response handling before any real GitHub or Linear mutation code is trusted.

Milestone 4 implements the production command path. At the end, `scripts/scherzo-github-pr-conflict-scout scan --repo bromanko/scherzo --json-summary --skip-local-preflight` can be exercised with fakes or fixtures, and the default scheduled invocation can run silently when there is nothing to enqueue.

Milestone 5 wires the workflow and schedule. At the end, `.scherzo/scherzo.yaml` routes and schedules `github-pr-conflict-scout`, and the workflow invokes the new script without `issue.*` variables.

Milestone 6 validates end to end. At the end, targeted scheduler tests, targeted scout tests, full tests, formatting checks, and glinter pass. A scheduler smoke run demonstrates that a shortened schedule fires a command-only workflow, and an offline scout run demonstrates that a conflicted PR produces one create/update action while a no-op scan exits zero without output in default mode.

## Plan of Work

First, implement daemon scheduling if it is not already present. Add a small scheduler runtime helper, preferably `src/scherzo/orchestrator/scheduled_runtime.gleam`, that converts enabled `ScheduledJobConfig` values into `schedule_core.ScheduleState` values, computes the next timer delay from `now_ms`, and applies `schedule_core.admit_due_boundaries` when a job tick arrives. Keep this helper pure except for accepting current time and returning decisions such as records to append, timers to set, and workflow starts to request. Use `schedule_core.initial_next_due`, `schedule_core.run_id`, and `schedule_core.iso_utc` rather than duplicating interval math.

Extend `src/scherzo/orchestrator/daemon.gleam` with a scheduled tick message such as `ScheduledTick(job_id: String, generation: Int)`, scheduled timer handles in daemon state, and pending scheduled run state keyed by job id and run id. On startup, initialize enabled jobs from `bundle.effective.orchestrator.scheduled_jobs`, schedule the first tick for each enabled job, and log a concise `scheduled_job_timer_set` event with job id, workflow id, and delay. On `ScheduledTick`, ignore stale generations, call the pure helper, append existing scheduled records from `src/scherzo/state/record.gleam`, start due workflows through the existing asynchronous workflow execution path, and immediately schedule the next due timer. Do not block the daemon actor while a scheduled workflow runs.

For each scheduled run, create a synthetic `tracker_issue.Issue` only for internal identity and workspace naming. Use an id that includes the job id and run id, an identifier such as `SCHEDULE-<job-id>`, a title such as `Scheduled workflow <job-id>`, an empty description, no comments, no labels, and a non-terminal state such as `Scheduled`. The scheduled workflow validation in `src/scherzo/runtime_bundle.gleam` must continue to reject `{{ issue.` references, so this synthetic issue must not be used to pass business inputs. If exact issue constructors have drifted, inspect `test/workflow_run_test.gleam` for the minimal valid issue fixture and use the same fields.

When a scheduled workflow starts, append `ScheduledJobDue`, `ScheduledRunPending`, and `ScheduledRunStarted` records using the existing record variants. When it completes, append `ScheduledRunSucceeded` or `ScheduledRunFailed` with the run id, attempt, reason, and run root if available. If a due boundary arrives while the same job is pending, active, or retry-waiting and the job uses `overlap: skip`, append `ScheduledJobSkipped` with reason `overlap_running` and do not start a second run. If the daemon is shutting down with a pending scheduled run that has not started, append `ScheduledRunPendingCancelled` with a clear reason.

Add scheduler tests before or alongside the runtime. Use the fake timer shape already exposed by `daemon.RuntimeDependencies.send_after`. A test in `test/orchestrator_daemon_scheduled_workflow_test.gleam` should start the daemon with a runtime bundle containing one enabled scheduled job whose workflow has one command step that writes a harmless marker under a repository-relative test workspace. The fake `send_after` should capture a `ScheduledTick`; the test should send that tick, release any command-step synchronization barrier, and assert that the marker exists or the command artifact succeeded and that the ledger/projection contains `scheduled_due`, `scheduled_run_started`, and `scheduled_run_succeeded` for the job. A second test should force a second tick while the first run is blocked and assert that exactly one workflow run starts and a skip record with `overlap_running` is recorded.

Then add `scripts/scherzo-github-pr-conflict-scout`. The script should be executable and should use only Python standard library modules: `argparse`, `dataclasses`, `json`, `os`, `re`, `subprocess`, `sys`, `tempfile`, `time`, `urllib.request`, `urllib.error`, and typing helpers. Define small data structures or dictionaries for pull requests, conflict observations, Linear issues, preflight results, and planned mutations. Keep network functions thin so unit tests can pass fake GitHub and Linear clients.

The production command should be:

    scripts/scherzo-github-pr-conflict-scout scan --repo bromanko/scherzo --linear-project-slug scherzo-f6f4bc92d6d7 --workflow-label workflow:merge-conflict-resolution

Support `--json-summary` for tests and manual debugging. Default production mode should print nothing when no issue is created or updated. Support `--skip-local-preflight` only for tests and fixture runs; the scheduled workflow must not pass it.

Implement GitHub scanning as follows. List open PRs from `https://api.github.com/repos/bromanko/scherzo/pulls?state=open&per_page=100`, following pagination until no next page remains. Accept `GITHUB_TOKEN` or `GH_TOKEN` as an optional bearer token. Validate that the response is a JSON list. For each PR, require an integer `number`, string `html_url`, object `base`, object `head`, object `base.repo`, object `head.repo`, string `base.repo.full_name`, string `head.repo.full_name`, string `base.ref`, string `head.ref`, and boolean-compatible `draft`. Skip any PR with `draft: true`, any PR whose base repo is not `bromanko/scherzo`, and any PR whose head repo does not equal the base repo. Treat missing required fields as malformed data unless the missing field is specifically `head.repo`, which GitHub can return as null for deleted branches; in that case skip the PR as unsafe with reason `deleted_head_repo`.

Implement conflict classification as follows. Fetch PR detail for each safe PR from `https://api.github.com/repos/bromanko/scherzo/pulls/<number>` because GitHub may populate `mergeable` and `mergeable_state` only on the detail endpoint. If `mergeable` is null, retry up to three times with a one-second delay. If `mergeable_state` is `dirty`, classify the PR as conflicted. If `mergeable` is true or `mergeable_state` is clearly mergeable, classify it as not conflicted. Treat `clean`, `has_hooks`, and `unstable` as non-conflicted metadata states unless `mergeable` is false. For `blocked`, unknown states, null mergeability after retries, or any false-but-not-dirty metadata, run local merge preflight.

Local merge preflight is mandatory for inconclusive safe PRs. Implement it in a function that can be replaced by a fake in tests. In production, create a temporary Git repository, read the current repository's `origin` remote URL using `git remote get-url origin`, add that remote to the temporary repository, fetch the base branch with `git fetch --depth=1 origin refs/heads/<base>:refs/remotes/origin/<base>`, fetch the head branch with `git fetch --depth=1 origin refs/heads/<head>:refs/remotes/origin/<head>`, check out the fetched base, and run `git merge --no-commit --no-ff refs/remotes/origin/<head>`. Return conflicted only when the merge exits nonzero and `git diff --name-only --diff-filter=U` reports at least one unmerged path. Return not conflicted when the merge exits zero. Return unavailable with reason `preflight_unavailable` when Git is unavailable, the origin URL cannot be read, either fetch fails, checkout fails, or the merge fails without unmerged paths. Abort or delete the temporary repository before returning so no merge state remains in the working tree.

Implement Linear dedupe as follows. Do not contact Linear until at least one conflicted PR has been found. Read `LINEAR_API_KEY` at that point and fail with a clear stderr message if it is absent. Query the Linear project by slug `scherzo-f6f4bc92d6d7` to obtain the project id and its team's id. Query labels for the team and find the label named `workflow:merge-conflict-resolution`. Query candidate issues in that project, paginating through results, and filter client-side. A dispatchable dedupe match must have state name `Todo` or `In Progress`, must have the workflow label, and must have a title or description containing the marker. If one dispatchable matching issue exists, update its description with the latest observed PR URL, base/head refs, SHAs when available, and timestamp. If more than one dispatchable matching issue exists, update the oldest matching issue and write a warning in `--json-summary`; do not create another issue. If matching marker issues exist only in non-dispatchable states such as `Triage`, ignore them for dedupe, include `historical_marker_issue_ignored` in `--json-summary`, and create a new dispatchable issue. If no dispatchable match exists, create an issue in the project/team with title `Resolve merge conflicts for PR #<number>`, description containing the PR URL, marker, repository, base branch, head branch, and a note that Scherzo's merge-conflict resolver should handle the repair, and label ids exactly `[<workflow label id>]`.

Add `.scherzo/workflows/github-pr-conflict-scout.yaml` with workflow id `github-pr-conflict-scout`, `max_parallel_steps: 1`, and one command step named `scan_open_prs`. The step should compute `repo_root` the same way existing workflows do, then execute the script with the production arguments above. Use `workspace: main` and a timeout of 300000 milliseconds unless implementation evidence shows real scans need more time.

Update `.scherzo/scherzo.yaml`. Under `routing.workflows`, add `github-pr-conflict-scout: workflows/github-pr-conflict-scout.yaml`. Under `linear_contract.workflow_labels`, do not add a scout workflow label. Under top-level `scheduled_jobs`, add one enabled job:

    scheduled_jobs:
      - id: github-pr-conflict-scout
        workflow: github-pr-conflict-scout
        every: 15m
        overlap: skip
        catch_up: false

If `.scherzo/scherzo.yaml` already has a `scheduled_jobs` section by the time this plan is implemented, append this job and preserve existing jobs. Do not add payload, variables, inputs, or `issue.*` template references because the config parser intentionally rejects scheduled payload fields and the runtime bundle rejects issue context in scheduled workflows.

## Concrete Steps

1. From the repository root, run `jj status --color=never` and confirm the working copy is clean or only contains intentional changes for this implementation.

2. Inspect `.scherzo/scherzo.yaml`, `.scherzo/workflows/merge-conflict-resolution.yaml`, `scripts/scherzo-merge-conflict`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/schedule_core.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/runtime_bundle.gleam`, `test/runtime_bundle_test.gleam`, `test/orchestrator_daemon_test.gleam`, and `test/merge_conflict_helper_test.gleam` to normalize drift from this plan. If a complete scheduled runtime and equivalent tests already exist, record that discovery in this plan and skip only the implementation substeps that are already satisfied.

3. Add a failing scheduler test file `test/orchestrator_daemon_scheduled_workflow_test.gleam`. The first test should configure one enabled job `scheduled-smoke` with `every: 1s`, one command-only workflow, fake timers, and a command step that records a harmless marker. Assert that, after the captured scheduled tick is delivered, the run succeeds and projection or ledger evidence contains due, started, and succeeded scheduled records for `scheduled-smoke`.

4. In the same test file, add a failing overlap test. Block the first scheduled command step with `test/test_async.gleam` barrier helpers, deliver a second tick for the same job, and assert that no second workflow starts and that a skipped record with reason `overlap_running` is recorded. Release the barrier during cleanup.

5. Run the targeted scheduler tests and confirm they fail because the daemon does not yet start scheduled workflows:

    direnv exec . gleam test --target erlang test/orchestrator_daemon_scheduled_workflow_test.gleam

   Expected failure before implementation: the test times out waiting for the scheduled marker or reports that no scheduled tick/start record was observed.

6. Add or complete `src/scherzo/orchestrator/scheduled_runtime.gleam` with pure initialization and tick-decision helpers around `schedule_core.ScheduleState` and `schedule_core.admit_due_boundaries`. Keep this module independent of Linear, GitHub, and command execution.

7. Extend `src/scherzo/orchestrator/daemon.gleam` with scheduled tick messages, timer state, stale-generation handling, startup timer initialization for enabled jobs, due/skip record appends, and asynchronous scheduled workflow starts. Reuse the existing workflow-run path; do not run command steps synchronously in the daemon actor.

8. Extend any daemon state, recovery, or projection wiring needed so scheduled run completion appends `ScheduledRunSucceeded` or `ScheduledRunFailed`, active scheduled jobs recover safely after daemon restart, and pending scheduled timers are cancelled on shutdown. Prefer existing record variants in `src/scherzo/state/record.gleam`; add new variants only if an existing scheduled record cannot represent the required state.

9. Run the targeted scheduler tests again and expect them to pass:

    direnv exec . gleam test --target erlang test/orchestrator_daemon_scheduled_workflow_test.gleam

10. Add Python unit tests under `test/python/github_pr_conflict_scout_test.py`. Use Python's `unittest` and import `scripts/scherzo-github-pr-conflict-scout` with `importlib.util.spec_from_file_location`. Define fake GitHub, Linear, and preflight functions in the test file; do not make network calls.

11. In `test/python/github_pr_conflict_scout_test.py`, add `test_noop_open_prs_succeeds_without_linear_mutation`. It should feed an empty GitHub PR list, call the pure scan function with a fake Linear client that fails the test if called, and assert the result has no created issues, no updated issues, and exit status success.

12. Add `test_conflicted_same_repo_pr_creates_issue`. It should feed one safe PR for `bromanko/scherzo` with `number: 123`, `draft: false`, `base.repo.full_name: bromanko/scherzo`, `head.repo.full_name: bromanko/scherzo`, and detail metadata `mergeable_state: dirty`. The fake Linear client should report no existing marker issue and should record one create mutation. Assert the create title contains `PR #123`, the description contains `https://github.com/bromanko/scherzo/pull/123` and `github-pr-conflict:bromanko/scherzo#123`, and the labels list contains exactly one id corresponding to `workflow:merge-conflict-resolution`.

13. Add `test_dedupe_updates_existing_dispatchable_issue`. It should feed the same conflicted PR and a fake Linear issue in state `Todo` whose description already contains `github-pr-conflict:bromanko/scherzo#123` and whose labels include `workflow:merge-conflict-resolution`. Assert the script records one update mutation for that issue id and no create mutation.

14. Add `test_triage_marker_does_not_suppress_dispatchable_work`. It should feed the same conflicted PR and a fake Linear issue in state `Triage` with the marker and workflow label. Assert the script creates one new issue, does not update the `Triage` issue, and includes `historical_marker_issue_ignored` in the JSON summary or returned summary object.

15. Add `test_skips_drafts_forks_cross_repo_and_deleted_head_prs`. It should include at least four PRs: a draft same-repo PR, a fork PR whose head repo differs from base repo, a PR whose base repo is not `bromanko/scherzo`, and a PR with `head.repo` null. Assert no Linear calls happen and the summary lists skipped reasons.

16. Add `test_malformed_github_response_fails`. It should feed malformed GitHub list data such as a JSON object where a list is expected or a PR missing `number`. Assert the script raises or returns an error whose message includes `malformed GitHub response`.

17. Add `test_malformed_linear_response_fails_when_conflict_needs_issue`. It should feed one conflicted safe PR and a fake Linear client that returns missing project/team/label fields or GraphQL errors. Assert the command fails with a message containing `malformed Linear response` or `Linear GraphQL returned errors`.

18. Add mandatory local-preflight tests. One test should feed inconclusive metadata such as `mergeable_state: blocked` with fake preflight `conflicted=False` and assert no Linear issue is created. A second should use fake preflight `conflicted=True` and assert exactly one create or update. A third should use fake preflight unavailable and assert no Linear mutation plus skipped reason `preflight_unavailable`.

19. Add a Gleam bridge test in `test/github_pr_conflict_scout_test.gleam` that uses `command_step.run` to execute `python3 -m unittest test.python.github_pr_conflict_scout_test` from the repository root. Assert exit code `0` and stdout contains `OK`. This ensures `gleam test` runs the Python unit tests.

20. Run the targeted bridge test and confirm it fails because `scripts/scherzo-github-pr-conflict-scout` does not exist yet:

    direnv exec . gleam test --target erlang test/github_pr_conflict_scout_test.gleam

   Expected failure before implementation: the command artifact reports a nonzero exit code and stderr or stdout mentions that the script file cannot be imported.

21. Create `scripts/scherzo-github-pr-conflict-scout` with the tested pure functions, local preflight function, Linear/GitHub HTTP clients, and the `scan` CLI. Make it executable. Keep side effects at the CLI boundary and make the fake-client unit tests pass without network.

22. Run the targeted scout test again:

    direnv exec . gleam test --target erlang test/github_pr_conflict_scout_test.gleam

   Expected success after implementation: the bridge test passes and the embedded Python unittest output ends with `OK`.

23. Add a runtime/config load test in `test/github_pr_conflict_scout_test.gleam` named `checked_in_github_pr_conflict_scout_workflow_loads_as_scheduled_command_test`. Use the same config/runtime loader helper used by `test/runtime_bundle_test.gleam`, with fake secrets if needed, to load `.scherzo/scherzo.yaml` through Scherzo's parser. Assert `orchestrator.scheduled_jobs` contains an enabled `github-pr-conflict-scout` job with `every_ms == 900_000`, `overlap == SkipOverlap`, and `catch_up == False`. Assert the loaded workflow has exactly one command step, invokes `scripts/scherzo-github-pr-conflict-scout`, and contains no issue-context template reference. A supplementary text check for absence of `{{ issue.` is allowed, but it must not be the only validation.

24. Run the runtime/config load test and confirm it fails because the workflow/config are not wired yet.

25. Add `.scherzo/workflows/github-pr-conflict-scout.yaml` with the single `scan_open_prs` command step described above.

26. Edit `.scherzo/scherzo.yaml` to add the routing entry and scheduled job. Preserve existing merge-conflict resolver routing, tracker states, and Linear workflow label list.

27. Run the targeted scout and config tests:

    direnv exec . gleam test --target erlang test/github_pr_conflict_scout_test.gleam

   Expected success: the Python bridge test and checked-in runtime/config load test pass.

28. Run the script help command:

    scripts/scherzo-github-pr-conflict-scout --help

   Expected success: exit code `0`, usage text lists the `scan` command, and no credentials are printed.

29. Perform the required local scheduler smoke validation. Use a temporary test configuration or uncommitted edit that sets the scout job interval to `1s` and uses fixture or no-op conditions so the command exits quickly. Start the daemon or in-process service using the repository's existing test harness, observe one scheduled run event or log for `github-pr-conflict-scout`, and restore the checked-in `15m` interval before committing. Do not commit the `1s` interval.

30. Run formatting and all tests from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

   Expected success: formatting check exits `0`, and `gleam test` reports all tests passed.

31. Run the production lint gate:

    direnv exec . gleam run -m glinter

   Expected success: no production lint errors. Existing warnings may remain, but the change must not add production `let assert`, `panic`, or `todo` in `src/`.

32. Commit after the tests and lint pass. Suggested logical commits: first `Add native scheduled workflow runtime` after the scheduler tests pass, then `Add scheduled GitHub PR conflict scout` after scout tests, config load, scheduler smoke, full tests, and glinter pass.

## Testing and Falsifiability

The scheduler-runtime claim is falsified if a configured enabled scheduled job loads but does not start a workflow when a due tick occurs. The tests in `test/orchestrator_daemon_scheduled_workflow_test.gleam` must prove this with fake timers and must fail before runtime implementation. They must also prove `overlap: skip` by blocking the first scheduled run, delivering a second tick, and observing one skip record instead of a second run.

The config-load claim is falsified if `.scherzo/scherzo.yaml` or `.scherzo/workflows/github-pr-conflict-scout.yaml` only pass substring checks but fail Scherzo's real parser or runtime workflow loader. The test in `test/github_pr_conflict_scout_test.gleam` must use the actual config/runtime loader path and assert the parsed scheduled job fields and loaded command step.

The scout's core falsifiable claim is that it creates or updates Linear issues only for safe, likely conflicted PRs and never creates duplicates for the same PR across active dispatchable states. The Python unit tests in `test/python/github_pr_conflict_scout_test.py` should directly falsify that claim by feeding controlled GitHub, Linear, and preflight data into the decision logic.

The no-op test must prove that an empty or all-clean PR scan returns success and does not contact Linear. This ensures no-conflict scheduled runs do not require a Linear mutation and can succeed silently.

The conflicted PR test must prove that one same-repository, non-draft PR with `mergeable_state: dirty` produces one create mutation with the marker `github-pr-conflict:bromanko/scherzo#123`, the PR URL, and exactly one workflow label id. This proves compatibility with the existing resolver and the Linear routing contract.

The dedupe tests must prove both sides of the state policy. A marker issue in `Todo` or `In Progress` must be updated instead of creating another issue. A marker issue only in `Triage` must not suppress new dispatchable work; the scout must create a new issue or planned create mutation.

The unsafe PR test must prove that drafts, forks, cross-repository PRs, deleted-head PRs, and PRs whose base repo is not `bromanko/scherzo` are skipped without Linear calls. This guards the safety boundary.

The local preflight tests are mandatory. They must prove that inconclusive metadata with fake preflight not conflicted produces no Linear mutation, fake preflight conflicted produces one create/update action, and unavailable preflight skips with reason `preflight_unavailable`. These tests make the metadata/preflight fallback behavior observable without constructing real Git repositories in unit tests.

The malformed GitHub and Linear tests must prove that invalid service responses fail closed. For GitHub, malformed list or PR detail data should produce a nonzero command result and a message naming the malformed GitHub response. For Linear, malformed project, team, label, issue, or mutation responses should fail only when a conflicted PR needs Linear work, and the message should name Linear without printing tokens.

## Validation and Acceptance

Run these commands from the repository root:

    direnv exec . gleam test --target erlang test/orchestrator_daemon_scheduled_workflow_test.gleam
    direnv exec . gleam test --target erlang test/github_pr_conflict_scout_test.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter

All commands must exit with status `0`. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands through direnv.

Run the script help command:

    scripts/scherzo-github-pr-conflict-scout --help

It must exit `0`, print usage for the `scan` command, and print no secret values.

Perform a scheduler smoke validation with a temporary interval of `1s`, then restore the checked-in `15m` interval before committing. The expected observation is a scheduled run event or log for `github-pr-conflict-scout`, a successful command step under fixture or no-op conditions, and scheduled records showing due, started, and succeeded. If this smoke validation cannot be made to run, do not enable or commit the production `scheduled_jobs` entry.

Using tests or an offline fixture mode, validate these acceptance cases:

- With no open PRs or no conflicted safe PRs, the scan exits `0` and creates or updates no Linear issue. In scheduled mode without `--json-summary`, stdout should be empty.
- A fork PR, cross-repository PR, draft PR, or deleted-head PR is skipped and causes no Linear mutation.
- A same-repository non-draft PR with a conflict creates one Linear issue with title naming the PR, description containing the PR URL and marker, and exactly one workflow label: `workflow:merge-conflict-resolution`.
- A second scan for the same conflicted PR updates an existing active marker issue in `Todo` or `In Progress` and does not create another issue.
- A marker issue in `Triage` does not suppress dispatchable work for a still-conflicted PR.
- Inconclusive GitHub metadata uses local preflight; preflight clean means no Linear mutation, preflight conflicted means one create/update action, and preflight unavailable means skip rather than guess.
- Malformed GitHub responses and malformed Linear responses produce nonzero command results with clear stderr and no token disclosure.
- `.scherzo/workflows/merge-conflict-resolution.yaml` and `scripts/scherzo-merge-conflict` remain the resolver path; no scheduled workflow invokes pi or the resolver directly.

## Rollout, Recovery, and Idempotence

The rollout is additive but must be gated. Native scheduler runtime can land first with tests and no production scout job. The new scout script is inert until the new workflow and scheduled job are configured. The production scheduled job is safe to disable by setting `enabled: false` on the `github-pr-conflict-scout` job or by removing the job entry from `.scherzo/scherzo.yaml`; the resolver workflow remains available for manually-created `workflow:merge-conflict-resolution` issues.

Scheduled runtime must be idempotent across restarts. On daemon startup, enabled jobs should schedule their next due boundary from the current time using `catch_up: false`; the daemon should not try to replay missed runs from downtime. If a run was active during a crash, recovery should leave enough record evidence to mark the old run failed or cancelled and then allow a future due boundary to start a fresh run. Overlap policy applies per job: while one run for `github-pr-conflict-scout` is pending, active, or retry-waiting, a later due boundary must be skipped rather than queued.

The scout command is idempotent for each PR because every create/update uses the marker `github-pr-conflict:bromanko/scherzo#<number>`. Re-running the same scan should update an existing active dispatchable issue, not create another. If a run fails after creating an issue but before printing its summary, the next run should find the marker if the issue is in `Todo` or `In Progress` and update it.

If GitHub is unavailable or returns malformed data, the scheduled run should fail without creating Linear work. Scherzo's scheduled records and logs should show the command failure. If Linear is unavailable after conflicts are found, the run should fail without losing safety; the next interval will retry and dedupe by marker if a prior create partially succeeded.

If the scout creates a wrong issue, the operator can remove or change the workflow label to stop automatic resolver dispatch, or mark the issue terminal. Because the scout only creates Linear issues and does not mutate Git branches, rollback does not require repository or GitHub repair.

## Artifacts and Notes

Important existing snippets verified while drafting:

    .scherzo/scherzo.yaml routes merge-conflict-resolution:
      merge-conflict-resolution: workflows/merge-conflict-resolution.yaml

    .scherzo/workflows/merge-conflict-resolution.yaml command steps invoke:
      scripts/scherzo-merge-conflict prepare
      scripts/scherzo-merge-conflict validate
      scripts/scherzo-merge-conflict publish

    test/merge_conflict_helper_test.gleam already checks that the resolver workflow is routed and guarded.

Recommended created Linear issue body shape:

    GitHub PR conflict scout found a likely merge conflict.

    PR: https://github.com/bromanko/scherzo/pull/123
    Repository: bromanko/scherzo
    Base branch: main
    Head branch: feature/example
    Dedupe marker: github-pr-conflict:bromanko/scherzo#123

    This issue is intended for Scherzo's existing workflow:merge-conflict-resolution resolver. The resolver should extract the PR URL above and repair only merge conflicts.

Expected scheduled scout config shape:

    scheduled_jobs:
      - id: github-pr-conflict-scout
        workflow: github-pr-conflict-scout
        every: 15m
        overlap: skip
        catch_up: false

## Interfaces and Dependencies

Daemon scheduler interfaces at the end of the runtime milestone:

- `src/scherzo/orchestrator/scheduled_runtime.gleam` should expose pure helpers to initialize enabled jobs from `List(ScheduledJobConfig)`, compute timer delays, admit due boundaries, and update per-job mode when runs start, finish, fail, or are skipped.
- `src/scherzo/orchestrator/daemon.gleam` should have a scheduled tick message, scheduled timer state, startup timer initialization, stale tick protection, and completion handling that writes scheduled records.
- Scheduled runs should use existing `src/scherzo/workflow_run.gleam` execution machinery and existing `src/scherzo/state/record.gleam` record variants.

The new script interface should be:

    scripts/scherzo-github-pr-conflict-scout scan \
      --repo bromanko/scherzo \
      --linear-project-slug scherzo-f6f4bc92d6d7 \
      --workflow-label workflow:merge-conflict-resolution \
      [--json-summary] \
      [--skip-local-preflight]

Environment variables:

- `GITHUB_TOKEN` or `GH_TOKEN` is optional and, when present, is used as the GitHub bearer token.
- `LINEAR_API_KEY` is required only when at least one conflicted PR needs a Linear create/update.
- `SCHERZO_REPO_ROOT`, `SCHERZO_CONFIG_DIR`, and `SCHERZO_WORKSPACE_PATH` may be present in scheduled workflow runs; the workflow should compute `repo_root` consistently with existing workflows and run the script from that root.

GitHub REST endpoints:

- `GET https://api.github.com/repos/bromanko/scherzo/pulls?state=open&per_page=100` for listing open PRs.
- `GET https://api.github.com/repos/bromanko/scherzo/pulls/<number>` for mergeability detail.

Linear GraphQL operations:

- Query the project by slug to obtain project id and team id.
- Query the label named `workflow:merge-conflict-resolution` to obtain its id.
- Query project issues, page through them, and filter client-side for dispatchable states `Todo` and `In Progress`, the marker, and the workflow label.
- Use `issueCreate` with exactly one label id when no dispatchable marker issue exists.
- Use `issueUpdate` to refresh description when a dispatchable marker issue exists.

New workflow file:

    .scherzo/workflows/github-pr-conflict-scout.yaml

It must define workflow id `github-pr-conflict-scout`, contain only command steps, and contain no `{{ issue.` template references.

## Open Questions and Clarifications Needed

None.
