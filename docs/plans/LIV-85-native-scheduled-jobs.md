# Implement native scheduled jobs with silent success and Linear failure triage

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators need a way to run recurring autonomous maintenance workflows without turning every successful interval into a Linear issue. After this change, an operator can declare a scheduled job in `scherzo.yaml`, have the daemon run an existing workflow on a fixed cadence, inspect local schedule state with `scherzoctl`, and only see Linear activity when a scheduled run terminally fails or explicitly needs human attention.

The visible outcome is a PR conflict repair job configured once in `scherzo.yaml`, running every 15 minutes, recording local due/start/success/failure history, skipping overlaps by default, respecting the same global agent concurrency limits as issue-triggered work, and creating or updating one open Linear triage issue for the job only after retries for a due interval are exhausted.

## Problem Framing and Constraints

Today Scherzo is shaped around Linear issues as the source of work. That is useful for human-requested work, but it is noisy and awkward for recurring maintenance jobs. If a scheduler creates a Linear issue for every interval, the Linear board becomes a log stream rather than an exception queue. Operators then have to mentally filter routine successes away from real failures.

The product shape for this plan is different: scheduled jobs are orchestration config, not work payloads. The schedule answers when to run, how overlaps and missed intervals behave, and what to do on failure. The workflow YAML continues to describe prompts, command steps, workspace hooks, model settings, and policy. The first slice must not add arbitrary schedule-level `input:` or `vars:` blobs. Job-specific details belong in the workflow file, prompt file, scripts, environment, or repository configuration.

The plan deliberately does not introduce a broad generic non-Linear work-source abstraction. It adds a narrow scheduled-invocation path that reuses existing workflow DAG execution while keeping issue-triggered dispatch intact. It also does not promise exactly-once execution. Recovery is at-least-once for an interrupted scheduled due interval, with deterministic run IDs and overlap rules to make duplicate or skipped work observable.


## Strategy Overview

Add a native scheduler alongside the existing Linear polling loop. The scheduler is driven by the existing daemon tick and local ledger, not by an external service. It reads `scheduled_jobs` from `scherzo.yaml`, computes fixed-interval due times, dispatches due runs into the same workflow execution machinery, and records every material lifecycle event in the local ledger.

The smallest coherent first releasable slice is local-only. It includes config parsing, scheduled template context, durable schedule state, daemon dispatch, retry, overlap handling, startup recovery, and `scherzoctl schedules status`, `history`, and `run --now`. Linear failure reporting, `logs --last`, examples, and polish come after that local slice is green. The hard gate is that implementation must not start Linear failure reporting until tests for delayed ticks, pause/resume, global-capacity waits, pending manual runs, startup recovery, and `next_due_at_ms` advancement all pass.

This approach is the right size because scheduled jobs need local state, recovery, and operator visibility to be safe. A cron workaround that shells into Scherzo cannot reliably see daemon concurrency, worker lifecycle, retries, or retained workspace paths. A generic work-source framework would be larger than the MVP because only one new source kind is needed and its semantics are fixed-interval schedules.

## Alternatives Considered

The simplest alternative is to document an external scheduler that creates or reopens Linear issues. That avoids daemon changes, but it makes Linear the primary recurring-work ledger and creates issue churn even when nothing needs human attention. It also cannot naturally respect Scherzo's in-process worker registry, global concurrency, retry timers, or local workflow recovery.

A second alternative is to add a generic `WorkSource` abstraction that treats Linear issues, scheduled jobs, future webhooks, and other triggers uniformly. That may become useful later, but it is not required for one fixed-interval scheduler. The first slice should avoid this refactor unless implementation proves the narrow scheduled path cannot safely reuse workflow execution.

A third alternative is to let scheduled jobs define their own prompt or input payload directly in `scherzo.yaml`. This duplicates the workflow mechanism, makes config harder to validate, and encourages two places to define work. The MVP explicitly rejects schedule-level arbitrary input blobs.


## Risks and Countermeasures

A scheduled agent can run too often, fall behind, or overlap with itself. The plan defines a durable schedule state machine with `idle`, `due_pending`, `waiting_for_global_slot`, `paused`, `active`, `retry_waiting`, `report_retry_waiting`, terminal, and skipped states. `next_due_at_ms` advances only when a due boundary is represented by a due or skipped ledger event. Delayed ticks summarize missed boundaries instead of silently enqueueing a burst, and overlap skips are visible in local history.

A daemon restart can interrupt a scheduled run or lose pre-start intent if that intent is only in process memory. The plan uses deterministic run IDs derived from job ID and due time, durable pending records before a worker starts, durable retry records, and at-least-once startup recovery. If a run was due but did not reach a terminal scheduled outcome before the daemon stopped, startup reconstructs whether it was pending, active, retrying, or waiting to report a failure and resumes or records a safe interruption.

A scheduled workflow can accidentally reference `{{ issue.* }}` and fail at runtime. The plan adds validation in config loading and doctor checks. Enabled scheduled jobs whose routed workflow references `issue.*` are rejected before dispatch. Disabled scheduled jobs may warn in doctor so operators can fix them before enabling.

A failing schedule can create a Linear issue storm. The MVP supports only `open_issue_per_job` dedupe, and the reporter always uses the stable dedupe key `scheduled-job:<job-id>`. It applies reserved Linear labels and a body marker derived from that key, records successful reports in the ledger, and records failed report attempts durably so startup and later ticks can retry without creating duplicate issues.

A local-only success model can leave operators blind. The first local slice includes `scherzoctl schedules status`, `history`, and `run --now`; the full MVP adds `logs --last` and `doctor`. Operators can see next due time, last success, last failure, skipped counts by reason, pending/waiting status, recent run IDs, and the latest run transcript when retained.

A scheduled workflow can bypass existing safety controls. Dispatch uses the same global `agent.max_concurrent_agents` limit as issue dispatch. Running scheduled sessions appear in the session hub and can be inspected or aborted through existing session controls. The new scheduler never starts automatic work while dispatch is paused, and manual `run --now` is rejected while paused.

Ledger schema changes can make rollback unsafe. The implementation should keep the existing ledger record schema version unless the on-disk record envelope itself must change. New scheduled record kinds must not be written when no schedules are configured and no scheduled records already exist. If scheduled records have been written, rollback to an older binary that does not know those record kinds is not guaranteed; `scherzoctl state status` must warn clearly and point operators to archive or reinitialize procedures before using an older binary.


## Progress

- [x] (2026-05-05 00:00Z) Drafted the ExecPlan from the Linear issue and current repository inspection.
- [x] (2026-05-05 00:30Z) Incorporated adversarial review feedback by specifying the schedule state machine, durable pending/report-retry records, Linear dedupe, rollback compatibility, daemon TDD split, and needs-human mapping.
- [x] (2026-05-06 00:00Z) Implemented config parsing and validation for `scheduled_jobs`, including fixed-interval parsing, defaults, unsupported payload fields, unsupported `catch_up: true`, unsupported overlap modes, unknown enabled workflows, and Linear failure-report config shape.
- [x] (2026-05-06 00:20Z) Implemented scheduled template context and enabled-scheduled-workflow issue-variable validation in runtime bundle loading.
- [x] (2026-05-06 00:45Z) Added scheduled ledger record encoding/decoding, a lightweight scheduled projection, pure schedule-core boundary/run-ID helpers, scheduled workspace path helpers, scheduled hook environment helpers, and focused unit tests for those foundations.
- [x] (2026-05-06 01:00Z) Added scheduled worker registry handles, run/session lookup, down-resolution support, and daemon down-resolution stubs that log scheduled worker exits without affecting issue worker behavior.
- [x] (2026-05-06 01:20Z) Applied review hardening for scheduled projection history by bounding per-job `recent_run_ids` retention to the latest 25 entries while leaving full detailed history in the ledger.
- [ ] Complete durable schedule projection semantics and startup recovery. Completed: ledger record schema, basic projection folding with bounded recent-run retention, scheduled worker registry bookkeeping, pending-run recovery, active-run interruption recovery into the scheduled retry path, and recovery of retry-waiting scheduled runs. Remaining: failure-report retry recovery and state-status rollback warning for scheduled records.
- [x] (2026-05-07 00:00Z) Integrated the local scheduled runtime into the daemon for enabled fixed-interval jobs: automatic due/pending/start/success/failure records, overlap skips, retry timers, command-only scheduled workflow dispatch without Linear issues, manual `schedules run --now`, and local `schedules status`/`history` projection output.
- [x] (2026-05-07 00:30Z) Applied review feedback by recording paused boundary skips as `schedule_paused`, including active run details in schedule JSON output, retrying interrupted active scheduled runs instead of terminally exhausting them, restoring retry-waiting scheduled runs on startup, accounting due boundaries on worker finish, scheduled retry tick, pause/resume, and manual run requests, and adding targeted daemon/protocol/CLI tests for those paths.
- [ ] Implement Linear failure reporting after local scheduler tests are green.
- [ ] Implement remaining `scherzoctl schedules` diagnostics, examples, tests, and rollout documentation.

## Surprises & Discoveries

- Observation: The current workflow execution path is issue-shaped all the way through `workflow_run.execute`, workspace preparation, template rendering, session summaries, and ledger records.
  Evidence: `src/scherzo/workflow_run.gleam` takes `tracker_issue.Issue`, `src/scherzo/template.gleam` exposes `issue.*`, and `src/scherzo/workspace_run.gleam` builds paths from `issue.identifier`.

- Observation: The local ledger already has workflow run, step attempt, retry, parking, outbox, and command records, but no scheduled job records.
  Evidence: `src/scherzo/state/record.gleam` defines `WorkflowRunStarted`, `StepAttemptStarted`, `RetryScheduled`, and related record bodies, while no `Scheduled...` record body exists.

- Observation: `scherzoctl` already has a local control API, session inspection, attach/events, pause/resume, retry, park/unpark, cleanup, and offline state commands.
  Evidence: `src/scherzo/ctl.gleam`, `src/scherzo/control/command.gleam`, and `src/scherzo/control/protocol.gleam` define the current command and protocol surfaces.

- Observation: Current doctor checks load workflow config and validate Linear, instance lock, workspace hooks, and pi, but there is no scheduled-job-specific check.
  Evidence: `src/scherzo/doctor.gleam` lists `WorkflowConfig`, `LinearContract`, `LinearSmoke`, `InstanceLock`, `WorkspaceHooks`, and `PiProbe`.

- Observation: The repository test harness does not accept individual file paths after `gleam test`; it accepts suite selectors through `-- --suite ...`.
  Evidence: `direnv exec . gleam test --target erlang test/config_test.gleam` exited with usage text, while `direnv exec . gleam test --target erlang -- --suite unit` ran the deterministic unit suite successfully.

- Observation: Runtime bundle loading resolves prompt files before scheduled workflow validation, so enabled scheduled jobs can be rejected using the actual prompt contents rather than just prompt file paths.
  Evidence: `src/scherzo/runtime_bundle.gleam` calls `resolve_prompt_files` before `validate_scheduled_workflows`, and `test/runtime_bundle_test.gleam` now covers a command template containing `{{ issue.identifier }}`.

- Observation: Storing every scheduled run ID in projection snapshots would make snapshots grow forever for high-frequency jobs.
  Evidence: Review identified unbounded `recent_run_ids` insertion in `src/scherzo/state/projection.gleam`; the projection now trims the list to 25 IDs and tests cover the cap.

- Observation: The scheduled projection records that a run is retry-waiting and what the next attempt is, but it does not preserve the original retry timer deadline as a first-class projected field.
  Evidence: `ScheduledRunRetryScheduled` includes `delay_ms` and `generation` in the ledger record, while `projection.ScheduledRunSummary` stores the retrying run ID, due time, reason, and next attempt. Startup recovery therefore restores a fresh daemon-local timer generation and retries soon rather than reconstructing an exact remaining delay from projection alone.


## Decision Log

- Decision: The MVP adds `scheduled_jobs` to `scherzo.yaml` as orchestration config and does not add schedule-level `input:` or `vars:`.
  Rationale: Work payloads already belong in workflow YAML, prompt files, scripts, environment, and repository config. Avoiding a second payload mechanism keeps the first slice small and prevents divergent workflow semantics.
  Date: 2026-05-05

- Decision: The MVP supports only fixed intervals with `every: <positive integer><ms|s|m|h>` and stores the resolved value as milliseconds.
  Rationale: Fixed intervals cover the requested 15-minute use case and avoid cron calendar semantics, timezone bugs, daylight-saving ambiguity, and missed-calendar catch-up questions.
  Date: 2026-05-05

- Decision: `overlap: skip` and `catch_up: false` are the MVP defaults. `overlap` values other than `skip` and `catch_up: true` are rejected for now.
  Rationale: Skipping overlaps is safe for recurring autonomous maintenance. Catch-up can create bursts after downtime and needs separate product decisions.
  Date: 2026-05-05

- Decision: Scheduled run IDs are deterministic from job ID and due time, and retry attempts keep the same logical run ID while using attempt-specific session IDs.
  Rationale: A stable due-run ID makes at-least-once recovery and dedupe observable. Attempt-specific session IDs can still distinguish retries in local history.
  Date: 2026-05-05

- Decision: Failure reporting supports only `open_issue_per_job` dedupe in the first slice.
  Rationale: It is enough to prevent issue storms while still surfacing recurring failures. Per-run or per-failure dedupe can be added later if operators need separate incident issues.
  Date: 2026-05-05

- Decision: Successful scheduled intervals never create Linear issues and do not update Linear by default, even if a previous failure issue exists.
  Rationale: The product goal is silent success. Operators can close or annotate failure issues manually; local history remains the source of successful recovery evidence.
  Date: 2026-05-05

- Decision: The scheduler uses a durable state machine with explicit pending and blocked states before worker start.
  Rationale: Delayed ticks, pause, global-capacity waits, queued manual runs, and restart recovery are safety-critical. Recording pending intent prevents due intervals and manual requests from disappearing on daemon restart.
  Date: 2026-05-05

- Decision: `next_due_at_ms` advances when an automatic due boundary is represented by `ScheduledJobDue` or `ScheduledJobSkipped`, not merely when a worker starts.
  Rationale: This prevents duplicate due records every tick while a run is pending or active, and it makes late evaluation, pause, and capacity waits deterministic.
  Date: 2026-05-05

- Decision: Linear `open_issue_per_job` dedupe is keyed by `scheduled-job:<job-id>` and enforced with reserved labels plus a stable body marker.
  Rationale: Configured labels are optional operator metadata and cannot be the only dedupe mechanism. A stable key lets Scherzo find an open failure issue even if local remembered issue state is absent.
  Date: 2026-05-05

- Decision: The implementation should not bump the ledger record schema version unless the shared record envelope changes, and it must not write scheduled records when no schedules are configured.
  Rationale: Existing deployments without schedules should remain safely rollbackable. Deployments that have written scheduled records need an explicit state-status warning because older binaries may not decode the new record kinds.
  Date: 2026-05-05

- Decision: For scheduled runs, `FinalTerminal` is silent success. `FinalActive` and `FinalNonActive` are treated as `needs_human` terminal scheduled failures with no retry and optional Linear reporting.
  Rationale: The current agent classification constructors are `FinalActive`, `FinalTerminal`, and `FinalNonActive`; there is no explicit `NeedsHuman` constructor. Issue-triggered behavior remains unchanged, while scheduled runs map non-terminal outcomes to the product promise of human triage.
  Date: 2026-05-05

- Decision: Linear failure reporting is gated behind the local scheduler slice.
  Rationale: It is safer to prove delayed ticks, pause/resume, global capacity waits, durable pending recovery, and local diagnostics before adding external issue creation or update side effects.
  Date: 2026-05-05

- Decision: The first implementation pass keeps scheduled projection snapshot support lightweight and leaves full daemon recovery for the next slice.
  Rationale: Adding the ledger record kinds, config validation, scheduled template context, pure boundary helpers, and workspace path support is a safe foundation that keeps the unit suite green. Full startup recovery depends on daemon runtime state and retry timers and should be completed with daemon dispatch tests rather than hidden inside record/projection work.
  Date: 2026-05-06

- Decision: Do not add the `examples/scherzo.yaml` scheduled job block yet.
  Rationale: The daemon, control protocol, CLI schedule commands, and Linear failure reporting are not implemented in this pass. Publishing an example that appears runnable before those surfaces exist would mislead operators.
  Date: 2026-05-06

- Decision: Scheduled projection retains at most 25 `recent_run_ids` per job.
  Rationale: `scherzoctl schedules status` needs quick recent context, not unbounded detailed history. Keeping only the latest 25 IDs bounds projection snapshot size for frequent schedules, while the append-only ledger remains the source for full `scherzoctl schedules history` details.
  Date: 2026-05-06

- Decision: Startup recovery for an enabled active scheduled run records `daemon_restart` as a retryable scheduled failure when another attempt is allowed, then schedules the same logical run ID for the next attempt.
  Rationale: This matches the plan's at-least-once recovery model. Treating an interrupted active run as terminally exhausted hides the fact that Scherzo can safely retry the due interval and prevents the operator from seeing the retry path in local history.
  Date: 2026-05-07

- Decision: Recovered retry-waiting scheduled runs use a fresh in-memory retry generation and a near-immediate daemon timer instead of trying to preserve the previous process timer identity.
  Rationale: Process timers do not survive daemon restart, and the projected scheduled status does not retain an exact target deadline. A fresh generation keeps stale ticks harmless and preserves the important invariant: the retry starts the same logical run ID at the recorded next attempt.
  Date: 2026-05-07

- Decision: The daemon now accounts automatic schedule boundaries not only on poll ticks, but also before worker-finish handling, scheduled retry timer handling, pause/resume transitions, and manual run requests.
  Rationale: These transitions all observe wall-clock time and can otherwise delay or misclassify skipped boundaries. Accounting before the transition preserves overlap, paused, and capacity attribution while still using the existing poll-driven loop for ordinary operation.
  Date: 2026-05-07

## Outcomes & Retrospective

Foundational milestones are partially complete as of 2026-05-06. The code now understands scheduled job configuration, rejects schedule-level arbitrary input payloads, renders scheduled template variables without fabricating a Linear issue, rejects enabled scheduled workflows that reference `issue.*`, records and decodes scheduled ledger event kinds, folds basic scheduled history into projection state, bounds projected recent run ID retention to prevent snapshot growth, computes fixed-interval boundaries and deterministic run IDs, can build issue-free scheduled workspace paths and hook environments, and can track scheduled worker handles in the worker registry. Validation for the foundation pass used `direnv exec . gleam format --check src test` and `direnv exec . gleam test --target erlang -- --suite unit`, with the unit suite reporting 780 passed and no failures. Review hardening added the recent-run retention cap and targeted projection coverage, then reran `direnv exec . gleam format --check src test` and `direnv exec . gleam test --target erlang -- --suite unit`; the unit suite reported 783 passed and no failures.

As of 2026-05-07, the local-only scheduled runtime is observable: an enabled scheduled job reaches due/pending/started/succeeded records, command-step success is visible in the local ledger, same-job active overlap records `overlap_running` without a second start, manual `schedules run --now` reaches the daemon through the control protocol, and `scherzoctl schedules status`/`history` can inspect local projection state without creating Linear issues. Review follow-up hardened recovery and boundary accounting: active runs interrupted by daemon restart now enter the scheduled retry path when attempts remain, retry-waiting runs are restored on startup, and due-boundary accounting also happens around worker finish, scheduled retry ticks, pause/resume, and manual-run requests. The final review-fix validation ran `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, and `direnv exec . gleam run -m glinter`; tests reported 814 passed and no failures, and glinter reported 0 errors with the existing warning inventory. Remaining gaps are Linear failure reporting, failure-report retry recovery, `schedules logs --last`, schedule doctor, examples, and the state-status rollback warning for ledgers containing scheduled records.

## Context and Orientation

Scherzo is a Gleam application. The daemon in `src/scherzo/orchestrator/daemon.gleam` polls Linear, selects candidate issues, dispatches workers, tracks active sessions, records local state, and reacts to retries and operator commands. The pure dispatch decisions live mostly in `src/scherzo/orchestrator/core.gleam` and runtime data structures live in `src/scherzo/orchestrator/state.gleam`.

Config loading starts in `src/scherzo/runtime_bundle.gleam`, which loads `scherzo.yaml`, resolves orchestrator config through `src/scherzo/config.gleam`, loads workflow DAG YAML files through `src/scherzo/workflow_dag.gleam`, resolves prompt files, and builds a `RuntimeBundle`. Config types are in `src/scherzo/config/types.gleam`. The documented example is `examples/scherzo.yaml`.

A workflow DAG is the workflow YAML format with `version`, `id`, optional `description`, optional `max_parallel_steps`, and `steps`. Agent steps reference prompt files and command steps run shell commands. `src/scherzo/workflow_run.gleam` executes a DAG, prepares workspaces through `src/scherzo/workspace_run.gleam`, runs command or agent steps, and writes workflow checkpoints through `src/scherzo/workflow_checkpoint.gleam`.

Template rendering currently lives in `src/scherzo/template.gleam`. Its context contains a Linear issue and exposes variables such as `issue.id`, `issue.identifier`, `issue.title`, `issue.description`, `issue.state`, `issue.labels`, and `attempt`. Scheduled workflows need a different context: they must expose schedule/run variables and must not expose `issue.*` unless the run was actually triggered by a Linear issue.

The local durable state ledger lives under the configured workspace root in `.scherzo-state/ledger/current.jsonl`, with snapshots in `.scherzo-state/ledger/snapshot.json`. The record schema is in `src/scherzo/state/record.gleam`, the in-memory projection is in `src/scherzo/state/projection.gleam`, and startup recovery is in `src/scherzo/state/recovery.gleam`. The ledger is append-only between compactions and is already used for workflow run recovery, retries, parking, command receipts, and outbox replay.

The local operator CLI is implemented in `src/scherzo/ctl.gleam`. It talks to the daemon control API defined in `src/scherzo/control/protocol.gleam` and `src/scherzo/control/server.gleam`. Existing commands include `ps`, `session`, `events`, `attach`, `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`, `cleanup`, and `state` commands.

## Preconditions and Verified Facts

From the repository root, `jj status --color=never` reported a clean working copy before this plan was written.

After the 2026-05-06 foundation pass, `config_types.OrchestratorConfig` contains `effective`, `config_dir`, `routing`, `dag_hooks`, `artifact_limits`, `model_settings`, and `scheduled_jobs`. `config_types.EffectiveConfig` still contains tracker, polling, workspace, hooks, agent, pi, handoff, Linear contract, and Linear command config; schedules remain outside `EffectiveConfig` as local orchestration config. The new scheduled job config types are `ScheduledJobConfig`, `ScheduledOverlap`, `ScheduledFailureConfig`, `ScheduledLinearFailureConfig`, and `ScheduledFailureDedupe` in `src/scherzo/config/types.gleam`.

`src/scherzo/config.gleam` already has helpers for strict booleans, strict strings, relative workflow paths, workflow IDs, positive integers, and YAML map access. Reuse those patterns when adding scheduled jobs. Do not introduce a second YAML parser.

`examples/scherzo.yaml` currently documents tracker, polling, workspace hooks, agent, pi, handoff, routing, artifact limits, Linear contract, and Linear commands. It has no `scheduled_jobs` example.

`src/scherzo/workflow_dag.gleam` validates workflow IDs and workspace names as lower-case letters or digits followed by lower-case letters, digits, `_`, or `-`. Reuse the same shape for scheduled job IDs.

`src/scherzo/template.gleam` currently renders with an issue-shaped context. It supports variables, `if`, and `for` blocks. Unknown variables are render errors. This is useful: scheduled runs can reject `issue.*` by rendering with a scheduled context, and doctor can pre-scan templates before runtime.

`src/scherzo/workspace_run.gleam` currently builds workflow run roots from `workspace.root`, workflow ID, issue identifier, and run ID. Scheduled jobs need a sibling path builder that does not rely on a Linear issue identifier.

`src/scherzo/orchestrator/daemon.gleam` schedules poll ticks with `poll_scheduler`, starts worker processes, registers sessions in the session hub, handles worker finish/failure, applies side effects through `effect_runner`, and appends ledger bodies through `append_ledger_bodies`.

`src/scherzo/orchestrator/core.gleam` enforces global and per-state issue concurrency and retry behavior for issue dispatch. Scheduled runs must share the global `agent.max_concurrent_agents` budget, but they should not be subject to Linear issue active/terminal state checks or per-Linear-state limits.

`src/scherzo/linear_triage.gleam` currently reports invalid workflow labels by commenting or moving an existing Linear issue. It does not create or update scheduled failure issues yet. Add scheduled failure reporting there or in a new adjacent module, reusing the existing `linear` transport conventions.

## Scope Boundaries

In scope for this ExecPlan:

- Parse `scheduled_jobs` from `scherzo.yaml` into orchestrator config.
- Add fixed-interval scheduling with `every: 15m`-style durations.
- Add scheduled workflow invocation context variables: `scheduled_job.id`, `scheduled_job.workflow`, `schedule.due_at`, `schedule.started_at`, `run.id`, and `run.attempt`.
- Validate that enabled scheduled workflows do not reference `issue.*` in agent prompts or command templates.
- Record local scheduled job state in the ledger and projection.
- Dispatch scheduled workflows directly from the daemon without Linear issues.
- Respect global dispatch pause and `agent.max_concurrent_agents`.
- Support default `overlap: skip` and default `catch_up: false`.
- Retry a failed due interval with existing retry limits and backoff.
- Report terminal scheduled failures or needs-human outcomes to Linear after retries are exhausted.
- Add `scherzoctl schedules status`, `history`, `logs --last`, `run --now`, and `doctor`.
- Add tests for config, template context, schedule state, daemon dispatch, failure reporting, and CLI behavior.

Out of scope for this first slice:

- Cron expressions, calendars, timezones, business hours, jitter, or distributed scheduling.
- `catch_up: true` behavior.
- `overlap: queue` or `overlap: cancel` behavior.
- Arbitrary schedule-level `input:`, `vars:`, or parameter blobs.
- Generic work-source abstractions for future triggers.
- Creating Linear issues for successful intervals.
- Auto-closing or commenting on a prior failure issue after a later success.
- Exactly-once execution guarantees.
- Per-run or per-failure Linear dedupe modes beyond `open_issue_per_job`.


## MVP Config Schema

Add an optional top-level `scheduled_jobs` list to `scherzo.yaml`. The MVP schema is:

    scheduled_jobs:
      - id: pr-conflict-repair
        workflow: pr-conflict-repair
        enabled: true
        every: 15m
        overlap: skip
        catch_up: false
        on_failure:
          linear:
            enabled: true
            state: Triage
            labels:
              - scherzo:scheduled
              - job:pr-conflict-repair
            dedupe: open_issue_per_job

The fields are:

- `id`: required. A stable local job ID. Use the same character rules as workflow IDs: lower-case letter or digit first, then lower-case letters, digits, `_`, or `-`.
- `workflow`: required. The ID of an existing workflow in `routing.workflows`. It must match a loaded workflow DAG ID.
- `enabled`: optional, default `true`. Disabled jobs are parsed and shown by doctor/status, but not scheduled.
- `every`: required for enabled jobs. A fixed positive duration string with units `ms`, `s`, `m`, or `h`. Examples: `500ms`, `30s`, `15m`, `2h`. Whitespace around the value is trimmed. The resolved interval must be at least `1000` milliseconds for enabled jobs to prevent tight agent loops; tests should cover rejecting `every: 0s` and `every: 500ms`.
- `overlap`: optional, default `skip`. The MVP accepts only `skip`; any other value is a config error with code `invalid_scheduled_job_overlap`.
- `catch_up`: optional, default `false`. The MVP accepts `false`; `true` is a config error with code `scheduled_job_catch_up_unsupported`.
- `on_failure.linear.enabled`: optional, default `false`. When false, terminal failures remain local only.
- `on_failure.linear.state`: required when Linear failure reporting is enabled. It is a Linear workflow state name, such as `Triage`, resolved at report time using the configured tracker project/team metadata.
- `on_failure.linear.labels`: optional string list, default `[]`. These are extra operator labels applied to created failure issues. Scherzo always also applies reserved dedupe labels `scherzo:scheduled` and `scherzo:scheduled-job:<job-id>`; configured labels cannot remove or replace those reserved labels.
- `on_failure.linear.dedupe`: optional, default `open_issue_per_job`. The MVP accepts only `open_issue_per_job`.

Reject these fields under each scheduled job in the MVP: `input`, `inputs`, `vars`, `variables`, and `payload`. The error message must say that schedule-level arbitrary inputs are intentionally deferred and that job-specific details should live in workflow YAML, prompt files, scripts, environment, or repository config.

Represent this in `src/scherzo/config/types.gleam` with concrete types similar to:

    pub type ScheduledOverlap {
      SkipOverlap
    }

    pub type ScheduledFailureDedupe {
      OpenIssuePerJob
    }

    pub type ScheduledLinearFailureConfig {
      ScheduledLinearFailureConfig(
        enabled: Bool,
        state: Option(String),
        labels: List(String),
        dedupe: ScheduledFailureDedupe,
      )
    }

    pub type ScheduledFailureConfig {
      ScheduledFailureConfig(linear: ScheduledLinearFailureConfig)
    }

    pub type ScheduledJobConfig {
      ScheduledJobConfig(
        id: String,
        workflow: String,
        enabled: Bool,
        every_ms: Int,
        overlap: ScheduledOverlap,
        catch_up: Bool,
        on_failure: ScheduledFailureConfig,
      )
    }

Add `scheduled_jobs: List(ScheduledJobConfig)` to `OrchestratorConfig`, not `EffectiveConfig`, because schedules are local orchestration config that point at workflow DAGs. `runtime_bundle.RuntimeBundle` already carries `orchestrator`, so the daemon can read schedules from the loaded bundle.

## Scheduled Workflow Template Context

Add a scheduled invocation context while preserving current issue-triggered rendering. The scheduled context exposes:

- `scheduled_job.id`: the job ID, for example `pr-conflict-repair`.
- `scheduled_job.workflow`: the workflow ID, for example `pr-conflict-repair`.
- `schedule.due_at`: the due interval timestamp in UTC ISO-8601, for example `2026-05-05T12:00:00Z`.
- `schedule.started_at`: the actual dispatch start timestamp in UTC ISO-8601.
- `run.id`: the deterministic logical run ID, for example `schedule-pr-conflict-repair-20260505T120000Z`.
- `run.attempt`: the attempt number for this due interval, starting at `1`.

Do not expose `issue.*` in scheduled context. Rendering a scheduled prompt that references `issue.identifier`, `issue.title`, or any other `issue.*` variable must fail validation before dispatch.

Implement this by changing `src/scherzo/template.gleam` from a context that always stores `issue: tracker_issue.Issue` to a context with an invocation variant, for example:

    pub type InvocationContext {
      IssueInvocation(issue: tracker_issue.Issue)
      ScheduledInvocation(run: ScheduledTemplateContext)
    }

    pub type ScheduledTemplateContext {
      ScheduledTemplateContext(
        job_id: String,
        workflow_id: String,
        due_at: String,
        started_at: String,
        run_id: String,
        attempt: Int,
      )
    }

Keep existing public `render` and `render_with_locals` wrappers for issue runs so existing callers and tests continue to pass. Add `render_scheduled` and `render_scheduled_with_locals` wrappers for scheduled runs. The evaluator should resolve issue variables only under `IssueInvocation` and scheduled variables only under `ScheduledInvocation`.

Add a small static template-reference helper in `src/scherzo/template.gleam`:

    pub fn referenced_variables(template: String) -> List(String)

This function should scan `{{ ... }}` variables and the expression part of `{% if ... %}` and `{% for name in ... %}` tags. It does not need to evaluate filters because filters are already unsupported. Use it in validation to detect any variable that equals `issue` or starts with `issue.`.

Update workflow execution so agent prompts and command strings render using the invocation context. If command steps currently receive issue fields through `workflow_run.StepContext`, keep existing issue fields for issue runs but add scheduled fields or a new `invocation` field so scheduled command environment and template rendering do not require a fake issue.


## Runtime Semantics

### Parsing and validation

`runtime_bundle.load` must parse schedules as part of orchestrator config. After workflows are loaded, validate that every enabled scheduled job references a known workflow ID. Validate disabled jobs for basic shape, but do not require their workflow to pass scheduled template validation until enabled.

For each enabled scheduled job, inspect the loaded workflow DAG. For every agent step prompt and command step run string, use `template.referenced_variables`. If any reference is `issue` or starts with `issue.`, return a config or bundle error with code `scheduled_workflow_requires_issue_context` and a message that names the job ID, workflow ID, step ID, and variable. Doctor should present the same issue with remediation.

### Interval representation

The MVP stores `every` as `every_ms: Int` in config. Duration parsing accepts only integer values followed by `ms`, `s`, `m`, or `h`. Convert with checked multiplication. Reject overflow, missing unit, unknown unit, zero, negative values, and enabled intervals below `1000` milliseconds.

### Due time computation and `next_due_at_ms`

Due times are UTC interval boundaries anchored at Unix epoch milliseconds. For interval `I` and current time `now_ms`, the boundary at or before now is `floor(now_ms / I) * I`, and the next boundary after any timestamp `t` is `((t / I) + 1) * I` using integer division.

For a new enabled job with no local schedule state, initialize `next_due_at_ms` to the next boundary after daemon startup time. This prevents every newly configured job from running immediately on first daemon start unless the operator explicitly runs it with `scherzoctl schedules run <job> --now`.

For an existing job, `next_due_at_ms` is computed from the latest automatic due boundary represented in the ledger. A boundary is represented by either `ScheduledJobDue` or `ScheduledJobSkipped`. Manual runs never represent an automatic boundary and never move automatic `next_due_at_ms`.

When the scheduler observes an idle enabled job and `now_ms >= next_due_at_ms`, compute every boundary from `next_due_at_ms` through `due_at_or_before(now_ms, every_ms)`. With `catch_up: false`, Scherzo admits only the latest due boundary in that range. If the range contains older boundaries, append one `ScheduledJobSkipped` with reason `catch_up_disabled` and `skipped_count` equal to the number of older boundaries, then append `ScheduledJobDue` and `ScheduledRunPending` for the latest boundary. This handles delayed ticks and intervals shorter than the polling interval without creating a burst.

When the scheduler observes that a job already has a pending, active, or retry-waiting run and one or more later automatic boundaries have arrived, append one `ScheduledJobSkipped` summarizing those later boundaries and advance `next_due_at_ms` to the next boundary after `now_ms`. Use reason `overlap_running` while an attempt is active or retrying, `waiting_for_global_slot` while a run is pending only because global capacity is full, and `schedule_paused` while dispatch is paused. This prevents duplicate due records while still making skipped intervals visible.

Every daemon transition that observes wall-clock time must first account for automatic boundaries through that time. This includes poll ticks, worker finish messages, retry timer messages, pause/resume changes, manual-run requests, and startup recovery. The pure `schedule_core` tests should cover the same accounting rules so daemon tests do not become the only specification.

### Scheduled run state machine

The schedule state machine is per job. The projection reconstructs the current state from ledger records and current daemon facts.

`idle` means the job is enabled, has no pending run, no active scheduled worker, and no scheduled retry waiting. On a due boundary, `idle` moves to `due_pending` by appending `ScheduledJobDue` and `ScheduledRunPending`. If dispatch is not paused and global capacity is available, the daemon may append `ScheduledRunStarted` in the same ledger batch and move directly to `active`.

`due_pending` means a due or manual run is durably admitted but no worker has started. If dispatch is paused, append `ScheduledRunPendingBlocked` with reason `paused` and expose status `paused`. If global capacity is full, append `ScheduledRunPendingBlocked` with reason `waiting_for_global_slot` and expose status `waiting_for_global_slot`. Do not append repeated blocked records on every tick; append a new blocked record only when the blocking reason changes.

`waiting_for_global_slot` means the due or manual run must start later, not be skipped. When capacity becomes available and dispatch is not paused, append `ScheduledRunStarted` and move to `active`. If the daemon restarts before start, startup recovery keeps this pending run durable and tries again under the same run ID.

`paused` means the due or manual run is pending while dispatch is paused. Automatic due intervals that arrive while paused are skipped with reason `schedule_paused`; the already-pending run remains the only pending run for that job. On resume, the daemon starts it if capacity is available or moves it to `waiting_for_global_slot`.

`active` means a scheduled worker is registered in the worker registry and session hub. On success, append `ScheduledRunSucceeded` and move to `terminal_success`. On retryable failure, append `ScheduledRunFailed(retry_exhausted: False)` and `ScheduledRunRetryScheduled`, then move to `retry_waiting`. On final failure or needs-human outcome, append `ScheduledRunFailed(retry_exhausted: True)` and move to `terminal_failure`.

`retry_waiting` means the same logical due interval will be attempted again after the existing retry backoff. Automatic due intervals that arrive while retrying are skipped with reason `overlap_running`. When the retry timer fires, append `ScheduledRunStarted` for the same `run_id`, the same `due_at_ms`, and the next attempt number. If the daemon restarts, recover the retry timer from the ledger.

`terminal_success` and `terminal_failure` are completed local outcomes for one logical run. A terminal success is silent: no Linear call, no handoff success report, and no Linear issue update. A terminal failure enters failure reporting only if `on_failure.linear.enabled` is true.

`report_retry_waiting` means the run is terminal locally, Linear reporting was configured, and the previous report attempt failed. The run must not execute again because the workflow outcome is already terminal. Later ticks and startup recovery retry only the report side effect.

`skipped` is a represented automatic boundary that will not run. Valid MVP skip reasons are `catch_up_disabled`, `overlap_running`, `waiting_for_global_slot`, and `schedule_paused`. Skipped manual runs are not created; invalid manual requests are rejected before ledger admission.

### Deterministic run IDs

The logical run ID for a scheduled due interval is:

    schedule-<safe-job-id>-<utc-basic-due-time>

For the worked example due at `2026-05-05T12:00:00Z`, use:

    schedule-pr-conflict-repair-20260505T120000Z

The safe job ID uses the same safe component rules as `src/scherzo/workflow_identity.gleam`, preserving letters, digits, `.`, `_`, and `-`, and replacing other graphemes with `_`. The timestamp is always UTC and contains no colon characters so it is safe in file names, session IDs, and logs.

Retries for the same due interval use the same logical `run.id` in template context and ledger dedupe. The local session ID and attempt-specific records include `a<attempt>` so operators can distinguish retry attempts. If a retry starts after a daemon restart, it must reuse the same logical run ID and increment or restore the attempt count from ledger state.

Manual runs use due time equal to the operator request time rounded to milliseconds and a run ID:

    schedule-<safe-job-id>-manual-<utc-basic-start-time>

Manual runs are recorded with trigger `manual` and do not alter the next automatic due boundary. Manual runs can be durably queued while waiting for global capacity, but they are rejected while dispatch is paused or while the same job already has a pending, active, or retry-waiting run.

### Ledger records

Add scheduled job records to `src/scherzo/state/record.gleam`, update JSON encoding/decoding, update projection, and update state tests. The exact record bodies should be:

    ScheduledJobDue(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      trigger: String,
    )

    ScheduledJobSkipped(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      reason: String,
      skipped_count: Int,
    )

    ScheduledRunPending(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      trigger: String,
      requested_at_ms: Int,
    )

    ScheduledRunPendingBlocked(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      reason: String,
      observed_at_ms: Int,
    )

    ScheduledRunPendingCancelled(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      reason: String,
      cancelled_at_ms: Int,
    )

    ScheduledRunStarted(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      started_at_ms: Int,
      run_id: String,
      attempt: Int,
      session_id: String,
      run_root: String,
    )

    ScheduledRunSucceeded(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      attempt: Int,
      finished_at_ms: Int,
      token_total: Int,
      turns: Int,
    )

    ScheduledRunFailed(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      attempt: Int,
      finished_at_ms: Int,
      reason: String,
      retry_exhausted: Bool,
      run_root: Option(String),
    )

    ScheduledRunRetryScheduled(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      next_attempt: Int,
      delay_ms: Int,
      generation: Int,
      reason: String,
    )

    ScheduledRunRetryCancelled(
      job_id: String,
      run_id: String,
      generation: Int,
      reason: String,
    )

    ScheduledFailureReported(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      attempt: Int,
      dedupe_key: String,
      linear_issue_id: String,
      action: String,
    )

    ScheduledFailureReportFailed(
      job_id: String,
      workflow_id: String,
      due_at_ms: Int,
      run_id: String,
      attempt: Int,
      dedupe_key: String,
      error_code: String,
      error_message: String,
      next_retry_at_ms: Int,
      generation: Int,
    )

Use snake-case kinds such as `scheduled_job_due`, `scheduled_run_pending`, `scheduled_run_pending_blocked`, `scheduled_run_pending_cancelled`, and `scheduled_failure_report_failed`. Keep the ledger additive. Do not rewrite existing issue workflow records.

Projection should expose, at minimum, for each job:

- job ID and workflow ID;
- next due time computed from represented automatic boundaries and current config;
- current state: `idle`, `due_pending`, `waiting_for_global_slot`, `paused`, `active`, `retry_waiting`, `report_retry_waiting`, `terminal_success`, or `terminal_failure`;
- current pending run, if any, including trigger and blocking reason;
- current active run, if any;
- last due time;
- last success time and run ID;
- last failure time, run ID, reason, and retry count;
- skipped counts by reason;
- recent run IDs with due/start/finish status, capped to the latest 25 entries in projection snapshots while full history remains in the ledger;
- open failure issue ID and dedupe key, if known;
- pending failure report retry, if any.

The new binary must decode old snapshots that do not have scheduled projection data by treating the scheduled projection as empty. Do not bump `record.schema_version` only because new scheduled kinds exist. If a schema bump proves unavoidable, add explicit migration tests, state-status warnings, and rollback refusal behavior before writing any scheduled records.

### Startup recovery

Startup loads the ledger using the existing `ledger.replay` path. Add scheduled recovery after existing issue recovery projection is loaded and before the first poll tick dispatches schedules.

For every enabled job:

1. Rebuild scheduled projection from ledger.
2. If `ScheduledRunPending` exists without `ScheduledRunStarted`, `ScheduledRunPendingCancelled`, or a terminal record for the same run, keep it pending under the same `run_id`. If the job is now disabled or its workflow no longer exists, append `ScheduledRunPendingCancelled` with reason `job_disabled` or `workflow_missing` and do not start it.
3. If a pending run survives recovery and dispatch is paused, append or preserve `ScheduledRunPendingBlocked(reason: "paused")`. If dispatch is not paused but global capacity is full, append or preserve `ScheduledRunPendingBlocked(reason: "waiting_for_global_slot")`. If capacity is available, start the run with the same run ID.
4. If a scheduled run has `ScheduledRunStarted` with no `ScheduledRunSucceeded` and no terminal `ScheduledRunFailed(retry_exhausted: True)` record, treat it as interrupted by daemon restart.
5. Append `ScheduledRunFailed` with reason `daemon_restart`, `retry_exhausted: false`, and the last known run root if available.
6. Schedule a retry for the same due interval if the next attempt is within `agent.max_retry_attempts`; otherwise append a terminal failure and trigger failure reporting.
7. If a `ScheduledRunRetryScheduled` has no cancel, subsequent start, or terminal record, restore its retry timer. If its target time has already passed, retry soon; otherwise schedule the remaining delay.
8. If a `ScheduledFailureReportFailed` has no later `ScheduledFailureReported` for the same run and generation, restore the report retry. This retry performs only Linear reporting; it never reruns the workflow.
9. If no unfinished scheduled state exists and `catch_up: false`, skip missed automatic boundaries as described above and set the next due after startup time.

This is at-least-once recovery. If a worker completed useful external work but the daemon died before recording success, Scherzo may retry the same due interval. Workflow authors must make recurring maintenance workflows idempotent or safe to repeat. The example prompt and rollout docs must say this explicitly.

### Overlap behavior

The MVP supports only `overlap: skip`. The overlap check is per job and includes pending runs, active runs, and retry-waiting runs. If a job becomes due while the same job has any of those states, append `ScheduledJobSkipped` with reason `overlap_running` unless the current block is specifically `waiting_for_global_slot` or `paused`, in which case use that reason. Increment the relevant skipped count and advance `next_due_at_ms` to the next boundary after the observed time. Do not queue the skipped interval.

Manual `run --now` requests are stricter than automatic due intervals. If the same job has a pending, active, or retry-waiting run, the command returns `rejected` with reason `overlap_running` and appends no manual due record. This avoids creating an operator-requested run that cannot start because the MVP has no queue policy beyond one pending run per job.

Overlaps are per job, not per workflow. Two different jobs may point at the same workflow and can run concurrently if global concurrency allows. This is acceptable because the MVP has no schedule-level vars; if two schedules need different behavior, they should use distinct workflows or scripts.

### Catch-up behavior

The MVP default is `catch_up: false`, and `catch_up: true` is rejected. When the daemon was stopped, paused, capacity-blocked, retrying, or active through one or more intervals, Scherzo records summarized skipped events and resumes at the next future boundary. It never runs a burst of historical intervals.

Manual runs do not count as catch-up and do not move automatic next due time.

### Global concurrency and pause

Scheduled dispatch must respect `agent.max_concurrent_agents`. The count used for admission is:

    active issue workers
    + pending issue claims
    + active scheduled workers
    + pending scheduled starts

If the count is at or above the limit, a due scheduled run is not skipped. It remains durably pending with status `waiting_for_global_slot` until a later daemon tick, worker finish, or retry timer observes available capacity. Later automatic boundaries for the same job are skipped and counted while the original pending run waits.

When dispatch is paused through `scherzoctl pause`, do not start automatic scheduled jobs. Automatic due intervals become or remain durably pending with status `paused`. Manual `scherzoctl schedules run <job> --now` is rejected while paused; the MVP has no force flag. Existing running scheduled workers can still be aborted with existing session controls.

Per-state concurrency limits in `agent.max_concurrent_agents_by_state` apply only to Linear issue states. Scheduled jobs have no Linear state and should not consume any per-state bucket. They consume only the global limit.

### Retry behavior

A scheduled due interval starts at attempt `1`. If the workflow returns a retryable failure, schedule a retry for the same due interval using the existing backoff curve from `core.backoff_delay(attempt, agent.max_retry_backoff_ms)`. The hard maximum number of attempts is `agent.max_retry_attempts`. Do not add schedule-level retry config in the MVP.

Retry attempts use the same logical run ID and due time, increment `run.attempt` in the template context, and create separate scheduled start/failure records. A retry does not create a new Linear issue. Linear failure reporting happens only after the final allowed attempt fails, unless the workflow returns a needs-human outcome that should bypass retry because retrying cannot help.

If the daemon restarts with a scheduled retry timer pending, recover the remaining delay from ledger the same way issue retries are recovered: if due time has already passed, retry soon; otherwise schedule the remaining delay.

### Needs-human outcome

Current agent final classifications are `FinalActive`, `FinalTerminal`, and `FinalNonActive` in `src/scherzo/agent/types.gleam`. Preserve current issue-run behavior. For scheduled runs, use this exact mapping:

- `FinalTerminal`: `ScheduledRunSucceeded`; silent success; no Linear success call.
- `FinalActive`: `ScheduledRunFailed` with reason `needs_human`, `retry_exhausted: True`; do not retry; report to Linear if configured.
- `FinalNonActive`: `ScheduledRunFailed` with reason `needs_human`, `retry_exhausted: True`; do not retry; report to Linear if configured.

If the scheduled workflow runner can avoid producing `FinalActive` or `FinalNonActive`, still keep tests with a fake runner that returns each constructor. The tests must prove issue-triggered behavior is unchanged and scheduled non-terminal classifications enter the needs-human reporting path.


## Failure Reporting Model

Add a scheduled failure reporter to `src/scherzo/linear_triage.gleam` or a new adjacent module `src/scherzo/scheduled_failure_reporter.gleam`. The daemon dependency type should gain a reporter factory in `RuntimeDependencies`, similar to `make_triage`, so tests can inject a fake reporter.

The MVP supports only `open_issue_per_job`. The dedupe key is:

    scheduled-job:<job-id>

The reporter must make this key first-class in both Linear and the ledger. It always applies these reserved labels, even when `on_failure.linear.labels` is empty:

    scherzo:scheduled
    scherzo:scheduled-job:<job-id>

It also includes this marker in the issue body and in update comments:

    <!-- scherzo-dedupe: scheduled-job:<job-id> -->

Configured `on_failure.linear.labels` are additional labels only. They are not the dedupe mechanism and must not be required for dedupe to work.

When a scheduled due interval terminally fails after retries are exhausted, or reaches a needs-human outcome, and `on_failure.linear.enabled` is true:

1. Build the dedupe key and reserved labels from the job ID.
2. Ensure the reserved labels exist for the configured Linear team or project. If the Linear API cannot create or verify those labels, append `ScheduledFailureReportFailed` and do not create an unlabeled issue.
3. Prefer the locally remembered `linear_issue_id` from the latest `ScheduledFailureReported` for the same dedupe key if it still exists and is not in a terminal state.
4. If local state does not know an open issue, query Linear for open issues in the configured project that have both reserved labels. If exactly one exists, update it. If more than one exists, update the most recently updated matching issue and include a comment noting that duplicate dedupe labels were found; do not create another issue. If none exists, create a new issue.
5. When updating an open issue, add a comment with this failure occurrence and set it to the configured triage state if needed. Record `ScheduledFailureReported` with `action: "updated"`.
6. When creating a new issue, use the configured triage state, apply reserved labels plus configured labels, include the stable body marker, and record `ScheduledFailureReported` with `action: "created"`.
7. If any report attempt fails after a terminal scheduled failure, append `ScheduledFailureReportFailed` with `next_retry_at_ms` and `generation`. Startup recovery and later ticks retry the reporting side effect until `ScheduledFailureReported` exists for that run and generation. They never rerun the workflow solely because reporting failed.

The failure issue title should be stable and human-readable:

    Scherzo scheduled job failed: pr-conflict-repair

The body or comment for each terminal failure should include:

- scheduled job ID;
- workflow ID;
- due time in UTC ISO-8601;
- run ID;
- attempt count and max attempts;
- failure summary, including command failure details when available;
- token total and turn count if known;
- run root and retained workspace path if available;
- relevant local artifact references from the workflow result and step artifacts;
- latest scheduled session ID if known;
- dedupe key and current failure issue ID when updating;
- reserved dedupe labels;
- suggested local commands:

    scherzoctl schedules status pr-conflict-repair
    scherzoctl schedules history pr-conflict-repair
    scherzoctl schedules logs pr-conflict-repair --last
    scherzoctl attach <scheduled-session-id>
    scherzoctl schedules run pr-conflict-repair --now

Successful scheduled runs do not create Linear issues, comments, or state transitions. If a previous failure issue remains open, later successes remain local. The operator can close the failure issue manually after reviewing local diagnostics.


## Local Diagnostics and Operator UX

Add a `schedules` command group to `scherzoctl` through `src/scherzo/ctl.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/control/command.gleam`, `src/scherzo/control/client.gleam`, `src/scherzo/control/server.gleam`, and daemon control handling.

The MVP commands are:

    scherzoctl schedules status
    scherzoctl schedules status pr-conflict-repair
    scherzoctl schedules history pr-conflict-repair
    scherzoctl schedules logs pr-conflict-repair --last
    scherzoctl schedules run pr-conflict-repair --now
    scherzoctl schedules doctor pr-conflict-repair

`status` is the minimum operator dashboard. It should show one row per job by default:

    JOB                 ENABLED  WORKFLOW            STATUS  NEXT DUE              LAST SUCCESS          LAST FAILURE          SKIPPED  RECENT RUNS
    pr-conflict-repair  true     pr-conflict-repair  idle    2026-05-05T12:15:00Z  2026-05-05T12:00:42Z  -                     0        schedule-pr-conflict-repair-20260505T120000Z

For a single job, print detailed fields:

    job: pr-conflict-repair
    workflow: pr-conflict-repair
    enabled: true
    status: idle
    every_ms: 900000
    overlap: skip
    catch_up: false
    next_due_at: 2026-05-05T12:15:00Z
    last_success_at: 2026-05-05T12:00:42Z
    last_success_run_id: schedule-pr-conflict-repair-20260505T120000Z
    last_failure_at: -
    skipped_overlap_count: 0
    skipped_catch_up_count: 0
    skipped_paused_count: 0
    skipped_capacity_count: 0
    pending_run: -
    active_run: -
    failure_issue_id: -
    recent_run_ids: schedule-pr-conflict-repair-20260505T120000Z

`history <job>` prints recent due intervals in reverse chronological order. Include due time, run ID, trigger, attempts, terminal status, pending or blocked status, failure summary if any, skipped reason and count if skipped, and failure issue ID if reported. Add `--json` support using the existing JSON output conventions.

`logs <job> --last` resolves the latest scheduled session ID for the job and prints the same human-readable event replay as `scherzoctl events --pretty <session-id>`. If the latest session events are no longer retained, print the run ID, run root, artifact paths, and a message saying the session transcript has expired from the local event hub.

`run <job> --now` enqueues a manual run for the job. It must validate that the job exists, is enabled, has no pending, active, or retry-waiting run for the same job, and dispatch is not paused. If global capacity is available, it appends manual due/pending/start records and returns `started`. If global capacity is full, it appends durable manual due and pending records, marks the pending run `waiting_for_global_slot`, and returns `queued`. If the daemon restarts before that queued manual run starts, startup recovery preserves it under the same run ID. Manual runs use trigger `manual` and do not shift the automatic next due boundary.

`doctor [job]` validates schedule config and template context. It can run online through the daemon for current loaded config or offline by loading `scherzo.yaml` if no control file is present. The minimum checks are: job exists, enabled workflow exists, interval is valid, unsupported fields are absent, Linear failure config is complete when enabled, reserved dedupe labels can be created or found when Linear reporting is enabled, and the scheduled workflow does not reference `issue.*`.

## Worked Example: PR Conflict Repair Every 15 Minutes

Add this example to `examples/scherzo.yaml` after `routing.workflows` once implementation is complete:

    routing:
      workflow_label_prefix: "workflow:"
      require_exactly_one_workflow_label: true
      workflows:
        research: workflows/research.yaml
        implementation: workflows/implementation.yaml
        pr-conflict-repair: workflows/pr-conflict-repair.yaml

    scheduled_jobs:
      - id: pr-conflict-repair
        workflow: pr-conflict-repair
        enabled: true
        every: 15m
        overlap: skip
        catch_up: false
        on_failure:
          linear:
            enabled: true
            state: Triage
            labels:
              - scherzo:scheduled
              - job:pr-conflict-repair
            dedupe: open_issue_per_job

The workflow file `workflows/pr-conflict-repair.yaml` should keep work details out of the schedule:

    version: 1
    id: pr-conflict-repair
    description: Detect and repair PR merge conflicts on a fixed cadence.
    max_parallel_steps: 1
    steps:
      - id: inspect
        kind: command
        run: ./scripts/pr-conflict-repair-inspect.sh
        timeout_ms: 300000
      - id: repair
        kind: agent
        depends_on: [inspect]
        prompt: prompts/pr-conflict-repair.md

The prompt file should use scheduled context variables, not issue variables:

    You are running Scherzo scheduled job {{ scheduled_job.id }}.

    Workflow: {{ scheduled_job.workflow }}
    Due at: {{ schedule.due_at }}
    Started at: {{ schedule.started_at }}
    Run ID: {{ run.id }}
    Attempt: {{ run.attempt }}

    Inspect the repository and repair merge conflicts reported by the inspect step.
    This job may be retried for the same due interval, so make all changes idempotent.
    If there is no conflict to repair, report success concisely and stop.

At due time `2026-05-05T12:00:00Z`, the automatic logical run ID is:

    schedule-pr-conflict-repair-20260505T120000Z

If the inspect command fails on all retry attempts, Scherzo records local failure history and creates or updates one Linear issue titled:

    Scherzo scheduled job failed: pr-conflict-repair

If the 12:15 run succeeds, Scherzo records local success only. It does not create or update Linear.


## Milestones

Milestone 1 adds schedule config parsing and validation without running anything. At the end, `scherzo.yaml` can contain `scheduled_jobs`, invalid intervals and forbidden `vars` fields are rejected, reserved failure-reporting labels are known, and a loaded `RuntimeBundle` exposes scheduled job config. This milestone proves the schema and avoids touching daemon lifecycle first.

Milestone 2 adds scheduled template context and workflow compatibility validation. At the end, a scheduled prompt can render `scheduled_job.id`, `schedule.due_at`, and `run.id`, while a scheduled workflow that references `issue.identifier` fails doctor/config validation. This retires the biggest semantic mismatch between issue workflows and scheduled workflows.

Milestone 3 adds local scheduled ledger records, projection, and pure schedule core. At the end, tests can fold records into status showing next due, pending state, active state, last success, last failure, skipped counts by reason, retry attempts, report retry status, and failure issue ID. Pure tests also cover delayed ticks, pause, capacity waits, manual queueing, catch-up-disabled skips, and `next_due_at_ms` advancement before worker processes are involved.

Milestone 4 adapts workspace and workflow execution for scheduled invocations. At the end, scheduled runs have issue-free workspace paths, scheduled hook environment variables, command and agent steps can render scheduled variables, and issue-triggered workflow execution still behaves exactly as before.

Milestone 5 integrates the local scheduler into the daemon and adds the first local operator controls: `scherzoctl schedules status`, `history`, and `run --now`. At the end, due jobs start workflow workers, respect global pause and concurrency, skip overlaps, queue manual runs durably when capacity is full, retry failed due intervals, recover pending and active scheduled runs on startup, and expose local status/history. This is the local-only releasable slice.

Milestone 6 adds Linear failure reporting. This milestone must not begin until Milestone 5's delayed-tick, pause, capacity, manual-queue, and recovery tests are green. At the end, a terminal failed scheduled run creates or updates one deduped Linear triage issue, report failures retry durably, and successful intervals remain silent.

Milestone 7 adds remaining operator UX, examples, and end-to-end validation. At the end, `scherzoctl schedules logs --last` and `doctor` work, `examples/scherzo.yaml` includes the PR conflict repair schedule, and the smoke-test recipe demonstrates local success and failure behavior.


## Plan of Work

In `src/scherzo/config/types.gleam`, add scheduled job config types and a `scheduled_jobs` field to `OrchestratorConfig`. Keep the field out of `EffectiveConfig`.

In `src/scherzo/config.gleam`, add `default_scheduled_failure_config`, `resolve_scheduled_jobs`, duration parsing, unsupported-field detection, job ID validation, dedupe parsing, and failure config parsing. Call `resolve_scheduled_jobs` from `resolve_orchestrator_root` after routing is resolved so job workflow IDs can be checked against `routing.workflows`. Treat reserved Linear labels as generated reporter labels, not user-configured labels.

In `src/scherzo/runtime_bundle.gleam`, after workflow DAGs are loaded, validate enabled scheduled jobs against the loaded workflow map. Add helper functions that find scheduled workflow issue-context references by scanning agent prompt strings and command run strings with `template.referenced_variables`.

In `src/scherzo/template.gleam`, introduce `InvocationContext`, `ScheduledTemplateContext`, scheduled render wrappers, and `referenced_variables`. Preserve existing issue render APIs and tests. Add tests for scheduled variables, unknown `issue.*` in scheduled context, and variable reference scanning in variables, if tags, and for tags.

In `src/scherzo/workflow_run.gleam`, introduce a narrow invocation type for workflow execution. Preserve `execute` for issue runs by wrapping the issue in `IssueInvocation`. Add `execute_scheduled` or `execute_with_invocation` for scheduled runs. Ensure command steps, agent steps, locals, final classifications, and `StepContext` carry scheduled fields. Do not pass scheduled templates a fake Linear issue.

In `src/scherzo/workspace_run.gleam`, add scheduled workspace path functions. The scheduled run root should be under the configured workspace root with this shape:

    <workspace-root>/<workflow-id>/scheduled/<job-id>/<run-id>

Workspace paths remain under:

    <run-root>/workspaces/<workspace-name>

Keep existing issue workspace functions unchanged. Add hook environment variables for scheduled runs: `SCHERZO_RUN_KIND=scheduled`, `SCHERZO_SCHEDULED_JOB_ID`, `SCHERZO_SCHEDULE_DUE_AT`, `SCHERZO_SCHEDULE_STARTED_AT`, `SCHERZO_RUN_ID`, and `SCHERZO_RUN_ATTEMPT`. Existing `SCHERZO_ISSUE_*` variables should be empty for scheduled runs.

In `src/scherzo/state/record.gleam`, add scheduled record variants, JSON encoding, decoding, redaction behavior, and `kind` names. Include durable pre-start records and report retry records: `ScheduledRunPending`, `ScheduledRunPendingBlocked`, `ScheduledRunPendingCancelled`, and `ScheduledFailureReportFailed`.

In `src/scherzo/state/projection.gleam`, add scheduled job status data structures and fold logic. Expose functions for status, recent history, active scheduled runs, pending scheduled runs, blocked reasons, pending retries, last failure issue ID, open failure dedupe state, and pending failure report retries.

In `src/scherzo/state/recovery.gleam`, extend recovery planning to include scheduled jobs. Recover unfinished scheduled runs as failed with reason `daemon_restart`, restore scheduled retry timers, restore pending automatic and manual runs, cancel pending runs whose job is disabled or whose workflow disappeared, summarize catch-up-disabled skips, and expose scheduled retry/report timers to daemon startup.

Add `src/scherzo/orchestrator/schedule_core.gleam` as a pure module for due calculation, run ID creation, state transitions, skip summarization, pending admission, retry decisions, and next-due advancement. Keep this module free of process spawning and Linear side effects so tests can cover all edge cases cheaply.

In `src/scherzo/orchestrator/state.gleam`, add scheduled runtime fields for active scheduled workers, pending scheduled starts, pending scheduled retries, pending failure report retries, and per-job counters. Do not mix scheduled runs into existing issue `running` maps that are refreshed from Linear.

In `src/scherzo/orchestrator/worker_registry.gleam`, add scheduled worker registration and lookup functions without changing existing issue worker behavior. Scheduled workers need job ID, workflow ID, run ID, pid, monitor, run root, session ID, attempt, and command subject. Existing session controls should be able to resolve scheduled session IDs.

In `src/scherzo/orchestrator/daemon.gleam`, add messages for scheduled retry ticks, scheduled worker finished, scheduled manual run requests, schedule status/history requests, and scheduled failure-report retry ticks. Evaluate schedule due work on each poll tick after config reload and before Linear candidate fetch. Dispatch scheduled workers through the same workflow runner dependencies but using scheduled invocation context. Update global dispatch slot calculations to include active and pending scheduled work.

In `src/scherzo/orchestrator/effect_runner.gleam`, add any asynchronous effects needed for scheduled Linear failure reporting and scheduled reporter retries. Keep file and network effects out of pure schedule core.

In `src/scherzo/linear_triage.gleam` or new `src/scherzo/scheduled_failure_reporter.gleam`, implement `open_issue_per_job` failure reporting. Reuse `src/scherzo/linear.gleam` transport helpers where possible. Always enforce reserved labels and the body marker. Add fake-client support through daemon dependencies.

In `src/scherzo/control/command.gleam`, add operator command variants for schedule manual run and schedule doctor if they mutate or query daemon state. Query-only protocol requests can live directly in `control/protocol.gleam` if that matches existing conventions.

In `src/scherzo/control/protocol.gleam`, add request/response types and JSON encoders/decoders for schedule status, schedule history, schedule logs lookup, schedule run now, and schedule doctor.

In `src/scherzo/ctl.gleam`, add parsing, usage text, pretty printing, and JSON output for `schedules` subcommands. Reuse existing `events --pretty` rendering for `schedules logs --last` after resolving the latest scheduled session ID.

In `src/scherzo/doctor.gleam`, add a scheduled job check or extend `WorkflowConfig` output to include schedule validation. The human remediation should explain that scheduled workflows cannot reference `issue.*`, schedule-level `vars` are intentionally deferred, and reserved Linear dedupe labels must be available when reporting is enabled.

In `examples/scherzo.yaml`, add the documented `scheduled_jobs` example only after code and tests are in place.


## Concrete Steps

1. From the repository root, run `direnv exec . gleam test --target erlang test/config_test.gleam` to confirm the current config tests pass. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command through direnv.

2. Add scheduled config tests in `test/config_test.gleam`: valid `scheduled_jobs`, defaults, invalid durations, unsupported fields, unsupported `catch_up: true`, unsupported `overlap: queue`, unknown workflow, and optional extra Linear labels.

3. In `src/scherzo/config/types.gleam`, add `ScheduledOverlap`, `ScheduledFailureDedupe`, `ScheduledLinearFailureConfig`, `ScheduledFailureConfig`, `ScheduledJobConfig`, and `scheduled_jobs` on `OrchestratorConfig`.

4. In `src/scherzo/config.gleam`, implement schedule parsing, duration parsing, unsupported field detection, defaults, and config error codes.

5. Run `direnv exec . gleam test --target erlang test/config_test.gleam` and expect the new config tests to pass.

6. Commit milestone 1 after the targeted config test passes. Suggested commit message: `Add scheduled job config parsing`.

7. Add scheduled template tests in `test/template_test.gleam`: render scheduled variables; fail scheduled render for `{{ issue.identifier }}`; scan references from `{{ issue.title }}`, `{% if issue.description %}`, and `{% for label in issue.labels %}`.

8. Implement `InvocationContext`, `ScheduledTemplateContext`, scheduled render wrappers, and `referenced_variables` in `src/scherzo/template.gleam` while preserving existing issue render wrappers.

9. Run `direnv exec . gleam test --target erlang test/template_test.gleam` and expect template tests to pass.

10. Add runtime bundle or doctor tests in `test/doctor_test.gleam` or a new focused test that loads a scheduled workflow with `{{ issue.identifier }}` and asserts `scheduled_workflow_requires_issue_context` with job ID, workflow ID, step ID, and variable in the message.

11. Implement scheduled workflow validation in `src/scherzo/runtime_bundle.gleam` and diagnostic text in `src/scherzo/doctor.gleam`.

12. Rerun `direnv exec . gleam test --target erlang test/template_test.gleam test/doctor_test.gleam` and expect the new tests to pass.

13. Commit milestone 2. Suggested commit message: `Add scheduled workflow template context`.

14. Add scheduled ledger record tests in `test/state_record_test.gleam` for encode/decode round trips of every new scheduled record kind, including pending, blocked, pending-cancelled, and report-failed records.

15. Add projection tests in `test/state_projection_test.gleam` for due, pending, blocked by pause, blocked by capacity, skipped by each reason, started, succeeded, failed, retry scheduled/cancelled, failure reported, and failure report failed records.

16. Implement scheduled record variants in `src/scherzo/state/record.gleam` and projection updates in `src/scherzo/state/projection.gleam`.

17. Run `direnv exec . gleam test --target erlang test/state_record_test.gleam test/state_projection_test.gleam` and expect the new tests to pass.

18. Add `src/scherzo/orchestrator/schedule_core.gleam` and tests in `test/orchestrator_schedule_core_test.gleam` for duration boundary calculation, first next due after startup, deterministic run IDs, manual run IDs, retry exhaustion, and no immediate startup run.

19. Add schedule-core state-machine tests for these exact scenarios: delayed tick with three missed boundaries admits only the latest due and records one `catch_up_disabled` skip; active run across two boundaries records one `overlap_running` skip with `skipped_count: 2`; paused run across two boundaries records one `schedule_paused` skip; global capacity wait across two boundaries records one `waiting_for_global_slot` skip; manual queued run does not advance automatic next due.

20. Run `direnv exec . gleam test --target erlang test/orchestrator_schedule_core_test.gleam` and expect failures until the module is implemented.

21. Implement pure due calculation, run ID creation, skip summarization, pending admission, and next-due advancement in `src/scherzo/orchestrator/schedule_core.gleam`.

22. Rerun `direnv exec . gleam test --target erlang test/orchestrator_schedule_core_test.gleam` and expect all tests to pass.

23. Commit milestone 3. Suggested commit message: `Add scheduled job state ledger and core`.

24. Add workspace path tests in `test/workspace_run_test.gleam` for scheduled run roots and scheduled workspace paths. Assert paths stay under the configured workspace root and contain `scheduled/<job-id>/<run-id>`.

25. Add hook environment tests in `test/workspace_run_test.gleam` or the existing hook test file asserting scheduled hooks receive `SCHERZO_RUN_KIND=scheduled`, `SCHERZO_SCHEDULED_JOB_ID`, `SCHERZO_SCHEDULE_DUE_AT`, `SCHERZO_RUN_ID`, and `SCHERZO_RUN_ATTEMPT`, while `SCHERZO_ISSUE_*` values are empty.

26. Implement scheduled workspace path and hook environment support in `src/scherzo/workspace_run.gleam`.

27. Run `direnv exec . gleam test --target erlang test/workspace_run_test.gleam` and expect tests to pass.

28. Add workflow run tests in `test/workflow_run_test.gleam` for scheduled execution rendering `run.id`, scheduled command-step environment, and no issue variables. Use fake dependencies so no real pi or Linear calls occur.

29. Add workflow run tests that fake scheduled final classifications: `FinalTerminal` becomes success, `FinalActive` becomes `needs_human`, and `FinalNonActive` becomes `needs_human`. Add an issue-run regression test proving existing issue classifications are unchanged.

30. Refactor `src/scherzo/workflow_run.gleam` just enough to accept issue or scheduled invocation. Preserve existing public `execute` for issue runs. Add scheduled wrapper for daemon use.

31. Run `direnv exec . gleam test --target erlang test/workflow_run_test.gleam` and expect existing issue tests plus new scheduled tests to pass.

32. Commit milestone 4. Suggested commit message: `Support scheduled workflow invocation`.

33. Add worker registry tests in `test/orchestrator_worker_registry_test.gleam` for registering, finding, and removing scheduled workers by run ID and session ID without breaking issue worker lookups.

34. Implement scheduled worker registry support in `src/scherzo/orchestrator/worker_registry.gleam`.

35. Run the worker registry tests and expect them to pass.

36. Add orchestrator state tests in `test/orchestrator_state_test.gleam` for adding/removing pending scheduled starts, active scheduled workers, pending retries, and pending report retries, and for global slot accounting that includes pending scheduled starts.

37. Implement scheduled runtime fields in `src/scherzo/orchestrator/state.gleam` without changing existing issue worker maps.

38. Run `direnv exec . gleam test --target erlang test/orchestrator_state_test.gleam` and expect state tests to pass.

39. Add daemon test in `test/orchestrator_daemon_test.gleam` proving a new job does not run immediately on daemon startup; expected records are no `ScheduledJobDue` and status `idle` with the next future boundary.

40. Implement the startup next-due initialization in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/state/recovery.gleam`.

41. Add daemon test proving an idle due job with available capacity appends `ScheduledJobDue`, `ScheduledRunPending`, `ScheduledRunStarted`, and starts a fake scheduled worker.

42. Implement automatic due admission and scheduled worker spawning in `src/scherzo/orchestrator/daemon.gleam`.

43. Add daemon test proving a fake successful scheduled worker appends `ScheduledRunSucceeded`, unregisters the worker, and does not call success handoff or Linear reporting.

44. Implement scheduled worker success handling.

45. Add daemon test proving a retryable failure appends `ScheduledRunFailed(retry_exhausted: False)` and `ScheduledRunRetryScheduled` for the same run ID.

46. Implement scheduled failure handling and retry timer setup.

47. Add daemon test proving retry timer firing starts the same run ID with the next attempt number and cancels or supersedes the retry generation.

48. Implement scheduled retry tick handling.

49. Add daemon test proving final retry exhaustion appends `ScheduledRunFailed(retry_exhausted: True)` and does not report to Linear yet when Milestone 5 uses a fake disabled reporter.

50. Implement retry exhaustion local terminal handling.

51. Add daemon test proving `FinalActive` and `FinalNonActive` scheduled results append `ScheduledRunFailed(reason: "needs_human", retry_exhausted: True)` without retry.

52. Implement the scheduled needs-human mapping in daemon or workflow result handling.

53. Add daemon test proving active same-job overlap records `ScheduledJobSkipped(reason: "overlap_running")` and does not start a second worker.

54. Implement overlap skip handling.

55. Add daemon test proving global capacity full records or preserves pending status `waiting_for_global_slot`, does not start the worker, and starts it later when capacity frees.

56. Implement global-capacity waiting and wake-up on worker finish or tick.

57. Add daemon test proving dispatch pause records or preserves pending status `paused`, skips later boundaries with reason `schedule_paused`, and starts the pending run after resume if capacity is available.

58. Implement pause/resume schedule handling.

59. Add daemon test proving a delayed tick with `every_ms` shorter than `polling.interval_ms` records one `catch_up_disabled` skip and one due for the latest boundary.

60. Implement delayed tick catch-up-disabled summarization in daemon integration.

61. Add daemon test proving `scherzoctl schedules run <job> --now` starts immediately with capacity, queues durably when capacity is full, rejects while paused, rejects disabled jobs, and rejects same-job overlap.

62. Implement scheduled manual run request handling in daemon and the minimum control path needed by the test.

63. Add recovery tests in `test/state_recovery_test.gleam` or `test/orchestrator_daemon_test.gleam`: interrupted started run becomes `daemon_restart` failure and retry; pending automatic run survives restart; pending manual run survives restart; pending run for disabled job is cancelled; pending failure report retry is restored.

64. Implement scheduled startup recovery in `src/scherzo/state/recovery.gleam` and daemon startup wiring.

65. Add control/protocol/CLI tests for the local slice: `schedules status`, `schedules status <job>`, `schedules history <job>`, and `schedules run <job> --now` parse and round-trip, including JSON output.

66. Implement the local schedule control protocol, daemon handling, and `scherzoctl` pretty/JSON output for status, history, and run-now.

67. Run `direnv exec . gleam test --target erlang test/orchestrator_worker_registry_test.gleam test/orchestrator_state_test.gleam test/orchestrator_daemon_test.gleam test/state_recovery_test.gleam test/control_protocol_test.gleam test/ctl_test.gleam` and expect local scheduler tests to pass.

68. Commit milestone 5. Suggested commit message: `Dispatch scheduled workflow runs locally`.

69. Add failure reporter tests in `test/scheduled_failure_reporter_test.gleam` or `test/linear_triage_test.gleam`: creates new issue when none exists, updates existing open issue from local remembered ID, finds existing issue by reserved labels when local state is absent, applies reserved and configured labels, includes body marker, handles multiple matching issues without creating a duplicate, records report failure when labels cannot be verified, and does nothing when reporting disabled.

70. Implement scheduled failure reporter and fake dependency wiring.

71. Add daemon tests asserting no Linear report before retries are exhausted, exactly one report after final failure, a report failure appends `ScheduledFailureReportFailed`, and startup/tick retries the report without rerunning the workflow.

72. Implement daemon scheduled failure-report effects and report retry handling.

73. Run targeted tests for failure reporter and daemon failure paths.

74. Commit milestone 6. Suggested commit message: `Report scheduled failures to Linear triage`.

75. Add CLI parse tests in `test/ctl_test.gleam` for `schedules logs <job> --last` and `schedules doctor <job>` plus JSON options.

76. Add protocol and control-server tests for schedule logs lookup and schedule doctor responses.

77. Implement logs lookup, doctor protocol, control server handling, and `scherzoctl` printing.

78. Update `examples/scherzo.yaml` with the PR conflict repair schedule example. Do not add schedule-level inputs.

79. Add a local smoke integration test in `test/scheduled_smoke_test.gleam` or the nearest existing integration test file that uses fake tracker/reporter dependencies and files under `tmp/scheduled-smoke/` to exercise a command-only scheduled workflow success and a forced command failure.

80. Run the focused suite:

    direnv exec . gleam test --target erlang test/config_test.gleam test/template_test.gleam test/workflow_run_test.gleam test/workspace_run_test.gleam test/state_record_test.gleam test/state_projection_test.gleam test/orchestrator_schedule_core_test.gleam test/orchestrator_worker_registry_test.gleam test/orchestrator_state_test.gleam test/orchestrator_daemon_test.gleam test/state_recovery_test.gleam test/scheduled_failure_reporter_test.gleam test/control_protocol_test.gleam test/control_server_test.gleam test/ctl_test.gleam test/doctor_test.gleam test/scheduled_smoke_test.gleam

81. Run formatting and the full test suite:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

82. Commit milestone 7 after the focused suite, format check, and full test suite pass. Suggested commit message: `Add scheduled job operator UX`.


## Testing and Falsifiability

The implementation is wrong if any of these tests cannot be made to pass without weakening the assertions.

Config tests in `test/config_test.gleam` must assert:

- A valid `scheduled_jobs` block resolves to one `ScheduledJobConfig` with `every_ms` of `900000` for `15m`.
- `enabled`, `overlap`, `catch_up`, and `dedupe` default to `true`, `skip`, `false`, and `open_issue_per_job`.
- `every: 0s`, `every: -1m`, `every: 500ms`, `every: 15`, and `every: 15d` fail with clear config errors.
- `catch_up: true` fails with `scheduled_job_catch_up_unsupported`.
- `overlap: queue` fails with `invalid_scheduled_job_overlap`.
- A scheduled job referencing an unknown workflow fails.
- `vars:`, `input:`, or `payload:` under a scheduled job fails with a message explaining the MVP deferral.
- `on_failure.linear.labels` is optional and is treated as extra labels, while reserved dedupe labels are generated by the reporter.

Template tests in `test/template_test.gleam` must assert:

- Rendering `{{ scheduled_job.id }} {{ schedule.due_at }} {{ run.id }} {{ run.attempt }}` in scheduled context returns expected strings.
- Rendering `{{ issue.identifier }}` in scheduled context returns a template error with `unknown variable issue.identifier` or the chosen scheduled-specific error.
- `referenced_variables` finds variables in interpolation, `if`, and `for` expressions.

Runtime bundle or doctor tests must assert:

- An enabled scheduled job whose prompt contains `{{ issue.title }}` fails validation before dispatch.
- The error names the job ID, workflow ID, step ID, and variable.
- A disabled scheduled job with an issue-shaped workflow does not dispatch and appears as a warning in doctor rather than blocking the daemon, if the implementation chooses warning behavior for disabled jobs.

Schedule core tests in `test/orchestrator_schedule_core_test.gleam` must assert:

- The next boundary after `2026-05-05T12:00:01Z` for `15m` is `2026-05-05T12:15:00Z`.
- The due boundary at or before `2026-05-05T12:14:59Z` is `2026-05-05T12:00:00Z`.
- The run ID for `pr-conflict-repair` at `2026-05-05T12:00:00Z` is `schedule-pr-conflict-repair-20260505T120000Z`.
- A new job initialized at `2026-05-05T12:00:01Z` does not run immediately and has next due `2026-05-05T12:15:00Z`.
- Delayed evaluation at `2026-05-05T12:46:00Z` with next due `2026-05-05T12:15:00Z` and interval `15m` records one `catch_up_disabled` skip with `skipped_count: 2`, admits only `2026-05-05T12:45:00Z`, and sets the next represented boundary after that due.
- Active overlap across two later boundaries records one `overlap_running` skip with `skipped_count: 2` and does not enqueue another run.
- Pause across two later boundaries records one `schedule_paused` skip with `skipped_count: 2` and keeps the original pending run.
- Global capacity wait across two later boundaries records one `waiting_for_global_slot` skip with `skipped_count: 2` and keeps the original pending run.
- Manual queued runs use `schedule-<job>-manual-<time>` IDs and do not advance automatic next due.
- Retry attempt counts stop at `agent.max_retry_attempts`.

Ledger/projection tests must assert:

- Every scheduled record round-trips through JSON.
- Folding due and pending records sets current state `due_pending`.
- Folding pending blocked records sets current state `paused` or `waiting_for_global_slot`.
- Folding due, started, succeeded records sets last success and recent run ID.
- Folding failed retry records sets last failure reason and retry count.
- Folding skipped records increments skipped counts by reason.
- Folding failure reported records remembers the dedupe key and Linear issue ID.
- Folding failure report failed records exposes `report_retry_waiting` without changing the terminal workflow outcome.
- Old snapshots without scheduled projection data decode with an empty scheduled projection.

Workspace and workflow tests must assert:

- Scheduled run roots use `<workspace-root>/<workflow-id>/scheduled/<job-id>/<run-id>` and stay under the configured workspace root.
- Scheduled command steps and hooks receive `SCHERZO_RUN_KIND=scheduled`, scheduled fields are present, and `SCHERZO_ISSUE_*` variables are empty by design.
- Scheduled execution renders `scheduled_job.*`, `schedule.*`, and `run.*` variables.
- Scheduled execution never fabricates an issue for templates or workspace identity.
- For scheduled runs, `FinalTerminal` is success, while `FinalActive` and `FinalNonActive` become `needs_human` terminal failures with no retry.
- Existing issue-run classification behavior remains unchanged.

Daemon tests must assert:

- A new job does not run immediately on startup; it runs at the next interval boundary.
- A due job starts when dispatch is not paused and global capacity is available.
- A due job appends `ScheduledJobDue`, `ScheduledRunPending`, and `ScheduledRunStarted` in deterministic order before worker execution.
- A due job does not start while paused and exposes status `paused`.
- A due job waits rather than skips when global capacity is full and exposes status `waiting_for_global_slot`.
- Later automatic boundaries while paused, capacity-blocked, active, or retrying are summarized with the correct skip reason and count.
- A due job skips with reason `overlap_running` when the same job is already active, pending, or retry-waiting.
- A manual run starts immediately with capacity, queues durably when capacity is full, is rejected while paused, is rejected for disabled jobs, and is rejected for same-job overlap.
- A failed attempt schedules a retry for the same due run ID.
- A retry timer starts the same run ID with the next attempt number after restart or normal timer fire.
- Linear failure reporting is not called before retries are exhausted.
- Linear failure reporting is called once after final failure when reporting is enabled.
- If Linear reporting fails, `ScheduledFailureReportFailed` is appended and a later tick retries reporting without rerunning the workflow.
- A successful scheduled run does not call success handoff or Linear reporting.
- Startup recovery of an unfinished scheduled run records interruption and retries at least once.
- Startup recovery preserves pending automatic and manual runs that have not started.
- Startup recovery cancels pending runs whose job is disabled or whose workflow disappeared.

Failure reporter tests must assert:

- With no open issue, a terminal failure creates a Linear issue with the expected title, state, reserved labels, configured labels, body marker, and body fields.
- With an existing open issue remembered locally for `scheduled-job:<job-id>`, the reporter updates/comments instead of creating a second issue.
- With no local remembered issue, the reporter finds an existing open issue by reserved labels and updates it instead of creating a second issue.
- If multiple open issues have the reserved labels, the reporter updates one existing issue and does not create a new one.
- If reserved labels cannot be verified or created, the reporter records a report failure and does not create an unlabeled issue.
- The body includes scheduled job ID, workflow ID, due time, run ID, failure summary, retry count, run root or retained workspace path, artifact/log references, suggested `scherzoctl` commands, dedupe key, reserved labels, and body marker.
- Reporting disabled is a no-op.
- Successful scheduled runs do not call the reporter.

CLI and protocol tests must assert:

- `scherzoctl schedules status`, `status <job>`, `history <job>`, `logs <job> --last`, `run <job> --now`, and `doctor <job>` parse correctly.
- JSON responses decode and include current state, next due, pending run, active run, last success, last failure, skipped counts by reason, recent run IDs, failure issue ID, and report retry status.
- Pretty `status` output includes the required operator fields.
- `history` output distinguishes automatic due, manual due, pending, blocked, active, skipped, retrying, terminal success, terminal failure, and report retry waiting.
- `logs --last` resolves the latest session ID and uses existing event rendering or prints a clear transcript-expired message.

Compatibility tests must assert:

- A config with no `scheduled_jobs` writes no scheduled ledger records.
- A config with `scheduled_jobs: []` writes no scheduled ledger records.
- New code can read old ledgers and snapshots that do not contain scheduled records.
- `scherzoctl state status` reports a clear warning when scheduled records are present and rollback to an older binary may be unsafe.

The non-functional safety claim is that scheduled jobs do not bypass existing global concurrency. Falsify it by a daemon test where `agent.max_concurrent_agents` is `1`, one issue worker is active, and a schedule is due. If the scheduled worker starts before capacity frees, the implementation is wrong.

The silent-success claim is falsified if any success path calls handoff success reporting, creates a Linear issue, comments on a Linear issue, or updates Linear state for a scheduled success.

The `open_issue_per_job` claim is falsified if a second terminal failure creates a second open Linear issue when an open issue with reserved dedupe labels already exists, even when local ledger state has no remembered `linear_issue_id`.


## Validation and Acceptance

After implementation, from the repository root run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expected result: formatting succeeds and all tests pass.

Run a local smoke integration test that avoids real Linear side effects by using fake tracker and reporter dependencies in the test harness:

    direnv exec . gleam test --target erlang test/scheduled_smoke_test.gleam

Expected result: the test creates files only under `tmp/scheduled-smoke/`, starts the scheduler through test dependencies, runs one command-only scheduled success, forces one command failure, and asserts local status/history without creating a real Linear issue.

For manual operator validation with a real daemon, use only a dedicated safe Linear test project or team. If safe credentials are not available, do not run this manual daemon smoke; the local integration test above is the required fallback. Prepare repository-relative smoke files under `tmp/scheduled-smoke/`:

    mkdir -p tmp/scheduled-smoke/workflows tmp/scheduled-smoke/scripts tmp/scheduled-smoke/workspaces
    cat > tmp/scheduled-smoke/scripts/smoke-success.sh <<'EOF'
    #!/usr/bin/env sh
    echo scheduled smoke ok
    EOF
    chmod +x tmp/scheduled-smoke/scripts/smoke-success.sh
    cat > tmp/scheduled-smoke/workflows/smoke-schedule.yaml <<'EOF'
    version: 1
    id: smoke-schedule
    max_parallel_steps: 1
    steps:
      - id: smoke
        kind: command
        run: ./tmp/scheduled-smoke/scripts/smoke-success.sh
        timeout_ms: 30000
    EOF

Create `tmp/scheduled-smoke/scherzo.yaml` by copying the complete required config shape from `examples/scherzo.yaml`, then set these fields for the smoke run: `workspace.root: tmp/scheduled-smoke/workspaces`, a routing entry `smoke-schedule: tmp/scheduled-smoke/workflows/smoke-schedule.yaml`, a single scheduled job with `id: smoke-schedule`, `workflow: smoke-schedule`, `every: 15m`, `overlap: skip`, `catch_up: false`, and `on_failure.linear.enabled: false`. Use a safe Linear test project key because the daemon still initializes the configured tracker even though scheduled successes stay local.

Start the daemon in one terminal:

    LINEAR_API_KEY=<safe-test-linear-api-key> direnv exec . scherzo-start tmp/scheduled-smoke/scherzo.yaml

In another terminal, observe status before the first due boundary:

    direnv exec . scherzoctl schedules status smoke-schedule

Expected result: the job is listed with `enabled: true`, `status: idle`, a future `next_due_at`, no failure issue, no pending run, and no active run.

Force a manual run:

    direnv exec . scherzoctl schedules run smoke-schedule --now
    direnv exec . scherzoctl schedules history smoke-schedule

Expected result: the manual run appears with trigger `manual`, a deterministic manual run ID beginning `schedule-smoke-schedule-manual-`, and a success status. The automatic next due time is unchanged except for normal wall-clock advancement.

Change `tmp/scheduled-smoke/scripts/smoke-success.sh` to exit non-zero and set `agent.max_retry_attempts` to a small value in `tmp/scheduled-smoke/scherzo.yaml`. Keep `on_failure.linear.enabled: false` for the first forced-failure run. Then run:

    direnv exec . scherzoctl reload
    direnv exec . scherzoctl schedules run smoke-schedule --now
    direnv exec . scherzoctl schedules history smoke-schedule

Expected result: history shows attempt 1 failed, retry scheduled, final attempt failed, no Linear issue ID, and local diagnostics include the run root.

Only after local failure behavior is confirmed, enable `on_failure.linear.enabled: true` in the safe test config with `state: Triage`, run another forced failure, and observe:

    direnv exec . scherzoctl schedules history smoke-schedule

Expected result: the history shows failure reported with dedupe key `scheduled-job:smoke-schedule`. Linear contains one open triage issue with reserved labels `scherzo:scheduled` and `scherzo:scheduled-job:smoke-schedule`. Repeating another terminal failure updates the same open issue rather than creating another issue.

Run doctor on a deliberately issue-shaped scheduled prompt or command template:

    direnv exec . scherzoctl schedules doctor smoke-schedule

Expected result: doctor reports `scheduled_workflow_requires_issue_context` and names the offending `issue.*` variable. After replacing the template with scheduled variables, doctor passes.


## Rollout, Recovery, and Idempotence

Roll out this feature disabled by default. Existing configs without `scheduled_jobs` must behave exactly as before. A config with `scheduled_jobs: []` must also behave exactly as before and must not write scheduled ledger records.

The first deployment should use the local-only slice: a command-only no-op or read-only workflow, a conservative interval such as `15m`, and `on_failure.linear.enabled: false` until local status, history, delayed ticks, pause/resume, and recovery look correct. Then enable Linear failure reporting for that job.

Rollback is safe before any scheduled records have been written: remove the `scheduled_jobs` block or set it to `[]`, restart, and use the previous binary if needed. The implementation should keep the current ledger record schema version for additive scheduled record kinds if possible, and should avoid writing scheduled records when no schedules are configured.

If schedules have run, the local ledger may contain scheduled record kinds that older binaries do not understand. In that case, rollback to an older binary against the same state directory is not guaranteed. `scherzoctl state status --root <workspace-root> --json` and the human state status output must warn that scheduled records are present and explain the safe options: keep the new binary, archive old state with the existing state archive command, or reinitialize state only after the operator accepts losing local schedule history.

Recurring agents must be idempotent. Workflow prompts and scripts should assume the same due interval can run more than once after daemon restart, worker crash, or retry. Command steps that mutate external systems should use their own idempotency keys based on `SCHERZO_SCHEDULED_JOB_ID` and `SCHERZO_RUN_ID`.

If a schedule is misconfigured and fails repeatedly, disable it by setting `enabled: false` and run `scherzoctl reload`. The daemon should stop creating new automatic due runs for the job, cancel unstarted pending runs for that disabled job with reason `job_disabled`, and keep local history available. Running attempts can be stopped with `scherzoctl abort <session-ref> --yes`.

If Linear failure reporting is misconfigured, scheduled failures remain visible locally and report attempts are retried from `ScheduledFailureReportFailed`. Operators can inspect `scherzoctl schedules history <job>`, fix `on_failure.linear.state` or Linear label permissions, and either wait for the next report retry or run `scherzoctl schedules run <job> --now` after the underlying workflow issue is fixed.


## Artifacts and Notes

Expected status output shape:

    JOB                 ENABLED  WORKFLOW            STATUS  NEXT DUE              LAST SUCCESS          LAST FAILURE          SKIPPED  RECENT RUNS
    pr-conflict-repair  true     pr-conflict-repair  idle    2026-05-05T12:15:00Z  2026-05-05T12:00:42Z  -                     0        schedule-pr-conflict-repair-20260505T120000Z

Expected failure issue body excerpt:

    <!-- scherzo-dedupe: scheduled-job:pr-conflict-repair -->
    Scheduled job: pr-conflict-repair
    Workflow: pr-conflict-repair
    Due at: 2026-05-05T12:00:00Z
    Run ID: schedule-pr-conflict-repair-20260505T120000Z
    Attempts: 5 of 5
    Failure: workflow_command_failed:inspect: command exited 1
    Run root: <workspace-root>/pr-conflict-repair/scheduled/pr-conflict-repair/schedule-pr-conflict-repair-20260505T120000Z
    Dedupe key: scheduled-job:pr-conflict-repair
    Reserved labels: scherzo:scheduled, scherzo:scheduled-job:pr-conflict-repair

    Local diagnostics:
      scherzoctl schedules status pr-conflict-repair
      scherzoctl schedules history pr-conflict-repair
      scherzoctl schedules logs pr-conflict-repair --last

Do not include machine-specific absolute paths in tests or docs. Use repository-relative paths in examples and placeholders such as `<workspace-root>` or `<absolute-local-path>` when a path shape must be shown generically.


## Interfaces and Dependencies

No new external service is required beyond the existing Linear tracker configuration. No new package dependency is required for fixed intervals; implement parsing in Gleam.

New or changed public interfaces should include:

In `src/scherzo/config/types.gleam`:

    pub type ScheduledJobConfig
    pub type ScheduledOverlap
    pub type ScheduledFailureConfig
    pub type ScheduledLinearFailureConfig
    pub type ScheduledFailureDedupe

    pub type OrchestratorConfig {
      OrchestratorConfig(
        effective: EffectiveConfig,
        config_dir: String,
        routing: RoutingConfig,
        dag_hooks: DagHooksConfig,
        artifact_limits: ArtifactLimits,
        model_settings: model_config.Settings,
        scheduled_jobs: List(ScheduledJobConfig),
      )
    }

In `src/scherzo/template.gleam`:

    pub type InvocationContext
    pub type ScheduledTemplateContext
    pub fn render_scheduled(String, ScheduledTemplateContext) -> Result(String, error.TemplateError)
    pub fn render_scheduled_with_locals(String, ScheduledTemplateContext, List(#(String, Value))) -> Result(String, error.TemplateError)
    pub fn referenced_variables(String) -> List(String)

In `src/scherzo/workflow_run.gleam`:

    pub type WorkflowInvocation
    pub fn execute_scheduled(
      scheduled: schedule_core.ScheduledRunContext,
      dag: workflow_dag.WorkflowDag,
      orchestrator: config_types.OrchestratorConfig,
      tracker_client: tracker.Client,
      secrets: List(String),
      dependencies: Dependencies,
    ) -> Result(WorkflowRunSuccess, WorkflowRunFailure)

The exact scheduled context type may live in `src/scherzo/orchestrator/schedule_core.gleam` or a neutral `src/scherzo/schedule.gleam` module. Keep it narrow and specific to scheduled jobs.

In `src/scherzo/workspace_run.gleam`:

    pub fn scheduled_run_root_for(
      job_id: String,
      workflow_id: String,
      run_id: String,
      orchestrator: config_types.OrchestratorConfig,
    ) -> Result(String, error.WorkspaceError)

    pub fn scheduled_workspace_path_for_attempt(
      job_id: String,
      workflow_id: String,
      run_id: String,
      step_id: String,
      attempt_index: Int,
      workspace_name: String,
      orchestrator: config_types.OrchestratorConfig,
    ) -> Result(String, error.WorkspaceError)

In `src/scherzo/orchestrator/schedule_core.gleam`:

    pub type ScheduledRunContext
    pub type ScheduleState
    pub type ScheduleDecision
    pub fn parse_every(String) -> Result(Int, String)
    pub fn next_due_after(now_ms: Int, every_ms: Int) -> Int
    pub fn due_at_or_before(now_ms: Int, every_ms: Int) -> Int
    pub fn run_id(job_id: String, due_at_ms: Int) -> String
    pub fn manual_run_id(job_id: String, started_at_ms: Int) -> String
    pub fn retry_delay(attempt: Int, max_backoff_ms: Int) -> Int
    pub fn admit_due_boundaries(state: ScheduleState, now_ms: Int) -> List(ScheduleDecision)
    pub fn account_blocked_boundaries(state: ScheduleState, now_ms: Int, reason: String) -> List(ScheduleDecision)

`ScheduleDecision` should be a pure description of ledger records to append and runtime actions to attempt. It must not spawn processes, read files, or call Linear.

In `src/scherzo/control/protocol.gleam`, add JSON request/response functions for schedule status, history, logs lookup, manual run, and doctor. Keep the protocol version strategy consistent with existing tests. If adding request types is backward-compatible under version `1`, keep version `1`; if not, bump deliberately and update all protocol tests.

In `src/scherzo/scheduled_failure_reporter.gleam` if using a new module:

    pub type FailureReportRequest
    pub type FailureReportResult
    pub fn report_failure(FailureReportRequest, linear.Client) -> Result(FailureReportResult, FailureReportError)

The reporter request must include the dedupe key, reserved labels, configured extra labels, triage state, job ID, workflow ID, due time, run ID, attempt count, failure summary, run root, artifact references, and suggested local commands.

## Open Questions and Clarifications Needed

None.
