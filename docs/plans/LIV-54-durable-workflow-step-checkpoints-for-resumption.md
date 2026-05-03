# Durable workflow step checkpoints for resumption

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo runs workflow DAGs for Linear issues. A workflow DAG is a directed graph of named steps where a downstream step may use artifacts or a workspace produced by an upstream step. Today, if the daemon exits in the middle of a workflow, startup recovery can see an issue-level run, but it cannot reconstruct which workflow steps already finished, which step was running, which workspace path belonged to which logical workspace, or which artifacts are safe to reuse. Operators therefore risk either repeating completed work or losing enough context that the only safe action is to retry the issue from the beginning.

After this change, Scherzo will write durable workflow-run and step-attempt checkpoints to the local ledger. On daemon restart, Scherzo can rebuild the DAG scheduler from those facts, reuse only artifacts that were written safely, mark interrupted attempts explicitly, and continue from the last durable step boundary when the selected workflow definition and issue observation still match. An operator can observe this by starting a multi-step workflow, killing the daemon after one step finishes, restarting it, and seeing the remaining steps continue without rerunning the completed step.

## Problem Framing and Constraints

The operator problem is not merely that a JSONL ledger lacks some fields. The problem is that the current recovery boundary is an entire issue-level worker run, while a workflow run contains smaller units of work that may finish independently. A daemon failure between two workflow steps should not erase the fact that earlier steps finished, and a daemon failure during a step should not make Scherzo pretend that the step completed.

This plan deliberately does not provide backward compatibility for pre-workflow-resumption local durable state. Existing local ledgers and projection snapshots are allowed to be rejected after the schema replacement. The safe operator reset path is to stop the daemon and archive or delete the configured workspace root's `.scherzo-state/ledger` and `.scherzo-state/artifacts` directories before restarting. The implementation must fail closed for old issue-level records: old state must not be silently partially replayed as valid workflow recovery state.

The plan covers durable facts, projection behavior, recovery behavior, step-attempt identity, artifact durability, and tests. It does not implement continuation of an existing pi agent session after daemon restart. It leaves a nullable durable attachment field for a future pi session reference, but recovery must not attempt to attach to or resume a pi session in this work.

## Strategy Overview

The chosen approach is to replace the local ledger schema with a workflow-aware schema and add a small durable artifact store under the configured workspace root's `.scherzo-state` directory. The ledger remains the source of truth for ordering and recovery, while large or bounded step artifacts are written as separate recoverable files. A `step_attempt_finished` ledger record is appended only after its artifact file is written, renamed into place, directory durability has been attempted, the content hash has been verified, and any required `after_step` hook has been handled under the explicit idempotency contract described below.

Each workflow run receives one stable `run_id` before any step starts. Each selected workflow definition is identified by both a logical `workflow_id`, taken from the DAG id, and a deterministic `workflow_fingerprint`, computed from the loaded DAG contents that affect execution. Each step execution receives an `attempt_index` that is monotonically allocated per `run_id` and `step_id`, starting at `1`. The physical `workspace_path` belongs to a specific step attempt and includes the logical workspace name, a collision-resistant step component, and the attempt index. The physical path is never used as the logical identity of the workflow, step, or workspace.

Startup recovery is split into two explicit phases. `src/scherzo/state/recovery.gleam` reads only durable facts and returns recovery candidates. `src/scherzo/orchestrator/daemon.gleam` owns all live dependencies: fetching current issue observations from the tracker, selecting the current workflow DAG through the same path normal dispatch uses, recomputing workflow fingerprints, and deciding whether candidates can resume, must be superseded, or must be left interrupted. This boundary prevents projection-only code from guessing about live Linear state or workflow files.

This is proportionate because it preserves the current append-only ledger model, the current projection-and-recovery startup shape, and the existing workflow runner structure. It avoids introducing a database, a distributed lock, or full process checkpointing. The system only records the stable facts needed to decide whether completed steps can be reused and whether in-flight attempts must be treated as interrupted.

## Alternatives Considered

The simplest plausible alternative is to append a single list of completed step ids to the existing issue-level `RunFinished` or `RunInterrupted` records. That is insufficient because recovery also needs attempt indexes, workspace path identity, artifact references, workflow fingerprinting, failure policy outcomes, and a safe ordering guarantee that artifacts exist before a step is considered finished.

Another alternative is to store all step artifacts inline in the ledger. That would keep recovery single-file, but it would make the ledger grow quickly with command output and agent responses. The current `src/scherzo/step_artifact.gleam` module already caps fields for template use, but even capped command output should not be repeated through every compacted projection snapshot. This plan stores artifacts in `.scherzo-state/artifacts` and keeps only bounded references and hashes in ledger facts.

A third alternative is to require every daemon restart to retry the whole issue from the beginning. That is operationally simple, but it fails the purpose of workflow resumption: multi-step workflows and parallel branches would lose completed work even when the ledger can safely prove that earlier steps finished.

## Risks and Countermeasures

The main risk is silently resuming with the wrong workflow definition or issue contents. The countermeasure is to persist `workflow_id`, `workflow_fingerprint`, `issue_fingerprint`, and the normalized issue update timestamp in `workflow_run_started`; then, during daemon startup, the daemon fetches current issue observations, selects the current workflow by the normal dispatch path, recomputes the fingerprint, and resumes only when workflow identity and issue fingerprint still match. If the tracker is unavailable, startup fails before appending normalization records. If the issue or workflow is unavailable after a successful startup check, recovery fails closed by interrupting or superseding the old run rather than reusing step progress.

Another risk is treating a step as complete when its artifact file was not durably written. The countermeasure is strict write ordering: write the artifact to a temporary path in the final artifact directory, sync the temporary file, rename it to its final artifact path, fsync the artifact directory when the runtime supports it, verify the content hash, and only then append `step_attempt_finished` with `fsync: True`. Recovery must fail closed if a finished record references a missing or hash-mismatched artifact. If directory fsync is not portable on a target, the implementation must record the weaker guarantee in this plan's Surprises & Discoveries and keep the missing-artifact startup error as the recovery behavior.

A third risk is reusing a workspace that was partially mutated by an interrupted step, or colliding two different steps that use the same logical workspace name. The countermeasure is attempt-specific and step-specific workspace paths. A logical workspace name such as `main` is resolved to the workspace path of the latest completed attempt for that logical workspace. A new attempt gets a new physical path containing the logical workspace, a safe step component plus hash, and the attempt index, so interrupted partial output is not mistaken for the clean source for downstream work and two different step ids cannot share a path merely because they both use attempt `1`.

A fourth risk is breaking operators with old local durable state. The countermeasure is an explicit schema replacement and reset story. Version 1 records and snapshots are rejected with a clear startup error. The reset procedure is documented, and tests assert that old issue-level records do not replay silently.

A fifth risk is overbuilding pi continuation. The countermeasure is to keep pi session continuation out of scope. The plan adds an optional `external_session_ref` field to `step_attempt_started` for a future attachment point, but the value remains `None` and recovery never calls pi attach behavior.

A sixth risk is duplicating `after_step` side effects in the crash window after an `after_step` hook succeeds but before `step_attempt_finished` is durable. This plan chooses an explicit at-least-once hook contract rather than claiming exactly-once side effects. The runner must pass both a per-attempt key derived from `run_id`, `step_id`, and `attempt_index`, and a logical hook idempotency key derived from `run_id` and `step_id` that stays stable across retries of the same workflow step. Workflow authors must make external `after_step` side effects idempotent by the logical hook key. Tests must cover the crash-window behavior and assert that recovery does not pretend the original attempt completed unless the finish record is durable.

A seventh risk is leaving top-level workflow runs active forever because only step attempts are terminal. The countermeasure is to wire and test `WorkflowRunFinished`, `WorkflowRunInterrupted`, and `WorkflowRunSuperseded` in every top-level path: normal DAG success, fatal workflow failure, explicit operator stop, worker down, startup restart normalization, and workflow or issue supersession.

## Progress

- [x] (2026-05-03 00:00Z) Read the repo-local ExecPlan authoring guidance in `.pi/skills/exec-plan/SKILL.md`.
- [x] (2026-05-03 00:00Z) Checked source-control state with `jj status --color=never`; the working copy was clean before drafting.
- [x] (2026-05-03 00:00Z) Verified the requested plan filename did not already exist under `docs/plans`.
- [x] (2026-05-03 00:00Z) Read the current workflow and durable-state implementation files named in the Linear issue before drafting this plan.
- [ ] Implement schema version 2 workflow checkpoint records and artifact serialization.
- [ ] Implement projection and snapshot behavior for workflow runs and step attempts.
- [ ] Implement artifact store and write-ordering guarantees.
- [ ] Wire checkpoint writing into daemon dispatch, workflow execution, workspace preparation, and recovery.
- [ ] Add tests for replay, parallel steps, attempt indexes, session ids, artifact ordering, and old-state rejection.
- [ ] Run formatting and test validation, then update this plan's Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: The current workflow runner already executes ready steps concurrently when the scheduler returns a batch, then applies results back in DAG order.
  Evidence: `src/scherzo/workflow_run.gleam` has `run_prepared_batch`, `collect_step_results`, and `apply_prepared_results`; the comment above `execute_prepared_steps` states that ready batches can contain independent steps and that results are applied in DAG order.
- Observation: Current YAML step operator session ids are derived as `run_id <> "-" <> step_id` and do not include an attempt index.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` builds `session_id` that way in both `run_yaml_command_step` and `run_yaml_agent_step`.
- Observation: Current workspace cleanup deletes the workflow run root unless `.scherzo-keep-workspace` is present, so step artifacts stored inside that run root would not survive normal cleanup.
  Evidence: `src/scherzo/workspace_run.gleam` deletes `run_root` in `cleanup_run` unless `cleanup_retention_marker(run_root)` exists.
- Observation: Current ledger and projection schema version is `1`, and the projection snapshot uses the same version.
  Evidence: `src/scherzo/state/record.gleam` defines `schema_version = 1`; `src/scherzo/state/projection.gleam` writes that value in `to_json` and rejects mismatched snapshots in `snapshot_decoder`.

## Decision Log

- Decision: Replace the local durable-state schema with schema version `2` rather than migrating version `1` ledgers in place.
  Rationale: The issue explicitly allows rejecting old local state. Failing closed avoids the dangerous middle ground where issue-level records replay but workflow recovery fields are missing.
  Date: 2026-05-03
- Decision: Store step artifacts outside cleanup-targeted workflow run roots under `.scherzo-state/artifacts`, with hashes referenced by ledger records.
  Rationale: Current cleanup can delete run roots. A `step_attempt_finished` fact is only useful if the artifact it names survives cleanup and restart.
  Date: 2026-05-03
- Decision: Allocate attempt-specific workspace paths and promote only completed attempts as the logical workspace source for downstream steps.
  Rationale: A daemon failure during a step can leave a partially mutated workspace. Reusing that path as if it were clean would corrupt recovery.
  Date: 2026-05-03
- Decision: Keep pi session continuation out of scope, but include `external_session_ref: Option(String)` on started attempts.
  Rationale: The durable model should not block future attachment work, but this change must remain focused on workflow DAG checkpoints and must not claim to resume pi sessions.
  Date: 2026-05-03
- Decision: Split workflow recovery into durable-candidate collection in `src/scherzo/state/recovery.gleam` and live current-state validation in `src/scherzo/orchestrator/daemon.gleam`.
  Rationale: Projection replay cannot fetch Linear issues, select workflows, or read prompt files. Keeping live dependency decisions in the daemon prevents unsafe resume decisions when those dependencies are unavailable or changed.
  Date: 2026-05-03
- Decision: Include a safe step component and step hash in attempt workspace paths.
  Rationale: Attempt indexes are allocated per step. Without a step component, two different steps that share a logical workspace and attempt index could collide on the same physical path.
  Date: 2026-05-03
- Decision: Treat artifact directory fsync as part of the strong durability contract and fail closed on missing or corrupt artifacts.
  Rationale: File rename alone does not guarantee the directory entry survives a crash on all filesystems. If directory sync cannot be supported, startup must still refuse to resume from a finished record whose artifact is absent.
  Date: 2026-05-03
- Decision: Use an at-least-once, idempotency-keyed safety rule for `after_step` hooks rather than claiming exactly-once hook side effects.
  Rationale: A crash can occur after hook success and before the durable finish record. Requiring idempotent hooks by a stable logical step key makes rerun behavior explicit and testable without adding a larger hook transaction system.
  Date: 2026-05-03
- Decision: Make top-level workflow terminal records mandatory in normal, fatal, operator-stop, worker-down, restart, and supersession paths.
  Rationale: Step-attempt facts alone cannot tell startup that a whole workflow run is done or intentionally abandoned. Missing terminal run facts would cause repeated normalization on later restarts.
  Date: 2026-05-03

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo's daemon is the long-running process that polls Linear, claims issues, starts workers, receives worker updates, and performs startup recovery. The relevant daemon implementation is `src/scherzo/orchestrator/daemon.gleam`. It loads durable state with `ledger.replay`, asks `src/scherzo/state/recovery.gleam` for a recovery plan, starts the event hub, schedules recovered retry timers, queues workspace cleanup, and replays pending outbox entries.

The workflow runner is `src/scherzo/workflow_run.gleam`. It receives a `workflow_dag.WorkflowDag`, prepares ready step workspaces through `src/scherzo/workspace_run.gleam`, executes command or agent steps, collects artifacts in memory, marks scheduler state in memory, runs `after_step`, and cleans up the workflow run root at the end.

A logical workflow id is the stable name of a workflow definition. In the current code this is `dag.id`, passed from `workflow_run.execute` into `workspace_run.prepare_step` as `workflow_id`. A run id is one execution of one workflow for one issue. A step id is the id of a step inside the DAG. A workspace name is the logical workspace reference from the DAG. A workspace path is the physical path prepared for one step attempt. These are different identifiers and must not be conflated.

The current workspace preparation module is `src/scherzo/workspace_run.gleam`. It creates a run root under the configured workspace root using sanitized `workflow_id`, issue identifier, and `run_id`, then creates a workspace path under that run root using the logical workspace name. It records `source_workspace_name` and `source_workspace_path` in `PreparedStepWorkspace`, and exposes those values to hooks through environment variables such as `SCHERZO_WORKFLOW_ID`, `SCHERZO_RUN_ID`, `SCHERZO_STEP_ID`, `SCHERZO_WORKSPACE_NAME`, and `SCHERZO_WORKSPACE_PATH`.

The current artifact module is `src/scherzo/step_artifact.gleam`. It defines `StepArtifact` with a status, optional final response, command exit data, stdout, stderr, truncation flags, and `summary_text`. It also converts artifacts into template locals and a final workflow result artifact. Artifacts are currently in memory only during a workflow run.

The durable state modules are `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, and `src/scherzo/state/recovery.gleam`. `record.gleam` encodes and decodes JSONL records. `projection.gleam` folds records into in-memory state and serializes projection snapshots. `ledger.gleam` stores `current.jsonl`, `snapshot.json`, and archive segments under `.scherzo-state/ledger`. `recovery.gleam` turns a projection into runtime state, retry timers, cleanup requests, outbox replays, and warnings.

## Preconditions and Verified Facts

This plan was drafted after reading the current versions of these files: `src/scherzo/workflow_run.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, and `src/scherzo/state/recovery.gleam`. Before implementation starts, re-read those files again and update this section if any named function or type has moved.

The current `record.schema_version` is `1`. Current record kinds include issue-level `run_started`, `run_finished`, `run_interrupted`, `known_workspace`, retry records, issue counter records, parked issue records, Linear command records, and outbox records. None of the current run records names `workflow_id`, `workflow_fingerprint`, `step_id`, `attempt_index`, or an artifact reference.

The current projection stores run state in `runs: Dict(String, RunStatus)`, keyed by `run_id`, where a running run has one `workspace_path`. It does not store a DAG, step attempts, per-step artifacts, workspace names, or attempt indexes.

The current ledger appends JSONL with an optional fsync flag and tolerates a truncated final JSON line by ignoring the tail. It reads `snapshot.json` before folding `current.jsonl`. Any schema replacement must handle both files deliberately so an old snapshot cannot seed a new projection silently.

The current recovery path treats running or interrupted issue-level runs as interrupted worker runs, increments failure counters when appropriate, schedules retries or parks issues, and queues cleanup for terminal issues. It does not resume workflow DAG state.

The current daemon creates a `run_id` during dispatch, reserves a worker session sequence, claims the issue through a side effect, spawns a worker after claim completion, and calls `workflow_run.execute` for YAML workflows. YAML command and agent substeps currently register operator sessions whose ids are `run_id` plus `step_id` without an attempt index.

The current workspace cleanup function in `src/scherzo/workspace_run.gleam` may delete the entire run root. Any artifact needed after restart must be outside that run root, or be safely stored inline in the ledger. This plan chooses external durable artifact files under `.scherzo-state/artifacts`.

## Scope Boundaries

In scope: schema version `2` records and snapshots; workflow run started, finished, interrupted, and superseded behavior; step attempt prepared, started, finished, interrupted, and superseded behavior; attempt index allocation; workflow definition fingerprinting; issue fingerprint or observed update recording; attempt-specific workspace preparation; durable artifact storage; startup recovery that reconstructs scheduler progress; tests for replay, recovery, ordering, identity, and old-state rejection.

Out of scope: migrating existing version `1` local state; attaching to an existing pi session after daemon restart; changing Linear's remote issue model; changing workflow YAML syntax; adding a database; changing the operator control protocol except for exposing unique step session ids that include attempt indexes.

All issue dispatch that participates in this plan is treated as a YAML workflow DAG run. Before implementation changes record semantics, re-read `src/scherzo/orchestrator/daemon.gleam` and confirm there is no remaining non-workflow issue worker path that needs generic issue-level run lifecycle records. If a non-workflow path still exists, do not delete generic run lifecycle facts; instead add schema version `2` generic run records for that path and update this plan before implementing the deletion. Existing retry, parked issue, Linear command, and outbox behavior should stay semantically the same, but their record and snapshot schema version changes to `2`. If a v1 local ledger exists, startup rejects it with a reset message rather than replaying the old issue-level records.

## Schema Compatibility Matrix

Version `1` `run_started`, `run_finished`, and `run_interrupted` records are not decoded as valid schema version `2` records. For workflow-dispatched issues, `workflow_run_started`, `workflow_run_finished`, `workflow_run_interrupted`, and `workflow_run_superseded` replace those facts. For any non-workflow issue execution path discovered during implementation, add schema version `2` generic issue-run records before removing the old constructors.

Version `1` `known_workspace` records are replaced by `step_attempt_prepared` and `step_attempt_finished` workspace fields plus projection helpers that return the latest completed workspace for a logical workspace name. Pending, running, interrupted, or superseded attempts never replace a completed logical workspace source.

Retry records, issue counter records, parked issue records, Linear command records, and outbox records are preserved semantically under schema version `2`. Their encoders, decoders, projection folds, redaction, and snapshot representation must be updated only as needed for schema version `2`; tests must prove existing retry, parking, command acknowledgement, and outbox replay behavior still works.

Projection snapshots are schema version `2` only. A version `1` `snapshot.json` is an unsupported local-state error, not an empty projection and not a truncated-tail condition. Cleanup behavior uses `workflow_run_finished`, `workflow_run_interrupted`, and `workflow_run_superseded` terminal run records plus their recorded `run_root`; it must not rely on the removed `known_workspace` record.

## Durable Record Model and Projection Semantics

Use schema version `2` in `src/scherzo/state/record.gleam`. The implementation may keep the existing `RecordBody` union name, but it must replace the issue-level run records with workflow-aware records and add step-attempt records. All new record kind strings are lowercase snake case. Keep existing retry, counter, parked v2, Linear command, and outbox v2 behavior under schema version `2`; remove or stop decoding schema version `1` issue-level run and workspace records.

Define these workflow run facts:

    WorkflowRunStarted(
      run_id: String,
      workflow_id: String,
      workflow_fingerprint: String,
      issue_id: String,
      issue_identifier: String,
      issue_fingerprint: String,
      observed_updated_at_ms: Int,
      run_root: String,
    )

    WorkflowRunFinished(
      run_id: String,
      workflow_id: String,
      issue_id: String,
      outcome: String,
      token_total: Int,
      turns: Int,
    )

    WorkflowRunInterrupted(
      run_id: String,
      workflow_id: String,
      issue_id: String,
      reason: String,
    )

    WorkflowRunSuperseded(
      run_id: String,
      workflow_id: String,
      issue_id: String,
      superseded_by_run_id: String,
      reason: String,
    )

`WorkflowRunStarted` means Scherzo selected a workflow for an issue and assigned a durable `run_id`. It must be appended with `fsync: True` before any workflow step starts. `workflow_id` is the logical DAG id. `workflow_fingerprint` is a deterministic hash of the loaded workflow definition fields that affect execution. `issue_fingerprint` is `core.issue_fingerprint(issue)` or an equivalent canonical hash of issue fields used for dispatch decisions. `observed_updated_at_ms` is the issue update timestamp normalized to an integer when available, or `0` when the tracker did not provide a timestamp. `run_root` is the root for this workflow run's cleanup-targeted workspaces; it is not the artifact store.

`WorkflowRunFinished` has `outcome` equal to `completed`, `failed_fatal`, or `cancelled`. `completed` means the DAG reached workflow success. `failed_fatal` means a step failed with stop-workflow policy or an after-step failure made the workflow unrecoverable. `cancelled` is for explicit operator stop paths when the implementation can observe them durably. `WorkflowRunInterrupted` means the daemon found a started run that did not finish before shutdown or restart and is not currently resumable. `WorkflowRunSuperseded` means recovery decided not to resume the run because the workflow or issue observation changed, and a later run should handle the issue.

Top-level workflow terminal records are mandatory. `src/scherzo/workflow_run.gleam` must append or request `WorkflowRunFinished(..., outcome: "completed", ...)` before returning normal workflow success and `WorkflowRunFinished(..., outcome: "failed_fatal", ...)` before returning a fatal workflow result. `src/scherzo/orchestrator/daemon.gleam` must append or request `WorkflowRunFinished(..., outcome: "cancelled", ...)` for explicit operator stop paths when cancellation is observed, and `WorkflowRunInterrupted` for worker-down or restart-normalization paths where the workflow did not reach a durable terminal outcome. Startup supersession appends `WorkflowRunSuperseded` before any fresh run for the same issue can reuse dispatch capacity.

Define these step attempt facts:

    StepAttemptPrepared(
      run_id: String,
      workflow_id: String,
      step_id: String,
      attempt_index: Int,
      workspace_name: String,
      workspace_path: String,
      run_root: String,
      source_workspace_name: Option(String),
      source_workspace_path: Option(String),
    )

    StepAttemptStarted(
      run_id: String,
      workflow_id: String,
      step_id: String,
      attempt_index: Int,
      operator_session_id: String,
      external_session_ref: Option(String),
    )

    StepAttemptFinished(
      run_id: String,
      workflow_id: String,
      step_id: String,
      attempt_index: Int,
      outcome: String,
      artifact_ref: String,
      artifact_sha256: String,
      workspace_name: String,
      workspace_path: String,
      token_total: Int,
      turns: Int,
    )

    StepAttemptInterrupted(
      run_id: String,
      workflow_id: String,
      step_id: String,
      attempt_index: Int,
      reason: String,
    )

    StepAttemptSuperseded(
      run_id: String,
      workflow_id: String,
      step_id: String,
      attempt_index: Int,
      superseded_by_attempt_index: Int,
      reason: String,
    )

`StepAttemptPrepared` is the pending state. It means Scherzo allocated an attempt index and prepared a physical workspace path, but has not durably started the command or agent. If the daemon restarts while this is the latest fact for an attempt, recovery marks it interrupted with reason `daemon_restart_before_step_start`; a future attempt receives the next index and a different workspace path.

`StepAttemptStarted` is the running state. It must be appended before the command or agent process is started. `operator_session_id` must be unique for the run, step, and attempt. Its shape is:

    workflow-step-<run_id>-<safe-step-id>-a<attempt_index>-<step-hash>

`safe-step-id` is produced by the same sanitization style used for workspace path components; if sanitization yields an empty string, use `step`. `step-hash` is a short deterministic hash of the original unsanitized step id, at least 12 hex characters. This shape makes retries distinguishable to operators and prevents collisions when different step ids sanitize to the same text. `external_session_ref` is reserved for future pi attachment and is always `None` in this plan.

`StepAttemptFinished` is terminal for the attempt. Its `outcome` is `completed`, `failed_continued`, or `failed_fatal`. A command or agent success records `completed`. A failed step with `ContinueWorkflow` records `failed_continued`; this outcome satisfies downstream dependencies and contributes its artifact to template locals, matching current continue-on-failure semantics. A failed step with stop-workflow policy records `failed_fatal`; this outcome does not satisfy downstream dependencies and leads to `WorkflowRunFinished(..., outcome: "failed_fatal", ...)` after any required after-step handling. If an `after_step` hook is configured, `StepAttemptFinished` is appended only after the hook path has returned according to current workflow semantics; the hook may run more than once across crash recovery and must receive the stable idempotency key described in Artifact Store and Write Ordering.

`StepAttemptInterrupted` is terminal for an attempt but not for the whole run. It means the attempt did not durably finish. It never satisfies downstream dependencies. `StepAttemptSuperseded` is terminal for an older attempt when a newer attempt has been allocated for the same `run_id` and `step_id`. Superseded attempts never satisfy downstream dependencies, even if their earlier state was pending or running.

In `src/scherzo/state/projection.gleam`, add workflow-specific projection state. The exact names may be idiomatic Gleam, but the projection must be able to answer these questions without reading source code outside the projection and artifact store:

- Which workflow runs are active, finished, interrupted, or superseded.
- For a `run_id`, what `workflow_id`, `workflow_fingerprint`, `issue_id`, `issue_identifier`, `issue_fingerprint`, and `run_root` were recorded.
- For each `run_id` and `step_id`, what is the highest allocated `attempt_index`.
- For each step attempt, what is its status: pending, running, completed, failed-continued, failed-fatal, interrupted, or superseded.
- For each completed or failed-continued attempt, what artifact reference and workspace path should be used for recovery.
- For each logical `workspace_name`, which latest completed attempt path is the clean source for downstream steps.

A practical representation is to add `workflow_runs: Dict(String, WorkflowRunStatus)`, `step_attempts: Dict(StepAttemptKey, StepAttemptStatus)`, and derived helper functions such as `next_attempt_index(projection, run_id, step_id)`, `resumable_steps(projection, run_id)`, and `latest_completed_workspace(projection, run_id, workspace_name)`. `StepAttemptKey` may be a custom Gleam type or a carefully encoded string; if it is a string, use an encoding that cannot collide when `step_id` contains punctuation.

Snapshot JSON must include the new workflow run and step attempt projection fields and must use schema version `2`. The snapshot decoder must reject version `1` snapshots explicitly. Do not silently treat a version `1` snapshot as an empty projection, because that could combine old snapshot state with new ledger state in confusing ways.

## Workflow Fingerprinting and Issue Observation

Add `src/scherzo/workflow_fingerprint.gleam` to compute a deterministic fingerprint from a loaded `workflow_dag.WorkflowDag`. The fingerprint must include the DAG id, every step id, dependencies or ordering information used by the scheduler, step kind, command text, command timeout, agent prompt reference or inline prompt text, workspace name, workspace source, failure policy, and model settings. If prompt file contents are available in the loaded runtime bundle, include those contents. If only a prompt file path is available, read the prompt file during fingerprinting and include both the repository-relative prompt path and file contents; fail workflow selection if a referenced prompt file cannot be read for fingerprinting.

The fingerprint must not include volatile values such as generated run ids, physical workspace paths, current time, process ids, or absolute checkout paths. Use a canonical JSON or canonical string representation with sorted keys and stable list order, then hash it with SHA-256. Add a small `src/scherzo/hash.gleam` wrapper if the repository does not already expose SHA-256 helpers. Do not add a new package dependency for hashing; use the Erlang runtime crypto functionality through a narrow FFI wrapper if needed.

On daemon startup, recovery recomputes the workflow fingerprint for the current workflow selected for each issue. If the recorded `workflow_id` and `workflow_fingerprint` match the current selection and the issue fingerprint still matches, the run is eligible for DAG resumption. If either workflow identity or issue fingerprint differs, recovery must not apply old step progress to the new workflow. It appends `WorkflowRunSuperseded` and `StepAttemptSuperseded` records for unfinished attempts, then lets normal dispatch create a fresh run for the issue if the issue remains dispatchable.

The exact daemon/recovery boundary is part of the contract. First, `src/scherzo/state/recovery.gleam` folds the projection and returns `WorkflowRecoveryCandidate` values containing only durable facts: `run_id`, `workflow_id`, recorded fingerprints, issue identity, run root, latest attempt state, and artifact refs. It must not fetch Linear, read prompt files, or choose a current workflow. Second, `src/scherzo/orchestrator/daemon.gleam` uses `fetch_recovery_issue_states` to refresh candidate issue ids. If the tracker fetch fails, `load_startup_recovery` returns a startup error such as `recovery_issue_fetch_failed`, appends no interruption or supersession records, and leaves the ledger unchanged for a later restart. Third, for each successfully fetched issue, the daemon runs the same workflow selection and validation path normal dispatch uses, computes the workflow fingerprint, and builds a `CurrentWorkflowObservation` for recovery. If the tracker succeeds but omits an issue, the observation is `IssueUnavailable`. If workflow selection fails because the workflow definition is missing, invalid, or a prompt file cannot be read for fingerprinting, the observation is `WorkflowUnavailable(reason)`.

Finally, the daemon calls a second recovery function, for example `recovery.finalize_workflow_candidates(projection, candidates, observations, now_ms)`, which returns normalization records and resumable workflow requests. `IssueUnavailable` and `WorkflowUnavailable` produce `WorkflowRunInterrupted` plus `StepAttemptInterrupted` for unfinished attempts; they do not reuse artifacts and do not supersede the run as if a replacement workflow were known. A changed issue fingerprint or changed workflow fingerprint produces `WorkflowRunSuperseded` plus `StepAttemptSuperseded` for unfinished attempts. A full match verifies finished artifacts, returns completed artifacts and logical workspace sources for resumption, and appends `StepAttemptInterrupted` for pending or running attempts so the next attempt index is allocated safely.

## Workspace Preparation Rules

Change `src/scherzo/workspace_run.gleam` so `PreparedStepWorkspace` includes `attempt_index`. Keep existing fields for `workflow_id`, `run_id`, `run_root`, `workspace_name`, `path`, `source_workspace_name`, and `source_workspace_path`.

The physical workspace path must include the logical workspace name, a collision-resistant step component, and the attempt index. A stable, safe layout inside the workflow run root is:

    workspaces / <workspace_name> / steps / <safe-step-id>-<step-hash> / attempt-<attempt_index>

Do not write that conceptual placeholder literally in code. In code, use `path.join` and sanitized path components under the configured workspace root. `safe-step-id` is produced with the same sanitization style used for operator session ids. `step-hash` is at least 12 hex characters from the original unsanitized step id. The important invariants are that two attempts for the same `run_id` and `step_id` never share `workspace_path`, and two different step ids never share `workspace_path` merely because they use the same logical `workspace_name` and the same `attempt_index`. Downstream steps still refer to logical `workspace_name`, not to the step-specific physical path.

`prepare_step` should receive `attempt_index` and a map of completed logical workspaces recovered from finished attempts. When a workflow step has `workspace.from`, use the latest completed attempt path for that source workspace as `source_workspace_path`. Do not use a pending, running, interrupted, or superseded attempt as a source workspace. If the source workspace is not completed, return a workspace preparation error rather than guessing.

After a workspace is successfully prepared and hooks have confirmed the path exists, append `StepAttemptPrepared` with `fsync: True`. If the append fails, do not start the step. Clean up the newly prepared path when it is safe to do so and return a workflow failure that does not pretend the step was durably pending.

## Artifact Store and Write Ordering

Add `src/scherzo/state/artifact_store.gleam`. It stores serialized `step_artifact.StepArtifact` values under the configured workspace root's `.scherzo-state/artifacts` directory, not under a workflow run root. A suggested relative layout within the artifact store is:

    runs/<run_id>/<safe-step-id>-<step-hash>/attempt-<attempt_index>.json

The ledger `artifact_ref` field stores this relative artifact-store reference, not an absolute local path. Recovery resolves it through the configured workspace root. The artifact file contains schema version `2`, `run_id`, `workflow_id`, `step_id`, `attempt_index`, and the serialized `StepArtifact` payload. Add `to_json`, `to_string`, `decode_string`, and equality-friendly helpers to `src/scherzo/step_artifact.gleam` if they do not already exist.

The artifact store durability contract is explicit. The strong contract is: write the final bytes to a temporary file in the same directory as the final artifact; flush and fsync the temporary file; close it; rename it to the final artifact path; fsync the containing artifact directory so the directory entry is durable; read back or hash the final contents; and return the relative ref plus SHA-256 only if the final bytes match. If Gleam or the Erlang runtime cannot provide portable directory fsync in this repository, the implementation must use the strongest available file sync, record the weaker directory-entry guarantee in Surprises & Discoveries, and keep recovery fail-closed for missing artifacts. Do not silently downgrade a missing artifact into a retry of a step whose ledger says it finished.

The write path for a finished step is strict:

1. The command or agent returns a `StepArtifact`.
2. The runner writes the artifact JSON to a temporary file in the artifact store.
3. The runner flushes and fsyncs the temporary file, renames it to the final artifact path, fsyncs the artifact directory when supported, and verifies the SHA-256 hash of final contents.
4. The runner runs `after_step` for the attempt when current semantics require it. It passes `SCHERZO_ATTEMPT_INDEX`, `SCHERZO_ATTEMPT_KEY`, and `SCHERZO_HOOK_IDEMPOTENCY_KEY`. `SCHERZO_ATTEMPT_KEY` is derived from `run_id`, original `step_id`, and `attempt_index` and distinguishes attempts. `SCHERZO_HOOK_IDEMPOTENCY_KEY` is derived from `run_id` and original `step_id` and stays stable across retries of the same workflow step. The safety contract for `after_step` is at-least-once, not exactly-once: a crash after hook success but before `StepAttemptFinished` is durable may rerun the step and hook, so hooks that perform external side effects must dedupe by `SCHERZO_HOOK_IDEMPOTENCY_KEY`.
5. Only after the artifact is recoverable and `after_step` has succeeded or the workflow has recorded the after-step failure policy, the runner appends `StepAttemptFinished` with `artifact_ref` and `artifact_sha256` using `fsync: True`.
6. Only after `StepAttemptFinished` is durable may the in-memory scheduler mark the step as finished and render downstream prompts with that artifact.

If the artifact write fails, do not append `StepAttemptFinished`. The attempt remains pending or running in the projection and will be marked interrupted on recovery. If recovery sees a `StepAttemptFinished` whose artifact file is missing or whose hash does not match, startup fails with a clear `missing_step_artifact` or `corrupt_step_artifact` recovery error. It must not silently rerun the step while also treating the finished record as valid. If recovery sees an attempt that may have run `after_step` but lacks `StepAttemptFinished`, the attempt is interrupted and retried under the at-least-once hook contract; it is not treated as completed.

## Milestones

Milestone 1 replaces the durable schema and proves old state fails closed. At the end, `record.gleam`, `projection.gleam`, and `ledger.gleam` know schema version `2`, can encode and decode workflow run and step-attempt facts, can snapshot and reload the new projection, and reject version `1` records and snapshots with explicit errors. This comes first because recovery must not be built on ambiguous durable data.

Milestone 2 adds artifact serialization and the artifact store. At the end, a `StepArtifact` can be written atomically outside cleanup-targeted workspaces, read back, and hash-verified. This is second because the finished-step fact depends on artifact durability.

Milestone 3 teaches workflow execution to allocate attempt indexes, write prepared and started attempt facts, write finished facts after artifacts are recoverable and `after_step` has been handled under the idempotency contract, and mark interrupted or superseded attempts when workers are stopped. At the end, unit tests with fake workflow dependencies prove the ordering without starting real pi sessions.

Milestone 4 adds recovery of workflow DAG progress and the daemon-owned live validation boundary. At the end, startup recovery can reconstruct completed and failed-continued steps, ignore interrupted or superseded attempts, compute next attempt indexes, verify workflow and issue fingerprints through current daemon observations, handle unavailable tracker or workflow dependencies safely, and return resumable workflow requests to the daemon.

Milestone 5 wires daemon startup, operator session identity, and top-level terminal run records. At the end, recovered workflow runs can be spawned with their original `run_id`, step operator session ids include attempt indexes, parallel steps remain distinguishable, workflow success and failure write terminal records, operator stops and worker-down paths write cancellation or interruption records, and pi session continuation remains unused.

Milestone 6 completes integration validation and reset documentation. At the end, the test suite covers the acceptance cases, old local state rejection has a documented reset path, and the plan's retrospective records any deviations.

## Plan of Work

In `src/scherzo/state/record.gleam`, set `schema_version` to `2`. Replace the old issue-level run constructors with the workflow run constructors named in this plan for workflow-dispatched issues. Add step attempt constructors. Extend the JSON encoder, decoder field record, `kind`, `body_entries`, redaction behavior, and error descriptions. Keep retry, counter, parked v2, Linear command, and outbox v2 records under version `2`. Do not decode schema version `1` records. Before deleting the old generic constructors, search call sites for non-workflow issue-worker paths; if one exists, add explicit schema version `2` generic run records and tests instead of removing the only durable lifecycle facts for that path.

In `src/scherzo/state/projection.gleam`, add workflow run and step attempt projection types and snapshot support in small seams. First define the workflow run and step attempt status types. Then add fold functions for each workflow run fact and each step attempt fact. Then add helper query functions for highest attempt index, latest completed logical workspace, dependency-satisfying attempts, active workflow runs, and recovery candidates. Then add snapshot encoder support, and finally add snapshot decoder support. Applying `StepAttemptPrepared` creates pending status. Applying `StepAttemptStarted` changes that attempt to running. Applying `StepAttemptFinished` changes it to completed, failed-continued, or failed-fatal according to `outcome`. Applying `StepAttemptInterrupted` changes it to interrupted. Applying `StepAttemptSuperseded` changes it to superseded and records the replacement attempt index. Applying workflow run terminal records changes the run status but does not delete attempt history.

In `src/scherzo/state/ledger.gleam`, update snapshot decoding errors so stale schema snapshots are not mistaken for malformed current ledgers or empty state. A version `1` snapshot should return `UnsupportedVersion(1)` or a more specific ledger error that maps to a startup error with reset instructions. Keep the existing truncated-tail behavior for the final current JSONL line, but only for malformed JSON in the last line. A valid old-schema line is not a truncated tail; it is unsupported state.

In `src/scherzo/step_artifact.gleam`, add JSON serialization and decoding for `StepArtifact` and `StepStatus`. Keep the existing field caps and redaction behavior. The serialized payload must include enough fields to reconstruct template locals exactly as before restart: status, final response, exit code, stdout, stderr, timeout flag, truncation flags, and summary text.

Add `src/scherzo/state/artifact_store.gleam`. Define an artifact store rooted at the configured workspace root, functions to build relative artifact refs, write artifacts atomically, read artifacts by ref, and verify content hashes. The module must never return absolute artifact paths in ledger data.

Add `src/scherzo/workflow_fingerprint.gleam` and, if needed, `src/scherzo/hash.gleam`. The fingerprint module must be deterministic and independent of local checkout paths. It should expose a function equivalent to `fingerprint(dag: workflow_dag.WorkflowDag) -> Result(String, FingerprintError)`.

In `src/scherzo/workspace_run.gleam`, add `attempt_index` to `PreparedStepWorkspace` and to `prepare_step`. Change path generation so the physical path includes the logical workspace name, safe step component, step hash, and attempt index. Preserve hook environment variables and add `SCHERZO_ATTEMPT_INDEX`, `SCHERZO_ATTEMPT_KEY`, and `SCHERZO_HOOK_IDEMPOTENCY_KEY` so hooks can distinguish retries and dedupe external side effects safely. Keep `SCHERZO_WORKSPACE_NAME` as the logical name and `SCHERZO_WORKSPACE_PATH` as the physical attempt path.

In `src/scherzo/workflow_run.gleam`, add a resume-aware entry point. A practical shape is to keep `execute` as a fresh-run wrapper and add `execute_with_resume` that accepts recovered artifacts, recovered completed logical workspaces, next attempt index state, and a checkpoint writer. The checkpoint writer can live in a new module such as `src/scherzo/workflow_checkpoint.gleam` and should be injectable through `workflow_run.Dependencies` for tests. Split this work into explicit helper additions: define the checkpoint writer type, add a no-op writer used by `default_dependencies()`, add a fake writer for tests, add a real ledger-backed writer outside `workflow_run.gleam`, then wire call sites in `prepare_ready_steps`, `run_prepared_batch`, `run_after_step`, `apply_prepared_results`, `finish_fatal_batch_result`, and the final return path from `loop`.

In `src/scherzo/workflow_run.gleam`, allocate attempts through the projection-derived next-attempt state rather than by local counters only. Before preparing ready steps, allocate attempt indexes for each step in the ready batch. After preparing workspaces, append `StepAttemptPrepared` records. Before spawning command or agent work, append `StepAttemptStarted` records and register operator step sessions with the new id shape. When a step returns, write its artifact through the artifact store, run after-step handling with both the attempt key and logical hook idempotency key, append `StepAttemptFinished`, and only then mark scheduler state finished. In the final success path from `loop`, append `WorkflowRunFinished(..., outcome: "completed", ...)` before returning success. In fatal paths through `finish_fatal_batch_result` or equivalent workflow failure handling, append `WorkflowRunFinished(..., outcome: "failed_fatal", ...)` before returning fatal failure. For a fatal result in a parallel batch, terminate remaining step workers and append `StepAttemptInterrupted` for siblings that had started but did not finish.

In `src/scherzo/state/recovery.gleam`, split recovery into durable candidate extraction and live-observation finalization. Candidate extraction inspects active workflow runs from the projection and returns only durable data needed by the daemon. Finalization accepts daemon-built current observations and extends `RecoveryPlan` with recovered workflow resumptions plus record bodies needed to normalize interrupted or superseded attempts. For each run, if the run is already finished, do nothing beyond existing cleanup behavior. If the run is active and current workflow and issue fingerprints match, verify finished artifacts, rebuild completed artifacts and logical workspace sources, append interruptions for latest pending or running attempts, and return a resumable run request with the same `run_id`. If the workflow or issue fingerprint does not match, append superseding facts and do not reuse step progress. If tracker data or workflow selection is unavailable after a successful startup check, append interruption facts rather than guessing that the old progress applies.

In `src/scherzo/orchestrator/daemon.gleam`, append `WorkflowRunStarted` before any step can start. Pass the selected `workflow_id`, `workflow_fingerprint`, issue observation fields, and `run_root` into the worker. In `load_startup_recovery`, keep `fetch_recovery_issue_states` as the daemon-owned live issue refresh, add current workflow selection and fingerprinting for each workflow recovery candidate, and call the recovery finalization function only after those observations are available. On tracker fetch failure, return startup error without appending normalization records. On startup after successful finalization, spawn recovered workflow runs from `RecoveryPlan.workflow_resumptions` rather than waiting for poll redispatch. Register top-level sessions for recovered runs, and ensure step sessions generated during resumed attempts use the attempt-indexed operator session id shape. In `stop_session_for_operator`, append cancellation or interruption records for stopped YAML workflow runs. In `handle_registry_down_resolution` and `handle_worker_finished`, ensure worker-down, success, and failure paths do not leave a workflow run without exactly one terminal durable record. Keep `external_session_ref` as `None`.

Update any helper modules that currently depend on old `RunStarted`, `RunFinished`, `RunInterrupted`, or `KnownWorkspace` records. Existing issue counters, retries, parking, command acknowledgements, and outbox replay should continue to work with schema version `2` records.

## Concrete Steps

1. From the repository root, re-read the implementation files named in Preconditions and Verified Facts. If a named type or function moved, update this ExecPlan before coding.
2. Run `jj status --color=never` and confirm the implementation starts from only intended changes.
3. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through `direnv exec .`.
4. Add failing schema tests in `test/scherzo/state/workflow_record_test.gleam`. Cover encoding and decoding `workflow_run_started`, `workflow_run_finished`, `workflow_run_interrupted`, `workflow_run_superseded`, `step_attempt_prepared`, `step_attempt_started`, `step_attempt_finished`, `step_attempt_interrupted`, and `step_attempt_superseded`. Add tests that version `1` `run_started` and `known_workspace` JSON lines return unsupported version rather than valid records. Add preservation tests for one retry record, one issue counter record, one parked issue record, one Linear command record, and one outbox record under schema version `2`.
5. Update `src/scherzo/state/record.gleam` to schema version `2` and implement the record tests.
6. Run `direnv exec . gleam test test/scherzo/state/workflow_record_test.gleam` and expect the new record tests to pass.
7. Add projection tests in `test/scherzo/state/workflow_projection_test.gleam`. The first test folds a run started, attempt prepared, attempt started, and attempt finished with outcome `completed`, then asserts the run is active and the attempt status is completed. The second test folds `failed_continued` and asserts it is dependency-satisfying. The third test folds `failed_fatal` and asserts it is not dependency-satisfying and marks the run failed. The fourth test covers pending, running, interrupted, and superseded statuses. Add helper tests for `next_attempt_index`, `latest_completed_workspace`, and active workflow recovery candidates.
8. Update `src/scherzo/state/projection.gleam` in separate passes: add workflow run status types, add step attempt status types, add record fold behavior, add helper query functions, add snapshot encoding, and add snapshot decoding. Run the projection tests after each pass that compiles.
9. Add old snapshot rejection tests in `test/scherzo/state/workflow_projection_test.gleam` or `test/scherzo/state/ledger_schema_reset_test.gleam`. The fixture is a JSON object with `schema_version: 1` and `kind: "projection_snapshot"`. Assert decode or ledger replay rejects it explicitly.
10. Update `src/scherzo/state/ledger.gleam` error handling so version `1` snapshots and version `1` current records fail closed.
11. Run `direnv exec . gleam test test/scherzo/state/workflow_projection_test.gleam test/scherzo/state/ledger_schema_reset_test.gleam` and expect all tests in those files to pass.
12. Commit the schema and projection work after the targeted tests pass. Suggested commit message: `Replace local durable state with workflow checkpoint schema`.
13. Add artifact serialization tests in `test/scherzo/state/artifact_store_test.gleam`. Create a sample command artifact with stdout, stderr, exit code, timeout false, and truncation flags. Assert writing and reading through the artifact store returns the same artifact and a stable hash. Assert the returned `artifact_ref` is relative and includes the safe step component plus hash. Add a negative test where the file contents are changed after writing and read returns `corrupt_step_artifact`. Add a missing-file test returning `missing_step_artifact`. If directory fsync is implemented through an injectable filesystem wrapper, add a test that the final artifact directory is synced after rename; if the runtime cannot support directory sync, add a test for the explicit weaker-guarantee branch and keep the missing-artifact startup-error test.
14. Implement `StepArtifact` JSON helpers in `src/scherzo/step_artifact.gleam` and artifact store functions in `src/scherzo/state/artifact_store.gleam`.
15. Run `direnv exec . gleam test test/scherzo/state/artifact_store_test.gleam` and expect the artifact store tests to pass.
16. Commit the artifact work. Suggested commit message: `Add durable workflow step artifact store`.
17. Add workflow fingerprint tests in `test/scherzo/workflow_fingerprint_test.gleam`. Build two equivalent DAG values and assert equal fingerprints. Change one step command or dependency and assert the fingerprint changes. Assert no generated `run_id`, workspace path, or current time appears in the canonical input.
18. Implement `src/scherzo/workflow_fingerprint.gleam` and `src/scherzo/hash.gleam` if needed.
19. Run `direnv exec . gleam test test/scherzo/workflow_fingerprint_test.gleam` and expect those tests to pass.
20. Commit the fingerprint work. Suggested commit message: `Fingerprint workflow definitions for recovery`.
21. Add workspace preparation tests in `test/scherzo/workspace_run_attempt_test.gleam`. Assert attempt `1` and attempt `2` for the same run, step, and workspace name produce different `workspace_path` values, preserve the same logical `workspace_name`, and expose `SCHERZO_ATTEMPT_INDEX`, `SCHERZO_ATTEMPT_KEY`, and `SCHERZO_HOOK_IDEMPOTENCY_KEY` in hook env helpers. Assert the attempt key changes between attempt `1` and attempt `2`, while the hook idempotency key stays stable for the same run and step. Add a test where two different step ids use the same logical workspace name and attempt index `1`; assert their paths differ. Add a sanitization-collision test where two step ids sanitize to the same visible text; assert the step hash makes their paths differ.
22. Update `src/scherzo/workspace_run.gleam` to carry `attempt_index` and produce workspace, step, hash, and attempt-specific paths.
23. Run `direnv exec . gleam test test/scherzo/workspace_run_attempt_test.gleam` and expect those tests to pass.
24. Commit the workspace attempt path work. Suggested commit message: `Prepare attempt-specific workflow workspaces`.
25. Add workflow runner ordering tests in `test/scherzo/workflow_run_checkpoint_test.gleam` with fake dependencies. One test uses two sequential steps and asserts the recorded sequence is prepared, started, artifact written, after_step handled when configured, finished for the first step before the second step renders template locals. One test uses two parallel ready steps and asserts both receive attempt index `1`, unique operator session ids, and distinct workspace paths even when they share a logical workspace name. One test simulates artifact write failure and asserts no `step_attempt_finished` record is appended. One test simulates a crash after `after_step` succeeds but before the finish record and asserts the projection does not mark the attempt completed, the rerun may allocate a new attempt index, the new `SCHERZO_ATTEMPT_KEY` distinguishes the new attempt, and `SCHERZO_HOOK_IDEMPOTENCY_KEY` remains stable for external dedupe. Add success and fatal-failure tests asserting `workflow_run_finished` is requested with `completed` and `failed_fatal` respectively.
26. Implement checkpoint writer interfaces and wire them into `src/scherzo/workflow_run.gleam` without changing real daemon behavior yet. Keep a no-op writer for callers that are not ready.
27. Run `direnv exec . gleam test test/scherzo/workflow_run_checkpoint_test.gleam` and expect the new runner tests to pass.
28. Commit the workflow runner checkpoint work. Suggested commit message: `Checkpoint workflow step attempts during execution`.
29. Add recovery tests in `test/scherzo/state/recovery_workflow_test.gleam`. Cover a new-format ledger where step A completed, step B was running, and step C was not started. Assert recovery returns a durable candidate before live observations, then finalization with matching observations returns artifacts and workspace source for A, appends interruption for B, and next attempt index for B is `2`. Add a parallel case where A and B completed independently and C depends on both. Add a changed issue fingerprint case and a changed workflow fingerprint case that supersede the run and do not reuse artifacts. Add `IssueUnavailable` and `WorkflowUnavailable` cases that interrupt rather than supersede and do not reuse artifacts.
30. Update `src/scherzo/state/recovery.gleam` to build durable workflow recovery candidates, live-observation finalization, recovered workflow resumptions, and normalization records.
31. Run `direnv exec . gleam test test/scherzo/state/recovery_workflow_test.gleam` and expect recovery tests to pass.
32. Commit the recovery work. Suggested commit message: `Recover workflow DAG progress from checkpoints`.
33. Add daemon tests in `test/scherzo/orchestrator/daemon_workflow_checkpoint_test.gleam`. Assert `workflow_run_started` is appended before worker spawn. Assert recovered workflow resumptions are spawned on startup only after the daemon fetches current issues, selects the current workflow, and gets matching issue and workflow fingerprints. Assert tracker unavailability returns a startup error without appending normalization records. Assert changed issue fingerprint and changed workflow fingerprint append supersession records and allow fresh dispatch instead of reusing artifacts. Assert missing workflow definition or prompt-read failure appends interruption records and does not resume artifacts. Assert operator step session ids differ for attempt `1` and attempt `2`, and differ for two step ids that sanitize to the same visible text. Add terminal-record assertions for success, fatal failure, explicit operator stop through `stop_session_for_operator`, worker down through `handle_registry_down_resolution`, restart interruption, and supersession.
34. Update `src/scherzo/orchestrator/daemon.gleam` to create ledger-backed checkpoint writers, append workflow run start and terminal records, perform daemon-owned recovery observation, enqueue recovered runs, and use the new operator step session id helper.
35. Run `direnv exec . gleam test test/scherzo/orchestrator/daemon_workflow_checkpoint_test.gleam` and expect daemon checkpoint tests to pass.
36. Commit daemon wiring. Suggested commit message: `Resume workflow runs from durable checkpoints`.
37. Run `direnv exec . gleam format --check src test` and expect no formatting changes needed.
38. Run `direnv exec . gleam test` and expect the full test suite to pass.
39. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective with the final result and any deviations from the plan.

## Testing and Falsifiability

The implementation is false if a completed step can be replayed without its artifact, if a running attempt after restart is treated as completed, if old version `1` state starts the daemon as though it were valid workflow state, if the daemon resumes without a matching current issue and workflow observation, if two different steps collide on the same physical workspace path, if `after_step` hook duplication is hidden instead of governed by the idempotency-key contract, or if a workflow fingerprint mismatch reuses old step progress. Tests must target those failure modes directly.

`test/scherzo/state/workflow_record_test.gleam` should assert exact round-trip JSON behavior for every new record kind. Include concrete values that distinguish `run_id`, `workflow_id`, `step_id`, `workspace_name`, `attempt_index`, and `workspace_path`. For example, use `run_id` like `run-001`, `workflow_id` like `workflow-alpha`, `step_id` like `build`, `workspace_name` like `main`, `attempt_index` `2`, and `workspace_path` like `workspace-root/workflow-alpha/LIV-54/run-001/workspaces/main/steps/build-abcdef123456/attempt-2`. This is intentionally a relative test string, not an absolute local path.

`test/scherzo/state/workflow_projection_test.gleam` should assert the seven attempt statuses. A pending attempt is created by `StepAttemptPrepared`. A running attempt is created by applying `StepAttemptStarted` after prepared. A completed attempt is created by `StepAttemptFinished` with outcome `completed`. A failed-continued attempt is created by outcome `failed_continued` and must satisfy dependencies. A failed-fatal attempt is created by outcome `failed_fatal` and must not satisfy dependencies. An interrupted attempt is created by `StepAttemptInterrupted`. A superseded attempt is created by `StepAttemptSuperseded` and must point to the replacement attempt index.

`test/scherzo/state/artifact_store_test.gleam` should prove write ordering. Use a fake or temporary workspace root provided by the test harness. Write an artifact and assert the returned ref is relative to the artifact store and includes the safe step component plus hash. Decode the file and assert all artifact fields match. Modify the file after writing and assert read fails with a corrupt-artifact error. Delete the file and assert read fails with a missing-artifact error. If directory fsync is injectable, assert the final artifact directory is synced after rename; if directory fsync is not supported, assert the weaker-guarantee branch is visible and recovery still fails closed on missing artifacts.

`test/scherzo/workflow_run_checkpoint_test.gleam` should use fake command and agent step dependencies, not real pi. One test should simulate an artifact store failure and assert the fake ledger never receives `StepAttemptFinished`. One test should simulate an after-step failure and assert the step is not marked as successfully completed for downstream scheduling. One test should simulate a crash after `after_step` succeeds but before `StepAttemptFinished`; expected behavior is that the attempt is not considered completed, the retry uses a new attempt index as needed, the attempt key distinguishes the new attempt, and `SCHERZO_HOOK_IDEMPOTENCY_KEY` remains stable for external dedupe. One test should make a first step finish, then fail the daemon-equivalent before a second step starts, and assert resume state contains only the first artifact. Add tests that normal workflow success and fatal workflow failure request the correct `WorkflowRunFinished` outcome.

`test/scherzo/state/recovery_workflow_test.gleam` should cover new-format ledger replay for sequential and parallel workflows. Sequential case: step A completed, step B running at crash, step C depends on B. Expected candidate extraction returns durable facts without reading live issue or workflow state. Expected finalization with matching observations: A remains completed with artifact loaded, B gets interruption appended, B's next attempt index is `2`, and C is pending but not runnable until B finishes. Parallel case: A and B are independent and both completed before crash; C depends on both. Expected recovery: C is ready and A and B are not rerun. Attempt-index case: records for attempts `1` and `2` exist for the same step; expected next index is `3`. Observation cases must cover issue unavailable, workflow unavailable, issue fingerprint changed, and workflow fingerprint changed.

`test/scherzo/orchestrator/daemon_workflow_checkpoint_test.gleam` should cover the daemon-owned recovery boundary and operator session id uniqueness. Use fake tracker and workflow-selection dependencies. Matching current issue and workflow observations must resume the run. Tracker unavailability must return a startup error without appending normalization records. Changed issue or workflow fingerprints must supersede the old run and avoid artifact reuse. Missing workflow definition or prompt-read failure must interrupt and avoid artifact reuse. Use two attempts for the same step and assert ids differ by attempt index. Use two step ids that sanitize to the same safe text and assert ids differ by hash. Assert the session id includes the `run_id`, a safe step component, and `a<attempt_index>`. Add assertions for top-level terminal records on success, fatal failure, operator cancellation, worker down, restart interruption, and supersession.

Old-state reset tests must include both an old current ledger line and an old projection snapshot. The old current ledger fixture should be a JSONL line with `schema_version: 1`, `kind: "run_started"`, `run_id`, `issue_id`, `issue_identifier`, and `workspace_path`. Expected result: `ledger.replay` returns unsupported version or daemon startup maps it to a reset-required startup error. The old snapshot fixture should have `schema_version: 1` and `kind: "projection_snapshot"`. Expected result: snapshot load rejects it explicitly and does not return `projection.new()`.

Falsify the non-functional safety claim by forcing these crash-window simulations in tests: crash after `StepAttemptPrepared` before `StepAttemptStarted`; crash after `StepAttemptStarted` before artifact write; crash after artifact write before `StepAttemptFinished`; crash after `after_step` success before `StepAttemptFinished`; crash after `StepAttemptFinished` before workflow finish; and durable finish record with missing artifact. The expected recovery outcomes are, respectively, interrupted and new attempt index; interrupted and new attempt index; interrupted because no finish record exists; interrupted and rerun only under the explicit at-least-once hook contract; completed step reused because finish record and artifact exist; and startup error because the artifact is missing.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test

Expected result: the command exits successfully without reporting files that need formatting.

Then run:

    direnv exec . gleam test

Expected result: all tests pass, including the new workflow checkpoint, projection, recovery, artifact store, old-state rejection, daemon recovery-boundary, terminal-record, workspace-collision, artifact-durability, after-step idempotency, and daemon session id tests. The exact pass count may change as tests are added; update this plan's Outcomes & Retrospective with the observed count.

Manual acceptance for an operator-focused review can be performed with a test workflow fixture rather than real Linear if the repository has daemon integration harnesses. Start a workflow with at least three steps where A and B are independent and C depends on both. Stop the daemon after A finishes and B is running. Restart. Expected observation: A is not rerun, B's old attempt is recorded as interrupted, B resumes with attempt index `2`, C starts only after B attempt `2` finishes, and operator step session ids show distinct attempt indexes.

Acceptance is met when the implementation demonstrates all of the following behaviors: new-format ledger replay reconstructs completed workflow steps; multi-step and parallel-step DAGs resume from durable boundaries; attempt-index allocation is monotonic per run and step; operator step session ids are unique across attempts; `step_attempt_finished` is never appended before artifacts are recoverable; version `1` local state is rejected with a reset path; and pi session continuation is not attempted.

## Rollout, Recovery, and Idempotence

This is a local durable-state schema replacement. Rollout is safe only when operators know that old local state is not migrated. The startup error for old state must say that the local durable schema is unsupported and that the operator must reset or archive local state. The reset procedure is:

1. Stop the Scherzo daemon.
2. Preserve audit history if desired by moving the configured workspace root's `.scherzo-state/ledger` directory to an archive location that Scherzo will not read on startup.
3. Remove the configured workspace root's `.scherzo-state/ledger` directory.
4. Remove the configured workspace root's `.scherzo-state/artifacts` directory if it contains artifacts from an aborted schema `2` test run that should not be reused.
5. Start Scherzo again so it creates fresh schema `2` durable state.

Do not copy version `1` `current.jsonl` lines into a schema `2` ledger. Do not keep a version `1` `snapshot.json` in the active ledger directory. Tests must prove these old files do not silently seed recovery.

Most implementation steps are idempotent at the test level. Ledger append operations are intentionally not idempotent unless a record id is reused. For recovery normalization records, use deterministic record ids or dedupe checks where repeat startup could otherwise append duplicate interruption or superseded records for the same attempt. A safe rule is: before appending a recovery-generated interruption or supersession, ask the projection whether that terminal fact is already present. `after_step` hooks are explicitly at-least-once across crash recovery and must use `SCHERZO_HOOK_IDEMPOTENCY_KEY` for external dedupe when they perform side effects; `SCHERZO_ATTEMPT_KEY` remains available for per-attempt logs and diagnostics.

Rollback during development is straightforward before deployment: revert the schema `2` code changes and delete any schema `2` test state under the configured workspace root. After deployment, rollback to schema `1` code cannot read schema `2` state. If rollback is required, operators must archive the schema `2` `.scherzo-state/ledger` directory before starting older code.

## Artifacts and Notes

The key current facts observed before authoring were:

    jj status --color=never
    The working copy has no changes.

    src/scherzo/state/record.gleam
    pub const schema_version = 1

    src/scherzo/state/ledger.gleam
    current ledger: .scherzo-state/ledger/current.jsonl
    projection snapshot: .scherzo-state/ledger/snapshot.json

    src/scherzo/orchestrator/daemon.gleam
    YAML step session id shape today: run_id <> "-" <> step_id

    src/scherzo/workspace_run.gleam
    cleanup_run deletes run_root unless the retention marker exists

These notes are included to make the plan self-contained. Re-verify them before implementation because the repository may change between plan review and execution.

## Interfaces and Dependencies

No new package dependency is required. Use existing Gleam, Erlang runtime capabilities, and repository modules. If SHA-256 helpers do not exist, add a narrow FFI wrapper in `src/scherzo/hash.gleam` with functions similar to:

    pub fn sha256_hex(contents: String) -> String
    pub fn short_sha256_hex(contents: String, chars: Int) -> String

Add workflow fingerprinting in `src/scherzo/workflow_fingerprint.gleam` with an interface similar to:

    pub type FingerprintError {
      PromptFileReadFailed(path: String)
      UnsupportedWorkflowShape(reason: String)
    }

    pub fn fingerprint(dag: workflow_dag.WorkflowDag) -> Result(String, FingerprintError)

Add artifact storage in `src/scherzo/state/artifact_store.gleam` with an interface similar to:

    pub type Store {
      Store(workspace_root: String)
    }

    pub type ArtifactRef {
      ArtifactRef(ref: String, sha256: String, bytes: Int)
    }

    pub type ArtifactError {
      ArtifactIo(String)
      MissingStepArtifact(String)
      CorruptStepArtifact(String)
      InvalidArtifactRef(String)
      DecodeArtifactFailed(String)
      DirectorySyncUnsupported(String)
    }

    pub fn new(workspace_root: String) -> Store

    pub fn write_step_artifact(
      store: Store,
      run_id: String,
      workflow_id: String,
      step_id: String,
      attempt_index: Int,
      artifact: step_artifact.StepArtifact,
    ) -> Result(ArtifactRef, ArtifactError)

    pub fn read_step_artifact(
      store: Store,
      ref: String,
      expected_sha256: String,
    ) -> Result(step_artifact.StepArtifact, ArtifactError)

Add checkpoint writing in a module such as `src/scherzo/workflow_checkpoint.gleam`. The exact data wrappers may differ, but the workflow runner must not manually build JSON strings. It should call typed functions that append records and write artifacts in the correct order. A suitable high-level writer shape is:

    pub type Writer {
      Writer(
        workflow_started: fn(WorkflowStarted) -> Result(Nil, CheckpointError),
        workflow_finished: fn(WorkflowFinished) -> Result(Nil, CheckpointError),
        workflow_interrupted: fn(WorkflowInterrupted) -> Result(Nil, CheckpointError),
        workflow_superseded: fn(WorkflowSuperseded) -> Result(Nil, CheckpointError),
        step_prepared: fn(StepPrepared) -> Result(Nil, CheckpointError),
        step_started: fn(StepStarted) -> Result(Nil, CheckpointError),
        step_finished: fn(StepFinished, step_artifact.StepArtifact) -> Result(Nil, CheckpointError),
        step_interrupted: fn(StepInterrupted) -> Result(Nil, CheckpointError),
        step_superseded: fn(StepSuperseded) -> Result(Nil, CheckpointError),
      )
    }

The real writer resolves `ledger.LedgerPath` from the configured workspace root, resolves an `artifact_store.Store` from the same root, writes artifacts before step-finished records, and uses `ledger.append` or `ledger.append_many` with `fsync: True` for workflow terminal and step checkpoint records. Tests can use a fake writer that records call order and injects failures.

Extend `workflow_run.Dependencies` in `src/scherzo/workflow_run.gleam` to include the checkpoint writer or enough functions to perform equivalent typed checkpoint operations. Keep `default_dependencies()` working by returning a no-op writer for tests or callers that do not need durable workflow checkpoints. The daemon must override this with a real ledger-backed writer for actual workflow runs.

Extend recovery types in `src/scherzo/state/recovery.gleam` with a resumable workflow request similar to:

    pub type RecoveredWorkflowRun {
      RecoveredWorkflowRun(
        issue: domain.Issue,
        run_id: String,
        workflow_id: String,
        workflow_fingerprint: String,
        completed_artifacts: Dict(String, step_artifact.StepArtifact),
        completed_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
        next_attempt_indexes: Dict(String, Int),
      )
    }

If importing `workspace_run` into recovery would create an undesirable dependency cycle, define a smaller recovery-only workspace summary type with `workflow_id`, `run_id`, `run_root`, `workspace_name`, `path`, `source_workspace_name`, `source_workspace_path`, and `attempt_index`, then convert it in the daemon or workflow runner.

## Open Questions and Clarifications Needed

None.
