# Implement a Durable-Contract-Aligned Pure Startup Recovery Planner for Workflow DAG Progress

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo will have a pure, deterministic planner that can look at durable workflow-run and step-attempt facts recorded before a daemon restart, compare those facts with the freshly selected workflow DAG and refreshed issue fingerprint, and decide what is safe to do next. Operators gain a conservative recovery answer before the runner or daemon startup path is changed: completed step artifacts are preserved, recorded `failed_continued` outcomes keep satisfying dependencies, recorded `failed_fatal` outcomes remain fatal even if the current YAML would handle the failure differently, prepared or started attempts that did not finish are marked for interruption, and cleanup is requested only when a durable top-level workflow terminal record proves the run root is safe to remove.

The visible outcome of this plan is not automatic workflow resumption in production. The visible outcome is a reviewed and tested module whose output says, for a concrete workflow run, which steps are terminal, which pending steps may be started by a later integration, which steps are blocked, which prepared or started attempts must be durably interrupted, which drift condition blocks continuation, which recorded agent sessions are only candidates for future inspection, and which already-terminal run roots can be cleaned by a later side-effecting integration.

## Problem Framing and Constraints

Scherzo workflows are directed acyclic graphs, or DAGs: each workflow YAML file names steps, each step can depend on earlier steps, and the runner starts a step only after its dependencies are complete. Today the repository has workflow execution and run-level startup recovery, but the current recovery path in `src/scherzo/state/recovery.gleam` is not step-aware. If the daemon restarts after some workflow steps finished and another step was running, a coarse run-level retry is too risky: it can throw away terminal artifacts, rerun a command that may not be safe to rerun, continue after a workflow YAML or issue changed, or unblock downstream work before the interrupted dependency has been resolved.

This plan designs and implements a pure startup recovery planner only. Pure means the planner receives all facts as arguments and returns a value; it does not read files, call Linear, spawn agents, run commands, append ledgers, verify artifact files, clean directories, or mutate runtime state. The daemon and workflow runner will not be changed by this implementation ticket. In particular, this ticket is standalone-only: it defines planner-facing input types that mirror the durable workflow checkpoint contract from the checked-in LIV-54 plan, but it does not implement an adapter from `src/scherzo/state/projection.gleam`, does not import `src/scherzo/state/recovery.gleam`, and does not wire daemon startup. A later integration ticket must build and test the adapter once the durable checkpoint implementation exists in source.

Backward compatibility with ledgers or snapshots written before workflow-resumption work is explicitly out of scope. The current tree still uses durable schema version `1`, and `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` do not yet expose workflow run or step-attempt projection types. That absence is not an open question for this plan; it is the reason this ticket remains standalone. The planner contract below embeds the durable fields a later adapter must supply, so the future adapter does not have to invent recovery semantics.

## Strategy Overview

The right-sized approach is to add one isolated Gleam module, `src/scherzo/workflow_recovery_planner.gleam`, plus a focused test suite. The planner's input model is deliberately shaped like the durable checkpoint contract that LIV-54 specifies: `WorkflowRunStarted`, optional top-level workflow terminal records, `StepAttemptPrepared`, `StepAttemptStarted`, `StepAttemptFinished`, `StepAttemptInterrupted`, and `StepAttemptSuperseded`. Finished attempts carry the recorded durable outcome `completed`, `failed_continued`, or `failed_fatal` and a verified artifact payload with its durable `artifact_ref` and `artifact_sha256`. The planner must consume that recorded outcome as historical truth; it must not recompute a failed attempt's meaning from the current workflow DAG.

The planner works in five conceptual passes. First, it validates the recorded run identity against the current observation: workflow id, workflow fingerprint, and issue fingerprint. A workflow fingerprint is the deterministic hash of the workflow definition fields that affect execution, including prompt content when prompt files are used; it replaces the earlier idea of per-step prompt fingerprint inputs. Second, it groups attempts by step id and selects the latest durable attempt by `attempt_index`, with deterministic tie-breaking only for malformed synthetic input. Third, it classifies each step as unattempted, completed, failed-continued, failed-fatal, interrupted-before-start, interrupted-after-start, already-interrupted, or superseded. Fourth, when there is no drift and no unsafe unresolved attempt, it maps the recovery states back to `workflow_scheduler.StepRuntime` and reuses `workflow_scheduler.ready_steps` for ready-step selection, preserving the live scheduler's `max_parallel_steps` and same-workspace behavior. Fifth, it returns a conservative recovery plan: preserve verified artifacts from terminal attempts, emit idempotent interruption-record intents for prepared or started attempts that still lack a durable interruption record, request park or operator inspection for unsafe interruptions, identify agent session recovery candidates without trying to continue pi sessions, emit workflow-finish record intents for active runs that appear terminal after a no-drift check, and emit cleanup requests only for runs that already have a durable `WorkflowRunFinished` record and no cleanup marker.

This approach is proportionate because it introduces one isolated planner with focused tests. It avoids changing the runner while still closing the dangerous design choices that a future runner integration would otherwise have to invent under pressure. It also avoids duplicating the live scheduler's selection semantics by calling the scheduler for the scheduling subset rather than reimplementing capacity and workspace serialization from scratch.

## Alternatives Considered

The simplest plausible alternative is to recover only at the whole-run level: if a run was started but not finished, mark it interrupted and retry or park the issue. That is insufficient because workflow DAGs can have completed terminal artifacts that downstream prompts need, recorded `failed_continued` failures that intentionally satisfy dependencies, and parallel branches where some work is safe while other work is blocked. A whole-run retry can duplicate expensive agent work or rerun unsafe commands.

Another option is to extend `src/scherzo/workflow_scheduler.gleam` directly with recovery behavior. That module currently models live execution statuses as pending, running, succeeded, failed-continued, and failed-fatal. It is intentionally small and focused on selecting live ready steps. Recovery needs additional concepts: prepared-but-not-started attempts, durable started attempts, recorded interruption facts, superseded attempts, artifact verification boundaries, drift errors, inspection requests, and top-level workflow terminal records. Folding those concerns into the live scheduler would make the scheduler harder to reason about. This plan still reuses `workflow_scheduler.ready_steps` for the part it owns: selecting ready pending steps after recovery states have been reduced to the live scheduler's statuses.

A third option is to implement the projection adapter now. That is rejected for this ticket because the current source tree does not yet contain the schema version `2` workflow checkpoint records or workflow recovery candidate projection. Building an adapter against non-existent types would either block on stakeholder clarification or invent a second durable projection. This plan instead defines a stable pure planner input contract that mirrors the checked-in LIV-54 durable contract and leaves adapter implementation to the later ticket that has the real projection source available.

A fourth option is to immediately wire recovery into `src/scherzo/orchestrator/daemon.gleam` startup. That is too large for this ticket and would mix policy design with side effects. This plan deliberately stops at a pure planner and test suite so the recovery semantics can be reviewed before any production path invokes them.

## Risks and Countermeasures

The main safety risk is accidental continuation after facts drift. The countermeasure is to make drift checking the first gate for any start request. If workflow id, workflow fingerprint, or issue fingerprint differs from the current observation, or if the current issue or workflow is unavailable, the plan must contain no `StartStep` requests and must contain an inspection or park request describing the drift or unavailability.

A second risk is deleting useful evidence after drift. The countermeasure is conservative cleanup: if any drift or current-observation unavailability exists, the planner emits no `CleanupRunRoot` unless the input already contains a durable top-level `WorkflowRunFinished` record for that run and `cleanup_recorded` is false. The planner must not decide that a drifted active run is terminal by comparing its attempts to the current DAG, because the current DAG may have removed or changed an unfinished recorded step. Cleanup is only inert data in this ticket, but the data must already encode this safety rule for the later side-effecting integration.

A third risk is rerunning an interrupted command that created side effects before the restart. The countermeasure is to classify every prepared attempt and every started attempt without a terminal or interruption record as unresolved. A prepared attempt produces an interruption intent with reason `daemon_restart_before_step_start`. A started command attempt produces an interruption intent with reason `daemon_restart_during_step`, an inspection request, and, by default, a park request. This implementation must not add an automatic command rerun path. A future explicit rerun-safe policy can extend the planner, but absence of that policy always means no command continuation.

A fourth risk is double-counting the same unresolved attempt every time the daemon starts. The countermeasure is to key interruption record intents by `(run_id, step_id, attempt_index)`. If the latest durable status is already `StepAttemptInterrupted`, the planner emits no new interruption intent for that attempt. Repeated startup over the same projection plus the durable interruption record returns no duplicate intent.

A fifth risk is misclassifying historical failures by consulting today's workflow YAML. The countermeasure is to store the recorded finished-attempt outcome in the planner input and use it as the source of truth. The current DAG's `on_failure` policy is relevant only through workflow fingerprint drift and scheduler topology after drift has been ruled out; it must not turn a recorded `failed_fatal` into `failed_continued` or the reverse.

A sixth risk is accepting corrupt or missing artifacts. The pure planner cannot read or hash artifact files, so the boundary must fail closed before `plan_run` is called. A later adapter may construct `VerifiedArtifact` only after reading the durable `artifact_ref`, checking `artifact_sha256`, decoding the artifact, and confirming the decoded payload matches the expected run, workflow, step, and attempt. If verification fails, the adapter must return a recovery error such as missing or corrupt step artifact instead of calling the planner with a guessed artifact. The standalone tests use `VerifiedArtifact` values directly to exercise planner behavior after this boundary has succeeded.

A seventh risk is duplicating live scheduler behavior incorrectly. The countermeasure is to reuse `workflow_scheduler.ready_steps` for final `StartStep` selection and to add parity tests that compare the planner's ready step ids against the scheduler's ready step ids for equivalent pending, succeeded, failed-continued, and failed-fatal states. The planner may still build its own blocked-step explanations, but the action list that would start work must follow the live scheduler.

## Progress

- [x] (2026-05-03 00:00Z) Drafted the first ExecPlan for a pure startup recovery planner without source implementation.
- [x] (2026-05-03 00:00Z) Incorporated adversarial review findings: aligned the planner contract with LIV-54 durable records, removed the open prerequisite clarification by making the ticket standalone-only, consumed recorded finished outcomes, made cleanup under drift conservative, added prepared and superseded attempts, and required scheduler parity coverage.
- [ ] Add the pure planner module and durable-contract-shaped type model.
- [ ] Add tests for durable attempt classification, recorded outcomes, drift blocking, scheduler-backed dependency planning, multiple attempts, idempotence, artifact-boundary behavior, and cleanup safety.
- [ ] Run formatting and the Gleam test suite.
- [ ] Update this ExecPlan with implementation discoveries, decisions, and outcomes.

## Surprises & Discoveries

- Observation: The current live workflow scheduler already treats `FailedContinued` as a complete dependency.
  Evidence: `src/scherzo/workflow_scheduler.gleam` defines `StepRuntime` with `FailedContinued`, and `workflow_scheduler.ready_steps` uses `Succeeded` and `FailedContinued` as dependency-complete states.

- Observation: Current startup recovery is run-level and not step-attempt-aware.
  Evidence: `src/scherzo/state/recovery.gleam` plans around issue-level `RunStarted`, `RunFinished`, and `RunInterrupted` projections, while workflow step-attempt recovery states do not exist in the inspected module.

- Observation: The daemon already has a startup shape that replays durable state and refreshes Linear issue state before planning recovery.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` has `load_startup_recovery`, which replays the ledger, fetches issue states by known ids, and calls `recovery.plan`. A later integration can use the same shape after this pure planner exists.

- Observation: The checked-in LIV-54 plan defines the durable checkpoint contract this planner must align with, including `WorkflowRunStarted`, `WorkflowRunFinished`, `StepAttemptPrepared`, `StepAttemptStarted`, `StepAttemptFinished(outcome, artifact_ref, artifact_sha256, ...)`, `StepAttemptInterrupted`, and `StepAttemptSuperseded`.
  Evidence: `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md` restates those record constructors and says finished outcomes are `completed`, `failed_continued`, or `failed_fatal`.

- Observation: The current source tree does not yet implement the LIV-54 schema version `2` workflow checkpoint records.
  Evidence: `src/scherzo/state/record.gleam` still defines `pub const schema_version = 1`, and searches of `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` find no `WorkflowRunStarted` or `StepAttemptPrepared` constructors.

## Decision Log

- Decision: Implement the new behavior as a pure planner module rather than wiring daemon startup now.
  Rationale: The ticket asks for conservative recovery planning without changing the runner. A pure module is reviewable, testable, and safe to build before side effects are introduced.
  Date: 2026-05-03

- Decision: Make this ticket standalone-only and defer the projection adapter even if LIV-54 source code lands before implementation starts.
  Rationale: The current tree lacks the durable projection types. Defining the planner contract now is useful, but importing a projection adapter would expand scope and couple this plan to source that may still be changing. A later integration ticket can add adapter tests against the real projection.
  Date: 2026-05-03

- Decision: Shape planner inputs after the LIV-54 durable checkpoint contract: workflow run start facts, top-level run terminal status, prepared, started, finished, interrupted, and superseded step-attempt statuses, recorded finished outcomes, artifact refs, and artifact hashes.
  Rationale: A future adapter should map durable projection facts mechanically into planner input. It must not invent semantics for prepared attempts, superseded attempts, artifact verification, or terminal outcomes.
  Date: 2026-05-03

- Decision: Treat `failed_continued` and `failed_fatal` as recorded historical outcomes, not as values recomputed from the current DAG's `on_failure` setting.
  Rationale: A workflow file can change after a step finished. Recovery must preserve what actually happened under the then-current workflow, while using the current DAG only after fingerprint equality proves it is the same execution definition.
  Date: 2026-05-03

- Decision: Emit no cleanup under drift unless the run already has a durable `WorkflowRunFinished` record.
  Rationale: A drifted current DAG can omit or change recorded unfinished steps. Deleting the run root based on current-DAG terminal calculation could destroy evidence. A top-level durable finish record proves the run was terminal before drift, and LIV-54 stores durable artifacts outside cleanup-targeted run roots.
  Date: 2026-05-03

- Decision: Treat prepared and started attempts without durable terminal facts as interruption candidates and use `(run_id, step_id, attempt_index)` as the idempotence key.
  Rationale: `StepAttemptPrepared` is a real crash window, and `StepAttemptStarted` may have side effects. A durable `StepAttemptInterrupted` fact is the marker that prevents repeated startup from emitting the same interruption intent.
  Date: 2026-05-03

- Decision: Keep artifact verification outside the pure planner and require the input type name `VerifiedArtifact` to make the boundary explicit.
  Rationale: Reading files and hashing bytes are side effects. The planner can remain pure only if a later adapter verifies `artifact_ref` and `artifact_sha256` before constructing terminal attempt inputs.
  Date: 2026-05-03

- Decision: Reuse `workflow_scheduler.ready_steps` for ready-step selection and add parity tests.
  Rationale: The live scheduler already owns capacity and same-workspace selection. The recovery planner should not fork those semantics when it can reduce safe recovery states to scheduler statuses.
  Date: 2026-05-03

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam service that runs pi agents from Linear issues. Linear is the issue tracker. A workflow is a YAML-defined DAG selected from an issue label such as `workflow:implementation`. A DAG step is either an agent step, which prompts pi, or a command step, which runs a shell command in a prepared workspace. A step artifact is the durable summary of a finished step; for an agent it contains the final response, and for a command it contains exit code, stdout, stderr, timeout, and diagnostics. A run root is the directory that contains per-step workspaces for one workflow run. LIV-54's artifact store design keeps durable artifacts under `.scherzo-state/artifacts`, not inside cleanup-targeted run roots.

The current workflow parser and domain model live in `src/scherzo/workflow_dag.gleam`. It defines `WorkflowDag`, `WorkflowStep`, `StepKind`, `WorkspaceRef`, and `FailurePolicy`. The parser supports `on_failure: continue`, represented as `ContinueWorkflow`; absence of that setting defaults to fatal workflow failure. A workflow fingerprint is the deterministic identity of the loaded workflow definition fields that affect execution. It must include prompt contents or prompt identity as part of the whole-workflow hash, so this planner does not take separate per-step prompt fingerprint fields.

The current live scheduler lives in `src/scherzo/workflow_scheduler.gleam`. It can initialize all steps as pending, mark steps running or finished, choose ready steps while respecting dependencies, same-workspace serialization, and `max_parallel_steps`, and decide whether a live workflow is in progress, succeeded, or failed. This planner should reuse `workflow_scheduler.ready_steps` after recovery states are reduced to scheduler statuses and only when drift and unsafe interruptions have already been ruled out.

The current executor lives in `src/scherzo/workflow_run.gleam`. It prepares workspaces, starts ready batches, runs command and agent steps, gathers `step_artifact.StepArtifact` values, passes artifacts into downstream prompts, and cleans the run root on success or fatal failure. This ticket does not change that executor.

Durable run-level state currently lives under `src/scherzo/state/`. `src/scherzo/state/record.gleam` defines ledger record shapes such as `RunStarted`, `RunFinished`, `RunInterrupted`, `KnownWorkspace`, and parked issue records. `src/scherzo/state/projection.gleam` folds ledger records into a projection. `src/scherzo/state/recovery.gleam` plans run-level startup recovery. The checked-in LIV-54 plan defines future schema version `2` workflow facts. This plan consumes durable-contract-shaped facts but does not require those future source modules to exist yet.

The daemon startup flow in `src/scherzo/orchestrator/daemon.gleam` already shows the side-effect boundary a later integration can use: replay durable state, refresh issues from the tracker, call a pure planning function, append recovery records, then perform cleanup or schedule timers. This plan only builds the workflow-specific pure planning function and tests.

## Preconditions and Verified Facts

The repository is a Gleam project. `gleam.toml` declares package name `scherzo`, target `erlang`, and `gleeunit` as the test dependency. From the repository root, validation commands should run through direnv when available, for example `direnv exec . gleam test` and `direnv exec . gleam format --check src test`. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

The inspected tree contains `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, and `src/scherzo/orchestrator/daemon.gleam`. It also contains workflow tests including `test/workflow_scheduler_test.gleam` and `test/workflow_run_test.gleam`.

The current source tree still uses durable schema version `1`. `src/scherzo/state/record.gleam` defines `pub const schema_version = 1`, and the state modules do not expose `WorkflowRunStarted`, `WorkflowRunFinished`, `StepAttemptPrepared`, `StepAttemptStarted`, `StepAttemptFinished`, `StepAttemptInterrupted`, or `StepAttemptSuperseded`. Therefore the implementation must not add a projection adapter in this ticket. It should proceed with only `src/scherzo/workflow_recovery_planner.gleam` and `test/workflow_recovery_planner_test.gleam`.

The durable contract this planner aligns with is restated here so the implementer does not need prior-plan memory. A workflow run start fact has `run_id`, `workflow_id`, `workflow_fingerprint`, `issue_id`, `issue_identifier`, `issue_fingerprint`, `observed_updated_at_ms`, and `run_root`. A top-level workflow finished fact has `run_id`, `workflow_id`, `issue_id`, an outcome string `completed`, `failed_fatal`, or `cancelled`, plus token and turn counts. A prepared step attempt has `run_id`, `workflow_id`, `step_id`, `attempt_index`, `workspace_name`, `workspace_path`, `run_root`, and optional source workspace fields. A started step attempt has `run_id`, `workflow_id`, `step_id`, `attempt_index`, `operator_session_id`, and `external_session_ref`. A finished step attempt has `run_id`, `workflow_id`, `step_id`, `attempt_index`, recorded outcome string `completed`, `failed_continued`, or `failed_fatal`, `artifact_ref`, `artifact_sha256`, `workspace_name`, `workspace_path`, `token_total`, and `turns`. An interrupted attempt has `run_id`, `workflow_id`, `step_id`, `attempt_index`, and `reason`. A superseded attempt has `run_id`, `workflow_id`, `step_id`, `attempt_index`, `superseded_by_attempt_index`, and `reason`.

The planner input is a projection-like summary of those records, not raw JSONL. For `StepAttemptStarted`, the input must include the workspace fields joined from the prepared attempt so the planner can produce session recovery candidates and blocked-step explanations without querying the projection. For `StepAttemptFinished`, the input must include `VerifiedArtifact`, meaning the future adapter has already read `artifact_ref`, verified `artifact_sha256`, decoded `step_artifact.StepArtifact`, and failed closed if verification did not succeed.

The current `step_artifact.StepArtifact` constructor is public and includes fields for `step_id`, `status`, optional final response, optional command exit code, stdout, stderr, timeout, truncation flags, and summary text. Tests in this plan should construct small artifacts directly rather than depending on artifact limits or artifact-store side effects.

## Scope Boundaries

In scope: a new pure planner module; durable-contract-shaped input and output types; a test suite for classification, recorded outcomes, drift, DAG readiness, idempotence, scheduler parity, artifact-boundary assumptions, and cleanup; and this ExecPlan's living-document updates during implementation.

Out of scope: changing the workflow runner; spawning resumed agents; rerunning command steps automatically; continuing pi sessions; appending new ledger records from daemon startup; implementing a projection or artifact-store adapter; importing `src/scherzo/state/projection.gleam` or `src/scherzo/state/recovery.gleam`; changing Linear state; deleting workspaces; supporting ledgers or snapshots older than the workflow-resumption projection; changing workflow YAML syntax; adding a public operator command; or changing the current dogfood workflow files.

The planner may return typed requests such as `StartStep`, `ParkIssue`, `RecordStepAttemptInterrupted`, `RecordWorkflowRunFinished`, or `CleanupRunRoot`, but those requests are inert data. No implementation step in this plan should execute those requests from daemon startup.

## Milestones

Milestone 1 adds the planner type model. At the end, `src/scherzo/workflow_recovery_planner.gleam` exists with input and output types, a `default_policy` function, a safe `plan_run` stub or minimal implementation, and helper constructors only as needed by tests. The project still compiles. This milestone reduces ambiguity by naming the exact durable statuses and actions the planner can produce.

Milestone 2 implements durable attempt normalization. At the end, the planner can classify unattempted steps, prepared attempts that need interruption, started attempts that need interruption, finished completed attempts, finished failed-continued attempts, finished failed-fatal attempts, already-interrupted attempts, and superseded attempts. Tests prove that the latest attempt index wins, older attempts remain historical only, recorded outcomes are consumed directly, and an already interrupted attempt does not produce a duplicate interruption intent.

Milestone 3 implements drift gates and cleanup safety. At the end, any current-observation drift or unavailability produces no start requests. Cleanup is emitted under drift only when `run_status` is a durable `RunFinished` value and `cleanup_recorded` is false. Tests cover workflow fingerprint drift where the current DAG omits an old recorded step, issue fingerprint drift, workflow id mismatch, issue unavailable, workflow unavailable, and failure-policy drift that would be unsafe if historical outcomes were recomputed.

Milestone 4 implements scheduler-backed DAG planning and workflow finish intents. At the end, completed and failed-continued artifacts are preserved, failed-fatal stops the run, pending steps are startable only when the live scheduler says they are ready, interrupted or superseded dependencies block downstream work, parallel fixture DAGs behave deterministically, and active no-drift runs that appear terminal emit `RecordWorkflowRunFinished` instead of cleanup. Scheduler parity tests compare the planner's `start_steps` with `workflow_scheduler.ready_steps` for equivalent live states.

Milestone 5 completes validation and review readiness. At the end, formatting passes, the Gleam test suite passes, no adapter or daemon integration has been added, this ExecPlan records any discoveries, and the module is ready for a later integration ticket.

## Plan of Work

Create `src/scherzo/workflow_recovery_planner.gleam`. Import `gleam/dict`, `gleam/list`, `gleam/option`, and `gleam/result` as needed, plus `scherzo/step_artifact`, `scherzo/workflow_dag`, and `scherzo/workflow_scheduler`. Do not import `scherzo/state/projection`, `scherzo/state/recovery`, `scherzo/orchestrator/daemon`, or modules that perform I/O. Keep all functions pure.

Define the input model in that module. The names below are prescriptive for this standalone planner contract.

    pub type PlannerInput {
      PlannerInput(
        run: WorkflowRunFacts,
        dag: workflow_dag.WorkflowDag,
        current: CurrentWorkflowObservation,
        policy: RecoveryPolicy,
        now_ms: Int,
      )
    }

    pub type WorkflowRunFacts {
      WorkflowRunFacts(
        run_id: String,
        workflow_id: String,
        workflow_fingerprint: String,
        issue_id: String,
        issue_identifier: String,
        issue_fingerprint: String,
        observed_updated_at_ms: Int,
        run_root: String,
        cleanup_recorded: Bool,
        run_status: DurableRunStatus,
        step_attempts: List(StepAttemptFacts),
      )
    }

    pub type DurableRunStatus {
      RunActive
      RunFinished(outcome: WorkflowRunOutcome, token_total: Int, turns: Int)
      RunInterrupted(reason: String)
      RunSuperseded(superseded_by_run_id: String, reason: String)
    }

    pub type WorkflowRunOutcome {
      WorkflowCompleted
      WorkflowFailedFatal
      WorkflowCancelled
    }

    pub type CurrentWorkflowObservation {
      CurrentWorkflowObservation(
        workflow_id: String,
        workflow_fingerprint: String,
        issue_fingerprint: String,
      )
      IssueUnavailable(reason: String)
      WorkflowUnavailable(reason: String)
    }

    pub type StepAttemptFacts {
      StepAttemptFacts(
        run_id: String,
        workflow_id: String,
        step_id: String,
        attempt_index: Int,
        status: DurableStepAttemptStatus,
      )
    }

    pub type DurableStepAttemptStatus {
      AttemptPrepared(
        workspace_name: String,
        workspace_path: String,
        run_root: String,
        source_workspace_name: Option(String),
        source_workspace_path: Option(String),
      )
      AttemptStarted(
        workspace_name: String,
        workspace_path: String,
        run_root: String,
        operator_session_id: String,
        external_session_ref: Option(String),
      )
      AttemptFinished(
        outcome: RecordedStepOutcome,
        artifact: VerifiedArtifact,
        workspace_name: String,
        workspace_path: String,
        token_total: Int,
        turns: Int,
      )
      AttemptInterrupted(reason: String)
      AttemptSuperseded(superseded_by_attempt_index: Int, reason: String)
    }

    pub type RecordedStepOutcome {
      RecordedCompleted
      RecordedFailedContinued
      RecordedFailedFatal
    }

    pub type VerifiedArtifact {
      VerifiedArtifact(
        artifact_ref: String,
        artifact_sha256: String,
        artifact: step_artifact.StepArtifact,
      )
    }

Define the recovery state and output model in the same module. The final plan should never contain a live running state; prepared and started durable statuses are startup crash windows and become interrupted or inspection-needed outputs.

    pub type StepRecoveryState {
      StepUnattempted(step_id: String)
      StepCompleted(step_id: String, attempt_index: Int, artifact: VerifiedArtifact)
      StepFailedContinued(step_id: String, attempt_index: Int, artifact: VerifiedArtifact)
      StepFailedFatal(step_id: String, attempt_index: Int, artifact: VerifiedArtifact)
      StepNeedsInterruptionBeforeStart(
        step_id: String,
        attempt_index: Int,
        workspace_path: String,
      )
      StepNeedsInterruptionAfterStart(
        step_id: String,
        attempt_index: Int,
        workspace_path: String,
        operator_session_id: String,
        external_session_ref: Option(String),
      )
      StepAlreadyInterrupted(step_id: String, attempt_index: Int, reason: String)
      StepSuperseded(step_id: String, attempt_index: Int, reason: String)
    }

    pub type RunRecoveryOutcome {
      Continuable
      DriftBlocked
      NeedsInspection
      InProgressBlocked
      TerminalSucceeded
      TerminalFailed
      TerminalCancelled
      TerminalRecordNeeded
      AlreadyInterrupted
      AlreadySuperseded
    }

    pub type RecoveryPlan {
      RecoveryPlan(
        run_id: String,
        issue_id: String,
        outcome: RunRecoveryOutcome,
        step_states: Dict(String, StepRecoveryState),
        preserved_artifacts: Dict(String, VerifiedArtifact),
        start_steps: List(StartStep),
        blocked_steps: List(BlockedStep),
        interruption_records: List(InterruptionRecordIntent),
        workflow_finish_records: List(WorkflowFinishRecordIntent),
        inspection_requests: List(InspectionRequest),
        park_requests: List(ParkRequest),
        cleanup_run_roots: List(CleanupRunRoot),
        session_recovery_candidates: List(SessionRecoveryCandidate),
        drift_errors: List(DriftError),
        warnings: List(String),
      )
    }

Define output actions as inert data. `StartStep` should contain `step_id`, `workspace_name`, and a reason such as `dependencies_complete_after_startup`. `BlockedStep` should contain `step_id` and a list of dependency step ids or run-level reasons that block it. `InterruptionRecordIntent` should contain `run_id`, `workflow_id`, `step_id`, `attempt_index`, and a typed reason whose string rendering is exactly `daemon_restart_before_step_start` for prepared attempts and `daemon_restart_during_step` for started attempts. `WorkflowFinishRecordIntent` should contain `run_id`, `workflow_id`, `issue_id`, and `WorkflowRunOutcome`; it is emitted when a no-drift active run appears terminal but does not yet have a durable top-level `RunFinished`. `InspectionRequest` should contain `issue_id`, `issue_identifier`, `run_id`, and a typed reason. `ParkRequest` should contain `issue_id`, `issue_identifier`, reason, release policy string, and the current issue fingerprint when one is available. `CleanupRunRoot` should contain `run_id`, `issue_id`, and `run_root`. `SessionRecoveryCandidate` should contain the interrupted agent step id, attempt index, operator session id, optional external session ref, and workspace path; the planner must not try to continue the session. `DriftError` should distinguish workflow id mismatch, workflow fingerprint drift, issue fingerprint drift, issue unavailable, workflow unavailable, recorded attempt for a step missing from the current DAG, attempt run id mismatch, and attempt workflow id mismatch.

Define a `RecoveryPolicy` with conservative defaults.

    pub type RecoveryPolicy {
      RecoveryPolicy(
        allow_starting_ready_pending_steps: Bool,
        park_unsafe_interruptions: Bool,
      )
    }

    pub fn default_policy() -> RecoveryPolicy {
      RecoveryPolicy(
        allow_starting_ready_pending_steps: True,
        park_unsafe_interruptions: True,
      )
    }

Do not add an automatic command rerun policy in this implementation. If a later workflow schema adds an explicit rerun-safe policy, that later change can extend `RecoveryPolicy` and add tests. The current planner must default prepared and started command attempts to inspection or park before any new attempt can be allocated by a future runner.

Implement `pub fn plan_run(input: PlannerInput) -> RecoveryPlan`. It should perform these steps internally. First, compare durable run identity with `input.current`. If the current observation is `IssueUnavailable` or `WorkflowUnavailable`, add the corresponding drift error. If the current observation is available, compare `input.run.workflow_id` with `current.workflow_id`, `input.run.workflow_fingerprint` with `current.workflow_fingerprint`, and `input.run.issue_fingerprint` with `current.issue_fingerprint`. Do not compare per-step prompt fingerprints; prompt contents are part of the whole `workflow_fingerprint` contract.

Second, validate attempt identity. Every `StepAttemptFacts` should have the same `run_id` and `workflow_id` as `input.run`; mismatches become warnings and drift errors and must not be used for scheduling. Attempts that reference a `step_id` not found in `input.dag.steps` become warnings and drift errors. Unknown-step attempts are especially important in drift tests because a changed current DAG may otherwise look terminal by omitting an unfinished historical step.

Third, build a `Dict(String, StepRecoveryState)` by iterating over `input.dag.steps`. For each step, collect valid attempts with the same step id. If there are no attempts, classify as `StepUnattempted`. If there are attempts, sort by `attempt_index` and select the highest attempt. If synthetic test input contains duplicate attempt indexes for the same step, choose the last deterministic value by sorting on a stable textual rendering of the status and add a warning; durable projection code should prevent that situation. Classify `AttemptFinished(RecordedCompleted, ...)` as `StepCompleted`, `AttemptFinished(RecordedFailedContinued, ...)` as `StepFailedContinued`, and `AttemptFinished(RecordedFailedFatal, ...)` as `StepFailedFatal`. Do not inspect the current step's `on_failure` field to decide these outcomes. Classify `AttemptPrepared` as `StepNeedsInterruptionBeforeStart`. Classify `AttemptStarted` as `StepNeedsInterruptionAfterStart`. Classify `AttemptInterrupted` as `StepAlreadyInterrupted`. Classify `AttemptSuperseded` as `StepSuperseded`, which is terminal for the old attempt but never dependency-satisfying.

Fourth, build `preserved_artifacts` from `StepCompleted`, `StepFailedContinued`, and `StepFailedFatal` states. For dependency evaluation, only `StepCompleted` and `StepFailedContinued` count as complete. `StepFailedFatal` makes the run failed and must block all pending downstream work. `StepNeedsInterruptionBeforeStart`, `StepNeedsInterruptionAfterStart`, `StepAlreadyInterrupted`, and `StepSuperseded` are unresolved for dependency purposes and block their downstream dependents.

Fifth, produce interruption intents. For each `StepNeedsInterruptionBeforeStart`, add one `InterruptionRecordIntent` keyed by `(run_id, step_id, attempt_index)` with reason `daemon_restart_before_step_start`. For each `StepNeedsInterruptionAfterStart`, add one `InterruptionRecordIntent` keyed by `(run_id, step_id, attempt_index)` with reason `daemon_restart_during_step`. If the current workflow observation has no workflow drift and the current DAG says the step is an agent step, also add a `SessionRecoveryCandidate` with the operator session fields. For command steps and for unknown kind under drift, add an inspection request; when `park_unsafe_interruptions` is true, add a park request. Agent interruptions should also produce inspection or park by default; the only extra behavior is the session-recovery candidate. The planner must not add a `StartStep` for an interrupted agent or command in this ticket.

Sixth, call the live scheduler only for safe start selection. If any drift error exists, any unresolved prepared, started, interrupted, or superseded state exists, or any fatal state exists, the final plan must contain no `start_steps`. Otherwise, construct a `workflow_scheduler.SchedulerState` whose statuses map `StepUnattempted` to `Pending`, `StepCompleted` to `Succeeded`, and `StepFailedContinued` to `FailedContinued`, with failure policies copied from `input.dag.steps` and `cancelling: False`. Pass that state and `input.dag` to `workflow_scheduler.ready_steps`. Convert the returned workflow steps to `StartStep` values only when `policy.allow_starting_ready_pending_steps` is true. This preserves `dag.max_parallel_steps` and same-workspace selection semantics.

Seventh, determine the run outcome and cleanup behavior. If `input.run.run_status` is `RunFinished(WorkflowCompleted, ...)`, return `TerminalSucceeded`; if it is `RunFinished(WorkflowFailedFatal, ...)`, return `TerminalFailed`; if it is `RunFinished(WorkflowCancelled, ...)`, return `TerminalCancelled`. For any `RunFinished` value, emit one `CleanupRunRoot` when `cleanup_recorded` is false, even if drift exists, because the durable top-level record proves the run was terminal before drift. If `run_status` is `RunInterrupted`, return `AlreadyInterrupted` and emit no cleanup unless a future plan explicitly proves cleanup is safe for interrupted runs. If `run_status` is `RunSuperseded`, return `AlreadySuperseded` and emit no cleanup in this ticket. If `run_status` is `RunActive` and drift exists, return `DriftBlocked` and emit no cleanup. If `run_status` is `RunActive` and every DAG step is completed or failed-continued with no drift, emit `WorkflowFinishRecordIntent(WorkflowCompleted)`, return `TerminalRecordNeeded`, and emit no cleanup yet. If `run_status` is `RunActive` and any step is failed-fatal with no drift, emit `WorkflowFinishRecordIntent(WorkflowFailedFatal)`, return `TerminalRecordNeeded`, and emit no cleanup yet. If unresolved attempts exist, return `NeedsInspection`. If start steps exist, return `Continuable`. Otherwise return `InProgressBlocked` with blocked-step explanations.

Add tests in `test/workflow_recovery_planner_test.gleam`. Use hand-built DAG fixtures as YAML strings parsed through `workflow_dag.parse`, not the repository's current example workflow files. Keep fixture paths repository-relative, such as `test/tmp/workflow-recovery/run-1/workspaces/main/steps/implement-abcdef123456/attempt-1`, and never embed absolute local paths. If a test needs an opaque fingerprint, use strings such as `workflow-fp-a` and `issue-fp-a`, not filesystem paths.

Use these fixture YAML strings unless `workflow_dag.parse` has changed before implementation; if it has changed, update this ExecPlan and record the parser discovery before continuing.

    fn fan_in_yaml() -> String {
      "version: 1
id: review-flow
max_parallel_steps: 3
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    workspace:
      name: main
  - id: test_after_implement
    depends_on: [implement]
    kind: command
    run: gleam test
    on_failure: continue
    workspace:
      name: main
      from: main
  - id: code_review
    depends_on: [implement]
    kind: agent
    prompt: prompts/code-review.md
    workspace:
      name: review
      from: main
  - id: security_review
    depends_on: [implement]
    kind: agent
    prompt: prompts/security-review.md
    workspace:
      name: security
      from: main
  - id: apply_feedback
    depends_on: [test_after_implement, code_review, security_review]
    kind: agent
    prompt: prompts/apply-feedback.md
    workspace:
      name: main
      from: main
"
    }

    fn independent_roots_yaml() -> String {
      "version: 1
id: parallel-roots
max_parallel_steps: 2
steps:
  - id: docs
    kind: agent
    prompt: prompts/docs.md
    workspace:
      name: docs
  - id: tests
    kind: command
    run: gleam test
    workspace:
      name: test
"
    }

    fn same_workspace_roots_yaml() -> String {
      "version: 1
id: same-workspace-roots
max_parallel_steps: 2
steps:
  - id: docs
    kind: agent
    prompt: prompts/docs.md
    workspace:
      name: main
  - id: tests
    kind: command
    run: gleam test
    workspace:
      name: main
"
    }

    fn failure_policy_continue_yaml() -> String {
      "version: 1
id: review-flow
max_parallel_steps: 1
steps:
  - id: test_after_implement
    kind: command
    run: gleam test
    on_failure: continue
    workspace:
      name: main
"
    }

    fn failure_policy_default_yaml() -> String {
      "version: 1
id: review-flow
max_parallel_steps: 1
steps:
  - id: test_after_implement
    kind: command
    run: gleam test
    workspace:
      name: main
"
    }

In the test file, define artifact helpers directly with the public `step_artifact.StepArtifact` constructor so no artifact-store or artifact-limit behavior is involved. A success helper should use `step_artifact.StepSucceeded`, `command_exit_code: Some(0)`, `stdout: "ok"`, empty stderr, `timed_out: False`, both truncation flags `False`, and `summary_text: "success " <> step_id`. A failure helper should use `step_artifact.StepFailed`, `command_exit_code: Some(1)`, empty stdout, `stderr: "failed"`, the same boolean flags, and `summary_text: "failure " <> step_id`. Wrap those in `VerifiedArtifact(artifact_ref: "runs/run-1/" <> step_id <> "/attempt-1.json", artifact_sha256: "sha256-" <> step_id, artifact: artifact)`.

Also define `base_run(step_attempts)` returning `WorkflowRunFacts` with `run_id: "run-1"`, `workflow_id: "review-flow"`, `workflow_fingerprint: "workflow-fp-a"`, `issue_id: "issue-1"`, `issue_identifier: "LIV-55"`, `issue_fingerprint: "issue-fp-a"`, `observed_updated_at_ms: 1000`, `run_root: "test/tmp/workflow-recovery/run-1"`, `cleanup_recorded: False`, `run_status: RunActive`, and the supplied attempts. Define `current_ok()` as `CurrentWorkflowObservation(workflow_id: "review-flow", workflow_fingerprint: "workflow-fp-a", issue_fingerprint: "issue-fp-a")`. For fixtures whose DAG id is not `review-flow`, override the run and current workflow id consistently in the test helper.

## Concrete Steps

1. From the repository root, inspect the working tree before implementation:

       jj status --color=never

   Expect no unrelated source changes. If there are unrelated changes, do not overwrite them; pause and record the situation in this ExecPlan.

2. Confirm this ticket remains standalone. Search `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` for `WorkflowRunStarted` and `StepAttemptPrepared`. If they are absent, proceed exactly as this plan says. If they are present because LIV-54 landed after this plan revision, still do not implement an adapter in this ticket; record the discovery in Surprises & Discoveries and keep adapter work for a later plan.

3. Create `src/scherzo/workflow_recovery_planner.gleam` with the public types, `default_policy`, and a temporary safe `plan_run` implementation that returns a `RecoveryPlan` with no `start_steps`, no cleanup, and enough fields populated to compile.

4. Create `test/workflow_recovery_planner_test.gleam` with helpers to parse the fixture YAML strings, build `VerifiedArtifact` values, build `WorkflowRunFacts`, build `StepAttemptFacts`, and assert that a plan has no start steps. Include a helper `step_ids(list_of_start_steps)` for comparing scheduler output by id.

5. Write the first failing classification tests. Add tests that no attempts produce `StepUnattempted`; `AttemptFinished(RecordedCompleted, ...)` produces `StepCompleted`; `AttemptFinished(RecordedFailedContinued, ...)` produces `StepFailedContinued`; `AttemptFinished(RecordedFailedFatal, ...)` produces `StepFailedFatal`; `AttemptPrepared` produces `StepNeedsInterruptionBeforeStart` plus exactly one interruption intent with reason `daemon_restart_before_step_start`; `AttemptStarted` for a command step produces `StepNeedsInterruptionAfterStart`, exactly one interruption intent with reason `daemon_restart_during_step`, inspection or park data, and no start request; `AttemptInterrupted` produces `StepAlreadyInterrupted` and no duplicate interruption intent; and `AttemptSuperseded` produces `StepSuperseded` and never satisfies dependencies.

6. Run the tests and expect failure because the planner logic is not implemented yet:

       direnv exec . gleam test test/workflow_recovery_planner_test.gleam

   If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command. The expected red-phase result is a non-zero exit with assertion failures in `workflow_recovery_planner_test`.

7. Implement attempt grouping, latest-attempt selection, recorded-outcome classification, artifact preservation, and idempotent interruption intent generation in `src/scherzo/workflow_recovery_planner.gleam`.

8. Run `direnv exec . gleam test test/workflow_recovery_planner_test.gleam` again. Expect the classification tests to pass and any later tests not yet written to be absent.

9. Add recorded-outcome safety tests. Use `failure_policy_continue_yaml()` for a current DAG where `test_after_implement` has `on_failure: continue`, but the durable latest attempt is `AttemptFinished(RecordedFailedFatal, ...)`. Assert that the planner returns `StepFailedFatal`, preserves the artifact, returns no start requests, and does not treat the step as dependency-complete. Use `failure_policy_default_yaml()` for the inverse case with `RecordedFailedContinued` against a default-fail current step, and assert it remains `StepFailedContinued` when workflow fingerprint equality says the current DAG is the same execution definition.

10. Add drift tests. For each of workflow id mismatch, workflow fingerprint mismatch, issue fingerprint mismatch, `IssueUnavailable`, and `WorkflowUnavailable`, set up a pending ready step that would otherwise start. Assert that `plan_run` returns a non-empty `drift_errors` list, `start_steps == []`, no `cleanup_run_roots`, and an inspection or park request for an active run.

11. Add current-DAG removal and cleanup-under-drift tests. Build an active run with a recorded attempt for a step id that is absent from the current DAG and a workflow fingerprint mismatch. Assert that the plan has a missing-step drift error, no start requests, and no cleanup. Then set the same run's `run_status` to `RunFinished(WorkflowCompleted, ...)` and assert that cleanup is emitted when `cleanup_recorded` is false, because the durable top-level terminal record proves the run was terminal before drift.

12. Implement drift comparison, unknown-step detection, and conservative cleanup behavior. Ensure active drifted runs never use the current DAG to decide terminal cleanup.

13. Add multiple-attempt and idempotence tests. Cover a step with attempt `1` failed and attempt `2` completed; a step with attempt `1` completed and attempt `2` started, where the step is unresolved and downstream is blocked; a prepared attempt without an interruption record, where exactly one `InterruptionRecordIntent` is returned; and the same logical attempt represented as `AttemptInterrupted`, where no duplicate intent is returned.

14. Add scheduler parity tests. Build a `workflow_scheduler.SchedulerState` for the fan-out/fan-in fixture where `implement` is `Succeeded` and the remaining dependent steps are `Pending`; assert that the scheduler ready ids match the planner's `start_steps` ids. Add a case where `test_after_implement` is `FailedContinued` and both review steps are `Succeeded`; assert that `apply_feedback` is ready in both scheduler and planner output. Add a fatal case where one dependency is `FailedFatal`; assert both scheduler and planner return no ready starts. Add independent-roots and same-workspace-root fixture cases proving `max_parallel_steps` and same-workspace selection match `workflow_scheduler.ready_steps`.

15. Implement scheduler-backed start selection by reducing safe recovery states to `workflow_scheduler.StepRuntime` and calling `workflow_scheduler.ready_steps` only when there is no drift, no unresolved interruption, and no fatal failure.

16. Add workflow terminal and cleanup tests. Assert that an active all-completed run returns one `WorkflowFinishRecordIntent(WorkflowCompleted)` and no cleanup. Assert that an active run with `StepFailedFatal` returns one `WorkflowFinishRecordIntent(WorkflowFailedFatal)` and no cleanup. Assert that `RunFinished(WorkflowCompleted, ...)`, `RunFinished(WorkflowFailedFatal, ...)`, and `RunFinished(WorkflowCancelled, ...)` each return terminal outcomes and one `CleanupRunRoot` when `cleanup_recorded` is false. Assert that `cleanup_recorded: True` returns no cleanup. Assert that active pending, interrupted, superseded, drift-blocked, and inspection-needed runs return no cleanup.

17. Implement run outcome and workflow finish record generation.

18. Add an adapter-boundary guard test or compile check. The test should not import `scherzo/state/projection` or `scherzo/state/recovery`. The acceptance condition is that `workflow_recovery_planner` compiles and tests pass as a standalone pure module. Record in the test module comments that real projection adapter tests belong to the later LIV-54 integration ticket and must cover prepared, started, finished, interrupted, superseded, missing artifact, corrupt artifact, and top-level run terminal records.

19. Run formatting and tests from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expect both commands to exit with status 0. The test command should report no failures.

20. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections with actual implementation evidence.

21. Commit after the tree is green. A suitable commit message is `Add workflow startup recovery planner`.

## Testing and Falsifiability

The planner's safety claims are falsifiable through focused unit tests in `test/workflow_recovery_planner_test.gleam`. The tests must exercise durable-contract-shaped inputs, not old issue-level run records and not production daemon startup.

Classification tests must prove these cases. With no attempts for `implement`, `plan_run` returns `StepUnattempted("implement")`. With a latest `AttemptFinished(RecordedCompleted, verified_success_artifact, ...)` for `implement`, it returns `StepCompleted` and `preserved_artifacts` contains `implement`. With a latest `AttemptFinished(RecordedFailedContinued, verified_failure_artifact, ...)`, it returns `StepFailedContinued`, preserves the failed artifact, and treats the step as dependency-complete. With a latest `AttemptFinished(RecordedFailedFatal, verified_failure_artifact, ...)`, it returns `StepFailedFatal`, no start requests, and no downstream dependency satisfaction. With a latest `AttemptPrepared`, it returns `StepNeedsInterruptionBeforeStart` and exactly one interruption intent keyed by `(run-1, implement, 1)`. With a latest `AttemptStarted`, it returns `StepNeedsInterruptionAfterStart`, an interruption intent, inspection or park data, and no start request. With a latest `AttemptInterrupted`, it returns `StepAlreadyInterrupted` and no new interruption intent. With a latest `AttemptSuperseded`, it returns `StepSuperseded` and never unblocks downstream work.

Recorded-outcome tests must prove that finished-step outcome is not recomputed from the current DAG. A recorded `failed_fatal` attempt viewed against a current step whose YAML says `on_failure: continue` must remain `StepFailedFatal` and must not unblock downstream work. A recorded `failed_continued` attempt viewed against a current default-fail step must remain `StepFailedContinued` when workflow id and workflow fingerprint match. If workflow fingerprint does not match, the run must be drift-blocked and must not start or clean up unless a durable `RunFinished` exists.

Drift tests must prove that no silent continuation is possible. Each drift case should set up a pending ready step that would otherwise start, then change exactly one current observation. The assertions are `start_steps == []`, `cleanup_run_roots == []` for active runs, `drift_errors` contains the expected constructor, and the plan outcome is drift-blocked or needs-inspection. The cases are workflow id mismatch, workflow fingerprint mismatch, issue fingerprint mismatch, issue unavailable, workflow unavailable, attempt run id mismatch, attempt workflow id mismatch, and recorded attempt for a step not present in the current DAG.

Cleanup tests must prove that run roots are cleaned only once and only with a durable terminal run record. Active runs with all steps complete should emit `WorkflowFinishRecordIntent` and no cleanup. `RunFinished(WorkflowCompleted, ...)`, `RunFinished(WorkflowFailedFatal, ...)`, and `RunFinished(WorkflowCancelled, ...)` should emit cleanup when `cleanup_recorded` is false and no cleanup when it is true. Active runs with drift, pending, prepared, started, already-interrupted, superseded, or inspection-needed state should not request cleanup. A drifted active run whose current DAG omits an old recorded unfinished step must not be cleaned; the same drifted run with durable `RunFinished` may be cleaned because the top-level terminal fact proves the run ended before drift.

Multiple-attempt tests must prove that latest durable attempts are used and source interruptions are not double-counted. A step with attempts `1` failed and `2` completed must be completed from attempt `2`. A step with attempt `1` completed and attempt `2` started must be unresolved from attempt `2`, must not unblock downstream, and must not use attempt `1` as the dependency artifact. A prepared or started attempt must produce exactly one interruption intent. The same source attempt represented as `AttemptInterrupted` must produce zero new interruption intents.

DAG and scheduler parity tests must use explicit fixtures. In the fan-out/fan-in fixture, completed `implement` should make `test_after_implement`, `code_review`, and `security_review` ready subject to capacity and workspace constraints, and the planner's `start_steps` ids must equal `workflow_scheduler.ready_steps` ids for the equivalent scheduler state. A failed-continued `test_after_implement` plus completed review steps should make `apply_feedback` ready in both scheduler and planner output. An interrupted `code_review` should block `apply_feedback`, and the blocked-step explanation should mention `code_review`. A fatal failure in any dependency should produce no ready downstream work. In the independent-roots fixture, two roots in different workspaces should both be returned when `max_parallel_steps` is at least `2`, proving the planner is not assuming a single current dogfood workflow. In the same-workspace-roots fixture, the planner should select exactly the same subset as `workflow_scheduler.ready_steps`.

Artifact-boundary tests are intentionally pure. They should construct `VerifiedArtifact` directly and assert that terminal attempts preserve `artifact_ref`, `artifact_sha256`, and `step_artifact.StepArtifact` in the output. They should not test missing or corrupt artifact files because this ticket has no artifact store adapter. The later adapter ticket must add tests proving missing and corrupt artifact refs fail before `plan_run` is called.

The non-functional safety claim is that the planner prevents unsafe automatic continuation. It is disproved if any test creates workflow drift, issue drift, a prepared attempt, a started command attempt, an interrupted attempt, a superseded attempt, or a recorded fatal failure and still receives a `StartStep` for that run. Add a helper assertion named `assert_no_start_steps(plan)` and use it in every unsafe test.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Accept the implementation only when both commands exit with status `0`. If direnv is unavailable but Gleam is on the path, `gleam format --check src test` and `gleam test` are acceptable fallbacks; record the fallback in Surprises & Discoveries.

Behavioral acceptance for the module is as follows. Given durable-contract-shaped run facts with unattempted, prepared, started, finished completed, finished failed-continued, finished failed-fatal, interrupted, and superseded attempts, `plan_run` returns explicit states for each and does not collapse them into a generic retry. Given any mismatch in workflow id, workflow fingerprint, or issue fingerprint for an active run, `plan_run` returns no start request and no cleanup request. Given a recorded `failed_fatal` finished attempt, `plan_run` preserves fatal behavior even if the current DAG would now say `on_failure: continue`. Given multiple attempts for the same step, the latest attempt index controls dependency status and the same interrupted source attempt is not counted twice across repeated startups. Given explicit parallel fixture DAGs, planner start selection matches `workflow_scheduler.ready_steps`. Given an active terminal-looking run without durable `RunFinished`, the planner emits a workflow finish record intent and no cleanup. Given a durable `RunFinished` without a cleanup marker, the planner emits a cleanup request; with a cleanup marker, it does not.

Compatibility acceptance for this standalone ticket is negative and explicit: `src/scherzo/workflow_recovery_planner.gleam` must not import `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, or `src/scherzo/orchestrator/daemon.gleam`; no daemon startup behavior changes; no ledger records are appended; no artifact files are read; and no adapter from a real durable projection is implemented. The future adapter work remains separately testable once LIV-54 source modules exist.

## Rollout, Recovery, and Idempotence

The implementation is additive. It adds a pure module and tests but does not change the daemon, runner, workflow YAML schema, ledger replay, Linear calls, artifact store, or workspace deletion. If the implementation is wrong, reverting the planner module and its tests returns the repository to prior behavior because no production path depends on it yet.

All output actions are idempotent data. `InterruptionRecordIntent` is keyed by `(run_id, step_id, attempt_index)`, so a later ledger append can dedupe by source attempt. `WorkflowFinishRecordIntent` is keyed by `run_id` and emitted only for active no-drift runs that appear terminal and lack a durable `RunFinished`. `CleanupRunRoot` is keyed by `run_id` and run root and is emitted only when `cleanup_recorded` is false and `run_status` is already `RunFinished`. `ParkRequest` should include the current issue fingerprint when available and a stable reason so a later integration can avoid repeated park comments or state churn. `StartStep` is emitted only for unattempted pending steps selected by `workflow_scheduler.ready_steps`; completed, failed-continued, failed-fatal, prepared, started, interrupted, and superseded steps are never emitted as start requests.

Repeated calls to `plan_run` with the same input must return the same output. Repeated calls after applying `StepAttemptInterrupted` records and cleanup markers must return no duplicate interruption or cleanup requests. Drift-blocked and inspection-needed outputs are safe to recompute because they do not execute work.

## Artifacts and Notes

The following repository facts guided this plan:

    src/scherzo/workflow_scheduler.gleam
    # Defines Pending, Running, Succeeded, FailedContinued, FailedFatal.
    # workflow_scheduler.ready_steps owns max_parallel_steps and same-workspace selection.

    src/scherzo/step_artifact.gleam
    # Defines public StepArtifact and StepStatus constructors.

    src/scherzo/state/record.gleam
    # Current source still has pub const schema_version = 1.

    docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md
    # Defines future workflow checkpoint records and artifact verification contract.

Self-review checklist for this plan: it defines pure inputs and outputs, mirrors durable prepared/started/finished/interrupted/superseded facts, consumes recorded finished outcomes, defaults interrupted commands to inspection or park, blocks continuation on workflow or issue drift, avoids cleanup under drift unless a durable top-level finish record exists, handles multiple attempts and repeated startup idempotence, requires scheduler parity tests, keeps pi session continuation and projection adapters out of scope, and requests cleanup only for durable terminal run roots.

## Interfaces and Dependencies

The main interface is the new module `src/scherzo/workflow_recovery_planner.gleam`. It should export `PlannerInput`, `WorkflowRunFacts`, `DurableRunStatus`, `WorkflowRunOutcome`, `CurrentWorkflowObservation`, `StepAttemptFacts`, `DurableStepAttemptStatus`, `RecordedStepOutcome`, `VerifiedArtifact`, `StepRecoveryState`, `RunRecoveryOutcome`, `RecoveryPlan`, output action types, `RecoveryPolicy`, `default_policy`, and `plan_run`.

The module should depend on existing in-repository pure modules only: `scherzo/workflow_dag` for DAG and step types, `scherzo/workflow_scheduler` for ready-step selection, and `scherzo/step_artifact` for terminal artifacts. No package dependency should be added.

Do not add a `from_projection` function in this ticket. When the durable schema version `2` projection exists in source, a later plan should add an adapter whose tests cover `StepAttemptPrepared`, `StepAttemptStarted`, `StepAttemptFinished` with each recorded outcome, `StepAttemptInterrupted`, `StepAttemptSuperseded`, missing artifact, corrupt artifact, changed workflow fingerprint, changed issue fingerprint, current workflow unavailable, current issue unavailable, and top-level `WorkflowRunFinished`. That adapter must verify `artifact_ref` and `artifact_sha256` before constructing `VerifiedArtifact`.

No external services are called. No files are read. No commands are run by the planner. Tests may use parsed YAML strings and direct artifact constructors but should not require network, Linear credentials, pi, actual workspace directories, or durable artifact files.

## Open Questions and Clarifications Needed

None.
