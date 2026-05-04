# Add recovery-aware workflow runner execution

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can restart the Scherzo daemon while a YAML workflow run is in progress and Scherzo will continue that same logical workflow run from durable step checkpoints instead of starting from the beginning. A completed upstream step will not run a second time after restart. Its full durable artifact, including failed artifacts for steps whose `on_failure` policy allows continuation, will be restored and made available to downstream prompt rendering. Pending steps will run once their dependencies are satisfied, and interrupted steps will follow the recovery planner's safety decision until pi session continuation exists.

The observable result is that a daemon restart no longer discards already completed workflow work. The operator will see recovered work continue under the original workflow run id and run root. A recovered worker process will get a distinct operator-visible worker process session id, while preserving the original run id in worker metadata and logs. Any new attempt for an interrupted step gets a distinct, operator-visible YAML step session id that identifies the attempt.

## Problem Framing and Constraints

Today `src/scherzo/workflow_run.gleam` executes a workflow DAG with all scheduler state and step artifacts in memory. If the daemon exits after step A completed and before step B completed, the in-memory `artifacts` dict, the scheduler statuses, and the prepared workspace map are gone. `src/scherzo/orchestrator/daemon.gleam` already has startup recovery for the general daemon ledger: it replays the ledger, calls `scherzo/state/recovery.plan`, schedules recovered retry timers, enqueues cleanup workspaces, and replays outbox entries. It does not yet pass recovered workflow step state into a new workflow worker.

The operator problem is data loss and duplicated work. A workflow step can be expensive, can make visible comments or file changes, or can produce an artifact that downstream prompts need. Restarting the daemon should not rerun terminal steps merely because the runner forgot its in-memory state.

This plan is constrained by three important facts. First, this is a planning ticket only; implementation must happen later. Second, backward compatibility with local state written before workflow resumption is not required. If existing local ledger or artifact data is in the old run-only shape, the implementation may reset or reject that data according to the new schema policy rather than migrate it. Third, pi session continuation does not exist yet. An interrupted agent step cannot attach to an old pi process; it can only be retried as a new step attempt when the recovery planner says that is safe.

Implementation must not rely on memory of earlier plans. If durable workflow checkpoint interfaces already exist when this plan is implemented, normalize them to the concrete interfaces named in this plan before runner hydration begins. If they do not exist, Milestone 0 below creates them first: durable step attempt start and finish records, full-shape artifact storage, a stable workflow DAG fingerprint, and pure recovery actions that classify each recovered workflow run before any side effect is resumed.

## Strategy Overview

The right-sized approach is to keep the workflow runner as the owner of step scheduling, but make its initial state injectable from durable recovery. The runner already knows how to compute ready steps, render prompts from a step artifact map, prepare workspaces, execute parallel batches, apply failure policy, and clean up the run root. Instead of building a second recovery executor, add a `workflow_run.RunContext` that can be either fresh or recovered, and teach `workflow_scheduler` to initialize from recovered terminal and rerunnable step statuses.

Startup recovery in `src/scherzo/orchestrator/daemon.gleam` should thread recovered workflow run contexts through the same lifecycle surfaces used by fresh dispatch: startup recovery, optional pending claim retry, worker spawn, workflow execution, YAML step session registration, worker finish, and cleanup. The daemon should keep the original `run_id` and logical workflow run root for resumed executions. It should create a new operator-visible worker process session for the resumed process by using a recovered-session id derived from the original `run_id` and a newly reserved process sequence; it must not pretend that reserving a sequence changes identity while `make_session_id` still returns only `run_id`. Individual YAML step sessions must include attempt identity by deriving ids from `run_id`, `step_id`, and the durable attempt number.

The recovery planner remains the safety authority. The runner should not decide whether an interrupted command can be rerun or whether a corrupt artifact can be ignored. The runner should consume a pure recovered context that has already classified steps as terminal, pending, rerunnable with a next attempt number, or blocked/abandoned. This keeps side-effect safety policy centralized while keeping runner changes focused on hydration and execution.

Attempt identity is a durable lifecycle, not just a string formatting change. Before a recovered or fresh YAML step invokes a command or agent dependency, the daemon-backed runner dependency must append a durable `WorkflowStepAttemptStarted` record. After the step returns, it must write the full `step_artifact.StepArtifact` to the workflow artifact store and append `WorkflowStepAttemptFinished` with the same attempt number and artifact reference. If the daemon crashes after started attempt `N` and before a finish record, the next recovery decision must either abandon the step or schedule attempt `N + 1`; it must never reuse attempt `N` or the same step session id.

## Alternatives Considered

The simplest plausible alternative is to rerun the whole workflow after daemon restart. That would be easy to implement but fails the central operator requirement: completed terminal steps must be skipped, and durable artifacts from those steps must feed downstream prompt rendering.

Another alternative is to make the daemon manually skip and run YAML steps during startup recovery without using `src/scherzo/workflow_run.gleam`. That duplicates scheduler, workspace, artifact, failure policy, parallelism, and cleanup logic. It increases the chance that recovered runs behave differently from fresh runs.

A third alternative is to block all workflow recovery until pi session continuation exists. That is safer for interrupted agent sessions but too conservative. Many useful recoveries do not need pi continuation: terminal steps can be skipped, pending steps can run, failed-continued artifacts can be rendered downstream, and safe interrupted steps can be retried as new attempts.

The selected design is smaller and safer: hydrate the existing runner, preserve durable policy decisions, and add only the daemon plumbing needed to spawn a recovered worker.

## Risks and Countermeasures

A corrupt or missing artifact could cause downstream prompts to render with incomplete data. The countermeasure is to fail closed during startup planning. Any terminal step whose artifact is needed to mark the step complete must have a decodable full-shape `step_artifact.StepArtifact` loaded through `workflow_checkpoint.read_step_artifact`. If the artifact is missing, corrupt, or missing representative fields such as status, command result details, stdout/stderr truncation flags, diagnostic path, or summary text, startup recovery must not pass a partial artifact to the runner. It should record a recovery warning and use the planner's abandonment policy, which this plan defines as parking the issue for operator intervention without rerunning potentially side-effecting completed steps.

A missing workspace could cause a resumed downstream step to run in the wrong filesystem state. The countermeasure is to validate the original run root, every recovered prepared workspace record, and any recovered source workspace before spawning a worker. Each restored `workspace_run.PreparedStepWorkspace` must have the same workflow id, run id, and run root as the recovered context, must match the workspace name from the current DAG, must normalize under the configured workspace root, and must exist on disk when it represents terminal upstream state used as a source. If a pending or rerunnable step depends on a source workspace whose recovered record or directory is missing, abandon and park the run. If only the target workspace for a not-yet-run step is missing, normal recovered preparation may create it because that workspace has not produced terminal state yet.

A workflow file might change between the original run and restart. The countermeasure is to persist and compare a workflow identity and stable DAG fingerprint in the durable checkpoint schema. The fingerprint must be computed by `src/scherzo/workflow_fingerprint.gleam` from the parsed DAG, not from raw YAML bytes, so comments, YAML formatting, and map ordering do not change it. Semantically meaningful changes such as different dependencies, commands, prompts, workspace refs, hooks, failure policies, or parallelism must change the fingerprint. If the selected workflow id or fingerprint differs from the recovered context, do not resume the run. Park the issue with a `workflow_drift` reason so an operator can decide whether to retry from scratch under the new workflow.

An issue might drift in Linear while the daemon is down. The countermeasure is to use the refreshed issue from startup recovery for prompt rendering and final reporting only if it has the same issue id and still routes to the same workflow id. If it no longer routes to the same workflow or is no longer dispatchable under current policy, abandon and park instead of silently continuing under a different workflow.

An interrupted command step may have partially completed unsafe side effects. The countermeasure is to treat interrupted command steps as unsafe to rerun unless the recovery planner explicitly marks them rerunnable under durable policy. If no such policy metadata exists for the step, the planner must abandon and park the run. Interrupted agent steps may be retried as new attempts only because pi session continuation is unavailable and the retry starts from restored upstream artifacts.

Recovered hook preflight can fail after restart even when it passed before. The countermeasure is to split workspace preparation so fresh preparation may keep the current cleanup-on-error behavior, but `prepare_recovered_step` never deletes the recovered run root when create or before-step hooks fail. Recovered preparation returns a normal error, the worker records and reports the failure, and cleanup is skipped unless a new step attempt has actually started or the workflow was already terminal at hydration.

Recovered deadlocks and preflight failures must not delete diagnostic state before new side effects occur. The runner must carry a `cleanup_allowed` flag. Fresh runs start with cleanup allowed. Recovered runs start with cleanup disallowed unless the scheduler is already terminal at hydration. The flag flips to allowed only after a new step attempt start record has been durably appended and the command or agent dependency is about to run. Failure paths such as scheduler deadlock, recovered workspace validation failure, and hook preflight failure must return the recovered run root in the failure value but must not call `cleanup_run` while `cleanup_allowed` is false.

Repeated attempts for the same step can confuse operators if they share the same session id or durable attempt number. The countermeasure is to append `WorkflowStepAttemptStarted` before side effects, append `WorkflowStepAttemptFinished` after artifact persistence, and compute recovered `next_attempt` from both started and finished records. If attempt `2` started and the daemon crashed before finish, the next safe retry is attempt `3`, not another attempt `2`.

Top-level recovered workers can confuse operators if the process session id is reused accidentally. The countermeasure is to keep `run_id` as the logical workflow id and use a separate recovered worker process session id such as `run_id <> "-resume-" <> int.to_string(session_sequence)`. Fresh worker session ids may remain equal to `run_id` for compatibility. Recovered worker tests must assert both facts: the logical run id is preserved and the process session id is distinct.

## Progress

- [x] (2026-05-03 00:00Z) Drafted this ExecPlan from the Linear ticket and a targeted inspection of the current runner, daemon, workspace, scheduler, and state recovery paths.
- [x] (2026-05-03 01:00Z) Incorporated adversarial review findings about durable recovery interfaces, attempt lifecycle, recovered cleanup safety, worker session identity, workspace filesystem validation, and test visibility.
- [ ] Normalize or create durable workflow checkpoint interfaces, artifact storage, DAG fingerprinting, and pure recovery action types.
- [ ] Implement recovery context types and scheduler hydration.
- [ ] Thread recovered context through daemon startup, pending claims, worker spawn, and workflow execution.
- [ ] Add durable attempt lifecycle records and attempt-aware YAML step sessions.
- [ ] Add recovery tests for skipped terminal steps, failed-continued artifacts, parallel interruption, unique attempts, mid-attempt crash recovery, filesystem validation, and unsafe recovery conditions.
- [ ] Run formatting and test validation.

## Surprises & Discoveries

- Observation: The current runner already stores enough in-memory shape to make hydration straightforward: a scheduler state, an artifact dict, prepared workspaces, a run root, token totals, and final issue state are threaded through the `loop` function in `src/scherzo/workflow_run.gleam`.
  Evidence: `workflow_run.execute` calls `loop` with `workflow_scheduler.init(dag)`, empty artifact and workspace dicts, `run_root: None`, and zero token totals.

- Observation: YAML step session ids currently do not include attempt identity.
  Evidence: `run_yaml_command_step` and `run_yaml_agent_step` in `src/scherzo/orchestrator/daemon.gleam` derive `session_id` as `run_id <> "-" <> step_id`.

- Observation: Startup recovery currently restores runtime state, retry timers, cleanup requests, outbox replay, command receipts, and warnings, but it does not yet expose recovered workflow runs to the daemon actor initializer.
  Evidence: `StartupRecovery` in `src/scherzo/orchestrator/daemon.gleam` contains `runtime`, `retry_timers`, `cleanup_workspaces`, `outbox_to_replay`, `command_receipts`, and `warnings`.

- Observation: The adversarial review identified that a recovered prepare failure could delete the original recovered run root if `prepare_recovered_step` blindly reused fresh `prepare_step` cleanup-on-error behavior.
  Evidence: The plan now requires an internal preparation helper with an explicit cleanup policy and a recovered preflight failure test that asserts cleanup is not called.

- Observation: The adversarial review identified that top-level worker session sequence reservation is ineffective if `make_session_id` continues to return only `run_id`.
  Evidence: The plan now requires a recovered worker process session id that includes the newly reserved session sequence while preserving the logical `run_id` separately.

## Decision Log

- Decision: Hydrate the existing workflow runner rather than build a separate recovery runner in the daemon.
  Rationale: `src/scherzo/workflow_run.gleam` already owns DAG scheduling, workspace preparation, parallel batch execution, prompt rendering, failure policy, token aggregation, and cleanup. Duplicating those behaviors in startup recovery would create two inconsistent execution engines.
  Date: 2026-05-03

- Decision: Keep the original `run_id` and run root for recovered runs unless the pure recovery planner returns an abandonment decision.
  Rationale: The acceptance criteria require recovered executions to keep the same logical run root unless the plan explicitly chooses abandonment. Keeping the original run root preserves workspace continuity and makes durable artifacts correspond to the same run.
  Date: 2026-05-03

- Decision: Treat missing/corrupt terminal artifacts, workflow drift, issue drift, missing required source workspaces, and unsafe interrupted command steps as abandon-and-park conditions.
  Rationale: Rerunning completed or unsafe work can duplicate side effects, and rendering prompts with partial artifacts can mislead downstream agents. Parking is safer and reversible by an operator.
  Date: 2026-05-03

- Decision: Put attempt identity into durable step attempt records and operator-visible YAML step session ids.
  Rationale: A string-only attempt parameter is not enough. A crash after attempt startup but before terminal artifact persistence must not let recovery reuse the same attempt number or session id.
  Date: 2026-05-03

- Decision: Preserve recovered run roots on recovered preflight, validation, and deadlock failures until a new attempt has started or the recovered workflow is already terminal.
  Rationale: Operators may need the original run root to diagnose recovery failures. Deleting it before the failure is recorded and reported is the unsafe outcome this recovery work is intended to avoid.
  Date: 2026-05-03

- Decision: Use a distinct recovered worker process session id while preserving the original logical `run_id`.
  Rationale: Current fresh session ids may remain compatible, but a recovered worker process needs a distinct operator target. The recovered id must include a newly reserved sequence because `run_id` alone cannot distinguish processes.
  Date: 2026-05-03

- Decision: Put YAML step session id formatting in a small public module rather than a private daemon helper.
  Rationale: The id contract is safety-critical and should be tested directly without reaching into private functions.
  Date: 2026-05-03

- Decision: Use a code-level workflow recovery mode for rollback instead of adding a new operator configuration surface in this plan.
  Rationale: The immediate safety requirement is that production can be patched to park recovered workflows instead of spawning them. The implementation will expose `WorkflowRecoveryMode` to tests and startup wiring, and production will pass `ResumeRecoveredWorkflows` by default.
  Date: 2026-05-03

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam application that dispatches Linear issues to workers. A daemon is the long-running process in `src/scherzo/orchestrator/daemon.gleam`. A workflow run is a YAML-defined set of steps selected for an issue. A step can be a command step, which runs a shell command through `src/scherzo/command_step.gleam`, or an agent step, which sends a prompt to pi through `src/scherzo/agent/run_attempt.gleam`. A step artifact is a structured record from `src/scherzo/step_artifact.gleam` that captures the outcome and data from a step. Downstream prompts render these artifacts through `step_artifact.to_template_locals`.

The runner lives in `src/scherzo/workflow_run.gleam`. Its public `execute` function receives a `domain.Issue`, a `workflow_dag.WorkflowDag`, a `domain.OrchestratorConfig`, a tracker client, secrets, a `run_id`, and a `workflow_run.Dependencies` record. It then loops until `workflow_scheduler.outcome` reports success or failure. In each loop, it asks `src/scherzo/workflow_scheduler.gleam` for ready steps, prepares their workspaces through `src/scherzo/workspace_run.gleam`, runs the prepared batch concurrently, inserts returned artifacts into the artifact dict, marks each step finished in the scheduler, runs after-step hooks, and repeats.

The scheduler lives in `src/scherzo/workflow_scheduler.gleam`. Its runtime statuses are `Pending`, `Running`, `Succeeded`, `FailedContinued`, and `FailedFatal`. `init` currently creates a fresh state where every step is `Pending`. `ready_steps` returns pending steps whose dependencies are complete and whose workspaces are not blocked by a running step. A dependency is complete when its status is `Succeeded` or `FailedContinued`.

Workspace preparation lives in `src/scherzo/workspace_run.gleam`. `PreparedStepWorkspace` contains `workflow_id`, `run_id`, `run_root`, `workspace_name`, `path`, and optional source workspace fields. `prepare_step` computes paths from the workflow id, issue identifier, run id, and workspace name, creates the run root, runs create and before-step hooks, and returns a prepared workspace. `cleanup_run` deletes a run root unless the retention marker `.scherzo-keep-workspace` exists.

Daemon startup recovery is in `src/scherzo/orchestrator/daemon.gleam`. `start` loads the runtime bundle, creates clients, calls `load_startup_recovery`, starts the control plane and effect runner, schedules recovered retry timers, enqueues recovered cleanup workspaces, replays outbox entries, and logs recovery warnings. `load_startup_recovery` gets the ledger path from `src/scherzo/state/ledger.gleam`, replays it, refreshes known issue states, calls `src/scherzo/state/recovery.gleam` to plan recovery, appends records the planner requested, and returns a `StartupRecovery` value.

Fresh dispatch in the daemon uses `dispatch_issue_with_continuation`. It computes a workspace path for the issue, reserves a `session_sequence` through `src/scherzo/orchestrator/worker_registry.gleam`, creates a `run_id`, stores a `PendingClaim`, and enqueues an `effect_runner.ClaimIssue` side effect. When the claim succeeds, the daemon calls `spawn_worker`, which registers a worker session in `src/scherzo/session/hub.gleam`, applies worker-start runtime state, spawns `run_workflow_worker`, monitors it, and stores a `worker_registry.WorkerHandle`.

`run_workflow_worker` selects the workflow from the runtime bundle and calls `workflow_run.execute`. It wraps the runner dependencies with `yaml_workflow_dependencies`, so every YAML step gets an operator-visible session. `run_yaml_command_step` and `run_yaml_agent_step` currently create step session ids as `run_id <> "-" <> step_id`; this is insufficient for multiple attempts.

The durable state modules currently include run-level ledger records such as `record.RunStarted`, `record.RunFinished`, and `record.RunInterrupted` in `src/scherzo/state/record.gleam`, and a run projection in `src/scherzo/state/projection.gleam`. This plan does not assume a hidden prior design for workflow step checkpoints. If durable workflow step checkpoints and full-shape artifact storage are absent, Milestone 0 creates them; if equivalent interfaces already exist, normalize them to the interfaces in this plan before wiring the runner.

## Preconditions and Verified Facts

The implementation should begin from the repository root. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through `direnv exec .`.

The following facts were verified in the current tree before this plan was written:

- `src/scherzo/workflow_run.gleam` exposes `WorkflowRunSuccess`, `WorkflowRunFailure`, `Dependencies`, `default_dependencies`, `execute`, and `failure_report`. `execute` currently initializes a fresh scheduler and empty artifact/workspace state.
- `src/scherzo/workflow_scheduler.gleam` exposes `StepRuntime`, `SchedulerState`, `WorkflowOutcome`, `init`, `ready_steps`, `mark_running`, `mark_finished`, `mark_cancelling`, `outcome`, and `status_of`.
- `src/scherzo/workspace_run.gleam` exposes `PreparedStepWorkspace`, `PrepareError`, `prepare_step`, `after_step`, `cleanup_run`, `cleanup_retention_marker`, and `workspace_path_for`.
- `src/scherzo/orchestrator/daemon.gleam` contains the startup recovery path, dispatch and pending claim path, worker spawn path, workflow worker wrapper, YAML step session registration, worker finish handling, and cleanup side-effect enqueueing.
- `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam` currently model run-level records and projection states including `RunStarted`, `RunFinished`, and `RunInterrupted`.

Before recovered runner execution is wired to the daemon, the repository must have the following concrete durable workflow checkpoint interfaces. If earlier resumption work already added equivalent types under different names, keep those names and add adapters so the daemon-facing and runner-facing shapes below are still available. If the interfaces are absent, implement them in Milestone 0 before changing `workflow_run.gleam`.

`src/scherzo/state/record.gleam` must include durable workflow attempt records with these logical fields:

    WorkflowStepAttemptStarted(
      workflow_id: String,
      workflow_fingerprint: String,
      run_id: String,
      issue_id: String,
      step_id: String,
      attempt: Int,
      workspace_name: String,
      workspace_path: String,
      source_workspace_name: Option(String),
      source_workspace_path: Option(String),
      started_at: String,
    )

    WorkflowStepAttemptFinished(
      run_id: String,
      step_id: String,
      attempt: Int,
      terminal_status: WorkflowStepTerminalStatus,
      artifact_ref: String,
      finished_at: String,
    )

    WorkflowRecoveryDecisionRecorded(
      run_id: String,
      issue_id: String,
      decision: String,
      reason: String,
      recorded_at: String,
    )

The concrete Gleam constructors may follow existing project naming conventions, but the encoded ledger data must contain every field above. `WorkflowStepTerminalStatus` must distinguish at least `StepSucceeded`, `StepFailedContinued`, and `StepFailedFatal`; interrupted in-flight attempts are represented by a started record without a matching finished record.

`src/scherzo/state/workflow_checkpoint.gleam` must expose full-shape artifact IO. Artifact references stored in ledger records must be relative to the configured state root, for example `workflow-artifacts/<run-id>/<step-id>/attempt-<attempt>.json`, never a machine-specific absolute path. The module must expose a writer used after a step returns and a reader used during startup recovery:

    pub type WorkflowStepTerminalStatus {
      StepSucceeded
      StepFailedContinued
      StepFailedFatal
    }

    pub type ArtifactStorageError {
      ArtifactMissing(artifact_ref: String)
      ArtifactCorrupt(artifact_ref: String, reason: String)
      ArtifactWriteFailed(artifact_ref: String, reason: String)
    }

    pub fn artifact_ref(run_id: String, step_id: String, attempt: Int) -> String

    pub fn write_step_artifact(
      state_root: String,
      run_id: String,
      step_id: String,
      attempt: Int,
      artifact: step_artifact.StepArtifact,
    ) -> Result(String, ArtifactStorageError)

    pub fn read_step_artifact(
      state_root: String,
      artifact_ref: String,
    ) -> Result(step_artifact.StepArtifact, ArtifactStorageError)

`read_step_artifact` must decode the same `step_artifact.StepArtifact` shape that fresh execution produces. It must not synthesize a minimal placeholder artifact to satisfy downstream rendering.

`src/scherzo/workflow_fingerprint.gleam` must expose a stable fingerprint function:

    pub fn for_dag(workflow_id: String, dag: workflow_dag.WorkflowDag) -> String

The returned value should be `sha256:<hex>` or an existing project-standard equivalent. The input to the hash must be a canonical representation of the parsed DAG: workflow id, maximum parallelism, every step sorted by step id, step kind, command or prompt template content, dependencies sorted by id, workspace name and source workspace reference, create/before/after hooks in their declared order, `on_failure` policy, and artifact limit settings that affect behavior. Raw YAML comments, whitespace, and map ordering must not affect the fingerprint.

`src/scherzo/state/recovery.gleam` must expose pure recovered workflow actions. The recovery planner must consume the projection, refreshed issue state, workflow fingerprint, artifact reader results, and filesystem validation results, then return one of: resume an already claimed recovered run, retry the Linear claim and then resume, or abandon and park with a reason. It must append or request `WorkflowRecoveryDecisionRecorded` before the daemon spawns a worker or reissues a claim.

## Scope Boundaries

In scope:

- Add or normalize durable workflow checkpoint records, full-shape artifact IO, workflow fingerprinting, recovery decision types, and recovery-mode rollback wiring when they are not already present.
- Modify `src/scherzo/workflow_run.gleam` so execution can start from a recovered context containing terminal artifacts, scheduler statuses, prepared workspace knowledge, attempt counters, the original run id, and the original run root.
- Modify `src/scherzo/workflow_scheduler.gleam` so recovered terminal statuses can initialize scheduler state, and so interrupted steps selected for retry start as `Pending` rather than `Running`.
- Modify `src/scherzo/workspace_run.gleam` so recovered runs can prepare pending or rerunnable steps under an already-known run root without accidentally creating a different run root and without deleting the original run root on recovered preflight failure.
- Modify `src/scherzo/orchestrator/daemon.gleam` so startup recovery can spawn recovered workflow workers directly or after a recovered claim retry, and so worker spawn and YAML step sessions receive attempt-aware context.
- Add `src/scherzo/orchestrator/yaml_step_session.gleam` or an equivalent small public module so the YAML step session id contract is directly testable.
- Modify state recovery modules only as needed to connect the concrete schema and pure decisions to daemon startup. Expected files are `src/scherzo/state/recovery.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/workflow_checkpoint.gleam`, and `src/scherzo/workflow_fingerprint.gleam`.
- Add or update tests under `test/` for durable checkpoint projection, runner hydration, daemon startup recovery wiring, step artifact restoration, parallel interrupted branches, unique step attempt sessions, mid-attempt crash recovery, filesystem workspace validation, cleanup guarding, and unsafe recovery handling.

Out of scope:

- Implementing pi session continuation or attaching to an old pi process.
- Supporting local state written before the workflow resumption schema and reset policy.
- Adding a new user-facing configuration surface solely for rollback. This plan uses an internal `WorkflowRecoveryMode` for code-level rollback and tests.
- Changing Linear issue selection policy except where startup recovery must park or abandon unsafe recovered workflow runs.
- Rewriting the workflow DAG parser beyond adding or consuming recovery metadata and stable fingerprinting.
- Changing the user-visible meaning of fresh workflow dispatch.

## Milestones

Milestone 0 normalizes durable recovery interfaces before any runner behavior changes. At the end, the repository has tested record variants or adapters for workflow step attempt start and finish, full-shape artifact read/write helpers, a stable workflow DAG fingerprint function, and pure recovery action types. This milestone comes first because the recovered runner and daemon plumbing are unsafe if attempt numbers, artifacts, and workflow identity are not durable.

Milestone 1 adds recovered scheduler and runner context without touching daemon startup. At the end, a unit test can call the workflow runner with a recovered context where step A is already terminal, and only step B runs. This proves the core hydration path before daemon complexity is involved.

Milestone 2 preserves run root and workspace preparation for recovered execution. At the end, pending and rerunnable steps prepare under the original run root, source workspaces are reconstructed and filesystem-validated from recovered workspace data, and cleanup cannot delete the recovered run root before a new attempt starts. This proves that recovered runs do not fork into a new logical run directory or destroy diagnostic state prematurely.

Milestone 3 threads recovered context through daemon startup, pending claim handling, worker spawn, and workflow worker execution. At the end, `load_startup_recovery` returns recovered workflow work, the actor initializer either spawns recovered workers or requeues recovered claims, the recovered worker process session id is distinct from the logical run id, and the workflow worker calls the recovered runner entry point.

Milestone 4 adds durable attempt lifecycle and attempt-aware YAML step sessions. At the end, a second attempt for the same `run_id` and `step_id` has a distinct operator-visible session id, all durable started and finished records identify the correct attempt, and a crash after attempt start cannot recover by reusing the same attempt number.

Milestone 5 completes safety handling and tests. At the end, missing/corrupt artifacts, missing workspaces, hook preflight failure, workflow or issue drift, disabled recovery mode, and unsafe interrupted command steps all fail closed with explicit logs or parked issues instead of silently rerunning or rendering partial data.

## Plan of Work

Start by normalizing the durable workflow checkpoint layer. In `src/scherzo/state/record.gleam`, add or adapt ledger variants for `WorkflowStepAttemptStarted`, `WorkflowStepAttemptFinished`, and `WorkflowRecoveryDecisionRecorded` with the fields listed in `Preconditions and Verified Facts`. In `src/scherzo/state/projection.gleam`, project these records by `(run_id, step_id, attempt)` so recovery can distinguish finished attempts from started-but-unfinished attempts. A started attempt without a finished record is an interrupted attempt; the projection must keep the highest started attempt for each step even if the artifact was never written.

Add `src/scherzo/state/workflow_checkpoint.gleam` if an equivalent module does not already exist. Implement `WorkflowStepTerminalStatus`, `ArtifactStorageError`, `artifact_ref`, `write_step_artifact`, and `read_step_artifact` so artifact references are relative to the configured state root. The writer must encode the full `step_artifact.StepArtifact` value returned by fresh execution and return `ArtifactWriteFailed` on IO or encode failure. The reader must fail with `ArtifactMissing` or `ArtifactCorrupt` instead of inventing partial artifacts.

Add `src/scherzo/workflow_fingerprint.gleam` if an equivalent function does not already exist. Implement `for_dag(workflow_id, dag)` by serializing the parsed DAG into a canonical string and hashing that string. Sort unordered collections such as step ids and dependency ids. Preserve order for hooks because hook order is behavior. Include every DAG field that can change scheduling, prompt rendering, workspace selection, hook execution, failure policy, or artifact limits. Do not include raw YAML comments or formatting.

In `src/scherzo/state/recovery.gleam`, expose a pure recovered workflow action shape and a `WorkflowRecoveryMode`:

    pub type WorkflowRecoveryMode {
      ResumeRecoveredWorkflows
      ParkRecoveredWorkflows
    }

    pub type RecoveredWorkflowAction {
      ResumeAlreadyClaimed(RecoveredWorkflowRun)
      RetryClaimThenResume(RecoveredWorkflowRun)
      AbandonAndPark(issue: domain.Issue, run_id: String, reason: String, warnings: List(String))
    }

When `WorkflowRecoveryMode` is `ParkRecoveredWorkflows`, the planner must convert otherwise resumable workflow runs to `AbandonAndPark` with reason `workflow_recovery_disabled`. Production startup should pass `ResumeRecoveredWorkflows`; tests and emergency code rollback can pass `ParkRecoveredWorkflows`.

The recovery planner must validate artifacts, workflow identity, issue routing, and recovered workspaces before returning a resumable action. It must load terminal artifacts through `workflow_checkpoint.read_step_artifact`; compute and compare `workflow_fingerprint.for_dag`; refresh the issue and ensure it still routes to the same workflow id; and validate every recovered prepared workspace against the current DAG, configured workspace root, run id, and run root. It must check that terminal source workspace paths exist on disk. For a started-but-unfinished attempt `N`, it must either abandon for unsafe steps or set the runner context's next attempt for that step to `N + 1`.

In `src/scherzo/workflow_scheduler.gleam`, add a recovered initializer. Keep `init` for fresh runs, but add `pub fn init_with_statuses(dag: workflow_dag.WorkflowDag, recovered: Dict(String, StepRuntime)) -> Result(SchedulerState, String)`. This function should build the normal failure policy dict from the current DAG, then validate that every recovered step id exists in the DAG and every DAG step not present in `recovered` starts as `Pending`. It must reject `Running` in recovered input; the pure recovery planner should translate interrupted work to either `Pending` for rerun, `FailedContinued` or `FailedFatal` for terminal policy, or an abandonment decision before the runner sees it. It must return a clear error string such as `unknown_recovered_step:<step_id>` or `running_recovered_step:<step_id>`.

In `src/scherzo/workflow_run.gleam`, add explicit context types. Use stable names so daemon and tests can depend on them:

    pub type StepAttemptContext {
      StepAttemptContext(step_id: String, next_attempt: Int)
    }

    pub type RecoveredRunContext {
      RecoveredRunContext(
        workflow_id: String,
        workflow_fingerprint: String,
        run_id: String,
        run_root: String,
        scheduler_statuses: Dict(String, workflow_scheduler.StepRuntime),
        artifacts: Dict(String, step_artifact.StepArtifact),
        prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
        step_attempts: Dict(String, Int),
        token_totals: domain.TokenTotals,
        final_issue: Option(domain.Issue),
        turns: Int,
        warnings: List(String),
      )
    }

    pub type RunContext {
      FreshRun(run_id: String)
      RecoveredRun(RecoveredRunContext)
    }

`step_attempts` stores the next attempt number to use for each nonterminal step. For a step that already has a terminal artifact, the value is the last completed attempt and is used only for reporting. For a step that will be retried after interruption, the value is one greater than the highest durable started attempt. The first attempt number is `1`.

Change runner execution so the existing fresh call path remains easy to use. Keep `execute` as a wrapper around a new `execute_with_context`:

    pub fn execute(...) -> Result(WorkflowRunSuccess, WorkflowRunFailure) {
      execute_with_context(..., FreshRun(run_id), dependencies)
    }

    pub fn execute_with_context(
      issue: domain.Issue,
      dag: workflow_dag.WorkflowDag,
      orchestrator: domain.OrchestratorConfig,
      tracker_client: tracker.Client,
      secrets: List(String),
      context: RunContext,
      dependencies: Dependencies,
    ) -> Result(WorkflowRunSuccess, WorkflowRunFailure)

`execute_with_context` should derive the initial `run_id`, `scheduler_state`, `artifacts`, `prepared_workspaces`, `run_root`, token totals, final issue, turns, attempts, and cleanup flag from the context. For `FreshRun`, use the current defaults and `cleanup_allowed: True`. For `RecoveredRun`, call `workflow_scheduler.init_with_statuses`, use the durable artifacts directly, use `Some(context.run_root)`, and start with `cleanup_allowed: False` unless the scheduler outcome is already terminal. If scheduler initialization fails, return `WorkflowRunFailure` with reason `workflow_recovery_invalid:<reason>`, preserved artifacts, `run_root: Some(context.run_root)`, and no failed step id; do not call cleanup for this failure.

Thread attempt identity and cleanup permission through the runner. Add `attempts: Dict(String, Int)` and `cleanup_allowed: Bool` parameters to `loop`, `execute_prepared_steps`, `run_prepared_batch`, `spawn_prepared_steps`, `spawn_prepared_steps_loop`, and `run_step`. When a step is about to run, look up its attempt number with default `1`. Before invoking `command_step` or `agent_step`, call a new dependency `record_step_attempt_started`. If that record append fails, return a workflow failure before side effects begin. Once the start record succeeds, set cleanup permission to true for later failure paths and invoke the step dependency. Include the attempt number in `StepExecutionResult`. After the step returns, persist the artifact and append a matching finish record through `record_step_attempt_finished`; if the finish append fails, return a failure that still preserves the artifact in memory and lets recovery handle the started-but-unfinished durable state on restart.

Extend `workflow_run.Dependencies` so YAML wrappers can observe attempt identity and write durable attempt records. Change `command_step` and `agent_step` dependency signatures to include `attempt: Int`. Add dependencies named `record_step_attempt_started` and `record_step_attempt_finished`. Fresh test defaults may use no-op recorders, but `yaml_workflow_dependencies` in the daemon must append real ledger records and write artifacts through `workflow_checkpoint`.

In `src/scherzo/workspace_run.gleam`, add a recovered preparation path that preserves run root and does not clean up on recovered errors. Split the current preparation implementation into a private helper that accepts `cleanup_on_error: Bool`. Keep `prepare_step` behavior unchanged by passing `True`. Add `prepare_recovered_step` that passes `False`, computes the run root using the same logic as `prepare_step`, compares the normalized computed run root to `expected_run_root`, validates source workspace records and directories before hooks run, and returns a `PrepareError` without deleting `expected_run_root` on mismatch or hook failure.

Update `workflow_run.Dependencies.prepare_step` to keep fresh semantics and add a second dependency function `prepare_recovered_step`. `default_dependencies` should wire it to `workspace_run.prepare_recovered_step`. `prepare_ready_steps` in `workflow_run.gleam` should call `prepare_recovered_step` when the runner has a recovered run root, and `prepare_step` otherwise.

In `src/scherzo/orchestrator/daemon.gleam`, extend startup recovery data with recovered workflow actions from `src/scherzo/state/recovery.gleam`. `load_startup_recovery` should accept or construct `WorkflowRecoveryMode`, call the planner, append requested recovery decision records before any worker or claim side effect, and return the actions in `StartupRecovery`. If appending recovery records fails, startup should fail with `ledger_recovery_append_failed`, as it does today.

Add `recovered_context: Option(workflow_run.RecoveredRunContext)` to `PendingClaim`. Fresh dispatch stores `None`. A recovered run that still needs claim confirmation stores `Some(context)` and reuses the original `run_id`, original run root, and recovered worker workspace path. The pending claim path must not generate a new run id for recovered work.

Add a daemon helper such as `resume_recovered_workflow_runs(state, actions)`. For `ResumeAlreadyClaimed`, call `spawn_worker` with the recovered context and no claim side effect. For `RetryClaimThenResume`, insert a `PendingClaim` carrying the recovered context and enqueue `effect_runner.ClaimIssue` with the original `run_id`. For `AbandonAndPark`, call the same parking path used for operator parking or recovery parking, append/log the planner's reason, and do not spawn a worker. If the issue is already active in `runtime.running`, `registry`, or `pending_claims`, log `recovered_workflow_duplicate_active` and skip the duplicate to avoid double execution.

Change `spawn_worker` to accept a start mode:

    type WorkerStartContext {
      FreshWorker
      RecoveredWorker(workflow_run.RecoveredRunContext)
    }

Fresh callers pass `FreshWorker`. Recovered callers pass `RecoveredWorker(context)`. The worker handle and runtime state must preserve `context.run_id` as the logical run id. The top-level session id must be distinct for recovered worker processes: keep fresh `make_session_id` compatible if needed, but add or modify a helper so recovered workers use `context.run_id <> "-resume-" <> int.to_string(session_sequence)`. Tests must prove that reserving a new sequence changes the recovered process session id.

Change `run_workflow_worker` to accept `start_context: WorkerStartContext`. After selecting the workflow DAG, validate that a recovered context's workflow id and fingerprint match the selected DAG by calling `workflow_fingerprint.for_dag`. If validation fails, return `yaml_worker_failure("workflow_recovery_invalid:workflow_drift", Some(context.run_root), issue)`. If validation succeeds, call `workflow_run.execute_with_context` with `RecoveredRun(context)`. Fresh runs keep calling `FreshRun(run_id)`.

Add `src/scherzo/orchestrator/yaml_step_session.gleam` with:

    pub fn id(run_id: String, step_id: String, attempt: Int) -> String {
      run_id <> "-" <> step_id <> "-a" <> int.to_string(attempt)
    }

Change `yaml_workflow_dependencies`, `run_yaml_command_step`, and `run_yaml_agent_step` to accept the `attempt` integer from the runner and use `yaml_step_session.id`. Publish `StepStarted` with a message that includes the step id and attempt, for example `step_id <> " attempt " <> int.to_string(attempt)`. If the session event payload has a more structured metadata field available from previous work, store attempt there too; otherwise include it in the lifecycle message and durable record.

Update worker finish handling so recovered runs preserve cleanup behavior. `workflow_run.execute_with_context` should return `WorkflowRunSuccess.run_root` as the recovered run root on success. On failure, `WorkflowRunFailure.run_root` should also be the recovered run root. `finish_worker_success` and `finish_worker_failure` can continue to append `record.RunFinished`, report success/failure, and apply runtime transitions. For abandon-and-park decisions that happen before spawn, do not append a normal success or failure report; append the durable recovery abandonment record from the recovery planner and park the issue.

## Concrete Steps

1. From the repository root, inspect the working copy and ensure no unrelated changes are present:

       jj status --color=never

   Expected output should say either `The working copy has no changes.` or list only files intentionally changed for this implementation.

2. In `src/scherzo/state/record.gleam`, add or adapt durable record variants for `WorkflowStepAttemptStarted`, `WorkflowStepAttemptFinished`, and `WorkflowRecoveryDecisionRecorded` with the logical fields named in this plan.

3. In `src/scherzo/state/projection.gleam`, project workflow attempt records by run id, step id, and attempt. Ensure a started record without a matching finish record remains visible as an interrupted attempt.

4. Add tests in the nearest state projection test file under `test/scherzo/state/` proving that a started-only attempt is projected, a finished attempt keeps its terminal status and artifact reference, and the highest started attempt is retained for next-attempt computation.

5. Add `src/scherzo/state/workflow_checkpoint.gleam` or normalize the equivalent existing module. Implement `artifact_ref`, `write_step_artifact`, and `read_step_artifact` with relative artifact references under `workflow-artifacts/<run-id>/<step-id>/attempt-<attempt>.json`.

6. Add artifact IO tests under `test/scherzo/state/` that write a representative `step_artifact.StepArtifact`, read it back, and assert the full shape is preserved. Include status, command or agent details, exit code when applicable, stdout/stderr text, truncation flags if present in the type, diagnostic path, and summary text. Also assert missing and malformed artifact files return typed errors.

7. Add `src/scherzo/workflow_fingerprint.gleam` or normalize the equivalent existing module. Implement `for_dag(workflow_id, dag)` using a canonical representation of the parsed DAG and a stable hash.

8. Add tests for `workflow_fingerprint.for_dag`: YAML comments, whitespace, and map ordering must not change the fingerprint; changing dependencies, command text, prompt text, hooks, workspace refs, failure policy, or maximum parallelism must change it.

9. In `src/scherzo/state/recovery.gleam`, add or adapt `WorkflowRecoveryMode`, `RecoveredWorkflowRun`, and `RecoveredWorkflowAction`. Ensure the planner maps `ParkRecoveredWorkflows` to `AbandonAndPark(..., reason: "workflow_recovery_disabled", ...)`.

10. Add pure recovery tests for artifact errors, workflow drift, issue drift, missing recovered source workspace records, missing source workspace directories, unsafe command interruption, and `ParkRecoveredWorkflows`. Each test should assert `AbandonAndPark` and the expected reason.

11. Add a pure recovery test for the mid-attempt crash window. Feed the projection a started-but-not-finished attempt `2` for a step the policy allows to rerun. Assert that the recovered runner context sets that step's next attempt to `3`. Add a companion unsafe command case that parks instead of rerunning.

12. In `src/scherzo/workflow_scheduler.gleam`, add `init_with_statuses` and private validation helpers. Do not change `ready_steps`, `mark_finished`, or `outcome` yet.

13. In `test/scherzo/workflow_scheduler_test.gleam`, add tests for `init_with_statuses`: unknown step id returns `Error("unknown_recovered_step:<step_id>")`, recovered `Running` returns `Error("running_recovered_step:<step_id>")`, and missing DAG steps default to `Pending`.

14. Run the targeted scheduler tests:

       direnv exec . gleam test --target erlang test/scherzo/workflow_scheduler_test.gleam

   If the project test runner does not accept a single file argument, run the nearest existing targeted command used by this repository, or run `direnv exec . gleam test` and note that the new tests are included. The new tests should fail before implementing `init_with_statuses` and pass after step 12.

15. In `src/scherzo/workflow_run.gleam`, add `StepAttemptContext`, `RecoveredRunContext`, `RunContext`, and `execute_with_context`. Keep the existing `execute` as a fresh-run wrapper.

16. Update `loop` and its helper functions in `src/scherzo/workflow_run.gleam` to carry the attempts dict and `cleanup_allowed` flag. Recovered contexts should start cleanup-disallowed unless already terminal; fresh runs should keep current cleanup behavior.

17. In `test/scherzo/workflow_run_test.gleam`, add `recovered_completed_upstream_step_is_not_rerun_test`. Build a two-step DAG where step `collect` is a command step and step `summarize` depends on `collect`. Create a recovered context where `collect` is `Succeeded` and has a durable success artifact. Use fake dependencies that append each executed step id to a list. Assert that only `summarize` executes, the final artifacts contain both steps, and cleanup receives the recovered run root only after the workflow reaches success.

18. Run the workflow runner test subset or the full suite. Expect the new test to fail before the runner hydration is complete and pass after steps 15 and 16.

19. In `src/scherzo/workflow_run.gleam`, change `Dependencies.command_step` and `Dependencies.agent_step` to accept `attempt`. Add `record_step_attempt_started` and `record_step_attempt_finished` dependencies. Update `default_dependencies`, `run_step`, `spawn_prepared_steps_loop`, and all test fakes to pass and receive the attempt number.

20. Add runner tests proving durable attempt ordering. One test should assert that `record_step_attempt_started` is called before the fake command or agent dependency. Another should make `record_step_attempt_started` fail and assert the command or agent dependency is not called.

21. In `test/scherzo/workflow_run_test.gleam`, add `failed_continued_artifact_is_restored_for_downstream_prompt_test`. Build step `lint` as a failed command step with `on_failure: continue`, and step `repair` as an agent step whose prompt references `lint` artifact fields. Recover `lint` as `FailedContinued` with a full failed artifact containing stderr such as `lint failed`, exit code, truncation flags, diagnostic path, and summary. Assert that `lint` is not rerun and the captured `repair` prompt includes the failed artifact content.

22. In `test/scherzo/workflow_run_test.gleam`, add `parallel_recovery_runs_only_interrupted_branch_test`. Build a DAG with `max_parallel_steps` at least `2`, branch step `docs` already `Succeeded`, branch step `tests` recovered as `Pending` with next attempt `2`, and join step `final` depending on both. Assert that `docs` is not executed, `tests` executes with attempt `2`, and `final` executes only after `tests` finishes.

23. In `src/scherzo/workspace_run.gleam`, split preparation internals so cleanup-on-error is explicit. Keep fresh `prepare_step` behavior unchanged and add `prepare_recovered_step` with cleanup-on-error disabled.

24. Add tests in `test/scherzo/workspace_run_test.gleam` or `test/scherzo/workflow_run_test.gleam` for recovered preparation. Assert run-root mismatch fails, recovered before-step hook failure does not call cleanup, missing source workspace record fails, and a recovered source workspace whose directory is absent fails before hooks run. Use placeholder strings or relative fixture paths; do not hard-code an absolute local path.

25. Update `workflow_run.Dependencies` to include `prepare_recovered_step`, and update `prepare_ready_steps` to call it whenever the runner has a recovered run root. Add a runner test where recovered scheduler deadlock or recovered preflight failure occurs before any new step starts; assert `cleanup_run` is not called and the failure retains the recovered run root.

26. Commit the green checkpoint, runner, and workspace hydration work. Suggested commit message: `Add recovered workflow runner context`.

27. In `src/scherzo/orchestrator/daemon.gleam`, add recovered workflow actions to `StartupRecovery`, pass `WorkflowRecoveryMode.ResumeRecoveredWorkflows` from production startup, set the field in `load_startup_recovery`, and call `resume_recovered_workflow_runs` during actor initialization after the event hub and effect runner are started.

28. Add `recovered_context` to `PendingClaim`, update fresh dispatch to store `None`, and update recovered claim retry to store `Some(context)`. In the side-effect completion handler for claim success, pass the context through to `spawn_worker`.

29. Change `spawn_worker` and `run_workflow_worker` in `src/scherzo/orchestrator/daemon.gleam` to accept `WorkerStartContext`. Preserve `context.run_id` as the logical run id and create a distinct recovered worker process session id using the recovered run id plus the newly reserved session sequence.

30. Add daemon tests in `test/scherzo/orchestrator/daemon_test.gleam` for startup recovery. Use fake dependencies and a fake recovery planner output to simulate a recovered run that is already claimed. Assert that startup spawns a worker with the original `run_id`, original run root, distinct recovered process session id, and recovered artifacts. If the daemon tests are split by feature, place these tests in the existing daemon recovery test file instead.

31. Add a daemon test for recovered pending claim handling. Simulate a recovered run whose claim needs to be retried. Assert that `PendingClaim.recovered_context` is present, `effect_runner.ClaimIssue` uses the original `run_id`, and claim success spawns the worker with that context.

32. Add an integration-style restart recovery test if the harness supports it. Seed the ledger with a run start, `WorkflowStepAttemptStarted`, and `WorkflowStepAttemptFinished` for upstream step `collect`; reinitialize the startup recovery path; and assert downstream step `summarize` runs under the original run id and run root. If the repository has no full daemon restart harness, implement this as `load_startup_recovery` plus `resume_recovered_workflow_runs` assertions and record that boundary in the test name or comments.

33. Add `src/scherzo/orchestrator/yaml_step_session.gleam` with public `id(run_id, step_id, attempt)`. Use it in `run_yaml_command_step` and `run_yaml_agent_step`. Update lifecycle messages to include attempt identity.

34. Add tests proving repeated attempts are operator-visible and preserve identity. Add a pure test for `yaml_step_session.id` asserting that `run-1-build-a1` and `run-1-build-a2` differ for attempts `1` and `2`. Also add an integration-style daemon or runner test where a recovered interrupted step runs attempt `2`; assert that the registered step session id ends in `-a2` and the durable started and finished records for that execution record attempt `2`.

35. Commit the daemon and attempt-session wiring. Suggested commit message: `Resume recovered workflow runs from daemon startup`.

36. Implement remaining safe handling cases from the recovery planner adapter. In `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/state/recovery.gleam`, ensure missing/corrupt artifacts, missing required source workspace directories, workflow drift, issue drift, hook preflight failure, disabled recovery mode, and unsafe command interruption map to explicit outcomes described in this plan.

37. Add negative tests for each safety case. They may be pure recovery tests when possible, with one daemon test proving abandon-and-park does not spawn a worker.

38. Run formatting and tests:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expected result is that formatting reports no changes needed and all tests pass. If `direnv` is unavailable but `gleam` is on `PATH`, run `gleam format --check src test` and `gleam test` as a fallback and record the reason in this plan's Surprises & Discoveries.

39. Commit the safety tests and fixes. Suggested commit message: `Handle unsafe workflow recovery cases`.

## Testing and Falsifiability

The core claim is that recovered execution skips terminal steps and still renders their artifacts downstream. This is falsified if any test observes a recovered terminal step being passed to `command_step` or `agent_step`, or if a downstream prompt renders without the restored full-shape artifact fields.

Durable checkpoint tests must prove the recovery substrate before runner hydration. Add state projection tests under `test/scherzo/state/` showing that `WorkflowStepAttemptStarted` and `WorkflowStepAttemptFinished` project by run id, step id, and attempt; that started-without-finished remains visible as interrupted; and that a crash after started attempt `2` never recovers with next attempt `2`. Add artifact IO tests showing that `workflow_checkpoint.write_step_artifact` and `read_step_artifact` preserve representative `step_artifact.StepArtifact` fields including terminal status, command or agent details, exit code when applicable, stdout/stderr text, truncation flags if present, diagnostic path, and summary text. Missing and malformed artifacts must return typed errors.

Add `workflow_fingerprint` tests proving semantic drift detection. Build two DAGs from equivalent workflows with different comments, whitespace, or map ordering and assert equal fingerprints. Then change one semantic field at a time: dependency list, command text, prompt template, workspace source, hook command, `on_failure`, artifact limits, or maximum parallelism. Each semantic change must produce a different fingerprint.

Add `recovered_completed_upstream_step_is_not_rerun_test` in `test/scherzo/workflow_run_test.gleam`. The fixture should create a two-step DAG: `collect` produces a success artifact and `summarize` depends on `collect`. The recovered context should mark `collect` as `Succeeded`, include a full artifact for `collect`, set `run_root` to a relative fixture value such as `test-output/recovered-run`, and leave `summarize` pending. Fake `command_step` should fail the test if called for `collect`. The assertions are: only `summarize` is called, returned artifacts contain `collect` and `summarize`, and cleanup receives `test-output/recovered-run` only after success.

Add `failed_continued_artifact_is_restored_for_downstream_prompt_test` in `test/scherzo/workflow_run_test.gleam`. The DAG should set `lint.on_failure` to continue and make `repair` depend on `lint`. The recovered `lint` artifact should be a failed command artifact with non-empty stderr, exit code, truncation metadata if supported by the type, diagnostic path, and summary. The fake `agent_step` for `repair` should capture the rendered prompt. Assert that `lint` is not rerun, `repair` runs, the prompt contains a value from the failed artifact, and the restored artifact in the final map is equal to the full artifact supplied by the durable reader.

Add `parallel_recovery_runs_only_interrupted_branch_test` in `test/scherzo/workflow_run_test.gleam`. The DAG should have independent branch steps `docs` and `tests`, `max_parallel_steps` of `2`, and join step `final`. Recover `docs` as `Succeeded` with a full artifact. Recover `tests` as `Pending` with next attempt `2`, representing an interrupted branch that the planner chose to rerun. Assert that `docs` is never called, `tests` is called with attempt `2`, and `final` is not called until after `tests` has produced a terminal artifact.

Add durable attempt lifecycle tests in `test/scherzo/workflow_run_test.gleam` or daemon tests. One test should assert `record_step_attempt_started` is called before the fake command or agent dependency. One test should make start-record append fail and assert no command, agent, or step session is started. One test should make finish-record append fail after the fake command returns and assert the worker fails with the recovered run root preserved; a subsequent pure recovery test should see started-without-finished attempt `N` and choose attempt `N + 1` or park according to policy.

Add recovered preparation and cleanup guard tests. Hook preflight failure tests should make `prepare_recovered_step` return `HookFailure` and assert the runner returns a failure without creating a terminal artifact for that step and without calling `cleanup_run`. Deadlock or invalid recovered scheduler tests should assert cleanup is skipped when no new attempt started. Missing workspace tests should delete the recovered source directory, not merely omit it from the dict, and assert no worker spawn.

Add `repeated_step_attempts_have_unique_session_ids_test` in a test file that can import `src/scherzo/orchestrator/yaml_step_session.gleam`. Inputs are the same `run_id` and `step_id` with attempts `1` and `2`. Assertions are: the ids differ, both include the same run id and step id, one includes attempt `1`, one includes attempt `2`, and the attempt number sent to durable started and finished records matches the session id.

Add startup wiring tests in daemon recovery tests. One test should simulate a recovered claimed run and assert that no fresh `make_run_id` is called for it, `spawn_worker` receives the original `run_id`, the recovered worker process session id includes the reserved recovery sequence, and `workflow_run.execute_with_context` receives `RecoveredRun`. Another should simulate a recovered claim retry and assert that pending claim state stores the recovered context until claim success.

Add an integration-style restart test for the operator-visible story. Seed the ledger with a workflow run where upstream step `collect` has a started and finished durable attempt and downstream step `summarize` is pending. Reinitialize the startup recovery path and assert recovery resumes `summarize` under the original run id and run root without rerunning `collect`. If the repository does not provide a full daemon restart harness, implement this by invoking `load_startup_recovery` and `resume_recovered_workflow_runs` with fake effects; the test name should make clear that it simulates restart at the recovery boundary.

Add safety tests. Missing artifact and corrupt artifact tests should feed the pure recovery planner a terminal step checkpoint whose artifact cannot be loaded and assert an abandon-and-park decision. Missing workspace tests should remove or delete a required recovered source workspace from the projected state and assert no worker spawn. Workflow drift tests should use a recovered fingerprint different from `workflow_fingerprint.for_dag` for the selected current DAG and assert `workflow_recovery_invalid:workflow_drift` or the planner's park reason. Issue drift tests should refresh an issue that no longer routes to the same workflow and assert park. Unsafe command interruption tests should project an interrupted command step without explicit rerun-safe policy and assert abandon-and-park rather than rerun. Disabled recovery mode tests should pass `ParkRecoveredWorkflows` and assert otherwise resumable workflow runs park with reason `workflow_recovery_disabled`.

Run validation from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

The plan is disproved if the full suite passes while any of the named assertions are weakened to merely checking that a function returns `Ok`; each test must inspect the behavior that matters: skipped calls, restored full-shape artifacts, prompt content, attempt numbers, durable attempt start-before-side-effect ordering, original run root, distinct recovered worker process session id, and no spawn or cleanup for unsafe recovery.

## Validation and Acceptance

Implementation is accepted when the following behaviors are demonstrated by tests and, where practical, logs:

- A restart-simulated workflow with an upstream completed step skips that step after startup recovery and runs only downstream pending work under the original run id and run root.
- A recovered failed upstream step with `on_failure: continue` contributes its durable failed artifact to downstream prompt rendering, with representative full-shape fields preserved.
- A parallel workflow with one completed branch and one interrupted branch resumes by skipping the completed branch and rerunning only the branch that the recovery planner marked rerunnable.
- Repeated attempts for the same logical step produce unique operator-visible step session ids and preserve the durable attempt number.
- A crash after a step attempt start record but before a finish record recovers by parking or by scheduling a strictly higher attempt number; it never reuses the same attempt number.
- Startup recovery passes recovered context through retry or pending claim handling, worker spawn, workflow execution, scheduler hydration, step scheduling, durable attempt recording, and cleanup.
- Recovered worker process sessions are distinct from the logical run id while worker metadata and logs preserve the original run id.
- Missing or corrupt artifacts, missing required workspace records or directories, hook preflight failures, workflow or issue drift, disabled recovery mode, and unsafe interrupted command steps are handled safely without silent reruns, partial prompt rendering, premature cleanup, or worker spawn.

The final validation commands are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expect formatting to report no changes needed and all tests to pass. The exact test count may change as the repository evolves; record the observed count in Outcomes & Retrospective during implementation.

## Rollout, Recovery, and Idempotence

This change affects daemon startup and stored workflow state, so it must fail closed. Startup recovery should append durable recovery decision records before it spawns workers or reissues claims. If record append fails, daemon startup should fail rather than resume side effects without a durable audit trail.

Fresh dispatch remains compatible with the current operator workflow. The existing `workflow_run.execute` wrapper should continue to run fresh workflows with attempt `1` and no recovered context. Recovered dispatch uses new paths only when startup recovery returns recovered workflow actions.

Repeated daemon starts must be idempotent. A recovered run that has already been converted into a terminal, abandoned, parked, or running state by a previous startup attempt must not spawn twice. The recovery planner and daemon duplicate-active checks should use run id and issue id to suppress duplicate workers. For a started-but-unfinished attempt, idempotence means not reusing the same attempt number; the next safe retry must use a higher attempt number or park.

Cleanup must target the original run root. On normal recovered success, `workflow_run.execute_with_context` should call `cleanup_run` for the recovered run root exactly once, respecting `.scherzo-keep-workspace`. On normal recovered workflow failure after a new attempt has started, use the same cleanup behavior as fresh failures unless the recovery planner or retention marker says to preserve the run root. On recovered validation, deadlock, or hook preflight failure before any new attempt starts, do not delete the run root automatically. On abandon-and-park before worker spawn, do not delete the run root automatically; operators may need it for investigation.

Rollback is a code-level recovery mode, not a new user-facing configuration surface in this plan. `src/scherzo/state/recovery.gleam` must expose `WorkflowRecoveryMode`. Production startup passes `ResumeRecoveredWorkflows` by default. An emergency rollback patch changes the production startup argument to `ParkRecoveredWorkflows`, causing otherwise resumable recovered workflows to park with reason `workflow_recovery_disabled` instead of spawning or retrying claims. Tests must prove this mode parks recovered workflows and emits no worker spawn side effects.

## Artifacts and Notes

Current fresh runner shape, simplified:

    workflow_run.execute(..., run_id, dependencies)
      -> loop(..., workflow_scheduler.init(dag), dict.new(), dict.new(), None, zero tokens, None, 0)
      -> ready_steps
      -> prepare_ready_steps
      -> run_prepared_batch
      -> apply_prepared_results
      -> cleanup_if_needed

Target recovered runner shape:

    workflow_run.execute_with_context(..., RecoveredRun(context), dependencies)
      -> workflow_scheduler.init_with_statuses(dag, context.scheduler_statuses)
      -> loop(..., restored artifacts, restored workspaces, Some(context.run_root), restored tokens, restored turns, restored attempts, cleanup_allowed: False)
      -> ready_steps skips Succeeded and FailedContinued steps
      -> pending or rerunnable steps run with durable next attempt number
      -> downstream prompt rendering sees restored full-shape artifacts
      -> cleanup_if_needed(Some(context.run_root), ...) only when cleanup_allowed is true or workflow is terminal

Target durable attempt lifecycle:

    prepare recovered or fresh workspace
      -> append WorkflowStepAttemptStarted(run_id, step_id, attempt, ...)
      -> register YAML step session id run_id-step_id-a<attempt>
      -> run command or agent dependency
      -> write workflow-artifacts/<run-id>/<step-id>/attempt-<attempt>.json
      -> append WorkflowStepAttemptFinished(run_id, step_id, attempt, terminal_status, artifact_ref, ...)

If the daemon crashes after `WorkflowStepAttemptStarted` for attempt `N` and before the finish record, recovery must not reuse `N`. It must either park or use `N + 1` according to the planner's safety policy.

Current YAML step session id shape:

    run_id <> "-" <> step_id

Target YAML step session id shape:

    run_id <> "-" <> step_id <> "-a" <> int.to_string(attempt)

Target recovered worker process session id shape:

    run_id <> "-resume-" <> int.to_string(session_sequence)

Use placeholders for any runtime absolute paths in test names, comments, or expected output. For example, write `<absolute-local-path>/workspace-root/workflow/ISSUE/run/main` rather than a machine-specific path.

## Interfaces and Dependencies

No new package dependency is required unless the repository lacks an existing stable hash helper. Prefer an existing project hash function for `workflow_fingerprint.for_dag`; if none exists, add the smallest local helper needed to produce a stable `sha256:<hex>` string without introducing a broad dependency. Use existing Gleam standard library modules, existing state ledger modules, existing session hub modules, and existing workflow modules.

At the end of implementation, `src/scherzo/state/record.gleam` should expose or encode these logical records:

    WorkflowStepAttemptStarted(
      workflow_id: String,
      workflow_fingerprint: String,
      run_id: String,
      issue_id: String,
      step_id: String,
      attempt: Int,
      workspace_name: String,
      workspace_path: String,
      source_workspace_name: Option(String),
      source_workspace_path: Option(String),
      started_at: String,
    )

    WorkflowStepAttemptFinished(
      run_id: String,
      step_id: String,
      attempt: Int,
      terminal_status: WorkflowStepTerminalStatus,
      artifact_ref: String,
      finished_at: String,
    )

    WorkflowRecoveryDecisionRecorded(
      run_id: String,
      issue_id: String,
      decision: String,
      reason: String,
      recorded_at: String,
    )

At the end of implementation, `src/scherzo/state/workflow_checkpoint.gleam` should expose:

    pub type WorkflowStepTerminalStatus {
      StepSucceeded
      StepFailedContinued
      StepFailedFatal
    }

    pub type ArtifactStorageError {
      ArtifactMissing(artifact_ref: String)
      ArtifactCorrupt(artifact_ref: String, reason: String)
      ArtifactWriteFailed(artifact_ref: String, reason: String)
    }

    pub fn artifact_ref(run_id: String, step_id: String, attempt: Int) -> String

    pub fn write_step_artifact(
      state_root: String,
      run_id: String,
      step_id: String,
      attempt: Int,
      artifact: step_artifact.StepArtifact,
    ) -> Result(String, ArtifactStorageError)

    pub fn read_step_artifact(
      state_root: String,
      artifact_ref: String,
    ) -> Result(step_artifact.StepArtifact, ArtifactStorageError)

At the end of implementation, `src/scherzo/workflow_fingerprint.gleam` should expose:

    pub fn for_dag(workflow_id: String, dag: workflow_dag.WorkflowDag) -> String

At the end of implementation, `src/scherzo/workflow_scheduler.gleam` should expose:

    pub fn init_with_statuses(
      dag: workflow_dag.WorkflowDag,
      recovered: Dict(String, StepRuntime),
    ) -> Result(SchedulerState, String)

At the end of implementation, `src/scherzo/workflow_run.gleam` should expose:

    pub type StepAttemptContext {
      StepAttemptContext(step_id: String, next_attempt: Int)
    }

    pub type RecoveredRunContext {
      RecoveredRunContext(
        workflow_id: String,
        workflow_fingerprint: String,
        run_id: String,
        run_root: String,
        scheduler_statuses: Dict(String, workflow_scheduler.StepRuntime),
        artifacts: Dict(String, step_artifact.StepArtifact),
        prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
        step_attempts: Dict(String, Int),
        token_totals: domain.TokenTotals,
        final_issue: Option(domain.Issue),
        turns: Int,
        warnings: List(String),
      )
    }

    pub type RunContext {
      FreshRun(run_id: String)
      RecoveredRun(RecoveredRunContext)
    }

    pub type AttemptStart {
      AttemptStart(
        workflow_id: String,
        workflow_fingerprint: String,
        run_id: String,
        issue_id: String,
        step_id: String,
        attempt: Int,
        workspace_name: String,
        workspace_path: String,
        source_workspace_name: Option(String),
        source_workspace_path: Option(String),
      )
    }

    pub type AttemptFinish {
      AttemptFinish(
        run_id: String,
        step_id: String,
        attempt: Int,
        terminal_status: workflow_checkpoint.WorkflowStepTerminalStatus,
        artifact: step_artifact.StepArtifact,
      )
    }

    pub fn execute_with_context(
      issue: domain.Issue,
      dag: workflow_dag.WorkflowDag,
      orchestrator: domain.OrchestratorConfig,
      tracker_client: tracker.Client,
      secrets: List(String),
      context: RunContext,
      dependencies: Dependencies,
    ) -> Result(WorkflowRunSuccess, WorkflowRunFailure)

`workflow_run.Dependencies` should include the existing dependency functions plus attempt-aware step functions and durable attempt record callbacks:

    command_step: fn(
      String,
      Int,
      String,
      String,
      Int,
      List(String),
      domain.ArtifactLimits,
    ) -> step_artifact.StepArtifact

    agent_step: fn(
      String,
      Int,
      String,
      String,
      String,
      domain.Issue,
      List(String),
      domain.ArtifactLimits,
    ) -> step_artifact.StepArtifact

    record_step_attempt_started: fn(AttemptStart) -> Result(Nil, String)

    record_step_attempt_finished: fn(AttemptFinish) -> Result(Nil, String)

At the end of implementation, `src/scherzo/workspace_run.gleam` should expose:

    pub fn prepare_recovered_step(
      issue: domain.Issue,
      workflow_id: String,
      run_id: String,
      expected_run_root: String,
      step_id: String,
      workspace_ref: workflow_dag.WorkspaceRef,
      orchestrator: domain.OrchestratorConfig,
      known_workspaces: Dict(String, PreparedStepWorkspace),
    ) -> Result(PreparedStepWorkspace, PrepareError)

At the end of implementation, `src/scherzo/orchestrator/yaml_step_session.gleam` should expose:

    pub fn id(run_id: String, step_id: String, attempt: Int) -> String

At the end of implementation, `src/scherzo/orchestrator/daemon.gleam` should contain:

    fn resume_recovered_workflow_runs(
      state: State,
      actions: List(recovery.RecoveredWorkflowAction),
    ) -> State

    type WorkerStartContext {
      FreshWorker
      RecoveredWorker(workflow_run.RecoveredRunContext)
    }

The recovery planner interface may already exist under another module name. The implementation should adapt it to provide this minimum daemon-facing shape:

    pub type RecoveredWorkflowRun {
      RecoveredWorkflowRun(
        issue: domain.Issue,
        workflow_id: String,
        workflow_fingerprint: String,
        run_id: String,
        run_root: String,
        worker_workspace_path: String,
        session_sequence: Int,
        runner_context: workflow_run.RecoveredRunContext,
        warnings: List(String),
      )
    }

    pub type RecoveredWorkflowAction {
      ResumeAlreadyClaimed(RecoveredWorkflowRun)
      RetryClaimThenResume(RecoveredWorkflowRun)
      AbandonAndPark(issue: domain.Issue, run_id: String, reason: String, warnings: List(String))
    }

    pub type WorkflowRecoveryMode {
      ResumeRecoveredWorkflows
      ParkRecoveredWorkflows
    }

If existing modules name these concepts differently, keep the existing names and add conversion helpers instead of renaming broadly. The important interface boundary is that `src/scherzo/orchestrator/daemon.gleam` receives a pure, already-decided list of recovered workflow actions and `src/scherzo/workflow_run.gleam` receives a concrete recovered runner context. No implementation step may require the runner to decide whether a corrupt artifact, drifted workflow, missing workspace, disabled recovery mode, or unsafe interrupted command should continue.

## Open Questions and Clarifications Needed

None.
