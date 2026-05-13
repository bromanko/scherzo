# Workflow resumption hardening: recover workflow DAG runs after daemon failure

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this umbrella plan is complete, Scherzo has a shared recovery model for interrupted YAML workflow runs and a set of smaller implementation ExecPlans that can be written and executed safely. The operator-visible goal for the later implementation work is that a Scherzo daemon can restart after a crash, inspect the durable ledger, understand which workflow run and step were interrupted, and either resume, rerun, park, or clean up the work according to explicit policy rather than starting an unrelated fresh run.

This umbrella deliberately does not implement workflow resumption. It defines the recovery units, invariants, sequencing, child plans, and Linear tickets needed to replace the obsolete issue-level pi session continuation approach in `docs/plans/hardening-04-pi-session-continuation.md`. That older plan assumed one issue-level pi session and one issue workspace. Current Scherzo executes YAML workflow DAGs where one issue run can contain multiple agent steps, command steps, workspaces, artifacts, and pi sessions. Resumption must therefore be workflow-step scoped.

The visible proof for this umbrella itself is a checked-in architecture plan plus Linear tickets for the child plans. The visible proof for the full future program is a deterministic daemon-restart test where a multi-step workflow has completed upstream steps, crashes during one step, restarts from the same workspace root, preserves completed step artifacts, handles the interrupted step by policy, and never attaches a pi session to the wrong workspace.

## Problem Framing and Constraints

Hardening 03 added single-instance daemon crash recovery from the local durable ledger. It can mark started-but-unfinished Scherzo runs as interrupted, restore retry counters and parked state, and schedule retries or cleanup. That is necessary but not enough for workflow resumption. At the moment, an interrupted workflow run is effectively treated as one issue-level unit. A retry can create a new run id and new workspace paths even though the previous run may have completed several workflow steps and left partial work in one step workspace.

Current workflow execution is DAG-based. As of this review, the checked-in `.scherzo/workflows/implementation.yaml` workflow has command steps `prepare_context`, `analyze_changes`, `final_validate`, and `publish_pr`, agent steps `implement`, `code_review`, and `apply_feedback`, `max_parallel_steps: 1`, and all steps use workspace `main`. The checked-in `.scherzo/workflows/research.yaml` workflow has agent step `research` and command step `collect_findings`, also on workspace `main`. This current dogfood shape is sequential, but the workflow DAG format and scheduler support multiple logical workspaces and concurrent independent steps when `max_parallel_steps` is greater than one. Recovery therefore must not collapse the model back to one issue-level pi session: even a sequential run can contain several agent steps, command steps, artifacts, and potential pi sessions, and future or test workflows can contain multiple running step attempts at once. Agent steps launch pi and can each have a different pi session. Command steps run arbitrary shell and may mutate workspaces. Downstream prompts can depend on upstream step artifacts. Therefore, recovering only `run_id`, `issue_id`, and one `sessionFile` is not precise enough.

The recovery model must accept these constraints:

- A BEAM restart loses Erlang processes, worker command subjects, ports, EventHub in-memory history, pending UI requests, and live pi streams.
- Pi session files, step workspaces, run roots, result artifacts, and the local durable ledger can survive if they were written before the crash.
- Scherzo cannot assume command steps are idempotent because workflow `run` commands are arbitrary shell.
- Scherzo cannot safely resume a pi session unless it also verifies the exact prior step workspace path and launches pi from that path.
- The local durable ledger is single-writer, single-workspace-root state. This work does not make Scherzo distributed or exactly-once.
- Linear state remains the source of truth for whether an issue is still active, terminal, or non-active. Startup recovery must refresh ledger-known issues before dispatching, and it must compare the refreshed issue against the issue fingerprint or observed update timestamp recorded when the workflow run started. If the issue changed while Scherzo was down, the conservative default is operator inspection or a full fresh workflow decision, not silent reuse of stale completed-step artifacts.
- Workflow definitions, prompt files, model settings, workspace hooks, and workspace root configuration can change between crash and restart. Recovery must record enough workflow/config identity at run start to detect drift. If the current definition or root no longer matches the recorded run, Scherzo must park or require inspection unless a later plan defines a safe migration.
- Backward compatibility with pre-workflow-resumption local durable state is not required. Child plans may change ledger record schemas, projection snapshot schemas, state directory layout, and workflow recovery record shapes without supporting existing issue-level ledgers or snapshots. They must still fail safely and document the operator reset path for old local state before enabling new runtime behavior.

## Recovery Vocabulary

In current code, a Scherzo issue run is one daemon-managed attempt to execute a selected workflow for one Linear issue. It has a `run_id` and corresponds to a registered operator session in the EventHub while live. The recovery program must make this vocabulary more precise: the logical workflow run id is the stable id used in workflow run-root paths and durable workflow facts, while worker executions and operator step sessions are volatile process attempts that may be recreated after restart. A child plan may keep the existing field name `run_id` for the stable workflow run id, but it must not allocate a different run-root id merely because the daemon process was restarted.

A workflow DAG is the YAML-defined graph of steps for a workflow id such as `implementation` or `research`. A workflow step is one node in that graph. It is either an agent step, which launches pi, or a command step, which runs shell. A workflow definition fingerprint is a stable hash or comparable digest of the workflow YAML plus prompt files and step settings that affect execution. Recovery uses it to detect that the workflow being resumed is still the workflow that originally ran.

A step attempt is one execution of one workflow step within one issue run. The durable recovery model treats this as the main unit of checkpointing. It is identified by `run_id`, `workflow_id`, `step_id`, `workspace_name`, `workspace_path`, and an attempt discriminator such as `attempt_index`. The first execution of a step in a run uses attempt index 1. If recovery policy chooses to rerun or continue the same step inside the same run, the new execution must get a new attempt index and must record which prior attempt it recovers from. Without this discriminator, a crash-restart-crash sequence can overwrite or double-count attempts for the same step.

A workflow workspace is the filesystem directory prepared for a workflow step. Current code derives absolute paths under the configured workspace root after sanitizing workflow id, issue identifier, run id, and workspace name. With the checked-in `.scherzo/scherzo.yaml`, the effective path is under `.scherzo/workspaces/<workflow>/<issue>/<run>/<workspace-name>/` relative to the repository root. A run root is the parent directory for all workspaces in one issue run.

A pi session is pi's persisted conversation history file. It is not a live process. Resuming a pi session means starting a new pi process with `--session <session-file>` in the exact prior step workspace and sending a recovery prompt. It does not recover the live in-flight model stream or tool call that existed before the crash.

A step artifact is the bounded result data Scherzo records from a completed step so downstream prompts and final reports can use it. It must preserve the fields currently exposed by `src/scherzo/step_artifact.gleam` to templates: status, final response, exit code, stdout, stderr, timeout flag, truncation flags, and summary text. A failed step with `on_failure: continue` is a terminal dependency when it has a durable failed artifact; recovery must not assume that only successful artifacts can unblock downstream steps.

## Strategy Overview

Treat workflow resumption as a staged hardening program, not as one large implementation. The first child plan must make workflow run and step state durable. The second must build a pure startup recovery planner that can reconstruct workflow progress and choose conservative recovery actions. The third must integrate that planner with workflow execution so completed steps can be skipped and interrupted steps can be handled by policy. Only after those foundations exist should Scherzo add step-scoped pi session continuation.

The key architectural shift is that the durable ledger must record step-scoped facts, not just run-scoped facts. The minimum durable facts are workflow run start/finish/interruption/resumption, workflow definition fingerprint, issue fingerprint or observed update timestamp, step attempt start/finish/interruption, workspace preparation, durable step artifact recording, and optional pi session attachment for an agent step attempt. The recovery projection must answer which steps are complete under their failure policy, which attempts are still running or interrupted, which artifacts are valid, which workspace path belongs to each attempt, whether a pi session file is available, whether the current workflow definition still matches the recorded run, and whether a later attempt superseded an earlier one.

Child Plan 1 must treat ledger and snapshot schema ownership as part of the state design, not as an implementation detail. `src/scherzo/state/record.gleam` currently uses `schema_version = 1` and rejects unknown record kinds, while `src/scherzo/state/projection.gleam` snapshots also require that schema version. The child plan does not need to preserve compatibility with those old ledgers or snapshots. It may bump schema versions, reject old records, discard snapshots, or require the operator to delete and reinitialize local state under the configured workspace root before rollout. What it must do is choose an explicit reset or replacement policy and test that old state is not silently partially replayed as valid workflow recovery state.

Step artifacts must be durable in their own right. A recovery planner may rely on a completed step only after the artifact payload has been redacted, bounded, written durably, and referenced by the ledger. An artifact reference that points only into a workflow workspace or run root is not enough unless the plan also guarantees that cleanup cannot delete it before recovery consumes it.

Recovery policy should be conservative by default. Completed steps with durable terminal artifacts should not rerun. A terminal artifact is either a successful artifact or a failed artifact whose step declares `on_failure: continue`. Pending steps should remain pending. Interrupted command steps should not automatically rerun unless a future workflow option declares them safe to rerun; the first safe default is to park or mark the workflow as needing operator inspection. Interrupted agent steps may be eligible for fresh recovery or pi-session recovery only when their workspace path exists, remains under `workspace.root`, the current workflow definition and issue fingerprint match the recorded run, the normal step preflight check passes, and their step-scoped session file exists if session continuation is requested. For recovery of an existing workspace, the preflight check means validating the existing workspace and running only hooks that the child plan has documented as safe to repeat; it must not blindly rerun a workspace creation hook against a partially completed workspace unless the hook is proven idempotent.

Step-scoped pi session continuation remains valuable, but it is a later layer. It must attach sessions to `run_id + workflow_id + step_id + attempt_index + workspace_path`, not just to an issue. It must also include a mandatory real-pi validation milestone because fake-pi tests only prove command-line plumbing, not real pi persistence semantics.

## Alternatives Considered

One alternative is to implement the existing `hardening-04-pi-session-continuation.md` directly. That is rejected because it can resume the wrong pi session or launch it from the wrong workspace in current YAML workflows.

Another alternative is to persist the entire in-memory workflow runner state. That is rejected because runtime state contains process-owned and transient concepts that cannot survive restart. The safer approach is a fact ledger plus a recovery projection that intentionally does not restore live handles.

A third alternative is to rerun all workflow steps from the beginning after restart. That is simpler, but it wastes tokens, can duplicate side effects, and defeats the purpose of preserving partial work and review artifacts.

A fourth alternative is to only support live process reattachment. That is not feasible after BEAM process death because Erlang ports, worker actors, command subjects, and streamed JSONL events are gone.

A fifth alternative is to make command steps always rerunnable. That is unsafe because command steps are arbitrary shell and may mutate workspaces, push branches, or perform external effects. Rerun safety must be explicit, not assumed.

## Risks and Countermeasures

The main correctness risk is resuming a pi session against a workspace that no longer matches the conversation. Countermeasure: session continuation is step-scoped and requires exact workspace path validation before launch. If validation fails, Scherzo parks the issue or follows a documented fresh-recovery policy; it must not silently attach the old session to a new workspace.

The main workflow risk is losing completed step artifacts. Countermeasure: completed steps are only considered reusable when both the step-finished record and artifact record are durable. If the artifact is missing or corrupt, recovery treats the step as not safely reusable and follows policy.

The main command-step risk is duplicate side effects. Countermeasure: interrupted command steps default to operator inspection or park unless a later plan adds an explicit `rerun_safe` declaration and tests it. The first implementation should not infer idempotence from command text.

The main parallelism risk is recovering only one interrupted step while another parallel step also had state. Countermeasure: the recovery projection models every step attempt in the run. Startup recovery must handle multiple running/interrupted steps and only unblock downstream steps when all dependencies have durable terminal artifacts according to each dependency step's failure policy.

The main repeated-startup risk is double-counting one interrupted run or step on every restart. Countermeasure: interruption and counter records include source ids (`run_id`, `step_id`, and `attempt_index`) and recovery checks whether they already contributed before incrementing counters or appending duplicate interruption records. Operator-visible step session ids must also include enough attempt identity to avoid collisions, because current YAML step sessions use `run_id <> "-" <> step_id`, which would collide if the same step is recovered inside the same logical run.

The main drift risk is resuming a workflow after its YAML, prompt files, issue contents, hooks, or workspace root changed. Countermeasure: record a workflow definition fingerprint, the selected workflow id, relevant prompt/template identity, workspace root, and issue fingerprint at workflow-run start. On restart, compare the recorded facts against the current repository and refreshed Linear issue. If they differ, do not silently skip steps or resume pi; park or request operator inspection unless a child plan explicitly defines safe migration semantics.

The main ledger-evolution risk is silently half-reading old issue-level recovery state as if it were valid workflow recovery state. Countermeasure: Child Plan 1 must specify record-kind decoding, snapshot decoding, schema-version policy, and local-state reset behavior before any runtime emits the new records. Existing ledgers that contain only issue-level records do not need to replay; they may be rejected with a clear operator-facing reset instruction.

The main artifact-durability risk is treating workspace-local output as durable even though run-root cleanup can delete it and command steps can mutate it. Countermeasure: store bounded artifacts inline in the ledger or copy them to a Scherzo-owned state artifact path outside the cleanup target before appending the durable artifact record. Append `step_finished` only after the artifact is recoverable, and make recovery reject missing, corrupt, or cleanup-targeted artifact references.

The main privacy risk is local retention of pi transcripts, workflow artifacts, and command output. Countermeasure: session persistence remains opt-in, artifacts and session files are stored only under Scherzo-owned local state that is gitignored, output is bounded and redacted with the same secret list used at runtime, and Child Plan 5 documents retention and cleanup as sensitive local data handling.

The main hook-safety risk is treating existing workspace validation as equivalent to initial workspace creation. Countermeasure: child plans must distinguish first-time workspace creation from recovery preflight. Current checked-in hooks are intended to be repeatable, but workflow hooks are arbitrary shell, so recovery must document which hooks run during resume, what environment variables they receive, and what happens if they fail.

The main scope risk is turning this into a distributed workflow database. Countermeasure: all child plans preserve the existing single-daemon, single-canonical-workspace-root model. Multi-host coordination remains out of scope.

## Progress

- [x] (2026-05-03 15:34Z) Wrote this umbrella plan to supersede issue-level pi continuation with a workflow-step-scoped recovery program.
- [x] (2026-05-03 15:34Z) Created Linear tickets LIV-54 through LIV-58 for the five child planning ExecPlans and recorded them in Outcomes & Retrospective.
- [x] (2026-05-03 15:50Z) Reviewed this umbrella against the current repository tree and tightened stale workflow facts, stable run identity, attempt identity, operator session id uniqueness, workflow/issue drift detection, hook safety, ledger schema/reset policy, artifact durability, and validation expectations.
- [x] (2026-05-03 16:01Z) Confirmed and refreshed Linear tasks LIV-54 through LIV-58 for writing all five child ExecPlans, including the explicit no-backward-compatibility scope rule.
- [x] (2026-05-03 20:19Z) Wrote child ExecPlan 1 at `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`.
- [x] (2026-05-04 03:52Z) Wrote child ExecPlan 2 at `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`.
- [x] (2026-05-04 03:52Z) Wrote child ExecPlan 3 at `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`.
- [x] (2026-05-04 14:13Z) Wrote child ExecPlan 4 at `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`.
- [x] (2026-05-04 14:13Z) Wrote child ExecPlan 5 at `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`.
- [x] (2026-05-05 17:52Z) Reconciled this umbrella and the related Linear tickets to the actual checked-in `LIV-xx-...md` child plan filenames.

## Surprises & Discoveries

- Observation: The old pi session continuation plan was technically plausible for pi itself but not for current Scherzo workflow DAGs.
  Evidence: `src/scherzo/workflow_run.gleam` executes YAML DAG steps, `src/scherzo/workspace_run.gleam` prepares per-run/per-step workspace paths, and `src/scherzo/orchestrator/daemon.gleam` registers step sessions through `YamlStepStarted` and `YamlStepUpdate`.

- Observation: The checked-in dogfood workflows are currently sequential rather than the parallel review shape assumed in the first umbrella draft.
  Evidence: `.scherzo/workflows/implementation.yaml` has `max_parallel_steps: 1`, command steps `prepare_context`, `analyze_changes`, `final_validate`, and `publish_pr`, agent steps `implement`, `code_review`, and `apply_feedback`, and every step uses workspace `main`. `.scherzo/workflows/research.yaml` has `max_parallel_steps: 1` by default and uses workspace `main`.

- Observation: Workflow recovery records must be planned with a deliberate ledger and snapshot reset policy, not accidental compatibility.
  Evidence: `src/scherzo/state/record.gleam` defines `schema_version = 1` and turns unknown record kinds into decode errors; `src/scherzo/state/ledger.gleam` treats those decode errors as corrupt records; `src/scherzo/state/projection.gleam` requires the same schema version for projection snapshots. Backward compatibility with that old state is not required for this program.

- Observation: Current YAML step operator session ids do not include an attempt discriminator.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` builds YAML command and agent step session ids as `run_id <> "-" <> step_id` in `run_yaml_command_step` and `run_yaml_agent_step`.

- Observation: Current issue-level `RunStarted` and `KnownWorkspace` records use the legacy per-issue workspace path, not the workflow DAG run root.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` records `pending.workspace_path` from `workspace.workspace_path(...)` during claim handling, while `src/scherzo/workspace_run.gleam` later derives workflow run roots under `<workspace.root>/<workflow>/<issue>/<run>/` for DAG steps.

- Observation: Downstream workflow prompt rendering depends on the full `StepArtifact` shape, not just a success flag.
  Evidence: `src/scherzo/workflow_run.gleam` renders agent prompts with `step_artifact.to_template_locals(artifacts)`, and `src/scherzo/step_artifact.gleam` exposes status, final response, exit code, stdout, stderr, timeout, truncation flags, and summary fields.

- Observation: The checked-in child plan files use their Linear planning issue identifiers in the filename rather than the original `workflow-resumption-0x-...md` names drafted in this umbrella.
  Evidence: the current repository contains `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`, `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`, `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`, `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`, and `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`; Linear issue LIV-66 failed its `prepare_plan` step while pointing at the old expected `docs/plans/workflow-resumption-04-step-scoped-pi-sessions.md` path.

## Decision Log

- Decision: Make workflow step attempt the primary recovery unit.
  Rationale: A single issue run can contain multiple agent steps, command steps, workspaces, and pi sessions. Run-level state is too coarse to resume safely.
  Date: 2026-05-03

- Decision: Defer pi session continuation until durable workflow checkpoints and recovery planning exist.
  Rationale: Pi session files are only safe to use when Scherzo can identify the exact interrupted agent step and exact prior workspace.
  Date: 2026-05-03

- Decision: Treat interrupted command steps as not automatically rerunnable by default.
  Rationale: Workflow command steps are arbitrary shell and may have side effects. Idempotence must be declared and tested before automatic rerun.
  Date: 2026-05-03

- Decision: Keep the recovery program as multiple child plans.
  Rationale: Durable step records, recovery planning, runner integration, pi sessions, and operator UX have different risks and can be validated independently.
  Date: 2026-05-03

- Decision: Normalize this umbrella to the current checked-in dogfood workflows while still requiring the recovery model to support multiple steps, multiple workspaces, and parallel step attempts.
  Rationale: The current `implementation` and `research` workflows are sequential and use workspace `main`, but the parser and scheduler support broader DAG shapes. The plan should not rely on stale workflow facts, and tests can use explicit fixture DAGs for parallel recovery cases.
  Date: 2026-05-03

- Decision: Require explicit attempt identity for repeated executions of the same workflow step within one run.
  Rationale: Recovery can rerun or continue an interrupted step after a restart. Without an attempt discriminator, durable records for the same `run_id`, `workflow_id`, and `step_id` are ambiguous and repeated restarts can double-count or overwrite state.
  Date: 2026-05-03

- Decision: Require Child Plan 1 to specify ledger/snapshot schema replacement and local-state reset behavior before runner behavior changes.
  Rationale: Backward compatibility with existing local issue-level ledgers is not required, but startup must not partially replay old state as workflow recovery state. Completed-step reuse is only safe after the new artifact and workflow records are written under the new policy.
  Date: 2026-05-03

- Decision: Require workflow definition and issue drift detection before reusing completed steps or resuming pi.
  Rationale: Workflow YAML, prompt files, hooks, model settings, workspace root, and Linear issue content can change while the daemon is down. Reusing artifacts or pi sessions from an older task definition can make the agent solve the wrong problem.
  Date: 2026-05-03

- Decision: Treat the existing `run_id` as the stable logical workflow run id for recovered workflow state unless a child plan explicitly introduces a separate stable id and migration.
  Rationale: Current workspace paths include `run_id`. Allocating a new run-root id on restart loses the prior step workspaces and artifacts. Process-level executions and operator step sessions need distinct attempt identity instead of replacing the logical workflow run id.
  Date: 2026-05-03

- Decision: Treat the checked-in `docs/plans/LIV-xx-...md` child plan filenames as canonical instead of renaming the files back to the original `workflow-resumption-0x-...md` draft names.
  Rationale: The `LIV-xx` filenames match the PRs and implementation issues that already landed. Updating this umbrella and Linear descriptions is less disruptive than renaming completed plan files and rewriting historical PR references.
  Date: 2026-05-05

## Outcomes & Retrospective

Umbrella setup completed on 2026-05-03. The old `docs/plans/hardening-04-pi-session-continuation.md` plan is retained but now has a supersession notice warning not to implement it as written. The child planning tickets created in Linear are:

- LIV-54: `Write plan: durable workflow step checkpoints for resumption`, actual plan path `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`.
- LIV-55: `Write plan: startup recovery planner for workflow DAG progress`, actual plan path `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`.
- LIV-56: `Write plan: recovery-aware workflow runner execution`, actual plan path `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`.
- LIV-57: `Write plan: step-scoped pi session continuation`, actual plan path `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`.
- LIV-58: `Write plan: workflow recovery operator UX and retention policy`, actual plan path `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`.

These tickets were created in the LIV team, Scherzo project, Backlog state, with the `workflow:execplan` label so they can be promoted into the ExecPlan workflow when ready. On 2026-05-03, their descriptions were refreshed to match this reviewed umbrella, including the rule that child plans do not need backward compatibility with pre-workflow-resumption local ledgers or snapshots and may instead specify an explicit old-state reset path. No runtime implementation was performed as part of this umbrella step.

On 2026-05-05, this umbrella was reconciled with the actual checked-in child plan filenames. The repository uses `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`, `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`, `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`, `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`, and `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md` as the canonical child plan paths. Linear planning tickets LIV-54 through LIV-58 and implementation ticket LIV-66 were updated to reference those canonical paths so future workflow runs do not look for the obsolete `workflow-resumption-0x-...md` filenames.

## Context and Orientation

The current workflow runtime is implemented across these repository areas:

`src/scherzo/orchestrator/daemon.gleam` owns daemon startup, recovery loading, worker spawning, EventHub session registration, retry handling, and ledger emission around issue-level run transitions. It currently records issue-level `RunStarted` and `KnownWorkspace` facts from a legacy per-issue workspace path before `workflow_run.execute` prepares workflow DAG workspaces.

`src/scherzo/workflow_run.gleam` executes a parsed workflow DAG. It prepares ready steps, runs command and agent steps, collects `step_artifact.StepArtifact` values, renders downstream agent prompts from those artifacts, and cleans up the run root when the workflow finishes.

`src/scherzo/workspace_run.gleam` derives run-root and step-workspace paths under `workspace.root`, prepares workspaces, runs `create`, `before_step`, `after_step`, and `remove` hooks, and contains the cleanup safety checks for run roots. Recovery must not assume that these hooks are all safe to repeat against partially completed workspaces.

`src/scherzo/agent/runner.gleam` launches pi for agent steps, sends prompts, receives pi events, handles operator UI requests, and emits `runner.PiUpdate` values.

`src/scherzo/agent/pi_rpc.gleam` implements the pi JSONL RPC protocol. It currently stores `session_id` but not `session_file`.

`src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, and `src/scherzo/state/recovery.gleam` implement durable local facts and startup recovery for issue-level runtime state.

`src/scherzo/session/event.gleam`, `src/scherzo/session/hub.gleam`, and `src/scherzo/orchestrator/event_publisher.gleam` provide operator-visible session summaries and event streams. These are currently runtime-only and should not be mistaken for durable workflow checkpoints.

## Preconditions and Verified Facts

Hardening 01, 02, and 03 are already implemented in this repository. The local durable ledger exists, daemon startup replays it before polling, and interrupted issue-level runs can be marked interrupted without recovering live pi sessions.

The checked-in dogfood config at `.scherzo/scherzo.yaml` uses YAML workflows under `.scherzo/workflows/`. As of 2026-05-03, `.scherzo/workflows/implementation.yaml` has `max_parallel_steps: 1`, uses workspace `main` for every step, and contains command steps `prepare_context`, `analyze_changes`, `final_validate`, and `publish_pr` plus agent steps `implement`, `code_review`, and `apply_feedback`. `.scherzo/workflows/research.yaml` uses workspace `main` for agent step `research` and command step `collect_findings`. Child plans must re-read these files before freezing because dogfood workflows are expected to evolve.

Even though the current dogfood workflows are sequential, the implementation must not assume one step or one workspace forever. `src/scherzo/workflow_dag.gleam` parses `max_parallel_steps`, per-step `workspace`, and `workspace.from`; `src/scherzo/workflow_scheduler.gleam` can select multiple ready steps when capacity and distinct workspaces allow; and `src/scherzo/workspace_run.gleam` prepares paths under `<workspace.root>/<workflow>/<issue>/<run>/<workspace-name>/` after sanitizing path components.

The durable state implementation currently has issue-level run, retry, parking, Linear command, outbox, counter, and known-workspace records. `src/scherzo/state/record.gleam` uses `schema_version = 1`; `src/scherzo/state/ledger.gleam` treats unknown non-tail records as corrupt; and `src/scherzo/state/projection.gleam` stores compacted snapshots with the same schema version. Child plans may break compatibility with these existing ledgers and snapshots. They must state whether old local state is rejected, deleted, archived, or reinitialized, and must provide clear operator instructions for that reset. The existing issue-level `RunStarted.workspace_path` and `KnownWorkspace.workspace_path` are not sufficient workflow run-root facts because they are recorded before `workspace_run.prepare_step` derives `<workspace.root>/<workflow>/<issue>/<run>/<workspace-name>/`.

Recovery must account for workflow and issue drift. Child plans should record the selected workflow id, a workflow definition fingerprint, enough prompt/template identity to detect changed prompts, the effective workspace root, and the Linear issue fingerprint or observed update timestamp when a workflow run starts. On restart, a mismatch should lead to a documented conservative action, not silent continuation.

Pi supports session files at the CLI and RPC layer, but Scherzo's current `pi_rpc.Session` does not store `sessionFile`. Real-pi validation is still required before depending on session-file continuation behavior.

## Scope Boundaries

In scope for the recovery program: durable workflow run and step checkpoint facts; durable step artifacts or state-owned artifact references; workspace path recovery; workflow definition and issue drift detection; startup recovery planning for workflow DAG progress; runner integration that can skip completed steps and handle interrupted steps by policy; step-scoped pi session persistence; recovery prompts; operator-visible recovery status; documentation; deterministic fake tests; schema/snapshot reset-policy tests; and a mandatory real-pi session continuation probe before enabling pi resume.

Out of scope for the recovery program: distributed multi-host scheduling; exactly-once external side effects; live stream/tool-call reattachment; durable EventHub transcript archive; Linear webhooks; automatic stale instance-lock takeover; full filesystem snapshots; and guessing command-step idempotence from command strings.

## Child Plan Roadmap

Child Plan 1 is `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`. It adds durable workflow run, step attempt, workspace, artifact, and step-interruption records. It must also specify ledger record decoding, projection snapshot replacement, old-state reset behavior, attempt-index assignment, workflow definition fingerprinting, issue fingerprint recording, artifact storage location, artifact write ordering, and step session id shape. It does not need compatibility with existing issue-level ledgers. At the end, tests can replay a new-format ledger and reconstruct which steps and attempts of a workflow run are pending, running, completed, failed, or interrupted, without changing daemon recovery behavior yet.

Child Plan 2 is `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`. It builds the pure recovery planner for workflow progress. At the end, tests can feed durable step facts plus refreshed Linear issue states and receive conservative recovery actions: preserve completed terminal artifacts, block or park unsafe interrupted command steps, identify recoverable agent steps, schedule retry only when safe, reject workflow or issue drift, and cleanup terminal run roots. Parallel recovery cases should use explicit fixture DAGs with `max_parallel_steps` greater than one and distinct workspace names rather than relying on the current dogfood workflows to be parallel.

Child Plan 3 is `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`. It wires the planner into workflow execution. At the end, daemon restart can resume a workflow run from durable step checkpoints without pi session continuation: completed steps are skipped, artifacts are available to downstream prompts, pending steps can continue, and interrupted steps follow the policy from Child Plan 2.

Child Plan 4 is `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`. It replaces the obsolete issue-level session continuation plan with step-scoped pi persistence. At the end, an interrupted agent step can launch pi with the recorded session file from the exact same step workspace and send a recovery prompt. This plan must include a mandatory real-pi probe.

Child Plan 5 is `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`. It makes recovery understandable and operable. At the end, `scherzoctl`/session views expose recovered, interrupted, resumed, parked, and inspection-needed states; README documents recovery guarantees and limits; and pi session/artifact retention policy is explicit.

## Milestones

Milestone 1 completes this umbrella architecture and backlog setup. The acceptance is that this file exists, the stale issue-level pi continuation plan is no longer treated as implementation-ready, and Linear has child tickets for the five child plans.

Milestone 2 writes and reviews Child Plans 1 and 2 before implementation. These plans must de-risk the hard state model first: durable facts, stable run identity, attempt identity, artifact durability, workflow/issue drift detection, ledger/snapshot schema replacement, old-state reset behavior, and pure recovery decisions. No pi session continuation work should start before these are reviewed.

Milestone 3 writes and reviews Child Plan 3. This is the first plan that should change workflow execution semantics. It must remain useful even if pi session continuation is never implemented.

Milestone 4 writes and reviews Child Plan 4. This plan can then safely reintroduce pi session continuation as a step-scoped feature, using the durable workflow context from the earlier plans.

Milestone 5 writes and reviews Child Plan 5. This closes operational visibility, documentation, and local retention policy.

## Plan of Work

Create this umbrella file under `docs/plans/workflow-resumption-umbrella.md`. Mark the old `docs/plans/hardening-04-pi-session-continuation.md` as superseded or keep it only as historical context so no implementer follows it accidentally.

Create Linear tickets in the `LIV` team and Scherzo project for each child plan. Each ticket should be a planning ticket, not a direct implementation ticket. It should reference this umbrella plan, name the canonical checked-in plan file, and instruct the assignee to produce a reviewed ExecPlan before implementation.

When writing child plans, preserve self-containment. Each child plan should repeat the relevant definitions from this umbrella rather than relying on memory. Each child plan should include deterministic tests, validation commands, rollout and recovery behavior, and a clear boundary with the other child plans. Before freezing a child plan, re-read the current versions of the workflow YAML files, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, and `src/scherzo/state/recovery.gleam`; if they differ from this umbrella, update the child plan's verified facts rather than copying stale claims.

## Concrete Steps

1. From the repository root, create `docs/plans/workflow-resumption-umbrella.md` with this recovery model and child roadmap. This is complete.

2. Add a supersession note to `docs/plans/hardening-04-pi-session-continuation.md` or otherwise prevent it from being selected for implementation as-is. This is complete.

3. Create the Child Plan 1 Linear ticket with title `Write plan: durable workflow step checkpoints for resumption`. Its acceptance is a new ExecPlan at `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`. This is complete as LIV-54.

4. Create the Child Plan 2 Linear ticket with title `Write plan: startup recovery planner for workflow DAG progress`. Its acceptance is a new ExecPlan at `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`. This is complete as LIV-55.

5. Create the Child Plan 3 Linear ticket with title `Write plan: recovery-aware workflow runner execution`. Its acceptance is a new ExecPlan at `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`. This is complete as LIV-56.

6. Create the Child Plan 4 Linear ticket with title `Write plan: step-scoped pi session continuation`. Its acceptance is a new ExecPlan at `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`. This is complete as LIV-57.

7. Create the Child Plan 5 Linear ticket with title `Write plan: workflow recovery operator UX and retention policy`. Its acceptance is a new ExecPlan at `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`. This is complete as LIV-58.

8. Update the Outcomes & Retrospective section of this umbrella plan with the created Linear identifiers. This is complete.

9. Keep runtime code unchanged as part of this umbrella. If validation is desired after documentation edits, from the repository root run `direnv exec . gleam test` only as a repository health check; docs and Linear ticket creation do not require it. The expected result for a healthy tree is that all Gleam tests pass.

10. When starting LIV-54, write `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`. Before drafting, re-read `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and the workflow YAML files under `.scherzo/workflows/`. The plan must close the schema-version, old-state reset, stable-run-id, attempt-index, operator-session-id, workflow-fingerprint, issue-fingerprint, artifact-store, and artifact-write-ordering decisions before implementation. Do not spend design complexity on backward-compatible replay of old issue-level ledgers.

11. When starting LIV-55, write `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`. It must define pure planner inputs and outputs, include fixture DAGs for sequential and parallel cases, and state how refreshed Linear issue state gates retry, park, resume, and cleanup decisions.

12. When starting LIV-56, write `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`. It must specify how `src/scherzo/workflow_run.gleam` consumes the planner output, skips durable completed or failed-continued steps, restores the full artifact shape for downstream prompts, keeps current behavior unchanged when no workflow recovery facts exist, and uses unique operator step session ids for repeated step attempts.

13. When starting LIV-57, write `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`. It must depend on the step-attempt and workspace facts from the earlier plans, store pi session files by step attempt, validate cwd before launch, and include a mandatory real-pi probe.

14. When starting LIV-58, write `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md`. It must cover `scherzoctl` and session-view status terms, README/operator documentation, local artifact and transcript retention, cleanup, and sensitive-data handling.

## Testing and Falsifiability

This umbrella is falsified if a child plan can be written that still treats pi session continuation as issue-level only, if completed workflow steps and artifacts have no durable representation, if interrupted command steps are automatically rerun without an explicit safety declaration, if recovery can attach a pi session file to a different workspace path, if recovery silently ignores workflow or issue drift, if recovered work allocates a fresh run-root id without an explicit abandonment policy, or if repeated daemon restarts can double-count the same interrupted step.

Future implementation tests must include at least these scenarios:

- A single-agent `research` workflow crashes during the agent step and recovers according to policy.
- A multi-step `implementation` workflow crashes after `implement` succeeds but before `apply_feedback`; recovery does not rerun `implement` unless explicitly invalidated.
- A fixture workflow with `max_parallel_steps: 2` and distinct workspaces has one completed parallel step and one interrupted parallel step; downstream work remains blocked until dependencies are resolved.
- A fixture workflow with `on_failure: continue` has a failed upstream command step with a durable failed artifact; recovery treats that dependency as terminal and preserves its artifact for downstream prompt rendering.
- A command step interruption defaults to park or inspection-needed rather than automatic rerun.
- A pi session resume launches with the exact recorded `sessionFile` and exact recorded step workspace cwd.
- A fresh rerun or continuation of an interrupted step creates a distinct attempt identity, uses a non-colliding operator-visible session id, and links back to the interrupted attempt.
- A missing workspace, missing artifact, corrupt artifact, cleanup-targeted artifact path, or missing session file follows documented policy.
- A crash after artifact payload write but before step-finished record does not mark the step reusable, and a crash after step-finished without a recoverable artifact record does not unblock downstream work.
- Restarting twice over the same ledger does not append duplicate interruption/counter records or allocate duplicate attempt indexes for the same recovery action.
- An existing issue-level ledger and an existing projection snapshot from before workflow recovery records were introduced are rejected, archived, deleted, or reinitialized exactly according to Child Plan 1's documented reset policy; they do not need to load successfully.
- A workflow YAML file, prompt file, workspace root, or refreshed Linear issue fingerprint differs from the recorded run facts; recovery refuses silent continuation and returns the documented park or inspection-needed action.

## Validation and Acceptance

Accept this umbrella when:

- This file exists under `docs/plans/` and states that workflow-step scoped recovery supersedes issue-level pi continuation.
- The child roadmap is explicit and sequenced so durable step facts and recovery planning come before pi session continuation.
- Linear tickets exist for all child plans and are recorded in Outcomes & Retrospective.
- The stale hardening-04 pi session plan is not presented as ready to implement unchanged.
- The repository facts in this plan match the current checked-in workflow YAML and durable-state modules, or any drift is documented in Surprises & Discoveries before child plans copy the facts.

Accept the full future recovery program only when deterministic restart tests prove Scherzo can recover a multi-step workflow without rerunning completed durable steps, without losing artifacts, without resuming pi in the wrong workspace, without colliding operator step session ids across attempts, without silently ignoring workflow or issue drift, and without automatically rerunning unsafe command steps. Compatibility with existing issue-level ledgers or snapshots is not an acceptance requirement; a clear reset path is sufficient.

## Rollout, Recovery, and Idempotence

The umbrella has no runtime rollout. Child plans should roll out additively. Durable records can be added before recovery behavior changes. Recovery planner changes should be tested in pure modules before daemon startup uses them. Runner behavior should initially preserve current behavior unless recovery state is present. Pi session persistence must remain opt-in.

If any child plan uncovers a false assumption in this umbrella, update this file's Surprises & Discoveries and Decision Log before proceeding. If implementation stops halfway through the program, the system should remain safe: durable step facts and copied artifacts may exist without being used, but daemon restart should not perform new automatic resume behavior until the relevant recovery planner and runner integration are complete. Breaking ledger and snapshot changes are allowed; the child plan must document how an operator safely discards, archives, or reinitializes old local state before runtime emits new workflow recovery records.

## Artifacts and Notes

Expected child ticket descriptions should include this standard note:

    This is a planning ticket, not a direct implementation ticket. Produce a reviewed ExecPlan under docs/plans/ that follows docs/plans/workflow-resumption-umbrella.md. Do not implement runtime changes in the same ticket unless the operator explicitly changes the scope.

The obsolete plan to avoid implementing as-is is:

    docs/plans/hardening-04-pi-session-continuation.md

The replacement child plan for pi sessions should be:

    docs/plans/LIV-57-step-scoped-pi-session-continuation.md

## Interfaces and Dependencies

The child plans should converge on durable identifiers equivalent to:

    WorkflowRunKey(
      issue_id: String,
      issue_identifier: String,
      workflow_id: String,
      run_id: String,
      workflow_definition_hash: String,
      issue_fingerprint: String,
      workspace_root: String,
    )

    WorkflowStepAttemptKey(
      issue_id: String,
      workflow_id: String,
      run_id: String,
      step_id: String,
      workspace_name: String,
      attempt_index: Int,
    )

    WorkflowStepAttempt(
      key: WorkflowStepAttemptKey,
      operator_session_id: String,
    )

    WorkflowStepWorkspace(
      key: WorkflowStepAttemptKey,
      run_root: String,
      workspace_path: String,
    )

    WorkflowStepArtifactRef(
      key: WorkflowStepAttemptKey,
      artifact_kind: String,
      artifact_path_or_inline_payload: String,
      content_hash: Option(String),
      recorded_at_ms: Int,
    )

    WorkflowStepRecoveryLink(
      key: WorkflowStepAttemptKey,
      recovers_attempt_index: Int,
      mode: String,
    )

    PiStepSession(
      key: WorkflowStepAttemptKey,
      session_id: Option(String),
      session_file: String,
      attached_at_ms: Int,
    )

Exact Gleam type names are left to the child implementation plans, but the identity shape is not optional. Any design that cannot identify `run_id`, `workflow_id`, `step_id`, `attempt_index`, and the exact `workspace_path` for a resumed agent step should be rejected. Any design that allocates a new run-root id for recovered work without a deliberate abandonment policy, stores artifacts only in a cleanup-targeted workspace, ignores workflow or issue drift, or lacks a clear old-state reset path should also be rejected.

## Open Questions and Clarifications Needed

No open questions remain for this umbrella. The canonical child plan files are the checked-in `docs/plans/LIV-54-durable-workflow-step-checkpoints-for-resumption.md`, `docs/plans/LIV-55-startup-recovery-planner-workflow-dag-progress.md`, `docs/plans/LIV-56-recovery-aware-workflow-runner-execution.md`, `docs/plans/LIV-57-step-scoped-pi-session-continuation.md`, and `docs/plans/LIV-58-workflow-recovery-operator-ux-retention.md` files.

## Review Note

Reviewed and revised on 2026-05-03 to make the umbrella implementable against the current repository tree. The changes correct stale workflow assumptions, add stable logical run identity and step-attempt identity, require unique operator session ids for repeated attempts, require workflow and Linear issue drift detection, tighten artifact durability and failed-continued step handling, and make ledger/snapshot schema replacement plus old-state reset behavior a first-class requirement for Child Plan 1. Backward compatibility with existing issue-level ledgers and snapshots is explicitly not required.

Updated on 2026-05-05 to reconcile the umbrella and Linear ticket descriptions with the actual checked-in child plan filenames. The original `workflow-resumption-0x-...md` filenames are historical draft names only.
