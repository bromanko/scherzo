# Decompose the workflow runner safely

This ExecPlan v2 review document frames the decomposition plan for LIV-525. It is intentionally a planning artifact: mechanical extraction steps, tests, module interfaces, and derivative implementation slices are captured in Scherzo's structured implementation pack and canonical bundle.

## Purpose / Big Picture

After this plan is accepted, Scherzo maintainers have a ranked, test-first sequence for reducing `src/scherzo/workflow_run.gleam` from one central runtime module into smaller owned subsystems without changing workflow behavior. The immediate visible result is a reviewed plan and implementation pack, not moved production code.

The long-term outcome is that changes to contract IO, structured-output retry behavior, workspace preparation, step workers, recovery, cleanup, and workstream handoff can be made in focused modules with characterization tests that prove existing workflows still run the same way.

## Problem Framing and Constraints

`src/scherzo/workflow_run.gleam` currently mixes public execution entrypoints, scheduler loop control, contract input/output manifests, workstream handoff, workspace preparation, process spawning and monitoring, command and agent execution, structured-output artifact handling, step recovery, checkpointing, and cleanup. That concentration makes recovery and structured-output changes riskier than their actual business scope.

The hard constraints are behavior preservation and incremental migration. The plan must not change workflow DAG semantics, public workflow contracts, structured-output behavior, workstream behavior, checkpoint meaning, or scheduler behavior. This ticket also must not implement the extraction directly; it should hand off a clear derivative implementation sequence.

## Strategy Overview

The decomposition should keep `workflow_run.gleam` as the public facade and high-level orchestration loop while moving bounded concerns into `src/scherzo/workflow_run/` submodules. The first recommended slice is contract input/output materialization plus workstream handoff, because those functions are already clustered, have direct workflow-run tests, and provide high value without touching concurrent step execution.

The proposed ownership boundaries are: `workflow_run/contract_io.gleam` for contract run values, input and output manifest recording, output source materialization, and manifest diagnostics; `workflow_run/workstream_handoff.gleam` for post-output workstream ledger emission; `workflow_run/workspace_preparation.gleam` for ready-batch workspace selection and preparation; `workflow_run/step_worker_pool.gleam` for spawning, monitoring, sibling cancellation, and after-step process supervision; `workflow_run/step_execution.gleam` for command-versus-agent dispatch, prompt context, model resolution, command env, and artifacts; `workflow_run/structured_output_step.gleam` for runtime structured-output validation, artifact writing, and retry finishing; `workflow_run/recovery_execution.gleam` for step recovery and checkpoint-guard interaction; `workflow_run/terminal_policy.gleam` for terminal checkpoint, cleanup, and failure-report policy; and a small shared-types seam only when needed to avoid import cycles while preserving the existing `scherzo/workflow_run` facade.

## Alternatives Considered

One alternative is to leave the module intact and rely on more comments. That was rejected because comments do not create compile-time ownership boundaries or smaller review surfaces.

A second alternative is a single large rewrite that moves all responsibilities at once. That was rejected because the module contains concurrency, checkpointing, and recovery paths where small behavior drift is costly and difficult to review.

A third alternative is to extract the scheduler or DAG semantics first. That was rejected because `workflow_scheduler` and `workflow_dag` already exist as separate modules; the immediate risk is the runner glue around those modules, not the graph model itself.

## Risks and Countermeasures

The main risk is behavioral drift during code motion. The countermeasure is to add or identify characterization tests for each extraction before moving code, then run full format, test, glinter, and Scherzo lint gates after every slice.

A second risk is breaking public callers by moving exported types such as `WorkflowRunSuccess`, `WorkflowRunFailure`, `Dependencies`, `ContractRunValues`, `ResumeState`, or `StepContext`. The countermeasure is to preserve the current public facade during the migration and only re-export or privatize symbols in a dedicated compatibility slice.

A third risk is introducing import cycles or public-type breakage while moving code that currently references `workflow_run.Dependencies`, `workflow_run.StepContext`, `workflow_run.ContractRunValues`, and `workflow_run.ResumeState` from other modules. The countermeasure is to keep the public facade stable, use narrow child-module input records for early slices, and schedule any shared-type migration as its own compatibility-tested slice.

A fourth risk is introducing process supervision regressions while extracting step workers and after-step hooks. The countermeasure is to defer that extraction until contract and workspace slices are green, then pin crash, sibling-cancellation, serial-workspace, and DAG-order tests before moving process code.

A fifth risk is future regrowth of `workflow_run.gleam`. The countermeasure is to add a structural guardrail after the first extractions: document module ownership and enforce, through Scherzo lint or a focused source-structure test, that new contract IO, structured-output retry, worker-pool, and recovery logic does not get added back to the facade.

A sixth risk is accepting the planning ticket after checking only `src/scherzo/workflow_run.gleam`, which could miss accidental extraction edits elsewhere in `src/` or `test/`. The countermeasure is to make no-source-extraction evidence explicit: before publish, run a changed-file check scoped to `src` and `test` and require no output for this planning task.

## Scope Boundaries

In scope for the plan are responsibility inventory, proposed module names, ranked extraction order, first-slice recommendation, behavior-preservation tests, migration strategy, ownership after extraction, and guardrails against future regrowth.

Out of scope are changing workflow DAG semantics, changing public workflow or contract behavior, changing structured-output semantics, changing workstream behavior, changing checkpoint formats, or performing extraction work in this planning ticket. After decomposition, `workflow_run.gleam` should retain public entrypoints, compatibility types or re-exports, dependency assembly, and the high-level run loop until a later intentionally scoped state-machine refactor is justified.

## Milestones

Each milestone below maps to a retained implementation-pack concrete step. Completion is not proven by prose alone; it is proven by the matching pack slice naming the module and file ownership, the behavior-preservation tests for that boundary, and the full validation gates that must pass after the slice.

First, establish the safety net and ownership guardrails. The outcome is a documented responsibility map, explicit first-slice tests, and an agreed rule that the facade must not regain extracted logic. The proof anchor is the baseline and characterization slice in the retained implementation pack, including the planning-ticket changed-file check that shows no accidental `src/` or `test/` extraction edits were made here.

Second, extract contract IO and workstream handoff. The outcome is dedicated ownership for input manifests, output manifests, output blobs, absent-output diagnostics, idempotent output-manifest reuse on resume, and handoff ledger emission. The proof anchor is the pack slice that names `src/scherzo/workflow_run/contract_io.gleam`, `src/scherzo/workflow_run/workstream_handoff.gleam`, their caller points in the facade, the contract and handoff characterization tests, and the validation gates.

Third, extract workspace preparation. The outcome is a module that owns ready-batch workspace locking, fresh-versus-recovered prepare calls, attempt-index updates, and prepare checkpoint failure behavior. The proof anchor is the pack slice that names `src/scherzo/workflow_run/workspace_preparation.gleam`, its narrow prepare dependency record, workspace serialization tests, prepare-failure tests, and validation gates.

Fourth, extract step worker spawning and after-step supervision. The outcome is a process module that owns spawn, monitor, demonitor, sibling cancellation, crash diagnostics, fatal-versus-continue handling, and after-step process result collection. The proof anchor is the pack slice that names `src/scherzo/workflow_run/step_worker_pool.gleam`, injected step and after-step runners, crash/cancellation/ordering tests, and validation gates.

Fifth, extract command and agent step execution boundaries. The outcome is a step-execution module that owns `StepContext`, command environment, prompt rendering, model application, agent invocation, and command artifact construction while preserving the facade API. The proof anchor is the pack slice that names `src/scherzo/workflow_run/step_context.gleam` and `src/scherzo/workflow_run/step_execution.gleam`, public facade compatibility checks, command/agent behavior tests, and validation gates.

Sixth, extract structured-output runtime handling. The outcome is a module that owns tool-spec env setup, validation, retained artifact writing, retry prompt invocation, retry diagnostics, and transient Pi failure retry behavior. The proof anchor is the pack slice that names `src/scherzo/workflow_run/structured_output_step.gleam`, artifact and retry behavior tests, provider-independent validation tests, and validation gates.

Seventh, extract recovery execution and terminal policy last. The outcome is dedicated ownership for step recovery checkpoints, checkpoint-guard restore behavior, terminal workflow checkpointing, cleanup warnings, and failure reports after lower-risk seams are already proven. The proof anchor is the pack slice that names `src/scherzo/workflow_run/recovery_execution.gleam` and `src/scherzo/workflow_run/terminal_policy.gleam`, recovery/cleanup/checkpoint tests, DAG and scheduler parity checks, and validation gates.

## Progress

- [x] (2026-05-25) Inspected `src/scherzo/workflow_run.gleam`, existing workflow-run tests, related workflow modules, and the prepared output target for LIV-525.
- [x] (2026-05-25) Drafted this human-reviewable ExecPlan review document for the decomposition sequence.
- [x] (2026-05-25) Incorporated review feedback by strengthening no-source-extraction acceptance evidence, stating manual/dogfood status, aligning acceptance with DAG and scheduler preservation, and adding milestone proof anchors tied to retained implementation-pack slices.

## Decision Log

- Decision: Make contract IO plus workstream handoff the recommended first implementation slice.
  Rationale: Those functions are already contiguous, have visible tests in `test/workflow_run_test.gleam`, and avoid the highest-risk process and recovery code.
  Date: 2026-05-25

- Decision: Use submodules under `src/scherzo/workflow_run/` rather than many flat `workflow_*` module names.
  Rationale: A namespace under the facade makes ownership clear while keeping the existing public `scherzo/workflow_run` import path stable.
  Date: 2026-05-25

- Decision: Leave DAG semantics and scheduler semantics outside this decomposition.
  Rationale: Separate `workflow_dag` and `workflow_scheduler` modules already own those domains; this plan targets runner glue and side-effect orchestration.
  Date: 2026-05-25

- Decision: Treat acceptance evidence, milestone proof anchors, manual/dogfood status, and DAG/scheduler preservation as obligations in both this review document and the structured implementation pack.
  Rationale: Review feedback should not live only in prose or only in the pack; implementers and reviewers need matching human-readable and mechanical acceptance criteria.
  Date: 2026-05-25

## Validation and Acceptance

Planning acceptance is verifiable before publish by confirming this file exists at `docs/plans/LIV-525-workflow-runner-decomposition.md`, the structured implementation-pack submission for LIV-525 was captured by Scherzo, and no source extraction was made in this ticket. A reviewer can check the last condition with `git diff --name-only -- src test` and expect no output. If reviewing through jj, `jj status --no-pager` or an equivalent status command may show this review document and workflow metadata, but it must show no changed files under `src/` or `test/` for this planning task.

Content acceptance is verifiable by manual review of this document and the captured implementation pack: every derivative item must name a subsystem boundary, proposed module names, behavior-preservation tests, risk level, migration strategy, expected ownership after extraction, and the proof anchor that ties the milestone to a retained implementation-pack slice. The implementation pack must also include full validation gates: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` for each future extraction slice.

No manual browser or live dogfood run is a pre-publish blocker for this planning handoff because no runtime code changes are made here. A deferred human/operator dogfood check after follow-up implementation may run one local workflow and one recovery-resume fixture through the daemon, but that check must supplement rather than replace the automated characterization tests and lint gates.

Post-handoff implementation acceptance is deferred to the follow-up implementation workflow. Each extraction slice must provide red-or-characterization evidence before code motion, green test evidence after code motion, and explicit evidence that public workflow behavior, DAG ordering, scheduler behavior, contract IO, structured-output handling, workstream handoff, checkpoint records, recovery, and cleanup behavior remain unchanged.

## Rollout, Recovery, and Idempotence

Rollout should be additive and slice-based. Each extracted module should be introduced behind the existing `scherzo/workflow_run` public facade, with tests green and public imports preserved before any compatibility cleanup.

Recovery is ordinary source rollback: if a slice regresses behavior, revert that slice while keeping earlier green slices. Generated test fixtures, reports, and temporary transcripts should live under `tmp/` and be safe to delete.

The future implementation should be idempotent at the workflow level. Re-running tests or rerunning a workflow recovery fixture should produce the same manifests, checkpoint events, handoff records, cleanup policy, and failure diagnostics as before extraction.

## Open Questions and Clarifications Needed

No blocking questions remain for the planning handoff. A non-blocking implementation choice remains: after the first extraction lands, maintainers should decide whether the anti-regrowth guardrail is best enforced as a Scherzo lint rule, a focused Gleam source-structure test, or both.
