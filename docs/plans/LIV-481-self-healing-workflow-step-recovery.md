# Add self-healing workflow step recovery

## Purpose / Big Picture

Scherzo should be able to make one bounded, auditable repair attempt when a workflow step fails for a fixable reason such as failing tests, formatting, lint, or incomplete agent work. After this change, a workflow author can add `recover` defaults to a workflow, override them per step, and see a failed step run a nested recovery worker before the workflow accepts the original failure. If the recovery worker requests retry, Scherzo retries the original step unchanged and records that the workflow succeeded or failed after recovery rather than as a clean run.

## Problem Framing and Constraints

Workflows currently encode many repair paths as ordinary DAG steps, which makes the happy path harder to read and makes recovery look like product intent rather than runtime remediation. The new mechanism must stay optional, bounded by configuration, and visible enough for operators to understand what happened later. It must not become a rules engine or a hidden policy classifier; existing Pi configuration, workspace permissions, and model settings remain the safety boundary.

The repository already has workflow DAG parsing in `src/scherzo/workflow_dag.gleam`, runtime execution and checkpointing in `src/scherzo/workflow_run.gleam` and `src/scherzo/workflow_checkpoint.gleam`, retained artifacts in `src/scherzo/state/artifact_store.gleam`, and durable projection in `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam`. The design should extend those surfaces rather than introduce a separate scheduler.

## Strategy Overview

Add `recover` as workflow-DAG metadata, not as a normal DAG node. A step gets an effective recovery config by shallow-merging its optional step config over the optional workflow config. `enabled: false` disables recovery for that step, and `attempts` counts recovery-worker sessions after the step would otherwise fail. Recovery attempts are independent from the original step attempt index; a retry uses the same original step definition and the next normal step attempt number.

At runtime, when a step result would be recorded as failed, the runner records the failed attempt and starts a nested recovery worker in the same workspace with system-supplied failure context. The worker reports either `retry_requested` or `gave_up` through a small structured protocol. `retry_requested` requeues the original step unchanged. `gave_up`, a crash, a timeout, or exhausted recovery attempts lets the original failure proceed. Recovery records and artifacts link the failed step attempt, the recovery worker/session, the retry attempt when present, and the retry outcome when known. The externally visible terminal outcome names for the MVP are fixed as `succeeded_after_recovery` and `failed_after_recovery`; clean runs keep the existing observable success and failure outcomes, and internal implementation details may map to these names only at projection or history boundaries if that keeps compatibility simpler.

## Alternatives Considered

One alternative is to keep adding explicit repair steps to each workflow. That is simple but leaves every workflow author to rediscover the same retry-and-fix pattern, and it makes normal workflow history harder to read.

Another option is to build a classification engine that decides whether a failure is a missing import, flaky test, formatter issue, or unsafe product change. That is larger than the MVP and would add policy complexity before Scherzo has enough recovery data. The chosen approach gives the recovery agent context and a strict retry protocol while keeping classification inside the prompt.

A third option is to reuse existing structured-output retry only. That helps malformed structured output, but it does not cover command failures, test failures, or ordinary agent failures, and it cannot make workspace changes before retrying the original step.

## Risks and Countermeasures

The main risk is an unbounded loop. The countermeasure is to treat recovery attempts as a separate configured budget and to make exhausted attempts final. Tests must prove `attempts: 1` can produce at most one recovery worker and at most one retry of the failed original step.

A second risk is hiding product or workflow intent changes inside recovery. The recovery prompt must instruct the agent to make minimal changes toward the original step, avoid redesigning the workflow, and give up when recovery is not appropriate. The retry always runs the original step definition unchanged.

A third risk is losing auditability. The checkpoint ledger, projection snapshot, retained artifacts, and human history rendering must show the failed attempt, recovery attempt, decision, retry attempt, and final status. Recovery must not be represented only in transient logs.

A fourth risk is breaking existing completed and failed workflow semantics. Clean runs should keep their current behavior. New recovered outcomes should be additive, and existing failed-run selectors such as retry-step repair must treat `failed_after_recovery` as a failed terminal outcome.

## Scope Boundaries

In scope are workflow-level and step-level `recover` config, shallow merge semantics, bounded recovery-worker execution, retry/give-up protocol, retrying the original step unchanged, durable records, retained recovery artifacts, projection/history visibility, and recovered success or failure outcomes.

Out of scope are a failure-classification rules engine, recovery-specific workspace privilege changes, changing product requirements from recovery, overriding the retry prompt or input, web UI work, and automatic resumption of a partially running recovery worker after daemon restart. If the daemon restarts during recovery, durable records should make the state inspectable and safe rather than silently continuing unknown work.

## Milestones

Milestone 1 defines and validates the configuration contract. At the end, workflow YAML can express global defaults, step overrides, and step disablement, and invalid recovery maps fail with clear diagnostics.

Milestone 2 defines the recovery worker contract and retained artifact shape. At the end, Scherzo has a structured decision protocol, a default recovery prompt, and durable record/projection types that can represent retry, give-up, crash, and timeout.

Milestone 3 integrates recovery into step execution. At the end, a failed step with effective recovery starts a nested worker, `retry_requested` requeues the original step unchanged, `gave_up` preserves the original failure, and the configured attempt budget is enforced.

Milestone 4 makes recovery observable. At the end, retained artifacts and workflow history show nested recovery activity, recovered workflow outcomes are distinguishable from clean outcomes, and existing failed-run repair logic still recognizes recovered failures.

Milestone 5 completes validation and operator documentation. At the end, unit tests, projection tests, history/rendering tests, lint, formatting, and runbook examples prove the required positive and negative paths. The documentation evidence must include the implemented `recover` YAML shape, the default recovery prompt and structured-output schema names, the retained artifact layout, and the operator command or helper output used to inspect nested recovery history.

## Progress

- [x] (2026-05-21) Reviewed the current workflow DAG parser, workflow runner, checkpoint writer, artifact store, projection, scheduler, runtime bundle, and existing workflow tests to frame an implementation-sized plan.
- [x] (2026-05-21) Authored this concise review document and separated mechanical implementation detail into the structured implementation pack.
- [x] (2026-05-21) Incorporated review feedback by fixing observable recovered outcome names and adding acceptance evidence for no-op rollout behavior, interruption safety, negative recovery-result/conflict cases, and documentation/helper outputs.

## Decision Log

- Decision: Model recovery as runtime metadata and nested records, not as a normal workflow step.
  Rationale: The recovery step must attach to a failed step attempt without changing dependency semantics or appearing as another DAG node.
  Date: 2026-05-21

- Decision: Use a structured recovery decision with `retry_requested` and `gave_up` for the MVP.
  Rationale: It is simpler and more testable than free-form response parsing while avoiding a larger classification engine.
  Date: 2026-05-21

- Decision: Preserve the original step definition for retries.
  Rationale: Recovery should fix the workspace or finish bounded remediation, not rewrite workflow intent.
  Date: 2026-05-21

- Decision: Use `succeeded_after_recovery` and `failed_after_recovery` as the stable observable recovered terminal outcomes in the MVP.
  Rationale: Acceptance, history rendering, and repair selection need fixed names so implementers do not defer a user-visible compatibility decision.
  Date: 2026-05-21

## Validation and Acceptance

Validation must be evidence-based. Parser tests must prove global recovery, step override, step disablement, missing prompt, invalid attempts, and malformed field behavior. Runtime tests must use fake command and agent dependencies to prove recovery starts only after failure, uses the same workspace, respects the attempt budget, records give-up/crash/timeout paths, and retries the original step with the same prompt/config and the next attempt index. Rollout regression tests must prove workflows without `recover` and steps with `recover.enabled: false` do not start a recovery worker, do not write recovery records or artifacts, and keep the existing clean success and clean failure outcomes. Projection/history tests must prove retained records include step id, failed attempt id, recovery attempt number, worker/session id, model, prompt path, timestamps, result, summary, retry attempt id, and retry result. End-to-end workflow tests must distinguish clean success from `succeeded_after_recovery` and clean failure from `failed_after_recovery`, using those exact names as the stable observable MVP outcome strings.

Restart and interruption safety must have explicit evidence. Add a checkpoint/projection or startup recovery test that simulates a recovery-start record with no finish record, then proves Scherzo leaves an inspectable interrupted recovery state in history or retained artifacts and does not silently continue an unknown in-flight recovery worker after daemon restart. Negative-path evidence must cover missing, malformed, and duplicate recovery-result submissions, with stable protocol errors and retained audit records. Conflict evidence must cover immutable recovery artifact or checkpoint write conflicts: the implementation should surface the write error and preserve existing recovery evidence rather than overwriting it.

Docs and helper migration evidence are publish blockers. Before publish, the implementation must include or update `docs/runbooks/workflow-step-recovery.md`, any concise README pointer needed for workflow authors, the default recovery prompt, the provider and canonical recovery-result schemas, and the operator-facing history/helper output that shows nested recovery activity. Run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; all must pass or any pre-existing unrelated lint inventory must be explicitly identified. No browser evidence is required. Optional dogfood evidence may be collected after handoff by enabling recovery on a small local workflow that fails once, requesting retry from the recovery worker, and confirming the retained history shows the nested recovery timeline.

## Rollout, Recovery, and Idempotence

The rollout is additive: workflows without `recover` must behave exactly as before. A step with `recover.enabled: false` must also behave as before even when workflow defaults exist; this no-op claim must be backed by runtime tests that assert no recovery worker/session is started and no recovery artifact or record is written. Recovery records should be append-only and safe to inspect after interruption. If recovery crashes, times out, produces invalid structured output, or exhausts its budget, Scherzo should fail or continue according to the original step failure policy and record the recovery result for audit.

Repeated execution of tests and local workflows should not require manual cleanup beyond existing `test/tmp` cleanup helpers. Retained artifacts are immutable by reference; if a write conflict is detected, the implementation should surface a checkpoint/artifact error rather than overwriting recovery evidence.

## Open Questions and Clarifications Needed

The MVP fixes the observable recovered terminal outcome strings as `succeeded_after_recovery` and `failed_after_recovery`, while preserving compatibility for existing clean outcomes where possible. The future design can revisit whether recovery workers may override retry input, whether daemon restart can resume an in-flight recovery worker automatically, and how much nested recovery history should be exposed in richer UI surfaces beyond the local ledger/history view.
