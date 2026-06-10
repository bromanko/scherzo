# Runtime workflow step recovery execution

## Purpose / Big Picture

Scherzo should be able to make one bounded, automatic repair pass when a workflow step that opted into recovery would otherwise fail the workflow. After LIV-488, a failed original attempt is durably recorded, a nested recovery worker runs in the same workspace with the configured prompt and model, and a valid `recheck` decision rechecks the original step definition unchanged at the next normal attempt index. If recovery is absent, disabled, continued by `on_failure: continue`, gives up, crashes, times out, emits invalid output, or exhausts its budget, the workflow keeps the same original failure behavior operators already understand.

## Problem Framing and Constraints

The foundation work from LIV-482/PR 268 added workflow YAML parsing for `recover`, prompt bundling, the recovery-result protocol, recovery ledger records, projection fields, and retained artifact helpers, but `src/scherzo/workflow_run.gleam` still treats `recover` as inert metadata. The runtime slice must activate only the minimal execution path needed by LIV-488 without changing parser semantics, workflow DAG scheduling semantics, terminal recovered outcome names, or operator history rendering.

The important constraint is behavioral compatibility. Recovery must not run for `on_failure: continue`, because that step is not otherwise fatal. Recovery must also be a no-op when the effective config is absent or disabled. The recheck must reuse the original command or agent step definition, prompt, workspace reference, structured-output contract, and model settings for the normal step; only the nested recovery worker uses the recovery prompt and optional recovery model.

## Strategy Overview

Add a small recovery decision layer around the existing step-failure path in `workflow_run.gleam`. When a prepared step returns a failed artifact, the runner first asks `workflow_dag.effective_recovery_config(dag, step)` and also checks that the step would otherwise be fatal under its failure policy. If recovery is unavailable, disabled, invalid, or budget-exhausted, the runner follows the current failure path unchanged.

For a recoverable fatal failure, the runner records the failed original attempt using the same checkpoint artifact and step-finished machinery as an ordinary failure before starting recovery. It then launches a nested agent worker in the already-prepared workspace using `workflow_step_recovery.prompt` for bounded failure context and `workflow_step_recovery.tool_spec` for the required tool-call output. A parsed `recheck` records `workflow_step_recovery_finished` with `retry_attempt_index` set to the next normal attempt index, marks the step pending again, and lets the existing scheduler prepare and run that same original step definition. A parsed `gave_up`, recovery worker failure, timeout, invalid output, checkpoint failure, artifact conflict, or exhausted `recover.attempts` budget records what can be recorded and then preserves the original failure behavior.

## Alternatives Considered

One alternative was to model recovery as an explicit hidden DAG node. That would make recheck scheduling visible to the scheduler but would overcomplicate dependency semantics and risk exposing internal recovery workers to prompt templating, outputs, and operator surfaces that are not part of LIV-488.

A second alternative was to rerun the failed step with a modified prompt that includes recovery instructions. That is rejected because the acceptance criteria require the original step definition to be requeued unchanged and because the recovery worker is supposed to make workspace changes before the original step is rechecked.

A third alternative was to defer all crash, timeout, invalid-output, and budget behavior to hardening. That is too weak for runtime activation: LIV-488 must at least prove these paths do not change the original failure result. More exhaustive interruption, cleanup, and resumed-mid-recovery hardening remains deferred.

## Risks and Countermeasures

The main risk is an infinite repair loop. The countermeasure is to treat `recover.attempts` as a per-step recovery-worker budget for the current workflow run, increment it before each nested recovery worker starts, and refuse further recovery after the budget is consumed.

A second risk is losing the original failure artifact or writing it twice. The countermeasure is to split original-attempt finalization into a helper that is called exactly once before recovery and is skipped when a recheck result is applied back to the scheduler.

A third risk is accidentally changing existing failure policy behavior. The countermeasure is explicit tests for absent recovery, `recover.enabled: false`, and `on_failure: continue`, all of which must show no recovery start/finish records and the existing result shape.

A fourth risk is unbounded or secret-leaking recovery context. The countermeasure is to use the existing bounded `StepArtifact` summary, failure code, status, and stderr context from `workflow_step_recovery.prompt`, with existing artifact limits and redaction paths, rather than passing full transcripts.

## Scope Boundaries

In scope are runtime detection of effective recovery config, failed original-attempt recording before recovery starts, nested recovery worker launch in the same workspace, recovery attempt budgeting, parsing and mapping `recheck` and `gave_up`, recheck scheduling of the original step unchanged, preservation of original failure behavior for no-op and negative recovery outcomes, and any narrow test helper or documentation correction needed to keep the already-landed recovery runbook and helper names truthful after runtime activation.

Out of scope are recovered terminal workflow outcome names such as `succeeded_after_recovery` and `failed_after_recovery`, which belong to LIV-489; operator history and UI rendering of the failed-attempt to recovery to recheck timeline, which belong to LIV-490/LIV-499; and deeper hardening such as daemon interruption during recovery, retention cleanup policy, and comprehensive crash-resume cases, which belong to LIV-491. Parser, protocol, state-record, projection, and bundled asset groundwork should not be redesigned in this ticket. Provider-live behavior, provider cache or cache-TTL behavior, browser checks, and required manual dogfood are not changed by this runtime slice; deterministic fake dependencies and ledger assertions are the pre-publish evidence.

## Milestones

Milestone 1 verifies the foundation contracts before runtime changes. The implementer should inspect `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_step_recovery.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/artifact_store.gleam`, and `test/workflow_run_test.gleam`. At the end, the current tree has confirmed functions and types for `workflow_dag.effective_recovery_config`, `workflow_step_recovery.decision`, `workflow_step_recovery.prompt`, `workflow_step_recovery.tool_spec`, `workflow_checkpoint.step_recovery_started`, `workflow_checkpoint.step_recovery_finished`, and `workflow_checkpoint.write_recovery_artifact`; if any are missing or incompatible, the implementation stops with evidence instead of inventing new protocol or record shapes.

Milestone 2 adds failing workflow-run tests for no-op, successful recheck, and original-definition preservation. At the end, `test/workflow_run_test.gleam` contains deterministic fake command and fake agent dependencies that describe a recoverable failed command step whose original attempt is recorded before recovery starts, whose nested recovery worker returns `recheck`, and whose unchanged original step reruns at attempt index 2. The same milestone adds an agent-step case that captures the original prompt, structured-output spec, workspace, normal model settings, recovery prompt, and optional recovery model so the recheck can be proven not to mutate the original step. It also adds no-op cases for absent recovery, `recover.enabled: false`, and `on_failure: continue`; these red tests should fail because the runtime has not yet launched recovery.

Milestone 3 implements recovery detection and original-attempt finalization. At the end, `src/scherzo/workflow_run.gleam` distinguishes recoverable fatal failures from ordinary failures by combining `workflow_dag.effective_recovery_config` with the existing failure policy, writes the failed step artifact exactly once before any recovery start record, and leaves absent, disabled, and `on_failure: continue` cases on the existing failure or continue path with no recovery records.

Milestone 4 implements nested recovery worker execution and budget accounting. At the end, the runner launches recovery in the same prepared workspace with `workflow_step_recovery.prompt`, `workflow_step_recovery.tool_spec`, the configured recovery model when present, and the default model settings otherwise. It emits `workflow_step_recovery_started` and `workflow_step_recovery_finished`, writes the retained recovery result artifact when a parseable decision is available, increments the recovery-worker attempt budget before launch, and refuses further recovery after `recover.attempts` is consumed.

Milestone 5 implements recheck scheduling and negative-path preservation. At the end, `recheck` records `retry_attempt_index: 2` for the first recheck, marks the original scheduler step pending without changing its command, prompt, structured-output contract, workspace, or normal model settings, and lets the existing prepare path assign the next normal attempt index. `gave_up`, recovery worker crash, timeout/failure, invalid or missing recovery-result output, checkpoint/artifact write failure, and exhausted attempts all surface the same original workflow failure reason, failed step id, and original artifact shape as the no-recovery path, apart from any recovery diagnostics already durably written.

Milestone 6 completes docs/helper checks, validation, and handoff. At the end, `docs/runbooks/workflow-step-recovery.md` and any test helper names referenced by the implementation have been checked against the activated runtime behavior and updated only if they would otherwise be stale. The full pre-publish evidence is `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root, with any unrelated pre-existing warnings explicitly called out before publish.

## Progress

- [x] (2026-05-23 00:00Z) Inspected the runtime runner, DAG recovery config, recovery protocol, checkpoint records, artifact store, projection state, recovery runbook, and existing workflow-run tests relevant to LIV-488.
- [x] (2026-05-23 00:00Z) Authored this concise review document and placed mechanical implementation detail in the structured implementation-pack submission for LIV-488.
- [x] (2026-05-23 00:00Z) Incorporated review feedback by making milestones independently verifiable, adding explicit acceptance evidence for tests, ledger artifacts, docs/helper checks, provider-live/cache non-scope, manual dogfood timing, and full format/lint validation.

## Decision Log

- Decision: Recovery only starts for a step failure that would otherwise be fatal.
  Rationale: `on_failure: continue` is already an intentional non-fatal behavior and must remain a no-op for recovery.
  Date: 2026-05-23

- Decision: The failed original attempt is finalized before the nested recovery worker starts.
  Rationale: Durable history must show the failure that recovery is responding to, and a recovery crash must not erase the original evidence.
  Date: 2026-05-23

- Decision: A recovery recheck decision requeues the original workflow step instead of creating a synthetic DAG step.
  Rationale: This preserves dependency semantics and guarantees the rechecked command, prompt, structured-output contract, and workspace definition are unchanged.
  Date: 2026-05-23

- Decision: `recover.attempts` counts nested recovery worker invocations, not normal step attempts.
  Rationale: The budget exists to bound repair work; normal rechecks are observable attempt indexes driven by the existing scheduler.
  Date: 2026-05-23

- Decision: Recovered terminal outcomes, operator timeline rendering, and deep interruption hardening are deferred.
  Rationale: LIV-488 is the runtime execution slice; later tickets own presentation, compatibility naming, and robust recovery from partial recovery execution.
  Date: 2026-05-23

- Decision: Provider-live behavior, provider caches, browser checks, and required manual dogfood are not pre-publish gates for this ticket.
  Rationale: LIV-488 can be proven deterministically with fake command and agent dependencies plus durable ledger/artifact assertions; live-provider and operator-history confidence belongs to optional post-implementation dogfood or later presentation tickets.
  Date: 2026-05-23

- Decision: Runtime activation should include only narrow docs or helper updates if existing recovery documentation becomes stale.
  Rationale: The parser, protocol, schema, default prompt, and runbook foundation already landed, but the implementation must not publish a runtime behavior whose helper names or documented artifact paths are false.
  Date: 2026-05-23

## Validation and Acceptance

Acceptance is verifiable through `test/workflow_run_test.gleam` and ledger artifacts. New tests must prove a recoverable fatal command step writes the original `step_attempt_finished` record before `workflow_step_recovery_started`, writes `workflow_step_recovery_finished` with `result: recheck` and `retry_attempt_index: 2`, and then reruns the original command step at attempt index 2 in the same logical workspace. A companion agent-step test must prove the original prompt mode, structured-output spec, workspace reference, and normal model settings are unchanged on recheck, while the nested recovery invocation uses `workflow_step_recovery.prompt`, the recovery structured-output tool spec, and the configured recovery model when present.

Negative acceptance must also be tested. Absent recovery, `recover.enabled: false`, and `on_failure: continue` must produce no recovery start/finish records and keep today’s workflow behavior. `gave_up`, recovery worker crash, timeout/failure, invalid or missing recovery-result output, checkpoint/artifact write failure, and exhausted recovery attempts must leave the workflow failure reason, failed step id, and final original artifact behavior equivalent to the no-recovery path, apart from any durable recovery diagnostics successfully recorded before the negative outcome. The budget test must show `recover.attempts: 1` starts no second recovery worker after a failed recheck.

The implementation evidence required before publish is: `direnv exec . gleam test` passes; `direnv exec . gleam format --check src test` passes; `direnv exec . gleam run -m glinter` passes under the repository lint policy; and `direnv exec . gleam run -m scherzo_lint` passes. The observable artifact evidence is a test ledger segment or in-memory checkpoint assertion showing the ordered original attempt, recovery start, recovery finish, and recheck attempt records for the happy path, plus assertions that the no-op paths write no recovery records. Documentation/helper evidence is a checked or updated `docs/runbooks/workflow-step-recovery.md` and any affected test helper names so the activated runtime behavior, recovery-result artifact path, and helper names are not stale.

No browser, live-provider, provider cache, cache-TTL, dogfood, or manual operator-history evidence is required before LIV-488 publish. If a human wants a manual smoke check after implementation, it should be deferred until after implementation handoff: run a small local workflow with a deliberately failing first attempt, let the recovery worker request recheck, and verify the same ledger record order. That post-implementation human/operator check is useful confidence, but it is not a pre-publish blocker for this plan.

## Rollout, Recovery, and Idempotence

Rollout is additive because workflows without effective recovery keep their existing behavior and existing ledgers require no migration. A rollback can disable the new behavior by removing or disabling `recover` config from workflows; already-written recovery records remain parseable strings and should not require cleanup. This rollout does not change provider-live setup, provider cache keys, cache TTLs, browser behavior, or operator history rendering.

The runtime work should be idempotent at the artifact boundary. Retained recovery artifacts use immutable refs under `runs/<run>/<step>/attempt-<n>/recovery-<m>/`; rewriting the same payload may be accepted as existing, while conflicting payloads should be treated as recovery failure and preserve the original step failure. Re-running tests should create fresh run roots or deterministic in-memory writers so recovery attempt numbers and recheck indexes remain stable. Docs and helper checks are idempotent: leave already-correct files unchanged, and update only stale names or paths introduced by the runtime activation.

If implementation stops halfway, the safe state is either no recovery execution wired into `workflow_run.gleam` or recovery disabled by config. Do not ship a state where the original failed attempt can be skipped, where recovery can loop without budget, where recovery records can be written without the original failure evidence, or where a `recheck` can mutate the original step definition.

## Open Questions and Clarifications Needed

No product decision is blocking. The implementer must verify the exact helper names in the implementation branch before coding and stop if the foundation contracts from LIV-482 are missing or have incompatible signatures. The recommended recovery-session id format and whether to expose a new internal prompt-mode constructor are implementation details as long as start/finish records link the failed attempt, recovery attempt number, configured model, prompt reference, and eventual recheck attempt index.
