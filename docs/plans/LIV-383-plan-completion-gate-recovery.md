# Make execplan implementation recover from late plan-completion failures

This ExecPlan v2 review document frames the design for LIV-383. Mechanical implementation details, tests, file-by-file edits, and command instructions are supplied through the structured implementation pack.

## Purpose / Big Picture

After this change, a long `workflow:execplan-implementation` run that reaches the pre-review plan-completion gate with actionable unmet requirements should not discard the operator's ability to recover the completed work. Scherzo should feed the verifier's blocking findings into one bounded late repair pass, then re-check the work before native review begins.

If the late repair still leaves unmet requirements, or if the final pre-publish plan-completion gate fails after review and final validation, Scherzo should stop with a retained workspace, a concise recovery artifact, and a supported full-workflow retry path. The final pre-publish gate is deliberately diagnostic-only for recovery: it must not start another automatic code-edit pass after review and validation have already completed.

The observable outcome is that `plan_completion_failed` is no longer an opaque terminal gate failure. Operators and Linear readers see which plan requirements remain unmet, whether Scherzo attempted the bounded repair, whether the failure happened before review or at the final pre-publish gate, where the recovery artifacts live, and that the safe next action is to inspect the retained workspace and then use the normal `scherzoctl retry <issue>` workflow retry when ready.

## Problem Framing and Constraints

The incident behind this work was a late `gate_plan_completion` failure after substantial implementation effort. The verifier had already produced structured `blocking_findings`, but the workflow treated the command gate as an ordinary fatal failure, so the actionable feedback was not used automatically and the retained-workspace path was not obvious to operators.

The main constraint is safety. Only a fresh, well-formed plan-completion verdict with `verdict: "fail"` and stable context values is retryable before native review. Missing, malformed, or stale verdict artifacts are command or configuration failures and must remain terminal. The workflow must not loop forever, broaden the ExecPlan scope, or proceed into code review or publish when required plan behavior is still missing.

The final pre-publish gate has a stricter constraint than the pre-review gate. By that point native review feedback and final validation have already run, so an automatic code-edit pass would need to re-enter earlier review and validation stages to be safe. This MVP therefore gives the final gate retained-workspace recovery and clear retry guidance, but no extra automatic repair attempt.

## Strategy Overview

Keep the existing verifier contract and gate semantics, but add a narrow recovery layer around the two late plan-completion gates. The recovery layer classifies a failed gate by reading the same verdict and current diff fingerprint that the gate already validates. Fresh `plan_completion_failed` verdicts become actionable recovery input; malformed, missing, and stale verdicts fail closed with their existing stable codes.

The automatic repair budget is exactly two plan-completion repair attempts per `workflow:execplan-implementation` run. Attempt 1 is the existing early `apply_plan_completion_feedback` pass after the first verifier. Attempt 2 is the new late pre-review repair pass after `gate_plan_completion`, and it is available whether the early pass changed code or was a no-op. After attempt 2, the workflow re-analyzes, re-verifies, and gates again. If that second pre-review gate still fails, Scherzo writes a recovery summary, keeps the workspace retained, and fails with a distinct exhausted-recovery code that includes the findings and next action.

The final pre-publish gate reuses the same recovery artifact format in a final diagnostic mode. A passing final verdict publishes normally. A fresh failing final verdict writes the recovery summary, keeps the workspace retained, and fails without launching another agent repair step. A new full workflow retry gets a new run and a new budget; daemon recovery or replay of the same run cannot allocate extra automatic repairs because the budget is represented by explicit static DAG steps and fixed attempt numbers.

## Alternatives Considered

The simplest alternative is to only improve the failure comment for `plan_completion_failed`. That would make diagnosis easier, but it would still waste the verifier output and require a human to perform the pre-review repair that Scherzo can safely attempt once.

Another option is a general dynamic retry loop in the workflow runner. That is too broad for this incident because the current workflow language is a static DAG and plan-completion failures already have a domain-specific verdict artifact. A local, explicit recovery branch solves the immediate problem with less scheduler risk.

A third option is to give the final pre-publish gate its own automatic repair chance. That is rejected for this MVP because final-gate edits would happen after review feedback and final validation; making that safe would require a larger loop back through review and validation rather than a small recovery branch.

A fourth option is to treat every verifier or validation failure the same way. That is deferred because stale verdicts, schema failures, transport failures, and final validation failures have different safety properties from a fresh `plan_completion_failed` verdict with actionable findings.

## Risks and Countermeasures

The main risk is accidentally treating a stale or malformed verifier artifact as repairable. The countermeasure is to reuse the existing plan-completion context checks in the classifier and preserve the existing terminal failure codes for non-actionable cases.

A second risk is unbounded agent churn. The countermeasure is the explicit two-attempt budget: the existing early repair plus one new late pre-review repair. Final-gate recovery never edits code automatically, and exhausted pre-review recovery stops with retained artifacts instead of looping.

A third risk is losing operator context in Linear and session events. The countermeasure is a bounded human-readable recovery summary that lists the unmet requirements, artifact paths, retention status, failure phase, and next action, plus handoff wording for the exhausted-recovery failure code.

A fourth risk is implying that the operator should use a same-run failed-step repair or ad hoc follow-up prompt when the safe path is not yet implemented for this exact recovery point. The countermeasure is to make the supported MVP next action explicit: inspect or salvage from the retained workspace, then run the normal full workflow retry with `scherzoctl retry <issue>` when ready. Same-run resume from the exhausted plan-completion point remains out of scope.

## Scope Boundaries

In scope is `workflow:execplan-implementation`, the helper script that validates plan-completion verdicts, the prompt used for late pre-review plan-completion recovery, the workflow YAML branch around `gate_plan_completion`, final-gate retained recovery, recovery-summary artifacts, handoff wording, operator runbook wording, and tests for helper behavior, workflow structure, retry-budget enforcement, and failure-comment content.

Out of scope is a general workflow-runner retry primitive, automatic recovery for arbitrary command failures, changing the provider-facing structured output schemas, changing the ExecPlan authoring workflow, implementing same-run resume from exhausted plan-completion recovery, and adding an automatic final-gate code-edit loop after review and validation.

## Milestones

First, factor the plan-completion verdict classification so the helper can distinguish retryable fresh `plan_completion_failed` verdicts from terminal missing, malformed, and stale verdict failures without changing passing gate behavior.

Second, add the pre-review workflow recovery branch. A failed `gate_plan_completion` becomes a continued step, a classifier decides whether late repair is allowed under attempt 2 of 2, a new agent prompt applies only the blocking findings, and the workflow re-analyzes and re-verifies before a second pre-review gate.

Third, add the exhausted pre-review recovery path. If the second pre-review gate still reports a fresh fail verdict, Scherzo writes a recovery artifact, leaves the retention marker in place, prints the full-workflow retry path, and fails with a stable recovery-exhausted code.

Fourth, add final pre-publish gate recovery in diagnostic mode. If the final gate reports a fresh fail verdict, Scherzo writes the same kind of recovery artifact and retains the workspace, but it does not allocate another repair attempt or proceed to publish.

Fifth, improve operator-facing diagnostics so Linear comments, session failure summaries, and runbook guidance clearly show the unmet requirements, phase, retention status, and `scherzoctl retry <issue>` next action rather than only the raw command failure.

Sixth, cover the behavior with focused helper, workflow, handoff, and runbook tests before dogfooding it against retained failed-run fixtures.

## Progress

- [x] (2026-05-18 00:00Z) Drafted the human-reviewable ExecPlan v2 review document for LIV-383.
- [x] (2026-05-18 00:00Z) Incorporated plan review feedback by deciding the final pre-publish gate is diagnostic-only, defining the full-workflow retry path, and spelling out the two-attempt repair budget.
- [ ] Implementation pack materialized into a follow-up implementation task.
- [ ] Recovery branch implemented and validated.

## Decision Log

- Decision: The default automatic repair cap is two plan-completion repair attempts per `workflow:execplan-implementation` run.
  Rationale: The workflow already performs one early repair after the first verifier, and one additional late pre-review repair addresses this incident class without creating an open-ended loop.
  Date: 2026-05-18

- Decision: Attempt 1 is the existing early `apply_plan_completion_feedback` step; attempt 2 is the new late pre-review repair after `gate_plan_completion`.
  Rationale: Naming the counted stages removes ambiguity about whether the late repair is available after the early pass changed code; it is available once per run after the pre-review gate when the verdict is fresh and actionable.
  Date: 2026-05-18

- Decision: The final pre-publish plan-completion gate receives retained-workspace recovery only, not another automatic repair pass.
  Rationale: Code edits after review and final validation would need another review and validation cycle, which is larger than this targeted recovery branch.
  Date: 2026-05-18

- Decision: The supported MVP next action after exhausted recovery is to inspect or salvage from the retained workspace and then use the normal full workflow retry, `scherzoctl retry <issue>`, when safe.
  Rationale: Same-run resume from this semantic failure point is not yet specified; full retry preserves current operator semantics while retained artifacts keep the completed work available for inspection and manual salvage.
  Date: 2026-05-18

- Decision: Only fresh `plan_completion_failed` verdicts enter the resilience path.
  Rationale: Missing, malformed, and stale verdicts indicate command, verifier, or configuration failures, not actionable unmet plan requirements.
  Date: 2026-05-18

- Decision: Earlier verifier failures outside the plan-completion verdict contract remain out of scope for the MVP.
  Rationale: The incident and current artifact contract are specific to the late gate receiving structured blocking findings.
  Date: 2026-05-18

## Validation and Acceptance

Acceptance is behavioral. A fresh fail verdict at the pre-review `gate_plan_completion` with blocking findings should cause exactly one late repair attempt, a new verifier pass, and a second pre-review gate before native code review can start. If the second pre-review gate passes, the workflow continues normally. If it fails again, the workspace is retained and the failure report names the unmet requirements, recovery artifact, failure phase, and `scherzoctl retry <issue>` retry path.

A fresh fail verdict at the final pre-publish plan-completion gate should not start another repair prompt. It should retain the workspace, write the recovery artifact, fail before publish, and report the same supported next action.

Negative validation must prove that missing, malformed, and stale verdicts are not repairable and keep their terminal command/config failure behavior. Budget validation must prove that daemon recovery or workflow replay of the same run cannot grant a third automatic repair attempt. Handoff validation must show that Linear-facing comments and session summaries contain concise unmet-plan, phase, retention, artifact-path, and next-action text for exhausted plan-completion recovery.

## Rollout, Recovery, and Idempotence

The rollout is additive to the execplan implementation workflow. Passing verdicts continue through the same downstream review and publish path, and non-actionable gate failures still fail closed. The new late repair branch runs at most once after the pre-review gate in a given run. Replaying or recovering the same run reuses the same explicit static DAG steps and attempt numbers, so it cannot silently allocate additional repair attempts.

If the new recovery code causes trouble, it can be rolled back by restoring the pre-review and final gates to fail-fast behavior and removing the late recovery steps. Runs stopped by exhausted or final-gate recovery are safe to inspect because the existing retention marker keeps the workspace and the new recovery artifact records the verdict, findings, phase, and suggested full-workflow retry action.

A normal full retry is intentionally a new run with a new automatic repair budget. The retained failed run remains available for inspection and manual salvage, but this MVP does not promise same-run resumption from the exhausted plan-completion point.

## Open Questions and Clarifications Needed

No unresolved MVP questions remain. A future iteration may add a purpose-built same-run resume command for exhausted plan-completion recovery, but that is explicitly outside this plan.
