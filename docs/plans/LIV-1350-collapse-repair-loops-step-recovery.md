# LIV-1350 collapse implementation repair loops onto step recovery

This ExecPlan review document is paired with a structured implementation pack. The review document stays concise and human-reviewable; the implementation pack contains the mechanical file-by-file steps, interfaces, tests, and command details.

## Purpose / Big Picture

After this change, Scherzo's dogfood implementation workflows will stop spending happy-path agent sessions on repair workers that are only supposed to no-op. Plan-completion verification and base-refresh validation will fail at the step that actually needs repair, Scherzo's engine-native step recovery will run the bounded repair worker only on failure, and the original verifier or validator will be rerun unchanged as the independent recheck.

The visible operator outcome is shorter implementation runs with fewer no-op agents, fewer near-duplicate prompts, and the same fail-closed safety when the repair budget is exhausted. Operators will inspect recovery attempts through `workflow_step_recovery_history` rather than through hand-written classifier artifacts.

## Problem Framing and Constraints

`workflows/dogfood/execplan-implementation.yaml` still contains a manually unrolled plan-completion loop: verify, checkpoint, repair, analyze, restore, reverify, gate, classify, repeat. Every DAG step runs unconditionally, so a passing implementation still pays for verifier and repair sessions whose prompts ask the agent to make no edits. `workflows/dogfood/implementation.yaml` has the same always-on pattern for base-drift repair after refresh and validation.

The key constraint is separation of duties. A repair agent must never grade its own work. Step recovery preserves that separation because the recovery worker can only return `recheck`; Scherzo then reruns the original failed verifier or command step in a fresh normal attempt. The current workflow schema calls this configuration `recovery`, even though some issue text uses the older shorthand `recover`.

## Strategy Overview

Fuse each verifier and gate into one recoverable step. The ExecPlan plan-completion verifier remains an agent step that submits only semantic verdict fields with `submit_plan_completion_verdict`; a same-step command validator runs `scherzo-implementation gate-plan-completion --from-submission`, stamps the current workspace fingerprint into the canonical run-root verdict artifact, and exits nonzero when the semantic verdict is `fail`. That failure starts a bounded plan-completion recovery worker, and a `recheck` decision reruns the same verifier prompt and validator against the repaired tree.

Fuse base refresh and validation into one fail-fast command step with a base-drift recovery prompt. On the happy path, refresh and validation pass and no drift agent runs. On conflicts or clean-rebase validation fallout, the recovery worker makes only mechanical base-drift repairs and asks for a recheck. On nonrepairable refresh states or validation failures not caused by base drift, it returns `gave_up`, producing `failed_after_recovery` with retained workspace evidence.

## Alternatives Considered

Keeping the hand-unrolled loops is operationally safe but preserves the latency, token cost, and duplicate prompt maintenance burden. Adding a multi-step rewind primitive such as `recheck_from` is more general than this problem needs and would add engine complexity when fusing verifier and gate makes the failed step self-contained. A command-only plan-completion verifier would avoid an agent session but cannot judge ExecPlan acceptance with the required semantic understanding, so the agent verifier remains and only the gate becomes deterministic. Keeping the base-drift repair agent unconditional is also rejected because the engine already knows how to run a bounded recovery worker only after failure.

## Risks and Countermeasures

The largest risk is publishing after an incomplete implementation because the gate no longer runs as a separate DAG step. The countermeasure is that the command validator is part of the required structured-output validation path, uses `validation_retries: 0`, stamps fingerprints before returning, and fails the step on a `fail` verdict so recovery or terminal failure is unavoidable.

A second risk is losing useful repair context when fresh verifier sessions no longer receive long prior-verdict transcripts. The countermeasure is to keep canonical run-root artifacts for the plan, pack, bundle, validation result, and stamped verdict, and to write recovery prompts that read those artifacts directly. A third risk is accidentally enabling recovery on side-effecting publication steps. The countermeasure is to keep publication and code-change-bundle materialization recovery disabled and to validate that no publish path depends on a recoverable verifier gate.

A fourth risk is stale target counts because the review-lane collapse from LIV-1351 has already landed in this tree. The countermeasure is to record measured before/after step and agent-session counts during implementation rather than relying on the older issue baseline.

## Scope Boundaries

In scope are `workflows/dogfood/execplan-implementation.yaml`, `workflows/dogfood/implementation.yaml`, the prompt files they reference, the plan-completion verdict schemas and gate helper behavior needed by those workflows, focused workflow/helper tests, and runbook updates that remove references to the deleted hand-rolled recovery artifacts.

Out of scope are review-lane prompt semantics, `prepare_review` and `finalize_lanes`, review finding disposition semantics, publication behavior, `workflows/dogfood/execplan.yaml`, `workflows/dogfood/execplan-revision.yaml`, and any new engine primitive beyond existing step recovery. Live dogfood proof on real tasks is rollout evidence for operators after the implementation PR is available; it is not a reason to broaden the implementation into unrelated workflow changes.

## Milestones

Milestone 1 locks the current topology and recovery assumptions in tests. The tests should show the present unrolled steps are being replaced by two recoverable plan-completion verifier steps in `execplan-implementation` and one recoverable refresh-and-validate step in each implementation workflow.

Milestone 2 collapses the ExecPlan plan-completion loop. At the end, there is one shared plan-completion verifier prompt, one plan-completion recovery prompt, no checkpoint/classifier/finalizer scaffold, and the verifier's structured-output command validator is the only plan-completion gate.

Milestone 3 collapses base-drift repair in both implementation workflows. At the end, happy-path runs execute no base-drift repair agent, while conflicts and repairable validation fallout go through step recovery and rerun the same refresh-and-validate command.

Milestone 4 removes stale helper, prompt, test, and runbook surfaces. At the end, the deleted loop commands and per-stage prompts are not referenced by checked-in workflows or docs, and retained run diagnostics point operators to step-recovery history.

Milestone 5 validates and records acceptance evidence. At the end, workflow config, structured-output contracts, workflow portability, tests, format, production lints, and the review-doc validator pass or record unrelated pre-existing failures, and before/after step and agent-session counts are captured.

## Progress

- [x] (2026-07-05) Read the repo-local ExecPlan guidance and prepared this split review document with mechanical details reserved for the structured implementation pack.
- [x] (2026-07-05) Inspected the current dogfood workflow YAML, step-recovery runbook, plan-completion schemas, helper script, prompts, and workflow tests.
- [ ] Implement the collapsed workflow topology and supporting prompts/helpers.
- [ ] Run validation, record before/after counts, and update this living document during implementation.

## Surprises & Discoveries

The current tree already includes the review-lane pipeline collapse expected from LIV-1351. The measured current counts are 51 steps and 15 agent steps for `execplan-implementation`, and 20 steps and 7 agent steps for `implementation`, so the older 66-step and 38-step issue baseline is stale even though the repair-loop problem remains.

The current helper script already has `gate-plan-completion --from-submission`, and the provider/canonical plan-completion verdict schemas already describe semantic agent submissions without machine-stamped fields. The implementation should harden and use that existing surface rather than creating a duplicate tool contract.

## Decision Log

- Decision: Use the existing workflow schema field `recovery`, not the issue shorthand `recover`.
  Rationale: `schemas/scherzo.workflow.v1.schema.json` rejects `recover` and documents `recovery` as the active step configuration.
  Date: 2026-07-05.

- Decision: Keep two plan-completion verifier steps in `execplan-implementation`: one before review and one before final validation/publish.
  Rationale: Review feedback and base refresh can change the tree after the first verifier passes, so the final pre-publish verifier still has distinct value.
  Date: 2026-07-05.

- Decision: Treat real dogfood runs as rollout evidence after the implementation PR is available, while requiring automated workflow, contract, lint, and unit validation before publish.
  Rationale: The changed workflow cannot reliably prove itself through the currently running implementation workflow, but operators still need live confidence before broad dispatch.
  Date: 2026-07-05.

## Outcomes & Retrospective

Not yet implemented. During implementation, update this section with the final topology, measured step and agent-session counts, validation evidence, and any accepted behavior differences from the old hand-unrolled loops.

## Validation and Acceptance

Acceptance requires the checked workflows to load through the workflow-config doctor check, pass the structured-output contract checker for both implementation workflows, and pass the workflow-portability check. Focused tests must prove that plan-completion `fail` verdicts start step recovery and recheck the verifier, `gave_up` or exhausted budgets finish as `failed_after_recovery`, base-drift recovery runs only after refresh/validation failure, and publish/materialization steps remain recovery-disabled.

The implementation must also run the repository's normal Gleam test suite, format check, `glinter`, and `scherzo_lint`. Before/after counts must show that happy-path no-op plan-completion repair agents and the always-on base-drift agent were removed. After the PR is available, an operator should dogfood both workflows on real tasks: one happy path, one induced plan-completion recovery that passes after recheck, and one induced unrepairable failure that retains the workspace as `failed_after_recovery`.

## Rollout, Recovery, and Idempotence

Rollout is a workflow-bundle change with deterministic helper and test updates. If the change misbehaves, operators can pause dispatch, revert the workflow YAML/prompts/helper changes, and retry affected issues from retained workspaces. Publication steps remain non-recoverable, so reverting the workflow does not need to undo remote PR side effects produced by a recovery worker.

The new repair paths are bounded and idempotent at the workflow-attempt level. Repeating a failed verifier recheck consumes the configured recovery budget and then fails closed; it does not spawn an unbounded agent loop. Repeating the refresh-and-validate recheck reruns the same command against the current tree and either validates the repaired base-drift state or preserves the original failure with recovery history.

## Open Questions and Clarifications Needed

No blocking open questions. The implementer should record the exact target step and agent counts after editing because the issue's count estimates predate the already-landed review-lane collapse.
