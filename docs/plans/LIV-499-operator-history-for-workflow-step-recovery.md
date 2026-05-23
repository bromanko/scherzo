# Expose operator history for workflow step recovery

## Purpose / Big Picture

Operators need a quick way to understand what happened when a workflow step failed, ran recovery, retried, and eventually succeeded or failed after recovery. After this change, an operator can run the human `scripts/scherzoctl session <session-ref>` command for the original step session, a continuation session, or the nested recovery session and see a readable `workflow_step_recovery_history` block that ties together the failed attempt, recovery decision, retry attempt, and final recovered workflow outcome where that outcome is known.

## Problem Framing and Constraints

The recovery foundation records durable workflow step recovery state, but the local operator surface still leaves the recovery story scattered across ledger records, projection state, step sessions, and retained events. That makes failed-run triage slow and makes interrupted recovery-start-without-finish records easy to miss. The implementation should solve the display problem only. It should not change runtime recovery scheduling, retry mechanics, ledger record schemas, provider-live preflight, review-lane cache behavior, or JSON session contracts.

This plan depends on the predecessor runtime work from LIV-488 and LIV-489. The implementation checkout must already contain durable recovery start and finish records, retry step attempts, and stable recovered terminal outcomes named `succeeded_after_recovery` and `failed_after_recovery`. If those contracts are absent, implementation should stop and report the missing predecessor evidence instead of inventing compatibility behavior.

## Strategy Overview

Add a small read-only helper under `src/scherzo/ctl/workflow_recovery_history.gleam` that replays the local ledger, folds or receives the existing projection, and renders recovery history for a session summary. The helper should keep impure ledger loading separate from pure history construction so the timeline logic can be tested deterministically with in-memory records. The human `scherzoctl session` path should print the normal session detail first, then append the rendered recovery history. The `session --json` output should remain unchanged; if JSON output needs to change, implementation should stop and revise this plan with explicit JSON acceptance and tests.

The history should be built from durable identities, not display names or retained Pi transcripts. Candidate run ids should come from step attempt session ids, continuation session ids, and recovery session ids recorded in the ledger and projection. Each recovery-start record should produce one timeline entry, sorted by ledger order. A matching finish record fills in the recovery decision, summary or reason, retry attempt index and retry result when present, and final workflow outcome when the projection exposes `succeeded_after_recovery` or `failed_after_recovery`. A start record without a finish record should render as incomplete and inspectable rather than disappearing.

## Alternatives Considered

One alternative is to tell operators to inspect `scripts/scherzoctl events` for every related session. That works for deep debugging but is too manual for first-response triage, and it hides interrupted recovery records when no recovery session emitted useful events.

Another option is to add recovery history to `session --json` immediately. That would be useful for future automation, but it broadens the compatibility surface and requires a stable protocol shape. The smaller change is to add the human-readable block first and keep JSON unchanged.

A third option is to derive the timeline from retained agent transcripts or command diagnostics. Those artifacts are useful context, but they are optional and sensitive. The durable ledger and projection are the correct source of truth for the recovery timeline.

## Risks and Countermeasures

The main risk is showing an invented or misleading timeline. The countermeasure is to join only exact durable identifiers from `WorkflowStepRecoveryStarted`, `WorkflowStepRecoveryFinished`, step-attempt records, and projection keys. Partial data should render as incomplete or unknown, not as a guessed decision or retry result.

A second risk is breaking existing `scherzoctl session` behavior. The countermeasure is to preserve the existing base session output, leave `session --json` unchanged, and add CLI tests that prove a ledger replay failure still prints `display_name:` and an unavailable-history note.

A third risk is hiding duplicate or conflicting recovery records. The countermeasure is one rendered entry per recovery-start record, sorted by ledger order. Duplicate, overlapping, or conflicting records should remain visible as separate entries or bounded diagnostics rather than being silently collapsed.

A fourth risk is implementing against missing predecessor contracts. The first implementation milestone must search the current tree for recovery records and recovered outcome strings. If LIV-488 or LIV-489 contracts are still absent, the implementer should stop with evidence instead of filling gaps inside this display ticket.

## Scope Boundaries

In scope are a read-only recovery-history helper, deterministic helper tests, non-JSON `scherzoctl session` integration, CLI tests for normal and unavailable-history output, and runbook updates that point operators to the implemented command.

Out of scope are runtime recovery execution, retry scheduling, recovery-result parsing, checkpoint or ledger schema changes, provider-live or review-lane cache behavior, web UI changes, automatic repair of interrupted recovery, broad redaction hardening, and any intentional change to `scherzoctl session --json`.

## Milestones

Milestone 1 validates predecessor contracts before coding. The implementer should search `src` and `test` for `WorkflowStepRecoveryStarted`, `WorkflowStepRecoveryFinished`, `workflow_step_recovery_started`, `workflow_step_recovery_finished`, `succeeded_after_recovery`, and `failed_after_recovery`. The observable result is either evidence that the runtime contracts exist or a stopped implementation report that names the missing contract.

Milestone 2 adds pure helper tests. The new `test/ctl_workflow_recovery_history_test.gleam` should use in-memory ledger records and projection fixtures to cover retry-requested with retry attempt 2 and `succeeded_after_recovery`, gave-up with `failed_after_recovery`, interrupted start without finish, original step session lookup, continuation session lookup, nested recovery session lookup, duplicate or multiple recovery starts, and an unrelated session rendering `workflow_step_recovery_history: -`.

Milestone 3 implements the helper. The new `src/scherzo/ctl/workflow_recovery_history.gleam` should expose an impure `load` wrapper around existing ledger replay and pure construction and rendering functions. The observable result is that helper tests pass and long summary or reason text is normalized, newline-safe, and bounded.

Milestone 4 wires the helper into human session output. The non-JSON `Session` branch in `src/scherzo/ctl.gleam` should fetch the normal session summary, print the existing fields, then append the history block. The JSON branch should remain unchanged. CLI tests in `test/ctl_test.gleam` should prove the normal labels and the new `workflow_step_recovery_history` labels appear together, and that corrupt or unavailable ledger state still returns success with an unavailable-history note.

Milestone 5 migrates documentation and completes validation. `docs/runbooks/workflow-step-recovery.md` should replace any deferred-history guidance with a concrete `scripts/scherzoctl session <session-ref>` example, and `docs/runbooks/workflow-recovery.md` should cross-link that operator surface. The final evidence should include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Progress

- [x] (2026-05-22) Reviewed the failed LIV-499 workflow artifacts and recovered the accepted implementation-pack content.
- [x] (2026-05-22) Authored this concise review document without mechanical implementation sections so it can be paired with the structured implementation pack.
- [x] (2026-05-22) Incorporated the prior plan-review feedback by requiring error-path evidence, all session-resolution entry points, unrelated-session behavior, duplicate or multi-entry visibility, exhaustive runbook migration, and unchanged `session --json` scope.
- [x] (2026-05-23) Implemented `src/scherzo/ctl/workflow_recovery_history.gleam`, wired human `scherzoctl session` output to append recovery history, and added deterministic helper plus CLI acceptance tests.
- [x] (2026-05-23) Migrated `docs/runbooks/workflow-step-recovery.md` and `docs/runbooks/workflow-recovery.md` to point operators at the new history surface.
- [x] (2026-05-23) Ran `direnv exec . gleam test` and `direnv exec . gleam format --check src test`; both passed.
- [x] (2026-05-23) Ran `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint`; both completed with the existing warning inventory and no new errors.

## Decision Log

- Decision: Expose the first operator surface through the human `scherzoctl session` output.
  Rationale: Operators already use `session` for triage, and the implementation can append history without changing the JSON protocol.
  Date: 2026-05-22

- Decision: Use local ledger and projection records as the source of truth.
  Rationale: Retained transcripts and command diagnostics can be sensitive or absent, while durable recovery records are designed for audit and restart visibility.
  Date: 2026-05-22

- Decision: Render incomplete recovery-start records explicitly.
  Rationale: A recovery that started but never finished is an operator-relevant state and should not be hidden by a renderer that only shows completed decisions.
  Date: 2026-05-22

- Decision: Keep runtime mechanics and hardening out of this ticket.
  Rationale: LIV-488 and LIV-489 own runtime recovery semantics; this issue should stay an additive display change over those contracts.
  Date: 2026-05-22

## Validation and Acceptance

Validation must be deterministic. Helper tests must cover retry-requested, gave-up, interrupted start-without-finish, failed-after-recovery, succeeded-after-recovery, original step sessions, continuation sessions, nested recovery sessions, unrelated sessions, and duplicate or multiple recovery-start entries. CLI tests must prove `scripts/scherzoctl session <session-ref>` human output contains the existing session fields plus `workflow_step_recovery_history`, `decision: retry_requested`, `retry_attempt_index: 2`, `retry_result: succeeded`, and `final_workflow_outcome: succeeded_after_recovery` for a successful recovered timeline.

Error-path evidence is required. A corrupt ledger snapshot or current segment must still print the normal session summary, including `display_name:`, and append a bounded `workflow_step_recovery_history: unavailable (...)` diagnostic instead of failing the command. The JSON command `scripts/scherzoctl session <session-ref> --json` must remain byte-shape compatible for the existing session object unless this plan is revised before implementation.

Documentation evidence is required. The workflow step recovery runbook must show the implemented `scherzoctl session` history block and point to `scripts/scherzoctl events --pretty <recovery-session-id>` only for deeper transcript inspection. The general workflow recovery runbook must point operators to the same surface so stale deferred-history guidance does not remain.

Before publish, run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. All four commands should pass. If a lint command reports unrelated pre-existing inventory, the handoff must record exact evidence and why this change did not introduce it. Provider-live, required-live, review-lane cache, browser checks, and cache TTL behavior are not applicable to this local read-only history display. Optional manual dogfood can run after implementation but is not a pre-publish blocker.

## Rollout, Recovery, and Idempotence

The rollout is additive and read-only. The helper reads local ledger and projection state and renders text; it does not append ledger records, mutate sessions, alter workflow scheduling, or change retained artifacts. Re-running `scripts/scherzoctl session <session-ref>` should be idempotent and should produce the same history for the same durable state.

If ledger replay fails, the command should preserve the existing session output and append an unavailable-history note. If predecessor recovery records or recovered outcomes are missing from the implementation checkout, the safe recovery path is to stop before coding and report the missing prerequisite rather than silently weakening the display contract. If the change needs to be backed out, removing the helper call from the non-JSON session path should restore the previous behavior because no durable data format changes are introduced.

## Open Questions and Clarifications Needed

There are no stakeholder decisions required before implementation. The implementer still must verify that LIV-488 and LIV-489 runtime contracts are present in the checkout before coding. Future work may add a stable JSON recovery-history protocol or a richer UI view, but those surfaces are intentionally outside this display-only ticket.

## Outcomes & Retrospective

The implementation stayed within the intended display-only scope. Operators can now inspect workflow step recovery history from the human `scherzoctl session` output for failed-step sessions, retry continuations, and nested recovery sessions, while `session --json` remains unchanged. The unavailable-history path is bounded and non-fatal, duplicate recovery-start records remain visible, and the required docs now point operators at the implemented surface.

Validation passed for `direnv exec . gleam test` and `direnv exec . gleam format --check src test`. The required lint gates also completed successfully, but they still report the pre-existing repository warning inventory; this change did not add new lint errors or new warning categories.
