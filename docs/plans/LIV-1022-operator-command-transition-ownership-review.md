# LIV-1022 operator command transition ownership review

This review defines the approach for a follow-up implementation task. It is intentionally concise; mechanical edits, test cases, interfaces, dependencies, and command sequences are carried in the structured implementation pack for this issue.

## Purpose / Big Picture

Scherzo's daemon should never lose transition-state updates because an operator command happened to run in the same transition batch. After the follow-up implementation, `scherzo ctl` should still receive the same `CommandResult` statuses for pause, resume, reload, stop, abort, prompt, UI response, retry, park, unpark, cleanup, and scheduled-run commands, while the daemon has one owner for transition-state mutation and no nested transition runs from operator shell handlers. The handoff must also be falsifiable: the follow-up implementer should know which tests, grep audits, lint gates, and deferred operator/manual checks prove the transition-ownership invariant without changing unrelated workflow helpers, provider-live paths, or cache behavior.

## Problem Framing and Constraints

The current daemon has a fail-open escape hatch: `shell_state_overrides_transition` in `src/scherzo/orchestrator/daemon.gleam` discards a transition run's entire state output after an `ApplyOperatorCommand` shell effect. That is only safe while the operator-command transition produces no other state change. If an earlier effect in the same run appends a follow-up message, or a future operator command returns follow-up messages, those transition changes can be silently dropped. Operator stop handling also calls `run_transition_messages` from inside an outer operator shell effect, which creates re-entrant transition execution. The redesign must preserve synchronous command replies for `scherzo ctl`, must keep production lint rules intact, and must move runtime-state writes into the transition path or explicitly outside the transition-owned state. Review feedback also requires explicit acceptance evidence for targeted tests, full validation, linting, manual/dogfood timing, and no-op boundaries for docs/helper migration, provider-live behavior, and cache behavior.

## Strategy Overview

First fence the current landmine with a tripwire that compares the input transition state with the discarded output when the override flag is still present; a non-empty delta must log and crash the daemon process instead of being silent. Then replace the override with normal effect completion flow: `ApplyOperatorCommand` produces an operator-command completion message, transition handling converts that completion into `FinishOperatorCommand`, and the daemon shell sends the pending `CommandResult` reply through a correlation id rather than reading `last_operator_command_result` from state. Shell-only operator work may still perform shell side effects, but any change to transition-owned fields must be returned as follow-up transition messages and processed before the command reply is finished.

Runtime ownership is cleaned up in the same design. Token aggregation, reload limit updates, and any direct `State(..state, runtime: ...)` sites discovered by audit should become transition messages unless the implementer deliberately removes those fields from `transition_types.State` and documents them as shell-owned. The preferred outcome is that `transition_types.State.runtime` remains transition-owned and all runtime mutations pass through `src/scherzo/orchestrator/transition.gleam`.

The structured implementation pack must mirror these review decisions mechanically. It should list every production and test file likely to change, call out that docs/helper migration, provider-live probes, and cache invalidation are not expected, and require final evidence that either those areas were untouched or any tiny compatibility touch was separately validated.

## Alternatives Considered

Keeping the override and adding comments was rejected because it still permits silent state loss. Keeping synchronous replies by storing a single `last_operator_command_result` was rejected because it is fragile if command effects ever overlap or emit more than one completion-like effect. Moving all operator commands directly into the transition layer was rejected because reloads, live worker command sends, ledger repair, remote-client reconciliation, and cleanup operations still need daemon shell dependencies. Fully solving asynchronous worker-command waits in this task was considered but is not required unless the owner chooses to merge that overlapping ticket; this plan creates the correlation and completion-message boundary that can support that later work.

## Risks and Countermeasures

The main risk is accidentally preserving a second transition-state mutation path. The countermeasure is a grep-audited removal of `shell_state_overrides_transition`, `last_operator_command_result`, re-entrant `run_transition_messages` calls from operator shell handlers, and direct runtime writes outside transition handlers. A second risk is breaking `scherzo ctl` command replies. The countermeasure is correlation-based pending replies, a fallback `operator_command_result_missing` rejection if a local command finishes without a completion, and daemon/control tests for applied, rejected, not-found, and timeout paths. A third risk is changing operator stop semantics while removing re-entrancy. The countermeasure is a follow-up-message ordering test proving `WorkerStopRequested` or equivalent stop transition messages are processed before the command reply is sent. A fourth risk is expanding into the async worker-command-waits ticket without coordination. The countermeasure is to keep worker waits synchronous for this implementation unless that ticket is explicitly merged, and to document any merged scope in the follow-up task.

A fifth risk is review/pack drift: the Markdown review could say a validation or rollout obligation that the structured pack omits. The countermeasure is to keep acceptance evidence, test obligations, milestone outcomes, lint/full-validation commands, deferred manual smoke timing, docs/helper migration boundaries, and provider-live/cache no-op expectations in both places. A sixth risk is accidental helper or provider scope creep. The countermeasure is to treat `.scherzo/workflows/scripts/*`, workflow schemas, provider structured-output helpers, provider-live paths, and cache behavior as no-change surfaces unless a tiny compatibility update is unavoidable and validated explicitly.

## Scope Boundaries

For this planning issue, scope is exactly this Markdown review document under `docs/plans/` and one structured implementation-pack submission. No production daemon code, tests, canonical bundle JSON, generated bundle references, or implementation task values are written by this issue.

For the follow-up implementation, in scope are `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/daemon_transition_shell.gleam`, `src/scherzo/orchestrator/effects/interpreter.gleam`, `src/scherzo/orchestrator/effects/types.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, `src/scherzo/orchestrator/transition.gleam`, `src/scherzo/orchestrator/transitions/operator.gleam`, `src/scherzo/orchestrator/operator_runtime.gleam`, and focused tests under `test/`. Out of scope are public control protocol changes, workflow YAML changes, ledger schema changes, UI/browser work, provider-live behavior, cache behavior, docs/helper migration, and broad daemon extraction. No `.scherzo/workflows/scripts/*`, workflow schema, provider structured-output helper, review-lane contract, required-live provider probe, cache/TTL path, or root-helper migration should change for this ticket. If one of those surfaces proves unavoidable, split it out unless the change is a tiny compatibility update with explicit validation evidence and the final evidence calls it out.

## Milestones

Milestone 1 fences the existing override before redesign. The observable outcome is a guard in the transition-shell merge path that logs and kills the daemon if an override would discard a changed transition state, with a focused transition-shell or daemon-boundary test proving the guard compares the input transition state with the discarded output transition state. No operator behavior changes are made in this slice beyond failing closed on the impossible-to-merge state delta.

Milestone 2 introduces operator command completion messages. The observable outcome is that `ApplyOperatorCommand` no longer calls `finish_operator_command` inline; instead the operator shell handler returns a `CommandResult` plus ordered follow-up transition messages, and the transition runner later handles an explicit operator-command completion message/effect that leads to `FinishOperatorCommand`. Tests must prove follow-up transition messages from an operator shell effect run before the command reply is finished.

Milestone 3 preserves synchronous command replies without `last_operator_command_result`. The observable outcome is a daemon-owned pending-reply map keyed by an operator command correlation id, command replies sent by the `FinishOperatorCommand` shell effect, and tests showing `scherzo ctl`-style local calls receive the expected `CommandResult` statuses for applied, rejected, not-found, queued/timeout-equivalent, and explicit missing-result paths. This milestone also proves duplicate, stale, or missing completions do not reply to the wrong caller.

Milestone 4 removes re-entrant operator stop handling. The observable outcome is that abort or stop fallback creates transition follow-up messages instead of calling `run_transition_messages` from inside the operator shell handler. Tests cover worker-found, worker-missing, worker-command-timeout, duplicate or stale stop request, and stop-request state-update ordering.

Milestone 5 moves runtime mutations into transition messages. The observable outcome is that token aggregation, reload limit updates, invalid-workflow report result updates if still present, and every audited direct `State(..state, runtime: ...)` write are handled by `src/scherzo/orchestrator/transition.gleam` or are documented as shell-owned by removing them from `transition_types.State`. Tests cover invalid workflow reload/reporting and any new runtime-message handler.

Milestone 6 deletes the override path and validates the invariant. The observable outcome is zero grep hits for `shell_state_overrides_transition` and `last_operator_command_result`, no operator shell handler re-enters `run_transition_messages`, direct runtime writes are either gone or explicitly shell-owned, docs/helper/provider-live/cache surfaces are unchanged or explicitly justified, and targeted plus full validation commands pass: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Progress

- [x] (2026-06-11) Read the prepared target file and confirmed the review document belongs directly under `docs/plans/`.
- [x] (2026-06-11) Reviewed the current operator command, transition shell, effect interpreter, runtime-write, and operator stop paths named by LIV-1022.
- [x] (2026-06-11) Wrote this planning review document only; no production implementation was changed.
- [x] (2026-06-11) Incorporated review feedback by tightening milestone specificity, acceptance evidence, lint/full-validation obligations, deferred manual smoke timing, docs/helper migration boundaries, and provider-live/cache no-op expectations.
- [x] (2026-06-13 00:00Z) Implemented Milestone 1's fail-closed tripwire by snapshotting the pre-shell transition state, logging discarded mergeable sections, and marking the daemon for fatal shutdown when an override would hide transition-owned changes.
- [ ] (2026-06-13 00:00Z) Implement Milestone 2 by replacing inline `ApplyOperatorCommand` completion with explicit operator-command completion messages and transition-owned finish handling.

## Surprises & Discoveries

- Observation: the override path can only detect silent loss correctly if it compares against the pre-shell transition snapshot rather than the shell-mutated daemon state.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` now stores `override_input_transition_state` when `ApplyOperatorCommand` enters the shell-owned override path.

## Decision Log

- Decision: Use a tripwire before removing the override. Rationale: it immediately converts the known silent-loss hazard into an observable process-fatal failure while the larger redesign is implemented. Date: 2026-06-11.
- Decision: Use correlation-based pending replies instead of `last_operator_command_result`. Rationale: it preserves synchronous `scherzo ctl` semantics without coupling command replies to a single mutable daemon field. Date: 2026-06-11.
- Decision: Treat operator shell follow-ups as transition messages processed before command completion. Rationale: stop, reload, and future shell commands can mutate transition-owned state through the same mechanism as all other effects. Date: 2026-06-11.
- Decision: Keep full async worker-command waits as coordinated overlap, not mandatory scope. Rationale: this issue must remove override and re-entrancy; the correlation boundary can support async waits if that ticket is merged, but it should not expand the implementation without owner approval. Date: 2026-06-11.
- Decision: Detect discarded transition state by snapshotting mergeable transition-owned fields before the shell command runs. Rationale: comparing against the pre-shell snapshot catches silent loss without treating shell-owned mutations as false positives. Date: 2026-06-13.

## Outcomes & Retrospective

Milestone 1 is now implemented in code. The daemon no longer silently tolerates an override that would discard changes to mergeable transition-owned fields; it logs `operator_command_transition_state_discarded` and proceeds to the existing fatal cleanup path. The remaining work is still the larger ownership redesign: explicit operator completion messages, correlated replies, re-entrancy removal, runtime-mutation migration, and final override deletion.

## Validation and Acceptance

This planning issue is accepted when `test -f docs/plans/LIV-1022-operator-command-transition-ownership-review.md` succeeds, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1022-operator-command-transition-ownership-review.md` reports `REVIEW_DOC_VALID=ok`, every required level-2 section is non-empty, and Scherzo captures the structured implementation-pack submission for LIV-1022.

The follow-up implementation is accepted only with pre-publish evidence for each required outcome: targeted tests prove the tripwire observes changed discarded transition state; operator shell follow-ups run before command completion; local `scherzo ctl`-style replies preserve `CommandResult` statuses; abort and stop no longer re-enter transition execution; token aggregation, reload limit updates, and invalid-workflow report results mutate runtime through transition messages or are explicitly shell-owned outside `transition_types.State`; `grep -R "shell_state_overrides_transition\|last_operator_command_result" src test` returns no matches; a direct-runtime-write audit such as `grep -R "State(..state, runtime:" src/scherzo/orchestrator --include='*.gleam'` either returns no transition-owned writes outside transition handlers or documents shell-owned exclusions; and `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` pass. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

Negative-path evidence is required for missing operator completion, stale or duplicate completion correlation ids, worker command timeout, worker/session not found, duplicate or stale stop requests, and invalid workflow reload/reporting. No browser evidence, provider-live validation, cache validation, or pre-publish dogfood run is required. A human/operator may optionally run a post-implementation daemon smoke with `scherzo ctl pause-dispatch`, `resume-dispatch`, and a stop or prompt command against a disposable session; that smoke is deferred post-implementation evidence, not a publish blocker. Final evidence must explicitly state whether docs/helpers, workflow schemas, provider structured-output helpers, provider-live paths, required-live probes, or cache behavior changed; the expected answer is no. If any such surface changed, final evidence must include the exact file list, why it was unavoidable, and the validation command or manual check proving the tiny compatibility update.

## Rollout, Recovery, and Idempotence

Rollout for this planning issue is this document plus the structured pack; Scherzo will materialize the canonical bundle and follow-up implementation task. Recovery is to edit or revert this document and resubmit a corrected structured pack.

Rollout for the follow-up implementation should use small behavior-preserving commits: tripwire, completion-message plumbing, correlated replies, stop follow-ups, runtime mutation messages, then override deletion. Each commit should keep targeted tests green, and the final implementation note should report the full validation/lint command results plus the no-change status for docs/helpers, provider-live paths, and cache behavior. Recovery is to revert the latest slice; the tripwire slice should remain in place until the override field is deleted. The implementation is idempotent because it does not require durable data migration, ledger schema changes, provider cache invalidation, provider-live reconfiguration, helper migration, or operator retraining.

## Open Questions and Clarifications Needed

No open question blocks implementation handoff. The implementer should coordinate with the async worker-command-waits owner before converting worker command waits from synchronous waits to asynchronous completions; without that coordination, keep worker waits synchronous and limit the change to the correlation/completion boundary described here.
