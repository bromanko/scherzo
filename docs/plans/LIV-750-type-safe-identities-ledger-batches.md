# LIV-750 type-safe identities and typed ledger batches review

This review document plans the follow-up implementation for Scherzo Core. It intentionally does not include mechanical implementation steps; those are supplied through the structured implementation pack for this ExecPlan workflow.

## Purpose / Big Picture

Scherzo operators should not be able to get a claimed workflow whose retained ledger is missing the parent workflow-run records because one path used a Linear issue id where another path expected an encoded task identity. After this hardening pass, the highest-risk orchestrator paths use distinct identity types for task identity, issue id, run id, and session id, and ledger appends for claim starts and workflow-step progress are assembled through named batch constructors instead of arbitrary `List(record.RecordBody)` values. Success is observable through compiler-enforced type boundaries, targeted regression tests for issue-id/task-identity mixups, unchanged retained-ledger JSON decoding, and the normal format, test, and production lint gates.

## Problem Framing and Constraints

LIV-749 exposed a class of bug in which a workflow claim handoff could cross from one subsystem to another using incompatible string meanings. A Linear issue id such as `issue-1` and an encoded task identity such as `6:linear|7:issue-1` are both `String`, so the compiler allowed a pending-claim or worker lookup to use the wrong value. When that lookup failed, the path that should have produced parent ledger records such as `workflow_run_started`, `known_workspace`, and `run_started` could be skipped or malformed while nearby control flow still represented the handoff as successful.

The first pass must reduce that risk without turning into a broad ledger redesign. Existing retained ledger JSON, record kinds, record field names, operator command inputs, tracker ids, and session/event JSON must remain compatible. The plan must not introduce append-time aggregate invariant validation or a full typed causal-effect system; both are explicitly deferred to a second plan.

## Strategy Overview

The implementation should add a small orchestrator identity module and migrate the dangerous state dictionaries before widening the scope. The first-pass typed values are `TaskIdentity`, `IssueId`, `RunId`, and `SessionId`. `TaskIdentity` becomes the key type for task-keyed dictionaries in `src/scherzo/orchestrator/state.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, and `src/scherzo/orchestrator/worker_registry.gleam`; `IssueId`, `RunId`, and `SessionId` replace raw strings at transition and effect boundaries where handoff, worker lifecycle, retry, and session lookup paths meet. Issue identifiers, workflow ids, step ids, command route ids, scheduled job ids, and artifact refs remain raw strings in this pass because they are either display labels, configuration ids, or lower-risk compatibility values.

Ledger batch construction should be typed at the ledger/effects boundary. Add an opaque batch type, for example in `src/scherzo/state/ledger_batch.gleam`, and have `src/scherzo/orchestrator/effects/types.gleam` carry a `LedgerBatch` instead of a public `List(record.RecordBody)` for orchestrator append effects. Domain constructors should cover the claim-started batch currently assembled in `src/scherzo/orchestrator/daemon.gleam` and the workflow-step records currently written in `src/scherzo/workflow_checkpoint.gleam`. The constructors still lower to the existing `record.RecordBody` variants immediately before writing, so ledger JSON compatibility stays intact.

## Alternatives Considered

The smallest alternative is to keep strings but standardize helper names such as `linear_issue_id_identity`. That is insufficient because a future bare-string lookup would still type-check. A larger alternative is to make every identifier in the repository opaque immediately, including workflow ids, step ids, scheduled job ids, artifact refs, and tracker labels. That would create broad fixture churn and distract from the failure class exposed by LIV-749. A third alternative is to add projection validation to every append. That may be valuable later, but it is too large for this hardening pass and would mix type-safety refactoring with aggregate invariant policy.

## Risks and Countermeasures

The main risk is creating a large mechanical churn that hides a behavior change. Counter this by writing source-level and behavioral tests first, migrating one identity family at a time, and keeping all JSON encoding and decoding functions unchanged. A second risk is preserving an escape hatch that still accepts arbitrary ledger bodies in the claim-start path. Counter this by changing `HandoffClaimResult`, `ClaimLedgerAppendRequested`, and `effects_types.LedgerAppend` to carry typed batches for the handoff path, while any unavoidable legacy raw-body helper is private, explicitly named as a compatibility escape hatch, and excluded from claim-start construction. A third risk is over-typing public or stored compatibility surfaces. Counter this by converting to and from strings only at tracker, CLI/operator, ledger-record, log-field, and JSON boundaries. A fourth risk is believing the refactor is safe because it compiles. Counter this with negative mixup tests, source guardrails against string-keyed pending claims and worker directories, retained-ledger decode tests, and full lint gates before publish.

## Scope Boundaries

In scope: inspect and modify `src/scherzo/orchestrator/state.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, `src/scherzo/orchestrator/transitions/claims.gleam`, `src/scherzo/orchestrator/transition.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/daemon_transition_shell.gleam`, `src/scherzo/orchestrator/effect_completion_handler.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/effects/types.gleam`, `src/scherzo/orchestrator/effects/interpreter.gleam`, `src/scherzo/orchestrator/worker_registry.gleam`, and `src/scherzo/workflow_checkpoint.gleam`. Add new modules only if they keep boundaries smaller, with `src/scherzo/orchestrator/identity.gleam` and `src/scherzo/state/ledger_batch.gleam` as the intended locations.

Relevant tests to add or update are `test/orchestrator_state_test.gleam`, `test/orchestrator_transition_ledger_test.gleam`, `test/orchestrator_transition_dispatch_test.gleam`, `test/orchestrator_transition_runner_test.gleam`, `test/orchestrator_worker_registry_test.gleam`, `test/orchestrator_effect_interpreter_test.gleam`, `test/orchestrator_effect_completion_handler_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_retry_step_test.gleam`, `test/workflow_run_test.gleam`, `test/state_record_test.gleam`, and `test/state_projection_test.gleam`.

Out of scope: changing ledger schema version, renaming existing JSON record kinds or fields, rewriting retained ledgers, changing operator command syntax, changing tracker protocol payloads, adding append-time projection or aggregate invariant validation, introducing typed causal effects for every possible transition, and migrating low-risk ids beyond the first-pass identity set.

## Milestones

Milestone 1 is test-first characterization. It is complete when new tests fail on the current tree because pending claims, worker lookup paths, and handoff append effects are still string-keyed or raw-list-based, and when existing characterization still documents the current handoff and worker lifecycle behavior.

Milestone 2 introduces identity wrappers and migrates state keys. It is complete when pending claims, pending dispatch validations, runtime task-keyed dictionaries, and worker registry lookups use `TaskIdentity`, `IssueId`, `RunId`, and `SessionId` at their boundaries, while tracker, CLI/operator, logging, and ledger JSON still use strings only at explicit conversion points.

Milestone 3 introduces typed ledger batch construction. It is complete when the claim-started batch and workflow-step append records are created through opaque constructors, `effects_types.LedgerAppend` no longer exposes arbitrary `List(record.RecordBody)` for orchestrator append effects, and the actual records written to disk are byte-shape compatible with existing `record.RecordBody` JSON.

Milestone 4 migrates daemon handoff and transition effects. It is complete when `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/transitions/claims.gleam`, `src/scherzo/orchestrator/transition.gleam`, and the transition shell/effect-completion modules no longer need a bare issue id to recover a task-keyed pending claim or worker entry.

Milestone 5 collects acceptance evidence. It is complete when targeted identity and ledger-batch tests, retained-ledger compatibility tests, full unit and contract suites, format checking, `glinter`, and `scherzo_lint` all pass with no required manual/browser/dogfood evidence before publish.

## Progress

- [x] (2026-05-29) Verified the prepared output target is the default directory `docs/plans`.
- [x] (2026-05-29) Inspected the current orchestrator state, transition, daemon handoff, effect, ledger, workflow checkpoint, and test files named in this review.
- [x] (2026-05-29) Wrote this concise review document for the follow-up implementation task; no code implementation was performed in this workflow.
- [x] (2026-05-29) Validated this review document with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-750-type-safe-identities-ledger-batches.md`, which reported `REVIEW_DOC_VALID=ok`.

## Decision Log

- Decision: First-pass distinct identifiers are `TaskIdentity`, `IssueId`, `RunId`, and `SessionId`; workflow ids, step ids, issue display identifiers, command route ids, scheduled job ids, and artifact refs remain raw strings temporarily. Rationale: these four identifiers cross the handoff, pending-claim, worker lookup, and ledger-batch seams that caused the LIV-749 class of bug. Date: 2026-05-29.
- Decision: Put typed ledger batch constructors at the ledger/effects boundary rather than scattering ad hoc constructors only inside transition modules. Rationale: the append effect should not expose arbitrary record-body lists, but constructors must still lower to existing record bodies to preserve JSON compatibility. Date: 2026-05-29.
- Decision: Allow targeted test fixture churn only where type signatures force it. Rationale: broad fixture rewrites would make compatibility harder to review, so retained-ledger JSON fixtures should change only if a test is deliberately added for this plan. Date: 2026-05-29.
- Decision: Defer append-time aggregate invariant validation and typed causal effects to a second plan. Rationale: this pass is about making common invalid states harder to express, not proving every ledger append against a full aggregate model. Date: 2026-05-29.

## Validation and Acceptance

This review artifact is accepted when `test -f docs/plans/LIV-750-type-safe-identities-ledger-batches.md` succeeds and `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-750-type-safe-identities-ledger-batches.md` reports `REVIEW_DOC_VALID=ok` for this path.

The follow-up implementation is accepted only with concrete evidence for each required outcome. The identity outcome is evidenced by failing-then-passing tests or source guardrails proving `pending_claims`, pending dispatch validations, runtime task-keyed dictionaries, and worker lookup dictionaries are not `Dict(String, ...)` and that bare issue ids cannot satisfy task-identity lookups. The ledger-batch outcome is evidenced by tests proving the claim-started batch contains the workflow start, known workspace, run start, and issue-counter records in order, and that workflow-step batch constructors emit the same record bodies as the prior direct append path. The compatibility outcome is evidenced by `test/state_record_test.gleam`, `test/state_projection_test.gleam`, and retained-ledger decode/projection tests showing existing JSON records still decode and project unchanged. The deferred-scope outcome is evidenced by source review or tests showing no new append-time aggregate validator and no broad typed causal-effect hierarchy were introduced.

Before publish, run from the repository root: `direnv exec . gleam test -- --suite unit`, `direnv exec . gleam test -- --suite contract`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. Success means the test suites report no failures, format exits zero, and both lint commands exit zero with no new production policy errors. No manual/browser/dogfood evidence is required before publish for this internal hardening pass; if a human operator wants post-implementation evidence, they may replay or inspect an existing retained ledger and confirm that operator commands still accept Linear issue ids as strings.

## Rollout, Recovery, and Idempotence

Rollout should be a normal code rollout with no data migration. The safe sequence is tests first, identity wrappers and dictionary-key migration, typed ledger batches, daemon/transition handoff migration, then cleanup of obsolete raw-list append helpers. Each step is reversible by reverting the most recent migration commit because stored ledger records and operator command syntax stay unchanged.

Recovery from a regression is to revert the identity or batch migration commit and keep any failing regression test that reproduced the mismatch. Re-running tests, format checks, source guardrails, and lint gates is idempotent. Existing retained ledgers require no rewrite, and repeated implementation attempts must not append records or mutate operator state as part of validation.

## Open Questions and Clarifications Needed

No open question blocks implementation. The first pass intentionally chooses the four high-risk identity types named above, places batch constructors in the ledger/effects boundary, permits only targeted fixture churn, and keeps retained ledger JSON plus operator/tracker string inputs unchanged. The second plan should decide whether to add append-time aggregate invariant validation, a typed causal-effect model, and broader identity coverage for workflow ids, step ids, scheduled jobs, and artifact refs.
