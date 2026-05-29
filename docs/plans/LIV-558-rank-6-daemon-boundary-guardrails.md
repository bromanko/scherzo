# LIV-558 Rank 6 daemon boundary guardrails review

This is a focused review document for Rank 6 of `docs/plans/LIV-523-daemon-decomposition-v2.md`. It plans daemon-boundary guardrails only; it does not implement the guardrails or change runtime behavior.

## Purpose / Big Picture

Rank 6 should make daemon regrowth visible after the extraction ranks establish real ownership boundaries. After the follow-up implementation, maintainers should get deterministic review signals when `src/scherzo/orchestrator/daemon.gleam` grows past its ratchet, when top-level helper prefixes owned by extracted subsystems return to the daemon, or when extracted orchestrator modules import `scherzo/orchestrator/daemon`. The work is source-test and documentation work, not a daemon behavior change. Review feedback makes the evidence contract explicit: the follow-up implementation must surface targeted guardrail proof, full repository gates, no docs/helper or provider-live/cache migration, and manual/dogfood timing in both the human review and implementation pack.

## Problem Framing and Constraints

The source plan names Rank 6 as daemon boundary guardrails and explicitly says guardrails are not a substitute for extraction. The current tree already has several extracted orchestrator modules, including `scheduled_runtime.gleam`, `startup_recovery.gleam`, `worker_lifecycle.gleam`, `yaml_workflow_lifecycle.gleam`, `operator_runtime.gleam`, and `remote_command_runtime.gleam`, while `src/scherzo/orchestrator/daemon.gleam` still reports `6250` lines in this workspace. There is no `test/orchestrator_daemon_boundary_test.gleam` yet, and there is no `docs/architecture/daemon-boundary.md` yet.

The repository already has broader guardrails in `test/source_guardrail_test.gleam` and `test/architecture_guardrail_test.gleam`; the latter already allows `src/scherzo/orchestrator/service.gleam` to import the daemon as the process-startup edge. Rank 6 should add a daemon-specific, ownership-aware guardrail rather than replace those broader checks. It must not plan ranks 1 through 5 except as dependencies, and it must not change public daemon/control behavior, workflow semantics, provider-live behavior, cache behavior, or the OTP/process architecture. The only planned documentation change is the new daemon-boundary architecture document; workflow helper scripts, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, and cache behavior should remain untouched and be confirmed as unchanged in the implementation handoff.

## Strategy Overview

Add a short ownership document at `docs/architecture/daemon-boundary.md` and a deterministic source test at `test/orchestrator_daemon_boundary_test.gleam`. The document should name what the daemon is allowed to own: public actor startup, public message receipt, compatibility types, dependency injection, control-plane/process/timer edges, top-level logging/redaction context, and handoff between subsystem outcomes. It should also name extracted owners and their allowed top-level function prefixes, with any explicit daemon shell exceptions.

The source test should check four things. First, the ownership document exists and names the daemon allow-list and extracted owner modules. Second, `daemon.gleam` stays at or below a checked-in `max_daemon_lines` ratchet; the initial value should be the exact line count on the implementation branch after ranks 1-5 are present, lowered whenever the daemon shrinks and raised only with an explicit daemon-owned rationale in the boundary document. Third, top-level `fn` and `pub fn` declarations in `daemon.gleam` must not start with prefixes assigned to extracted modules unless the exact function name is listed as a shell exception. Fourth, extracted orchestrator subsystem modules must not import `scherzo/orchestrator/daemon`; `service.gleam` remains the documented startup-edge exception.

Use source tests by default. Add a narrow custom `scherzo_lint` rule only if string-based source tests prove too brittle for function declarations or import parsing. Negative source-test fixtures should be added only if that linter path is chosen cleanly; otherwise synthetic in-memory strings inside `test/orchestrator_daemon_boundary_test.gleam` are sufficient. Each implementation step should capture the proof it creates: targeted guardrail test output, any focused linter output if the linter path is used, the full validation and lint gates, and the explicit no-change inventory for docs/helpers and provider-live/cache behavior.

## Alternatives Considered

A line-count-only guardrail was rejected because it would catch size but not ownership regression. Relying only on the existing architecture guardrail was rejected because it checks daemon imports but not daemon line-count ratcheting or extracted function prefixes. Starting with a custom linter rule was rejected as the default because the required checks are narrow and can be expressed as deterministic source tests; the linter remains available if source scanning becomes fragile. Manual reviewer discipline alone was rejected because daemon regrowth should fail before review, not depend on a human noticing a repeated pattern.

## Risks and Countermeasures

One risk is brittleness from parsing Gleam with simple string scans. The countermeasure is to keep the scanner narrow, add synthetic negative tests for forbidden declarations and imports, and escalate to a focused `scherzo_lint` rule only if the source-test scanner cannot remain stable. Another risk is false positives for legitimate daemon shell callbacks. The countermeasure is an explicit shell-exception list in `docs/architecture/daemon-boundary.md` and the test constants; exceptions must be exact function names with a rationale, not broad new prefixes.

A third risk is ratchet drift. The countermeasure is a clear update policy: lower `max_daemon_lines` in the same change when the daemon shrinks, never raise it for extracted subsystem code returning to the daemon, and raise it only when reviewers agree the added code is daemon-owned by the boundary document. A fourth risk is running this before ranks 1-5 have real boundaries. The countermeasure is to calibrate the blocked-prefix list only from ownership that exists on the implementation branch and to leave not-yet-extracted concerns out of the forbidden prefix list until their owners are real.

A fifth risk is review/pack drift: the prose document could require evidence that the structured implementation pack does not ask the follow-up implementer to collect. The countermeasure is to mirror acceptance evidence, exact test obligations, milestone proof anchors, manual/dogfood timing, docs/helper non-migration, provider-live/cache non-scope, full validation, and lint gates in the implementation pack concrete steps and testing notes.

## Scope Boundaries

In scope: `test/orchestrator_daemon_boundary_test.gleam`, `docs/architecture/daemon-boundary.md`, a possible narrow `test/scherzo_lint/rules/daemon_boundary.gleam` with its tests and registration only if source tests are too brittle, acceptance-evidence handoff notes for targeted and full gates, a scope inventory confirming docs/helper and provider-live/cache surfaces stayed unchanged, and optional lowering of the existing daemon baseline in `test/source_guardrail_test.gleam` if it is higher than the new daemon-specific ratchet.

Out of scope: implementing or replanning LIV-523 ranks 1-5, moving daemon runtime code, changing public daemon/control messages, changing Linear command behavior, changing workflow YAML behavior, changing EventHub or ledger shapes, provider-live or cache behavior changes, workflow helper/script/schema/provider helper/review-lane contract migrations beyond adding the daemon-boundary architecture document, and any browser/manual/dogfood validation requirement for this guardrail-only rank.

## Milestones

Milestone 1 documents the boundary. It is complete when `docs/architecture/daemon-boundary.md` exists, names daemon-owned responsibilities, lists extracted subsystem owner modules and blocked top-level prefixes, and records the ratchet update policy and exact shell-exception process.

Milestone 2 adds daemon-specific source tests. It is complete when `test/orchestrator_daemon_boundary_test.gleam` checks the boundary document, daemon line-count ratchet, forbidden extracted top-level prefixes, synthetic prefix failures, extracted-module daemon imports, and synthetic import failures. The import check should preserve the startup-edge exception for `src/scherzo/orchestrator/service.gleam`.

Milestone 3 decides whether a custom lint rule is necessary. It is complete when the implementation records that source tests are sufficient, or, if they are not, adds `test/scherzo_lint/rules/daemon_boundary.gleam`, registers it in `test/scherzo_lint.gleam`, and covers it with focused positive and negative tests. Negative fixture files belong only in this linter path.

Milestone 4 collects and records acceptance evidence. It is complete when the handoff includes targeted `direnv exec . gleam test test/orchestrator_daemon_boundary_test.gleam` output, or focused linter test output if a custom linter rule is added, showing the guardrails fail on synthetic regressions and pass on the current tree. It also requires `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` to pass; a diff/scope inventory confirming no workflow helper scripts, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache behavior changed; and a handoff note stating that no pre-publish runtime manual/browser/dogfood validation is required unless the implementation unexpectedly changes behavior.

## Progress

- [x] (2026-05-29) Read the prepared output target and confirmed the default directory is `docs/plans`.
- [x] (2026-05-29) Re-read `docs/plans/LIV-523-daemon-decomposition-v2.md` and scoped this document to Rank 6 only.
- [x] (2026-05-29) Checked current daemon-boundary facts: `daemon.gleam` line count, existing orchestrator extraction modules, absence of `test/orchestrator_daemon_boundary_test.gleam`, absence of `docs/architecture/daemon-boundary.md`, and existing broader source/architecture guardrails.
- [x] (2026-05-29) Wrote this review document without implementing guardrail tests or documentation.
- [x] (2026-05-29) Validated this review document with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-558-rank-6-daemon-boundary-guardrails.md`; it reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-05-29) Incorporated review feedback by making acceptance evidence, targeted and synthetic-negative test obligations, milestone proof anchors, docs/helper and provider-live/cache non-scope inventory, full validation and lint gates, and manual/dogfood timing explicit in this review document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Keep this artifact focused only on LIV-523 Rank 6. Rationale: the task explicitly excludes replanning extraction ranks and says guardrails should follow real ownership boundaries. Date: 2026-05-29.
- Decision: Use deterministic source tests as the default implementation mechanism. Rationale: the required checks are narrow and reviewable, while a custom linter rule adds maintenance cost unless source tests prove brittle. Date: 2026-05-29.
- Decision: Make the daemon line-count budget a ratchet rather than a fixed historical number. Rationale: the useful signal is growth beyond the post-extraction branch baseline, not comparison to the old 7k-line daemon. Date: 2026-05-29.
- Decision: Keep `service.gleam` as the documented daemon-import exception. Rationale: existing architecture guardrails identify it as the process edge that launches the daemon actor, while lower orchestrator subsystems should not import the daemon. Date: 2026-05-29.
- Decision: Require no pre-publish manual/browser/dogfood validation for the guardrail-only implementation unless it changes runtime behavior. Rationale: the planned change has no runtime behavior surface; automated source tests and lint gates are the acceptance evidence. Date: 2026-05-29.
- Decision: Treat review feedback about evidence, tests, milestone specificity, docs/helper boundaries, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations. Rationale: Scherzo materializes follow-up work from the structured pack, so prose-only acceptance requirements are easy for later implementers to miss. Date: 2026-05-29.

## Validation and Acceptance

This review document is present when `test -f docs/plans/LIV-558-rank-6-daemon-boundary-guardrails.md` succeeds. It should validate with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-558-rank-6-daemon-boundary-guardrails.md`, expecting `REVIEW_DOC_VALID=ok` and `REVIEW_DOC_PATH=docs/plans/LIV-558-rank-6-daemon-boundary-guardrails.md`.

The follow-up implementation is acceptable only when `test/orchestrator_daemon_boundary_test.gleam` exists and proves all required guardrails: the boundary document exists, the daemon line count is at or below the checked-in ratchet, synthetic line growth fails, extracted top-level function prefixes cannot appear in `daemon.gleam` unless exactly allow-listed as shell exceptions, synthetic forbidden prefixes fail, extracted orchestrator subsystem modules do not import `scherzo/orchestrator/daemon`, and synthetic forbidden imports fail. `docs/architecture/daemon-boundary.md` must exist and include the daemon ownership allow-list, extracted owner modules, prefix policy, shell-exception process, and ratchet update policy.

If the implementation adds `test/scherzo_lint/rules/daemon_boundary.gleam`, acceptance also requires focused linter tests and registration in `test/scherzo_lint.gleam`; if no linter is added, the implementation evidence must say source tests were sufficient and no negative fixture files were needed. The implementation must also include a scope inventory evidence note confirming `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers and schemas, review-lane contract files, provider-live probes, and cache behavior were untouched; if any of those surfaces must change, split that work or roll it back before publishing this guardrail rank. Before publish, run from the repository root: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. No runtime manual/browser/dogfood validation is required for this rank unless the final implementation changes runtime behavior; if that happens, the implementation must name the changed behavior and collect manual/operator evidence before publish.

## Rollout, Recovery, and Idempotence

Rollout is additive and review-only: add the boundary document, add the source tests, optionally add the linter rule if justified, record the docs/helper and provider-live/cache no-change inventory, and run the gates. Recovery is to revert the document, the guardrail test, and any linter registration; there is no data migration, cache invalidation, provider-live cleanup, daemon restart, operator retraining, workflow helper migration, or structured-output helper cleanup. The validation commands are idempotent. If a future legitimate daemon-owned change trips the ratchet or prefix checks, update the boundary document and exact allow-list in the same review; if extracted subsystem code is returning to the daemon, do not loosen the guardrail without a new decomposition decision. If the implementation discovers that workflow helpers, provider-facing contracts, provider-live probes, or cache behavior must change, split that work or revert it before publishing this guardrail-only rank.

## Open Questions and Clarifications Needed

No open question blocks the implementation handoff. The exact forbidden-prefix list and shell exceptions should be calibrated on the implementation branch after the relevant extraction boundaries exist, then reviewed as part of `docs/architecture/daemon-boundary.md` and `test/orchestrator_daemon_boundary_test.gleam`.
