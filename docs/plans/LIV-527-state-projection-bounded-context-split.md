# LIV-527 ExecPlan Review: State Projection Modularization

This ExecPlan v2 review document is the human review surface for LIV-527. It plans a follow-up state-projection bounded-context split only; exact implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

The durable ledger should remain a single replayable source of truth while its records, reducers, snapshot codecs, and query helpers become easier to review by runtime concern. After the follow-up implementation plan is executed, maintainers should be able to change scheduled-job recovery, command receipts, workstreams, or workflow-step recovery without re-reading one monolithic `src/scherzo/state/record.gleam` and one monolithic `src/scherzo/state/projection.gleam`.

This is visible when a later implementer extracts scheduled-job logic behind the existing facades, replays the existing schema fixtures without byte or semantic drift, and presents validation evidence from the state test suite, schema guardrails, format check, production linters, and a manual reducer-composition review before publishing.

## Problem Framing and Constraints

Today `src/scherzo/state/record.gleam` owns the durable envelope, 59 `RecordBody` variants, JSON kind mapping, record encode/decode, redaction, and error descriptions. `src/scherzo/state/projection.gleam` owns replay for 18 projection fields, snapshot encode/decode, and query helpers. The split must preserve ledger schema version 2, existing JSON `kind` strings, top-level replay semantics in `src/scherzo/state/ledger.gleam`, projection snapshot shape, recovery behavior, and upgrade diagnostics. This ticket is planning only; no production split is implemented here.

Because this is durable state code, compatibility is more important than directory shape. The work does not require docs/helper migration, provider-live probes, review-lane cache behavior, or browser UI checks; any implementation that discovers such a dependency should split it into a separate ticket rather than folding it into the state extraction.

## Strategy Overview

Keep `scherzo/state/record.gleam` and `scherzo/state/projection.gleam` as public compatibility facades while extracting context-owned logic behind them one slice at a time. Proposed bounded contexts are: legacy run records; workflow runs, manifests, provenance, and workflow repairs; step attempts and step recovery; issue recovery, retry, parking, counters, and known workspaces; remote commands and command receipts; outbox; scheduled jobs; and workstreams with artifact and handoff references.

Record constructor helpers should live in context modules such as `scherzo/state/record/scheduled`, `.../commands`, and `.../workstreams`; record codecs should be split by the same context but keep the existing top-level `record.to_string`, `record.decode_string`, and `record.kind` API. Projection reducer modules should own context-specific transition helpers and snapshot entry codecs, while `projection.apply` remains the deterministic composition point. Snapshot encoding should continue to write the flat `projection_snapshot` object with the same top-level arrays until a separate explicit migration ticket chooses otherwise.

Top-level ledger replay should keep the current flow: `ledger.replay` and `ledger.load_projection` read an optional snapshot, parse JSONL records, and fold them through `projection.fold_from` or streaming `projection.apply`. After extraction, `projection.apply` should dispatch a record through context reducers in a stable order, with each durable record kind having one owning reducer unless a shared index is explicitly updated at the root facade.

The recommended first extraction slice is scheduled jobs because the `Scheduled*` record family is cohesive, mostly updates only `scheduled_jobs`, already has optional snapshot compatibility handling, and has focused projection tests.

## Alternatives Considered

Leaving the modules centralized and adding comments would be lowest risk but would not reduce schema-review blast radius. A single large split of all record and projection concerns would create too much durable-state and replay risk in one change. Introducing a nested or version-3 snapshot schema during the split was rejected because the current problem is maintainability, not schema semantics. Removing old record variants or compatibility shims is out of scope without an explicit migration decision.

## Risks and Countermeasures

The main risk is durable schema drift. Counter it with golden ledger JSONL and projection snapshot fixtures that must remain byte-for-byte stable unless a migration is approved. Replay-order regressions are countered by a composition contract in `projection.apply` and parity tests over the full schema fixture. Public API churn is countered by keeping the existing facade modules until call sites are intentionally migrated. Snapshot decode regressions are countered by tests for older snapshots with omitted optional fields. Recovery regressions are countered by replay, compaction, retry, parking, step-recovery, and outbox replay tests. Upgrade diagnostic regressions are countered by malformed JSON, unknown kind, unsupported record version, and unsupported snapshot version tests.

A second risk is that review feedback could remain only in this prose document while Scherzo materializes later implementation steps from the structured pack. Counter it by mirroring acceptance evidence, test obligations, manual pre-publish review, docs/helper and provider-live/cache non-scope, full validation, and lint gates in the implementation-pack concrete steps and testing notes before bundle materialization.

## Scope Boundaries

In scope for this planning issue is exactly this Markdown review document and one structured implementation-pack submission. No production split, canonical bundle JSON, helper migration, or schema migration belongs in this issue.

In scope for the follow-up ExecPlan is a module-boundary design and incremental extraction plan. The current record-family ownership map is: legacy runs own `RunStarted`, `RunFinished`, and `RunInterrupted`; workflow runs own `WorkflowRunStarted`, `WorkflowRunStartedWithTask`, `WorkflowRunFinished`, `WorkflowRunFinishedWithTask`, `WorkflowRunInputsRecorded`, `WorkflowRunOutputsRecorded`, `WorkflowRunDiagnostic`, `WorkflowRunInterrupted`, `WorkflowRunSuperseded`, and `WorkflowRepairRequested`; step execution owns `StepAttemptPrepared`, `StepAttemptStarted`, `StepAttemptContinuationStarted`, `StepAttemptPiSessionRecorded`, `StepAttemptPiSessionRecordedWithTask`, `StepAttemptFinished`, `WorkflowStepRecoveryStarted`, `WorkflowStepRecoveryFinished`, `StepAttemptInterrupted`, and `StepAttemptSuperseded`; issue recovery owns `RetryScheduled`, `RetryCancelled`, `IssueCounterUpdated`, `KnownWorkspace`, `IssueParked`, `IssueParkedV2`, and `IssueUnparked`; commands own `LinearCommandSeen`, `LinearCommandStarted`, `LinearCommandCompleted`, `LinearCommandAcked`, `RemoteCommandSeen`, `RemoteCommandStarted`, `RemoteCommandCompleted`, and `RemoteCommandAcked`; scheduled jobs own all twelve `Scheduled*` variants; outbox owns `OutboxPending`, `OutboxPendingV2`, `OutboxCompleted`, and `OutboxFailed`; workstreams own `WorkstreamCreated`, `WorkstreamAssigned`, `WorkstreamArtifactRecorded`, `WorkstreamHandoffRecorded`, and `WorkstreamPhaseRunQueued`.

The projection-field ownership map is: legacy runs own `runs`; workflow runs own `workflow_runs`, `workflow_run_provenances`, `workflow_task_refs`, `workflow_input_manifests`, `workflow_output_manifests`, and `workflow_repairs`; step execution owns `step_attempts` and `step_recoveries`; issue recovery owns `retries`, `parked_issues`, `issue_counters`, and `known_workspaces`; commands own `commands` and `command_receipts`; outbox owns `outbox`; workstreams own `workstreams`; scheduled jobs own `scheduled_jobs`. Out of scope here are schema semantic changes, variant removal, migration to a different snapshot shape, and implementing the extraction directly in this planning ticket.

The follow-up implementation may update tests and narrow docs that explain state module ownership, but it should not migrate workflow helper scripts, provider-facing structured-output helpers, review-lane live checks, or cache behavior. If a slice unexpectedly needs those surfaces, that work is split into a separate acceptance item with explicit helper or contract tests; otherwise evidence records that they were unchanged.

## Milestones

Milestone 1 packages the ownership inventory and compatibility contract. Reviewers should see a canonical ExecPlan bundle whose implementation plan names `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, the new context module paths, the 59 record variants, the 18 projection fields, and the rule that `record.to_string`, `record.decode_string`, `record.kind`, `projection.apply`, `projection.to_string`, and `projection.decode_string` remain the facade APIs.

Milestone 2 establishes parity tests and reducer-composition evidence before moving durable logic. Reviewers should see golden fixture tests for `test/fixtures/schema/ledger_records_v2.jsonl` and `test/fixtures/schema/projection_snapshot_v2.json`, malformed JSON/unknown-kind/unsupported-version diagnostics, old-snapshot optional-field decoding, and a manual pre-publish note confirming that `projection.apply` dispatches context reducers in a deterministic order with no record kind handled by two contexts unless the root facade owns a shared index.

Milestone 3 extracts the scheduled-job slice. Reviewers should see new scheduled context modules behind the `record` and `projection` facades, scheduled record encode/decode parity, scheduled snapshot encode/decode parity, focused scheduled-state tests in `test/state_projection_test.gleam`, and unchanged output from `direnv exec . gleam test`.

Milestone 4 repeats the slice pattern for outbox plus commands, workstreams, issue recovery, and then the more coupled workflow-run and step-attempt contexts. Each slice lands only after its record helpers, decoder cases, reducer cases, snapshot entries, and query helpers have parity coverage in the relevant state tests.

Milestone 5 completes compatibility hardening, helper-boundary inventory, and full gates. Reviewers should see evidence from `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; retained fixture diffs showing `test/fixtures/schema/ledger_records_v2.jsonl` and `test/fixtures/schema/projection_snapshot_v2.json` unchanged; recovery, compaction, retry, parking, step-recovery, outbox, and upgrade-diagnostic coverage; and an explicit note that no docs/helper migration or provider-live/cache behavior changed for this state-only refactor.

## Progress

- [x] (2026-05-25) Inspected `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/ledger.gleam`, and schema guardrail fixtures to inventory durable record and projection responsibilities.
- [x] (2026-05-25) Chose a facade-preserving modularization strategy and selected scheduled jobs as the first extraction slice.
- [x] (2026-05-25) Prepared this review document for `docs/plans/` and the structured implementation-pack handoff for Scherzo bundle materialization.
- [x] (2026-05-25) Validated the review document with `python3 workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-527-state-projection-bounded-context-split.md` before and after incorporating review feedback.
- [x] (2026-05-25) Incorporated review feedback by making acceptance evidence, milestone specificity, test obligations, manual pre-publish reducer review, docs/helper and provider-live/cache non-scope, full validation, and lint gates explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Keep the existing `record` and `projection` modules as public facades during the split. Rationale: existing call sites and durable replay depend on their APIs, and facade preservation isolates code motion from schema migration. Date: 2026-05-25.
- Decision: Keep projection snapshots flat and schema-version 2 during extraction. Rationale: modular code ownership does not require a stored-data migration. Date: 2026-05-25.
- Decision: Use scheduled jobs as the first extraction slice. Rationale: it is cohesive, has a single projection field, and already has targeted tests and optional snapshot decoding. Date: 2026-05-25.
- Decision: Treat review feedback about evidence, tests, manual checks, helper boundaries, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations. Rationale: Scherzo materializes the follow-up implementation plan from the structured pack, so prose-only obligations would be easy for later implementers to miss. Date: 2026-05-25.

## Validation and Acceptance

This planning issue is accepted when this file exists at `docs/plans/LIV-527-state-projection-bounded-context-split.md`, `python3 workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-527-state-projection-bounded-context-split.md` exits zero with `REVIEW_DOC_VALID=ok`, and Scherzo captures the structured implementation-pack submission for LIV-527. In the packaged workflow, the same validator may be invoked from the workflow bundle as `scripts/scherzo-execplan validate-review-doc`.

The materialized follow-up implementation task should contain `Bundle ref:` and `Bundle sha256:` lines for the canonical ExecPlan. The plan should include the 59-variant ownership map and the 18-field projection ownership map above, plus exact module names for each context slice.

Later implementation evidence includes record codec parity, projection reducer parity, snapshot encode/decode parity, replay parity, recovery replay, compaction, retry, parking, step-recovery, outbox replay, malformed JSON, unknown kind, unsupported record version, and unsupported snapshot version checks. The golden `test/fixtures/schema/ledger_records_v2.jsonl` and `test/fixtures/schema/projection_snapshot_v2.json` remain unchanged unless an explicit migration ticket updates them.

Repository validation evidence includes these repository-root commands exiting zero: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. State-specific test evidence should name `test/schema_guardrail_test.gleam`, `test/state_record_test.gleam`, `test/state_projection_test.gleam`, `test/state_ledger_test.gleam`, and `test/state_compaction_test.gleam` among the covered files.

Manual pre-publish evidence is a short reviewer note confirming that `projection.apply` composes context reducers deterministically, snapshot encode/decode still accepts old optional omissions, unsupported-version or corrupt-record diagnostics remain observable, and the facade APIs did not churn. No browser check is needed. A post-implementation dogfood run against a live Scherzo daemon is deferred to a human/operator after implementation and should be recorded as an optional observation, not as a blocker for the state-only code split.

Docs/helper evidence is an inventory rather than a migration: if the later implementation does not touch workflow helper scripts, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache behavior, the acceptance note says they were unchanged. If it does touch any of those surfaces, helper or contract tests and provider-live/cache stale-read, invalidation, and TTL-disabling coverage become part of that separate change before acceptance.

## Rollout, Recovery, and Idempotence

The rollout should be additive and reversible: introduce context modules behind facades, prove parity, then remove duplicated helper code only after tests pass. No ledger data migration is required for the first slice. If an extraction causes replay, snapshot, or diagnostic drift, revert that slice and keep the facade implementation centralized.

If a follow-up implementation discovers that workflow helper scripts, provider-facing structured-output helpers, provider-live probes, or cache behavior need to change, split that work or explicitly roll it back before publishing the state extraction. The safe default is no helper migration and no provider-live/cache behavior change.

Re-running the extraction should be idempotent because record IDs, JSON kinds, snapshot keys, facade API names, and replay order are unchanged. Re-running validation should not rewrite the golden fixtures; any fixture diff is treated as migration evidence and rejected unless a separate migration ticket authorizes it.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this planning issue. Clarify during implementation whether Gleam type aliasing or re-export patterns are strong enough to move public projection status types out of `projection.gleam`; the first scheduled-job slice can avoid this by moving behavior before public type definitions. Clarify whether legacy Linear command records and generic remote command records should remain in one command context long term or split after the generic transport migration is complete.
