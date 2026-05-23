# Workflow step recovery negative-path hardening

## Purpose / Big Picture

This plan prepares LIV-491 to make workflow step recovery trustworthy when recovery itself fails. After implementation, a failed recovery worker, timeout, invalid recovery result, write conflict, or daemon interruption will leave the original failed step evidence intact, add redacted durable diagnostics where possible, and expose enough state for an operator or follow-up tooling to understand the failed attempt, recovery attempt, retry attempt, and retry result.

## Problem Framing and Constraints

LIV-491 is a hardening slice, not the feature activation slice. It assumes LIV-488 has already made `recover` runtime execution real: fatal step failure records the original attempt, starts a nested recovery worker in the same workspace, accepts `retry_requested` or `gave_up`, and retries the original step unchanged only for a valid retry request. If those runtime contracts are absent, implementation must stop and report the missing prerequisite rather than rebuilding the happy path inside LIV-491.

The plan also assumes LIV-490 is the owner for rich operator timeline rendering. LIV-491 should supply redacted, inspectable projection/session facts and should wire them into any LIV-490 history helper that is present in the checkout, but it should not invent a broader UI or JSON protocol. Recovered terminal outcome naming, including `succeeded_after_recovery` and `failed_after_recovery`, remains out of scope except as a prerequisite data shape.

Validation scope is deterministic. Provider-live behavior, provider cache or cache-TTL behavior, browser UI checks, and required manual dogfood are not part of the pre-publish gate for LIV-491. Any live recovery dogfood run is a deferred human/operator confidence check after deterministic tests, documentation checks, and lint pass.

## Strategy Overview

Harden recovery at four boundaries. First, normalize every negative recovery result into a non-retry outcome that preserves the original step failure and writes a redacted finish diagnostic when checkpointing permits. Second, treat recovery artifacts and checkpoint records as immutable evidence: identical rewrites may be reused, conflicting rewrites must fail visibly, and existing bytes or records must not be overwritten. Third, extend projection and startup recovery logic so a `workflow_step_recovery_started` record without a matching finish record is an inspectable interrupted state and is never silently resumed as unknown in-flight recovery work. Fourth, add an evidence-link helper or projection view that ties the failed step attempt artifact, recovery attempt, recovery result artifact when present, retry attempt artifact, and retry outcome together without changing happy-path scheduling. If the LIV-490 operator-history helper has landed, wire the facts into that helper and update its tests; if it has not landed, keep the helper pure, tested, and ready for that later display layer. Finish by checking `docs/runbooks/workflow-step-recovery.md` and helper names so the runbook, artifact paths, and history terminology do not describe stale behavior.

## Alternatives Considered

One alternative is to leave these cases as ordinary worker failure logs. That is too weak because logs can be transient and do not protect the immutable evidence trail operators need after restart.

Another alternative is to fold all visibility into LIV-490. That would make the UI better but would not fix runtime write-conflict behavior, startup safety, or artifact preservation.

A third option is to automatically resume a recovery worker after daemon restart. That is intentionally rejected for this slice because Scherzo cannot know whether the previous recovery worker made partial workspace changes or emitted an uncheckpointed result.

## Risks and Countermeasures

The main risk is corrupting or replacing the original evidence while trying to record recovery diagnostics. The countermeasure is fail-closed immutable writes, explicit conflict tests, and preserving the original failed attempt as the terminal source of truth whenever recovery evidence cannot be completed.

A second risk is leaking secrets through recovery prompts, conflict messages, retained artifacts, or session summaries. The countermeasure is to run all recovery context and diagnostics through the existing redaction/truncation path and to test both secret values and local workspace paths.

A third risk is silently restarting unsafe work after interruption. The countermeasure is to detect recovery-start-without-finish records during projection/startup recovery, mark them as interrupted or inspection-needed, and suppress automatic recovery-worker resumption.

A fourth risk is scope creep into recovered outcome naming or operator UI design. The countermeasure is to keep LIV-491 limited to durable facts, redaction, negative-path runtime behavior, and optional wiring into already-landed LIV-490 helpers.

## Scope Boundaries

In scope are recovery worker crash and timeout behavior; missing, duplicate, malformed, wrong-schema, and otherwise invalid recovery-result behavior; recovery artifact and checkpoint write-conflict handling; redaction for recovery context, diagnostics, retained artifacts, and session summaries; startup safety for recovery-start-without-finish records; durable evidence linkage across failed, recovery, retry, and retry-result artifacts; and narrow documentation or helper migration needed to keep implemented recovery diagnostics discoverable.

Out of scope are changing normal clean workflow behavior, changing the valid `retry_requested` happy path, renaming recovered terminal outcomes, adding browser UI, adding a classification engine, changing workspace privileges, changing provider-live setup, changing provider cache keys or TTLs, requiring manual dogfood before publish, or automatically resuming an unknown in-flight recovery worker.

## Milestones

Milestone 1 verifies prerequisite contracts and branch timing before coding. The implementer must inspect `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_step_recovery.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/artifact_store.gleam`, `src/scherzo/state/recovery.gleam`, and `docs/runbooks/workflow-step-recovery.md`. The outcome is a short implementation note proving the checkout has LIV-488 runtime recovery records, retained recovery artifacts, and retry requeueing; it also records whether a LIV-490 operator-history helper exists. If those predecessor contracts are absent or incompatible, implementation stops with evidence instead of rebuilding them in this hardening ticket.

Milestone 2 closes protocol and runtime negative-path gaps. The outcome is deterministic `test/workflow_step_recovery_test.gleam` and `test/workflow_run_test.gleam` coverage for recovery worker crash/failure, timeout-shaped worker failure, missing result, duplicate result, malformed JSON, missing arguments, wrong artifact type, wrong schema version, invalid decision, `gave_up`, checkpoint-start failure, checkpoint-finish failure, and recovery artifact write failure. Each test must assert that the original failed step artifact and `workflow_step_failed` path remain the source of truth, that no retry occurs unless `retry_requested` is valid and durably recorded, and that any diagnostic text is redacted.

Milestone 3 hardens immutable evidence writes. The outcome is `test/artifact_store_test.gleam` and checkpoint-level coverage proving identical recovery artifact rewrites are accepted as existing, conflicting recovery artifact bytes return an explicit immutable-conflict error without mutation, conflicting recovery finish checkpoints do not overwrite existing records, and `workflow_run` treats those write errors as recovery failure while preserving the original failure evidence.

Milestone 4 makes interruption state inspectable and non-resumable. The outcome is projection/startup/session evidence for a `workflow_step_recovery_started` record without a matching finish record. `test/state_projection_test.gleam` must show the start-only recovery survives folding and snapshot round trip as a visible started/interrupted-or-inspection-needed state; `test/state_recovery_test.gleam` or the appropriate startup-recovery test must show no recovered workflow resumption is scheduled for the nested recovery worker; session or helper tests must show redacted, bounded operator-facing diagnostics for the incomplete recovery.

Milestone 5 links retained evidence and completes documentation and validation. The outcome is a tested helper or projection view that joins the failed attempt artifact, recovery attempt, recovery result artifact when present, retry attempt artifact, and retry result. If `src/scherzo/ctl/workflow_recovery_history.gleam` or another LIV-490 helper is present, wire into it and update its tests; otherwise add the pure helper in the smallest existing module that already owns projection/session facts. Finish by updating `docs/runbooks/workflow-step-recovery.md` only where stale, then collect full `gleam test`, format, `glinter`, and `scherzo_lint` evidence.

## Progress

- [x] (2026-05-23) Reviewed the current recovery runtime, protocol, checkpoint, projection, artifact-store, startup recovery, session recovery, and related plans.
- [x] (2026-05-23) Authored this concise review document for LIV-500; mechanical implementation detail is supplied through the structured implementation-pack submission for LIV-491.
- [x] (2026-05-23) Incorporated review feedback by making milestones file-specific, adding malformed/wrong-schema protocol coverage, immutable conflict evidence, startup/session interruption evidence, docs/helper migration, provider-live/cache non-scope, deferred dogfood timing, and full validation/lint obligations.

## Decision Log

- Decision: Negative recovery outcomes preserve the original failed step behavior and never retry the original step.
  Rationale: Recovery is optional remediation; failed remediation must not hide or replace the product failure that triggered it.
  Date: 2026-05-23

- Decision: Recovery evidence is immutable and fail-closed.
  Rationale: Operators need to trust retained artifacts after conflicts, duplicate submissions, and restarts.
  Date: 2026-05-23

- Decision: A recovery start without finish is an interrupted, inspectable state rather than resumable work.
  Rationale: The daemon cannot prove whether the old recovery worker made partial changes or emitted an unrecorded result.
  Date: 2026-05-23

- Decision: LIV-491 exposes durable facts for LIV-490 instead of owning rich timeline presentation.
  Rationale: This keeps hardening focused while still giving operator visibility work reliable, redacted source data.
  Date: 2026-05-23

- Decision: Provider-live, provider-cache, browser, and required manual dogfood checks are not pre-publish gates for LIV-491.
  Rationale: The negative-path hardening claims are falsifiable with deterministic fake workers, local ledgers, artifact-store fixtures, projection/session tests, and lint gates; live dogfood remains useful only as deferred operator confidence evidence.
  Date: 2026-05-23

- Decision: Documentation and helper migration are required finishing checks, not optional cleanup.
  Rationale: Recovery hardening is only operator-useful if the runbook and helper names point to the durable evidence that the implementation actually writes.
  Date: 2026-05-23

## Validation and Acceptance

Acceptance must be backed by deterministic evidence. Runtime tests must show recovery worker crash/failure, timeout-shaped worker failure, missing result, duplicate result, malformed JSON, missing tool arguments, wrong artifact type, wrong schema version, invalid decision, `gave_up`, checkpoint start failure, checkpoint finish failure, recovery artifact write failure, and artifact conflict all leave the workflow on the original failure path and write only redacted recovery diagnostics. These tests must also prove that no retry occurs unless a valid `retry_requested` decision is parsed, its recovery artifact write succeeds, and its finish checkpoint succeeds.

Artifact-store and checkpoint tests must show immutable conflicts return explicit errors and preserve the original bytes or records. Projection/startup tests must show a recovery start without finish is visible as started, interrupted, or inspection-needed after fold and snapshot round trip and produces no automatic recovery-worker resumption. Session/redaction or helper tests must show secrets and local workspace paths are absent from recovery prompts, protocol errors, conflict diagnostics, retained artifacts, projection/session summaries, and evidence-link output.

The retained-evidence acceptance artifact is a deterministic ledger/projection assertion or concise transcript that links `runs/<run>/<step>/attempt-<n>.json`, `runs/<run>/<step>/attempt-<n>/recovery-<m>/workflow_step_recovery_result.json` when present, the retry attempt artifact, and the retry outcome. Documentation/helper acceptance requires checking `docs/runbooks/workflow-step-recovery.md` and any LIV-490 helper present in the checkout; stale artifact paths, helper names, or deferred-history text must be updated before handoff.

Before implementation handoff, run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. All four gates should pass, or the handoff must record exact unrelated pre-existing lint inventory without treating it as this change's success. No browser, live-provider, provider-cache, cache-TTL, or dogfood evidence is required before publish; any manual dogfood recovery run is optional deferred post-implementation operator evidence.

## Rollout, Recovery, and Idempotence

Rollout is additive over existing recovery records. Old ledgers remain readable, workflows without `recover` behave as before, and already-retained recovery artifacts are never rewritten. The change does not alter provider-live configuration, provider cache keys, cache TTLs, browser behavior, or the required manual operator workflow. If the change must be backed out, keeping the immutable artifact conflict checks and projection readers is safe because they only make existing evidence safer to inspect.

The implementation must be idempotent. Replaying the same ledger should produce the same interrupted recovery state and evidence links; re-running tests should use fresh run roots or deterministic fixtures; repeated artifact writes with identical bytes may succeed as existing, while different bytes at the same recovery artifact ref must fail without mutation. A partially completed implementation must not ship if it can skip the original failed artifact, retry after an invalid or undurable recovery result, hide a start-without-finish recovery, or require manual cleanup of provider cache or live-provider state.

## Open Questions and Clarifications Needed

No stakeholder clarification is required before implementation. The implementer must verify the exact LIV-488 runtime contracts and the branch timing of LIV-490 before coding, then either wire into the landed operator-history helper or leave redacted projection/session facts ready for that helper to consume.
