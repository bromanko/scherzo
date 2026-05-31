# Artifact Publication Ledger and Operator Inspection

This review document frames the ExecPlan for implementing durable artifact publication state and operator inspection for Scherzo. It intentionally keeps low-level implementation mechanics in the structured implementation pack.

## Purpose / Big Picture

After this change, an operator can answer what happened to a workflow artifact publication without rerunning the workflow or inspecting workspace files by hand. Scherzo will retain the full publication manifest as a canonical artifact, append durable publication attempt records to the existing state ledger, project the latest status for a run, publication id, and publication series, and expose that state through `scherzoctl artifact publication list --run <run-id>` and `scherzoctl artifact publication show --run <run-id> --publication <publication-id>`.

## Problem Framing and Constraints

The PRD makes Scherzo's internal artifact store the canonical source and treats external repositories as derived copies. Today the repository already has publication route parsing and a dry-run publication planner, but publication outcomes are not durable ledger state and there is no operator command for inspecting them. The design must use backend-neutral artifact refs, avoid large ledger records by retaining full manifests as artifacts, preserve the existing state ledger and projection architecture, and expose retryability information without adding full retry execution yet.

## Strategy Overview

The plan uses the existing state ledger as the publication summary log and the artifact store as the manifest body store. Publication execution for this slice remains dry-run/planning-oriented: each configured route writes a retained publication manifest, appends deterministic attempt records, and updates projections that the CLI can inspect. This is proportionate because it creates the state and operator seam needed by the future GitHub publisher without prematurely implementing remote mutation or retry orchestration.

## Alternatives Considered

A separate publication ledger was rejected because the existing ledger already supplies append ordering, projection loading, snapshots, and local inspection. Storing only manifests without summary ledger records was rejected because operators need fast latest-status queries and attempt history. Implementing GitHub publishing and retry execution now was rejected because this task is specifically the ledger and inspection slice, and remote mutation would expand risk before the state model is testable.

## Risks and Countermeasures

The main downgrade risk is that older binaries will not understand new ledger record kinds. The rollout must be additive in code, include snapshot decoding defaults for older snapshots, and document that downgrades after new records are written require restoring or archiving state. The main correctness risk is duplicate publication attempts after a crash or repeated workflow finalization; deterministic record ids, immutable manifest refs, and idempotent append behavior counter that. The main operator risk is misleading retry information; the CLI must separate `retryable` from `retry_execution_available`, because this plan exposes retryability metadata but does not implement retry commands. A review-process risk is letting acceptance evidence, test obligations, milestone proof, local dogfood timing, docs/helper boundaries, provider-live/cache non-scope, full validation, or lint gates live only in this prose document while Scherzo materializes implementation artifacts from the structured pack. The countermeasure is to mirror those obligations in the implementation pack's concrete steps and testing notes before publication.

## Scope Boundaries

In scope: publication attempt ledger records, retained full publication manifests, projection helpers for latest status by run/publication/series, pretty and JSON CLI inspection for list and show, retryability metadata, idempotency behavior, targeted tests for ledger, projection, manifest retention, and CLI output, and a pre-publish scope inventory that says whether docs/helper surfaces changed. If implementation touches `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, or operator docs helpers, it must run the relevant helper or offline contract tests and preserve existing provider-live/cache semantics; if it does not touch them, the evidence must say no helper migration, provider-live validation, or cache validation was applicable. Out of scope: GitHub remote branch or pull-request mutation, `scherzoctl artifact publication retry`, generalized review state, migration away from `workspace-driver publish-change`, provider-live/cache behavior, browser evidence, live GitHub dogfood, workflow helper-script migrations, workflow schema migrations, and provider-facing structured-output contract changes.

## Milestones

Milestone 1 proves the state model. Reviewers should see new publication attempt records round-trip through ledger JSON fixtures, duplicate deterministic appends return an already-recorded result, old projection snapshots still decode with defaults, and projection tests expose attempt history plus latest status by run/publication and by series.

Milestone 2 proves manifest retention. Reviewers should see the existing dry-run publication planner output wrapped in a full publication manifest, written under deterministic immutable artifact-store refs, and linked from compact ledger attempt records without embedding the large manifest body in the ledger. Targeted tests must prove manifest refs, hashes, byte counts, immutable-write idempotency, and conflict handling.

Milestone 3 proves runtime recording. Workflows with configured publication routes record dry-run publication attempts after contract outputs are available, required planning failures produce durable failed publication state without pretending external publication succeeded, optional route failures remain non-blocking warnings, and repeated finalization after a crash does not create duplicate attempts.

Milestone 4 proves operator inspection. `scherzoctl artifact publication list` and `show` read local state, print human-readable summaries, support JSON output, include manifest refs, series ids, latest status, attempt ids, error summaries, `retryable`, and `retry_execution_available: false`, and return clear not-found errors for missing runs or publication ids.

Milestone 5 proves release readiness. New focused tests, schema guardrails, helper/scope inventory, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/artifact-publication-ledger-and-ctl.md`, `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` all pass. Pre-publish manual evidence is limited to seeded or dogfooded local `scherzoctl artifact publication list/show` output; browser checks, live GitHub PR checks, and live provider/cache dogfood are deferred human/operator checks after this ledger-and-inspection slice.

## Progress

- [x] (2026-05-30) Reviewed `docs/specs/ARTIFACT_PUBLICATION_PRD.md` and `docs/specs/WORKFLOW_ARTIFACT_TAXONOMY.md`.
- [x] (2026-05-30) Inspected the current publication planner, workflow contract manifest path, artifact store, state ledger/projection, and `scherzoctl` command structure.
- [x] (2026-05-30) Captured the review-level plan and deferred mechanical implementation detail to the structured implementation pack.
- [x] (2026-05-30) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, local pre-publish CLI dogfood, deferred browser/GitHub/live-provider checks, docs/helper inventory, provider-live/cache non-scope, full validation, and lint gates explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Use the existing Scherzo state ledger for publication summary records and retained artifacts for full manifests. Rationale: this matches the PRD, keeps ledger records bounded, and reuses existing projection and snapshot infrastructure. Date: 2026-05-30.
- Decision: Represent retry information as metadata only in this slice. Rationale: operators need to know whether a failed publication should eventually be retryable, but executing retries belongs to a later workflow. Date: 2026-05-30.
- Decision: Keep remote GitHub mutation out of scope and record dry-run/planning publication attempts honestly. Rationale: the repository already has dry-run planning support, and durable state should be validated before adding external side effects. Date: 2026-05-30.
- Decision: Treat review feedback about evidence, tests, local dogfood timing, helper boundaries, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations. Rationale: Scherzo will materialize follow-up implementation artifacts from the structured pack, so these gates must be mechanically visible and not only implicit in review prose. Date: 2026-05-30.
- Decision: Make seeded or dogfooded local `scherzoctl artifact publication list/show` output a pre-publish check, while deferring browser, live GitHub, and live-provider/cache evidence to a later mutation-capable publication slice. Rationale: this plan adds local ledger state and inspection commands but deliberately avoids remote repository mutation and provider-cache behavior. Date: 2026-05-30.

## Validation and Acceptance

Implementation is accepted only when automated, local manual, helper-inventory, and lint evidence are present. Targeted test evidence must include ledger JSON round trips for new publication attempt records, old snapshot decoding defaults, deterministic ledger idempotency, projection latest-status behavior by run/publication and by series, retained manifest refs with hashes and byte counts, immutable manifest write idempotency and conflict behavior, required versus optional route recording, repeated workflow-finalization dedupe, CLI pretty output, CLI JSON output, and missing-run or missing-publication error paths. Full repository validation must run from the repository root and pass: `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/artifact-publication-ledger-and-ctl.md`. These commands are acceptance evidence, not unchecked Progress TODOs.

Pre-publish local manual evidence must run the new `scherzoctl artifact publication list --run <run-id> --root <workspace-root>` and `scherzoctl artifact publication show --run <run-id> --publication <publication-id> --root <workspace-root>` against seeded or dogfooded local state and capture latest status, attempt count, manifest ref, series id, publication id, `retryable`, and `retry_execution_available: false` in both human-readable and JSON forms. This manual check is a local CLI/operator check before publish. Browser evidence, GitHub PR evidence, live provider evidence, and provider-cache evidence are deferred human/operator checks for later mutation-capable publication work and are not pre-publish blockers for this slice.

Docs/helper evidence must include a scope inventory. If `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, or operator docs helpers changed, the implementation must run the relevant helper or offline contract tests and show that provider-live/cache semantics were preserved. If those surfaces did not change, the evidence must explicitly say no helper migration, provider-live validation, or cache validation was applicable.

## Rollout, Recovery, and Idempotence

Rollout should be a single binary/schema update because new ledger record kinds are not readable by older binaries. Before deployment, keep a copy or archive of `.scherzo-state/ledger` if downgrade recovery is needed. Pre-publish rollout checks require the full validation commands, review-doc validation, helper/scope inventory, and local `scherzoctl artifact publication list/show` evidence; no browser, live GitHub, provider-live, or provider-cache check is required before this non-mutating publication-state slice ships. Repeated workflow finalization must be safe: manifest writes use deterministic immutable refs, ledger records use deterministic ids, and duplicate appends return an already-recorded result instead of creating new attempts. If implementation must be backed out before new records are written, a code revert is sufficient; after new records are written, recover by restoring the pre-rollout ledger archive or staying on binaries that understand the new records. If implementation discovers it must migrate workflow helpers, workflow schemas, provider-facing structured-output helper contracts, review-lane contracts, provider-live behavior, or cache behavior, split or roll back that work before publishing this slice unless the relevant helper or offline contract tests and cache/live-provider evidence are added explicitly.

## Open Questions and Clarifications Needed

No open questions. The implementation should not invent bundle refs, GitHub retry behavior, provider-live/cache behavior, browser checks, or workflow-helper migrations; those remain future work after this ledger and inspection slice is validated.
