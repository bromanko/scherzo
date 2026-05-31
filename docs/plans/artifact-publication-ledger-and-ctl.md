# Artifact publication ledger and operator inspection

This review document frames the implementation slice for durable artifact publication state and operator inspection. Mechanical implementation details, exact steps, tests, interfaces, and artifact notes are carried in the structured implementation pack for this task.

## Purpose / Big Picture

After this change, Scherzo operators can answer what happened to a workflow artifact publication without reading raw JSONL state or rerunning a workflow. Publication attempts are recorded in the existing Scherzo state ledger, full publication manifests are retained in the artifact store, and `scherzoctl artifact publication list --run <run-id>` plus `scherzoctl artifact publication show --run <run-id> --publication <publication-id>` expose latest status, retained manifest refs, selected files, external refs when present, diagnostics, and whether a failed attempt is eligible for a future retry without rerunning artifact-producing steps.

## Problem Framing and Constraints

The artifact publication PRD makes Scherzo's internal artifact store canonical and treats external repositories as derived copies. The repository already has typed publication configuration and a dry-run publication planner, but there is no durable publication-attempt state and no operator view over publication status. The implementation must use the existing local state ledger rather than a new storage system, keep ledger records small by storing full manifests as artifacts, project latest state by workflow run, publication id, and publication series, and preserve existing workflows that still publish through `publish-change`. This slice must not implement GitHub mutation or full retry execution; retryability is informational only.

## Strategy Overview

The right-sized strategy is an additive state-and-inspection layer. Add explicit artifact publication attempt records to the existing ledger, with deterministic record ids and bounded summary fields. Write each full publication manifest to the artifact store under a deterministic attempt ref, and put only the ref, hash, byte count, status, and bounded diagnostics in ledger records. Extend the state projection so callers can ask for latest publications for a run, a specific run/publication pair, and the latest known attempt for a publication series. Then add `scherzoctl artifact publication list` and `show` commands that read the projected local state, load retained manifest details for `show`, and present retryability as data while clearly stating that retry execution is not implemented in this slice.

## Alternatives Considered

A separate publication ledger was rejected because the PRD explicitly prefers the existing Scherzo state ledger for summary events and because a second ledger would complicate crash recovery and operator inspection. Storing full publication manifests directly in ledger records was rejected because manifests can contain selected artifact descriptors, rendered PR text, backend details, and diagnostics that would bloat JSONL state and snapshots. Implementing GitHub publication and retry now was rejected as too broad: remote mutation, checkout management, idempotent retry execution, and branch/PR conflict handling should build on this durable state contract after it is observable and tested.

## Risks and Countermeasures

The main risk is corrupting or bloating local state. The countermeasure is small versioned ledger records, retained manifest refs with sha256 and byte counts, legacy snapshot decoding with empty publication indexes, and full replay tests. A second risk is duplicate or conflicting attempt writes during crash recovery; deterministic attempt ids, immutable manifest refs, and idempotent ledger append tests catch that early. A third risk is misleading operators into thinking retry execution exists; the CLI must print retryability separately from retry execution and must not add a working retry command. A fourth risk is projection mistakes across reruns of the same publication series; tests must cover failed, unchanged, published, optional failure, and superseded/latest-series behavior. A fifth risk is scope creep into GitHub, workflow migration, helper migrations, or provider-live/cache behavior; pre-publish diff evidence must show those surfaces are untouched unless explicitly split and validated. A sixth risk is letting review feedback live only in this prose document while the structured implementation pack omits acceptance evidence, manual-check timing, docs/helper inventory, provider-live/cache non-scope, full validation, or linting; this revision treats those items as pack obligations too.

## Scope Boundaries

In scope are publication attempt ledger record types, immutable retained publication manifest writes, projection of latest status by run, run/publication id, and publication series, bounded diagnostics and retryability fields, pretty and JSON operator inspection for `artifact publication list` and `artifact publication show`, tests for idempotency, projection, and CLI output, and documentation of the operator surface in `docs/runbooks/artifact-store.md` or a closely related operator runbook. Out of scope are creating branches, committing files, pushing to GitHub, opening or updating pull requests, executing retries, adding `artifact publication retry`, changing `workflows/dogfood/execplan.yaml` away from its current publishing path, generalized review/approval state, non-GitHub repository adapters, provider-live/cache behavior, and workflow helper or schema migrations not directly needed by this state-and-inspection slice. If implementation discovers that a workflow helper, provider-facing schema, provider-live probe, or cache path must change, that work must be split or explicitly rolled back before this slice publishes.

## Milestones

Milestone 1 establishes durable attempt records and manifest retention. At the end, code can write a publication attempt manifest artifact for a run/publication attempt, append idempotent started/completed/failed ledger records that reference that manifest, reject conflicting duplicate ids, and replay those records from the existing ledger. The proof for this milestone is a focused ledger test file such as `test/artifact_publication_ledger_test.gleam` covering identical duplicate writes, conflicting duplicate record ids, immutable manifest ref byte conflicts, bounded diagnostics, and replay alongside unrelated existing record kinds.

Milestone 2 establishes publication projections. At the end, the state projection exposes latest publication summaries for a run, the latest summary for one run/publication id, and the latest summary for a publication series, including retryability and manifest refs while keeping legacy ledgers and snapshots valid. The proof for this milestone is a focused projection test file such as `test/artifact_publication_projection_test.gleam` covering failed, published, unchanged, optional-failed, and superseded/latest-series cases plus legacy snapshot decoding with empty publication indexes.

Milestone 3 adds operator inspection. At the end, `scherzoctl artifact publication list --run <run-id>` shows one row per latest publication for that run, `scherzoctl artifact publication show --run <run-id> --publication <publication-id>` shows detailed status and manifest information, and `--json` output contains stable machine-readable fields. The proof for this milestone is `test/ctl_test.gleam` or equivalent command tests for parsing, pretty rendering, JSON rendering, missing arguments, unknown run/publication errors, and explicit display that retry execution is unavailable in this slice.

Milestone 4 closes documentation, scope inventory, validation, and rollout evidence. At the end, operator documentation explains the new inspection commands, manual pre-publish fixture-ledger examples show list and show output, full test/format/lint gates pass, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/artifact-publication-ledger-and-ctl.md` passes, and a pre-publish diff audit proves the slice is additive, idempotent, inspectable, and free of GitHub mutation, retry execution, workflow helper migration, provider-live behavior, or cache behavior.

## Progress

- [x] (2026-05-30) Reviewed the artifact publication PRD, workflow artifact taxonomy, existing publication configuration plan, dry-run planner plan and code, state ledger/projection modules, artifact store API, workflow output recording path, and `scherzoctl` command structure.
- [x] (2026-05-30) Drafted this review document and separated the mechanical implementation handoff into the structured implementation pack.
- [x] (2026-05-31) Incorporated review feedback by making acceptance evidence, targeted test obligations, milestone-specific proofs, pre-publish manual fixture CLI evidence, deferred operator dogfood timing, docs/helper and provider-live/cache boundaries, full validation, and lint gates explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Use the existing Scherzo state ledger for publication summary records.
  Rationale: This follows the PRD, keeps crash recovery and operator inspection on one replay path, and avoids introducing a second durable state system.
  Date: 2026-05-30.
- Decision: Retain full publication manifests as artifact-store payloads and reference them from ledger records.
  Rationale: Operators need full details, but ledger records and projection snapshots must remain small and replayable.
  Date: 2026-05-30.
- Decision: Add `list` and `show` inspection before retry execution.
  Rationale: Retry needs trustworthy durable state first; exposing retryability now helps operators triage failures without pretending that retry mutation exists.
  Date: 2026-05-30.
- Decision: Keep GitHub mutation and ExecPlan workflow migration out of this slice.
  Rationale: Remote publication and workflow migration are larger follow-up changes that should consume the ledger contract rather than be combined with it.
  Date: 2026-05-30.
- Decision: Treat review feedback about evidence, tests, milestone specificity, manual/dogfood timing, docs/helper boundaries, provider-live/cache boundaries, full validation, and linting as structured implementation-pack obligations.
  Rationale: Scherzo materializes the follow-up implementation plan from the structured pack, so prose-only requirements would be easy for later implementers to miss.
  Date: 2026-05-31.
- Decision: Require fixture-based CLI inspection examples before publish and defer real retained-run operator dogfood until after implementation.
  Rationale: This slice can prove behavior deterministically with local fixture ledgers, while a live retained-run check depends on operator timing and should not block the code publish path.
  Date: 2026-05-31.

## Validation and Acceptance

Ledger idempotency is accepted when `direnv exec . gleam test test/artifact_publication_ledger_test.gleam` passes tests that write the same manifest and attempt records twice with identical bodies, detect a duplicate record id with different body content, detect an immutable manifest ref conflict with different bytes, bound diagnostics, and replay publication records through the existing ledger without corrupting non-publication state. Projection behavior is accepted when `direnv exec . gleam test test/artifact_publication_projection_test.gleam` or equivalent focused projection tests pass cases for latest status by run, latest status by run/publication id, latest status by series across reruns, failed-to-published or failed-to-unchanged recovery, optional failed publications, retryability fields, bounded diagnostics, and legacy snapshot decoding with no publication field.

CLI output is accepted when `direnv exec . gleam test test/ctl_test.gleam` passes parse and rendering tests for `artifact publication list --run run-1`, `artifact publication show --run run-1 --publication review_doc`, missing `--run`, missing `--publication`, unknown run, unknown publication, `--json` list output containing a `publications` array, and `--json` show output containing `run_id`, `publication_id`, `status`, `series_id`, `version_id`, `manifest_ref`, and `retryable`. Manual pre-publish evidence must include example pretty output for list and show from a fixture ledger and must explicitly show retry execution as unavailable or not implemented. No browser check is relevant because this slice adds no browser UI. A real retained-run dogfood check is deferred: after implementation, an operator may run `scherzoctl artifact publication list --run <real-run-id>` and `scherzoctl artifact publication show --run <real-run-id> --publication <publication-id>` against a retained run, but that live check is not a pre-publish requirement.

Documentation and helper-boundary acceptance requires an operator-facing doc update for the new commands and a helper/cache inventory. If `.scherzo/workflows/scripts/*`, `workflows/dogfood/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, or cache behavior are unchanged, the final evidence must say no helper migration, provider-live validation, or cache validation was applicable. If any such surface is changed despite the intended scope, the implementation must either split or roll back that change before publishing, or add explicit helper/contract and provider-live/cache stale-read, invalidation, and TTL-disabling validation before acceptance.

Full validation is accepted only after `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/artifact-publication-ledger-and-ctl.md` pass from the repository root. Pre-publish diff evidence must show no `git push`, `gh`, GitHub API mutation, publication retry execution, `artifact publication retry` command, provider-live/cache behavior, workflow helper/schema migration, or `workflows/dogfood/execplan.yaml` publishing migration was introduced in this slice.

## Rollout, Recovery, and Idempotence

Rollout is additive: existing ledgers without publication records project to empty publication indexes, existing workflows continue to run without publication routes, and existing `publish-change` behavior is unchanged. Recovery from a bad implementation is a revert of new record/projection/CLI/test/docs changes because no remote systems are mutated. Re-running the publication recording path with the same attempt id, manifest bytes, and ledger body must be idempotent; reusing the same attempt id with different manifest bytes or a different record body must fail loudly. Failed publication records remain inspectable and can advertise future retry eligibility, but no retry work is queued or executed by this slice. If helper, provider-live, or cache behavior changes appear during review, the safe recovery is to revert or split those changes before publish; the default rollout contains only local ledger, local artifact-store, local CLI, tests, and operator-doc changes.

## Open Questions and Clarifications Needed

No blocking open questions. Future implementation slices still need to choose the GitHub mutation adapter, durable retry scheduling command, and exact migration timing for ExecPlan workflows from `publish-change` to `artifacts.publications`.
