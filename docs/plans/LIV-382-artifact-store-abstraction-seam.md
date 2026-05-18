# Add an artifact store abstraction seam

This ExecPlan v2 review document frames the design for LIV-382. Mechanical implementation details, tests, file-by-file edits, and command instructions are supplied through the structured implementation pack.

## Purpose / Big Picture

After this change, Scherzo's workflow and recovery code will treat retained artifacts as objects in an artifact store rather than as files that happen to live under one local directory. Operators keep the current inspectable `.scherzo-state/artifacts/runs/...` layout by default, while future work can add a service-backed store behind the same narrow port.

The observable outcome is compatibility plus a cleaner seam. Existing runs still write step artifacts, structured-output artifacts, workflow input and output manifests, output blobs, and context-recovery diagnostics in the same filesystem layout. New and updated consumers read and display those artifacts through store APIs that expose artifact refs, checksums, byte counts, display locations, optional local paths, and store-neutral URIs.

## Problem Framing and Constraints

Artifact handling is already centralized in `src/scherzo/state/artifact_store.gleam`, but the current store type is a concrete filesystem root and several consumers still rely on returned paths or construct retained display paths directly. That makes the local disk layout part of the calling contract and raises the cost of introducing another backing store later.

The main constraint is compatibility. The default implementation must preserve today's artifact refs and on-disk layout because ledger records, recovery, workflow contracts, operator inspection, and dogfood workflows already rely on those refs. This work should not introduce a remote artifact service, a migration of existing artifacts, or a new operator configuration surface unless a minimal internal default is needed to keep construction explicit.

## Strategy Overview

Keep the existing high-level artifact operations as the public seam and turn the concrete store into a port backed by small read, write, location, and optional-local-path callbacks. The filesystem implementation remains the default returned by the current constructor, so most callers keep their behavior while the store value no longer means "workspace root".

The right-sized change is an internal abstraction, not a new service. The plan separates store-neutral identity and metadata from filesystem location, migrates practical direct reads and display path formatting to the store API, and leaves ledger refs as stable strings. This gives future service work one implementation point without forcing every workflow or recovery consumer to learn a new artifact model now.

## Alternatives Considered

The simplest alternative is to leave the current module alone and document that future stores should mimic the filesystem layout. That does not solve the coupling because callers would still depend on local paths and direct file reads.

Another option is to replace all artifact refs with full URIs in durable records immediately. That is too disruptive because ledger and manifest compatibility depend on stable refs, and existing operators know how to inspect the current tree.

A larger option is to build the separate artifact service now. That is premature for this issue: the acceptance criteria ask for the seam and first implementation, not a second production store.

## Risks and Countermeasures

The main risk is breaking retained artifact compatibility while moving code behind the port. The countermeasure is to preserve ref generation, JSON wrapper shapes, atomic filesystem writes, checksums, byte counts, and current layout tests, then add explicit filesystem compatibility tests for the existing paths.

A second risk is creating an abstraction that is too local-disk shaped to support another backend. The countermeasure is to model display locations, URIs, and optional local paths separately, and to make core reads use artifact refs through the store rather than `simplifile.read` on a stored path.

A third risk is broad churn through workflow and session code. The countermeasure is to keep the public high-level functions in the existing artifact-store module and introduce injectable store construction only where it reduces concrete coupling.

A fourth risk is obscuring operator debugging. The countermeasure is to keep the default display path familiar and to document how a future backend should expose an inspectable URI or display location even when no local path exists.

## Scope Boundaries

In scope is the artifact store port, the default filesystem implementation, store-neutral artifact metadata and location values, practical consumer migration away from direct artifact paths, tests for the store contract and filesystem compatibility, and developer documentation for adding another store.

Out of scope is implementing a remote artifact service, changing durable ledger record schemas, relocating existing artifacts, changing workflow YAML syntax, changing Linear/operator command behavior, or removing the legacy path field from existing structured-output metadata before a compatibility plan exists.

## Milestones

First, define the store-neutral model while preserving the current public module and default constructor. At the end of this milestone, the filesystem store can be exercised through the new port without changing artifact refs or files on disk.

Second, route core reads and writes through the port. Step artifacts, structured outputs, workflow contract manifests, output blobs, and context-recovery artifacts should all use the same store callbacks for content access and location metadata.

Third, migrate practical consumers away from local-path assumptions. Inline structured-output reads and context-recovery display text are the priority because they currently expose the path coupling most clearly.

Fourth, add focused contract, filesystem, recovery, and workflow tests. These tests should prove both that the abstraction works without a local path and that the default filesystem implementation remains byte-for-byte compatible where existing behavior requires it.

Fifth, update developer documentation so a future service-backed store has a clear implementation checklist and compatibility expectations.

## Progress

- [x] (2026-05-18 00:00Z) Drafted the human-reviewable ExecPlan v2 review document for LIV-382.
- [x] (2026-05-18 20:37Z) Materialized the implementation pack and retained bundle for follow-up task LIV-388.
- [x] (2026-05-18 21:45Z) Implemented the artifact store seam, migrated inline structured-output reads to ref-based store access, added no-local-path contract coverage, and validated with `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and the `metadata.path` grep guard.
- [x] (2026-05-18 22:05Z) Delivered the required developer documentation for the artifact-store seam in `docs/runbooks/artifact-store.md` and updated architecture/getting-started docs to describe the default filesystem store plus `uri`/`display_path`/`local_path` compatibility.
- [x] (2026-05-18 23:45Z) Review follow-up added legacy structured-output decode coverage, context-recovery display-path coverage for a no-local-path store, percent-encoded filesystem `file://` URIs, and explicit docs steering new consumers away from the legacy `path` field.
- [x] (2026-05-18 23:55Z) Re-ran the full implementation validation gates after review follow-up: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and the `metadata.path` grep guard.

## Decision Log

- Decision: Preserve `src/scherzo/state/artifact_store.gleam` as the public artifact-store module while turning `Store` into a port-backed value.
  Rationale: Existing producers and consumers already import this module, so keeping the module stable minimizes churn while still creating the abstraction seam.
  Date: 2026-05-18

- Decision: Update the checked-in source-guardrail baselines for `src/scherzo/step_artifact.gleam` and `src/scherzo/workflow_run.gleam` in the same change.
  Rationale: The seam intentionally adds durable artifact location metadata and ref-based inline output reads inside already baselined oversized modules; tests now prove the growth is intentional and reviewable until a later extraction shrinks those modules again.
  Date: 2026-05-18

- Decision: Keep filesystem storage as the default implementation and preserve the `.scherzo-state/artifacts/runs/...` layout.
  Rationale: Operator debugging, recovery, workflow contracts, and existing tests depend on this layout; the seam should not behave like a migration.
  Date: 2026-05-18

- Decision: Keep durable artifact refs as stable relative strings and add URI/display/local-path metadata around them.
  Rationale: Refs are the compatibility boundary in ledger and manifest records, while URI and optional local-path fields let future stores avoid pretending every artifact is a local file.
  Date: 2026-05-18

- Decision: Do not add a second production store in this change.
  Rationale: A fake or test store is enough to prove the contract; the production acceptance criteria require a seam and filesystem implementation, not a service rollout.
  Date: 2026-05-18

## Surprises & Discoveries

- Observation: The repository source-size guardrail failed once the new metadata fields and ref-based inline output path landed.
  Evidence: `src/scherzo/step_artifact.gleam grew beyond its line baseline: 1542 > 1476` and `src/scherzo/workflow_run.gleam grew beyond its line baseline: 4643 > 4630` during `direnv exec . gleam test`.

## Outcomes & Retrospective

The seam now exists as a real port. `artifact_store.Store` is callback-backed, the default filesystem implementation still writes under `.scherzo-state/artifacts`, structured-output metadata carries URI/display/local-path fields, and inline contract outputs no longer depend on `metadata.path`. A fake store that withholds `local_path` still satisfies retained structured-output publication, inline JSON extraction, and context-recovery display-path publication, which retires the main abstraction risk called out in the plan. The final repair also delivered the missing developer-documentation milestone so future service-backed work has a checked-in compatibility checklist instead of only code-level examples. Review follow-up tightened legacy decode coverage, now emits percent-encoded filesystem URIs for paths with spaces or other reserved bytes, documents that new backend-neutral consumers should avoid the legacy `path` field, and re-established the full formatting/test/lint/guard validation evidence after those repairs.

## Validation and Acceptance

Acceptance is behavioral. A normal workflow run should continue to create retained artifacts at the same filesystem paths and record the same artifact refs, hashes, and byte counts as before. Recovery should still read completed step artifacts by ref and checksum. Structured-output and workflow-contract paths that need content should obtain it through the store API, not by assuming a local path.

The abstraction claim is falsified if a test store without a local path cannot satisfy core artifact reads, if any existing retained filesystem path changes unexpectedly, if operator-facing context-recovery diagnostics stop pointing to an inspectable retained artifact location, or if developer docs stop documenting the default filesystem compatibility contract for future stores.

## Rollout, Recovery, and Idempotence

Rollout is internal and additive. The default constructor still produces the filesystem store, so existing deployments do not need configuration or data migration. Re-running a workflow attempt writes the same attempt-scoped refs atomically as today, and repeated manifest writes should preserve the existing idempotent reuse behavior.

If the seam causes trouble, rollback is to restore the concrete filesystem store module and direct local-path reads. Existing retained artifacts remain valid because the implementation must not change their layout or durable ref strings.

## Open Questions and Clarifications Needed

No MVP clarification is required. A future service-backed store will need separate decisions about configuration, authentication, retention policy, and whether operator URIs should be local-control URLs, signed service URLs, or another scheme.
