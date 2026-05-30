# Artifact publication planner and dry-run manifest

This review document frames the implementation slice for planning artifact publication from retained workflow outputs. The implementation details, concrete edits, interfaces, tests, and artifact notes are carried in the structured implementation pack for this task.

## Purpose / Big Picture

After this change, Scherzo can take an opted-in workflow publication route and produce a deterministic dry-run publication manifest from canonical retained artifacts. Operators and future repository adapters will be able to see which output artifacts would be copied, where they would land, which branch and pull-request text would be used, and which stable `series_id` and content-derived `version_id` identify the planned publication, all without creating a branch, commit, or pull request.

## Problem Framing and Constraints

`docs/specs/ARTIFACT_PUBLICATION_PRD.md` makes Scherzo's internal artifact store the canonical source and treats external repositories as derived copies. The repository already has typed publication configuration in `src/scherzo/artifact_publication_config.gleam`, output manifests in `src/scherzo/workflow_contract_manifest.gleam`, generic descriptors in `src/scherzo/workflow_artifact_descriptor.gleam`, backend-neutral artifact reads in `src/scherzo/state/artifact_store.gleam`, and the current ExecPlan workflow publishing shape in `workflows/dogfood/execplan.yaml`, which still requires `publish-change` and publishes the review doc through `publish_review_doc`. The missing middle layer is a planner that resolves configured selectors against retained output descriptors, verifies selected bytes by artifact-store ref, renders deterministic targets, and emits a manifest that a later GitHub publisher can consume. This slice must stay side-effect-free with respect to GitHub and must not migrate `workflows/dogfood/execplan.yaml` or any installed `.scherzo/workflows` equivalent away from its current `publish-change`-based review-doc publishing path.

## Strategy Overview

The right-sized strategy is a pure planning layer plus a dry-run manifest codec. The planner should accept a workflow output manifest, publication routes, repository targets, work/run context, and body-template contents; select only explicit output names and optional artifact-set entry names; read selected file bytes through the artifact-store API; verify hashes and byte counts; render destination paths, branch names, PR titles, and PR bodies; validate rendered destinations; compute stable identifiers; and return a manifest with `dry_run: true` and no backend mutation fields. This provides the hard artifact-selection and idempotency behavior before any GitHub adapter or durable publication state is introduced.

## Alternatives Considered

One alternative is to implement the GitHub branch and pull-request backend now. That is too broad because it combines selector semantics, artifact-store reads, template rendering, durable publication state, retry behavior, local git checkout management, and remote mutation in one change. A second alternative is to let the future GitHub backend resolve selectors directly. That would bury deterministic planning and error handling inside a mutating adapter, making dry-run validation and retry safety harder. A third alternative is to select artifacts by `artifact_type`, media type, or metadata tags; the PRD rejects that for MVP because those fields are not guaranteed unique, so this plan uses only output name plus optional artifact-set entry name.

## Risks and Countermeasures

The main risk is accidentally reading local workspace paths instead of canonical artifact-store refs. The countermeasure is to plan only from output descriptors and use `artifact_store.read_artifact_bytes_unverified` or equivalent store callbacks, with tests using a custom store that has no local path. A second risk is selector ambiguity or unsafe rendered destinations; the countermeasure is exact output/entry matching, file-kind enforcement, hash and byte verification, duplicate rendered-path rejection, and post-render path validation. A third risk is a circular `version_id` when templates reference `{{ publication.version_id }}`; the countermeasure is a two-phase identity algorithm that computes `version_id` from selected artifact metadata, repository target, target templates, and the non-version render context before rendering final templates. A fourth risk is scope creep into GitHub mutation; the countermeasure is automated tests and pre-publish diff evidence showing no `git push`, `gh`, GitHub API mutation, publication ledger state, retry command, local publication checkout, provider-live/cache behavior, workflow helper/schema migration, or ExecPlan workflow migration was added. A review-specific risk is letting acceptance evidence, negative and idempotency tests, manual/dogfood timing, docs/helper boundaries, provider-live/cache non-scope, full validation, or lint gates live only in prose; the countermeasure is to mirror those obligations in the structured implementation pack before Scherzo materializes the implementation artifacts.

## Scope Boundaries

In scope are dry-run publication planning, output and artifact-set entry selector resolution, canonical artifact-store byte reads and verification, rendered destination-path and branch validation, template rendering for paths, branches, PR titles, and PR bodies, deterministic `series_id` and `version_id` computation, duplicate destination-path detection, dry-run manifest JSON, targeted tests, a small documentation note for the dry-run manifest shape, and a docs/helper inventory that records whether any helper or schema surface changed. Out of scope are GitHub branch creation, commits, pushes, pull-request creation or updates, local publication checkouts, durable publication attempt ledger records, operator `publication list/show/retry` commands, retry scheduling, generalized review state, non-GitHub repository backends, selecting by metadata or artifact type, provider-live or provider-cache behavior, workflow helper migrations under `.scherzo/workflows/scripts/*`, workflow schema migrations, provider-facing structured-output helper changes, review-lane contract changes, and migration of `workflows/dogfood/execplan.yaml` or installed `.scherzo/workflows` equivalents from their current publishing steps.

## Milestones

Milestone 1 establishes the planner's pure data model and dry-run manifest shape. At the end, a fixed publication route and retained output manifest can produce canonical JSON containing selected repository metadata, `dry_run: true`, a PRD-shaped `series_id`, and a stable content-derived `version_id` without any remote fields such as PR URL, commit SHA, or pushed branch result.

Milestone 2 proves selector resolution and canonical artifact reads. At the end, tests cover selecting a leaf file output, selecting a named file entry from an artifact-set descriptor, rejecting unknown outputs and entries, rejecting non-file selections, reading bytes only by artifact-store ref, and failing closed on missing artifacts or hash/byte mismatches.

Milestone 3 proves target rendering and safety. At the end, destination paths, branch names, PR titles, and PR bodies render from the allowed publication and artifact variables; PR body rendering receives `publication.files_markdown`; rendered paths and branches are validated after substitution; duplicate destination paths are rejected; and templates that require unavailable variables fail before any mutation-capable code can run.

Milestone 4 closes documentation, helper inventory, and validation. At the end, the dry-run manifest shape is documented, targeted planner tests and full repository validation pass, `glinter` and `scherzo_lint` are green or retain only pre-existing unrelated warnings, and pre-publish evidence includes both focused test output and `git diff --stat`/focused diffs demonstrating that this slice added no GitHub mutation, no publication ledger/retry surface, no provider-live/cache behavior, no workflow helper/schema migration, and no workflow migration from `publish-change`.

## Progress

- [x] (2026-05-30) Reviewed the artifact publication PRD, workflow artifact taxonomy, existing artifact publication config model, output manifest code, generic descriptor code, artifact-store API, and current ExecPlan workflow publishing shape.
- [x] (2026-05-30) Drafted this review document and separated mechanical implementation detail into the structured implementation pack for handoff.
- [x] (2026-05-30) Incorporated review feedback by making acceptance evidence, negative and idempotency tests, milestone-specific proof, pre-publish manual diff audit, deferred live dogfood, docs/helper inventory, provider-live/cache non-scope, full validation, and lint gates explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Implement a dry-run planner before any GitHub adapter.
  Rationale: Selector resolution, artifact verification, template rendering, and idempotent identity are independently testable and should be proven before remote mutation exists.
  Date: 2026-05-30.
- Decision: MVP selection is exact output name plus optional artifact-set entry name only.
  Rationale: The PRD explicitly defers selection by `artifact_type`, `media_type`, or metadata because those fields may be non-unique.
  Date: 2026-05-30.
- Decision: Compute `version_id` before final template rendering from selected artifact metadata, repository target, target template strings, and the non-version render context.
  Rationale: This avoids circular rendering when templates reference `{{ publication.version_id }}` while still making artifact bytes and target mappings drive idempotency.
  Date: 2026-05-30.
- Decision: Keep workflow migration and durable publication state out of this slice.
  Rationale: The task asks for planning and dry-run manifest behavior; retries, ledger projections, and GitHub mutation need a later plan once the manifest contract is stable.
  Date: 2026-05-30.
- Decision: Treat review feedback about evidence, tests, milestone specificity, manual/dogfood timing, docs/helper migration, provider-live/cache behavior, full validation, and linting as implementation-pack obligations.
  Rationale: The implementation handoff must make those gates mechanically visible, not merely implicit in the review prose.
  Date: 2026-05-30.
- Decision: Limit documentation/helper work to the dry-run manifest note plus an explicit helper inventory.
  Rationale: The planner slice should not migrate workflow scripts, schemas, provider-facing structured-output helpers, review-lane contracts, provider-live behavior, or cache behavior unless that work is split into a later implementation plan.
  Date: 2026-05-30.

## Validation and Acceptance

Acceptance requires automated evidence from `direnv exec . gleam test test/artifact_publication_planner_test.gleam` proving leaf output selection, artifact-set entry selection, canonical artifact-store byte verification, rendered path and branch validation, PR title and body rendering, `publication.files_markdown`, deterministic `series_id` and `version_id`, duplicate destination-path rejection, and dry-run JSON output with no PR URL, commit SHA, push result, or mutation status. Negative-path acceptance requires tests for unknown output, absent output, entry on a non-aggregate output, missing artifact-set entry, selected non-file descriptor, missing artifact ref, hash mismatch, byte-count mismatch, unsafe rendered path, unsafe branch, duplicate rendered destination path, missing body template, and unavailable template variable. Idempotency acceptance requires tests or deterministic fixture comparisons showing that unchanged selected bytes, templates, repository target, and render context keep the same `version_id`, while changed bytes or target mapping change it.

Full validation is accepted only after `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` pass from the repository root, with evidence retained in the implementation handoff rather than left as unchecked Progress work. Documentation acceptance requires a checked-in note in `docs/specs/ARTIFACT_PUBLICATION_PRD.md` or the adjacent YAML spec describing the dry-run manifest shape and stating that GitHub mutation remains deferred. Docs/helper acceptance also requires an explicit inventory: if `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files changed, the implementation must run the relevant helper or contract tests and preserve provider-live/cache semantics; if they did not change, it must record that no helper migration, provider-live validation, or cache validation was applicable.

The pre-publish manual check is a local diff audit, not a live dogfood run: run `git diff --stat` and inspect focused diffs showing no `git push`, `gh`, GitHub API mutation path, publication ledger state, retry command, local publication checkout, provider-live/cache behavior, helper/schema migration, or `workflows/dogfood/execplan.yaml`/installed `.scherzo/workflows` migration was added. No browser, live GitHub, or live operator dogfood run is required before publish; any live operator dogfood of a later mutation-capable publisher is deferred to a future implementation slice.

## Rollout, Recovery, and Idempotence

Rollout is additive because the planner can be introduced as an internal module and tested without altering workflow completion or external repositories. If the planner is wrong, reverting the new module, tests, and documentation returns the system to the existing config-only publication behavior. Re-running the planner with the same output manifest, selected artifact bytes, repository target, templates, and relevant render context must produce the same `series_id`, `version_id`, rendered targets, and dry-run manifest; changing artifact bytes, destination templates, repository target, rendered branch inputs, or selected files must change the `version_id`. Failed planning leaves no remote state and no durable publication attempt state, so recovery is correcting configuration or retained artifacts and re-running the planner. If implementation discovers that workflow helpers, schemas, provider-facing structured-output helpers, review-lane contracts, provider-live behavior, or cache behavior must change, that work should be split or explicitly rolled back before publishing this planner slice; otherwise the helper inventory should state that those surfaces were untouched and no cache invalidation or live-provider cleanup is needed.

## Open Questions and Clarifications Needed

No blocking clarification is needed. A future implementation slice must decide how the dry-run manifest is queued for a GitHub publisher, where publication attempt ledger events live, and which operator commands expose retry history.
