# Design v2 ExecPlan Bundle Workflows

This is a living ExecPlan. Keep `## Progress`, `## Surprises & Discoveries`, `## Decision Log`, and `## Outcomes & Retrospective` current as implementation proceeds. The plan is self-contained and uses repository-relative paths only.

## Purpose / Big Picture

Scherzo operators currently get one large checked-in ExecPlan file from `workflow:execplan`. That file is useful to agents because it contains repo context, mechanical implementation steps, validation details, and handoff notes, but it is noisy for humans reviewing the plan PR. It also makes future implementation workflows depend on a large source document instead of a small, tamper-evident artifact contract.

The goal is to introduce an experimental v2 ExecPlan family that keeps the current v1 dogfood workflows intact while proving a cleaner split:

- a checked-in Markdown review document under `docs/plans/` that is concise enough for human review; and
- retained JSON artifacts that carry the detailed implementation pack and the bundle metadata future workflows need.

The resulting operator experience should be: label a planning issue with `workflow:execplan-v2`, review a focused PR containing only the human-oriented plan, and let the follow-up `workflow:execplan-implementation-v2` issue consume an `exec_plan_bundle` artifact that verifies the review doc and implementation pack hashes before code changes begin.

## Problem Framing and Constraints

The problem is real now because current ExecPlan PRs are intentionally exhaustive. That exhaustiveness is valuable to agents, but it forces reviewers to scan implementation mechanics that do not need to be checked in. It also hides the true workflow contract: future automation needs stable artifact references, hashes, provenance, and validation status, not a prose convention embedded in a plan body.

Hard constraints for this implementation:

- Do not mutate the current production behaviors of `workflow:execplan`, `workflow:execplan-revision`, or `workflow:execplan-implementation`.
- Keep existing labels and workflows available for normal dogfood use.
- Add experimental labels and workflow files:
  - `workflow:execplan-v2`
  - `workflow:execplan-revision-v2`
  - `workflow:execplan-implementation-v2`
  - `.scherzo/workflows/execplan-v2.yaml`
  - `.scherzo/workflows/execplan-revision-v2.yaml`
  - `.scherzo/workflows/execplan-implementation-v2.yaml`
- Retained artifacts must be addressable by repository-relative artifact refs and tamper-evident by SHA-256 hashes.
- No canonical artifact may contain a SHA-256 of its own exact bytes. Self-hashes live outside the hashed object in `outputs.v1.json`, helper diagnostics, and Linear handoff text.
- Model final responses are not authoritative outputs. Deterministic helper commands must materialize canonical artifacts and workflow contract output manifests.
- All stored paths inside bundle artifacts must be repository-relative or Scherzo artifact refs such as `runs/<run-id>/outputs/exec_plan_bundle.json`.
- The implementation must preserve the current `.scherzo/scherzo.yaml` routing invariant that active Linear issues need exactly one configured `workflow:*` label.

## Strategy Overview

Build the v2 family as a parallel artifact-contract layer on top of Scherzo's existing workflow runtime, structured-output support, and local artifact store.

The design has four parts:

1. **Schemas.** Add JSON schemas for the durable v2 artifacts under `docs/schemas/workflows/`:
   - `docs/schemas/workflows/exec-plan-bundle.v2.schema.json`
   - `docs/schemas/workflows/implementation-pack.v2.schema.json`
   - `docs/schemas/workflows/code-change-bundle.v2.schema.json`

   Add provider submission schemas under `docs/schemas/provider/` only for model-authored drafts that helper commands canonicalize:
   - `docs/schemas/provider/implementation-pack-submission.v2.schema.json`
   - `docs/schemas/provider/exec-plan-revision-submission.v2.schema.json`

2. **Contract types and retention.** Extend workflow contract parsing with explicit JSON contract types `exec_plan_bundle`, `implementation_pack`, and `code_change_bundle`. They should retain as `.json` output blobs in `.scherzo-state/artifacts/runs/<run-id>/outputs/` and appear in `outputs.v1.json` with `ref_kind: run_artifact`, `sha256`, `bytes`, and `media_type: application/json`.

3. **Deterministic helpers.** Add a single script, `scripts/scherzo-execplan-v2`, with subcommands that validate review-doc shape, discover dynamic workflow artifacts, canonicalize implementation-pack submissions, publish the concise review-doc PR, write a publish-context JSON file, validate bundle/pack consistency, create or reuse the implementation handoff issue, prepare a bundle for implementation, and materialize the final `code_change_bundle`. Existing scripts stay unchanged except for shared utility extraction if the implementation needs a small reusable helper.

4. **Experimental workflows and prompts.** Add three v2 workflow YAML files plus v2 prompts. The v2 drafting workflow produces two retained outputs: `implementation_pack` and `exec_plan_bundle`. The v2 revision workflow transforms a previous bundle plus feedback into a superseding bundle. The v2 implementation workflow consumes a bundle, validates hashes up front, reads the implementation pack as primary mechanical input, treats the checked-in review doc as authoritative for intent and scope, and emits a retained `code_change_bundle` after publication.

## Alternatives Considered

- **Keep one checked-in full ExecPlan and add metadata sidecars.** Rejected because it leaves PR review noisy and does not solve the core human/agent separation.
- **Check in both the review doc and implementation pack.** Rejected for the experiment because the implementation pack is intentionally mechanical and would still make PRs noisy. It also increases the temptation to review generated execution detail as source material.
- **Use the model's final response as the bundle.** Rejected because final responses are presentation artifacts. They can be truncated, reformatted, or mixed with conversational text. The source of truth must be helper-materialized JSON retained in Scherzo's artifact store.
- **Replace v1 workflows in place.** Rejected because current dogfood depends on the existing contracts. Parallel v2 workflows let operators fall back immediately.
- **Use generic `code_change` for all JSON outputs.** Rejected for clarity. Dedicated contract types make manifests and tests self-documenting and prevent bundle artifacts from being confused with implementation PR outputs.

## Risks and Countermeasures

- **Risk: v2 labels accidentally disrupt current dogfood routing.** Countermeasure: only add new route keys and new completion-state entries; do not alter existing route keys. Add routing tests or config assertions that the existing v1 workflow mappings still point to the same files.
- **Risk: bundle and implementation pack drift apart.** Countermeasure: require `implementation_pack.derived_from.review_doc_sha256` to equal `exec_plan_bundle.review_doc.sha256`, and make `scripts/scherzo-execplan-v2 validate-bundle` fail with a specific stale-pack failure code when they differ.
- **Risk: implementation starts from an unmerged or stale review doc.** Countermeasure: `implementation-prepare` checks that `review_doc.path` exists in the implementation workspace and that its bytes hash to the bundle's `review_doc.sha256`; otherwise it fails before any agent step.
- **Risk: model-authored pack content contains unsafe local paths or unverifiable claims.** Countermeasure: provider schema and helper validation reject absolute local path shapes, require evidence fields for verified facts, and keep canonical helper artifacts as the only workflow outputs.
- **Risk: revision feedback updates only the review doc and forgets the pack.** Countermeasure: revision materialization validates pack derivation every time. If review-doc hash changes, the old pack cannot be carried forward unless the revision is explicitly `unchanged` and the review-doc hash is unchanged.
- **Risk: follow-up implementation issue cannot find the retained bundle.** Countermeasure: handoff issue body must include `Bundle ref`, `Bundle sha256`, `Review doc`, and `Implementation pack sha256` lines generated by the helper, not written by the agent. `implementation-prepare` must resolve refs through an explicit artifact-root order, print the roots it tried without secrets, and fail closed if the source run artifacts have been pruned. V2 is supported only while the source Scherzo artifact store is retained; recovery is to rerun or revise the planning workflow, not to reconstruct bundle JSON by hand.
- **Risk: semantic conflict between review doc and implementation pack.** Countermeasure: the implementation prompt instructs the agent to write `tmp/execplan-v2-conflict.md` and stop when intent, scope, acceptance, or safety disagree. A command gate fails if that file exists, routing the issue to revision rather than letting the implementation silently choose one source.

## Progress

- [x] 2026-05-15: Inspected current v1 workflow YAML files and confirmed `execplan`, `execplan-revision`, and `execplan-implementation` are separate workflows that can be paralleled without editing their existing files.
- [x] 2026-05-15: Confirmed `.scherzo/scherzo.yaml` maps workflow labels to workflow files and requires exactly one workflow label.
- [x] 2026-05-15: Confirmed Scherzo already records workflow contract input/output manifests under `.scherzo-state/artifacts/runs/<run-id>/` and can retain structured JSON outputs with refs and SHA-256 hashes.
- [x] 2026-05-15: Incorporated review feedback: removed the self-referential bundle hash, specified v2 publication, dynamic artifact discovery, retained-ref lookup, unchanged-revision review surface semantics, and code-change artifact retention.
- [ ] Add v2 schemas and fixtures.
- [ ] Add v2 helper commands and tests.
- [ ] Add v2 contract types and manifest retention tests.
- [ ] Add v2 workflow YAML files and prompts.
- [ ] Dogfood the v2 drafting and implementation flow on one small issue.

## Surprises & Discoveries

- Scherzo already has a workflow contract manifest layer with retained run artifact refs, output blobs, inline JSON, and mapped inputs. The v2 bundle design should reuse that layer instead of inventing a separate artifact registry.
- Existing native review workflows already use JSON-schema-validated structured agent submissions through `structured_output`, `parameters_schema_path`, and retained structured artifacts. The v2 implementation pack can follow the same pattern while still using helper-materialized output as the final source of truth.
- The current implementation helper already has strong plan-path extraction and plan-brief mechanics for v1. V2 should not overload those semantics; it should add a separate bundle preparation path.
- A bundle cannot store the SHA-256 of its own canonical bytes without defining a separate hash domain. This plan keeps the bundle's final hash outside the bundle artifact.

## Decision Log

- 2026-05-15: Use new experimental labels and workflow files rather than changing v1 workflows in place, preserving current dogfood behavior.
- 2026-05-15: Store v2 canonical artifacts as JSON with schema version `2` and artifact types `exec_plan_bundle`, `implementation_pack`, and `code_change_bundle`.
- 2026-05-15: Use `docs/schemas/workflows/` for canonical workflow artifact schemas and `docs/schemas/provider/` for model submission schemas.
- 2026-05-15: Make `exec_plan_bundle` the handoff unit. The bundle references the checked-in review doc by path/hash and the retained implementation pack by artifact ref/hash.
- 2026-05-15: Include dedicated workflow contract types for v2 artifacts rather than reusing `code_change` for non-code-change JSON.
- 2026-05-15: Do not store `exec_plan_bundle`'s own SHA-256 inside `exec_plan_bundle`; record the final bundle hash in `outputs.v1.json`, command diagnostics, and the Linear handoff issue.
- 2026-05-15: Add an explicit v2 `publish-review-doc` helper command and `tmp/scherzo-execplan-v2-publish-context.json` contract instead of reusing the v1 publish path.
- 2026-05-15: Resolve bundle and pack refs through retained Scherzo artifact roots only; if those artifacts are pruned, v2 implementation must fail and rerun planning or revision.
- 2026-05-15: Represent unchanged revision publication with `review_surface.status: "reused"` carrying the previous PR metadata, avoiding no-op PR creation.
- 2026-05-15: Make `materialize-code-change-bundle` copy diff, validation, plan-completion, and review artifacts into the run artifact store before referencing them from the bundle.

## Outcomes & Retrospective

Not started. Fill this in after the v2 workflows have been implemented and at least one dogfood run has completed.

## Context and Orientation

Scherzo is a workflow runner for Linear issues. The repository stores workflow configuration under `.scherzo/`, helper scripts under `scripts/`, Gleam source under `src/`, and tests under `test/`.

Relevant current behavior:

- `.scherzo/scherzo.yaml` routes labels with the `workflow:` prefix to workflow YAML files. It currently routes `execplan`, `execplan-revision`, and `execplan-implementation` to their v1 workflow files.
- `.scherzo/workflows/execplan.yaml` drafts, validates, reviews, publishes, and creates a follow-up implementation issue for one checked-in Markdown plan under `docs/plans/`.
- `.scherzo/workflows/execplan-revision.yaml` prepares PR feedback, lets an agent revise the existing plan file, validates the revision, publishes it, and acknowledges feedback.
- `.scherzo/workflows/execplan-implementation.yaml` prepares a referenced plan, refreshes the base, implements, analyzes changes, verifies plan completion, runs native review lanes, validates, and publishes a PR.
- `scripts/scherzo-execplan` enforces that v1 planning changes exactly one newly added `docs/plans/*.md` file and validates the full v1 ExecPlan section set.
- `scripts/scherzo-implementation` extracts v1 plan paths from Linear issue text and prepares plan briefs for implementation.
- `src/scherzo/workflow_contract.gleam`, `src/scherzo/workflow_contract_manifest.gleam`, `src/scherzo/workflow_run.gleam`, and `src/scherzo/state/artifact_store.gleam` implement workflow contract parsing, manifest values, output recording, and run artifact refs.
- Existing provider schemas live under `docs/schemas/provider/` for native review lane submissions.

Important terms:

- **Review doc**: a checked-in Markdown plan under `docs/plans/` that contains human-reviewable intent, scope, risk, milestones, acceptance, and history.
- **Implementation pack**: a retained JSON artifact that contains agent-oriented execution detail such as repo context, verified facts, concrete steps, dependencies, testing detail, and mechanical notes.
- **Exec plan bundle**: a retained JSON artifact that references the review doc and implementation pack by path/ref and hash, plus provenance, PR surface, and implementation handoff metadata.
- **Code change bundle**: a retained JSON artifact emitted by v2 implementation that references the source bundle and records the resulting PR, diff, validations, plan-completion verdict, and review artifacts.

## Preconditions and Verified Facts

Verified facts from repository inspection:

- `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, and `.scherzo/workflows/execplan-implementation.yaml` exist and are separate files, so v2 can be added without editing the v1 workflow YAML files.
- `.scherzo/scherzo.yaml` has routing entries for current workflows and a `linear_contract.workflow_labels` allow-list. New v2 labels must be added in both places to be dispatchable.
- `.scherzo/scherzo.yaml` has completion-state policy entries for reviewable workflows. V2 workflows should be added there so successful runs land in the same review state policy as v1.
- `docs/schemas/provider/` exists and currently contains native review lane provider schemas. There is no existing `docs/schemas/workflows/` directory, so this plan creates it for canonical workflow artifacts.
- `src/scherzo/workflow_contract.gleam` currently knows contract types including `text`, `artifact[]`, `document.markdown`, `exec_plan`, `git_ref`, `url`, and `code_change`. It needs explicit v2 JSON types.
- `src/scherzo/workflow_run.gleam` records step-field outputs as retained blobs and uses the contract type to choose extension and media type. V2 JSON types should use `.json` and `application/json`.
- `src/scherzo/state/artifact_store.gleam` defines output refs as `runs/<run-id>/outputs/<output-name><extension>`, which the bundle can predict for `implementation_pack.json` and `exec_plan_bundle.json`.
- Existing structured-output artifacts are stored under the run artifact directory by step id and attempt; v2 helper commands should resolve them through `SCHERZO_RUN_ARTIFACT_DIR` or the repository artifact store rather than hard-coding workspace temp paths.
- Existing workflow tests cover contract parsing, mapped inputs, output manifests, structured output retention, and current implementation helper behavior.

Preconditions before implementation:

- The implementer should run commands from the repository root.
- If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same command through `direnv exec .`.
- Do not modify existing v1 workflow files except if a shared prompt or helper extraction is strictly necessary; prefer new v2 files.

## Scope Boundaries

In scope:

- Add v2 workflow labels, routing, completion-state entries, and Linear contract allow-list entries.
- Add the three v2 workflow YAML files.
- Add v2 prompts under `.scherzo/workflows/prompts/`.
- Add canonical v2 artifact schemas and provider submission schemas.
- Add deterministic helper support in `scripts/scherzo-execplan-v2`.
- Extend workflow contract types so v2 artifacts are retained as first-class JSON outputs.
- Add tests for bundle validation, stale-pack rejection, revision supersession, and implementation output generation.
- Dogfood the v2 flow on one small issue after implementation.

Out of scope:

- Changing behavior of `workflow:execplan`, `workflow:execplan-revision`, or `workflow:execplan-implementation`.
- Migrating existing v1 plans or implementation issues to v2.
- Removing legacy Markdown or HTML plan support from current implementation helpers.
- Changing Linear state names, project settings, or operator dispatch policy beyond adding v2 labels to the existing config.
- Replacing native review lane artifacts or the current implementation review process.

## Milestones

1. **Schema fixtures and contract type support.** Add JSON schemas, valid and invalid fixtures, and contract type support for retaining v2 JSON artifacts. Verification: schema validation tests pass and workflow contract tests show the new types parse and retain as JSON.
2. **Bundle helper foundation.** Add `scripts/scherzo-execplan-v2` commands for review-doc validation, pack canonicalization, bundle validation, and implementation preparation. Verification: helper tests reject stale packs and absolute local path shapes and accept a valid fixture bundle.
3. **Drafting workflow.** Add `execplan-v2.yaml` and prompts that produce a concise checked-in review doc plus retained `implementation_pack` and `exec_plan_bundle` outputs. Verification: a local workflow parse/fingerprint test covers the YAML and a fixture command run emits valid artifacts.
4. **Revision workflow.** Add `execplan-revision-v2.yaml` and helper paths that transform a previous bundle and feedback into a superseding bundle, including unchanged revisions. Verification: tests assert supersession metadata and hash carry-forward for unchanged revisions.
5. **Implementation workflow.** Add `execplan-implementation-v2.yaml` and preparation/output generation helpers that validate the bundle before implementation and emit a `code_change_bundle` after publication. Verification: stale or missing review docs fail before agent steps, and code-change bundle fixture generation validates.
6. **Dogfood and rollback readiness.** Run v2 on one small issue without disrupting v1, then document the observed result in this plan. Verification: v2 PR and implementation PR are produced, and removing v2 labels/routes would fully disable the experiment.

## Plan of Work

Implement in narrow layers so each layer can be validated independently before touching workflow routing.

First, add schemas and fixtures. A canonical artifact schema must be strict about `schema_version`, `artifact_type`, required hashes, repository-relative paths, and artifact refs. Provider schemas may be less rich than canonical schemas, because helper commands will add provenance, hashes, refs, and handoff metadata.

Second, extend workflow contract types. This is the runtime foundation that lets workflow outputs be retained with meaningful types instead of being smuggled through generic output names. Add tests before changing workflows.

Third, implement helper commands. The helper owns canonicalization, publication, dynamic artifact discovery, retained-ref resolution, and validation. It should sort JSON keys, end files with one newline, compute SHA-256 from exact bytes, and produce stable error codes. The helper should be idempotent for PR and issue operations by finding existing PRs/issues before creating new ones.

Fourth, add v2 workflows and prompts. Prompts should be short because the contract and helper do the authoritative work. The drafting prompt tells the agent to edit only the review doc and submit the implementation pack draft. The revision prompt tells the agent to update the review doc and pack only when feedback requires it. The implementation prompt tells the agent to use the implementation pack as mechanical guidance and the review doc as authoritative intent. Workflow YAML must call helper discovery modes directly, so review-doc paths, structured-output submissions, and bundle ref/hash extraction are not left as implementer choices.

Finally, dogfood on one small issue. Keep v1 labels available throughout and do not promote v2 labels as defaults until the dogfood run has proven retention, handoff, revision, and implementation outputs.

## Concrete Steps

1. Check the current tree state.
   - Run: `$SCHERZO_WORKSPACE_DRIVER status --human`
   - Expected: no unrelated changes, or only changes from the current implementation task.

2. Create schema directories and fixtures.
   - Add `docs/schemas/workflows/exec-plan-bundle.v2.schema.json`.
   - Add `docs/schemas/workflows/implementation-pack.v2.schema.json`.
   - Add `docs/schemas/workflows/code-change-bundle.v2.schema.json`.
   - Add `docs/schemas/provider/implementation-pack-submission.v2.schema.json`.
   - Add `docs/schemas/provider/exec-plan-revision-submission.v2.schema.json`.
   - Add fixtures under `test/fixtures/execplan_v2/`:
     - `review-doc.valid.md`
     - `implementation-pack.valid.json`
     - `exec-plan-bundle.valid.json`
     - `code-change-bundle.valid.json`
     - `exec-plan-bundle.stale-pack.json`
     - `exec-plan-bundle.absolute-path.json`
   - Commit point: schemas and fixtures compile conceptually before runtime code changes.

3. Define canonical `implementation_pack` fields.
   - In `docs/schemas/workflows/implementation-pack.v2.schema.json`, require:
     - `schema_version: 2`
     - `artifact_type: "implementation_pack"`
     - `pack_id`
     - `source_issue.identifier`, `source_issue.title`, `source_issue.url`
     - `review_doc.path`, `review_doc.sha256`, `review_doc.bytes`
     - `derived_from.review_doc_path`, `derived_from.review_doc_sha256`
     - `provenance.workflow_id`, `provenance.run_id`, `provenance.step_id`, `provenance.created_at`
     - `sections.repo_context`
     - `sections.verified_facts[]` with `fact` and `evidence`
     - `sections.concrete_steps[]` with `title`, `instructions`, `files`, `commands`, and `expected_result`
     - `sections.testing_and_falsifiability`
     - `sections.interfaces_and_dependencies`
     - `sections.artifacts_and_notes`
     - `conflict_policy`
   - Add schema patterns so path fields are repository-relative and artifact refs match `runs/<run-id>/...`.

4. Define canonical `exec_plan_bundle` fields.
   - In `docs/schemas/workflows/exec-plan-bundle.v2.schema.json`, require:
     - `schema_version: 2`
     - `artifact_type: "exec_plan_bundle"`
     - `bundle_id`
     - `source_issue`
     - `workflow.workflow_id`, `workflow.run_id`, `workflow.workflow_fingerprint`
     - `revision.status` with enum `created`, `changed`, `unchanged`
     - `revision.number`
     - `revision.supersedes` as nullable object with `ref` and `sha256`
     - `review_doc.path`, `review_doc.sha256`, `review_doc.bytes`
     - `implementation_pack.ref`, `implementation_pack.sha256`, `implementation_pack.bytes`, `implementation_pack.schema`, `implementation_pack.derived_from_review_doc_sha256`
     - `review_surface.status` with enum `published`, `reused`, `not_applicable`
     - `review_surface.pr_url`, `review_surface.branch`
     - `implementation_handoff.issue_identifier`, `implementation_handoff.issue_url`, `implementation_handoff.workflow_label`, `implementation_handoff.bundle_ref`
     - `validation[]` entries with `name`, `status`, and optional `diagnostic`
   - Require `implementation_handoff.workflow_label` to equal `workflow:execplan-implementation-v2`.
   - Do not include a field that stores the SHA-256 of the bundle's own bytes. The helper computes the final bundle hash after writing canonical bytes and records it only in `outputs.v1.json`, command stdout/stderr diagnostics, and the Linear handoff issue.
   - For `revision.status: "created"` or `"changed"`, require `review_surface.status: "published"` plus `pr_url` and `branch`. For `revision.status: "unchanged"`, set `review_surface.status: "reused"`, carry the previous `pr_url` and `branch`, and record `review_surface.source_bundle_ref`.

5. Define canonical `code_change_bundle` fields.
   - In `docs/schemas/workflows/code-change-bundle.v2.schema.json`, require:
     - `schema_version: 2`
     - `artifact_type: "code_change_bundle"`
     - `source_bundle.ref`, `source_bundle.sha256`, `source_bundle.review_doc_sha256`, `source_bundle.implementation_pack_sha256`
     - `review_doc.path`, `review_doc.sha256`
     - `implementation_pack.ref`, `implementation_pack.sha256`
     - `pr.url`, `pr.branch`, `pr.base_revision`, `pr.head_revision`
     - `change.changed_files[]` with `path` and `status`
     - `change.diff.ref`, `change.diff.sha256`, `change.diff.bytes`
     - `validation_artifacts[]`
     - `plan_completion.verdict`, `plan_completion.ref`, `plan_completion.sha256`
     - `review_artifacts[]`
   - The accepted `plan_completion.verdict` values are `complete`, `incomplete`, and `blocked`.

6. Extend workflow contract types.
   - Edit `src/scherzo/workflow_contract.gleam` to add `ExecPlanBundle`, `ImplementationPack`, and `CodeChangeBundle` variants.
   - Map strings:
     - `exec_plan_bundle`
     - `implementation_pack`
     - `code_change_bundle`
   - Edit `src/scherzo/workflow_run.gleam` so these types retain as `.json` with `application/json`.
   - Edit any formatting, decoding, or tests that enumerate contract types.
   - Add tests in `test/workflow_contract_test.gleam` for parsing the new types.
   - Add tests in `test/workflow_run_test.gleam` for output refs such as `runs/run-1/outputs/exec_plan_bundle.json`.
   - Run: `direnv exec . gleam test`
   - Commit point: runtime contract support is green.

7. Add the v2 helper script skeleton.
   - Create `scripts/scherzo-execplan-v2` as an executable Python script.
   - Implement usage for these subcommands:
     - `validate-review-doc --path PATH`
     - `validate-review-doc --discover-changed-review-doc --write-path PATH`
     - `materialize-pack --review-doc PATH --submission PATH --output PATH`
     - `materialize-pack --review-doc-path-file PATH --submission-step STEP --submission-artifact NAME --output PATH`
     - `validate-bundle --bundle PATH [--bundle-sha256 SHA] [--artifact-root PATH] --repo-root PATH`
     - `publish-review-doc --review-doc-path-file PATH --publish-context PATH [--previous-bundle PATH --skip-if-unchanged]`
     - `materialize-bundle --review-doc-path-file PATH --pack PATH --publish-context PATH --output PATH`
     - `prepare-revision --from-issue-context --write-bundle PATH --write-review-doc-path PATH --write-pack PATH`
     - `materialize-revision --previous-bundle PATH --review-doc-path-file PATH --pack PATH --publish-context PATH --status auto --output PATH`
     - `implementation-prepare --from-issue-context`
     - `gate-no-conflict`
     - `materialize-code-change-bundle --bundle PATH --output PATH`
   - Keep command names stable; add options only when needed by tests. Discovery flags are part of the stable v2 contract, not test-only conveniences.

8. Implement review-doc validation.
   - `validate-review-doc` must accept either `--path PATH` or `--discover-changed-review-doc --write-path tmp/execplan-v2-review-doc.path`. Discovery must use `$SCHERZO_WORKSPACE_DRIVER changed-files --json`, require exactly one added or modified repository-relative `docs/plans/*.md` path, reject generated HTML, reject absolute local path shapes, and require these sections:
     - `Purpose / Big Picture`
     - `Problem Framing and Constraints`
     - `Strategy Overview`
     - `Alternatives Considered`
     - `Risks and Countermeasures`
     - `Scope Boundaries`
     - `Milestones`
     - `Progress`
     - `Decision Log`
     - `Validation and Acceptance`
     - `Rollout, Recovery, and Idempotence`
     - `Open Questions and Clarifications Needed`
   - It should reject detailed mechanical sections in the checked-in review doc when they appear as level-2 headings: `Concrete Steps`, `Testing and Falsifiability`, `Interfaces and Dependencies`, and `Artifacts and Notes`.

9. Implement pack canonicalization.
   - `materialize-pack` reads the provider submission JSON, reads the review doc, computes the review doc SHA-256 and byte count, injects provenance from `SCHERZO_RUN_ID`, workflow id, step id, and issue metadata, sorts keys, writes canonical JSON, validates it against `docs/schemas/workflows/implementation-pack.v2.schema.json`, prints the same canonical JSON to stdout, and exits nonzero on mismatch. With `--submission-step STEP --submission-artifact NAME`, it must resolve the latest successful structured-output artifact for that step from the retained run artifact directory and fail if there are zero or multiple valid candidates.
   - It must set `derived_from.review_doc_sha256` to the actual review-doc hash and must not trust a model-supplied hash.

10. Implement bundle validation.
    - `validate-bundle` loads the bundle, validates schema, validates the bundle file hash when `--bundle-sha256` is supplied, loads the referenced implementation pack by resolving `implementation_pack.ref` against the artifact-root order below, validates pack schema, recomputes pack hash, loads `review_doc.path`, recomputes review-doc hash, and checks all hash relationships. It must reject any bundle schema that attempts to store the bundle's own SHA-256 inside the bundle object.
    - Artifact-root resolution order for refs such as `runs/<run-id>/outputs/implementation_pack.json` is: `--artifact-root` when supplied; the parent of `SCHERZO_RUN_ARTIFACT_DIR` when set; `$SCHERZO_REPO_ROOT/.scherzo-state/artifacts` when `SCHERZO_REPO_ROOT` is set; `.scherzo-state/artifacts` relative to the current repository root; and a test fixture root supplied by `--repo-root`. Diagnostics should print the repository-relative roots or environment variable names tried, not token values or expanded secret-bearing paths.
    - Failure codes to print on stderr before the human message:
      - `SCHERZO_FAILURE_CODE=execplan_v2_bundle_missing`
      - `SCHERZO_FAILURE_CODE=execplan_v2_bundle_hash_mismatch`
      - `SCHERZO_FAILURE_CODE=execplan_v2_review_doc_missing`
      - `SCHERZO_FAILURE_CODE=execplan_v2_review_doc_hash_mismatch`
      - `SCHERZO_FAILURE_CODE=execplan_v2_implementation_pack_missing`
      - `SCHERZO_FAILURE_CODE=execplan_v2_implementation_pack_hash_mismatch`
      - `SCHERZO_FAILURE_CODE=execplan_v2_stale_implementation_pack`

11. Implement review-doc publication, bundle materialization, and handoff.
    - `publish-review-doc` reads `tmp/execplan-v2-review-doc.path`, reruns v2 review-doc validation, refuses any changed tracked file outside that one `docs/plans/*.md` path, creates or reuses a branch named `execplan-v2/<issue-identifier-slug>`, and creates or reuses a GitHub PR for that branch.
    - The PR title must be `ExecPlan v2: <issue identifier> <issue title>`. The PR body must include the review doc path, the source Linear issue URL, and a note that implementation details are retained in Scherzo artifacts rather than checked in.
    - `publish-review-doc` writes `tmp/scherzo-execplan-v2-publish-context.json` with this canonical shape, sorted keys, and one trailing newline:
      ```json
      {
        "artifact_type": "execplan_v2_publish_context",
        "pr": {
          "base_revision": "<revision>",
          "branch": "<branch>",
          "head_revision": "<revision>",
          "url": "https://github.com/<owner>/<repo>/pull/<number>"
        },
        "published_at": "<iso8601>",
        "review_doc": {
          "bytes": 123,
          "path": "docs/plans/<slug>.md",
          "sha256": "<sha256>"
        },
        "review_surface": {
          "source_bundle_ref": null,
          "status": "published"
        },
        "schema_version": 1,
        "source_issue": {
          "identifier": "LIV-123",
          "title": "<title>",
          "url": "https://linear.app/<workspace>/issue/LIV-123/<slug>"
        }
      }
      ```
    - `materialize-bundle` reads the final review doc, implementation pack, and publish context, computes hashes, predicts final output refs:
      - `runs/$SCHERZO_RUN_ID/outputs/implementation_pack.json`
      - `runs/$SCHERZO_RUN_ID/outputs/exec_plan_bundle.json`
    - It creates or reuses a Linear implementation issue labeled `workflow:execplan-implementation-v2`.
    - It writes the final bundle with `implementation_handoff.bundle_ref` populated but without any field containing the bundle's own SHA-256. After canonical bytes are written, the helper computes the bundle SHA-256 and updates the handoff issue body or comment with `Bundle ref`, `Bundle sha256`, `Review doc`, and `Implementation pack sha256` lines.
    - It prints canonical bundle JSON to stdout so the workflow contract output retains it. The workflow output manifest, not the bundle body, is the canonical retained location for the bundle ref/hash/byte count.

12. Add helper tests.
    - Add `test/execplan_v2_bundle_test.gleam` for command-level helper behavior using `command_step.run`.
    - Cover:
      - valid bundle accepted;
      - missing review doc rejected;
      - review doc hash mismatch rejected;
      - implementation pack hash mismatch rejected;
      - stale pack derived from a different review-doc hash rejected;
      - bundle schema rejects attempts to store the bundle's own SHA-256 inside the bundle object;
      - absolute local path placeholder shapes rejected without writing those shapes literally in source fixtures;
      - review-doc discovery fails for zero or multiple changed `docs/plans/*.md` files;
      - structured-output discovery finds the latest successful `implementation_pack_submission` and fails on ambiguity;
      - `publish-review-doc` writes the publish context shape above and reuses an existing branch/PR;
      - unchanged revision carries forward review doc and pack hashes while setting `revision.status` to `unchanged`, `revision.supersedes` to the prior bundle ref/hash, and `review_surface.status` to `reused`.
    - Run: `direnv exec . gleam test`.

13. Add v2 drafting prompts.
    - Add `.scherzo/workflows/prompts/execplan-v2-draft.md`.
    - Add `.scherzo/workflows/prompts/execplan-v2-review.md`.
    - Add `.scherzo/workflows/prompts/execplan-v2-incorporate-review.md`.
    - The draft prompt must instruct the agent to create exactly one concise review doc under `docs/plans/` and submit the implementation-pack draft through structured output. It must not ask the agent to write the canonical bundle.
    - The incorporate prompt must update both review doc and implementation-pack submission when review feedback changes implementation mechanics.

14. Add `.scherzo/workflows/execplan-v2.yaml`.
    - Use `workspace_profile: dogfood-jj` and do not modify v1 workflow YAML.
    - Include this contract shape:
      ```yaml
      contract:
        version: 1
        inputs:
          brief:
            type: text
            required: true
            source: issue_context
          supporting_context:
            type: artifact[]
            required: false
            source: mapped_output
        outputs:
          implementation_pack:
            type: implementation_pack
            source:
              step: materialize_pack
              field: stdout
          exec_plan_bundle:
            type: exec_plan_bundle
            source:
              step: materialize_bundle
              field: stdout
      ```
    - Wire the dynamic paths with helper discovery modes rather than hard-coded filenames. The command steps should be copyable in this form:
      ```yaml
      steps:
        - id: draft_review_doc_and_pack
          kind: agent
          prompt: prompts/execplan-v2-draft.md
          workspace: main
          structured_output:
            format: json
            artifact_name: implementation_pack_submission
            source:
              type: pi_tool_call
              tool_name: submit_implementation_pack_submission
              parameters_schema_path: docs/schemas/provider/implementation-pack-submission.v2.schema.json
              require_single: true
              reject_sibling_tool_calls: true
            required: true

        - id: validate_review_doc
          kind: command
          depends_on: [draft_review_doc_and_pack]
          run: 'repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-v2" validate-review-doc --discover-changed-review-doc --write-path tmp/execplan-v2-review-doc.path'
          workspace: main

        - id: review_plan
          kind: agent
          depends_on: [validate_review_doc]
          prompt: prompts/execplan-v2-review.md
          workspace: main

        - id: incorporate_review
          kind: agent
          depends_on: [review_plan]
          prompt: prompts/execplan-v2-incorporate-review.md
          workspace: main
          structured_output:
            format: json
            artifact_name: implementation_pack_submission
            source:
              type: pi_tool_call
              tool_name: submit_implementation_pack_submission
              parameters_schema_path: docs/schemas/provider/implementation-pack-submission.v2.schema.json
              require_single: true
              reject_sibling_tool_calls: true
            required: true

        - id: validate_review_doc_after_review
          kind: command
          depends_on: [incorporate_review]
          run: 'repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; review_doc_path=$(cat tmp/execplan-v2-review-doc.path); "$repo_root/scripts/scherzo-execplan-v2" validate-review-doc --path "$review_doc_path"'
          workspace: main

        - id: materialize_pack
          kind: command
          depends_on: [validate_review_doc_after_review]
          run: 'repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-v2" materialize-pack --review-doc-path-file tmp/execplan-v2-review-doc.path --submission-step incorporate_review --submission-artifact implementation_pack_submission --output tmp/execplan-v2-implementation-pack.json'
          workspace: main

        - id: publish_review_doc
          kind: command
          depends_on: [materialize_pack]
          run: 'repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-v2" publish-review-doc --review-doc-path-file tmp/execplan-v2-review-doc.path --publish-context tmp/scherzo-execplan-v2-publish-context.json'
          timeout_ms: 300000
          workspace: main

        - id: materialize_bundle
          kind: command
          depends_on: [publish_review_doc]
          run: 'repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-v2" materialize-bundle --review-doc-path-file tmp/execplan-v2-review-doc.path --pack tmp/execplan-v2-implementation-pack.json --publish-context tmp/scherzo-execplan-v2-publish-context.json --output tmp/execplan-v2-bundle.json'
          timeout_ms: 300000
          workspace: main
      ```

15. Add v2 revision support.
    - Add `.scherzo/workflows/prompts/execplan-revision-v2.md`.
    - Add `.scherzo/workflows/execplan-revision-v2.yaml`.
    - The workflow consumes a previous bundle pointer and review feedback, prepares the PR branch, validates the previous bundle, lets the agent revise the review doc and pack submission, materializes a superseding bundle, publishes the branch when changed, and acknowledges feedback.
    - `prepare_revision` must parse `Bundle ref:` and `Bundle sha256:` from the Linear issue context with `scripts/scherzo-execplan-v2 prepare-revision --from-issue-context --write-bundle tmp/execplan-v2-previous-bundle.json --write-review-doc-path tmp/execplan-v2-review-doc.path --write-pack tmp/execplan-v2-previous-pack.json`. It validates the previous bundle before any agent step and writes the repository-relative review-doc path file for later commands.
    - The revision agent must use structured output `implementation_pack_submission` with provider schema `docs/schemas/provider/exec-plan-revision-submission.v2.schema.json`.
    - After the agent step, run `materialize-pack` with `--review-doc-path-file tmp/execplan-v2-review-doc.path --submission-step revise_plan --submission-artifact implementation_pack_submission --output tmp/execplan-v2-implementation-pack.json`, then run `publish-review-doc --review-doc-path-file tmp/execplan-v2-review-doc.path --publish-context tmp/scherzo-execplan-v2-publish-context.json --previous-bundle tmp/execplan-v2-previous-bundle.json --skip-if-unchanged` to either publish changed review-doc bytes or write a reused publish context.
    - Run `materialize-revision --previous-bundle tmp/execplan-v2-previous-bundle.json --review-doc-path-file tmp/execplan-v2-review-doc.path --pack tmp/execplan-v2-implementation-pack.json --publish-context tmp/scherzo-execplan-v2-publish-context.json --status auto --output tmp/execplan-v2-bundle.json`.
    - For no-change feedback, the new `exec_plan_bundle` must set `revision.status: "unchanged"`, populate `revision.supersedes`, carry the same review doc and pack hashes, and set `review_surface.status: "reused"` with the previous `pr_url`, `branch`, and `source_bundle_ref`. It must not create a no-op PR.

16. Add v2 implementation support.
    - Add `.scherzo/workflows/prompts/execplan-implementation-v2-implement.md`.
    - Add `.scherzo/workflows/execplan-implementation-v2.yaml`.
    - Early command steps:
      - `prepare_bundle`: `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-v2" implementation-prepare --from-issue-context`
      - `refresh_base_before_implementation`: reuse current refresh-base behavior if safe through `scripts/scherzo-implementation refresh-base --stage before-implementation`.
    - `implementation-prepare --from-issue-context` must parse `Bundle ref:` and `Bundle sha256:` lines from the Linear issue context, resolve and validate the bundle and pack through the artifact-root order, recompute the checked-in review doc hash in the implementation workspace, and write `tmp/execplan-v2-review-doc.md`, `tmp/execplan-v2-implementation-pack.json`, and `tmp/execplan-v2-bundle.json`.
    - Agent step reads those three prepared files. If intent, scope, acceptance, or safety disagree between the review doc and implementation pack, the agent writes `tmp/execplan-v2-conflict.md` and stops.
    - Add `gate_no_conflict` immediately after the first implementation agent step with `scripts/scherzo-execplan-v2 gate-no-conflict`.
    - Reuse current analyze, plan-completion, native review, validation, and publish mechanics where possible.
    - Final command `materialize_code_change_bundle` runs `scripts/scherzo-execplan-v2 materialize-code-change-bundle --bundle tmp/execplan-v2-bundle.json --output tmp/execplan-v2-code-change-bundle.json`, emits canonical JSON to stdout, and copies diff, validation, plan-completion, and review artifacts into `$SCHERZO_RUN_ARTIFACT_DIR/execplan-v2/code-change/` before referencing them as `runs/$SCHERZO_RUN_ID/execplan-v2/code-change/...` refs in the bundle.

17. Update routing config after workflows parse locally.
    - Edit `.scherzo/scherzo.yaml` only after the new workflow files exist.
    - Add routes:
      - `execplan-v2: workflows/execplan-v2.yaml`
      - `execplan-revision-v2: workflows/execplan-revision-v2.yaml`
      - `execplan-implementation-v2: workflows/execplan-implementation-v2.yaml`
    - Add the same labels to `linear_contract.workflow_labels`.
    - Add completion-state entries mirroring v1:
      - `execplan-v2`
      - `execplan-revision-v2`
      - `execplan-implementation-v2`
    - Do not reorder or remove existing v1 entries unless the formatter requires stable ordering.

18. Add workflow parse and fingerprint coverage.
    - Add a named parser test such as `v2_execplan_workflows_parse_before_routing_test` in `test/workflow_dag_validator_parser_test.gleam` that parses the three new v2 workflow YAML files, including structured output and contract outputs.
    - Add a named fingerprint test such as `v2_workflow_fingerprint_includes_structured_output_and_contract_types_test` in `test/workflow_fingerprint_test.gleam` that asserts changing schema path, tool name, helper command, or output type changes the workflow fingerprint.
    - Pre-route gate: run `direnv exec . gleam test` and do not edit `.scherzo/scherzo.yaml` until those named tests pass with the v2 workflow files present.

19. Add implementation output generation tests.
    - Add a fixture run root under `test/tmp/` during tests.
    - Write fake publish metadata, validation metadata, review artifacts, plan-completion verdict, changed files, and diff content.
    - Run `scripts/scherzo-execplan-v2 materialize-code-change-bundle --bundle <fixture> --output <tmp-output>` through `command_step.run` with `SCHERZO_RUN_ID` and `SCHERZO_RUN_ARTIFACT_DIR` pointing at the fixture artifact store.
    - Assert the output validates against `docs/schemas/workflows/code-change-bundle.v2.schema.json` and includes PR URL, branch, base/head revisions, changed files, diff ref/hash, validation artifact refs, plan-completion verdict, and review artifact refs.
    - For every ref in `change.diff`, `validation_artifacts[]`, `plan_completion`, and `review_artifacts[]`, assert the referenced file exists under the fixture artifact root and its SHA-256 and byte count match the bundle.

20. Run focused validation.
    - Run: `direnv exec . gleam format --check src test`
    - Run: `direnv exec . gleam test`
    - Run: `direnv exec . gleam run -m glinter`
    - Run: `direnv exec . gleam run -m scherzo_lint`
    - Run helper fixture validations:
      - `direnv exec . scripts/scherzo-execplan-v2 validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --repo-root .`
      - `direnv exec . scripts/scherzo-execplan-v2 validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md`
    - Expected: all commands exit zero except negative tests that intentionally assert nonzero behavior.

21. Dogfood v2 on one small issue.
    - Create or choose a small issue whose implementation can be a docs-only or test-only change.
    - Label it only with `workflow:execplan-v2`.
    - Confirm the v2 draft PR contains only one checked-in human review doc under `docs/plans/`.
    - Confirm `.scherzo-state/artifacts/runs/<run-id>/outputs.v1.json` lists `implementation_pack` and `exec_plan_bundle` outputs with JSON refs and hashes.
    - Merge the plan PR after review.
    - Move the generated implementation issue to `Todo` with only `workflow:execplan-implementation-v2`.
    - Confirm implementation fails closed if the bundle hash is edited, then restore the correct hash and rerun or retry.
    - Confirm a successful implementation emits a `code_change_bundle` output.

22. Update this plan's living sections.
    - Mark milestones complete as they land.
    - Record any helper command name changes in `## Decision Log`.
    - Record dogfood observations in `## Outcomes & Retrospective`.

## Testing and Falsifiability

The implementation is correct only if these checks pass:

- **Schema validation:** valid fixture artifacts validate against their schemas; fixtures with missing required fields, non-relative paths, or invalid artifact refs fail.
- **Bundle validation:** `scripts/scherzo-execplan-v2 validate-bundle` accepts a valid bundle and rejects:
  - missing review doc;
  - review doc hash mismatch;
  - missing implementation pack;
  - implementation pack hash mismatch;
  - implementation pack derived from a different review-doc hash;
  - bundle handoff label other than `workflow:execplan-implementation-v2`;
  - any bundle body that tries to contain its own SHA-256.
- **Publication context:** `publish-review-doc` validates the concise v2 review doc, refuses unrelated checked-in changes, creates or reuses the PR, and writes schema-valid `tmp/scherzo-execplan-v2-publish-context.json` with review doc path/hash, PR metadata, source issue, and review-surface status.
- **Stale-pack rejection:** `implementation-prepare` fails before any agent step when the review doc bytes in the workspace do not match `exec_plan_bundle.review_doc.sha256` or when `implementation_pack.derived_from.review_doc_sha256` differs from the bundle review doc hash.
- **Revision supersession:** a no-change revision emits a new bundle with `revision.status: "unchanged"`, `revision.supersedes.ref` and `revision.supersedes.sha256` populated, the same review doc and pack hashes as the previous bundle, and `review_surface.status: "reused"`. A changed revision emits `revision.status: "changed"`, `review_surface.status: "published"`, and updated hashes.
- **Implementation output generation:** after a fixture publish, `materialize-code-change-bundle` emits a schema-valid `code_change_bundle` with PR URL, branch, base/head revisions, changed files, diff artifact/hash, validation artifacts, plan-completion verdict, and review artifacts. Every ref nested in the bundle resolves under the retained artifact root and hash-verifies.
- **Pre-route workflow parsing:** the named v2 workflow parser and fingerprint tests pass under `direnv exec . gleam test` before `.scherzo/scherzo.yaml` exposes the labels.
- **Non-regression:** current v1 workflow YAML files, labels, and helper behavior remain available. Existing v1 tests keep passing.

Commands to run from the repository root:

```sh
direnv exec . gleam format --check src test
direnv exec . gleam test
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint
```

Additional helper checks after `scripts/scherzo-execplan-v2` exists:

```sh
direnv exec . scripts/scherzo-execplan-v2 validate-bundle --bundle test/fixtures/execplan_v2/exec-plan-bundle.valid.json --repo-root .
direnv exec . scripts/scherzo-execplan-v2 validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md
```

This plan is falsified if any of the following are true after implementation:

- A v2 implementation can start after the review doc hash changes without a bundle revision.
- A v2 implementation can start with an implementation pack derived from a different review doc.
- The v2 drafting PR requires checking in the full mechanical implementation pack.
- A model final response is the only source for `exec_plan_bundle`, `implementation_pack`, or `code_change_bundle`.
- A retained ref inside `code_change_bundle` points only to `tmp/` or another non-retained workspace path.
- Existing `workflow:execplan`, `workflow:execplan-revision`, or `workflow:execplan-implementation` behavior changes for v1 issues.

## Validation and Acceptance

Acceptance criteria for this issue map to implementation outcomes as follows:

- **Reviewed ExecPlan describes v2 contract, rollout, failure modes, validation, and migration path.** This document names the v2 artifacts, schemas, workflows, failure codes, validation tests, and dogfood rollout.
- **Exact schema names and locations specified.** Canonical schemas are `docs/schemas/workflows/exec-plan-bundle.v2.schema.json`, `docs/schemas/workflows/implementation-pack.v2.schema.json`, and `docs/schemas/workflows/code-change-bundle.v2.schema.json`.
- **Outputs materialized and retained.** V2 workflows expose contract outputs retained as run artifacts: `runs/<run-id>/outputs/implementation_pack.json`, `runs/<run-id>/outputs/exec_plan_bundle.json`, and `runs/<run-id>/outputs/code_change_bundle.json`. Workflow manifests record refs, hashes, bytes, media type, and source; the bundle's own hash is never stored inside the bundle body.
- **Publication path is deterministic.** `publish-review-doc` owns v2 review-doc PR creation/reuse and writes `tmp/scherzo-execplan-v2-publish-context.json`; v2 does not call the v1 `scripts/scherzo-execplan create-pr` path.
- **Dogfood one small issue without disrupting current workflows.** V2 labels and routes are added in parallel; current labels remain unchanged. The plan includes a one-issue dogfood path and rollback by removing v2 labels/routes.
- **Tests cover required failure modes.** The plan requires tests for bundle validation, stale-pack rejection, revision supersession, and implementation output generation.

The final implementation should also pass this ExecPlan validation command for the plan file itself when run from any workflow workspace:

```sh
repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}
"$repo_root/scripts/scherzo-execplan" validate docs/plans/LIV-313-design-v2-execplan-bundle-workflows.md
```

## Rollout, Recovery, and Idempotence

Rollout sequence:

1. Merge schema, helper, tests, and v2 workflow config behind new labels.
2. Do not relabel existing v1 issues.
3. Run one v2 planning issue with `workflow:execplan-v2`.
4. Review the generated plan PR and inspect retained artifacts before merging.
5. Run the generated implementation handoff issue with `workflow:execplan-implementation-v2` only after the plan PR is merged.
6. Try one revision pass with `workflow:execplan-revision-v2` if actionable feedback appears.
7. Promote only after operators agree the PR review surface is smaller and the artifact handoff is reliable.

Recovery paths:

- If v2 drafting fails before publication, the workspace remains recoverable under normal Scherzo failure handling. Inspect helper diagnostics and retained step artifacts.
- If the bundle is invalid, fix the helper or prompt and rerun the v2 planning workflow. Do not hand-edit retained artifacts to force success.
- If an implementation issue cannot find its bundle because the ref/hash is wrong, rerun or repair the v2 planning handoff so the issue contains a correct bundle ref/hash. If the source run artifacts have been pruned, rerun planning or revision to produce a new retained bundle; v2 does not support reconstructing a bundle from copied JSON alone.
- If review doc and implementation pack conflict semantically, stop implementation and create or run `workflow:execplan-revision-v2` rather than choosing one source silently.
- If v2 causes routing trouble, remove the three v2 labels from `linear_contract.workflow_labels` and remove the three v2 route entries. Existing v1 workflows remain available.

Idempotence requirements:

- `publish-review-doc` must find an existing PR for the same branch before creating a new one and must write the same publish context bytes for identical PR/review-doc inputs.
- Implementation issue creation must find an existing handoff issue for the same source issue and bundle before creating a new one.
- Bundle materialization must produce identical bytes for identical inputs, except for explicit revision metadata or handoff metadata changes.
- Validation commands must be read-only except for writing diagnostics under `tmp/` or `$SCHERZO_RUN_ROOT/artifacts/`.

## Artifacts and Notes

Canonical v2 artifact schemas:

- `exec_plan_bundle`: `docs/schemas/workflows/exec-plan-bundle.v2.schema.json`
- `implementation_pack`: `docs/schemas/workflows/implementation-pack.v2.schema.json`
- `code_change_bundle`: `docs/schemas/workflows/code-change-bundle.v2.schema.json`

Provider submission schemas:

- `docs/schemas/provider/implementation-pack-submission.v2.schema.json`
- `docs/schemas/provider/exec-plan-revision-submission.v2.schema.json`

Retained output refs:

- Drafting workflow implementation pack: `runs/<run-id>/outputs/implementation_pack.json`
- Drafting workflow bundle: `runs/<run-id>/outputs/exec_plan_bundle.json`
- Revision workflow implementation pack: `runs/<run-id>/outputs/implementation_pack.json`
- Revision workflow superseding bundle: `runs/<run-id>/outputs/exec_plan_bundle.json`
- Implementation workflow code change bundle: `runs/<run-id>/outputs/code_change_bundle.json`

Artifact-root resolution for retained refs:

1. `--artifact-root` when a helper test or operator command supplies it.
2. The parent directory of `SCHERZO_RUN_ARTIFACT_DIR` when Scherzo sets it for the current run.
3. `$SCHERZO_REPO_ROOT/.scherzo-state/artifacts` when `SCHERZO_REPO_ROOT` is set.
4. `.scherzo-state/artifacts` relative to the current repository root.
5. A fixture artifact root under `test/fixtures/execplan_v2/` only when tests pass `--repo-root`.

The helper resolves only refs shaped like `runs/<run-id>/...`; it rejects absolute paths, parent-directory traversal, and refs that resolve outside the chosen artifact root.

Code-change bundle subordinate retained refs:

- Diff patch: `runs/<run-id>/execplan-v2/code-change/diff.patch`
- Changed files JSON: `runs/<run-id>/execplan-v2/code-change/changed-files.v1.json`
- Validation summaries: `runs/<run-id>/execplan-v2/code-change/validation/<name>.json`
- Plan-completion verdict: `runs/<run-id>/execplan-v2/code-change/plan-completion.json`
- Native review artifacts: `runs/<run-id>/execplan-v2/code-change/review/<name>.json`

`materialize-code-change-bundle` must copy the exact source bytes for those subordinate artifacts into `$SCHERZO_RUN_ARTIFACT_DIR/execplan-v2/code-change/` before calculating their refs, byte counts, and hashes.

Publish context:

- Drafting and changed revisions write `tmp/scherzo-execplan-v2-publish-context.json` with `review_surface.status: "published"`.
- Unchanged revisions write the same file with `review_surface.status: "reused"`, `review_surface.source_bundle_ref` set to the previous bundle ref, and previous PR URL/branch carried forward.

Review doc section set for v2 generated plans:

- `Purpose / Big Picture`
- `Problem Framing and Constraints`
- `Strategy Overview`
- `Alternatives Considered`
- `Risks and Countermeasures`
- `Scope Boundaries`
- `Milestones`
- `Progress`
- `Decision Log`
- `Validation and Acceptance`
- `Rollout, Recovery, and Idempotence`
- `Open Questions and Clarifications Needed`

Mechanical implementation material belongs in `implementation_pack.sections`, not in the checked-in review doc.

## Interfaces and Dependencies

- `.scherzo/scherzo.yaml`: routing, Linear contract labels, and completion-state policy for v2 workflows.
- `.scherzo/workflows/*.yaml`: workflow DAG definitions with contracts, command steps, agent steps, structured output specs, and output mappings.
- `.scherzo/workflows/prompts/*.md`: prompt templates for draft, review, revision, and implementation behavior.
- `scripts/scherzo-execplan-v2`: new deterministic helper for v2 review-doc validation, structured-output discovery, publication, artifact validation, materialization, retained-ref resolution, handoff, and implementation preparation.
- `scripts/scherzo-execplan`: remains the v1 helper and should not gain v2 behavior unless shared validation utilities are extracted without changing v1 command semantics.
- `scripts/scherzo-implementation`: may be reused by v2 implementation for refresh-base, analyze, validation, native review, and publish mechanics. If shared functions are extracted, preserve existing CLI outputs and tests.
- `src/scherzo/workflow_contract.gleam`: add v2 contract types.
- `src/scherzo/workflow_run.gleam`: retain v2 JSON output types with `.json` and `application/json`.
- `src/scherzo/workflow_contract_manifest.gleam`: no schema-version bump is expected unless manifest semantics change; add tests if new contract types affect validation.
- `src/scherzo/state/artifact_store.gleam`: existing artifact ref format is reused for contract outputs, and helper-written subordinate code-change artifacts must live under the same retained run artifact root.
- Linear: v2 handoff issue bodies/comments carry bundle refs and hashes. The helper must keep tokens out of logs.
- GitHub: v2 review doc PRs use the same publish style as v1, but the PR diff should contain only the checked-in review doc.

## Open Questions and Clarifications Needed

- [CLARIFY] Should `workflow:execplan-v2` create the implementation handoff issue in the same default state as v1 (`Backlog`) or use a separate experimental state/label to make v2 handoffs easier to distinguish?
- [CLARIFY] Should v2 revision issues be created manually by referencing a PR and bundle, or should the v2 drafting workflow pre-create a revision helper issue template?
- [CLARIFY] Should `supporting_context` artifacts be accepted only through Scherzo mapped outputs at first, or should the helper also parse explicit artifact refs from Linear issue text for manual testing?
