# LIV-857 Generic Custom Artifact Descriptors and Binary Workflow Outputs

## Purpose / Big Picture

This plan enables workflow authors to declare opaque, descriptor-first artifacts such as `scherzo_ui.visual_artifact_bundle.v1` and binary files such as PNG or WebM outputs without teaching Scherzo core each domain-specific artifact type. The visible result is that a workflow can retain exact local visual artifacts in Scherzo's artifact store, record hashes and byte counts in its output manifest, and publish a selected nested file by descriptor name when explicitly configured.

## Problem Framing and Constraints

Today the output path is still shaped by `workflow_contract.ContractType`. Parsing, canonical contract JSON, mapped-output compatibility, output materialization, retained manifests, workstream handoff/input bundles, repair manifests, and publication selection all depend on fixed legacy carrier names such as `exec_plan_bundle`, `implementation_pack`, `code_change_bundle`, and `artifact[]`. Descriptor declarations are accepted only when `workflow_contract_descriptor_compat.gleam` can infer one of those legacy types, so a custom artifact set cannot be declared generically. Command-step file outputs are also text-oriented: step files are read as strings, output blobs are written as strings, JSON decisions come from `ContractType`, and media types are derived from legacy types.

The change must remain local-artifact-store only for this slice. It must not upload images to GitHub Actions, add a visual gallery, or make core understand Scherzo UI bundle semantics. Core owns generic carrier safety only: safe paths, syntactically valid media types, opaque artifact-type strings, retained refs, exact-byte SHA-256 values, byte counts, and descriptor integrity.

## Strategy Overview

Migrate outputs to a descriptor-first model and keep legacy `type:` names only as parse-boundary aliases that immediately normalize to descriptor fields. New contracts and new manifests should carry `kind`, `media_type`, `artifact_type`, `ref_type`, `source`, `validation`, `metadata`, `ref`, `sha256`, `bytes`, `value`, and nested `entries` directly rather than deriving behavior from `ContractType`.

The safest sequence is additive first: introduce descriptor validation, descriptor compatibility helpers, binary artifact-store writes, and v2 manifest decoding alongside historical v1 decoding. Then switch output materialization, publication planning, first-party workflows, mapped outputs, repair-generation tests, and workstream handoff/input-bundle surfaces to descriptors. Finally remove broad output behavior keyed on `ContractType`, leaving only narrow legacy alias decoding for old YAML, old manifests, and old workstream records.

## Alternatives Considered

Adding new `ContractType` enum cases for `scherzo_ui.visual_artifact_bundle.v1`, `image/png`, or `video/webm` was rejected because it repeats the current limitation and makes core responsible for workflow-owned semantics. Treating all custom artifacts as `artifact[]` was rejected because it hides carrier differences and keeps publication tied to legacy names. Storing screenshots as base64 JSON was rejected because it risks byte corruption, bloats manifests, and does not provide first-class retained file descriptors.

A minimal patch that only relaxes descriptor compatibility was also rejected. It would let parsing pass but would leave text-only materialization, legacy manifest shape, repair and workstream type checks, and publication selection inconsistent.

## Risks and Countermeasures

The main risk is breaking existing dogfood workflows and historical runs while replacing a central type model. Countermeasure: keep v1 manifest and legacy `type:` decoding, migrate first-party YAML and fixtures in one milestone, and run full Gleam tests plus both production lints at every checkpoint.

A second risk is corrupting binary outputs through text decoding, newline normalization, or JSON-only validation. Countermeasure: read command-step source files as bytes, write output artifacts as bytes, hash with `sha256_hex_bytes`, and add invalid-UTF-8 PNG/WebM-style tests.

A third risk is accepting unsafe descriptors or paths. Countermeasure: centralize safe relative path checks, reject symlink escapes at materialization time, validate MIME syntax without an allowlist, validate opaque artifact-type string safety, and recursively verify retained nested artifacts for existence, SHA-256, and byte count.

A fourth risk is publication accidentally inferring workflow-specific bundle semantics. Countermeasure: allow nested publication only when `select.entry` names a descriptor entry inside a `kind: artifact_set` output; require that entry to be `kind: file`; and verify bytes before planning or publishing.

## Scope Boundaries

In scope are `src/scherzo/workflow_contract.gleam`, `src/scherzo/workflow_contract_descriptor_compat.gleam`, `src/scherzo/workflow_artifact_descriptor.gleam`, `src/scherzo/workflow_contract_manifest.gleam`, `src/scherzo/workflow_run/contract_io.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/artifact_store.gleam`, `src/scherzo/artifact_publication_config.gleam`, `src/scherzo/artifact_publication_planner.gleam`, workstream handoff/input-bundle code, workflow schemas, canonical dogfood workflow YAML under `workflows/dogfood/`, execplan materialization helpers such as `workflows/dogfood/scripts/scherzo-execplan`, operator-facing docs such as `docs/review-artifacts.md` and `.scherzo/README.md` when they describe artifact contract paths or helper behavior, contract/manifest/publication fixtures, mapped-output and repair-generation tests, and artifact inspection/publication helpers that display or consume output descriptors.

Out of scope are GitHub Actions artifacts, remote object-store hosting, a visual gallery UI, and Scherzo UI-specific semantic validation such as required screenshot naming. Core should validate generic descriptor safety and retained bytes only.

## Milestones

Milestone 1 establishes the descriptor model. It is complete when contracts parse `kind: file`, `kind: artifact_set`, `kind: value`, and `kind: ref` descriptors with arbitrary safe `artifact_type` values and syntactically valid MIME types, while legacy `type:` names normalize at the parser boundary and no new custom enum cases are added.

Milestone 2 moves retained manifests to descriptor-first shape. It is complete when new output manifests preserve descriptor fields directly, historical v1 manifests still decode, mapped-output compatibility compares descriptors rather than broad legacy enum cases, and repair-generation tests prove repaired output manifests and blobs still use the correct generation paths.

Milestone 3 implements binary file materialization. It is complete when command-step `source.path` outputs are read and retained as exact bytes, unsafe paths and symlink escapes are rejected, JSON validation is driven by descriptor media/kind instead of legacy type, and PNG/WebM-style binary tests prove byte preservation.

Milestone 4 validates generic artifact sets. It is complete when retained artifact-set JSON can carry custom `artifact_type` and nested descriptors, malformed descriptors fail, and every nested retained file or retained artifact set is verified against the local artifact store for existence, SHA-256, and byte count.

Milestone 5 migrates publication, workstream, and first-party/dogfood surfaces. It is complete when `select.output` plus `select.entry` publishes generic custom artifact-set entries by descriptor name, first-party workflow YAML and fixtures are descriptor-first, helper scripts and operator docs no longer describe new outputs through legacy `ContractType` or `contract_type` fields, workstream handoff/input bundles carry descriptors or normalized descriptor data, and legacy `type:` coverage is limited to explicit compatibility tests.

Milestone 6 validates the whole migration. It is complete when targeted parser, manifest, materialization, artifact-set integrity, publication, repair, workstream, schema, docs/helper, and helper tests pass; `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` pass; and any pre-publish helper or dogfood evidence required by changed workflow scripts is recorded.

## Progress

As of 2026-06-05, this review document and its paired structured implementation pack were drafted after inspecting the current contract parser, descriptor compatibility shim, manifest encoder/decoder, contract I/O materialization, artifact store byte APIs, publication planner/config, workflow schema, first-party workflow YAML, workstream handoff/start code, and representative tests. During review incorporation on 2026-06-05, the plan was tightened to make docs/helper migration and canonical `workflows/dogfood/` paths explicit; no implementation code has been changed in this authoring run.

## Decision Log

Decision: Make descriptor fields the canonical output contract and manifest representation. Rationale: custom visual artifact bundles must remain opaque to core while still using core-owned carrier safety. Date: 2026-06-05.

Decision: Keep legacy `type:` support only as a narrow alias parser and historical decoder boundary. Rationale: old workflows and manifests need compatibility, but new retained state should not require broad `ContractType` behavior or new enum cases. Date: 2026-06-05.

Decision: Validate MIME syntax without an allowlist. Rationale: valid media types such as `image/png`, `video/webm`, and vendor-tree JSON should work without core releases for each new domain. Date: 2026-06-05.

Decision: Require descriptor-name publication selection for nested artifact sets. Rationale: `select.entry` is explicit, stable, and generic, while inferring from custom `artifact_type` values would make core interpret workflow-owned semantics. Date: 2026-06-05.

Decision: Treat dogfood helper scripts and operator-facing docs as migration surfaces when they display or describe output contract identity. Rationale: a descriptor-first core migration can still confuse implementers and operators if helpers or docs continue to present new outputs as legacy `ContractType`/`contract_type` records. Date: 2026-06-05.

## Validation and Acceptance

Acceptance requires evidence for each named behavior. Parser evidence must include tests that descriptor-first `file`, `artifact_set`, `value`, and `ref` outputs with custom artifact types parse without legacy enum cases, malformed descriptors and invalid MIME strings fail, and legacy `type:` aliases normalize only at the boundary. Manifest evidence must include golden or round-trip tests showing new output manifests preserve descriptor fields directly and old v1 manifests still decode.

Materialization evidence must include tests that exact binary bytes from command-step paths are retained with matching `ref`, `sha256`, `bytes`, and `media_type`; that invalid-UTF-8 image/video fixtures are not decoded as text; that unsafe paths and symlink escapes fail; and that JSON validation applies only where descriptor kind/media require it. Artifact-set evidence must include custom artifact-set output tests, malformed nested descriptor tests, retained nested artifact existence/hash/byte mismatch tests, and proof that domain-specific visual semantics are not enforced by core.

Publication evidence must include planner tests for `select.output` plus `select.entry` against a generic custom artifact set, negative tests for missing entries and non-file entries, and byte-verification failures for selected nested files. Migration evidence must include updated first-party workflow YAML under `workflows/dogfood/`, schema/fixture tests, mapped-output/workstream compatibility tests, repair-generation tests, docs/helper checks for `docs/review-artifacts.md`, `.scherzo/README.md`, and changed helper scripts, and artifact publication/inspection helper tests. Final validation must run `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; deterministic local helper checks are pre-publish if workflow scripts change, while live visual dogfood review can be deferred to a human/operator after implementation unless the implementation changes live dispatch or publishing behavior.

## Rollout, Recovery, and Idempotence

Roll out in green commits matching the milestones: descriptor parser and validators, manifest migration, binary materialization, artifact-set verification, publication/workstream/first-party/docs-helper migration, and final validation. Each milestone should leave historical v1 decoding intact and can be rerun safely because artifact writes remain deterministic by run id, output name, and repair generation.

Recovery is to revert the latest descriptor-first slice while keeping historical decoders and old workflow YAML aliases until the replacement tests pass. If binary writes reveal artifact-store incompatibility, restore text output writes for non-binary descriptors and keep the byte path behind file-output tests until fixed. If workstream or publication migration becomes too large, stop after manifest compatibility and split descriptor-bearing workstream record schema changes into a follow-up rather than publishing a half-migrated core.

## Open Questions and Clarifications Needed

No open questions block implementation. The visual UI workflow itself remains a follow-up consumer; this task should provide generic descriptor, binary retention, and explicit publication-selection support only.
