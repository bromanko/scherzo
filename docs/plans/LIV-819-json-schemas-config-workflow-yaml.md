# LIV-819 Publish JSON Schemas for Scherzo config and workflow YAML

## Purpose / Big Picture

Scherzo operators should be able to open `.scherzo/scherzo.yaml` and workflow DAG YAML files in an editor and get useful completion, hover descriptions, and early diagnostics before running `doctor` or dispatching work. This plan adds versioned, checked-in JSON Schema artifacts for the public v1 root config and workflow DAG shapes, plus tests and docs that keep those artifacts aligned with the parser and examples.

This revision also makes the implementation evidence explicit: the follow-up pack must specify positive and negative parser/schema tests, schema self-checks, documentation evidence, full format/test/lint gates, and the timing for manual editor dogfood checks.

## Problem Framing and Constraints

The current public YAML shape is documented in `docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md` and enforced by Gleam parsers, but editors cannot consume those parsers directly. The schemas must be stable and reviewable, must include current accepted fields and useful diagnostics for removed legacy keys, and must not become a second unchecked source of truth. The solution must fit the existing Gleam/devenv toolchain and pass the repository's required format, test, glinter, and Scherzo lint gates.

Review feedback requires the human-readable plan and structured implementation pack to agree on acceptance evidence, test obligations, milestone-specific proof, manual/dogfood timing, docs/helper boundaries, provider-live/cache non-scope, full validation, and linting. These obligations are planning requirements, not optional TODOs for a later implementer to rediscover.

## Strategy Overview

Add hand-reviewable draft 2020-12 schemas under a top-level `schemas/` directory, using stable `$id` values and filename versioning such as `scherzo.config.v1.schema.json` and `scherzo.workflow.v1.schema.json`. Keep them intentionally declarative rather than building a broad code generator, then add parser-and-schema tests that validate all shipped examples and negative legacy-key fixtures through both the existing parser boundary and the published schemas. Document editor usage through `$schema` comments and yaml-language-server settings.

Sequence the implementation as independently verifiable increments: first create schemas and schema self-check fixtures, then prove current examples pass both parser and schema validation, then prove documented legacy keys fail both boundaries, then update operator-facing docs, and finally run full repository validation. Schema validation remains local and offline; this plan does not change live provider behavior, liveness caches, or runtime dispatch.

## Alternatives Considered

A full schema generator from Gleam parser code would reduce duplicate declarations, but the current parser is hand-written around diagnostics and migration behavior rather than a schema AST, so building a generator would be larger than the editor-support outcome. Publishing prose-only docs would be cheaper but would not help editors or catch drift. Embedding schemas only in workflow bundles would help Scherzo internals but would miss the public config and external editor use case.

## Risks and Countermeasures

The main risk is schema drift from parser behavior. Counter it with tests that parse examples through Scherzo and validate the same YAML data against the schemas, plus negative tests for removed keys such as `routing.workflows`, `polling.interval_ms`, `workspace.profiles`, workflow `max_parallel_steps`, step `workspace`, and validator `timeout_ms`. Another risk is over-restricting valid YAML and blocking operators; counter it by covering documented examples first, using descriptions and enums where behavior is stable, and allowing extension points where parser behavior is intentionally permissive.

A third risk is editor-specific ambiguity. Counter it by documenting both inline `$schema` comments and yaml-language-server settings, making command-line schema validation a pre-publish requirement, and treating live editor hover/completion verification as deferred human/operator dogfood evidence if the implementation runner lacks a GUI editor.

A fourth risk is letting review feedback live only in this prose document while Scherzo materializes later implementation work from the structured pack. Counter it by mirroring acceptance evidence, exact tests, milestone proof anchors, manual/dogfood timing, docs/helper non-scope, provider-live/cache non-scope, full validation, and lint gates in the updated implementation-pack submission.

## Scope Boundaries

In scope are JSON Schema artifacts for the root orchestrator config and workflow DAG YAML v1, docs explaining how to attach them to YAML files, example/schema drift tests, and targeted negative fixtures for removed public keys. In scope test support may include a small test-only helper for converting parsed YAML nodes into JSON values before invoking the existing JSON Schema validator.

Out of scope are changing Scherzo's YAML semantics, replacing parser diagnostics with JSON Schema diagnostics at runtime, supporting old config shapes, publishing schemas to a remote registry, and building a general Gleam-to-JSON-Schema generator. Also out of scope are workflow helper migrations, provider-live adapter behavior, liveness/cache behavior, remote schema fetching, and any dispatch/runtime changes; docs work is limited to operator-facing schema usage instructions.

## Milestones

Milestone 1 produces the public schema artifacts with stable `$id`, draft 2020-12 declarations, descriptions, versioned filenames, accepted v1 fields, and explicit legacy-key rejection guidance where JSON Schema can express it. Its proof is a schema self-check using representative valid config and workflow payload fixtures through the repository's JSON Schema validator path; the helper's draft 2020-12 schema check must fail if either schema document is invalid.

Milestone 2 proves examples and parser behavior stay aligned by adding tests that validate `examples/scherzo*.yaml`, `examples/workflows/*.yaml`, and `workflows/dogfood/*.yaml` against the schemas after parsing them through the existing Scherzo loaders. Its proof is a targeted test run that would fail if an example parses but violates the schema, or validates against the schema but no longer parses through Scherzo.

Milestone 3 hardens drift detection with negative fixtures for documented removed keys and shape errors, ensuring the parser still emits migration diagnostics and the schemas also reject those stale shapes. Its proof includes named cases for `routing.workflows`, `polling.interval_ms`, `workspace.profiles`, workflow `max_parallel_steps`, step `workspace`, command step `timeout_ms`, and structured-output validator `timeout_ms`.

Milestone 4 updates operator documentation so a human can add `$schema` comments or yaml-language-server mappings and know which schema file applies to config versus workflow YAML. Its proof is a docs diff containing both inline and yaml-language-server examples, plus an explicit inventory that no workflow helper migration, provider-live behavior, or cache/liveness behavior was changed in this slice.

Milestone 5 runs the required validation gates and records any editor dogfood evidence that was possible before handoff. Pre-publish evidence is command-line schema validation, targeted tests, full `gleam test`, format, glinter, and Scherzo lint. If no editor is available in the implementation environment, live hover/completion verification is deferred to a human/operator after implementation and must remain documented as deferred rather than silently treated as done.

## Progress

- [x] (2026-06-01) Created this concise review document and prepared the structured implementation pack for the follow-up implementation workflow.
- [x] (2026-06-01) Incorporated review feedback by making acceptance evidence, test obligations, milestone proof anchors, manual/dogfood timing, docs/helper non-scope, provider-live/cache non-scope, full validation, and linting explicit in this review document and the updated implementation-pack obligations.

## Decision Log

- Decision: Publish checked-in, hand-reviewable schemas under `schemas/` rather than generating them in the first version. Rationale: the current parser and migration diagnostics are hand-written, so tests are the safer proportional drift control for this task. Date: 2026-06-01.
- Decision: Treat editor hover/completion dogfooding as post-implementation manual evidence when no GUI editor is available to the implementer. Rationale: command-line schema validation and documentation can be gated before publish, while editor UX can be verified by a human using the documented settings. Date: 2026-06-01.
- Decision: Mirror review feedback about evidence, tests, milestone specificity, manual/dogfood timing, docs/helper scope, provider-live/cache boundaries, full validation, and linting in the structured implementation pack. Rationale: Scherzo materializes the follow-up implementation from that pack, so prose-only acceptance criteria would be easy for later implementers to miss. Date: 2026-06-01.

## Validation and Acceptance

Planning acceptance is met when this file remains at `docs/plans/LIV-819-json-schemas-config-workflow-yaml.md`, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-819-json-schemas-config-workflow-yaml.md` exits zero with `REVIEW_DOC_VALID=ok`, every required level-2 review-doc section is present and non-empty, and Scherzo captures the updated structured implementation-pack submission.

Implementation acceptance is met when `schemas/scherzo.config.v1.schema.json` and `schemas/scherzo.workflow.v1.schema.json` exist, declare draft 2020-12, have stable `$id` values, and pass JSON Schema self-checks through representative valid fixtures. Automated evidence must show that shipped config and workflow examples validate against the schemas and still parse through Scherzo, and that documented legacy keys are rejected by both the parser boundary and the schemas. Required negative evidence includes `routing.workflows`, `polling.interval_ms`, `workspace.profiles`, workflow `max_parallel_steps`, step `workspace`, command step `timeout_ms`, and structured-output validator `timeout_ms`.

Documentation acceptance is shown by a docs update with concrete `$schema` and yaml-language-server examples and an explicit note that local checked-in schemas are authoritative until a remote registry exists. Final implementation evidence must include successful runs of `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. Command-line schema validation and docs are pre-publish requirements; editor hover/completion evidence may be collected after handoff if no editor is available during implementation.

## Rollout, Recovery, and Idempotence

The rollout is additive: new schema, fixture, test, and docs files do not change runtime parsing or dispatch behavior. JSON Schema is an editor and validation artifact, not a replacement for parser diagnostics. No provider-live adapter, liveness cache, workflow helper, remote registry, or dispatch path is migrated by this slice.

If a schema proves too strict, revert or relax the schema and keep parser behavior unchanged while tests identify the mismatch. If a test helper for YAML-to-JSON conversion is wrong, fix or remove that helper without changing production parsing semantics. Re-running the implementation is idempotent because schema files, docs, and fixtures are deterministic checked-in artifacts; repeated validation commands should leave the working tree unchanged except for intentional edits.

## Open Questions and Clarifications Needed

No open questions. Use `https://scherzo.dev/schemas/scherzo.config.v1.schema.json` and `https://scherzo.dev/schemas/scherzo.workflow.v1.schema.json` as stable schema identifiers, and document that the checked-in files are the authoritative local artifacts until Scherzo publishes a remote schema registry.
