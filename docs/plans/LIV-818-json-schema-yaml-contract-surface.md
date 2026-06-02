# LIV-818 Adopt JSON Schema as the Scherzo YAML contract surface

## Purpose / Big Picture

Scherzo operators should be able to open `.scherzo/scherzo.yaml` and workflow DAG YAML in a normal editor and get completion, hover text, and early shape diagnostics from the same public contract Scherzo documents. This plan chooses JSON Schema as that YAML contract surface, while keeping the existing Gleam parsers as the runtime enforcement path and preserving Scherzo's current compatibility diagnostics.

## Problem Framing and Constraints

Today the accepted YAML shape is spread across parser code, the simplified YAML spec, examples, fixtures, and tests, so editor support can drift from runtime behavior. The authoritative facts to reconcile are in `src/scherzo/config.gleam`, `src/scherzo/config/root_schema.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/config/duration_config.gleam`, `src/scherzo/config/tracker_config.gleam`, `src/scherzo/config/workspace_driver_config.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_dag_validator_parser.gleam`, `src/scherzo/workflow_yaml_migration.gleam`, `docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md`, shipped YAML examples, and parser/guardrail tests. OpenAPI is explicitly not a goal for config or workflow files because this is not an HTTP API surface and YAML language servers consume JSON Schema. Runtime parser errors, provider-backed live probes, cache or liveness behavior, dispatch, and workflow helper execution are constraints to leave unchanged unless a test proves the new schemas broke an existing path.

## Strategy Overview

Publish checked-in draft 2020-12 JSON Schema artifacts under `schemas/`: `schemas/scherzo.config.v1.schema.json` for `.scherzo/scherzo.yaml` and `schemas/scherzo.workflow.v1.schema.json` for workflow DAG files. Their stable `$id` values should be `https://scherzo.dev/schemas/scherzo.config.v1.schema.json` and `https://scherzo.dev/schemas/scherzo.workflow.v1.schema.json`; breaking YAML contract changes require new v2 filenames and IDs, while wording-only or compatibility fixes keep v1. The first migration slice should hand-author reviewable schemas from the current parser/spec facts and lock them to runtime behavior with schema self-checks, parser/schema parity tests, negative drift fixtures, shipped-example validation, and documentation comments. Code generation can be revisited only after a smaller schema description source exists.

## Alternatives Considered

OpenAPI was rejected because it models HTTP APIs, not repository YAML files, and would not be the editor-native association format for YAML language servers. Prose-only documentation was rejected because it does not give editor completion or automated drift checks. A full generator from the current Gleam parsers was deferred because the parsers encode migration diagnostics and semantic checks rather than a reusable schema AST, making a generator larger than the immediate contract-surface need.

## Risks and Countermeasures

The largest risk is schema/parser drift. Counter it by validating shipped examples and schema fixtures through both the existing parser boundary and the JSON Schemas, and by adding negative fixtures for removed keys such as `routing.workflows`, `polling.interval_ms`, `workspace.profiles`, workflow `max_parallel_steps`, step `workspace`, command step `timeout_ms`, and structured-output validator `timeout_ms`.

A second risk is over-constraining YAML that Scherzo currently accepts. Counter it by starting from `docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md`, parser tests, and examples, and by leaving intentionally permissive extension points where runtime code is permissive. A third risk is confusing JSON Schema diagnostics with Scherzo's compatibility diagnostics; counter it by keeping parser migration messages authoritative and using schema failures as editor/preflight hints. A fourth risk is accidental scope creep into provider-live checks, cache/liveness behavior, dispatch, or workflow helper migration; counter it by making those areas explicit non-goals and by keeping implementation evidence focused on schemas, docs, examples, fixtures, and tests. A fifth risk is accepting a plan without human-observable editor evidence; counter it by requiring automated schema evidence before publish and recording live editor hover/completion dogfood as a deferred human/operator check when the implementation environment has no GUI editor.

## Scope Boundaries

In scope are public JSON Schemas for root config YAML and workflow DAG YAML, schema IDs and versioning policy, editor association documentation, example `# yaml-language-server: $schema=...` comments where those comments help operators, drift/golden tests, schema self-check tests, shipped example parity tests, negative removed-key tests, and compatibility diagnostics coverage. Documentation migration is limited to docs and examples that describe YAML schema association; existing helper scripts such as `scripts/scherzo-json-schema-validate`, `workflows/dogfood/scripts/scherzo-execplan`, and workflow structured-output helper paths should stay in place unless the implementer finds a direct validation gap and records the reason. Out of scope are OpenAPI artifacts, runtime replacement of Gleam parser diagnostics, remote schema registry publication, YAML semantic changes, provider-live behavior, cache or liveness behavior, workflow helper migrations, dispatch changes, and broad parser refactors.

## Milestones

Milestone 1 produces the public schema files under `schemas/` with `$schema`, `$id`, v1 filenames, definitions for the current accepted config/workflow shapes, descriptions for editor hover, and clear treatment of removed legacy keys. The observable outcome is that `scripts/scherzo-json-schema-validate` accepts representative JSON payloads converted from `test/fixtures/schema/orchestrator_config_complete.yaml` and `test/fixtures/schema/workflow_dag_complete.yaml`, and rejects a deliberately invalid payload for each schema.

Milestone 2 adds schema self-check and parser/schema parity tests using `test/fixtures/schema/orchestrator_config_complete.yaml`, `test/fixtures/schema/workflow_dag_complete.yaml`, `examples/scherzo*.yaml`, `examples/workflows/*.yaml`, and `workflows/dogfood/*.yaml` so examples cannot parse while violating the schemas. The tests should name the file that failed and should prove both the parser result and JSON Schema result for every positive fixture.

Milestone 3 adds negative drift coverage for documented removed keys and incompatible shapes, proving that Scherzo's parser still emits migration diagnostics and that the JSON Schemas reject stale YAML before operators rely on it. The required negative cases are `routing.workflows`, `polling.interval_ms`, `workspace.profiles`, workflow `max_parallel_steps`, step `workspace`, command step `timeout_ms`, and structured-output validator `timeout_ms`.

Milestone 4 updates documentation and examples with `# yaml-language-server: $schema=...` comments and yaml-language-server association settings for `.scherzo/scherzo.yaml` and workflow YAML globs. This milestone must also review docs for stale helper or schema paths and keep any helper-path migration explicitly deferred unless a path is now misleading for editor schema association.

Milestone 5 runs full repository validation and records acceptance evidence. Command-line schema validation, parity tests, negative tests, formatting, unit tests, `glinter`, and `scherzo_lint` are pre-publish evidence. Live editor hover/completion dogfood is a deferred human/operator check after implementation when the implementer has no GUI editor, and the implementation handoff must state whether that check was completed or deferred.

## Progress

- [x] (2026-06-02) Created this review document at `docs/plans/LIV-818-json-schema-yaml-contract-surface.md` and prepared the structured implementation-pack submission for the follow-up implementation workflow.
- [x] (2026-06-02) Incorporated review feedback by making acceptance evidence, tests, milestone outputs, manual dogfood timing, documentation/helper scope, provider-live/cache non-goals, full validation, and lint gates explicit in this review document and the implementation-pack submission.

## Decision Log

- Decision: JSON Schema is the Scherzo YAML contract surface for root config and workflow DAG files; OpenAPI is a non-goal. Rationale: YAML language servers and common editors consume JSON Schema, and config/workflow files are not HTTP API documents. Date: 2026-06-02.
- Decision: Start with checked-in hand-authored schemas rather than a generator. Rationale: current Gleam parsers are the runtime authority and include compatibility diagnostics that are better guarded by parity tests than by a premature generator. Date: 2026-06-02.
- Decision: Use top-level `schemas/` artifacts with stable `https://scherzo.dev/schemas/...` IDs. Rationale: public operator-facing schemas should be separate from internal `.scherzo/workflows/schemas/` structured-output contracts while still being available locally. Date: 2026-06-02.
- Decision: Treat command-line schema validation and parser/schema parity as pre-publish acceptance, while live editor hover/completion is deferred when no GUI editor is available. Rationale: automated evidence is reproducible in the implementation environment, but editor UI dogfood may require a human/operator environment after handoff. Date: 2026-06-02.
- Decision: Keep provider-live behavior, cache/liveness behavior, dispatch, and workflow helper migration out of scope. Rationale: this work adds an editor contract surface and should not change runtime integration behavior or workflow helper execution. Date: 2026-06-02.

## Validation and Acceptance

Planning acceptance is verifiable by running `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-818-json-schema-yaml-contract-surface.md` and expecting `REVIEW_DOC_VALID=ok`, plus Scherzo's capture of the structured implementation-pack submission.

Implementation acceptance requires observable artifacts `schemas/scherzo.config.v1.schema.json` and `schemas/scherzo.workflow.v1.schema.json` with draft 2020-12 declarations and the chosen `$id` values, docs/examples showing editor associations, positive parser/schema parity evidence for shipped config and workflow examples, and negative evidence for the removed-key cases listed in Risks. The implementer must record evidence from targeted schema/parity tests, from at least one direct `scripts/scherzo-json-schema-validate` acceptance and rejection run, and from full repository gates. Final automated gates are `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; if `.envrc` is blocked, inspect it, run `direnv allow .`, and retry through direnv. Manual editor hover/completion dogfood is post-implementation evidence if no GUI editor is available before publish, and that deferral must be documented by the implementer.

## Rollout, Recovery, and Idempotence

The rollout is additive: schema files, docs, comments, fixtures, and tests can land without changing runtime parsing, dispatch, provider-live checks, cache/liveness behavior, or workflow helper execution. If a schema rejects valid YAML, relax or revert the schema while keeping the parser unchanged; if a docs association is wrong, fix the docs without touching runtime code. Re-running the implementation is idempotent because the schema filenames, `$id` values, fixtures, and validation commands are deterministic. If the implementation discovers a legitimate reason to change a helper script, it should keep that change isolated, document the reason in the plan handoff, and prove existing structured-output helper tests still pass.

## Open Questions and Clarifications Needed

No open questions. The plan assumes local checked-in schemas are authoritative until Scherzo deliberately publishes a remote schema registry at the same `$id` URLs, and it assumes live editor hover/completion dogfood may be completed by a human/operator after implementation when the implementation workspace has no GUI editor.
