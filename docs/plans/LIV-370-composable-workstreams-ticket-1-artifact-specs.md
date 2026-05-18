# Define composable workstream and artifact specs

This ExecPlan v2 review document is the human review surface for LIV-370. It covers Ticket 1 from the composable workstreams UberPlan: defining workstream and artifact specs on top of the existing structured-output validator and workflow-contract foundations. The mechanical implementation instructions, exact test cases, interfaces, dependencies, and fixture notes are intentionally supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Ticket 1 gives Scherzo a stable vocabulary for composable workstreams before any runtime behavior depends on it. After the later implementation issue completes, a developer can read, encode, decode, and validate representative JSON artifacts for workstreams, handoffs, decisions, input bundles, assignments, next actions, and optional phase metadata. This makes later ledger, snapshot, handoff-emission, and start-from-handoff tickets safer because they will consume typed data with fixtures and stable error kinds instead of inventing shapes inside runtime code.

## Problem Framing and Constraints

The parent UberPlan in `docs/plans/LIV-241-composable-workstreams-uberplan.html` defines workstreams as durable cross-run delivery threads, not as one giant workflow DAG. LIV-360 already proved the foundation-alignment slice from `docs/plans/LIV-244-composable-workstreams-ticket-0-foundation-alignment.md`: future workstream validation can reuse `src/scherzo/workstream/foundation.gleam`, existing structured-output validators, and existing workflow `contract` inputs.

Ticket 1 must therefore stay at the spec layer. It must not implement a workstream ledger, snapshot store, handoff emitter, start tooling, inspection CLI, gates, playbooks, or dogfood conversion. It must also preserve current workflows: `.scherzo/workflows/execplan.yaml`, `.scherzo/workflows/execplan-revision.yaml`, and `.scherzo/workflows/execplan-implementation.yaml` already use top-level `contract` blocks and must continue to parse unchanged.

## Strategy Overview

The strategy is to add a small workstream spec namespace with shared JSON artifact descriptors, concrete artifact records, encoders, decoders, validation helpers, fixtures, and optional `workstream_phase` parsing. The implementation should be additive and should reuse `workflow_contract.ContractType` for run-local contract types while giving workstream artifacts richer cross-run names such as `scherzo.handoff.v1`.

The artifact taxonomy is intentionally small. A workstream artifact is the current rollup for one durable delivery thread: its issue, status, produced artifact snapshots, and suggested next actions. A handoff artifact is the phase-completion envelope that carries completed phase outputs, validation results, recommended follow-up actions, open questions, and a summary. A decision artifact records a human or automated gate outcome such as approve, request changes, reject, or deviate. An input bundle artifact packages the resolved inputs needed to start a downstream workflow from a prior handoff. An assignment artifact records why a workflow or playbook was selected for the workstream. A next-action artifact describes a candidate follow-up workflow, its required inputs or artifacts, gate requirements, auto-enqueue flag, state, and priority. Optional `workstream_phase` metadata is a workflow YAML annotation that maps ordinary workflow `contract` outputs to handoff and next-action hints; it is not a replacement contract language.

Stable fixture validation is the main deliverable. Valid fixtures prove the intended shapes are usable. Invalid fixtures prove missing headers, missing snapshot refs, bad paths, unknown decision kinds, and invalid metadata fail with stable error codes. Optional phase metadata should only describe workstream-specific facts and must not replace the existing workflow `contract` block.

## Alternatives Considered

One alternative is to start with the ledger and snapshot store, then discover artifact shapes as runtime needs appear. That is rejected because later durable events would immediately depend on unreviewed data shapes.

A second alternative is to make this ticket JSON Schema files only. That is too weak for the Gleam codebase because later runtime modules need typed values, encoders, decoders, and stable internal error kinds, not only external schema documents.

A third alternative is to introduce a separate phase-contract language for workstream inputs and outputs. That is rejected because Scherzo already has top-level workflow `contract` blocks and workflow contract manifests for run-local interfaces.

## Risks and Countermeasures

The main risk is scope creep into runtime behavior. The countermeasure is a hard stop at types, JSON encoding/decoding, fixtures, metadata parsing, and parser compatibility tests.

A second risk is producing permissive decoders that accept ambiguous or unsafe artifacts. The countermeasure is to require `schema_version`, `artifact_type`, immutable snapshot refs where relevant, repository-relative `original_path` values, known decision kinds, and stable error codes for every invalid fixture.

A third risk is breaking existing workflow parsing by making `workstream_phase` required or by disturbing existing `contract` handling. The countermeasure is explicit regression coverage for current workflow YAML files and a metadata parser that returns `None` when the field is absent.

A fourth risk is concentrating too much code in one large module. The countermeasure is to split shared types, artifact JSON, and phase metadata into small modules under `src/scherzo/workstream/` and keep source-guardrail validation green.

## Scope Boundaries

In scope for this planning issue is exactly this review document and one structured implementation-pack submission. No source implementation belongs in LIV-370.

In scope for the later Ticket 1 implementation issue are workstream spec types, JSON encoders and decoders, valid and invalid fixtures, fixture validation tests, stable error kinds, optional `workstream_phase` metadata parsing, workflow-fingerprint inclusion for present metadata, and regression tests proving existing workflows and existing top-level `contract` blocks still parse unchanged.

Out of scope are the workstream runtime, ledger records, content-addressed snapshot store, handoff emitter, start-from-handoff tooling, manual import tooling, inspection CLI, human gate commands, playbook parser, auto-enqueue policy, Linear state changes, and dogfood workflow conversion.

## Milestones

The first implementation milestone reconfirms the current foundations and adds failing tests that describe the desired artifact and metadata behavior.

The second milestone adds shared workstream artifact types and JSON encoders/decoders for workstream, handoff, decision, input bundle, assignment, and next-action artifacts.

The third milestone adds valid and invalid fixtures and makes every invalid fixture fail with a named stable error code.

The fourth milestone adds optional `workstream_phase` parsing without changing current workflow behavior, then includes present metadata in workflow fingerprints so future workstream-aware workflow changes are detectable.

The final milestone runs the targeted fixture tests plus the standard format, test, and lint gates, then stops before runtime work begins.

## Progress

- [x] (2026-05-18 00:00Z) Read the ExecPlan authoring guidance and the LIV-370 task boundaries.
- [x] (2026-05-18 00:00Z) Inspected the parent UberPlan, the Ticket 0 plan, and the current foundation modules and tests.
- [x] (2026-05-18 00:00Z) Drafted this concise review document for human review.
- [x] (2026-05-18 00:00Z) Prepared the structured implementation-pack content for Scherzo capture.
- [x] (2026-05-18 00:00Z) Incorporated review feedback by adding an artifact taxonomy and tightening fixture and fingerprint acceptance criteria.
- [x] (2026-05-18 20:45Z) Added `test/workstream_spec_test.gleam` with red round-trip and stable-error expectations for all six artifact families.
- [x] (2026-05-18 20:47Z) Confirmed the intended red phase: `direnv exec . gleam test` failed only for the missing workstream spec modules and fixtures.
- [x] (2026-05-18 21:20Z) Added typed workstream artifact modules, fixture JSON files, and JSON Schema files for workstream, handoff, decision, input bundle, assignment, and next action artifacts.
- [x] (2026-05-18 21:35Z) Extended validator-foundation coverage so handoff and decision fixtures validate through the existing structured-output seam.
- [x] (2026-05-18 22:05Z) Implemented optional `workstream_phase` parsing, workflow compatibility tests, and fingerprint coverage for absent versus present metadata.
- [x] (2026-05-18 22:15Z) Ran format, test, lint, review-doc validation, and scope-guardrail commands for the completed spec-only slice.
- [x] (2026-05-19 01:10Z) Review tightened decoder/schema parity for required fields, terminal parent paths, snapshot hash consistency, and wrong-typed optional metadata.
- [x] (2026-05-19 01:25Z) Added broader fingerprint regression coverage for optional `workstream_phase` fields and reran the full repository validation gates.

## Decision Log

- Decision: Ticket 1 should create typed workstream specs and fixtures before adding durable runtime state.
  Rationale: Later ledger and snapshot work needs stable artifact shapes and error kinds first.
  Date: 2026-05-18.

- Decision: Workstream specs reuse existing workflow contracts rather than defining a parallel phase contract.
  Rationale: Current workflow `contract` blocks already model run-local inputs and outputs; workstream metadata should only describe cross-run interpretation.
  Date: 2026-05-18.

- Decision: Optional `workstream_phase` metadata should be accepted only when present and should be included in workflow fingerprints when present.
  Rationale: Absence must preserve old workflows, while present metadata affects future workstream interpretation and should be detectable.
  Date: 2026-05-18.

- Decision: Review acceptance should explicitly name every artifact type and the fingerprint behavior under test.
  Rationale: The implementation issue should not be able to satisfy Ticket 1 with partial fixture coverage or metadata parsing that is invisible to workflow fingerprints.
  Date: 2026-05-18.

- Decision: Keep artifact decoding and JSON construction split between `src/scherzo/workstream/artifacts.gleam` and `src/scherzo/workstream/artifact_values.gleam`.
  Rationale: The spec slice added enough artifact families that a single new module would have tripped the source-guardrail size limit.
  Date: 2026-05-18.

- Decision: The typed decoders should enforce the same required-field and path invariants as the JSON Schemas rather than relying on a separate validation step to reject malformed artifacts.
  Rationale: Later runtime callers may use the typed decoders directly, so schema/decoder drift would weaken the stable artifact contract.
  Date: 2026-05-19.

## Validation and Acceptance

This planning issue is accepted when this Markdown review document exists under `docs/plans/`, `scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-370-composable-workstreams-ticket-1-artifact-specs.md` accepts it, and Scherzo captures the structured implementation-pack submission.

The later implementation issue is accepted only when valid fixtures for every named artifact type decode and re-encode: workstream, handoff, decision, input bundle, assignment, and next action. Invalid fixtures must also cover every named artifact family and the required negative cases with stable error kinds: missing `schema_version` or `artifact_type` headers, missing snapshot refs, absolute or otherwise non-repository-relative `original_path` values, unknown decision kinds, unknown next-action states, missing required workstream identity, and invalid `workstream_phase` metadata such as unknown handoff outputs, unknown next-action inputs, or unsupported snapshot settings.

The later implementation is also accepted only when optional `workstream_phase` metadata remains absent by default, current workflow YAML files with existing top-level `contract` blocks still parse unchanged, workflows without `workstream_phase` preserve existing workflow behavior and do not add a canonical workstream fingerprint field, workflows with `workstream_phase` parse successfully, and present metadata is included in workflow fingerprints so metadata changes produce fingerprint changes. The repository format, test, and lint gates must pass.

## Rollout, Recovery, and Idempotence

The planning change is additive. If review rejects this document, revise or remove only `docs/plans/LIV-370-composable-workstreams-ticket-1-artifact-specs.md` and resubmit the structured pack.

The later implementation should also be additive. New fixtures and modules can be removed without data migration because no production ledger or snapshot state is introduced in Ticket 1. Re-running fixture validation and workflow parsing tests is idempotent. Any attempt to add runtime records, operator commands, or workflow dispatch behavior should be treated as a scope failure and backed out into a later ticket.

## Open Questions and Clarifications Needed

No blocking clarification is needed for Ticket 1. Later tickets may refine exact ledger record names, snapshot storage paths, handoff emission timing, operator command syntax, and playbook composition; those choices should not be resolved by implementing runtime behavior in this spec-only slice.
