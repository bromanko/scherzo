# Align composable workstreams with Scherzo's existing validator and workflow contract foundations

This ExecPlan v2 review document is the human review surface for LIV-244. It intentionally covers only Ticket 0, the foundation-alignment slice from the composable workstreams UberPlan, and leaves the mechanical implementation instructions in the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Ticket 0 exists to keep future composable-workstream implementation from forking Scherzo's workflow foundations. After the later code ticket implements this plan, a developer can prove that workstream-adjacent artifact validation uses the existing structured-output JSON Schema and command validator runner, and that handoff-derived inputs can enter a later workflow through the existing top-level `contract` input model and `workflow_contract_inputs` manifest.

The observable result is deliberately small: tests and any minimal helper seams show foundation reuse. This is not the workstream runtime. It does not create a ledger, snapshot store, handoff emitter, start command, inspection CLI, gate model, playbook engine, dogfood integration, or follow-up ticket set.

## Problem Framing and Constraints

The parent UberPlan in `docs/plans/LIV-241-composable-workstreams-uberplan.html` warned that workstreams must not grow a bespoke validator layer or a competing phase-contract system. The current repository has already moved further than that warning's original context: `src/scherzo/workflow_dag.gleam` models generic `JsonSchemaValidator` and `CommandValidator` declarations, `src/scherzo/structured_output.gleam` dispatches them through `default_validator_runner`, and `src/scherzo/structured_output_json_schema.gleam` plus `src/scherzo/structured_output_command_validator.gleam` implement the two concrete runner paths.

The repository also already has a top-level workflow `contract` model. `src/scherzo/workflow_contract.gleam` parses workflow inputs, context, and outputs; `src/scherzo/workflow_run.gleam` accepts supplied mapped contract values through `execute_with_contract_values`; and `src/scherzo/workflow_contract_manifest.gleam` records those values in `workflow_contract_inputs` manifests. Ticket 0 must align future workstream handoffs to these existing contracts instead of introducing `phase_contract` as a second contract language.

The planning issue itself must stop after this document and the structured implementation pack are produced and validated. The later implementation issue must also stop at foundation alignment and must not proceed into Ticket 1 or any runtime workstream behavior.

## Strategy Overview

The chosen strategy is to make foundation reuse explicit and testable before any workstream runtime is built. Future workstream code should validate JSON artifacts by reusing `workflow_dag.StructuredOutputValidator` declarations and `structured_output.default_validator_runner`, not by adding artifact-specific validator enums or scripts hidden inside workstream modules. A missing validator configuration must fail closed so an unvalidated workstream artifact cannot become authoritative by accident.

For handoff-derived inputs, the plan treats a handoff as a source of existing `workflow_contract_manifest.ManifestValue` entries that populate `workflow_run.ContractRunValues`. The target workflow remains an ordinary workflow with a top-level `contract.inputs` entry whose source is `mapped_output`. When the workflow starts, existing workflow-run code records the supplied value in a `workflow_contract_inputs` manifest. This answers the Ticket 0 question without deciding the later handoff schema, ledger events, snapshot refs, or start command UX.

This approach is proportionate because it adds only the proof and any small reuse seam needed to prevent architectural drift. It defers artifact specs, workstream state, handoff emission, operator commands, gates, playbooks, and dogfood conversion to their own later tickets.

## Alternatives Considered

One alternative was to keep the parent UberPlan's proposed optional `phase_contract` block as the future workstream input/output declaration. That is rejected for Ticket 0 because the repository already has a top-level `contract` model that can express mapped inputs and retained outputs. Adding `phase_contract` now would create exactly the competing contract system this task asks us to avoid.

A second alternative was to introduce a workstream-specific validator registry with domain names such as handoff, decision, or input-bundle validators. That is rejected because the structured-output subsystem already supports JSON Schema and command validators generically. Workstream artifact types may choose validator declarations later, but the runtime boundary should stay generic.

A third alternative was to start implementing the workstream ledger, snapshot store, handoff artifacts, or start tooling while touching these foundations. That is rejected as unsafe scope creep. Ticket 0 should be a small guardrail that later tickets build on, not the beginning of a hidden one-shot implementation.

A fourth alternative was to make this planning ticket documentation-only and leave all proof to later work. That is insufficient for the later code ticket: the implementation pack requires concrete tests that falsify validator reuse, fail-closed behavior, and workflow-contract manifest visibility.

## Risks and Countermeasures

The main risk is architectural duplication. A future implementer might add a new phase-contract parser or workstream validator enum because the workstream feature feels separate from workflow execution. The countermeasure is to make the later code ticket add focused tests and helper names that point directly at `structured_output.default_validator_runner`, `workflow_dag.StructuredOutputValidator`, `workflow_run.ContractRunValues`, and `workflow_contract_manifest`.

A second risk is silent acceptance of unvalidated artifacts. If an artifact type has no configured validator, future workstream code could accidentally treat it as valid. The countermeasure is a stable fail-closed error kind for missing or unconfigured validators, covered by tests before any handoff or ledger feature exists.

A third risk is overstating what `ContractRunValues` proves. Mapping a handoff-derived value into a workflow input manifest proves representation and visibility, not durable workstream history. The countermeasure is to state that ledger replay, immutable snapshots, handoff schema validation, duplicate starts, and audit history remain Ticket 1+ work.

A fourth risk is implementation scope creep. The countermeasure is an explicit stop rule: once the four Ticket 0 acceptance tests pass and the standard validation gates are green, the later implementer must stop and must not begin Ticket 1.

## Scope Boundaries

In scope for this planning issue is exactly one new review document under `docs/plans/` and one structured implementation-pack submission. No runtime workstream source modules are implemented by LIV-244.

In scope for the later Ticket 0 implementation issue is only foundation alignment: tests and any minimal helper seam that let future workstream code call existing structured-output validators, fail closed without a configured validator, and map handoff-derived values into existing workflow contract supplied values. The later code may adjust existing foundation modules if needed, but it should avoid creating a workstream runtime namespace unless a tiny boundary module is the least invasive way to express the alignment.

Out of scope for both this planning issue and the later Ticket 0 implementation issue are the workstream runtime, workstream ledger, content-addressed snapshot store, handoff artifact schema, handoff emitter, start-from-handoff tooling, manual import tooling, read-only inspection CLI, human gates, playbooks, auto-enqueue, dogfood workflow conversion, and creation of all follow-up Linear tickets.

## Milestones

The first milestone for the later implementation is foundation inventory. The implementer confirms the current structured-output validator declarations and workflow contract model still match the inspected repository, then updates the plan if names or paths drifted.

The second milestone proves validator reuse. The code path used for future workstream artifacts validates a good artifact through an existing JSON Schema or command validator, rejects a bad artifact with a stable error kind, and refuses to validate when no validator is configured.

The third milestone proves contract reuse. A handoff-derived value is represented as a supplied workflow contract value, starts an ordinary workflow with `mapped_output`, and appears in the retained `workflow_contract_inputs` manifest without any phase-contract system.

The fourth milestone is the stop gate. The implementer runs the targeted tests plus the repository validation gates, records the result, and stops before Ticket 1+ work.

## Progress

- [x] (2026-05-17 00:00Z) Inspected the parent LIV-241 UberPlan sections relevant to validator reuse, workflow contracts, milestones, and follow-up Ticket 0 boundaries.
- [x] (2026-05-17 00:00Z) Inspected the current structured-output validator runner modules, workflow DAG parser, workflow contract parser, workflow contract manifest, workflow run contract handling, and ExecPlan v2 workflow YAML files named in LIV-244.
- [x] (2026-05-17 00:00Z) Drafted this LIV-244 Ticket 0 review document as a concise human-reviewable plan surface.
- [x] (2026-05-17 00:00Z) Prepared the structured implementation-pack content with detailed steps, tests, interfaces, dependencies, and verified facts for Scherzo tool capture.
- [x] (2026-05-17 00:00Z) Validated this review document with `scripts/scherzo-execplan-v2 validate-review-doc --path docs/plans/LIV-244-composable-workstreams-ticket-0-foundation-alignment.md`.
- [x] (2026-05-17 23:00Z) Reconfirmed during LIV-360 implementation that the repository still uses `workflow_dag.StructuredOutputValidator`, `structured_output.default_validator_runner`, top-level workflow `contract`, `workflow_run.execute_with_contract_values`, and `workflow_contract_inputs` manifests; no drift required plan changes.
- [x] (2026-05-17 23:00Z) Added `src/scherzo/workstream/foundation.gleam`, handoff JSON Schema fixtures, and `test/workstream_foundation_test.gleam` to prove valid and invalid workstream-adjacent artifacts reuse the existing JSON Schema validator path and fail closed when validators are unconfigured.
- [x] (2026-05-17 23:00Z) Added `handoff_derived_contract_values_are_recorded_in_input_manifest_test` in `test/workflow_run_test.gleam` to prove handoff-derived supplied values appear in the retained `workflow_contract_inputs` manifest without any `phase_contract` system.
- [x] (2026-05-17 23:00Z) Ran `direnv exec . gleam test` successfully (1342 passed) and re-ran formatting and lint gates, observing only the repository's pre-existing warning inventory and no new lint errors.
- [x] (2026-05-17 23:20Z) Review follow-up added focused coverage proving `ArtifactValidationSpec.required_keys` is forwarded to the existing `StructuredObjectSchema` baseline schema before configured validators.
- [x] (2026-05-17 23:30Z) Re-ran `direnv exec . gleam format --check src/scherzo/workstream/foundation.gleam test/workstream_foundation_test.gleam test/workflow_run_test.gleam` and `direnv exec . gleam test`; formatting stayed clean and the suite passed with 1343 tests.

## Decision Log

- Decision: Ticket 0 aligns future workstream code to existing structured-output validators instead of introducing workstream-specific validator cases.
  Rationale: The current repository already supports JSON Schema and command validators through `workflow_dag.StructuredOutputValidator` and `structured_output.default_validator_runner`, and the parent UberPlan explicitly warned against domain-specific validator sprawl.
  Date: 2026-05-17.

- Decision: Ticket 0 aligns handoff-derived inputs to the top-level workflow `contract` model instead of adding `phase_contract`.
  Rationale: `workflow_run.execute_with_contract_values` and `workflow_contract_inputs` manifests already provide a typed path for mapped inputs; adding another contract layer would duplicate behavior before the workstream runtime exists.
  Date: 2026-05-17.

- Decision: Missing or unconfigured workstream artifact validators must fail closed.
  Rationale: Future durable workstream artifacts should never become authoritative because validation was omitted by configuration or because an unsupported artifact type silently fell through.
  Date: 2026-05-17.

- Decision: The later implementer must stop before Ticket 1+.
  Rationale: LIV-244 is intentionally a bounded foundation-alignment slice replacing a broad implementation handoff.
  Date: 2026-05-17.

- Decision: The Ticket 0 proof uses a tiny `src/scherzo/workstream/foundation.gleam` seam instead of editing future runtime modules that do not exist yet.
  Rationale: A small wrapper around `structured_output.validate_final_response` makes validator reuse explicit, gives the fail-closed case a stable workstream-specific error code, and avoids introducing any ledger, snapshot, handoff-runtime, or start-tooling behavior.
  Date: 2026-05-17.

## Outcomes & Retrospective

Ticket 0 stayed bounded after review. The added foundation seam still only wraps existing structured-output and workflow-contract machinery, and the follow-up test closed the main medium finding by proving `required_keys` is enforced before the configured validator runs. A fresh post-review test pass kept the slice green without pulling in any Ticket 1 runtime behavior.

## Validation and Acceptance

This planning issue is accepted when this single Markdown review document exists under `docs/plans/`, the ExecPlan v2 review-doc validator accepts it, and Scherzo captures the structured implementation-pack submission. The working tree must not contain workstream runtime implementation modules as a side effect of the planning issue.

The later implementation issue is accepted only when it proves the four Ticket 0 behaviors: a valid artifact validates through an existing JSON Schema or command validator, an invalid artifact fails with a stable error kind, a missing or unconfigured validator fails closed, and handoff-derived inputs are visible in a `workflow_contract_inputs` manifest through the existing workflow contract model. The detailed test names, file paths, commands, and commit map are intentionally stored in the structured implementation pack rather than repeated in this review document.

## Rollout, Recovery, and Idempotence

This planning change is additive. If the review document is rejected, remove or revise `docs/plans/LIV-244-composable-workstreams-ticket-0-foundation-alignment.md` and resubmit the structured pack. Re-running the review-doc validator is idempotent.

The later implementation should also be additive. It should add tests first, add only the minimal reuse seam needed for those tests, and avoid changing existing workflow behavior for workflows that do not opt into supplied contract values or structured-output validators. If the implementation begins to require a ledger, snapshot store, handoff schema, or new workflow YAML contract, that is a scope failure; back out the extra work and split it into Ticket 1+ planning.

## Open Questions and Clarifications Needed

No blocking clarification is needed for Ticket 0. A later Ticket 1+ plan still needs to decide where workstream artifact validator declarations live, what the handoff schema looks like, how immutable snapshot refs are stored, and how operators invoke start-from-handoff behavior. Those questions are intentionally deferred and must not be answered by implementing runtime workstream features in Ticket 0.
