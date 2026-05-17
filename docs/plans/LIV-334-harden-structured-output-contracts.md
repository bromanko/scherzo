# Harden structured-output contracts

This ExecPlan v2 review document frames the implementation plan for LIV-334. Mechanical file-by-file steps, test commands, and commit boundaries are retained in Scherzo's structured implementation pack and canonical bundle.

## Purpose / Big Picture

After this change, Scherzo's production structured-output path is explicit, provider-safe, and easy to audit. Workflow authors declare structured output with `source.type: pi_tool_call`, a named tool, a provider-facing parameters schema, exactly-one matching-call behavior, and downstream canonical validation. The older final-response extraction path is not kept as a production compatibility mode.

The visible result is a single generic structured-output contract gate that inventories production workflow structured-output blocks and verifies source policy, provider schema safety, prompt/tool consistency, downstream validator alignment, and materialization conventions. Review-lane-specific checks should reuse that foundation instead of carrying a second schema allowlist.

## Problem Framing and Constraints

Scherzo already has a working tool-call structured-output mechanism, but the surrounding contract is still too broad. The parser can default a missing `structured_output.source` to a final-response source, stale references to final-response JSON remain easy to preserve accidentally, provider-schema keyword allowlists exist in more than one place, and the generic Pi extension still contains rich-schema sanitization and partial validation behavior that should not be the production trust boundary.

The hard constraint is to keep the production boundary simple. Provider-facing schemas must be a narrow subset that providers can safely accept. Canonical JSON Schema validation and semantic validation remain Scherzo-side responsibilities. The Pi extension should register exactly the provider-safe schema Scherzo generated and return a receipt; it should not become a second implementation of Scherzo validation.

This work is also constrained by existing review-lane workflows. The migration must preserve review-lane offline checks and SelfCI coverage while removing duplicated provider-schema policy and retiring the legacy review-lane Pi extension rollback shim only after current workflows no longer require it.

## Strategy Overview

First make the parser strict: production workflow `structured_output` blocks must include an explicit `source` and that source must be `pi_tool_call`. Tests should prove omitted sources and explicit `final_response` sources are rejected with stable diagnostics, while existing pi-tool-call failure cases remain covered.

Second, introduce a reusable structured-output contract policy and CLI. The shared policy defines provider-safe schema keywords and source-policy checks in one place. Runtime tool-spec generation and review-lane contract checks both call into this shared policy. The CLI can check one schema, one workflow, or all production workflows and can write deterministic JSON reports under `tmp/`.

Third, simplify the generic Pi structured-output extension. It should validate the spec envelope, register the exact provider-safe parameters schema supplied by Scherzo, require object-valued tool arguments, emit a compact receipt with `remote_mutations: none`, and terminate when configured. Rich-schema sanitization and execute-time partial JSON Schema validation should be removed from the extension.

Finally, remove the legacy review-lane extension shim and update documentation and SelfCI so operators have one obvious local gate for structured-output contracts.

## Alternatives Considered

One alternative is to keep `final_response` as a compatibility source while recommending `pi_tool_call` for new workflows. That was rejected because the goal is production strictness; preserving the old path would keep prompt-only JSON extraction as a supported contract and make future audits ambiguous.

A second alternative is to let each subsystem keep its own provider-schema allowlist. That was rejected because duplicated allowlists drift. Runtime tool-spec generation, generic workflow checks, and review-lane checks should all use the same policy and diagnostics.

A third alternative is to make the Pi extension responsible for canonical validation. That was rejected because Scherzo already owns canonical schema validation, semantic validation, artifacts, and retry semantics. The extension should stay a provider tool adapter, not the authoritative validator.

## Risks and Countermeasures

The main risk is breaking workflows by rejecting an implicit source that was previously accepted. The countermeasure is to inventory production workflow structured-output blocks first, add red tests for omitted and final-response sources, and migrate or remove stale fixtures deliberately.

A second risk is over-restricting provider schemas and blocking legitimate future schemas. The countermeasure is to define the provider-safe subset explicitly and make failures actionable with schema paths and JSON-pointer-like locations. Rich canonical schemas remain allowed downstream; only provider-facing schemas are narrow.

A third risk is accidentally deleting review-lane behavior that is still needed. The countermeasure is to keep review-lane materialization and semantic checks in the review-lane contract layer while delegating only generic source, prompt, schema, and alignment checks to the new contract foundation. The legacy extension shim is removed only after generic workflow checks and offline review-lane checks pass.

A fourth risk is confusing `final_response` workflow output fields with `structured_output.source: final_response`. The implementation must preserve ordinary workflow-output field names where they are part of contract output mapping, while rejecting final-response extraction as a production structured-output source.

## Scope Boundaries

In scope are parser/runtime strictness for production `structured_output.source`, a generic structured-output contract policy and CLI, migration of provider-schema checks onto that policy, simplification of `.pi/extensions/scherzo-structured-output`, removal of the legacy review-lane extension shim when proven unused, documentation updates, and SelfCI integration guidance.

Out of scope are adding provider-native structured-output APIs, adding a new dependency such as PyYAML, changing the canonical review-lane draft schema, changing Linear/GitHub side effects, or implementing the structured-output hardening directly in this plan-authoring issue. The generated implementation task should perform code changes; this issue produces the reviewed plan and bundle.

## Milestones

First, lock down the source-policy contract with tests and parser changes so production structured output requires explicit `pi_tool_call`.

Second, extract provider-schema safety and workflow source checks into a generic contract policy and CLI with deterministic reports.

Third, move runtime tool-spec provider checks and review-lane provider checks onto the generic policy so there is no duplicated allowlist.

Fourth, simplify the generic Pi structured-output extension to provider-safe schema registration and receipt emission only.

Fifth, remove the legacy review-lane extension shim and stale references after the generic checks prove current workflows no longer require it.

Sixth, update operator documentation and SelfCI so the new generic contract gate is part of routine validation.

## Progress

- [x] (2026-05-16) Drafted the human-reviewable ExecPlan v2 review document for LIV-334.
- [x] (2026-05-16) Incorporated review feedback; no Markdown intent changes were required.
- [ ] Implementation pack to be consumed by Scherzo's canonical bundle generator.
- [ ] Code implementation and validation not yet started.

## Decision Log

- Decision: Production structured outputs must use explicit `source.type: pi_tool_call`; omitted source and `source.type: final_response` are rejected.
  Rationale: The hardening work should remove prompt-only final-response JSON extraction as a production contract, not preserve it as a fallback.
  Date: 2026-05-16

- Decision: Keep exactly-one matching structured-output tool-call behavior as the production default.
  Rationale: The existing single-call contract is easy to reason about and prevents ambiguous structured artifacts.
  Date: 2026-05-16

- Decision: Use one generic provider-schema policy for runtime tool-spec generation, workflow contract checks, and review-lane contract checks.
  Rationale: Duplicated allowlists drift and make provider-facing safety hard to audit.
  Date: 2026-05-16

- Decision: Simplify the generic Pi extension rather than making it a canonical validator.
  Rationale: Scherzo owns canonical validation, artifacts, retries, and semantic checks; the extension should adapt provider tool calls and emit receipts.
  Date: 2026-05-16

- Decision: Retire the legacy review-lane extension shim after current workflows pass the generic contract gate.
  Rationale: Keeping a rollback-only extension after the generic path is established increases maintenance and audit surface.
  Date: 2026-05-16

## Validation and Acceptance

Acceptance requires a complete implementation plan and canonical bundle that make the strict `pi_tool_call` direction unambiguous. The implementation described by the bundle is accepted only when parser tests reject omitted source and explicit final-response source, pi-tool-call success and failure tests still pass, provider-schema policy tests reject disallowed keywords at nested locations, workflow contract checks detect prompt/tool/schema/materialization mismatches, review-lane offline checks still pass through the generic contract foundation, the generic extension no longer performs rich-schema sanitization or execute-time partial validation, and the legacy review-lane extension shim is no longer required by current workflows.

Final validation should include formatting, the full Gleam test suite, glinter, Scherzo-specific lint, the generic structured-output contract gate over production workflows, review-lane offline contract checks for implementation and ExecPlan implementation workflows, and the raw-schema smoke for the simplified generic extension. The contract reports must be local-only with `remote_mutations: none`.

## Rollout, Recovery, and Idempotence

The rollout is additive until the parser strictness lands. Add the generic contract gate and run it against production workflows before deleting fallback code. Once strict parsing is enabled, workflows that still omit source or request final-response extraction should fail at config validation rather than at runtime.

Recovery is straightforward because the change does not mutate remote systems. If a workflow fails after strictness lands, operators can inspect the contract report and either add the missing `pi_tool_call` source fields or fix the provider schema/prompt/materialization mismatch. Generated reports and smoke transcripts are written under `tmp/` and can be deleted safely.

The contract command should be idempotent: repeated runs over the same tree write equivalent reports. Extension receipts should remain deterministic for the same run, step, attempt, artifact, and schema metadata, and must continue to state that no remote mutations occurred.

## Open Questions and Clarifications Needed

No unresolved MVP questions remain. Future work may revisit provider-native structured-output APIs, but this plan intentionally hardens the current Pi tool-call mechanism first.
