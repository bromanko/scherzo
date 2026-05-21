# Draft optional tracker conformance packs for comments, state transitions, and routing

This ExecPlan v2 review document is the human review surface for LIV-412. It plans later optional tracker conformance packs only; exact implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo already has a black-box conformance MVP for the required read-only `task_source` profile. The next useful capability is a safe plan for optional packs that prove comment writes, state transitions, and routing metadata without letting side effects leak across test runs or treating privileged probes as adapter behavior.

After the later implementation tickets complete, an adapter author will be able to opt into only the packs their backend claims. The runner will create clear reports showing which public adapter cases passed, which side-effect fixture or cleanup paths failed, and which optional cases were not run because they were not requested or not claimed.

## Problem Framing and Constraints

The current MVP intentionally avoids writes. Comments and state transitions mutate real tracker data, while routing metadata needs backend visibility checks for labels and blockers. These features need stronger fixture safety, cleanup rules, probe boundaries, and report classification than the read-only `task_source` pack.

This issue must not implement those packs. It must produce a reviewable plan and a captured implementation pack that future implementation tickets can follow. The plan must preserve the existing `task_source` behavior, keep optional packs capability-gated, and avoid any contract where setup, cleanup, or probe commands count as adapter-under-test operations.

## Strategy Overview

Extend the conformance profile model with a clear separation between claimed capabilities and requested packs. `task_source` remains the required default. Optional packs run only when the manifest requests the pack, the adapter claims the matching capability, and the public operation declarations allow the case. A claimed but unrequested optional capability does not select any extra cases; the report should show only the selected packs that actually ran. A requested but unclaimed optional pack is a manifest validation error before setup hooks, probes, cleanup hooks, or driver operations can run.

Plan comments as the first side-effect pack because it exercises create-only writes, update-existing writes, and allow-create-fallback behavior against one isolated task. Plan state transitions as a second side-effect pack with isolated tasks, target id/name coverage, reason propagation where claimed, and normalized receipt/error checks. Plan routing metadata as a read-mostly pack that compares adapter-returned normalized task labels and blocker refs against fixture expectations and backend probes, while keeping those probes outside `profile.adapter_operations`.

## Alternatives Considered

One alternative is to add all optional packs to the MVP runner immediately. That is too risky because comments, transitions, and routing have different fixtures, cleanup paths, and failure reports.

A second alternative is to let probes verify side effects and call that conformance. That is rejected because probes are privileged backend checks; they can confirm fixture state, but only driver operations can prove adapter behavior.

A third alternative is to make every optional feature a separate standalone runner. That would duplicate transport, manifest, fixture, and report code. Separate packs behind one conformance boundary are smaller and keep reports comparable.

## Risks and Countermeasures

The main risk is damaging real tracker data. Counter this with isolated fixture tasks, unique run markers in all write bodies and reasons, mandatory cleanup hooks for side-effect manifests, and cleanup-failure report counters that remain visible even when adapter cases pass.

A second risk is capability drift where optional cases run by accident. Counter this with manifest validation that distinguishes requested packs from claimed capabilities, plus tests proving a claimed-but-unrequested optional pack produces no optional cases, and a requested-but-unclaimed optional pack fails manifest validation before any side-effect setup, probe, cleanup, or driver command can run.

A third risk is confusing fixture or probe failures with adapter failures. Counter this by keeping setup, probe, and cleanup namespaces reserved, banning them from adapter operation declarations, and reporting `setup_failed`, `probe_failed`, and `cleanup_failed` separately from public case failures.

A fourth risk is ambiguous write semantics. Counter this by requiring normalized receipts for successful comment and transition operations and normalized errors for unsupported update, missing comment, unknown target state, wrong backend, and malformed receipt cases.

## Scope Boundaries

In scope for LIV-412 is exactly this review document and one structured implementation-pack submission. No conformance runner implementation, canonical bundle file, optional pack code, live tracker mutation, or production adapter change belongs in this issue.

The first follow-up implementation ticket should add the protocol and profile foundation for optional pack declaration, requested-pack selection, granular capability names, new request and response payload shapes, and backwards-compatible manifest validation.

The second follow-up implementation ticket should add the comments conformance pack, fake-driver fixtures, side-effect probes, cleanup hooks, and report classifications for create-only, update-existing, fallback-create, and expected error cases.

The third follow-up implementation ticket should add the state-transition conformance pack, including target id/name behavior, reason propagation where claimed, normalized receipts, normalized errors, state restoration cleanup, and defective fake-driver modes.

The fourth follow-up implementation ticket should add the routing metadata conformance pack for workflow labels and blocker refs, with probes used only as support evidence and never as adapter operations.

Out of scope are remote-command acknowledgement retry, handoff, scheduled-failure dedupe, HTTP service supervision beyond existing transport work, and any requirement that optional packs run against a live third-party tracker by default.

## Milestones

Milestone 1 establishes the optional-pack contract. Reviewers should see manifest/schema/type support for claimed capabilities and requested packs, existing `task_source` fixtures still passing, and deterministic gating tests: `claimed-comments-not-requested` runs only the selected `task_source` pack with no comment cases, while `invalid-requested-comments-without-capability` fails manifest validation with an error naming the requested pack and missing capability before side-effect commands are invoked.

Milestone 2 delivers the comments pack. Reviewers should see a fake external driver pass create-only, update-existing, and allow-create-fallback cases; defective fake modes should produce normalized errors or receipt failures; reports should classify setup, probe, cleanup, and adapter failures separately.

Milestone 3 delivers the state-transition pack. Reviewers should see target-state-id precedence, name-only resolution, reason propagation when claimed, normalized receipts, expected target errors, and cleanup that restores the fixture task to its original state.

Milestone 4 delivers the routing metadata pack. Reviewers should see workflow label and blocker-ref cases derived from adapter-returned normalized tasks, backend probes recorded as support evidence only, and manifest validation rejecting any attempt to list probe operations as adapter operations.

Milestone 5 hardens documentation and gates. Reviewers should see adapter-author guidance for safe fixtures and cleanup, deterministic fake-driver examples for every optional pack, and full validation output from the repository test, format, and lint commands.

## Progress

- [x] 2026-05-21: Read the LIV-412 brief, the LIV-365 review document, the tracker conformance protocol spec, current conformance modules, and tracker adapter capability definitions.
- [x] 2026-05-21: Drafted this concise review document under `docs/plans/`.
- [x] 2026-05-21: Prepared the structured implementation-pack submission for Scherzo capture.
- [x] 2026-05-21: Validated this review document with `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-412-comments-state-transitions-routing-conformance-packs.md`.
- [x] 2026-05-21: Incorporated review feedback by aligning the acceptance helper path with the runnable workflow helper and making requested-but-unclaimed optional packs manifest validation errors before side effects.

## Decision Log

- Decision: Optional packs use separate requested-pack and claimed-capability concepts.
  Rationale: This prevents accidental side effects while still letting adapter authors advertise capabilities before selecting conformance packs.
  Date: 2026-05-21.

- Decision: Comment and state-transition packs require isolated fixtures, probes, and cleanup before they can run against a real backend.
  Rationale: These packs mutate tracker data, so safe recovery and report visibility are part of the conformance contract.
  Date: 2026-05-21.

- Decision: Routing metadata cases must not introduce probe operations into the adapter operation namespace.
  Rationale: Workflow labels and blocker refs should be verified from normalized task data returned by public adapter operations, with probes only confirming fixture truth.
  Date: 2026-05-21.

- Decision: Deliver comments, state transitions, and routing as separate implementation-ticket boundaries after the shared profile foundation.
  Rationale: Each pack has different side effects, fixtures, cleanup, and failure modes; separate tickets keep review and rollback small.
  Date: 2026-05-21.

- Decision: Treat a requested optional pack without its matching claimed capability as a manifest validation error before setup, probe, cleanup, or driver operations.
  Rationale: A single fail-fast outcome is easier to test and prevents an ambiguous unsupported-report path from mutating tracker fixtures by accident.
  Date: 2026-05-21.

## Validation and Acceptance

This planning issue is accepted when this Markdown file exists at `docs/plans/LIV-412-comments-state-transitions-routing-conformance-packs.md`, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-412-comments-state-transitions-routing-conformance-packs.md` exits zero with `REVIEW_DOC_VALID=ok`, and Scherzo captures the structured implementation-pack submission for LIV-412.

The later implementation is accepted only when existing `task_source` manifests still run unchanged; claimed-but-unrequested optional capabilities select no optional cases; requested-but-unclaimed optional packs fail manifest validation before setup, probe, cleanup, or driver commands; requested-and-claimed comments, state-transition, and routing packs run; fake-driver pass manifests produce passing reports; defective fake-driver manifests produce expected negative/error-path report classifications; and side-effect manifests leave observable cleanup evidence with no duplicate marker data after reruns.

The implementation-ticket gates are protocol/profile compatibility first, comments side-effect coverage second, state-transition side-effect coverage third, routing metadata probe-boundary coverage fourth, and documentation plus full validation last. Final evidence must include `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` output.

## Rollout, Recovery, and Idempotence

Rollout should be additive. Existing read-only conformance manifests keep their current behavior, optional packs default to not selected, claimed-but-unrequested optional capabilities do not run extra cases, requested-but-unclaimed optional packs fail fast during manifest validation, and all new examples should use fake drivers before any real tracker fixture is documented.

Recovery for this planning issue is to revise or remove only this Markdown file and resubmit the structured pack. Recovery for later implementation is to omit optional packs from the manifest or revert the specific pack ticket while keeping `task_source` intact.

Side-effect reruns must be idempotent from the operator perspective. Comments and transition reasons should include unique run markers, cleanup hooks should delete or restore every marker they create, and reports should make leftover fixture data visible as cleanup or probe failures rather than silent adapter success.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this planning issue. Later implementation review should confirm the exact public spelling of requested-pack and granular capability names before freezing the manifest schema.
