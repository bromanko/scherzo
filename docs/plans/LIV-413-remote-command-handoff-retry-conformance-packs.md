# Draft remote command and handoff retry conformance packs

This ExecPlan v2 review document is the human review surface for LIV-413. It plans future optional conformance packs only; exact implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo's tracker conformance suite now has a black-box foundation for `task_source` plus later optional side-effect packs. The next useful boundary is a plan for remote-command and handoff packs that prove operator-visible retry behavior instead of treating acknowledgements and handoff reports as best-effort side effects.

After the later implementation tickets complete, an adapter author will be able to opt into remote-command and handoff packs only when their manifest both requests the pack and claims the matching capability. Reports will show whether command fetches, acknowledgements, handoff reports, retries, duplicate handling, probes, cleanup, and redaction behaved as promised.

## Problem Framing and Constraints

Remote-command and handoff behavior is not part of the read-only `task_source` MVP. Remote-command fetches have event identity, `since_event_ids`, per-task limit, and normalized field requirements, while acknowledgement posting is an operator-visible write that may be retried after Scherzo restarts. Handoff reports are also operator-visible writes, and runtime compatibility still includes claim, success, failure, park, and legacy event variants.

This issue must not implement the packs. It must draft a reviewable plan and capture the implementation details for later tickets. The later work must reuse the black-box driver boundary, fixture/probe/cleanup separation, redacted reports, and optional capability gating already used by the comments, state-transition, and routing packs.

## Strategy Overview

Add two optional packs behind the existing profile model: `remote_commands` and `handoff`. Each pack runs only when `profile.requested_packs` names it and `profile.capabilities` contains the required capability names; requesting a pack without the matching claimed capability fails manifest validation before setup, probes, cleanup, or driver operations run.

The remote-command pack should add public driver operations for `remote_commands.fetch_events` and `remote_commands.post_ack`. Fetch cases should cover multiple fixture tasks, stable event ids, event-task identity, empty and populated `since_event_ids`, `limit_per_task`, and normalized fields such as author, body, command name, excerpt, and observed time. Acknowledgement cases should post a visible acknowledgement, retry the same event, verify the declared duplicate or update behavior through probes, and make acknowledgement failures visible in the report.

The handoff pack should add a public `handoff.report` operation with generic claim, success, failure, and park cases. If the current adapter contract still requires legacy handoff event variants, the pack should also exercise legacy claim, success, failure, and park events with issue-shaped fixtures. Retry cases should send the same task, run id, and event class twice and verify through probes and report summaries that the backend-visible result is either idempotently updated/de-duplicated or explicitly classified as duplicate-visible according to the manifest's declared retry behavior.

## Alternatives Considered

One alternative is to fold remote-command fetches into `task_source` because both are reads. That is rejected because remote-command fetches use event identity, event cursors, per-task limits, and command-parser input fields that are unrelated to task snapshots.

A second alternative is to treat acknowledgement and handoff retries as documentation-only behavior. That is insufficient because Scherzo persists remote-command acknowledgements and can retry handoff-like reporting paths; conformance must prove visible retry outcomes with deterministic fake drivers and probes.

A third alternative is to require every adapter to be exactly-once for acknowledgements and handoff reports. That is too strict for existing compatibility paths. The plan instead requires explicit declared retry behavior and tests that make duplicates, updates, failures, and cleanup visible.

## Risks and Countermeasures

The main risk is accidental mutation of real tracker data. Counter this with isolated fixture tasks, unique run markers in acknowledgement and handoff bodies, mandatory cleanup guidance for side-effect manifests, and support-failure counters that remain visible when cleanup or probes fail.

A second risk is hiding duplicate acknowledgements or handoff reports. Counter this with retry cases that execute the same event twice, probes that count backend-visible markers, and report summaries that distinguish idempotent update/de-duplication from duplicate-visible behavior.

A third risk is confusing privileged probes with adapter conformance. Counter this by keeping setup, probe, and cleanup namespaces out of `profile.adapter_operations`; probes may confirm visibility, but only driver operations satisfy public cases.

A fourth risk is leaking command bodies, handoff summaries, run ids, workspace paths, or tokens in reports. Counter this with configured redaction applied to request transcripts, response transcripts, diagnostics, probe output, hook output, and CLI summaries, plus tests that seed secret markers into every path.

A fifth risk is letting review feedback live only in this prose document while the structured implementation pack still omits acceptance evidence, docs/helper boundaries, dogfood timing, provider-live/cache non-scope, or full lint gates. Counter this by mirroring those obligations in the pack's concrete steps and testing notes before Scherzo materializes follow-up implementation artifacts.

## Scope Boundaries

In scope for LIV-413 is exactly this Markdown review document and one structured implementation-pack submission. No conformance runner code, canonical bundle file, production adapter change, real tracker mutation, or optional pack implementation belongs in this issue.

The first follow-up implementation ticket should add shared protocol/profile/schema support for remote-command and handoff pack declaration, requested-pack validation, operation names, payload and response shapes, fixture declarations, and retry-policy declaration.

The second follow-up implementation ticket should implement the remote-command pack, fake-driver fixtures, probes, cleanup, redaction tests, and report classifications for fetch, acknowledgement, acknowledgement retry, duplicate handling, and acknowledgement failure visibility.

The third follow-up implementation ticket should implement the handoff pack, including generic claim/success/failure/park events, legacy variants if the adapter contract still exposes them, retry visibility probes, cleanup, and defective fake-driver cases.

The fourth follow-up implementation ticket should update adapter-author documentation, runbook examples, fake-driver dogfood commands, final repository validation gates, and a docs/helper migration inventory. That inventory must either prove no `.scherzo/workflows/scripts/*`, workflow schemas, or provider-facing helper contracts need to change for these packs, or name the exact helper/doc tests that validate any intentional helper edit.

Scheduled-failure dedupe, new transport types, production Linear behavior changes, unrelated workflow-helper rewrites, and provider-live/cache behavior remain out of scope. The conformance runner must continue issuing fresh black-box driver requests and must not introduce a cache; if a future ticket intentionally adds caching, that work must be split and accepted only with stale-read, invalidation, and TTL-disabling tests.

## Milestones

Milestone 1 establishes the shared optional-pack contract. Reviewers should see schemas, typed profile names, capability names, operation names, request/response payload types, and manifest validation proving `task_source` remains unchanged, claimed-but-unrequested remote-command or handoff capabilities select no extra cases, and requested-but-unclaimed packs fail before side effects.

Milestone 2 delivers remote-command fetch conformance. Reviewers should see deterministic fake-driver cases for stable event identity, task identity, empty and populated `since_event_ids`, `limit_per_task`, normalized event fields, malformed events, and driver failure reports, with `direnv exec . gleam test` showing the new fetch cases pass.

Milestone 3 delivers remote-command acknowledgement conformance. Reviewers should see acknowledgement receipt validation, a same-event retry case, duplicate/update visibility probes, defective duplicate behavior, explicit acknowledgement failure visibility, redacted transcripts, and idempotent cleanup evidence before the pack is accepted.

Milestone 4 delivers handoff report conformance. Reviewers should see generic claim, success, failure, and park report cases; legacy event cases when still required; same-run retry visibility checks; defective fake-driver cases; probe and cleanup failure classifications; and redaction of workspace paths, summaries, reasons, and secret markers.

Milestone 5 completes documentation, helper inventory, and gates. Reviewers should see `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, adapter-author examples, fake-driver dogfood commands that write sanitized reports under `test/tmp/tracker-conformance/`, schema fixtures, an explicit docs/helper migration inventory, `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` all passing. If a follow-up implementation touches `.scherzo/workflows/scripts/*`, workflow schemas, or provider-facing helper contracts, reviewers should also see the relevant helper or contract tests; otherwise the evidence should explicitly state that provider-live/cache behavior and helper contracts were not changed.

## Progress

- [x] (2026-05-23) Read the LIV-413 brief, ExecPlan authoring guidance, prior conformance review documents, the tracker conformance protocol, current conformance modules, and the tracker adapter remote-command and handoff contracts.
- [x] (2026-05-23) Drafted this concise review document under `docs/plans/`.
- [x] (2026-05-23) Prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-23) Validated this review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-413-remote-command-handoff-retry-conformance-packs.md`.
- [x] (2026-05-23) Incorporated review feedback by making acceptance evidence, milestone specificity, test obligations, pre-publish fake-driver dogfood, deferred live-backend checks, docs/helper inventory, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Remote commands and handoff are planned as optional packs, not extensions of the `task_source` MVP.
  Rationale: They have side effects, retry behavior, visibility probes, and report classifications that are materially different from task reads.
  Date: 2026-05-23.

- Decision: Retry behavior must be tested as a first-class conformance case.
  Rationale: Remote-command acknowledgements and handoff reports are retried by runtime recovery paths; duplicate or missing visible effects must be observable rather than hidden as best-effort behavior.
  Date: 2026-05-23.

- Decision: Retry outcomes may be idempotent update/de-duplication or duplicate-visible only when the manifest explicitly declares that behavior and probes verify it.
  Rationale: Existing compatibility paths may be at-least-once, but conformance should still force adapters to prove and report the real operator-visible outcome.
  Date: 2026-05-23.

- Decision: Legacy handoff event coverage is conditional on the current adapter contract still exposing legacy variants at implementation time.
  Rationale: The current spec includes legacy events for Linear compatibility, but the conformance pack should not preserve legacy cases longer than the public adapter contract requires them.
  Date: 2026-05-23.

- Decision: Treat review feedback about evidence, tests, dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations.
  Rationale: The workflow materializes follow-up implementation instructions from the structured pack, so prose-only obligations would be easy for later implementers to miss.
  Date: 2026-05-23.

## Validation and Acceptance

This planning issue is accepted when this file exists at `docs/plans/LIV-413-remote-command-handoff-retry-conformance-packs.md`, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-413-remote-command-handoff-retry-conformance-packs.md` exits zero with `REVIEW_DOC_VALID=ok`, and Scherzo captures the structured implementation-pack submission for LIV-413. In the packaged workflow, the same validator is invoked as `scripts/scherzo-execplan validate-review-doc` from the workflow bundle.

The later implementation is accepted only with concrete evidence for every required behavior. Remote-command fetch evidence must include tests and report artifacts for event identity, task identity, `since_event_ids`, `limit_per_task`, normalized event fields, malformed event failures, and redacted command bodies. Remote-command acknowledgement evidence must include tests and report artifacts for successful posting, same-event retry, duplicate/update handling, normalized receipt validation, acknowledgement failure visibility, probe failures, cleanup failures, and redaction.

Handoff evidence must include tests and report artifacts for claim, success, failure, park, legacy events if still required, same-run retry visibility, duplicate/update classification, defective fake-driver failures, probe failures, cleanup failures, and redaction. Capability evidence must include tests proving requested-and-claimed packs run, claimed-but-unrequested packs select no optional cases, and requested-but-unclaimed packs fail manifest validation before any side-effect setup, probe, cleanup, or driver command.

Repository validation evidence must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. Deterministic fake-driver dogfood evidence must complete before publish by running the documented manifests and confirming stdout plus retained JSON reports show the new classifications without raw secret markers. Optional real-backend manual evidence is deferred to a human/operator after implementation and must record only redacted report excerpts.

Docs/helper evidence must include updates to `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, and any adapter-author examples that describe these packs. It must also include a helper inventory: if `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files are changed, run the relevant helper or offline contract tests and preserve provider-live/cache semantics; if they are not changed, record that no helper migration or provider-live/cache validation was applicable. Provider-live/cache behavior is accepted as unchanged for this plan unless a future split ticket intentionally adds a cache, in which case stale-read, invalidation, and TTL-disabling tests become mandatory before acceptance.

## Rollout, Recovery, and Idempotence

Rollout should be additive. Existing `task_source`, comments, state-transition, routing, CLI, and HTTP conformance behavior remains unchanged, and remote-command or handoff packs default to not selected unless requested and claimed.

If a follow-up implementation discovers it must alter workflow helpers, provider-facing structured-output contracts, provider-live checks, or cache behavior, that change should be split or explicitly rolled back before publishing these packs. The safe default is no helper migration beyond docs and no provider-live/cache behavior change.

Recovery for this planning issue is to revise or remove only this Markdown file and resubmit the structured pack. Recovery for later implementation is to omit the new packs from manifests or revert the specific pack ticket while keeping the established conformance runner and existing optional packs intact.

Idempotence is an acceptance requirement for repeatable test runs. Fake fixtures must use unique run markers and cleanup hooks that can run after success or failure. Where a backend cannot update or de-duplicate, the manifest and report must classify the retry as duplicate-visible; silent duplicate creation or missing retry visibility fails conformance.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this planning issue. During implementation, reviewers should confirm the final public spelling of remote-command and handoff capability names, whether retry policy is represented as capabilities or manifest fields, and whether legacy handoff variants are still required by the public adapter contract.
