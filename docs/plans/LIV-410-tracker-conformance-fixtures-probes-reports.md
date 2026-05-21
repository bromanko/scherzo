# Harden tracker conformance fixtures, probes, and reports

This ExecPlan v2 review document is the human review surface for LIV-410. It plans hardening work after the LIV-406 `task_source` MVP; exact implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo's tracker conformance suite should be safe to run against real tracker backends, not only fake in-repository drivers. After the later implementation tickets complete, an adapter author or operator will be able to declare pre-provisioned fixture tasks, run optional privileged setup, cleanup, and backend probes without confusing them with adapter-under-test behavior, and receive a report that explains what failed without leaking secrets.

The visible result is a richer conformance report: each adapter case retains its case id, operation name, and request id, adds expected and actual summaries, includes redacted bounded request and response transcripts, separates setup, probe, and cleanup failures from conformance failures, and gives recovery guidance that tells an operator whether to fix fixtures, probes, cleanup, or adapter behavior.

## Problem Framing and Constraints

LIV-406 proved the CLI `task_source` path and already covers part of this scope. The current tree has a manifest `fixtures.task_file`, optional setup and cleanup hooks, optional probes, rejection of fixture, probe, and hook names inside `profile.adapter_operations`, separate `setup_failed`, `probe_failed`, and `cleanup_failed` counters, configured redaction for reports and summaries, truncated external diagnostics, and basic report fields for case ids, operation names, request ids, and aggregate counts.

What remains after LIV-406 is narrower than the original hardening idea. The remaining work is to make fixture declarations more explicit than a task-list response file, strengthen the hook and probe safety contract, capture redacted bounded request and response transcripts, add expected and actual summaries, and add recovery guidance to reports and documentation. This plan must not reimplement the MVP runner, change Scherzo's runtime tracker adapter contract, or treat privileged fixture and probe operations as conformance evidence.

## Strategy Overview

The implementation should first lock the LIV-406 baseline with regression tests and documentation so existing CLI `task_source` behavior remains unchanged. Then it should add explicit fixture task declarations around the existing fixture file model, using named fixture records that describe the pre-provisioned task identities and operator references the public cases expect.

Setup, cleanup, and probe commands should stay optional and privileged. Their results should continue to be counted outside adapter case failures, and manifests should make that privilege boundary visible in names, schemas, docs, and reports. Cleanup should be attempted after setup, probes, and cases whenever a cleanup hook is declared, and every hook or probe diagnostic should be bounded and redacted before it reaches a report or stdout summary.

Report hardening should be additive. Keep the existing fields reviewers already rely on, then add redacted transcript evidence, expected and actual summaries, aggregate-count structure, and recovery guidance. The fake-driver fixtures should prove both the success path and the negative paths: setup failure, probe failure, cleanup failure, adapter failure, malformed or missing driver output, and configured secret leakage attempts.

## Alternatives Considered

One alternative is to close this follow-up because LIV-406 already implemented hooks, probes, classification, and basic redaction. That is too optimistic: the MVP still lacks explicit fixture declarations, request and response transcripts, expected and actual summaries, and recovery guidance.

A second alternative is to combine this work with optional side-effect packs such as comments, state transitions, remote commands, handoff, or scheduled failures. That is too broad. Those packs need their own backend side-effect semantics and should consume the hardened fixture, probe, and report foundation rather than define it.

A third alternative is to allow backend probes to prove conformance when adapter cases are hard to observe. That is rejected because it would collapse the black-box boundary. Probes may explain fixture or backend readiness, but only adapter-under-test operations can satisfy conformance cases.

## Risks and Countermeasures

The main risk is leaking secrets through transcripts or diagnostics. The countermeasure is a single redaction path applied before JSON report writing and stdout summary generation, plus tests that place configured secret values in driver responses, hook diagnostics, probe diagnostics, request payloads, and report summaries and assert the raw value is absent.

A second risk is making real backend runs destructive. The countermeasure is to prefer pre-provisioned fixture declarations, keep setup and cleanup hooks optional, require bounded hook execution, attempt cleanup on failure, and document that manifests with privileged hooks are trusted operator artifacts.

A third risk is confusing support failures with adapter failures. The countermeasure is to preserve separate `setup_failed`, `probe_failed`, and `cleanup_failed` classifications, keep fixture and probe namespaces out of adapter operations, and require reports to state recovery guidance for each failure class.

A fourth risk is report schema churn. The countermeasure is to evolve reports additively, retain the current fields during a compatibility window, and gate schema changes with expected JSON fixtures and CLI summary tests.

## Scope Boundaries

In scope for LIV-410 itself is only this review document and the structured implementation-pack submission. No runner code, canonical bundle, production adapter, or live tracker backend setup belongs in this issue.

The follow-up implementation scope starts from the LIV-406 MVP. It may add manifest/schema fields, Gleam types, fake fixtures, report rendering, report schemas, protocol docs, runbook updates, and tests around the existing conformance runner. It must preserve the black-box CLI `task_source` path and must not remove the existing fake-driver coverage.

Out of scope are HTTP transport work, which is covered by LIV-411; optional side-effect capability packs; production Jira, Trello, Linear, or other adapter implementations; service supervision; Docker as a first-class transport; provider-live cache or TTL behavior; and changes to Scherzo's runtime tracker adapter API outside the conformance package. The conformance runner should continue issuing fresh black-box driver requests and should not introduce a cache; if later implementation work adds any cache, that new cache must come with explicit stale-read and invalidation tests before it is accepted.

Documentation and helper migration are in scope only where they keep the conformance surface coherent: `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, manifest/report schema fixtures, fake-driver fixtures, and CLI/helper examples must be updated together when fields or evidence shape changes. No workflow-helper migration, provider credential setup, or live backend provisioning belongs in this issue.

Implementation-ticket boundaries should be split into fixture and support-operation hardening first, report evidence and redaction hardening second, and adapter-author documentation plus final validation last. If the first implementation inventory finds that current post-LIV-406 code already covers one boundary completely, that boundary should be closed or narrowed rather than duplicated.

## Milestones

Milestone 1 locks and inventories the LIV-406 baseline with named regression evidence. Reviewers should see existing CLI `task_source` tests passing, a short protocol-doc note stating which LIV-410 behaviors already exist, and no report or manifest behavior regressions in the current fake-driver fixtures. The evidence for this milestone is a passing `direnv exec . gleam test` run that covers `test/tracker_conformance_protocol_test.gleam`, `test/tracker_conformance_cli_driver_test.gleam`, `test/tracker_conformance_task_source_test.gleam`, `test/tracker_conformance_fixture_probe_test.gleam`, `test/tracker_conformance_report_test.gleam`, and `test/tracker_conformance_cli_test.gleam`.

Milestone 2 hardens fixture task declarations and support-operation boundaries with negative tests before changing the runner. Reviewers should see explicit fixture task declarations tied to pre-provisioned backend identities, optional setup and cleanup hooks that remain privileged support operations, backend probes that cannot appear in adapter operation lists, and failure fixtures that produce `setup_failed`, `probe_failed`, or `cleanup_failed` without inflating adapter conformance failures. The evidence is a manifest/schema test for named fixture declarations, a rejection test for fixture/probe/hook operation names in `profile.adapter_operations`, and fake-driver setup, probe, and cleanup failure runs with support-failure counters and zero adapter-case failures for the support failure itself.

Milestone 3 adds redacted bounded transcripts and diagnostics with explicit leak tests. Reviewers should see request and response evidence in JSON reports, truncation metadata when diagnostics or transcripts exceed the configured bound, and tests proving configured secret values never appear in report files or stdout summaries. The evidence is an expected JSON report fixture containing redacted request and response transcript summaries, a long-transcript fixture that marks truncation, and assertions that raw secret markers are absent from both report files and CLI summaries.

Milestone 4 enriches report fields and recovery guidance while preserving compatibility. Reviewers should see case ids, operation names, request ids, expected summaries, actual summaries, aggregate counts, and failure-specific recovery guidance in report schemas, expected report fixtures, and CLI summaries. The evidence is a schema or expected-fixture assertion for each new field and separate recovery guidance for adapter failures, setup failures, probe failures, and cleanup failures.

Milestone 5 updates adapter-author docs, manual dogfood instructions, and repository gates. Reviewers should see `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, conformance schemas, tests, formatting, glinter, and `scherzo_lint` all passing, with implementation tickets closed or narrowed to reflect any behavior already delivered by LIV-406. The evidence includes a documented fake-driver dogfood command that writes a sanitized report under `test/tmp/tracker-conformance/`, an optional live-backend checklist for operators with pre-provisioned fixtures, and an explicit note that provider-live cache behavior is not applicable unless the implementation introduces a cache.

## Progress

- [x] (2026-05-21) Read the LIV-410 task brief and ExecPlan authoring guidance.
- [x] (2026-05-21) Inspected the LIV-365 review document and the current tracker conformance protocol, types, runner, report, fixture, probe, and test files.
- [x] (2026-05-21) Drafted this concise human-reviewable review document under `docs/plans/`.
- [x] (2026-05-21) Prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-21) Validated this review document with `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-410-tracker-conformance-fixtures-probes-reports.md`.
- [x] (2026-05-21) Incorporated review feedback by making acceptance evidence, test obligations, milestone evidence, manual dogfood checks, docs/helper migration scope, provider-live/cache non-applicability, full validation, and linting explicit in this review document and the updated implementation-pack submission.

## Decision Log

- Decision: Treat LIV-410 as a hardening and narrowing plan after LIV-406, not as a replacement for the MVP conformance runner.
  Rationale: The current tree already implements the CLI `task_source` path and several requested support-operation classifications; duplicating that work would create churn instead of closing the remaining gaps.
  Date: 2026-05-21.

- Decision: Keep fixture setup, cleanup, and probes outside adapter-under-test operations.
  Rationale: Privileged backend access is useful for reliable tests, but conformance must be proven only through the public adapter driver operations.
  Date: 2026-05-21.

- Decision: Add report evidence and guidance additively while retaining current case id, operation, request id, and counter fields during a compatibility window.
  Rationale: The MVP reports are already useful; hardening should enrich them without breaking local fake-driver workflows unnecessarily.
  Date: 2026-05-21.

- Decision: Redaction and bounding are acceptance gates, not best-effort cleanup.
  Rationale: A report feature that leaks tokens or unbounded backend payloads is unsafe to run against real tracker backends.
  Date: 2026-05-21.

- Decision: Treat manual dogfood evidence as fake-driver evidence plus optional live-backend operator evidence, not as a mandatory live-provider run.
  Rationale: LIV-410 hardens the conformance foundation and should be safe for real backends, but this issue does not provision production adapters or credentials. A deterministic fake-driver dogfood command is required; a live run is documented for operators who already have pre-provisioned fixtures.
  Date: 2026-05-21.

## Validation and Acceptance

This planning issue is accepted when the file `docs/plans/LIV-410-tracker-conformance-fixtures-probes-reports.md` exists, running `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-410-tracker-conformance-fixtures-probes-reports.md` prints `REVIEW_DOC_VALID=ok`, and Scherzo captures the structured implementation-pack submission for LIV-410. In the packaged workflow, the same validator is invoked as `scripts/scherzo-execplan validate-review-doc` from the workflow bundle.

The later implementation is accepted only with concrete evidence for each required behavior. Fixture declaration evidence is a manifest/schema test that decodes named pre-provisioned fixture tasks and a fake-driver run whose report identifies the fixture identities used by the `task_source` cases. Boundary evidence is a negative manifest test that rejects fixture, probe, or hook operation names inside `profile.adapter_operations`. Classification evidence is fake setup, probe, and cleanup failure runs whose reports contain the matching support-failure counters and no adapter case failure for the support failure itself.

Report safety evidence is a test run with configured secret markers in driver, hook, probe, request, and response data; the expected output is that the JSON report and stdout summary contain `[REDACTED]`, do not contain the raw secret marker, and mark long diagnostics or transcripts as truncated. Report richness evidence is an expected JSON fixture or schema assertion showing case id, operation name, request id, expected summary, actual summary, aggregate counts, and recovery guidance for both adapter failures and support-operation failures.

Manual and dogfood evidence is also required. The deterministic dogfood check is running the documented fake-driver command from the repository root, writing a sanitized report under `test/tmp/tracker-conformance/`, and confirming the stdout summary and JSON report include the enriched evidence without raw secret markers. Live-provider evidence is optional because LIV-410 does not add production adapters, credentials, or cache behavior; if an operator has a real backend and pre-provisioned fixtures available, the docs should explain how to run the same manifest contract safely and record only a redacted report excerpt. Provider-live cache behavior is accepted as not applicable unless the implementation introduces a cache, in which case stale-cache and invalidation tests become mandatory before acceptance.

Repository validation evidence is running `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root and recording zero failing tests or lint errors. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands. The implementation handoff must not treat these as optional TODOs; if a command cannot run, the retained evidence must state the environment failure and the exact retry command.

## Rollout, Recovery, and Idempotence

Rollout should be additive. Existing CLI `task_source` manifests and fake-driver fixtures remain valid while enriched fixture declarations and report fields are introduced. Operators can continue using the MVP report shape until the enriched schema is documented and tested. Docs, report fixtures, schema examples, fake-driver helpers, and CLI examples should migrate in the same implementation slice as the field changes so adapter authors never see a half-documented contract.

Recovery is to fall back to the LIV-406 MVP path: run the existing CLI conformance manifest without new fixture declaration fields or transcript-heavy reporting, delete generated report files under test output directories, and revert the additive report/schema changes if redaction or compatibility fails review. If manual dogfood or optional live-backend evidence exposes unsafe output, stop before publishing, keep only redacted retained artifacts, and narrow the implementation ticket to fix the redaction or bounding path first.

Idempotence depends on the fixture contract. The current `task_source` pack is read-only and can be rerun against the same pre-provisioned tasks. Setup and cleanup hooks remain optional trusted support operations; they should be written so repeated runs either leave the fixture in the same state or report a support failure with recovery guidance rather than being counted as adapter conformance failure.

## Open Questions and Clarifications Needed

No blocking clarification is needed to draft the follow-up implementation. During implementation, reviewers should confirm whether enriched reports use a new report schema version immediately or retain the current schema version until adapter-author documentation is updated, but either choice must preserve redaction, bounded diagnostics, support-failure classification, and the existing CLI `task_source` MVP behavior.
