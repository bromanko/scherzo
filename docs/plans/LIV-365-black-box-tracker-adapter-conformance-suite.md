# Plan black-box tracker adapter conformance

This ExecPlan v2 review document is the human review surface for LIV-365. It describes the intended verification boundary and delivery shape for a black-box tracker adapter conformance suite. Mechanical implementation steps, exact tests, detailed protocol fields, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo needs a way to verify future tracker adapters that are not Gleam modules inside this repository. After the later implementation tickets complete, an adapter author will be able to provide a driver command, a capability declaration, and fixture configuration, then run a conformance suite that proves the adapter can satisfy Scherzo's public tracker contract without exposing its implementation internals.

The first useful result is intentionally small: a minimal `task_source` profile that exercises candidate fetch, refresh by stable task references, and operator-reference lookup through an external driver. Later test packs add optional side-effect capabilities only after the black-box boundary, fixture model, and reporting format are stable.

## Problem Framing and Constraints

The current normalized tracker contract is defined in repository code and documentation, but importing adapter code directly would only test in-process Gleam adapters. That boundary is too narrow for HTTP services, CLI tools, Dockerized adapters, or closed-box integrations, and it would couple the suite to implementation details Scherzo does not own.

The suite must therefore treat the adapter as an external system under test. The conformance contract is the public adapter operation set and normalized data model, not the adapter's source files, language, storage layer, or backend SDK. Fixture setup, cleanup, and backend probes are allowed, but they must be separate from adapter-under-test operations so the suite never proves conformance by using privileged fixture paths.

The implementation must stay proportionate. This planning slice does not build the full runner, all optional capability packs, or a production Jira, Trello, or other adapter. It defines a staged implementation plan that starts with CLI transport and `task_source`, while reserving room for HTTP or other transports without committing to them in the MVP.

## Strategy Overview

The suite should introduce an adapter test driver protocol. A driver is a process or service supplied by the adapter author. For the MVP, Scherzo invokes a CLI driver, sends one normalized JSON request per operation, and reads one normalized JSON response. The same request and response envelope should be transport-neutral enough to support a future HTTP driver, but HTTP support should be a follow-up ticket rather than an MVP requirement.

The public operation schema should mirror Scherzo's tracker capabilities. The minimal profile includes `task_source.fetch_candidates`, `task_source.refresh_by_refs`, and `task_source.lookup_by_operator_ref`, returning normalized `Task`, `TaskRef`, and `TrackerError` shapes. Capability profiles declare what the adapter claims to support. The runner verifies every required operation for the selected profile and only runs optional packs, such as comments or scheduled failures, when both the declared capabilities and requested test pack include them.

Configuration should be split into four concepts: driver transport, capability profile, fixtures, and probes. Driver transport tells the suite how to call the adapter. Capability profile states the public features under test. Fixtures identify pre-provisioned backend tasks and optional setup/cleanup hooks. Probes are backend-specific checks used to confirm fixture existence or visible side effects, not adapter conformance operations. Reports should include a machine-readable result file plus a concise human summary with failing operation names, request ids, normalized errors, redacted transcripts, and fixture/probe context.

## Alternatives Considered

One alternative is to write conformance tests that import `src/scherzo/tracker/adapter.gleam` implementations directly. That is rejected because it cannot verify closed-box adapters and would make implementation internals part of the suite contract.

A second alternative is to start with a full Linear-equivalent conformance suite. That is too large and would delay the basic boundary decision. The required `task_source` profile proves the essential read-only path first and avoids risky side effects while the driver protocol is still new.

A third alternative is to make each adapter own its own tests. Adapter-owned tests are useful, but they do not give Scherzo a consistent capability declaration, fixture vocabulary, failure report, or cross-adapter baseline.

## Risks and Countermeasures

The main risk is conflating fixture power with adapter conformance. The countermeasure is a hard namespace and report distinction between adapter-under-test operations and setup, cleanup, or probe operations.

A second risk is over-designing transports before one works. The countermeasure is to implement CLI first with a transport-neutral envelope, then add HTTP only after the `task_source` pack and reports are useful.

A third risk is under-specifying idempotency and recovery. The countermeasure is to name these checks explicitly even when they are deferred: stable task identity belongs in the MVP, while scheduled failure dedupe, comment update fallback, remote command acknowledgement retry, and handoff retry behavior belong in later capability packs.

A fourth risk is leaking secrets or backend payloads in diagnostics. The countermeasure is redacted transcripts, bounded error messages, and tests that fail if report output includes configured secret values.

## Scope Boundaries

In scope for this planning issue is exactly this review document and one structured implementation-pack submission. No runner implementation, canonical bundle file, or production adapter belongs in LIV-365.

In scope for the first follow-up implementation ticket are schemas and typed representations for the conformance manifest, driver request and response envelopes, capability profiles, fixtures, probes, and report output.

In scope for the second follow-up implementation ticket are the CLI driver transport and the minimal `task_source` conformance pack, including a fake external driver fixture that runs out of process.

In scope for later follow-up tickets are fixture setup/cleanup and probe hardening, HTTP transport, comments and state-transition packs, routing metadata tests, remote-command acknowledgement retry tests, handoff tests, scheduled-failure idempotency tests, richer reporting, and documentation for adapter authors.

Out of scope are a full verification runner in this ticket, all optional capability packs in the MVP, any production Jira/Trello/etc. adapter, changing Scherzo's runtime tracker adapter contract, and using backend-specific privileged APIs as evidence for adapter-under-test behavior.

## Milestones

The first milestone should confirm the current tracker contract and add the conformance schema foundation: manifest version, driver transport declaration, capability profile, fixture references, probe declarations, request envelope, response envelope, and result report shape.

The second milestone should implement a CLI driver harness and a fake external driver so the suite proves it is crossing a process boundary rather than importing Gleam adapter code.

The third milestone should implement the minimal `task_source` test pack. It should verify candidate reads are read-only, returned task refs use the declared backend kind, refresh uses stable `(backend_kind, remote_id)` identity, wrong-backend refs fail or are omitted only as documented, empty operator refs return no match, and known operator refs resolve consistently.

The fourth milestone should separate setup, cleanup, and probe behavior from adapter-under-test operations in both configuration and reports. This is where pre-provisioned tasks, optional hooks, backend-specific visibility probes, and redaction checks become explicit.

The fifth milestone should split optional capability packs into their own implementation tickets: comments including update/fallback semantics, state transitions, routing metadata, remote commands including acknowledgement retry, handoff, scheduled failures including dedupe, and future transports such as HTTP.

## Progress

- [x] (2026-05-19 16:40Z) LIV-406 milestone 1 landed the conformance protocol foundation: typed manifest/request/response/report modules, JSON encoders/decoders, schema files, protocol fixtures, and protocol round-trip tests.
- [x] (2026-05-19 16:40Z) Added `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md` with the MVP manifest, CLI driver envelope, and report skeleton.
- [x] (2026-05-19 18:45Z) LIV-406 milestone 2 landed the CLI driver transport harness, failure classification, and fake out-of-process driver fixtures.
- [x] (2026-05-19 18:45Z) LIV-406 milestone 3 landed the minimal `task_source` conformance pack with passing and intentionally defective fake-driver manifests.
- [x] (2026-05-19 18:45Z) LIV-406 milestone 4 separated setup, cleanup, probes, and report classification, including redacted report output.
- [x] (2026-05-19 18:45Z) LIV-406 milestone 5 added the local `scherzo tracker-conformance run` command, CLI tests, and adapter-author runbook updates.
- [x] (2026-05-20 01:10Z) Review pass tightened driver response-envelope correlation, manifest timeout and operation validation, and negative coverage for cleanup, stale responses, and task-source failure branches.
- [x] (2026-05-20 01:35Z) Feedback follow-up capped captured external-process diagnostics and tightened fixture-path validation for Windows-style drive and backslash escape forms.
- [x] (2026-05-19 00:00Z) Read the ExecPlan authoring guidance and the LIV-365 task brief.
- [x] (2026-05-19 00:00Z) Inspected the current tracker adapter specification, task model, adapter capability types, and review-doc validation rules.
- [x] (2026-05-19 00:00Z) Drafted this concise review document for human review.
- [x] (2026-05-19 00:00Z) Prepared the structured implementation-pack content for Scherzo capture.
- [x] (2026-05-19 00:00Z) Validated this review document with `scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-365-black-box-tracker-adapter-conformance-suite.md`.

## Decision Log

- Decision: The manifest foundation stores fixture provenance as a repository-relative `fixtures.task_file` path and rejects fixture/probe/hook namespaces inside `profile.adapter_operations`.
  Rationale: The first milestone needed one concrete fixture path shape and a hard boundary that prevents privileged support operations from being mistaken for adapter conformance operations.
  Date: 2026-05-19.

- Decision: The MVP protocol modules use typed Gleam representations for manifests, request envelopes, response envelopes, normalized driver errors, and the first report skeleton, plus matching JSON Schema files under `.scherzo/workflows/schemas/`.
  Rationale: LIV-406 milestone 1 is the schema foundation milestone, so the repository now has both executable decoders and portable schema artifacts for future external drivers.
  Date: 2026-05-19.

- Decision: The CLI driver accepts a response only when it uses schema version 1 and echoes the outbound request id; manifest driver timeouts are capped at 60 seconds.
  Rationale: Request correlation is part of the black-box protocol contract, and bounded per-operation timeouts prevent malformed manifests from hanging local conformance runs indefinitely.
  Date: 2026-05-20.

- Decision: Captured stderr diagnostics from drivers, hooks, and probes are truncated before entering reports, and manifest fixture paths reject Windows-style drive or backslash escape forms before runtime realpath confinement checks.
  Rationale: Review feedback identified unbounded external-process diagnostics and narrower-than-documented fixture-path validation as avoidable operator risks that could be tightened without changing the MVP protocol surface.
  Date: 2026-05-20.

- Decision: The conformance suite boundary is an external adapter driver protocol, not in-process module imports.
  Rationale: Future adapters may be HTTP services, CLI tools, containers, or proprietary integrations; Scherzo should verify only the public normalized contract.
  Date: 2026-05-19.

- Decision: The MVP profile is `task_source` through CLI transport.
  Rationale: `task_source` is required for every adapter, is read-only, and proves stable identity before optional side effects add backend risk.
  Date: 2026-05-19.

- Decision: Fixture setup, cleanup, and probes are distinct from adapter-under-test operations.
  Rationale: Privileged backend preparation is necessary for reliable tests, but it must not become evidence that the public adapter operations conform.
  Date: 2026-05-19.

- Decision: Optional capability conformance should be delivered as separate test-pack tickets.
  Rationale: Comments, state moves, routing, remote commands, handoff, and scheduled failures have different fixtures, side effects, retry semantics, and cleanup risks.
  Date: 2026-05-19.

- Decision: The MVP runner should treat setup hooks, adapter-under-test cases, probes, and cleanup hooks as separately counted report paths rather than flattening every failure into case failures.
  Rationale: The verifier feedback required observable separation between privileged fixture/probe failures and public adapter operation failures, and distinct counters make that separation visible in both JSON reports and CLI summaries.
  Date: 2026-05-19.

## Validation and Acceptance

This planning issue is accepted when this Markdown review document exists under `docs/plans/`, `scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-365-black-box-tracker-adapter-conformance-suite.md` accepts it, and Scherzo captures the structured implementation-pack submission.

The later MVP implementation is accepted only when a conformance manifest can declare a CLI driver, the `task_source` profile, pre-provisioned task fixtures, and optional probes; a fake out-of-process driver can pass the `task_source` pack; intentional fake-driver defects produce clear failures; and the report distinguishes adapter operation failures from fixture or probe failures.

The later optional-pack implementations are accepted only when they explicitly test the idempotency and recovery semantics relevant to the claimed capability: stable task identity, comment update fallback behavior, remote command acknowledgement retry behavior, handoff retry visibility, and scheduled failure dedupe by key.

## Outcomes & Retrospective

LIV-406 now reaches the planned MVP boundary. Scherzo can spawn an external CLI driver, send normalized `task_source` requests over stdin, classify malformed JSON, response-envelope mismatches, missing stdout, timeout, and non-zero exit as driver failures, and execute a minimal `task_source` pack that proves backend-kind preservation, stable refresh identity, wrong-backend handling, empty operator-ref lookup, and known operator-ref lookup.

The repository now also distinguishes setup, probe, and cleanup failures from adapter operation failures in machine-readable reports and CLI summaries. The local `scherzo tracker-conformance run <manifest.json> --report <report.json>` command makes acceptance observable without importing adapter implementation code, and the fake-driver fixtures give the suite both a passing out-of-process adapter and intentional failing variants for clear negative coverage.

The post-review follow-up also reduced two operator risks without widening scope: external-process diagnostics are now truncated before reports or summaries capture them, and fixture-path validation now rejects Windows-style drive and backslash escape forms before runtime repository-confinement checks resolve the fixture path.

## Rollout, Recovery, and Idempotence

The planning change is additive. If review rejects the approach, revise or remove only `docs/plans/LIV-365-black-box-tracker-adapter-conformance-suite.md` and resubmit the structured pack.

The later implementation should also be additive. The first runner should live beside existing in-process adapter tests and should not replace them. Test fixtures and generated reports should be safe to delete and regenerate. Driver calls should carry request ids so failed runs can be diagnosed and retried without confusing repeated adapter operations with repeated setup hooks.

Side-effect packs must require explicit fixture declarations and cleanup guidance before they run against a real backend. If a capability pack cannot prove a safe retry or dedupe story, it should fail conformance for that capability rather than weakening the profile.

## Open Questions and Clarifications Needed

No blocking clarification is needed for the `task_source` CLI MVP. Later tickets should decide the exact HTTP transport contract, whether Docker invocation is a first-class transport or a CLI wrapper convention, how much probe vocabulary should be standardized, and which optional capability pack should follow immediately after `task_source`.
