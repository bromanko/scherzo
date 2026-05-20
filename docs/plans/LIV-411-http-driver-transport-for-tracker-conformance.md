# Add HTTP driver transport for tracker conformance

This review document is the human-facing proposal for LIV-411. It intentionally plans future HTTP support only; exact implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

Scherzo's tracker conformance suite can already prove the MVP `task_source` contract through an external CLI driver. The next useful transport is HTTP, so adapter authors can point the same conformance pack at a running service without wrapping it in a local command.

After the later implementation tickets complete, a manifest will be able to select either `driver.transport = "cli"` or `driver.transport = "http"`. Both transports will carry the same request and response envelopes, and the same conformance cases and reports will decide whether an adapter conforms.

## Problem Framing and Constraints

The accepted MVP deliberately started with CLI transport to avoid designing several transports before one worked. That constraint still matters: HTTP must depend on the existing CLI MVP and preserve its envelope, report, fixture, probe, and `task_source` semantics rather than replacing them.

HTTP adds risks CLI does not have: endpoint lifecycle, network timeouts, retry boundaries, non-2xx status codes, malformed response bodies, redirects, and authorization headers that may contain secrets. The plan must classify those failures as transport-level driver failures unless the service returns a valid conformance response envelope, and it must prevent configured tokens from appearing in reports or summaries.

## Strategy Overview

Add HTTP as a second driver transport behind the existing conformance driver boundary. The conformance runner should still build one transport-neutral request envelope per adapter operation. The selected transport only decides how that envelope reaches the driver: CLI writes JSON to stdin and reads JSON from stdout; HTTP sends the same JSON as a POST body and reads one JSON response body.

The HTTP service is a pre-started external driver owned by the adapter author. The first implementation should not invent service supervision; setup and cleanup hooks remain available for fixture preparation or test-only service lifecycle, but they do not become adapter-under-test operations. Endpoint configuration should be explicit, retries should default to one attempt and be capped when configured, redirects should not be followed, and all status-code handling should be deterministic in reports.

## Alternatives Considered

The smallest alternative is to keep requiring HTTP adapters to ship a CLI wrapper. That is acceptable for the MVP but not enough long term because it hides real service behavior, authentication headers, network failures, and HTTP status handling from the conformance suite.

A second alternative is to redesign the conformance protocol around HTTP first and retrofit CLI afterward. That is rejected because the CLI MVP already proves the black-box boundary and the request/response envelope; changing pack semantics now would make adapter authors re-learn the contract for no benefit.

A third alternative is to add a general plug-in transport abstraction before HTTP. That is too broad. The plan should add only the seam needed for CLI and HTTP to coexist, then let future transports justify themselves with their own tickets.

## Risks and Countermeasures

The largest risk is semantic drift between CLI and HTTP. Counter this by sharing request generation, response decoding, case execution, and report building, and by retaining existing CLI compatibility tests as a gate for every HTTP change.

Another risk is leaking tokens through manifests, diagnostics, failed URLs, or reports. Counter this by resolving secret header values from environment variables at runtime, automatically adding resolved values to the redaction set, rejecting unsafe header names, and testing that configured token values never appear in JSON reports or CLI summaries.

HTTP status handling can also become ambiguous. Counter this by accepting conformance envelopes only from successful non-redirect responses, classifying non-2xx and redirect responses as driver transport failures, and making retry eligibility depend on status class and network errors rather than on adapter operation names.

A final risk is flaky tests caused by a fake service that is not ready or does not shut down. Counter this by using deterministic test handshakes, per-test localhost endpoints, bounded timeouts, and cleanup that terminates the fake service even when assertions fail.

## Scope Boundaries

In scope for LIV-411 is only this review document and the structured implementation-pack submission. No HTTP transport code, canonical bundle file, or implementation ticket mutation belongs in this issue.

In scope for the first follow-up implementation ticket are the manifest/schema/type changes and the transport dispatch seam that let CLI remain the default while HTTP manifests decode and validate. The validation gate is that existing CLI manifests and CLI conformance tests keep passing unchanged.

In scope for the second follow-up implementation ticket are the HTTP client invocation, endpoint/header/timeout/retry handling, status-code classification, malformed-response classification, automatic redaction of secret header values, and fake HTTP service compatibility tests.

In scope for a small final documentation or hardening ticket, if needed, are adapter-author examples and any compatibility cleanup discovered during review. Out of scope are changing conformance pack semantics, adding side-effect capability packs, managing production service processes, adding Docker as a first-class transport, or accepting privileged fixture/probe operations as adapter evidence.

## Milestones

Milestone 1 preserves the CLI MVP while opening the manifest shape. Reviewers should see a typed driver configuration that can represent CLI and HTTP, schema coverage for both shapes, and unchanged behavior for all existing CLI fixture manifests.

Milestone 2 implements HTTP request and response mapping. Reviewers should see one POST per conformance operation, the existing JSON request body and response body reused without envelope changes, deterministic handling for timeouts, retries, redirects, non-2xx statuses, empty bodies, malformed JSON, and stale request ids, plus report redaction for secrets.

Milestone 3 proves compatibility with a fake HTTP driver service. Reviewers should see passing `task_source` cases over HTTP, intentionally defective fake-service modes for failure classification, and explicit tests that HTTP and CLI transports produce the same conformance conclusions for equivalent fixture data.

Milestone 4 updates operator-facing docs and release notes. Reviewers should see clear guidance that HTTP support is additive, that the service must already be running, that secrets come from environment variables, and that CLI remains supported.

## Progress

- [x] (2026-05-20) Read the LIV-411 task brief, the LIV-365 review document, the current conformance protocol spec, and the current conformance implementation files.
- [x] (2026-05-20) Drafted this concise human-reviewable review document under `docs/plans/`.
- [x] (2026-05-20) Prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-20) Validated this review document with `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-411-http-driver-transport-for-tracker-conformance.md`.

## Decision Log

- Decision: HTTP support depends on the CLI MVP and must not replace CLI transport.
  Rationale: The accepted MVP already established the black-box boundary and the transport-neutral envelopes; HTTP should add a delivery mechanism, not redefine conformance.
  Date: 2026-05-20.

- Decision: Treat the HTTP driver as a pre-started external service for the first HTTP ticket.
  Rationale: Service supervision, containers, and health-check orchestration are separate lifecycle problems; the conformance runner only needs a bounded endpoint invocation to validate adapter semantics.
  Date: 2026-05-20.

- Decision: Keep non-2xx, redirect, timeout, network, and malformed-body failures as driver transport failures rather than normalized tracker errors.
  Rationale: A normalized tracker error is valid only when the adapter service returns the agreed conformance response envelope; HTTP delivery failures should not be confused with adapter contract behavior.
  Date: 2026-05-20.

- Decision: Resolve HTTP secret headers from environment variables and redact resolved values automatically.
  Rationale: Manifests and reports should remain shareable test artifacts without embedding tokens or leaking them in diagnostics.
  Date: 2026-05-20.

## Validation and Acceptance

This planning issue is accepted when this file validates as an ExecPlan review document and Scherzo captures the structured implementation-pack submission for LIV-411.

The later implementation is accepted only when CLI conformance behavior remains unchanged, HTTP manifests use the same operation envelopes and pack semantics, a fake HTTP driver can pass the existing `task_source` pack, intentional fake HTTP failures produce stable driver-failure classifications, and reports redact configured header secrets.

The implementation-ticket gates are: schema/type compatibility first, HTTP transport behavior second, fake-service compatibility and failure-mode tests third, and adapter-author documentation last.

## Rollout, Recovery, and Idempotence

Rollout should be additive. Existing CLI manifests remain valid, CLI remains the documented MVP path, and HTTP examples appear only after the fake service and compatibility tests pass.

Recovery is straightforward because HTTP transport can be disabled by using CLI manifests. If HTTP behavior is found unsafe or flaky, revert the HTTP transport files and schema additions while keeping the CLI MVP intact. Re-running conformance against the same HTTP endpoint is idempotent for the current read-only `task_source` pack; future side-effect packs must state their own retry and cleanup rules.

## Open Questions and Clarifications Needed

No blocking clarification is needed to plan the first HTTP transport implementation. Later implementation review should confirm only whether adapter-author docs should recommend a standardized path such as `/tracker-conformance` or allow any full endpoint URL from the start.
