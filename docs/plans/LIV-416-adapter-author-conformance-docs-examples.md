# Plan adapter-author conformance docs and examples

This ExecPlan v2 review document is the human review surface for LIV-416. It plans a later documentation and examples pass for external tracker adapter authors; mechanical implementation steps, tests, interfaces, dependencies, and artifact notes are supplied through the structured implementation-pack submission captured by Scherzo.

## Purpose / Big Picture

External adapter authors need one trustworthy path from "I have an adapter" to "I can prove it conforms." After the later implementation tickets complete, an author should be able to read a focused guide, copy a minimal CLI driver example and manifest, run the conformance suite, understand intentional failure reports, and package their adapter without guessing which parts of the protocol are stable.

This plan is deliberately conditional. If the first implementation inventory proves that LIV-406 and its follow-ups already provide enough MVP adapter-author documentation and examples, the implementation should close or narrow itself to publishing that evidence instead of writing another documentation layer.

## Problem Framing and Constraints

The current conformance material is split between protocol reference, tracker runbook notes, JSON Schema files, Gleam tests, fake-driver fixtures, and prior plans. That is sufficient for maintainers who know the repository, but it is too fragmented for an external author who needs manifest fields, driver envelopes, capability profiles, fixtures, probes, hooks, reports, redaction, optional packs, and packaging expectations explained as one workflow.

The documentation must not make future or experimental behavior look stable. Optional packs and transports should be documented only when their schema, fixtures, tests, and runner behavior exist in the current tree; otherwise the guide should name them as planned or deferred and point authors to the stable MVP surface. This issue drafts the plan only and must not write the adapter-author guide, examples, runner changes, schema changes, or canonical bundle by hand.

## Strategy Overview

Start with an inventory gate that compares the current protocol docs, runbook, schemas, fixtures, and examples against the required adapter-author journey. That gate either closes the follow-up as unnecessary, narrows it to missing artifacts, or confirms that a focused guide is needed.

If docs are still needed, add `docs/runbooks/tracker-adapter-author-guide.md` as the single adapter-author guide. It should be task-oriented and link to authoritative references instead of copying schemas by hand. Keep JSON Schema files and checked-in conformance fixtures as the contract source of truth. Documentation examples should either reference checked-in fixture files directly or be validated from the same fixtures so snippets cannot drift from executable behavior.

Add examples under `examples/tracker-conformance/adapter-author/` in two layers: a minimal passing shell CLI driver and manifest that are safe to run locally, then intentionally failing manifests or driver modes that explain diagnostics for malformed envelopes, missing capabilities, fixture/probe/hook boundary violations, redaction, and report counters. Optional packs and future transports should appear in a compatibility matrix that distinguishes implemented, documented-stable, planned, and deliberately unsupported behavior.

## Alternatives Considered

One alternative is to rely on `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md` and `docs/runbooks/tracker-adapters.md` alone. That may be enough if the inventory proves a novice adapter author can run and diagnose the MVP from those files, so the first milestone can close or narrow this plan. It is not assumed enough because protocol references and operator runbooks are not the same as an adapter-author onboarding path.

A second alternative is to generate a full documentation site from JSON Schema. That is too large for the current need. Schema-generated tables can help synchronization, but authors also need narrative examples, packaging guidance, and failure diagnostics.

A third alternative is to document every planned optional pack and future transport now. That is rejected because it would make unimplemented behavior look stable. The safer approach is to document only implemented surfaces and include explicit status labels for deferred packs or transports.

## Risks and Countermeasures

The main risk is documentation drift from schemas and fixtures. Counter it by making checked-in schemas and conformance fixture files the source of truth, adding validation commands that parse documented manifests and driver envelopes, and requiring docs updates in the same implementation slice as any schema or fixture change.

A second risk is overpromising optional packs or transports. Counter it with a status matrix that marks a surface stable only when the current tree contains schema support, runner behavior, fixtures, tests, and report evidence for it.

A third risk is unsafe examples that mutate real tracker data or leak secrets. Counter it by making all required examples fake-driver based before publish, documenting live-backend runs as optional human/operator evidence after implementation, and requiring redacted reports with no raw secret markers.

A fourth risk is hiding negative behavior. Counter it by making intentionally failing examples part of acceptance: each failure example should produce a named diagnostic, report counter, or validation error that teaches authors how to recover.

A fifth risk is letting review feedback live only in this prose document while the structured implementation pack omits acceptance evidence, test obligations, milestone specificity, dogfood timing, docs/helper boundaries, provider-live/cache non-scope, full validation, or lint gates. Counter it by mirroring those obligations in the pack's concrete steps and testing notes before Scherzo materializes follow-up implementation artifacts.

## Scope Boundaries

In scope for LIV-416 is exactly this Markdown review document under `docs/plans/` and one structured implementation-pack submission. No adapter-author documentation, example driver, schema edit, runner edit, production adapter change, live backend run, helper migration, provider-live/cache behavior, or canonical bundle file belongs in this issue.

The first follow-up implementation boundary is an inventory and closure decision. It must verify whether LIV-406 and later conformance work already cover the MVP author journey; if they do, it should close with evidence or narrow to the specific missing docs and examples before changing files.

The documentation boundary is a focused guide at `docs/runbooks/tracker-adapter-author-guide.md` unless the inventory closes the work as already covered. The guide should link to `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md` and `docs/runbooks/tracker-adapters.md` as authoritative references while explaining manifests, CLI driver request and response envelopes, capability profiles, fixtures, probes, hooks, reports, redaction, optional pack status, future transport status, and packaging expectations as one author workflow.

The example boundary is a minimal, portable shell CLI driver and manifests under `examples/tracker-conformance/adapter-author/`, plus intentionally failing examples whose diagnostics are documented and validated. Language-specific examples, real third-party tracker fixtures, new optional packs, and new transports are deferred.

The docs/helper boundary is explicit. A later implementation may add small documentation validation helpers or tests only when needed to keep snippets synchronized with checked-in schemas and fixtures. It must not migrate unrelated workflow helpers. If it touches `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files, it must run and retain the relevant helper or contract tests; if it does not touch them, acceptance evidence must explicitly say that no helper migration, provider-live behavior, or cache behavior changed.

Out of scope are changing conformance semantics, stabilizing unimplemented transports, adding new optional packs, provisioning real third-party tracker fixtures, introducing provider-live or cache behavior, and migrating unrelated workflow helpers.

## Milestones

Milestone 1 produces an inventory and close-or-narrow decision before documentation files change. The implementer reviews `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`, `docs/runbooks/tracker-adapters.md`, `.scherzo/workflows/schemas/tracker-conformance-*.v1.schema.json`, `test/fixtures/tracker_conformance/`, `test/tracker_conformance_*_test.gleam`, and any existing `examples/tracker-conformance/` files. Reviewers should see an artifact or final implementation note mapping every required author topic to current evidence, stating whether existing LIV-406-era docs already satisfy the MVP author journey, and either stopping with evidence or naming the exact missing guide and example artifacts.

Milestone 2 produces `docs/runbooks/tracker-adapter-author-guide.md` when the inventory finds a gap. Reviewers should see one coherent document explaining the stable manifest shape, CLI driver envelopes, capability profiles, fixture files and explicit fixture declarations, privileged setup/cleanup hooks, probes, report fields, redaction behavior, optional-pack status, future-transport status, and the rule that probes and hooks never count as adapter-under-test evidence.

Milestone 3 produces executable examples under `examples/tracker-conformance/adapter-author/`. Reviewers should see a portable `driver.sh`, `manifest.pass.json`, failing manifests for invalid shape, missing capabilities, and support-operation namespace misuse, plus driver modes or fixtures for malformed response envelopes, stale response envelopes, and redaction diagnostics. The passing example must dogfood locally with `direnv exec . gleam run -- tracker-conformance run examples/tracker-conformance/adapter-author/manifest.pass.json --report test/tmp/tracker-conformance/adapter-author-pass.report.json` before publish.

Milestone 4 produces packaging and distribution guidance in the guide. Reviewers should see expectations for shipping a driver executable or service wrapper, pinning schema versions, bundling manifests and fixtures, running conformance in adapter CI, retaining redacted reports, and avoiding stable promises for planned transports. Optional live-backend checks remain deferred human/operator checks after implementation, not pre-publish requirements.

Milestone 5 completes synchronization, helper inventory, and validation gates. Reviewers should see schema and fixture validation for documented examples, `test/tracker_conformance_adapter_author_docs_test.gleam` or an equivalent docs-example test covering the new checked-in examples, fake-driver dogfood output retained under `test/tmp/tracker-conformance/`, full repository test, format, and lint evidence, and a docs/helper inventory proving no unrelated workflow helper, provider-live, or cache behavior changed.

## Progress

- [x] (2026-05-29) Read the LIV-416 task brief, ExecPlan authoring guidance, prior tracker conformance review documents, the current protocol spec, runbook notes, schemas, and fixture inventory.
- [x] (2026-05-29) Drafted this concise human-reviewable review document under `docs/plans/`.
- [x] (2026-05-29) Prepared the structured implementation-pack submission for Scherzo capture.
- [x] (2026-05-29) Validated this review document with `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-416-adapter-author-conformance-docs-examples.md`.
- [x] (2026-05-29) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, pre-publish fake-driver dogfood, deferred human/operator live checks, docs/helper inventory, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack obligations.

## Decision Log

- Decision: Start the follow-up implementation with a close-or-narrow inventory instead of assuming new docs are always required.
  Rationale: LIV-406 and later conformance tickets have already added MVP protocol and runbook material; duplicating adequate docs would create drift.
  Date: 2026-05-29.

- Decision: Treat JSON Schema files and checked-in conformance fixtures as the stable source of truth for documented examples.
  Rationale: Adapter-author docs are only trustworthy if examples are validated against the same artifacts the runner and tests consume.
  Date: 2026-05-29.

- Decision: Document optional packs and future transports with explicit implementation status.
  Rationale: External authors need guidance without mistaking planned behavior for a stable contract.
  Date: 2026-05-29.

- Decision: Require intentionally failing examples as documentation artifacts.
  Rationale: Diagnostics are part of the author experience; examples that only pass do not teach authors how to recover from malformed manifests, bad envelopes, capability drift, or redaction failures.
  Date: 2026-05-29.

- Decision: Use `docs/runbooks/tracker-adapter-author-guide.md` and `examples/tracker-conformance/adapter-author/` as the default public guide and example locations if the inventory does not close the work.
  Rationale: Naming paths now makes the later milestones independently executable and avoids leaving location choices to the implementer.
  Date: 2026-05-29.

- Decision: Treat review feedback about evidence, tests, dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations.
  Rationale: The workflow materializes follow-up implementation instructions from the structured pack, so prose-only obligations would be easy for later implementers to miss.
  Date: 2026-05-29.

## Validation and Acceptance

This planning issue is accepted when `docs/plans/LIV-416-adapter-author-conformance-docs-examples.md` exists, `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-416-adapter-author-conformance-docs-examples.md` exits zero and prints `REVIEW_DOC_VALID=ok`, every required level-2 review-doc section is present and non-empty, and Scherzo captures the structured implementation-pack submission for LIV-416. In the packaged workflow, the equivalent validator invocation is `scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-416-adapter-author-conformance-docs-examples.md`.

The later implementation has two acceptable outcomes. Outcome A is that the inventory gate closes or narrows the work with concrete evidence that existing LIV-406-era docs, schemas, fixtures, tests, and examples already satisfy the MVP author journey; that evidence must map required topics to files and include the validation commands run. Outcome B is that new docs and examples are still needed, in which case acceptance requires `docs/runbooks/tracker-adapter-author-guide.md`, examples under `examples/tracker-conformance/adapter-author/`, and documented coverage for manifests, driver envelopes, capability profiles, fixtures, probes, hooks, reports, redaction, optional-pack status, future-transport status, and external packaging expectations.

Test obligations are explicit for Outcome B. Add `test/tracker_conformance_adapter_author_docs_test.gleam` or an equivalent docs-example test that parses every checked-in example manifest, decodes documented driver request and response envelopes, verifies fixture references stay under `test/fixtures/tracker_conformance/`, and asserts each failing example produces the named validation error, diagnostic, or report counter promised by the guide. Negative coverage must include invalid manifest shape, missing capability for a requested pack, fixture/probe/hook namespace misuse, malformed response envelope, stale response envelope, and configured secret redaction.

Example evidence must include a fake-driver passing run, documented intentionally failing examples, retained sanitized JSON reports, and assertions that expected diagnostics or counters appear. Dogfood evidence is a pre-publish requirement and must use fake drivers with reports under `test/tmp/tracker-conformance/`, including `direnv exec . gleam run -- tracker-conformance run examples/tracker-conformance/adapter-author/manifest.pass.json --report test/tmp/tracker-conformance/adapter-author-pass.report.json` when examples are created. Optional live-backend manual evidence is deferred to a human/operator after implementation and must retain only redacted excerpts.

Synchronization evidence must prove documented manifests and envelopes validate against `.scherzo/workflows/schemas/tracker-conformance-*.v1.schema.json` or decode through the same repository tests, and documented fixture references point at checked-in files under `test/fixtures/tracker_conformance/`. Repository evidence must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Docs/helper evidence must include a helper inventory. If `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, or review-lane contract files are changed, run the relevant helper or offline contract tests and preserve provider-live/cache semantics. If they are not changed, record that no helper migration, provider-live validation, or cache validation was applicable.

## Rollout, Recovery, and Idempotence

Rollout is documentation-only and additive. Existing conformance manifests, schemas, fixtures, and runner behavior should not change unless the inventory discovers a documented example that cannot be validated; in that case the implementation should fix the smallest doc/example mismatch or narrow the plan before publishing.

Pre-publish rollout requires fake-driver dogfood only. The passing example and all intentionally failing examples should run against local fake drivers, write reports to disposable paths under `test/tmp/tracker-conformance/`, and leave the repository clean except for intended docs, examples, and tests. Live-backend checks are deferred human/operator checks after implementation, not publish blockers, and must use unique run markers, idempotent cleanup, and redacted retained evidence.

Recovery for this planning issue is to revise or remove only this Markdown file and resubmit the structured pack. Recovery for later implementation is to remove or revert the focused adapter-author guide, example directory, and docs-example tests while leaving established protocol docs, runbook notes, schemas, fixtures, and runner behavior intact unless a specific mismatch fix was separately justified.

Examples must be safe to rerun. Fake-driver examples should use deterministic fixture data and overwrite or replace their own `test/tmp/tracker-conformance/` reports. Provider-live and cache behavior are not part of this plan; if a future implementation introduces either, it must split that work and add stale-read, invalidation, TTL-disabling, and live-provider tests before acceptance.

## Open Questions and Clarifications Needed

No open questions. The default public guide path is `docs/runbooks/tracker-adapter-author-guide.md`, the default examples directory is `examples/tracker-conformance/adapter-author/`, and the required sample is a portable shell CLI driver. Language-specific samples are deferred until a separate task requests them.
