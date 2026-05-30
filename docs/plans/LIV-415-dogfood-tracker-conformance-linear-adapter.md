# Dogfood tracker conformance against the Linear adapter safely

This ExecPlan v2 review document is the human review surface for LIV-415. It plans a later, operator-approved Linear dogfood path for the tracker conformance suite; it does not run conformance against production Linear and does not create fixture data in this issue.

## Purpose / Big Picture

Scherzo should eventually prove that its only production tracker adapter, Linear, satisfies the same black-box conformance contract offered to external adapters. The visible outcome of the later implementation is an operator-run dogfood command that invokes the Linear adapter through the conformance driver process boundary, writes a redacted report, and uses only synthetic fixture issues from a dedicated Linear conformance project.

The first dogfood target is read-only: `task_source` against pre-provisioned Linear fixture issues. Optional side-effect packs become eligible only after fixture cleanup, redaction, and recovery rules are proven with fake Linear transports and then with a human-approved fixture project.

## Problem Framing and Constraints

The current conformance suite is useful because it treats an adapter as a black box. Dogfooding it against Linear must not weaken that proof by importing `src/scherzo/tracker/linear_adapter.gleam` inside the runner or by calling adapter functions directly as acceptance evidence. A small driver executable may construct the public Linear adapter and translate conformance envelopes, but the proof must be the `tracker-conformance run` report produced by spawning that driver as a separate process.

The safety constraint is stronger than ordinary test isolation. No run may target the production Scherzo project, scrape real issue text, mutate real work items, or store raw Linear API tokens in manifests, reports, stdout, retained artifacts, or review comments. Credentials must come from a dedicated environment variable such as `SCHERZO_LINEAR_CONFORMANCE_API_KEY`, preferably attached to a bot account whose access is limited to the fixture workspace or fixture project. The ordinary `LINEAR_API_KEY` used by Scherzo daemon operations is not an acceptable dogfood credential.

The fixture project must contain only synthetic data. Stable fixture issues should be labeled and titled with a conformance prefix, for example `scherzo-conformance-fixture` and `[Scherzo conformance fixture]`, and their descriptions must avoid customer, operator, and production run data. Generated run markers must include a unique run id so cleanup and recovery can identify every comment, state move, or scheduled-failure issue created by one dogfood run.

## Strategy Overview

Use a two-layer safety design. The first layer is the existing black-box conformance runner and manifest format. The Linear manifest declares a CLI driver command, `adapter_kind = "linear"`, explicit `fixtures.tasks`, a bounded timeout, and report redaction values loaded from operator-reviewed configuration. The second layer is a Linear-specific operator wrapper and runbook that refuses unsafe projects, requires the dedicated credential variable, blocks side-effect packs by default, writes reports under `tmp/tracker-conformance/linear/<run-id>/`, and runs redaction checks before an operator shares any output.

Fixture setup should be mostly manual and pre-provisioned. A human creates a dedicated Linear workspace or, if a separate workspace is unavailable, a dedicated Linear team/project that is not the normal Scherzo project. They create fixture issues for read-only task-source and routing cases, record only issue ids/identifiers and synthetic labels in repository-local template manifests, and keep the actual secret token outside the repository. Setup hooks may verify fixture visibility, but they must not be used as conformance evidence.

Cleanup must be explicit and idempotent. Read-only packs should not need cleanup beyond deleting local reports. Side-effect packs require a cleanup helper that can be run repeatedly for the same run id, restores fixture issue state, removes or archives generated comments/issues where Linear permits it, and reports any leftover marker as `cleanup_failed` or as operator recovery work rather than hiding it behind a passing adapter case.

## Alternatives Considered

The simplest alternative is to keep Linear dogfood entirely manual by asking an operator to inspect the Linear adapter with existing `doctor` or smoke commands. That is insufficient because it does not exercise the conformance driver boundary, case reports, fixture/probe separation, or optional-pack gating that external adapters must satisfy.

A second alternative is to point conformance directly at Scherzo's production Linear project because that project already has realistic data. That is rejected. Real tasks contain operator context, comments, labels, and workflow state that must not be mutated or retained in conformance reports, and accidental side effects would have production impact.

A third alternative is to write in-process tests that import the Linear adapter and call its capabilities. Those tests can help develop the driver, but they cannot be dogfood proof. The accepted evidence must come from the conformance runner invoking a separate driver command and producing a report.

## Risks and Countermeasures

The main risk is touching real production issue data. Counter this by requiring a dedicated fixture workspace or project, rejecting known production project slugs in the operator wrapper, requiring explicit fixture issue ids, and making live dogfood manual until a human confirms the target project contains only synthetic conformance fixtures.

A second risk is secret leakage. Counter this by using `SCHERZO_LINEAR_CONFORMANCE_API_KEY` rather than `LINEAR_API_KEY`, resolving credentials at runtime, adding the resolved token and configured fixture secrets to the report redaction set, and requiring grep or equivalent checks that reports, summaries, hook diagnostics, and retained artifacts contain no raw token, bearer header, or synthetic secret marker.

A third risk is a run failing after side effects but before cleanup. Counter this by keeping side-effect packs disabled for the first Linear dogfood, using unique run markers for all later writes, attempting cleanup after setup/probe/case failures, preserving a private redacted report for recovery, and documenting a manual recovery checklist for resetting fixture states and removing or archiving generated artifacts.

A fourth risk is giving false confidence by proving adapter internals instead of the public boundary. Counter this with a driver-boundary guardrail: live dogfood evidence must name the manifest, external driver command, report path, and conformance summary; tests that call Linear adapter internals directly are development support only and cannot satisfy dogfood acceptance.

A fifth risk is that review feedback is captured only in this prose document while the structured implementation pack still omits acceptance evidence, test obligations, manual dogfood timing, docs/helper migration boundaries, provider-live/cache non-scope, or lint/full-validation gates. Counter this by mirroring those obligations in the pack's concrete steps and testing notes before Scherzo materializes follow-up implementation artifacts.

## Scope Boundaries

In scope for LIV-415 is only this review document and one structured implementation-pack submission. No Linear API call, fixture project creation, conformance run, canonical bundle file, production issue mutation, or checked-in credential belongs in this issue.

The first follow-up implementation boundary is a Linear dogfood driver and offline tests. It may add a CLI driver command, fake Linear transport fixtures, manifest templates, redaction tests, and a boundary guardrail, but it must not require a live Linear credential to pass repository validation.

The second boundary is operator documentation and fixture provisioning. It should add a runbook for creating or verifying the dedicated fixture workspace/project, naming fixture labels and states, recording data-retention rules, using the dedicated credential variable, collecting only redacted report evidence, and deciding where private live reports are retained. The implementation must update `docs/runbooks/tracker-adapters.md` or a clearly named companion runbook; if it changes workflow helpers, workflow schemas, provider-facing structured-output helpers, or `.scherzo/workflows/scripts/*`, it must name and run the matching helper or contract tests. If no helper migration is needed, the evidence must say so explicitly.

The third boundary is manual read-only dogfood for `task_source`, then `routing_metadata` after fixture labels and blockers are proven synthetic. Comments, state transitions, handoff, scheduled failures, and any other write-capable pack remain disabled until cleanup helpers, probes, idempotency, and retention rules pass separate safety gates. `remote_commands` remains disabled for Linear because the current Linear adapter does not expose that capability and production runtime no longer consumes Linear command comments.

The fourth boundary is provider-live and cache behavior. This plan should not change provider-live review-lane checks, provider contract cache files, or tracker-conformance caching semantics; the conformance runner must issue fresh driver requests. If the follow-up implementation discovers it needs a cache, provider-live change, or workflow-helper contract change, that work should split into a separate reviewed ticket and require stale-read, invalidation, TTL-disabling, and helper-contract validation before acceptance.

The fifth boundary, if the manual dogfood path proves stable, is a decision about CI or scheduling. CI may continue to run offline fake-transport tests by default. Live Linear dogfood should become a non-blocking scheduled job only after dedicated credentials, fixture isolation, cleanup recovery, redaction checks, rate limits, and artifact retention are all documented and observed over repeated manual runs.

## Milestones

Milestone 1 defines the safe fixture target and operator preflight. Reviewers should see a runbook and manifest template that require a dedicated Linear fixture workspace or project, reject the normal Scherzo production project, name `SCHERZO_LINEAR_CONFORMANCE_API_KEY`, describe setup, cleanup, and retention rules, and show exact redaction checks an operator must run before sharing a report. This milestone also records a docs/helper migration inventory: either no workflow helper, workflow schema, provider-facing structured-output helper, or `.scherzo/workflows/scripts/*` file changed, or the matching helper/contract tests ran and passed.

Milestone 2 adds the black-box Linear driver path without live credentials. Reviewers should see a driver executable that reads one conformance request from stdin, invokes the public Linear adapter using runtime configuration, writes one conformance response to stdout, and is tested with fake Linear transports. Pre-publish evidence must include an offline conformance run or driver-invocation test proving the process boundary, a guardrail that direct Linear adapter calls are not accepted as dogfood proof, and tests that fake tokens, bearer headers, fixture identifiers, diagnostics, summaries, and retained reports are redacted.

Milestone 3 enables manual read-only dogfood. Reviewers should see an operator-approved `task_source` manifest using explicit fixture task ids and instructions to write a redacted report under `tmp/tracker-conformance/linear/<run-id>/`. This live Linear run is a deferred human/operator check after implementation handoff, not a pre-publish requirement for the implementation workflow. `routing_metadata` may be added only after labels and blocker fixtures are synthetic and probe results remain support evidence rather than adapter evidence.

Milestone 4 gates side-effect packs. Reviewers should see comments, state transitions, handoff, scheduled failures, and any other write-capable pack disabled by default for live Linear; each pack requires fake-transport negative tests, cleanup idempotency evidence, probe evidence, redaction evidence, explicit report artifacts, and a human fixture review before any live Linear run. Remote commands stay disabled unless a future Linear adapter intentionally adds the capability.

Milestone 5 decides whether automation is appropriate. Reviewers should see a written decision that keeps live Linear dogfood manual, or promotes only the read-only profile to a non-blocking scheduled job with private artifacts, rate limits, cleanup verification, and secret rotation instructions. PR-blocking CI remains limited to offline fake-transport tests unless a later review explicitly accepts live-provider flakiness and blast radius. If provider-live checks, provider contract caches, or tracker-conformance caching are touched, the milestone fails until that work is split or validated with stale-read, invalidation, TTL-disabling, and helper-contract tests.

## Progress

- [x] 2026-05-29: Read the LIV-415 task brief and ExecPlan authoring guidance.
- [x] 2026-05-29: Inspected the existing tracker conformance review documents, protocol spec, tracker adapter runbook, conformance runner boundary, and Linear adapter capability surface.
- [x] 2026-05-29: Drafted this concise review document under `docs/plans/`.
- [x] 2026-05-29: Prepared the structured implementation-pack submission for Scherzo capture.
- [x] 2026-05-30: Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, deferred manual dogfood checks, docs/helper migration inventory, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack submission.

## Decision Log

- Decision: Linear dogfood must use a dedicated fixture workspace or project and a dedicated credential variable, not the normal Scherzo production project or `LINEAR_API_KEY`.
  Rationale: The task explicitly requires avoiding real production issue data and strict credential constraints.
  Date: 2026-05-29.

- Decision: The first live Linear dogfood profile is `task_source`; `routing_metadata` follows only after synthetic labels and blockers are verified, while write-capable packs stay disabled until separate cleanup and recovery gates pass.
  Rationale: Read-only cases prove the black-box boundary with the least backend risk, and optional packs have distinct side effects that should not be enabled by accident.
  Date: 2026-05-29.

- Decision: Direct imports of Linear adapter internals cannot be dogfood evidence.
  Rationale: Conformance is meaningful only when the runner invokes an external driver command and validates the public request/response envelope.
  Date: 2026-05-29.

- Decision: Live Linear dogfood remains manual until repeated redacted reports and cleanup evidence justify a non-blocking scheduled job.
  Rationale: Live provider availability, rate limits, credentials, and cleanup failures should not block ordinary PR validation until their blast radius is proven small.
  Date: 2026-05-29.

- Decision: Treat review feedback about acceptance evidence, tests, dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations.
  Rationale: The workflow materializes follow-up implementation instructions from the structured pack, so prose-only obligations would be easy for later implementers to miss.
  Date: 2026-05-30.

## Validation and Acceptance

This planning issue is accepted when this file exists at `docs/plans/LIV-415-dogfood-tracker-conformance-linear-adapter.md`, the review document validator exits zero for it, and Scherzo captures the structured implementation-pack submission for LIV-415. In the workflow bundle the validator command is `scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-415-dogfood-tracker-conformance-linear-adapter.md`; in the current repository checkout the equivalent helper path is `workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-415-dogfood-tracker-conformance-linear-adapter.md` and should print `REVIEW_DOC_VALID=ok`.

The later implementation is accepted only with concrete offline evidence before publish and clearly marked manual evidence after handoff. Offline evidence must include passing tests for driver request/response translation with fake Linear transports, runtime rejection when `SCHERZO_LINEAR_CONFORMANCE_API_KEY` is missing, manifest or wrapper rejection of unsafe production project identifiers, side-effect pack disabling by default, direct-import guardrails, cleanup helper idempotency using fake Linear data, and redaction checks that seed a fake token, bearer header, fixture secret, diagnostics text, report body, and summary text and prove the raw values are absent.

Pre-publish deterministic dogfood evidence must include at least one offline fake-driver or fake-transport run of the documented Linear manifest template. The run should write a JSON report under `test/tmp/tracker-conformance/` or `tmp/tracker-conformance/linear/offline-<run-id>/`, print a `tracker-conformance adapter=linear` summary, and show zero adapter, setup, probe, and cleanup failures without raw secret markers. This offline run is distinct from the later live Linear dogfood and must complete before publishing the implementation.

Post-implementation manual evidence is deferred to a human/operator with access to the dedicated fixture project. That evidence must include the exact manifest path, run id, driver command, report path, conformance summary, redaction check output, fixture project identifier, and confirmation that no non-fixture Linear issue was read or mutated. A live `task_source` run is required before declaring Linear dogfood usable; live side-effect pack evidence is required only before enabling each side-effect pack and must include cleanup and recovery proof.

Docs/helper evidence must include updates to `docs/runbooks/tracker-adapters.md` or a clearly named companion runbook, plus any manifest template or adapter-author example that operators need for Linear dogfood. It must include a helper inventory: if `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, or conformance helper scripts are changed, run the relevant helper or offline contract tests and preserve provider-live/cache semantics; if they are not changed, record that no helper migration or provider-live/cache validation was applicable. Provider-live checks, provider contract caches, and tracker-conformance caching are accepted as unchanged for this plan unless a future split ticket intentionally adds such behavior, in which case stale-read, invalidation, TTL-disabling, and helper-contract tests become mandatory before acceptance.

Repository validation for the later implementation must include `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root. If direnv blocks `.envrc`, inspect it, run `direnv allow .`, and retry the same commands.

## Rollout, Recovery, and Idempotence

Rollout is additive and manual-first. The driver, runbook, and manifest templates can ship with no live Linear execution; operators must opt in by creating or approving the fixture project and setting `SCHERZO_LINEAR_CONFORMANCE_API_KEY`. Existing fake-driver conformance tests, provider-live review-lane checks, provider contract caches, and normal Scherzo Linear runtime behavior remain unchanged.

If a follow-up implementation discovers that it must alter workflow helpers, provider-facing structured-output contracts, provider-live checks, provider contract caches, or tracker-conformance caching, that change should be split or explicitly rolled back before publishing the Linear dogfood path. The safe default is no helper migration beyond docs/templates and no provider-live/cache behavior change.

Recovery for this planning issue is to revise or remove only this Markdown file and resubmit the structured pack. Recovery for a later failed dogfood run starts by stopping further packs, keeping the report private, running the redaction check, and quarantining the fixture project if any cleanup counter is nonzero or any raw secret appears. Operators then rerun the cleanup helper for the same run id, inspect the fixture project by run marker, reset fixture states, remove or archive generated comments/issues where Linear permits it, and rotate the credential if any secret may have escaped.

Idempotence is mandatory before side-effect packs are enabled. Read-only `task_source` and `routing_metadata` can be rerun against stable pre-provisioned fixture issues. Comments, transitions, handoff, and scheduled failures require unique run markers and cleanup helpers that tolerate repeated setup, partial execution, and repeated cleanup without creating duplicate untracked fixture data. Offline fake-transport tests must prove repeated setup, partial execution, and repeated cleanup before any live side-effect pack is offered to a human operator.

## Open Questions and Clarifications Needed

No blocking clarification is needed to draft the follow-up implementation plan. Before live dogfood, a human operator must choose whether the fixture target is a separate Linear workspace or a restricted project in the existing workspace, confirm what API scopes or bot-account restrictions Linear can enforce, and decide the retention period for private redacted reports and synthetic fixture artifacts.
