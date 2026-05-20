# Populate validation evidence before native implementation review

This ExecPlan review document captures the human-facing intent, scope, risks, and acceptance for LIV-440. Mechanical implementation details are submitted separately through Scherzo's structured implementation-pack channel.

## Purpose / Big Picture

After this change, native implementation review lanes will start with a trustworthy `validation-status.v1.json` artifact. Reviewers will be able to see whether validation passed, failed, was explicitly not run yet by workflow design, or could not be interpreted because an expected validation artifact was missing or malformed. The visible improvement is that review findings can focus on implementation quality instead of repeatedly treating `unknown` or `not_supplied` validation status as the main problem.

## Problem Framing and Constraints

Today the native review preparation path writes `validation-status.v1.json` from the review brief's test/build status, and the execplan-implementation workflow invokes review preparation without supplying concrete validation status. That produces `unknown` / `not_supplied` evidence even when Scherzo has enough context to distinguish a real validation result from a deliberate pre-validation review point.

The solution must preserve read-only native review lanes, bounded retained artifacts, repository-relative artifact references, and the existing final validation gates. It must not let missing or malformed validation evidence crash review preparation when the safer behavior is to record the evidence problem explicitly for reviewers.

## Strategy Overview

The right-sized approach is to make review preparation own a small normalization layer for validation evidence. When a structured implementation validation result exists, review preparation should project it into `validation-status.v1.json` with command, exit status, result-artifact reference, diagnostic/log reference, and bounded summary or output. When validation has not been run before native review by design, the same artifact should say so explicitly. When the expected validation result is missing or malformed, the artifact should record that condition as an artifact/plumbing issue instead of silently degrading to `unknown`.

For execplan-implementation, the workflow should give review preparation the available pre-native validation result path and diagnostics reference before lane prompts start. Native lane prompts should treat explicit `not_run` as scheduling context, not as missing plumbing, and should use failed, missing, or malformed validation evidence only where it is relevant to their lane.

## Alternatives Considered

One alternative is to update only the lane prompts to stop complaining about `unknown` validation status. That reduces noise but leaves the artifact ambiguous and does not help reviewers distinguish unvalidated work from broken artifact plumbing.

Another alternative is to make every native review lane run validation itself. That would duplicate expensive commands across lanes, risk workspace mutation, and weaken the single-source-of-truth artifact model.

A broader alternative is to redesign all review artifacts around a new schema. That is disproportionate for this issue. A focused enrichment of the existing `validation-status.v1.json` path satisfies the acceptance criteria with less workflow risk.

## Risks and Countermeasures

The main risk is that pre-native validation can be expensive or flaky. The countermeasure is to preserve the later final validation gates and to record pre-native validation as evidence for reviewers rather than as the sole publish gate.

A second risk is that a malformed or missing validation artifact could hide a real implementation failure. The countermeasure is to serialize malformed or expected-but-missing states explicitly, with artifact references and summaries, so review lanes and the final review can classify them as workflow evidence problems.

A third risk is unbounded command output entering retained artifacts or prompts. The countermeasure is to keep only bounded summaries and excerpts in `validation-status.v1.json` and point to retained diagnostics for full output.

## Scope Boundaries

In scope: the native review preparation artifact writer, the execplan-implementation pre-lane workflow handoff to that writer, native review prompt guidance around validation status, tests for present, failed, skipped/not-yet-run, missing expected validation artifacts, and malformed validation evidence, and concise documentation of the artifact meaning where appropriate.

Out of scope: changing the canonical ExecPlan bundle format, changing the final validation and publish gates, allowing review lanes to execute arbitrary validation commands, rewriting the review-lane schemas, or changing Linear/GitHub publication behavior.

## Milestones

First, establish the current artifact behavior with focused tests around native review preparation. Those tests should show that a concrete validation result is serialized into review inputs, that validation intentionally not run yet is represented as a scheduling state, and that an expected-but-missing or malformed validation artifact is represented explicitly instead of as generic `unknown`.

Second, add validation-evidence normalization in the review preparation helper and thread the execplan-implementation workflow's pre-lane validation evidence into that helper. At the end of this milestone, the prepared review artifact should contain actionable evidence before native lane prompts run.

Third, update native review prompts and supporting documentation so reviewers use the artifact correctly and do not treat explicit `not_run` as missing plumbing.

Fourth, run targeted artifact tests and the repository gates to prove that the enriched artifact remains bounded, stable, and compatible with the staged review flow.

## Progress

- [x] (2026-05-20 00:00Z) Drafted the human-reviewable ExecPlan review document for LIV-440.
- [x] (2026-05-20 00:00Z) Incorporated review feedback requiring missing expected validation artifacts to be explicit acceptance and test scope.
- [ ] Implementation pack materialization is pending Scherzo's structured bundle capture.
- [ ] Code implementation, tests, and validation remain pending the follow-up implementation task.

## Decision Log

- Decision: Keep `validation-status.v1.json` as the review-lane source of truth and enrich its contents instead of introducing a separate native-review validation artifact.
  Rationale: The lanes already receive and are instructed to read this artifact, so enriching it solves the reviewer-noise problem with minimal workflow churn.
  Date: 2026-05-20

- Decision: Represent `not_run`, missing, and malformed validation states explicitly rather than falling back to `unknown` / `not_supplied`.
  Rationale: The issue is primarily about distinguishing unvalidated work from missing artifact plumbing; explicit states make that distinction reviewable.
  Date: 2026-05-20

- Decision: Keep full validation output out of prompt-facing artifacts and retain only bounded excerpts plus artifact/log references.
  Rationale: Review lanes need enough evidence to judge status, but retained artifacts must remain bounded and safe to include in prompts.
  Date: 2026-05-20

- Decision: Test an expected-but-missing validation result separately from validation intentionally not run yet.
  Rationale: The review feedback identified that missing expected artifacts are a workflow evidence problem, while `not_run` is allowed scheduling context; conflating them would preserve the ambiguity this issue is meant to remove.
  Date: 2026-05-20

## Validation and Acceptance

Acceptance is met when an execplan-implementation run prepares native review inputs whose `validation-status.v1.json` records concrete command evidence if validation ran, records failed validation with exit status and bounded diagnostics if it failed, records explicit `not_run` when validation has not run yet by design, and records malformed or missing expected validation artifacts as evidence problems rather than generic unknowns.

Tests should cover a passing structured validation result, a failing structured validation result, the skipped/not-yet-run path, an expected-but-missing validation artifact path, and a malformed validation artifact. Review prompts should be able to rely on those states and avoid making `unknown/not_supplied` the primary finding when the artifact is explicit.

## Rollout, Recovery, and Idempotence

The rollout is additive to retained review artifacts and workflow preparation. If the new evidence path misbehaves, operators can recover by inspecting the explicit missing or malformed state in the prepared artifact and by relying on the unchanged final validation gate before publish.

The preparation step should be idempotent: rerunning it for the same diff and validation result rewrites the same bounded artifact shape under the run artifact directory. Repeated runs should not mutate remote services, review workspaces, Linear, or GitHub.

## Open Questions and Clarifications Needed

No stakeholder clarification is required before implementation. The implementer should verify the exact retained diagnostics path available for the pre-native validation command in the current workflow runner and use the most stable repository- or run-root-relative reference available.
