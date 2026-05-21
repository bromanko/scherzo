# Require final finding disposition evidence

This ExecPlan review document captures the human-facing intent, scope, risks, and acceptance for LIV-442. Mechanical implementation details are submitted separately through Scherzo's structured implementation-pack channel.

## Purpose / Big Picture

After this change, a reader of a native implementation review can see what happened to every synthesized review finding after review fixes and feedback application. The final review and the ExecPlan code-change bundle will include a disposition table showing whether each finding was resolved, rejected, deferred, or made obsolete, with concise rationale and evidence references.

## Problem Framing and Constraints

Native review lanes already synthesize findings and preserve lane evidence, but the final artifact currently leaves the post-review outcome implicit. A human must infer from the final diff, validation logs, or agent responses whether a finding was fixed, intentionally rejected, postponed, or superseded by later work.

The solution must preserve the existing native lane model, final validation gate, read-only lane containment, and repository-relative retained artifacts. It must integrate after `apply_review_feedback`, because disposition evidence is about the final state after feedback, and it must block publication if a blocking review finding has no acceptable disposition evidence.

## Strategy Overview

The right-sized approach is to add a final disposition materialization gate after feedback and final validation. The feedback application step records an agent-authored disposition input for every synthesized finding, and a deterministic review helper validates that input against the synthesized final review artifact, the final validation artifact, and the current retained diff.

The helper then writes a schema-valid disposition artifact and a post-feedback final review artifact with both JSON fields and a Markdown table. The code-change bundle should retain those artifacts so non-blocking rejected or deferred findings remain visible for human judgment even when publish is allowed.

## Alternatives Considered

One alternative is to ask the feedback agent to mention dispositions only in its final response. That is insufficient because responses are not schema-validated, are hard to bundle reliably, and can omit a finding without failing publication.

Another alternative is to rerun native review lanes after feedback and treat missing findings as resolved. That would be expensive, could produce new nondeterministic findings, and still would not explain rejected or deferred items.

A broader alternative is to redesign review synthesis around a new canonical finding lifecycle. That may be useful later, but this issue only needs final disposition evidence for existing synthesized findings, so an additive finalizer is safer.

## Risks and Countermeasures

The main risk is that the feedback agent could provide incomplete or vague disposition data. The countermeasure is a deterministic validator that requires exact coverage of all synthesized finding IDs, a valid state, non-empty rationale, and at least one evidence reference for every entry.

A second risk is allowing a blocking finding to be hidden behind an unsupported deferral. The countermeasure is a publish-time gate: blocking findings may not be missing or deferred, and rejected or obsolete blocking dispositions must carry evidence strong enough for a human to judge the decision.

A third risk is breaking existing review validation before feedback has run. The countermeasure is to keep the initial synthesis artifact valid without dispositions and require disposition evidence only in the new post-feedback finalization step and publication path.

## Scope Boundaries

In scope: native review synthesis outputs, the feedback application prompt contract, a deterministic disposition finalizer, artifact/schema validation for disposition evidence, workflow publish gating, code-change bundle materialization, and tests for all disposition states and blocking behavior.

Out of scope: changing native lane prompts to adjudicate their own findings, running review lanes a second time after feedback, changing Linear or GitHub publication semantics beyond blocking unsafe publish, or making non-blocking deferred/rejected findings disappear from final artifacts.

## Milestones

First, codify the disposition artifact contract and write focused tests that demonstrate the current gap: synthesized findings lack final states, and an unresolved blocking finding can reach the publish path without a disposition gate.

Second, implement the deterministic finalizer that consumes the synthesized final review, the feedback disposition input, and final validation evidence, then emits both a disposition artifact and an updated final review with a Markdown disposition table.

Third, wire the feedback prompts and implementation workflows so disposition input is produced after feedback and validated after final validation but before publish.

Fourth, extend code-change bundle materialization so the finalized review JSON, disposition artifact, and human-readable Markdown are retained in the bundle.

Fifth, run targeted tests and repository gates to prove all disposition states, unresolved-blocking failure, bundle exposure, workflow wiring, and lint policy remain green.

## Progress

- [x] (2026-05-20 00:00Z) Drafted the human-reviewable ExecPlan review document for LIV-442.

## Decision Log

- Decision: Add a post-feedback disposition finalizer instead of requiring dispositions during initial review synthesis.
  Rationale: The required evidence concerns what happened after feedback application, so initial synthesis cannot truthfully know final outcomes.
  Date: 2026-05-20

- Decision: Keep non-blocking rejected and deferred findings in the final table and bundle.
  Rationale: Human reviewers need visibility into judgment calls even when publication is not blocked.
  Date: 2026-05-20

- Decision: Make missing or deferred blocking findings fail the publish gate.
  Rationale: A blocking finding is not safely publishable unless it is resolved, proven rejected, or made obsolete with evidence.
  Date: 2026-05-20

## Validation and Acceptance

Acceptance is met when a native implementation workflow run produces a final review JSON artifact containing one disposition entry for every synthesized finding, a final review Markdown table with the same entries, and a code-change bundle that retains those artifacts. Each entry must show one of `resolved`, `rejected`, `deferred`, or `obsolete`, plus rationale and evidence references such as retained diff paths, validation artifacts, test output, commit/change identifiers, or a documented deferral reason.

Targeted tests must cover all four disposition states, exact finding-ID coverage, duplicate or extra disposition rejection, and publish-blocking behavior for a blocking finding that is missing or deferred. Workflow tests must prove the finalizer runs after `apply_review_feedback` and final validation but before publish. Bundle tests must prove the finalized JSON and Markdown artifacts are retained in the code-change bundle. Final validation should include `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Rollout, Recovery, and Idempotence

The rollout is additive: native review synthesis can keep producing its existing pre-feedback final review, and the new finalized review is written under a later run artifact directory. If disposition finalization fails, publication should stop before remote mutation and the retained workspace should contain the missing or invalid disposition input for repair.

The finalizer should be idempotent for the same final review, disposition input, validation artifact, and diff. Rerunning it should rewrite the same output paths, preserve repository-relative references, and avoid Linear, GitHub, workspace-driver publish, or review-lane mutations.

## Open Questions and Clarifications Needed

No stakeholder clarification is required before implementation. The implementer should verify the exact retained path names available for the final validation artifact and synthesized final review in both `implementation` and `execplan-implementation` workflows before wiring the publish gate.
