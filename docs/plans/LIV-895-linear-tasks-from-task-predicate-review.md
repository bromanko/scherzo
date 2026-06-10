# LIV-895 Linear tasks_from task predicate review

This review defines the follow-up implementation plan for specifying `tracker.linear.tasks_from`. Detailed file-edit steps, tests, interfaces, and artifact notes are carried in the structured implementation pack for this issue.

## Purpose / Big Picture

Scherzo needs a precise, Scherzo-owned way to say which Linear issues are viable daemon tasks. After the follow-up implementation, maintainers should be able to read a checked-in spec under `docs/specs/` and understand the `tracker.linear.tasks_from` AST, how the current single-project default desugars into it, how label and boolean predicates mean the same thing on every Linear read path, and how operators avoid overlapping daemon scopes.

## Problem Framing and Constraints

Today the practical task boundary is `tracker.linear.project` or legacy `project_slug`, stored as `TrackerConfig.project_slug` and applied as a Linear project filter in candidate reads, task-source reads, scheduled-failure search, and contract checks. That name describes a project, but the operational concept is broader: it is the predicate that decides which Linear issues Scherzo owns. The new spec must preserve the current default while leaving room for multi-project, team, label, and restricted boolean composition without accepting arbitrary raw GraphQL filters.

This planning issue must not implement the parser or multi-project runtime behavior. The follow-up implementation should be documentation-first, with enough repository context and acceptance evidence that a later parser implementation can apply the same predicate consistently.

## Strategy Overview

Write a focused spec, tentatively `docs/specs/TRACKER_LINEAR_TASKS_FROM.md`, and link or summarize it from the simplified YAML spec. The spec should define a restricted one-key AST with `project`, `projects`, `and`, `or`, and explicit label leaves such as `all_labels` and `any_label`; define validation and doctor behavior; describe desugaring from `tracker.linear.project`, `tracker.linear.project_slug`, and flat `tracker.project_slug`; and inventory every Linear query path that must consume the same compiled predicate.

The spec should also update operator safety language from “one daemon per project/root” to “one daemon per non-overlapping Linear task scope/root.” This is proportionate because it changes the contract before changing code, preventing future parser work from inventing incompatible meanings. Review feedback tightens the handoff by making acceptance evidence explicit: the implementation pack should separate documentation drafting, cross-doc safety wording, manual checklist evidence, and validation commands instead of leaving those obligations implicit.

## Alternatives Considered

Leaving the docs at single-project ownership was rejected because it would keep future multi-project or label scopes underspecified. Exposing raw Linear GraphQL filters was rejected because it would make overlap analysis, doctor diagnostics, compatibility, and safe query reuse too hard for operators and maintainers. Implementing parser and runtime support in this task was rejected because the acceptance criteria ask for a spec and explicitly exclude multi-project, team, and label implementation.

## Risks and Countermeasures

The main risk is ambiguous label semantics. The countermeasure is for the spec to define `all_labels` as requiring every listed label and `any_label` as requiring at least one listed label, including how each compiles to Linear `IssueFilter` composition.

A second risk is query-path drift, where candidate polling applies the predicate but task detail or scheduled failure search does not. The countermeasure is an explicit path inventory covering candidate polling, task-source list/detail, scheduled failure search, and contract validation, plus acceptance evidence that the spec names all four.

A third risk is overlapping daemon instances. The countermeasure is to make the updated safety invariant and future `or`/team/label overlap warnings first-class in the spec and getting-started guidance.

A fourth risk is prose-only acceptance. The countermeasure is to require file existence, grep/manual evidence for every required spec topic, review-doc validation, and an explicit statement that no browser, dogfood, provider-live, or cache evidence is applicable for this docs-only specification.

A fifth risk is validation overreach or underreach. The countermeasure is to classify checks by touched surface: docs-only implementation requires pre-publish documentation and manual-review evidence; full Gleam test, format, `glinter`, and `scherzo_lint` evidence becomes mandatory only if the follow-up touches `src/`, `test/`, workflow scripts, or validation helpers; provider-live, cache, browser, and dogfood checks stay deferred to later runtime/parser work unless this scope intentionally expands.

## Scope Boundaries

For this planning issue, scope is exactly this Markdown review document under `docs/plans/` and one structured implementation-pack submission. No production parser, tests, canonical bundle JSON, generated bundle reference, or Linear runtime behavior should be written here.

For the follow-up implementation, in scope are a new spec/design doc under `docs/specs/`, a short link or summary in the simplified YAML spec, any necessary getting-started safety wording update, and documentation-only validation evidence. Out of scope are parser implementation, multi-project support, team or label runtime support, arbitrary raw GraphQL predicates, live Linear dogfood, browser evidence, provider-live/cache behavior, and workflow-helper migrations. If the implementer discovers that a workflow helper, validation script, `src/`, or `test/` change is needed, that change should be split into a separate implementation slice or accompanied by the full matching validation evidence before publish.

## Milestones

Milestone 1 records current behavior as the baseline: the follow-up implementer confirms that `TrackerConfig.project_slug`, `src/scherzo/config/tracker_config.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/linear/task_query.gleam`, `src/scherzo/scheduled_failure_reporter.gleam`, and `src/scherzo/linear_contract.gleam` currently express a project-scoped task predicate. The milestone is complete only when the implementer captures grep or manual evidence showing `projectSlug` filters in candidate polling, task-source list/detail, scheduled failure search, and contract validation.

Milestone 2 drafts the AST semantics. The outcome is a spec section with examples for `project`, `projects`, nested `and`, nested `or`, `all_labels`, and `any_label`, plus rejected shapes for empty arrays, mixed keys, unknown keys, raw GraphQL, and excessive nesting.

Milestone 3 defines compatibility, validation, and doctor expectations. The outcome is explicit desugaring from existing project fields, precedence or conflict behavior when both old and new fields are present, error messages for invalid predicates, and doctor output expectations for canonical summaries, unsupported future leaves, and overlap warnings.

Milestone 4 defines Linear query application and safety guidance. The outcome is a query-path inventory for candidate polling, task-source list/detail, scheduled failure search, and contract validation, plus updated “one daemon per non-overlapping Linear task scope/root” wording and overlap-risk guidance.

Milestone 5 completes documentation validation. The outcome is a reviewer-readable docs-only diff with command evidence, manual checklist evidence for every acceptance criterion, and a clear note that no parser, live Linear, browser, provider-live, cache, dogfood, or helper-migration validation was required. If the implementation touches non-doc surfaces despite the intended scope, this milestone expands to include `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` evidence before publish.

## Progress

- [x] (2026-06-09) Read the prepared output target and confirmed the review document belongs directly under `docs/plans/`.
- [x] (2026-06-09) Inspected current config, Linear query, task-source, scheduled-failure, doctor, contract, and safety docs to ground the plan in the repository.
- [x] (2026-06-09) Wrote this review document as a planning artifact only; no production parser or runtime behavior was changed.
- [x] (2026-06-09) Incorporated review feedback by making milestone evidence, manual checklist timing, non-doc validation obligations, and deferred live/cache/dogfood checks explicit in this review document and the structured implementation pack.

## Decision Log

- Decision: Treat `tracker.linear.tasks_from` as a Scherzo-owned AST, not a raw Linear GraphQL passthrough. Rationale: Scherzo needs stable validation, doctor diagnostics, and overlap guidance independent of Linear schema churn. Date: 2026-06-09.
- Decision: Make existing `tracker.linear.project`, `tracker.linear.project_slug`, and `tracker.project_slug` compatibility aliases desugar only when `tasks_from` is absent. Rationale: mixed ownership-boundary fields are easy to misread and should not silently create overlapping daemon scopes. Date: 2026-06-09.
- Decision: Put parser and runtime support out of scope for the follow-up implementation. Rationale: this issue is a specification slice, and a checked-in spec reduces risk for later code work. Date: 2026-06-09.
- Decision: Treat manual documentation review and grep evidence as pre-publish requirements, while deferring browser, live Linear dogfood, provider-live, and cache checks unless the implementation leaves the docs-only scope. Rationale: review feedback asked for explicit evidence timing without inventing irrelevant runtime validation for a specification-only change. Date: 2026-06-09.

## Validation and Acceptance

This planning issue is accepted when `test -f docs/plans/LIV-895-linear-tasks-from-task-predicate-review.md` succeeds, `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-895-linear-tasks-from-task-predicate-review.md` reports `REVIEW_DOC_VALID=ok`, every required level-2 section is present and non-empty, and Scherzo captures the structured implementation-pack submission for LIV-895.

The follow-up implementation is accepted only with pre-publish evidence for each required outcome: `test -f docs/specs/TRACKER_LINEAR_TASKS_FROM.md`; grep or manual review evidence that the spec contains `project`, `projects`, `and`, `or`, `all_labels`, `any_label`, compatibility/desugaring, validation, doctor, candidate polling, task-source list/detail, scheduled failure search, contract validation, non-overlapping Linear task scope/root, and overlap risks; `grep -n "tracker.linear.tasks_from" docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md docs/specs/TRACKER_LINEAR_TASKS_FROM.md`; and `grep -n "non-overlapping Linear task scope/root" docs/GETTING_STARTED.md docs/specs/TRACKER_LINEAR_TASKS_FROM.md` or equivalent updated safety wording.

Manual documentation review is a pre-publish requirement for the follow-up. The implementer must record a short checklist confirming that invalid shapes, empty arrays, mixed keys, unknown keys, raw GraphQL passthrough, excessive nesting, compatibility conflicts, doctor summaries, unsupported future leaves, and overlap warnings are covered by the spec. Negative/error-path coverage is accepted as explicit spec examples and manual pre-publish review evidence, not automated parser tests, because no parser is implemented in this slice.

Browser evidence, live Linear dogfood, provider-live validation, cache validation, and helper migration are deferred human/operator or later-runtime checks, not pre-publish requirements for this docs-only follow-up. Full Gleam validation and linting are also not required while the diff remains confined to Markdown docs. If `src/`, `test/`, workflow scripts, schemas, or validation helpers are touched, publish is blocked until the implementer either splits that work out or provides matching evidence from `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`.

## Rollout, Recovery, and Idempotence

Rollout for this planning issue is to check in this review document and let Scherzo materialize the implementation bundle from the structured pack. Recovery is to revert or edit this single review document and resubmit corrected structured output.

Rollout for the follow-up implementation is additive documentation: add the spec, link or summarize it in existing YAML/operator docs, and do not change runtime behavior. The work is idempotent because rerunning validation commands only rereads documentation and grep/manual checks do not mutate state. Recovery is to revert the documentation diff; no Linear tasks, local state, cache entries, provider-live records, dogfood runs, or workflow helpers are mutated. If non-doc changes appear during implementation, recovery is to revert or split them before publish unless their expanded validation obligations are satisfied.

## Open Questions and Clarifications Needed

No open question blocks implementation handoff. A later parser ticket should decide exact version-gating for future leaves such as `team`, `all_labels`, and `any_label` if the runtime cannot support them immediately, but this spec should still define their intended semantics now.
