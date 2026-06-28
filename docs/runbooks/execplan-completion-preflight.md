# ExecPlan completion preflight

Scherzo now moves common late `gate-plan-completion` failures into the `workflow:execplan` drafting/review path where the omission is visible before implementation starts.

## Earlier blocking checks

The ExecPlan helper blocks high-confidence plan defects before materializing an implementation pack:

- unverifiable `Validation and Acceptance` sections that do not name commands, tests, observable artifacts/output, explicit manual evidence, or explicitly deferred post-implementation manual evidence;
- ambiguous milestones such as "finish the work" or "address remaining items as needed";
- unchecked implementation or validation obligations left in `Progress` instead of represented as planned milestones/acceptance evidence;
- acceptance cues for negative/error-path tests, idempotency/duplicate conflicts, manual/browser/dogfood checks, full validation/linting, docs/helper migrations, or provider-live/cache behavior that are not mirrored in implementation-pack steps or testing notes, including whether manual/browser/dogfood checks are pre-publish requirements or deferred post-implementation operator checks.

The review and incorporation prompts also ask agents to flag required behavior that is not represented in the implementation pack. These are blocking when the deterministic helper can identify the gap; otherwise they are advisory review feedback.

## Required review-doc section failures

`validate-review-doc` fails closed when any required review-doc section is missing or has no meaningful content. Intentionally empty sections must keep their heading and use an explicit sentinel such as `None.` or `No open questions.`. If this fails in `validate_review_doc_after_review`, retry or rerun the incorporate-review step only when the agent can regenerate the section from known context; otherwise repair the review document before materializing the bundle, or move the issue back to Todo when the missing content is unknown and needs human input.

## Intentionally late checks

The final execplan-implementation plan-completion gate still runs. It remains responsible for failures that require inspecting the actual implementation diff or command results, including:

- whether code changes truly implement the promised behavior rather than just naming it;
- whether all referenced files, tests, docs, and helper paths were actually updated;
- whether validation commands, lint, the `scripts/scherzo-ci` gate, pre-publish manual browser/dogfood checks, or retained evidence really ran and passed;
- whether review feedback or base refresh introduced new gaps after the ExecPlan pack was materialized.

Retained runs that motivated the preflight included missing negative/idempotency coverage, unchecked dogfood/full-validation obligations, and required docs/helper migrations omitted from implementation steps. Those classes are now checked before implementation when they appear in the plan surface or pack. Manual/browser/dogfood checks that are explicitly marked for a human/operator after implementation are carried as deferred manual verification instead of blocking the implementation workflow.
