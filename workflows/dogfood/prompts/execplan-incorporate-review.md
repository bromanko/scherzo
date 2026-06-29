Incorporate the review feedback for workflow:execplan.

Review document validation output:

{{ steps.validate_review_doc.stdout }}

Before editing, read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`. Use it as Scherzo's authoritative guidance for maintaining self-contained ExecPlan artifacts in this workflow; do not load machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths.

Edit only the single review document discovered by `validate_review_doc` (see `REVIEW_DOC_PATH` in the prior command output) when the human-reviewable intent, scope, risk, rollout, or acceptance needs to change; it may be under `docs/plans/` or a task-requested repository-relative destination. Preserve every required level-2 review-doc section from the drafted document: Purpose / Big Picture, Problem Framing and Constraints, Strategy Overview, Alternatives Considered, Risks and Countermeasures, Scope Boundaries, Milestones, Progress, Surprises & Discoveries, Decision Log, Outcomes & Retrospective, Validation and Acceptance, Rollout, Recovery, and Idempotence, and Open Questions and Clarifications Needed. After incorporating review feedback, re-check that each required section is present and non-empty; if a section intentionally has no substantive content, keep the heading and write an explicit sentinel sentence such as `None.` or `No open questions.` rather than deleting it, leaving it blank, or leaving only an empty list item/comment. Also submit an updated `implementation_pack_submission` through the structured output tool `submit_implementation_pack_submission` using the provider schema `.scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json`; it must reflect any mechanical changes needed by the review feedback.

Before submitting the updated pack, make sure review feedback about acceptance evidence, test obligations, milestone specificity, manual/dogfood checks, docs/helper migration, provider-live/cache behavior, full validation, or linting is mirrored in `sections.concrete_steps` and `sections.testing_and_falsifiability`. For manual/browser/dogfood checks, preserve whether they are pre-publish requirements or deferred human/operator checks after implementation. Do not leave required implementation or validation TODOs as unchecked `Progress` items.

Do not write `exec_plan_bundle` or canonical implementation-pack JSON yourself. The helper will materialize and validate those artifacts from the structured implementation pack submission.

Final response: summarize review-doc edits and confirm the structured implementation-pack submission was updated.
