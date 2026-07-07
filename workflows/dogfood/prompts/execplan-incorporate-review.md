Incorporate the review feedback for Scherzo's `workflow:execplan` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}

Review document validation output:

{{ steps.validate_review_doc.stdout }}

Before editing, read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`. Use it as Scherzo's authoritative guidance for maintaining self-contained ExecPlan artifacts in this workflow; do not load machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths.

Edit only the single review document discovered by `validate_review_doc` (see `REVIEW_DOC_PATH` in the prior command output) when the human-reviewable intent, scope, risk, rollout, or acceptance needs to change; it may be under `docs/plans/` or a task-requested repository-relative destination. Preserve every required level-2 review-doc section from the drafted document: Purpose / Big Picture, Problem Framing and Constraints, Strategy Overview, Alternatives Considered, Risks and Countermeasures, Scope Boundaries, Milestones, Progress, Surprises & Discoveries, Decision Log, Outcomes & Retrospective, Validation and Acceptance, Rollout, Recovery, and Idempotence, and Open Questions and Clarifications Needed. After incorporating review feedback, re-check that each required section is present and non-empty; if a section intentionally has no substantive content, keep the heading and write an explicit sentinel sentence such as `None.` or `No open questions.` rather than deleting it, leaving it blank, or leaving only an empty list item/comment. Also submit an updated `implementation_pack_submission` through the structured output tool `submit_implementation_pack_submission` using the provider schema `.scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json`; it must reflect any mechanical changes needed by the review feedback. Use the task metadata above for the schema's `source_issue` compatibility field; never use a workflow label such as `workflow:execplan` as the source issue identifier.

Before submitting the updated pack, re-read the final review doc and implementation-pack submission together. Use agent comprehension, not keyword matching, to keep the pack consistent with required acceptance, milestone, rollout/safety, and validation obligations in the review doc and with explicit reviewer-requested mechanical changes. When acceptance requires negative/error-path coverage, idempotency or duplicate-conflict checks, manual/browser/dogfood evidence, docs/helper migration, provider-live/cache behavior, full validation, or linting, carry the corresponding implementation or evidence obligation into `sections.concrete_steps` or `sections.testing_and_falsifiability`; for manual/browser/dogfood evidence, preserve whether it is pre-publish blocking or deferred to a human/operator after the implementation workflow. The helper enforces review-doc structure and pack schema shape; it will not infer semantic alignment by keyword cue matching.

Do not write `exec_plan_bundle` or canonical implementation-pack JSON yourself. The helper will materialize and validate those artifacts from the structured implementation pack submission.

Final response: summarize review-doc edits and confirm the structured implementation-pack submission was updated.
