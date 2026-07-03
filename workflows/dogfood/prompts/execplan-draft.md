You are running Scherzo's `workflow:execplan` workflow for this task.

Task:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}
- State: {{ issue.state }}
- Priority: {{ issue.priority }}
- Labels: {% for label in issue.labels %}{{ label }} {% endfor %}

Description:

{{ issue.description }}

Output target preparation:

{{ steps.prepare_review_doc_target.stdout }}

Before drafting, read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`. Treat that file as Scherzo's authoritative ExecPlan guidance for this workflow, and do not load or rely on machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths. Apply the guidance through this workflow's split artifact contract: the checked-in review doc remains concise and human-reviewable, while mechanical implementation detail goes in the structured implementation pack. The review doc and implementation pack together must remain fully self-contained and executable without prior chat context or external skill files.

Create exactly one concise human-reviewable ExecPlan review document at the prepared output target above. If the task did not request a destination, the default target is `docs/plans/`; if it requested a repository-relative directory such as `doobar/docs/plans`, write the Markdown file directly under that directory; if it requested a `.md` file path, write exactly that file. Create missing target directories when needed. The checked-in review doc must include these level-2 sections and no mechanical implementation sections: Purpose / Big Picture, Problem Framing and Constraints, Strategy Overview, Alternatives Considered, Risks and Countermeasures, Scope Boundaries, Milestones, Progress, Surprises & Discoveries, Decision Log, Outcomes & Retrospective, Validation and Acceptance, Rollout, Recovery, and Idempotence, and Open Questions and Clarifications Needed. Keep every required section present and non-empty. If a required section has no substantive content, keep its heading and write an explicit sentinel sentence such as `None.` or `No open questions.`; never leave a required section blank, omit it, or fill it only with an empty list item/comment.

Do not write the canonical bundle yourself. Submit the mechanical implementation detail through the structured output tool `submit_implementation_pack_submission` using the provider schema `.scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json`; Scherzo will validate the captured submission against the canonical implementation-pack schema after tool capture. Use the task metadata above for the schema's `source_issue` compatibility field. Put concrete steps, tests, interfaces, dependencies, and artifact notes in `sections`, not in the review doc.

Structural checks before submitting:
- Keep every required level-2 section present and non-empty, using a sentinel sentence such as `None.` or `No open questions.` only when there is intentionally no substantive content.
- Keep mechanical implementation sections (`Concrete Steps`, `Testing and Falsifiability`, `Interfaces and Dependencies`, `Artifacts and Notes`) out of the review doc; put that detail in the structured implementation-pack submission.
- Keep the review document at the prepared repository-relative Markdown target and avoid generated HTML or absolute local paths.

Agent handoff consistency before submitting:
- Read the review doc and implementation-pack submission together. Use agent comprehension, not keyword matching, to ensure required acceptance, milestone, rollout/safety, and validation obligations in the review doc are carried by `sections.concrete_steps` or `sections.testing_and_falsifiability`.
- When acceptance requires negative/error-path coverage, idempotency or duplicate-conflict checks, manual/browser/dogfood evidence, docs/helper migration, provider-live/cache behavior, full validation, or linting, carry the corresponding implementation or evidence obligation into the pack. For manual/browser/dogfood evidence, explicitly state whether it must complete before publish or is deferred to a human/operator after the implementation workflow.

After your submission, Scherzo materializes the ExecPlan bundle and creates or reuses the follow-up implementation task containing `Bundle ref:` and `Bundle sha256:` lines; do not invent those values in the review document.

Final response: summarize the review doc path, using the prepared output target, and confirm the structured implementation-pack submission was made.
