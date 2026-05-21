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

Create exactly one concise human-reviewable ExecPlan review document at the prepared output target above. If the task did not request a destination, the default target is `docs/plans/`; if it requested a repository-relative directory such as `doobar/docs/plans`, write the Markdown file directly under that directory; if it requested a `.md` file path, write exactly that file. Create missing target directories when needed. The checked-in review doc must include these level-2 sections and no mechanical implementation sections: Purpose / Big Picture, Problem Framing and Constraints, Strategy Overview, Alternatives Considered, Risks and Countermeasures, Scope Boundaries, Milestones, Progress, Decision Log, Validation and Acceptance, Rollout, Recovery, and Idempotence, and Open Questions and Clarifications Needed.

Do not write the canonical bundle yourself. Submit the mechanical implementation detail through the structured output tool `submit_implementation_pack_submission` using the provider schema `.scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json`; Scherzo will validate the captured submission against the canonical implementation-pack schema after tool capture. Use the task metadata above for the schema's `source_issue` compatibility field. Put concrete steps, tests, interfaces, dependencies, and artifact notes in `sections`, not in the review doc.

Completion-preflight requirements before submitting:
- Make `Validation and Acceptance` verifiable: each required acceptance outcome needs concrete evidence such as commands, tests, observable artifacts/output, explicit pre-publish manual evidence, or explicit post-implementation manual evidence to collect after handoff.
- Keep `Milestones` concrete and outcome-oriented; avoid vague milestones such as "finish the work" or "address remaining items as needed".
- Do not leave unchecked implementation or validation obligations in `Progress`; planned work belongs in `Milestones` and completed living-document updates belong in `Progress`.
- Ensure every required behavior named by the review doc is represented in the implementation pack's `concrete_steps` and/or `testing_and_falsifiability`.
- If acceptance requires negative/error-path coverage, idempotency, duplicate-conflict checks, manual/browser/dogfood evidence, docs/helper migration, provider-live/cache behavior, full validation, or linting, include matching pack steps and evidence requirements. For manual/browser/dogfood evidence, explicitly state whether it must complete before publish or is deferred to a human/operator after the implementation workflow.

After your submission, Scherzo materializes the ExecPlan bundle and creates or reuses the follow-up implementation task containing `Bundle ref:` and `Bundle sha256:` lines; do not invent those values in the review document.

Final response: summarize the review doc path, using the prepared output target, and confirm the structured implementation-pack submission was made.
