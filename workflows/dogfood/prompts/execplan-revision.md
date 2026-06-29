You are running Scherzo's `workflow:execplan-revision` workflow to revise a previously published ExecPlan bundle from actionable review feedback on this task.

Task:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}
- State: {{ issue.state }}
- Priority: {{ issue.priority }}
- Labels: {% for label in issue.labels %}{{ label }} {% endfor %}

Feedback and task description:

{{ issue.description }}

Before revising, read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`. Treat that file as Scherzo's authoritative guidance for revising self-contained ExecPlan artifacts in this workflow, and do not load or rely on machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths.

Read `tmp/execplan-previous-bundle.json`, `tmp/execplan-previous-pack.json`, and the checked-in review document path in `tmp/execplan-review-doc.path`. Update the review document when the feedback changes human-reviewable intent, scope, risk, rollout, milestones, living-document sections, or acceptance. If the feedback requires no substantive change but the existing review document predates a required living-document section, add that missing section with meaningful content or an explicit sentinel and treat the revision as changed. Otherwise, leave the review document unchanged. When editing, preserve every required review-doc section, including Progress, Surprises & Discoveries, Decision Log, Outcomes & Retrospective, Validation and Acceptance, Rollout, Recovery, and Idempotence, and Open Questions and Clarifications Needed.

Submit a structured `exec_plan_revision_submission` through `submit_implementation_pack_submission` using the provider schema `.scherzo/workflows/schemas/provider/exec-plan-revision-submission.v2.schema.json`; Scherzo will validate the captured submission against the canonical revision schema after tool capture. Set `revision_status` to `unchanged` only when both the review doc and implementation mechanics remain unchanged; otherwise set it to `changed` and include updated mechanical sections.

Do not write canonical bundle JSON yourself. The helper will materialize the superseding bundle and preserve the follow-up handoff contract that implementation tasks carry `Bundle ref:` and `Bundle sha256:` lines.
