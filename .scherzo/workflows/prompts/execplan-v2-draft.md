You are running Scherzo's `workflow:execplan-v2` workflow for this task.

Task:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}
- State: {{ issue.state }}
- Priority: {{ issue.priority }}
- Labels: {% for label in issue.labels %}{{ label }} {% endfor %}

Description:

{{ issue.description }}

Create exactly one concise human-reviewable ExecPlan v2 review document under `docs/plans/*.md`. The checked-in review doc must include these level-2 sections and no mechanical implementation sections: Purpose / Big Picture, Problem Framing and Constraints, Strategy Overview, Alternatives Considered, Risks and Countermeasures, Scope Boundaries, Milestones, Progress, Decision Log, Validation and Acceptance, Rollout, Recovery, and Idempotence, and Open Questions and Clarifications Needed.

Do not write the canonical bundle yourself. Submit the mechanical implementation detail through the structured output tool `submit_implementation_pack_submission` using the provider schema `.scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json`; Scherzo will validate the captured submission against the canonical implementation-pack schema after tool capture. Use the task metadata above for the schema's `source_issue` compatibility field. Put concrete steps, tests, interfaces, dependencies, and artifact notes in `sections`, not in the review doc.

Final response: summarize the review doc path and confirm the structured implementation-pack submission was made.
