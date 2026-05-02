You are running Scherzo's checked-in `workflow:implementation` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Issue labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Ticket preparation output:
{{ steps.prepare_context.stdout }}

Workflow contract:

- This workflow is for `workflow:implementation` issues that should be implemented directly from ticket context, without requiring an ExecPlan.
- The preparation output above contains `BRIEF_PATH=<path>`. Read that ticket-context brief before editing anything; it contains the Linear title, description, labels, and recent comments fetched at workflow start.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Do not commit, squash, abandon, open a PR, or otherwise integrate changes. The publish step creates the final jj description/bookmark and opens or finds the PR after validation passes.
- Keep the change focused on the Linear issue. Avoid broad refactors, opportunistic cleanup, and unrelated documentation churn.
- Prefer repository-local conventions over generic advice. Read the nearby source, tests, and docs before editing.
- If the ticket asks for a plan, design, or research instead of implementation, stop and explain that the workflow label is wrong.
- If the ticket lacks enough context for a bounded implementation, stop and state the concrete clarification needed rather than inventing broad requirements.

Implementation process:

1. Restate the concrete implementation target in one or two sentences.
2. Read the ticket-context brief from `BRIEF_PATH` and inspect the smallest useful set of files and tests.
3. Make the smallest maintainable code, script, workflow, test, or documentation change that satisfies the ticket.
4. Add or update focused tests when practical.
5. Run targeted validation when cheap and relevant. The workflow will run final format and full test validation after feedback, so avoid duplicate full-suite runs unless the issue needs them.
6. Finish with a concise summary of changed files and any validation you ran.

Final response format:

## Summary
What you changed and why.

## Files changed
- `path`: short note.

## Validation
- Commands you ran, or `Not run; deferred to workflow validation`.

## Notes for review
Anything reviewers or the feedback step should know.
