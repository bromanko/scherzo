You are running Scherzo's checked-in implementation workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Issue labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Workflow contract:

- This workflow is for `workflow:implementation` issues only.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, or switch jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Do not commit, squash, abandon, or otherwise integrate changes. Leave the workspace dirty with the implementation change.
- Keep the change focused on the Linear issue. Avoid broad refactors, opportunistic cleanup, and unrelated documentation churn.
- Prefer repository-local conventions over generic advice. Read the nearby source, tests, and docs before editing.
- If the issue asks for a plan, design, or research instead of implementation, stop and explain that the workflow label is wrong.

Implementation process:

1. Restate the concrete implementation target in one or two sentences.
2. Inspect the smallest useful set of files and tests.
3. Make the smallest maintainable code or documentation change that satisfies the issue.
4. Add or update focused tests when practical.
5. Run targeted validation when cheap. The workflow will run format and the full test suite after this step, so avoid duplicate full-suite runs unless the issue needs them.
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
