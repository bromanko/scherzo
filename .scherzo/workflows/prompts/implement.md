You are running Scherzo's checked-in `workflow:implementation` workflow for task {{ issue.identifier }}: {{ issue.title }}.

Task description:
{{ issue.description }}

Task labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Task preparation output:
{{ steps.prepare_context.stdout }}

Workflow contract:

- This workflow is for `workflow:implementation` tasks that should be implemented directly from source-task context, without requiring an ExecPlan.
- The preparation output above contains `BRIEF_PATH=<path>`. Read that task-context brief before editing anything; it contains the source task title, description, labels, and recent comments fetched from Linear at workflow start.
- You are already inside a dedicated workflow workspace prepared by Scherzo; do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` for source-control inspection.
- Do not commit, squash, abandon, open a PR, or otherwise integrate changes. The publish step uses the configured workspace driver to publish the change after validation passes.
- Keep the change focused on the task. Avoid broad refactors, opportunistic cleanup, and unrelated documentation churn.
- Prefer repository-local conventions over generic advice. Read the nearby source, tests, and docs before editing.
- If the task asks for a plan, design, or research instead of implementation, stop and explain that the workflow label is wrong.
- If the task lacks enough context for a bounded implementation, stop and state the concrete clarification needed rather than inventing broad requirements.

Implementation process:

1. Restate the concrete implementation target in one or two sentences.
2. Read the task-context brief from `BRIEF_PATH` and inspect the smallest useful set of files and tests.
3. Make the smallest maintainable code, script, workflow, test, or documentation change that satisfies the task.
4. Add or update focused tests when practical.
5. Run targeted validation when cheap and relevant. The workflow will run final format and full test validation after feedback, so avoid duplicate full-suite runs unless the task needs them.
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
