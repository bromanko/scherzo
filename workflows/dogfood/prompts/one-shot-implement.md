You are running Scherzo's checked-in `workflow:one-shot` workflow for task {{ issue.identifier }}: {{ issue.title }}.

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

- This workflow is for small, well-specified `workflow:one-shot` tasks that a single focused pass can implement directly from ticket context. There are no separate review lanes or feedback steps: you are the only agent that sees this change before the deterministic validation gate and publication.
- The preparation output above contains `BRIEF_PATH=<path>`. Read that task-context brief before editing anything; it contains the source task title, description, labels, and recent comments fetched from Linear at workflow start.
- You are already inside a dedicated workflow workspace prepared by Scherzo; do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` for source-control inspection.
- Do not commit, squash, abandon, open a PR, or otherwise integrate changes. The publish step uses the configured workspace driver to publish the change after the validation gate passes.
- Keep the change focused on the task. Avoid broad refactors, opportunistic cleanup, and unrelated documentation churn.
- Prefer repository-local conventions over generic advice. Read the nearby source, tests, and docs before editing.

Scope guard — stop instead of sprawling:

- If the task needs a plan, design exploration, or research rather than a direct implementation, stop and explain that the workflow label is wrong (`workflow:implementation`, `workflow:execplan`, or `workflow:research` may fit better).
- If the task requires multi-part changes, cross-cutting redesign, ambiguous product decisions, or coordination across several subsystems, stop and state that the task exceeds one-shot scope rather than attempting a partial implementation.
- If the task lacks enough context for a bounded implementation, stop and state the concrete clarification needed rather than inventing broad requirements.

Validation — your judgement, then a hard gate:

- Because no review pass follows, you own pre-gate quality. Choose and run the validation you judge necessary for the change: targeted tests, affected-module checks, formatting, or lint on touched production code.
- After you finish, the workflow runs the full deterministic validation suite (tests, format check, and production lint) and blocks publication on any failure. Do not skip validation you believe is load-bearing just because the gate exists, and do not duplicate the entire suite when targeted checks already prove the change.
- Production code in `src/` must not add `let assert`, `panic`, or `todo`; the lint gate enforces this.

Implementation process:

1. Restate the concrete implementation target in one or two sentences.
2. Read the task-context brief from `BRIEF_PATH` and inspect the smallest useful set of files and tests.
3. Make the smallest maintainable code, script, workflow, test, or documentation change that satisfies the task.
4. Add or update focused tests when practical.
5. Run the validation you judge necessary, as described above.
6. Finish with a concise summary of changed files and the validation you ran.

Final response format:

## Summary
What you changed and why.

## Files changed
- `path`: short note.

## Validation
- Commands you ran and their results, or the explicit reason a check was unnecessary.

## Notes
Anything the operator should know when reviewing the published PR.
