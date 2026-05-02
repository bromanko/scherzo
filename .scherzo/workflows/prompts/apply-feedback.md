Apply implementation feedback for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Implementation summary:
{{ steps.implement.final_response }}

Format check exit code: {{ steps.format_after_implement.exit_code }}

Format check stdout:
{{ steps.format_after_implement.stdout }}

Format check stderr:
{{ steps.format_after_implement.stderr }}

Test exit code: {{ steps.test_after_implement.exit_code }}

Test stdout:
{{ steps.test_after_implement.stdout }}

Test stderr:
{{ steps.test_after_implement.stderr }}

Code review:
{{ steps.code_review.final_response }}

Feedback contract:

- You are in the same dedicated jj workspace as the implementation.
- Do not create, forget, finish, or switch jj workspaces.
- Do not commit, squash, abandon, or otherwise integrate changes.
- Fix blocking review findings and validation failures.
- If a reported problem is invalid or intentionally deferred, explain why in the final response.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `jj status --color=never` and `jj diff --color=never`.
2. Fix blocking findings and test/format failures.
3. Run targeted validation if useful. The workflow will run final format and test validation next.
4. Summarize what changed after feedback.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
