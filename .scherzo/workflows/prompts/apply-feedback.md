Apply review feedback for Scherzo's `workflow:implementation` workflow on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Ticket preparation output:
{{ steps.prepare_context.stdout }}

Implementation summary:
{{ steps.implement.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Review summary:
{{ steps.code_review.final_response }}

Feedback contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and review steps.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Do not create VCS commits, open a PR, or otherwise integrate changes. The publish step uses the configured workspace driver to publish the change after final validation passes.
- Fix blocking review findings, safe medium-or-smaller findings, and obvious validation risks.
- If a finding is invalid, too risky, too broad, or intentionally deferred, explain why in the final response.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
2. Read the review summary above and fix safe relevant findings.
3. Run targeted validation if useful and cheap.
4. Summarize what changed after feedback.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
