Apply review feedback for Scherzo's `workflow:execplan-implementation` workflow on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Implementation summary:
{{ steps.implement_plan.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Review summary:
{{ steps.review_changes.final_response }}

Feedback contract:

- You are in the same dedicated jj workspace as the implementation and review steps.
- Do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Do not create jj/git commits. The publish step creates the final logical jj commit after final validation passes.
- Fix blocking review findings, safe medium-or-smaller findings, and obvious validation risks.
- If a finding is invalid, too risky, too broad, or intentionally deferred, explain why in the final response.
- Keep the ExecPlan's living-document sections current if you make implementation changes after review.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `jj status --color=never` and, if needed, `jj diff --color=never`.
2. Read the review summary above and fix safe relevant findings.
3. Run targeted validation if useful and cheap.
4. Update the ExecPlan living-document sections if your changes affect progress, decisions, discoveries, or outcomes.
5. Summarize what changed after feedback.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
