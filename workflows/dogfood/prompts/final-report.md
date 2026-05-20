Prepare the final implementation report for task {{ issue.identifier }}: {{ issue.title }}.

Task description:
{{ issue.description }}

Implementation summary:
{{ steps.implement.final_response }}

Feedback application summary:
{{ steps.apply_feedback.final_response }}

Final validation exit code: {{ steps.final_validate.exit_code }}

Final validation stdout:
{{ steps.final_validate.stdout }}

Final validation stderr:
{{ steps.final_validate.stderr }}

Report contract:

- Do not edit files and do not commit.
- Inspect `jj status --color=never` and `jj diff --stat --color=never` if you need a concise view of the final change.
- Produce a concise, Linear-ready status update.
- If final validation passed, say so clearly. If it did not pass, this step should normally not run; if output indicates otherwise, call it out.

Final response format:

## Summary
One short paragraph describing the completed change.

## Validation
- `direnv exec . selfci check --base <base> --candidate @ --print-output`: passed or relevant note.

## Files changed
- Bullet list of important changed files/directories.

## Linear update draft
A concise comment suitable for posting back to Linear.
