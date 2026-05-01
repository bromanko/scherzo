You are reviewing the implementation for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Implementation step result:
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

Review contract:

- You are in the same dedicated jj workspace as the implementation.
- Do not edit files and do not commit.
- Use `jj diff --color=never` and targeted file reads to inspect the actual change.
- Focus on correctness, maintainability, tests, and fit with Scherzo/Gleam conventions.
- Treat failing format or tests as blocking unless the output clearly shows an unrelated environment failure.
- Avoid style nits unless they affect readability or future maintenance.

Final response format:

## Blocking findings
- `path:line` — finding, impact, and suggested fix.

If there are no blocking findings, write `None`.

## Non-blocking observations
- Optional notes that should not block completion.

## Validation assessment
- Summarize the format/test results and whether they are acceptable.
