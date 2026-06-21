Review the candidate visual iteration using the local artifacts prepared by the workflow.

Requirements:
- Read `tmp/scherzo-ui-design-context.md` and `tmp/scherzo-ui-visual-artifacts.json`.
- Base your review on the retained candidate screenshots/reports and any retained reference artifacts.
- Write structured findings to `tmp/scherzo-ui-visual-review.json` with this shape:
  - `verdict`: `pass` or `changes_requested`
  - `blocking_findings`: array of objects with stable `id`, `summary`, `evidence_refs`, and optional `disposition`
  - `nonblocking_notes`: array of concise strings
  - `requested_changes`: array of concise strings
  - `evidence_refs`: array of retained artifact refs you used
- Write a concise human summary to `tmp/scherzo-ui-visual-review.md`.
- If there are no blocking findings, write `blocking_findings: []` explicitly.

Return a brief summary, but do not skip writing the JSON and Markdown files.
