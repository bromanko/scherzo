Apply the visual review feedback to the current branch.

Requirements:
- Read `tmp/scherzo-ui-visual-review.json` and `tmp/scherzo-ui-visual-review.md`.
- Preserve local-only visual artifacts, avoid unrelated refactors, and keep the code ready for the next capture pass.
- Write structured dispositions to `tmp/scherzo-ui-visual-feedback.json` with this shape:
  - `dispositions`: array with one item per blocking finding
  - each item must include `finding_id`, `status`, and optional `rationale`
  - use `fixed`, `resolved`, `accepted`, or `waived`; `waived` requires rationale
- Write a concise repair summary to `tmp/scherzo-ui-visual-feedback.md` that maps each blocking finding to what changed.
- If the review had no blocking findings, still write both files with an empty `dispositions` array and a note saying no blocking changes were required.

Return a brief summary, but do not skip writing the JSON and Markdown files.
