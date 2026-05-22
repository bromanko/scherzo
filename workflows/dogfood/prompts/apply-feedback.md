Apply review feedback for Scherzo's `workflow:implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task description:
{{ issue.description }}

Task preparation output:
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
- Submit review finding dispositions by calling `submit_review_finding_dispositions` exactly once as your final structured-output action. Final assistant JSON alone is invalid for this workflow; Scherzo will materialize the validated tool arguments to `tmp/review-finding-dispositions.v1.json` after this step.
- Pass a JSON object with `schema_version` as JSON number `1` (not string `"1"`), `artifact_type` `review_finding_disposition_input`, and one `entries` item per synthesized finding id from `REVIEW_FINAL_ARTIFACT_PATH` / `artifacts/review/synthesize_review/final-review.v1.json`.
- Each disposition entry must include `finding_id`, `disposition`, `rationale`, and non-empty object `evidence_refs`. Use `resolved` for fixed findings with diff/test evidence, `rejected` for invalid findings with evidence, `deferred` only for non-blocking findings with a deferral reason, and `obsolete` when later changes make the finding inapplicable.
- Each evidence ref object requires `type` and `description`; it may also include `path`, `ref`, or `command`. Bare string evidence refs are invalid for structured output. Paths/refs must be repository- or run-root-relative with no `..`, absolute paths, or environment placeholders. Examples: `{"type":"path","description":"start-key fix","path":"src/scherzo/workstream/start_key.gleam"}` and `{"type":"command","description":"full validation","command":"direnv exec . gleam test"}`.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
2. Read the review summary above and fix safe relevant findings.
3. Run targeted validation if useful and cheap.
4. Call `submit_review_finding_dispositions` with dispositions covering every synthesized finding id exactly once.
5. Summarize what changed after feedback and mention that structured disposition output was submitted.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
