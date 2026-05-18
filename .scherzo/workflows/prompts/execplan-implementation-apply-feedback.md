Apply review feedback for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

Implementation summary:
{{ steps.implement_plan.final_response }}

Post-plan-feedback change analysis output:
{{ steps.analyze_changes_after_plan_feedback.stdout }}

Plan-completion gate output:
{{ steps.gate_plan_completion.stdout }}

Review summary:
{{ steps.review_changes.final_response }}

Feedback contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and review steps.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after final validation passes.
- Fix blocking review findings, safe medium-or-smaller findings, and obvious validation risks.
- If a finding is invalid, too risky, too broad, or intentionally deferred, explain why in the final response.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json` when plan context is needed.
- Determine the checked-in review doc path from `tmp/scherzo-implementation.json` field `plan_path`, falling back to `review_doc.path` in the bundle. Treat that checked-in review doc as authoritative for current intent, scope, acceptance, risks, milestones, and living-document sections. Treat the implementation pack as mechanical context only when it does not conflict with the review doc.
- Keep the checked-in review doc's living-document sections current if you make implementation changes after review.
- Do not try to refresh `tmp/scherzo-plan-completion-verdict.json` yourself; the workflow runs a final plan-completion verifier before final validation so any tracked review fixes are checked before publish.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
2. Read the review summary above and fix safe relevant findings.
3. Run targeted validation if useful and cheap.
4. Update the checked-in review doc living-document sections if your changes affect progress, decisions, discoveries, validation, or outcomes.
5. Summarize what changed after feedback.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
