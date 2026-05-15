Apply review feedback for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

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
- Keep the ExecPlan's living-document sections current if you make implementation changes after review.
- Use bounded plan context first when plan context is needed: read `PLAN_BRIEF_PATH` when `PLAN_BRIEF_STATUS=ok`, run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-brief --check` when brief metadata exists, use `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" section "$PLAN_PATH" "<section>"` for needed sections, and fall back to the full plan at `PLAN_PATH` when the brief is stale, missing, unavailable, truncated, inconsistent, or ambiguous.
- For Markdown plan living-document edits, edit `PLAN_PATH` directly. For legacy HTML plan living-document edits, prefer `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" extract-md "$PLAN_PATH" > tmp/current-execplan.md`, edit the temporary Markdown, render back with `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" render tmp/current-execplan.md "$PLAN_PATH" "$PLAN_PATH"`, then run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-brief --refresh-if-stale`.
- Do not try to refresh `tmp/scherzo-plan-completion-verdict.json` yourself; the workflow runs a final plan-completion verifier before final validation so any tracked review fixes are checked before publish.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
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
