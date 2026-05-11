Apply plan-completion feedback for Scherzo's `workflow:execplan-implementation` workflow on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

Implementation summary:
{{ steps.implement_plan.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Plan-completion verifier summary:
{{ steps.verify_plan_completion.final_response }}

Feedback contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and verification steps.
- Do not create, forget, finish, switch, push, bookmark, or otherwise manage workflow workspaces, branches, bookmarks, pushes, or pull requests.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after validation passes.
- Read `tmp/scherzo-plan-completion-verdict.json` first.
- If the verdict is `pass`, make no tracked-file edits. Report that no plan-completion feedback was required.
- If the verdict is `fail`, this is the workflow's single automatic repair chance before code review. Complete only the missing required ExecPlan behavior described in `blocking_findings`.
- Keep the ExecPlan as a living document while you work: update its Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections when your changes affect them.
- Use bounded plan context first when it is available: read `PLAN_BRIEF_PATH` when `PLAN_BRIEF_STATUS=ok`, run `scripts/scherzo-implementation plan-brief --check` when brief metadata exists, use `scripts/scherzo-execplan-html section "$PLAN_PATH" "<section>"` for needed sections, and fall back to the full plan at `PLAN_PATH` when the brief is stale, missing, unavailable, truncated, inconsistent, or ambiguous.
- For HTML plan living-document edits, prefer `scripts/scherzo-execplan-html extract-md "$PLAN_PATH" > tmp/current-execplan.md`, edit the temporary Markdown, render back with `python3 scripts/scherzo-execplan-html render tmp/current-execplan.md "$PLAN_PATH" "$PLAN_PATH"`, then run `scripts/scherzo-implementation plan-brief --refresh-if-stale`.
- Do not broaden scope, start optional/stretch work, or perform code-review cleanup unrelated to the blocking plan-completion findings.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and read `tmp/scherzo-plan-completion-verdict.json`.
2. If the verdict is `pass`, stop without editing tracked files.
3. If the verdict is `fail`, use the brief and named `section` helper for plan context before falling back to the full ExecPlan, then implement the blocking findings with the smallest maintainable change.
4. Add or update focused tests when required by the missing work.
5. Update the ExecPlan living-document sections so the verifier can see what changed.
6. Run targeted validation if useful and cheap.
7. Summarize what changed after plan-completion feedback.

Final response format:

## Plan-completion feedback applied
- Bullet list of fixes made, or `None; verifier already passed`.

## Deferred or rejected findings
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
