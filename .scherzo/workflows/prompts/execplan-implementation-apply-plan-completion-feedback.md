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

- You are in the same dedicated jj workspace as the implementation and verification steps.
- Do not create, forget, finish, switch, push, bookmark, or otherwise manage jj workspaces, branches, bookmarks, pushes, or pull requests.
- Do not create jj/git commits. The publish step creates the final logical jj commit after validation passes.
- Read `tmp/scherzo-plan-completion-verdict.json` first.
- If the verdict is `pass`, make no tracked-file edits. Report that no plan-completion feedback was required.
- If the verdict is `fail`, this is the workflow's single automatic repair chance before code review. Complete only the missing required ExecPlan behavior described in `blocking_findings`.
- Keep the ExecPlan as a living document while you work: update its Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections when your changes affect them.
- Do not broaden scope, start optional/stretch work, or perform code-review cleanup unrelated to the blocking plan-completion findings.

Process:

1. Inspect `jj status --color=never` and read `tmp/scherzo-plan-completion-verdict.json`.
2. If the verdict is `pass`, stop without editing tracked files.
3. If the verdict is `fail`, read the full ExecPlan and implement the blocking findings with the smallest maintainable change.
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
