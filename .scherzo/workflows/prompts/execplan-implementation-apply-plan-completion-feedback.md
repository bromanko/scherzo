Apply plan-completion feedback for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

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
- If the verdict is `fail`, this is the workflow's single automatic repair chance before code review. Complete only the missing required behavior described in `blocking_findings`.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json` before making a repair.
- Treat the checked-in review doc named by `tmp/scherzo-implementation.json` `plan_path` (or `review_doc.path` in the bundle) as authoritative for current intent, scope, acceptance, risks, milestones, and living-document sections. Treat `tmp/execplan-review-doc.md` as the prepared bundle baseline. Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with review-doc intent, scope, acceptance, or safety.
- Keep the checked-in review doc's living-document sections current when your changes affect Progress, Validation and Acceptance, Surprises/Discoveries, Decisions, or Outcomes.
- Do not broaden scope, start optional/stretch work, or perform code-review cleanup unrelated to the blocking plan-completion findings.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and read `tmp/scherzo-plan-completion-verdict.json`.
2. If the verdict is `pass`, stop without editing tracked files.
3. If the verdict is `fail`, read the checked-in review doc and prepared implementation pack, then implement the blocking findings with the smallest maintainable change.
4. Add or update focused tests when required by the missing work.
5. Update checked-in review-doc living sections when your changes affect them.
6. Run targeted validation if useful and cheap.
7. Summarize what changed after plan-completion feedback.

Final response format:

## Plan-completion feedback applied
- Bullet list of fixes made, or `None; verifier already passed`.

## Deferred or rejected findings
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
