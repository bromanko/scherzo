Apply plan-completion feedback for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- Use the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md` for implementation-mode expectations when plan context is ambiguous. Do not load or rely on machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths.
- The workflow task in this prompt is the implementation handoff issue; it owns this implementation run and should be used for Linear/GitHub linkage.
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json` records that handoff under `implementation_handoff` and records the source ExecPlan/review-doc issue under `source_issue`.
- `implementation_handoff.issue_identifier` may differ from `source_issue.identifier`; that split is valid and expected for handoff tasks.
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

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
- Read `$SCHERZO_RUN_ROOT/state/implementation/scherzo-plan-completion-verdict.json` first.
- If the verdict is `pass`, make no tracked-file edits. Report that no plan-completion feedback was required.
- If the verdict is `fail`, this is the workflow's first automatic plan-completion repair chance before code review. Complete only the missing required behavior described in `blocking_findings`.
- Read `$SCHERZO_RUN_ROOT/state/implementation/metadata.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`, and `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json` before making a repair.
- Treat `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md` as the authoritative canonical plan resolved during prepare from descriptor-first `plan` entry in `exec_plan_bundle.entries` (or legacy `exec_plan_bundle.plan.ref` / `review_doc.path` fallback). `$SCHERZO_RUN_ROOT/state/implementation/metadata.json` `plan_path` points at that prepared local plan; any `review_surface_path` or legacy `review_doc.path` is optional publication metadata. Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Do not edit the prepared canonical plan artifact during repair; describe any living-document updates that should happen through a follow-up ExecPlan revision or optional review surface.
- Do not broaden scope, start optional/stretch work, or perform code-review cleanup unrelated to the blocking plan-completion findings.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and read `$SCHERZO_RUN_ROOT/state/implementation/scherzo-plan-completion-verdict.json`.
2. If the verdict is `pass`, stop without editing tracked files.
3. If the verdict is `fail`, read the canonical plan artifact and prepared implementation pack, then implement the blocking findings with the smallest maintainable change.
4. Add or update focused tests when required by the missing work.
5. Report any canonical-plan/review-surface living-section updates that should be handled by a follow-up ExecPlan revision.
6. Run targeted validation if useful and cheap.
7. Summarize what changed after plan-completion feedback.

Final response format:

## Plan-completion feedback applied
- Bullet list of fixes made, or `None; verifier already passed`.

## Deferred or rejected findings
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
