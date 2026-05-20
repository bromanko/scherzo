Apply late plan-completion feedback for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- The workflow task in this prompt is the implementation handoff issue; it owns this implementation run and should be used for Linear/GitHub linkage.
- `tmp/execplan-bundle.json` records that handoff under `implementation_handoff` and records the source ExecPlan/review-doc issue under `source_issue`.
- `implementation_handoff.issue_identifier` may differ from `source_issue.identifier`; that split is valid and expected for handoff tasks.
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

Implementation summary:
{{ steps.implement_plan.final_response }}

Post-feedback change analysis output:
{{ steps.analyze_changes_after_plan_feedback.stdout }}

Pre-review plan-completion gate output:
{{ steps.gate_plan_completion.stdout }}
{{ steps.gate_plan_completion.stderr }}

Late recovery classifier output:
{{ steps.classify_plan_completion_gate.stdout }}
{{ steps.classify_plan_completion_gate.stderr }}

Feedback contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and verification steps.
- Do not create, forget, finish, switch, push, bookmark, or otherwise manage workflow workspaces, branches, bookmarks, pushes, or pull requests.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after validation passes.
- Read `tmp/scherzo-plan-completion-recovery.json` and `tmp/scherzo-plan-completion-verdict.json` first.
- If the recovery status is not `repair_needed`, make no tracked-file edits. Report that no late plan-completion repair was required.
- If the recovery status is `repair_needed`, this is attempt 2 of 2 and the final automatic plan-completion repair attempt in the current run.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json` before making a repair.
- Treat the checked-in review doc named by `tmp/scherzo-implementation.json` `plan_path` (or `review_doc.path` in the bundle) as authoritative for current intent, scope, acceptance, risks, milestones, and living-document sections. Treat `tmp/execplan-review-doc.md` as the prepared bundle baseline. Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with review-doc intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Implement only the `blocking_findings` described in the recovery JSON/verdict. Do not broaden scope or perform unrelated cleanup.
- Update the checked-in review doc living sections only when the repair changes Progress, Validation and Acceptance, decisions, or outcomes.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and read `tmp/scherzo-plan-completion-recovery.json` plus `tmp/scherzo-plan-completion-verdict.json`.
2. If the recovery status is not `repair_needed`, stop without editing tracked files.
3. If the recovery status is `repair_needed`, read the checked-in review doc and prepared implementation pack, then implement the blocking findings with the smallest maintainable change.
4. Add or update focused tests when required by the missing work.
5. Update checked-in review-doc living sections only when the repair changes progress, validation, decisions, or outcomes.
6. Run targeted validation if useful and cheap.
7. Summarize what changed after the late repair pass.

Final response format:

## Late plan-completion feedback applied
- Bullet list of fixes made, or `None; recovery status was not repair_needed`.

## Deferred or rejected findings
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
