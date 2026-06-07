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
- If the recovery status is `repair_needed`, this is attempt 2 of 3; one final automatic plan-completion repair pass may remain if independent verification still fails.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json` before making a repair.
- Treat `tmp/execplan-review-doc.md` as the authoritative canonical plan resolved during prepare from descriptor-first `plan` entry in `exec_plan_bundle.entries` (or legacy `exec_plan_bundle.plan.ref` / `review_doc.path` fallback). `tmp/scherzo-implementation.json` `plan_path` points at that prepared local plan; any `review_surface_path` or legacy `review_doc.path` is optional publication metadata. Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Implement only the `blocking_findings` described in the recovery JSON/verdict. Do not broaden scope or perform unrelated cleanup.
- Do not edit the prepared canonical plan artifact during repair; describe any living-document updates that should happen through a follow-up ExecPlan revision or optional review surface.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and read `tmp/scherzo-plan-completion-recovery.json` plus `tmp/scherzo-plan-completion-verdict.json`.
2. If the recovery status is not `repair_needed`, stop without editing tracked files.
3. If the recovery status is `repair_needed`, read the canonical plan artifact and prepared implementation pack, then implement the blocking findings with the smallest maintainable change.
4. Add or update focused tests when required by the missing work.
5. Report any canonical-plan/review-surface living-section updates that should be handled by a follow-up ExecPlan revision.
6. Run targeted validation if useful and cheap.
7. Summarize what changed after the late repair pass.

Final response format:

## Late plan-completion feedback applied
- Bullet list of fixes made, or `None; recovery status was not repair_needed`.

## Deferred or rejected findings
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
