Apply review feedback for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

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

Post-plan-feedback change analysis output:
{{ steps.analyze_changes_after_plan_feedback.stdout }}

Plan-completion gate output:
{{ steps.gate_plan_completion.stdout }}

Late recovery finalizer output:
{{ steps.finalize_plan_completion_gate_recovery.stdout }}

Latest plan-completion verifier response:
{{ steps.verify_plan_completion_after_late_repair.final_response }}

Review summary:
{{ steps.review_changes.final_response }}

Feedback contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and review steps.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after final validation passes.
- Fix blocking review findings, safe medium-or-smaller findings, and obvious validation risks.
- If a finding is invalid, too risky, too broad, or intentionally deferred, explain why in the final response. Do not convert explicitly deferred post-implementation manual verification into a blocking review fix; preserve it for handoff.
- Write `tmp/review-finding-dispositions.v1.json` before finishing. Use schema_version `1`, artifact_type `review_finding_disposition_input`, and one `entries` item per synthesized finding id from `REVIEW_FINAL_ARTIFACT_PATH` / `artifacts/review/synthesize_review/final-review.v1.json`.
- Each disposition entry must include `finding_id`, `disposition`, `rationale`, and non-empty `evidence_refs`. Use `resolved` for fixed findings with diff/test evidence, `rejected` for invalid findings with evidence, `deferred` only for non-blocking findings with a deferral reason, and `obsolete` when later changes make the finding inapplicable.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json` when plan context is needed.
- Treat `tmp/execplan-review-doc.md` as the authoritative canonical plan resolved during prepare from `exec_plan_bundle.plan.ref` (or legacy `review_doc.path` fallback). `tmp/scherzo-implementation.json` `plan_path` points at that prepared local plan; any `review_surface_path` or legacy `review_doc.path` is optional publication metadata. Treat the implementation pack as mechanical context only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Do not edit the prepared canonical plan artifact after review; report any living-document updates that should happen through a follow-up ExecPlan revision or optional review surface.
- Do not try to refresh `tmp/scherzo-plan-completion-verdict.json` yourself; the workflow runs a final plan-completion verifier before final validation so any tracked review fixes are checked before publish.
- Keep changes focused; do not start unrelated cleanup.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
2. Read the review summary above and fix safe relevant findings.
3. Run targeted validation if useful and cheap.
4. Write `tmp/review-finding-dispositions.v1.json` covering every synthesized finding id exactly once.
5. Report any canonical-plan/review-surface living-document updates needed because of your changes.
6. Summarize what changed after feedback and mention that the disposition input file was written.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
