You are reviewing the implementation produced by Scherzo's `workflow:execplan-implementation` workflow for task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- The workflow task in this prompt is the implementation handoff issue; it owns this implementation run and should be used for Linear/GitHub linkage.
- `tmp/execplan-bundle.json` records that handoff under `implementation_handoff` and records the source ExecPlan/review-doc issue under `source_issue`.
- `implementation_handoff.issue_identifier` may differ from `source_issue.identifier`; that split is valid and expected for handoff tasks.
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

Implementation step response:
{{ steps.implement_plan.final_response }}

Post-plan-feedback change analysis output:
{{ steps.analyze_changes_after_plan_feedback.stdout }}

Post-late-repair change analysis output:
{{ steps.analyze_changes_after_late_plan_feedback.stdout }}

Plan-completion gate output:
{{ steps.gate_plan_completion.stdout }}

Late recovery finalizer output:
{{ steps.finalize_plan_completion_gate_recovery.stdout }}

Native review cutover contract output:
{{ steps.assert_native_review_cutover.stdout }}

Native review preparation output:
{{ steps.prepare_review.stdout }}

Native specialist lane structured outputs:

Correctness lane status: {{ steps.lane_correctness.status }}
Correctness structured output status: {{ steps.lane_correctness.structured_output.status }}
Correctness structured output path: {{ steps.lane_correctness.structured_output.path }}
Correctness structured output error: {{ steps.lane_correctness.structured_output.error }}
Correctness evidence verification output:
{{ steps.verify_correctness_evidence.stdout }}
Correctness normalization output:
{{ steps.normalize_correctness.stdout }}

Test-quality lane status: {{ steps.lane_test_quality.status }}
Test-quality structured output status: {{ steps.lane_test_quality.structured_output.status }}
Test-quality structured output path: {{ steps.lane_test_quality.structured_output.path }}
Test-quality structured output error: {{ steps.lane_test_quality.structured_output.error }}
Test-quality evidence verification output:
{{ steps.verify_test_quality_evidence.stdout }}
Test-quality normalization output:
{{ steps.normalize_test_quality.stdout }}

Idioms / maintainability lane status: {{ steps.lane_idioms_maintainability.status }}
Idioms / maintainability structured output status: {{ steps.lane_idioms_maintainability.structured_output.status }}
Idioms / maintainability structured output path: {{ steps.lane_idioms_maintainability.structured_output.path }}
Idioms / maintainability structured output error: {{ steps.lane_idioms_maintainability.structured_output.error }}
Idioms / maintainability evidence verification output:
{{ steps.verify_idioms_maintainability_evidence.stdout }}
Idioms / maintainability normalization output:
{{ steps.normalize_idioms_maintainability.stdout }}

Security / performance lane status: {{ steps.lane_security_performance.status }}
Security / performance structured output status: {{ steps.lane_security_performance.structured_output.status }}
Security / performance structured output path: {{ steps.lane_security_performance.structured_output.path }}
Security / performance structured output error: {{ steps.lane_security_performance.structured_output.error }}
Security / performance evidence verification output:
{{ steps.verify_security_performance_evidence.stdout }}
Security / performance normalization output:
{{ steps.normalize_security_performance.stdout }}

Native synthesis and final review artifact output:
{{ steps.synthesize_review.stdout }}

Native review artifact validation output:
{{ steps.validate_native_review_artifacts.stdout }}

Review contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` and `$SCHERZO_WORKSPACE_DRIVER diff --human` only for orientation; the analysis output above is authoritative for changed files across the workflow run.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json` for ExecPlan handoff context.
- Treat `tmp/execplan-review-doc.md` as the authoritative canonical plan resolved during prepare from descriptor-first `plan` entry in `exec_plan_bundle.entries` (or legacy `exec_plan_bundle.plan.ref` / `review_doc.path` fallback). `tmp/scherzo-implementation.json` `plan_path` points at that prepared local plan; any `review_surface_path` or legacy `review_doc.path` is optional publication metadata. Treat the implementation pack as mechanical context only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- If `REVIEW_BRIEF_PATH=...` is present in the native preparation output, read that local artifact for orientation. It is additive context only; do not post the artifact to PRs or Linear.
- Read each normalized native `ReviewLaneResult` referenced by `REVIEW_LANE_RESULT_PATH=...` in the normalization outputs, plus its retained evidence ledger/log/analysis artifacts. Treat lane findings as normalized review input: fix or report blocking findings, preserve non-blocking suspicions as feedback, and do not discard empty-finding lane logs.
- If synthesis output includes `REVIEW_SYNTHESIS_PATH=...` or `REVIEW_FINAL_ARTIFACT_PATH=...`, read the referenced artifacts first. Use the final artifact as the concise normalized review input, including lane failures and downgraded/unproven correctness claims, but still inspect the actual diff before applying fixes.
- When reporting remaining findings, cite synthesized finding ids from the final review artifact so the feedback step can submit exact structured disposition coverage.
- This review step is self-contained in the repository. Do not invoke local pi slash commands, home-directory pi skills, or files outside the checkout for review behavior.
- Use the native staged review artifacts produced by Scherzo `kind: agent` lane steps plus `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$bundle_dir/scripts/scherzo-review"` normalization command steps as the specialist review input. If `REVIEW_FINAL_ARTIFACT_PATH=...` is present, treat that final artifact as the primary findings list, including lane failures, blocking findings, downgraded findings, and retained notes.
- Inspect the actual diff before applying a fix. The staged artifacts guide review scope, but the current files decide whether a finding is still valid and safe to fix.
- If a native lane status, validation output, or final artifact reports a lane failure, malformed structured output, evidence-verification failure, or workspace mutation, preserve it as blocking workflow feedback. Do not substitute manual, heuristic, fixture, command-lane, or language-skill review for the native lane result.
- If the plan-completion gate output lists `PLAN_COMPLETION_DEFERRED_MANUAL_VERIFICATION`, preserve those items in review notes/handoff instead of treating them as blocking implementation review findings.
- If `LANGUAGES=none`, do not invent a language review. Check the changed files briefly for obvious workflow breakage and report that no supported language review was required.
- Treat unsupported review-relevant files listed in `UNSUPPORTED_REVIEW_FILES` as out of scope for this first version unless they are obviously broken by the current change.
- Keep changes focused. Do not start unrelated cleanup.
- Do not run final full validation; the workflow has a dedicated final validation step.

Review process:

1. Read the post-plan-feedback change analysis output and native staged review command outputs.
2. Read the final review artifact and any referenced lane result/log artifacts.
3. Inspect the actual diff and current files only as needed to verify findings.
4. Apply safe and relevant medium-or-smaller fixes identified by the native staged artifacts or bounded inspection.
5. Leave risky, broad, ambiguous, unsupported, or product-scope findings for the feedback step with a clear explanation.
6. Do not edit the prepared canonical plan artifact; report any living-section updates that should be handled through a follow-up ExecPlan revision or optional review surface.
7. Finish with a concise review report.

Final response format:

## Review performed
- Staged review artifacts and files inspected, or `No supported language review required`.

## Fixes applied
- Bullet list of safe fixes made, or `None`.

## Remaining findings
- Bullet list of findings for the feedback step, or `None`.

## Notes
Anything the final feedback or validation step should know.
