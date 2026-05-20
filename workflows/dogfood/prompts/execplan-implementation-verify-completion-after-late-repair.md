Verify ExecPlan completion after the late plan-completion repair branch for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- The workflow task in this prompt is the implementation handoff issue; it owns this implementation run and should be used for Linear/GitHub linkage.
- `tmp/execplan-bundle.json` records that handoff under `implementation_handoff` and records the source ExecPlan/review-doc issue under `source_issue`.
- `implementation_handoff.issue_identifier` may differ from `source_issue.identifier`; that split is valid and expected for handoff tasks.
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

Initial implementation step response:
{{ steps.implement_plan.final_response }}

Initial plan-completion verifier response:
{{ steps.verify_plan_completion.final_response }}

Early plan-completion feedback application response:
{{ steps.apply_plan_completion_feedback.final_response }}

Post-early-feedback verifier response:
{{ steps.verify_plan_completion_after_feedback.final_response }}

Pre-review gate output:
{{ steps.gate_plan_completion.stdout }}
{{ steps.gate_plan_completion.stderr }}

Late recovery classifier output:
{{ steps.classify_plan_completion_gate.stdout }}
{{ steps.classify_plan_completion_gate.stderr }}

Late repair response:
{{ steps.apply_late_plan_completion_feedback.final_response }}

Post-late-repair change analysis output:
{{ steps.analyze_changes_after_late_plan_feedback.stdout }}

Verification contract:

- This is a plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, or the ExecPlan review doc. Your only allowed write is replacing the verdict artifact at `tmp/scherzo-plan-completion-verdict.json`.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json`.
- Determine the checked-in review doc path from `tmp/scherzo-implementation.json` field `plan_path`, falling back to `review_doc.path` in `tmp/execplan-bundle.json`, and read that file. Treat the checked-in review doc as authoritative for current intent, scope, risks, milestones, progress, and acceptance. Treat `tmp/execplan-review-doc.md` as the prepared bundle baseline.
- Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with review-doc intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Inspect the checked-in review doc's Progress, Validation and Acceptance, Milestones, Scope Boundaries, Open Questions, and any explicit non-goals/deferred/stretch sections.
- Compare the post-late-repair implementation summary and changed files/tests against the checked-in review doc and implementation pack. Inspect the smallest useful set of changed files and tests when the summaries are not enough.
- Explicitly return `fail` when required Progress checklist items are still unchecked, required milestones/acceptance criteria are undelivered, or outcomes promised by the review doc are not observable.
- Do not fail for imperfect wording, formatting, or optional/stretch work that is clearly marked optional, stretch, deferred, or out of scope.
- Before writing the verdict, run `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$bundle_dir/scripts/scherzo-implementation" plan-completion-context` from the repository root and copy its context values exactly into the JSON artifact.
- The following command gate blocks code review if this verdict is missing, malformed, stale, or `fail`.
- Replace `tmp/scherzo-plan-completion-verdict.json`; do not append to it.

Required verdict artifact:

Write valid JSON (no Markdown fences, no comments, no trailing commas) to `tmp/scherzo-plan-completion-verdict.json` with this schema:

```json
{
  "schema_version": 1,
  "verdict": "pass",
  "blocking_findings": [],
  "evidence": ["Evidence that required behavior and tests are present after the late repair branch."],
  "checked_acceptance_criteria": ["Acceptance criterion or required milestone checked."],
  "plan_path": "<PLAN_COMPLETION_PLAN_PATH>",
  "verified_base_change_id": "<PLAN_COMPLETION_BASE_CHANGE_ID>",
  "verified_change_id": "<PLAN_COMPLETION_CHANGE_ID>",
  "verified_diff_fingerprint": "<PLAN_COMPLETION_DIFF_FINGERPRINT>",
  "changed_files": ["<files from PLAN_COMPLETION_CHANGED_FILES>"]
}
```

Use `"verdict": "fail"` when promised behavior is still incomplete after the late repair/no-op branch. In that case, keep `blocking_findings` concrete and actionable for retained-workspace triage.

Process:

1. Read `tmp/scherzo-implementation.json`, the checked-in review doc, `tmp/execplan-review-doc.md`, `tmp/execplan-implementation-pack.json`, and `tmp/execplan-bundle.json`.
2. Read the verifier, gate, recovery-classifier, late-repair, and post-late-repair analysis responses above.
3. Inspect changed files/tests only as needed to verify promised behavior and acceptance criteria.
4. Run `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$bundle_dir/scripts/scherzo-implementation" plan-completion-context` and copy the context values exactly.
5. Replace `tmp/scherzo-plan-completion-verdict.json` with the post-late-repair verdict.
6. Finish with a concise summary of the verdict and the most important evidence/findings.

Final response format:

## Plan-completion verdict
`pass` or `fail`.

## Blocking findings
- Bullet list, or `None`.

## Evidence checked
- Bullet list of the most important evidence and acceptance criteria checked.

## Artifact
- `tmp/scherzo-plan-completion-verdict.json` written.
